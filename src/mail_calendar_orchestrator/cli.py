from __future__ import annotations

import argparse
import os
import re
from dataclasses import replace
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

from mail_to_calendar.microsoft_auth import (
    MicrosoftAuthenticator,
    mask_account,
)
from mail_to_calendar.outlook_provider import (
    OutlookProvider,
    OutlookProviderConfig,
)
from mail_to_calendar.hybrid_analyzer import HybridMailAnalyzer
from mail_to_calendar.llm_classifier import LLMCalendarClassifier
from mail_to_calendar.ollama_client import OllamaClient, OllamaError
from mail_to_calendar.gmail_auth import (
    DEFAULT_GMAIL_CREDENTIALS,
    DEFAULT_GMAIL_TOKEN,
    GmailReadOnlyAuth,
)
from mail_to_calendar.gmail_client import (
    GmailProviderConfig,
    GmailReadOnlyClient,
    ScheduledGmailProvider,
)
from mail_to_calendar.classifier import RuleBasedImportanceClassifier
from mail_to_calendar.extractor import RuleBasedCalendarExtractor
from mail_to_calendar.text_normalization import without_transport_headers

from .service import MailCalendarOrchestrator
from .scheduler import (
    DEFAULT_OUTPUT_DIR,
    SystemdUserScheduler,
    run_scheduled_batch,
)
from .approvals import APPROVAL_STATUSES, ApprovalService
from calendar_execution.google_auth import (
    DEFAULT_CREDENTIALS, DEFAULT_TOKEN, GoogleCalendarAuth,
)
from calendar_execution.google_calendar import (
    GoogleCalendarClient, GoogleCalendarExecutor,
)
from calendar_execution.service import CalendarExecutionService
from line_approval.cli import (
    DEFAULT_LINE_ENV, configuration_status, initialize_config, load_config,
)
from line_approval.client import LineMessagingClient, text_message
from line_approval.service import LineApprovalService
from line_approval.webhook import serve_webhook
from line_approval.systemd import LineWebhookSystemdService
from .state import DEFAULT_STATE_DB, MailStateStore


_ENV_KEY = re.compile(r"[A-Za-z_][A-Za-z0-9_]*\Z")


def _env_enabled(name: str, default: bool = False) -> bool:
    value = os.environ.get(name)
    if value is None:
        return default
    return value.strip().casefold() in {"1", "true", "yes", "on"}


def _print_gmail_debug_body(message: object) -> None:
    """Explicit terminal-only escape hatch for inspecting MIME extraction."""
    metadata = getattr(message, "metadata", {})
    mime = metadata.get("gmail_mime", {}) if isinstance(metadata, dict) else {}
    body = str(getattr(message, "body_text", ""))
    print("DEBUG ONLY: canonical Gmail body may contain sensitive email content.")
    print(f"Selected MIME part type: {mime.get('selected_part_type', 'none')}")
    print(f"Canonical body length: {len(body)}")
    print("--- BEGIN DEBUG CANONICAL BODY ---")
    print(body)
    print("--- END DEBUG CANONICAL BODY ---")


def _load_dotenv(path: Path = Path(".env")) -> list[str]:
    """Load AGENTLEDGER_* values without executing the file as shell code."""
    if not path.exists():
        return []
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as exc:
        raise ValueError(f"could not read {path}: {exc}") from exc
    loaded: list[str] = []
    for line_number, original in enumerate(lines, start=1):
        line = original.strip()
        if not line or line.startswith("#"):
            continue
        if line.startswith("export "):
            line = line[7:].lstrip()
        if "=" not in line:
            raise ValueError(f"invalid .env entry on line {line_number}")
        key, value = line.split("=", 1)
        key = key.strip()
        value = value.strip()
        if not _ENV_KEY.fullmatch(key):
            raise ValueError(f"invalid .env key on line {line_number}")
        if not key.startswith("AGENTLEDGER_"):
            continue
        if value[:1] in {'"', "'"}:
            quote = value[0]
            if len(value) < 2 or value[-1] != quote:
                raise ValueError(f"unterminated .env quote on line {line_number}")
            value = value[1:-1]
        if key not in os.environ:
            os.environ[key] = value
            loaded.append(key)
    return loaded


def _add_runtime_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--base-year", type=int, default=datetime.now().year)
    parser.add_argument("--timezone", default="Asia/Tokyo")
    approval = parser.add_mutually_exclusive_group()
    approval.add_argument(
        "--requires-approval",
        action="store_true",
        dest="requires_approval",
    )
    approval.add_argument(
        "--no-requires-approval",
        action="store_false",
        dest="requires_approval",
    )
    parser.set_defaults(requires_approval=True)
    parser.add_argument(
        "--analysis-mode",
        choices=("rule-only", "hybrid", "llm-first", "llm-all"),
        default="llm-first",
    )
    parser.add_argument("--analysis-only", action="store_true")
    parser.add_argument(
        "--ollama-base-url",
        default=os.environ.get("AGENTLEDGER_OLLAMA_BASE_URL", "http://localhost:11434"),
    )
    parser.add_argument(
        "--ollama-model",
        default=os.environ.get("AGENTLEDGER_OLLAMA_MODEL", "qwen3:8b"),
    )
    parser.add_argument("--ollama-timeout-seconds", type=float, default=120)
    parser.add_argument("--ollama-keep-alive", default="5m")
    parser.add_argument("--ollama-temperature", type=float, default=0)
    thinking = parser.add_mutually_exclusive_group()
    thinking.add_argument(
        "--ollama-thinking",
        action="store_true",
        dest="ollama_thinking",
        help="Enable model thinking (not parsed or saved).",
    )
    thinking.add_argument(
        "--no-ollama-thinking",
        action="store_false",
        dest="ollama_thinking",
        help="Disable model thinking (default for mail analysis).",
    )
    parser.set_defaults(ollama_thinking=False)
    parser.add_argument("--llm-confidence-threshold", type=float, default=0.75)
    parser.add_argument("--llm-body-max-chars", type=int, default=6000)
    parser.add_argument("--allow-remote-ollama", action="store_true")
    parser.add_argument("--require-llm", action="store_true")
    parser.add_argument(
        "--state-db", type=Path, default=DEFAULT_STATE_DB,
        help="SQLite operational state database.",
    )
    parser.add_argument("--no-state", action="store_true")
    parser.add_argument("--reprocess", action="store_true")
    parser.add_argument("--retry-failed", action="store_true")


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="mail-calendar-orchestrator")
    commands = parser.add_subparsers(dest="command", required=True)
    process = commands.add_parser(
        "process",
        help="Process local email and propose safe calendar candidates.",
    )
    process.add_argument("--input", required=True, type=Path)
    _add_runtime_arguments(process)
    outlook = commands.add_parser(
        "outlook",
        help="Read personal Outlook mail and create local proposals.",
    )
    outlook.add_argument("--client-id")
    outlook.add_argument(
        "--authority",
        default="https://login.microsoftonline.com/consumers",
    )
    outlook.add_argument(
        "--token-cache",
        type=Path,
        default=Path("~/.config/agentledger/microsoft_token_cache.json"),
    )
    outlook.add_argument("--folder", default="inbox")
    outlook.add_argument("--max-messages", type=int, default=20)
    unread = outlook.add_mutually_exclusive_group()
    unread.add_argument(
        "--unread-only",
        action="store_true",
        dest="unread_only",
    )
    unread.add_argument(
        "--include-read",
        action="store_false",
        dest="unread_only",
    )
    outlook.set_defaults(unread_only=True)
    outlook.add_argument("--received-after")
    body = outlook.add_mutually_exclusive_group()
    body.add_argument("--include-body", action="store_true", dest="include_body")
    body.add_argument("--no-body", action="store_false", dest="include_body")
    outlook.set_defaults(include_body=True)
    outlook.add_argument("--body-max-chars", type=int, default=10_000)
    outlook.add_argument("--request-timeout-seconds", type=int, default=30)
    _add_runtime_arguments(outlook)
    check = commands.add_parser("check-llm", help="Check local Ollama and model availability.")
    check.add_argument(
        "--ollama-base-url",
        default=os.environ.get("AGENTLEDGER_OLLAMA_BASE_URL", "http://localhost:11434"),
    )
    check.add_argument(
        "--ollama-model",
        default=os.environ.get("AGENTLEDGER_OLLAMA_MODEL", "qwen3:8b"),
    )
    check.add_argument("--ollama-timeout-seconds", type=float, default=120)
    check_thinking = check.add_mutually_exclusive_group()
    check_thinking.add_argument(
        "--ollama-thinking", action="store_true", dest="ollama_thinking"
    )
    check_thinking.add_argument(
        "--no-ollama-thinking", action="store_false", dest="ollama_thinking"
    )
    check.set_defaults(ollama_thinking=False)
    check.add_argument("--allow-remote-ollama", action="store_true")
    state = commands.add_parser("state", help="Inspect or reset mail state.")
    state.add_argument(
        "--state-db", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_STATE_DB", str(DEFAULT_STATE_DB))),
    )
    state.add_argument(
        "--timezone", default=os.environ.get("AGENTLEDGER_TIMEZONE", "Asia/Tokyo")
    )
    state.add_argument("--poll-overlap-minutes", type=int, default=5)
    state_commands = state.add_subparsers(dest="state_command", required=True)
    state_commands.add_parser("summary")
    recent = state_commands.add_parser("recent")
    recent.add_argument("--limit", type=int, default=20)
    reset = state_commands.add_parser("reset-message")
    reset.add_argument("--provider", required=True)
    reset.add_argument("--message-id", required=True)
    state_commands.add_parser("cursor")

    scheduled = commands.add_parser(
        "run-scheduled", help="Run one non-interactive mail provider batch."
    )
    scheduled.add_argument("--client-id")
    scheduled.add_argument("--authority", default="https://login.microsoftonline.com/consumers")
    scheduled.add_argument(
        "--token-cache", type=Path,
        default=Path("~/.config/agentledger/microsoft_token_cache.json"),
    )
    scheduled.add_argument("--folder", default="inbox")
    scheduled.add_argument("--max-messages", type=int, default=50)
    scheduled.add_argument("--poll-overlap-minutes", type=int, default=5)
    scheduled.add_argument("--start-from", default=None)
    scheduled.add_argument("--reset-start-from", action="store_true")
    scheduled.add_argument(
        "--state-db", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_STATE_DB", str(DEFAULT_STATE_DB))),
    )
    scheduled.add_argument(
        "--output-dir", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_OUTPUT_DIR", str(DEFAULT_OUTPUT_DIR))),
    )
    scheduled.add_argument(
        "--timezone", default=os.environ.get("AGENTLEDGER_TIMEZONE", "Asia/Tokyo")
    )
    scheduled.add_argument("--base-year", type=int, default=datetime.now().year)
    scheduled.add_argument(
        "--ollama-base-url",
        default=os.environ.get("AGENTLEDGER_OLLAMA_BASE_URL", "http://localhost:11434"),
    )
    scheduled.add_argument(
        "--ollama-model",
        default=os.environ.get("AGENTLEDGER_OLLAMA_MODEL", "qwen3:8b"),
    )
    scheduled.add_argument("--ollama-timeout-seconds", type=float, default=120)
    scheduled.add_argument("--generate-explorer", action="store_true")
    gmail_toggle = scheduled.add_mutually_exclusive_group()
    gmail_toggle.add_argument(
        "--enable-gmail", action="store_true", dest="gmail_enabled"
    )
    gmail_toggle.add_argument(
        "--disable-gmail", action="store_false", dest="gmail_enabled"
    )
    scheduled.set_defaults(
        gmail_enabled=_env_enabled("AGENTLEDGER_GMAIL_ENABLED", False)
    )
    scheduled.add_argument(
        "--gmail-credentials", type=Path,
        default=Path(os.environ.get(
            "AGENTLEDGER_GOOGLE_CREDENTIALS", str(DEFAULT_GMAIL_CREDENTIALS)
        )),
    )
    scheduled.add_argument(
        "--gmail-token-cache", type=Path,
        default=Path(os.environ.get(
            "AGENTLEDGER_GMAIL_TOKEN", str(DEFAULT_GMAIL_TOKEN)
        )),
    )
    scheduled.add_argument("--gmail-max-messages", type=int, default=50)

    scheduler = commands.add_parser("scheduler", help="Manage the systemd user timer.")
    scheduler.add_argument(
        "--state-db", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_STATE_DB", str(DEFAULT_STATE_DB))),
    )
    scheduler.add_argument(
        "--timezone", default=os.environ.get("AGENTLEDGER_TIMEZONE", "Asia/Tokyo")
    )
    scheduler.add_argument("--start-from", default=None)
    scheduler.add_argument("--reset-start-from", action="store_true")
    scheduler_commands = scheduler.add_subparsers(dest="scheduler_command", required=True)
    install = scheduler_commands.add_parser("install")
    install.add_argument("--enable", action="store_true")
    for name in ("status", "enable", "disable", "uninstall", "run-now"):
        scheduler_commands.add_parser(name)

    approvals = commands.add_parser("approvals", help="Manage pending calendar approvals.")
    approvals.add_argument(
        "--state-db", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_STATE_DB", str(DEFAULT_STATE_DB))),
    )
    approvals.add_argument(
        "--output-dir", type=Path,
        default=Path("~/.local/share/agentledger/approvals"),
    )
    approval_commands = approvals.add_subparsers(dest="approval_command", required=True)
    listing = approval_commands.add_parser("list")
    listing.add_argument("--status", choices=sorted(APPROVAL_STATUSES))
    listing.add_argument("--all", action="store_true")
    listing.add_argument("--limit", type=int, default=20)
    show = approval_commands.add_parser("show")
    show.add_argument("approval_id")
    approval_commands.add_parser("summary")
    for resolution in ("approve", "reject"):
        command = approval_commands.add_parser(resolution)
        command.add_argument("approval_id")
        command.add_argument("--actor", required=True)
        command.add_argument("--reason", required=True)
        if resolution == "approve":
            command.add_argument("--execute", action="store_true")
            _add_google_execution_arguments(command)
    approval_commands.add_parser("expire")
    execute = approval_commands.add_parser("execute")
    execute.add_argument("approval_id")
    _add_google_execution_arguments(execute)
    recover = approval_commands.add_parser(
        "recover", help="Reconcile stale calendar executions without inserting events."
    )
    recover.add_argument("--stale-after-seconds", type=int, default=300)
    recover.add_argument("--limit", type=int, default=100)
    _add_google_execution_arguments(recover)

    google = commands.add_parser(
        "google-calendar", help="Configure Google Calendar OAuth."
    )
    google.add_argument(
        "--credentials", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_GOOGLE_CREDENTIALS", str(DEFAULT_CREDENTIALS))),
    )
    google.add_argument(
        "--token-cache", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_GOOGLE_TOKEN", str(DEFAULT_TOKEN))),
    )
    google.add_argument(
        "--google-calendar-id",
        default=os.environ.get("AGENTLEDGER_GOOGLE_CALENDAR_ID", "primary"),
    )
    google_commands = google.add_subparsers(dest="google_command", required=True)
    google_commands.add_parser("auth")
    google_commands.add_parser("status")
    gmail = commands.add_parser(
        "gmail", help="Configure Gmail read-only OAuth access."
    )
    gmail.add_argument(
        "--credentials", type=Path,
        default=Path(os.environ.get(
            "AGENTLEDGER_GOOGLE_CREDENTIALS", str(DEFAULT_GMAIL_CREDENTIALS)
        )),
    )
    gmail.add_argument(
        "--token-cache", type=Path,
        default=Path(os.environ.get(
            "AGENTLEDGER_GMAIL_TOKEN", str(DEFAULT_GMAIL_TOKEN)
        )),
    )
    gmail_commands = gmail.add_subparsers(dest="gmail_command", required=True)
    gmail_commands.add_parser("auth")
    gmail_commands.add_parser("status")
    gmail_list = gmail_commands.add_parser(
        "list", help="List bounded Gmail message metadata."
    )
    gmail_list.add_argument("--limit", type=int, default=5)
    gmail_analyze = gmail_commands.add_parser(
        "analyze", help="Analyze one Gmail message without creating side effects."
    )
    gmail_analyze.add_argument("message_id")
    gmail_analyze.add_argument("--base-year", type=int, default=datetime.now().year)
    gmail_analyze.add_argument("--timezone", default="Asia/Tokyo")
    gmail_analyze.add_argument(
        "--ollama-base-url",
        default=os.environ.get("AGENTLEDGER_OLLAMA_BASE_URL", "http://localhost:11434"),
    )
    gmail_analyze.add_argument(
        "--ollama-model",
        default=os.environ.get("AGENTLEDGER_OLLAMA_MODEL", "qwen3:8b"),
    )
    gmail_analyze.add_argument("--ollama-timeout-seconds", type=float, default=120)
    gmail_analyze.add_argument("--llm-confidence-threshold", type=float, default=0.75)
    gmail_analyze.add_argument("--llm-body-max-chars", type=int, default=6000)
    gmail_analyze.add_argument("--allow-remote-ollama", action="store_true")
    gmail_analyze.add_argument("--debug-grounding", action="store_true")
    gmail_analyze.add_argument(
        "--debug-body",
        action="store_true",
        help="DEBUG ONLY: print the extracted canonical email body to stdout.",
    )
    line = commands.add_parser("line", help="Manage LINE approval notifications.")
    line.add_argument("--config", type=Path, default=DEFAULT_LINE_ENV)
    line.add_argument(
        "--state-db", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_STATE_DB", str(DEFAULT_STATE_DB))),
    )
    line.add_argument(
        "--output-dir", type=Path,
        default=Path("~/.local/share/agentledger/approvals"),
    )
    line.add_argument(
        "--google-calendar-id",
        default=os.environ.get("AGENTLEDGER_GOOGLE_CALENDAR_ID", "primary"),
    )
    line.add_argument(
        "--google-credentials", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_GOOGLE_CREDENTIALS", str(DEFAULT_CREDENTIALS))),
    )
    line.add_argument(
        "--google-token-cache", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_GOOGLE_TOKEN", str(DEFAULT_TOKEN))),
    )
    line_commands = line.add_subparsers(dest="line_command", required=True)
    line_commands.add_parser("init-config")
    line_commands.add_parser("status")
    line_commands.add_parser("test-message")
    notify = line_commands.add_parser("notify")
    notify.add_argument("approval_id")
    webhook = line_commands.add_parser("webhook")
    webhook.add_argument("--host", default="127.0.0.1")
    webhook.add_argument("--port", type=int, default=8787)
    line_service = line_commands.add_parser(
        "service", help="Manage the systemd user LINE webhook service."
    )
    line_service_commands = line_service.add_subparsers(
        dest="line_service_command", required=True
    )
    for name in ("install", "status", "enable", "disable", "restart", "uninstall"):
        line_service_commands.add_parser(name)
    return parser


def _add_google_execution_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--calendar-provider", choices=("google",), default="google")
    parser.add_argument(
        "--google-calendar-id",
        default=os.environ.get("AGENTLEDGER_GOOGLE_CALENDAR_ID", "primary"),
    )
    parser.add_argument(
        "--google-credentials", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_GOOGLE_CREDENTIALS", str(DEFAULT_CREDENTIALS))),
    )
    parser.add_argument(
        "--google-token-cache", type=Path,
        default=Path(os.environ.get("AGENTLEDGER_GOOGLE_TOKEN", str(DEFAULT_TOKEN))),
    )


def _calendar_execution_service(
    store: MailStateStore, args: argparse.Namespace
) -> CalendarExecutionService:
    auth = GoogleCalendarAuth(args.google_credentials, args.google_token_cache)
    google_service = auth.build_service(interactive=False)
    executor = GoogleCalendarExecutor(GoogleCalendarClient(google_service))
    return CalendarExecutionService(
        store, executor, output_dir=args.output_dir
    )


def _check_google_event_access(
    auth: GoogleCalendarAuth, calendar_id: str, *, interactive: bool
) -> None:
    try:
        service = auth.build_service(interactive=interactive)
        service.events().list(
            calendarId=calendar_id,
            maxResults=1,
            singleEvents=True,
        ).execute()
    except Exception as exc:
        response = getattr(exc, "resp", None)
        status = getattr(response, "status", None)
        if status == 403:
            raise RuntimeError(
                "Google Calendar event access was denied (403); "
                "verify the calendar.events permission"
            ) from exc
        if isinstance(exc, (OSError, RuntimeError, ValueError)):
            raise
        raise RuntimeError("Google Calendar event access check failed") from exc


def _check_gmail_read_access(
    auth: GmailReadOnlyAuth, *, interactive: bool
) -> None:
    try:
        service = auth.build_service(interactive=interactive)
        service.users().messages().list(
            userId="me", maxResults=1, includeSpamTrash=False
        ).execute()
    except Exception as exc:
        response = getattr(exc, "resp", None)
        status = getattr(response, "status", None)
        if status == 403:
            raise RuntimeError(
                "Gmail read access was denied (403); verify the gmail.readonly permission"
            ) from exc
        if status == 401:
            raise RuntimeError(
                "Gmail authentication was rejected (401); run gmail auth again"
            ) from exc
        if isinstance(exc, (OSError, RuntimeError, ValueError)):
            raise
        raise RuntimeError("Gmail read access check failed") from exc


def _notify_pending_line_best_effort(state_path: str | Path) -> None:
    """Notify pending approvals without changing the mail batch result."""
    status = configuration_status(DEFAULT_LINE_ENV)
    if not all(status.values()):
        return
    try:
        config = load_config(DEFAULT_LINE_ENV)
        with MailStateStore(state_path) as store:
            approvals = ApprovalService(store)
            service = LineApprovalService(
                store,
                approvals,
                LineMessagingClient(config.channel_access_token),
                channel_secret=config.channel_secret,
                allowed_user_id=config.allowed_user_id,
            )
            for record in approvals.list(status="awaiting_approval", limit=50):
                try:
                    service.notify(record.approval_id)
                except (OSError, RuntimeError, ValueError):
                    continue
    except (OSError, RuntimeError, ValueError):
        return


def main() -> None:
    try:
        _load_dotenv()
    except ValueError as exc:
        argparse.ArgumentParser(prog="mail-calendar-orchestrator").error(
            str(exc)
        )
    parser = _parser()
    args = parser.parse_args()
    state_store = None
    try:
        if args.command == "check-llm":
            client = OllamaClient(
                base_url=args.ollama_base_url,
                model=args.ollama_model,
                timeout_seconds=args.ollama_timeout_seconds,
                thinking=args.ollama_thinking,
                allow_remote=args.allow_remote_ollama,
            )
            checked = client.check()
            print("Ollama: reachable")
            print(f"Model: {args.ollama_model} ({'available' if checked.model_available else 'not found'})")
            print(
                "Structured output: "
                + ("available" if checked.structured_output_available else "unavailable")
            )
            print(f"Structured output latency: {checked.latency_ms} ms")
            if not checked.model_available:
                parser.error(f"Ollama model is not installed: {args.ollama_model}")
            return
        if args.command == "state":
            with MailStateStore(args.state_db) as store:
                if args.state_command == "summary":
                    summary = store.summary()
                    counts = summary["counts"]
                    print(f"State DB: {store.path}")
                    print(f"Total processed messages: {summary['total']}")
                    print(f"Processed: {counts.get('processed', 0)}")
                    print(f"Retryable: {counts.get('retryable', 0)}")
                    print(f"Failed: {counts.get('failed', 0)}")
                    print(f"Last successful run: {summary['last_successful_run'] or 'none'}")
                    print(f"Last run duration: {summary['last_run_duration_ms'] or 0} ms")
                    print(f"Last processed message time: {summary['last_processed_message_time'] or 'none'}")
                    for provider_result in summary["provider_results"]:
                        line = (
                            f"Provider {provider_result['provider']}: "
                            f"{provider_result['status']} "
                            f"(fetched={provider_result['fetched_messages']})"
                        )
                        if provider_result["error_type"]:
                            line += f" error={provider_result['error_type']}"
                        print(line)
                elif args.state_command == "recent":
                    print(f"State DB: {store.path}")
                    for row in store.recent(max(1, args.limit)):
                        print("\t".join(str(row[key] or "-") for key in row.keys()))
                elif args.state_command == "reset-message":
                    removed = store.reset_message(args.provider, args.message_id)
                    if not removed:
                        raise ValueError("message state not found")
                    print(f"Reset: {args.provider}/{args.message_id}")
                else:
                    cursor = store.cursor(
                        overlap_minutes=args.poll_overlap_minutes
                    )
                    print(f"Processing started from: {cursor['processing_started_from'].isoformat()}")
                    print(f"Last successful poll: {cursor['last_successful_poll_at'] or 'none'}")
                    print(f"Overlap: {cursor['overlap_minutes']} minutes")
                    print(f"Next fetch from: {cursor['next_fetch_from'].isoformat()}")
            return
        if args.command == "scheduler":
            scheduler = SystemdUserScheduler()
            if args.scheduler_command == "install":
                with MailStateStore(args.state_db) as store:
                    started = store.ensure_bootstrap(
                        timezone_name=args.timezone,
                        start_from=args.start_from,
                        reset=args.reset_start_from,
                    )
                created = scheduler.install(enable=args.enable)
                print("Created:")
                for path in created:
                    print(path)
                print(f"Initial processing start: {started.isoformat()}")
                print("Emails before this time will not be processed.")
                print("Next runs: 07:30, 12:30, 18:30")
            elif args.scheduler_command == "status":
                print(scheduler.status())
            elif args.scheduler_command == "enable":
                scheduler.enable()
            elif args.scheduler_command == "disable":
                scheduler.disable()
            elif args.scheduler_command == "uninstall":
                scheduler.uninstall()
            else:
                scheduler.run_now()
            return
        if args.command == "line":
            if args.line_command == "init-config":
                print(f"LINE config: {initialize_config(args.config)}")
                return
            if args.line_command == "status":
                status = configuration_status(args.config)
                print(
                    "Channel secret: "
                    + ("configured" if status["LINE_CHANNEL_SECRET"] else "missing")
                )
                print(
                    "Access token: "
                    + ("configured" if status["LINE_CHANNEL_ACCESS_TOKEN"] else "missing")
                )
                print(
                    "Allowed user: "
                    + ("configured" if status["LINE_ALLOWED_USER_ID"] else "missing")
                )
                print("Webhook server: configured")
                return
            if args.line_command == "service":
                manager = LineWebhookSystemdService(env_file=args.config)
                if args.line_service_command == "install":
                    print(f"Installed: {manager.install()}")
                elif args.line_service_command == "status":
                    print(manager.status())
                elif args.line_service_command == "enable":
                    manager.enable()
                    print("LINE webhook service: enabled")
                elif args.line_service_command == "disable":
                    manager.disable()
                    print("LINE webhook service: disabled")
                elif args.line_service_command == "restart":
                    manager.restart()
                    print("LINE webhook service: restarted")
                else:
                    manager.uninstall()
                    print("LINE webhook service: uninstalled")
                return
            config = load_config(args.config)
            client = LineMessagingClient(config.channel_access_token)
            if args.line_command == "test-message":
                result = client.push(
                    config.allowed_user_id,
                    text_message("AgentLedger LINE integration test"),
                )
                if not result.success:
                    raise RuntimeError(result.error_message or "LINE test message failed")
                print("LINE test message: sent")
                return
            with MailStateStore(args.state_db) as store:
                approvals = ApprovalService(store, output_dir=args.output_dir)
                calendar_service = None
                if args.line_command == "webhook":
                    calendar_service = _calendar_execution_service(store, args)
                service = LineApprovalService(
                    store, approvals, client,
                    channel_secret=config.channel_secret,
                    allowed_user_id=config.allowed_user_id,
                    calendar_service=calendar_service,
                    calendar_id=args.google_calendar_id,
                )
                if args.line_command == "notify":
                    result = service.notify(args.approval_id)
                    if not result.success:
                        raise RuntimeError(
                            result.error_message or "LINE notification failed"
                        )
                    print(f"LINE notification: sent ({args.approval_id})")
                else:
                    print(f"LINE webhook: http://{args.host}:{args.port}")
                    serve_webhook(service, host=args.host, port=args.port)
            return
        if args.command == "approvals":
            with MailStateStore(args.state_db) as store:
                approval_service = ApprovalService(
                    store, output_dir=args.output_dir
                )
                if args.approval_command == "list":
                    status = None if args.all else (args.status or "awaiting_approval")
                    records = approval_service.list(status=status, limit=args.limit)
                    print("Approval ID\tDate\tTime\tTitle\tStatus")
                    for record in records:
                        print(
                            f"{record.approval_id}\t{record.date or '-'}\t"
                            f"{record.start or '-'}\t{record.title}\t{record.status}"
                        )
                elif args.approval_command == "show":
                    record = approval_service.get(args.approval_id)
                    message_id = record.source_message_id or "-"
                    safe_message_id = (
                        message_id if len(message_id) <= 12
                        else f"{message_id[:6]}…{message_id[-4:]}"
                    )
                    print(f"Approval ID: {record.approval_id}")
                    print(f"Title: {record.title}")
                    print(f"Date/time: {record.date or '-'} {record.start or '-'}")
                    print(f"Duration: {record.duration_minutes or '-'}")
                    print(f"Location: {record.location or '-'}")
                    print(f"Candidate type: {record.candidate_type}")
                    print(f"Decision summary: {record.classification_summary or '-'}")
                    print(f"Source provider: {record.source_provider or '-'}")
                    print(f"Source message ID: {safe_message_id}")
                    print(f"Created at: {record.created_at.isoformat()}")
                    print(f"Expires at: {record.expires_at.isoformat() if record.expires_at else '-'}")
                    print(f"Status: {record.status}")
                    print(
                        "Execution started at: "
                        f"{record.execution_started_at.isoformat() if record.execution_started_at else '-'}"
                    )
                    print(
                        "Executed at: "
                        f"{record.executed_at.isoformat() if record.executed_at else '-'}"
                    )
                    print(f"Calendar event ID: {record.calendar_event_id or '-'}")
                    print(f"Last execution error: {record.last_execution_error or '-'}")
                    print(f"Retry count: {record.retry_count}")
                    print(f"Source JSONL: {record.source_jsonl_path}")
                elif args.approval_command == "summary":
                    summary = approval_service.summary()
                    for status in sorted(APPROVAL_STATUSES):
                        print(f"{status.replace('_', ' ').title()}: {summary[status]}")
                    print(f"Oldest pending: {summary['oldest_pending'] or '-'}")
                    print(f"Newest pending: {summary['newest_pending'] or '-'}")
                elif args.approval_command == "approve":
                    record = approval_service.approve(
                        args.approval_id, args.actor, args.reason
                    )
                    print(f"Approved: {record.approval_id}")
                    print(f"Output: {record.outcome_jsonl_path}")
                    if args.execute:
                        result = _calendar_execution_service(store, args).execute(
                            record.approval_id,
                            provider=args.calendar_provider,
                            calendar_id=args.google_calendar_id,
                        )
                        print(
                            f"Calendar execution: "
                            f"{'created' if result.success else 'failed'}"
                        )
                        if not result.success:
                            raise RuntimeError("Google Calendar execution failed")
                elif args.approval_command == "reject":
                    record = approval_service.reject(
                        args.approval_id, args.actor, args.reason
                    )
                    print(f"Rejected: {record.approval_id}")
                    print(f"Output: {record.outcome_jsonl_path}")
                elif args.approval_command == "expire":
                    print(f"Expired: {approval_service.expire()}")
                elif args.approval_command == "recover":
                    recovered = _calendar_execution_service(
                        store, args
                    ).recover_stale(
                        provider=args.calendar_provider,
                        calendar_id=args.google_calendar_id,
                        stale_after_seconds=args.stale_after_seconds,
                        limit=args.limit,
                    )
                    print(f"Recovery candidates: {len(recovered)}")
                    for item in recovered:
                        print(f"{item.approval_id}: {item.status}")
                    print(
                        "Reconciled: "
                        f"{sum(item.status == 'reconciled' for item in recovered)}"
                    )
                    print(
                        "Event not found: "
                        f"{sum(item.status == 'event_not_found' for item in recovered)}"
                    )
                    print(
                        "Lookup failed: "
                        f"{sum(item.status == 'lookup_failed' for item in recovered)}"
                    )
                else:
                    result = _calendar_execution_service(store, args).execute(
                        args.approval_id, provider=args.calendar_provider,
                        calendar_id=args.google_calendar_id,
                    )
                    print(f"Calendar execution: {'created' if result.success else 'failed'}")
                    print(f"External event ID: {result.external_event_id or '-'}")
                    if not result.success:
                        raise RuntimeError("Google Calendar execution failed")
            return
        if args.command == "google-calendar":
            auth = GoogleCalendarAuth(args.credentials, args.token_cache)
            if args.google_command == "status":
                status = auth.status()
                print(
                    "Credentials file: "
                    + ("found" if status["credentials_found"] else "missing")
                )
                print(
                    "Token cache: "
                    + ("found" if status["token_found"] else "missing")
                )
                print(f"Calendar ID: {args.google_calendar_id}")
                if status["credentials_found"] and status["token_found"]:
                    _check_google_event_access(
                        auth, args.google_calendar_id, interactive=False
                    )
                    print("Authentication: available")
                    print("Event access: available")
                else:
                    print("Authentication: authorization required")
                    print("Event access: unavailable")
            else:
                _check_google_event_access(
                    auth, args.google_calendar_id, interactive=True
                )
                print("Google Calendar authentication: available")
                print(f"Calendar ID: {args.google_calendar_id}")
                print("Event access: available")
            return
        if args.command == "gmail":
            auth = GmailReadOnlyAuth(args.credentials, args.token_cache)
            if args.gmail_command == "status":
                status = auth.status()
                print(
                    "Credentials file: "
                    + ("found" if status["credentials_found"] else "missing")
                )
                print(
                    "Gmail token cache: "
                    + ("found" if status["token_found"] else "missing")
                )
                if status["credentials_found"] and status["token_found"]:
                    _check_gmail_read_access(auth, interactive=False)
                    print("Authentication: available")
                    print("Gmail read-only access: available")
                else:
                    print("Authentication: authorization required")
                    print("Gmail read-only access: unavailable")
            elif args.gmail_command == "auth":
                _check_gmail_read_access(auth, interactive=True)
                print("Gmail authentication: available")
                print("Gmail read-only access: available")
            elif args.gmail_command == "list":
                service = auth.build_service(interactive=False)
                summaries = GmailReadOnlyClient(service).list_messages(
                    limit=args.limit
                )
                print("Message ID\tReceived at\tFrom\tSubject")
                for message in summaries:
                    print(
                        f"{message.message_id}\t{message.received_at}\t"
                        f"{message.sender}\t{message.subject}"
                    )
            else:
                service = auth.build_service(interactive=False)
                message = GmailReadOnlyClient(service).get_message(args.message_id)
                client = OllamaClient(
                    base_url=args.ollama_base_url,
                    model=args.ollama_model,
                    timeout_seconds=args.ollama_timeout_seconds,
                    thinking=False,
                    allow_remote=args.allow_remote_ollama,
                )
                analyzer = HybridMailAnalyzer(
                    LLMCalendarClassifier(
                        client, max_body_chars=args.llm_body_max_chars
                    ),
                    base_year=args.base_year,
                    timezone=args.timezone,
                    mode="llm-first",
                    confidence_threshold=args.llm_confidence_threshold,
                    require_llm=True,
                )
                analysis_message = replace(
                    message,
                    body_text=without_transport_headers(message.body_text),
                )
                importance = RuleBasedImportanceClassifier().classify(
                    analysis_message
                )
                candidate = RuleBasedCalendarExtractor(
                    base_year=args.base_year, timezone=args.timezone
                ).extract(analysis_message, importance)
                analysis = analyzer.analyze(
                    analysis_message, importance, candidate
                )
                normalized = analysis.final_candidate
                print(f"Message ID: {message.message_id}")
                print(f"Final classification: {analysis.final_classification}")
                print(f"Candidate allowed: {'yes' if normalized else 'no'}")
                print(f"Confidence: {analysis.confidence:.2f}")
                if normalized:
                    print(f"Candidate type: {normalized.candidate_type}")
                    print(f"Title: {normalized.title}")
                    print(f"Date: {normalized.date or 'not recorded'}")
                    print(f"Start: {normalized.start or 'not recorded'}")
                    print(f"End: {normalized.end or 'not recorded'}")
                    print(
                        "Duration minutes: "
                        + (
                            str(normalized.duration_minutes)
                            if normalized.duration_minutes is not None
                            else "not recorded"
                        )
                    )
                    print(f"Timezone: {normalized.timezone}")
                    print(f"Location: {normalized.location or 'not recorded'}")
                print(
                    "Validation issues: "
                    + (", ".join(analysis.validation_issues) or "none")
                )
                if args.debug_body:
                    _print_gmail_debug_body(message)
                if args.debug_grounding:
                    proposed_date = (
                        analysis.llm_result_summary or {}
                    ).get("date")
                    debug = analyzer.date_grounding_debug(
                        proposed_date, message
                    )
                    print(f"Received at: {debug['received_at']}")
                    print(f"Timezone: {debug['timezone']}")
                    print(
                        "Local received at: "
                        f"{debug['local_received_at'] or 'unavailable'}"
                    )
                    print(
                        "LLM proposed date: "
                        f"{debug['proposed_date'] or 'not recorded'}"
                    )
                    print(f"LLM body length: {debug['llm_body_length']}")
                    print(
                        "Validator body length: "
                        f"{debug['validator_body_length']}"
                    )
                    print(f"LLM body hash: {debug['llm_body_hash']}")
                    print(
                        "Validator body hash: "
                        f"{debug['validator_body_hash']}"
                    )
                    print(
                        f"Same body: {'yes' if debug['same_body'] else 'no'}"
                    )
                    mime = message.metadata.get("gmail_mime", {})
                    print(f"MIME type: {mime.get('mime_type', 'unknown')}")
                    print(
                        "Plain text parts: "
                        f"{mime.get('plain_text_parts', 0)}"
                    )
                    print(f"HTML parts: {mime.get('html_parts', 0)}")
                    print(
                        "Selected part type: "
                        f"{mime.get('selected_part_type', 'none')}"
                    )
                    print(
                        "Selected body length: "
                        f"{mime.get('selected_body_length', 0)}"
                    )
                    print(
                        "Selected charset: "
                        f"{mime.get('selected_charset', 'none')}"
                    )
                    print(
                        "Charset source: "
                        f"{mime.get('charset_source', 'unavailable')}"
                    )
                    print(
                        "Decode errors: "
                        + ("yes" if mime.get("decode_errors") else "no")
                    )
                    print(
                        "Plain contains date: "
                        + ("yes" if mime.get("plain_contains_date") else "no")
                    )
                    print(
                        "HTML contains date: "
                        + ("yes" if mime.get("html_contains_date") else "no")
                    )
                    print(
                        "Plain contains time: "
                        + ("yes" if mime.get("plain_contains_time") else "no")
                    )
                    print(
                        "HTML contains time: "
                        + ("yes" if mime.get("html_contains_time") else "no")
                    )
                    print(
                        "Plain visible length: "
                        f"{mime.get('plain_visible_length', 0)}"
                    )
                    print(
                        "HTML visible length: "
                        f"{mime.get('html_visible_length', 0)}"
                    )
                    print(
                        "Selection reason: "
                        f"{mime.get('selection_reason', 'unavailable')}"
                    )
                    print("Plain:")
                    print(
                        '- Contains "8月": '
                        + ("yes" if mime.get("plain_contains_8_month") else "no")
                    )
                    print(
                        '- Contains "10日": '
                        + ("yes" if mime.get("plain_contains_10_day") else "no")
                    )
                    print(
                        '- Contains exact "8月10日": '
                        + (
                            "yes"
                            if mime.get("plain_contains_exact_august_10") else "no"
                        )
                    )
                    print(
                        '- NFKC normalized contains "8月10日": '
                        + (
                            "yes"
                            if mime.get("plain_nfkc_contains_exact_august_10")
                            else "no"
                        )
                    )
                    print("HTML text:")
                    print(
                        '- Contains "8月": '
                        + ("yes" if mime.get("html_contains_8_month") else "no")
                    )
                    print(
                        '- Contains "10日": '
                        + ("yes" if mime.get("html_contains_10_day") else "no")
                    )
                    print(
                        '- Contains exact "8月10日": '
                        + (
                            "yes"
                            if mime.get("html_contains_exact_august_10") else "no"
                        )
                    )
                    print(
                        '- NFKC normalized contains "8月10日": '
                        + (
                            "yes"
                            if mime.get("html_nfkc_contains_exact_august_10")
                            else "no"
                        )
                    )
                    print(
                        "Canonical NFKC date detection: "
                        + ("yes" if debug["nfkc_date_detection"] else "no")
                    )
                    print(
                        "Canonical whitespace-normalized date detection: "
                        + (
                            "yes"
                            if debug["whitespace_normalized_date_detection"]
                            else "no"
                        )
                    )
                    print(
                        "Contains explicit Japanese date: "
                        + (
                            "yes"
                            if debug["contains_explicit_japanese_date"]
                            else "no"
                        )
                    )
                    print(
                        "Contains time expression: "
                        + (
                            "yes"
                            if debug["contains_time_expression"] else "no"
                        )
                    )
                    print("Groundable fields used by LLM:")
                    for field_name in debug["groundable_fields"]:
                        print(f"- {field_name}")
                    print(
                        "Subject has date-like expression: "
                        + (
                            "yes"
                            if debug["subject_has_date_like_expression"]
                            else "no"
                        )
                    )
                    subject_expressions = debug["subject_expressions"]
                    if subject_expressions:
                        for item in subject_expressions:
                            print(
                                "Subject date expression: "
                                f"{item['expression']}"
                            )
                            print(
                                "Subject resolved date: "
                                f"{item['resolved_date'] or 'unresolved'}"
                            )
                    else:
                        print("Subject date expression: none")
                        print("Subject resolved date: unresolved")
                    expressions = debug["expressions"]
                    if expressions:
                        for item in expressions:
                            print(
                                "Detected date expression: "
                                f"{item['expression']}"
                            )
                            print(
                                "Resolved date: "
                                f"{item['resolved_date'] or 'unresolved'}"
                            )
                    else:
                        print("Detected date expression: none")
                        print("Resolved date: unresolved")
                    tokens = debug.get("date_like_tokens", [])
                    print("Date-like tokens:")
                    if tokens:
                        for token in tokens:
                            print(f'- "{token}"')
                    else:
                        print("- none")
                    print(
                        f"Grounded: {'yes' if debug['grounded'] else 'no'}"
                    )
                    print(f"Reason: {debug['reason'] or 'none'}")
            return
        if args.command == "run-scheduled":
            if not 1 <= args.max_messages <= 100:
                raise ValueError("max_messages must be between 1 and 100")
            client_id = args.client_id or os.environ.get("AGENTLEDGER_MICROSOFT_CLIENT_ID")
            if not client_id:
                raise ValueError("Microsoft client ID is required via environment")
            state_store = MailStateStore(args.state_db)
            state_store.ensure_bootstrap(
                timezone_name=args.timezone, start_from=args.start_from,
                reset=args.reset_start_from,
            )
            cursor = state_store.cursor(
                overlap_minutes=args.poll_overlap_minutes, provider="outlook"
            )
            client = OllamaClient(
                base_url=args.ollama_base_url, model=args.ollama_model,
                timeout_seconds=args.ollama_timeout_seconds, thinking=False,
            )
            check = client.check_model()
            if not check.model_available:
                raise OllamaError(f"Ollama model is not installed: {args.ollama_model}")
            analyzer = HybridMailAnalyzer(
                LLMCalendarClassifier(client), base_year=args.base_year,
                timezone=args.timezone, mode="llm-first",
            )
            config = OutlookProviderConfig(
                client_id=client_id, authority=args.authority, scopes=["Mail.Read"],
                token_cache_path=args.token_cache, folder=args.folder,
                max_messages=args.max_messages, unread_only=False,
                received_after=cursor["next_fetch_from"], include_body=True,
            )
            authenticator = MicrosoftAuthenticator(
                client_id=config.client_id, authority=config.authority,
                scopes=config.scopes, token_cache_path=config.token_cache_path,
            )
            provider = OutlookProvider(config, authenticator=authenticator)
            scheduled_providers = {"outlook": provider}
            if args.gmail_enabled:
                if not 1 <= args.gmail_max_messages <= 100:
                    raise ValueError(
                        "gmail_max_messages must be between 1 and 100"
                    )
                gmail_cursor = state_store.cursor(
                    overlap_minutes=args.poll_overlap_minutes,
                    provider="gmail",
                )
                gmail_auth = GmailReadOnlyAuth(
                    args.gmail_credentials, args.gmail_token_cache
                )
                scheduled_providers["gmail"] = ScheduledGmailProvider(
                    gmail_auth,
                    GmailProviderConfig(
                        max_messages=args.gmail_max_messages,
                        received_after=gmail_cursor["next_fetch_from"],
                    ),
                )
            orchestrator = MailCalendarOrchestrator(
                base_year=args.base_year, timezone=args.timezone,
                analyzer=analyzer, state_store=state_store,
                analysis_mode="llm-first", model_name=args.ollama_model,
            )
            scheduled_result = run_scheduled_batch(
                providers=scheduled_providers, orchestrator=orchestrator,
                state_store=state_store, output_dir=args.output_dir,
                overlap_minutes=args.poll_overlap_minutes,
                generate_explorer=args.generate_explorer,
                now=datetime.now(ZoneInfo(args.timezone)),
            )
            result = scheduled_result.orchestration
            print("Mode: scheduled read-only Outlook / pending local proposals")
            print(f"Fetch from: {scheduled_result.fetch_from.isoformat()}")
            for name, provider_result in (
                scheduled_result.provider_results or {}
            ).items():
                line = (
                    f"Provider {name}: {provider_result['status']} "
                    f"(fetched={provider_result['fetched_messages']})"
                )
                if provider_result.get("error_type"):
                    line += f" error={provider_result['error_type']}"
                print(line)
            if scheduled_result.html_path:
                print(f"Explorer: {scheduled_result.html_path}")
            provider = None
        else:
            provider = None
            analyzer = None
        if args.command != "run-scheduled" and args.analysis_mode != "rule-only":
            client = OllamaClient(
                base_url=args.ollama_base_url,
                model=args.ollama_model,
                timeout_seconds=args.ollama_timeout_seconds,
                keep_alive=args.ollama_keep_alive,
                temperature=args.ollama_temperature,
                thinking=args.ollama_thinking,
                allow_remote=args.allow_remote_ollama,
            )
            analyzer = HybridMailAnalyzer(
                LLMCalendarClassifier(client, max_body_chars=args.llm_body_max_chars),
                base_year=args.base_year,
                timezone=args.timezone,
                mode=args.analysis_mode,
                confidence_threshold=args.llm_confidence_threshold,
                require_llm=args.require_llm,
            )
        elif args.command != "run-scheduled":
            analyzer = HybridMailAnalyzer(
                None,
                base_year=args.base_year,
                timezone=args.timezone,
                mode="rule-only",
            )
        if args.command != "run-scheduled":
            state_store = None if args.no_state else MailStateStore(args.state_db)
            orchestrator = MailCalendarOrchestrator(
            base_year=args.base_year, timezone=args.timezone, analyzer=analyzer,
            state_store=state_store, analysis_mode=args.analysis_mode,
            model_name=args.ollama_model if args.analysis_mode != "rule-only" else None,
            )
        if args.command == "process":
            result = orchestrator.process(
                args.input,
                args.output,
                requires_approval=args.requires_approval,
                analysis_only=args.analysis_only,
                reprocess=args.reprocess,
                retry_failed=args.retry_failed,
            )
        elif args.command == "outlook":
            client_id = args.client_id or os.environ.get(
                "AGENTLEDGER_MICROSOFT_CLIENT_ID"
            )
            if not client_id:
                raise ValueError(
                    "Microsoft client ID is required via --client-id or "
                    "AGENTLEDGER_MICROSOFT_CLIENT_ID"
                )
            if client_id.startswith("<") and client_id.endswith(">"):
                raise ValueError(
                    "replace the AGENTLEDGER_MICROSOFT_CLIENT_ID placeholder "
                    "in .env with the registered Application Client ID"
                )
            received_after = (
                datetime.fromisoformat(args.received_after)
                if args.received_after
                else None
            )
            config = OutlookProviderConfig(
                client_id=client_id,
                authority=args.authority,
                scopes=["Mail.Read"],
                token_cache_path=args.token_cache,
                folder=args.folder,
                max_messages=args.max_messages,
                unread_only=args.unread_only,
                received_after=received_after,
                include_body=args.include_body,
                body_max_chars=args.body_max_chars,
                request_timeout_seconds=args.request_timeout_seconds,
            )
            authenticator = MicrosoftAuthenticator(
                client_id=config.client_id,
                authority=config.authority,
                scopes=config.scopes,
                token_cache_path=config.token_cache_path,
            )
            provider = OutlookProvider(
                config,
                authenticator=authenticator,
            )
            result = orchestrator.process_provider(
                provider,
                args.output,
                requires_approval=args.requires_approval,
                analysis_only=args.analysis_only,
                reprocess=args.reprocess,
                retry_failed=args.retry_failed,
            )
    except (OSError, RuntimeError, ValueError, OllamaError) as exc:
        if state_store is not None:
            state_store.close()
        parser.error(str(exc))
    if provider is not None:
        print(
            "Mode: read-only Outlook / local calendar proposal\n"
            "No mailbox or calendar changes will be made.\n"
            f"Authenticated account: "
            f"{mask_account(provider.authenticated_account)}\n"
            f"Fetched messages: {result.fetched_messages}"
        )
    if args.command == "run-scheduled" and result.state_db:
        _notify_pending_line_best_effort(result.state_db)
    if result.new_messages == 0:
        print("No new messages to process.")
    print(
        f"New messages: {result.new_messages}\n"
        f"Skipped already processed: {result.skipped_messages}\n"
        f"Processed messages: {result.processed_messages}\n"
        f"Important messages: {result.important_messages}\n"
        f"Ignored messages: {result.ignored_messages}\n"
        f"Candidates: {result.candidates}\n"
        f"Calendar candidates: {result.calendar_candidate_messages}\n"
        f"Ready for calendar: {result.ready_candidates}\n"
        f"Clarification required: {result.clarification_required}\n"
        f"Unsupported: {result.unsupported_candidates}\n"
        f"Calendar proposals: {result.calendar_proposals}\n"
        f"Pending calendar actions: {result.pending_calendar_actions}\n"
        f"Confirmed calendar actions: {result.confirmed_calendar_actions}\n"
        f"Generated events: {result.generated_events}\n"
        f"Rule-only decisions: {result.rule_only_decisions}\n"
        f"LLM-assisted decisions: {result.llm_assisted_decisions}\n"
        f"Informational: {result.informational_messages}\n"
        f"Promotions: {result.promotion_messages}\n"
        f"Security notifications: {result.security_notifications}\n"
        f"Invalid: {result.invalid_messages}\n"
        f"Retryable failures: {result.retryable_failures}\n"
        f"Permanent failures: {result.permanent_failures}\n"
        f"Approvals created: {result.approvals_created}\n"
        f"Run ID: {result.run_id or '-'}\n"
        f"State DB: {result.state_db or 'disabled'}\n"
        f"Mode: {'analysis only' if result.analysis_only else getattr(args, 'analysis_mode', 'llm-first')}\n"
        f"Output: {result.output_path}"
    )
    if state_store is not None:
        state_store.close()


if __name__ == "__main__":
    main()
