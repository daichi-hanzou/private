from __future__ import annotations

import argparse
import os
import re
from datetime import datetime
from pathlib import Path

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

from .service import MailCalendarOrchestrator
from .state import DEFAULT_STATE_DB, MailStateStore


_ENV_KEY = re.compile(r"[A-Za-z_][A-Za-z0-9_]*\Z")


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
    state.add_argument("--state-db", type=Path, default=DEFAULT_STATE_DB)
    state_commands = state.add_subparsers(dest="state_command", required=True)
    state_commands.add_parser("summary")
    recent = state_commands.add_parser("recent")
    recent.add_argument("--limit", type=int, default=20)
    reset = state_commands.add_parser("reset-message")
    reset.add_argument("--provider", required=True)
    reset.add_argument("--message-id", required=True)
    return parser


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
                elif args.state_command == "recent":
                    print(f"State DB: {store.path}")
                    for row in store.recent(max(1, args.limit)):
                        print("\t".join(str(row[key] or "-") for key in row.keys()))
                else:
                    removed = store.reset_message(args.provider, args.message_id)
                    if not removed:
                        raise ValueError("message state not found")
                    print(f"Reset: {args.provider}/{args.message_id}")
            return
        analyzer = None
        if args.analysis_mode != "rule-only":
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
        else:
            analyzer = HybridMailAnalyzer(
                None,
                base_year=args.base_year,
                timezone=args.timezone,
                mode="rule-only",
            )
        state_store = None if args.no_state else MailStateStore(args.state_db)
        orchestrator = MailCalendarOrchestrator(
            base_year=args.base_year, timezone=args.timezone, analyzer=analyzer,
            state_store=state_store, analysis_mode=args.analysis_mode,
            model_name=args.ollama_model if args.analysis_mode != "rule-only" else None,
        )
        provider = None
        if args.command == "process":
            result = orchestrator.process(
                args.input,
                args.output,
                requires_approval=args.requires_approval,
                analysis_only=args.analysis_only,
                reprocess=args.reprocess,
                retry_failed=args.retry_failed,
            )
        else:
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
        f"Run ID: {result.run_id or '-'}\n"
        f"State DB: {result.state_db or 'disabled'}\n"
        f"Mode: {'analysis only' if result.analysis_only else args.analysis_mode}\n"
        f"Output: {result.output_path}"
    )
    if state_store is not None:
        state_store.close()


if __name__ == "__main__":
    main()
