from __future__ import annotations

import argparse
import os
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
    return parser


def main() -> None:
    parser = _parser()
    args = parser.parse_args()
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
        orchestrator = MailCalendarOrchestrator(
            base_year=args.base_year,
            timezone=args.timezone,
            analyzer=analyzer,
        )
        provider = None
        if args.command == "process":
            result = orchestrator.process(
                args.input,
                args.output,
                requires_approval=args.requires_approval,
                analysis_only=args.analysis_only,
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
            )
    except (OSError, RuntimeError, ValueError, OllamaError) as exc:
        parser.error(str(exc))
    if provider is not None:
        print(
            "Mode: read-only Outlook / local calendar proposal\n"
            "No mailbox or calendar changes will be made.\n"
            f"Authenticated account: "
            f"{mask_account(provider.authenticated_account)}\n"
            f"Fetched messages: {result.processed_messages}"
        )
    print(
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
        f"Mode: {'analysis only' if result.analysis_only else args.analysis_mode}\n"
        f"Output: {result.output_path}"
    )


if __name__ == "__main__":
    main()
