from __future__ import annotations

import argparse
from datetime import datetime
from pathlib import Path

from .service import MailCalendarOrchestrator


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="mail-calendar-orchestrator")
    commands = parser.add_subparsers(dest="command", required=True)
    process = commands.add_parser(
        "process",
        help="Process local email and propose safe calendar candidates.",
    )
    process.add_argument("--input", required=True, type=Path)
    process.add_argument("--output", required=True, type=Path)
    process.add_argument("--base-year", type=int, default=datetime.now().year)
    process.add_argument("--timezone", default="Asia/Tokyo")
    approval = process.add_mutually_exclusive_group()
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
    process.set_defaults(requires_approval=True)
    return parser


def main() -> None:
    parser = _parser()
    args = parser.parse_args()
    try:
        result = MailCalendarOrchestrator(
            base_year=args.base_year,
            timezone=args.timezone,
        ).process(
            args.input,
            args.output,
            requires_approval=args.requires_approval,
        )
    except (OSError, ValueError) as exc:
        parser.error(str(exc))
    print(
        f"Processed messages: {result.processed_messages}\n"
        f"Important messages: {result.important_messages}\n"
        f"Ignored messages: {result.ignored_messages}\n"
        f"Candidates: {result.candidates}\n"
        f"Ready for calendar: {result.ready_candidates}\n"
        f"Clarification required: {result.clarification_required}\n"
        f"Unsupported: {result.unsupported_candidates}\n"
        f"Calendar proposals: {result.calendar_proposals}\n"
        f"Pending calendar actions: {result.pending_calendar_actions}\n"
        f"Confirmed calendar actions: {result.confirmed_calendar_actions}\n"
        f"Generated events: {result.generated_events}\n"
        f"Output: {result.output_path}"
    )


if __name__ == "__main__":
    main()
