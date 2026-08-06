from __future__ import annotations

import argparse
from datetime import datetime
from pathlib import Path

from .audit import write_jsonl
from .local_provider import LocalMailProvider
from .service import MailToCalendarService


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="mail-to-calendar")
    commands = parser.add_subparsers(dest="command", required=True)
    process = commands.add_parser(
        "process",
        help="Process local JSON or JSONL email records.",
    )
    process.add_argument("--input", required=True, type=Path)
    process.add_argument("--output", required=True, type=Path)
    process.add_argument("--base-year", type=int, default=datetime.now().year)
    process.add_argument("--timezone", default="Asia/Tokyo")
    return parser


def main() -> None:
    parser = _parser()
    args = parser.parse_args()
    try:
        result = MailToCalendarService(
            base_year=args.base_year,
            timezone=args.timezone,
        ).process(LocalMailProvider(args.input))
        output = write_jsonl(args.output, result.events)
    except (OSError, ValueError) as exc:
        parser.error(str(exc))
    print(
        f"Processed: {result.processed} messages\n"
        f"Important: {result.important}\n"
        f"Candidates: {len(result.candidates)}\n"
        f"Clarification required: {result.clarification_required}\n"
        f"Ignored: {result.ignored}\n"
        f"Output: {output}"
    )


if __name__ == "__main__":
    main()
