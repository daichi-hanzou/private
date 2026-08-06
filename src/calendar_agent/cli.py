from __future__ import annotations

import argparse
from pathlib import Path

from .agent import CalendarAgent
from .audit import AuditFileError, read_complete_jsonl, write_jsonl
from .models import CalendarRequest


def _add_propose_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--title", required=True)
    parser.add_argument("--date", required=True, dest="requested_date")
    parser.add_argument("--preferred-period")
    parser.add_argument("--duration", required=True, type=int)
    parser.add_argument("--slots", nargs="*", default=[])
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
    parser.add_argument("--output", required=True, type=Path)


def _add_resolution_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--action-id", required=True)
    parser.add_argument("--actor", required=True)
    parser.add_argument("--reason", required=True)
    parser.add_argument("--output", required=True, type=Path)


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="calendar-agent",
        description="Run the deterministic local Calendar Agent demo.",
    )
    commands = parser.add_subparsers(dest="command", required=True)
    propose = commands.add_parser(
        "propose",
        help="Create a calendar proposal audit log.",
    )
    _add_propose_arguments(propose)
    for name in ("approve", "reject"):
        resolution = commands.add_parser(
            name,
            help=f"{name.title()} a pending calendar action.",
        )
        _add_resolution_arguments(resolution)
    return parser


def _propose(args: argparse.Namespace) -> tuple[list[dict], Path]:
    request = CalendarRequest(
        title=args.title,
        requested_date=args.requested_date,
        preferred_period=args.preferred_period,
        duration_minutes=args.duration,
        available_slots=args.slots,
        requires_approval=args.requires_approval,
    )
    events = CalendarAgent().propose(request)
    return events, write_jsonl(args.output, events)


def _resolve(args: argparse.Namespace) -> tuple[list[dict], Path]:
    if args.input.resolve() == args.output.resolve():
        raise ValueError("input and output must be different files")
    events = read_complete_jsonl(args.input)
    resolved = CalendarAgent().resolve(
        events,
        action_id=args.action_id,
        resolution=args.command,
        actor=args.actor,
        reason=args.reason,
    )
    return resolved, write_jsonl(args.output, resolved)


def main() -> None:
    parser = _parser()
    args = parser.parse_args()
    try:
        if args.command == "propose":
            events, output = _propose(args)
        else:
            events, output = _resolve(args)
    except (AuditFileError, OSError, ValueError) as exc:
        parser.error(str(exc))
    print(f"Generated: {len(events)} events\nOutput: {output}")


if __name__ == "__main__":
    main()
