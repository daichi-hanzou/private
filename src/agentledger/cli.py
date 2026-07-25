from __future__ import annotations

import argparse
from pathlib import Path

from .cases import decision_case, related_case
from .html import write_explorer
from .ingestion import read_jsonl
from .normalizer import normalize_events
from .query import AuditQuery, filter_events


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="agentledger")
    subparsers = parser.add_subparsers(dest="command", required=True)
    build = subparsers.add_parser("build", help="Build a static audit explorer")
    build.add_argument("audit_log", type=Path)
    build.add_argument("--output", type=Path, required=True)
    build.add_argument("--run-id")
    build.add_argument("--day", type=int)
    build.add_argument("--agent")
    build.add_argument("--event-type")
    build.add_argument("--action-type")
    build.add_argument("--case-type")
    build.add_argument("--case-id")
    build.add_argument("--proposal-id")
    build.add_argument("--decision-id")
    build.add_argument("--status")
    build.add_argument("--counterparty")
    build.add_argument("--search")
    build.add_argument(
        "--view",
        choices=("business", "technical"),
        default="business",
    )
    return parser


def main() -> None:
    parser = _parser()
    args = parser.parse_args()
    if not args.audit_log.is_file():
        parser.error(f"audit log not found: {args.audit_log}")
    ingestion = read_jsonl(args.audit_log)
    all_events = normalize_events(ingestion.events)
    case_id = args.case_id or args.proposal_id
    events = filter_events(
        all_events,
        AuditQuery(
            run_id=args.run_id,
            day=args.day,
            agent=args.agent,
            event_type=args.event_type,
            action_type=args.action_type,
            case_type=args.case_type if not case_id else None,
            status=args.status,
            counterparty=args.counterparty,
            keyword=args.search,
        ),
    )
    if case_id:
        events = related_case(events, case_id)
    if args.decision_id:
        events = decision_case(events, args.decision_id)
    if not events:
        parser.error("no events matched the selected filters")
    output = write_explorer(
        args.output,
        events,
        ingestion=ingestion,
        view=args.view,
        source_path=args.audit_log,
    )
    print(
        f"Loaded: {ingestion.loaded} events\n"
        f"Skipped: {ingestion.skipped} malformed lines\n"
        f"Selected: {len(events)} events\n"
        f"Output: {output}"
    )


if __name__ == "__main__":
    main()
