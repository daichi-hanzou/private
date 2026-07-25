from __future__ import annotations

import argparse
import sys
from pathlib import Path

from circular_coffee.audit.replay import load_events
from circular_coffee.audit.sequence import (
    build_sequence_diagram,
    filter_events,
    sort_events,
    write_sequence_output,
)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate a Mermaid audit sequence diagram."
    )
    parser.add_argument("audit_log", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--day", type=int)
    parser.add_argument("--agent")
    parser.add_argument("--proposal-id")
    parser.add_argument("--decision-id")
    parser.add_argument(
        "--view",
        choices=("business", "technical"),
        default="business",
    )
    args = parser.parse_args()

    if not args.audit_log.is_file():
        parser.error(f"audit log file not found: {args.audit_log}")
    events = sort_events(
        filter_events(
            load_events(args.audit_log),
            day=args.day,
            proposal_id=args.proposal_id,
            decision_id=args.decision_id,
            agent_id=args.agent,
        )
    )
    if not events:
        parser.error("no audit events matched the selected filters")
    if len(events) > 200:
        print(
            f"WARNING: generating a diagram with {len(events)} events; "
            "consider --day or --proposal-id.",
            file=sys.stderr,
        )
    filters = {
        "day": args.day,
        "agent": args.agent,
        "proposal_id": args.proposal_id,
        "decision_id": args.decision_id,
    }
    mermaid_source = build_sequence_diagram(events, view=args.view)
    output = write_sequence_output(
        args.output,
        events=events,
        mermaid_source=mermaid_source,
        filters=filters,
        view=args.view,
    )
    print(output)


if __name__ == "__main__":
    main()
