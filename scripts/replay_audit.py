from __future__ import annotations

import argparse
from pathlib import Path

from circular_coffee.audit.replay import load_events, render_events


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Replay a CoffeeBench audit_events.jsonl file."
    )
    parser.add_argument("audit_log", type=Path)
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Include raw model output in the replay.",
    )
    args = parser.parse_args()
    if not args.audit_log.is_file():
        parser.error(
            f"audit log file not found: {args.audit_log}. "
            "Run an experiment with the audit-enabled code first."
        )
    print(render_events(load_events(args.audit_log), debug=args.debug))


if __name__ == "__main__":
    main()
