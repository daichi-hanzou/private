from __future__ import annotations

import argparse
from pathlib import Path

from circular_coffee.core_metrics import build_core_metrics_from_run_dir
from circular_coffee.logging_utils import write_json


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Rebuild minimal metrics.json from authoritative simulation artifacts."
        ),
    )
    parser.add_argument(
        "run_dir",
        help="Run directory containing config, state, and JSONL event logs.",
    )
    parser.add_argument(
        "--output",
        help="Output path. Defaults to <run_dir>/metrics.json.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    run_dir = Path(args.run_dir)
    output_path = Path(args.output) if args.output else run_dir / "metrics.json"
    metrics = build_core_metrics_from_run_dir(run_dir)
    write_json(output_path, metrics)
    print(output_path)


if __name__ == "__main__":
    main()
