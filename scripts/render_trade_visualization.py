from __future__ import annotations

import argparse

from pathlib import Path

from circular_coffee.visualization import (
    render_html_report,
    render_multi_seed_html_report,
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Render a static HTML visualization for a simulation run.",
    )
    parser.add_argument(
        "run_dir",
        help="Directory containing metrics.json and trades.jsonl.",
    )
    parser.add_argument(
        "--output",
        help="Output HTML path. Defaults to <run_dir>/visualization_report.html.",
    )
    parser.add_argument(
        "--title",
        help="Optional report title override.",
    )
    parser.add_argument(
        "--seeds",
        nargs="+",
        type=int,
        help="Render one comparison report for these sibling seed directories.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.seeds:
        run_path = Path(args.run_dir)
        experiment_path = run_path.parent if run_path.name.startswith("seed_") else run_path
        output_path = render_multi_seed_html_report(
            experiment_path,
            seeds=args.seeds,
            output_path=args.output,
            title=args.title,
        )
    else:
        output_path = render_html_report(
            args.run_dir,
            output_path=args.output,
            title=args.title,
        )
    print(output_path)


if __name__ == "__main__":
    main()
