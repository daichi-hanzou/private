from __future__ import annotations

import argparse
import gzip
import time
from pathlib import Path

from agentledger.bundles import build_action_bundles
from agentledger.html import render_action_bundles
from agentledger.normalizer import normalize_events

from volume_test.generate_volume_data import generate_events
from volume_test.reporting import write_results

DEFAULT_SCALES = (100, 500, 1000, 5000, 10000)


def benchmark_scale(actions: int, html_dir: Path) -> dict:
    raw_events = generate_events(actions)
    normalization_started = time.perf_counter()
    normalized = normalize_events(raw_events)
    normalization_ms = (
        time.perf_counter() - normalization_started
    ) * 1000
    bundle_started = time.perf_counter()
    bundles = build_action_bundles(normalized)
    bundle_ms = (time.perf_counter() - bundle_started) * 1000
    serialization_started = time.perf_counter()
    content = render_action_bundles(bundles)
    serialization_ms = (time.perf_counter() - serialization_started) * 1000
    output = html_dir / f"agentledger_{actions}.html"
    output.parent.mkdir(parents=True, exist_ok=True)
    write_started = time.perf_counter()
    output.write_text(content, encoding="utf-8")
    write_ms = (time.perf_counter() - write_started) * 1000
    raw_bytes = content.encode()
    return {
        "actions": actions,
        "events": len(raw_events),
        "normalization_ms": normalization_ms,
        "bundle_construction_ms": bundle_ms,
        "html_serialization_ms": serialization_ms,
        "html_write_ms": write_ms,
        "total_generation_ms": (
            normalization_ms + bundle_ms + serialization_ms + write_ms
        ),
        "html_size_bytes": len(raw_bytes),
        "gzip_size_bytes": len(gzip.compress(raw_bytes)),
        "html_path": str(output),
        "initial_render_ms": None,
        "search_max_ms": None,
        "column_filter_ms": None,
        "detail_switch_max_ms": None,
        "sequence_render_max_ms": None,
        "dom_node_count": None,
        "table_row_count": None,
        "js_heap_bytes": None,
        "browser_error": None,
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--actions",
        type=int,
        nargs="+",
        default=DEFAULT_SCALES,
    )
    parser.add_argument(
        "--root",
        type=Path,
        default=Path("volume_test"),
    )
    args = parser.parse_args()
    rows = []
    for scale in args.actions:
        row = benchmark_scale(scale, args.root / "html")
        rows.append(row)
        print(
            f"{scale:>6} actions: "
            f"{row['total_generation_ms']:.1f} ms, "
            f"{row['html_size_bytes'] / 1024 / 1024:.2f} MiB"
        )
    write_results(rows, args.root / "results")


if __name__ == "__main__":
    main()
