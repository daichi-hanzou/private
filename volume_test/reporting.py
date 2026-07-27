from __future__ import annotations

import csv
import json
from pathlib import Path
from typing import Any


def write_results(
    rows: list[dict[str, Any]],
    results_dir: Path,
) -> None:
    results_dir.mkdir(parents=True, exist_ok=True)
    (results_dir / "benchmark_results.json").write_text(
        json.dumps(rows, indent=2),
        encoding="utf-8",
    )
    keys = [
        "actions",
        "events",
        "normalization_ms",
        "bundle_construction_ms",
        "html_serialization_ms",
        "html_write_ms",
        "total_generation_ms",
        "html_size_bytes",
        "gzip_size_bytes",
        "initial_render_ms",
        "search_max_ms",
        "incremental_search_max_ms",
        "column_filter_ms",
        "detail_switch_max_ms",
        "sequence_render_max_ms",
        "dom_node_count",
        "table_row_count",
        "js_heap_bytes",
        "browser_error",
    ]
    with (results_dir / "benchmark_results.csv").open(
        "w",
        encoding="utf-8",
        newline="",
    ) as handle:
        writer = csv.DictWriter(handle, fieldnames=keys)
        writer.writeheader()
        writer.writerows({key: row.get(key) for key in keys} for row in rows)
    write_report(rows, results_dir / "benchmark_report.md")


def _value(value: Any, digits: int = 1) -> str:
    if value is None:
        return "n/a"
    if isinstance(value, float):
        return f"{value:.{digits}f}"
    return str(value)


def write_report(rows: list[dict[str, Any]], path: Path) -> None:
    lines = [
        "# AgentLedger Volume Benchmark",
        "",
        "## Method",
        "",
        "- Synthetic logs preserve the CoffeeBench four-event Action Bundle.",
        "- Chrome headless measures synchronous filtering and DOM updates.",
        "- Sequence source insertion is measured without CDN/network latency.",
        "- Mermaid remains lazy-loaded during normal Explorer use.",
        "",
        "## Benchmark Results",
        "",
        "| Actions | Events | Generate ms | HTML MiB | Gzip MiB | "
        "Initial Render ms | Search Max ms | Filter ms | Detail Max ms |",
        "| ------: | -----: | ----------: | -------: | -------: | "
        "----------------: | ------------: | --------: | ------------: |",
    ]
    for row in rows:
        lines.append(
            f"| {row['actions']} | {row['events']} | "
            f"{_value(row.get('total_generation_ms'))} | "
            f"{row['html_size_bytes'] / 1024 / 1024:.2f} | "
            f"{row['gzip_size_bytes'] / 1024 / 1024:.2f} | "
            f"{_value(row.get('initial_render_ms'))} | "
            f"{_value(row.get('search_max_ms'))} | "
            f"{_value(row.get('column_filter_ms'))} | "
            f"{_value(row.get('detail_switch_max_ms'))} |"
        )
    largest = rows[-1]
    lines.extend(
        [
            "",
            "## Bottlenecks",
            "",
            "1. Full-table DOM rendering grows linearly with Action count.",
            "2. Embedded Observation and state snapshots dominate HTML size.",
            "3. Embedded raw data drives browser heap usage before row count "
            "becomes a responsiveness problem.",
            "",
            "## Changes Made",
            "",
            "1. Action Bundle construction uses prebuilt ID indexes.",
            "2. Row selection uses an Action ID map and does not rebuild the table.",
            "3. Search uses precomputed searchable text with input debounce.",
            "4. Rows are inserted through a DocumentFragment.",
            "5. Sequence rendering remains lazy and has an offline text fallback.",
            "",
            "## Remaining Risks",
            "",
            "1. Large reports still create one DOM row per visible Action.",
            "2. Raw snapshots remain embedded to preserve standalone audit detail.",
            "3. Mermaid is loaded from a CDN; offline mode shows source text.",
            "4. Above 10,000 Actions, raw-data separation should be evaluated "
            "before adding virtual scrolling.",
            "",
            "## Recommendation",
            "",
        ]
    )
    within_targets = all(
        (
            largest.get("initial_render_ms") is not None
            and largest["initial_render_ms"] <= 3000,
            largest.get("search_max_ms") is not None
            and largest["search_max_ms"] <= 500,
            largest.get("column_filter_ms") is not None
            and largest["column_filter_ms"] <= 500,
            largest.get("detail_switch_max_ms") is not None
            and largest["detail_switch_max_ms"] <= 200,
        )
    )
    if within_targets:
        recommendation = (
            "A. Current full-table rendering is sufficient up to "
            f"{largest['actions']:,} actions on the benchmark machine."
        )
    else:
        recommendation = (
            "B. Pagination is required above 5,000 actions; start with "
            "250 rows per page before considering virtual scrolling."
        )
    lines.append(recommendation)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
