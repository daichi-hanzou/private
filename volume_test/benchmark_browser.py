from __future__ import annotations

import argparse
import html
import json
import os
import re
import selectors
import signal
import subprocess
import tempfile
import time
from pathlib import Path
from typing import Any
from urllib.parse import quote

from volume_test.reporting import write_results

DEFAULT_CHROME = Path(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
)


def _benchmark_url(path: Path) -> str:
    return f"file://{quote(str(path.resolve()))}?agentledgerBenchmark=1"


def benchmark_html(
    chrome: Path,
    path: Path,
    *,
    timeout: int,
) -> dict[str, Any]:
    with tempfile.TemporaryDirectory(prefix="agentledger-chrome-") as profile:
        command = [
            str(chrome),
            "--headless=new",
            "--disable-gpu",
            "--no-sandbox",
            "--disable-background-networking",
            "--disable-component-update",
            "--disable-default-apps",
            "--disable-sync",
            "--no-first-run",
            "--allow-file-access-from-files",
            "--enable-precise-memory-info",
            f"--user-data-dir={profile}",
            "--virtual-time-budget=120000",
            "--dump-dom",
            _benchmark_url(path),
        ]
        output = _run_dump_dom(
            command,
            timeout=timeout,
        )
    match = re.search(
        r'<pre id="agentledger-benchmark-results">(.*?)</pre>',
        output,
        re.DOTALL,
    )
    if not match:
        raise RuntimeError(
            "benchmark result was not emitted "
            f"(dump={len(output)} bytes, rows={output.count('<tr')}, "
            f"action_list={'Action List' in output})"
        )
    return json.loads(html.unescape(match.group(1)))


def _run_dump_dom(command: list[str], *, timeout: int) -> str:
    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )
    assert process.stdout is not None
    selector = selectors.DefaultSelector()
    selector.register(process.stdout, selectors.EVENT_READ)
    chunks = bytearray()
    deadline = time.monotonic() + timeout
    try:
        while time.monotonic() < deadline:
            for key, _ in selector.select(timeout=0.25):
                chunk = os.read(key.fileobj.fileno(), 1024 * 1024)
                if not chunk:
                    break
                chunks.extend(chunk)
                if b"</html>" in chunks:
                    return chunks.decode("utf-8", errors="replace")
            if process.poll() is not None:
                remainder = process.stdout.read()
                chunks.extend(remainder)
                return chunks.decode("utf-8", errors="replace")
        raise TimeoutError(f"Chrome did not emit HTML within {timeout}s")
    finally:
        selector.close()
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)


def _maximum(values: list[Any], key: str) -> float | None:
    numbers = [
        float(value[key])
        for value in values
        if value and value.get(key) is not None
    ]
    return max(numbers) if numbers else None


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path("volume_test"))
    parser.add_argument("--chrome", type=Path, default=DEFAULT_CHROME)
    parser.add_argument("--timeout", type=int, default=180)
    parser.add_argument("--actions", type=int, nargs="+")
    args = parser.parse_args()
    results_path = args.root / "results" / "benchmark_results.json"
    rows = json.loads(results_path.read_text(encoding="utf-8"))
    for row in rows:
        if args.actions and row["actions"] not in args.actions:
            continue
        try:
            metrics = benchmark_html(
                args.chrome,
                Path(row["html_path"]),
                timeout=args.timeout,
            )
            row.update(
                {
                    "initial_render_ms": metrics.get("initialRenderMs"),
                    "search_max_ms": max(
                        [
                            *metrics["searches"].values(),
                            *metrics["incrementalSearches"].values(),
                        ]
                    ),
                    "incremental_search_max_ms": max(
                        metrics["incrementalSearches"].values()
                    ),
                    "column_filter_ms": metrics.get("compositeFilterMs"),
                    "detail_switch_max_ms": _maximum(
                        metrics["detailSwitches"],
                        "detailMs",
                    ),
                    "sequence_render_max_ms": _maximum(
                        metrics["detailSwitches"],
                        "sequenceMs",
                    ),
                    "dom_node_count": metrics.get("domNodeCount"),
                    "table_row_count": metrics.get("tableRowCount"),
                    "js_heap_bytes": metrics.get("jsHeapBytes"),
                    "browser_error": None,
                }
            )
            print(
                f"{row['actions']:>6} actions: "
                f"initial {row['initial_render_ms']:.1f} ms, "
                f"search max {row['search_max_ms']:.1f} ms"
            )
        except Exception as exc:
            row["browser_error"] = str(exc)
            print(f"{row['actions']:>6} actions: ERROR {exc}")
    write_results(rows, args.root / "results")


if __name__ == "__main__":
    main()
