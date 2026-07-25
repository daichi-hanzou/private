from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from .models import IngestionIssue, IngestionResult


def read_jsonl(path: str | Path) -> IngestionResult:
    events: list[dict[str, Any]] = []
    issues: list[IngestionIssue] = []
    empty_lines = 0
    with Path(path).open(encoding="utf-8") as handle:
        for line_number, line in enumerate(handle, start=1):
            if not line.strip():
                empty_lines += 1
                continue
            try:
                value = json.loads(line)
            except json.JSONDecodeError as exc:
                issues.append(
                    IngestionIssue(line_number, f"invalid JSON: {exc.msg}")
                )
                continue
            if not isinstance(value, dict):
                issues.append(
                    IngestionIssue(line_number, "event must be a JSON object")
                )
                continue
            events.append(dict(value))
    return IngestionResult(events, issues, empty_lines)
