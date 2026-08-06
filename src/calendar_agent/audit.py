from __future__ import annotations

import json
from collections.abc import Iterable
from pathlib import Path
from typing import Any

from agentledger.ingestion import read_jsonl


class AuditFileError(ValueError):
    """Raised when an audit file cannot be safely resolved."""


def write_jsonl(
    path: str | Path,
    events: Iterable[dict[str, Any]],
) -> Path:
    output = Path(path)
    output.parent.mkdir(parents=True, exist_ok=True)
    lines = [json.dumps(event, ensure_ascii=False) for event in events]
    output.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return output


def read_complete_jsonl(path: str | Path) -> list[dict[str, Any]]:
    ingestion = read_jsonl(path)
    if ingestion.issues:
        details = "; ".join(
            f"line {issue.line}: {issue.message}"
            for issue in ingestion.issues
        )
        raise AuditFileError(
            "refusing to resolve JSONL with malformed lines: " + details
        )
    return ingestion.events
