from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


@dataclass(frozen=True)
class OrchestrationResult:
    processed_messages: int
    important_messages: int
    ignored_messages: int
    candidates: int
    ready_candidates: int
    clarification_required: int
    unsupported_candidates: int
    calendar_proposals: int
    pending_calendar_actions: int
    confirmed_calendar_actions: int
    generated_events: int
    output_path: Path
    events: list[dict[str, Any]] = field(repr=False)
