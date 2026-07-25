from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any


@dataclass(frozen=True)
class NormalizedAuditEvent:
    event_id: str
    event_type: str
    run_id: str | None = None
    timestamp: datetime | None = None
    day: int | None = None
    actor_id: str | None = None
    actor_name: str | None = None
    target_id: str | None = None
    target_name: str | None = None
    decision_id: str | None = None
    action_id: str | None = None
    correlation_id: str | None = None
    case_type: str | None = None
    case_id: str | None = None
    action_type: str | None = None
    action_parameters: dict[str, Any] = field(default_factory=dict)
    explanation: str | None = None
    expected_outcome: str | None = None
    actual_outcome: Any = None
    status: str | None = None
    summary: str = ""
    raw_event: dict[str, Any] = field(default_factory=dict)
    source_line: int | None = None


@dataclass(frozen=True)
class IngestionIssue:
    line: int
    message: str


@dataclass(frozen=True)
class IngestionResult:
    events: list[dict[str, Any]]
    issues: list[IngestionIssue]
    empty_lines: int = 0

    @property
    def loaded(self) -> int:
        return len(self.events)

    @property
    def skipped(self) -> int:
        return len(self.issues)
