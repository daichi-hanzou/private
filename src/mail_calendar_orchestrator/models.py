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
    rule_only_decisions: int = 0
    llm_assisted_decisions: int = 0
    analysis_only: bool = False
    informational_messages: int = 0
    promotion_messages: int = 0
    security_notifications: int = 0
    invalid_messages: int = 0
    calendar_candidate_messages: int = 0
    fetched_messages: int = 0
    new_messages: int = 0
    skipped_messages: int = 0
    retryable_failures: int = 0
    permanent_failures: int = 0
    run_id: str | None = None
    state_db: Path | None = None
