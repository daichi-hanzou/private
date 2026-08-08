from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


@dataclass(frozen=True)
class EmailMessage:
    provider: str
    message_id: str
    sender: str
    recipients: list[str]
    subject: str
    received_at: str
    body_text: str
    thread_id: str | None = None
    body_preview: str | None = None
    labels: list[str] = field(default_factory=list)
    importance_hint: str | None = None
    has_attachments: bool = False
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass(frozen=True)
class ImportanceResult:
    is_important: bool
    score: float
    reasons: list[str]
    category: str


@dataclass(frozen=True)
class CalendarCandidate:
    candidate_id: str
    source_provider: str
    source_message_id: str
    source_thread_id: str | None
    title: str
    candidate_type: str
    date: str | None
    start: str | None
    end: str | None
    duration_minutes: int | None
    timezone: str
    location: str | None
    description: str | None
    importance_score: float
    importance_reasons: list[str]
    requires_approval: bool
    clarification_required: bool
    extraction_notes: list[str]
    source_subject: str
    original_time_expression: str | None = None
    normalized_time: str | None = None
    date_rollover_days: int = 0
