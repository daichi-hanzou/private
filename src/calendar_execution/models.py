from __future__ import annotations

from dataclasses import dataclass
from typing import Any


@dataclass(frozen=True)
class CalendarExecutionRequest:
    approval_id: str
    calendar_action_id: str
    candidate_id: str | None
    title: str
    start: str
    end: str
    duration_minutes: int
    timezone: str
    location: str | None
    description: str
    source_provider: str | None
    source_message_id: str | None
    calendar_id: str
    duration_source: str = "extracted"


@dataclass(frozen=True)
class CalendarExecutionResult:
    success: bool
    provider: str
    calendar_id: str
    external_event_id: str | None = None
    html_link: str | None = None
    created_at: str | None = None
    start: str | None = None
    end: str | None = None
    error_type: str | None = None
    error_message: str | None = None
    already_exists: bool = False
    raw_response_summary: dict[str, Any] | None = None
    retryable: bool = False


@dataclass(frozen=True)
class CalendarRecoveryResult:
    approval_id: str
    status: str
    external_event_id: str | None = None
    error_type: str | None = None
