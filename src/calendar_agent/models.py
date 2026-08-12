from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(frozen=True)
class CalendarRequest:
    title: str
    requested_date: str
    duration_minutes: int | None
    available_slots: list[str] = field(default_factory=list)
    preferred_period: str | None = None
    requires_approval: bool = True

    def __post_init__(self) -> None:
        if not self.title.strip():
            raise ValueError("title must not be empty")
        if self.duration_minutes is not None and self.duration_minutes <= 0:
            raise ValueError("duration_minutes must be positive")


@dataclass(frozen=True)
class CalendarResult:
    event_id: str
    title: str
    start: str
    duration_minutes: int | None
