from __future__ import annotations

import re
from dataclasses import dataclass
from datetime import date, timedelta


@dataclass(frozen=True)
class NormalizedCalendarDateTime:
    date: str | None
    time: str | None
    original_time_expression: str | None = None
    normalized_time: str | None = None
    date_rollover_days: int = 0

    def audit_data(self) -> dict[str, str | int] | None:
        if not self.date_rollover_days:
            return None
        return {
            "original_time_expression": (
                self.original_time_expression or "24:00"
            ),
            "normalized_time": self.normalized_time or "00:00",
            "date_rollover_days": self.date_rollover_days,
        }


def normalize_calendar_datetime(
    date_value: str | None,
    time_value: str | None,
    *,
    original_time_expression: str | None = None,
) -> NormalizedCalendarDateTime:
    """Normalize an ISO date and HH:MM time, including end-of-day 24:00."""
    if time_value is None:
        return NormalizedCalendarDateTime(date_value, None)
    match = re.fullmatch(r"(\d{1,2}):(\d{2})", time_value.strip())
    if not match:
        raise ValueError(f"invalid calendar time: {time_value}")
    hour, minute = map(int, match.groups())
    if hour == 24:
        if minute != 0:
            raise ValueError("24-hour time only permits 24:00")
        if date_value is None:
            raise ValueError("24:00 requires a date for next-day rollover")
        try:
            normalized_date = date.fromisoformat(date_value) + timedelta(days=1)
        except ValueError as exc:
            raise ValueError(f"invalid calendar date: {date_value}") from exc
        return NormalizedCalendarDateTime(
            normalized_date.isoformat(),
            "00:00",
            original_time_expression or time_value,
            "00:00",
            1,
        )
    if hour > 23 or minute > 59:
        raise ValueError(f"invalid calendar time: {time_value}")
    return NormalizedCalendarDateTime(
        date_value,
        f"{hour:02d}:{minute:02d}",
    )
