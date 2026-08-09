from __future__ import annotations

import re
from dataclasses import dataclass
from datetime import date, datetime, timedelta


@dataclass(frozen=True)
class NormalizedCalendarDateTime:
    date: str | None
    time: str | None
    original_time_expression: str | None = None
    normalized_time: str | None = None
    date_rollover_days: int = 0
    source_date: str | None = None
    utc_offset_seconds: int | None = None

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
    """Normalize an ISO date and calendar time, including end-of-day 24:00."""
    if time_value is None:
        return NormalizedCalendarDateTime(date_value, None)
    stripped = time_value.strip()
    source_date = None
    utc_offset_seconds = None
    if "T" in stripped:
        try:
            parsed_datetime = datetime.fromisoformat(stripped)
        except ValueError as exc:
            raise ValueError(f"invalid calendar datetime: {time_value}") from exc
        if parsed_datetime.second or parsed_datetime.microsecond:
            raise ValueError(f"invalid calendar datetime: {time_value}")
        source_date = parsed_datetime.date().isoformat()
        hour, minute = parsed_datetime.hour, parsed_datetime.minute
        offset = parsed_datetime.utcoffset()
        if offset is not None:
            utc_offset_seconds = int(offset.total_seconds())
    else:
        hour, minute = normalize_calendar_time(stripped)
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
            source_date,
            utc_offset_seconds,
        )
    if hour > 23 or minute > 59:
        raise ValueError(f"invalid calendar time: {time_value}")
    return NormalizedCalendarDateTime(
        date_value,
        f"{hour:02d}:{minute:02d}",
        source_date=source_date,
        utc_offset_seconds=utc_offset_seconds,
    )


def normalize_calendar_time(value: str) -> tuple[int, int]:
    """Return a normalized hour/minute pair for common mail time expressions."""
    stripped = value.strip()
    colon = re.fullmatch(r"(\d{1,2}):(\d{2})(?::(\d{2}))?", stripped)
    japanese = re.fullmatch(
        r"(?:(午前|午後)\s*)?(\d{1,2})時(?:(\d{1,2})分)?",
        stripped,
    )
    if colon:
        hour, minute = int(colon.group(1)), int(colon.group(2))
        seconds = colon.group(3)
        if seconds is not None and seconds != "00":
            raise ValueError(f"invalid calendar time: {value}")
    elif japanese:
        period, hour_text, minute_text = japanese.groups()
        hour = int(hour_text)
        minute = int(minute_text or 0)
        if period:
            if not 1 <= hour <= 12:
                raise ValueError(f"invalid calendar time: {value}")
            hour %= 12
            if period == "午後":
                hour += 12
    else:
        raise ValueError(f"invalid calendar time: {value}")
    if hour > 24 or minute > 59 or (hour == 24 and minute != 0):
        raise ValueError(f"invalid calendar time: {value}")
    return hour, minute
