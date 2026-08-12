from __future__ import annotations

import hashlib
import re
from datetime import datetime, timedelta

from calendar_agent.models import CalendarRequest

from .models import (
    CalendarCandidate, EmailMessage, ImportanceResult, canonical_message_id,
)
from .time_normalization import normalize_calendar_datetime


class RuleBasedCalendarExtractor:
    _ambiguous = ("今日", "明日", "来週", "来月", "どこかで")

    def __init__(self, *, base_year: int, timezone: str = "Asia/Tokyo") -> None:
        self.base_year = base_year
        self.timezone = timezone

    def extract(
        self,
        message: EmailMessage,
        importance: ImportanceResult,
    ) -> CalendarCandidate | None:
        if not importance.is_important or importance.category in {
            "promotion",
            "informational",
            "unknown",
        }:
            return None
        text = f"{message.subject}\n{message.body_text}"
        notes: list[str] = []
        ambiguous = [word for word in self._ambiguous if word in text]
        if ambiguous:
            notes.append(
                "ambiguous date expression requires clarification: "
                + ", ".join(ambiguous)
            )
        date = None
        start = None
        original_time_expression = None
        normalized_time = None
        date_rollover_days = 0
        invalid_temporal = False
        if not ambiguous:
            try:
                date = self._extract_date(text)
            except ValueError as exc:
                notes.append(str(exc))
                invalid_temporal = True
            start, original_time_expression = self._extract_time_with_expression(
                text
            )
            if start is not None:
                try:
                    normalized = normalize_calendar_datetime(
                        date,
                        start,
                        original_time_expression=original_time_expression,
                    )
                except ValueError as exc:
                    notes.append(str(exc))
                    start = None
                    invalid_temporal = True
                else:
                    date = normalized.date
                    start = normalized.time
                    normalized_time = normalized.normalized_time
                    date_rollover_days = normalized.date_rollover_days
                    if normalized.date_rollover_days:
                        notes.append(
                            "end-of-day time normalized with next-day rollover"
                        )
        if date is None and not ambiguous:
            notes.append("date was not found")
        if start is None and importance.category != "deadline":
            notes.append("start time was not found")
        if importance.category == "deadline" and start is None:
            notes.append("deadline has no explicit time; no time was inferred")
        candidate_type = {
            "deadline": "deadline",
            "task": "task",
        }.get(importance.category, "event")
        clarification = bool(ambiguous) or date is None or invalid_temporal
        if candidate_type != "deadline" and start is None:
            clarification = True
        # A duration is not inferred from a start time alone.  Calendar API
        # adapters may apply an execution-only default later.
        duration = None
        digest = hashlib.sha256(
            canonical_message_id(message.provider, message.message_id).encode()
        ).hexdigest()[:12]
        return CalendarCandidate(
            candidate_id=f"calendar-candidate-{digest}",
            source_provider=message.provider,
            source_message_id=message.message_id,
            source_thread_id=message.thread_id,
            title=self._title(message.subject, importance.category),
            candidate_type=candidate_type,
            date=date,
            start=start,
            end=self._end_time(start, duration),
            duration_minutes=duration,
            timezone=self.timezone,
            location="Zoom" if "zoom" in text.casefold() else None,
            description=f"Calendar candidate from email: {message.subject}",
            importance_score=importance.score,
            importance_reasons=list(importance.reasons),
            requires_approval=True,
            clarification_required=clarification,
            extraction_notes=notes,
            source_subject=message.subject,
            original_time_expression=(
                original_time_expression if date_rollover_days else None
            ),
            normalized_time=normalized_time,
            date_rollover_days=date_rollover_days,
        )

    def _extract_date(self, text: str) -> str | None:
        match = re.search(r"(\d{4})年(\d{1,2})月(\d{1,2})日", text)
        if match:
            return self._date(*map(int, match.groups()))
        match = re.search(r"(\d{4})-(\d{1,2})-(\d{1,2})", text)
        if match:
            return self._date(*map(int, match.groups()))
        match = re.search(r"(\d{1,2})月(\d{1,2})日", text)
        if match:
            month, day = map(int, match.groups())
            return self._date(self.base_year, month, day)
        return None

    @staticmethod
    def _date(year: int, month: int, day: int) -> str:
        try:
            return datetime(year, month, day).date().isoformat()
        except ValueError as exc:
            raise ValueError(
                f"invalid extracted date: {year}-{month}-{day}"
            ) from exc

    @staticmethod
    def _extract_time(text: str) -> str | None:
        return RuleBasedCalendarExtractor._extract_time_with_expression(text)[0]

    @staticmethod
    def _extract_time_with_expression(
        text: str,
    ) -> tuple[str | None, str | None]:
        match = re.search(r"午後\s*(\d{1,2})時(?:\s*(\d{1,2})分)?", text)
        if match:
            hour = int(match.group(1)) % 12 + 12
            minute = int(match.group(2) or 0)
            return f"{hour:02d}:{minute:02d}", match.group(0)
        match = re.search(r"午前\s*(\d{1,2})時(?:\s*(\d{1,2})分)?", text)
        if match:
            hour = int(match.group(1)) % 12
            minute = int(match.group(2) or 0)
            return f"{hour:02d}:{minute:02d}", match.group(0)
        match = re.search(r"(?<!\d)(\d{1,2}):(\d{2})(?!\d)", text)
        if match:
            return (
                f"{int(match.group(1)):02d}:{int(match.group(2)):02d}",
                match.group(0),
            )
        match = re.search(r"(?<!\d)(\d{1,2})時(?:\s*(\d{1,2})分)?", text)
        if match:
            return (
                f"{int(match.group(1)):02d}:{int(match.group(2) or 0):02d}",
                match.group(0),
            )
        return None, None

    @staticmethod
    def _end_time(start: str | None, duration: int | None) -> str | None:
        if not start or duration is None:
            return None
        value = datetime.strptime(start, "%H:%M") + timedelta(minutes=duration)
        return value.strftime("%H:%M")

    @staticmethod
    def _title(subject: str, category: str) -> str:
        if category == "meeting":
            cleaned = re.sub(
                r"\d{4}年|\d{1,2}月\d{1,2}日|\d{1,2}時(?:\d{1,2}分)?",
                "",
                subject,
            )
            cleaned = re.sub(r"\s+", " ", cleaned).strip()
            return cleaned or subject
        return subject


def to_calendar_request(candidate: CalendarCandidate) -> CalendarRequest:
    if candidate.clarification_required:
        raise ValueError("candidate requires clarification")
    if not candidate.date:
        raise ValueError("candidate date is required")
    if not candidate.start:
        raise ValueError("candidate start time is required")
    return CalendarRequest(
        title=candidate.title,
        requested_date=candidate.date,
        preferred_period=None,
        duration_minutes=candidate.duration_minutes,
        available_slots=[candidate.start],
        requires_approval=candidate.requires_approval,
    )
