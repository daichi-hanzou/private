from __future__ import annotations

import hashlib
import json
import socket
import time
from datetime import datetime, timedelta
from typing import Any, Callable
from zoneinfo import ZoneInfo, ZoneInfoNotFoundError

from .models import CalendarExecutionRequest, CalendarExecutionResult


class GoogleCalendarAPIError(RuntimeError):
    def __init__(
        self, status: int | None, message: str,
        *, reason: str | None = None, retry_after: int | None = None,
    ) -> None:
        super().__init__(message[:300])
        self.status = status
        self.reason = reason
        self.retry_after = retry_after


class GoogleCalendarClient:
    def __init__(
        self, service: Any, *, max_retries: int = 2,
        sleep: Callable[[float], None] = time.sleep,
    ) -> None:
        if not 0 <= max_retries <= 5:
            raise ValueError("Google retry count must be between 0 and 5")
        self.service = service
        self.max_retries = max_retries
        self.sleep = sleep

    def find_by_approval(self, calendar_id: str, approval_id: str) -> dict[str, Any] | None:
        response = self._execute(lambda: self.service.events().list(
            calendarId=calendar_id,
            privateExtendedProperty=f"agentledger_approval_id={approval_id}",
            maxResults=2,
            showDeleted=False,
        ).execute())
        items = response.get("items") if isinstance(response, dict) else None
        if not isinstance(items, list):
            raise GoogleCalendarAPIError(None, "Google Calendar returned malformed event list")
        return items[0] if items else None

    def insert(self, calendar_id: str, payload: dict[str, Any]) -> dict[str, Any]:
        response = self._execute(lambda: self.service.events().insert(
            calendarId=calendar_id, body=payload, sendUpdates="none"
        ).execute())
        if not isinstance(response, dict) or not response.get("id"):
            raise GoogleCalendarAPIError(None, "Google Calendar response is missing event ID")
        return response

    def _execute(self, operation: Callable[[], Any]) -> Any:
        for attempt in range(self.max_retries + 1):
            try:
                return operation()
            except GoogleCalendarAPIError as exc:
                error = exc
            except (TimeoutError, socket.timeout, ConnectionError) as exc:
                error = GoogleCalendarAPIError(None, "Google Calendar network timeout")
            except Exception as exc:
                response = getattr(exc, "resp", None)
                status = getattr(response, "status", None)
                headers = getattr(response, "headers", {}) or {}
                reason = _error_reason(exc)
                error = GoogleCalendarAPIError(
                    status, f"Google Calendar API error ({status or 'unknown'})",
                    reason=str(reason)[:100] if reason else None,
                    retry_after=_integer(headers.get("retry-after")),
                )
            if not _retryable(error) or attempt >= self.max_retries:
                raise error
            delay = error.retry_after if error.retry_after is not None else 2**attempt
            self.sleep(max(0, min(delay, 60)))
        raise GoogleCalendarAPIError(None, "Google Calendar request failed")


class GoogleCalendarExecutor:
    provider = "google"

    def __init__(self, client: GoogleCalendarClient) -> None:
        self.client = client

    def create_event(
        self, request: CalendarExecutionRequest
    ) -> CalendarExecutionResult:
        try:
            payload = google_event_payload(request)
            existing = self.client.find_by_approval(
                request.calendar_id, request.approval_id
            )
            if existing:
                return _success(existing, request, already_exists=True)
            created = self.client.insert(request.calendar_id, payload)
            return _success(created, request, already_exists=False)
        except (GoogleCalendarAPIError, ValueError) as exc:
            if isinstance(exc, GoogleCalendarAPIError) and exc.status == 409:
                try:
                    existing = self.client.find_by_approval(
                        request.calendar_id, request.approval_id
                    )
                    if existing:
                        return _success(existing, request, already_exists=True)
                except GoogleCalendarAPIError:
                    pass
            retryable = isinstance(exc, GoogleCalendarAPIError) and _retryable(exc)
            return CalendarExecutionResult(
                success=False, provider=self.provider,
                calendar_id=request.calendar_id,
                error_type=type(exc).__name__, error_message=str(exc)[:300],
                retryable=retryable,
            )


def google_event_payload(request: CalendarExecutionRequest) -> dict[str, Any]:
    if not request.title.strip():
        raise ValueError("calendar event title is required")
    if request.duration_minutes <= 0:
        raise ValueError("calendar event duration must be positive")
    try:
        zone = ZoneInfo(request.timezone)
    except ZoneInfoNotFoundError as exc:
        raise ValueError(f"unknown calendar timezone: {request.timezone}") from exc
    start = datetime.fromisoformat(request.start)
    end = datetime.fromisoformat(request.end)
    if start.tzinfo is None:
        start = start.replace(tzinfo=zone)
    else:
        start = start.astimezone(zone)
    if end.tzinfo is None:
        end = end.replace(tzinfo=zone)
    else:
        end = end.astimezone(zone)
    if end <= start:
        raise ValueError("calendar event end must be after start")
    source_hash = hashlib.sha256(
        (request.source_message_id or "").encode("utf-8")
    ).hexdigest()[:16]
    private = {
        "agentledger_approval_id": request.approval_id,
        "agentledger_action_id": request.calendar_action_id,
        "agentledger_candidate_id": request.candidate_id or "none",
        "agentledger_source_provider": request.source_provider or "unknown",
        "agentledger_source_message_hash": source_hash,
    }
    payload = {
        "summary": request.title[:200],
        "description": request.description[:500],
        "start": {"dateTime": start.isoformat(), "timeZone": request.timezone},
        "end": {"dateTime": end.isoformat(), "timeZone": request.timezone},
        "extendedProperties": {"private": private},
    }
    if request.location:
        payload["location"] = request.location[:300]
    return payload


def execution_datetimes(
    *, date: str | None, start: str | None, end: str | None,
    duration_minutes: int | None, timezone: str,
) -> tuple[str, str, int]:
    if not start or not duration_minutes or duration_minutes <= 0:
        raise ValueError("approved calendar event requires start and duration")
    try:
        zone = ZoneInfo(timezone)
    except ZoneInfoNotFoundError as exc:
        raise ValueError(f"unknown calendar timezone: {timezone}") from exc
    start_value = start if "T" in start else f"{date}T{start}" if date else ""
    if not start_value:
        raise ValueError("approved calendar event requires a date")
    start_dt = datetime.fromisoformat(start_value)
    if start_dt.tzinfo is None:
        start_dt = start_dt.replace(tzinfo=zone)
    if end:
        end_value = end if "T" in end else f"{date}T{end}" if date else ""
        end_dt = datetime.fromisoformat(end_value)
        if end_dt.tzinfo is None:
            end_dt = end_dt.replace(tzinfo=zone)
    else:
        end_dt = start_dt + timedelta(minutes=duration_minutes)
    if end_dt <= start_dt:
        raise ValueError("approved calendar event end must be after start")
    return start_dt.isoformat(), end_dt.isoformat(), duration_minutes


def _success(
    value: dict[str, Any], request: CalendarExecutionRequest, *, already_exists: bool
) -> CalendarExecutionResult:
    event_id = value.get("id")
    if not event_id:
        raise GoogleCalendarAPIError(None, "Google Calendar response is missing event ID")
    start = value.get("start") if isinstance(value.get("start"), dict) else {}
    end = value.get("end") if isinstance(value.get("end"), dict) else {}
    return CalendarExecutionResult(
        success=True, provider="google", calendar_id=request.calendar_id,
        external_event_id=str(event_id),
        html_link=str(value.get("htmlLink")) if value.get("htmlLink") else None,
        created_at=str(value.get("created")) if value.get("created") else None,
        start=str(start.get("dateTime") or request.start),
        end=str(end.get("dateTime") or request.end),
        already_exists=already_exists,
        raw_response_summary={
            "status": str(value.get("status") or "confirmed"),
            "event_id_present": True,
        },
    )


def _retryable(error: GoogleCalendarAPIError) -> bool:
    if error.status in {429} or (error.status is not None and error.status >= 500):
        return True
    if error.status == 403 and error.reason:
        value = error.reason.casefold()
        return "ratelimit" in value or "quota" in value
    return error.status is None and "timeout" in str(error).casefold()


def _integer(value: Any) -> int | None:
    try:
        return int(value) if value is not None else None
    except (TypeError, ValueError):
        return None


def _error_reason(error: Exception) -> str | None:
    content = getattr(error, "content", None)
    if isinstance(content, bytes):
        try:
            payload = json.loads(content.decode("utf-8"))
            errors = payload.get("error", {}).get("errors", [])
            if errors and isinstance(errors[0], dict) and errors[0].get("reason"):
                return str(errors[0]["reason"])[:100]
        except (UnicodeDecodeError, ValueError, AttributeError):
            pass
    reason = getattr(error, "reason", None)
    return str(reason)[:100] if reason else None
