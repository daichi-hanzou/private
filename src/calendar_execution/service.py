from __future__ import annotations

import sqlite3
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

from calendar_agent.audit import read_complete_jsonl
from mail_calendar_orchestrator.approvals import ApprovalRecord, ApprovalService
from mail_calendar_orchestrator.audit import write_jsonl_atomic
from mail_calendar_orchestrator.state import MailStateStore

from .executor import CalendarExecutor
from .google_calendar import execution_datetimes
from .models import (
    CalendarExecutionRequest, CalendarExecutionResult, CalendarRecoveryResult,
)


class CalendarExecutionService:
    def __init__(
        self, state: MailStateStore, executor: CalendarExecutor,
        *, output_dir: str | Path | None = None, max_attempts: int = 3,
        execution_lease_seconds: int = 300,
    ) -> None:
        if not 1 <= max_attempts <= 10:
            raise ValueError("calendar execution attempts must be between 1 and 10")
        self.state = state
        self.executor = executor
        self.output_dir = Path(output_dir).expanduser() if output_dir else None
        self.max_attempts = max_attempts
        if not 1 <= execution_lease_seconds <= 3600:
            raise ValueError("execution lease must be between 1 and 3600 seconds")
        self.execution_lease_seconds = execution_lease_seconds

    def execute(
        self, approval_id: str, *, provider: str = "google",
        calendar_id: str = "primary",
    ) -> CalendarExecutionResult:
        if provider != "google":
            raise ValueError(f"unsupported calendar provider: {provider}")
        record, attempt, mode, existing = self._reserve(
            approval_id, provider=provider, calendar_id=calendar_id
        )
        if existing:
            return existing
        request = self._request(record, calendar_id)
        if mode == "check_only":
            reconciled = self._find_existing(request)
            if reconciled is None:
                return CalendarExecutionResult(
                    success=False, provider=provider, calendar_id=calendar_id,
                    error_type="AlreadyProcessing",
                    error_message="calendar execution is already processing",
                    retryable=True, already_exists=True,
                )
            if not reconciled.success:
                return reconciled
            return self._complete(record, reconciled, attempt)
        try:
            reconciled = self._find_existing(request)
            if reconciled is not None:
                return self._complete(record, reconciled, attempt)
            result = self.executor.create_event(request)
        except ValueError as exc:
            result = CalendarExecutionResult(
                success=False, provider=provider, calendar_id=calendar_id,
                error_type=type(exc).__name__, error_message=str(exc)[:300],
                retryable=False,
            )
        return self._complete(record, result, attempt)

    def _complete(
        self, record: ApprovalRecord, result: CalendarExecutionResult, attempt: int
    ) -> CalendarExecutionResult:
        try:
            result_path = self._write_outcome(record, result, attempt)
        except Exception as exc:
            self._finish(
                record, CalendarExecutionResult(
                    success=False, provider=result.provider,
                    calendar_id=result.calendar_id,
                    error_type=type(exc).__name__,
                    error_message="calendar audit output could not be written",
                    retryable=True,
                ),
                attempt=attempt, result_path=None,
            )
            raise
        self._finish(record, result, attempt=attempt, result_path=result_path)
        return result

    def _find_existing(
        self, request: CalendarExecutionRequest
    ) -> CalendarExecutionResult | None:
        finder = getattr(self.executor, "find_existing", None)
        return finder(request) if callable(finder) else None

    def recover_stale(
        self, *, provider: str = "google", calendar_id: str = "primary",
        stale_after_seconds: int = 300, limit: int = 100,
        now: datetime | None = None,
    ) -> list[CalendarRecoveryResult]:
        """Reconcile stale executions by lookup only; never create an event."""
        if provider != "google":
            raise ValueError(f"unsupported calendar provider: {provider}")
        if not 1 <= stale_after_seconds <= 86400:
            raise ValueError("recovery stale threshold must be between 1 and 86400 seconds")
        if not 1 <= limit <= 500:
            raise ValueError("recovery limit must be between 1 and 500")
        cutoff = (now or datetime.now(timezone.utc)) - timedelta(
            seconds=stale_after_seconds
        )
        rows = self.state.connection.execute(
            """
            SELECT approval.approval_id
            FROM approval_queue approval
            JOIN calendar_execution execution
              ON execution.approval_id=approval.approval_id
             AND execution.provider=?
            WHERE approval.status='executing'
              AND COALESCE(approval.execution_started_at,execution.started_at) IS NOT NULL
              AND COALESCE(approval.execution_started_at,execution.started_at)<=?
            ORDER BY COALESCE(approval.execution_started_at,execution.started_at) ASC
            LIMIT ?
            """,
            (provider, cutoff.isoformat(), limit),
        ).fetchall()
        return [
            self._reconcile_only(
                str(row["approval_id"]), provider=provider,
                calendar_id=calendar_id,
            )
            for row in rows
        ]

    def _reconcile_only(
        self, approval_id: str, *, provider: str, calendar_id: str,
    ) -> CalendarRecoveryResult:
        approval = self.state.connection.execute(
            "SELECT * FROM approval_queue WHERE approval_id=? AND status='executing'",
            (approval_id,),
        ).fetchone()
        if approval is None:
            return CalendarRecoveryResult(approval_id, "no_longer_executing")
        execution = self.state.connection.execute(
            "SELECT attempt_count FROM calendar_execution "
            "WHERE approval_id=? AND provider=?",
            (approval_id, provider),
        ).fetchone()
        if execution is None:
            return CalendarRecoveryResult(approval_id, "lookup_failed", error_type="MissingExecutionState")
        record = ApprovalService._record(approval)
        request = self._request(record, calendar_id)
        existing = self._find_existing(request)
        if existing is None:
            return CalendarRecoveryResult(approval_id, "event_not_found")
        if not existing.success:
            return CalendarRecoveryResult(
                approval_id, "lookup_failed", error_type=existing.error_type
            )
        self._complete(record, existing, int(execution["attempt_count"]))
        return CalendarRecoveryResult(
            approval_id, "reconciled", external_event_id=existing.external_event_id
        )

    def _reserve(
        self, approval_id: str, *, provider: str, calendar_id: str,
    ) -> tuple[ApprovalRecord, int, str, CalendarExecutionResult | None]:
        connection = sqlite3.connect(self.state.path, timeout=30)
        connection.row_factory = sqlite3.Row
        try:
            connection.execute("BEGIN IMMEDIATE")
            approval = connection.execute(
                "SELECT * FROM approval_queue WHERE approval_id=?", (approval_id,)
            ).fetchone()
            if approval is None:
                raise ValueError(f"approval not found: {approval_id}")
            execution = connection.execute(
                "SELECT * FROM calendar_execution WHERE approval_id=? AND provider=?",
                (approval_id, provider),
            ).fetchone()
            if approval["status"] == "calendar_created" and execution:
                connection.commit()
                return ApprovalService._record(approval), int(execution["attempt_count"]), "done", (
                    self._stored_result(execution, calendar_id)
                )
            if approval["status"] == "executing" and execution:
                started = (
                    datetime.fromisoformat(execution["started_at"])
                    if execution["started_at"] else None
                )
                stale_before = datetime.now(timezone.utc) - timedelta(
                    seconds=self.execution_lease_seconds
                )
                if started is not None and started > stale_before:
                    connection.commit()
                    return (
                        ApprovalService._record(approval),
                        int(execution["attempt_count"]), "check_only", None,
                    )
            retry_allowed = (
                approval["status"] == "calendar_failed"
                and execution is not None
                and execution["status"] == "retryable"
            )
            stale_execution = approval["status"] == "executing" and execution is not None
            if approval["status"] != "approved" and not retry_allowed and not stale_execution:
                raise ValueError(
                    f"approval is not executable: {approval_id} ({approval['status']})"
                )
            attempts = int(execution["attempt_count"] if execution else 0)
            if attempts >= self.max_attempts:
                raise ValueError(f"calendar execution retry limit reached: {approval_id}")
            attempt = attempts + 1
            now = datetime.now(timezone.utc).isoformat()
            allowed_status = str(approval["status"])
            cursor = connection.execute(
                "UPDATE approval_queue SET status='executing',updated_at=?,"
                "execution_started_at=?,retry_count=?,last_execution_error=NULL "
                "WHERE approval_id=? AND status=?",
                (now, now, attempt - 1, approval_id, allowed_status),
            )
            if cursor.rowcount != 1:
                connection.rollback()
                latest = ApprovalService(self.state).get(approval_id)
                return latest, attempts, "check_only", None
            connection.execute(
                """
                INSERT INTO calendar_execution (
                    approval_id,provider,calendar_id,status,attempt_count,started_at,
                    completed_at,last_error_type,last_error_message
                ) VALUES (?,?,?,?,?,?,NULL,NULL,NULL)
                ON CONFLICT(approval_id,provider) DO UPDATE SET
                    calendar_id=excluded.calendar_id,status='executing',
                    attempt_count=excluded.attempt_count,started_at=excluded.started_at,
                    completed_at=NULL,last_error_type=NULL,last_error_message=NULL
                """,
                (approval_id, provider, calendar_id, "executing", attempt, now),
            )
            connection.commit()
            claimed = connection.execute(
                "SELECT * FROM approval_queue WHERE approval_id=?", (approval_id,)
            ).fetchone()
            return ApprovalService._record(claimed), attempt, "claimed", None
        except Exception:
            connection.rollback()
            raise
        finally:
            connection.close()

    @staticmethod
    def _stored_result(row: sqlite3.Row, calendar_id: str) -> CalendarExecutionResult:
        return CalendarExecutionResult(
            success=True, provider=row["provider"], calendar_id=calendar_id,
            external_event_id=row["external_event_id"], html_link=row["html_link"],
            already_exists=True,
        )

    @staticmethod
    def _request(record: ApprovalRecord, calendar_id: str) -> CalendarExecutionRequest:
        start, end, duration = execution_datetimes(
            date=record.date, start=record.start, end=record.end,
            duration_minutes=record.duration_minutes, timezone=record.timezone,
        )
        provider_name = (record.source_provider or "mail").title()
        return CalendarExecutionRequest(
            approval_id=record.approval_id,
            calendar_action_id=record.calendar_action_id,
            candidate_id=record.candidate_id, title=record.title,
            start=start, end=end, duration_minutes=duration,
            timezone=record.timezone, location=record.location,
            description=(
                f"Created by AgentLedger.\nApproval ID: {record.approval_id}\n"
                f"Source: {provider_name} mail"
            ),
            source_provider=record.source_provider,
            source_message_id=record.source_message_id,
            calendar_id=calendar_id,
        )

    def _write_outcome(
        self, record: ApprovalRecord, result: CalendarExecutionResult,
        attempt: int,
    ) -> Path:
        execution = self.state.connection.execute(
            "SELECT result_jsonl_path FROM calendar_execution "
            "WHERE approval_id=? AND provider=?",
            (record.approval_id, result.provider),
        ).fetchone()
        source_value = (
            execution["result_jsonl_path"] if execution and execution["result_jsonl_path"]
            else record.outcome_jsonl_path
        )
        if not source_value:
            raise ValueError("approved audit JSONL is missing")
        events = read_complete_jsonl(source_value)
        now = datetime.now(timezone.utc).isoformat()
        actual: dict[str, Any]
        if result.success:
            actual = {
                "outcome_type": "calendar_event_created",
                "provider": result.provider,
                "calendar_id": result.calendar_id,
                "external_event_id": result.external_event_id,
                "html_link": result.html_link,
                "start": result.start,
                "end": result.end,
                "already_exists": result.already_exists,
            }
            status = "confirmed"
            suffix = "created"
        else:
            actual = {
                "outcome_type": "calendar_event_creation_failed",
                "provider": result.provider,
                "error_type": result.error_type,
                "error_message": result.error_message,
            }
            status = "failed"
            suffix = "failed"
        outcome_event = {
            "schema_version": "0.1",
            "event_id": (
                f"outcome-calendar-created-{record.calendar_action_id}"
                if result.success else
                f"outcome-calendar-failed-{record.calendar_action_id}-attempt-{attempt}"
            ),
            "event_type": "outcome_observed",
            "run_id": record.calendar_run_id,
            "agent_id": "calendar_agent",
            "correlation_id": record.calendar_correlation_id,
            "decision_id": record.calendar_decision_id,
            "action_id": record.calendar_action_id,
            "observed_at": now,
            "status": status,
            "actual_outcome": actual,
        }
        if not any(
            event.get("event_id") == outcome_event["event_id"]
            for event in events
        ):
            events.append(outcome_event)
        source = Path(source_value)
        directory = self.output_dir or source.parent
        directory.mkdir(parents=True, exist_ok=True, mode=0o700)
        output = (
            directory / f"calendar-execution-{record.approval_id}-created.jsonl"
            if result.success else
            directory / f"{source.stem}-calendar-{suffix}-attempt-{attempt}.jsonl"
        )
        return write_jsonl_atomic(output, events)

    def _finish(
        self, record: ApprovalRecord, result: CalendarExecutionResult,
        *, attempt: int, result_path: Path | None,
    ) -> None:
        approval_status = "calendar_created" if result.success else "calendar_failed"
        execution_status = (
            "succeeded" if result.success else "retryable" if result.retryable else "failed"
        )
        now = datetime.now(timezone.utc).isoformat()
        with self.state.connection:
            self.state.connection.execute(
                "UPDATE approval_queue SET status=?,updated_at=?,executed_at=?,"
                "calendar_event_id=?,last_execution_error=?,retry_count=? "
                "WHERE approval_id=? AND status='executing'",
                (
                    approval_status, now, now if result.success else None,
                    result.external_event_id if result.success else None,
                    result.error_type if not result.success else None,
                    max(attempt - 1, 0), record.approval_id,
                ),
            )
            self.state.connection.execute(
                """
                UPDATE calendar_execution SET status=?,external_event_id=?,html_link=?,
                    completed_at=?,last_error_type=?,last_error_message=?,
                    result_jsonl_path=COALESCE(?,result_jsonl_path)
                WHERE approval_id=? AND provider=?
                """,
                (
                    execution_status, result.external_event_id, result.html_link,
                    now, result.error_type,
                    result.error_message[:300] if result.error_message else None,
                    str(result_path) if result_path else None,
                    record.approval_id, result.provider,
                ),
            )
