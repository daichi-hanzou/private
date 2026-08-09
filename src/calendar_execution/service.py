from __future__ import annotations

import sqlite3
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from calendar_agent.audit import read_complete_jsonl
from mail_calendar_orchestrator.approvals import ApprovalRecord, ApprovalService
from mail_calendar_orchestrator.audit import write_jsonl_atomic
from mail_calendar_orchestrator.state import MailStateStore

from .executor import CalendarExecutor
from .google_calendar import execution_datetimes
from .models import CalendarExecutionRequest, CalendarExecutionResult


class CalendarExecutionService:
    def __init__(
        self, state: MailStateStore, executor: CalendarExecutor,
        *, output_dir: str | Path | None = None, max_attempts: int = 3,
    ) -> None:
        if not 1 <= max_attempts <= 10:
            raise ValueError("calendar execution attempts must be between 1 and 10")
        self.state = state
        self.executor = executor
        self.output_dir = Path(output_dir).expanduser() if output_dir else None
        self.max_attempts = max_attempts

    def execute(
        self, approval_id: str, *, provider: str = "google",
        calendar_id: str = "primary",
    ) -> CalendarExecutionResult:
        if provider != "google":
            raise ValueError(f"unsupported calendar provider: {provider}")
        record, attempt, existing = self._reserve(
            approval_id, provider=provider, calendar_id=calendar_id
        )
        if existing:
            return existing
        try:
            request = self._request(record, calendar_id)
            result = self.executor.create_event(request)
        except ValueError as exc:
            result = CalendarExecutionResult(
                success=False, provider=provider, calendar_id=calendar_id,
                error_type=type(exc).__name__, error_message=str(exc)[:300],
                retryable=False,
            )
        try:
            result_path = self._write_outcome(record, result, attempt)
        except Exception as exc:
            self._finish(
                record, CalendarExecutionResult(
                    success=False, provider=provider, calendar_id=calendar_id,
                    error_type=type(exc).__name__,
                    error_message="calendar audit output could not be written",
                    retryable=True,
                ),
                attempt=attempt, result_path=None,
            )
            raise
        self._finish(record, result, attempt=attempt, result_path=result_path)
        return result

    def _reserve(
        self, approval_id: str, *, provider: str, calendar_id: str,
    ) -> tuple[ApprovalRecord, int, CalendarExecutionResult | None]:
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
                return ApprovalService._record(approval), int(execution["attempt_count"]), (
                    self._stored_result(execution, calendar_id)
                )
            retry_allowed = (
                approval["status"] == "calendar_failed"
                and execution is not None
                and execution["status"] == "retryable"
            )
            if approval["status"] != "approved" and not retry_allowed:
                raise ValueError(
                    f"approval is not executable: {approval_id} ({approval['status']})"
                )
            attempts = int(execution["attempt_count"] if execution else 0)
            if attempts >= self.max_attempts:
                raise ValueError(f"calendar execution retry limit reached: {approval_id}")
            attempt = attempts + 1
            now = datetime.now(timezone.utc).isoformat()
            connection.execute(
                "UPDATE approval_queue SET status='executing',updated_at=? "
                "WHERE approval_id=?", (now, approval_id)
            )
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
            return ApprovalService._record(approval), attempt, None
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
        events.append({
            "schema_version": "0.1",
            "event_id": (
                f"outcome-calendar-{suffix}-{record.calendar_action_id}-attempt-{attempt}"
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
        })
        source = Path(source_value)
        directory = self.output_dir or source.parent
        directory.mkdir(parents=True, exist_ok=True, mode=0o700)
        output = directory / (
            f"{source.stem}-calendar-{suffix}-attempt-{attempt}.jsonl"
        )
        return write_jsonl_atomic(output, events)

    def _finish(
        self, record: ApprovalRecord, result: CalendarExecutionResult,
        *, attempt: int, result_path: Path | None,
    ) -> None:
        del attempt
        approval_status = "calendar_created" if result.success else "calendar_failed"
        execution_status = (
            "succeeded" if result.success else "retryable" if result.retryable else "failed"
        )
        now = datetime.now(timezone.utc).isoformat()
        with self.state.connection:
            self.state.connection.execute(
                "UPDATE approval_queue SET status=?,updated_at=? "
                "WHERE approval_id=? AND status='executing'",
                (approval_status, now, record.approval_id),
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
