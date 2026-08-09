from __future__ import annotations

import sqlite3
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

from calendar_agent.agent import CalendarAgent
from calendar_agent.audit import read_complete_jsonl

from .audit import write_jsonl_atomic
from .state import MailStateStore


APPROVAL_STATUSES = {
    "awaiting_approval", "approved", "executing", "calendar_created",
    "calendar_failed", "rejected", "expired", "failed"
}


@dataclass(frozen=True)
class ApprovalRecord:
    approval_id: str
    calendar_action_id: str
    calendar_decision_id: str | None
    calendar_correlation_id: str | None
    calendar_run_id: str | None
    candidate_id: str | None
    source_provider: str | None
    source_message_id: str | None
    source_thread_id: str | None
    title: str
    candidate_type: str
    date: str | None
    start: str | None
    end: str | None
    duration_minutes: int | None
    timezone: str
    location: str | None
    classification_summary: str | None
    status: str
    created_at: datetime
    updated_at: datetime
    expires_at: datetime | None
    approved_at: datetime | None
    rejected_at: datetime | None
    expired_at: datetime | None
    actor: str | None
    reason: str | None
    source_jsonl_path: str
    outcome_jsonl_path: str | None


class ApprovalService:
    def __init__(
        self, state: MailStateStore, *, expires_after_hours: int = 72,
        output_dir: str | Path | None = None,
    ) -> None:
        if not 1 <= expires_after_hours <= 24 * 30:
            raise ValueError("approval expiry must be between 1 and 720 hours")
        self.state = state
        self.expires_after_hours = expires_after_hours
        self.output_dir = Path(output_dir).expanduser() if output_dir else None

    def register_pending(
        self, events: list[dict[str, Any]], source_jsonl_path: str | Path,
        *, now: datetime | None = None,
    ) -> list[ApprovalRecord]:
        created = now or datetime.now(timezone.utc)
        source = str(Path(source_jsonl_path).expanduser().resolve())
        records = []
        for action in events:
            if (
                action.get("event_type") != "action_executed"
                or action.get("agent_id") != "calendar_agent"
                or action.get("status") != "awaiting_approval"
            ):
                continue
            existing = self._by_action(str(action.get("action_id") or ""))
            if existing:
                continue
            explanation = next(
                (
                    event.get("explanation")
                    for event in events
                    if event.get("event_type") == "decision_made"
                    and event.get("decision_id") == action.get("decision_id")
                ),
                None,
            )
            record = self._insert(action, source, created, explanation)
            if record:
                records.append(record)
        return records

    def _insert(
        self, action: dict[str, Any], source: str, created: datetime,
        explanation: Any,
    ) -> ApprovalRecord | None:
        action_id = str(action.get("action_id") or "")
        if not action_id:
            raise ValueError("pending calendar action is missing action_id")
        parameters = action.get("action_parameters")
        parameters = parameters if isinstance(parameters, dict) else {}
        metadata = action.get("metadata")
        metadata = metadata if isinstance(metadata, dict) else {}
        start_value = parameters.get("start")
        start_value = str(start_value) if start_value else None
        date_value = metadata.get("candidate_date")
        if not date_value and start_value and "T" in start_value:
            date_value = start_value.split("T", 1)[0]
        time_value = start_value.split("T", 1)[1] if start_value and "T" in start_value else start_value
        expires = created + timedelta(hours=self.expires_after_hours)
        connection = self.state.connection
        try:
            with connection:
                sequence = connection.execute(
                    "INSERT INTO approval_id_sequence DEFAULT VALUES"
                ).lastrowid
                approval_id = f"AP-{sequence:06d}"
                connection.execute(
                    """
                    INSERT INTO approval_queue (
                        approval_id,calendar_action_id,calendar_decision_id,
                        calendar_correlation_id,calendar_run_id,candidate_id,
                        source_provider,source_message_id,source_thread_id,title,
                        candidate_type,date,start,end,duration_minutes,timezone,
                        location,classification_summary,status,created_at,updated_at,expires_at,
                        source_jsonl_path
                    ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
                    """,
                    (
                        approval_id, action_id, action.get("decision_id"),
                        action.get("correlation_id"), action.get("run_id"),
                        metadata.get("mail_candidate_id"),
                        metadata.get("source_provider"),
                        metadata.get("source_message_id"),
                        metadata.get("source_thread_id"),
                        str(parameters.get("title") or "Untitled event")[:200],
                        str(metadata.get("candidate_type") or "calendar_event")[:80],
                        str(date_value) if date_value else None,
                        time_value, metadata.get("candidate_end"),
                        parameters.get("duration_minutes"),
                        str(metadata.get("candidate_timezone") or "Asia/Tokyo"),
                        metadata.get("candidate_location"),
                        str(explanation)[:300] if explanation else None,
                        "awaiting_approval",
                        created.isoformat(), created.isoformat(), expires.isoformat(),
                        source,
                    ),
                )
        except sqlite3.IntegrityError:
            return None
        return self.get(approval_id)

    def get(self, approval_id: str) -> ApprovalRecord:
        row = self.state.connection.execute(
            "SELECT * FROM approval_queue WHERE approval_id=?", (approval_id,)
        ).fetchone()
        if row is None:
            raise ValueError(f"approval not found: {approval_id}")
        return self._record(row)

    def _by_action(self, action_id: str) -> ApprovalRecord | None:
        row = self.state.connection.execute(
            "SELECT * FROM approval_queue WHERE calendar_action_id=?", (action_id,)
        ).fetchone()
        return self._record(row) if row else None

    def list(
        self, *, status: str | None = "awaiting_approval", limit: int = 20,
    ) -> list[ApprovalRecord]:
        if status is not None and status not in APPROVAL_STATUSES:
            raise ValueError(f"invalid approval status: {status}")
        if not 1 <= limit <= 500:
            raise ValueError("approval limit must be between 1 and 500")
        query = "SELECT * FROM approval_queue"
        parameters: tuple[Any, ...] = ()
        if status is not None:
            query += " WHERE status=?"
            parameters = (status,)
        query += " ORDER BY created_at ASC LIMIT ?"
        return [
            self._record(row)
            for row in self.state.connection.execute(query, (*parameters, limit))
        ]

    def summary(self) -> dict[str, Any]:
        counts = {status: 0 for status in APPROVAL_STATUSES}
        for row in self.state.connection.execute(
            "SELECT status,COUNT(*) count FROM approval_queue GROUP BY status"
        ):
            counts[str(row["status"])] = int(row["count"])
        pending = self.state.connection.execute(
            "SELECT MIN(created_at) oldest,MAX(created_at) newest "
            "FROM approval_queue WHERE status='awaiting_approval'"
        ).fetchone()
        return {
            **counts,
            "oldest_pending": pending["oldest"],
            "newest_pending": pending["newest"],
        }

    def approve(self, approval_id: str, actor: str, reason: str) -> ApprovalRecord:
        return self._resolve(approval_id, "approve", actor, reason)

    def reject(self, approval_id: str, actor: str, reason: str) -> ApprovalRecord:
        return self._resolve(approval_id, "reject", actor, reason)

    def _resolve(
        self, approval_id: str, resolution: str, actor: str, reason: str,
    ) -> ApprovalRecord:
        actor = actor.strip()
        reason = reason.strip()
        if not actor or len(actor) > 100:
            raise ValueError("actor must be between 1 and 100 characters")
        if not reason or len(reason) > 500:
            raise ValueError("reason must be between 1 and 500 characters")
        connection = sqlite3.connect(self.state.path, timeout=30)
        connection.row_factory = sqlite3.Row
        try:
            connection.execute("BEGIN IMMEDIATE")
            row = connection.execute(
                "SELECT * FROM approval_queue WHERE approval_id=?", (approval_id,)
            ).fetchone()
            if row is None:
                raise ValueError(f"approval not found: {approval_id}")
            if row["status"] != "awaiting_approval":
                raise ValueError(
                    f"approval is not awaiting approval: {approval_id} ({row['status']})"
                )
            record = self._record(row)
            events = read_complete_jsonl(record.source_jsonl_path)
            self._validate_source(record, events)
            resolved = CalendarAgent().resolve(
                events, action_id=record.calendar_action_id,
                resolution=resolution, actor=actor, reason=reason,
            )
            if actor.startswith("line:"):
                resolved = [
                    {
                        **event,
                        "input_method": "line_postback",
                    }
                    if event.get("event_type") == "human_intervention"
                    and event.get("related_action_id") == record.calendar_action_id
                    else event
                    for event in resolved
                ]
            if resolution == "approve":
                actual = resolved[-1].get("actual_outcome")
                if isinstance(actual, dict):
                    resolved[-1] = {
                        **resolved[-1],
                        "actual_outcome": {
                            **actual,
                            "outcome_type": "calendar_event_approved",
                        },
                    }
            output = self._resolution_path(record, resolution)
            write_jsonl_atomic(output, resolved)
            now = datetime.now(timezone.utc).isoformat()
            final = "approved" if resolution == "approve" else "rejected"
            timestamp_column = "approved_at" if resolution == "approve" else "rejected_at"
            cursor = connection.execute(
                f"UPDATE approval_queue SET status=?,updated_at=?,{timestamp_column}=?,"
                "actor=?,reason=?,outcome_jsonl_path=? "
                "WHERE approval_id=? AND status='awaiting_approval'",
                (final, now, now, actor, reason, str(output), approval_id),
            )
            if cursor.rowcount != 1:
                raise ValueError(f"approval was resolved concurrently: {approval_id}")
            connection.commit()
        except Exception:
            connection.rollback()
            raise
        finally:
            connection.close()
        return self.get(approval_id)

    def _validate_source(
        self, record: ApprovalRecord, events: list[dict[str, Any]]
    ) -> None:
        matches = [
            event for event in events
            if event.get("event_type") == "action_executed"
            and event.get("action_id") == record.calendar_action_id
        ]
        if len(matches) != 1:
            raise ValueError("source JSONL does not contain exactly one approval action")
        action = matches[0]
        metadata = action.get("metadata")
        metadata = metadata if isinstance(metadata, dict) else {}
        if record.candidate_id and metadata.get("mail_candidate_id") != record.candidate_id:
            raise ValueError("source JSONL candidate_id does not match approval")
        if action.get("status") != "awaiting_approval":
            raise ValueError("source action is not awaiting approval")
        if any(
            event.get("event_type") in {"outcome_observed", "human_intervention"}
            and (
                event.get("action_id") == record.calendar_action_id
                or event.get("related_action_id") == record.calendar_action_id
            )
            for event in events
        ):
            raise ValueError("source action is already resolved")

    def _resolution_path(self, record: ApprovalRecord, resolution: str) -> Path:
        source = Path(record.source_jsonl_path)
        directory = self.output_dir or source.parent
        directory.mkdir(parents=True, exist_ok=True, mode=0o700)
        suffix = "approved" if resolution == "approve" else "rejected"
        return directory / f"{source.stem}-{suffix}-{record.approval_id}.jsonl"

    def expire(self, *, now: datetime | None = None) -> int:
        current = (now or datetime.now(timezone.utc)).isoformat()
        with self.state.connection:
            cursor = self.state.connection.execute(
                "UPDATE approval_queue SET status='expired',updated_at=?,expired_at=? "
                "WHERE status='awaiting_approval' AND expires_at IS NOT NULL "
                "AND expires_at<=?",
                (current, current, current),
            )
        return cursor.rowcount

    @staticmethod
    def _record(row: sqlite3.Row) -> ApprovalRecord:
        def date(name: str) -> datetime | None:
            return datetime.fromisoformat(row[name]) if row[name] else None
        return ApprovalRecord(
            approval_id=row["approval_id"], calendar_action_id=row["calendar_action_id"],
            calendar_decision_id=row["calendar_decision_id"],
            calendar_correlation_id=row["calendar_correlation_id"],
            calendar_run_id=row["calendar_run_id"], candidate_id=row["candidate_id"],
            source_provider=row["source_provider"],
            source_message_id=row["source_message_id"],
            source_thread_id=row["source_thread_id"], title=row["title"],
            candidate_type=row["candidate_type"], date=row["date"],
            start=row["start"], end=row["end"],
            duration_minutes=row["duration_minutes"], timezone=row["timezone"],
            location=row["location"], status=row["status"],
            classification_summary=row["classification_summary"],
            created_at=date("created_at"), updated_at=date("updated_at"),
            expires_at=date("expires_at"), approved_at=date("approved_at"),
            rejected_at=date("rejected_at"), expired_at=date("expired_at"),
            actor=row["actor"], reason=row["reason"],
            source_jsonl_path=row["source_jsonl_path"],
            outcome_jsonl_path=row["outcome_jsonl_path"],
        )
