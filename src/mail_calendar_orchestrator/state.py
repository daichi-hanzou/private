from __future__ import annotations

import hashlib
import json
import os
import sqlite3
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any
from zoneinfo import ZoneInfo
from zoneinfo import ZoneInfoNotFoundError

from mail_to_calendar.models import EmailMessage


DEFAULT_STATE_DB = Path("~/.local/share/agentledger/mail_state.sqlite3")
BOOTSTRAP_VERSION = "1"


def _now() -> datetime:
    return datetime.now(timezone.utc)


def _timestamp(value: datetime | None = None) -> str:
    return (value or _now()).isoformat()


def _normalized_text(value: str) -> str:
    return " ".join(value.split())


def subject_hash(message: EmailMessage) -> str:
    return hashlib.sha256(
        _normalized_text(message.subject).encode("utf-8")
    ).hexdigest()


def content_hash(message: EmailMessage) -> str:
    payload = {
        "provider": message.provider,
        "message_id": message.message_id,
        "subject": _normalized_text(message.subject),
        "body_text": _normalized_text(message.body_text),
        "received_at": message.received_at,
    }
    encoded = json.dumps(
        payload, ensure_ascii=False, sort_keys=True, separators=(",", ":")
    ).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


@dataclass(frozen=True)
class ProcessingDecision:
    process: bool
    reason: str
    content_hash: str


class MailStateStore:
    """SQLite-backed operational state; audit evidence remains in JSONL."""

    def __init__(self, path: str | Path = DEFAULT_STATE_DB) -> None:
        self.path = Path(path).expanduser()
        self.path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
        try:
            os.chmod(self.path.parent, 0o700)
        except OSError:
            pass
        self.connection = sqlite3.connect(self.path)
        self.connection.row_factory = sqlite3.Row
        self.connection.execute("PRAGMA foreign_keys = ON")
        self._create_schema()
        try:
            os.chmod(self.path, 0o600)
        except OSError:
            pass

    def close(self) -> None:
        self.connection.close()

    def __enter__(self) -> MailStateStore:
        return self

    def __exit__(self, *_: object) -> None:
        self.close()

    def _create_schema(self) -> None:
        with self.connection:
            self.connection.executescript(
                """
                CREATE TABLE IF NOT EXISTS processed_messages (
                    provider TEXT NOT NULL,
                    message_id TEXT NOT NULL,
                    thread_id TEXT,
                    received_at TEXT,
                    subject_hash TEXT NOT NULL,
                    content_hash TEXT NOT NULL,
                    first_seen_at TEXT NOT NULL,
                    processing_started_at TEXT,
                    last_processed_at TEXT,
                    processing_status TEXT NOT NULL,
                    final_classification TEXT,
                    candidate_id TEXT,
                    mail_action_id TEXT,
                    calendar_action_id TEXT,
                    analysis_mode TEXT,
                    model_name TEXT,
                    prompt_template_version TEXT,
                    schema_version TEXT,
                    retry_count INTEGER NOT NULL DEFAULT 0,
                    last_error_type TEXT,
                    last_error_message TEXT,
                    source_run_id TEXT,
                    source_jsonl_path TEXT,
                    message_source_ref TEXT,
                    PRIMARY KEY (provider, message_id)
                );
                CREATE INDEX IF NOT EXISTS idx_processed_candidate
                    ON processed_messages(candidate_id);
                CREATE TABLE IF NOT EXISTS runs (
                    run_id TEXT PRIMARY KEY,
                    started_at TEXT NOT NULL,
                    finished_at TEXT,
                    provider TEXT NOT NULL,
                    analysis_mode TEXT NOT NULL,
                    model_name TEXT,
                    fetched_messages INTEGER NOT NULL DEFAULT 0,
                    new_messages INTEGER NOT NULL DEFAULT 0,
                    skipped_messages INTEGER NOT NULL DEFAULT 0,
                    processed_messages INTEGER NOT NULL DEFAULT 0,
                    failed_messages INTEGER NOT NULL DEFAULT 0,
                    calendar_candidates INTEGER NOT NULL DEFAULT 0,
                    clarification_required INTEGER NOT NULL DEFAULT 0,
                    promotions INTEGER NOT NULL DEFAULT 0,
                    informational INTEGER NOT NULL DEFAULT 0,
                    security_notifications INTEGER NOT NULL DEFAULT 0,
                    invalid INTEGER NOT NULL DEFAULT 0,
                    duration_ms INTEGER,
                    status TEXT NOT NULL
                );
                CREATE TABLE IF NOT EXISTS provider_runs (
                    run_id TEXT NOT NULL,
                    provider TEXT NOT NULL,
                    status TEXT NOT NULL,
                    fetched_messages INTEGER NOT NULL DEFAULT 0,
                    error_type TEXT,
                    error_message TEXT,
                    updated_at TEXT NOT NULL,
                    PRIMARY KEY (run_id, provider)
                );
                CREATE TABLE IF NOT EXISTS system_state (
                    key TEXT PRIMARY KEY,
                    value TEXT NOT NULL,
                    updated_at TEXT NOT NULL
                );
                CREATE TABLE IF NOT EXISTS scheduled_locks (
                    name TEXT PRIMARY KEY,
                    owner TEXT NOT NULL,
                    acquired_at TEXT NOT NULL
                );
                CREATE TABLE IF NOT EXISTS approval_id_sequence (
                    id INTEGER PRIMARY KEY AUTOINCREMENT
                );
                CREATE TABLE IF NOT EXISTS approval_queue (
                    approval_id TEXT PRIMARY KEY,
                    calendar_action_id TEXT NOT NULL UNIQUE,
                    calendar_decision_id TEXT,
                    calendar_correlation_id TEXT,
                    calendar_run_id TEXT,
                    candidate_id TEXT,
                    source_provider TEXT,
                    source_message_id TEXT,
                    source_thread_id TEXT,
                    title TEXT NOT NULL,
                    candidate_type TEXT NOT NULL,
                    date TEXT,
                    start TEXT,
                    end TEXT,
                    duration_minutes INTEGER,
                    timezone TEXT NOT NULL,
                    location TEXT,
                    classification_summary TEXT,
                    status TEXT NOT NULL,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL,
                    expires_at TEXT,
                    approved_at TEXT,
                    rejected_at TEXT,
                    expired_at TEXT,
                    actor TEXT,
                    reason TEXT,
                    source_jsonl_path TEXT NOT NULL,
                    outcome_jsonl_path TEXT
                );
                CREATE INDEX IF NOT EXISTS idx_approval_status
                    ON approval_queue(status, created_at);
                CREATE TABLE IF NOT EXISTS calendar_execution (
                    approval_id TEXT NOT NULL,
                    provider TEXT NOT NULL,
                    calendar_id TEXT NOT NULL,
                    status TEXT NOT NULL,
                    attempt_count INTEGER NOT NULL DEFAULT 0,
                    external_event_id TEXT,
                    html_link TEXT,
                    started_at TEXT,
                    completed_at TEXT,
                    last_error_type TEXT,
                    last_error_message TEXT,
                    result_jsonl_path TEXT,
                    PRIMARY KEY (approval_id, provider),
                    FOREIGN KEY (approval_id) REFERENCES approval_queue(approval_id)
                );
                CREATE TABLE IF NOT EXISTS line_notifications (
                    notification_id TEXT PRIMARY KEY,
                    approval_id TEXT NOT NULL UNIQUE,
                    line_user_hash TEXT NOT NULL,
                    status TEXT NOT NULL,
                    sent_at TEXT,
                    response_at TEXT,
                    retry_count INTEGER NOT NULL DEFAULT 0,
                    last_error_type TEXT,
                    last_error_message TEXT,
                    FOREIGN KEY (approval_id) REFERENCES approval_queue(approval_id)
                );
                CREATE TABLE IF NOT EXISTS approval_interaction_tokens (
                    approval_id TEXT PRIMARY KEY,
                    token_hash TEXT NOT NULL,
                    created_at TEXT NOT NULL,
                    expires_at TEXT NOT NULL,
                    consumed_at TEXT,
                    FOREIGN KEY (approval_id) REFERENCES approval_queue(approval_id)
                );
                CREATE TABLE IF NOT EXISTS line_webhook_events (
                    webhook_event_id TEXT PRIMARY KEY,
                    processed_at TEXT NOT NULL
                );
                CREATE TABLE IF NOT EXISTS important_mail_notifications (
                    provider TEXT NOT NULL,
                    message_id TEXT NOT NULL,
                    notification_type TEXT NOT NULL DEFAULT 'important_mail',
                    category TEXT NOT NULL,
                    subject TEXT NOT NULL,
                    notification_date TEXT,
                    amount TEXT,
                    should_notify_user INTEGER NOT NULL,
                    status TEXT NOT NULL,
                    attempt_count INTEGER NOT NULL DEFAULT 0,
                    attempted_at TEXT,
                    notified_at TEXT,
                    last_error_type TEXT,
                    retryable INTEGER NOT NULL DEFAULT 0,
                    PRIMARY KEY (provider,message_id,notification_type)
                );
                CREATE TABLE IF NOT EXISTS review_case_id_sequence (
                    id INTEGER PRIMARY KEY AUTOINCREMENT
                );
                CREATE TABLE IF NOT EXISTS review_cases (
                    review_case_id TEXT PRIMARY KEY,
                    provider TEXT NOT NULL,
                    message_id TEXT NOT NULL,
                    source_run_id TEXT,
                    source_jsonl_path TEXT NOT NULL,
                    message_source_ref TEXT,
                    reviewer_model TEXT NOT NULL,
                    review_version TEXT NOT NULL,
                    review_status TEXT NOT NULL,
                    suggested_classification TEXT,
                    issue_type TEXT,
                    confidence REAL NOT NULL,
                    reason_summary TEXT NOT NULL,
                    needs_human_review INTEGER NOT NULL,
                    original_classification TEXT,
                    final_classification TEXT,
                    human_review_status TEXT NOT NULL DEFAULT 'pending',
                    human_verdict TEXT,
                    human_final_classification TEXT,
                    lesson_summary TEXT,
                    recommended_change_target TEXT,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL,
                    human_reviewed_at TEXT,
                    UNIQUE(provider, message_id, reviewer_model, review_version)
                );
                CREATE INDEX IF NOT EXISTS idx_review_queue
                    ON review_cases(review_status, human_review_status, created_at);
                CREATE TABLE IF NOT EXISTS review_chat_messages (
                    review_case_id TEXT NOT NULL,
                    message_index INTEGER NOT NULL,
                    role TEXT NOT NULL,
                    content TEXT NOT NULL,
                    created_at TEXT NOT NULL,
                    PRIMARY KEY (review_case_id, message_index),
                    FOREIGN KEY (review_case_id) REFERENCES review_cases(review_case_id)
                );
                """
            )
            self._ensure_column(
                "approval_queue", "execution_started_at", "TEXT"
            )
            self._ensure_column("approval_queue", "executed_at", "TEXT")
            self._ensure_column("approval_queue", "calendar_event_id", "TEXT")
            self._ensure_column(
                "approval_queue", "last_execution_error", "TEXT"
            )
            self._ensure_column(
                "approval_queue", "retry_count", "INTEGER NOT NULL DEFAULT 0"
            )
            self._ensure_column(
                "important_mail_notifications", "retryable",
                "INTEGER NOT NULL DEFAULT 0",
            )
            self._ensure_column("processed_messages", "source_jsonl_path", "TEXT")
            self._ensure_column("processed_messages", "message_source_ref", "TEXT")

    def register_important_notification(
        self, *, provider: str, message_id: str, category: str,
        subject: str, notification_date: str | None, amount: str | None,
    ) -> bool:
        with self.connection:
            cursor = self.connection.execute(
                """
                INSERT OR IGNORE INTO important_mail_notifications(
                    provider,message_id,notification_type,category,subject,
                    notification_date,amount,should_notify_user,status
                ) VALUES(?,?,'important_mail',?,?,?,?,1,'pending')
                """,
                (
                    provider, message_id, category[:80], subject[:200],
                    notification_date, amount,
                ),
            )
        return cursor.rowcount == 1

    def pending_important_notifications(self, *, limit: int = 50) -> list[sqlite3.Row]:
        if not 1 <= limit <= 500:
            raise ValueError("important notification limit must be between 1 and 500")
        return self.connection.execute(
            "SELECT * FROM important_mail_notifications "
            "WHERE (status='pending' OR (status='failed' AND retryable=1)) "
            "AND attempt_count<3 "
            "ORDER BY rowid ASC LIMIT ?", (limit,),
        ).fetchall()

    def _ensure_column(self, table: str, name: str, declaration: str) -> None:
        columns = {
            str(row["name"])
            for row in self.connection.execute(f"PRAGMA table_info({table})")
        }
        if name not in columns:
            self.connection.execute(
                f"ALTER TABLE {table} ADD COLUMN {name} {declaration}"
            )

    def get_system_value(self, key: str) -> str | None:
        row = self.connection.execute(
            "SELECT value FROM system_state WHERE key=?", (key,)
        ).fetchone()
        return str(row["value"]) if row else None

    def set_system_value(self, key: str, value: str) -> None:
        with self.connection:
            self.connection.execute(
                "INSERT INTO system_state(key,value,updated_at) VALUES(?,?,?) "
                "ON CONFLICT(key) DO UPDATE SET value=excluded.value,"
                "updated_at=excluded.updated_at",
                (key, value, _timestamp()),
            )

    def ensure_bootstrap(
        self, *, timezone_name: str, start_from: str | None = None,
        reset: bool = False, now: datetime | None = None,
    ) -> datetime:
        try:
            zone = ZoneInfo(timezone_name)
        except ZoneInfoNotFoundError as exc:
            raise ValueError(f"unknown timezone: {timezone_name}") from exc
        existing = self.get_system_value("processing_started_from")
        if existing and not reset:
            return datetime.fromisoformat(existing)
        current = (now or _now()).astimezone(zone)
        if start_from in {None, "today"}:
            started = current.replace(hour=0, minute=0, second=0, microsecond=0)
        else:
            started = datetime.fromisoformat(start_from)
            if started.tzinfo is None:
                raise ValueError("start-from must include a timezone offset")
            started = started.astimezone(zone)
        with self.connection:
            for key, value in (
                ("processing_started_from", started.isoformat()),
                ("timezone", timezone_name),
                ("bootstrap_version", BOOTSTRAP_VERSION),
            ):
                self.connection.execute(
                    "INSERT INTO system_state(key,value,updated_at) VALUES(?,?,?) "
                    "ON CONFLICT(key) DO UPDATE SET value=excluded.value,"
                    "updated_at=excluded.updated_at",
                    (key, value, _timestamp()),
                )
        return started

    @staticmethod
    def _provider_state_key(base: str, provider: str) -> str:
        return base if provider == "outlook" else f"{base}:{provider}"

    def cursor(
        self, *, overlap_minutes: int = 5, provider: str = "outlook"
    ) -> dict[str, Any]:
        if not 0 <= overlap_minutes <= 60:
            raise ValueError("poll overlap must be between 0 and 60 minutes")
        started = self.get_system_value("processing_started_from")
        if not started:
            raise ValueError("mail state has not been bootstrapped")
        last = self.get_system_value(
            self._provider_state_key("last_successful_poll_at", provider)
        )
        next_fetch = (
            datetime.fromisoformat(last) - timedelta(minutes=overlap_minutes)
            if last else datetime.fromisoformat(started)
        )
        return {
            "processing_started_from": datetime.fromisoformat(started),
            "last_successful_poll_at": (
                datetime.fromisoformat(last) if last else None
            ),
            "overlap_minutes": overlap_minutes,
            "next_fetch_from": next_fetch,
            "timezone": self.get_system_value("timezone"),
            "provider": provider,
        }

    def mark_successful_poll(
        self, value: datetime | None = None, *, provider: str = "outlook"
    ) -> datetime:
        completed = value or _now()
        if completed.tzinfo is None:
            raise ValueError("successful poll timestamp must be timezone-aware")
        self.set_system_value(
            self._provider_state_key("last_successful_poll_at", provider),
            completed.isoformat(),
        )
        return completed

    def record_provider_run(
        self, run_id: str, *, provider: str, status: str,
        fetched_messages: int = 0, error: BaseException | None = None,
    ) -> None:
        error_type = type(error).__name__ if error else None
        error_message = "provider fetch failed" if error else None
        with self.connection:
            self.connection.execute(
                "INSERT INTO provider_runs(run_id,provider,status,fetched_messages,"
                "error_type,error_message,updated_at) VALUES(?,?,?,?,?,?,?) "
                "ON CONFLICT(run_id,provider) DO UPDATE SET status=excluded.status,"
                "fetched_messages=excluded.fetched_messages,error_type=excluded.error_type,"
                "error_message=excluded.error_message,updated_at=excluded.updated_at",
                (run_id, provider, status, fetched_messages, error_type,
                 error_message, _timestamp()),
            )

    def acquire_scheduled_lock(
        self, owner: str, *, name: str = "outlook", stale_after: timedelta = timedelta(minutes=35),
        now: datetime | None = None,
    ) -> bool:
        current = now or _now()
        with self.connection:
            row = self.connection.execute(
                "SELECT owner,acquired_at FROM scheduled_locks WHERE name=?",
                (name,),
            ).fetchone()
            if row:
                try:
                    stale = datetime.fromisoformat(row["acquired_at"]) <= current - stale_after
                except ValueError:
                    stale = True
                if not stale:
                    return False
                self.connection.execute(
                    "DELETE FROM scheduled_locks WHERE name=?", (name,)
                )
            try:
                self.connection.execute(
                    "INSERT INTO scheduled_locks(name,owner,acquired_at) VALUES(?,?,?)",
                    (name, owner, current.isoformat()),
                )
            except sqlite3.IntegrityError:
                return False
        return True

    def release_scheduled_lock(self, owner: str, *, name: str = "outlook") -> None:
        with self.connection:
            self.connection.execute(
                "DELETE FROM scheduled_locks WHERE name=? AND owner=?",
                (name, owner),
            )

    def decision(
        self,
        message: EmailMessage,
        *,
        reprocess: bool = False,
        retry_failed: bool = False,
        stale_after: timedelta = timedelta(minutes=30),
    ) -> ProcessingDecision:
        digest = content_hash(message)
        row = self.connection.execute(
            "SELECT * FROM processed_messages WHERE provider=? AND message_id=?",
            (message.provider, message.message_id),
        ).fetchone()
        if row is None and message.message_id.startswith(f"{message.provider}:"):
            legacy_id = message.message_id.removeprefix(f"{message.provider}:")
            legacy = self.connection.execute(
                "SELECT 1 FROM processed_messages WHERE provider=? AND message_id=?",
                (message.provider, legacy_id),
            ).fetchone()
            if legacy is not None:
                with self.connection:
                    self.connection.execute(
                        "UPDATE processed_messages SET message_id=?,content_hash=? "
                        "WHERE provider=? AND message_id=?",
                        (message.message_id, digest, message.provider, legacy_id),
                    )
                row = self.connection.execute(
                    "SELECT * FROM processed_messages WHERE provider=? AND message_id=?",
                    (message.provider, message.message_id),
                ).fetchone()
        if row is None:
            return ProcessingDecision(True, "new", digest)
        if reprocess:
            return ProcessingDecision(True, "reprocess", digest)
        if row["content_hash"] != digest:
            return ProcessingDecision(True, "content_changed", digest)
        status = row["processing_status"]
        if status == "retryable":
            return ProcessingDecision(True, "retryable", digest)
        if status == "failed":
            return ProcessingDecision(retry_failed, "failed", digest)
        if status == "processing":
            started = row["processing_started_at"]
            stale = not started
            if started:
                try:
                    stale = datetime.fromisoformat(started) <= _now() - stale_after
                except ValueError:
                    stale = True
            return ProcessingDecision(stale, "stale" if stale else "processing", digest)
        return ProcessingDecision(False, "already_processed", digest)

    def mark_processing(
        self, message: EmailMessage, *, run_id: str, analysis_mode: str,
        model_name: str | None, digest: str | None = None,
        message_source_ref: str | None = None,
    ) -> None:
        now = _timestamp()
        digest = digest or content_hash(message)
        with self.connection:
            self.connection.execute(
                """
                INSERT INTO processed_messages (
                    provider,message_id,thread_id,received_at,subject_hash,
                    content_hash,first_seen_at,processing_started_at,
                    processing_status,analysis_mode,model_name,source_run_id,
                    message_source_ref
                ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
                ON CONFLICT(provider,message_id) DO UPDATE SET
                    thread_id=excluded.thread_id,
                    received_at=excluded.received_at,
                    subject_hash=excluded.subject_hash,
                    content_hash=excluded.content_hash,
                    processing_started_at=excluded.processing_started_at,
                    processing_status='processing',
                    analysis_mode=excluded.analysis_mode,
                    model_name=excluded.model_name,
                    source_run_id=excluded.source_run_id,
                    message_source_ref=COALESCE(
                        excluded.message_source_ref,
                        processed_messages.message_source_ref
                    ),
                    retry_count=processed_messages.retry_count + 1,
                    last_error_type=NULL,last_error_message=NULL
                """,
                (message.provider, message.message_id, message.thread_id,
                 message.received_at, subject_hash(message), digest, now, now,
                 "processing", analysis_mode, model_name, run_id,
                 message_source_ref),
            )

    def mark_result(
        self, message: EmailMessage, *, status: str,
        final_classification: str | None = None,
        candidate_id: str | None = None, mail_action_id: str | None = None,
        calendar_action_id: str | None = None,
        prompt_template_version: str | None = None,
        schema_version: str | None = None,
        source_run_id: str | None = None,
        source_jsonl_path: str | None = None,
        error: BaseException | str | None = None,
    ) -> None:
        if status not in {"processed", "skipped", "failed", "retryable"}:
            raise ValueError(f"unsupported processing status: {status}")
        error_type = (
            type(error).__name__
            if isinstance(error, BaseException)
            else "ProcessingError" if error else None
        )
        error_message = str(error)[:500] if error else None
        with self.connection:
            self.connection.execute(
                """
                UPDATE processed_messages SET processing_status=?,
                    processing_started_at=NULL,last_processed_at=?,
                    final_classification=?,candidate_id=COALESCE(?,candidate_id),
                    mail_action_id=COALESCE(?,mail_action_id),
                    calendar_action_id=COALESCE(?,calendar_action_id),
                    prompt_template_version=COALESCE(?,prompt_template_version),
                    schema_version=COALESCE(?,schema_version),
                    source_run_id=COALESCE(?,source_run_id),
                    source_jsonl_path=COALESCE(?,source_jsonl_path),
                    last_error_type=?,last_error_message=?
                WHERE provider=? AND message_id=?
                """,
                (status, _timestamp(), final_classification, candidate_id,
                 mail_action_id, calendar_action_id, prompt_template_version,
                 schema_version, source_run_id, source_jsonl_path,
                 error_type, error_message,
                 message.provider, message.message_id),
            )

    def candidate_seen(self, candidate_id: str) -> bool:
        return self.connection.execute(
            "SELECT 1 FROM processed_messages WHERE candidate_id=? "
            "AND last_processed_at IS NOT NULL LIMIT 1", (candidate_id,)
        ).fetchone() is not None

    def start_run(self, run_id: str, *, provider: str, analysis_mode: str,
                  model_name: str | None, fetched: int, new: int,
                  skipped: int) -> str:
        with self.connection:
            self.connection.execute(
                "INSERT INTO runs(run_id,started_at,provider,analysis_mode,model_name,"
                "fetched_messages,new_messages,skipped_messages,status) "
                "VALUES(?,?,?,?,?,?,?,?, 'running')",
                (run_id, _timestamp(), provider, analysis_mode, model_name,
                 fetched, new, skipped),
            )
        return run_id

    def finish_run(self, run_id: str, **metrics: Any) -> None:
        row = self.connection.execute(
            "SELECT started_at FROM runs WHERE run_id=?", (run_id,)
        ).fetchone()
        if row is None:
            raise ValueError(f"run not found: {run_id}")
        finished = _now()
        started = datetime.fromisoformat(row["started_at"])
        allowed = {
            "processed_messages", "failed_messages", "calendar_candidates",
            "clarification_required", "promotions", "informational",
            "security_notifications", "invalid", "status",
        }
        values = {key: value for key, value in metrics.items() if key in allowed}
        values.setdefault("status", "completed")
        values.update(finished_at=_timestamp(finished),
                      duration_ms=max(0, int((finished - started).total_seconds() * 1000)))
        assignments = ",".join(f"{key}=?" for key in values)
        with self.connection:
            self.connection.execute(
                f"UPDATE runs SET {assignments} WHERE run_id=?",
                (*values.values(), run_id),
            )

    def summary(self) -> dict[str, Any]:
        counts = {
            row["processing_status"]: row["count"]
            for row in self.connection.execute(
                "SELECT processing_status,COUNT(*) count FROM processed_messages "
                "GROUP BY processing_status"
            )
        }
        last = self.connection.execute(
            "SELECT run_id,finished_at,duration_ms FROM runs "
            "WHERE status IN ('completed','completed_with_errors') "
            "ORDER BY started_at DESC LIMIT 1"
        ).fetchone()
        processed_at = self.connection.execute(
            "SELECT MAX(last_processed_at) value FROM processed_messages"
        ).fetchone()["value"]
        return {
            "total": sum(counts.values()), "counts": counts,
            "last_successful_run": last["finished_at"] if last else None,
            "last_run_duration_ms": last["duration_ms"] if last else None,
            "last_processed_message_time": processed_at,
            "provider_results": (
                [dict(row) for row in self.connection.execute(
                    "SELECT provider,status,fetched_messages,error_type "
                    "FROM provider_runs WHERE run_id=? ORDER BY provider",
                    (last["run_id"],),
                )]
                if last else []
            ),
        }

    def recent(self, limit: int = 20) -> list[sqlite3.Row]:
        return list(self.connection.execute(
            "SELECT provider,message_id,received_at,processing_status,"
            "final_classification,last_processed_at,retry_count,last_error_type "
            "FROM processed_messages ORDER BY first_seen_at DESC LIMIT ?", (limit,)
        ))

    def processed_message(
        self, provider: str, message_id: str
    ) -> sqlite3.Row | None:
        return self.connection.execute(
            "SELECT * FROM processed_messages WHERE provider=? AND message_id=?",
            (provider, message_id),
        ).fetchone()

    def review_candidates(
        self,
        *,
        reviewer_model: str,
        review_version: str,
        since: str | None = None,
        include_reviewed: bool = False,
        limit: int | None = None,
    ) -> list[sqlite3.Row]:
        query = [
            "SELECT pm.* FROM processed_messages pm ",
            "LEFT JOIN review_cases rc ON rc.provider=pm.provider "
            "AND rc.message_id=pm.message_id "
            "AND rc.reviewer_model=? AND rc.review_version=? ",
            "WHERE pm.processing_status='processed' "
            "AND pm.source_jsonl_path IS NOT NULL ",
        ]
        parameters: list[Any] = [reviewer_model, review_version]
        if not include_reviewed:
            query.append("AND rc.review_case_id IS NULL ")
        if since is not None:
            query.append("AND pm.last_processed_at>=? ")
            parameters.append(since)
        query.append(
            "ORDER BY COALESCE(pm.last_processed_at, pm.first_seen_at) DESC "
        )
        if limit is not None:
            query.append("LIMIT ?")
            parameters.append(limit)
        return list(
            self.connection.execute("".join(query), tuple(parameters))
        )

    def next_review_case_id(self) -> str:
        with self.connection:
            value = self.connection.execute(
                "INSERT INTO review_case_id_sequence DEFAULT VALUES"
            ).lastrowid
        return f"RV-{int(value):06d}"

    def retryable_message_ids(
        self, *, provider: str = "outlook", limit: int = 50
    ) -> list[str]:
        return [
            str(row["message_id"])
            for row in self.connection.execute(
                "SELECT message_id FROM processed_messages "
                "WHERE provider=? AND processing_status='retryable' "
                "ORDER BY last_processed_at ASC LIMIT ?",
                (provider, max(0, limit)),
            )
        ]

    def reset_message(self, provider: str, message_id: str) -> bool:
        with self.connection:
            cursor = self.connection.execute(
                "DELETE FROM processed_messages WHERE provider=? AND message_id=?",
                (provider, message_id),
            )
        return cursor.rowcount == 1
