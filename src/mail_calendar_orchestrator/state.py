from __future__ import annotations

import hashlib
import json
import os
import sqlite3
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

from mail_to_calendar.models import EmailMessage


DEFAULT_STATE_DB = Path("~/.local/share/agentledger/mail_state.sqlite3")


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
                """
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
    ) -> None:
        now = _timestamp()
        digest = digest or content_hash(message)
        with self.connection:
            self.connection.execute(
                """
                INSERT INTO processed_messages (
                    provider,message_id,thread_id,received_at,subject_hash,
                    content_hash,first_seen_at,processing_started_at,
                    processing_status,analysis_mode,model_name,source_run_id
                ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
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
                    retry_count=processed_messages.retry_count + 1,
                    last_error_type=NULL,last_error_message=NULL
                """,
                (message.provider, message.message_id, message.thread_id,
                 message.received_at, subject_hash(message), digest, now, now,
                 "processing", analysis_mode, model_name, run_id),
            )

    def mark_result(
        self, message: EmailMessage, *, status: str,
        final_classification: str | None = None,
        candidate_id: str | None = None, mail_action_id: str | None = None,
        calendar_action_id: str | None = None,
        prompt_template_version: str | None = None,
        schema_version: str | None = None,
        source_run_id: str | None = None,
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
                    last_error_type=?,last_error_message=?
                WHERE provider=? AND message_id=?
                """,
                (status, _timestamp(), final_classification, candidate_id,
                 mail_action_id, calendar_action_id, prompt_template_version,
                 schema_version, source_run_id, error_type, error_message,
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
            "SELECT finished_at,duration_ms FROM runs WHERE status='completed' "
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
        }

    def recent(self, limit: int = 20) -> list[sqlite3.Row]:
        return list(self.connection.execute(
            "SELECT provider,message_id,received_at,processing_status,"
            "final_classification,last_processed_at,retry_count,last_error_type "
            "FROM processed_messages ORDER BY first_seen_at DESC LIMIT ?", (limit,)
        ))

    def reset_message(self, provider: str, message_id: str) -> bool:
        with self.connection:
            cursor = self.connection.execute(
                "DELETE FROM processed_messages WHERE provider=? AND message_id=?",
                (provider, message_id),
            )
        return cursor.rowcount == 1
