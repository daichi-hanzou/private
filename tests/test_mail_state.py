from __future__ import annotations

import json
import sqlite3
from datetime import datetime, timedelta, timezone

import pytest

from mail_calendar_orchestrator.cli import main
from mail_calendar_orchestrator.service import MailCalendarOrchestrator
from mail_calendar_orchestrator.state import MailStateStore, content_hash
from mail_to_calendar.models import EmailMessage


class FakeProvider:
    def __init__(self, messages: list[EmailMessage]) -> None:
        self.messages = messages

    def list_messages(self) -> list[EmailMessage]:
        return self.messages


def message(message_id: str = "mail-1", body: str = "8月12日 13:00に歯科予約") -> EmailMessage:
    return EmailMessage(
        provider="outlook", message_id=message_id, thread_id="thread-1",
        sender="clinic@example.test", recipients=["user@example.test"],
        subject="歯科予約", received_at="2026-08-08T10:00:00+09:00",
        body_text=body,
    )


def test_hash_normalizes_whitespace_and_state_decisions(tmp_path) -> None:
    db = tmp_path / "state.sqlite3"
    original = message(body="予約は\n  8月12日  13:00です")
    whitespace = message(body="予約は 8月12日 13:00です")
    changed = message(body="予約は8月12日 15:00です")
    assert content_hash(original) == content_hash(whitespace)

    with MailStateStore(db) as store:
        assert store.decision(original).reason == "new"
        store.mark_processing(original, run_id="run-1", analysis_mode="llm-first", model_name="qwen")
        store.mark_result(original, status="processed")
        assert not store.decision(whitespace).process
        assert store.decision(changed).reason == "content_changed"
        assert store.decision(original, reprocess=True).process


def test_legacy_outlook_id_migrates_without_reprocessing(tmp_path) -> None:
    legacy = message("legacy-id")
    canonical = EmailMessage(**{
        **legacy.__dict__, "message_id": "outlook:legacy-id"
    })
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        store.mark_processing(
            legacy, run_id="old-run", analysis_mode="llm-first",
            model_name="qwen3:8b",
        )
        store.mark_result(legacy, status="processed")
        decision = store.decision(canonical)
        assert not decision.process
        assert decision.reason == "already_processed"
        row = store.connection.execute(
            "SELECT message_id FROM processed_messages WHERE provider='outlook'"
        ).fetchone()
        assert row["message_id"] == "outlook:legacy-id"


def test_retry_failed_retryable_and_stale_transitions(tmp_path) -> None:
    item = message()
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        store.mark_processing(item, run_id="run-1", analysis_mode="llm-first", model_name="qwen")
        store.mark_result(item, status="retryable", error=TimeoutError("temporary"))
        assert store.decision(item).process

        store.mark_processing(item, run_id="run-2", analysis_mode="llm-first", model_name="qwen")
        store.mark_result(item, status="failed", error=ValueError("bad schema"))
        assert not store.decision(item).process
        assert store.decision(item, retry_failed=True).process

        store.mark_processing(item, run_id="run-3", analysis_mode="llm-first", model_name="qwen")
        store.connection.execute(
            "UPDATE processed_messages SET processing_started_at=?",
            ((datetime.now(timezone.utc) - timedelta(minutes=31)).isoformat(),),
        )
        store.connection.commit()
        assert store.decision(item).reason == "stale"


def test_llm_failure_classification_distinguishes_temporary_and_permanent() -> None:
    classify = MailCalendarOrchestrator._is_retryable_llm_failure
    assert classify("Ollama request timed out")
    assert classify("connection refused")
    assert classify("HTTP 429")
    assert classify("HTTP 503")
    assert not classify("invalid JSON")
    assert not classify("schema mismatch")
    assert not classify("model not found")


def test_batch_dedup_second_run_and_new_message(tmp_path) -> None:
    db = tmp_path / "state.sqlite3"
    output = tmp_path / "first.jsonl"
    item = message()
    with MailStateStore(db) as store:
        service = MailCalendarOrchestrator(
            base_year=2026, state_store=store, analysis_mode="rule-only"
        )
        first = service.process_provider(FakeProvider([item, item]), output)
        assert first.fetched_messages == 2
        assert first.new_messages == 1
        assert first.skipped_messages == 1
        assert output.exists()

        untouched = output.read_bytes()
        second = service.process_provider(FakeProvider([item]), output)
        assert second.new_messages == 0
        assert second.skipped_messages == 1
        assert output.read_bytes() == untouched

        absent = tmp_path / "no-new.jsonl"
        no_new = service.process_provider(FakeProvider([item]), absent)
        assert no_new.new_messages == 0
        assert not absent.exists()

        third = service.process_provider(
            FakeProvider([item, message("mail-2", "8月13日 14:00に面談")]),
            tmp_path / "third.jsonl",
        )
        assert third.new_messages == 1
        assert third.skipped_messages == 1
        assert store.summary()["counts"]["processed"] == 2


def test_reprocess_does_not_duplicate_calendar_proposal(tmp_path) -> None:
    item = message()
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        service = MailCalendarOrchestrator(
            base_year=2026, state_store=store, analysis_mode="rule-only"
        )
        first = service.process_provider(FakeProvider([item]), tmp_path / "first.jsonl")
        second = service.process_provider(
            FakeProvider([item]), tmp_path / "second.jsonl", reprocess=True
        )
        assert first.calendar_proposals == 1
        assert second.calendar_proposals == 0
        assert not [event for event in second.events if event.get("agent_id") == "calendar_agent"]


def test_privacy_schema_and_run_metrics(tmp_path) -> None:
    item = message(body="PRIVATE BODY TOKEN refresh_token raw llm response")
    db = tmp_path / "state.sqlite3"
    with MailStateStore(db) as store:
        service = MailCalendarOrchestrator(
            base_year=2026, state_store=store, analysis_mode="rule-only"
        )
        result = service.process_provider(FakeProvider([item]), tmp_path / "audit.jsonl")
        row = store.connection.execute("SELECT * FROM processed_messages").fetchone()
        assert row["processing_status"] == "processed"
        assert row["mail_action_id"]
        run = store.connection.execute("SELECT * FROM runs WHERE run_id=?", (result.run_id,)).fetchone()
        assert run["status"] == "completed"
        assert run["processed_messages"] == 1
        assert run["duration_ms"] >= 0

    raw = db.read_bytes()
    for secret in (b"PRIVATE BODY", b"refresh_token", b"raw llm response"):
        assert secret not in raw
    assert db.stat().st_mode & 0o777 == 0o600


def test_atomic_write_failure_does_not_mark_processed(tmp_path, monkeypatch) -> None:
    item = message()
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        service = MailCalendarOrchestrator(
            base_year=2026, state_store=store, analysis_mode="rule-only"
        )
        monkeypatch.setattr(
            "mail_calendar_orchestrator.service.write_jsonl_atomic",
            lambda *_: (_ for _ in ()).throw(OSError("disk full")),
        )
        with pytest.raises(OSError, match="disk full"):
            service.process_provider(FakeProvider([item]), tmp_path / "audit.jsonl")
        row = store.connection.execute("SELECT processing_status FROM processed_messages").fetchone()
        assert row["processing_status"] == "retryable"
        run = store.connection.execute("SELECT status FROM runs").fetchone()
        assert run["status"] == "failed"


def test_state_cli_summary_recent_and_reset(tmp_path, monkeypatch, capsys) -> None:
    db = tmp_path / "state.sqlite3"
    with MailStateStore(db) as store:
        item = message()
        store.mark_processing(item, run_id="run", analysis_mode="rule-only", model_name=None)
        store.mark_result(item, status="processed")

    monkeypatch.setattr("sys.argv", ["mail-calendar-orchestrator", "state", "--state-db", str(db), "summary"])
    main()
    assert "Processed: 1" in capsys.readouterr().out
    monkeypatch.setattr("sys.argv", ["mail-calendar-orchestrator", "state", "--state-db", str(db), "recent", "--limit", "1"])
    main()
    assert "mail-1" in capsys.readouterr().out
    monkeypatch.setattr("sys.argv", ["mail-calendar-orchestrator", "state", "--state-db", str(db), "reset-message", "--provider", "outlook", "--message-id", "mail-1"])
    main()
    with sqlite3.connect(db) as connection:
        assert connection.execute("SELECT COUNT(*) FROM processed_messages").fetchone()[0] == 0


def test_no_state_cli_keeps_previous_behavior(tmp_path, monkeypatch, capsys) -> None:
    source = tmp_path / "mail.jsonl"
    source.write_text(json.dumps(message().__dict__, ensure_ascii=False) + "\n", encoding="utf-8")
    output = tmp_path / "audit.jsonl"
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "process", "--input", str(source),
        "--output", str(output), "--base-year", "2026",
        "--analysis-mode", "rule-only", "--no-state",
    ])
    main()
    summary = capsys.readouterr().out
    assert "State DB: disabled" in summary
    assert output.exists()
