from __future__ import annotations

import json
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta, timezone

import pytest

from agentledger.bundles import build_action_bundles, outcome_status
from agentledger.html import render_explorer
from agentledger.ingestion import read_jsonl
from agentledger.normalizer import normalize_events
from calendar_agent.agent import CalendarAgent
from calendar_agent.models import CalendarRequest
from mail_calendar_orchestrator.approvals import ApprovalService
from mail_calendar_orchestrator.audit import write_jsonl_atomic
from mail_calendar_orchestrator.cli import main
from mail_calendar_orchestrator.service import MailCalendarOrchestrator
from mail_calendar_orchestrator.state import MailStateStore
from mail_to_calendar.models import EmailMessage


NOW = datetime(2026, 8, 8, 12, 0, tzinfo=timezone.utc)


def pending_events(*, requires_approval: bool = True) -> list[dict]:
    return CalendarAgent().propose(CalendarRequest(
        title="Dental appointment", requested_date="2026-08-12",
        preferred_period="afternoon", duration_minutes=60,
        available_slots=["13:00"], requires_approval=requires_approval,
    ))


def register(tmp_path, *, now: datetime = NOW):
    store = MailStateStore(tmp_path / "state.sqlite3")
    source = write_jsonl_atomic(tmp_path / "pending.jsonl", pending_events())
    record = ApprovalService(store).register_pending(
        pending_events(), source, now=now
    )[0]
    return store, source, record


def test_pending_register_duplicate_confirmed_list_show_summary(tmp_path) -> None:
    store, source, record = register(tmp_path)
    try:
        service = ApprovalService(store)
        assert record.approval_id == "AP-000001"
        assert record.status == "awaiting_approval"
        assert record.calendar_action_id
        assert record.title == "Dental appointment"
        assert record.date == "2026-08-12"
        assert record.start == "13:00"
        assert "earliest available time" in record.classification_summary
        assert record.expires_at == NOW + timedelta(hours=72)
        assert service.register_pending(pending_events(), source, now=NOW) == []
        confirmed = pending_events(requires_approval=False)
        confirmed_path = write_jsonl_atomic(tmp_path / "confirmed.jsonl", confirmed)
        assert service.register_pending(confirmed, confirmed_path, now=NOW) == []
        assert service.list() == [record]
        assert service.get(record.approval_id) == record
        summary = service.summary()
        assert summary["awaiting_approval"] == 1
        assert summary["approved"] == 0
        assert summary["oldest_pending"] == NOW.isoformat()
    finally:
        store.close()


def test_approve_writes_new_audit_and_keeps_source(tmp_path) -> None:
    store, source, record = register(tmp_path)
    original = source.read_bytes()
    try:
        approved = ApprovalService(store).approve(
            record.approval_id, "daichi", "Approved"
        )
        assert approved.status == "approved"
        assert approved.actor == "daichi"
        assert approved.reason == "Approved"
        assert approved.approved_at is not None
        assert approved.outcome_jsonl_path
        assert source.read_bytes() == original
        assert approved.outcome_jsonl_path != str(source)

        ingestion = read_jsonl(approved.outcome_jsonl_path)
        normalized = normalize_events(ingestion.events)
        bundle = build_action_bundles(normalized)[0]
        assert ingestion.loaded == 5
        assert len(bundle.human_interventions) == 1
        assert bundle.human_interventions[0].intervention_type == "accept"
        assert bundle.outcome is not None
        assert outcome_status(bundle) == "confirmed"
        assert ingestion.events[-1]["actual_outcome"]["outcome_type"] == (
            "calendar_event_approved"
        )
        html = render_explorer(
            normalized, ingestion=ingestion,
            source_path=approved.outcome_jsonl_path,
        )
        assert "Human Intervention" in html
        assert "calendar_event_approved" in html
    finally:
        store.close()


def test_reject_writes_contradicted_outcome(tmp_path) -> None:
    store, _, record = register(tmp_path)
    try:
        rejected = ApprovalService(store).reject(
            record.approval_id, "daichi", "Not relevant"
        )
        assert rejected.status == "rejected"
        ingestion = read_jsonl(rejected.outcome_jsonl_path)
        bundle = build_action_bundles(normalize_events(ingestion.events))[0]
        assert bundle.human_interventions[0].intervention_type == "reject"
        assert outcome_status(bundle) == "contradicted"
        assert ingestion.events[-1]["actual_outcome"]["outcome_type"] == (
            "calendar_event_rejected"
        )
    finally:
        store.close()


@pytest.mark.parametrize("first,second", [
    ("approve", "approve"), ("approve", "reject"), ("reject", "approve")
])
def test_resolved_approval_cannot_transition_again(tmp_path, first, second) -> None:
    store, _, record = register(tmp_path)
    try:
        service = ApprovalService(store)
        getattr(service, first)(record.approval_id, "actor", "reason")
        with pytest.raises(ValueError, match="not awaiting approval"):
            getattr(service, second)(record.approval_id, "actor", "reason")
    finally:
        store.close()


def test_expire_only_due_pending_and_blocks_approve(tmp_path) -> None:
    store, _, record = register(tmp_path)
    try:
        service = ApprovalService(store)
        assert service.expire(now=NOW + timedelta(hours=71)) == 0
        assert service.expire(now=NOW + timedelta(hours=72)) == 1
        assert service.get(record.approval_id).status == "expired"
        assert service.expire(now=NOW + timedelta(hours=100)) == 0
        with pytest.raises(ValueError, match="not awaiting approval"):
            service.approve(record.approval_id, "actor", "reason")
    finally:
        store.close()


def test_source_integrity_rejects_missing_action_outcome_and_intervention(tmp_path) -> None:
    for kind in ("missing", "outcome", "intervention"):
        directory = tmp_path / kind
        directory.mkdir()
        store, source, record = register(directory)
        try:
            events = pending_events()
            if kind == "missing":
                events = events[:2]
            elif kind == "outcome":
                events.append({
                    "event_type": "outcome_observed",
                    "action_id": record.calendar_action_id,
                    "event_id": "existing-outcome",
                })
            else:
                events.append({
                    "event_type": "human_intervention",
                    "related_action_id": record.calendar_action_id,
                    "event_id": "existing-intervention",
                })
            write_jsonl_atomic(source, events)
            with pytest.raises(ValueError, match="source"):
                ApprovalService(store).approve(record.approval_id, "actor", "reason")
            assert ApprovalService(store).get(record.approval_id).status == "awaiting_approval"
        finally:
            store.close()


def test_concurrent_approve_only_one_succeeds(tmp_path) -> None:
    store, _, record = register(tmp_path)
    db = store.path
    store.close()

    def approve() -> str:
        with MailStateStore(db) as local:
            try:
                ApprovalService(local).approve(record.approval_id, "actor", "reason")
                return "approved"
            except ValueError:
                return "rejected"

    with ThreadPoolExecutor(max_workers=2) as executor:
        results = list(executor.map(lambda _: approve(), range(2)))
    assert sorted(results) == ["approved", "rejected"]


def test_orchestrator_registers_only_pending_and_preserves_privacy(tmp_path) -> None:
    private_body = "PRIVATE BODY access_token raw llm response"
    message = EmailMessage(
        provider="outlook", message_id="mail-1", thread_id="thread",
        sender="sender@example.test", recipients=["user@example.test"],
        subject="Dental appointment", received_at="2026-08-08T10:00:00Z",
        body_text="8月12日13時に歯科予約 " + private_body,
    )

    class Provider:
        def list_messages(self):
            return [message]

    db = tmp_path / "state.sqlite3"
    with MailStateStore(db) as store:
        service = MailCalendarOrchestrator(
            base_year=2026, state_store=store, analysis_mode="rule-only"
        )
        first = service.process_provider(Provider(), tmp_path / "run.jsonl")
        assert first.approvals_created == 1
        assert ApprovalService(store).summary()["awaiting_approval"] == 1
        second = service.process_provider(Provider(), tmp_path / "second.jsonl")
        assert second.approvals_created == 0
    raw = db.read_bytes()
    assert private_body.encode() not in raw
    assert b"access_token" not in raw
    assert b"raw llm response" not in raw


def test_cli_list_show_summary_approve_reject_expire(tmp_path, monkeypatch, capsys) -> None:
    store, _, record = register(tmp_path)
    db = store.path
    store.close()
    base = [
        "mail-calendar-orchestrator", "approvals", "--state-db", str(db),
        "--output-dir", str(tmp_path / "approval-output"),
    ]
    for command, expected in (
        (["list"], record.approval_id),
        (["show", record.approval_id], "Dental appointment"),
        (["summary"], "Awaiting Approval: 1"),
    ):
        monkeypatch.setattr("sys.argv", [*base, *command])
        main()
        assert expected in capsys.readouterr().out
    monkeypatch.setattr("sys.argv", [
        *base, "approve", record.approval_id, "--actor", "daichi",
        "--reason", "Approved",
    ])
    main()
    assert "Approved:" in capsys.readouterr().out

    other = tmp_path / "other"
    other.mkdir()
    other_store, _, rejected = register(other)
    other_db = other_store.path
    other_store.close()
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "approvals", "--state-db", str(other_db),
        "--output-dir", str(other / "approval-output"),
        "reject", rejected.approval_id, "--actor", "daichi", "--reason", "No",
    ])
    main()
    assert "Rejected:" in capsys.readouterr().out

    expiring = tmp_path / "expiring"
    expiring.mkdir()
    exp_store, _, _ = register(expiring, now=NOW - timedelta(hours=73))
    exp_db = exp_store.path
    exp_store.close()
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "approvals", "--state-db", str(exp_db), "expire"
    ])
    main()
    assert "Expired: 1" in capsys.readouterr().out
