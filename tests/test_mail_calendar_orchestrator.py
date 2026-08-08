from __future__ import annotations

import json
from pathlib import Path

import pytest

from agentledger.bundles import build_action_bundles, outcome_status
from agentledger.html import render_explorer
from agentledger.ingestion import read_jsonl
from agentledger.normalizer import normalize_events
from mail_calendar_orchestrator.cli import main
from mail_calendar_orchestrator.service import MailCalendarOrchestrator
from mail_to_calendar.local_provider import LocalMailProvider


SAMPLE = (
    Path(__file__).parents[1]
    / "examples"
    / "mail_to_calendar"
    / "sample_messages.jsonl"
)


def _process(tmp_path, *, requires_approval: bool = True):
    return MailCalendarOrchestrator(
        base_year=2026,
        timezone="Asia/Tokyo",
    ).process(
        SAMPLE,
        tmp_path / (
            "pending.jsonl" if requires_approval else "confirmed.jsonl"
        ),
        requires_approval=requires_approval,
    )


def _calendar_events(events: list[dict]) -> list[dict]:
    return [
        event
        for event in events
        if event.get("agent_id") == "calendar_agent"
    ]


def test_e2e_routes_only_ready_candidates_to_calendar(tmp_path) -> None:
    result = _process(tmp_path)
    calendar_events = _calendar_events(result.events)
    calendar_message_ids = {
        event["metadata"]["source_message_id"]
        for event in calendar_events
    }

    assert result.processed_messages == 5
    assert result.important_messages == 4
    assert result.ignored_messages == 1
    assert result.candidates == 4
    assert result.ready_candidates == 2
    assert result.clarification_required == 1
    assert result.unsupported_candidates == 1
    assert result.calendar_proposals == 2
    assert result.pending_calendar_actions == 2
    assert result.confirmed_calendar_actions == 0
    assert result.generated_events == 26
    assert calendar_message_ids == {
        "mail-meeting-001",
        "mail-outlook-001",
    }
    assert "mail-promotion-001" not in calendar_message_ids
    assert "mail-ambiguous-001" not in calendar_message_ids
    assert "mail-deadline-001" not in calendar_message_ids

    statuses = {
        event["metadata"]["source_message_id"]: event["metadata"][
            "orchestration_status"
        ]
        for event in result.events
        if event.get("agent_id") == "mail_to_calendar_agent"
    }
    assert statuses == {
        "mail-meeting-001": "ready",
        "mail-deadline-001": "unsupported",
        "mail-promotion-001": "ignored",
        "mail-ambiguous-001": "clarification_required",
        "mail-outlook-001": "ready",
    }
    unsupported = next(
        event
        for event in result.events
        if event.get("event_type") == "action_executed"
        and event.get("metadata", {}).get("source_message_id")
        == "mail-deadline-001"
    )
    assert unsupported["action_parameters"]["start"] is None
    assert unsupported["metadata"]["calendar_conversion_error"] == (
        "candidate start time is required"
    )


def test_approval_required_calendar_bundles_are_pending(tmp_path) -> None:
    result = _process(tmp_path)
    bundles = build_action_bundles(normalize_events(result.events))
    calendar_bundles = [
        bundle
        for bundle in bundles
        if bundle.action.actor_id == "calendar_agent"
    ]

    assert len(calendar_bundles) == 2
    assert all(bundle.outcome is None for bundle in calendar_bundles)
    assert all(
        outcome_status(bundle) == "pending" for bundle in calendar_bundles
    )
    assert all(
        bundle.action.action_type == "create_calendar_event"
        for bundle in calendar_bundles
    )


def test_no_approval_calendar_bundles_are_confirmed(tmp_path) -> None:
    result = _process(tmp_path, requires_approval=False)
    bundles = build_action_bundles(normalize_events(result.events))
    calendar_bundles = [
        bundle
        for bundle in bundles
        if bundle.action.actor_id == "calendar_agent"
    ]

    assert result.generated_events == 28
    assert result.pending_calendar_actions == 0
    assert result.confirmed_calendar_actions == 2
    assert len(calendar_bundles) == 2
    assert all(bundle.outcome is not None for bundle in calendar_bundles)
    assert all(
        outcome_status(bundle) == "confirmed"
        for bundle in calendar_bundles
    )


def test_calendar_events_keep_parent_mail_relationships(tmp_path) -> None:
    result = _process(tmp_path)
    meeting_mail_action = next(
        event
        for event in result.events
        if event.get("event_type") == "action_executed"
        and event.get("agent_id") == "mail_to_calendar_agent"
        and event["metadata"]["source_message_id"] == "mail-meeting-001"
    )
    meeting_calendar = [
        event
        for event in _calendar_events(result.events)
        if event["metadata"]["source_message_id"] == "mail-meeting-001"
    ]

    assert len(meeting_calendar) == 3
    for event in meeting_calendar:
        metadata = event["metadata"]
        assert metadata["source_provider"] == "local"
        assert metadata["source_message_id"] == "mail-meeting-001"
        assert metadata["source_thread_id"] == "thread-meeting-001"
        assert metadata["mail_candidate_id"].startswith(
            "calendar-candidate-"
        )
        assert metadata["parent_mail_action_id"] == (
            meeting_mail_action["action_id"]
        )
        assert metadata["parent_mail_decision_id"] == (
            meeting_mail_action["decision_id"]
        )
        assert metadata["parent_mail_correlation_id"] == (
            meeting_mail_action["correlation_id"]
        )
        assert event["case_id"] == metadata["mail_candidate_id"]
        assert event["case_type"] == "calendar_candidate"


def test_integrated_output_is_deterministic_private_and_explorable(
    tmp_path,
) -> None:
    first = MailCalendarOrchestrator(base_year=2026).process(
        SAMPLE,
        tmp_path / "first.jsonl",
    )
    second = MailCalendarOrchestrator(base_year=2026).process(
        SAMPLE,
        tmp_path / "second.jsonl",
    )

    assert [event["event_id"] for event in first.events] == [
        event["event_id"] for event in second.events
    ]
    assert (tmp_path / "first.jsonl").read_text(encoding="utf-8") == (
        tmp_path / "second.jsonl"
    ).read_text(encoding="utf-8")

    serialized = json.dumps(first.events, ensure_ascii=False)
    for message in LocalMailProvider(SAMPLE).list_messages():
        assert message.body_text not in serialized
    assert "gmail_history_id" not in serialized
    assert "outlook_conversation_index" not in serialized
    previews = [
        event["observation"]["body_preview"]
        for event in first.events
        if event.get("agent_id") == "mail_to_calendar_agent"
        and event.get("event_type") == "observation_received"
    ]
    assert all(value is None or len(value) <= 160 for value in previews)

    ingestion = read_jsonl(first.output_path)
    normalized = normalize_events(ingestion.events)
    bundles = build_action_bundles(normalized)
    html = render_explorer(
        normalized,
        ingestion=ingestion,
        source_path=first.output_path,
    )
    assert ingestion.loaded == 26
    assert ingestion.issues == []
    assert len(bundles) == 7
    for value in (
        "ignore_email",
        "request_clarification",
        "propose_calendar_candidate",
        "create_calendar_event",
        "source_message_id",
        "parent_mail_action_id",
        '"status": "pending"',
    ):
        assert value in html


def test_bad_input_and_same_output_path_fail_without_partial_output(
    tmp_path,
) -> None:
    malformed = tmp_path / "malformed.jsonl"
    malformed.write_text('{"provider":"local"}\n{bad}\n', encoding="utf-8")
    output = tmp_path / "output.jsonl"
    service = MailCalendarOrchestrator(base_year=2026)

    with pytest.raises(ValueError, match="invalid JSON on line 2"):
        service.process(malformed, output)
    assert not output.exists()

    with pytest.raises(ValueError, match="different files"):
        service.process(SAMPLE, SAMPLE)


def test_cli_prints_non_overlapping_summary(
    tmp_path,
    monkeypatch,
    capsys,
) -> None:
    output = tmp_path / "flow.jsonl"
    monkeypatch.setattr(
        "sys.argv",
        [
            "mail-calendar-orchestrator",
            "process",
            "--input",
            str(SAMPLE),
            "--output",
            str(output),
            "--base-year",
            "2026",
            "--timezone",
            "Asia/Tokyo",
            "--analysis-mode",
            "rule-only",
            "--requires-approval",
            "--no-state",
        ],
    )

    main()

    summary = capsys.readouterr().out
    for line in (
        "Processed messages: 5",
        "Important messages: 4",
        "Ignored messages: 1",
        "Candidates: 4",
        "Ready for calendar: 2",
        "Clarification required: 1",
        "Unsupported: 1",
        "Calendar proposals: 2",
        "Pending calendar actions: 2",
        "Confirmed calendar actions: 0",
        "Generated events: 26",
    ):
        assert line in summary
    assert read_jsonl(output).loaded == 26
