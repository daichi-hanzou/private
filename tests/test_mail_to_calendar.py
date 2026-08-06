from __future__ import annotations

import json
from pathlib import Path

import pytest

from agentledger.bundles import build_action_bundles
from agentledger.html import render_explorer
from agentledger.ingestion import read_jsonl
from agentledger.normalizer import normalize_events
from calendar_agent.models import CalendarRequest
from mail_to_calendar.classifier import RuleBasedImportanceClassifier
from mail_to_calendar.cli import main
from mail_to_calendar.extractor import (
    RuleBasedCalendarExtractor,
    to_calendar_request,
)
from mail_to_calendar.local_provider import LocalMailProvider
from mail_to_calendar.models import EmailMessage
from mail_to_calendar.service import MailToCalendarService


SAMPLE = (
    Path(__file__).parents[1]
    / "examples"
    / "mail_to_calendar"
    / "sample_messages.jsonl"
)


def _message(
    *,
    subject: str,
    body: str = "",
    provider: str = "local",
    message_id: str = "message-1",
    hint: str | None = None,
) -> EmailMessage:
    return EmailMessage(
        provider=provider,
        message_id=message_id,
        thread_id="thread-1",
        sender="sender@example.com",
        recipients=["me@example.com"],
        subject=subject,
        received_at="2026-08-01T09:00:00+09:00",
        body_text=body,
        body_preview="Short preview",
        labels=[],
        importance_hint=hint,
        has_attachments=False,
        metadata={},
    )


def _classify_extract(message: EmailMessage):
    importance = RuleBasedImportanceClassifier().classify(message)
    candidate = RuleBasedCalendarExtractor(
        base_year=2026
    ).extract(message, importance)
    return importance, candidate


def test_local_provider_normalizes_gmail_and_outlook_messages() -> None:
    messages = LocalMailProvider(SAMPLE).list_messages()
    gmail = next(item for item in messages if item.provider == "gmail")
    outlook = next(item for item in messages if item.provider == "outlook")

    assert isinstance(gmail, EmailMessage)
    assert isinstance(outlook, EmailMessage)
    assert gmail.message_id == "mail-deadline-001"
    assert outlook.thread_id == "conversation-outlook-001"
    assert LocalMailProvider(SAMPLE).get_message(
        "mail-outlook-001"
    ) == outlook


def test_importance_rules_cover_meeting_deadline_and_promotion() -> None:
    classifier = RuleBasedImportanceClassifier()
    meeting = classifier.classify(
        _message(
            subject="8月12日 15時 定例会議",
            body="参加をお願いします。",
        )
    )
    deadline = classifier.classify(
        _message(subject="8月20日までに書類をご提出ください")
    )
    promotion = classifier.classify(
        _message(subject="期間限定セールのお知らせ")
    )

    assert meeting.is_important
    assert meeting.category == "meeting"
    assert meeting.reasons
    assert deadline.is_important
    assert deadline.category == "deadline"
    assert deadline.reasons
    assert not promotion.is_important
    assert promotion.category == "promotion"
    assert promotion.reasons


@pytest.mark.parametrize(
    ("subject", "expected_date", "expected_start"),
    [
        ("2026年8月12日15時 定例会議", "2026-08-12", "15:00"),
        ("8月12日 15:00 定例会議", "2026-08-12", "15:00"),
        ("8月12日 午後3時 定例会議", "2026-08-12", "15:00"),
    ],
)
def test_rule_based_japanese_datetime_extraction(
    subject: str,
    expected_date: str,
    expected_start: str,
) -> None:
    _, candidate = _classify_extract(_message(subject=subject))

    assert candidate is not None
    assert candidate.date == expected_date
    assert candidate.start == expected_start


def test_deadline_without_time_does_not_invent_time() -> None:
    _, candidate = _classify_extract(
        _message(subject="8月20日までに書類をご提出ください")
    )

    assert candidate is not None
    assert candidate.candidate_type == "deadline"
    assert candidate.date == "2026-08-20"
    assert candidate.start is None
    assert candidate.duration_minutes is None
    assert not candidate.clarification_required
    assert any("no time was inferred" in note for note in candidate.extraction_notes)


def test_ambiguous_relative_date_requires_clarification() -> None:
    _, candidate = _classify_extract(
        _message(
            subject="来週どこかで打ち合わせしましょう",
            hint="high",
        )
    )

    assert candidate is not None
    assert candidate.date is None
    assert candidate.start is None
    assert candidate.clarification_required
    assert any("ambiguous date" in note for note in candidate.extraction_notes)


def test_candidates_for_meeting_deadline_ambiguous_and_promotion() -> None:
    messages = LocalMailProvider(SAMPLE).list_messages()
    by_id = {}
    for message in messages:
        importance, candidate = _classify_extract(message)
        by_id[message.message_id] = (importance, candidate)

    meeting = by_id["mail-meeting-001"][1]
    deadline = by_id["mail-deadline-001"][1]
    ambiguous = by_id["mail-ambiguous-001"][1]
    promotion = by_id["mail-promotion-001"][1]
    assert meeting is not None
    assert meeting.title == "定例会議"
    assert meeting.date == "2026-08-12"
    assert meeting.start == "15:00"
    assert meeting.duration_minutes == 60
    assert meeting.requires_approval
    assert deadline is not None
    assert deadline.candidate_type == "deadline"
    assert ambiguous is not None and ambiguous.clarification_required
    assert promotion is None


def test_audit_events_are_private_and_agentledger_compatible() -> None:
    messages = LocalMailProvider(SAMPLE).list_messages()
    result = MailToCalendarService(base_year=2026).process(
        LocalMailProvider(SAMPLE)
    )
    serialized = json.dumps(result.events, ensure_ascii=False)

    assert result.processed == 5
    assert result.important == 4
    assert len(result.candidates) == 4
    assert result.clarification_required == 1
    assert result.ignored == 1
    assert len(result.events) == 20
    for message in messages:
        assert message.body_text not in serialized
        assert message.message_id in serialized
    previews = [
        event["observation"]["body_preview"]
        for event in result.events
        if event["event_type"] == "observation_received"
    ]
    assert all(value is None or len(value) <= 160 for value in previews)
    assert "gmail_history_id" not in serialized
    assert "outlook_conversation_index" not in serialized

    normalized = normalize_events(result.events)
    bundles = build_action_bundles(normalized)
    assert len(bundles) == 5
    assert all(bundle.observation is not None for bundle in bundles)
    assert all(bundle.decision is not None for bundle in bundles)
    assert all(bundle.outcome is not None for bundle in bundles)
    assert {bundle.action.action_type for bundle in bundles} == {
        "propose_calendar_candidate",
        "request_clarification",
        "ignore_email",
    }

    html = render_explorer(
        normalized,
        ingestion=read_jsonl(SAMPLE),
        source_path=SAMPLE,
    )
    assert "AgentLedger" in html
    assert "mail-meeting-001" in html
    assert "A calendar candidate was extracted" in html
    assert "calendar_candidate_created" in html
    assert "body_preview" in html


def test_preview_equal_to_full_body_is_not_logged_verbatim() -> None:
    message = _message(
        subject="8月12日 15時 定例会議",
        body="Short but complete private body",
    )
    message = EmailMessage(
        **{
            **message.__dict__,
            "body_preview": message.body_text,
        }
    )

    class _Provider:
        def list_messages(self) -> list[EmailMessage]:
            return [message]

        def get_message(self, message_id: str) -> EmailMessage:
            assert message_id == message.message_id
            return message

    result = MailToCalendarService(base_year=2026).process(_Provider())
    serialized = json.dumps(result.events, ensure_ascii=False)

    assert message.body_text not in serialized


def test_calendar_agent_conversion_boundary() -> None:
    messages = LocalMailProvider(SAMPLE).list_messages()
    candidates = {
        message.message_id: _classify_extract(message)[1]
        for message in messages
    }
    meeting = candidates["mail-meeting-001"]
    ambiguous = candidates["mail-ambiguous-001"]
    deadline = candidates["mail-deadline-001"]
    assert meeting is not None
    request = to_calendar_request(meeting)
    assert isinstance(request, CalendarRequest)
    assert request.requested_date == "2026-08-12"
    assert request.available_slots == ["15:00"]

    assert ambiguous is not None
    with pytest.raises(ValueError, match="requires clarification"):
        to_calendar_request(ambiguous)
    assert deadline is not None
    with pytest.raises(ValueError, match="start time is required"):
        to_calendar_request(deadline)


def test_cli_processes_sample_and_prints_summary(
    tmp_path,
    monkeypatch,
    capsys,
) -> None:
    output = tmp_path / "mail-audit.jsonl"
    monkeypatch.setattr(
        "sys.argv",
        [
            "mail-to-calendar",
            "process",
            "--input",
            str(SAMPLE),
            "--output",
            str(output),
            "--base-year",
            "2026",
            "--timezone",
            "Asia/Tokyo",
        ],
    )

    main()

    summary = capsys.readouterr().out
    assert "Processed: 5 messages" in summary
    assert "Important: 4" in summary
    assert "Candidates: 4" in summary
    assert "Clarification required: 1" in summary
    assert "Ignored: 1" in summary
    ingestion = read_jsonl(output)
    assert ingestion.loaded == 20
    assert ingestion.issues == []
