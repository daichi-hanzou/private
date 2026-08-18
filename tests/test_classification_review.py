from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path

from calendar_agent.audit import write_jsonl
from mail_calendar_orchestrator.review_models import (
    HumanVerdictInput, ReviewContext, ReviewResult,
)
from mail_calendar_orchestrator.review_service import ClassificationReviewService
from mail_calendar_orchestrator.review_sources import (
    MessageResolver, ReviewSourceConfig,
)
from mail_calendar_orchestrator.state import MailStateStore
from mail_to_calendar.local_provider import LocalMailProvider


class FakeReviewClient:
    def __init__(self, statuses: dict[str, str], *, model_name: str = "frontier-test") -> None:
        self.model_name = model_name
        self.statuses = statuses

    def review_case(self, context: ReviewContext) -> ReviewResult:
        status = self.statuses[context.message_id]
        return ReviewResult(
            review_status=status,  # type: ignore[arg-type]
            suggested_classification=(
                "promotion" if status == "agree" else "calendar_candidate"
            ),
            issue_type=f"{status}_issue",
            confidence=0.8,
            reason_summary=f"summary for {context.message_id}",
            needs_human_review=status != "agree",
        )

    def chat(
        self,
        context: ReviewContext,
        history: list[object],
        user_message: str,
    ) -> str:
        del history
        return f"{context.message_id}: {user_message}"


def _seed_review_data(tmp_path: Path) -> tuple[MailStateStore, ClassificationReviewService]:
    input_path = tmp_path / "messages.jsonl"
    messages = [
        {
            "provider": "local",
            "message_id": "msg-agree",
            "sender": "a@example.com",
            "recipients": ["me@example.com"],
            "subject": "FYI only",
            "received_at": "2026-08-10T00:00:00+00:00",
            "body_text": "Informational update only.",
        },
        {
            "provider": "local",
            "message_id": "msg-disagree",
            "sender": "b@example.com",
            "recipients": ["me@example.com"],
            "subject": "Book clinic visit",
            "received_at": "2026-08-11T00:00:00+00:00",
            "body_text": "Your appointment is confirmed for 2026-08-20 14:00.",
        },
        {
            "provider": "local",
            "message_id": "msg-uncertain",
            "sender": "c@example.com",
            "recipients": ["me@example.com"],
            "subject": "Maybe next week",
            "received_at": "2026-08-12T00:00:00+00:00",
            "body_text": "We should meet next week sometime.",
        },
    ]
    input_path.write_text(
        "\n".join(json.dumps(item, ensure_ascii=False) for item in messages) + "\n",
        encoding="utf-8",
    )
    audit_path = tmp_path / "audit.jsonl"
    events = []
    for item in messages:
        digest = item["message_id"]
        final_classification = {
            "msg-agree": "informational",
            "msg-disagree": "promotion",
            "msg-uncertain": "clarification_required",
        }[digest]
        events.extend(
            [
                {
                    "event_id": f"obs-{digest}",
                    "event_type": "observation_received",
                    "run_id": "mail-batch-test",
                    "timestamp": item["received_at"],
                    "agent_id": "mail_to_calendar_agent",
                    "correlation_id": f"corr-{digest}",
                    "metadata": {"source_message_id": item["message_id"]},
                    "observation": {
                        "provider": item["provider"],
                        "subject": item["subject"],
                    },
                },
                {
                    "event_id": f"dec-{digest}",
                    "event_type": "decision_made",
                    "run_id": "mail-batch-test",
                    "timestamp": item["received_at"],
                    "agent_id": "mail_to_calendar_agent",
                    "correlation_id": f"corr-{digest}",
                    "decision_id": f"decision-{digest}",
                    "selected_action": "ignore_email",
                    "importance_score": 0.9 if digest != "msg-agree" else 0.1,
                    "metadata": {"source_message_id": item["message_id"]},
                    "analysis": {
                        "final_classification": final_classification,
                        "llm_proposed_classification": "promotion",
                        "classification_corrections": [
                            "semantic_consistency:promotion->calendar_candidate"
                        ] if digest != "msg-agree" else [],
                        "validation_issues": (
                            ["rule_llm_conflict"] if digest == "msg-uncertain" else []
                        ),
                        "calendar_candidate_rejected_reason": None,
                        "time_normalization": None,
                        "fallback_reason": None,
                        "should_notify_user": False,
                        "llm_should_notify_user": False,
                        "normalized_candidate": (
                            {"date": "2026-08-20", "start": "14:00"}
                            if digest == "msg-disagree" else None
                        ),
                    },
                },
                {
                    "event_id": f"act-{digest}",
                    "event_type": "action_executed",
                    "run_id": "mail-batch-test",
                    "timestamp": item["received_at"],
                    "agent_id": "mail_to_calendar_agent",
                    "correlation_id": f"corr-{digest}",
                    "decision_id": f"decision-{digest}",
                    "action_id": f"action-{digest}",
                    "action": "ignore_email",
                    "metadata": {
                        "source_message_id": item["message_id"],
                        "orchestration_status": (
                            "ready" if digest == "msg-disagree" else "ignored"
                        ),
                    },
                    "action_parameters": {"reason_category": "promotion"},
                },
                {
                    "event_id": f"out-{digest}",
                    "event_type": "outcome_observed",
                    "run_id": "mail-batch-test",
                    "timestamp": item["received_at"],
                    "agent_id": "mail_to_calendar_agent",
                    "correlation_id": f"corr-{digest}",
                    "decision_id": f"decision-{digest}",
                    "action_id": f"action-{digest}",
                    "metadata": {"source_message_id": item["message_id"]},
                    "actual_outcome": {"outcome_type": "email_ignored"},
                },
            ]
        )
    write_jsonl(audit_path, events)
    store = MailStateStore(tmp_path / "state.sqlite3")
    provider = LocalMailProvider(input_path)
    for message in provider.list_messages():
        store.mark_processing(
            message,
            run_id="mail-batch-test",
            analysis_mode="llm-first",
            model_name="qwen3:8b",
            message_source_ref=str(input_path),
        )
        store.mark_result(
            message,
            status="processed",
            final_classification={
                "msg-agree": "informational",
                "msg-disagree": "promotion",
                "msg-uncertain": "clarification_required",
            }[message.message_id],
            source_run_id="mail-batch-test",
            source_jsonl_path=str(audit_path),
        )
    store.register_important_notification(
        provider="local",
        message_id="msg-disagree",
        category="deadline",
        subject="Book clinic visit",
        notification_date="2026-08-20",
        amount=None,
    )
    service = ClassificationReviewService(
        store,
        FakeReviewClient(
            {
                "msg-agree": "agree",
                "msg-disagree": "disagreement",
                "msg-uncertain": "uncertain",
            }
        ),
        MessageResolver(ReviewSourceConfig()),
        review_version="v1",
    )
    return store, service


def test_review_run_and_queue_filters(tmp_path: Path) -> None:
    store, service = _seed_review_data(tmp_path)
    result = service.run()

    assert result.selected == 3
    assert result.created == 3
    queue = service.list_cases(queue_only=True, limit=10)
    queue_ids = [item.message_id for item in queue]
    assert "msg-agree" not in queue_ids
    assert "msg-disagree" in queue_ids
    assert "msg-uncertain" in queue_ids
    store.close()


def test_same_version_does_not_duplicate_and_new_version_reruns(tmp_path: Path) -> None:
    store, service = _seed_review_data(tmp_path)
    first = service.run()
    second = service.run(include_reviewed=True)

    assert first.created == 3
    assert second.created == 0
    assert second.skipped_existing == 3

    service_v2 = ClassificationReviewService(
        store,
        FakeReviewClient(
            {
                "msg-agree": "agree",
                "msg-disagree": "disagreement",
                "msg-uncertain": "uncertain",
            }
        ),
        MessageResolver(ReviewSourceConfig()),
        review_version="v2",
    )
    third = service_v2.run()
    assert third.created == 3
    store.close()


def test_human_verdict_and_chat_are_case_scoped(tmp_path: Path) -> None:
    store, service = _seed_review_data(tmp_path)
    service.run()
    cases = service.list_cases(queue_only=False, limit=10)
    disagree = next(item for item in cases if item.message_id == "msg-disagree")
    uncertain = next(item for item in cases if item.message_id == "msg-uncertain")

    reply = service.send_chat_message(
        disagree.review_case_id,
        "why not promotion?",
    )
    assert "msg-disagree" in reply
    assert len(service.chat_history(disagree.review_case_id)) == 2
    assert service.chat_history(uncertain.review_case_id) == []

    updated = service.save_verdict(
        disagree.review_case_id,
        HumanVerdictInput(
            human_verdict="modified",
            final_classification="calendar_candidate",
            lesson_summary="Confirmed appointments should become calendar candidates.",
            recommended_change_target="validator",
        ),
    )
    assert updated.human_review_status == "resolved"
    assert updated.human_verdict == "modified"
    assert updated.recommended_change_target == "validator"
    store.close()


def test_review_storage_does_not_persist_raw_llm_or_chain_of_thought(
    tmp_path: Path,
) -> None:
    store, service = _seed_review_data(tmp_path)
    service.run()

    dump = "\n".join(
        json.dumps(dict(row), ensure_ascii=False)
        for row in store.connection.execute("SELECT * FROM review_cases")
    )
    assert "chain-of-thought" not in dump
    assert "raw_response" not in dump
    store.close()
