from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path
from types import SimpleNamespace

import pytest

from calendar_agent.audit import write_jsonl
from mail_calendar_orchestrator.cli import _parser
from mail_calendar_orchestrator.feedback_synthesis import (
    EVIDENCE_PROMPT, POLICY_PROMPT, FeedbackSynthesisService,
)
from mail_calendar_orchestrator.review_models import (
    HumanVerdictInput, ReviewContext, ReviewResult,
)
from mail_calendar_orchestrator.review_provider import OpenAIReviewClient
from mail_calendar_orchestrator.review_service import (
    ClassificationReviewService, _reconcile_review_status,
)
from mail_calendar_orchestrator.review_sources import (
    MessageResolver, ReviewSourceConfig,
)
from mail_calendar_orchestrator.review_web import _INDEX_HTML
from mail_calendar_orchestrator.state import MailStateStore
from mail_to_calendar.local_provider import LocalMailProvider
from mail_to_calendar.taxonomy import (
    CLASSIFICATION_SET, classifications_equivalent, normalize_classification,
)


def test_reviewer_classification_enum_accepts_transactional_and_rejects_unknown() -> None:
    base = {
        "review_status": "disagreement",
        "suggested_classification": "transactional",
        "issue_type": "personal_transaction_missed",
        "confidence": 0.9,
        "reason_summary": "A user-specific transaction was confirmed.",
        "needs_human_review": True,
    }
    assert ReviewResult.from_dict(base).suggested_classification == "transactional"
    for unknown in ("financial", "payment", "reservation", "important"):
        with pytest.raises(ValueError, match="suggested_classification"):
            ReviewResult.from_dict({**base, "suggested_classification": unknown})


def test_legacy_informational_and_promotion_normalize_to_ignored() -> None:
    assert normalize_classification("informational") == "ignored"
    assert normalize_classification("promotion") == "ignored"
    assert classifications_equivalent("informational", "ignored")
    assert classifications_equivalent("promotion", "ignored")


@pytest.mark.parametrize(
    "original,final,suggested,input_status,expected_status,needs_human",
    [
        ("ignored", "ignored", "transactional", "agree", "disagreement", True),
        ("ignored", "ignored", "ignored", "disagreement", "agree", False),
        ("promotion", "promotion", "ignored", "disagreement", "agree", False),
        (
            "calendar_candidate", "ignored", "ignored", "disagreement",
            "agree", False,
        ),
        ("ignored", "ignored", "transactional", "uncertain", "uncertain", True),
        ("ignored", "ignored", "ignored", "uncertain", "uncertain", True),
    ],
)
def test_review_status_matches_normalized_taxonomy_or_preserves_uncertainty(
    original, final, suggested, input_status, expected_status, needs_human,
) -> None:
    context = SimpleNamespace(
        original_classification=original,
        final_classification=final,
    )
    result = ReviewResult(
        review_status=input_status,
        suggested_classification=suggested,
        issue_type="test",
        confidence=0.8,
        reason_summary="test",
        needs_human_review=False,
    )

    reconciled = _reconcile_review_status(context, result)

    assert reconciled.review_status == expected_status
    assert reconciled.needs_human_review is needs_human


def test_reviewer_prompt_uses_only_current_private_mailbox_taxonomy() -> None:
    from mail_calendar_orchestrator.review_provider import (
        REVIEW_RESULT_SCHEMA, REVIEW_SYSTEM_PROMPT,
    )

    suggested = REVIEW_RESULT_SCHEMA["properties"]["suggested_classification"]
    assert set(suggested["enum"]) == CLASSIFICATION_SET
    assert "legacy labels such as informational" in REVIEW_SYSTEM_PROMPT
    assert "executed investment purchase" in REVIEW_SYSTEM_PROMPT
    assert "shipment" in REVIEW_SYSTEM_PROMPT
    assert "suspicious access" in REVIEW_SYSTEM_PROMPT
    assert "generic service outage" in REVIEW_SYSTEM_PROMPT


def test_openai_review_requests_use_model_default_temperature() -> None:
    requests: list[dict[str, object]] = []

    class FakeCompletions:
        def create(self, **kwargs: object) -> object:
            requests.append(kwargs)
            content = json.dumps({
                "review_status": "agree",
                "suggested_classification": "ignored",
                "issue_type": "none",
                "confidence": 1.0,
                "reason_summary": "Classification is consistent.",
                "needs_human_review": False,
            })
            return SimpleNamespace(choices=[SimpleNamespace(
                message=SimpleNamespace(content=content)
            )])

    client = OpenAIReviewClient.__new__(OpenAIReviewClient)
    client.model_name = "review-model"
    client.review_version = "v1"
    client.client = SimpleNamespace(
        chat=SimpleNamespace(completions=FakeCompletions())
    )
    context = SimpleNamespace(to_prompt_payload=lambda: {})

    result = client.review_case(context)
    client.chat(context, [], "Explain briefly")

    assert result.review_status == "agree"
    assert len(requests) == 2
    assert all("temperature" not in request for request in requests)
    review_format = requests[0]["response_format"]
    assert review_format["type"] == "json_schema"
    assert review_format["json_schema"]["schema"]["properties"][
        "suggested_classification"
    ]["enum"] == [
        "calendar_candidate", "transactional", "security_notification",
        "ignored", "invalid", "clarification_required",
    ]


def test_event_snapshot_uses_provider_message_and_correlation() -> None:
    def trace(provider: str, correlation: str, classification: str) -> list[dict]:
        shared = {
            "agent_id": "mail_to_calendar_agent",
            "correlation_id": correlation,
            "metadata": {"source_message_id": "shared-id"},
        }
        return [
            {
                **shared,
                "event_type": "observation_received",
                "observation": {
                    "provider": provider,
                    "message_id": "shared-id",
                    "subject": f"{provider} subject",
                },
            },
            {
                **shared,
                "event_type": "decision_made",
                "selected_action": "ignore_email",
                "importance_score": 0,
                "analysis": {"final_classification": classification},
            },
            {
                **shared,
                "event_type": "action_executed",
                "action_parameters": {"provider_marker": provider},
                "metadata": {
                    "source_message_id": "shared-id",
                    "orchestration_status": "ignored",
                },
            },
        ]

    events = trace("outlook", "corr-outlook", "promotion")
    events += trace("gmail", "corr-gmail", "informational")

    snapshot = ClassificationReviewService._event_snapshot(
        events, "gmail", "shared-id"
    )

    assert snapshot["provider"] == "gmail"
    assert snapshot["subject"] == "gmail subject"
    assert snapshot["analysis"]["final_classification"] == "informational"
    assert snapshot["action_parameters"] == {"provider_marker": "gmail"}


def test_review_outlook_resolver_uses_authenticator_token_cache_path(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch,
) -> None:
    captured: dict[str, object] = {}

    class FakeAuthenticator:
        def __init__(self, **kwargs: object) -> None:
            captured.update(kwargs)

    class FakeOutlookProvider:
        def __init__(self, config: object, *, authenticator: object) -> None:
            captured["config"] = config
            captured["authenticator"] = authenticator

    monkeypatch.setattr(
        "mail_calendar_orchestrator.review_sources.MicrosoftAuthenticator",
        FakeAuthenticator,
    )
    monkeypatch.setattr(
        "mail_calendar_orchestrator.review_sources.OutlookProvider",
        FakeOutlookProvider,
    )
    cache = tmp_path / "microsoft-token-cache.json"
    resolver = MessageResolver(ReviewSourceConfig(
        outlook_client_id="client-id", outlook_token_cache=cache,
    ))

    resolver._outlook_provider()

    assert captured["token_cache_path"] == cache
    assert "cache_path" not in captured
    assert captured["scopes"] == ["Mail.Read"]


class FakeReviewClient:
    def __init__(self, statuses: dict[str, str], *, model_name: str = "frontier-test") -> None:
        self.model_name = model_name
        self.statuses = statuses
        self.chat_calls: list[dict[str, object]] = []

    def review_case(self, context: ReviewContext) -> ReviewResult:
        status = self.statuses[context.message_id]
        return ReviewResult(
            review_status=status,  # type: ignore[arg-type]
            suggested_classification=(
                "ignored" if status == "agree" else "calendar_candidate"
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
        human_context: dict[str, object] | None = None,
    ) -> str:
        self.chat_calls.append({
            "message_id": context.message_id,
            "history": list(history),
            "user_message": user_message,
            "human_context": human_context or {},
        })
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


def test_review_batch_does_not_queue_legacy_alias_only_disagreement(
    tmp_path: Path,
) -> None:
    store, seeded = _seed_review_data(tmp_path)

    class AliasReviewer(FakeReviewClient):
        def review_case(self, context: ReviewContext) -> ReviewResult:
            return ReviewResult(
                review_status="disagreement",
                suggested_classification="ignored",
                issue_type="legacy_taxonomy_alias",
                confidence=0.9,
                reason_summary="Legacy value maps to ignored.",
                needs_human_review=True,
            )

    service = ClassificationReviewService(
        store,
        AliasReviewer({}, model_name="taxonomy-reviewer"),
        seeded.resolver,
        review_version="taxonomy-v1",
    )
    service.run()
    cases = {item.message_id: item for item in service.list_cases(limit=10)}

    assert cases["msg-agree"].review_status == "agree"
    assert cases["msg-disagree"].review_status == "agree"
    assert cases["msg-uncertain"].review_status == "disagreement"
    store.close()


def test_review_keeps_ignored_to_transactional_as_meaningful_disagreement(
    tmp_path: Path,
) -> None:
    store, seeded = _seed_review_data(tmp_path)

    class TransactionalReviewer(FakeReviewClient):
        def review_case(self, context: ReviewContext) -> ReviewResult:
            return ReviewResult(
                review_status="disagreement",
                suggested_classification="transactional",
                issue_type="missed_transaction",
                confidence=0.95,
                reason_summary="A concrete personal transaction was ignored.",
                needs_human_review=True,
            )

    service = ClassificationReviewService(
        store,
        TransactionalReviewer({}, model_name="taxonomy-reviewer"),
        seeded.resolver,
        review_version="transactional-policy-v1",
    )
    service.run()
    cases = service.list_cases(queue_only=True, limit=10)

    assert {case.message_id for case in cases} == {
        "msg-agree", "msg-disagree", "msg-uncertain"
    }
    assert all(case.review_status == "disagreement" for case in cases)
    store.close()


def test_review_selection_diagnostics_explain_every_filter(tmp_path: Path) -> None:
    store, service = _seed_review_data(tmp_path)
    baseline = service.selection_diagnostics()
    assert len(baseline) == 3
    assert all(item.selected for item in baseline)
    assert all(item.source_jsonl_available for item in baseline)
    assert all(item.provider_retrieval_available for item in baseline)

    with store.connection:
        store.connection.execute(
            "UPDATE processed_messages SET source_jsonl_path=NULL,"
            "last_processed_at='2026-08-10T00:00:00+00:00' "
            "WHERE message_id='msg-agree'"
        )
        store.connection.execute(
            "UPDATE processed_messages SET processing_status='processing' "
            "WHERE message_id='msg-disagree'"
        )
        store.connection.execute(
            "UPDATE processed_messages SET message_source_ref=NULL "
            "WHERE message_id='msg-uncertain'"
        )
    diagnostics = {
        item.message_id: item for item in service.selection_diagnostics(
            since="2026-08-12T12:00:00+00:00"
        )
    }

    assert "source_jsonl_path_missing" in diagnostics[
        "msg-agree"
    ].exclusion_reasons
    assert "outside_since_window" in diagnostics[
        "msg-agree"
    ].exclusion_reasons
    assert "processing_status_not_processed" in diagnostics[
        "msg-disagree"
    ].exclusion_reasons
    assert "provider_retrieval_unavailable" in diagnostics[
        "msg-uncertain"
    ].exclusion_reasons
    assert diagnostics["msg-uncertain"].provider_retrieval_reason == (
        "unsupported_provider_without_message_source_ref"
    )
    store.close()


def test_review_selection_reports_existing_review_and_limit(tmp_path: Path) -> None:
    store, service = _seed_review_data(tmp_path)
    service.run(limit=1)

    diagnostics = service.selection_diagnostics(limit=1)

    assert any(
        item.already_reviewed and "already_reviewed" in item.exclusion_reasons
        for item in diagnostics
    )
    assert sum(item.selected for item in diagnostics) == 1
    assert any("excluded_by_limit" in item.exclusion_reasons for item in diagnostics)
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


def test_review_case_listing_and_queue_are_version_scoped(tmp_path: Path) -> None:
    store, service_v1 = _seed_review_data(tmp_path)
    service_v1.run()
    service_v2 = ClassificationReviewService(
        store,
        FakeReviewClient({
            "msg-agree": "agree",
            "msg-disagree": "disagreement",
            "msg-uncertain": "uncertain",
        }),
        service_v1.resolver,
        review_version="v2",
    )
    service_v2.run()

    assert {case.review_version for case in service_v2.list_cases()} == {"v2"}
    assert {
        case.review_version
        for case in service_v2.list_cases(version="v1")
    } == {"v1"}
    assert {
        case.review_version
        for case in service_v2.list_cases(all_versions=True)
    } == {"v1", "v2"}
    assert {
        case.review_version
        for case in service_v2.list_cases(queue_only=True)
    } == {"v2"}

    with store.connection:
        store.connection.execute(
            "UPDATE review_cases SET suggested_classification='promotional' "
            "WHERE review_version='v1' AND message_id='msg-agree'"
        )
    legacy = next(
        case for case in service_v2.list_cases(version="v1")
        if case.message_id == "msg-agree"
    )
    assert legacy.suggested_classification == "promotional"
    assert store.connection.execute(
        "SELECT suggested_classification FROM review_cases "
        "WHERE review_case_id=?", (legacy.review_case_id,),
    ).fetchone()[0] == "promotional"

    v1_case = next(
        case for case in service_v2.list_cases(version="v1")
        if case.message_id == "msg-disagree"
    )
    v2_case = next(
        case for case in service_v2.list_cases()
        if case.message_id == "msg-disagree"
    )
    assert v1_case.review_case_id != v2_case.review_case_id
    service_v1.save_verdict(v1_case.review_case_id, HumanVerdictInput(
        human_verdict="original_correct", final_classification="ignored",
        lesson_summary="v1 history", recommended_change_target="none",
        human_comment="legacy review",
    ))
    assert len(service_v1.get_case_detail(v1_case.review_case_id)[
        "human_review_history"
    ]) == 1
    assert service_v2.get_case_detail(v2_case.review_case_id)[
        "human_review_history"
    ] == []
    with pytest.raises(RuntimeError, match="does not match the selected case"):
        service_v2.send_chat_message(v1_case.review_case_id, "Review this")
    store.close()


def test_review_list_cli_has_independent_version_options() -> None:
    parser = _parser()
    current = parser.parse_args(["review", "list"])
    selected = parser.parse_args(["review", "list", "--version", "v1"])
    all_versions = parser.parse_args(["review", "list", "--all-versions"])

    assert current.version is None and not current.all_versions
    assert selected.version == "v1" and not selected.all_versions
    assert all_versions.all_versions and all_versions.version is None
    with pytest.raises(SystemExit):
        parser.parse_args([
            "review", "list", "--version", "v1", "--all-versions"
        ])


def test_human_verdict_and_chat_are_case_scoped(tmp_path: Path) -> None:
    store, service = _seed_review_data(tmp_path)
    service.run()
    cases = service.list_cases(queue_only=False, limit=10)
    disagree = next(item for item in cases if item.message_id == "msg-disagree")
    uncertain = next(item for item in cases if item.message_id == "msg-uncertain")

    reviewer = service.reviewer
    detail = service.get_case_detail(disagree.review_case_id)
    json.dumps(detail, ensure_ascii=False)
    assert isinstance(detail["case"]["created_at"], str)
    assert isinstance(detail["case"]["updated_at"], str)
    assert reviewer.chat_calls == []
    reply = service.send_chat_message(
        disagree.review_case_id,
        "why not promotion?",
        human_comment="It looks like a personal reservation.",
        human_verdict="reviewer_correct",
        final_classification="calendar_candidate",
        recommended_change_target="qwen_prompt",
    )
    assert "msg-disagree" in reply
    assert len(service.chat_history(disagree.review_case_id)) == 2
    assert service.chat_history(uncertain.review_case_id) == []
    assert len(reviewer.chat_calls) == 1
    call = reviewer.chat_calls[0]
    assert call["message_id"] == "msg-disagree"
    assert call["history"] == []
    assert call["human_context"]["human_comment"] == (
        "It looks like a personal reservation."
    )
    assert call["human_context"]["human_verdict"] == "reviewer_correct"
    assert call["human_context"]["reviewer_initial_assessment"][
        "suggested_classification"
    ] == "calendar_candidate"
    service.send_chat_message(
        disagree.review_case_id, "What evidence is strongest?",
        human_comment="Still deciding.", human_verdict="unresolved",
    )
    assert len(reviewer.chat_calls) == 2
    assert len(reviewer.chat_calls[1]["history"]) == 2
    assert all(
        item.content.find("msg-uncertain") == -1
        for item in reviewer.chat_calls[1]["history"]
    )

    updated = service.save_verdict(
        disagree.review_case_id,
        HumanVerdictInput(
            human_verdict="modified",
            final_classification="calendar_candidate",
            lesson_summary="Confirmed appointments should become calendar candidates.",
            recommended_change_target="validator",
            human_comment="The reservation is a personal commitment.",
        ),
    )
    assert updated.human_review_status == "resolved"
    assert updated.human_verdict == "modified"
    assert updated.recommended_change_target == "validator"
    assert updated.human_comment == "The reservation is a personal commitment."
    untouched = service.get_case(uncertain.review_case_id)
    assert untouched.human_review_status == "pending"
    assert untouched.human_comment is None
    history = list(store.connection.execute(
        "SELECT * FROM human_review_history WHERE review_case_id=?",
        (disagree.review_case_id,),
    ))
    assert len(history) == 1
    assert history[0]["human_comment"] == (
        "The reservation is a personal commitment."
    )
    assert len(reviewer.chat_calls) == 2
    store.close()


def test_unresolved_verdict_stays_in_human_review_queue(tmp_path: Path) -> None:
    store, service = _seed_review_data(tmp_path)
    service.run()
    case = next(
        item for item in service.list_cases(queue_only=True, limit=10)
        if item.review_status == "uncertain"
    )

    updated = service.save_verdict(case.review_case_id, HumanVerdictInput(
        human_verdict="unresolved",
        final_classification="clarification_required",
        lesson_summary="",
        recommended_change_target="none",
        human_comment="More evidence is required.",
    ))

    assert updated.human_review_status == "pending"
    assert updated.human_comment == "More evidence is required."
    assert case.review_case_id in {
        item.review_case_id for item in service.list_cases(queue_only=True, limit=10)
    }
    store.close()


def test_accept_reviewer_resolves_without_comment_and_records_history(
    tmp_path: Path,
) -> None:
    store, service = _seed_review_data(tmp_path)
    service.run()
    case = next(
        item for item in service.list_cases(queue_only=True, limit=10)
        if item.message_id == "msg-disagree"
    )

    updated = service.accept_reviewer(case.review_case_id)

    assert updated.human_review_status == "resolved"
    assert updated.human_verdict == "reviewer_correct"
    assert updated.human_final_classification == case.suggested_classification
    assert updated.human_comment == ""
    assert updated.recommended_change_target == "none"
    assert case.review_case_id not in {
        item.review_case_id for item in service.list_cases(queue_only=True, limit=10)
    }
    history = list(store.connection.execute(
        "SELECT * FROM human_review_history WHERE review_case_id=?",
        (case.review_case_id,),
    ))
    assert len(history) == 1
    assert history[0]["human_verdict"] == "reviewer_correct"
    assert history[0]["human_comment"] == ""
    store.close()


def test_bulk_accept_reviewer_only_resolves_selected_pending_cases(
    tmp_path: Path,
) -> None:
    store, service = _seed_review_data(tmp_path)
    service.run()
    cases = service.list_cases(limit=10)
    selected = cases[:2]
    service.accept_reviewer(selected[0].review_case_id)

    accepted = service.accept_reviewers([
        selected[0].review_case_id,
        selected[1].review_case_id,
        selected[1].review_case_id,
    ])

    assert [item.review_case_id for item in accepted] == [
        selected[1].review_case_id,
    ]
    assert all(item.human_verdict == "reviewer_correct" for item in accepted)
    history_counts = {
        row["review_case_id"]: row["count"]
        for row in store.connection.execute(
            "SELECT review_case_id, count(*) count FROM human_review_history "
            "GROUP BY review_case_id"
        )
    }
    assert history_counts[selected[0].review_case_id] == 1
    assert history_counts[selected[1].review_case_id] == 1
    untouched = service.get_case(cases[2].review_case_id)
    assert untouched.human_review_status == "pending"
    store.close()


def test_human_review_ui_is_subject_first_and_chat_free() -> None:
    assert "Reviewer status" in _INDEX_HTML
    assert "Human review status" in _INDEX_HTML
    assert "All reviewer statuses" in _INDEX_HTML
    assert "All human statuses" in _INDEX_HTML
    assert 'id="reviewerStatus"' in _INDEX_HTML
    assert 'id="humanStatus"' in _INDEX_HTML
    assert "Disagreement" in _INDEX_HTML
    assert "Uncertain" in _INDEX_HTML
    assert "Resolved" in _INDEX_HTML
    assert "x.review_status===reviewer" in _INDEX_HTML
    assert "x.human_review_status==='pending'" in _INDEX_HTML
    assert ")&&(human==='all'||" in _INDEX_HTML
    assert "Human Review" in _INDEX_HTML
    assert "Human comment" in _INDEX_HTML
    assert "Save & Next" in _INDEX_HTML
    assert "Accept Reviewer & Next" in _INDEX_HTML
    assert "acceptReviewerNext" in _INDEX_HTML
    assert "/accept-reviewer" in _INDEX_HTML
    assert "Accept All Visible" in _INDEX_HTML
    assert "acceptAllVisible" in _INDEX_HTML
    assert "/api/cases/accept-reviewer-bulk" in _INDEX_HTML
    assert "Ask Reviewer" in _INDEX_HTML
    assert "sendReviewer" in _INDEX_HTML
    assert "human_comment" in _INDEX_HTML
    assert "Decision trace" in _INDEX_HTML
    assert "Technical details" in _INDEX_HTML
    assert "Reviewer Chat" not in _INDEX_HTML
    assert "case-title" in _INDEX_HTML
    assert "canonical_message_id" in _INDEX_HTML
    assert "Review version" in _INDEX_HTML
    assert "all versions" in _INDEX_HTML
    assert "current_version" in _INDEX_HTML
    assert "versionEl" in _INDEX_HTML


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


def test_human_feedback_synthesis_is_grounded_versioned_and_exportable(
    tmp_path: Path,
) -> None:
    store, reviews = _seed_review_data(tmp_path)
    reviews.run()
    cases = {case.message_id: case for case in reviews.list_cases()}
    reviews.save_verdict(cases["msg-agree"].review_case_id, HumanVerdictInput(
        human_verdict="original_correct", final_classification="ignored",
        human_comment="情報メールは通知しなくてよい", lesson_summary="",
        recommended_change_target="qwen_prompt",
    ))
    reviews.save_verdict(cases["msg-disagree"].review_case_id, HumanVerdictInput(
        human_verdict="reviewer_correct", final_classification="calendar_candidate",
        human_comment="確定した本人予約は予定にしてほしい", lesson_summary="",
        recommended_change_target="validator",
    ))
    reviews.save_verdict(cases["msg-uncertain"].review_case_id, HumanVerdictInput(
        human_verdict="unresolved", final_classification="clarification_required",
        human_comment="まだ判断できない", lesson_summary="",
        recommended_change_target="none",
    ))

    class FakeFeedbackClient:
        model_name = "frontier-feedback-test"

        def __init__(self) -> None:
            self.extractions: list[dict[str, object]] = []
            self.synthesis_inputs: list[list[dict[str, object]]] = []

        def extract_evidence(self, payload):
            self.extractions.append(payload)
            snippet = (
                "Informational update only."
                if payload["review_id"] == cases["msg-agree"].review_case_id
                else "Your appointment is confirmed"
            )
            return {"evidence_snippets": [snippet, "invented secret token"],
                    "evidence_summary": "Minimal grounded evidence."}

        def synthesize(self, payload):
            self.synthesis_inputs.append(payload)
            ids = [item["review_id"] for item in payload]
            evidence = [value for item in payload for value in item["evidence_snippets"]]
            return {"policy_candidates": [{
                "policy_id": "personal_confirmation_policy",
                "summary": "Grounded personal confirmations require explicit handling.",
                "supporting_cases": ids, "supporting_evidence": evidence,
                "confidence": "high", "recommended_targets": ["qwen_prompt", "test"],
                "implementation_plan": ["Refine the grounded confirmation rule."],
                "validation_plan": ["Add regression fixtures for both classifications."],
            }], "conflicts": [{"summary": "No material conflict", "supporting_cases": ids}],
                "preference_changes": [{"older_policy": "notify less",
                    "newer_policy": "handle confirmed reservations", "supporting_cases": ids}]}

    client = FakeFeedbackClient()
    synthesis = FeedbackSynthesisService(store, reviews, client)
    first = synthesis.synthesize(limit=50)
    second = synthesis.synthesize(limit=50)

    assert first.selected == 2
    assert first.evidence_created == 2 and first.evidence_reused == 0
    assert second.evidence_created == 0 and second.evidence_reused == 2
    assert len(client.extractions) == 2
    assert all(len(call["analysis_text"]) > 0 for call in client.extractions)
    assert all(len(item["evidence_snippets"]) == 1 for item in client.synthesis_inputs[0])
    assert all("system_final_classification" in item
               for item in client.synthesis_inputs[0])
    assert all("reviewer_suggested_classification" in item
               for item in client.synthesis_inputs[0])
    assert all("review_status" in item for item in client.synthesis_inputs[0])
    assert all("invented secret token" not in item["evidence_snippets"]
               for item in client.synthesis_inputs[0])
    assert cases["msg-uncertain"].review_case_id not in {
        item["review_id"] for item in client.synthesis_inputs[0]
    }
    assert store.connection.execute(
        "SELECT COUNT(*) FROM review_feedback_evidence"
    ).fetchone()[0] == 2
    synthesis_row = store.connection.execute(
        "SELECT * FROM review_policy_syntheses WHERE synthesis_id=?",
        (first.synthesis_id,),
    ).fetchone()
    assert json.loads(synthesis_row["conflicts_json"])
    assert json.loads(synthesis_row["preference_changes_json"])

    markdown = synthesis.export(synthesis_id=first.synthesis_id)
    exported_json = synthesis.export(synthesis_id=first.synthesis_id, format="json")
    assert "personal_confirmation_policy" in markdown
    assert cases["msg-agree"].review_case_id in markdown
    assert "Human comments:" in markdown
    assert "Implementation plan:" in markdown
    assert "Validation plan:" in markdown
    assert "Reviewer disagreement evidence" in markdown
    assert "invented secret token" not in markdown
    assert "analysis_text" not in exported_json
    assert "OAuth" not in exported_json
    store.close()


def test_synthesis_includes_explicit_reviewer_acceptance_without_comment(
    tmp_path: Path,
) -> None:
    store, reviews = _seed_review_data(tmp_path)
    reviews.run()
    case = next(
        item for item in reviews.list_cases()
        if item.message_id == "msg-disagree"
    )
    reviews.accept_reviewer(case.review_case_id)

    class FakeFeedbackClient:
        model_name = "frontier-feedback-test"

        def __init__(self) -> None:
            self.extractions = []
            self.synthesis_inputs = []

        def extract_evidence(self, payload):
            self.extractions.append(payload)
            return {
                "evidence_snippets": ["Your appointment is confirmed"],
                "evidence_summary": "Confirmed personal appointment.",
            }

        def synthesize(self, payload):
            self.synthesis_inputs.append(payload)
            return {
                "policy_candidates": [], "conflicts": [],
                "preference_changes": [],
            }

    client = FakeFeedbackClient()
    result = FeedbackSynthesisService(store, reviews, client).synthesize(limit=50)

    assert result.selected == 1
    assert len(client.extractions) == 1
    extraction = client.extractions[0]
    assert extraction["human_comment"] == ""
    assert extraction["feedback_signal"] == "accepted_reviewer_disagreement"
    assert extraction["reviewer_initial_assessment"][
        "suggested_classification"
    ] == case.suggested_classification
    assert extraction["reviewer_initial_assessment"]["reason_summary"]
    assert client.synthesis_inputs[0][0]["feedback_signal"] == (
        "accepted_reviewer_disagreement"
    )
    assert client.synthesis_inputs[0][0]["reviewer_reason_summary"]
    assert client.synthesis_inputs[0][0]["system_final_classification"] == (
        case.final_classification
    )
    assert client.synthesis_inputs[0][0]["reviewer_suggested_classification"] == (
        case.suggested_classification
    )
    store.close()


def test_synthesis_selects_all_disagreements_and_marks_human_authority(
    tmp_path: Path,
) -> None:
    store, reviews = _seed_review_data(tmp_path)
    reviews.run()
    cases = {case.message_id: case for case in reviews.list_cases()}
    reviews.accept_reviewer(cases["msg-agree"].review_case_id)
    reviews.accept_reviewer(cases["msg-disagree"].review_case_id)
    reviews.accept_reviewer(cases["msg-uncertain"].review_case_id)

    class UnusedFeedbackClient:
        model_name = "unused"

    selected = FeedbackSynthesisService(
        store, reviews, UnusedFeedbackClient()
    )._select_cases(since=None, limit=50)

    assert {case.review_case_id for case in selected} == {
        cases["msg-disagree"].review_case_id,
    }
    store.close()


def test_empty_policy_export_explains_that_no_change_was_proposed(
    tmp_path: Path,
) -> None:
    store, reviews = _seed_review_data(tmp_path)
    reviews.run()
    case = next(case for case in reviews.list_cases() if case.message_id == "msg-disagree")
    reviews.accept_reviewer(case.review_case_id)

    class NoPolicyClient:
        model_name = "frontier-feedback-test"

        def extract_evidence(self, payload):
            del payload
            return {"evidence_snippets": ["Your appointment is confirmed"],
                    "evidence_summary": "Confirmed personal appointment."}

        def synthesize(self, payload):
            del payload
            return {"policy_candidates": [], "conflicts": [],
                    "preference_changes": []}

    synthesis = FeedbackSynthesisService(store, reviews, NoPolicyClient())
    result = synthesis.synthesize(limit=50)
    markdown = synthesis.export(synthesis_id=result.synthesis_id)

    assert f"Synthesis ID: {result.synthesis_id}" in markdown
    assert "Policy candidates: 0" in markdown
    assert "No policy changes were proposed" in markdown
    assert "Reviewer disagreement evidence" in markdown
    assert case.review_case_id in markdown
    store.close()


def test_synthesis_includes_unresolved_reviewer_disagreement(
    tmp_path: Path,
) -> None:
    store, reviews = _seed_review_data(tmp_path)
    reviews.run()
    cases = {case.message_id: case for case in reviews.list_cases()}

    selected = FeedbackSynthesisService(
        store, reviews, type("UnusedClient", (), {"model_name": "unused"})()
    )._select_cases(since=None, limit=50)

    assert cases["msg-disagree"].review_case_id in {
        case.review_case_id for case in selected
    }
    assert FeedbackSynthesisService._feedback_signal(
        cases["msg-disagree"]
    ) == "reviewer_disagreement_unconfirmed"
    assert cases["msg-agree"].review_case_id not in {
        case.review_case_id for case in selected
    }
    store.close()


def test_feedback_prompts_limit_evidence_and_do_not_apply_policy() -> None:
    assert "verbatim substrings" in EVIDENCE_PROMPT
    assert "Do not overgeneralize" in POLICY_PROMPT
    assert "time-ordered preference changes" in POLICY_PROMPT
    assert "modify code, prompts, validators, or deployments" in POLICY_PROMPT


def test_feedback_cli_arguments() -> None:
    parser = _parser()
    synthesize = parser.parse_args([
        "review", "synthesize", "--since", "30d", "--limit", "50",
    ])
    export = parser.parse_args([
        "review", "export-feedback", "--latest", "--format", "markdown",
    ])
    assert synthesize.review_command == "synthesize"
    assert synthesize.since == "30d" and synthesize.limit == 50
    assert export.review_command == "export-feedback" and export.latest
