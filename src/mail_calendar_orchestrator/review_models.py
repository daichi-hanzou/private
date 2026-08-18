from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Literal


ReviewStatus = Literal["agree", "disagreement", "uncertain"]
HumanVerdict = Literal[
    "original_correct", "reviewer_correct", "modified", "unresolved",
]
RecommendedChangeTarget = Literal[
    "qwen_prompt", "validator", "deterministic_rule", "test", "none",
]


@dataclass(frozen=True)
class ReviewResult:
    review_status: ReviewStatus
    suggested_classification: str
    issue_type: str
    confidence: float
    reason_summary: str
    needs_human_review: bool

    @classmethod
    def from_dict(cls, value: dict[str, Any]) -> "ReviewResult":
        if not isinstance(value, dict):
            raise ValueError("review result must be an object")
        status = str(value.get("review_status") or "")
        if status not in {"agree", "disagreement", "uncertain"}:
            raise ValueError("invalid review_status")
        confidence = value.get("confidence")
        if type(confidence) not in (int, float) or not 0 <= confidence <= 1:
            raise ValueError("confidence must be between 0 and 1")
        return cls(
            review_status=status,  # type: ignore[arg-type]
            suggested_classification=str(
                value.get("suggested_classification") or ""
            )[:80],
            issue_type=str(value.get("issue_type") or "")[:80],
            confidence=float(confidence),
            reason_summary=str(value.get("reason_summary") or "")[:500],
            needs_human_review=bool(value.get("needs_human_review")),
        )


@dataclass(frozen=True)
class ReviewContext:
    provider: str
    message_id: str
    source_run_id: str | None
    source_jsonl_path: str
    message_source_ref: str | None
    received_at: str | None
    subject: str
    analysis_text: str
    original_classification: str | None
    final_classification: str | None
    is_important: bool
    should_notify_user: bool | None
    llm_should_notify_user: bool | None
    calendar_candidate: dict[str, Any] | None
    validator_result: dict[str, Any]
    deterministic_corrections: list[str]
    line_notification: dict[str, Any] | None
    approval: dict[str, Any] | None
    calendar_execution: dict[str, Any] | None
    event_snapshot: dict[str, Any]

    def to_prompt_payload(self) -> dict[str, Any]:
        return {
            "provider": self.provider,
            "message_id": self.message_id,
            "received_at": self.received_at,
            "subject": self.subject,
            "analysis_text": self.analysis_text,
            "original_classification": self.original_classification,
            "final_classification": self.final_classification,
            "is_important": self.is_important,
            "should_notify_user": self.should_notify_user,
            "llm_should_notify_user": self.llm_should_notify_user,
            "calendar_candidate": self.calendar_candidate,
            "validator_result": self.validator_result,
            "deterministic_corrections": self.deterministic_corrections,
            "line_notification": self.line_notification,
            "approval": self.approval,
            "calendar_execution": self.calendar_execution,
        }


@dataclass(frozen=True)
class ReviewCaseRecord:
    review_case_id: str
    provider: str
    message_id: str
    source_run_id: str | None
    source_jsonl_path: str
    message_source_ref: str | None
    reviewer_model: str
    review_version: str
    review_status: ReviewStatus
    suggested_classification: str
    issue_type: str
    confidence: float
    reason_summary: str
    needs_human_review: bool
    original_classification: str | None
    final_classification: str | None
    human_review_status: str
    human_verdict: str | None
    human_final_classification: str | None
    lesson_summary: str | None
    recommended_change_target: str | None
    created_at: datetime
    updated_at: datetime
    human_reviewed_at: datetime | None


@dataclass(frozen=True)
class ReviewChatMessage:
    role: Literal["user", "assistant"]
    content: str
    created_at: datetime


@dataclass(frozen=True)
class HumanVerdictInput:
    human_verdict: HumanVerdict
    final_classification: str
    lesson_summary: str
    recommended_change_target: RecommendedChangeTarget


@dataclass(frozen=True)
class ReviewRunResult:
    selected: int
    created: int
    skipped_existing: int
    queue_items: int
    case_ids: list[str] = field(default_factory=list)
