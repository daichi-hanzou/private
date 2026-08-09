from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Literal


Category = Literal[
    "meeting", "deadline", "appointment", "task", "informational",
    "promotion", "security_notification", "unknown",
]
CandidateType = Literal["event", "deadline", "task", "none"]
FinalClassification = Literal[
    "calendar_candidate",
    "clarification_required",
    "informational",
    "promotion",
    "security_notification",
    "ignored",
    "invalid",
]


class LLMResultValidationError(ValueError):
    pass


@dataclass(frozen=True)
class LLMAnalysisInput:
    sender: str
    subject: str
    received_at: str
    body_text: str
    timezone: str
    base_year: int
    rule_result: dict[str, Any]
    rule_datetime_candidates: dict[str, Any]
    importance_hint: str | None
    categories: list[str]
    has_attachments: bool
    input_truncated: bool = False


@dataclass(frozen=True)
class LLMAnalysisResult:
    is_important: bool
    importance_score: float
    category: Category
    should_create_calendar_candidate: bool
    candidate_type: CandidateType
    title: str | None
    date: str | None
    start: str | None
    end: str | None
    duration_minutes: int | None
    timezone: str
    location: str | None
    clarification_required: bool
    final_classification: FinalClassification
    user_commitment_detected: bool
    user_commitment_evidence: list[str]
    generic_event_advertisement: bool
    security_notification_type: str | None
    should_notify_user: bool
    clarification_questions: list[str] = field(default_factory=list)
    confidence: float = 0.0
    reasons: list[str] = field(default_factory=list)
    evidence: list[str] = field(default_factory=list)
    fields_inferred: list[str] = field(default_factory=list)
    suspicious_instructions_detected: bool = False
    suspicious_instruction_summary: str | None = None
    reported_should_create_calendar_candidate: bool | None = None

    @classmethod
    def from_dict(cls, value: dict[str, Any]) -> "LLMAnalysisResult":
        if not isinstance(value, dict):
            raise LLMResultValidationError("LLM result must be an object")
        legacy_field = "should_create_calendar_candidate"
        internal_field = "reported_should_create_calendar_candidate"
        expected = set(cls.__dataclass_fields__) - {legacy_field, internal_field}
        missing = expected - value.keys()
        extra = value.keys() - expected - {legacy_field}
        if missing:
            raise LLMResultValidationError(
                "missing LLM fields: " + ", ".join(sorted(missing))
            )
        if extra:
            raise LLMResultValidationError(
                "unexpected LLM fields: " + ", ".join(sorted(extra))
            )
        if type(value["is_important"]) is not bool:
            raise LLMResultValidationError("is_important must be boolean")
        reported_candidate = value.get(legacy_field)
        if reported_candidate is not None and type(reported_candidate) is not bool:
            raise LLMResultValidationError(
                "should_create_calendar_candidate must be boolean"
            )
        if type(value["clarification_required"]) is not bool:
            raise LLMResultValidationError("clarification_required must be boolean")
        for key in (
            "user_commitment_detected",
            "generic_event_advertisement",
            "should_notify_user",
        ):
            if type(value[key]) is not bool:
                raise LLMResultValidationError(f"{key} must be boolean")
        if type(value["suspicious_instructions_detected"]) is not bool:
            raise LLMResultValidationError(
                "suspicious_instructions_detected must be boolean"
            )
        _number(value["importance_score"], "importance_score", 0, 1)
        _number(value["confidence"], "confidence", 0, 1)
        if value["category"] not in {
            "meeting", "deadline", "appointment", "task", "informational",
            "promotion", "security_notification", "unknown",
        }:
            raise LLMResultValidationError("invalid category")
        if value["candidate_type"] not in {"event", "deadline", "task", "none"}:
            raise LLMResultValidationError("invalid candidate_type")
        if value["final_classification"] not in {
            "calendar_candidate", "clarification_required", "informational",
            "promotion", "security_notification", "ignored", "invalid",
        }:
            raise LLMResultValidationError("invalid final_classification")
        duration = value["duration_minutes"]
        if duration is not None and (
            type(duration) is not int or not 1 <= duration <= 1440
        ):
            raise LLMResultValidationError("duration_minutes must be 1..1440 or null")
        for key in (
            "title", "date", "start", "end", "location",
            "security_notification_type", "suspicious_instruction_summary",
        ):
            item = value[key]
            if item is not None and not isinstance(item, str):
                raise LLMResultValidationError(f"{key} must be a string or null")
            if isinstance(item, str) and len(item) > 300:
                raise LLMResultValidationError(f"{key} is too long")
        if not isinstance(value["timezone"], str) or not value["timezone"]:
            raise LLMResultValidationError("timezone must be a non-empty string")
        limits = {
            "clarification_questions": (5, 200),
            "reasons": (5, 200),
            "evidence": (5, 100),
            "fields_inferred": (10, 60),
            "user_commitment_evidence": (5, 100),
        }
        for key, (count, length) in limits.items():
            items = value[key]
            if not isinstance(items, list) or any(not isinstance(x, str) for x in items):
                raise LLMResultValidationError(f"{key} must be a string list")
            if len(items) > count or any(len(x) > length for x in items):
                raise LLMResultValidationError(f"{key} exceeds its size limit")
        normalized = {
            key: item for key, item in value.items() if key != legacy_field
        }
        normalized[legacy_field] = (
            value["final_classification"] == "calendar_candidate"
        )
        normalized[internal_field] = reported_candidate
        return cls(**normalized)

    def summary(self) -> dict[str, Any]:
        return {
            "is_important": self.is_important,
            "category": self.category,
            "candidate_type": self.candidate_type,
            "should_create_calendar_candidate": (
                self.should_create_calendar_candidate
            ),
            "title": self.title,
            "date": self.date,
            "start": self.start,
            "end": self.end,
            "duration_minutes": self.duration_minutes,
            "timezone": self.timezone,
            "location": self.location,
            "clarification_required": self.clarification_required,
            "final_classification": self.final_classification,
            "user_commitment_detected": self.user_commitment_detected,
            "generic_event_advertisement": self.generic_event_advertisement,
            "security_notification_type": self.security_notification_type,
            "should_notify_user": self.should_notify_user,
            "confidence": self.confidence,
            "suspicious_instructions_detected": self.suspicious_instructions_detected,
        }

    @classmethod
    def json_schema(cls) -> dict[str, Any]:
        nullable_string = {"type": ["string", "null"], "maxLength": 300}
        properties: dict[str, Any] = {
            "is_important": {"type": "boolean"},
            "importance_score": {"type": "number", "minimum": 0, "maximum": 1},
            "category": {"type": "string", "enum": ["meeting", "deadline", "appointment", "task", "informational", "promotion", "security_notification", "unknown"]},
            "candidate_type": {"type": "string", "enum": ["event", "deadline", "task", "none"]},
            "title": nullable_string,
            "date": nullable_string,
            "start": nullable_string,
            "end": nullable_string,
            "duration_minutes": {"anyOf": [{"type": "integer", "minimum": 1, "maximum": 1440}, {"type": "null"}]},
            "timezone": {"type": "string", "minLength": 1, "maxLength": 100},
            "location": nullable_string,
            "clarification_required": {"type": "boolean"},
            "final_classification": {"type": "string", "enum": ["calendar_candidate", "clarification_required", "informational", "promotion", "security_notification", "ignored", "invalid"]},
            "user_commitment_detected": {"type": "boolean"},
            "user_commitment_evidence": _string_array_schema(5, 100),
            "generic_event_advertisement": {"type": "boolean"},
            "security_notification_type": nullable_string,
            "should_notify_user": {"type": "boolean"},
            "clarification_questions": _string_array_schema(5, 200),
            "confidence": {"type": "number", "minimum": 0, "maximum": 1},
            "reasons": _string_array_schema(5, 200),
            "evidence": _string_array_schema(5, 100),
            "fields_inferred": _string_array_schema(10, 60),
            "suspicious_instructions_detected": {"type": "boolean"},
            "suspicious_instruction_summary": nullable_string,
        }
        return {
            "type": "object",
            "properties": properties,
            "required": list(properties),
            "additionalProperties": False,
        }


def _number(value: Any, name: str, low: float, high: float) -> None:
    if type(value) not in (int, float) or not low <= value <= high:
        raise LLMResultValidationError(f"{name} must be between {low} and {high}")


def _string_array_schema(max_items: int, max_length: int) -> dict[str, Any]:
    return {
        "type": "array", "maxItems": max_items,
        "items": {"type": "string", "maxLength": max_length},
    }


@dataclass(frozen=True)
class HybridAnalysisResult:
    rule_result: dict[str, Any]
    llm_used: bool
    llm_result: LLMAnalysisResult | None
    final_source: Literal["rule", "llm", "merged", "fallback_rule", "clarification"]
    final_importance: Any
    final_candidate: Any
    confidence: float
    validation_issues: list[str]
    fallback_reason: str | None
    latency_ms: int
    model_name: str | None
    input_truncated: bool = False
    final_classification: FinalClassification = "invalid"
    calendar_candidate_rejected_reason: str | None = None
    llm_proposed_classification: FinalClassification | None = None
    classification_corrections: list[str] = field(default_factory=list)
    time_normalization: dict[str, str | int] | None = None
    llm_result_summary: dict[str, Any] | None = None
