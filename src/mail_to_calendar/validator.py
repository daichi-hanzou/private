from __future__ import annotations

import re
from dataclasses import dataclass, field, replace
from datetime import date, time

from .llm_models import FinalClassification, LLMAnalysisResult
from .models import EmailMessage


@dataclass(frozen=True)
class ValidationResult:
    valid: bool
    result: LLMAnalysisResult
    issues: list[str]
    final_classification: FinalClassification
    candidate_allowed: bool
    rejected_reason: str | None = None
    classification_corrections: list[str] = field(default_factory=list)


class LLMResultValidator:
    _relative = ("今日", "明日", "来週", "来月", "today", "tomorrow", "next week")
    _security_patterns = {
        "new_sign_in": ("新規サインイン", "新しいサインイン", "new sign-in", "unusual sign-in"),
        "new_app_connection": ("新しいアプリ", "アプリへの接続", "new app", "app connected"),
        "security_code": ("セキュリティコード", "security code", "verification code"),
        "password_change": ("パスワード変更", "password changed", "password change"),
        "suspicious_access": ("不審なアクセス", "suspicious access", "security alert"),
        "account_change": ("アカウント設定変更", "account settings changed"),
    }
    _generic_event_patterns = (
        "参加者募集中", "一般募集", "おすすめセミナー", "無料ウェビナー",
        "セミナーのご案内", "ウェビナーのご案内", "展示会のご案内",
        "register now", "webinar invitation", "public seminar",
    )

    def __init__(self, *, base_year: int, confidence_threshold: float = 0.75) -> None:
        if not 0 <= confidence_threshold <= 1:
            raise ValueError("LLM confidence threshold must be between 0 and 1")
        self.base_year = base_year
        self.confidence_threshold = confidence_threshold

    def validate(
        self, result: LLMAnalysisResult, message: EmailMessage
    ) -> ValidationResult:
        text = f"{message.subject}\n{message.body_text}"
        folded = text.casefold()
        issues: list[str] = []
        proposed_classification = result.final_classification
        normalized_security_type = self._normalize_security_type(
            result.security_notification_type
        )
        temporal_validation_required = not (
            result.candidate_type == "none"
            and not result.should_create_calendar_candidate
        )
        for evidence in result.evidence:
            if evidence not in text:
                issues.append("evidence_not_in_email")
        for evidence in result.user_commitment_evidence:
            if evidence not in text:
                issues.append("user_commitment_evidence_not_in_email")
        if temporal_validation_required and result.date:
            try:
                date.fromisoformat(result.date)
            except ValueError:
                issues.append("invalid_date")
            else:
                if not self._date_grounded(result.date, text):
                    issues.append("date_not_grounded")
        for name, value in (("start", result.start), ("end", result.end)):
            if not temporal_validation_required:
                continue
            if value:
                try:
                    time.fromisoformat(value)
                except ValueError:
                    issues.append(f"invalid_{name}")
                else:
                    if not self._time_grounded(value, text):
                        issues.append(f"{name}_not_grounded")
        if (
            temporal_validation_required
            and result.location
            and result.location.casefold() not in folded
        ):
            issues.append("location_not_grounded")
        if temporal_validation_required and result.duration_minutes is not None and not self._duration_grounded(
            result.duration_minutes, text
        ):
            issues.append("duration_not_grounded")
        if temporal_validation_required and result.title and result.title.casefold() not in folded:
            issues.append("title_not_grounded")
        if temporal_validation_required and any(word.casefold() in folded for word in self._relative) and result.date:
            issues.append("relative_date_requires_clarification")
        if temporal_validation_required and result.candidate_type == "deadline" and result.start and not self._has_time(text):
            issues.append("deadline_time_was_inferred")
        candidate_requested = (
            result.should_create_calendar_candidate
            or result.final_classification == "calendar_candidate"
        )
        if result.confidence < self.confidence_threshold and (
            candidate_requested or result.clarification_required
        ):
            issues.append("confidence_below_threshold")
        detected_security_type = self._security_type(folded)
        security = bool(
            detected_security_type
            or normalized_security_type
            or result.category == "security_notification"
        )
        generic_ad = result.generic_event_advertisement or any(
            pattern.casefold() in folded for pattern in self._generic_event_patterns
        )
        promotion = result.final_classification == "promotion" or result.category == "promotion"
        if candidate_requested and security:
            issues.append("security_notification_not_calendar_candidate")
        if candidate_requested and promotion:
            issues.append("promotion_not_calendar_candidate")
        if candidate_requested and generic_ad and not result.user_commitment_detected:
            issues.append("generic_event_without_user_commitment")
        if candidate_requested and not result.user_commitment_detected:
            issues.append("user_commitment_not_detected")
        if result.user_commitment_detected and not result.user_commitment_evidence:
            issues.append("user_commitment_evidence_missing")
        if candidate_requested and not result.date:
            issues.append("candidate_date_missing")
        if result.should_create_calendar_candidate != (
            result.final_classification == "calendar_candidate"
        ):
            issues.append("candidate_classification_mismatch")

        fact_invalid = any(
            issue.endswith("not_grounded")
            or issue.startswith("invalid_")
            or issue in {
                "evidence_not_in_email",
                "user_commitment_evidence_not_in_email",
                "deadline_time_was_inferred",
            }
            for issue in issues
        )
        candidate_blockers = bool(issues) or security or promotion
        candidate_allowed = candidate_requested and not candidate_blockers
        classification: FinalClassification
        rejected_reason = None
        if candidate_allowed:
            classification = "calendar_candidate"
        elif security:
            classification = "security_notification"
        elif promotion:
            classification = "promotion"
        elif generic_ad and not result.user_commitment_detected:
            classification = (
                "informational"
                if result.category == "informational"
                else "promotion"
            )
        elif fact_invalid:
            classification = "invalid"
        elif (
            result.category == "informational"
            and not result.user_commitment_detected
            and not result.should_create_calendar_candidate
            and result.candidate_type == "none"
        ):
            classification = "informational"
        elif candidate_requested:
            classification = "clarification_required"
        elif result.clarification_required or result.final_classification == "clarification_required":
            realistic = result.is_important or result.user_commitment_detected
            classification = "clarification_required" if realistic else "informational"
        elif result.final_classification in {
            "informational", "ignored", "invalid",
        }:
            classification = result.final_classification
        else:
            classification = "informational"
        if candidate_requested and not candidate_allowed:
            rejected_reason = ", ".join(sorted(set(issues))) or classification
        corrections: list[str] = []
        if proposed_classification != classification:
            corrections.append(
                f"final_classification:{proposed_classification}->{classification}"
            )
        if result.security_notification_type != normalized_security_type:
            corrections.append("security_notification_type:sentinel->null")
        normalized = replace(
            result,
            final_classification=classification,
            should_create_calendar_candidate=candidate_allowed,
            clarification_required=classification == "clarification_required",
            generic_event_advertisement=generic_ad,
            security_notification_type=(
                normalized_security_type or detected_security_type
            ),
        )
        return ValidationResult(
            not fact_invalid,
            normalized,
            sorted(set(issues)),
            classification,
            candidate_allowed,
            rejected_reason,
            corrections,
        )

    @staticmethod
    def _normalize_security_type(value: str | None) -> str | None:
        if value is None:
            return None
        normalized = value.strip()
        if normalized.casefold() in {"", "none", "null", "n/a", "na", "unknown"}:
            return None
        return normalized

    def _security_type(self, text: str) -> str | None:
        for notification_type, patterns in self._security_patterns.items():
            if any(pattern.casefold() in text for pattern in patterns):
                return notification_type
        return None

    def _date_grounded(self, value: str, text: str) -> bool:
        parsed = date.fromisoformat(value)
        variants = {
            value,
            f"{parsed.year}年{parsed.month}月{parsed.day}日",
            f"{parsed.month}月{parsed.day}日",
            f"{parsed.month}/{parsed.day}",
        }
        if parsed.year == self.base_year:
            variants.add(f"{parsed.month:02d}-{parsed.day:02d}")
        return any(item in text for item in variants)

    @staticmethod
    def _time_grounded(value: str, text: str) -> bool:
        parsed = time.fromisoformat(value)
        hour, minute = parsed.hour, parsed.minute
        variants = {value, f"{hour}:{minute:02d}", f"{hour}時"}
        if hour >= 12:
            variants.add(f"午後{hour % 12 or 12}時")
            variants.add(f"午後 {hour % 12 or 12}時")
        else:
            variants.add(f"午前{hour or 12}時")
        return any(item in text for item in variants)

    @staticmethod
    def _duration_grounded(value: int, text: str) -> bool:
        variants = {f"{value}分", f"{value} minutes", f"{value}-minute"}
        if value % 60 == 0:
            variants.add(f"{value // 60}時間")
        return any(item.casefold() in text.casefold() for item in variants)

    @staticmethod
    def _has_time(text: str) -> bool:
        return bool(re.search(r"(?:午前|午後)?\s*\d{1,2}(?::\d{2}|時)", text))
