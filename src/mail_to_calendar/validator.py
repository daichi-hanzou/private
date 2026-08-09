from __future__ import annotations

import re
from dataclasses import dataclass, field, replace
from datetime import date, datetime, time, timedelta
from zoneinfo import ZoneInfo, ZoneInfoNotFoundError

from .llm_models import FinalClassification, LLMAnalysisResult
from .models import EmailMessage
from .time_normalization import normalize_calendar_datetime, normalize_calendar_time


@dataclass(frozen=True)
class ValidationResult:
    valid: bool
    result: LLMAnalysisResult
    issues: list[str]
    final_classification: FinalClassification
    candidate_allowed: bool
    rejected_reason: str | None = None
    classification_corrections: list[str] = field(default_factory=list)
    time_normalization: dict[str, str | int] | None = None


class LLMResultValidator:
    _unresolved_relative = ("来週", "来月", "next week", "next month")
    _weekday_offsets = {
        "月": 0, "火": 1, "水": 2, "木": 3,
        "金": 4, "土": 5, "日": 6,
    }
    _security_patterns = {
        "new_sign_in": ("新規サインイン", "新しいサインイン", "new sign-in", "unusual sign-in"),
        "new_app_connection": ("新しいアプリ", "アプリへの接続", "new app", "app connected"),
        "security_code": ("セキュリティコード", "security code", "verification code"),
        "password_change": ("パスワード変更", "password changed", "password change"),
        "suspicious_access": ("不審なアクセス", "suspicious access", "security alert"),
        "account_change": ("アカウント設定変更", "account settings changed"),
    }
    _security_type_aliases = {
        "new_signin": "new_sign_in",
        "sign_in": "new_sign_in",
        "new_application_connection": "new_app_connection",
        "app_connection": "new_app_connection",
        "password_changed": "password_change",
        "security_alert": "suspicious_access",
        "suspicious_sign_in": "suspicious_access",
        "account_settings_change": "account_change",
    }
    _allowed_security_types = frozenset(
        {
            *_security_patterns.keys(),
            "account_activity",
            "account_connection",
            "account_setting_change",
        }
    )
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
        detected_security_type = self._security_type(folded)
        generic_ad = result.generic_event_advertisement or any(
            pattern.casefold() in folded for pattern in self._generic_event_patterns
        )
        promotion = (
            result.final_classification == "promotion"
            or result.category == "promotion"
        )
        security = bool(
            detected_security_type
            or normalized_security_type
            or result.category == "security_notification"
        )
        semantic_corrections: list[str] = []
        commitment_grounded = bool(
            result.user_commitment_evidence
            and any(item in text for item in result.user_commitment_evidence)
        )
        grounded_personal_datetime = self._personal_datetime_grounded(
            result, message, text
        )
        if (
            result.final_classification == "informational"
            and result.user_commitment_detected
            and commitment_grounded
            and not generic_ad
            and not promotion
            and not security
        ):
            if grounded_personal_datetime:
                original_candidate_type = result.candidate_type
                result = replace(
                    result,
                    final_classification="calendar_candidate",
                    candidate_type=(
                        "event" if result.candidate_type == "none"
                        else result.candidate_type
                    ),
                    should_create_calendar_candidate=True,
                )
                semantic_corrections.append(
                    "semantic_consistency:informational->"
                    "calendar_candidate_due_to_grounded_user_commitment"
                )
                if original_candidate_type == "none":
                    semantic_corrections.append(
                        "candidate_type:none->"
                        "event_due_to_grounded_user_commitment"
                    )
            elif not result.date or not result.start:
                original_candidate_type = result.candidate_type
                result = replace(
                    result,
                    final_classification="clarification_required",
                    candidate_type=(
                        "event" if result.candidate_type == "none"
                        else result.candidate_type
                    ),
                    should_create_calendar_candidate=False,
                    clarification_required=True,
                )
                semantic_corrections.append(
                    "semantic_consistency:informational->"
                    "clarification_required_due_to_incomplete_user_commitment"
                )
                if original_candidate_type == "none":
                    semantic_corrections.append(
                        "candidate_type:none->"
                        "event_due_to_grounded_user_commitment"
                    )
        temporal_validation_required = not (
            result.candidate_type == "none"
            and not result.should_create_calendar_candidate
        )
        normalized_date = result.date
        normalized_start = result.start
        normalized_end = result.end
        normalized_duration = result.duration_minutes
        normalized_location = self._normalize_optional_text(result.location)
        time_normalization = None
        start_grounded = False
        end_grounded = False
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
                if not self._date_grounded(
                    result.date, text, message, result.timezone
                ):
                    issues.append("date_not_grounded")
                relative_dates = self._relative_dates(
                    text, message, result.timezone
                )
                if relative_dates and date.fromisoformat(result.date) not in relative_dates:
                    issues.append("relative_date_mismatch")
        if temporal_validation_required and result.start:
            try:
                normalized_datetime = normalize_calendar_datetime(
                    result.date,
                    result.start,
                    original_time_expression=result.start,
                )
            except ValueError:
                issues.append("invalid_start")
            else:
                if (
                    normalized_datetime.source_date
                    and normalized_datetime.source_date != result.date
                ):
                    issues.append("invalid_start_date_mismatch")
                if not self._timezone_consistent(
                    normalized_datetime,
                    result.timezone,
                ):
                    issues.append("invalid_start_timezone_mismatch")
                normalized_date = normalized_datetime.date
                normalized_start = normalized_datetime.time
                time_normalization = normalized_datetime.audit_data()
                grounding_value = (
                    normalized_start
                    if normalized_datetime.source_date
                    else result.start
                )
                start_grounded = self._time_grounded(grounding_value, text)
                if not start_grounded:
                    issues.append("start_not_grounded")
        if temporal_validation_required and result.end:
            try:
                normalized_datetime = normalize_calendar_datetime(
                    result.date,
                    result.end,
                    original_time_expression=result.end,
                )
            except ValueError:
                issues.append("invalid_end")
            else:
                if (
                    normalized_datetime.source_date
                    and normalized_datetime.source_date != result.date
                ):
                    issues.append("invalid_end_date_mismatch")
                if not self._timezone_consistent(
                    normalized_datetime,
                    result.timezone,
                ):
                    issues.append("invalid_end_timezone_mismatch")
                normalized_end = normalized_datetime.time
                grounding_value = (
                    normalized_end
                    if normalized_datetime.source_date
                    else result.end
                )
                end_grounded = self._time_grounded(grounding_value, text)
                if not end_grounded:
                    issues.append("end_not_grounded")
        if (
            temporal_validation_required
            and normalized_location
            and normalized_location.casefold() not in folded
        ):
            issues.append("location_not_grounded")
        grounded_range_duration = self._grounded_range_duration(
            normalized_start,
            normalized_end,
            start_grounded=start_grounded,
            end_grounded=end_grounded,
        )
        if temporal_validation_required and result.duration_minutes is not None:
            if (
                grounded_range_duration is not None
                and grounded_range_duration != result.duration_minutes
            ):
                issues.append("duration_mismatch")
            elif grounded_range_duration is None and not self._duration_grounded(
                result.duration_minutes, text
            ):
                issues.append("duration_not_grounded")
        elif temporal_validation_required and grounded_range_duration is not None:
            normalized_duration = grounded_range_duration
        if temporal_validation_required and result.title and result.title.casefold() not in folded:
            issues.append("title_not_grounded")
        if (
            temporal_validation_required
            and result.date
            and self._has_unresolved_relative(text)
        ):
            issues.append("relative_date_requires_clarification")
        if temporal_validation_required and result.candidate_type == "deadline" and result.start and not self._has_time(text):
            issues.append("deadline_time_was_inferred")
        candidate_requested = result.final_classification == "calendar_candidate"
        if result.confidence < self.confidence_threshold and (
            candidate_requested or result.clarification_required
        ):
            issues.append("confidence_below_threshold")
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
        if candidate_requested and result.end and not result.start:
            issues.append("candidate_start_missing")
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
        corrections: list[str] = list(semantic_corrections)
        reported_candidate = result.reported_should_create_calendar_candidate
        semantic_candidate = proposed_classification == "calendar_candidate"
        if (
            reported_candidate is not None
            and reported_candidate != semantic_candidate
        ):
            corrections.append(
                "should_create_calendar_candidate:"
                f"{str(reported_candidate).casefold()}->"
                f"{str(semantic_candidate).casefold()}_from_final_classification"
            )
        if result.security_notification_type != normalized_security_type:
            original_type = (result.security_notification_type or "").strip()
            corrections.append(
                "security_notification_type:"
                + (original_type.casefold() or "<empty>")
                + "->"
                + (normalized_security_type or "null")
            )
        if proposed_classification != classification:
            corrections.append(
                f"final_classification:{proposed_classification}->{classification}"
            )
        normalized = replace(
            result,
            date=normalized_date,
            start=normalized_start,
            end=normalized_end,
            duration_minutes=normalized_duration,
            location=normalized_location,
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
            time_normalization,
        )

    @classmethod
    def _normalize_security_type(cls, value: str | None) -> str | None:
        if value is None:
            return None
        normalized = value.strip().casefold().replace("-", "_").replace(" ", "_")
        if normalized in {
            "",
            "none",
            "null",
            "n/a",
            "na",
            "unknown",
            "ignored",
            "ignore",
            "not_applicable",
            "notapplicable",
        }:
            return None
        normalized = cls._security_type_aliases.get(normalized, normalized)
        if normalized not in cls._allowed_security_types:
            return None
        return normalized

    def _security_type(self, text: str) -> str | None:
        for notification_type, patterns in self._security_patterns.items():
            if any(pattern.casefold() in text for pattern in patterns):
                return notification_type
        return None

    @staticmethod
    def _normalize_optional_text(value: str | None) -> str | None:
        if value is None:
            return None
        stripped = value.strip()
        if stripped.casefold() in {
            "", "none", "null", "unknown", "n/a", "na", "not applicable",
            "not specified", "unspecified", "not provided", "no location",
            "なし", "未指定", "記載なし",
        }:
            return None
        return stripped

    def _date_grounded(
        self,
        value: str,
        text: str,
        message: EmailMessage | None = None,
        timezone_name: str | None = None,
    ) -> bool:
        parsed = date.fromisoformat(value)
        variants = {
            value,
            f"{parsed.year}年{parsed.month}月{parsed.day}日",
            f"{parsed.month}月{parsed.day}日",
            f"{parsed.month}/{parsed.day}",
        }
        if parsed.year == self.base_year:
            variants.add(f"{parsed.month:02d}-{parsed.day:02d}")
        if any(item in text for item in variants):
            return True
        if message is None or timezone_name is None:
            return False
        return parsed in self._relative_dates(text, message, timezone_name)

    @classmethod
    def _relative_dates(
        cls, text: str, message: EmailMessage, timezone_name: str
    ) -> set[date]:
        try:
            received = datetime.fromisoformat(
                message.received_at.replace("Z", "+00:00")
            )
            if received.tzinfo is None:
                return set()
            local_date = received.astimezone(ZoneInfo(timezone_name)).date()
        except (ValueError, ZoneInfoNotFoundError):
            return set()
        resolved: set[date] = set()
        if "今日" in text or "本日" in text or re.search(r"\btoday\b", text, re.I):
            resolved.add(local_date)
        if "明後日" in text:
            resolved.add(local_date + timedelta(days=2))
        if "明日" in text or re.search(r"\btomorrow\b", text, re.I):
            resolved.add(local_date + timedelta(days=1))
        monday = local_date - timedelta(days=local_date.weekday())
        for week, weekday in re.findall(
            r"(今週|来週)(?:の)?([月火水木金土日])曜(?:日)?", text
        ):
            week_offset = 7 if week == "来週" else 0
            resolved.add(
                monday
                + timedelta(days=week_offset + cls._weekday_offsets[weekday])
            )
        for week, weekday in re.findall(
            r"\b(this|next)\s+(monday|tuesday|wednesday|thursday|friday|saturday|sunday)\b",
            text,
            re.I,
        ):
            names = {
                "monday": 0, "tuesday": 1, "wednesday": 2,
                "thursday": 3, "friday": 4, "saturday": 5, "sunday": 6,
            }
            resolved.add(
                monday
                + timedelta(
                    days=(7 if week.casefold() == "next" else 0)
                    + names[weekday.casefold()]
                )
            )
        return resolved

    @classmethod
    def _has_unresolved_relative(cls, text: str) -> bool:
        scrubbed = re.sub(
            r"(?:今週|来週)(?:の)?[月火水木金土日]曜(?:日)?", "", text
        )
        scrubbed = re.sub(
            r"\b(?:this|next)\s+(?:monday|tuesday|wednesday|thursday|friday|saturday|sunday)\b",
            "", scrubbed, flags=re.I,
        )
        return any(
            value.casefold() in scrubbed.casefold()
            for value in cls._unresolved_relative
        )

    def _personal_datetime_grounded(
        self, result: LLMAnalysisResult, message: EmailMessage, text: str
    ) -> bool:
        if not result.date or not result.start:
            return False
        try:
            date.fromisoformat(result.date)
            normalized = normalize_calendar_datetime(result.date, result.start)
        except ValueError:
            return False
        grounding_value = normalized.time if normalized.source_date else result.start
        return self._date_grounded(
            result.date, text, message, result.timezone
        ) and bool(
            grounding_value and self._time_grounded(grounding_value, text)
        )

    @staticmethod
    def _time_grounded(value: str, text: str) -> bool:
        hour, minute = normalize_calendar_time(value)
        if hour == 24:
            return "24:00" in text or bool(
                re.search(r"24時(?:00分)?(?!\d)", text)
            )
        variants = {f"{hour:02d}:{minute:02d}", f"{hour}:{minute:02d}"}
        if minute:
            variants.add(f"{hour}時{minute:02d}分")
            variants.add(f"{hour}時{minute}分")
        else:
            variants.add(f"{hour}時00分")
        if hour >= 12:
            period_hour = hour % 12 or 12
            suffix = f"{minute:02d}分" if minute else ""
            if suffix:
                variants.add(f"午後{period_hour}時{suffix}")
                variants.add(f"午後 {period_hour}時{suffix}")
            elif re.search(rf"午後\s*{period_hour}時(?!\d)", text):
                return True
        else:
            period_hour = hour or 12
            suffix = f"{minute:02d}分" if minute else ""
            if suffix:
                variants.add(f"午前{period_hour}時{suffix}")
            elif re.search(rf"午前\s*{period_hour}時(?!\d)", text):
                return True
        if not minute and re.search(rf"(?<!\d){hour}時(?!\d)", text):
            return True
        return any(item in text for item in variants)

    @staticmethod
    def _duration_grounded(value: int, text: str) -> bool:
        variants = {f"{value}分", f"{value} minutes", f"{value}-minute"}
        if value % 60 == 0:
            variants.add(f"{value // 60}時間")
        return any(item.casefold() in text.casefold() for item in variants)

    @staticmethod
    def _grounded_range_duration(
        start: str | None,
        end: str | None,
        *,
        start_grounded: bool,
        end_grounded: bool,
    ) -> int | None:
        if not (start and end and start_grounded and end_grounded):
            return None
        start_time = time.fromisoformat(start)
        end_time = time.fromisoformat(end)
        start_value = datetime.combine(date.min, start_time)
        end_value = datetime.combine(date.min, end_time)
        minutes = int((end_value - start_value).total_seconds() // 60)
        return minutes if minutes > 0 else None

    @staticmethod
    def _timezone_consistent(normalized_datetime, timezone_name: str) -> bool:
        if normalized_datetime.utc_offset_seconds is None:
            return True
        if not normalized_datetime.source_date or not normalized_datetime.time:
            return False
        try:
            zone = ZoneInfo(timezone_name)
            local = datetime.fromisoformat(
                f"{normalized_datetime.source_date}T{normalized_datetime.time}"
            ).replace(tzinfo=zone)
        except (ValueError, ZoneInfoNotFoundError):
            return False
        expected = local.utcoffset()
        return (
            expected is not None
            and int(expected.total_seconds())
            == normalized_datetime.utc_offset_seconds
        )

    @staticmethod
    def _has_time(text: str) -> bool:
        return bool(re.search(r"(?:午前|午後)?\s*\d{1,2}(?::\d{2}|時)", text))
