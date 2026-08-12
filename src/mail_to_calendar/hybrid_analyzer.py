from __future__ import annotations

import hashlib
import re
import time
from dataclasses import replace
from typing import Literal

from .llm_classifier import LLMCalendarClassifier
from .llm_models import HybridAnalysisResult, LLMAnalysisInput
from .models import (
    CalendarCandidate, EmailMessage, ImportanceResult, canonical_message_id,
)
from .ollama_client import OllamaError
from .reservation_evidence import detect_strong_personal_reservation
from .notification_evidence import detect_important_notification_evidence
from .validator import LLMResultValidator
from .text_normalization import (
    contains_japanese_explicit_date, date_detection_text, nfkc_text,
    without_transport_headers,
)


AnalysisMode = Literal["rule-only", "hybrid", "llm-first", "llm-all"]


class HybridMailAnalyzer:
    def __init__(
        self,
        classifier: LLMCalendarClassifier | None,
        *,
        base_year: int,
        timezone: str,
        mode: AnalysisMode = "hybrid",
        confidence_threshold: float = 0.75,
        require_llm: bool = False,
    ) -> None:
        if mode not in {"rule-only", "hybrid", "llm-first", "llm-all"}:
            raise ValueError("invalid analysis mode")
        if mode in {"llm-first", "llm-all"} and classifier is None:
            raise ValueError(f"{mode} requires an Ollama classifier")
        self.classifier = classifier
        self.base_year = base_year
        self.timezone = timezone
        self.mode = mode
        self.require_llm = require_llm
        self.validator = LLMResultValidator(
            base_year=base_year, confidence_threshold=confidence_threshold
        )

    def analyze(
        self,
        message: EmailMessage,
        rule_importance: ImportanceResult,
        rule_candidate: CalendarCandidate | None,
    ) -> HybridAnalysisResult:
        summary = {
            "is_important": rule_importance.is_important,
            "score": rule_importance.score,
            "category": rule_importance.category,
            "reasons": list(rule_importance.reasons),
        }
        if not self._should_use_llm(rule_importance, rule_candidate):
            classification = self._rule_classification(
                rule_importance, rule_candidate
            )
            return HybridAnalysisResult(
                summary, False, None, "rule", rule_importance, rule_candidate,
                rule_importance.score, [], None, 0, None, False,
                classification, None,
            )
        if self.classifier is None:
            return self._fallback(summary, rule_importance, rule_candidate, "Ollama is not configured", 0)
        started = time.monotonic()
        clean_body = without_transport_headers(message.body_text)
        canonical_body = self.classifier.canonical_body(clean_body)
        source_body_truncated = len(canonical_body) < len(clean_body)
        analysis_message = replace(message, body_text=canonical_body)
        value = LLMAnalysisInput(
            sender=message.sender,
            subject=message.subject,
            received_at=message.received_at,
            body_text=analysis_message.body_text,
            timezone=self.timezone,
            base_year=self.base_year,
            rule_result=summary,
            rule_datetime_candidates={
                "date": rule_candidate.date if rule_candidate else None,
                "start": rule_candidate.start if rule_candidate else None,
                "end": rule_candidate.end if rule_candidate else None,
            },
            importance_hint=message.importance_hint,
            categories=list(message.labels),
            has_attachments=message.has_attachments,
        )
        try:
            llm = self.classifier.analyze(value)
            self.classifier.last_input_truncated = source_body_truncated
            original_llm = llm
            reservation = detect_strong_personal_reservation(
                f"{analysis_message.subject}\n{analysis_message.body_text}",
                base_year=self.base_year,
            )
            semantic_corrections: list[str] = []
            rule_derived_datetime_used = False
            if (
                reservation.detected
                and llm.final_classification in {"promotion", "informational"}
            ):
                original_classification = llm.final_classification
                derived_date = llm.date or reservation.derived_date
                derived_start = llm.start or reservation.derived_start
                rule_derived_datetime_used = bool(
                    (not llm.date and derived_date)
                    or (not llm.start and derived_start)
                )
                llm = replace(
                    llm,
                    final_classification="calendar_candidate",
                    should_create_calendar_candidate=True,
                    candidate_type=(
                        "event" if llm.candidate_type == "none"
                        else llm.candidate_type
                    ),
                    title=llm.title or analysis_message.subject,
                    date=derived_date,
                    start=derived_start,
                    user_commitment_detected=True,
                    user_commitment_evidence=(
                        llm.user_commitment_evidence
                        or [reservation.commitment_evidence]
                    ),
                    generic_event_advertisement=False,
                )
                semantic_corrections.append(
                    f"semantic_consistency:{original_classification}->"
                    "calendar_candidate_due_to_strong_personal_reservation_evidence"
                )
                if not original_llm.user_commitment_detected:
                    semantic_corrections.append(
                        "user_commitment_detected:false->"
                        "true_from_strong_personal_reservation_evidence"
                    )
                if rule_derived_datetime_used:
                    semantic_corrections.append(
                        "datetime:null->rule_derived_from_strong_personal_reservation_evidence"
                    )
            checked = self.validator.validate(llm, analysis_message)
            notification = detect_important_notification_evidence(
                f"{analysis_message.subject}\n{analysis_message.body_text}",
                base_year=self.base_year,
            )
            notification_override = bool(
                notification.detected
                and checked.result.is_important
                and checked.final_classification
                not in {"calendar_candidate", "promotion", "security_notification"}
                and not checked.result.generic_event_advertisement
                and not checked.result.should_notify_user
            )
            if notification_override:
                checked = replace(
                    checked,
                    result=replace(checked.result, should_notify_user=True),
                )
                semantic_corrections.append(
                    "should_notify_user:false->true_"
                    "from_important_deadline_with_user_obligation"
                )
        except (OllamaError, ValueError) as exc:
            latency = int((time.monotonic() - started) * 1000)
            if self.require_llm or self.mode == "llm-all":
                raise RuntimeError(f"required LLM analysis failed: {exc}") from exc
            return self._fallback(summary, rule_importance, rule_candidate, str(exc), latency)
        latency = int((time.monotonic() - started) * 1000)
        truncated = source_body_truncated
        conflict = self._conflicts(
            rule_importance,
            rule_candidate,
            checked.result,
            compare_importance=self.mode != "llm-first",
        )
        classification = checked.final_classification
        issues = list(checked.issues)
        candidate = None
        rejection_reason = checked.rejected_reason
        if conflict and classification in {
            "calendar_candidate", "clarification_required"
        }:
            classification = "clarification_required"
            issues.append("rule_llm_conflict")
            rejection_reason = "rule_llm_conflict"
        if classification == "calendar_candidate" and checked.candidate_allowed:
            candidate = self._candidate(
                message, checked.result, checked.time_normalization
            )
        elif classification == "clarification_required":
            candidate = self._clarification_candidate(
                message,
                rule_candidate,
                checked.result,
                checked.time_normalization,
            )
        importance = ImportanceResult(
            checked.result.is_important,
            checked.result.importance_score,
            list(checked.result.reasons),
            checked.result.category,
        )
        if conflict and classification == "clarification_required":
            importance = ImportanceResult(
                True,
                max(rule_importance.score, checked.result.importance_score),
                ["rule/LLM conflict requires clarification"],
                checked.result.category,
            )
        final_source = (
            "clarification"
            if classification == "clarification_required"
            else "llm"
        )
        return HybridAnalysisResult(
            summary, True, checked.result, final_source, importance, candidate,
            checked.result.confidence, sorted(set(issues)), None, latency,
            self.classifier.client.model, truncated, classification,
            rejection_reason,
            original_llm.final_classification,
            semantic_corrections + checked.classification_corrections,
            checked.time_normalization,
            original_llm.summary(),
            reservation.detected,
            rule_derived_datetime_used,
            original_llm.should_notify_user,
            checked.result.should_notify_user,
            notification_override,
            notification.reason if notification_override else None,
            notification.grounded_date if notification.detected else None,
            notification.amount_detected if notification.detected else False,
        )

    def date_grounding_debug(
        self, proposed_date: str | None, message: EmailMessage
    ) -> dict[str, object]:
        if self.classifier is None:
            raise ValueError("LLM classifier is not configured")
        llm_body = self.classifier.canonical_body(
            without_transport_headers(message.body_text)
        )
        grounding_body = llm_body
        grounding_message = replace(message, body_text=grounding_body)
        debug = self.validator.date_grounding_debug(
            proposed_date, grounding_message, self.timezone
        )
        llm_hash = hashlib.sha256(llm_body.encode("utf-8")).hexdigest()
        validator_hash = hashlib.sha256(
            grounding_body.encode("utf-8")
        ).hexdigest()
        return {
            **debug,
            "llm_body_length": len(llm_body),
            "validator_body_length": len(grounding_body),
            "llm_body_hash": llm_hash,
            "validator_body_hash": validator_hash,
            "same_body": llm_body == grounding_body,
            "contains_explicit_japanese_date": bool(
                contains_japanese_explicit_date(grounding_body)
            ),
            "contains_time_expression": bool(
                re.search(
                    r"(?:午前|午後)?\s*\d{1,2}(?::\d{2}|時(?:\d{1,2}分)?)",
                    grounding_body,
                )
            ),
            "nfkc_date_detection": contains_japanese_explicit_date(
                nfkc_text(grounding_body)
            ),
            "whitespace_normalized_date_detection": (
                contains_japanese_explicit_date(
                    date_detection_text(grounding_body)
                )
            ),
        }

    def _should_use_llm(self, importance: ImportanceResult, candidate: CalendarCandidate | None) -> bool:
        if self.mode == "rule-only":
            return False
        if self.mode in {"llm-first", "llm-all"}:
            return True
        if importance.category == "promotion" and importance.score <= 0.1:
            return False
        return importance.score < 0.75 or (
            importance.is_important and (candidate is None or candidate.clarification_required)
        )

    @staticmethod
    def _conflicts(
        importance: ImportanceResult,
        candidate: CalendarCandidate | None,
        llm,
        *,
        compare_importance: bool = True,
    ) -> bool:
        if (
            compare_importance
            and importance.is_important != llm.is_important
            and importance.score >= 0.5
        ):
            return True
        if candidate and candidate.date and llm.date and candidate.date != llm.date:
            return True
        if candidate and candidate.start and llm.start and candidate.start != llm.start:
            return True
        return False

    def _candidate(
        self,
        message: EmailMessage,
        llm,
        time_normalization: dict[str, str | int] | None = None,
    ) -> CalendarCandidate:
        digest = hashlib.sha256(
            canonical_message_id(message.provider, message.message_id).encode()
        ).hexdigest()[:12]
        return CalendarCandidate(
            candidate_id=f"calendar-candidate-{digest}", source_provider=message.provider,
            source_message_id=message.message_id, source_thread_id=message.thread_id,
            title=llm.title or message.subject, candidate_type=llm.candidate_type,
            date=llm.date, start=llm.start, end=llm.end,
            duration_minutes=llm.duration_minutes, timezone=llm.timezone,
            location=llm.location, description=f"Calendar candidate from email: {message.subject}",
            importance_score=llm.importance_score, importance_reasons=list(llm.reasons),
            requires_approval=True, clarification_required=llm.clarification_required,
            extraction_notes=list(llm.clarification_questions), source_subject=message.subject,
            original_time_expression=(
                str(time_normalization["original_time_expression"])
                if time_normalization
                else None
            ),
            normalized_time=(
                str(time_normalization["normalized_time"])
                if time_normalization
                else None
            ),
            date_rollover_days=(
                int(time_normalization["date_rollover_days"])
                if time_normalization
                else 0
            ),
        )

    def _clarification_candidate(
        self,
        message: EmailMessage,
        rule_candidate,
        llm,
        time_normalization: dict[str, str | int] | None = None,
    ):
        candidate = rule_candidate or self._candidate(
            message, llm, time_normalization
        )
        return CalendarCandidate(**{
            **candidate.__dict__, "clarification_required": True,
            "extraction_notes": [*candidate.extraction_notes, "rule and LLM analysis require clarification"],
        })

    def _fallback(self, summary, importance, candidate, reason, latency):
        if self.mode == "llm-first":
            return HybridAnalysisResult(
                summary,
                True,
                None,
                "fallback_rule",
                ImportanceResult(
                    False,
                    0.0,
                    ["LLM unavailable; no semantic classification was accepted"],
                    "unknown",
                ),
                None,
                0.0,
                ["llm_unavailable"],
                reason,
                latency,
                self.classifier.client.model if self.classifier else None,
                self.classifier.last_input_truncated if self.classifier else False,
                "invalid",
                "llm_unavailable",
            )
        uncertain = importance.score < 0.75 or candidate is None or candidate.clarification_required
        final_candidate = candidate
        final_source = "fallback_rule"
        final_importance = importance
        if uncertain:
            final_source = "clarification"
            final_importance = ImportanceResult(
                True,
                importance.score,
                [*importance.reasons, "LLM unavailable; conservative clarification required"],
                importance.category,
            )
            if candidate is not None:
                final_candidate = CalendarCandidate(**{**candidate.__dict__, "clarification_required": True})
        return HybridAnalysisResult(
            summary, True, None, final_source, final_importance, final_candidate,
            importance.score, ["llm_unavailable"], reason, latency,
            self.classifier.client.model if self.classifier else None,
            self.classifier.last_input_truncated if self.classifier else False,
            "clarification_required" if final_source == "clarification" else self._rule_classification(importance, final_candidate),
            reason if final_source == "clarification" else None,
        )

    @staticmethod
    def _rule_classification(
        importance: ImportanceResult,
        candidate: CalendarCandidate | None,
    ):
        if importance.category == "promotion":
            return "promotion"
        if candidate is not None and candidate.clarification_required:
            return "clarification_required"
        if candidate is not None:
            return "calendar_candidate"
        if importance.category == "informational":
            return "informational"
        return "ignored"
