from __future__ import annotations

import hashlib
import time
from typing import Literal

from .llm_classifier import LLMCalendarClassifier
from .llm_models import HybridAnalysisResult, LLMAnalysisInput
from .models import CalendarCandidate, EmailMessage, ImportanceResult
from .ollama_client import OllamaError
from .validator import LLMResultValidator


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
        value = LLMAnalysisInput(
            sender=message.sender,
            subject=message.subject,
            received_at=message.received_at,
            body_text=message.body_text,
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
            checked = self.validator.validate(llm, message)
        except (OllamaError, ValueError) as exc:
            latency = int((time.monotonic() - started) * 1000)
            if self.require_llm or self.mode == "llm-all":
                raise RuntimeError(f"required LLM analysis failed: {exc}") from exc
            return self._fallback(summary, rule_importance, rule_candidate, str(exc), latency)
        latency = int((time.monotonic() - started) * 1000)
        truncated = self.classifier.last_input_truncated
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
            candidate = self._candidate(message, checked.result)
        elif classification == "clarification_required":
            candidate = self._clarification_candidate(
                message, rule_candidate, checked.result
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
            llm.final_classification,
            checked.classification_corrections,
        )

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

    def _candidate(self, message: EmailMessage, llm) -> CalendarCandidate:
        digest = hashlib.sha256(f"{message.provider}:{message.message_id}".encode()).hexdigest()[:12]
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
        )

    def _clarification_candidate(self, message: EmailMessage, rule_candidate, llm):
        candidate = rule_candidate or self._candidate(message, llm)
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
