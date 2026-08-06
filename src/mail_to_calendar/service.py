from __future__ import annotations

import hashlib
import json
import subprocess
from dataclasses import dataclass, field
from typing import Any

from .classifier import RuleBasedImportanceClassifier
from .extractor import RuleBasedCalendarExtractor
from .hybrid_analyzer import HybridMailAnalyzer
from .llm_models import HybridAnalysisResult
from .models import CalendarCandidate, EmailMessage, ImportanceResult
from .provider import MailProvider


@dataclass(frozen=True)
class ProcessingResult:
    events: list[dict[str, Any]]
    candidates: list[CalendarCandidate]
    processed: int
    important: int
    clarification_required: int
    ignored: int
    rule_only_decisions: int = 0
    llm_assisted_decisions: int = 0
    analysis_results: list[HybridAnalysisResult] | None = None
    classification_counts: dict[str, int] = field(default_factory=dict)


class MailToCalendarService:
    version = "0.1.0"
    tool_version = "local-mail-provider-0.1.0"
    preview_limit = 160

    def __init__(
        self,
        *,
        base_year: int,
        timezone: str = "Asia/Tokyo",
        analyzer: HybridMailAnalyzer | None = None,
    ) -> None:
        self.base_year = base_year
        self.timezone = timezone
        self.classifier = RuleBasedImportanceClassifier()
        self.extractor = RuleBasedCalendarExtractor(
            base_year=base_year,
            timezone=timezone,
        )
        self.analyzer = analyzer

    def process(self, provider: MailProvider) -> ProcessingResult:
        messages = provider.list_messages()
        run_id = self._run_id(messages)
        events: list[dict[str, Any]] = []
        candidates: list[CalendarCandidate] = []
        important_count = 0
        clarification_count = 0
        ignored_count = 0
        analyses: list[HybridAnalysisResult] = []
        classification_counts: dict[str, int] = {}
        for message in messages:
            importance = self.classifier.classify(message)
            candidate = self.extractor.extract(message, importance)
            analysis = None
            if self.analyzer is not None:
                analysis = self.analyzer.analyze(message, importance, candidate)
                analyses.append(analysis)
                importance = analysis.final_importance
                candidate = analysis.final_candidate
                classification_counts[analysis.final_classification] = (
                    classification_counts.get(analysis.final_classification, 0)
                    + 1
                )
            if importance.is_important:
                important_count += 1
            if candidate is not None:
                candidates.append(candidate)
            selected_action = self._selected_action(
                importance, candidate, analysis
            )
            if selected_action == "request_clarification":
                clarification_count += 1
            if selected_action == "ignore_email":
                ignored_count += 1
            events.extend(
                self._events(
                    message,
                    importance,
                    candidate,
                    selected_action,
                    run_id,
                    analysis,
                )
            )
        return ProcessingResult(
            events=events,
            candidates=candidates,
            processed=len(messages),
            important=important_count,
            clarification_required=clarification_count,
            ignored=ignored_count,
            rule_only_decisions=sum(not item.llm_used for item in analyses),
            llm_assisted_decisions=sum(item.llm_used for item in analyses),
            analysis_results=analyses,
            classification_counts=classification_counts,
        )

    @staticmethod
    def _selected_action(
        importance: ImportanceResult,
        candidate: CalendarCandidate | None,
        analysis: HybridAnalysisResult | None = None,
    ) -> str:
        if analysis is not None:
            if analysis.final_classification == "calendar_candidate":
                return (
                    "propose_calendar_candidate"
                    if candidate is not None
                    else "request_clarification"
                )
            if analysis.final_classification == "clarification_required":
                return "request_clarification"
            return "ignore_email"
        if not importance.is_important:
            return "ignore_email"
        if candidate is None or candidate.clarification_required:
            return "request_clarification"
        return "propose_calendar_candidate"

    def _run_id(self, messages: list[EmailMessage]) -> str:
        value = json.dumps(
            {
                "messages": [
                    [message.provider, message.message_id]
                    for message in messages
                ],
                "base_year": self.base_year,
                "timezone": self.timezone,
            },
            ensure_ascii=False,
            separators=(",", ":"),
        )
        digest = hashlib.sha256(value.encode()).hexdigest()[:12]
        return f"mail-batch-{digest}"

    def _events(
        self,
        message: EmailMessage,
        importance: ImportanceResult,
        candidate: CalendarCandidate | None,
        selected_action: str,
        run_id: str,
        analysis: HybridAnalysisResult | None = None,
    ) -> list[dict[str, Any]]:
        digest = hashlib.sha256(
            f"{message.provider}:{message.message_id}".encode()
        ).hexdigest()[:12]
        ids = {
            "correlation_id": f"mail-message-{digest}",
            "decision_id": f"mail-decision-{digest}",
            "action_id": f"mail-action-{digest}",
        }
        shared = {
            "schema_version": "0.1",
            "run_id": run_id,
            "timestamp": message.received_at,
            "agent_id": "mail_to_calendar_agent",
            "correlation_id": ids["correlation_id"],
            "metadata": {"source_message_id": message.message_id},
        }
        observation = {
            **shared,
            "event_id": f"mail-observation-{digest}",
            "event_type": "observation_received",
            "observation": {
                "provider": message.provider,
                "message_id": message.message_id,
                "thread_id": message.thread_id,
                "sender": message.sender,
                "subject": message.subject,
                "received_at": message.received_at,
                "body_preview": self._preview(message),
                "labels": message.labels,
                "importance_hint": message.importance_hint,
                "has_attachments": message.has_attachments,
            },
            "allowed_actions": [
                "propose_calendar_candidate",
                "request_clarification",
                "ignore_email",
            ],
        }
        explanation = self._explanation(
            importance,
            candidate,
            selected_action,
        )
        expected = {
            "propose_calendar_candidate": "calendar_candidate_created",
            "request_clarification": "clarification_requested",
            "ignore_email": "email_ignored",
        }[selected_action]
        decision = {
            **shared,
            "event_id": f"mail-decision-event-{digest}",
            "event_type": "decision_made",
            "decision_id": ids["decision_id"],
            "selected_action": selected_action,
            "explanation": explanation,
            "expected_outcome": {"outcome_type": expected},
            "importance_score": importance.score,
            "importance_reasons": importance.reasons,
            "category": importance.category,
        }
        if analysis is not None:
            decision["analysis"] = self._analysis_audit(analysis)
        parameters = self._action_parameters(
            importance,
            candidate,
            selected_action,
        )
        action = {
            **shared,
            "event_id": f"mail-action-event-{digest}",
            "event_type": "action_executed",
            "decision_id": ids["decision_id"],
            "action_id": ids["action_id"],
            "action": selected_action,
            "action_parameters": parameters,
            "status": "success",
            "execution_context": self._execution_context(analysis),
        }
        outcome = {
            **shared,
            "event_id": f"mail-outcome-{digest}",
            "event_type": "outcome_observed",
            "decision_id": ids["decision_id"],
            "action_id": ids["action_id"],
            "status": "confirmed",
            "actual_outcome": self._actual_outcome(
                candidate,
                selected_action,
            ),
        }
        return [observation, decision, action, outcome]

    def _preview(self, message: EmailMessage) -> str | None:
        if message.body_preview is None:
            return None
        preview = message.body_preview[: self.preview_limit]
        if preview == message.body_text:
            preview = preview[: max(self.preview_limit - 3, 0)]
            if preview == message.body_text:
                preview = preview[:-1]
            return preview + "..."
        return preview

    @staticmethod
    def _explanation(
        importance: ImportanceResult,
        candidate: CalendarCandidate | None,
        selected_action: str,
    ) -> str:
        reasons = "; ".join(importance.reasons)
        if selected_action == "ignore_email":
            return f"Email was not important enough to schedule: {reasons}."
        if selected_action == "request_clarification":
            notes = (
                "; ".join(candidate.extraction_notes)
                if candidate is not None
                else "no calendar candidate could be extracted"
            )
            return f"Additional calendar details are required: {notes}."
        return f"A calendar candidate was extracted: {reasons}."

    @staticmethod
    def _candidate_parameters(
        candidate: CalendarCandidate,
    ) -> dict[str, Any]:
        return {
            "candidate_id": candidate.candidate_id,
            "title": candidate.title,
            "candidate_type": candidate.candidate_type,
            "date": candidate.date,
            "start": candidate.start,
            "duration_minutes": candidate.duration_minutes,
            "timezone": candidate.timezone,
            "clarification_required": candidate.clarification_required,
        }

    def _action_parameters(
        self,
        importance: ImportanceResult,
        candidate: CalendarCandidate | None,
        selected_action: str,
    ) -> dict[str, Any]:
        if candidate is not None:
            return self._candidate_parameters(candidate)
        if selected_action == "ignore_email":
            return {"reason_category": importance.category}
        return {"clarification_required": True}

    def _actual_outcome(
        self,
        candidate: CalendarCandidate | None,
        selected_action: str,
    ) -> dict[str, Any]:
        if selected_action == "ignore_email":
            return {"outcome_type": "email_ignored"}
        if selected_action == "request_clarification":
            return {"outcome_type": "clarification_requested"}
        assert candidate is not None
        return {
            "outcome_type": "calendar_candidate_created",
            **self._candidate_parameters(candidate),
        }

    def _execution_context(
        self, analysis: HybridAnalysisResult | None = None
    ) -> dict[str, Any]:
        context: dict[str, Any] = {
            "model_name": "rule-based-mail-to-calendar",
            "model_version": self.version,
            "tool_version": self.tool_version,
            "config_hash": "sha256:"
            + hashlib.sha256(
                b"mail-to-calendar-rules-v0.1.0"
            ).hexdigest(),
            "environment": {
                "runtime": "local",
                "external_services": False,
                "timezone": self.timezone,
            },
        }
        if analysis is not None and self.analyzer is not None:
            context["model_name"] = analysis.model_name or context["model_name"]
            context["prompt_hash"] = None
            context["confidence_threshold"] = (
                self.analyzer.validator.confidence_threshold
            )
            context["analysis_mode"] = self.analyzer.mode
            classifier = self.analyzer.classifier
            if classifier is not None:
                metadata = classifier.prompt_metadata
                context["prompt_hash"] = metadata.system_template_hash
                context["prompt_template_version"] = metadata.template_version
                context["schema_hash"] = metadata.schema_hash
                context["ollama_base_url"] = classifier.client.safe_host
                context["temperature"] = classifier.client.temperature
                context["thinking"] = getattr(
                    classifier.client, "thinking", False
                )
                context["model_version"] = None
                config_value = json.dumps(
                    {
                        "analysis_mode": self.analyzer.mode,
                        "model": classifier.client.model,
                        "ollama_host": classifier.client.safe_host,
                        "temperature": classifier.client.temperature,
                        "thinking": getattr(classifier.client, "thinking", False),
                        "confidence_threshold": self.analyzer.validator.confidence_threshold,
                    },
                    sort_keys=True,
                    separators=(",", ":"),
                )
                context["config_hash"] = "sha256:" + hashlib.sha256(
                    config_value.encode()
                ).hexdigest()
        commit = self._git_commit()
        if commit:
            context["git_commit"] = commit
        return context

    def _analysis_audit(self, analysis: HybridAnalysisResult) -> dict[str, Any]:
        value = {
            "analysis_mode": (
                self.analyzer.mode if self.analyzer is not None else "rule-only"
            ),
            "llm_used": analysis.llm_used,
            "model_name": analysis.model_name,
            "confidence": analysis.confidence,
            "final_source": analysis.final_source,
            "final_classification": analysis.final_classification,
            "llm_proposed_classification": (
                analysis.llm_proposed_classification
            ),
            "classification_corrections": (
                analysis.classification_corrections
            ),
            "validation_issues": analysis.validation_issues,
            "fallback_reason": analysis.fallback_reason,
            "calendar_candidate_rejected_reason": (
                analysis.calendar_candidate_rejected_reason
            ),
            "user_commitment_detected": bool(
                analysis.llm_result
                and analysis.llm_result.user_commitment_detected
            ),
            "generic_event_advertisement": bool(
                analysis.llm_result
                and analysis.llm_result.generic_event_advertisement
            ),
            "security_notification_type": (
                analysis.llm_result.security_notification_type
                if analysis.llm_result
                else None
            ),
            "suspicious_instructions_detected": bool(
                analysis.llm_result
                and analysis.llm_result.suspicious_instructions_detected
            ),
            "latency_ms": analysis.latency_ms,
            "input_truncated": analysis.input_truncated,
            "rule_result_summary": analysis.rule_result,
            "llm_result_summary": (
                analysis.llm_result.summary() if analysis.llm_result else None
            ),
        }
        classifier = self.analyzer.classifier if self.analyzer else None
        if classifier is not None:
            value.update(
                {
                    "model_version": None,
                    "prompt_template_version": classifier.prompt_metadata.template_version,
                    "system_prompt_template_hash": classifier.prompt_metadata.system_template_hash,
                    "schema_version": "mail-analysis-schema-v2",
                    "schema_hash": classifier.prompt_metadata.schema_hash,
                }
            )
        return value

    @staticmethod
    def _git_commit() -> str | None:
        try:
            result = subprocess.run(
                ["git", "rev-parse", "HEAD"],
                capture_output=True,
                check=True,
                text=True,
                timeout=1,
            )
        except (OSError, subprocess.SubprocessError):
            return None
        return result.stdout.strip() or None
