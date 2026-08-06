from __future__ import annotations

import hashlib
import json
import subprocess
from dataclasses import dataclass
from typing import Any

from .classifier import RuleBasedImportanceClassifier
from .extractor import RuleBasedCalendarExtractor
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


class MailToCalendarService:
    version = "0.1.0"
    tool_version = "local-mail-provider-0.1.0"
    preview_limit = 160

    def __init__(self, *, base_year: int, timezone: str = "Asia/Tokyo") -> None:
        self.base_year = base_year
        self.timezone = timezone
        self.classifier = RuleBasedImportanceClassifier()
        self.extractor = RuleBasedCalendarExtractor(
            base_year=base_year,
            timezone=timezone,
        )

    def process(self, provider: MailProvider) -> ProcessingResult:
        messages = provider.list_messages()
        run_id = self._run_id(messages)
        events: list[dict[str, Any]] = []
        candidates: list[CalendarCandidate] = []
        important_count = 0
        clarification_count = 0
        ignored_count = 0
        for message in messages:
            importance = self.classifier.classify(message)
            candidate = self.extractor.extract(message, importance)
            if importance.is_important:
                important_count += 1
            if candidate is not None:
                candidates.append(candidate)
            selected_action = self._selected_action(importance, candidate)
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
                )
            )
        return ProcessingResult(
            events=events,
            candidates=candidates,
            processed=len(messages),
            important=important_count,
            clarification_required=clarification_count,
            ignored=ignored_count,
        )

    @staticmethod
    def _selected_action(
        importance: ImportanceResult,
        candidate: CalendarCandidate | None,
    ) -> str:
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
            "execution_context": self._execution_context(),
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

    def _execution_context(self) -> dict[str, Any]:
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
        commit = self._git_commit()
        if commit:
            context["git_commit"] = commit
        return context

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
