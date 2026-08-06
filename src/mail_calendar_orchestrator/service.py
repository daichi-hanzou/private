from __future__ import annotations

from dataclasses import replace
from pathlib import Path
from typing import Any

from calendar_agent.agent import CalendarAgent
from mail_to_calendar.extractor import to_calendar_request
from mail_to_calendar.local_provider import LocalMailProvider
from mail_to_calendar.models import CalendarCandidate
from mail_to_calendar.hybrid_analyzer import HybridMailAnalyzer
from mail_to_calendar.provider import MailProvider
from mail_to_calendar.service import MailToCalendarService

from .audit import write_jsonl_atomic
from .models import OrchestrationResult


class MailCalendarOrchestrator:
    """Connect the existing mail pipeline to CalendarAgent in-process."""

    def __init__(
        self,
        *,
        base_year: int,
        timezone: str = "Asia/Tokyo",
        analyzer: HybridMailAnalyzer | None = None,
    ) -> None:
        self.base_year = base_year
        self.timezone = timezone
        self.analyzer = analyzer

    def process(
        self,
        input_path: str | Path,
        output_path: str | Path,
        *,
        requires_approval: bool = True,
        analysis_only: bool = False,
    ) -> OrchestrationResult:
        source = Path(input_path)
        output = Path(output_path)
        if source.resolve() == output.resolve():
            raise ValueError("input and output must be different files")
        return self.process_provider(
            LocalMailProvider(source),
            output,
            requires_approval=requires_approval,
            analysis_only=analysis_only,
        )

    def process_provider(
        self,
        provider: MailProvider,
        output_path: str | Path,
        *,
        requires_approval: bool = True,
        analysis_only: bool = False,
    ) -> OrchestrationResult:
        output = Path(output_path)
        mail_result = MailToCalendarService(
            base_year=self.base_year,
            timezone=self.timezone,
            analyzer=self.analyzer,
        ).process(provider)
        mail_relations = self._mail_relations(mail_result.events)
        statuses: dict[str, tuple[str, str | None]] = {}
        calendar_events: list[dict[str, Any]] = []
        ready = 0
        unsupported = 0
        pending = 0
        confirmed = 0

        candidate_by_message = {
            candidate.source_message_id: candidate
            for candidate in mail_result.candidates
        }
        messages = provider.list_messages()
        classification_by_message = {
            message.message_id: analysis.final_classification
            for message, analysis in zip(
                messages,
                mail_result.analysis_results or [],
                strict=False,
            )
        }
        for message in messages:
            candidate = candidate_by_message.get(message.message_id)
            relation = mail_relations[message.message_id]
            mail_action = relation["action_type"]
            if analysis_only:
                statuses[message.message_id] = ("analysis_only", None)
                continue
            classification = classification_by_message.get(message.message_id)
            if classification and classification != "calendar_candidate":
                statuses[message.message_id] = (classification, None)
                continue
            if mail_action == "ignore_email":
                statuses[message.message_id] = ("ignored", None)
                continue
            if candidate is None or candidate.clarification_required:
                statuses[message.message_id] = (
                    "clarification_required",
                    None,
                )
                continue
            try:
                request = to_calendar_request(candidate)
            except ValueError as exc:
                unsupported += 1
                statuses[message.message_id] = ("unsupported", str(exc))
                continue

            ready += 1
            statuses[message.message_id] = ("ready", None)
            request = replace(
                request,
                requires_approval=requires_approval,
            )
            proposed = CalendarAgent().propose(request)
            calendar_events.extend(
                self._linked_calendar_events(
                    proposed,
                    candidate=candidate,
                    relation=relation,
                )
            )
            if requires_approval:
                pending += 1
            else:
                confirmed += 1

        mail_events = self._annotate_mail_events(
            mail_result.events,
            statuses,
        )
        events = [*mail_events, *calendar_events]
        written = write_jsonl_atomic(output, events)
        return OrchestrationResult(
            processed_messages=mail_result.processed,
            important_messages=mail_result.important,
            ignored_messages=mail_result.ignored,
            candidates=len(mail_result.candidates),
            ready_candidates=ready,
            clarification_required=mail_result.clarification_required,
            unsupported_candidates=unsupported,
            calendar_proposals=ready,
            pending_calendar_actions=pending,
            confirmed_calendar_actions=confirmed,
            generated_events=len(events),
            output_path=written,
            events=events,
            rule_only_decisions=mail_result.rule_only_decisions,
            llm_assisted_decisions=mail_result.llm_assisted_decisions,
            analysis_only=analysis_only,
            informational_messages=mail_result.classification_counts.get(
                "informational", 0
            ),
            promotion_messages=mail_result.classification_counts.get(
                "promotion", 0
            ),
            security_notifications=mail_result.classification_counts.get(
                "security_notification", 0
            ),
            invalid_messages=mail_result.classification_counts.get(
                "invalid", 0
            ),
            calendar_candidate_messages=mail_result.classification_counts.get(
                "calendar_candidate", 0
            ),
        )

    @staticmethod
    def _mail_relations(
        events: list[dict[str, Any]],
    ) -> dict[str, dict[str, str]]:
        result: dict[str, dict[str, str]] = {}
        for event in events:
            metadata = event.get("metadata")
            if not isinstance(metadata, dict):
                continue
            message_id = metadata.get("source_message_id")
            if not message_id:
                continue
            relation = result.setdefault(str(message_id), {})
            relation["correlation_id"] = str(event["correlation_id"])
            if event.get("event_type") == "decision_made":
                relation["decision_id"] = str(event["decision_id"])
            if event.get("event_type") == "action_executed":
                relation["action_id"] = str(event["action_id"])
                relation["action_type"] = str(event["action"])
        required = {
            "correlation_id",
            "decision_id",
            "action_id",
            "action_type",
        }
        for message_id, relation in result.items():
            missing = sorted(required - relation.keys())
            if missing:
                raise ValueError(
                    f"mail relation for {message_id} is missing: "
                    + ", ".join(missing)
                )
        return result

    @staticmethod
    def _annotate_mail_events(
        events: list[dict[str, Any]],
        statuses: dict[str, tuple[str, str | None]],
    ) -> list[dict[str, Any]]:
        annotated = []
        for event in events:
            metadata = event.get("metadata")
            metadata = dict(metadata) if isinstance(metadata, dict) else {}
            message_id = metadata.get("source_message_id")
            status, error = statuses[str(message_id)]
            metadata["orchestration_status"] = status
            if error:
                metadata["calendar_conversion_error"] = error
            annotated.append({**event, "metadata": metadata})
        return annotated

    def _linked_calendar_events(
        self,
        events: list[dict[str, Any]],
        *,
        candidate: CalendarCandidate,
        relation: dict[str, str],
    ) -> list[dict[str, Any]]:
        suffix = candidate.candidate_id
        id_fields = ("run_id", "correlation_id", "decision_id", "action_id")
        id_maps: dict[str, dict[str, str]] = {key: {} for key in id_fields}
        for key in id_fields:
            for event in events:
                value = event.get(key)
                if value:
                    id_maps[key].setdefault(
                        str(value),
                        f"{value}--{suffix}",
                    )

        linked = []
        calendar_event_ids: dict[str, str] = {}
        for event in events:
            metadata = event.get("metadata")
            metadata = dict(metadata) if isinstance(metadata, dict) else {}
            original_calendar_id = metadata.get("calendar_event_id")
            if original_calendar_id:
                calendar_event_ids[str(original_calendar_id)] = (
                    f"{original_calendar_id}--{suffix}"
                )
                metadata["calendar_event_id"] = calendar_event_ids[
                    str(original_calendar_id)
                ]
            metadata.update(
                {
                    "source_provider": candidate.source_provider,
                    "source_message_id": candidate.source_message_id,
                    "source_thread_id": candidate.source_thread_id,
                    "mail_candidate_id": candidate.candidate_id,
                    "parent_mail_action_id": relation["action_id"],
                    "parent_mail_decision_id": relation["decision_id"],
                    "parent_mail_correlation_id": relation[
                        "correlation_id"
                    ],
                }
            )
            value = {
                **event,
                "event_id": f"{event['event_id']}--{suffix}",
                "case_id": candidate.candidate_id,
                "case_type": "calendar_candidate",
                "metadata": metadata,
            }
            for key in id_fields:
                original = event.get(key)
                if original:
                    value[key] = id_maps[key][str(original)]
            actual = value.get("actual_outcome")
            if isinstance(actual, dict) and actual.get("event_id"):
                actual = dict(actual)
                original = str(actual["event_id"])
                actual["event_id"] = calendar_event_ids.get(
                    original,
                    f"{original}--{suffix}",
                )
                value["actual_outcome"] = actual
            linked.append(value)
        return linked
