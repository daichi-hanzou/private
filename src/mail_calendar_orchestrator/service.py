from __future__ import annotations

import re
from dataclasses import replace
from pathlib import Path
from typing import Any
from uuid import uuid4

from calendar_agent.agent import CalendarAgent
from mail_to_calendar.extractor import to_calendar_request
from mail_to_calendar.local_provider import LocalMailProvider
from mail_to_calendar.models import CalendarCandidate
from mail_to_calendar.hybrid_analyzer import HybridMailAnalyzer
from mail_to_calendar.provider import MailProvider
from mail_to_calendar.service import MailToCalendarService

from .audit import write_jsonl_atomic
from .approvals import ApprovalService
from .models import OrchestrationResult
from .state import MailStateStore


class _MessagesProvider:
    def __init__(self, messages: list[Any]) -> None:
        self.messages = messages

    def list_messages(self) -> list[Any]:
        return self.messages


class MailCalendarOrchestrator:
    """Connect the existing mail pipeline to CalendarAgent in-process."""

    def __init__(
        self,
        *,
        base_year: int,
        timezone: str = "Asia/Tokyo",
        analyzer: HybridMailAnalyzer | None = None,
        state_store: MailStateStore | None = None,
        analysis_mode: str = "rule-only",
        model_name: str | None = None,
    ) -> None:
        self.base_year = base_year
        self.timezone = timezone
        self.analyzer = analyzer
        self.state_store = state_store
        self.analysis_mode = analysis_mode
        self.model_name = model_name

    def process(
        self,
        input_path: str | Path,
        output_path: str | Path,
        *,
        requires_approval: bool = True,
        analysis_only: bool = False,
        reprocess: bool = False,
        retry_failed: bool = False,
        run_id: str | None = None,
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
            reprocess=reprocess,
            retry_failed=retry_failed,
            run_id=run_id,
        )

    def process_provider(
        self,
        provider: MailProvider,
        output_path: str | Path,
        *,
        requires_approval: bool = True,
        analysis_only: bool = False,
        reprocess: bool = False,
        retry_failed: bool = False,
        run_id: str | None = None,
        run_provider: str | None = None,
    ) -> OrchestrationResult:
        output = Path(output_path)
        fetched = provider.list_messages()
        unique = []
        seen: set[tuple[str, str]] = set()
        for message in fetched:
            key = (message.provider, message.message_id)
            if key not in seen:
                seen.add(key)
                unique.append(message)
        duplicate_count = len(fetched) - len(unique)
        run_id = run_id or f"mail-batch-{uuid4()}"
        selected = unique
        skipped = duplicate_count
        decisions = {}
        if self.state_store is not None:
            selected = []
            for message in unique:
                decision = self.state_store.decision(
                    message, reprocess=reprocess, retry_failed=retry_failed
                )
                decisions[message.message_id] = decision
                if decision.process:
                    selected.append(message)
                else:
                    skipped += 1
            provider_name = run_provider or (
                unique[0].provider if unique else "unknown"
            )
            self.state_store.start_run(
                run_id, provider=provider_name,
                analysis_mode=self.analysis_mode, model_name=self.model_name,
                fetched=len(fetched), new=len(selected), skipped=skipped,
            )
            for message in selected:
                self.state_store.mark_processing(
                    message, run_id=run_id, analysis_mode=self.analysis_mode,
                    model_name=self.model_name,
                    digest=decisions[message.message_id].content_hash,
                )
        if not selected:
            if self.state_store is not None:
                self.state_store.finish_run(run_id, status="completed")
            return OrchestrationResult(
                processed_messages=0, important_messages=0,
                ignored_messages=0, candidates=0, ready_candidates=0,
                clarification_required=0, unsupported_candidates=0,
                calendar_proposals=0, pending_calendar_actions=0,
                confirmed_calendar_actions=0, generated_events=0,
                output_path=output, events=[],
                analysis_only=analysis_only, fetched_messages=len(fetched),
                new_messages=0, skipped_messages=skipped, run_id=run_id,
                state_db=self.state_store.path if self.state_store else None,
            )
        filtered_provider = _MessagesProvider(selected)
        try:
            return self._process_selected(
                filtered_provider, output, requires_approval=requires_approval,
                analysis_only=analysis_only, fetched_count=len(fetched),
                skipped_count=skipped, run_id=run_id,
            )
        except Exception as exc:
            if self.state_store is not None:
                status = "retryable" if isinstance(exc, (OSError, RuntimeError)) else "failed"
                for message in selected:
                    self.state_store.mark_result(message, status=status, error=exc)
                self.state_store.finish_run(
                    run_id, failed_messages=len(selected), status="failed"
                )
            raise

    def _process_selected(
        self, provider: MailProvider, output: Path, *, requires_approval: bool,
        analysis_only: bool, fetched_count: int, skipped_count: int,
        run_id: str,
    ) -> OrchestrationResult:
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
            if self.state_store is not None and self.state_store.candidate_seen(
                candidate.candidate_id
            ):
                statuses[message.message_id] = ("duplicate_candidate", None)
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
        approvals_created = 0
        if self.state_store is not None and requires_approval:
            approvals_created = len(
                ApprovalService(self.state_store).register_pending(events, written)
            )
        important_notifications_created = 0
        if self.state_store is not None and not analysis_only:
            for message, analysis in zip(
                messages, mail_result.analysis_results or [], strict=False
            ):
                if not (
                    analysis.final_importance.is_important
                    and analysis.llm_result
                    and analysis.llm_result.should_notify_user
                    and analysis.final_classification != "calendar_candidate"
                ):
                    continue
                if self.state_store.register_important_notification(
                    provider=message.provider,
                    message_id=message.message_id,
                    category=analysis.llm_result.category,
                    subject=message.subject,
                    notification_date=(
                        analysis.notification_grounded_date
                        or analysis.llm_result.date
                    ),
                    amount=self._safe_amount(message.body_text),
                ):
                    important_notifications_created += 1
        failed_analyses = {
            message.message_id: analysis
            for message, analysis in zip(
                messages, mail_result.analysis_results or [], strict=False
            )
            if "llm_unavailable" in analysis.validation_issues
        }
        retryable_ids = {
            message_id
            for message_id, analysis in failed_analyses.items()
            if self._is_retryable_llm_failure(analysis.fallback_reason)
        }
        permanent_ids = set(failed_analyses) - retryable_ids
        if self.state_store is not None:
            candidate_map = {
                item.source_message_id: item.candidate_id
                for item in mail_result.candidates
            }
            calendar_action_map = {
                str(event.get("metadata", {}).get("source_message_id")): str(event["action_id"])
                for event in calendar_events
                if event.get("event_type") == "action_executed"
            }
            execution_context = next(
                (
                    event["execution_context"]
                    for event in mail_result.events
                    if event.get("event_type") == "action_executed"
                    and isinstance(event.get("execution_context"), dict)
                ),
                {},
            )
            source_run_id = str(mail_result.events[0]["run_id"])
            for message in messages:
                relation = mail_relations[message.message_id]
                classification = classification_by_message.get(message.message_id)
                self.state_store.mark_result(
                    message,
                    status=(
                        "retryable" if message.message_id in retryable_ids
                        else "failed" if message.message_id in permanent_ids
                        else "processed"
                    ),
                    final_classification=classification,
                    candidate_id=candidate_map.get(message.message_id),
                    mail_action_id=relation.get("action_id"),
                    calendar_action_id=calendar_action_map.get(message.message_id),
                    prompt_template_version=execution_context.get(
                        "prompt_template_version"
                    ),
                    schema_version=execution_context.get(
                        "schema_version", "mail-analysis-schema-v2"
                    ),
                    source_run_id=source_run_id,
                    error=(
                        failed_analyses[message.message_id].fallback_reason
                        if message.message_id in failed_analyses else None
                    ),
                )
            retryable_count = len(retryable_ids)
            permanent_count = len(permanent_ids)
            self.state_store.finish_run(
                run_id,
                processed_messages=len(messages) - retryable_count - permanent_count,
                failed_messages=retryable_count + permanent_count,
                calendar_candidates=mail_result.classification_counts.get("calendar_candidate", 0),
                clarification_required=mail_result.clarification_required,
                promotions=mail_result.classification_counts.get("promotion", 0),
                informational=mail_result.classification_counts.get("informational", 0),
                security_notifications=mail_result.classification_counts.get("security_notification", 0),
                invalid=mail_result.classification_counts.get("invalid", 0),
                status=(
                    "completed_with_errors"
                    if retryable_count or permanent_count else "completed"
                ),
            )
        else:
            retryable_count = 0
            permanent_count = 0
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
            fetched_messages=fetched_count,
            new_messages=len(messages),
            skipped_messages=skipped_count,
            retryable_failures=retryable_count,
            permanent_failures=permanent_count,
            run_id=run_id,
            state_db=self.state_store.path if self.state_store else None,
            approvals_created=approvals_created,
            important_notifications_created=important_notifications_created,
        )

    @staticmethod
    def _safe_amount(body_text: str) -> str | None:
        match = re.search(
            r"(?<!\d)(\d{1,3}(?:,\d{3})+|\d+)\s*円", body_text
        )
        return f"{match.group(1)}円" if match else None

    @staticmethod
    def _is_retryable_llm_failure(reason: str | None) -> bool:
        value = (reason or "").lower()
        retryable_markers = (
            "timeout", "timed out", "connection", "connect", "refused",
            "unreachable", "temporar", "429", "http 5", "network",
        )
        return any(marker in value for marker in retryable_markers)

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
                    "candidate_type": candidate.candidate_type,
                    "candidate_date": candidate.date,
                    "candidate_end": candidate.end,
                    "candidate_timezone": candidate.timezone,
                    "candidate_location": candidate.location,
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
