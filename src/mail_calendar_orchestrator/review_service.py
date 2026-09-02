from __future__ import annotations

import sqlite3
from dataclasses import asdict, replace
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

from .review_models import (
    HumanVerdictInput, ReviewCaseRecord, ReviewChatMessage, ReviewContext,
    ReviewResult, ReviewRunResult, ReviewSelectionDiagnostic,
)
from .review_provider import ReviewClient
from .review_sources import MessageResolver
from .state import MailStateStore
from mail_to_calendar.taxonomy import (
    CLASSIFICATION_SET, classifications_equivalent,
)


_QUEUE_STATUSES = {"disagreement", "uncertain"}
_HUMAN_VERDICTS = {
    "original_correct", "reviewer_correct", "modified", "unresolved",
}
_CHANGE_TARGETS = {
    "qwen_prompt", "validator", "deterministic_rule", "test", "none",
}
_CLASSIFICATIONS = CLASSIFICATION_SET


def parse_since(value: str | None, *, now: datetime | None = None) -> str | None:
    if value in {None, ""}:
        return None
    current = now or datetime.now(timezone.utc)
    suffix = value[-1].casefold()
    amount = int(value[:-1])
    units = {
        "d": timedelta(days=amount),
        "h": timedelta(hours=amount),
        "m": timedelta(minutes=amount),
    }
    if suffix not in units:
        raise ValueError("since must use d, h, or m suffix")
    return (current - units[suffix]).isoformat()


def _reconcile_review_status(
    context: ReviewContext, result: ReviewResult,
) -> ReviewResult:
    """Make status consistent with taxonomy-normalized classifications."""
    if result.review_status == "uncertain":
        return replace(result, needs_human_review=True)
    system_classification = (
        context.final_classification or context.original_classification
    )
    equivalent = classifications_equivalent(
        system_classification, result.suggested_classification
    )
    expected_status = "agree" if equivalent else "disagreement"
    return replace(
        result,
        review_status=expected_status,
        needs_human_review=not equivalent,
    )


class ClassificationReviewService:
    def __init__(
        self,
        state: MailStateStore,
        reviewer: ReviewClient,
        resolver: MessageResolver,
        *,
        review_version: str,
    ) -> None:
        self.state = state
        self.reviewer = reviewer
        self.resolver = resolver
        self.review_version = review_version

    def run(
        self,
        *,
        since: str | None = None,
        include_reviewed: bool = False,
        limit: int | None = None,
    ) -> ReviewRunResult:
        diagnostics = self.selection_diagnostics(
            since=since,
            include_reviewed=include_reviewed,
            limit=limit,
        )
        rows = [
            row
            for item in diagnostics
            if item.selected
            for row in [self.state.processed_message(item.provider, item.message_id)]
            if row is not None
        ]
        created = 0
        skipped_existing = 0
        case_ids: list[str] = []
        for row in rows:
            existing = self._existing_case(
                str(row["provider"]),
                str(row["message_id"]),
            )
            if existing is not None:
                skipped_existing += 1
                continue
            context = self._build_context(row)
            result = self.reviewer.review_case(context)
            result = _reconcile_review_status(context, result)
            record = self._create_case(row, context, result)
            created += 1
            case_ids.append(record.review_case_id)
        queue_items = len(self.list_cases(queue_only=True, limit=500))
        return ReviewRunResult(
            selected=len(rows),
            created=created,
            skipped_existing=skipped_existing,
            queue_items=queue_items,
            case_ids=case_ids,
        )

    def selection_diagnostics(
        self, *, since: str | None = None, include_reviewed: bool = False,
        limit: int | None = None,
    ) -> list[ReviewSelectionDiagnostic]:
        rows = self.state.review_selection_rows(
            reviewer_model=self.reviewer.model_name,
            review_version=self.review_version,
        )
        diagnostics: list[ReviewSelectionDiagnostic] = []
        eligible_seen = 0
        for row in rows:
            source_path = str(row["source_jsonl_path"]) if row["source_jsonl_path"] else None
            source_available = bool(
                source_path and Path(source_path).expanduser().is_file()
            )
            source_ref = str(row["message_source_ref"]) if row["message_source_ref"] else None
            retrieval_available, retrieval_reason = (
                self.resolver.retrieval_availability(
                    provider=str(row["provider"]),
                    message_source_ref=source_ref,
                )
            )
            already_reviewed = bool(row["existing_review_case_id"])
            last_processed = (
                str(row["last_processed_at"])
                if row["last_processed_at"] else None
            )
            if since is None:
                since_eligible = True
            elif last_processed is None:
                since_eligible = False
            else:
                try:
                    since_eligible = (
                        datetime.fromisoformat(last_processed)
                        >= datetime.fromisoformat(since)
                    )
                except ValueError:
                    since_eligible = False
            reasons: list[str] = []
            if row["processing_status"] != "processed":
                reasons.append("processing_status_not_processed")
            if source_path is None:
                reasons.append("source_jsonl_path_missing")
            elif not source_available:
                reasons.append("source_jsonl_path_not_found")
            if not retrieval_available:
                reasons.append("provider_retrieval_unavailable")
            if already_reviewed and not include_reviewed:
                reasons.append("already_reviewed")
            if not since_eligible:
                reasons.append(
                    "last_processed_at_missing" if last_processed is None
                    else "outside_since_window"
                )
            selected = not reasons
            if selected:
                if limit is not None and eligible_seen >= limit:
                    selected = False
                    reasons.append("excluded_by_limit")
                else:
                    eligible_seen += 1
            diagnostics.append(ReviewSelectionDiagnostic(
                provider=str(row["provider"]),
                message_id=str(row["message_id"]),
                processing_status=str(row["processing_status"]),
                last_processed_at=last_processed,
                source_jsonl_path=source_path,
                source_jsonl_available=source_available,
                message_source_ref=source_ref,
                provider_retrieval_available=retrieval_available,
                provider_retrieval_reason=retrieval_reason,
                already_reviewed=already_reviewed,
                since_eligible=since_eligible,
                selected=selected,
                exclusion_reasons=reasons,
            ))
        return diagnostics

    def list_cases(
        self,
        *,
        queue_only: bool = False,
        limit: int = 50,
        version: str | None = None,
        all_versions: bool = False,
    ) -> list[ReviewCaseRecord]:
        if not 1 <= limit <= 1000:
            raise ValueError("review list limit must be between 1 and 1000")
        query = "SELECT * FROM review_cases"
        parameters: list[Any] = []
        clauses: list[str] = []
        if not all_versions:
            clauses.append("review_version=?")
            parameters.append(version or self.review_version)
        if queue_only:
            clauses.append(
                "review_status IN ('disagreement','uncertain') "
                "AND human_review_status='pending'"
            )
        if clauses:
            query += " WHERE " + " AND ".join(clauses)
        query += " ORDER BY created_at DESC LIMIT ?"
        parameters.append(limit)
        return [
            self._record(row)
            for row in self.state.connection.execute(query, tuple(parameters))
        ]

    def available_versions(self) -> list[str]:
        rows = self.state.connection.execute(
            "SELECT DISTINCT review_version FROM review_cases "
            "ORDER BY review_version DESC"
        )
        versions = [str(row["review_version"]) for row in rows]
        if self.review_version not in versions:
            versions.insert(0, self.review_version)
        return versions

    def get_case(self, review_case_id: str) -> ReviewCaseRecord:
        row = self.state.connection.execute(
            "SELECT * FROM review_cases WHERE review_case_id=?",
            (review_case_id,),
        ).fetchone()
        if row is None:
            raise ValueError(f"review case not found: {review_case_id}")
        return self._record(row)

    def get_case_detail(self, review_case_id: str) -> dict[str, Any]:
        record = self.get_case(review_case_id)
        context = self._build_context_from_record(record)
        case = asdict(record)
        for key in ("created_at", "updated_at", "human_reviewed_at"):
            value = case.get(key)
            case[key] = value.isoformat() if value is not None else None
        return {
            "case": case,
            "context": context.to_prompt_payload(),
            "event_snapshot": context.event_snapshot,
            "human_review_history": [
                dict(row) for row in self.state.connection.execute(
                    "SELECT revision,human_verdict,final_classification,"
                    "human_comment,recommended_change_target,reviewed_at,"
                    "reviewer_model,review_version FROM human_review_history "
                    "WHERE review_case_id=? ORDER BY revision DESC",
                    (review_case_id,),
                )
            ],
            "chat_history": [
                {
                    "role": item.role,
                    "content": item.content,
                    "created_at": item.created_at.isoformat(),
                }
                for item in self.chat_history(review_case_id)
            ],
        }

    def case_subject(self, record: ReviewCaseRecord) -> str:
        if record.subject:
            return record.subject
        events = self.resolver.read_events(record.source_jsonl_path)
        return str(self._event_snapshot(
            events, record.provider, record.message_id
        ).get("subject") or "(No subject)")

    def chat_history(self, review_case_id: str) -> list[ReviewChatMessage]:
        rows = self.state.connection.execute(
            "SELECT role,content,created_at FROM review_chat_messages "
            "WHERE review_case_id=? ORDER BY message_index ASC",
            (review_case_id,),
        )
        return [
            ReviewChatMessage(
                role=str(row["role"]),  # type: ignore[arg-type]
                content=str(row["content"]),
                created_at=datetime.fromisoformat(str(row["created_at"])),
            )
            for row in rows
        ]

    def send_chat_message(
        self, review_case_id: str, message: str, *,
        human_comment: str = "", human_verdict: str | None = None,
        final_classification: str | None = None,
        recommended_change_target: str | None = None,
    ) -> str:
        text = message.strip()
        if not text:
            raise ValueError("chat message is required")
        record = self.get_case(review_case_id)
        client_model = str(getattr(self.reviewer, "model_name", ""))
        client_version = str(
            getattr(self.reviewer, "review_version", self.review_version)
        )
        if (
            client_model != record.reviewer_model
            or client_version != record.review_version
        ):
            raise RuntimeError(
                "reviewer configuration does not match the selected case "
                f"({record.reviewer_model}/{record.review_version})"
            )
        context = self._build_context_from_record(record)
        history = self.chat_history(review_case_id)
        human_context = {
            "human_comment": human_comment[:4000],
            "human_verdict": human_verdict,
            "final_classification": final_classification,
            "recommended_change_target": recommended_change_target,
            "reviewer_initial_assessment": {
                "review_status": record.review_status,
                "suggested_classification": record.suggested_classification,
                "issue_type": record.issue_type,
                "confidence": record.confidence,
                "reason_summary": record.reason_summary,
            },
        }
        reply = self.reviewer.chat(
            context, history, text, human_context=human_context
        )
        if not reply:
            raise RuntimeError("review chat returned an empty response")
        with self.state.connection:
            index_row = self.state.connection.execute(
                "SELECT COALESCE(MAX(message_index), -1) value "
                "FROM review_chat_messages WHERE review_case_id=?",
                (review_case_id,),
            ).fetchone()
            start = int(index_row["value"]) + 1
            created_at = datetime.now(timezone.utc).isoformat()
            self.state.connection.execute(
                "INSERT INTO review_chat_messages("
                "review_case_id,message_index,role,content,created_at"
                ") VALUES(?,?,?,?,?)",
                (review_case_id, start, "user", text[:4000], created_at),
            )
            self.state.connection.execute(
                "INSERT INTO review_chat_messages("
                "review_case_id,message_index,role,content,created_at"
                ") VALUES(?,?,?,?,?)",
                (review_case_id, start + 1, "assistant", reply[:4000], created_at),
            )
        return reply

    def save_verdict(
        self, review_case_id: str, verdict: HumanVerdictInput
    ) -> ReviewCaseRecord:
        if verdict.human_verdict not in _HUMAN_VERDICTS:
            raise ValueError("invalid human verdict")
        if verdict.recommended_change_target not in _CHANGE_TARGETS:
            raise ValueError("invalid recommended change target")
        if verdict.final_classification not in _CLASSIFICATIONS:
            raise ValueError("invalid final classification")
        now = datetime.now(timezone.utc).isoformat()
        review_status = (
            "pending" if verdict.human_verdict == "unresolved" else "resolved"
        )
        with self.state.connection:
            self.state.connection.execute(
                """
                UPDATE review_cases SET human_review_status=?,
                    human_verdict=?,human_final_classification=?,human_comment=?,lesson_summary=?,
                    recommended_change_target=?,updated_at=?,human_reviewed_at=?
                WHERE review_case_id=?
                """,
                (
                    review_status,
                    verdict.human_verdict,
                    verdict.final_classification[:80],
                    verdict.human_comment[:4000],
                    verdict.lesson_summary[:2000],
                    verdict.recommended_change_target,
                    now,
                    now,
                    review_case_id,
                ),
            )
            revision = self.state.connection.execute(
                "SELECT COALESCE(MAX(revision),0)+1 value "
                "FROM human_review_history WHERE review_case_id=?",
                (review_case_id,),
            ).fetchone()["value"]
            record = self.get_case(review_case_id)
            self.state.connection.execute(
                "INSERT INTO human_review_history("
                "review_case_id,revision,human_verdict,final_classification,"
                "human_comment,recommended_change_target,reviewed_at,"
                "reviewer_model,review_version) VALUES(?,?,?,?,?,?,?,?,?)",
                (
                    review_case_id, revision, verdict.human_verdict,
                    verdict.final_classification[:80], verdict.human_comment[:4000],
                    verdict.recommended_change_target, now,
                    record.reviewer_model, record.review_version,
                ),
            )
        return self.get_case(review_case_id)

    def accept_reviewer(
        self, review_case_id: str, *, human_comment: str = ""
    ) -> ReviewCaseRecord:
        """Explicitly accept the initial reviewer result without requiring prose."""
        record = self.get_case(review_case_id)
        return self.save_verdict(
            review_case_id,
            HumanVerdictInput(
                human_verdict="reviewer_correct",
                final_classification=record.suggested_classification,
                lesson_summary="",
                recommended_change_target="none",
                human_comment=human_comment,
            ),
        )

    def accept_reviewers(self, review_case_ids: list[str]) -> list[ReviewCaseRecord]:
        """Explicitly accept unresolved reviewer results selected by the UI."""
        unique_ids = list(dict.fromkeys(review_case_ids))
        if len(unique_ids) > 500:
            raise ValueError("at most 500 review cases can be accepted at once")
        accepted: list[ReviewCaseRecord] = []
        for review_case_id in unique_ids:
            record = self.get_case(review_case_id)
            if record.human_review_status != "pending":
                continue
            accepted.append(self.accept_reviewer(review_case_id))
        return accepted

    def _create_case(
        self,
        row: sqlite3.Row,
        context: ReviewContext,
        result: ReviewResult,
    ) -> ReviewCaseRecord:
        review_case_id = self.state.next_review_case_id()
        now = datetime.now(timezone.utc).isoformat()
        with self.state.connection:
            self.state.connection.execute(
                """
                INSERT INTO review_cases(
                    review_case_id,provider,message_id,source_run_id,
                    source_jsonl_path,message_source_ref,reviewer_model,
                    review_version,review_status,suggested_classification,
                    issue_type,confidence,reason_summary,needs_human_review,
                    original_classification,final_classification,created_at,
                    updated_at,subject
                ) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
                """,
                (
                    review_case_id,
                    row["provider"],
                    row["message_id"],
                    row["source_run_id"],
                    row["source_jsonl_path"],
                    row["message_source_ref"],
                    self.reviewer.model_name,
                    self.review_version,
                    result.review_status,
                    result.suggested_classification,
                    result.issue_type,
                    result.confidence,
                    result.reason_summary,
                    1 if result.needs_human_review else 0,
                    context.original_classification,
                    context.final_classification,
                    now,
                    now,
                    context.subject,
                ),
            )
        return self.get_case(review_case_id)

    def _build_context_from_record(self, record: ReviewCaseRecord) -> ReviewContext:
        row = self.state.processed_message(record.provider, record.message_id)
        if row is None:
            raise ValueError("processed message not found for review case")
        return self._build_context(row)

    def _existing_case(
        self, provider: str, message_id: str
    ) -> ReviewCaseRecord | None:
        row = self.state.connection.execute(
            "SELECT * FROM review_cases WHERE provider=? AND message_id=? "
            "AND reviewer_model=? AND review_version=?",
            (
                provider,
                message_id,
                self.reviewer.model_name,
                self.review_version,
            ),
        ).fetchone()
        return self._record(row) if row else None

    def _build_context(self, row: sqlite3.Row) -> ReviewContext:
        provider = str(row["provider"])
        message_id = str(row["message_id"])
        message = self.resolver.get_message(
            provider=provider,
            message_id=message_id,
            message_source_ref=row["message_source_ref"],
        )
        events = self.resolver.read_events(str(row["source_jsonl_path"]))
        snapshot = self._event_snapshot(events, provider, message_id)
        analysis = snapshot.get("analysis", {})
        important_line = self._important_line_notification(provider, message_id)
        approval = self._approval(provider, message_id)
        execution = self._calendar_execution(approval)
        return ReviewContext(
            provider=provider,
            message_id=message_id,
            source_run_id=(
                str(row["source_run_id"]) if row["source_run_id"] else None
            ),
            source_jsonl_path=str(row["source_jsonl_path"]),
            message_source_ref=(
                str(row["message_source_ref"])
                if row["message_source_ref"] else None
            ),
            received_at=str(row["received_at"]) if row["received_at"] else None,
            subject=snapshot.get("subject") or message.subject,
            analysis_text=self.resolver.analysis_text(message),
            original_classification=analysis.get("llm_proposed_classification"),
            final_classification=(
                analysis.get("final_classification")
                or row["final_classification"]
            ),
            is_important=bool(snapshot.get("is_important")),
            should_notify_user=analysis.get("should_notify_user"),
            llm_should_notify_user=analysis.get("llm_should_notify_user"),
            calendar_candidate=(
                analysis.get("normalized_candidate")
                or snapshot.get("action_parameters")
            ),
            validator_result={
                "validation_issues": analysis.get("validation_issues") or [],
                "calendar_candidate_rejected_reason": (
                    analysis.get("calendar_candidate_rejected_reason")
                ),
                "time_normalization": analysis.get("time_normalization"),
                "fallback_reason": analysis.get("fallback_reason"),
            },
            deterministic_corrections=list(
                analysis.get("classification_corrections") or []
            ),
            line_notification=important_line or self._approval_line_notification(approval),
            approval=approval,
            calendar_execution=execution,
            event_snapshot=snapshot,
        )

    @staticmethod
    def _event_snapshot(
        events: list[dict[str, Any]], provider: str, message_id: str
    ) -> dict[str, Any]:
        observations = [
            event for event in events
            if str(event.get("metadata", {}).get("source_message_id")) == message_id
            and event.get("agent_id") == "mail_to_calendar_agent"
            and event.get("event_type") == "observation_received"
            and str(event.get("observation", {}).get("provider")) == provider
        ]
        if len(observations) != 1:
            raise ValueError(
                "expected one mail audit observation for "
                f"provider/message: {provider}/{message_id}; "
                f"found {len(observations)}"
            )
        observation = observations[0]
        correlation_id = observation.get("correlation_id")
        matches = [
            event for event in events
            if str(event.get("metadata", {}).get("source_message_id")) == message_id
            and event.get("agent_id") == "mail_to_calendar_agent"
            and event.get("correlation_id") == correlation_id
        ]
        decisions = [
            event for event in matches if event.get("event_type") == "decision_made"
        ]
        actions = [
            event for event in matches if event.get("event_type") == "action_executed"
        ]
        if len(decisions) != 1 or len(actions) != 1:
            raise ValueError(
                "incomplete or ambiguous mail audit trace for "
                f"provider/message: {provider}/{message_id}"
            )
        decision = decisions[0]
        action = actions[0]
        return {
            "subject": observation.get("observation", {}).get("subject"),
            "provider": observation.get("observation", {}).get("provider"),
            "is_important": float(decision.get("importance_score", 0)) > 0
            and decision.get("selected_action") != "ignore_email",
            "selected_action": decision.get("selected_action"),
            "decision_explanation": decision.get("explanation"),
            "analysis": decision.get("analysis") or {},
            "action_parameters": action.get("action_parameters"),
            "orchestration_status": action.get("metadata", {}).get(
                "orchestration_status"
            ),
            "correlation_id": correlation_id,
        }

    def _important_line_notification(
        self, provider: str, message_id: str
    ) -> dict[str, Any] | None:
        row = self.state.connection.execute(
            "SELECT status,attempt_count,last_error_type,notified_at "
            "FROM important_mail_notifications WHERE provider=? AND message_id=? "
            "AND notification_type='important_mail'",
            (provider, message_id),
        ).fetchone()
        return dict(row) if row else None

    def _approval(self, provider: str, message_id: str) -> dict[str, Any] | None:
        row = self.state.connection.execute(
            "SELECT * FROM approval_queue WHERE source_provider=? "
            "AND source_message_id=? ORDER BY created_at DESC LIMIT 1",
            (provider, message_id),
        ).fetchone()
        return dict(row) if row else None

    def _approval_line_notification(
        self, approval: dict[str, Any] | None
    ) -> dict[str, Any] | None:
        if not approval:
            return None
        row = self.state.connection.execute(
            "SELECT status,retry_count,last_error_type,sent_at,response_at "
            "FROM line_notifications WHERE approval_id=?",
            (approval["approval_id"],),
        ).fetchone()
        return dict(row) if row else None

    def _calendar_execution(
        self, approval: dict[str, Any] | None
    ) -> dict[str, Any] | None:
        if not approval:
            return None
        row = self.state.connection.execute(
            "SELECT provider,calendar_id,status,attempt_count,external_event_id,"
            "html_link,last_error_type,completed_at "
            "FROM calendar_execution WHERE approval_id=? ORDER BY provider LIMIT 1",
            (approval["approval_id"],),
        ).fetchone()
        return dict(row) if row else None

    @staticmethod
    def _record(row: sqlite3.Row) -> ReviewCaseRecord:
        return ReviewCaseRecord(
            review_case_id=str(row["review_case_id"]),
            provider=str(row["provider"]),
            message_id=str(row["message_id"]),
            source_run_id=str(row["source_run_id"]) if row["source_run_id"] else None,
            source_jsonl_path=str(row["source_jsonl_path"]),
            message_source_ref=(
                str(row["message_source_ref"])
                if row["message_source_ref"] else None
            ),
            reviewer_model=str(row["reviewer_model"]),
            review_version=str(row["review_version"]),
            review_status=str(row["review_status"]),  # type: ignore[arg-type]
            suggested_classification=str(row["suggested_classification"] or ""),
            issue_type=str(row["issue_type"] or ""),
            confidence=float(row["confidence"]),
            reason_summary=str(row["reason_summary"]),
            needs_human_review=bool(row["needs_human_review"]),
            original_classification=(
                str(row["original_classification"])
                if row["original_classification"] else None
            ),
            final_classification=(
                str(row["final_classification"])
                if row["final_classification"] else None
            ),
            human_review_status=str(row["human_review_status"]),
            human_verdict=(
                str(row["human_verdict"]) if row["human_verdict"] else None
            ),
            human_final_classification=(
                str(row["human_final_classification"])
                if row["human_final_classification"] else None
            ),
            subject=str(row["subject"]) if row["subject"] else None,
            human_comment=(
                str(row["human_comment"])
                if row["human_comment"] is not None else None
            ),
            lesson_summary=(
                str(row["lesson_summary"]) if row["lesson_summary"] else None
            ),
            recommended_change_target=(
                str(row["recommended_change_target"])
                if row["recommended_change_target"] else None
            ),
            created_at=datetime.fromisoformat(str(row["created_at"])),
            updated_at=datetime.fromisoformat(str(row["updated_at"])),
            human_reviewed_at=(
                datetime.fromisoformat(str(row["human_reviewed_at"]))
                if row["human_reviewed_at"] else None
            ),
        )
