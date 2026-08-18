from __future__ import annotations

import sqlite3
from dataclasses import asdict
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

from .review_models import (
    HumanVerdictInput, ReviewCaseRecord, ReviewChatMessage, ReviewContext,
    ReviewResult, ReviewRunResult,
)
from .review_provider import ReviewClient
from .review_sources import MessageResolver
from .state import MailStateStore


_QUEUE_STATUSES = {"disagreement", "uncertain"}
_HUMAN_VERDICTS = {
    "original_correct", "reviewer_correct", "modified", "unresolved",
}
_CHANGE_TARGETS = {
    "qwen_prompt", "validator", "deterministic_rule", "test", "none",
}


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
        rows = self.state.review_candidates(
            reviewer_model=self.reviewer.model_name,
            review_version=self.review_version,
            since=since,
            include_reviewed=include_reviewed,
            limit=limit,
        )
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

    def list_cases(
        self,
        *,
        queue_only: bool = False,
        limit: int = 50,
    ) -> list[ReviewCaseRecord]:
        if not 1 <= limit <= 1000:
            raise ValueError("review list limit must be between 1 and 1000")
        query = "SELECT * FROM review_cases"
        parameters: list[Any] = []
        clauses: list[str] = []
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
        return {
            "case": asdict(record),
            "context": context.to_prompt_payload(),
            "event_snapshot": context.event_snapshot,
            "chat_history": [
                {
                    "role": item.role,
                    "content": item.content,
                    "created_at": item.created_at.isoformat(),
                }
                for item in self.chat_history(review_case_id)
            ],
        }

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

    def send_chat_message(self, review_case_id: str, message: str) -> str:
        text = message.strip()
        if not text:
            raise ValueError("chat message is required")
        record = self.get_case(review_case_id)
        context = self._build_context_from_record(record)
        history = self.chat_history(review_case_id)
        reply = self.reviewer.chat(context, history, text)
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
        now = datetime.now(timezone.utc).isoformat()
        with self.state.connection:
            self.state.connection.execute(
                """
                UPDATE review_cases SET human_review_status='resolved',
                    human_verdict=?,human_final_classification=?,lesson_summary=?,
                    recommended_change_target=?,updated_at=?,human_reviewed_at=?
                WHERE review_case_id=?
                """,
                (
                    verdict.human_verdict,
                    verdict.final_classification[:80],
                    verdict.lesson_summary[:2000],
                    verdict.recommended_change_target,
                    now,
                    now,
                    review_case_id,
                ),
            )
        return self.get_case(review_case_id)

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
                    updated_at
                ) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
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
        snapshot = self._event_snapshot(events, message_id)
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
        events: list[dict[str, Any]], message_id: str
    ) -> dict[str, Any]:
        matches = [
            event for event in events
            if str(event.get("metadata", {}).get("source_message_id")) == message_id
            and event.get("agent_id") == "mail_to_calendar_agent"
        ]
        if not matches:
            raise ValueError(f"mail audit events not found for message: {message_id}")
        observation = next(
            event for event in matches if event.get("event_type") == "observation_received"
        )
        decision = next(
            event for event in matches if event.get("event_type") == "decision_made"
        )
        action = next(
            event for event in matches if event.get("event_type") == "action_executed"
        )
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
