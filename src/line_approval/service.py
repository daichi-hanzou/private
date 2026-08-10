from __future__ import annotations

import hmac
import json
import secrets
import sqlite3
import uuid
from datetime import datetime, timezone
from typing import Any, Protocol

from mail_calendar_orchestrator.approvals import ApprovalRecord, ApprovalService
from mail_calendar_orchestrator.state import MailStateStore

from .client import text_message
from .models import LineSendResult, MessagePayload, WebhookResult
from .security import (
    decode_postback,
    encode_postback,
    masked_actor,
    token_hash,
    user_hash,
    verify_signature,
)


class LineClient(Protocol):
    def push(self, user_id: str, message: MessagePayload) -> LineSendResult: ...
    def reply(self, reply_token: str, message: MessagePayload) -> LineSendResult: ...


class CalendarService(Protocol):
    def execute(
        self, approval_id: str, *, provider: str, calendar_id: str
    ) -> Any: ...


class LineApprovalService:
    def __init__(
        self,
        state: MailStateStore,
        approval_service: ApprovalService,
        client: LineClient,
        *,
        channel_secret: str,
        allowed_user_id: str,
        calendar_service: CalendarService | None = None,
        calendar_id: str = "primary",
        max_notification_attempts: int = 3,
    ) -> None:
        if not channel_secret or not allowed_user_id:
            raise ValueError("LINE channel secret and allowed user ID are required")
        self.state = state
        self.approvals = approval_service
        self.client = client
        self.channel_secret = channel_secret
        self.allowed_user_id = allowed_user_id
        self.calendar_service = calendar_service
        self.calendar_id = calendar_id
        self.max_notification_attempts = max_notification_attempts

    def notify(self, approval_id: str) -> LineSendResult:
        record = self.approvals.get(approval_id)
        if record.status != "awaiting_approval":
            raise ValueError("only awaiting approvals can be sent to LINE")
        existing = self.state.connection.execute(
            "SELECT * FROM line_notifications WHERE approval_id=?", (approval_id,)
        ).fetchone()
        if existing and existing["status"] in {"sent", "responded"}:
            return LineSendResult(True)
        if existing and existing["status"] == "failed" and self._non_retryable_error(
            existing["last_error_type"]
        ):
            raise ValueError("LINE notification has a non-retryable failure")
        attempts = int(existing["retry_count"] if existing else 0)
        if attempts >= self.max_notification_attempts:
            raise ValueError("LINE notification retry limit reached")
        token = secrets.token_urlsafe(32)
        now = datetime.now(timezone.utc)
        expires = record.expires_at or now
        notification_id = (
            str(existing["notification_id"]) if existing else f"LN-{uuid.uuid4()}"
        )
        with self.state.connection:
            self.state.connection.execute(
                """
                INSERT INTO approval_interaction_tokens(
                    approval_id,token_hash,created_at,expires_at,consumed_at
                ) VALUES(?,?,?,?,NULL)
                ON CONFLICT(approval_id) DO UPDATE SET
                    token_hash=excluded.token_hash,created_at=excluded.created_at,
                    expires_at=excluded.expires_at,consumed_at=NULL
                """,
                (approval_id, token_hash(token), now.isoformat(), expires.isoformat()),
            )
            self.state.connection.execute(
                """
                INSERT INTO line_notifications(
                    notification_id,approval_id,line_user_hash,status,retry_count
                ) VALUES(?,?,?,?,?)
                ON CONFLICT(approval_id) DO UPDATE SET status='pending',
                    retry_count=excluded.retry_count,last_error_type=NULL,
                    last_error_message=NULL
                """,
                (
                    notification_id, approval_id, user_hash(self.allowed_user_id),
                    "pending", attempts + 1,
                ),
            )
        result = self.client.push(
            self.allowed_user_id, self.notification_payload(record, token)
        )
        with self.state.connection:
            self.state.connection.execute(
                """
                UPDATE line_notifications SET status=?,sent_at=?,last_error_type=?,
                    last_error_message=? WHERE approval_id=?
                """,
                (
                    "sent" if result.success else "failed",
                    now.isoformat() if result.success else None,
                    result.error_type,
                    result.error_message[:200] if result.error_message else None,
                    approval_id,
                ),
            )
        return result

    @staticmethod
    def _non_retryable_error(error_type: str | None) -> bool:
        if not error_type or not error_type.startswith("HTTP"):
            return False
        try:
            status = int(error_type[4:])
        except ValueError:
            return False
        return 400 <= status < 500 and status != 429

    @staticmethod
    def notification_payload(record: ApprovalRecord, token: str) -> MessagePayload:
        time_range = record.start or "時刻未記録"
        if record.end:
            time_range += f"–{record.end}"
        elif record.duration_minutes:
            time_range += f"（{record.duration_minutes}分）"
        reason = (record.classification_summary or "予定候補として抽出されました")[:200]
        text = (
            "📅 カレンダー登録候補\n"
            f"予定: {record.title[:200]}\n"
            f"日時: {record.date or '日付未記録'} {time_range}\n"
            f"種類: {record.candidate_type[:80]}\n"
            f"場所: {(record.location or '未記録')[:100]}\n"
            f"理由: {reason}\n"
            f"元: {(record.source_provider or 'mail').title()}\n"
            f"Approval ID: {record.approval_id}"
        )
        return {
            "type": "text",
            "text": text[:1000],
            "quickReply": {
                "items": [
                    {"type": "action", "action": {
                        "type": "postback", "label": "承認",
                        "data": encode_postback("approve", record.approval_id, token),
                        "displayText": "承認します",
                    }},
                    {"type": "action", "action": {
                        "type": "postback", "label": "拒否",
                        "data": encode_postback("reject", record.approval_id, token),
                        "displayText": "拒否します",
                    }},
                ]
            },
        }

    def handle_webhook(
        self, raw_body: bytes, signature: str | None
    ) -> WebhookResult:
        if not verify_signature(raw_body, signature, self.channel_secret):
            return WebhookResult(401, "invalid signature")
        try:
            payload = json.loads(raw_body)
        except (UnicodeDecodeError, json.JSONDecodeError):
            return WebhookResult(400, "invalid JSON")
        events = payload.get("events")
        if not isinstance(events, list):
            return WebhookResult(400, "invalid webhook")
        for event in events:
            result = self._handle_event(event)
            if result.status_code >= 400:
                return result
        return WebhookResult(200, "ok", bool(events))

    def _handle_event(self, event: Any) -> WebhookResult:
        if not isinstance(event, dict):
            return WebhookResult(400, "invalid event")
        event_id = event.get("webhookEventId")
        if event_id and self._event_seen(str(event_id)):
            return WebhookResult(200, "duplicate event")
        if event.get("type") != "postback":
            return WebhookResult(200, "ignored event")
        source = event.get("source")
        if not isinstance(source, dict) or source.get("type") != "user":
            return WebhookResult(403, "one-to-one user source required")
        incoming_user = str(source.get("userId") or "")
        if not hmac.compare_digest(incoming_user, self.allowed_user_id):
            return WebhookResult(403, "user is not allowed")
        postback = event.get("postback")
        try:
            action, approval_id, token = decode_postback(
                str(postback.get("data") if isinstance(postback, dict) else "")
            )
        except ValueError:
            return WebhookResult(400, "invalid postback")
        claim = self._consume_token(approval_id, token, action=action)
        if claim is not None:
            return claim
        actor = masked_actor(incoming_user)
        try:
            if action == "approve":
                record = self.approvals.get(approval_id)
                if record.status == "awaiting_approval":
                    record = self.approvals.approve(
                        approval_id, actor=actor, reason="Approved via LINE"
                    )
                elif record.status in {"rejected", "expired", "failed"}:
                    return WebhookResult(200, "approval already resolved", True)
                if self.calendar_service is None:
                    reply = "⚠️ 承認しましたがCalendar実行が設定されていません"
                else:
                    result = self.calendar_service.execute(
                        approval_id, provider="google", calendar_id=self.calendar_id
                    )
                    reply = (
                        "✅ Google Calendarに登録しました\n"
                        f"予定: {record.title}\n"
                        f"日時: {record.date or '-'} {record.start or '-'}"
                        if result.success else
                        "⚠️ 承認しましたがCalendar登録に失敗しました\n後で再試行できます。"
                    )
            else:
                record = self.approvals.get(approval_id)
                if record.status == "awaiting_approval":
                    self.approvals.reject(
                        approval_id, actor=actor, reason="Rejected via LINE"
                    )
                else:
                    return WebhookResult(200, "approval already resolved", True)
                reply = "❌ カレンダー登録候補を拒否しました"
        except ValueError:
            return WebhookResult(409, "approval cannot be resolved")
        self._mark_responded(approval_id)
        reply_token = str(event.get("replyToken") or "")
        if reply_token:
            self.client.reply(reply_token, text_message(reply))
        if event_id:
            self._record_event(str(event_id))
        return WebhookResult(200, "ok", True)

    def _consume_token(
        self, approval_id: str, token: str, *, action: str
    ) -> WebhookResult | None:
        connection = sqlite3.connect(self.state.path, timeout=30)
        connection.row_factory = sqlite3.Row
        try:
            connection.execute("BEGIN IMMEDIATE")
            row = connection.execute(
                "SELECT * FROM approval_interaction_tokens WHERE approval_id=?",
                (approval_id,),
            ).fetchone()
            approval = connection.execute(
                "SELECT status FROM approval_queue WHERE approval_id=?", (approval_id,)
            ).fetchone()
            now = datetime.now(timezone.utc)
            if row is None or approval is None:
                connection.rollback()
                return WebhookResult(404, "approval interaction not found")
            if not hmac.compare_digest(row["token_hash"], token_hash(token)):
                connection.rollback()
                return WebhookResult(403, "approval token mismatch")
            if row["consumed_at"]:
                connection.rollback()
                if action == "approve" and approval["status"] in {
                    "approved", "executing", "calendar_failed", "calendar_created"
                }:
                    return None
                return WebhookResult(200, "approval token already used")
            if datetime.fromisoformat(row["expires_at"]) <= now:
                connection.rollback()
                return WebhookResult(410, "approval token expired")
            if approval["status"] != "awaiting_approval":
                connection.rollback()
                return WebhookResult(200, "approval is no longer pending")
            connection.execute(
                "UPDATE approval_interaction_tokens SET consumed_at=? "
                "WHERE approval_id=? AND consumed_at IS NULL",
                (now.isoformat(), approval_id),
            )
            connection.commit()
            return None
        finally:
            connection.close()

    def _event_seen(self, event_id: str) -> bool:
        return self.state.connection.execute(
            "SELECT 1 FROM line_webhook_events WHERE webhook_event_id=?", (event_id,)
        ).fetchone() is not None

    def _record_event(self, event_id: str) -> None:
        with self.state.connection:
            self.state.connection.execute(
                "INSERT OR IGNORE INTO line_webhook_events VALUES(?,?)",
                (event_id, datetime.now(timezone.utc).isoformat()),
            )

    def _mark_responded(self, approval_id: str) -> None:
        with self.state.connection:
            self.state.connection.execute(
                "UPDATE line_notifications SET status='responded',response_at=? "
                "WHERE approval_id=?",
                (datetime.now(timezone.utc).isoformat(), approval_id),
            )
