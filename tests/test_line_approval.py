from __future__ import annotations

import base64
import hashlib
import hmac
import json
import subprocess
from types import SimpleNamespace
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest

from calendar_agent.agent import CalendarAgent
from calendar_agent.models import CalendarRequest
from calendar_execution.models import CalendarExecutionResult
from calendar_execution.service import CalendarExecutionService
from calendar_agent.audit import read_complete_jsonl
from line_approval.models import ImportantDispatchResult, LineSendResult
from line_approval.client import LineMessagingClient, text_message
from line_approval.cli import initialize_config
from line_approval.security import verify_signature
from line_approval.service import LineApprovalService
from line_approval.systemd import (
    LINE_WEBHOOK_SERVICE,
    LineWebhookSystemdService,
    webhook_service_unit,
)
from mail_calendar_orchestrator.cli import _parser
from mail_calendar_orchestrator.approvals import ApprovalService
from mail_calendar_orchestrator.audit import write_jsonl_atomic
from mail_calendar_orchestrator.state import MailStateStore


SECRET = "test-channel-secret"
USER_ID = "U-allowed-private-user"


class FakeLineClient:
    def __init__(self, push_result=None):
        self.pushes = []
        self.replies = []
        self.push_result = push_result or LineSendResult(True)

    def push(self, user_id, message):
        self.pushes.append((user_id, message))
        return self.push_result

    def reply(self, reply_token, message):
        self.replies.append((reply_token, message))
        return LineSendResult(True)


class FakeExecutor:
    def __init__(self, success=True):
        self.requests = []
        self.success = success

    def create_event(self, request):
        self.requests.append(request)
        if self.success:
            return CalendarExecutionResult(
                True, "google", request.calendar_id,
                external_event_id="google-event-1",
                start=request.start, end=request.end,
            )
        return CalendarExecutionResult(
            False, "google", request.calendar_id,
            error_type="TemporaryError", error_message="temporary failure",
            retryable=True,
        )


def setup_approval(tmp_path, *, executor_success=True):
    events = CalendarAgent().propose(CalendarRequest(
        title="Dental appointment", requested_date="2026-08-12",
        preferred_period="afternoon", duration_minutes=60,
        available_slots=["13:00"], requires_approval=True,
    ))
    events[-1]["metadata"].update({
        "source_provider": "outlook",
        "source_message_id": "PRIVATE-MESSAGE-ID",
        "candidate_type": "event",
        "candidate_date": "2026-08-12",
        "candidate_end": "14:00",
        "candidate_timezone": "Asia/Tokyo",
        "candidate_location": "Dental clinic",
    })
    source = write_jsonl_atomic(tmp_path / "pending.jsonl", events)
    store = MailStateStore(tmp_path / "state.sqlite3")
    approvals = ApprovalService(store, output_dir=tmp_path)
    record = approvals.register_pending(events, source)[0]
    client = FakeLineClient()
    executor = FakeExecutor(executor_success)
    calendar = CalendarExecutionService(store, executor, output_dir=tmp_path)
    service = LineApprovalService(
        store, approvals, client, channel_secret=SECRET,
        allowed_user_id=USER_ID, calendar_service=calendar,
    )
    return store, approvals, record, client, executor, service


def test_start_only_hospital_candidate_creates_approval_and_line_payload(tmp_path):
    events = CalendarAgent().propose(CalendarRequest(
        title="病院予約", requested_date="2026-08-15",
        preferred_period=None, duration_minutes=None,
        available_slots=["10:30"], requires_approval=True,
    ))
    events[-1]["metadata"].update({
        "mail_candidate_id": "hospital-candidate",
        "candidate_type": "event",
        "candidate_date": "2026-08-15",
        "candidate_end": None,
        "candidate_timezone": "Asia/Tokyo",
        "source_provider": "gmail",
        "source_message_id": "hospital-mail",
    })
    source = write_jsonl_atomic(tmp_path / "hospital.jsonl", events)
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        record = ApprovalService(store).register_pending(events, source)[0]

        payload = LineApprovalService.notification_payload(record, "safe-token")

        assert record.date == "2026-08-15"
        assert record.start == "10:30"
        assert record.end is None
        assert record.duration_minutes is None
        serialized = json.dumps(payload, ensure_ascii=False)
        assert payload["type"] == "flex"
        assert "quickReply" not in payload
        assert "2026-08-15 10:30" in serialized
        assert "60分" not in serialized
        assert {button["action"]["label"] for button in (
            payload["contents"]["footer"]["contents"]
        )} == {"承認", "拒否"}


def signed(body: bytes) -> str:
    return base64.b64encode(
        hmac.new(SECRET.encode(), body, hashlib.sha256).digest()
    ).decode()


def postback_body(data: str, *, user=USER_ID, source_type="user", event_id="evt-1"):
    return json.dumps({"events": [{
        "type": "postback", "webhookEventId": event_id,
        "source": {"type": source_type, "userId": user},
        "replyToken": "reply-token", "postback": {"data": data},
    }]}, separators=(",", ":")).encode()


def notify_and_data(service, client, approval_id, action="approve"):
    assert service.notify(approval_id).success
    payload = client.pushes[-1][1]
    action_payload = next(
        item["action"]
        for item in payload["contents"]["footer"]["contents"]
        if item["action"]["label"] == (
            "承認" if action == "approve" else "拒否"
        )
    )
    return payload, action_payload["data"]


def test_notification_is_minimal_private_and_deduplicated(tmp_path):
    store, _, record, client, _, service = setup_approval(tmp_path)
    try:
        payload, _ = notify_and_data(service, client, record.approval_id)
        assert payload["type"] == "flex"
        assert "quickReply" not in payload
        serialized = json.dumps(payload, ensure_ascii=False)
        assert "Dental appointment" in serialized
        assert "2026-08-12" in serialized
        assert "13:00–14:00" in serialized
        buttons = payload["contents"]["footer"]["contents"]
        assert {button["action"]["label"] for button in buttons} == {
            "承認", "拒否"
        }
        assert all(
            button["action"]["type"] == "postback" for button in buttons
        )
        assert "PRIVATE-MESSAGE-ID" not in serialized
        assert "sender" not in serialized
        assert "body_text" not in serialized
        assert service.notify(record.approval_id).success
        assert len(client.pushes) == 1
        row = store.connection.execute("SELECT * FROM line_notifications").fetchone()
        assert row["status"] == "sent"
        assert row["line_user_hash"] != USER_ID
        token = store.connection.execute(
            "SELECT * FROM approval_interaction_tokens"
        ).fetchone()
        assert len(token["token_hash"]) == 64
        assert "safe-token" not in serialized
    finally:
        store.close()


def test_important_mail_notification_is_button_free_private_and_deduplicated(
    tmp_path,
):
    store, approvals, _, client, _, _ = setup_approval(tmp_path)
    assert store.register_important_notification(
        provider="gmail", message_id="gmail:bank-1", category="deadline",
        subject="口座振替予定のお知らせ",
        notification_date="2026-08-27", amount="58,240円",
    )
    assert not store.register_important_notification(
        provider="gmail", message_id="gmail:bank-1", category="deadline",
        subject="口座振替予定のお知らせ",
        notification_date="2026-08-27", amount="58,240円",
    )
    service = LineApprovalService(
        store, approvals, client, channel_secret=SECRET,
        allowed_user_id=USER_ID,
    )
    try:
        assert service.notify_important("gmail", "gmail:bank-1").success
        assert service.notify_important("gmail", "gmail:bank-1").success
        assert len(client.pushes) == 1
        payload = client.pushes[0][1]
        assert payload["type"] == "text"
        assert "quickReply" not in payload
        assert "承認" not in payload["text"]
        assert "拒否" not in payload["text"]
        assert "口座振替予定のお知らせ" in payload["text"]
        assert "deadline" in payload["text"]
        assert "2026-08-27" in payload["text"]
        assert "58,240円" in payload["text"]
        row = store.connection.execute(
            "SELECT * FROM important_mail_notifications"
        ).fetchone()
        assert row["status"] == "sent"
        assert row["attempt_count"] == 1
        assert row["notified_at"]
    finally:
        store.close()


def test_important_mail_line_failure_is_recorded_safely_and_retryable(tmp_path):
    store, approvals, _, _, _, _ = setup_approval(tmp_path)
    store.register_important_notification(
        provider="outlook", message_id="outlook:tax-1", category="deadline",
        subject="税金の納付期限", notification_date="2026-08-31", amount=None,
    )
    client = FakeLineClient(
        LineSendResult(False, True, "HTTP503", "PRIVATE RAW RESPONSE")
    )
    service = LineApprovalService(
        store, approvals, client, channel_secret=SECRET,
        allowed_user_id=USER_ID,
    )
    try:
        result = service.notify_important("outlook", "outlook:tax-1")
        assert not result.success and result.retryable
        row = store.connection.execute(
            "SELECT * FROM important_mail_notifications"
        ).fetchone()
        assert row["status"] == "failed"
        assert row["last_error_type"] == "HTTP503"
        assert row["retryable"] == 1
        assert "PRIVATE RAW RESPONSE" not in str(dict(row))
        assert row["notified_at"] is None
    finally:
        store.close()


def test_important_dispatcher_sends_pending_once_and_does_not_mix_approval(
    tmp_path,
):
    store, approvals, record, client, _, service = setup_approval(tmp_path)
    store.register_important_notification(
        provider="gmail", message_id="gmail:bank-dispatch",
        category="deadline", subject="口座振替予定",
        notification_date="2026-08-27", amount="58,240円",
    )
    try:
        first = service.dispatch_pending_important()
        second = service.dispatch_pending_important()
        assert (first.selected, first.sent, first.failed, first.skipped) == (
            1, 1, 0, 0
        )
        assert second.selected == second.sent == second.failed == 0
        assert len(client.pushes) == 1
        assert "quickReply" not in client.pushes[0][1]
        assert approvals.get(record.approval_id).status == "awaiting_approval"
        assert store.connection.execute(
            "SELECT COUNT(*) FROM line_notifications"
        ).fetchone()[0] == 0
    finally:
        store.close()


def test_important_dispatcher_retries_retryable_failure_then_sends(tmp_path):
    store, approvals, _, _, _, _ = setup_approval(tmp_path)
    store.register_important_notification(
        provider="gmail", message_id="gmail:retry-bank", category="deadline",
        subject="カード引落し", notification_date="2026-08-27", amount="42,000円",
    )
    failed_client = FakeLineClient(
        LineSendResult(False, True, "HTTP503", "safe failure")
    )
    failed_service = LineApprovalService(
        store, approvals, failed_client, channel_secret=SECRET,
        allowed_user_id=USER_ID,
    )
    try:
        first = failed_service.dispatch_pending_important()
        assert (first.selected, first.sent, first.failed) == (1, 0, 1)
        row = store.connection.execute(
            "SELECT * FROM important_mail_notifications"
        ).fetchone()
        assert row["status"] == "failed"
        assert row["attempt_count"] == 1
        assert row["attempted_at"]
        assert row["retryable"] == 1

        success_client = FakeLineClient()
        success_service = LineApprovalService(
            store, approvals, success_client, channel_secret=SECRET,
            allowed_user_id=USER_ID,
        )
        second = success_service.dispatch_pending_important()
        assert (second.selected, second.sent, second.failed) == (1, 1, 0)
        row = store.connection.execute(
            "SELECT * FROM important_mail_notifications"
        ).fetchone()
        assert row["status"] == "sent"
        assert row["attempt_count"] == 2
        assert row["notified_at"]
        assert len(success_client.pushes) == 1
    finally:
        store.close()


def test_nonretryable_important_failure_is_not_redispatched(tmp_path):
    store, approvals, _, _, _, _ = setup_approval(tmp_path)
    store.register_important_notification(
        provider="gmail", message_id="gmail:no-retry", category="deadline",
        subject="税金期限", notification_date="2026-08-31", amount=None,
    )
    service = LineApprovalService(
        store, approvals,
        FakeLineClient(LineSendResult(False, False, "HTTP401", "rejected")),
        channel_secret=SECRET, allowed_user_id=USER_ID,
    )
    try:
        assert service.dispatch_pending_important().failed == 1
        assert service.dispatch_pending_important().selected == 0
    finally:
        store.close()


def test_line_important_send_pending_cli_uses_dispatcher(
    tmp_path, monkeypatch, capsys
):
    config = tmp_path / "line.env"
    config.write_text(
        "LINE_CHANNEL_SECRET=test-secret\n"
        "LINE_CHANNEL_ACCESS_TOKEN=test-token\n"
        "LINE_ALLOWED_USER_ID=test-user\n",
        encoding="utf-8",
    )
    monkeypatch.setattr(
        LineApprovalService, "dispatch_pending_important",
        lambda self, *, limit: ImportantDispatchResult(2, 1, 1, 0),
    )
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "line",
        "--config", str(config),
        "--state-db", str(tmp_path / "state.sqlite3"),
        "important", "send-pending", "--limit", "25",
    ])
    from mail_calendar_orchestrator.cli import main

    main()
    output = capsys.readouterr().out
    assert "Selected: 2" in output
    assert "Sent: 1" in output
    assert "Failed: 1" in output
    assert "Skipped: 0" in output


def test_scheduled_best_effort_dispatches_pending_important_mail(
    tmp_path, monkeypatch
):
    db = tmp_path / "state.sqlite3"
    with MailStateStore(db) as store:
        store.register_important_notification(
            provider="gmail", message_id="gmail:scheduled-bank",
            category="deadline", subject="口座振替予定",
            notification_date="2026-08-27", amount="58,240円",
        )
    client = FakeLineClient()
    monkeypatch.setattr(
        "mail_calendar_orchestrator.cli.configuration_status",
        lambda _path: {"secret": True, "token": True, "user": True},
    )
    monkeypatch.setattr(
        "mail_calendar_orchestrator.cli.load_config",
        lambda _path: SimpleNamespace(
            channel_access_token="safe-token",
            channel_secret="safe-secret",
            allowed_user_id="safe-user",
        ),
    )
    monkeypatch.setattr(
        "mail_calendar_orchestrator.cli.LineMessagingClient",
        lambda _token: client,
    )
    from mail_calendar_orchestrator.cli import _notify_pending_line_best_effort

    _notify_pending_line_best_effort(db)
    assert len(client.pushes) == 1
    with MailStateStore(db) as store:
        row = store.connection.execute(
            "SELECT * FROM important_mail_notifications"
        ).fetchone()
        assert row["status"] == "sent"
        assert row["notified_at"]


@pytest.mark.parametrize("action,expected", [("approve", "calendar_created"), ("reject", "rejected")])
def test_postback_approve_or_reject_end_to_end(tmp_path, action, expected):
    store, approvals, record, client, executor, service = setup_approval(tmp_path)
    try:
        _, data = notify_and_data(service, client, record.approval_id, action)
        body = postback_body(data)
        result = service.handle_webhook(body, signed(body))
        assert result.status_code == 200
        assert approvals.get(record.approval_id).status == expected
        assert len(executor.requests) == (1 if action == "approve" else 0)
        assert client.replies
        assert approvals.get(record.approval_id).actor.startswith("line:")
        assert USER_ID not in approvals.get(record.approval_id).actor
        resolved_events = read_complete_jsonl(
            approvals.get(record.approval_id).outcome_jsonl_path
        )
        intervention = next(
            event for event in resolved_events
            if event["event_type"] == "human_intervention"
        )
        assert intervention["input_method"] == "line_postback"
        replay = service.handle_webhook(body, signed(body))
        assert replay.status_code == 200
        assert len(executor.requests) == (1 if action == "approve" else 0)
        if action == "approve":
            second_click = postback_body(data, event_id="evt-second-click")
            assert service.handle_webhook(
                second_click, signed(second_click)
            ).status_code == 200
            assert len(executor.requests) == 1
    finally:
        store.close()


def test_old_flex_approval_still_works_after_multiple_important_messages(tmp_path):
    store, approvals, record, client, executor, service = setup_approval(tmp_path)
    try:
        approval_payload, data = notify_and_data(
            service, client, record.approval_id, "approve"
        )
        for index in range(2):
            message_id = f"gmail:important-{index}"
            assert store.register_important_notification(
                provider="gmail", message_id=message_id,
                category="deadline", subject=f"重要通知 {index}",
                notification_date="2026-08-27", amount=None,
            )
            assert service.notify_important("gmail", message_id).success

        assert approval_payload["type"] == "flex"
        assert all(payload["type"] == "text" for _, payload in client.pushes[1:])
        body = postback_body(data, event_id="old-flex-after-important")
        result = service.handle_webhook(body, signed(body))

        assert result.status_code == 200
        assert approvals.get(record.approval_id).status == "calendar_created"
        assert len(executor.requests) == 1
    finally:
        store.close()


def test_calendar_failure_keeps_retryable_state_and_replies(tmp_path):
    store, approvals, record, client, executor, service = setup_approval(
        tmp_path, executor_success=False
    )
    try:
        _, data = notify_and_data(service, client, record.approval_id)
        body = postback_body(data)
        assert service.handle_webhook(body, signed(body)).status_code == 200
        assert approvals.get(record.approval_id).status == "calendar_failed"
        assert len(executor.requests) == 1
        assert "後で再試行" in client.replies[-1][1]["text"]
    finally:
        store.close()


@pytest.mark.parametrize(
    "mutation,expected",
    [
        (lambda body, signature: (body, "invalid"), 401),
        (lambda body, signature: (body + b" ", signature), 401),
        (lambda body, signature: (body, None), 401),
    ],
)
def test_webhook_signature_required_on_exact_raw_body(tmp_path, mutation, expected):
    store, approvals, record, client, _, service = setup_approval(tmp_path)
    try:
        _, data = notify_and_data(service, client, record.approval_id)
        body = postback_body(data)
        changed, signature = mutation(body, signed(body))
        assert service.handle_webhook(changed, signature).status_code == expected
        assert approvals.get(record.approval_id).status == "awaiting_approval"
    finally:
        store.close()


def test_signature_helper_accepts_valid_and_rejects_forged():
    body = b'{"events":[]}'
    assert verify_signature(body, signed(body), SECRET)
    assert not verify_signature(body + b" ", signed(body), SECRET)


@pytest.mark.parametrize(
    "user,source_type,expected",
    [("U-other", "user", 403), (USER_ID, "group", 403)],
)
def test_unknown_user_and_group_cannot_resolve(tmp_path, user, source_type, expected):
    store, approvals, record, client, _, service = setup_approval(tmp_path)
    try:
        _, data = notify_and_data(service, client, record.approval_id)
        body = postback_body(data, user=user, source_type=source_type)
        assert service.handle_webhook(body, signed(body)).status_code == expected
        assert approvals.get(record.approval_id).status == "awaiting_approval"
    finally:
        store.close()


def test_token_tampering_expiry_and_replay_are_rejected(tmp_path):
    store, approvals, record, client, executor, service = setup_approval(tmp_path)
    try:
        _, data = notify_and_data(service, client, record.approval_id)
        tampered = data.replace("token=", "token=x")
        body = postback_body(tampered, event_id="tampered")
        assert service.handle_webhook(body, signed(body)).status_code == 403
        assert approvals.get(record.approval_id).status == "awaiting_approval"
        with store.connection:
            store.connection.execute(
                "UPDATE approval_interaction_tokens SET expires_at=? WHERE approval_id=?",
                ((datetime.now(timezone.utc) - timedelta(seconds=1)).isoformat(), record.approval_id),
            )
        body = postback_body(data, event_id="expired")
        assert service.handle_webhook(body, signed(body)).status_code == 410
        assert not executor.requests
    finally:
        store.close()


@pytest.mark.parametrize(
    "result",
    [
        LineSendResult(False, True, "HTTP429", "LINE API temporarily unavailable"),
        LineSendResult(False, True, "HTTP500", "LINE API temporarily unavailable"),
        LineSendResult(False, True, "Timeout", "LINE API request failed"),
    ],
)
def test_retryable_notification_failure_is_stored_without_secrets(tmp_path, result):
    store, approvals, record, _, _, _ = setup_approval(tmp_path)
    client = FakeLineClient(result)
    service = LineApprovalService(
        store, approvals, client, channel_secret=SECRET, allowed_user_id=USER_ID
    )
    try:
        sent = service.notify(record.approval_id)
        assert not sent.success and sent.retryable
        row = store.connection.execute("SELECT * FROM line_notifications").fetchone()
        assert row["status"] == "failed"
        stored = json.dumps(dict(row))
        assert SECRET not in stored
        assert USER_ID not in stored
        assert "token=" not in stored
    finally:
        store.close()


def test_authentication_failure_is_not_retried(tmp_path):
    store, approvals, record, _, _, _ = setup_approval(tmp_path)
    client = FakeLineClient(
        LineSendResult(False, False, "HTTP401", "LINE API rejected request")
    )
    service = LineApprovalService(
        store, approvals, client, channel_secret=SECRET, allowed_user_id=USER_ID
    )
    try:
        assert not service.notify(record.approval_id).success
        with pytest.raises(ValueError, match="non-retryable"):
            service.notify(record.approval_id)
        assert len(client.pushes) == 1
    finally:
        store.close()


def test_concurrent_postback_only_executes_once(tmp_path):
    store, _, record, client, _, service = setup_approval(tmp_path)
    _, data = notify_and_data(service, client, record.approval_id)
    store.close()

    def resolve(index):
        local = MailStateStore(tmp_path / "state.sqlite3")
        executor = FakeExecutor()
        local_service = LineApprovalService(
            local, ApprovalService(local, output_dir=tmp_path), FakeLineClient(),
            channel_secret=SECRET, allowed_user_id=USER_ID,
            calendar_service=CalendarExecutionService(local, executor, output_dir=tmp_path),
        )
        body = postback_body(data, event_id=f"concurrent-{index}")
        try:
            return local_service.handle_webhook(body, signed(body)).status_code, len(executor.requests)
        finally:
            local.close()

    with ThreadPoolExecutor(max_workers=2) as pool:
        results = list(pool.map(resolve, range(2)))
    assert sum(calls for _, calls in results) == 1
    assert sorted(status for status, _ in results) == [200, 200]


@pytest.mark.parametrize(
    "status,retryable",
    [(400, False), (401, False), (429, True), (500, True), (503, True)],
)
def test_line_client_classifies_http_failures_without_exposing_token(
    monkeypatch, status, retryable
):
    class Response:
        status_code = status

    captured = {}
    def fake_post(url, **kwargs):
        captured.update(url=url, **kwargs)
        return Response()

    monkeypatch.setattr("line_approval.client.requests.post", fake_post)
    result = LineMessagingClient("PRIVATE-TOKEN").push(
        USER_ID, text_message("test")
    )
    assert not result.success
    assert result.retryable is retryable
    assert "PRIVATE-TOKEN" not in (result.error_message or "")
    assert captured["url"].endswith("/push")


def test_line_config_is_created_private(tmp_path):
    path = initialize_config(tmp_path / "line-approval.env")
    assert path.stat().st_mode & 0o777 == 0o600
    assert "LINE_CHANNEL_SECRET=" in path.read_text()


def test_line_webhook_systemd_unit_is_local_and_contains_no_secrets():
    unit = webhook_service_unit(
        working_directory=Path("/home/daichi/Desktop/private"),
        uv_path=Path("/home/daichi/.local/bin/uv"),
        env_file=Path("/home/daichi/.config/agentledger/line-approval.env"),
    )
    assert "WorkingDirectory=/home/daichi/Desktop/private" in unit
    assert "ExecStart=/home/daichi/.local/bin/uv run" in unit
    assert "line webhook --host 127.0.0.1 --port 8787" in unit
    assert "EnvironmentFile=/home/daichi/.config/agentledger/line-approval.env" in unit
    assert "Restart=on-failure" in unit
    assert "RestartSec=5" in unit
    assert "WantedBy=default.target" in unit
    assert "0.0.0.0" not in unit
    assert "CHANNEL_SECRET=" not in unit
    assert "ACCESS_TOKEN=" not in unit
    assert "ALLOWED_USER_ID=" not in unit


def test_line_webhook_service_install_and_management_are_mocked(tmp_path):
    calls = []

    def runner(command, **kwargs):
        calls.append((command, kwargs))
        if "is-enabled" in command:
            return subprocess.CompletedProcess(command, 0, "enabled\n", "")
        if "is-active" in command:
            return subprocess.CompletedProcess(command, 0, "active\n", "")
        return subprocess.CompletedProcess(command, 0, "", "")

    env = tmp_path / ".config/agentledger/line-approval.env"
    env.parent.mkdir(parents=True)
    env.write_text("LINE_CHANNEL_SECRET=private\n", encoding="utf-8")
    manager = LineWebhookSystemdService(
        home=tmp_path,
        working_directory=Path("/srv/agentledger"),
        uv_path=Path("/opt/uv/bin/uv"),
        env_file=env,
        runner=runner,
        health_check=lambda: True,
    )
    installed = manager.install()
    assert installed.name == LINE_WEBHOOK_SERVICE
    assert installed.stat().st_mode & 0o777 == 0o644
    assert env.stat().st_mode & 0o777 == 0o600
    assert "private" not in installed.read_text()
    manager.enable()
    manager.restart()
    status = manager.status()
    assert "Installed: yes" in status
    assert "Enabled: yes" in status
    assert "Active: active" in status
    assert "Health: ok" in status
    manager.disable()
    manager.uninstall()
    commands = [item[0] for item in calls]
    assert ["systemctl", "--user", "daemon-reload"] in commands
    assert [
        "systemctl", "--user", "enable", "--now", LINE_WEBHOOK_SERVICE
    ] in commands
    assert ["systemctl", "--user", "restart", LINE_WEBHOOK_SERVICE] in commands
    assert [
        "systemctl", "--user", "disable", "--now", LINE_WEBHOOK_SERVICE
    ] in commands
    assert not manager.service_file.exists()
    assert env.exists()


def test_line_webhook_service_rejects_missing_env(tmp_path):
    manager = LineWebhookSystemdService(
        home=tmp_path,
        working_directory=tmp_path,
        uv_path=Path("/opt/uv/bin/uv"),
        env_file=tmp_path / "missing.env",
        runner=lambda *args, **kwargs: None,
    )
    with pytest.raises(ValueError, match="environment file not found"):
        manager.install()
    with pytest.raises(ValueError, match="environment file not found"):
        manager.enable()


def test_line_webhook_service_status_reports_health_failure(tmp_path):
    def runner(command, **kwargs):
        del kwargs
        return subprocess.CompletedProcess(
            command, 0 if "is-active" in command else 1,
            "active\n" if "is-active" in command else "disabled\n", "",
        )

    manager = LineWebhookSystemdService(
        home=tmp_path,
        working_directory=tmp_path,
        uv_path=Path("/opt/uv/bin/uv"),
        env_file=tmp_path / "missing.env",
        runner=runner,
        health_check=lambda: False,
    )
    status = manager.status()
    assert "Installed: no" in status
    assert "Enabled: no" in status
    assert "Active: active" in status
    assert "Health: unavailable" in status
    assert "http://127.0.0.1:8787/line/webhook" in status


def test_line_service_cli_subcommands_parse():
    parser = _parser()
    for command in ("install", "status", "enable", "disable", "restart", "uninstall"):
        parsed = parser.parse_args(["line", "service", command])
        assert parsed.line_command == "service"
        assert parsed.line_service_command == command
