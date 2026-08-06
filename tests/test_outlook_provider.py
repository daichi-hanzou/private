from __future__ import annotations

import json
import stat
from datetime import datetime, timezone

import pytest
import requests

from agentledger.bundles import build_action_bundles, outcome_status
from agentledger.html import render_explorer
from agentledger.ingestion import read_jsonl
from agentledger.normalizer import normalize_events
from mail_calendar_orchestrator.service import MailCalendarOrchestrator
from mail_calendar_orchestrator.cli import main
from mail_to_calendar.microsoft_auth import (
    MicrosoftAuthError,
    MicrosoftAuthenticator,
    mask_account,
)
from mail_to_calendar.models import EmailMessage
from mail_to_calendar.outlook_provider import (
    OutlookProvider,
    OutlookProviderConfig,
    OutlookProviderError,
    body_to_text,
)
from mail_to_calendar.service import MailToCalendarService


TOKEN = "fake-access-token-never-log"


class _Cache:
    def __init__(self, *, changed: bool = False, broken: bool = False) -> None:
        self.has_state_changed = changed
        self.broken = broken
        self.loaded = None

    def deserialize(self, value: str) -> None:
        if self.broken:
            raise ValueError("broken cache")
        self.loaded = value

    def serialize(self) -> str:
        return '{"cache":"opaque"}'


class _App:
    def __init__(
        self,
        *,
        accounts=None,
        silent=None,
        device=None,
    ) -> None:
        self.accounts = accounts or []
        self.silent = silent
        self.device = device or {
            "access_token": TOKEN,
            "id_token_claims": {"preferred_username": "person@outlook.com"},
        }
        self.device_started = 0

    def get_accounts(self):
        return self.accounts

    def acquire_token_silent(self, scopes, account):
        assert scopes == ["Mail.Read"]
        assert account == self.accounts[0]
        return self.silent

    def initiate_device_flow(self, scopes):
        assert scopes == ["Mail.Read"]
        self.device_started += 1
        return {
            "user_code": "SAFE-CODE",
            "message": "Visit Microsoft and enter SAFE-CODE",
        }

    def acquire_token_by_device_flow(self, flow):
        assert flow["user_code"] == "SAFE-CODE"
        return self.device


class _Response:
    def __init__(self, status, payload=None, *, headers=None, json_error=False):
        self.status_code = status
        self.payload = payload
        self.headers = headers or {}
        self.json_error = json_error

    def json(self):
        if self.json_error:
            raise ValueError("invalid")
        return self.payload


class _Transport:
    def __init__(self, responses):
        self.responses = list(responses)
        self.calls = []

    def get(self, url, *, headers, params, timeout):
        self.calls.append(
            {
                "url": url,
                "headers": headers,
                "params": params,
                "timeout": timeout,
            }
        )
        response = self.responses.pop(0)
        if isinstance(response, BaseException):
            raise response
        return response


class _Auth:
    last_account = "person@outlook.com"

    def acquire_token(self) -> str:
        return TOKEN


def _raw_message(message_id="message-1", *, subject="8月12日 15時 定例会議"):
    return {
        "id": message_id,
        "conversationId": "conversation-1",
        "subject": subject,
        "from": {"emailAddress": {"address": "sender@example.com"}},
        "toRecipients": [
            {"emailAddress": {"address": "me@outlook.com"}}
        ],
        "receivedDateTime": "2026-08-06T01:00:00Z",
        "body": {
            "contentType": "html",
            "content": (
                "<style>.secret{}</style><p>8月12日15時から"
                "<b>定例会議</b>です。</p><script>steal()</script>"
            ),
        },
        "bodyPreview": "定例会議への参加依頼" * 20,
        "categories": ["Work"],
        "importance": "high",
        "hasAttachments": True,
        "isRead": False,
        "internetMessageId": "<fixture@example.com>",
    }


def _config(tmp_path, **overrides):
    values = {
        "client_id": "fixture-client-id",
        "token_cache_path": tmp_path / "cache.json",
        "max_messages": 5,
    }
    values.update(overrides)
    return OutlookProviderConfig(**values)


def test_cached_token_avoids_device_flow(tmp_path) -> None:
    cache_path = tmp_path / "cache.json"
    cache_path.write_text("cached-state", encoding="utf-8")
    cache = _Cache()
    app = _App(
        accounts=[{"username": "person@outlook.com"}],
        silent={"access_token": TOKEN},
    )
    auth = MicrosoftAuthenticator(
        client_id="client-id",
        authority="https://login.microsoftonline.com/consumers",
        scopes=["Mail.Read"],
        token_cache_path=cache_path,
        app_factory=lambda *args, **kwargs: app,
        cache_factory=lambda: cache,
    )

    assert auth.acquire_token() == TOKEN
    assert app.device_started == 0
    assert cache.loaded == "cached-state"
    assert auth.last_account == "person@outlook.com"


def test_device_flow_and_atomic_private_cache(tmp_path) -> None:
    cache = _Cache(changed=True)
    app = _App()
    messages = []
    cache_path = tmp_path / "config" / "microsoft_token_cache.json"
    auth = MicrosoftAuthenticator(
        client_id="client-id",
        authority="https://login.microsoftonline.com/consumers",
        scopes=["Mail.Read"],
        token_cache_path=cache_path,
        app_factory=lambda *args, **kwargs: app,
        cache_factory=lambda: cache,
        device_code_callback=messages.append,
    )

    assert auth.acquire_token() == TOKEN
    assert app.device_started == 1
    assert messages == ["Visit Microsoft and enter SAFE-CODE"]
    assert cache_path.read_text(encoding="utf-8") == '{"cache":"opaque"}'
    assert stat.S_IMODE(cache_path.stat().st_mode) == 0o600
    assert not list(cache_path.parent.glob("*.tmp"))
    assert auth.last_account == "person@outlook.com"
    assert mask_account(auth.last_account) == "p***@outlook.com"


def test_auth_failure_and_broken_cache_do_not_expose_secrets(tmp_path) -> None:
    secret = "secret-token-in-description"
    failed = MicrosoftAuthenticator(
        client_id="client-id",
        authority="https://login.microsoftonline.com/consumers",
        scopes=["Mail.Read"],
        token_cache_path=tmp_path / "missing.json",
        app_factory=lambda *args, **kwargs: _App(
            device={"error": "authorization_declined", "error_description": secret}
        ),
        cache_factory=lambda: _Cache(),
        device_code_callback=lambda message: None,
    )
    with pytest.raises(MicrosoftAuthError) as captured:
        failed.acquire_token()
    assert "authorization_declined" in str(captured.value)
    assert secret not in str(captured.value)

    cache_path = tmp_path / "broken.json"
    cache_path.write_text("not-a-cache", encoding="utf-8")
    broken = MicrosoftAuthenticator(
        client_id="client-id",
        authority="https://login.microsoftonline.com/consumers",
        scopes=["Mail.Read"],
        token_cache_path=cache_path,
        app_factory=lambda *args, **kwargs: _App(),
        cache_factory=lambda: _Cache(broken=True),
    )
    with pytest.raises(MicrosoftAuthError, match="cache is unreadable"):
        broken.acquire_token()


def test_config_enforces_read_only_scope_and_small_limit(tmp_path) -> None:
    with pytest.raises(ValueError, match="only delegated Mail.Read"):
        _config(tmp_path, scopes=["Mail.ReadWrite"])
    with pytest.raises(ValueError, match="between 1 and 100"):
        _config(tmp_path, max_messages=101)
    with pytest.raises(ValueError, match="consumers endpoint"):
        _config(tmp_path, authority="https://login.example.com/consumers")


def test_graph_message_conversion_and_safe_html(tmp_path) -> None:
    transport = _Transport([_Response(200, {"value": [_raw_message()]})])
    provider = OutlookProvider(
        _config(tmp_path, body_max_chars=80),
        authenticator=_Auth(),
        transport=transport,
    )

    messages = provider.list_messages()

    assert len(messages) == 1
    message = messages[0]
    assert isinstance(message, EmailMessage)
    assert message.provider == "outlook"
    assert message.message_id == "message-1"
    assert message.thread_id == "conversation-1"
    assert message.sender == "sender@example.com"
    assert message.recipients == ["me@outlook.com"]
    assert message.labels == ["Work"]
    assert message.importance_hint == "high"
    assert message.has_attachments
    assert "定例会議" in message.body_text
    assert "steal" not in message.body_text
    assert "secret" not in message.body_text
    assert len(message.body_text) <= 80
    assert len(message.body_preview or "") == 160
    assert body_to_text("<p>A&amp;B</p>", "html", limit=20) == "A&B"

    single_transport = _Transport([_Response(200, _raw_message("single"))])
    single = OutlookProvider(
        _config(tmp_path),
        authenticator=_Auth(),
        transport=single_transport,
    ).get_message("single")
    assert single.message_id == "single"
    assert single_transport.calls[0]["url"].endswith("/me/messages/single")


def test_list_query_paging_and_maximum(tmp_path) -> None:
    next_link = "https://graph.microsoft.com/v1.0/me/messages?$skiptoken=safe"
    transport = _Transport(
        [
            _Response(
                200,
                {
                    "value": [_raw_message("one")],
                    "@odata.nextLink": next_link,
                },
            ),
            _Response(200, {"value": [_raw_message("two"), _raw_message("three")]}),
        ]
    )
    received = datetime(2026, 8, 1, tzinfo=timezone.utc)
    provider = OutlookProvider(
        _config(
            tmp_path,
            max_messages=2,
            unread_only=True,
            received_after=received,
        ),
        authenticator=_Auth(),
        transport=transport,
    )

    assert [message.message_id for message in provider.list_messages()] == [
        "one",
        "two",
    ]
    assert len(transport.calls) == 2
    first = transport.calls[0]
    assert first["url"].endswith("/me/mailFolders/inbox/messages")
    assert first["params"]["$top"] == 2
    assert "isRead eq false" in first["params"]["$filter"]
    assert "receivedDateTime ge 2026-08-01T00:00:00+00:00" in (
        first["params"]["$filter"]
    )
    select = first["params"]["$select"]
    assert "body" in select
    assert "hasAttachments" in select
    assert "attachments" not in select.split(",")
    assert transport.calls[1]["url"] == next_link
    assert transport.calls[1]["params"] is None
    assert set(transport.calls[0]["headers"]) == {
        "Authorization",
        "Accept",
        "Prefer",
    }


def test_paging_loop_and_unsafe_next_link_are_rejected(tmp_path) -> None:
    loop = "https://graph.microsoft.com/v1.0/me/messages?$skiptoken=loop"
    provider = OutlookProvider(
        _config(tmp_path, max_messages=3),
        authenticator=_Auth(),
        transport=_Transport(
            [
                _Response(200, {"value": [], "@odata.nextLink": loop}),
                _Response(200, {"value": [], "@odata.nextLink": loop}),
            ]
        ),
    )
    with pytest.raises(OutlookProviderError, match="paging loop"):
        provider.list_messages()

    unsafe = OutlookProvider(
        _config(tmp_path),
        authenticator=_Auth(),
        transport=_Transport(
            [
                _Response(
                    200,
                    {"value": [], "@odata.nextLink": "https://evil.example/x"},
                )
            ]
        ),
    )
    with pytest.raises(OutlookProviderError, match="unsafe nextLink"):
        unsafe.list_messages()


@pytest.mark.parametrize(
    ("status", "message"),
    [
        (401, "authentication was rejected"),
        (403, "permission was denied"),
        (404, "not found"),
    ],
)
def test_graph_auth_and_not_found_errors(tmp_path, status, message) -> None:
    provider = OutlookProvider(
        _config(tmp_path, max_retries=0),
        authenticator=_Auth(),
        transport=_Transport([_Response(status, {"token": TOKEN})]),
    )
    with pytest.raises(OutlookProviderError, match=message) as captured:
        provider.list_messages()
    assert TOKEN not in str(captured.value)


def test_graph_retries_429_and_5xx_and_handles_timeout(tmp_path) -> None:
    delays = []
    provider = OutlookProvider(
        _config(tmp_path, max_retries=2),
        authenticator=_Auth(),
        transport=_Transport(
            [
                _Response(429, {}, headers={"Retry-After": "3"}),
                _Response(503, {}),
                _Response(200, {"value": []}),
            ]
        ),
        sleep=delays.append,
    )
    assert provider.list_messages() == []
    assert delays == [3, 2]

    timed_out = OutlookProvider(
        _config(tmp_path, max_retries=0),
        authenticator=_Auth(),
        transport=_Transport([requests.Timeout("private details")]),
        sleep=lambda seconds: None,
    )
    with pytest.raises(OutlookProviderError, match="timed out") as captured:
        timed_out.list_messages()
    assert "private details" not in str(captured.value)


def test_invalid_json_and_bad_message_are_handled_safely(tmp_path) -> None:
    invalid = OutlookProvider(
        _config(tmp_path),
        authenticator=_Auth(),
        transport=_Transport([_Response(200, json_error=True)]),
    )
    with pytest.raises(OutlookProviderError, match="invalid JSON"):
        invalid.list_messages()

    provider = OutlookProvider(
        _config(tmp_path),
        authenticator=_Auth(),
        transport=_Transport(
            [_Response(200, {"value": [{"subject": "private"}, _raw_message()]})]
        ),
    )
    assert len(provider.list_messages()) == 1
    assert provider.conversion_errors == ["message conversion failed: unknown"]


def test_fake_outlook_e2e_is_private_pending_and_explorable(tmp_path) -> None:
    raw = _raw_message()
    full_body = raw["body"]["content"]
    transport = _Transport([_Response(200, {"value": [raw]})])
    provider = OutlookProvider(
        _config(tmp_path, max_messages=1),
        authenticator=_Auth(),
        transport=transport,
    )
    mail_result = MailToCalendarService(base_year=2026).process(provider)
    assert mail_result.processed == 1
    assert len(mail_result.candidates) == 1

    output = tmp_path / "outlook-flow.jsonl"
    result = MailCalendarOrchestrator(base_year=2026).process_provider(
        provider,
        output,
        requires_approval=True,
    )
    serialized = output.read_text(encoding="utf-8")
    assert result.processed_messages == 1
    assert result.calendar_proposals == 1
    assert result.pending_calendar_actions == 1
    assert full_body not in serialized
    assert TOKEN not in serialized
    assert "internet_message_id" not in serialized
    assert "fixture@example.com" not in serialized

    ingestion = read_jsonl(output)
    normalized = normalize_events(ingestion.events)
    bundles = build_action_bundles(normalized)
    calendar_bundle = next(
        bundle for bundle in bundles if bundle.action.actor_id == "calendar_agent"
    )
    assert len(bundles) == 2
    assert outcome_status(calendar_bundle) == "pending"
    assert calendar_bundle.action.raw_event["metadata"][
        "source_message_id"
    ] == "message-1"
    html = render_explorer(
        normalized,
        ingestion=ingestion,
        source_path=output,
    )
    assert "create_calendar_event" in html
    assert "parent_mail_action_id" in html
    assert TOKEN not in html


def test_fetch_failure_leaves_no_partial_orchestrator_output(tmp_path) -> None:
    class _FailedProvider:
        def list_messages(self):
            raise OutlookProviderError("Microsoft Graph request failed (503)")

        def get_message(self, message_id):
            raise AssertionError(message_id)

    output = tmp_path / "partial.jsonl"
    with pytest.raises(OutlookProviderError, match="503"):
        MailCalendarOrchestrator(base_year=2026).process_provider(
            _FailedProvider(),
            output,
        )
    assert not output.exists()


def test_outlook_cli_is_read_only_masked_and_private(
    tmp_path,
    monkeypatch,
    capsys,
) -> None:
    body = "8月12日15時から定例会議です。private full body"
    message = EmailMessage(
        provider="outlook",
        message_id="cli-message",
        thread_id="cli-thread",
        sender="sender@example.com",
        recipients=["me@outlook.com"],
        subject="8月12日 15時 定例会議",
        received_at="2026-08-06T01:00:00Z",
        body_text=body,
        body_preview="定例会議の案内",
        labels=[],
        importance_hint="high",
        has_attachments=False,
        metadata={"graph_private": TOKEN},
    )

    class _CLIProvider:
        authenticated_account = "person@outlook.com"

        def __init__(self, config, *, authenticator):
            del config, authenticator

        def list_messages(self):
            return [message]

        def get_message(self, message_id):
            assert message_id == message.message_id
            return message

    monkeypatch.setattr(
        "mail_calendar_orchestrator.cli.MicrosoftAuthenticator",
        lambda **kwargs: object(),
    )
    monkeypatch.setattr(
        "mail_calendar_orchestrator.cli.OutlookProvider",
        _CLIProvider,
    )
    output = tmp_path / "cli-outlook.jsonl"
    monkeypatch.setattr(
        "sys.argv",
        [
            "mail-calendar-orchestrator",
            "outlook",
            "--client-id",
            "fixture-client-id",
            "--max-messages",
            "1",
            "--output",
            str(output),
            "--base-year",
            "2026",
            "--analysis-mode",
            "rule-only",
            "--requires-approval",
        ],
    )

    main()

    stdout = capsys.readouterr().out
    assert "Mode: read-only Outlook / local calendar proposal" in stdout
    assert "No mailbox or calendar changes will be made." in stdout
    assert "Authenticated account: p***@outlook.com" in stdout
    assert "Fetched messages: 1" in stdout
    assert body not in stdout
    assert TOKEN not in stdout
    serialized = output.read_text(encoding="utf-8")
    assert body not in serialized
    assert TOKEN not in serialized
