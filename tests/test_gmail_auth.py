from __future__ import annotations

import base64
from pathlib import Path
from types import SimpleNamespace

import pytest

from calendar_execution.google_auth import DEFAULT_TOKEN as CALENDAR_TOKEN
from mail_calendar_orchestrator.cli import _parser, _print_gmail_debug_body, main
from mail_to_calendar.gmail_auth import (
    DEFAULT_GMAIL_CREDENTIALS,
    DEFAULT_GMAIL_TOKEN,
    GMAIL_READONLY_SCOPE,
    GmailReadOnlyAuth,
)
from mail_to_calendar.gmail_client import (
    GmailProvider,
    GmailProviderConfig,
    GmailReadOnlyClient,
)


def test_gmail_scope_and_token_are_isolated_from_calendar():
    assert GMAIL_READONLY_SCOPE == "https://www.googleapis.com/auth/gmail.readonly"
    assert DEFAULT_GMAIL_CREDENTIALS == Path(
        "~/.config/agentledger/google_credentials.json"
    )
    assert DEFAULT_GMAIL_TOKEN == Path("~/.config/agentledger/gmail_token.json")
    assert DEFAULT_GMAIL_TOKEN != CALENDAR_TOKEN


def test_gmail_auth_missing_refresh_scope_and_private_token(
    tmp_path, monkeypatch
):
    missing = GmailReadOnlyAuth(tmp_path / "missing.json", tmp_path / "gmail.json")
    with pytest.raises(ValueError, match="not found"):
        missing.credentials()

    credentials_path = tmp_path / "google_credentials.json"
    credentials_path.write_text("{}", encoding="utf-8")
    token_path = tmp_path / "gmail_token.json"
    token_path.write_text("{}", encoding="utf-8")

    class CredentialsValue:
        expired = True
        refresh_token = "PRIVATE_REFRESH_TOKEN"
        valid = False

        def refresh(self, _request):
            self.expired = False
            self.valid = True

        def to_json(self):
            return '{"saved":"without logging"}'

    value = CredentialsValue()
    calls = []
    from google.oauth2.credentials import Credentials

    def load(path, scopes):
        calls.append((path, scopes))
        return value

    monkeypatch.setattr(Credentials, "from_authorized_user_file", load)
    auth = GmailReadOnlyAuth(credentials_path, token_path)
    assert auth.credentials(interactive=False) is value
    assert calls == [(str(token_path), [GMAIL_READONLY_SCOPE])]
    assert token_path.stat().st_mode & 0o777 == 0o600
    assert credentials_path.stat().st_mode & 0o777 == 0o600
    assert "PRIVATE_REFRESH_TOKEN" not in token_path.read_text()


def test_gmail_cli_status_and_auth_check_read_only_list(
    tmp_path, monkeypatch, capsys
):
    credentials = tmp_path / "google_credentials.json"
    token = tmp_path / "gmail_token.json"
    credentials.write_text("{}", encoding="utf-8")
    token.write_text("{}", encoding="utf-8")
    calls = []

    class Request:
        def execute(self):
            calls.append("execute")
            return {"messages": []}

    class Messages:
        def list(self, **kwargs):
            calls.append(kwargs)
            return Request()

    class Users:
        def messages(self):
            return Messages()

    class Service:
        def users(self):
            return Users()

    class Auth:
        def __init__(self, credentials_path, token_path):
            assert credentials_path == credentials
            assert token_path == token

        def status(self):
            return {"credentials_found": True, "token_found": True}

        def build_service(self, *, interactive):
            calls.append(("interactive", interactive))
            return Service()

    monkeypatch.setattr("mail_calendar_orchestrator.cli.GmailReadOnlyAuth", Auth)
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "gmail",
        "--credentials", str(credentials), "--token-cache", str(token), "status",
    ])
    main()
    output = capsys.readouterr().out
    assert "Gmail token cache: found" in output
    assert "Authentication: available" in output
    assert "Gmail read-only access: available" in output
    assert ("interactive", False) in calls
    assert {
        "userId": "me", "maxResults": 1, "includeSpamTrash": False
    } in calls

    calls.clear()
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "gmail",
        "--credentials", str(credentials), "--token-cache", str(token), "auth",
    ])
    main()
    output = capsys.readouterr().out
    assert "Gmail authentication: available" in output
    assert "Gmail read-only access: available" in output
    assert ("interactive", True) in calls


def test_gmail_cli_missing_files_does_not_start_auth(monkeypatch, capsys):
    class Auth:
        def __init__(self, *_args):
            pass

        def status(self):
            return {"credentials_found": False, "token_found": False}

        def build_service(self, **_kwargs):
            raise AssertionError("service must not be built")

    monkeypatch.setattr("mail_calendar_orchestrator.cli.GmailReadOnlyAuth", Auth)
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "gmail", "status",
    ])
    main()
    output = capsys.readouterr().out
    assert "Credentials file: missing" in output
    assert "Gmail token cache: missing" in output
    assert "authorization required" in output


def test_gmail_access_403_is_clear_and_private(monkeypatch, capsys):
    secret = "PRIVATE_GMAIL_TOKEN_VALUE"

    class Response:
        status = 403

    class PermissionError(Exception):
        resp = Response()

    class Request:
        def execute(self):
            raise PermissionError(f"insufficientPermissions {secret}")

    class Service:
        def users(self):
            return self

        def messages(self):
            return self

        def list(self, **_kwargs):
            return Request()

    class Auth:
        def __init__(self, *_args):
            pass

        def status(self):
            return {"credentials_found": True, "token_found": True}

        def build_service(self, *, interactive):
            assert not interactive
            return Service()

    monkeypatch.setattr("mail_calendar_orchestrator.cli.GmailReadOnlyAuth", Auth)
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "gmail", "status",
    ])
    with pytest.raises(SystemExit):
        main()
    captured = capsys.readouterr()
    combined = captured.out + captured.err
    assert "Gmail read access was denied (403)" in combined
    assert "gmail.readonly permission" in combined
    assert secret not in combined
    assert "insufficientPermissions" not in combined


def test_gmail_cli_commands_parse_with_separate_token_default():
    parser = _parser()
    for command in ("status", "auth", "list"):
        parsed = parser.parse_args(["gmail", command])
        assert parsed.gmail_command == command
        assert parsed.token_cache == DEFAULT_GMAIL_TOKEN
    assert parser.parse_args(["gmail", "list"]).limit == 5
    assert parser.parse_args(["gmail", "list", "--limit", "2"]).limit == 2
    analyze = parser.parse_args(["gmail", "analyze", "abc123"])
    assert analyze.gmail_command == "analyze"
    assert analyze.message_id == "abc123"
    assert analyze.ollama_model == "qwen3:8b"
    assert analyze.debug_body is False
    assert parser.parse_args(
        ["gmail", "analyze", "abc123", "--debug-body"]
    ).debug_body is True


def test_gmail_debug_body_is_explicit_terminal_only_output(capsys):
    from mail_to_calendar.models import EmailMessage

    message = EmailMessage(
        provider="gmail", message_id="gmail:debug", sender="sender@example.com",
        recipients=[], subject="Safe subject",
        received_at="2026-08-10T00:00:00+09:00",
        body_text="PRIVATE CANONICAL BODY 8月10日",
        metadata={"gmail_mime": {"selected_part_type": "text/html"}},
    )
    _print_gmail_debug_body(message)
    output = capsys.readouterr().out
    assert "DEBUG ONLY" in output
    assert "Selected MIME part type: text/html" in output
    assert f"Canonical body length: {len(message.body_text)}" in output
    assert message.body_text in output
    assert "OAuth" not in output
    assert "raw response" not in output


def test_gmail_read_only_client_lists_metadata_without_body():
    calls = []
    messages = {
        "m1": {
            "id": "m1",
            "internalDate": "1786248000000",
            "snippet": "PRIVATE SNIPPET",
            "payload": {
                "body": {"data": "PRIVATE BODY"},
                "headers": [
                    {"name": "From", "value": "Sender\n <sender@example.com>"},
                    {"name": "Subject", "value": "Meeting\tupdate"},
                    {"name": "To", "value": "private@example.com"},
                ],
            },
        },
    }

    class Request:
        def __init__(self, value):
            self.value = value

        def execute(self):
            return self.value

    class Messages:
        def list(self, **kwargs):
            calls.append(("list", kwargs))
            return Request({"messages": [{"id": "m1", "threadId": "t1"}]})

        def get(self, **kwargs):
            calls.append(("get", kwargs))
            return Request(messages[kwargs["id"]])

    class Service:
        def users(self):
            return self

        def messages(self):
            return Messages()

    values = GmailReadOnlyClient(Service()).list_messages(limit=5)
    assert len(values) == 1
    assert values[0].message_id == "gmail:m1"
    assert values[0].sender == "Sender <sender@example.com>"
    assert values[0].subject == "Meeting update"
    assert values[0].received_at.endswith("+00:00")
    assert calls == [
        ("list", {"userId": "me", "maxResults": 5, "includeSpamTrash": False}),
        ("get", {
            "userId": "me", "id": "m1", "format": "metadata",
            "metadataHeaders": ["From", "Subject"],
        }),
    ]


def test_gmail_list_cli_outputs_only_allowed_metadata(monkeypatch, capsys):
    class Request:
        def __init__(self, value):
            self.value = value

        def execute(self):
            return self.value

    class Messages:
        def list(self, **_kwargs):
            return Request({"messages": [{"id": "abc"}]})

        def get(self, **_kwargs):
            return Request({
                "id": "abc",
                "internalDate": "1786248000000",
                "snippet": "PRIVATE_SNIPPET",
                "payload": {
                    "body": {"data": "PRIVATE_BODY"},
                    "headers": [
                        {"name": "From", "value": "sender@example.com"},
                        {"name": "Subject", "value": "Safe subject"},
                    ],
                },
            })

    class Service:
        def users(self):
            return self

        def messages(self):
            return Messages()

    class Auth:
        def __init__(self, *_args):
            pass

        def build_service(self, *, interactive):
            assert not interactive
            return Service()

    monkeypatch.setattr("mail_calendar_orchestrator.cli.GmailReadOnlyAuth", Auth)
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "gmail", "list", "--limit", "5",
    ])
    main()
    output = capsys.readouterr().out
    assert "gmail:abc" in output
    assert "sender@example.com" in output
    assert "Safe subject" in output
    assert "PRIVATE_SNIPPET" not in output
    assert "PRIVATE_BODY" not in output
    assert "token" not in output.casefold()


def _encoded(value: str) -> str:
    return base64.urlsafe_b64encode(value.encode()).decode().rstrip("=")


def _encoded_charset(value: str, charset: str) -> str:
    return base64.urlsafe_b64encode(value.encode(charset)).decode().rstrip("=")


@pytest.mark.parametrize(
    "declared,codec,expected_charset,mime_type",
    [
        ("utf-8", "utf-8", "utf-8", "text/plain"),
        ("iso-2022-jp", "iso-2022-jp", "iso2022_jp", "text/plain"),
        ("shift_jis", "shift_jis", "shift_jis", "text/plain"),
        ("cp932", "cp932", "cp932", "text/plain"),
        ("euc-jp", "euc-jp", "euc_jp", "text/html"),
    ],
)
def test_gmail_mime_charset_decodes_japanese_without_replacement(
    declared, codec, expected_charset, mime_type
):
    body = "8月10日 17:00から18:00まで、02会議室で打ち合わせです。"
    encoded_body = f"<p>{body}</p>" if mime_type == "text/html" else body
    message = GmailReadOnlyClient._message({
        "id": "charset-test",
        "internalDate": "1786248000000",
        "payload": {
            "mimeType": mime_type,
            "headers": [{
                "name": "Content-Type",
                "value": f"{mime_type}; charset={declared}",
            }],
            "body": {"data": _encoded_charset(encoded_body, codec)},
        },
    }, expected_id="charset-test")
    assert message.body_text == body
    assert "�" not in message.body_text
    assert GmailReadOnlyClient._contains_date(message.body_text)
    mime = message.metadata["gmail_mime"]
    assert mime["selected_charset"] == expected_charset
    assert mime["charset_source"] == "MIME"
    assert mime["decode_errors"] is False


def test_gmail_missing_charset_uses_strict_limited_fallback():
    body = "8月10日 17:00から18:00"
    message = GmailReadOnlyClient._message({
        "id": "fallback-charset",
        "internalDate": "1786248000000",
        "payload": {
            "mimeType": "text/plain",
            "headers": [{"name": "Content-Type", "value": "text/plain"}],
            "body": {"data": _encoded_charset(body, "iso-2022-jp")},
        },
    }, expected_id="fallback-charset")
    assert message.body_text == body
    assert message.metadata["gmail_mime"]["selected_charset"] == "iso2022_jp"
    assert message.metadata["gmail_mime"]["charset_source"] == "fallback"
    assert message.metadata["gmail_mime"]["decode_errors"] is False


def test_gmail_declared_charset_error_tries_fallback_before_replacement():
    body = "8月10日 17:00から18:00"
    message = GmailReadOnlyClient._message({
        "id": "wrong-declared-charset",
        "internalDate": "1786248000000",
        "payload": {
            "mimeType": "text/plain",
            "headers": [{
                "name": "Content-Type", "value": "text/plain; charset=utf-8"
            }],
            "body": {"data": _encoded_charset(body, "cp932")},
        },
    }, expected_id="wrong-declared-charset")
    assert message.body_text == body
    assert "�" not in message.body_text
    assert message.metadata["gmail_mime"]["selected_charset"] == "cp932"
    assert message.metadata["gmail_mime"]["charset_source"] == "fallback"
    assert message.metadata["gmail_mime"]["decode_errors"] is True


@pytest.mark.parametrize(
    "expression",
    ["8月10日", "8月 10日", "8月　10日", "８月１０日", "8 月 10 日"],
)
def test_gmail_date_detection_normalizes_unicode_and_date_whitespace(expression):
    assert GmailReadOnlyClient._contains_date(
        f"予定は{expression} 17:00からです"
    )


def test_gmail_get_message_prefers_plain_and_skips_mixed_attachments():
    calls = []
    value = {
        "id": "abc123",
        "threadId": "thread1",
        "internalDate": "1786248000000",
        "labelIds": ["INBOX", "IMPORTANT"],
        "payload": {
            "mimeType": "multipart/mixed",
            "headers": [
                {"name": "From", "value": "sender@example.com"},
                {"name": "To", "value": "one@example.com, Two <two@example.com>"},
                {"name": "Subject", "value": "Private meeting"},
            ],
            "parts": [
                {
                    "mimeType": "multipart/alternative",
                    "parts": [
                        {
                            "mimeType": "text/html",
                            "body": {"data": _encoded("<p>HTML SECRET</p>")},
                        },
                        {
                            "mimeType": "text/plain",
                            "headers": [{"name": "Content-Type", "value": "text/plain; charset=utf-8"}],
                            "body": {"data": _encoded("8月10日15時から会議です")},
                        },
                    ],
                },
                {
                    "mimeType": "text/plain",
                    "filename": "private.txt",
                    "headers": [{"name": "Content-Disposition", "value": "attachment"}],
                    "body": {"data": _encoded("ATTACHMENT SECRET")},
                },
            ],
        },
    }

    class Request:
        def execute(self):
            return value

    class Service:
        def users(self):
            return self

        def messages(self):
            return self

        def get(self, **kwargs):
            calls.append(kwargs)
            return Request()

    message = GmailReadOnlyClient(Service()).get_message("abc123")
    assert calls == [{"userId": "me", "id": "abc123", "format": "full"}]
    assert message.provider == "gmail"
    assert message.message_id == "gmail:abc123"
    assert message.thread_id == "gmail:thread1"
    assert message.body_text == "8月10日15時から会議です"
    assert "HTML SECRET" not in message.body_text
    assert "ATTACHMENT SECRET" not in message.body_text
    assert message.recipients == ["one@example.com", "two@example.com"]
    assert message.has_attachments
    assert message.importance_hint == "important"
    mime = message.metadata["gmail_mime"]
    assert mime["mime_type"] == "multipart/mixed"
    assert mime["plain_text_parts"] == 1
    assert mime["html_parts"] == 1
    assert mime["plain_parts_with_data"] == 1
    assert mime["html_parts_with_data"] == 1
    assert mime["attachment_id_only_parts"] == 0
    assert mime["selected_part_type"] == "text/plain"
    assert mime["selected_body_length"] == len(message.body_text)
    assert mime["plain_contains_date"] is True
    assert mime["html_contains_date"] is False
    assert mime["plain_contains_time"] is True
    assert mime["html_contains_time"] is False
    assert mime["selection_reason"] == "plain_preferred_valid_alternative"


def test_gmail_get_message_html_fallback_removes_markup_and_hidden_text():
    value = {
        "id": "html1",
        "internalDate": "1786248000000",
        "payload": {
            "mimeType": "text/html",
            "headers": [{"name": "Subject", "value": "HTML mail"}],
            "body": {
                "data": _encoded(
                    "<style>PRIVATE STYLE</style><p>Meeting &amp; appointment</p>"
                    "<script>PRIVATE SCRIPT</script><div>8月10日15時</div>"
                )
            },
        },
    }

    class Service:
        def users(self):
            return self

        def messages(self):
            return self

        def get(self, **_kwargs):
            return SimpleNamespace(execute=lambda: value)

    message = GmailReadOnlyClient(Service()).get_message("html1")
    assert "Meeting & appointment" in message.body_text
    assert "8月10日15時" in message.body_text
    assert "PRIVATE STYLE" not in message.body_text
    assert "PRIVATE SCRIPT" not in message.body_text
    assert "<p>" not in message.body_text


def test_alternative_falls_back_to_html_when_plain_is_effectively_empty():
    value = {
        "id": "empty-plain",
        "internalDate": "1786248000000",
        "payload": {
            "mimeType": "multipart/alternative",
            "headers": [],
            "parts": [
                {
                    "mimeType": "text/plain",
                    "body": {"data": _encoded(" \n\t　")},
                },
                {
                    "mimeType": "text/html",
                    "body": {"data": _encoded("<p>Visible mail body</p>")},
                },
            ],
        },
    }

    class Service:
        def users(self):
            return self

        def messages(self):
            return self

        def get(self, **_kwargs):
            return SimpleNamespace(execute=lambda: value)

    message = GmailReadOnlyClient(Service()).get_message("empty-plain")
    assert message.body_text == "Visible mail body"
    assert message.metadata["gmail_mime"]["selected_part_type"] == "text/html"
    assert message.metadata["gmail_mime"]["selection_reason"] == (
        "html_fallback_plain_empty"
    )


def test_outlook_to_gmail_nested_alternative_selects_substantive_html():
    actual_body = (
        "8月10日 17:00から18:00まで、02会議室で"
        "LLMの打ち合わせに参加します。"
    )
    value = {
        "id": "outlook-mail",
        "internalDate": "1786248000000",
        "payload": {
            "mimeType": "multipart/mixed",
            "headers": [{"name": "Subject", "value": "LLM meeting"}],
            "parts": [
                {
                    "mimeType": "multipart/related",
                    "parts": [{
                        "mimeType": "multipart/alternative",
                        "parts": [
                            {
                                "mimeType": "text/plain",
                                "body": {"data": _encoded("このメールは自動送信です")},
                            },
                            {
                                "mimeType": "text/html",
                                "body": {"data": _encoded(
                                    f"<html><body><p>{actual_body}</p></body></html>"
                                )},
                            },
                        ],
                    }],
                },
                {
                    "mimeType": "application/octet-stream",
                    "body": {"attachmentId": "do-not-fetch"},
                },
            ],
        },
    }

    class Service:
        def users(self):
            return self

        def messages(self):
            return self

        def get(self, **_kwargs):
            return SimpleNamespace(execute=lambda: value)

    message = GmailReadOnlyClient(Service()).get_message("outlook-mail")
    assert message.body_text == actual_body
    assert "自動送信" not in message.body_text
    assert "do-not-fetch" not in message.body_text
    assert message.has_attachments
    mime = message.metadata["gmail_mime"]
    assert mime["mime_type"] == "multipart/mixed"
    assert mime["plain_text_parts"] == 1
    assert mime["html_parts"] == 1
    assert mime["plain_parts_with_data"] == 1
    assert mime["html_parts_with_data"] == 1
    assert mime["attachment_id_only_parts"] == 1
    assert mime["selected_part_type"] == "text/html"
    assert mime["selected_body_length"] == len(actual_body)
    assert mime["plain_contains_date"] is False
    assert mime["html_contains_date"] is True
    assert mime["plain_contains_time"] is False
    assert mime["html_contains_time"] is True
    assert mime["selection_reason"] == "html_fallback_plain_footer_only"


def test_alternative_keeps_valid_plain_when_html_has_calendar_only_details():
    plain = (
        "会議に関するご案内です。17:00から18:00まで実施します。"
        "詳細をご確認のうえ参加してください。補足情報があります。"
    )
    html_text = (
        "8月10日 17:00から18:00まで、02会議室で"
        "LLMの打ち合わせに参加します。"
    )
    value = {
        "id": "missing-date-in-plain",
        "internalDate": "1786248000000",
        "payload": {
            "mimeType": "multipart/alternative",
            "headers": [{"name": "Subject", "value": "LLM meeting"}],
            "parts": [
                {
                    "mimeType": "text/plain",
                    "body": {"data": _encoded(plain)},
                },
                {
                    "mimeType": "text/html",
                    "body": {"data": _encoded(f"<p>{html_text}</p>")},
                },
            ],
        },
    }

    class Service:
        def users(self):
            return self

        def messages(self):
            return self

        def get(self, **_kwargs):
            return SimpleNamespace(execute=lambda: value)

    message = GmailReadOnlyClient(Service()).get_message(
        "missing-date-in-plain"
    )
    assert message.body_text == plain
    mime = message.metadata["gmail_mime"]
    assert mime["plain_visible_length"] == len(plain)
    assert mime["html_visible_length"] == len(html_text)
    assert mime["plain_contains_date"] is False
    assert mime["html_contains_date"] is True
    assert mime["plain_contains_time"] is True
    assert mime["html_contains_time"] is True
    assert mime["selected_part_type"] == "text/plain"
    assert mime["selection_reason"] == "plain_preferred_valid_alternative"


def test_alternative_keeps_plain_when_html_increment_is_footer_and_link_only():
    plain = "8月10日17:00から会議に参加します。"
    value = {
        "id": "footer-only-html-growth",
        "internalDate": "1786248000000",
        "payload": {
            "mimeType": "multipart/alternative",
            "headers": [],
            "parts": [
                {"mimeType": "text/plain", "body": {"data": _encoded(plain)}},
                {
                    "mimeType": "text/html",
                    "body": {"data": _encoded(
                        f"<p>{plain}</p><p>配信停止 https://example.test/unsubscribe</p>"
                    )},
                },
            ],
        },
    }

    class Service:
        def users(self):
            return self

        def messages(self):
            return self

        def get(self, **_kwargs):
            return SimpleNamespace(execute=lambda: value)

    message = GmailReadOnlyClient(Service()).get_message(
        "footer-only-html-growth"
    )
    assert message.body_text == plain
    assert message.metadata["gmail_mime"]["selected_part_type"] == "text/plain"
    assert message.metadata["gmail_mime"]["selection_reason"] == (
        "plain_preferred_valid_alternative"
    )


def test_gmail_get_message_rejects_prefixed_id_without_api_call():
    with pytest.raises(ValueError, match="must not include"):
        GmailReadOnlyClient(object()).get_message("gmail:abc123")


def test_gmail_analyze_cli_is_analysis_only_and_does_not_print_body_or_raw(
    monkeypatch, capsys
):
    private_body = "PRIVATE FULL EMAIL BODY 8月10日15時から16時"
    raw_response = "PRIVATE RAW LLM RESPONSE"
    captured = {}

    class Auth:
        def __init__(self, *_args):
            pass

        def build_service(self, *, interactive):
            assert not interactive
            return object()

    class Client:
        def __init__(self, _service):
            pass

        def get_message(self, message_id):
            assert message_id == "abc123"
            from mail_to_calendar.models import EmailMessage
            return EmailMessage(
                provider="gmail", message_id="gmail:abc123",
                sender="sender@example.com", recipients=[], subject="Meeting",
                received_at="2026-08-09T00:00:00+00:00",
                body_text=private_body,
            )

    class Analyzer:
        def __init__(self, _classifier, **kwargs):
            captured.update(kwargs)
        def date_grounding_debug(self, *_args):
            return {
                    "received_at": "2026-08-09T15:14:13+00:00",
                    "timezone": "Asia/Tokyo",
                    "local_received_at": "2026-08-10T00:14:13+09:00",
                    "proposed_date": "2026-08-10",
                    "expressions": [{
                        "expression": "明日", "resolved_date": "2026-08-11"
                    }],
                    "grounded": False,
                    "reason": "relative_date_mismatch",
                    "date_like_tokens": ["明日の", "月曜日"],
                    "llm_body_length": 184,
                    "validator_body_length": 184,
                    "llm_body_hash": "a" * 64,
                    "validator_body_hash": "a" * 64,
                    "same_body": True,
                    "groundable_fields": [
                        "subject", "body_text", "received_at"
                    ],
                    "subject_has_date_like_expression": True,
                    "subject_expressions": [{
                        "expression": "8月10日",
                        "resolved_date": "2026-08-10",
                    }],
                    "contains_explicit_japanese_date": True,
                    "contains_time_expression": True,
                    "nfkc_date_detection": True,
                    "whitespace_normalized_date_detection": True,
                }

        def analyze(self, message, _importance, _candidate):
            captured["message"] = message
            candidate = SimpleNamespace(
                candidate_type="event", title="Safe title", date="2026-08-10",
                start="15:00", end="16:00", duration_minutes=60,
                timezone="Asia/Tokyo", location=None,
            )
            return SimpleNamespace(
                final_classification="calendar_candidate",
                final_candidate=candidate,
                confidence=0.95,
                validation_issues=[],
                raw_response=raw_response,
                llm_result_summary={"date": "2026-08-10"},
            )

    monkeypatch.setattr("mail_calendar_orchestrator.cli.GmailReadOnlyAuth", Auth)
    monkeypatch.setattr("mail_calendar_orchestrator.cli.GmailReadOnlyClient", Client)
    monkeypatch.setattr("mail_calendar_orchestrator.cli.HybridMailAnalyzer", Analyzer)
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "gmail", "analyze", "abc123",
        "--base-year", "2026", "--debug-grounding",
    ])
    main()
    output = capsys.readouterr().out
    assert captured["mode"] == "llm-first"
    assert captured["require_llm"] is True
    assert captured["message"].body_text == private_body
    assert "Message ID: gmail:abc123" in output
    assert "Final classification: calendar_candidate" in output
    assert "Candidate allowed: yes" in output
    assert "Safe title" in output
    assert private_body not in output
    assert raw_response not in output
    assert "Local received at: 2026-08-10T00:14:13+09:00" in output
    assert "Detected date expression: 明日" in output
    assert "Resolved date: 2026-08-11" in output
    assert "Grounded: no" in output
    assert "Reason: relative_date_mismatch" in output
    assert "Date-like tokens:" in output
    assert '- "明日の"' in output
    assert '- "月曜日"' in output
    assert "LLM body length: 184" in output
    assert "Validator body length: 184" in output
    assert f"LLM body hash: {'a' * 64}" in output
    assert f"Validator body hash: {'a' * 64}" in output
    assert "Same body: yes" in output
    assert "Groundable fields used by LLM:" in output
    assert "- subject" in output
    assert "- body_text" in output
    assert "- received_at" in output
    assert "Subject has date-like expression: yes" in output
    assert "Subject date expression: 8月10日" in output
    assert "Subject resolved date: 2026-08-10" in output
    assert "MIME type: unknown" in output
    assert "Plain text parts: 0" in output
    assert "HTML parts: 0" in output
    assert "Selected part type: none" in output
    assert "Selected body length: 0" in output
    assert "Selected charset: none" in output
    assert "Charset source: unavailable" in output
    assert "Decode errors: no" in output
    assert "Plain contains date: no" in output
    assert "HTML contains date: no" in output
    assert "Plain contains time: no" in output
    assert "HTML contains time: no" in output
    assert "Plain visible length: 0" in output
    assert "HTML visible length: 0" in output
    assert "Selection reason: unavailable" in output
    assert "Contains explicit Japanese date: yes" in output
    assert "Contains time expression: yes" in output
    assert 'Plain:' in output
    assert '- Contains "8月": no' in output
    assert 'HTML text:' in output
    assert "Canonical NFKC date detection: yes" in output
    assert "Canonical whitespace-normalized date detection: yes" in output


def test_gmail_scheduled_provider_filters_inbox_after_cursor_and_normalizes():
    calls = []
    full = {
        "id": "new1", "threadId": "t1", "internalDate": "1786248000000",
        "payload": {
            "mimeType": "text/plain",
            "headers": [
                {"name": "From", "value": "sender@example.com"},
                {"name": "Subject", "value": "Meeting"},
            ],
            "body": {"data": _encoded("8月10日15時から会議")},
        },
    }

    class Request:
        def __init__(self, value):
            self.value = value

        def execute(self):
            return self.value

    class Service:
        def users(self):
            return self

        def messages(self):
            return self

        def list(self, **kwargs):
            calls.append(("list", kwargs))
            return Request({"messages": [{"id": "new1"}]})

        def get(self, **kwargs):
            calls.append(("get", kwargs))
            return Request(full)

    from datetime import datetime, timezone
    after = datetime(2026, 8, 8, 0, 0, tzinfo=timezone.utc)
    values = GmailProvider(
        Service(), GmailProviderConfig(max_messages=5, received_after=after)
    ).list_messages()
    assert [item.message_id for item in values] == ["gmail:new1"]
    assert calls[0] == ("list", {
        "userId": "me", "maxResults": 5, "includeSpamTrash": False,
        "labelIds": ["INBOX"], "q": f"after:{int(after.timestamp())}",
    })
    assert calls[1] == (
        "get", {"userId": "me", "id": "new1", "format": "full"}
    )
