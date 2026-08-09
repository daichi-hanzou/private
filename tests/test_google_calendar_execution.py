from __future__ import annotations

import json
import threading
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone

import pytest

from agentledger.bundles import build_action_bundles, outcome_status
from agentledger.html import render_explorer
from agentledger.ingestion import read_jsonl
from agentledger.normalizer import normalize_events
from calendar_agent.agent import CalendarAgent
from calendar_agent.models import CalendarRequest
from calendar_execution.google_auth import (
    GOOGLE_CALENDAR_SCOPE,
    GoogleCalendarAuth,
)
from calendar_execution.google_calendar import (
    GoogleCalendarAPIError,
    GoogleCalendarClient,
    GoogleCalendarExecutor,
    google_event_payload,
)
from calendar_execution.models import (
    CalendarExecutionRequest,
    CalendarExecutionResult,
)
from calendar_execution.service import CalendarExecutionService
from mail_calendar_orchestrator.approvals import ApprovalService
from mail_calendar_orchestrator.audit import write_jsonl_atomic
from mail_calendar_orchestrator.cli import main
from mail_calendar_orchestrator.state import MailStateStore


def approved(tmp_path):
    events = CalendarAgent().propose(CalendarRequest(
        title="Dental appointment", requested_date="2026-08-12",
        preferred_period="afternoon", duration_minutes=60,
        available_slots=["13:00"], requires_approval=True,
    ))
    action = events[-1]
    action["metadata"].update({
        "mail_candidate_id": "candidate-1",
        "source_provider": "outlook",
        "source_message_id": "PRIVATE-MESSAGE-ID",
        "candidate_type": "calendar_event",
        "candidate_date": "2026-08-12",
        "candidate_timezone": "Asia/Tokyo",
        "candidate_location": "Dental clinic",
    })
    source = write_jsonl_atomic(tmp_path / "pending.jsonl", events)
    store = MailStateStore(tmp_path / "state.sqlite3")
    record = ApprovalService(store, output_dir=tmp_path).register_pending(
        events, source,
        now=datetime(2026, 8, 8, 12, tzinfo=timezone.utc),
    )[0]
    return store, ApprovalService(store, output_dir=tmp_path).approve(
        record.approval_id, "daichi", "Approved"
    )


def request() -> CalendarExecutionRequest:
    return CalendarExecutionRequest(
        approval_id="AP-000001", calendar_action_id="action-1",
        candidate_id="candidate-1", title="Dental appointment",
        start="2026-08-12T13:00:00+09:00",
        end="2026-08-12T14:00:00+09:00", duration_minutes=60,
        timezone="Asia/Tokyo", location="Dental clinic",
        description="Created by AgentLedger.\nApproval ID: AP-000001\nSource: Outlook mail",
        source_provider="outlook", source_message_id="PRIVATE-MESSAGE-ID",
        calendar_id="primary",
    )


class FakeExecutor:
    def __init__(self, results=None):
        self.results = list(results or [CalendarExecutionResult(
            success=True, provider="google", calendar_id="primary",
            external_event_id="google-event-1",
            html_link="https://calendar.google.com/event?eid=safe",
            created_at="2026-08-08T12:00:00Z",
            start="2026-08-12T13:00:00+09:00",
            end="2026-08-12T14:00:00+09:00",
        )])
        self.requests = []

    def create_event(self, value):
        self.requests.append(value)
        return self.results.pop(0)


def test_google_payload_datetime_timezone_tracking_and_privacy() -> None:
    payload = google_event_payload(request())
    assert payload["summary"] == "Dental appointment"
    assert payload["start"] == {
        "dateTime": "2026-08-12T13:00:00+09:00", "timeZone": "Asia/Tokyo"
    }
    assert payload["end"]["dateTime"] == "2026-08-12T14:00:00+09:00"
    assert payload["location"] == "Dental clinic"
    private = payload["extendedProperties"]["private"]
    assert private["agentledger_approval_id"] == "AP-000001"
    assert private["agentledger_action_id"] == "action-1"
    assert private["agentledger_candidate_id"] == "candidate-1"
    assert private["agentledger_source_provider"] == "outlook"
    assert private["agentledger_source_message_hash"]
    serialized = json.dumps(payload)
    for forbidden in (
        "PRIVATE-MESSAGE-ID", "sender@example", "access_token",
        "refresh_token", "raw Graph", "LLM reasoning",
    ):
        assert forbidden not in serialized


def test_google_executor_existing_event_is_idempotent() -> None:
    class Client:
        def __init__(self):
            self.inserted = []

        def find_by_approval(self, calendar_id, approval_id):
            assert (calendar_id, approval_id) == ("primary", "AP-000001")
            return {
                "id": "existing", "htmlLink": "https://calendar.google/existing",
                "start": {"dateTime": request().start},
                "end": {"dateTime": request().end},
            }

        def insert(self, calendar_id, payload):
            self.inserted.append((calendar_id, payload))

    client = Client()
    result = GoogleCalendarExecutor(client).create_event(request())
    assert result.success
    assert result.already_exists
    assert result.external_event_id == "existing"
    assert client.inserted == []


def test_success_state_audit_latest_outcome_and_sqlite_idempotency(tmp_path) -> None:
    store, record = approved(tmp_path)
    executor = FakeExecutor()
    try:
        service = CalendarExecutionService(store, executor, output_dir=tmp_path)
        result = service.execute(record.approval_id)
        assert result.success
        assert len(executor.requests) == 1
        execution_request = executor.requests[0]
        assert execution_request.start == "2026-08-12T13:00:00+09:00"
        assert execution_request.end == "2026-08-12T14:00:00+09:00"
        assert execution_request.timezone == "Asia/Tokyo"
        assert execution_request.location == "Dental clinic"
        assert "PRIVATE-MESSAGE-ID" not in execution_request.description
        approval = ApprovalService(store).get(record.approval_id)
        assert approval.status == "calendar_created"
        row = store.connection.execute("SELECT * FROM calendar_execution").fetchone()
        assert row["status"] == "succeeded"
        assert row["attempt_count"] == 1
        assert row["external_event_id"] == "google-event-1"

        second = service.execute(record.approval_id)
        assert second.success and second.already_exists
        assert len(executor.requests) == 1

        ingestion = read_jsonl(row["result_jsonl_path"])
        bundles = build_action_bundles(normalize_events(ingestion.events))
        assert ingestion.events[-1]["actual_outcome"]["outcome_type"] == (
            "calendar_event_created"
        )
        assert len(bundles[0].human_interventions) == 1
        assert outcome_status(bundles[0]) == "confirmed"
        html = render_explorer(
            normalize_events(ingestion.events), ingestion=ingestion,
            source_path=row["result_jsonl_path"],
        )
        assert "calendar_event_created" in html
    finally:
        store.close()


def test_failure_and_retryable_then_success(tmp_path) -> None:
    store, record = approved(tmp_path)
    executor = FakeExecutor([
        CalendarExecutionResult(
            success=False, provider="google", calendar_id="primary",
            error_type="GoogleCalendarAPIError",
            error_message="Google Calendar API error (429)", retryable=True,
        ),
        CalendarExecutionResult(
            success=True, provider="google", calendar_id="primary",
            external_event_id="created-after-retry",
            start="2026-08-12T13:00:00+09:00",
            end="2026-08-12T14:00:00+09:00",
        ),
    ])
    try:
        service = CalendarExecutionService(store, executor, output_dir=tmp_path)
        first = service.execute(record.approval_id)
        assert not first.success and first.retryable
        assert ApprovalService(store).get(record.approval_id).status == "calendar_failed"
        row = store.connection.execute("SELECT * FROM calendar_execution").fetchone()
        assert row["status"] == "retryable"
        failed_ingestion = read_jsonl(row["result_jsonl_path"])
        assert failed_ingestion.events[-1]["actual_outcome"]["outcome_type"] == (
            "calendar_event_creation_failed"
        )
        second = service.execute(record.approval_id)
        assert second.success
        assert ApprovalService(store).get(record.approval_id).status == "calendar_created"
        row = store.connection.execute("SELECT * FROM calendar_execution").fetchone()
        assert row["attempt_count"] == 2
        assert row["status"] == "succeeded"
    finally:
        store.close()


@pytest.mark.parametrize("status", ["awaiting_approval", "rejected", "expired"])
def test_nonapproved_status_cannot_execute(tmp_path, status) -> None:
    store, record = approved(tmp_path)
    try:
        store.connection.execute(
            "UPDATE approval_queue SET status=? WHERE approval_id=?",
            (status, record.approval_id),
        )
        store.connection.commit()
        with pytest.raises(ValueError, match="not executable"):
            CalendarExecutionService(store, FakeExecutor()).execute(record.approval_id)
    finally:
        store.close()


def test_permanent_failure_is_not_retryable(tmp_path) -> None:
    store, record = approved(tmp_path)
    executor = FakeExecutor([CalendarExecutionResult(
        success=False, provider="google", calendar_id="primary",
        error_type="ValidationError", error_message="invalid calendar ID",
        retryable=False,
    )])
    try:
        service = CalendarExecutionService(store, executor, output_dir=tmp_path)
        assert not service.execute(record.approval_id).success
        row = store.connection.execute("SELECT * FROM calendar_execution").fetchone()
        assert row["status"] == "failed"
        with pytest.raises(ValueError, match="not executable"):
            service.execute(record.approval_id)
    finally:
        store.close()


def test_concurrent_execution_calls_executor_once(tmp_path) -> None:
    store, record = approved(tmp_path)
    db = store.path
    store.close()
    entered = threading.Event()
    release = threading.Event()

    class Slow(FakeExecutor):
        def create_event(self, value):
            self.requests.append(value)
            entered.set()
            release.wait(timeout=2)
            return self.results.pop(0)

    executor = Slow()

    def execute():
        with MailStateStore(db) as local:
            return CalendarExecutionService(local, executor, output_dir=tmp_path).execute(
                record.approval_id
            )

    with ThreadPoolExecutor(max_workers=2) as pool:
        first = pool.submit(execute)
        assert entered.wait(timeout=2)
        second = pool.submit(execute)
        with pytest.raises(ValueError, match="not executable"):
            second.result(timeout=2)
        release.set()
        assert first.result(timeout=2).success
    assert len(executor.requests) == 1


@pytest.mark.parametrize(
    "status,reason,retryable",
    [
        (401, "authError", False), (403, "forbidden", False),
        (403, "userRateLimitExceeded", True), (404, "notFound", False),
        (409, "duplicate", False), (429, "rateLimitExceeded", True),
        (500, "backendError", True),
    ],
)
def test_google_error_classification(status, reason, retryable) -> None:
    class Client:
        def find_by_approval(self, *_):
            raise GoogleCalendarAPIError(status, f"safe {status}", reason=reason)

    result = GoogleCalendarExecutor(Client()).create_event(request())
    assert not result.success
    assert result.retryable is retryable
    assert result.error_message == f"safe {status}"


def test_google_client_retries_timeout_and_rate_limit() -> None:
    sleeps = []

    class Execute:
        calls = 0

        def execute(self):
            self.calls += 1
            if self.calls == 1:
                raise GoogleCalendarAPIError(429, "rate limited", retry_after=3)
            return {"items": []}

    operation = Execute()

    class Events:
        def list(self, **kwargs):
            assert kwargs["privateExtendedProperty"] == (
                "agentledger_approval_id=AP-000001"
            )
            return operation

    class Service:
        def events(self):
            return Events()

    client = GoogleCalendarClient(Service(), sleep=sleeps.append)
    assert client.find_by_approval("primary", "AP-000001") is None
    assert sleeps == [3]


def test_google_client_retries_network_timeout_and_parses_quota_reason() -> None:
    sleeps = []

    class Operation:
        calls = 0

        def execute(self):
            self.calls += 1
            if self.calls == 1:
                raise TimeoutError("private network detail")
            return {"items": []}

    operation = Operation()

    class Events:
        def list(self, **_kwargs):
            return operation

    class Service:
        def events(self):
            return Events()

    assert GoogleCalendarClient(Service(), sleep=sleeps.append).find_by_approval(
        "primary", "AP-000001"
    ) is None
    assert sleeps == [1]

    class Response:
        status = 403
        headers = {}

    class QuotaError(Exception):
        resp = Response()
        content = json.dumps({
            "error": {"errors": [{"reason": "userRateLimitExceeded"}]}
        }).encode()

    class QuotaOperation:
        def execute(self):
            raise QuotaError("do not expose")

    class QuotaEvents:
        def list(self, **_kwargs):
            return QuotaOperation()

    class QuotaService:
        def events(self):
            return QuotaEvents()

    with pytest.raises(GoogleCalendarAPIError) as captured:
        GoogleCalendarClient(
            QuotaService(), max_retries=0
        ).find_by_approval("primary", "AP-000001")
    assert captured.value.reason == "userRateLimitExceeded"


def test_malformed_google_response_fails_safely() -> None:
    class Client:
        def find_by_approval(self, *_):
            return None

        def insert(self, *_):
            return {}

    result = GoogleCalendarExecutor(Client()).create_event(request())
    assert not result.success
    assert "missing event ID" in result.error_message


def test_google_auth_missing_refresh_and_private_token(tmp_path, monkeypatch) -> None:
    missing = GoogleCalendarAuth(tmp_path / "missing.json", tmp_path / "token.json")
    with pytest.raises(ValueError, match="not found"):
        missing.credentials()

    credentials_path = tmp_path / "credentials.json"
    credentials_path.write_text("{}", encoding="utf-8")
    token_path = tmp_path / "token.json"
    token_path.write_text("{}", encoding="utf-8")

    class CredentialsValue:
        expired = True
        refresh_token = "SECRET_REFRESH"
        valid = False
        refreshed = False

        def refresh(self, _request):
            self.refreshed = True
            self.expired = False
            self.valid = True

        def to_json(self):
            return '{"stored":"without logging"}'

    value = CredentialsValue()
    from google.oauth2.credentials import Credentials

    monkeypatch.setattr(
        Credentials, "from_authorized_user_file",
        lambda path, scopes: (
            value if scopes == [GOOGLE_CALENDAR_SCOPE] else None
        ),
    )
    auth = GoogleCalendarAuth(credentials_path, token_path)
    assert auth.credentials(interactive=False) is value
    assert value.refreshed
    assert token_path.stat().st_mode & 0o777 == 0o600
    assert credentials_path.stat().st_mode & 0o777 == 0o600


def test_cli_status_and_execute_use_injected_boundaries(
    tmp_path, monkeypatch, capsys
) -> None:
    credentials = tmp_path / "credentials.json"
    token = tmp_path / "token.json"
    credentials.write_text("{}", encoding="utf-8")
    token.write_text("{}", encoding="utf-8")

    calls = []

    class EventList:
        def execute(self):
            calls.append("execute")
            return {"items": []}

    class Events:
        def list(self, **kwargs):
            calls.append(kwargs)
            return EventList()

    class GoogleService:
        def events(self):
            return Events()

        def calendars(self):
            raise AssertionError("calendars.get must not be called")

    class Auth:
        def __init__(self, credentials_path, token_path):
            self.credentials_path = credentials_path
            self.token_path = token_path

        def status(self):
            return {
                "credentials_found": True, "token_found": True,
                "credentials_path": self.credentials_path,
                "token_path": self.token_path,
            }

        def build_service(self, *, interactive):
            calls.append(("interactive", interactive))
            return GoogleService()

    monkeypatch.setattr(
        "mail_calendar_orchestrator.cli.GoogleCalendarAuth", Auth
    )
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "google-calendar",
        "--credentials", str(credentials), "--token-cache", str(token), "status",
    ])
    main()
    output = capsys.readouterr().out
    assert "Credentials file: found" in output
    assert "Authentication: available" in output
    assert "Event access: available" in output
    assert ("interactive", False) in calls
    assert {
        "calendarId": "primary", "maxResults": 1, "singleEvents": True
    } in calls

    calls.clear()
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "google-calendar",
        "--credentials", str(credentials), "--token-cache", str(token), "auth",
    ])
    main()
    output = capsys.readouterr().out
    assert "Google Calendar authentication: available" in output
    assert "Event access: available" in output
    assert ("interactive", True) in calls

    store, record = approved(tmp_path / "approval")
    db = store.path
    store.close()

    class Service:
        def execute(self, approval_id, **kwargs):
            assert approval_id == record.approval_id
            return CalendarExecutionResult(
                success=True, provider="google", calendar_id="primary",
                external_event_id="fake-cli-event",
            )

    monkeypatch.setattr(
        "mail_calendar_orchestrator.cli._calendar_execution_service",
        lambda store, args: Service(),
    )
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "approvals", "--state-db", str(db),
        "--output-dir", str(tmp_path), "execute", record.approval_id,
    ])
    main()
    output = capsys.readouterr().out
    assert "Calendar execution: created" in output
    assert "fake-cli-event" in output


def test_google_event_access_403_is_clear_and_private(
    tmp_path, monkeypatch, capsys
) -> None:
    credentials = tmp_path / "credentials.json"
    token = tmp_path / "token.json"
    credentials.write_text("{}", encoding="utf-8")
    token.write_text("SECRET_TOKEN_VALUE", encoding="utf-8")

    class Response:
        status = 403

    class PermissionError(Exception):
        resp = Response()

    class EventList:
        def execute(self):
            raise PermissionError("insufficientPermissions SECRET_TOKEN_VALUE")

    class Service:
        def events(self):
            return self

        def list(self, **_kwargs):
            return EventList()

    class Auth:
        def __init__(self, *_args):
            pass

        def status(self):
            return {"credentials_found": True, "token_found": True}

        def build_service(self, *, interactive):
            assert not interactive
            return Service()

    monkeypatch.setattr(
        "mail_calendar_orchestrator.cli.GoogleCalendarAuth", Auth
    )
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "google-calendar",
        "--credentials", str(credentials), "--token-cache", str(token), "status",
    ])
    with pytest.raises(SystemExit):
        main()
    output = capsys.readouterr()
    combined = output.out + output.err
    assert "event access was denied (403)" in combined
    assert "calendar.events permission" in combined
    assert "SECRET_TOKEN_VALUE" not in combined
    assert "insufficientPermissions" not in combined
