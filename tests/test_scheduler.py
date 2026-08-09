from __future__ import annotations

import os
import subprocess
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest

from mail_calendar_orchestrator.scheduler import (
    ENV_NAME,
    SERVICE_NAME,
    TIMER_NAME,
    SystemdUserScheduler,
    environment_template,
    run_scheduled_batch,
    scheduled_output_path,
    service_unit,
    timer_unit,
)
from mail_calendar_orchestrator.cli import _parser, main
from mail_calendar_orchestrator.service import MailCalendarOrchestrator
from mail_calendar_orchestrator.state import MailStateStore
from mail_to_calendar.models import EmailMessage


NOW = datetime(2026, 8, 8, 9, 0, tzinfo=timezone.utc)


def message(message_id: str = "scheduled-1") -> EmailMessage:
    return EmailMessage(
        provider="outlook", message_id=message_id, thread_id="thread",
        sender="sender@example.test", recipients=["user@example.test"],
        subject="歯科予約", received_at="2026-08-08T17:00:00+09:00",
        body_text="8月12日 13:00に歯科予約があります",
    )


class FakeProvider:
    def __init__(self, messages: list[EmailMessage]) -> None:
        self.messages = messages

    def list_messages(self) -> list[EmailMessage]:
        return list(self.messages)

    def get_message(self, message_id: str) -> EmailMessage:
        return next(item for item in self.messages if item.message_id == message_id)


def orchestrator(store: MailStateStore) -> MailCalendarOrchestrator:
    return MailCalendarOrchestrator(
        base_year=2026, timezone="Asia/Tokyo", state_store=store,
        analysis_mode="rule-only",
    )


def test_bootstrap_today_timezone_explicit_and_reset(tmp_path) -> None:
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        started = store.ensure_bootstrap(
            timezone_name="Asia/Tokyo", now=NOW
        )
        assert started.isoformat() == "2026-08-08T00:00:00+09:00"
        unchanged = store.ensure_bootstrap(
            timezone_name="Asia/Tokyo",
            start_from="2026-08-01T00:00:00+09:00", now=NOW,
        )
        assert unchanged == started
        reset = store.ensure_bootstrap(
            timezone_name="Asia/Tokyo",
            start_from="2026-08-07T12:00:00+09:00", reset=True,
            now=NOW,
        )
        assert reset.isoformat() == "2026-08-07T12:00:00+09:00"
        assert store.get_system_value("timezone") == "Asia/Tokyo"
        assert store.get_system_value("bootstrap_version") == "1"


def test_cursor_initial_overlap_success_and_boundaries(tmp_path) -> None:
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        start = store.ensure_bootstrap(timezone_name="Asia/Tokyo", now=NOW)
        assert store.cursor()["next_fetch_from"] == start
        successful = datetime(2026, 8, 8, 12, 30, tzinfo=timezone.utc)
        store.mark_successful_poll(successful)
        assert store.cursor(overlap_minutes=5)["next_fetch_from"] == (
            successful - timedelta(minutes=5)
        )
        assert store.cursor(overlap_minutes=0)["next_fetch_from"] == successful
        with pytest.raises(ValueError, match="between 0 and 60"):
            store.cursor(overlap_minutes=61)


def test_scheduled_new_zero_existing_and_timestamped_output(tmp_path) -> None:
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        store.ensure_bootstrap(timezone_name="Asia/Tokyo", now=NOW)
        service = orchestrator(store)
        first = run_scheduled_batch(
            provider=FakeProvider([message()]), orchestrator=service,
            state_store=store, output_dir=tmp_path / "runs", now=NOW,
            run_id="mail-batch-a1b2c3d4",
        )
        assert first.jsonl_path is not None
        assert first.jsonl_path.name == "mail-calendar-20260808-090000-a1b2c3d4.jsonl"
        assert first.jsonl_path.exists()
        assert first.orchestration.approvals_created == 1
        assert store.connection.execute(
            "SELECT COUNT(*) FROM approval_queue"
        ).fetchone()[0] == 1
        second = run_scheduled_batch(
            provider=FakeProvider([message()]), orchestrator=service,
            state_store=store, output_dir=tmp_path / "runs", now=NOW,
            run_id="mail-batch-b1b2c3d4",
        )
        assert second.orchestration.new_messages == 0
        assert second.orchestration.approvals_created == 0
        assert second.jsonl_path is None
        assert not scheduled_output_path(
            tmp_path / "runs", run_id="mail-batch-b1b2c3d4", now=NOW
        ).exists()


def test_mixed_provider_run_is_provider_aware_and_creates_approvals(tmp_path):
    outlook = message("outlook:shared")
    gmail = EmailMessage(
        **{
            **message("gmail:shared").__dict__,
            "provider": "gmail",
            "thread_id": "gmail:thread",
        }
    )
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        store.ensure_bootstrap(timezone_name="Asia/Tokyo", now=NOW)
        result = run_scheduled_batch(
            providers={
                "outlook": FakeProvider([outlook]),
                "gmail": FakeProvider([gmail]),
            },
            orchestrator=orchestrator(store), state_store=store,
            output_dir=tmp_path / "runs", now=NOW,
        )
        assert result.orchestration.new_messages == 2
        assert result.orchestration.approvals_created == 2
        assert result.provider_results == {
            "outlook": {
                "status": "completed", "fetched_messages": 1,
                "error_type": None,
            },
            "gmail": {
                "status": "completed", "fetched_messages": 1,
                "error_type": None,
            },
        }
        rows = store.connection.execute(
            "SELECT provider,message_id FROM processed_messages ORDER BY provider"
        ).fetchall()
        assert [(row["provider"], row["message_id"]) for row in rows] == [
            ("gmail", "gmail:shared"), ("outlook", "outlook:shared")
        ]
        assert store.connection.execute(
            "SELECT provider FROM runs WHERE run_id=?",
            (result.orchestration.run_id,),
        ).fetchone()["provider"] == "mixed"


def test_mixed_provider_failure_isolated_and_recorded(tmp_path):
    class Failure:
        def list_messages(self):
            raise RuntimeError("PRIVATE PROVIDER DETAIL")

        def get_message(self, _message_id):
            raise AssertionError

    with MailStateStore(tmp_path / "state.sqlite3") as store:
        store.ensure_bootstrap(timezone_name="Asia/Tokyo", now=NOW)
        result = run_scheduled_batch(
            providers={
                "outlook": Failure(),
                "gmail": FakeProvider([EmailMessage(**{
                    **message("gmail:ok").__dict__, "provider": "gmail"
                })]),
            },
            orchestrator=orchestrator(store), state_store=store,
            output_dir=tmp_path / "runs", now=NOW,
        )
        assert result.orchestration.processed_messages == 1
        assert result.provider_results["outlook"]["status"] == "failed"
        assert result.provider_results["gmail"]["status"] == "completed"
        provider_row = store.connection.execute(
            "SELECT * FROM provider_runs WHERE run_id=? AND provider='outlook'",
            (result.orchestration.run_id,),
        ).fetchone()
        assert provider_row["status"] == "failed"
        assert provider_row["error_type"] == "RuntimeError"
        assert provider_row["error_message"] == "provider fetch failed"
        assert b"PRIVATE PROVIDER DETAIL" not in (tmp_path / "state.sqlite3").read_bytes()
        run = store.connection.execute(
            "SELECT status FROM runs WHERE run_id=?",
            (result.orchestration.run_id,),
        ).fetchone()
        assert run["status"] == "completed_with_errors"
        assert store.summary()["provider_results"] == [
            {
                "provider": "gmail", "status": "completed",
                "fetched_messages": 1, "error_type": None,
            },
            {
                "provider": "outlook", "status": "failed",
                "fetched_messages": 0, "error_type": "RuntimeError",
            },
        ]
        assert store.cursor(provider="outlook")["last_successful_poll_at"] is None
        assert store.cursor(provider="gmail")["last_successful_poll_at"] == NOW


def test_all_scheduled_providers_failing_records_failed_run(tmp_path):
    class Failure:
        def list_messages(self):
            raise RuntimeError("unavailable")

        def get_message(self, _message_id):
            raise AssertionError

    with MailStateStore(tmp_path / "state.sqlite3") as store:
        store.ensure_bootstrap(timezone_name="Asia/Tokyo", now=NOW)
        with pytest.raises(RuntimeError, match="all scheduled"):
            run_scheduled_batch(
                providers={"outlook": Failure(), "gmail": Failure()},
                orchestrator=orchestrator(store), state_store=store,
                output_dir=tmp_path / "runs", now=NOW, run_id="failed-mixed",
            )
        assert store.connection.execute(
            "SELECT status FROM runs WHERE run_id='failed-mixed'"
        ).fetchone()["status"] == "failed"
        assert store.connection.execute(
            "SELECT COUNT(*) FROM provider_runs WHERE run_id='failed-mixed'"
        ).fetchone()[0] == 2


def test_success_advances_cursor_but_failed_batch_does_not(tmp_path) -> None:
    class Failure:
        def process_provider(self, *_args, **_kwargs):
            raise OSError("output unavailable")

    with MailStateStore(tmp_path / "state.sqlite3") as store:
        store.ensure_bootstrap(timezone_name="Asia/Tokyo", now=NOW)
        with pytest.raises(OSError):
            run_scheduled_batch(
                provider=FakeProvider([message()]), orchestrator=Failure(),
                state_store=store, output_dir=tmp_path, now=NOW,
            )
        assert store.get_system_value("last_successful_poll_at") is None
        successful = run_scheduled_batch(
            provider=FakeProvider([]), orchestrator=orchestrator(store),
            state_store=store, output_dir=tmp_path, now=NOW,
        )
        assert successful.orchestration.new_messages == 0
        assert store.get_system_value("last_successful_poll_at") == NOW.isoformat()


def test_generate_explorer_for_nonempty_run(tmp_path) -> None:
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        store.ensure_bootstrap(timezone_name="Asia/Tokyo", now=NOW)
        result = run_scheduled_batch(
            provider=FakeProvider([message()]), orchestrator=orchestrator(store),
            state_store=store, output_dir=tmp_path / "runs", now=NOW,
            generate_explorer=True,
        )
        assert result.html_path is not None
        assert "AgentLedger" in result.html_path.read_text(encoding="utf-8")


def test_retryable_message_is_reinjected_outside_cursor_window(tmp_path) -> None:
    retry = message("retry-old")

    class Provider(FakeProvider):
        def __init__(self) -> None:
            super().__init__([])

        def get_message(self, message_id: str) -> EmailMessage:
            assert message_id == retry.message_id
            return retry

    with MailStateStore(tmp_path / "state.sqlite3") as store:
        store.ensure_bootstrap(timezone_name="Asia/Tokyo", now=NOW)
        store.mark_processing(
            retry, run_id="old-run", analysis_mode="llm-first", model_name="qwen"
        )
        store.mark_result(retry, status="retryable", error=TimeoutError("temporary"))
        result = run_scheduled_batch(
            provider=Provider(), orchestrator=orchestrator(store),
            state_store=store, output_dir=tmp_path / "runs", now=NOW,
        )
        assert result.orchestration.new_messages == 1
        row = store.connection.execute(
            "SELECT processing_status FROM processed_messages WHERE message_id=?",
            (retry.message_id,),
        ).fetchone()
        assert row["processing_status"] == "processed"


def test_scheduled_lock_rejects_overlap_and_recovers_stale(tmp_path) -> None:
    with MailStateStore(tmp_path / "state.sqlite3") as store:
        assert store.acquire_scheduled_lock("first", now=NOW)
        assert not store.acquire_scheduled_lock("second", now=NOW)
        later = NOW + timedelta(minutes=36)
        assert store.acquire_scheduled_lock("second", now=later)
        store.release_scheduled_lock("first")
        assert not store.acquire_scheduled_lock("third", now=later)
        store.release_scheduled_lock("second")
        assert store.acquire_scheduled_lock("third", now=later)


def test_units_are_user_scoped_private_and_scheduled() -> None:
    service = service_unit(
        working_directory=Path("/srv/agentledger"), uv_path=Path("/usr/bin/uv"),
        env_file=Path("/home/user/.config/agentledger") / ENV_NAME,
    )
    timer = timer_unit()
    env = environment_template()
    assert "Type=oneshot" in service
    assert "TimeoutStartSec=30min" in service
    assert "systemctl" not in service
    assert "/home/daichi" not in service
    for value in ("07:30:00", "12:30:00", "18:30:00", "Persistent=true"):
        assert value in timer
    assert "TOKEN" not in service.upper()
    assert "TOKEN" not in env.upper()
    assert "AGENTLEDGER_MICROSOFT_CLIENT_ID=" in env


def test_scheduler_install_status_and_management_are_fakeable(tmp_path) -> None:
    calls = []

    def runner(command, **kwargs):
        calls.append((command, kwargs))
        return subprocess.CompletedProcess(command, 0, stdout="ActiveState=active\n", stderr="")

    scheduler = SystemdUserScheduler(
        home=tmp_path, working_directory=tmp_path / "repo",
        uv_path=tmp_path / "bin/uv", runner=runner,
    )
    created = scheduler.install(enable=True)
    assert {path.name for path in created} == {SERVICE_NAME, TIMER_NAME, ENV_NAME}
    assert scheduler.env_file.stat().st_mode & 0o777 == 0o600
    assert ["systemctl", "--user", "daemon-reload"] in [item[0] for item in calls]
    assert ["systemctl", "--user", "enable", "--now", TIMER_NAME] in [item[0] for item in calls]
    assert "ActiveState=active" in scheduler.status()
    scheduler.disable()
    scheduler.enable()
    scheduler.run_now()
    scheduler.uninstall()
    commands = [item[0] for item in calls]
    assert ["systemctl", "--user", "disable", "--now", TIMER_NAME] in commands
    assert ["systemctl", "--user", "start", SERVICE_NAME] in commands
    assert not scheduler.service_file.exists()
    assert scheduler.env_file.exists()


def test_environment_file_is_not_overwritten(tmp_path) -> None:
    runner = lambda command, **kwargs: subprocess.CompletedProcess(command, 0, stdout="", stderr="")
    scheduler = SystemdUserScheduler(
        home=tmp_path, working_directory=tmp_path,
        uv_path=tmp_path / "uv", runner=runner,
    )
    scheduler.env_file.parent.mkdir(parents=True)
    scheduler.env_file.write_text("AGENTLEDGER_MICROSOFT_CLIENT_ID=existing\n", encoding="utf-8")
    scheduler.install(enable=False)
    assert scheduler.env_file.read_text(encoding="utf-8") == (
        "AGENTLEDGER_MICROSOFT_CLIENT_ID=existing\n"
    )
    assert scheduler.env_file.stat().st_mode & 0o777 == 0o600


def test_scheduler_cli_commands_and_state_cursor(tmp_path, monkeypatch, capsys) -> None:
    parser = _parser()
    assert parser.parse_args(["run-scheduled"]).gmail_enabled is False
    assert parser.parse_args(
        ["run-scheduled", "--enable-gmail"]
    ).gmail_enabled is True
    for command in ("install", "status", "enable", "disable", "uninstall", "run-now"):
        parsed = parser.parse_args(["scheduler", command])
        assert parsed.scheduler_command == command

    db = tmp_path / "state.sqlite3"
    with MailStateStore(db) as store:
        store.ensure_bootstrap(timezone_name="Asia/Tokyo", now=NOW)
    monkeypatch.setattr("sys.argv", [
        "mail-calendar-orchestrator", "state", "--state-db", str(db), "cursor"
    ])
    main()
    output = capsys.readouterr().out
    assert "Processing started from: 2026-08-08T00:00:00+09:00" in output
    assert "Overlap: 5 minutes" in output
    assert "Next fetch from:" in output
