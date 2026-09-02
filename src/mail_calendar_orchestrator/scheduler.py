from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import time
from dataclasses import dataclass, fields
from datetime import datetime
from pathlib import Path
from typing import Any, Callable
from uuid import uuid4

from agentledger.html import write_explorer
from agentledger.ingestion import read_jsonl
from agentledger.normalizer import normalize_events
from mail_to_calendar.provider import MailProvider

from .models import OrchestrationResult
from .service import MailCalendarOrchestrator
from .state import MailStateStore


DEFAULT_OUTPUT_DIR = Path("~/.local/share/agentledger/runs")
SERVICE_NAME = "agentledger-mail.service"
TIMER_NAME = "agentledger-mail.timer"
ENV_NAME = "mail-calendar.env"


@dataclass(frozen=True)
class ScheduledResult:
    orchestration: OrchestrationResult
    jsonl_path: Path | None
    html_path: Path | None
    fetch_from: datetime
    provider_results: dict[str, dict[str, Any]] | None = None
    batches_processed: int = 1
    backlog_remaining: int | None = 0
    elapsed_seconds: float = 0.0
    average_batch_seconds: float = 0.0
    stop_reason: str = "backlog_empty"
    jsonl_paths: tuple[Path, ...] = ()
    more_available: bool = False


class _ScheduledProvider:
    """Merge state retries with cursor-based reads without cursor deduping."""

    def __init__(
        self, provider: MailProvider, state_store: MailStateStore,
        *, max_messages: int, provider_name: str = "outlook",
    ) -> None:
        self.provider = provider
        self.state_store = state_store
        self.config = getattr(provider, "config", None)
        self.max_messages = max_messages
        self.provider_name = provider_name
        self.retry_ids = state_store.retryable_message_ids(
            provider=provider_name, limit=max_messages
        )
        self._messages: list[Any] | None = None
        self.provider_messages: list[Any] = []
        self.provider_at_capacity = False

    def list_messages(self) -> list[Any]:
        if self._messages is not None:
            return list(self._messages)
        values = []
        seen = set()
        for message_id in self.retry_ids:
            message = self.provider.get_message(message_id)
            key = (message.provider, message.message_id)
            if key not in seen:
                values.append(message)
                seen.add(key)
        self.provider_messages = self.provider.list_messages()
        configured_limit = getattr(self.config, "max_messages", None)
        self.provider_at_capacity = bool(
            configured_limit
            and len(self.provider_messages) >= int(configured_limit)
        )
        for message in self.provider_messages:
            key = (message.provider, message.message_id)
            if key not in seen:
                values.append(message)
                seen.add(key)
        eligible = [
            message for message in values
            if self.state_store.decision(message).process
        ]
        eligible.sort(key=lambda item: item.received_at)
        self._messages = eligible[: self.max_messages]
        return list(self._messages)

    def get_message(self, message_id: str) -> Any:
        return self.provider.get_message(message_id)


def scheduled_output_path(
    output_dir: str | Path, *, run_id: str, now: datetime
) -> Path:
    directory = Path(output_dir).expanduser()
    stamp = now.strftime("%Y%m%d-%H%M%S")
    suffix = run_id.removeprefix("mail-batch-")[:8]
    return directory / f"mail-calendar-{stamp}-{suffix}.jsonl"


def _run_scheduled_batch_once(
    *, provider: MailProvider | None = None,
    providers: dict[str, MailProvider] | None = None,
    orchestrator: MailCalendarOrchestrator,
    state_store: MailStateStore, output_dir: str | Path,
    overlap_minutes: int = 5, generate_explorer: bool = False,
    now: datetime, run_id: str | None = None, batch_limit: int = 10,
) -> ScheduledResult:
    if not 1 <= batch_limit <= 100:
        raise ValueError("scheduled batch limit must be between 1 and 100")
    run_id = run_id or f"mail-batch-{uuid4()}"
    sources = providers or ({"outlook": provider} if provider is not None else {})
    if not sources or any(value is None for value in sources.values()):
        raise ValueError("at least one scheduled mail provider is required")
    if batch_limit < len(sources):
        raise ValueError(
            "scheduled batch limit must be at least the provider count"
        )
    cursors = {
        name: state_store.cursor(
            overlap_minutes=overlap_minutes, provider=name
        )
        for name in sources
    }
    provider_results: dict[str, dict[str, Any]] = {}
    provider_failures: dict[str, Exception] = {}
    available_by_provider: dict[str, list[Any]] = {}
    raw_by_provider: dict[str, list[Any]] = {}
    provider_at_capacity: dict[str, bool] = {}
    fetched_by_provider: dict[str, list[Any]] = {}
    combined: list[Any] = []
    for name, source in sources.items():
        configured_limit = getattr(
            getattr(source, "config", None), "max_messages", 50
        )
        scheduled = _ScheduledProvider(
            source, state_store,
            max_messages=min(configured_limit, batch_limit),
            provider_name=name,
        )
        try:
            values = scheduled.list_messages()
        except Exception as exc:
            provider_failures[name] = exc
            provider_results[name] = {
                "status": "failed", "fetched_messages": 0,
                "error_type": type(exc).__name__,
            }
            available_by_provider[name] = []
            raw_by_provider[name] = []
            provider_at_capacity[name] = False
            continue
        available_by_provider[name] = values
        raw_by_provider[name] = scheduled.provider_messages
        provider_at_capacity[name] = scheduled.provider_at_capacity
        provider_results[name] = {
            "status": "available", "fetched_messages": 0,
            "error_type": None,
        }
    # Fill one bounded batch fairly. If one provider is empty, the other
    # provider can consume the unused capacity.
    offsets = {name: 0 for name in sources}
    while len(combined) < batch_limit:
        added = False
        for name in sources:
            values = available_by_provider.get(name, [])
            offset = offsets[name]
            if offset >= len(values):
                continue
            message = values[offset]
            offsets[name] += 1
            fetched_by_provider.setdefault(name, []).append(message)
            combined.append(message)
            added = True
            if len(combined) >= batch_limit:
                break
        if not added:
            break
    for name in sources:
        fetched_by_provider.setdefault(name, [])
        if provider_results[name]["status"] != "failed":
            provider_results[name]["fetched_messages"] = len(
                fetched_by_provider[name]
            )
    if provider_results and all(
        item["status"] == "failed" for item in provider_results.values()
    ):
        state_store.start_run(
            run_id,
            provider="mixed" if len(sources) > 1 else next(iter(sources)),
            analysis_mode=orchestrator.analysis_mode,
            model_name=orchestrator.model_name,
            fetched=0, new=0, skipped=0,
        )
        for name, error in provider_failures.items():
            state_store.record_provider_run(
                run_id, provider=name, status="failed", error=error
            )
        state_store.finish_run(run_id, status="failed")
        raise RuntimeError("all scheduled mail providers failed")
    scheduled_provider = _StaticScheduledProvider(combined)
    output = scheduled_output_path(output_dir, run_id=run_id, now=now)
    output.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    try:
        os.chmod(output.parent, 0o700)
    except OSError:
        pass
    result = orchestrator.process_provider(
        scheduled_provider, output,
        requires_approval=True, analysis_only=False,
        run_id=run_id,
        run_provider="mixed" if len(sources) > 1 else next(iter(sources)),
    )
    html_path = None
    jsonl_path = output if result.new_messages else None
    if jsonl_path is not None and generate_explorer:
        ingestion = read_jsonl(jsonl_path)
        normalized = normalize_events(ingestion.events)
        html_path = jsonl_path.with_suffix(".html")
        write_explorer(
            html_path, normalized, ingestion=ingestion,
            source_path=jsonl_path,
        )
    for name, values in fetched_by_provider.items():
        status = provider_results[name]
        if status["status"] == "failed":
            state_store.record_provider_run(
                run_id, provider=name, status="failed",
                error=provider_failures[name],
            )
            continue
        poll_completed = now
        provider_has_more = (
            len(available_by_provider.get(name, [])) > len(values)
            or len(available_by_provider.get(name, [])) >= batch_limit
            or provider_at_capacity.get(name, False)
        )
        cursor_values = values or raw_by_provider.get(name, [])
        if provider_has_more and cursor_values:
            received = []
            for message in cursor_values:
                try:
                    received.append(datetime.fromisoformat(message.received_at))
                except ValueError:
                    continue
            if received:
                poll_completed = max(received)
        state_store.mark_successful_poll(poll_completed, provider=name)
        state_store.record_provider_run(
            run_id, provider=name, status="completed",
            fetched_messages=len(values),
        )
        provider_results[name]["status"] = "completed"
    if provider_failures:
        state_store.finish_run(run_id, status="completed_with_errors")
    primary_cursor = cursors.get("outlook") or next(iter(cursors.values()))
    return ScheduledResult(
        result, jsonl_path, html_path, primary_cursor["next_fetch_from"],
        provider_results, jsonl_paths=((jsonl_path,) if jsonl_path else ()),
        more_available=any(
            provider_at_capacity.get(name, False)
            or len(available_by_provider.get(name, []))
            > len(fetched_by_provider.get(name, []))
            for name in sources
        ),
    )


_COUNT_FIELDS = {
    field.name for field in fields(OrchestrationResult)
    if field.name not in {
        "output_path", "events", "analysis_only", "run_id", "state_db"
    }
}


def _aggregate_orchestration(
    results: list[OrchestrationResult], *, run_id: str
) -> OrchestrationResult:
    last = results[-1]
    values: dict[str, Any] = {}
    for field in fields(OrchestrationResult):
        name = field.name
        if name in _COUNT_FIELDS:
            values[name] = sum(int(getattr(item, name)) for item in results)
        elif name == "events":
            values[name] = [event for item in results for event in item.events]
        elif name == "run_id":
            values[name] = run_id
        else:
            values[name] = getattr(last, name)
    return OrchestrationResult(**values)


def run_scheduled_batch(
    *, provider: MailProvider | None = None,
    providers: dict[str, MailProvider] | None = None,
    provider_factories: dict[str, Callable[[], MailProvider]] | None = None,
    orchestrator: MailCalendarOrchestrator,
    state_store: MailStateStore, output_dir: str | Path,
    overlap_minutes: int = 5, generate_explorer: bool = False,
    now: datetime, run_id: str | None = None, batch_limit: int = 10,
    time_budget_minutes: float = 55,
    monotonic: Callable[[], float] = time.monotonic,
    after_batch: Callable[[ScheduledResult], None] | None = None,
) -> ScheduledResult:
    """Drain scheduled mail in bounded batches within a soft time budget."""
    if not 1 <= batch_limit <= 100:
        raise ValueError("scheduled batch size must be between 1 and 100")
    if time_budget_minutes <= 0:
        raise ValueError("scheduled time budget must be positive")
    parent_run_id = run_id or f"mail-batch-{uuid4()}"
    if not state_store.acquire_scheduled_lock(parent_run_id, now=now):
        raise RuntimeError("another scheduled mail batch is already running")
    started = monotonic()
    batches: list[ScheduledResult] = []
    stop_reason = "backlog_empty"
    try:
        state_store.recover_stale_processing(now=now)
        while True:
            current_providers = (
                {name: factory() for name, factory in provider_factories.items()}
                if provider_factories is not None
                else providers
            )
            child_run_id = (
                parent_run_id if not batches
                else f"mail-batch-{uuid4()}"
            )
            batch = _run_scheduled_batch_once(
                provider=provider if current_providers is None else None,
                providers=current_providers,
                orchestrator=orchestrator, state_store=state_store,
                output_dir=output_dir, overlap_minutes=overlap_minutes,
                generate_explorer=generate_explorer, now=now,
                run_id=child_run_id, batch_limit=batch_limit,
            )
            selected = batch.orchestration.new_messages
            if selected:
                batches.append(batch)
            if after_batch is not None:
                after_batch(batch)
            if selected == 0:
                if batch.more_available:
                    stop_reason = "provider_page_limit"
                break
            if selected < batch_limit and not batch.more_available:
                break
            elapsed_so_far = monotonic() - started
            average_batch = elapsed_so_far / max(1, len(batches))
            if (
                elapsed_so_far >= time_budget_minutes * 60
                or elapsed_so_far + average_batch > time_budget_minutes * 60
            ):
                stop_reason = "time_budget"
                break
        if not batches:
            # Preserve the empty batch result and its run audit record.
            batches.append(batch)
        elapsed = max(0.0, monotonic() - started)
        orchestration = _aggregate_orchestration(
            [item.orchestration for item in batches], run_id=parent_run_id
        )
        provider_results: dict[str, dict[str, Any]] = {}
        for item in batches:
            for name, provider_result in (item.provider_results or {}).items():
                aggregate = provider_results.setdefault(name, {
                    "status": "completed", "fetched_messages": 0,
                    "error_type": None,
                })
                aggregate["fetched_messages"] += provider_result["fetched_messages"]
                if provider_result["status"] == "failed":
                    aggregate["status"] = "failed"
                    aggregate["error_type"] = provider_result["error_type"]
        paths = tuple(
            path for item in batches for path in item.jsonl_paths
        )
        last = batches[-1]
        return ScheduledResult(
            orchestration=orchestration,
            jsonl_path=last.jsonl_path,
            html_path=last.html_path,
            fetch_from=batches[0].fetch_from,
            provider_results=provider_results,
            batches_processed=sum(
                1 for item in batches if item.orchestration.new_messages
            ),
            backlog_remaining=(
                None if stop_reason in {"time_budget", "provider_page_limit"}
                else 0
            ),
            elapsed_seconds=elapsed,
            average_batch_seconds=(
                elapsed / max(1, sum(
                    1 for item in batches if item.orchestration.new_messages
                ))
            ),
            stop_reason=stop_reason,
            jsonl_paths=paths,
        )
    finally:
        state_store.release_scheduled_lock(parent_run_id)


class _StaticScheduledProvider:
    def __init__(self, messages: list[Any]) -> None:
        self.messages = messages

    def list_messages(self) -> list[Any]:
        return list(self.messages)


def service_unit(*, working_directory: Path, uv_path: Path,
                 env_file: Path) -> str:
    for value in (working_directory, uv_path, env_file):
        if "\n" in str(value):
            raise ValueError("systemd paths must not contain newlines")
    return f"""[Unit]
Description=AgentLedger Mail-to-Calendar batch
After=network-online.target
Wants=network-online.target

[Service]
Type=oneshot
WorkingDirectory={working_directory}
ExecStart={uv_path} run mail-calendar-orchestrator run-scheduled
EnvironmentFile={env_file}
TimeoutStartSec=60min
UMask=0077
"""


def timer_unit() -> str:
    return """[Unit]
Description=Run AgentLedger Mail-to-Calendar three times daily

[Timer]
OnCalendar=*-*-* 07:30:00 Asia/Tokyo
OnCalendar=*-*-* 12:30:00 Asia/Tokyo
OnCalendar=*-*-* 18:30:00 Asia/Tokyo
Persistent=true
Unit=agentledger-mail.service

[Install]
WantedBy=timers.target
"""


def environment_template() -> str:
    return """AGENTLEDGER_MICROSOFT_CLIENT_ID=
AGENTLEDGER_OLLAMA_MODEL=qwen3:8b
AGENTLEDGER_OLLAMA_BASE_URL=http://localhost:11434
AGENTLEDGER_TIMEZONE=Asia/Tokyo
AGENTLEDGER_SCHEDULED_BATCH_SIZE=10
AGENTLEDGER_SCHEDULED_TIME_BUDGET_MINUTES=55
# Set true only after Gmail read-only auth succeeds.
AGENTLEDGER_GMAIL_ENABLED=false
# AGENTLEDGER_STATE_DB=%h/.local/share/agentledger/mail_state.sqlite3
# AGENTLEDGER_OUTPUT_DIR=%h/.local/share/agentledger/runs
"""


def _atomic_text(path: Path, value: str, mode: int) -> None:
    path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    descriptor, temporary = tempfile.mkstemp(dir=path.parent, prefix=f".{path.name}.")
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            handle.write(value)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temporary, mode)
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


class SystemdUserScheduler:
    def __init__(
        self, *, home: Path | None = None, working_directory: Path | None = None,
        uv_path: Path | None = None,
        runner: Callable[..., Any] = subprocess.run,
    ) -> None:
        self.home = (home or Path.home()).expanduser()
        self.working_directory = (working_directory or Path.cwd()).resolve()
        resolved_uv = uv_path or (Path(value) if (value := shutil.which("uv")) else None)
        if resolved_uv is None:
            raise ValueError("uv executable was not found")
        self.uv_path = resolved_uv.resolve()
        self.runner = runner
        self.unit_dir = self.home / ".config/systemd/user"
        self.env_file = self.home / ".config/agentledger" / ENV_NAME
        self.service_file = self.unit_dir / SERVICE_NAME
        self.timer_file = self.unit_dir / TIMER_NAME

    def install(self, *, enable: bool = False) -> list[Path]:
        _atomic_text(
            self.service_file,
            service_unit(
                working_directory=self.working_directory,
                uv_path=self.uv_path,
                env_file=self.env_file,
            ),
            0o644,
        )
        _atomic_text(self.timer_file, timer_unit(), 0o644)
        if not self.env_file.exists():
            _atomic_text(self.env_file, environment_template(), 0o600)
        else:
            os.chmod(self.env_file, 0o600)
        self._systemctl("daemon-reload")
        if enable:
            self._systemctl("enable", "--now", TIMER_NAME)
        return [self.service_file, self.timer_file, self.env_file]

    def status(self) -> str:
        result = self._systemctl(
            "show", TIMER_NAME,
            "--property=UnitFileState,ActiveState,NextElapseUSecRealtime,LastTriggerUSec",
            check=False,
        )
        service = self._systemctl(
            "show", SERVICE_NAME,
            "--property=Result,ExecMainStatus", check=False,
        )
        return (result.stdout or "") + (service.stdout or "")

    def enable(self) -> None:
        self._systemctl("enable", "--now", TIMER_NAME)

    def disable(self) -> None:
        self._systemctl("disable", "--now", TIMER_NAME)

    def run_now(self) -> None:
        self._systemctl("start", SERVICE_NAME)

    def uninstall(self) -> None:
        self.disable()
        for path in (self.service_file, self.timer_file):
            path.unlink(missing_ok=True)
        self._systemctl("daemon-reload")

    def _systemctl(self, *arguments: str, check: bool = True) -> Any:
        return self.runner(
            ["systemctl", "--user", *arguments], check=check,
            capture_output=True, text=True,
        )
