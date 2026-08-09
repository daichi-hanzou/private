from __future__ import annotations

import json
import os
import shutil
import subprocess
from pathlib import Path
from typing import Any, Callable
from urllib.error import URLError
from urllib.request import urlopen

from mail_calendar_orchestrator.scheduler import _atomic_text

from .cli import DEFAULT_LINE_ENV


LINE_WEBHOOK_SERVICE = "agentledger-line-webhook.service"
LINE_WEBHOOK_HOST = "127.0.0.1"
LINE_WEBHOOK_PORT = 8787


def webhook_service_unit(
    *, working_directory: Path, uv_path: Path, env_file: Path,
) -> str:
    for value in (working_directory, uv_path, env_file):
        if "\n" in str(value):
            raise ValueError("systemd paths must not contain newlines")
        if not value.is_absolute():
            raise ValueError("systemd paths must be absolute")
    return f"""[Unit]
Description=AgentLedger LINE Webhook
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
WorkingDirectory={working_directory}
EnvironmentFile={env_file}
ExecStart={uv_path} run mail-calendar-orchestrator line webhook --host {LINE_WEBHOOK_HOST} --port {LINE_WEBHOOK_PORT}
Restart=on-failure
RestartSec=5
TimeoutStopSec=10
UMask=0077

[Install]
WantedBy=default.target
"""


class LineWebhookSystemdService:
    def __init__(
        self,
        *,
        home: Path | None = None,
        working_directory: Path | None = None,
        uv_path: Path | None = None,
        env_file: Path | None = None,
        runner: Callable[..., Any] = subprocess.run,
        health_check: Callable[[], bool] | None = None,
    ) -> None:
        self.home = (home or Path.home()).expanduser().resolve()
        self.working_directory = (working_directory or Path.cwd()).resolve()
        discovered = uv_path or (
            Path(value) if (value := shutil.which("uv")) else None
        )
        if discovered is None:
            raise ValueError("uv executable was not found")
        self.uv_path = discovered.expanduser().resolve()
        self.env_file = (
            env_file.expanduser().resolve()
            if env_file else (self.home / str(DEFAULT_LINE_ENV).removeprefix("~/"))
        )
        self.unit_dir = self.home / ".config/systemd/user"
        self.service_file = self.unit_dir / LINE_WEBHOOK_SERVICE
        self.runner = runner
        self.health_check = health_check or self._health

    def install(self) -> Path:
        self._require_env()
        _atomic_text(
            self.service_file,
            webhook_service_unit(
                working_directory=self.working_directory,
                uv_path=self.uv_path,
                env_file=self.env_file,
            ),
            0o644,
        )
        self._systemctl("daemon-reload")
        return self.service_file

    def enable(self) -> None:
        self._require_env()
        if not self.service_file.exists():
            raise ValueError("LINE webhook service is not installed")
        self._systemctl("enable", "--now", LINE_WEBHOOK_SERVICE)

    def disable(self) -> None:
        self._systemctl(
            "disable", "--now", LINE_WEBHOOK_SERVICE, check=False
        )

    def restart(self) -> None:
        self._require_env()
        if not self.service_file.exists():
            raise ValueError("LINE webhook service is not installed")
        self._systemctl("restart", LINE_WEBHOOK_SERVICE)

    def uninstall(self) -> None:
        self.disable()
        self.service_file.unlink(missing_ok=True)
        self._systemctl("daemon-reload")

    def status(self) -> str:
        installed = self.service_file.exists()
        enabled = self._systemctl(
            "is-enabled", LINE_WEBHOOK_SERVICE, check=False
        )
        active = self._systemctl(
            "is-active", LINE_WEBHOOK_SERVICE, check=False
        )
        active_value = (active.stdout or "inactive").strip() or "inactive"
        health = "ok" if active.returncode == 0 and self.health_check() else "unavailable"
        return (
            f"Service: {LINE_WEBHOOK_SERVICE}\n"
            f"Installed: {'yes' if installed else 'no'}\n"
            f"Enabled: {'yes' if enabled.returncode == 0 else 'no'}\n"
            f"Active: {active_value}\n"
            f"Endpoint: http://{LINE_WEBHOOK_HOST}:{LINE_WEBHOOK_PORT}/line/webhook\n"
            f"Health: {health}"
        )

    def _require_env(self) -> None:
        if not self.env_file.is_file():
            raise ValueError(f"LINE environment file not found: {self.env_file}")
        try:
            os.chmod(self.env_file, 0o600)
        except OSError as exc:
            raise ValueError("LINE environment file permissions could not be secured") from exc

    @staticmethod
    def _health() -> bool:
        try:
            with urlopen(
                f"http://{LINE_WEBHOOK_HOST}:{LINE_WEBHOOK_PORT}/health",
                timeout=2,
            ) as response:
                if response.status != 200:
                    return False
                payload = json.loads(response.read(1024))
                return payload == {"status": "ok"}
        except (OSError, URLError, ValueError, json.JSONDecodeError):
            return False

    def _systemctl(self, *arguments: str, check: bool = True) -> Any:
        return self.runner(
            ["systemctl", "--user", *arguments],
            check=check,
            capture_output=True,
            text=True,
        )
