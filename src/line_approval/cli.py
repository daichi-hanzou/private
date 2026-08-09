from __future__ import annotations

import os
from pathlib import Path

from .models import LineConfig


DEFAULT_LINE_ENV = Path("~/.config/agentledger/line-approval.env")


def initialize_config(path: str | Path = DEFAULT_LINE_ENV) -> Path:
    target = Path(path).expanduser()
    target.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    if not target.exists():
        target.write_text(
            "LINE_CHANNEL_SECRET=\n"
            "LINE_CHANNEL_ACCESS_TOKEN=\n"
            "LINE_ALLOWED_USER_ID=\n",
            encoding="utf-8",
        )
    os.chmod(target, 0o600)
    return target


def load_config(path: str | Path = DEFAULT_LINE_ENV) -> LineConfig:
    values: dict[str, str] = {}
    target = Path(path).expanduser()
    if target.exists():
        for raw in target.read_text(encoding="utf-8").splitlines():
            line = raw.strip()
            if not line or line.startswith("#"):
                continue
            key, separator, value = line.partition("=")
            if not separator:
                raise ValueError("invalid LINE configuration file")
            values[key.strip()] = value.strip().strip('"').strip("'")
    def configured(name: str) -> str:
        value = os.environ.get(name, values.get(name, ""))
        if not value:
            raise ValueError(f"{name} is not configured")
        return value
    return LineConfig(
        configured("LINE_CHANNEL_SECRET"),
        configured("LINE_CHANNEL_ACCESS_TOKEN"),
        configured("LINE_ALLOWED_USER_ID"),
        os.environ.get("AGENTLEDGER_GOOGLE_CALENDAR_ID", "primary"),
    )


def configuration_status(path: str | Path = DEFAULT_LINE_ENV) -> dict[str, bool]:
    target = Path(path).expanduser()
    values: dict[str, str] = {}
    if target.exists():
        for raw in target.read_text(encoding="utf-8").splitlines():
            key, separator, value = raw.strip().partition("=")
            if separator:
                values[key.strip()] = value.strip().strip('"').strip("'")
    return {
        name: bool(os.environ.get(name, values.get(name, "")))
        for name in (
            "LINE_CHANNEL_SECRET", "LINE_CHANNEL_ACCESS_TOKEN",
            "LINE_ALLOWED_USER_ID",
        )
    }
