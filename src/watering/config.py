from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path
from zoneinfo import ZoneInfo, ZoneInfoNotFoundError


def _boolean(value: str) -> bool:
    normalized = value.strip().casefold()
    if normalized in {"1", "true", "yes", "on"}:
        return True
    if normalized in {"0", "false", "no", "off"}:
        return False
    raise ValueError(f"invalid boolean setting: {value}")


@dataclass(frozen=True)
class WateringConfig:
    esp32_base_url: str
    esp32_api_token: str
    watering_time: str = "07:00"
    watering_duration_seconds: int = 30
    request_timeout_seconds: float = 5.0
    timezone: str = "Asia/Tokyo"
    simulation_mode: bool = False
    max_duration_seconds: int = 60
    state_db: Path = Path("~/.local/share/agentledger/watering_state.sqlite3")
    log_path: Path = Path("~/.local/share/agentledger/watering_history.jsonl")

    def __post_init__(self) -> None:
        if not self.esp32_base_url.startswith(("http://", "https://")):
            raise ValueError("ESP32_BASE_URL must be an HTTP(S) URL")
        if not self.esp32_api_token and not self.simulation_mode:
            raise ValueError("ESP32_API_TOKEN is required")
        try:
            hour, minute = map(int, self.watering_time.split(":"))
        except ValueError as exc:
            raise ValueError("WATERING_TIME must use HH:MM") from exc
        if not (0 <= hour <= 23 and 0 <= minute <= 59):
            raise ValueError("WATERING_TIME must be a valid time")
        if not 1 <= self.watering_duration_seconds <= self.max_duration_seconds:
            raise ValueError("watering duration exceeds the configured safe maximum")
        if not 1 <= self.max_duration_seconds <= 3600:
            raise ValueError("maximum duration must be between 1 and 3600 seconds")
        if self.request_timeout_seconds <= 0:
            raise ValueError("REQUEST_TIMEOUT_SECONDS must be positive")
        try:
            ZoneInfo(self.timezone)
        except ZoneInfoNotFoundError as exc:
            raise ValueError(f"unknown timezone: {self.timezone}") from exc

    @classmethod
    def from_env(cls) -> "WateringConfig":
        return cls(
            esp32_base_url=os.getenv("ESP32_BASE_URL", "http://127.0.0.1:8788").rstrip("/"),
            esp32_api_token=os.getenv("ESP32_API_TOKEN", ""),
            watering_time=os.getenv("WATERING_TIME", "07:00"),
            watering_duration_seconds=int(os.getenv("WATERING_DURATION_SECONDS", "30")),
            request_timeout_seconds=float(os.getenv("REQUEST_TIMEOUT_SECONDS", "5")),
            timezone=os.getenv("TIMEZONE", "Asia/Tokyo"),
            simulation_mode=_boolean(os.getenv("SIMULATION_MODE", "false")),
            max_duration_seconds=int(os.getenv("WATERING_MAX_DURATION_SECONDS", "60")),
            state_db=Path(os.getenv("WATERING_STATE_DB", "~/.local/share/agentledger/watering_state.sqlite3")).expanduser(),
            log_path=Path(os.getenv("WATERING_LOG_PATH", "~/.local/share/agentledger/watering_history.jsonl")).expanduser(),
        )
