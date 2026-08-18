from __future__ import annotations

from datetime import datetime
from typing import Any, Callable
from uuid import uuid4
from zoneinfo import ZoneInfo

from .client import ESP32Client
from .config import WateringConfig
from .history import WateringHistory


class WateringService:
    def __init__(self, config: WateringConfig, client: ESP32Client, history: WateringHistory, *, now: Callable[[], datetime] | None = None) -> None:
        self.config = config
        self.client = client
        self.history = history
        self.now = now or (lambda: datetime.now(ZoneInfo(config.timezone)))

    def water(self, seconds: int, *, method: str = "manual", request_id: str | None = None) -> dict[str, Any]:
        if not 1 <= seconds <= self.config.max_duration_seconds:
            raise ValueError("watering seconds exceed the configured safe range")
        request_id = request_id or f"watering-{uuid4()}"
        started = self.now().isoformat()
        event: dict[str, Any] = {
            "request_id": request_id, "started_at": started,
            "duration_seconds": seconds, "method": method,
        }
        try:
            if self.config.simulation_mode or method == "simulation":
                response = {"accepted": True, "request_id": request_id, "duration_seconds": seconds, "simulated": True}
            else:
                response = self.client.water(seconds, request_id)
            event.update({"success": True, "response": response})
            return response
        except Exception as exc:
            event.update({"success": False, "error_type": type(exc).__name__, "error": str(exc)[:300]})
            raise
        finally:
            self.history.append(event)

    def run_scheduled_once(self) -> bool:
        now = self.now().astimezone(ZoneInfo(self.config.timezone))
        if now.strftime("%H:%M") < self.config.watering_time:
            return False
        local_date = now.date().isoformat()
        request_id = f"scheduled-{local_date}"
        if not self.history.claim_day(local_date, request_id, now.isoformat()):
            return False
        try:
            self.water(self.config.watering_duration_seconds, method="scheduled", request_id=request_id)
        except Exception:
            self.history.set_status(local_date, "failed")
            raise
        self.history.set_status(local_date, "completed")
        return True
