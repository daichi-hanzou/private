from __future__ import annotations

import json
import threading
import time
from dataclasses import dataclass
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any, Callable


@dataclass
class SimulatorState:
    token: str
    max_duration_seconds: int = 60
    clock: Callable[[], float] = time.monotonic

    def __post_init__(self) -> None:
        self.lock = threading.Lock()
        self.pump_running = False
        self.last_request_id: str | None = None
        self.last_watering_at: float | None = None
        self.stop_at: float | None = None
        self.completed_request_ids: set[str] = set()
        self.events: list[dict[str, Any]] = []

    def refresh(self) -> None:
        with self.lock:
            if self.pump_running and self.stop_at is not None and self.clock() >= self.stop_at:
                self._stop_locked("duration_elapsed")

    def water(self, request_id: str, duration: Any) -> tuple[int, dict[str, Any]]:
        self.refresh()
        if type(duration) is not int or not 1 <= duration <= self.max_duration_seconds:
            return 400, {"accepted": False, "error": "invalid_duration"}
        if not request_id or not isinstance(request_id, str):
            return 400, {"accepted": False, "error": "invalid_request_id"}
        with self.lock:
            if request_id == self.last_request_id or request_id in self.completed_request_ids:
                return 200, {"accepted": False, "duplicate": True, "request_id": request_id}
            if self.pump_running:
                return 409, {"accepted": False, "error": "pump_already_running"}
            now = self.clock()
            self.pump_running = True
            self.last_request_id = request_id
            self.last_watering_at = now
            self.stop_at = now + duration
            self.events.append({"event": "started", "request_id": request_id, "duration_seconds": duration})
            return 202, {"accepted": True, "request_id": request_id, "duration_seconds": duration}

    def stop(self) -> dict[str, Any]:
        with self.lock:
            was_running = self.pump_running
            if was_running:
                self._stop_locked("emergency_stop")
            return {"stopped": was_running, "pump_running": False}

    def _stop_locked(self, reason: str) -> None:
        if self.last_request_id:
            self.completed_request_ids.add(self.last_request_id)
        self.events.append({"event": "stopped", "request_id": self.last_request_id, "reason": reason})
        self.pump_running = False
        self.stop_at = None

    def status(self) -> dict[str, Any]:
        self.refresh()
        with self.lock:
            remaining = max(0, int((self.stop_at or self.clock()) - self.clock())) if self.pump_running else 0
            return {
                "pump_running": self.pump_running,
                "last_request_id": self.last_request_id,
                "last_watering_at": self.last_watering_at,
                "remaining_seconds": remaining,
            }


def handler_for(state: SimulatorState) -> type[BaseHTTPRequestHandler]:
    class Handler(BaseHTTPRequestHandler):
        def do_GET(self) -> None:
            if not self._authorized():
                return self._json(401, {"error": "unauthorized"})
            if self.path == "/health":
                return self._json(200, {"status": "ok"})
            if self.path == "/status":
                return self._json(200, state.status())
            self._json(404, {"error": "not_found"})

        def do_POST(self) -> None:
            if not self._authorized():
                return self._json(401, {"error": "unauthorized"})
            try:
                size = int(self.headers.get("Content-Length", "0"))
                if size > 4096:
                    raise ValueError
                payload = json.loads(self.rfile.read(size) or b"{}")
            except (ValueError, json.JSONDecodeError):
                return self._json(400, {"error": "invalid_json"})
            if self.path == "/water":
                code, body = state.water(payload.get("request_id"), payload.get("duration_seconds"))
                return self._json(code, body)
            if self.path == "/stop":
                return self._json(200, state.stop())
            self._json(404, {"error": "not_found"})

        def _authorized(self) -> bool:
            return self.headers.get("Authorization") == f"Bearer {state.token}"

        def _json(self, code: int, value: dict[str, Any]) -> None:
            encoded = json.dumps(value, separators=(",", ":")).encode()
            self.send_response(code)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(encoded)))
            self.end_headers()
            self.wfile.write(encoded)

        def log_message(self, format: str, *args: object) -> None:
            return

    return Handler


def serve(host: str, port: int, token: str, max_duration: int = 60) -> None:
    state = SimulatorState(token, max_duration)
    server = ThreadingHTTPServer((host, port), handler_for(state))
    print(f"Watering simulator: http://{host}:{port}")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
