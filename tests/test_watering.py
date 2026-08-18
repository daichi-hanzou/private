from __future__ import annotations

import json
import threading
from datetime import datetime
from http.server import ThreadingHTTPServer
from zoneinfo import ZoneInfo

import pytest
import requests

from watering.client import ESP32Client, WateringClientError
from watering.config import WateringConfig
from watering.history import WateringHistory
from watering.service import WateringService
from watering.simulator import SimulatorState, handler_for


TOKEN = "test-token"


class Clock:
    def __init__(self) -> None:
        self.value = 100.0

    def __call__(self) -> float:
        return self.value


@pytest.fixture
def simulator():
    state = SimulatorState(TOKEN, max_duration_seconds=60)
    server = ThreadingHTTPServer(("127.0.0.1", 0), handler_for(state))
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield state, f"http://127.0.0.1:{server.server_port}"
    finally:
        server.shutdown()
        server.server_close()
        thread.join(timeout=2)


def test_simulator_normal_water_status_and_emergency_stop(simulator):
    _, url = simulator
    client = ESP32Client(url, TOKEN)
    assert client.health() == {"status": "ok"}
    assert client.water(30, "req-1")["accepted"]
    assert client.status()["pump_running"]
    assert client.stop() == {"stopped": True, "pump_running": False}
    assert not client.status()["pump_running"]


def test_simulator_auto_stops_without_real_sleep():
    clock = Clock()
    state = SimulatorState(TOKEN, clock=clock)
    code, _ = state.water("req-auto", 30)
    assert code == 202 and state.status()["pump_running"]
    clock.value += 31
    assert not state.status()["pump_running"]


@pytest.mark.parametrize("duration", [0, -1, 61, 1.5, True])
def test_simulator_rejects_invalid_or_excess_duration(duration):
    state = SimulatorState(TOKEN, max_duration_seconds=60)
    code, body = state.water("invalid", duration)
    assert code == 400
    assert body["error"] == "invalid_duration"


def test_authentication_running_request_and_duplicate(simulator):
    _, url = simulator
    response = requests.get(url + "/health", headers={"Authorization": "Bearer wrong"}, timeout=2)
    assert response.status_code == 401
    client = ESP32Client(url, TOKEN)
    client.water(20, "first")
    with pytest.raises(WateringClientError, match="pump_already_running"):
        client.water(10, "second")
    client.stop()
    duplicate = client.water(20, "first")
    assert duplicate == {"accepted": False, "duplicate": True, "request_id": "first"}
    assert not client.status()["pump_running"]


class RaisingSession:
    def __init__(self, error):
        self.error = error

    def request(self, *_args, **_kwargs):
        raise self.error


def test_timeout_and_unreachable_are_clear():
    with pytest.raises(WateringClientError, match="timed out"):
        ESP32Client("http://esp32", TOKEN, session=RaisingSession(requests.Timeout())).health()
    with pytest.raises(WateringClientError, match="unreachable"):
        ESP32Client("http://esp32", TOKEN, session=RaisingSession(requests.ConnectionError())).health()


def config(tmp_path, *, simulation=False):
    return WateringConfig(
        "http://esp32", TOKEN, watering_time="07:00",
        watering_duration_seconds=30, timezone="Asia/Tokyo",
        simulation_mode=simulation, max_duration_seconds=60,
        state_db=tmp_path / "state.sqlite3",
        log_path=tmp_path / "history.jsonl",
    )


class NoNetworkClient:
    def __init__(self):
        self.calls = []

    def water(self, seconds, request_id):
        self.calls.append((seconds, request_id))
        return {"accepted": True}


def test_simulation_mode_never_uses_network_and_logs_json(tmp_path):
    client = NoNetworkClient()
    with WateringHistory(tmp_path / "state.sqlite3", tmp_path / "history.jsonl") as history:
        result = WateringService(config(tmp_path, simulation=True), client, history).water(12)
    assert result["simulated"]
    assert client.calls == []
    event = json.loads((tmp_path / "history.jsonl").read_text())
    assert event["duration_seconds"] == 12
    assert event["method"] == "manual"
    assert event["success"] is True


def test_scheduled_watering_is_claimed_only_once_per_local_day(tmp_path):
    client = NoNetworkClient()
    current = datetime(2026, 8, 12, 7, 5, tzinfo=ZoneInfo("Asia/Tokyo"))
    with WateringHistory(tmp_path / "state.sqlite3", tmp_path / "history.jsonl") as history:
        service = WateringService(config(tmp_path), client, history, now=lambda: current)
        assert service.run_scheduled_once()
        assert not service.run_scheduled_once()
    assert len(client.calls) == 1
    assert client.calls[0] == (30, "scheduled-2026-08-12")


def test_scheduler_waits_until_configured_time(tmp_path):
    client = NoNetworkClient()
    current = datetime(2026, 8, 12, 6, 59, tzinfo=ZoneInfo("Asia/Tokyo"))
    with WateringHistory(tmp_path / "state.sqlite3", tmp_path / "history.jsonl") as history:
        assert not WateringService(config(tmp_path), client, history, now=lambda: current).run_scheduled_once()
    assert client.calls == []
