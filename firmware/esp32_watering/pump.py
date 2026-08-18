import time

import machine
import uasyncio as asyncio


class PumpController:
    def __init__(self, gpio, active_high=True, max_run_seconds=60):
        self.pin = machine.Pin(gpio, machine.Pin.OUT)
        self.active_high = active_high
        self.max_run_seconds = max_run_seconds
        self.pump_running = False
        self.last_request_id = None
        self.last_watering_at = None
        self.stop_at_ms = None
        self.seen_request_ids = set()
        self._set(False)  # fail-safe OFF immediately at boot

    def _set(self, enabled):
        self.pin.value(1 if enabled == self.active_high else 0)

    def start(self, duration, request_id):
        self.refresh()
        if not isinstance(duration, int) or isinstance(duration, bool):
            return False, "invalid_duration"
        if duration < 1 or duration > self.max_run_seconds:
            return False, "invalid_duration"
        if not request_id or not isinstance(request_id, str):
            return False, "invalid_request_id"
        if request_id in self.seen_request_ids or request_id == self.last_request_id:
            return False, "duplicate_request"
        if self.pump_running:
            return False, "pump_already_running"
        self.pump_running = True
        self.last_request_id = request_id
        self.last_watering_at = time.time()
        self.stop_at_ms = time.ticks_add(time.ticks_ms(), duration * 1000)
        self._set(True)
        return True, None

    def stop(self):
        was_running = self.pump_running
        self._set(False)
        if self.last_request_id:
            self.seen_request_ids.add(self.last_request_id)
        self.pump_running = False
        self.stop_at_ms = None
        return was_running

    def refresh(self):
        if self.pump_running and time.ticks_diff(time.ticks_ms(), self.stop_at_ms) >= 0:
            self.stop()

    def status(self):
        self.refresh()
        remaining = 0
        if self.pump_running:
            remaining = max(0, time.ticks_diff(self.stop_at_ms, time.ticks_ms()) // 1000)
        return {
            "pump_running": self.pump_running,
            "last_request_id": self.last_request_id,
            "last_watering_at": self.last_watering_at,
            "remaining_seconds": remaining,
        }

    async def watchdog(self):
        while True:
            try:
                self.refresh()
            except Exception:
                self.stop()
            await asyncio.sleep_ms(100)
