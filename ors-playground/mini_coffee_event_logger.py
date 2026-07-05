from __future__ import annotations

import json
import threading
import time
from dataclasses import asdict, is_dataclass


class EventLogger:
    """Append-only JSONL event stream for replaying a mini coffee run."""

    def __init__(self, path: str) -> None:
        self.path = path
        self._fh = open(path, "w", buffering=1)
        self._t0 = time.time()
        self._lock = threading.Lock()

    def _jsonable(self, value):
        if is_dataclass(value):
            return asdict(value)
        if isinstance(value, dict):
            return {k: self._jsonable(v) for k, v in value.items()}
        if isinstance(value, (list, tuple)):
            return [self._jsonable(v) for v in value]
        return value

    def emit(self, event_type: str, **data) -> None:
        event = {
            "ts_ms": int((time.time() - self._t0) * 1000),
            "type": event_type,
            **self._jsonable(data),
        }
        with self._lock:
            self._fh.write(json.dumps(event, default=str) + "\n")

    def close(self) -> None:
        if not self._fh.closed:
            self._fh.close()
