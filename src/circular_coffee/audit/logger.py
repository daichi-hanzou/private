from __future__ import annotations

import json
import logging
from collections.abc import Callable
from datetime import datetime, timezone
from pathlib import Path
from typing import Any
from uuid import uuid4

from .schemas import SCHEMA_VERSION, safe_jsonable


class AuditLogger:
    """Append-only JSONL logger whose failures never stop the simulation."""

    def __init__(
        self,
        path: str | Path,
        *,
        now: Callable[[], datetime] | None = None,
        event_id_factory: Callable[[], str] | None = None,
    ) -> None:
        self.path = Path(path)
        self._now = now or (lambda: datetime.now(timezone.utc))
        self._event_id_factory = event_id_factory or (lambda: str(uuid4()))
        self._warning_logger = logging.getLogger(__name__)

    def reset(self) -> None:
        try:
            self.path.parent.mkdir(parents=True, exist_ok=True)
            self.path.write_text("", encoding="utf-8")
        except Exception as exc:
            self._warning_logger.warning(
                "Failed to reset audit log %s: %s",
                self.path,
                exc,
            )

    def log_event(
        self,
        *,
        event_type: str,
        run_id: str,
        day: int,
        agent_id: str | None,
        agent_role: str | None,
        correlation_id: str,
        proposal_id: str | None = None,
        transaction_id: str | None = None,
        payload: dict[str, Any] | None = None,
    ) -> dict[str, Any] | None:
        try:
            event = {
                **(payload or {}),
                "schema_version": SCHEMA_VERSION,
                "event_id": self._event_id_factory(),
                "run_id": run_id,
                "day": day,
                "timestamp": self._now().isoformat(),
                "event_type": event_type,
                "agent_id": agent_id,
                "agent_role": agent_role,
                "correlation_id": correlation_id,
                "proposal_id": proposal_id,
                "transaction_id": transaction_id,
            }
            safe_event = safe_jsonable(event)
            self.path.parent.mkdir(parents=True, exist_ok=True)
            with self.path.open("a", encoding="utf-8") as handle:
                handle.write(json.dumps(safe_event, ensure_ascii=False) + "\n")
        except Exception as exc:
            self._warning_logger.warning(
                "Failed to append audit event to %s: %s",
                self.path,
                exc,
            )
            return None
        return safe_event
