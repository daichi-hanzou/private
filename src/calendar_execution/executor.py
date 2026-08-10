from __future__ import annotations

from typing import Protocol

from .models import CalendarExecutionRequest, CalendarExecutionResult


class CalendarExecutor(Protocol):
    def find_existing(
        self, request: CalendarExecutionRequest
    ) -> CalendarExecutionResult | None: ...

    def create_event(
        self, request: CalendarExecutionRequest
    ) -> CalendarExecutionResult: ...
