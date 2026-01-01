"""
Simulation clock for tracking time in Vending-Bench.
Handles day progression, time advancement, and morning events.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import date, datetime, timedelta
from typing import TYPE_CHECKING, Callable

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState


@dataclass
class SimulationClock:
    """
    Manages simulation time.

    Time advances when tools are called. Each tool has an associated time cost.
    The simulation runs on a day-by-day basis, with "morning" events triggering
    at the start of each day.
    """

    current_date: date
    current_hour: int = 8  # Start at 8 AM
    current_minute: int = 0
    day_number: int = 1

    # Callbacks for morning events
    _morning_callbacks: list[Callable[[EnvironmentState], None]] = field(default_factory=list)

    @classmethod
    def create(cls, start_date: date, start_hour: int = 8) -> SimulationClock:
        """Create a new simulation clock."""
        return cls(current_date=start_date, current_hour=start_hour)

    def advance_time(self, minutes: int) -> bool:
        """
        Advance simulation time by the specified number of minutes.

        Returns True if a new day has started (requiring morning events).
        """
        total_minutes = self.current_hour * 60 + self.current_minute + minutes
        days_passed = total_minutes // (24 * 60)

        self.current_minute = total_minutes % 60
        self.current_hour = (total_minutes // 60) % 24

        if days_passed > 0:
            self.current_date += timedelta(days=days_passed)
            self.day_number += days_passed
            return True

        return False

    def advance_to_next_day(self) -> None:
        """Advance to the start of the next day (8 AM)."""
        self.current_date += timedelta(days=1)
        self.day_number += 1
        self.current_hour = 8
        self.current_minute = 0

    def get_datetime(self) -> datetime:
        """Get current simulation datetime."""
        return datetime(
            year=self.current_date.year,
            month=self.current_date.month,
            day=self.current_date.day,
            hour=self.current_hour,
            minute=self.current_minute,
        )

    def get_day_of_week(self) -> int:
        """Get day of week (0=Monday, 6=Sunday)."""
        return self.current_date.weekday()

    def get_month(self) -> int:
        """Get month (0-indexed for multiplier lookup)."""
        return self.current_date.month - 1

    def minutes_until_end_of_day(self) -> int:
        """Calculate minutes remaining until midnight."""
        return (24 - self.current_hour) * 60 - self.current_minute

    def register_morning_callback(
        self, callback: Callable[[EnvironmentState], None]
    ) -> None:
        """Register a callback to be called at the start of each day."""
        self._morning_callbacks.append(callback)

    def trigger_morning_events(self, env_state: EnvironmentState) -> None:
        """Trigger all registered morning callbacks."""
        for callback in self._morning_callbacks:
            callback(env_state)

    def format_time(self) -> str:
        """Format current time as string."""
        return f"{self.current_hour:02d}:{self.current_minute:02d}"

    def format_datetime(self) -> str:
        """Format current datetime as string."""
        return f"{self.current_date.isoformat()} {self.format_time()}"

    def __repr__(self) -> str:
        return f"SimulationClock(day={self.day_number}, {self.format_datetime()})"
