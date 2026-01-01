"""
Scratchpad memory for Vending-Bench agents.
Append-only note storage with no explicit capacity limit.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any


@dataclass
class ScratchpadEntry:
    """A single entry in the scratchpad."""

    content: str
    timestamp: datetime
    day_number: int

    def to_dict(self) -> dict[str, Any]:
        return {
            "content": self.content,
            "timestamp": self.timestamp.isoformat(),
            "day_number": self.day_number,
        }


@dataclass
class Scratchpad:
    """
    Append-only scratchpad for agent notes.

    As per the paper: "a scratchpad" - simple append-only storage
    for the agent to write notes during operation.
    """

    entries: list[ScratchpadEntry] = field(default_factory=list)

    def write(self, content: str, timestamp: datetime, day_number: int) -> None:
        """Append a new entry to the scratchpad."""
        self.entries.append(
            ScratchpadEntry(
                content=content,
                timestamp=timestamp,
                day_number=day_number,
            )
        )

    def read(self, last_n: int | None = None) -> str:
        """
        Read scratchpad contents.

        Args:
            last_n: If specified, return only the last N entries.
                   If None, return all entries.

        Returns:
            Formatted string of all/selected entries.
        """
        if not self.entries:
            return "[Scratchpad is empty]"

        entries = self.entries if last_n is None else self.entries[-last_n:]

        lines = []
        for entry in entries:
            lines.append(f"[Day {entry.day_number} - {entry.timestamp.strftime('%Y-%m-%d %H:%M')}]")
            lines.append(entry.content)
            lines.append("")  # Blank line between entries

        return "\n".join(lines).strip()

    def read_by_day(self, day_number: int) -> str:
        """Read entries from a specific day."""
        day_entries = [e for e in self.entries if e.day_number == day_number]
        if not day_entries:
            return f"[No entries for day {day_number}]"

        lines = []
        for entry in day_entries:
            lines.append(f"[{entry.timestamp.strftime('%H:%M')}]")
            lines.append(entry.content)
            lines.append("")

        return "\n".join(lines).strip()

    def get_entry_count(self) -> int:
        """Get total number of entries."""
        return len(self.entries)

    def clear(self) -> None:
        """Clear all entries (for testing)."""
        self.entries.clear()

    def to_dict(self) -> dict[str, Any]:
        """Convert scratchpad to dictionary."""
        return {
            "entry_count": len(self.entries),
            "entries": [e.to_dict() for e in self.entries],
        }

    def __repr__(self) -> str:
        return f"Scratchpad(entries={len(self.entries)})"
