"""
Key-Value store memory for Vending-Bench agents.
Simple dictionary-based storage with no explicit capacity limit.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any


@dataclass
class KVEntry:
    """A single entry in the KV store."""

    value: str
    created_at: datetime
    updated_at: datetime
    day_number: int

    def to_dict(self) -> dict[str, Any]:
        return {
            "value": self.value,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
            "day_number": self.day_number,
        }


@dataclass
class KeyValueStore:
    """
    Key-Value store for agent memory.

    As per the paper: "key-value store" - allows saving, retrieving,
    and deleting values by key.
    """

    data: dict[str, KVEntry] = field(default_factory=dict)

    def set(self, key: str, value: str, timestamp: datetime, day_number: int) -> None:
        """
        Set a value for a key.
        Creates new entry or updates existing one.
        """
        if key in self.data:
            self.data[key].value = value
            self.data[key].updated_at = timestamp
            self.data[key].day_number = day_number
        else:
            self.data[key] = KVEntry(
                value=value,
                created_at=timestamp,
                updated_at=timestamp,
                day_number=day_number,
            )

    def get(self, key: str) -> str | None:
        """
        Get value for a key.
        Returns None if key doesn't exist.
        """
        entry = self.data.get(key)
        return entry.value if entry else None

    def delete(self, key: str) -> bool:
        """
        Delete a key-value pair.
        Returns True if key existed and was deleted, False otherwise.
        """
        if key in self.data:
            del self.data[key]
            return True
        return False

    def exists(self, key: str) -> bool:
        """Check if a key exists."""
        return key in self.data

    def keys(self) -> list[str]:
        """Get all keys."""
        return list(self.data.keys())

    def get_all(self) -> dict[str, str]:
        """Get all key-value pairs (values only)."""
        return {k: v.value for k, v in self.data.items()}

    def get_entry_count(self) -> int:
        """Get total number of entries."""
        return len(self.data)

    def clear(self) -> None:
        """Clear all entries (for testing)."""
        self.data.clear()

    def to_dict(self) -> dict[str, Any]:
        """Convert KV store to dictionary."""
        return {
            "entry_count": len(self.data),
            "keys": list(self.data.keys()),
            "entries": {k: v.to_dict() for k, v in self.data.items()},
        }

    def list_keys_with_preview(self, max_value_length: int = 50) -> str:
        """List all keys with a preview of their values."""
        if not self.data:
            return "[Key-Value store is empty]"

        lines = []
        for key, entry in sorted(self.data.items()):
            preview = entry.value[:max_value_length]
            if len(entry.value) > max_value_length:
                preview += "..."
            lines.append(f"  {key}: {preview}")

        return "Keys:\n" + "\n".join(lines)

    def __repr__(self) -> str:
        return f"KeyValueStore(entries={len(self.data)})"
