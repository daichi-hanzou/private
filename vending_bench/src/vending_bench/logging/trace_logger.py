"""
Trace logger for Vending-Bench.
Records all tool calls and agent actions for analysis.
"""

from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.tools.base import ToolCall
    from vending_bench.agent.base import AgentAction


class TraceLogger:
    """
    Logs all tool calls and agent actions.

    Creates a JSONL file with one entry per tool call, including:
    - Tool name and arguments
    - Result (success/error and output)
    - Timestamp (real and simulated)
    - Day number
    """

    def __init__(self, output_path: Path | str | None = None) -> None:
        self.output_path = Path(output_path) if output_path else None
        self.entries: list[dict[str, Any]] = []
        self._file_handle = None

    def start(self) -> None:
        """Start logging (open file if path specified)."""
        if self.output_path:
            self.output_path.parent.mkdir(parents=True, exist_ok=True)
            self._file_handle = open(self.output_path, "w")

    def stop(self) -> None:
        """Stop logging (close file)."""
        if self._file_handle:
            self._file_handle.close()
            self._file_handle = None

    def log_tool_call(
        self,
        tool_call: ToolCall,
        agent_reasoning: str = "",
    ) -> None:
        """Log a tool call."""
        entry = {
            "type": "tool_call",
            "timestamp_real": datetime.now().isoformat(),
            "timestamp_sim": tool_call.timestamp.isoformat(),
            "day_number": tool_call.day_number,
            "tool_name": tool_call.tool_name,
            "arguments": tool_call.arguments,
            "result": {
                "success": tool_call.result.success,
                "output": tool_call.result.output[:1000],  # Truncate long outputs
                "error": tool_call.result.error,
            },
            "time_cost_minutes": tool_call.time_cost_minutes,
            "reasoning": agent_reasoning,
        }

        self.entries.append(entry)
        self._write_entry(entry)

    def log_agent_action(
        self,
        action: AgentAction,
        day_number: int,
        sim_timestamp: datetime,
    ) -> None:
        """Log an agent action decision."""
        entry = {
            "type": "agent_action",
            "timestamp_real": datetime.now().isoformat(),
            "timestamp_sim": sim_timestamp.isoformat(),
            "day_number": day_number,
            "tool_name": action.tool_name,
            "arguments": action.arguments,
            "reasoning": action.reasoning,
        }

        self.entries.append(entry)
        self._write_entry(entry)

    def log_event(
        self,
        event_type: str,
        data: dict[str, Any],
        day_number: int,
        sim_timestamp: datetime,
    ) -> None:
        """Log a generic event."""
        entry = {
            "type": event_type,
            "timestamp_real": datetime.now().isoformat(),
            "timestamp_sim": sim_timestamp.isoformat(),
            "day_number": day_number,
            "data": data,
        }

        self.entries.append(entry)
        self._write_entry(entry)

    def log_morning_report(
        self,
        day_number: int,
        sales_report: dict[str, Any],
        new_emails: int,
        deliveries: list[str],
        sim_timestamp: datetime,
    ) -> None:
        """Log the morning report."""
        entry = {
            "type": "morning_report",
            "timestamp_real": datetime.now().isoformat(),
            "timestamp_sim": sim_timestamp.isoformat(),
            "day_number": day_number,
            "sales_report": sales_report,
            "new_emails": new_emails,
            "deliveries": deliveries,
        }

        self.entries.append(entry)
        self._write_entry(entry)

    def _write_entry(self, entry: dict[str, Any]) -> None:
        """Write entry to file if open."""
        if self._file_handle:
            self._file_handle.write(json.dumps(entry) + "\n")
            self._file_handle.flush()

    def get_tool_call_count(self) -> int:
        """Get total number of tool calls logged."""
        return sum(1 for e in self.entries if e["type"] == "tool_call")

    def get_tool_call_summary(self) -> dict[str, int]:
        """Get summary of tool calls by name."""
        summary: dict[str, int] = {}
        for entry in self.entries:
            if entry["type"] == "tool_call":
                name = entry["tool_name"]
                summary[name] = summary.get(name, 0) + 1
        return summary

    def get_entries_by_day(self, day_number: int) -> list[dict[str, Any]]:
        """Get all entries for a specific day."""
        return [e for e in self.entries if e.get("day_number") == day_number]

    def to_json(self) -> str:
        """Export all entries as JSON."""
        return json.dumps(self.entries, indent=2)

    def save(self, path: Path | str) -> None:
        """Save all entries to a file."""
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w") as f:
            for entry in self.entries:
                f.write(json.dumps(entry) + "\n")
