"""
Base classes for Vending-Bench tools.
Provides inspect-ai compatible abstractions.
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.config import Config


@dataclass
class ToolResult:
    """Result from executing a tool."""

    success: bool
    output: str
    data: dict[str, Any] = field(default_factory=dict)
    error: str | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "success": self.success,
            "output": self.output,
            "data": self.data,
            "error": self.error,
        }

    @classmethod
    def success_result(cls, output: str, data: dict[str, Any] | None = None) -> ToolResult:
        """Create a successful result."""
        return cls(success=True, output=output, data=data or {})

    @classmethod
    def error_result(cls, error: str) -> ToolResult:
        """Create an error result."""
        return cls(success=False, output=f"Error: {error}", error=error)


@dataclass
class ToolCall:
    """Record of a tool call for logging."""

    tool_name: str
    arguments: dict[str, Any]
    result: ToolResult
    timestamp: datetime
    day_number: int
    time_cost_minutes: int

    def to_dict(self) -> dict[str, Any]:
        return {
            "tool_name": self.tool_name,
            "arguments": self.arguments,
            "result": self.result.to_dict(),
            "timestamp": self.timestamp.isoformat(),
            "day_number": self.day_number,
            "time_cost_minutes": self.time_cost_minutes,
        }


class BaseTool(ABC):
    """
    Abstract base class for tools.

    Compatible with inspect-ai's tool interface pattern.
    Each tool has:
    - name: Unique identifier
    - description: Human-readable description for LLM
    - parameters: JSON schema for arguments
    - execute: Function to run the tool
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """Unique name of the tool."""
        pass

    @property
    @abstractmethod
    def description(self) -> str:
        """Description of what the tool does."""
        pass

    @property
    def parameters(self) -> dict[str, Any]:
        """JSON schema for tool parameters."""
        return {"type": "object", "properties": {}, "required": []}

    @abstractmethod
    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        """Execute the tool with given arguments."""
        pass

    def get_time_cost(self, config: Config) -> int:
        """Get time cost in minutes from config."""
        return config.get_tool_time_cost(self.name)

    def to_openai_schema(self) -> dict[str, Any]:
        """Convert to OpenAI function calling schema."""
        return {
            "type": "function",
            "function": {
                "name": self.name,
                "description": self.description,
                "parameters": self.parameters,
            },
        }

    def to_anthropic_schema(self) -> dict[str, Any]:
        """Convert to Anthropic tool schema."""
        return {
            "name": self.name,
            "description": self.description,
            "input_schema": self.parameters,
        }


class ToolRegistry:
    """
    Registry for managing available tools.

    Supports registration, lookup, and execution of tools.
    """

    def __init__(self) -> None:
        self._tools: dict[str, BaseTool] = {}
        self._tool_calls: list[ToolCall] = []

    def register(self, tool: BaseTool) -> None:
        """Register a tool."""
        self._tools[tool.name] = tool

    def register_many(self, tools: list[BaseTool]) -> None:
        """Register multiple tools."""
        for tool in tools:
            self.register(tool)

    def get(self, name: str) -> BaseTool | None:
        """Get a tool by name."""
        return self._tools.get(name)

    def list_tools(self) -> list[str]:
        """List all registered tool names."""
        return list(self._tools.keys())

    def get_all_tools(self) -> list[BaseTool]:
        """Get all registered tools."""
        return list(self._tools.values())

    def execute(
        self,
        name: str,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        """
        Execute a tool by name.

        Records the tool call and advances simulation time.
        """
        tool = self.get(name)
        if tool is None:
            return ToolResult.error_result(f"Unknown tool: {name}")

        # Get timestamp before execution
        timestamp = state.clock.get_datetime()
        day_number = state.clock.day_number

        # Execute tool
        try:
            result = tool.execute(state, config, **kwargs)
        except Exception as e:
            result = ToolResult.error_result(str(e))

        # Get time cost and advance clock
        time_cost = tool.get_time_cost(config)

        # Record tool call
        tool_call = ToolCall(
            tool_name=name,
            arguments=kwargs,
            result=result,
            timestamp=timestamp,
            day_number=day_number,
            time_cost_minutes=time_cost,
        )
        self._tool_calls.append(tool_call)

        # Update state
        state.record_tool_call()

        # Advance time (unless it's wait_for_next_day which handles its own time)
        if name != "wait_for_next_day" and time_cost > 0:
            state.clock.advance_time(time_cost)

        return result

    def get_tool_calls(self) -> list[ToolCall]:
        """Get all recorded tool calls."""
        return self._tool_calls

    def get_recent_calls(self, count: int = 10) -> list[ToolCall]:
        """Get most recent tool calls."""
        return self._tool_calls[-count:]

    def get_openai_schemas(self) -> list[dict[str, Any]]:
        """Get OpenAI function schemas for all tools."""
        return [tool.to_openai_schema() for tool in self._tools.values()]

    def get_anthropic_schemas(self) -> list[dict[str, Any]]:
        """Get Anthropic tool schemas for all tools."""
        return [tool.to_anthropic_schema() for tool in self._tools.values()]

    def format_tool_list(self) -> str:
        """Format list of tools for display."""
        lines = ["Available Tools:"]
        for tool in self._tools.values():
            lines.append(f"  - {tool.name}: {tool.description[:60]}...")
        return "\n".join(lines)
