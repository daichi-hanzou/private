"""
Base agent class for Vending-Bench.
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.config import Config
    from vending_bench.tools.base import ToolResult


@dataclass
class AgentAction:
    """An action taken by the agent."""

    tool_name: str
    arguments: dict[str, Any] = field(default_factory=dict)
    reasoning: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {
            "tool_name": self.tool_name,
            "arguments": self.arguments,
            "reasoning": self.reasoning,
        }


@dataclass
class AgentMessage:
    """A message in the agent's conversation history."""

    role: str  # "user", "assistant", "system", "tool"
    content: str
    tool_name: str | None = None
    tool_call_id: str | None = None

    def to_dict(self) -> dict[str, Any]:
        result = {"role": self.role, "content": self.content}
        if self.tool_name:
            result["tool_name"] = self.tool_name
        if self.tool_call_id:
            result["tool_call_id"] = self.tool_call_id
        return result


class BaseAgent(ABC):
    """
    Abstract base class for Vending-Bench agents.

    An agent receives observations about the environment and decides
    which tools to call to operate the vending machine business.
    """

    def __init__(self) -> None:
        self.history: list[AgentMessage] = []
        self.message_count: int = 0

    @abstractmethod
    def think(
        self,
        state: EnvironmentState,
        config: Config,
        observation: str,
    ) -> AgentAction:
        """
        Given an observation, decide on the next action.

        Args:
            state: Current environment state
            config: Configuration
            observation: Latest observation/message from environment

        Returns:
            The action to take (tool name and arguments)
        """
        pass

    @abstractmethod
    def receive_tool_result(
        self,
        action: AgentAction,
        result: ToolResult,
    ) -> None:
        """
        Receive the result of a tool call.

        Args:
            action: The action that was taken
            result: The result of executing the tool
        """
        pass

    def add_message(self, role: str, content: str, **kwargs: Any) -> None:
        """Add a message to history."""
        self.history.append(AgentMessage(role=role, content=content, **kwargs))
        self.message_count += 1

    def get_history(self) -> list[AgentMessage]:
        """Get conversation history."""
        return self.history

    def clear_history(self) -> None:
        """Clear conversation history."""
        self.history.clear()
        self.message_count = 0

    def get_system_prompt(self, state: EnvironmentState, config: Config) -> str:
        """Get the system prompt for the agent."""
        return f"""You are an AI agent operating a vending machine business.

Your goal is to maximize your net worth by:
1. Ordering products from wholesale suppliers via email
2. Stocking products in your vending machine
3. Setting competitive prices
4. Collecting cash from sales
5. Managing your daily operating costs (${config.environment.daily_fee}/day)

Current Status:
- Day: {state.clock.day_number}
- Money Balance: ${state.account.balance:.2f}
- Account Number: {state.account.account_number}
- Delivery Address: {state.email_system.delivery_address if state.email_system else 'N/A'}

You have access to various tools to manage your business. Use them wisely to grow your net worth.

Important:
- Each day costs ${config.environment.daily_fee} in operating fees
- You start with ${config.environment.initial_balance}
- If you can't pay the daily fee for {config.agent.bankruptcy_threshold} consecutive days, you go bankrupt
- Products need to be ordered, delivered (takes a few days), stocked, and priced before they can sell
- Use the sub-agent for physical tasks at the vending machine

Think carefully about your strategy and take actions to grow your business!"""
