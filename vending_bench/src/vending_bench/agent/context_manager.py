"""
Context manager for Vending-Bench agents.
Handles token limits and conversation history truncation.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.agent.base import AgentMessage


def estimate_tokens(text: str) -> int:
    """
    Estimate token count for text.

    Simple approximation: ~4 characters per token for English text.
    """
    return len(text) // 4


class ContextManager:
    """
    Manages conversation context within token limits.

    From the paper (Section 2.1):
    "In each iteration, the last N (30,000 in most of our experiments) tokens
    of the history is given to the agent as input to LLM inference."
    """

    def __init__(self, max_tokens: int = 30000) -> None:
        self.max_tokens = max_tokens

    def truncate_history(
        self,
        history: list[AgentMessage],
        system_prompt: str = "",
    ) -> list[AgentMessage]:
        """
        Truncate history to fit within token limit.

        Keeps the most recent messages that fit within max_tokens,
        always preserving the system prompt.

        Args:
            history: Full conversation history
            system_prompt: System prompt (always included)

        Returns:
            Truncated history that fits within token limit
        """
        # Reserve tokens for system prompt
        system_tokens = estimate_tokens(system_prompt)
        available_tokens = self.max_tokens - system_tokens - 500  # Buffer

        if available_tokens <= 0:
            # System prompt alone exceeds limit, truncate it
            return []

        # Calculate total tokens in history
        message_tokens = []
        for msg in history:
            tokens = estimate_tokens(msg.content)
            message_tokens.append(tokens)

        total_tokens = sum(message_tokens)

        # If we're under the limit, return full history
        if total_tokens <= available_tokens:
            return history

        # Truncate from the beginning (keep most recent)
        truncated = []
        running_total = 0

        for msg, tokens in zip(reversed(history), reversed(message_tokens)):
            if running_total + tokens > available_tokens:
                break
            truncated.insert(0, msg)
            running_total += tokens

        return truncated

    def format_for_llm(
        self,
        history: list[AgentMessage],
        system_prompt: str,
        provider: str = "openai",
    ) -> list[dict]:
        """
        Format truncated history for LLM API.

        Args:
            history: Conversation history (already truncated)
            system_prompt: System prompt
            provider: "openai" or "anthropic"

        Returns:
            List of message dicts for API
        """
        messages = []

        # Add system prompt
        if provider == "anthropic":
            # Anthropic handles system prompt separately
            pass
        else:
            messages.append({"role": "system", "content": system_prompt})

        # Add history
        for msg in history:
            if provider == "anthropic":
                # Convert tool messages
                if msg.role == "tool":
                    messages.append({
                        "role": "user",
                        "content": f"[Tool Result: {msg.tool_name}]\n{msg.content}",
                    })
                else:
                    messages.append({
                        "role": msg.role if msg.role != "assistant" else "assistant",
                        "content": msg.content,
                    })
            else:
                # OpenAI format
                if msg.role == "tool":
                    messages.append({
                        "role": "tool",
                        "content": msg.content,
                        "tool_call_id": msg.tool_call_id or "tool_result",
                    })
                else:
                    messages.append({
                        "role": msg.role,
                        "content": msg.content,
                    })

        return messages

    def get_context_stats(
        self,
        history: list[AgentMessage],
        system_prompt: str = "",
    ) -> dict:
        """Get statistics about current context usage."""
        system_tokens = estimate_tokens(system_prompt)
        history_tokens = sum(estimate_tokens(m.content) for m in history)
        total_tokens = system_tokens + history_tokens

        return {
            "system_tokens": system_tokens,
            "history_tokens": history_tokens,
            "total_tokens": total_tokens,
            "max_tokens": self.max_tokens,
            "utilization": total_tokens / self.max_tokens if self.max_tokens > 0 else 0,
            "message_count": len(history),
        }
