"""
Memory tools for Vending-Bench agents.
Tools for scratchpad, key-value store, and vector database.
"""

from __future__ import annotations

from typing import Any, TYPE_CHECKING

from vending_bench.tools.base import BaseTool, ToolResult

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.config import Config


class WriteScratchpadTool(BaseTool):
    """Write to scratchpad."""

    @property
    def name(self) -> str:
        return "write_scratchpad"

    @property
    def description(self) -> str:
        return "Append a note to your scratchpad for future reference."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "content": {
                    "type": "string",
                    "description": "The note content to write",
                },
            },
            "required": ["content"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        content: str = "",
        **kwargs: Any,
    ) -> ToolResult:
        if state.scratchpad is None:
            return ToolResult.error_result("Scratchpad not initialized")

        state.scratchpad.write(
            content=content,
            timestamp=state.clock.get_datetime(),
            day_number=state.clock.day_number,
        )

        return ToolResult.success_result(
            f"Note added to scratchpad. Total entries: {state.scratchpad.get_entry_count()}",
            {"entry_count": state.scratchpad.get_entry_count()},
        )


class ReadScratchpadTool(BaseTool):
    """Read from scratchpad."""

    @property
    def name(self) -> str:
        return "read_scratchpad"

    @property
    def description(self) -> str:
        return "Read your scratchpad notes."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "last_n": {
                    "type": "integer",
                    "description": "Number of most recent entries to read (optional, defaults to all)",
                },
            },
            "required": [],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        last_n: int | None = None,
        **kwargs: Any,
    ) -> ToolResult:
        if state.scratchpad is None:
            return ToolResult.error_result("Scratchpad not initialized")

        content = state.scratchpad.read(last_n=last_n)
        return ToolResult.success_result(
            content,
            {"entry_count": state.scratchpad.get_entry_count()},
        )


class SetKVValueTool(BaseTool):
    """Set value in key-value store."""

    @property
    def name(self) -> str:
        return "set_kv_value"

    @property
    def description(self) -> str:
        return "Store a value by key in your key-value store for later retrieval."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "key": {
                    "type": "string",
                    "description": "The key to store the value under",
                },
                "value": {
                    "type": "string",
                    "description": "The value to store",
                },
            },
            "required": ["key", "value"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        key: str = "",
        value: str = "",
        **kwargs: Any,
    ) -> ToolResult:
        if state.kv_store is None:
            return ToolResult.error_result("Key-value store not initialized")

        is_update = state.kv_store.exists(key)
        state.kv_store.set(
            key=key,
            value=value,
            timestamp=state.clock.get_datetime(),
            day_number=state.clock.day_number,
        )

        action = "Updated" if is_update else "Stored"
        return ToolResult.success_result(
            f"{action} value for key '{key}'",
            {"key": key, "action": action.lower()},
        )


class GetKVValueTool(BaseTool):
    """Get value from key-value store."""

    @property
    def name(self) -> str:
        return "get_kv_value"

    @property
    def description(self) -> str:
        return "Retrieve a value by key from your key-value store."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "key": {
                    "type": "string",
                    "description": "The key to retrieve",
                },
            },
            "required": ["key"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        key: str = "",
        **kwargs: Any,
    ) -> ToolResult:
        if state.kv_store is None:
            return ToolResult.error_result("Key-value store not initialized")

        value = state.kv_store.get(key)

        if value is None:
            return ToolResult.success_result(
                f"No value found for key '{key}'\n\n"
                + state.kv_store.list_keys_with_preview(),
                {"key": key, "found": False},
            )

        return ToolResult.success_result(
            f"Value for '{key}':\n{value}",
            {"key": key, "value": value, "found": True},
        )


class DeleteKVValueTool(BaseTool):
    """Delete value from key-value store."""

    @property
    def name(self) -> str:
        return "delete_kv_value"

    @property
    def description(self) -> str:
        return "Delete a key-value pair from your key-value store."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "key": {
                    "type": "string",
                    "description": "The key to delete",
                },
            },
            "required": ["key"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        key: str = "",
        **kwargs: Any,
    ) -> ToolResult:
        if state.kv_store is None:
            return ToolResult.error_result("Key-value store not initialized")

        deleted = state.kv_store.delete(key)

        if deleted:
            return ToolResult.success_result(
                f"Deleted key '{key}'",
                {"key": key, "deleted": True},
            )
        else:
            return ToolResult.success_result(
                f"Key '{key}' not found",
                {"key": key, "deleted": False},
            )


class AddToVectorDBTool(BaseTool):
    """Add text to vector database."""

    @property
    def name(self) -> str:
        return "add_to_vector_db"

    @property
    def description(self) -> str:
        return "Add text to your vector database for semantic search later."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "text": {
                    "type": "string",
                    "description": "The text to store",
                },
                "metadata": {
                    "type": "object",
                    "description": "Optional metadata to attach (e.g., {'type': 'supplier_info'})",
                },
            },
            "required": ["text"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        text: str = "",
        metadata: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> ToolResult:
        if state.vector_db is None:
            return ToolResult.error_result("Vector database not initialized")

        entry_id = state.vector_db.add(
            text=text,
            metadata=metadata,
            timestamp=state.clock.get_datetime(),
            day_number=state.clock.day_number,
        )

        return ToolResult.success_result(
            f"Added to vector database (ID: {entry_id}). "
            f"Total entries: {state.vector_db.get_entry_count()}",
            {"entry_id": entry_id, "entry_count": state.vector_db.get_entry_count()},
        )


class SearchVectorDBTool(BaseTool):
    """Search vector database."""

    @property
    def name(self) -> str:
        return "search_vector_db"

    @property
    def description(self) -> str:
        return "Search your vector database for semantically similar texts."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "Search query",
                },
                "top_k": {
                    "type": "integer",
                    "description": "Maximum number of results (default: 5)",
                    "default": 5,
                },
            },
            "required": ["query"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        query: str = "",
        top_k: int = 5,
        **kwargs: Any,
    ) -> ToolResult:
        if state.vector_db is None:
            return ToolResult.error_result("Vector database not initialized")

        results = state.vector_db.search(query, top_k=top_k)

        if not results:
            return ToolResult.success_result(
                f"No results found for query: '{query}'",
                {"query": query, "results": []},
            )

        lines = [f"Search results for: '{query}'", "-" * 40]
        for i, result in enumerate(results, 1):
            lines.append(f"\n{i}. (Score: {result.score:.3f})")
            lines.append(f"   {result.text[:200]}...")
            if result.metadata:
                lines.append(f"   Metadata: {result.metadata}")

        output = "\n".join(lines)
        return ToolResult.success_result(
            output,
            {
                "query": query,
                "results": [r.to_dict() for r in results],
            },
        )


def create_memory_tools() -> list[BaseTool]:
    """Create all memory tools."""
    return [
        WriteScratchpadTool(),
        ReadScratchpadTool(),
        SetKVValueTool(),
        GetKVValueTool(),
        DeleteKVValueTool(),
        AddToVectorDBTool(),
        SearchVectorDBTool(),
    ]
