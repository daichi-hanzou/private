"""
Vector database for Vending-Bench agents.
Stores text with embeddings for semantic search using cosine similarity.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, TYPE_CHECKING
import numpy as np

from vending_bench.memory.embeddings import (
    EmbeddingProvider,
    TFIDFEmbedding,
    create_embedding_provider,
)

if TYPE_CHECKING:
    from vending_bench.config import EmbeddingConfig


@dataclass
class VectorEntry:
    """A single entry in the vector database."""

    id: str
    text: str
    embedding: np.ndarray
    metadata: dict[str, Any]
    created_at: datetime
    day_number: int

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "text": self.text,
            "metadata": self.metadata,
            "created_at": self.created_at.isoformat(),
            "day_number": self.day_number,
        }


@dataclass
class SearchResult:
    """Result from a vector search."""

    id: str
    text: str
    score: float  # Cosine similarity score (0-1)
    metadata: dict[str, Any]

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "text": self.text,
            "score": self.score,
            "metadata": self.metadata,
        }


@dataclass
class VectorDB:
    """
    Vector database for semantic search.

    As per the paper: "vector database" - stores texts and embeddings,
    searched with cosine similarity. Uses text-embedding-3-small in the paper,
    but we default to TF-IDF for offline operation.
    """

    embedding_provider: EmbeddingProvider
    entries: dict[str, VectorEntry] = field(default_factory=dict)
    _next_id: int = 0

    @classmethod
    def create(cls, config: EmbeddingConfig | None = None) -> VectorDB:
        """Create a vector database with the specified embedding provider."""
        if config is None:
            provider = TFIDFEmbedding()
        else:
            provider = create_embedding_provider(config)

        return cls(embedding_provider=provider)

    def _generate_id(self) -> str:
        """Generate a unique ID for a new entry."""
        self._next_id += 1
        return f"vec_{self._next_id}"

    def add(
        self,
        text: str,
        metadata: dict[str, Any] | None = None,
        timestamp: datetime | None = None,
        day_number: int = 0,
        entry_id: str | None = None,
    ) -> str:
        """
        Add a text to the vector database.

        Args:
            text: The text to store
            metadata: Optional metadata dict
            timestamp: When the entry was created
            day_number: Current simulation day
            entry_id: Optional custom ID (auto-generated if not provided)

        Returns:
            The ID of the created entry
        """
        if timestamp is None:
            timestamp = datetime.now()

        if entry_id is None:
            entry_id = self._generate_id()

        # Generate embedding
        embedding = self.embedding_provider.embed(text)

        # Store entry
        self.entries[entry_id] = VectorEntry(
            id=entry_id,
            text=text,
            embedding=embedding,
            metadata=metadata or {},
            created_at=timestamp,
            day_number=day_number,
        )

        return entry_id

    def search(
        self,
        query: str,
        top_k: int = 5,
        min_score: float = 0.0,
    ) -> list[SearchResult]:
        """
        Search for similar texts using cosine similarity.

        Args:
            query: The search query
            top_k: Maximum number of results to return
            min_score: Minimum similarity score (0-1)

        Returns:
            List of search results, sorted by similarity (highest first)
        """
        if not self.entries:
            return []

        # Generate query embedding
        query_embedding = self.embedding_provider.embed(query)

        # Calculate cosine similarity with all entries
        results = []
        for entry in self.entries.values():
            score = self._cosine_similarity(query_embedding, entry.embedding)
            if score >= min_score:
                results.append(
                    SearchResult(
                        id=entry.id,
                        text=entry.text,
                        score=float(score),
                        metadata=entry.metadata,
                    )
                )

        # Sort by score (descending) and return top_k
        results.sort(key=lambda x: x.score, reverse=True)
        return results[:top_k]

    def _cosine_similarity(self, a: np.ndarray, b: np.ndarray) -> float:
        """Calculate cosine similarity between two vectors."""
        # Handle dimension mismatch (can happen with TF-IDF when vocabulary changes)
        if len(a) != len(b):
            # Pad shorter vector with zeros
            max_len = max(len(a), len(b))
            a = np.pad(a, (0, max_len - len(a)))
            b = np.pad(b, (0, max_len - len(b)))

        norm_a = np.linalg.norm(a)
        norm_b = np.linalg.norm(b)

        if norm_a == 0 or norm_b == 0:
            return 0.0

        return float(np.dot(a, b) / (norm_a * norm_b))

    def get(self, entry_id: str) -> VectorEntry | None:
        """Get an entry by ID."""
        return self.entries.get(entry_id)

    def delete(self, entry_id: str) -> bool:
        """Delete an entry by ID."""
        if entry_id in self.entries:
            del self.entries[entry_id]
            return True
        return False

    def get_entry_count(self) -> int:
        """Get total number of entries."""
        return len(self.entries)

    def list_entries(self, limit: int = 100) -> list[dict[str, Any]]:
        """List all entries (limited for display)."""
        entries = list(self.entries.values())[:limit]
        return [
            {
                "id": e.id,
                "text_preview": e.text[:100] + "..." if len(e.text) > 100 else e.text,
                "day_number": e.day_number,
            }
            for e in entries
        ]

    def clear(self) -> None:
        """Clear all entries (for testing)."""
        self.entries.clear()
        self._next_id = 0

    def to_dict(self) -> dict[str, Any]:
        """Convert vector DB to dictionary."""
        return {
            "entry_count": len(self.entries),
            "entries": {k: v.to_dict() for k, v in self.entries.items()},
        }

    def __repr__(self) -> str:
        return f"VectorDB(entries={len(self.entries)})"
