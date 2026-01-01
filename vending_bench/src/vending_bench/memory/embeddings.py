"""
Embedding providers for Vending-Bench vector database.
Supports TF-IDF (default/offline) and OpenAI embeddings (optional).
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import TYPE_CHECKING
import numpy as np

if TYPE_CHECKING:
    from vending_bench.config import EmbeddingConfig


class EmbeddingProvider(ABC):
    """Abstract base class for embedding providers."""

    @abstractmethod
    def embed(self, text: str) -> np.ndarray:
        """Generate embedding vector for text."""
        pass

    @abstractmethod
    def embed_batch(self, texts: list[str]) -> list[np.ndarray]:
        """Generate embedding vectors for multiple texts."""
        pass

    @property
    @abstractmethod
    def dimension(self) -> int:
        """Return the dimension of embedding vectors."""
        pass


class TFIDFEmbedding(EmbeddingProvider):
    """
    TF-IDF based embedding for offline operation.

    Uses sklearn's TfidfVectorizer to create sparse embeddings,
    then converts to dense vectors for cosine similarity search.
    """

    def __init__(self, max_features: int = 1000):
        from sklearn.feature_extraction.text import TfidfVectorizer

        self.max_features = max_features
        self.vectorizer = TfidfVectorizer(
            max_features=max_features,
            stop_words="english",
            ngram_range=(1, 2),
        )
        self._fitted = False
        self._corpus: list[str] = []

    def _ensure_fitted(self, text: str) -> None:
        """Ensure vectorizer is fitted, updating corpus if needed."""
        self._corpus.append(text)
        # Refit with updated corpus
        self.vectorizer.fit(self._corpus)
        self._fitted = True

    def embed(self, text: str) -> np.ndarray:
        """Generate TF-IDF embedding for text."""
        if not self._fitted:
            self._ensure_fitted(text)

        try:
            vector = self.vectorizer.transform([text]).toarray()[0]
        except Exception:
            # If transform fails, refit and try again
            self._ensure_fitted(text)
            vector = self.vectorizer.transform([text]).toarray()[0]

        # Normalize to unit vector for cosine similarity
        norm = np.linalg.norm(vector)
        if norm > 0:
            vector = vector / norm

        return vector

    def embed_batch(self, texts: list[str]) -> list[np.ndarray]:
        """Generate TF-IDF embeddings for multiple texts."""
        return [self.embed(text) for text in texts]

    @property
    def dimension(self) -> int:
        """Return the dimension of TF-IDF vectors."""
        if self._fitted:
            return len(self.vectorizer.get_feature_names_out())
        return self.max_features

    def add_to_corpus(self, text: str) -> None:
        """Add text to corpus and refit."""
        self._ensure_fitted(text)


class OpenAIEmbedding(EmbeddingProvider):
    """
    OpenAI embedding provider.

    Uses OpenAI's text-embedding-3-small model by default.
    Requires openai package and OPENAI_API_KEY environment variable.
    """

    def __init__(self, model: str = "text-embedding-3-small", api_key: str | None = None):
        self.model = model
        self._api_key = api_key
        self._client = None
        self._dimension = 1536  # Default for text-embedding-3-small

    def _get_client(self):
        """Lazy initialization of OpenAI client."""
        if self._client is None:
            try:
                import openai
                import os

                api_key = self._api_key or os.environ.get("OPENAI_API_KEY")
                if not api_key:
                    raise ValueError("OpenAI API key not found")

                self._client = openai.OpenAI(api_key=api_key)
            except ImportError:
                raise ImportError(
                    "OpenAI package not installed. "
                    "Install with: pip install openai"
                )
        return self._client

    def embed(self, text: str) -> np.ndarray:
        """Generate OpenAI embedding for text."""
        client = self._get_client()
        response = client.embeddings.create(
            input=text,
            model=self.model,
        )
        return np.array(response.data[0].embedding)

    def embed_batch(self, texts: list[str]) -> list[np.ndarray]:
        """Generate OpenAI embeddings for multiple texts."""
        client = self._get_client()
        response = client.embeddings.create(
            input=texts,
            model=self.model,
        )
        return [np.array(item.embedding) for item in response.data]

    @property
    def dimension(self) -> int:
        """Return the dimension of OpenAI embedding vectors."""
        return self._dimension


def create_embedding_provider(config: EmbeddingConfig) -> EmbeddingProvider:
    """Factory function to create embedding provider from config."""
    if config.provider == "openai":
        return OpenAIEmbedding(model=config.openai_model)
    else:
        return TFIDFEmbedding()
