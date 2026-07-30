from __future__ import annotations

import math
import re
from collections import Counter
from collections.abc import Callable, Iterable, Sequence

from .models import Chunk

TOKEN_PATTERN = re.compile(r"[A-Za-z0-9][A-Za-z0-9._%-]*|[\u3040-\u30ff\u3400-\u9fff]{1,4}")


def tokenize(text: str) -> list[str]:
    return [token.lower() for token in TOKEN_PATTERN.findall(text)]


class BM25Retriever:
    def __init__(self, chunks: Iterable[Chunk], k1: float = 1.5, b: float = 0.75):
        self.chunks = list(chunks)
        if not self.chunks:
            raise ValueError("At least one chunk is required")
        self.k1 = k1
        self.b = b
        self.term_frequencies = [Counter(tokenize(chunk.text)) for chunk in self.chunks]
        self.lengths = [sum(tf.values()) for tf in self.term_frequencies]
        self.average_length = sum(self.lengths) / len(self.lengths)
        self.document_frequency = Counter()
        for tf in self.term_frequencies:
            self.document_frequency.update(tf.keys())

    def search(
        self, query: str, limit: int = 8, categories: set[str] | None = None
    ) -> list[Chunk]:
        query_terms = tokenize(query)
        scored: list[tuple[float, int]] = []
        total = len(self.chunks)
        for index, (chunk, tf, length) in enumerate(
            zip(self.chunks, self.term_frequencies, self.lengths)
        ):
            if categories and chunk.category not in categories:
                continue
            score = 0.0
            for term in query_terms:
                frequency = tf[term]
                if not frequency:
                    continue
                df = self.document_frequency[term]
                idf = math.log(1 + (total - df + 0.5) / (df + 0.5))
                denominator = frequency + self.k1 * (
                    1 - self.b + self.b * length / max(self.average_length, 1)
                )
                score += idf * frequency * (self.k1 + 1) / denominator
            if score > 0:
                scored.append((score, index))
        scored.sort(key=lambda item: (-item[0], item[1]))
        return [self.chunks[index] for _, index in scored[:limit]]


EmbeddingFunction = Callable[[Sequence[str]], list[list[float]]]


def _normalize(vector: Sequence[float]) -> list[float]:
    magnitude = math.sqrt(sum(value * value for value in vector))
    if magnitude == 0:
        return [0.0 for _ in vector]
    return [value / magnitude for value in vector]


class VectorRetriever:
    """In-memory cosine-similarity retrieval using caller-provided embeddings."""

    def __init__(self, chunks: Iterable[Chunk], embed: EmbeddingFunction):
        self.chunks = list(chunks)
        if not self.chunks:
            raise ValueError("At least one chunk is required")
        self.embed = embed
        vectors = embed([chunk.text for chunk in self.chunks])
        if len(vectors) != len(self.chunks):
            raise ValueError("Embedding provider returned an unexpected vector count")
        dimensions = {len(vector) for vector in vectors}
        if len(dimensions) != 1 or not dimensions or 0 in dimensions:
            raise ValueError("Embedding vectors must have one consistent dimension")
        self.vectors = [_normalize(vector) for vector in vectors]

    def search(
        self, query: str, limit: int = 8, categories: set[str] | None = None
    ) -> list[Chunk]:
        query_vectors = self.embed([query])
        if len(query_vectors) != 1:
            raise ValueError("Embedding provider did not return a query vector")
        if len(query_vectors[0]) != len(self.vectors[0]):
            raise ValueError("Query and document embeddings have different dimensions")
        query_vector = _normalize(query_vectors[0])
        scored: list[tuple[float, int]] = []
        for index, (chunk, vector) in enumerate(zip(self.chunks, self.vectors)):
            if categories and chunk.category not in categories:
                continue
            score = sum(left * right for left, right in zip(query_vector, vector))
            scored.append((score, index))
        scored.sort(key=lambda item: (-item[0], item[1]))
        return [self.chunks[index] for _, index in scored[:limit]]


class HybridRetriever:
    """Fuse BM25 and semantic rankings with reciprocal rank fusion."""

    def __init__(
        self, chunks: Iterable[Chunk], embed: EmbeddingFunction,
        semantic_weight: float = 1.0, text_weight: float = 1.0,
        rrf_k: int = 60,
    ):
        chunks = list(chunks)
        if semantic_weight < 0 or text_weight < 0:
            raise ValueError("retrieval weights must be non-negative")
        if semantic_weight == 0 and text_weight == 0:
            raise ValueError("at least one retrieval weight must be positive")
        self.bm25 = BM25Retriever(chunks)
        self.vector = VectorRetriever(chunks, embed)
        self.semantic_weight = semantic_weight
        self.text_weight = text_weight
        self.rrf_k = rrf_k

    def search(
        self, query: str, limit: int = 8, categories: set[str] | None = None
    ) -> list[Chunk]:
        candidate_limit = max(limit * 4, 20)
        text_hits = self.bm25.search(query, candidate_limit, categories)
        semantic_hits = self.vector.search(query, candidate_limit, categories)
        scores: dict[tuple[str, int], float] = {}
        chunks: dict[tuple[str, int], Chunk] = {}
        for weight, hits in (
            (self.text_weight, text_hits),
            (self.semantic_weight, semantic_hits),
        ):
            for rank, chunk in enumerate(hits, start=1):
                key = (chunk.source_id, chunk.chunk_index)
                chunks[key] = chunk
                scores[key] = scores.get(key, 0.0) + weight / (self.rrf_k + rank)
        ranked = sorted(scores, key=lambda key: (-scores[key], key))
        return [chunks[key] for key in ranked[:limit]]
