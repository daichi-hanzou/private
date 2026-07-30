from __future__ import annotations

import math
import re
from collections import Counter
from typing import Iterable

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
