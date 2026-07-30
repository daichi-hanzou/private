from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any


@dataclass(frozen=True)
class Document:
    source_id: str
    company: str
    category: str
    path: Path
    text: str
    page: int | None = None


@dataclass(frozen=True)
class Chunk:
    source_id: str
    company: str
    category: str
    path: Path
    text: str
    page: int | None
    chunk_index: int

    def citation(self) -> dict[str, Any]:
        return {
            "source_id": self.source_id,
            "file": self.path.as_posix(),
            "page": self.page,
            "category": self.category,
        }
