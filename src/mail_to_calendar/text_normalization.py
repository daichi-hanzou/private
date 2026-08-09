from __future__ import annotations

import re
import unicodedata


def nfkc_text(value: str) -> str:
    """Return a Unicode compatibility projection without mutating source text."""
    return unicodedata.normalize("NFKC", value)


def date_detection_text(value: str) -> str:
    """Normalize width and date-adjacent whitespace for deterministic matching."""
    normalized = re.sub(r"\s+", " ", nfkc_text(value))
    normalized = re.sub(r"(?<=\d)\s+(?=月)", "", normalized)
    normalized = re.sub(r"(?<=月)\s+(?=\d)", "", normalized)
    normalized = re.sub(r"(?<=\d)\s+(?=日)", "", normalized)
    return normalized


def contains_japanese_explicit_date(value: str) -> bool:
    return bool(re.search(
        r"(?<!\d)\d{1,2}月\d{1,2}日", date_detection_text(value)
    ))


_TRANSPORT_HEADER = re.compile(
    r"^\s*(?:差出人|送信日時|宛先|件名|from|sent|to|cc|subject)\s*[:：]",
    re.IGNORECASE,
)
_FORWARD_SEPARATOR = re.compile(
    r"^\s*(?:[-_]{5,}|[-_ ]*(?:original|forwarded) message[-_ ]*)\s*$",
    re.IGNORECASE,
)


def without_transport_headers(value: str) -> str:
    """Remove forwarded-message transport header blocks, preserving its body.

    A lone header-like line is kept. At least two adjacent transport fields, or
    a recognized forward separator followed by a field, are required so normal
    prose such as ``件名: ...`` is not deleted accidentally.
    """
    lines = value.splitlines(keepends=True)
    remove: set[int] = set()
    index = 0
    while index < len(lines):
        separator = bool(_FORWARD_SEPARATOR.match(lines[index].rstrip("\r\n")))
        first = index + 1 if separator else index
        cursor = first
        header_indexes: list[int] = []
        while cursor < len(lines):
            current = lines[cursor].rstrip("\r\n")
            if _TRANSPORT_HEADER.match(current):
                header_indexes.append(cursor)
                cursor += 1
                continue
            break
        header_count = sum(
            bool(_TRANSPORT_HEADER.match(lines[item].rstrip("\r\n")))
            for item in header_indexes
        )
        if header_indexes and (header_count >= 2 or separator):
            remove.update(header_indexes)
            if separator:
                remove.add(index)
            index = cursor
            continue
        index += 1
    return "".join(line for item, line in enumerate(lines) if item not in remove)
