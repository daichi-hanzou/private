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


_REPLY_PREFIX = re.compile(r"^\s*(?:(?:re|fw|fwd)\s*[:：]\s*)+", re.I)
_TITLE_TOKEN = re.compile(r"[0-9a-z]+|[一-龥々〆ヵヶぁ-んァ-ヶー]+", re.I)
_TITLE_GENERIC_TOKENS = frozenset({
    "重要", "お知らせ", "ご案内", "案内", "確認", "メール",
    "予約", "予定", "通知", "について", "the", "a", "an",
})


def title_grounding_projection(value: str) -> str:
    """Return a punctuation/width-insensitive projection for title matching."""
    normalized = _REPLY_PREFIX.sub("", nfkc_text(value).casefold())
    return "".join(
        character
        for character in normalized
        if not unicodedata.category(character).startswith(("P", "S", "Z", "C"))
    )


def title_grounding_tokens(value: str) -> tuple[str, ...]:
    """Return bounded lexical chunks without attempting semantic similarity."""
    normalized = _REPLY_PREFIX.sub("", nfkc_text(value).casefold())
    separated = "".join(
        character
        if not unicodedata.category(character).startswith(("P", "S", "Z", "C"))
        else " "
        for character in normalized
    )
    return tuple(
        token for token in _TITLE_TOKEN.findall(separated)
        if len(token) >= 2 and token not in _TITLE_GENERIC_TOKENS
    )


def title_grounding_status(title: str, subject: str, body: str) -> str:
    """Return ``grounded``, ``relaxed``, or ``not_grounded`` deterministically."""
    title_projection = title_grounding_projection(title)
    if not title_projection:
        return "not_grounded"
    source_projections = (
        title_grounding_projection(subject), title_grounding_projection(body)
    )
    if any(
        title_projection in source
        or (len(source) >= 4 and source in title_projection)
        for source in source_projections if source
    ):
        return "grounded"
    tokens = title_grounding_tokens(title)
    if not tokens:
        return "not_grounded"
    searchable = title_grounding_projection(f"{subject}\n{body}")
    matched = tuple(token for token in tokens if token in searchable)
    total_weight = sum(len(token) for token in tokens)
    matched_weight = sum(len(token) for token in matched)
    if (
        matched_weight / total_weight >= 0.7
        and (len(matched) >= 2 or matched_weight >= 8)
    ):
        return "relaxed"
    return "not_grounded"


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
