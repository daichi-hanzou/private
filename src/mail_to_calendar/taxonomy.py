from __future__ import annotations


CLASSIFICATIONS = (
    "calendar_candidate",
    "transactional",
    "security_notification",
    "ignored",
    "invalid",
    "clarification_required",
)
CLASSIFICATION_SET = frozenset(CLASSIFICATIONS)
LEGACY_CLASSIFICATION_ALIASES = {
    "informational": "ignored",
    "promotion": "ignored",
}


def normalize_classification(value: str | None) -> str | None:
    """Project legacy audit values into the current taxonomy without mutation."""
    if value is None:
        return None
    normalized = value.strip().casefold()
    return LEGACY_CLASSIFICATION_ALIASES.get(normalized, normalized)


def classifications_equivalent(left: str | None, right: str | None) -> bool:
    return normalize_classification(left) == normalize_classification(right)
