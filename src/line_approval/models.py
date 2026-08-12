from __future__ import annotations

from dataclasses import dataclass
from typing import Any


@dataclass(frozen=True)
class LineConfig:
    channel_secret: str
    channel_access_token: str
    allowed_user_id: str
    calendar_id: str = "primary"


@dataclass(frozen=True)
class LineSendResult:
    success: bool
    retryable: bool = False
    error_type: str | None = None
    error_message: str | None = None


@dataclass(frozen=True)
class WebhookResult:
    status_code: int
    message: str
    processed: bool = False


MessagePayload = dict[str, Any]


@dataclass(frozen=True)
class ImportantDispatchResult:
    selected: int
    sent: int
    failed: int
    skipped: int
