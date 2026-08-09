from __future__ import annotations

from typing import Any

import requests

from .models import LineSendResult, MessagePayload


class LineMessagingClient:
    API_BASE = "https://api.line.me/v2/bot/message"

    def __init__(self, access_token: str, *, timeout_seconds: float = 10) -> None:
        if not access_token:
            raise ValueError("LINE channel access token is required")
        self._access_token = access_token
        self.timeout_seconds = timeout_seconds

    def push(self, user_id: str, message: MessagePayload) -> LineSendResult:
        return self._send("push", {"to": user_id, "messages": [message]})

    def reply(self, reply_token: str, message: MessagePayload) -> LineSendResult:
        return self._send(
            "reply", {"replyToken": reply_token, "messages": [message]}
        )

    def _send(self, endpoint: str, payload: dict[str, Any]) -> LineSendResult:
        try:
            response = requests.post(
                f"{self.API_BASE}/{endpoint}",
                headers={
                    "Authorization": f"Bearer {self._access_token}",
                    "Content-Type": "application/json",
                },
                json=payload,
                timeout=self.timeout_seconds,
            )
        except (requests.Timeout, requests.ConnectionError) as exc:
            return LineSendResult(
                False, True, type(exc).__name__, "LINE API request failed"
            )
        if 200 <= response.status_code < 300:
            return LineSendResult(True)
        retryable = response.status_code == 429 or response.status_code >= 500
        return LineSendResult(
            False,
            retryable,
            f"HTTP{response.status_code}",
            "LINE API temporarily unavailable" if retryable else "LINE API rejected request",
        )


def text_message(text: str) -> MessagePayload:
    return {"type": "text", "text": text[:5000]}
