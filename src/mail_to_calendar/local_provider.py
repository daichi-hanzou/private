from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from .models import EmailMessage


class LocalMailProvider:
    """Load provider-neutral email records from a local JSON or JSONL file."""

    def __init__(self, path: str | Path) -> None:
        self.path = Path(path)
        self._messages: list[EmailMessage] | None = None

    def list_messages(self) -> list[EmailMessage]:
        if self._messages is None:
            self._messages = [
                self._to_message(value) for value in self._read_values()
            ]
        return list(self._messages)

    def get_message(self, message_id: str) -> EmailMessage:
        matches = [
            message
            for message in self.list_messages()
            if message.message_id == message_id
        ]
        if not matches:
            raise KeyError(f"message not found: {message_id}")
        if len(matches) > 1:
            raise ValueError(f"duplicate message ID: {message_id}")
        return matches[0]

    def _read_values(self) -> list[dict[str, Any]]:
        if self.path.suffix.lower() == ".jsonl":
            values = []
            with self.path.open(encoding="utf-8") as handle:
                for line_number, line in enumerate(handle, start=1):
                    if not line.strip():
                        continue
                    try:
                        value = json.loads(line)
                    except json.JSONDecodeError as exc:
                        raise ValueError(
                            f"invalid JSON on line {line_number}: {exc.msg}"
                        ) from exc
                    if not isinstance(value, dict):
                        raise ValueError(
                            f"message on line {line_number} must be an object"
                        )
                    values.append(value)
            return values
        value = json.loads(self.path.read_text(encoding="utf-8"))
        if isinstance(value, dict):
            value = [value]
        if not isinstance(value, list) or not all(
            isinstance(item, dict) for item in value
        ):
            raise ValueError("JSON input must be an object or an array")
        return value

    @staticmethod
    def _to_message(value: dict[str, Any]) -> EmailMessage:
        required = (
            "provider",
            "message_id",
            "sender",
            "recipients",
            "subject",
            "received_at",
            "body_text",
        )
        missing = [key for key in required if key not in value]
        if missing:
            raise ValueError(
                "message is missing required fields: " + ", ".join(missing)
            )
        return EmailMessage(
            provider=str(value["provider"]),
            message_id=str(value["message_id"]),
            thread_id=(
                str(value["thread_id"])
                if value.get("thread_id") is not None
                else None
            ),
            sender=str(value["sender"]),
            recipients=[str(item) for item in value["recipients"]],
            subject=str(value["subject"]),
            received_at=str(value["received_at"]),
            body_text=str(value["body_text"]),
            body_preview=(
                str(value["body_preview"])
                if value.get("body_preview") is not None
                else None
            ),
            labels=[str(item) for item in value.get("labels", [])],
            importance_hint=(
                str(value["importance_hint"])
                if value.get("importance_hint") is not None
                else None
            ),
            has_attachments=bool(value.get("has_attachments", False)),
            metadata=(
                dict(value["metadata"])
                if isinstance(value.get("metadata"), dict)
                else {}
            ),
        )
