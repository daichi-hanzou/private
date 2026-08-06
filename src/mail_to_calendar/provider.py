from __future__ import annotations

from typing import Protocol

from .models import EmailMessage


class MailProvider(Protocol):
    def list_messages(self) -> list[EmailMessage]: ...

    def get_message(self, message_id: str) -> EmailMessage: ...
