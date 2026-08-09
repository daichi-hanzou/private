from __future__ import annotations

import base64
from dataclasses import dataclass
from datetime import datetime, timezone
from email.message import Message
from email.utils import getaddresses
from html.parser import HTMLParser
from typing import Any

from .models import EmailMessage, canonical_message_id


_MAX_BODY_CHARS = 50_000


class _TextHTMLParser(HTMLParser):
    """Extract visible text without executing or preserving HTML markup."""

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.fragments: list[str] = []
        self._hidden_depth = 0

    def handle_starttag(self, tag: str, _attrs: Any) -> None:
        if tag.casefold() in {"script", "style"}:
            self._hidden_depth += 1
        elif tag.casefold() in {"br", "p", "div", "li", "tr"}:
            self.fragments.append("\n")

    def handle_endtag(self, tag: str) -> None:
        if tag.casefold() in {"script", "style"} and self._hidden_depth:
            self._hidden_depth -= 1
        elif tag.casefold() in {"p", "div", "li", "tr"}:
            self.fragments.append("\n")

    def handle_data(self, data: str) -> None:
        if not self._hidden_depth:
            self.fragments.append(data)

    def text(self) -> str:
        return "\n".join(
            line.strip()
            for line in "".join(self.fragments).splitlines()
            if line.strip()
        )


@dataclass(frozen=True)
class GmailMessageSummary:
    message_id: str
    received_at: str
    sender: str
    subject: str


@dataclass(frozen=True)
class GmailProviderConfig:
    max_messages: int = 50
    received_after: datetime | None = None
    label_ids: tuple[str, ...] = ("INBOX",)

    def __post_init__(self) -> None:
        if not 1 <= self.max_messages <= 100:
            raise ValueError("Gmail max_messages must be between 1 and 100")
        if self.received_after is not None and self.received_after.tzinfo is None:
            raise ValueError("Gmail received_after must be timezone-aware")


class GmailProvider:
    """Read-only Gmail provider returning normalized common EmailMessage values."""

    def __init__(self, service: Any, config: GmailProviderConfig) -> None:
        self.service = service
        self.config = config
        self.client = GmailReadOnlyClient(service)
        self._messages: list[EmailMessage] | None = None

    def list_messages(self) -> list[EmailMessage]:
        if self._messages is not None:
            return list(self._messages)
        arguments: dict[str, Any] = {
            "userId": "me",
            "maxResults": self.config.max_messages,
            "includeSpamTrash": False,
            "labelIds": list(self.config.label_ids),
        }
        if self.config.received_after is not None:
            arguments["q"] = f"after:{int(self.config.received_after.timestamp())}"
        try:
            response = self.service.users().messages().list(**arguments).execute()
        except Exception as exc:
            raise RuntimeError("Gmail scheduled message list request failed") from exc
        references = response.get("messages", []) if isinstance(response, dict) else None
        if not isinstance(references, list):
            raise RuntimeError("Gmail scheduled message list response was malformed")
        values: list[EmailMessage] = []
        for reference in references[: self.config.max_messages]:
            if not isinstance(reference, dict) or not reference.get("id"):
                raise RuntimeError("Gmail scheduled message reference was malformed")
            values.append(self.client.get_message(str(reference["id"])))
        self._messages = values
        return list(values)

    def get_message(self, message_id: str) -> EmailMessage:
        raw_id = message_id.removeprefix("gmail:")
        return self.client.get_message(raw_id)


class ScheduledGmailProvider:
    """Lazily authenticate so scheduler can isolate Gmail provider failures."""

    def __init__(self, auth: Any, config: GmailProviderConfig) -> None:
        self.auth = auth
        self.config = config
        self._provider: GmailProvider | None = None

    def _value(self) -> GmailProvider:
        if self._provider is None:
            service = self.auth.build_service(interactive=False)
            self._provider = GmailProvider(service, self.config)
        return self._provider

    def list_messages(self) -> list[EmailMessage]:
        return self._value().list_messages()

    def get_message(self, message_id: str) -> EmailMessage:
        return self._value().get_message(message_id)


class GmailReadOnlyClient:
    """Fetch bounded Gmail metadata without retrieving message bodies."""

    def __init__(self, service: Any) -> None:
        self.service = service

    def list_messages(self, *, limit: int = 5) -> list[GmailMessageSummary]:
        if not 1 <= limit <= 100:
            raise ValueError("Gmail list limit must be between 1 and 100")
        try:
            response = self.service.users().messages().list(
                userId="me", maxResults=limit, includeSpamTrash=False
            ).execute()
        except Exception as exc:
            raise RuntimeError("Gmail message list request failed") from exc
        references = response.get("messages", [])
        if not isinstance(references, list):
            raise RuntimeError("Gmail message list response was malformed")
        summaries = []
        for reference in references[:limit]:
            if not isinstance(reference, dict) or not reference.get("id"):
                raise RuntimeError("Gmail message reference was malformed")
            try:
                message = self.service.users().messages().get(
                    userId="me",
                    id=str(reference["id"]),
                    format="metadata",
                    metadataHeaders=["From", "Subject"],
                ).execute()
            except Exception as exc:
                raise RuntimeError("Gmail message metadata request failed") from exc
            summaries.append(self._summary(message))
        return summaries

    def get_message(self, message_id: str) -> EmailMessage:
        """Fetch and normalize one Gmail message for local mail analysis."""
        message_id = message_id.strip()
        if not message_id:
            raise ValueError("Gmail message ID is required")
        if message_id.casefold().startswith("gmail:"):
            raise ValueError("Gmail message ID must not include the gmail: prefix")
        try:
            value = self.service.users().messages().get(
                userId="me", id=message_id, format="full"
            ).execute()
        except Exception as exc:
            raise RuntimeError("Gmail full message request failed") from exc
        return self._message(value, expected_id=message_id)

    @staticmethod
    def _summary(message: Any) -> GmailMessageSummary:
        if not isinstance(message, dict) or not message.get("id"):
            raise RuntimeError("Gmail message metadata response was malformed")
        payload = message.get("payload")
        headers = payload.get("headers", []) if isinstance(payload, dict) else []
        values: dict[str, str] = {}
        if isinstance(headers, list):
            for header in headers:
                if not isinstance(header, dict):
                    continue
                name = str(header.get("name") or "").casefold()
                if name in {"from", "subject"} and name not in values:
                    values[name] = " ".join(
                        str(header.get("value") or "").split()
                    )
        try:
            milliseconds = int(message["internalDate"])
            received = datetime.fromtimestamp(
                milliseconds / 1000, tz=timezone.utc
            ).isoformat()
        except (KeyError, TypeError, ValueError, OverflowError) as exc:
            raise RuntimeError("Gmail message received time was malformed") from exc
        return GmailMessageSummary(
            message_id=f"gmail:{message['id']}",
            received_at=received,
            sender=values.get("from", ""),
            subject=values.get("subject", ""),
        )

    @classmethod
    def _message(cls, value: Any, *, expected_id: str) -> EmailMessage:
        if not isinstance(value, dict) or str(value.get("id") or "") != expected_id:
            raise RuntimeError("Gmail full message response was malformed")
        payload = value.get("payload")
        if not isinstance(payload, dict):
            raise RuntimeError("Gmail full message payload was malformed")
        headers = cls._headers(payload)
        try:
            received_at = datetime.fromtimestamp(
                int(value["internalDate"]) / 1000, tz=timezone.utc
            ).isoformat()
        except (KeyError, TypeError, ValueError, OverflowError) as exc:
            raise RuntimeError("Gmail message received time was malformed") from exc
        plain, html, has_attachments = cls._body_parts(payload)
        if plain:
            body_text = "\n".join(plain)
        else:
            parser = _TextHTMLParser()
            parser.feed("\n".join(html))
            parser.close()
            body_text = parser.text()
        body_text = body_text[:_MAX_BODY_CHARS]
        recipients = [
            address
            for _name, address in getaddresses(
                [headers.get("to", ""), headers.get("cc", "")]
            )
            if address
        ]
        labels = [str(item) for item in value.get("labelIds", []) if item]
        thread_id = str(value.get("threadId") or "") or None
        return EmailMessage(
            provider="gmail",
            message_id=canonical_message_id("gmail", expected_id),
            sender=headers.get("from", ""),
            recipients=recipients,
            subject=headers.get("subject", ""),
            received_at=received_at,
            body_text=body_text,
            thread_id=f"gmail:{thread_id}" if thread_id else None,
            body_preview=body_text[:160] or None,
            labels=labels,
            importance_hint="important" if "IMPORTANT" in labels else None,
            has_attachments=has_attachments,
            metadata={},
        )

    @staticmethod
    def _headers(payload: dict[str, Any]) -> dict[str, str]:
        values: dict[str, str] = {}
        headers = payload.get("headers", [])
        if not isinstance(headers, list):
            return values
        for header in headers:
            if not isinstance(header, dict):
                continue
            name = str(header.get("name") or "").casefold()
            if name in {"from", "to", "cc", "subject", "content-type", "content-disposition"}:
                values.setdefault(name, " ".join(str(header.get("value") or "").split()))
        return values

    @classmethod
    def _body_parts(
        cls, payload: dict[str, Any]
    ) -> tuple[list[str], list[str], bool]:
        plain: list[str] = []
        html: list[str] = []
        has_attachments = False

        def visit(part: dict[str, Any]) -> None:
            nonlocal has_attachments
            headers = cls._headers(part)
            disposition = headers.get("content-disposition", "").casefold()
            filename = str(part.get("filename") or "").strip()
            body = part.get("body") if isinstance(part.get("body"), dict) else {}
            if filename or disposition.startswith("attachment"):
                has_attachments = True
                return
            children = part.get("parts")
            if isinstance(children, list):
                for child in children:
                    if isinstance(child, dict):
                        visit(child)
                return
            mime_type = str(part.get("mimeType") or "").casefold()
            if mime_type not in {"text/plain", "text/html"}:
                if body.get("attachmentId"):
                    has_attachments = True
                return
            # Large or attached MIME bodies require a separate attachment API call;
            # do not fetch them as analysis text.
            if body.get("attachmentId") or not isinstance(body.get("data"), str):
                return
            decoded = cls._decode(body["data"], headers.get("content-type", ""))
            if decoded:
                (plain if mime_type == "text/plain" else html).append(decoded)

        visit(payload)
        return plain, html, has_attachments

    @staticmethod
    def _decode(data: str, content_type: str) -> str:
        try:
            raw = base64.urlsafe_b64decode(data + "=" * (-len(data) % 4))
        except (ValueError, TypeError) as exc:
            raise RuntimeError("Gmail message body encoding was malformed") from exc
        header = Message()
        header["content-type"] = content_type or "text/plain; charset=utf-8"
        charset = header.get_content_charset() or "utf-8"
        try:
            return raw.decode(charset, errors="replace")
        except LookupError:
            return raw.decode("utf-8", errors="replace")
