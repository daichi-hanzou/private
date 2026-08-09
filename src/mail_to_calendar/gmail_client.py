from __future__ import annotations

import base64
import codecs
import re
from dataclasses import dataclass
from datetime import datetime, timezone
from email.message import Message
from email.utils import getaddresses
from html.parser import HTMLParser
from typing import Any

from .models import EmailMessage, canonical_message_id
from .text_normalization import (
    contains_japanese_explicit_date, date_detection_text, nfkc_text,
)


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
class _BodySelection:
    text: str
    mime_type: str
    charset: str
    charset_source: str
    decode_errors: bool


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
        selection, has_attachments, mime_diagnostics = cls._select_body(payload)
        body_text = (selection.text if selection else "")[:_MAX_BODY_CHARS]
        mime_diagnostics["selected_body_length"] = len(body_text)
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
            metadata={"gmail_mime": mime_diagnostics},
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
    def _select_body(
        cls, payload: dict[str, Any]
    ) -> tuple[_BodySelection | None, bool, dict[str, Any]]:
        has_attachments = False
        diagnostics: dict[str, Any] = {
            "mime_type": str(payload.get("mimeType") or "unknown"),
            "plain_text_parts": 0,
            "html_parts": 0,
            "plain_parts_with_data": 0,
            "html_parts_with_data": 0,
            "attachment_id_only_parts": 0,
            "selected_part_type": "none",
            "selected_body_length": 0,
            "plain_contains_date": False,
            "html_contains_date": False,
            "plain_contains_time": False,
            "html_contains_time": False,
            "plain_visible_length": 0,
            "html_visible_length": 0,
            "selection_reason": "no_supported_body_part",
            "selected_charset": "none",
            "charset_source": "unavailable",
            "decode_errors": False,
            "plain_contains_8_month": False,
            "plain_contains_10_day": False,
            "plain_contains_exact_august_10": False,
            "plain_nfkc_contains_exact_august_10": False,
            "html_contains_8_month": False,
            "html_contains_10_day": False,
            "html_contains_exact_august_10": False,
            "html_nfkc_contains_exact_august_10": False,
        }

        def visit(part: dict[str, Any]) -> _BodySelection | None:
            nonlocal has_attachments
            headers = cls._headers(part)
            disposition = headers.get("content-disposition", "").casefold()
            filename = str(part.get("filename") or "").strip()
            body = part.get("body") if isinstance(part.get("body"), dict) else {}
            if filename or disposition.startswith("attachment"):
                has_attachments = True
                return None
            children = part.get("parts")
            if isinstance(children, list):
                selections = [
                    selected
                    for child in children
                    if isinstance(child, dict)
                    and (selected := visit(child)) is not None
                    and selected.text.strip()
                ]
                mime_type = str(part.get("mimeType") or "").casefold()
                if mime_type == "multipart/alternative":
                    return cls._choose_alternative(selections, diagnostics)
                if not selections:
                    return None
                combined = "\n".join(item.text for item in selections if item.text)
                selected_types = {item.mime_type for item in selections}
                selected_charsets = {item.charset for item in selections}
                charset_sources = {item.charset_source for item in selections}
                return _BodySelection(
                    combined,
                    next(iter(selected_types))
                    if len(selected_types) == 1 else mime_type or "multipart/mixed",
                    next(iter(selected_charsets))
                    if len(selected_charsets) == 1 else "mixed",
                    next(iter(charset_sources))
                    if len(charset_sources) == 1 else "mixed",
                    any(item.decode_errors for item in selections),
                )
            mime_type = str(part.get("mimeType") or "").casefold()
            if mime_type not in {"text/plain", "text/html"}:
                if body.get("attachmentId"):
                    has_attachments = True
                    diagnostics["attachment_id_only_parts"] += 1
                return None
            count_key = "plain_text_parts" if mime_type == "text/plain" else "html_parts"
            diagnostics[count_key] += 1
            # Large or attached MIME bodies require a separate attachment API call;
            # do not fetch them as analysis text.
            if body.get("attachmentId"):
                diagnostics["attachment_id_only_parts"] += 1
                return None
            if not isinstance(body.get("data"), str):
                return None
            data_key = (
                "plain_parts_with_data"
                if mime_type == "text/plain" else "html_parts_with_data"
            )
            diagnostics[data_key] += 1
            decoded, charset, charset_source, decode_errors = cls._decode(
                body["data"], headers.get("content-type", "")
            )
            if not decoded.strip():
                return None
            if mime_type == "text/html":
                parser = _TextHTMLParser()
                parser.feed(decoded)
                parser.close()
                decoded = parser.text()
            return _BodySelection(
                decoded, mime_type, charset, charset_source, decode_errors
            )

        selected = visit(payload)
        if selected is not None:
            diagnostics["selected_part_type"] = selected.mime_type
            diagnostics["selected_charset"] = selected.charset
            diagnostics["charset_source"] = selected.charset_source
            diagnostics["decode_errors"] = selected.decode_errors
            if diagnostics["selection_reason"] == "no_supported_body_part":
                diagnostics["selection_reason"] = (
                    "single_plain_part"
                    if selected.mime_type == "text/plain"
                    else "single_html_part"
                    if selected.mime_type == "text/html"
                    else "multipart_body_combined"
                )
        return selected, has_attachments, diagnostics

    @classmethod
    def _choose_alternative(
        cls, selections: list[_BodySelection], diagnostics: dict[str, Any]
    ) -> _BodySelection | None:
        if not selections:
            return None
        plain = max(
            (item for item in selections if item.mime_type == "text/plain"),
            key=lambda item: len(item.text.strip()),
            default=None,
        )
        html = max(
            (item for item in selections if item.mime_type == "text/html"),
            key=lambda item: len(item.text.strip()),
            default=None,
        )
        if plain is None:
            diagnostics["selection_reason"] = (
                "html_fallback_plain_empty"
                if diagnostics["plain_text_parts"]
                else "html_only_alternative"
            )
            return html or max(selections, key=lambda item: len(item.text.strip()))
        if html is None:
            diagnostics["selection_reason"] = "plain_only_alternative"
            return plain
        plain_text = plain.text.strip()
        html_text = html.text.strip()
        plain_date = cls._contains_date(plain_text)
        html_date = cls._contains_date(html_text)
        plain_time = cls._contains_time(plain_text)
        html_time = cls._contains_time(html_text)
        diagnostics.update(
            plain_contains_date=plain_date,
            html_contains_date=html_date,
            plain_contains_time=plain_time,
            html_contains_time=html_time,
            plain_visible_length=len(plain_text),
            html_visible_length=len(html_text),
            plain_contains_8_month="8月" in plain_text,
            plain_contains_10_day="10日" in plain_text,
            plain_contains_exact_august_10="8月10日" in plain_text,
            plain_nfkc_contains_exact_august_10=(
                "8月10日" in nfkc_text(plain_text)
            ),
            html_contains_8_month="8月" in html_text,
            html_contains_10_day="10日" in html_text,
            html_contains_exact_august_10="8月10日" in html_text,
            html_nfkc_contains_exact_august_10=(
                "8月10日" in nfkc_text(html_text)
            ),
        )
        if cls._is_footer_only(plain_text):
            diagnostics["selection_reason"] = "html_fallback_plain_footer_only"
            return html
        diagnostics["selection_reason"] = "plain_preferred_valid_alternative"
        return plain

    @staticmethod
    def _contains_date(text: str) -> bool:
        normalized = date_detection_text(text)
        return contains_japanese_explicit_date(normalized) or bool(re.search(
            r"(?<!\d)\d{4}(?:年|[-/])\d{1,2}(?:月|[-/])\d{1,2}日?"
            r"|(?<![\d/])\d{1,2}/\d{1,2}(?![\d/])"
            r"|(?:今日|本日|明日|明後日|今週|来週)(?:の)?",
            normalized,
            re.I,
        ))

    @staticmethod
    def _contains_time(text: str) -> bool:
        return bool(re.search(
            r"(?:午前|午後)?\s*\d{1,2}(?::\d{2}|時(?:\d{1,2}分)?)",
            text,
        ))

    @staticmethod
    def _is_footer_only(text: str) -> bool:
        folded = text.casefold()
        footer_markers = (
            "自動送信", "このメールは自動", "sent from outlook",
            "unsubscribe", "配信停止", "privacy policy", "プライバシー",
            "view in browser", "ブラウザで表示",
        )
        if not any(marker in folded for marker in footer_markers):
            return False
        remainder = folded
        for marker in footer_markers:
            remainder = remainder.replace(marker, " ")
        remainder = re.sub(r"https?://\S+|\b\S+@\S+\b", " ", remainder)
        remainder = re.sub(r"[^a-z0-9一-龥ぁ-んァ-ヶ]+", "", remainder)
        return len(remainder) < 10

    @staticmethod
    def _decode(
        data: str, content_type: str
    ) -> tuple[str, str, str, bool]:
        try:
            raw = base64.urlsafe_b64decode(data + "=" * (-len(data) % 4))
        except (ValueError, TypeError) as exc:
            raise RuntimeError("Gmail message body encoding was malformed") from exc
        header = Message()
        header["content-type"] = content_type or "text/plain; charset=utf-8"
        declared = header.get_content_charset() if content_type else None
        declared_name = None
        declared_failed = False
        if declared:
            try:
                declared_name = codecs.lookup(declared).name
            except LookupError:
                declared_failed = True
            else:
                try:
                    return raw.decode(declared_name, errors="strict"), declared_name, "MIME", False
                except UnicodeDecodeError:
                    declared_failed = True
        candidates = (
            ("iso-2022-jp", "utf-8", "cp932", "shift_jis", "euc-jp")
            if b"\x1b$" in raw or b"\x1b(" in raw
            else ("utf-8", "iso-2022-jp", "cp932", "shift_jis", "euc-jp")
        )
        for candidate in candidates:
            normalized = codecs.lookup(candidate).name
            if normalized == declared_name:
                continue
            try:
                return (
                    raw.decode(normalized, errors="strict"),
                    normalized,
                    "fallback",
                    declared_failed,
                )
            except UnicodeDecodeError:
                continue
        fallback = "utf-8"
        try:
            fallback = codecs.lookup(declared or "utf-8").name
        except LookupError:
            pass
        return raw.decode(fallback, errors="replace"), fallback, "fallback", True
