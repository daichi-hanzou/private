from __future__ import annotations

import html
import time
from dataclasses import dataclass, field
from datetime import datetime
from html.parser import HTMLParser
from pathlib import Path
from typing import Any, Protocol
from urllib.parse import quote, urlparse

import requests

from .microsoft_auth import TokenProvider
from .models import EmailMessage, canonical_message_id


GRAPH_ROOT = "https://graph.microsoft.com/v1.0"
MESSAGE_FIELDS = (
    "id",
    "conversationId",
    "subject",
    "from",
    "toRecipients",
    "receivedDateTime",
    "body",
    "bodyPreview",
    "categories",
    "importance",
    "hasAttachments",
    "isRead",
    "internetMessageId",
)


class OutlookProviderError(RuntimeError):
    """A safe error raised for authentication or Graph failures."""


@dataclass(frozen=True)
class OutlookProviderConfig:
    client_id: str
    authority: str = "https://login.microsoftonline.com/consumers"
    scopes: list[str] = field(default_factory=lambda: ["Mail.Read"])
    token_cache_path: Path = Path(
        "~/.config/agentledger/microsoft_token_cache.json"
    )
    folder: str = "inbox"
    max_messages: int = 20
    unread_only: bool = True
    received_after: datetime | None = None
    include_body: bool = True
    body_max_chars: int = 10_000
    request_timeout_seconds: int = 30
    max_pages: int = 10
    max_retries: int = 2

    def __post_init__(self) -> None:
        if not self.client_id.strip():
            raise ValueError("Microsoft client ID is required")
        if set(self.scopes) != {"Mail.Read"}:
            raise ValueError("OutlookProvider allows only delegated Mail.Read")
        authority = urlparse(self.authority)
        if (
            authority.scheme != "https"
            or authority.hostname != "login.microsoftonline.com"
            or authority.path.rstrip("/") != "/consumers"
        ):
            raise ValueError(
                "authority must be the personal Microsoft account "
                "consumers endpoint"
            )
        if not 1 <= self.max_messages <= 100:
            raise ValueError("max_messages must be between 1 and 100")
        if self.body_max_chars < 0:
            raise ValueError("body_max_chars must not be negative")
        if self.request_timeout_seconds <= 0:
            raise ValueError("request timeout must be positive")
        if not 1 <= self.max_pages <= 20:
            raise ValueError("max_pages must be between 1 and 20")
        if not 0 <= self.max_retries <= 5:
            raise ValueError("max_retries must be between 0 and 5")
        object.__setattr__(
            self,
            "token_cache_path",
            Path(self.token_cache_path).expanduser(),
        )


class HttpResponse(Protocol):
    status_code: int
    headers: dict[str, str]

    def json(self) -> Any: ...


class HttpTransport(Protocol):
    def get(
        self,
        url: str,
        *,
        headers: dict[str, str],
        params: dict[str, str | int] | None,
        timeout: int,
    ) -> HttpResponse: ...


class RequestsTransport:
    """GET-only Graph transport; no mailbox mutation method is exposed."""

    def __init__(self) -> None:
        self.session = requests.Session()

    def get(
        self,
        url: str,
        *,
        headers: dict[str, str],
        params: dict[str, str | int] | None,
        timeout: int,
    ) -> HttpResponse:
        return self.session.get(
            url,
            headers=headers,
            params=params,
            timeout=timeout,
        )


class _SafeHTMLTextExtractor(HTMLParser):
    _ignored = {"script", "style"}
    _blocks = {"br", "p", "div", "li", "tr", "h1", "h2", "h3", "h4"}

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.parts: list[str] = []
        self.ignored_depth = 0

    def handle_starttag(
        self,
        tag: str,
        attrs: list[tuple[str, str | None]],
    ) -> None:
        del attrs
        if tag.casefold() in self._ignored:
            self.ignored_depth += 1
        elif not self.ignored_depth and tag.casefold() in self._blocks:
            self.parts.append("\n")

    def handle_endtag(self, tag: str) -> None:
        if tag.casefold() in self._ignored and self.ignored_depth:
            self.ignored_depth -= 1
        elif not self.ignored_depth and tag.casefold() in self._blocks:
            self.parts.append("\n")

    def handle_data(self, data: str) -> None:
        if not self.ignored_depth:
            self.parts.append(data)

    def text(self) -> str:
        lines = [" ".join(line.split()) for line in "".join(self.parts).splitlines()]
        return "\n".join(line for line in lines if line)


def body_to_text(content: str, content_type: str, *, limit: int) -> str:
    if content_type.casefold() != "html":
        return " ".join(content.split())[:limit]
    parser = _SafeHTMLTextExtractor()
    try:
        parser.feed(content)
        parser.close()
    except Exception as exc:
        raise OutlookProviderError("Outlook HTML body could not be converted") from exc
    return html.unescape(parser.text())[:limit]


class OutlookProvider:
    preview_limit = 160

    def __init__(
        self,
        config: OutlookProviderConfig,
        *,
        authenticator: TokenProvider,
        transport: HttpTransport | None = None,
        sleep: Any = time.sleep,
    ) -> None:
        self.config = config
        self.authenticator = authenticator
        self.transport = transport or RequestsTransport()
        self.sleep = sleep
        self._messages: list[EmailMessage] | None = None
        self.conversion_errors: list[str] = []

    @property
    def authenticated_account(self) -> str | None:
        return self.authenticator.last_account

    def list_messages(self) -> list[EmailMessage]:
        if self._messages is not None:
            return list(self._messages)
        token = self.authenticator.acquire_token()
        url = (
            f"{GRAPH_ROOT}/me/mailFolders/"
            f"{quote(self.config.folder, safe='')}/messages"
        )
        params = self._list_params()
        messages: list[EmailMessage] = []
        visited: set[str] = set()
        pages = 0
        while url and len(messages) < self.config.max_messages:
            self._validate_graph_url(url)
            if url in visited:
                raise OutlookProviderError("Microsoft Graph paging loop detected")
            visited.add(url)
            pages += 1
            if pages > self.config.max_pages:
                raise OutlookProviderError("Microsoft Graph page limit exceeded")
            payload = self._get_json(url, token=token, params=params)
            params = None
            values = payload.get("value")
            if not isinstance(values, list):
                raise OutlookProviderError("Microsoft Graph response has no message list")
            for raw in values:
                if len(messages) >= self.config.max_messages:
                    break
                try:
                    messages.append(self._to_message(raw))
                except (OutlookProviderError, TypeError, ValueError):
                    message_id = raw.get("id") if isinstance(raw, dict) else None
                    self.conversion_errors.append(
                        f"message conversion failed: {message_id or 'unknown'}"
                    )
            next_link = payload.get("@odata.nextLink")
            url = str(next_link) if next_link else ""
        self._messages = messages
        return list(messages)

    def get_message(self, message_id: str) -> EmailMessage:
        if not message_id:
            raise ValueError("message_id is required")
        token = self.authenticator.acquire_token()
        raw_message_id = message_id.removeprefix("outlook:")
        url = f"{GRAPH_ROOT}/me/messages/{quote(raw_message_id, safe='')}"
        payload = self._get_json(
            url,
            token=token,
            params={"$select": self._select_fields()},
        )
        return self._to_message(payload)

    def _list_params(self) -> dict[str, str | int]:
        params: dict[str, str | int] = {
            "$select": self._select_fields(),
            "$top": min(self.config.max_messages, 100),
        }
        filters = []
        if self.config.received_after is not None:
            filters.append(
                "receivedDateTime ge "
                + self.config.received_after.isoformat()
            )
        if self.config.unread_only:
            filters.append("isRead eq false")
        if filters:
            params["$filter"] = " and ".join(filters)
            params["$orderby"] = "receivedDateTime asc"
        else:
            params["$orderby"] = "receivedDateTime desc"
        return params

    def _select_fields(self) -> str:
        fields = [field for field in MESSAGE_FIELDS if field != "body"]
        if self.config.include_body:
            fields.append("body")
        return ",".join(fields)

    def _get_json(
        self,
        url: str,
        *,
        token: str,
        params: dict[str, str | int] | None,
    ) -> dict[str, Any]:
        headers = {
            "Authorization": f"Bearer {token}",
            "Accept": "application/json",
            "Prefer": 'outlook.body-content-type="html"',
        }
        for attempt in range(self.config.max_retries + 1):
            try:
                response = self.transport.get(
                    url,
                    headers=headers,
                    params=params,
                    timeout=self.config.request_timeout_seconds,
                )
            except (requests.Timeout, TimeoutError) as exc:
                if attempt >= self.config.max_retries:
                    raise OutlookProviderError(
                        "Microsoft Graph request timed out"
                    ) from exc
                self.sleep(2**attempt)
                continue
            status = response.status_code
            if status == 200:
                try:
                    payload = response.json()
                except (ValueError, TypeError) as exc:
                    raise OutlookProviderError(
                        "Microsoft Graph returned invalid JSON"
                    ) from exc
                if not isinstance(payload, dict):
                    raise OutlookProviderError(
                        "Microsoft Graph returned invalid JSON"
                    )
                return payload
            if status == 429 and attempt < self.config.max_retries:
                retry_after = response.headers.get("Retry-After", "1")
                try:
                    delay = max(0, min(int(retry_after), 60))
                except ValueError:
                    delay = 1
                self.sleep(delay)
                continue
            if 500 <= status < 600 and attempt < self.config.max_retries:
                self.sleep(2**attempt)
                continue
            messages = {
                401: "Microsoft Graph authentication was rejected (401)",
                403: "Microsoft Graph permission was denied (403)",
                404: "Outlook folder or message was not found (404)",
                429: "Microsoft Graph rate limit exceeded (429)",
            }
            raise OutlookProviderError(
                messages.get(status, f"Microsoft Graph request failed ({status})")
            )
        raise OutlookProviderError("Microsoft Graph request failed")

    def _to_message(self, raw: Any) -> EmailMessage:
        if not isinstance(raw, dict) or not raw.get("id"):
            raise OutlookProviderError("Outlook message is missing id")
        sender = self._address(raw.get("from"))
        recipients = [
            address
            for item in raw.get("toRecipients") or []
            if (address := self._address(item))
        ]
        body = raw.get("body")
        body = body if isinstance(body, dict) else {}
        content = str(body.get("content") or "")
        content_type = str(body.get("contentType") or "text")
        body_text = (
            body_to_text(
                content,
                content_type,
                limit=self.config.body_max_chars,
            )
            if self.config.include_body
            else ""
        )
        preview = raw.get("bodyPreview")
        return EmailMessage(
            provider="outlook",
            message_id=canonical_message_id("outlook", str(raw["id"])),
            thread_id=(
                str(raw["conversationId"])
                if raw.get("conversationId") is not None
                else None
            ),
            sender=sender,
            recipients=recipients,
            subject=str(raw.get("subject") or ""),
            received_at=str(raw.get("receivedDateTime") or ""),
            body_text=body_text,
            body_preview=(
                str(preview)[: self.preview_limit]
                if preview is not None
                else None
            ),
            labels=[str(value) for value in raw.get("categories") or []],
            importance_hint=(
                str(raw["importance"])
                if raw.get("importance") is not None
                else None
            ),
            has_attachments=bool(raw.get("hasAttachments", False)),
            metadata={
                "internet_message_id": raw.get("internetMessageId"),
                "is_read": raw.get("isRead"),
            },
        )

    @staticmethod
    def _address(value: Any) -> str:
        if not isinstance(value, dict):
            return ""
        email_address = value.get("emailAddress")
        if not isinstance(email_address, dict):
            return ""
        return str(email_address.get("address") or "")

    @staticmethod
    def _validate_graph_url(url: str) -> None:
        parsed = urlparse(url)
        if (
            parsed.scheme != "https"
            or parsed.hostname != "graph.microsoft.com"
            or parsed.port not in {None, 443}
            or not parsed.path.startswith("/v1.0/")
        ):
            raise OutlookProviderError("Microsoft Graph returned an unsafe nextLink")
