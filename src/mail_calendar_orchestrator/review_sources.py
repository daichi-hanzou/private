from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

from calendar_agent.audit import read_complete_jsonl
from mail_to_calendar.gmail_client import GmailReadOnlyClient
from mail_to_calendar.gmail_auth import GmailReadOnlyAuth
from mail_to_calendar.local_provider import LocalMailProvider
from mail_to_calendar.models import EmailMessage
from mail_to_calendar.outlook_provider import (
    OutlookProvider, OutlookProviderConfig,
)
from mail_to_calendar.microsoft_auth import MicrosoftAuthenticator
from mail_to_calendar.text_normalization import without_transport_headers


@dataclass(frozen=True)
class ReviewSourceConfig:
    outlook_client_id: str | None = None
    outlook_authority: str = "https://login.microsoftonline.com/consumers"
    outlook_token_cache: Path = Path(
        "~/.config/agentledger/microsoft_token_cache.json"
    )
    gmail_credentials: Path = Path(
        "~/.config/agentledger/google_credentials.json"
    )
    gmail_token_cache: Path = Path(
        "~/.config/agentledger/google_token.json"
    )


class MessageResolver:
    def __init__(self, config: ReviewSourceConfig) -> None:
        self.config = config
        self._local: dict[str, LocalMailProvider] = {}
        self._gmail: GmailReadOnlyClient | None = None
        self._outlook: OutlookProvider | None = None

    def get_message(
        self, *, provider: str, message_id: str, message_source_ref: str | None
    ) -> EmailMessage:
        if message_source_ref:
            return self._local_provider(message_source_ref).get_message(message_id)
        if provider == "gmail":
            return self._gmail_client().get_message(message_id.removeprefix("gmail:"))
        if provider == "outlook":
            return self._outlook_provider().get_message(message_id)
        raise ValueError(
            f"no review message source is configured for provider: {provider}"
        )

    def retrieval_availability(
        self, *, provider: str, message_source_ref: str | None
    ) -> tuple[bool, str]:
        """Check local configuration only; never contact a mail provider."""
        if message_source_ref:
            path = Path(message_source_ref).expanduser()
            return (
                path.is_file(),
                "message_source_ref_found" if path.is_file()
                else "message_source_ref_missing",
            )
        if provider == "gmail":
            credentials = self.config.gmail_credentials.expanduser().is_file()
            token = self.config.gmail_token_cache.expanduser().is_file()
            if credentials and token:
                return True, "gmail_readonly_credentials_available"
            missing = []
            if not credentials:
                missing.append("gmail_credentials")
            if not token:
                missing.append("gmail_token_cache")
            return False, "missing_" + "_and_".join(missing)
        if provider == "outlook":
            client = bool(self.config.outlook_client_id)
            token = self.config.outlook_token_cache.expanduser().is_file()
            if client and token:
                return True, "outlook_readonly_credentials_available"
            missing = []
            if not client:
                missing.append("outlook_client_id")
            if not token:
                missing.append("outlook_token_cache")
            return False, "missing_" + "_and_".join(missing)
        return False, "unsupported_provider_without_message_source_ref"

    @staticmethod
    def analysis_text(message: EmailMessage) -> str:
        return without_transport_headers(message.body_text)

    @staticmethod
    def read_events(source_jsonl_path: str) -> list[dict[str, Any]]:
        return read_complete_jsonl(source_jsonl_path)

    def _local_provider(self, path_value: str) -> LocalMailProvider:
        path = str(Path(path_value).expanduser().resolve())
        provider = self._local.get(path)
        if provider is None:
            provider = LocalMailProvider(path)
            self._local[path] = provider
        return provider

    def _gmail_client(self) -> GmailReadOnlyClient:
        if self._gmail is None:
            auth = GmailReadOnlyAuth(
                self.config.gmail_credentials,
                self.config.gmail_token_cache,
            )
            self._gmail = GmailReadOnlyClient(
                auth.build_service(interactive=False)
            )
        return self._gmail

    def _outlook_provider(self) -> OutlookProvider:
        if not self.config.outlook_client_id:
            raise ValueError("Outlook review requires --client-id")
        if self._outlook is None:
            self._outlook = OutlookProvider(
                OutlookProviderConfig(
                    client_id=self.config.outlook_client_id,
                    authority=self.config.outlook_authority,
                    token_cache_path=self.config.outlook_token_cache,
                ),
                authenticator=MicrosoftAuthenticator(
                    client_id=self.config.outlook_client_id,
                    authority=self.config.outlook_authority,
                    scopes=["Mail.Read"],
                    token_cache_path=self.config.outlook_token_cache,
                ),
            )
        return self._outlook
