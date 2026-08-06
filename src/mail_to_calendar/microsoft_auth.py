from __future__ import annotations

import os
import tempfile
from collections.abc import Callable
from pathlib import Path
from typing import Any, Protocol
from urllib.parse import urlparse

import msal


class MicrosoftAuthError(RuntimeError):
    """Authentication failed without exposing token-bearing responses."""


class TokenProvider(Protocol):
    last_account: str | None

    def acquire_token(self) -> str: ...


class MicrosoftAuthenticator:
    def __init__(
        self,
        *,
        client_id: str,
        authority: str,
        scopes: list[str],
        token_cache_path: Path,
        app_factory: Callable[..., Any] | None = None,
        cache_factory: Callable[[], Any] | None = None,
        device_code_callback: Callable[[str], None] = print,
    ) -> None:
        if set(scopes) != {"Mail.Read"}:
            raise ValueError("Microsoft scopes must contain only Mail.Read")
        parsed_authority = urlparse(authority)
        if (
            parsed_authority.scheme != "https"
            or parsed_authority.hostname != "login.microsoftonline.com"
            or parsed_authority.path.rstrip("/") != "/consumers"
        ):
            raise ValueError(
                "authority must be the personal Microsoft account "
                "consumers endpoint"
            )
        self.client_id = client_id
        self.authority = authority.rstrip("/")
        self.scopes = list(scopes)
        self.token_cache_path = Path(token_cache_path).expanduser()
        self.app_factory = app_factory or msal.PublicClientApplication
        self.cache_factory = cache_factory or msal.SerializableTokenCache
        self.device_code_callback = device_code_callback
        self.last_account: str | None = None

    def acquire_token(self) -> str:
        cache = self.cache_factory()
        self._load_cache(cache)
        try:
            app = self.app_factory(
                self.client_id,
                authority=self.authority,
                token_cache=cache,
            )
            accounts = app.get_accounts()
            result = None
            if accounts:
                result = app.acquire_token_silent(
                    self.scopes,
                    account=accounts[0],
                )
                self.last_account = self._account_name(accounts[0])
            if not result:
                flow = app.initiate_device_flow(scopes=self.scopes)
                if not isinstance(flow, dict) or "user_code" not in flow:
                    raise MicrosoftAuthError(
                        "device code authentication could not be started"
                    )
                message = flow.get("message")
                if isinstance(message, str):
                    self.device_code_callback(message)
                result = app.acquire_token_by_device_flow(flow)
            if not isinstance(result, dict) or not result.get("access_token"):
                error = (
                    result.get("error")
                    if isinstance(result, dict)
                    else "unknown_error"
                )
                raise MicrosoftAuthError(
                    f"Microsoft authentication failed: {error}"
                )
            claims = result.get("id_token_claims")
            if isinstance(claims, dict):
                name = claims.get("preferred_username") or claims.get("email")
                if name:
                    self.last_account = str(name)
            self._save_cache(cache)
            return str(result["access_token"])
        except MicrosoftAuthError:
            raise
        except (OSError, TimeoutError) as exc:
            raise MicrosoftAuthError(
                "Microsoft authentication was cancelled, timed out, or failed"
            ) from exc
        except Exception as exc:
            raise MicrosoftAuthError(
                "Microsoft authentication failed safely"
            ) from exc

    def _load_cache(self, cache: Any) -> None:
        if not self.token_cache_path.exists():
            return
        try:
            serialized = self.token_cache_path.read_text(encoding="utf-8")
            cache.deserialize(serialized)
        except (OSError, ValueError) as exc:
            raise MicrosoftAuthError(
                f"Microsoft token cache is unreadable: {self.token_cache_path}"
            ) from exc

    def _save_cache(self, cache: Any) -> None:
        if not getattr(cache, "has_state_changed", False):
            return
        path = self.token_cache_path
        path.parent.mkdir(parents=True, exist_ok=True)
        try:
            path.parent.chmod(0o700)
        except OSError:
            pass
        descriptor, temporary_name = tempfile.mkstemp(
            dir=path.parent,
            prefix=f".{path.name}.",
            suffix=".tmp",
            text=True,
        )
        temporary = Path(temporary_name)
        try:
            os.chmod(temporary, 0o600)
            with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
                handle.write(cache.serialize())
                handle.flush()
                os.fsync(handle.fileno())
            os.replace(temporary, path)
            try:
                path.chmod(0o600)
            except OSError:
                pass
        except BaseException:
            temporary.unlink(missing_ok=True)
            raise

    @staticmethod
    def _account_name(account: Any) -> str | None:
        if isinstance(account, dict):
            value = account.get("username")
            return str(value) if value else None
        return None


def mask_account(value: str | None) -> str:
    if not value:
        return "not reported"
    if "@" not in value:
        return value[:1] + "***"
    local, domain = value.split("@", 1)
    return (local[:1] or "*") + "***@" + domain
