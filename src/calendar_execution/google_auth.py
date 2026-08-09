from __future__ import annotations

import os
import tempfile
from pathlib import Path
from typing import Any


GOOGLE_CALENDAR_SCOPE = "https://www.googleapis.com/auth/calendar.events"
DEFAULT_CREDENTIALS = Path("~/.config/agentledger/google_credentials.json")
DEFAULT_TOKEN = Path("~/.config/agentledger/google_calendar_token.json")


class GoogleCalendarAuth:
    def __init__(
        self, credentials_path: str | Path = DEFAULT_CREDENTIALS,
        token_path: str | Path = DEFAULT_TOKEN,
    ) -> None:
        self.credentials_path = Path(credentials_path).expanduser()
        self.token_path = Path(token_path).expanduser()

    def credentials(self, *, interactive: bool = True) -> Any:
        if not self.credentials_path.is_file():
            raise ValueError(
                f"Google OAuth credentials file not found: {self.credentials_path}"
            )
        try:
            from google.auth.transport.requests import Request
            from google.oauth2.credentials import Credentials
            from google_auth_oauthlib.flow import InstalledAppFlow
        except ImportError as exc:
            raise RuntimeError("Google Calendar dependencies are not installed") from exc
        credentials = None
        if self.token_path.is_file():
            credentials = Credentials.from_authorized_user_file(
                str(self.token_path), [GOOGLE_CALENDAR_SCOPE]
            )
        if credentials and credentials.expired and credentials.refresh_token:
            try:
                credentials.refresh(Request())
            except Exception as exc:
                raise RuntimeError("Google Calendar token refresh failed") from exc
        elif not credentials or not credentials.valid:
            if not interactive:
                raise RuntimeError("Google Calendar authentication is unavailable")
            flow = InstalledAppFlow.from_client_secrets_file(
                str(self.credentials_path), [GOOGLE_CALENDAR_SCOPE]
            )
            try:
                credentials = flow.run_local_server(port=0)
            except Exception as exc:
                raise RuntimeError("Google Calendar authorization failed") from exc
        self._save_token(credentials.to_json())
        return credentials

    def build_service(self, *, interactive: bool = True) -> Any:
        try:
            from googleapiclient.discovery import build
        except ImportError as exc:
            raise RuntimeError("Google Calendar dependencies are not installed") from exc
        return build(
            "calendar", "v3",
            credentials=self.credentials(interactive=interactive),
            cache_discovery=False,
        )

    def status(self) -> dict[str, Any]:
        return {
            "credentials_found": self.credentials_path.is_file(),
            "token_found": self.token_path.is_file(),
            "credentials_path": self.credentials_path,
            "token_path": self.token_path,
        }

    def _save_token(self, value: str) -> None:
        self.token_path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
        try:
            os.chmod(self.token_path.parent, 0o700)
            if self.credentials_path.exists():
                os.chmod(self.credentials_path, 0o600)
        except OSError:
            pass
        descriptor, temporary = tempfile.mkstemp(
            dir=self.token_path.parent, prefix=f".{self.token_path.name}."
        )
        try:
            with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
                handle.write(value)
                handle.flush()
                os.fsync(handle.fileno())
            os.chmod(temporary, 0o600)
            os.replace(temporary, self.token_path)
        finally:
            if os.path.exists(temporary):
                os.unlink(temporary)
