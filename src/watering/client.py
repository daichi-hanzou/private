from __future__ import annotations

from typing import Any

import requests


class WateringClientError(RuntimeError):
    pass


class ESP32Client:
    def __init__(self, base_url: str, token: str, *, timeout: float = 5.0, session: Any = requests) -> None:
        self.base_url = base_url.rstrip("/")
        self.token = token
        self.timeout = timeout
        self.session = session

    def health(self) -> dict[str, Any]:
        return self._request("GET", "/health")

    def status(self) -> dict[str, Any]:
        return self._request("GET", "/status")

    def water(self, duration_seconds: int, request_id: str) -> dict[str, Any]:
        return self._request("POST", "/water", {
            "duration_seconds": duration_seconds, "request_id": request_id,
        })

    def stop(self) -> dict[str, Any]:
        return self._request("POST", "/stop", {})

    def _request(self, method: str, path: str, payload: dict[str, Any] | None = None) -> dict[str, Any]:
        try:
            response = self.session.request(
                method, self.base_url + path,
                headers={"Authorization": f"Bearer {self.token}"},
                json=payload if method == "POST" else None,
                timeout=self.timeout,
            )
        except requests.Timeout as exc:
            raise WateringClientError("ESP32 request timed out") from exc
        except requests.ConnectionError as exc:
            raise WateringClientError("ESP32 is unreachable") from exc
        try:
            body = response.json()
        except ValueError as exc:
            raise WateringClientError("ESP32 returned invalid JSON") from exc
        if not 200 <= response.status_code < 300:
            reason = body.get("error", "request rejected") if isinstance(body, dict) else "request rejected"
            raise WateringClientError(f"ESP32 rejected request ({response.status_code}): {reason}")
        if not isinstance(body, dict):
            raise WateringClientError("ESP32 returned invalid response")
        return body
