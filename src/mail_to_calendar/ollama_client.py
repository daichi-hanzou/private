from __future__ import annotations

import json
import time
from dataclasses import dataclass
from typing import Any
from urllib.parse import urlparse

import requests


class OllamaError(RuntimeError):
    pass


class OllamaConnectionError(OllamaError):
    pass


class OllamaTimeoutError(OllamaError):
    pass


class OllamaModelNotFoundError(OllamaError):
    pass


@dataclass(frozen=True)
class OllamaCheckResult:
    reachable: bool
    model_available: bool
    structured_output_available: bool
    latency_ms: int = 0


class OllamaClient:
    max_response_bytes = 1_000_000

    def __init__(
        self,
        *,
        base_url: str = "http://localhost:11434",
        model: str = "qwen3:8b",
        timeout_seconds: float = 120,
        keep_alive: str = "5m",
        temperature: float = 0,
        thinking: bool = False,
        allow_remote: bool = False,
        transport: Any = requests,
    ) -> None:
        parsed = urlparse(base_url)
        if parsed.scheme not in {"http", "https"} or not parsed.hostname:
            raise ValueError("Ollama base URL must be an http(s) URL")
        if parsed.username or parsed.password or parsed.query or parsed.fragment:
            raise ValueError("Ollama base URL must not contain credentials or query data")
        if not allow_remote and parsed.hostname not in {"localhost", "127.0.0.1", "::1"}:
            raise ValueError(
                "remote Ollama is blocked; use --allow-remote-ollama explicitly"
            )
        if not model.strip():
            raise ValueError("Ollama model is required")
        if timeout_seconds <= 0:
            raise ValueError("Ollama timeout must be positive")
        if not 0 <= temperature <= 2:
            raise ValueError("Ollama temperature must be between 0 and 2")
        self.base_url = base_url.rstrip("/")
        self.model = model
        self.timeout_seconds = timeout_seconds
        self.keep_alive = keep_alive
        self.temperature = temperature
        self.thinking = thinking
        self.transport = transport

    @property
    def safe_host(self) -> str:
        parsed = urlparse(self.base_url)
        return parsed.hostname or "unknown"

    def chat(self, *, messages: list[dict[str, str]], schema: dict[str, Any]) -> dict[str, Any]:
        payload = {
            "model": self.model,
            "messages": messages,
            "format": schema,
            "stream": False,
            "think": self.thinking,
            "keep_alive": self.keep_alive,
            "options": {"temperature": self.temperature},
        }
        response = self._request("post", "/api/chat", json=payload)
        data = self._response_json(response)
        message = data.get("message")
        content = message.get("content") if isinstance(message, dict) else None
        if not isinstance(content, str) or not content.strip():
            raise OllamaError("Ollama returned an empty response")
        if len(content.encode("utf-8")) > self.max_response_bytes:
            raise OllamaError("Ollama response exceeded the size limit")
        try:
            parsed = json.loads(content)
        except json.JSONDecodeError as exc:
            raise OllamaError("Ollama returned invalid JSON") from exc
        if not isinstance(parsed, dict):
            raise OllamaError("Ollama structured response must be an object")
        return parsed

    def check(self) -> OllamaCheckResult:
        response = self._request("get", "/api/tags")
        data = self._response_json(response)
        models = data.get("models")
        names = {
            str(item.get("name") or item.get("model"))
            for item in models or [] if isinstance(item, dict)
        }
        available = self.model in names
        if not available:
            return OllamaCheckResult(True, False, False, 0)
        started = time.monotonic()
        result = self.chat(
            messages=[
                {
                    "role": "system",
                    "content": "Return only JSON matching the supplied schema.",
                },
                {
                    "role": "user",
                    "content": (
                        "Analyze this short test email and set ok to true: "
                        "<email_subject>Security notice</email_subject> "
                        "<email_body_untrusted>A new sign-in was detected."
                        "</email_body_untrusted>"
                    ),
                },
            ],
            schema={
                "type": "object",
                "properties": {"ok": {"type": "boolean"}},
                "required": ["ok"],
                "additionalProperties": False,
            },
        )
        latency_ms = int((time.monotonic() - started) * 1000)
        if result != {"ok": True}:
            raise OllamaError("Ollama structured-output check returned an invalid result")
        return OllamaCheckResult(True, True, True, latency_ms)

    def _request(self, method: str, path: str, **kwargs: Any) -> Any:
        try:
            response = getattr(self.transport, method)(
                self.base_url + path,
                timeout=self.timeout_seconds,
                **kwargs,
            )
        except requests.Timeout as exc:
            raise OllamaTimeoutError("Ollama request timed out") from exc
        except requests.RequestException as exc:
            raise OllamaConnectionError("Ollama is not reachable") from exc
        if response.status_code == 404:
            raise OllamaModelNotFoundError(
                f"Ollama model or endpoint was not found: {self.model}"
            )
        if response.status_code >= 400:
            error = "unknown error"
            try:
                value = response.json().get("error")
                if isinstance(value, str):
                    error = value[:200]
            except (ValueError, AttributeError):
                pass
            raise OllamaError(f"Ollama HTTP {response.status_code}: {error}")
        return response

    @staticmethod
    def _response_json(response: Any) -> dict[str, Any]:
        try:
            value = response.json()
        except ValueError as exc:
            raise OllamaError("Ollama returned invalid JSON") from exc
        if not isinstance(value, dict):
            raise OllamaError("Ollama response must be an object")
        return value
