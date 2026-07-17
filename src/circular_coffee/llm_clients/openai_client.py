from __future__ import annotations

import json
import os
import time
from typing import Any

from .base import ACTION_JSON_SCHEMA


class OpenAIClient:
    def __init__(
        self,
        *,
        model: str | None = None,
        temperature: float | None = None,
        seed: int | None = None,
        supports_seed: bool = False,
        api_key: str | None = None,
        base_url: str | None = None,
        max_retries: int = 2,
        client: Any | None = None,
    ):
        self.model = model or os.environ.get("OPENAI_MODEL")
        if not self.model:
            raise ValueError("model is required via --model or OPENAI_MODEL")
        self.temperature = temperature
        self.seed = seed
        self.supports_seed = supports_seed
        self.max_retries = max_retries
        if client is None:
            from openai import OpenAI

            kwargs: dict[str, Any] = {
                "api_key": api_key or os.environ.get("OPENAI_API_KEY"),
                "max_retries": max_retries,
            }
            resolved_base_url = base_url or os.environ.get("OPENAI_BASE_URL")
            if resolved_base_url:
                kwargs["base_url"] = resolved_base_url
            client = OpenAI(**kwargs)
        self._client = client
        self.last_call_metadata: dict[str, Any] = {}

    def generate_action(self, system_prompt: str, observation: dict) -> str:
        started = time.perf_counter()
        self.last_call_metadata = {
            "model": self.model,
            "temperature": self.temperature,
            "configured_max_retries": self.max_retries,
            "seed_requested": self.seed,
            "seed_sent": self.seed if self.supports_seed else None,
        }
        try:
            request: dict[str, Any] = {
                "model": self.model,
                "messages": [
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": json.dumps(observation, ensure_ascii=False)},
                ],
                "response_format": {
                    "type": "json_schema",
                    "json_schema": ACTION_JSON_SCHEMA,
                },
            }
            if self.temperature is not None:
                request["temperature"] = self.temperature
            if self.seed is not None and self.supports_seed:
                request["seed"] = self.seed
            response = self._client.chat.completions.create(**request)
            content = response.choices[0].message.content or ""
            usage = getattr(response, "usage", None)
            self.last_call_metadata.update(
                raw_response=content,
                input_tokens=getattr(usage, "prompt_tokens", 0) or 0,
                output_tokens=getattr(usage, "completion_tokens", 0) or 0,
            )
            return content
        except Exception as exc:
            self.last_call_metadata["api_error"] = str(exc)
            raise
        finally:
            self.last_call_metadata["latency_ms"] = round(
                (time.perf_counter() - started) * 1000,
                2,
            )
