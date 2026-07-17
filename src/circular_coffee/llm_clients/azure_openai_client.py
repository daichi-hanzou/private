from __future__ import annotations

import os
from typing import Any

from .openai_client import OpenAIClient


class AzureOpenAIClient(OpenAIClient):
    def __init__(
        self,
        *,
        deployment: str | None = None,
        temperature: float | None = None,
        seed: int | None = None,
        supports_seed: bool = False,
        api_key: str | None = None,
        azure_endpoint: str | None = None,
        api_version: str | None = None,
        max_retries: int = 2,
        client: Any | None = None,
    ):
        resolved_deployment = deployment or os.environ.get("AZURE_OPENAI_DEPLOYMENT")
        if client is None:
            from openai import AzureOpenAI

            client = AzureOpenAI(
                api_key=api_key or os.environ.get("AZURE_OPENAI_API_KEY"),
                azure_endpoint=azure_endpoint or os.environ.get("AZURE_OPENAI_ENDPOINT"),
                api_version=api_version or os.environ.get("AZURE_OPENAI_API_VERSION"),
                max_retries=max_retries,
            )
        super().__init__(
            model=resolved_deployment,
            temperature=temperature,
            seed=seed,
            supports_seed=supports_seed,
            max_retries=max_retries,
            client=client,
        )
