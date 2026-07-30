from __future__ import annotations

import os

from openai import OpenAI


AZURE_COGNITIVE_SERVICES_SCOPE = (
    "https://cognitiveservices.azure.com/.default"
)


def _create_client(endpoint: str | None) -> OpenAI:
    if not endpoint:
        return OpenAI()

    # Keep Azure Identity optional for users of the standard OpenAI endpoint.
    try:
        from azure.identity import (
            DefaultAzureCredential,
            get_bearer_token_provider,
        )
    except ImportError as exc:
        raise RuntimeError(
            "Azure authentication requires the azure-identity package"
        ) from exc

    credential = DefaultAzureCredential()
    token = credential.get_token(AZURE_COGNITIVE_SERVICES_SCOPE)
    os.environ["AZURE_OPENAI_AD_TOKEN"] = token.token
    os.environ["AZURE_OPENAI_ENDPOINT"] = endpoint

    token_provider = get_bearer_token_provider(
        credential, AZURE_COGNITIVE_SERVICES_SCOPE
    )
    normalized_endpoint = endpoint.rstrip("/")
    if normalized_endpoint.endswith("/openai/v1"):
        base_url = f"{normalized_endpoint}/"
    else:
        base_url = f"{normalized_endpoint}/openai/v1/"
    return OpenAI(
        base_url=base_url,
        api_key=token_provider,
    )


def create_chat_client() -> OpenAI:
    endpoint = (
        os.getenv("AZURE_OPENAI_CHAT_ENDPOINT")
        or os.getenv("AZURE_OPENAI_ENDPOINT")
    )
    return _create_client(endpoint)


def create_embedding_client() -> OpenAI:
    endpoint = (
        os.getenv("AZURE_OPENAI_EMBEDDING_ENDPOINT")
        or os.getenv("AZURE_OPENAI_ENDPOINT")
    )
    return _create_client(endpoint)


def create_openai_client() -> OpenAI:
    """Backward-compatible alias for the chat client."""
    return create_chat_client()
