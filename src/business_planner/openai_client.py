from __future__ import annotations

import os

from openai import AzureOpenAI, OpenAI


AZURE_COGNITIVE_SERVICES_SCOPE = (
    "https://cognitiveservices.azure.com/.default"
)


def create_openai_client() -> OpenAI:
    endpoint = os.getenv("AZURE_OPENAI_ENDPOINT")
    if not endpoint:
        return OpenAI()

    api_version = os.getenv("OPENAI_API_VERSION")
    if not api_version:
        raise ValueError(
            "OPENAI_API_VERSION is required when AZURE_OPENAI_ENDPOINT is set"
        )

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
    os.environ["OPENAI_API_VERSION"] = api_version
    os.environ["AZURE_OPENAI_ENDPOINT"] = endpoint

    token_provider = get_bearer_token_provider(
        credential, AZURE_COGNITIVE_SERVICES_SCOPE
    )
    return AzureOpenAI(
        azure_endpoint=endpoint,
        api_version=api_version,
        azure_ad_token_provider=token_provider,
    )
