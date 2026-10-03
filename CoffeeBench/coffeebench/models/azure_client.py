"""Azure v1 Responses client with refreshable Entra ID authentication."""
import os
import time
from urllib.parse import urlsplit
from openai import OpenAI
from azure.identity import DefaultAzureCredential


def create_azure_client(**options):
    endpoint = os.getenv('AZURE_OPENAI_ENDPOINT', '').strip().rstrip('/')
    deployment = os.getenv('AZURE_OPENAI_DEPLOYMENT', '').strip()
    url = urlsplit(endpoint)
    if (url.scheme != 'https' or not url.hostname or url.username or url.password
            or url.query or url.fragment or url.path):
        raise ValueError('AZURE_OPENAI_ENDPOINT must be an HTTPS resource URL without an API path')
    if not deployment:
        raise ValueError('Set AZURE_OPENAI_DEPLOYMENT to the Azure deployment name')
    credential = DefaultAzureCredential()
    scope = 'https://cognitiveservices.azure.com/.default'
    token = credential.get_token(scope)

    def token_provider():
        nonlocal token
        if token.expires_on <= time.time() + 300:
            token = credential.get_token(scope)
        return token.token

    client = OpenAI(base_url=endpoint + '/openai/v1/', api_key=token_provider, **options)
    return client, deployment, credential
