"""Azure-only v1 Responses adapter; independent of the OpenAI adapter."""
import os
import subprocess
import time
from urllib.parse import urlsplit
from azure.identity import DefaultAzureCredential, CredentialUnavailableError
from dotenv import load_dotenv
from openai import OpenAI
from coffeebench.models.openai_model import OpenAIModel, _serialize_responses_item
from coffeebench.models._retry import call_with_retry
from coffeebench.models.types import ModelResponse, ToolCall

load_dotenv()


class AzureOpenAIModel:
    DEFAULT_MAX_INPUT_TOKENS = 200_000
    SCOPE = "https://cognitiveservices.azure.com/.default"

    def __init__(self, effort="low"):
        endpoint = os.getenv("AZURE_OPENAI_ENDPOINT", "").strip().rstrip("/")
        deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", "").strip()
        url = urlsplit(endpoint)
        if (url.scheme != "https" or not url.hostname or url.path or
                url.query or url.fragment or url.username or url.password):
            raise ValueError("AZURE_OPENAI_ENDPOINT must be an HTTPS resource root URL")
        if not deployment:
            raise ValueError("Set AZURE_OPENAI_DEPLOYMENT")
        self.model = "azure"
        self.api_model = deployment
        self.provider = "azure"
        self.credential = DefaultAzureCredential(process_timeout=60)
        self._token = self._get_token_with_retry()
        self.client = OpenAI(base_url=endpoint + "/openai/v1/",
                             api_key=self._token_provider, timeout=300.0, max_retries=0)
        self.cost = 0.0  # Internal bookkeeping placeholder; exports are unknown.
        self.cost_known = False
        self.n_calls = self.total_input_tokens = self.total_output_tokens = 0
        self.last_input_tokens = 0
        self.max_input_tokens = self.DEFAULT_MAX_INPUT_TOKENS
        self.max_tokens = 4096
        self._skip_reasoning = effort == "off"
        self.reasoning_effort = effort

    def _get_token_with_retry(self):
        """Retry only CLI subprocess timeouts, including wrapped SDK errors.

        Authentication failures (e.g. login required or insufficient permissions)
        remain fatal. Keep retries bounded independently of LLM API retries.
        """
        for attempt in range(1, 4):
            try:
                return self.credential.get_token(self.SCOPE)
            except CredentialUnavailableError as exc:
                cause, seen, timed_out = exc, set(), False
                while cause is not None and id(cause) not in seen:
                    seen.add(id(cause))
                    if isinstance(cause, subprocess.TimeoutExpired):
                        timed_out = True
                        break
                    cause = cause.__cause__ or cause.__context__
                if not timed_out or attempt == 3:
                    raise
                delay = 2 ** attempt
                # Never print CLI output or token contents.
                print(f"[azure-auth] CLI token acquisition timed out; "
                      f"attempt {attempt}/3, retrying in {delay}s")
                time.sleep(delay)

    def _token_provider(self):
        if self._token.expires_on <= time.time() + 300:
            self._token = self._get_token_with_retry()
        return self._token.token

    def _completion_cost(self, *args):
        return 0.0

    def query(self, messages, tools=None, tool_choice=None):
        return self._responses_query(messages, tools, tool_choice)

    def summarize(self, instructions, content, max_tokens=4096):
        return self._responses_query([{"role": "system", "content": instructions},
                                 {"role": "user", "content": content}],
                                max_tokens=max_tokens).content

    def get_usage_stats(self):
        return {"n_model_calls": self.n_calls, "model_cost": None,
                "total_input_tokens": self.total_input_tokens,
                "total_output_tokens": self.total_output_tokens,
                "last_input_tokens": self.last_input_tokens,
                "provider": "azure", "deployment": self.api_model,
                "cost_basis": "Unknown: deployment-only Azure configuration"}

    def _responses_query(self, messages, tools=None, tool_choice=None, max_tokens=None):
        import json

        kwargs = {
            "model": self.api_model,
            "input": OpenAIModel._to_responses_input(self, messages),
            "max_output_tokens": max_tokens or self.max_tokens,
        }
        if not self._skip_reasoning:
            kwargs["reasoning"] = {"effort": self.reasoning_effort, "summary": "auto"}
        specs = OpenAIModel._tools_to_openai(tools)
        if specs:
            kwargs["tools"] = specs
            kwargs["parallel_tool_calls"] = False
            if tool_choice is not None:
                kwargs["tool_choice"] = tool_choice
        response = call_with_retry(lambda: self.client.responses.create(**kwargs),
                                   label=f"azure:{self.api_model}")
        usage = response.usage
        if usage is None:
            raise ValueError("Azure Responses result is missing token usage")
        self.n_calls += 1
        self.total_input_tokens += usage.input_tokens
        self.total_output_tokens += usage.output_tokens
        self.last_input_tokens = usage.input_tokens
        calls, text, thinking, raw = [], [], [], []
        for item in response.output or []:
            raw.append(item.model_dump(exclude_none=True) if hasattr(item, "model_dump")
                       else _serialize_responses_item(item))
            if item.type == "function_call":
                arguments = json.loads(item.arguments or "{}")
                if not isinstance(arguments, dict):
                    raise ValueError("Azure tool arguments must be an object")
                calls.append(ToolCall(item.call_id, item.name, arguments))
            elif item.type == "message":
                text.extend(part.text for part in item.content if part.type == "output_text")
            elif item.type == "reasoning":
                thinking.extend(part.text for part in (item.summary or []))
        return ModelResponse(content="".join(text), thinking="".join(thinking),
                             tool_calls=calls, raw=raw,
                             stop_reason=response.status or "", cost=0.0)
