"""Azure-only Chat Completions adapter; independent of the OpenAI adapter."""
import os
import time
from urllib.parse import urlsplit
from azure.identity import DefaultAzureCredential
from dotenv import load_dotenv
from openai import AzureOpenAI
from coffeebench.models._retry import call_with_retry
from coffeebench.models.types import ModelResponse, ToolCall

load_dotenv()


class AzureOpenAIModel:
    DEFAULT_MAX_INPUT_TOKENS = 200_000
    SCOPE = "https://cognitiveservices.azure.com/.default"

    def __init__(self, effort="low"):
        endpoint = os.getenv("AZURE_OPENAI_ENDPOINT", "").strip().rstrip("/")
        version = os.getenv("AZURE_OPENAI_API_VERSION", "").strip()
        deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", "").strip()
        url = urlsplit(endpoint)
        if (url.scheme != "https" or not url.hostname or url.path or
                url.query or url.fragment or url.username or url.password):
            raise ValueError("AZURE_OPENAI_ENDPOINT must be an HTTPS resource root URL")
        if not version or not deployment:
            raise ValueError("Set AZURE_OPENAI_API_VERSION and AZURE_OPENAI_DEPLOYMENT")
        self.model = "azure"
        self.api_model = deployment
        self.provider = "azure"
        self.credential = DefaultAzureCredential()
        self._token = self.credential.get_token(self.SCOPE)
        self.client = AzureOpenAI(azure_endpoint=endpoint, api_version=version,
                                 azure_ad_token_provider=self._token_provider,
                                 timeout=300.0, max_retries=0)
        self.cost = 0.0  # Internal bookkeeping placeholder; exports are unknown.
        self.cost_known = False
        self.n_calls = self.total_input_tokens = self.total_output_tokens = 0
        self.last_input_tokens = 0
        self.max_input_tokens = self.DEFAULT_MAX_INPUT_TOKENS
        self.max_tokens = 4096
        self._skip_reasoning = effort == "off"
        self.reasoning_effort = effort

    def _token_provider(self):
        if self._token.expires_on <= time.time() + 300:
            self._token = self.credential.get_token(self.SCOPE)
        return self._token.token

    def _completion_cost(self, *args):
        return 0.0

    def query(self, messages, tools=None, tool_choice=None):
        return self._chat_query(messages, tools, tool_choice)

    def summarize(self, instructions, content, max_tokens=4096):
        return self._chat_query([{"role": "system", "content": instructions},
                                 {"role": "user", "content": content}],
                                max_tokens=max_tokens).content

    def get_usage_stats(self):
        return {"n_model_calls": self.n_calls, "model_cost": None,
                "total_input_tokens": self.total_input_tokens,
                "total_output_tokens": self.total_output_tokens,
                "last_input_tokens": self.last_input_tokens,
                "provider": "azure", "deployment": self.api_model,
                "cost_basis": "Unknown: deployment-only Azure configuration"}

    def _chat_query(self, messages, tools=None, tool_choice=None, max_tokens=None):
        import json

        history = []
        for message in messages:
            role = message.get("role")
            if role not in {"system", "developer", "user", "assistant", "tool"}:
                continue
            entry = {"role": role, "content": message.get("content") or ""}
            if role == "tool":
                entry["tool_call_id"] = message["tool_call_id"]
            if role == "assistant" and message.get("tool_calls"):
                calls = []
                for call in message["tool_calls"]:
                    if isinstance(call, ToolCall):
                        calls.append({"id": call.id, "type": "function", "function": {
                            "name": call.name, "arguments": json.dumps(call.input)}})
                    elif "function" in call:
                        calls.append(call)
                    else:
                        calls.append({"id": call["id"], "type": "function", "function": {
                            "name": call["name"], "arguments": json.dumps(call["input"])}})
                entry["tool_calls"] = calls
            history.append(entry)
        kwargs = {"model": self.api_model, "messages": history,
                  "max_completion_tokens": max_tokens or self.max_tokens}
        if not self._skip_reasoning:
            kwargs["reasoning_effort"] = self.reasoning_effort
        if tools:
            kwargs["tools"] = [{"type": "function", "function": {
                "name": t.name, "description": t.description,
                "parameters": t.input_schema}} for t in tools]
            kwargs["parallel_tool_calls"] = False
            if tool_choice is not None:
                if isinstance(tool_choice, dict) and "name" in tool_choice:
                    tool_choice = {"type": "function", "function": {"name": tool_choice["name"]}}
                kwargs["tool_choice"] = tool_choice
        response = call_with_retry(lambda: self.client.chat.completions.create(**kwargs),
                                   label=f"azure:{self.api_model}")
        usage = response.usage
        if usage is None:
            raise ValueError("Azure Chat Completions response is missing token usage")
        inputs, outputs = usage.prompt_tokens, usage.completion_tokens
        cached = getattr(getattr(usage, "prompt_tokens_details", None), "cached_tokens", 0) or 0
        cost = self._completion_cost(inputs - cached, cached, outputs)
        self.n_calls += 1
        self.cost += cost
        self.total_input_tokens += inputs
        self.total_output_tokens += outputs
        self.last_input_tokens = inputs
        choice = response.choices[0]
        result = choice.message
        calls = []
        for call in result.tool_calls or []:
            arguments = json.loads(call.function.arguments or "{}")
            if not isinstance(arguments, dict):
                raise ValueError("Azure tool arguments must be an object")
            calls.append(ToolCall(call.id, call.function.name, arguments))
        return ModelResponse(content=result.content or "", tool_calls=calls,
                             stop_reason=choice.finish_reason or "", cost=cost)

