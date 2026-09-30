"""OpenAI wrapper — native tool-use via the Responses API."""

import os

from openai import OpenAI, AzureOpenAI
from dotenv import load_dotenv

from coffeebench.models._retry import call_with_retry
from coffeebench.models.types import ModelResponse, ToolCall, ToolSpec

load_dotenv()


class OpenAIModel:
    DEFAULT_MAX_INPUT_TOKENS = 200_000
    # Standard USD / million tokens, verified 2026-09-26:
    # https://developers.openai.com/api/docs/models/gpt-6-astra
    PRICING = {
        "gpt-5.6-sol": {"input": 4.00, "cached_input": 0.40, "output": 20.00},
        "gpt-5.6": {"input": 4.00, "cached_input": 0.40, "output": 20.00},
        "gpt-5.5": {"input": 5.00, "cached_input": 0.50, "output": 30.00},
        "gpt-6-astra": {"input": 10.00, "cached_input": 1.00, "output": 50.00},
    }

    def __init__(self, model: str = "gpt-5.6-sol", effort: str | None = None):
        self.provider = ("azure" if model == "azure" else
                         os.getenv("COFFEEBENCH_OPENAI_PROVIDER", "openai").strip().lower())
        self.pricing_model = (os.getenv("AZURE_OPENAI_MODEL", "").strip()
                              if self.provider == "azure" else model)
        self.cost_known = self.pricing_model in self.PRICING
        if self.provider != "azure" and model not in self.PRICING:
            raise ValueError(f"No verified pricing for OpenAI model {model!r}")
        if model == "gpt-6-astra":
            effort = effort or "low"
            if effort not in {"low", "medium", "high", "xhigh", "max"}:
                raise ValueError("gpt-6-astra requires low/medium/high/xhigh/max reasoning")
        if model in {"gpt-5.6", "gpt-5.6-sol"}:
            effort = "none" if effort == "off" else (effort or "low")
            if effort not in {"none", "low", "medium", "high", "xhigh", "max"}:
                raise ValueError("gpt-5.6 requires none/low/medium/high/xhigh/max reasoning")
        self.cost = 0.0
        self.n_calls = 0
        self.total_input_tokens = 0
        self.total_output_tokens = 0
        self.last_input_tokens = 0
        self.max_input_tokens = self.DEFAULT_MAX_INPUT_TOKENS
        self.model = model
        # Retry policy is owned by call_with_retry; avoid hidden SDK retries
        # bypassing the budget agent's one-attempt limit.
        self.api_model = model
        client_options = {"timeout": 300.0, "max_retries": 0}
        if self.provider == "azure":
            from urllib.parse import urlsplit

            endpoint = os.getenv("AZURE_OPENAI_ENDPOINT", "").strip().rstrip("/")
            deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", "").strip()
            api_version = os.getenv("AZURE_OPENAI_API_VERSION", "").strip()
            url = urlsplit(endpoint)
            if (url.scheme != "https" or not url.hostname or url.username or url.password
                    or url.query or url.fragment or url.path not in ("",)):
                raise ValueError("AZURE_OPENAI_ENDPOINT must be an HTTPS resource URL without an API path")
            if not deployment:
                raise ValueError("Set AZURE_OPENAI_DEPLOYMENT to the Azure deployment name")
            if not api_version:
                raise ValueError("Set AZURE_OPENAI_API_VERSION")
            from azure.identity import DefaultAzureCredential
            import time

            self.azure_credential = DefaultAzureCredential()
            scope = "https://cognitiveservices.azure.com/.default"
            token = self.azure_credential.get_token(scope)

            def token_provider():
                nonlocal token
                if token.expires_on <= time.time() + 300:
                    token = self.azure_credential.get_token(scope)
                return token.token

            self.api_model = deployment
            self.client = AzureOpenAI(
                azure_endpoint=endpoint,
                api_version=api_version,
                azure_ad_token_provider=token_provider,
                **client_options,
            )
        elif self.provider == "openai":
            client_options["api_key"] = os.getenv("OPENAI_API_KEY")
        else:
            raise ValueError("COFFEEBENCH_OPENAI_PROVIDER must be openai or azure")
        if self.provider == "openai":
            self.client = OpenAI(**client_options)
        # Temperature is always left at the provider default (never sent).
        # `effort` (e.g. "minimal"/"low"/"medium"/"high"/"xhigh", via the
        # `gpt-5.5:high` model-string suffix) is forwarded as `reasoning.effort`.
        # Legacy GPT-5.5 maps `off` to "minimal". Astra is validated above
        # and defaults direct construction to "low".
        self._skip_temperature = True
        self._skip_reasoning = effort is None
        self.reasoning_effort = "minimal" if effort == "off" else effort
        self.max_tokens = 4096
        self.temperature = 0.0
        self.pricing = self.PRICING

    def _completion_cost(
        self, non_cached_input_tokens, cached_input_tokens, output_tokens
    ) -> float:
        if not self.cost_known:
            return 0.0  # Internal accumulator only; exported costs are None.
        p = self.pricing[self.pricing_model]
        long_context = (
            self.pricing_model in {"gpt-6-astra", "gpt-5.6", "gpt-5.6-sol"}
            and non_cached_input_tokens + cached_input_tokens > 272_000
        )
        input_multiplier = 2 if long_context else 1
        output_multiplier = 1.5 if long_context else 1
        return (
            non_cached_input_tokens * p["input"] * input_multiplier
            + cached_input_tokens * p["cached_input"] * input_multiplier
            + output_tokens * p["output"] * output_multiplier
        ) / 1_000_000

    @staticmethod
    def _tools_to_openai(tools: list[ToolSpec] | None) -> list[dict] | None:
        if not tools:
            return None
        return [
            {
                "type": "function",
                "name": t.name,
                "description": t.description,
                "parameters": t.input_schema,
                # Preserve optional arguments in the existing business tools.
                "strict": False,
            }
            for t in tools
        ]

    def _to_responses_input(self, messages: list[dict]) -> list[dict]:
        """Translate internal history into Responses API `input` items."""
        items: list[dict] = []
        for m in messages:
            role = m.get("role")
            if role in ("system", "developer", "user"):
                items.append(
                    {
                        "role": "developer" if role == "system" else role,
                        "content": m.get("content", ""),
                    }
                )
                continue
            if role == "tool":
                items.append(
                    {
                        "type": "function_call_output",
                        "call_id": m["tool_call_id"],
                        "output": m.get("content", ""),
                    }
                )
                continue
            if role == "assistant":
                raw = m.get("_raw")
                if raw is not None:
                    # _raw is a list of Responses output items; replay verbatim.
                    items.extend(raw)
                else:
                    text = m.get("content") or ""
                    if text:
                        items.append({"role": "assistant", "content": text})
                    for tc in m.get("tool_calls") or []:
                        import json as _json

                        items.append(
                            {
                                "type": "function_call",
                                "call_id": tc.id
                                if isinstance(tc, ToolCall)
                                else tc["id"],
                                "name": tc.name
                                if isinstance(tc, ToolCall)
                                else tc["name"],
                                "arguments": _json.dumps(
                                    tc.input
                                    if isinstance(tc, ToolCall)
                                    else tc["input"]
                                ),
                            }
                        )
                continue
        return items

    def query(
        self,
        messages: list[dict],
        tools: list[ToolSpec] | None = None,
        tool_choice: str | dict | None = None,
    ) -> ModelResponse:
        kwargs: dict = {
            "model": self.api_model,
            "input": self._to_responses_input(messages),
            "max_output_tokens": self.max_tokens,
        }
        if self.provider == "openai":
            kwargs["service_tier"] = "default"
        if not self._skip_temperature:
            kwargs["temperature"] = self.temperature
        if not self._skip_reasoning:
            # `summary: "auto"` asks the Responses API to surface the
            # reasoning summary as a `reasoning` output item — required
            # for `response.thinking` to be non-empty. Without it the
            # reasoning is hidden inside the model and we'd have to
            # fall back to text content for the agent's `thought`.
            kwargs["reasoning"] = {
                "effort": self.reasoning_effort,
                "summary": "auto",
            }
        oa_tools = self._tools_to_openai(tools)
        if oa_tools:
            kwargs["tools"] = oa_tools
            if tool_choice is not None:
                kwargs["tool_choice"] = tool_choice
            # parallel_tool_calls is supported on Responses; constrain to 1.
            kwargs["parallel_tool_calls"] = False

        def _do_call():
            try:
                return self.client.responses.create(**kwargs)
            except Exception as exc:
                msg = str(exc).lower()
                if (
                    "temperature" in msg
                    and any(
                        k in msg for k in ("not supported", "deprecated", "unsupported")
                    )
                    and "temperature" in kwargs
                ):
                    self._skip_temperature = True
                    kwargs.pop("temperature", None)
                    return self.client.responses.create(**kwargs)
                raise

        response = call_with_retry(_do_call, label=f"openai:{self.model}")

        usage = response.usage
        input_tokens = usage.input_tokens
        cached_tokens = getattr(usage.input_tokens_details, "cached_tokens", 0) or 0
        non_cached = input_tokens - cached_tokens
        output_tokens = usage.output_tokens
        cost = self._completion_cost(non_cached, cached_tokens, output_tokens)
        self.n_calls += 1
        self.cost += cost
        self.last_input_tokens = int(input_tokens)
        self.total_input_tokens += int(input_tokens)
        self.total_output_tokens += int(output_tokens)
        print(
            f"[openai:{self.model}] in={input_tokens} cached={cached_tokens} "
            f"out={output_tokens} status={getattr(response, 'status', '?')}"
        )

        # Walk response.output to extract text, tool_calls, and reasoning summary.
        content_parts: list[str] = []
        thinking_parts: list[str] = []
        tool_calls: list[ToolCall] = []
        raw_items: list[dict] = []
        import json as _json

        for item in response.output or []:
            itype = getattr(item, "type", None)
            if itype == "message":
                # The assistant's user-visible reply; concatenate text segments.
                for part in getattr(item, "content", []) or []:
                    ptype = getattr(part, "type", None)
                    if ptype == "output_text":
                        content_parts.append(getattr(part, "text", "") or "")
                # Replay item: include id+content so Responses can stitch the
                # next round of input correctly.
                raw_items.append(_serialize_responses_item(item))
            elif itype == "function_call":
                args_raw = getattr(item, "arguments", "") or ""
                try:
                    args = _json.loads(args_raw) if args_raw else {}
                except (ValueError, TypeError):
                    args = {}
                tool_calls.append(
                    ToolCall(
                        id=getattr(item, "call_id", "") or "",
                        name=getattr(item, "name", "") or "",
                        input=args,
                    )
                )
                raw_items.append(_serialize_responses_item(item))
            elif itype == "reasoning":
                summary = getattr(item, "summary", None)
                if summary:
                    for s in summary:
                        thinking_parts.append(getattr(s, "text", "") or "")
                raw_items.append(_serialize_responses_item(item))

        return ModelResponse(
            content="".join(content_parts) or (response.output_text or ""),
            thinking="".join(thinking_parts),
            tool_calls=tool_calls,
            stop_reason=getattr(response, "status", "") or "",
            cost=cost,
            raw=raw_items,
        )

    def get_usage_stats(self) -> dict:
        return {
            "n_model_calls": self.n_calls,
            "model_cost": self.cost if self.cost_known else None,
            "cost_known": self.cost_known,
            "pricing_model": self.pricing_model or None,
            "total_input_tokens": self.total_input_tokens,
            "total_output_tokens": self.total_output_tokens,
            "last_input_tokens": self.last_input_tokens,
            "provider": self.provider,
            "deployment": self.api_model,
            "cost_basis": (
                "unknown: Azure pricing model not supplied or unsupported"
                if not self.cost_known else
                "OpenAI standard reference estimate, NOT Azure billing; excludes cache-write surcharges"
                if self.provider == "azure" else
                "standard token estimate; excludes cache-write surcharges"
            ),
        }

    def summarize(self, instructions: str, content: str, max_tokens: int = 4096) -> str:
        kwargs: dict = {
            "model": self.api_model,
            "input": [
                {"role": "developer", "content": instructions},
                {"role": "user", "content": content},
            ],
            "max_output_tokens": int(max_tokens),
        }
        if self.provider == "openai":
            kwargs["service_tier"] = "default"
        if not self._skip_temperature:
            kwargs["temperature"] = self.temperature
        # Astra cannot disable reasoning. Pin the lowest supported level so
        # ReAct compaction does not inherit a more expensive provider default.
        if self.model in {"gpt-6-astra", "gpt-5.6", "gpt-5.6-sol"}:
            kwargs["reasoning"] = {"effort": "low"}

        def _do_call():
            try:
                return self.client.responses.create(**kwargs)
            except Exception as exc:
                msg = str(exc).lower()
                if "temperature" in msg and "temperature" in kwargs:
                    self._skip_temperature = True
                    kwargs.pop("temperature", None)
                    return self.client.responses.create(**kwargs)
                raise

        response = call_with_retry(_do_call, label=f"openai:{self.model}:summarize")
        usage = response.usage
        input_tokens = int(usage.input_tokens or 0)
        cached_tokens = int(
            getattr(usage.input_tokens_details, "cached_tokens", 0) or 0
        )
        output_tokens = int(usage.output_tokens or 0)
        cost = self._completion_cost(
            input_tokens - cached_tokens, cached_tokens, output_tokens
        )
        self.n_calls += 1
        self.cost += cost
        self.total_input_tokens += input_tokens
        self.total_output_tokens += output_tokens
        return response.output_text or ""


def _serialize_responses_item(item) -> dict:
    """Convert a Responses API item to a JSON-friendly dict for replay.

    The Responses API accepts the same item shapes back as input; we
    reflect the relevant fields verbatim. Unknown fields are dropped
    rather than passed through, since some are read-only and rejected
    on input.
    """
    itype = getattr(item, "type", None)
    if itype == "function_call":
        return {
            "type": "function_call",
            "id": getattr(item, "id", None),
            "call_id": getattr(item, "call_id", None),
            "name": getattr(item, "name", None),
            "arguments": getattr(item, "arguments", "") or "",
        }
    if itype == "message":
        # Normalize content parts.
        parts = []
        for p in getattr(item, "content", []) or []:
            if getattr(p, "type", None) == "output_text":
                parts.append(
                    {"type": "output_text", "text": getattr(p, "text", "") or ""}
                )
        return {
            "type": "message",
            "id": getattr(item, "id", None),
            "role": getattr(item, "role", "assistant"),
            "content": parts,
        }
    if itype == "reasoning":
        # The Responses API allows reasoning items to be replayed; preserve
        # id and summary text only — internal state isn't user-addressable.
        summary = []
        for s in getattr(item, "summary", []) or []:
            summary.append(
                {"type": "summary_text", "text": getattr(s, "text", "") or ""}
            )
        return {
            "type": "reasoning",
            "id": getattr(item, "id", None),
            "summary": summary,
        }
    # Fallback: best-effort dump.
    if hasattr(item, "model_dump"):
        return item.model_dump()
    return {"type": itype}


if __name__ == "__main__":
    m = OpenAIModel()
    print(m.query([{"role": "user", "content": "Hello!"}]).content)
