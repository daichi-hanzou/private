"""One paid tool-call check: python -m coffeebench.openai_smoke."""
import argparse
import json
import os

from jsonschema import validate

from coffeebench.models import get_model
from coffeebench.models._retry import attempt_limit
from coffeebench.models.types import ToolSpec


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--model", default="gpt-6-astra:low")
    args = parser.parse_args()
    from dotenv import load_dotenv

    load_dotenv()
    provider = os.getenv("COFFEEBENCH_OPENAI_PROVIDER", "openai").strip().lower()
    if provider == "openai" and not os.getenv("OPENAI_API_KEY"):
        parser.error("Set OPENAI_API_KEY in the environment or CoffeeBench/.env first")
    spec = ToolSpec("submit_plan", "Submit a test plan without executing business actions", {
        "type": "object", "additionalProperties": False,
        "properties": {
            "memory": {"type": "string"},
            "actions": {"type": "array", "maxItems": 0, "items": {"type": "object"}},
        }, "required": ["memory", "actions"],
    })
    model = get_model(args.model)
    token = attempt_limit.set(1)
    try:
        response = model.query(
            [{"role": "user", "content": "Call submit_plan with memory='ok' and actions=[]."}],
            tools=[spec], tool_choice={"type": "function", "name": "submit_plan"},
        )
    finally:
        attempt_limit.reset(token)
    if response.stop_reason != "completed" or len(response.tool_calls) != 1:
        raise RuntimeError(f"Tool-call check failed: status={response.stop_reason}")
    call = response.tool_calls[0]
    if call.name != spec.name:
        raise RuntimeError("Unexpected tool name")
    validate(call.input, spec.input_schema)
    print(json.dumps({"ok": True, "model": args.model, **model.get_usage_stats()}, indent=2))


if __name__ == "__main__":
    main()
