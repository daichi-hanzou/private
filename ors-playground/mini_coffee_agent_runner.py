from __future__ import annotations

import json
import os
import sys

from dotenv import load_dotenv
from ors.client import ORS


load_dotenv()

ORS_BASE_URL = os.getenv("MINI_COFFEE_ORS_URL", "http://localhost:8082")
LLM_PROVIDER = os.getenv("MINI_COFFEE_LLM_PROVIDER", "anthropic").lower()
ANTHROPIC_MODEL = os.getenv("ANTHROPIC_MODEL", "claude-opus-4-8")
AZURE_OPENAI_DEPLOYMENT = os.getenv("AZURE_OPENAI_DEPLOYMENT", "")
AZURE_OPENAI_API_VERSION = os.getenv("AZURE_OPENAI_API_VERSION", "2024-10-21")
MAX_TURNS = 60
STRATEGY_HINT = (
    "Balance cheap but risky direct procurement against immediate trader inventory. "
    "Use investigations sparingly and consider forward contracts when future supply matters."
)

TOOL_SCHEMAS: list[dict] = [
    {
        "name": "view_state",
        "description": "Inspect cash, retail inventory, direct farm offers, trader stock, and open contracts.",
        "input_schema": {"type": "object", "properties": {}},
    },
    {
        "name": "investigate_farmer",
        "description": "Pay to reveal ledger-derived fulfillment and liquidity metrics for one farmer.",
        "input_schema": {
            "type": "object",
            "properties": {"farmer_id": {"type": "string", "enum": ["sierra_verde", "cloud_peak", "riverbend"]}},
            "required": ["farmer_id"],
        },
    },
    {
        "name": "buy_spot_direct",
        "description": "Prepay a spot order from a farmer. Delivery is processed on advance_day.",
        "input_schema": {
            "type": "object",
            "properties": {
                "farmer_id": {"type": "string", "enum": ["sierra_verde", "cloud_peak", "riverbend"]},
                "item_id": {"type": "string", "enum": ["standard", "premium"]},
                "quantity_kg": {"type": "integer", "minimum": 1, "maximum": 100},
            },
            "required": ["farmer_id", "item_id", "quantity_kg"],
        },
    },
    {
        "name": "create_forward_contract",
        "description": "Prepay a future direct delivery contract with a farmer.",
        "input_schema": {
            "type": "object",
            "properties": {
                "farmer_id": {"type": "string", "enum": ["sierra_verde", "cloud_peak", "riverbend"]},
                "item_id": {"type": "string", "enum": ["standard", "premium"]},
                "quantity_kg": {"type": "integer", "minimum": 1, "maximum": 100},
                "delivery_day": {"type": "integer", "minimum": 2, "maximum": 7},
            },
            "required": ["farmer_id", "item_id", "quantity_kg", "delivery_day"],
        },
    },
    {
        "name": "buy_from_trader",
        "description": "Buy immediate stock from trader inventory at a higher but safer price.",
        "input_schema": {
            "type": "object",
            "properties": {
                "item_id": {"type": "string", "enum": ["standard", "premium"]},
                "quantity_kg": {"type": "integer", "minimum": 1, "maximum": 100},
            },
            "required": ["item_id", "quantity_kg"],
        },
    },
    {
        "name": "set_price",
        "description": "Set the retail price for one item for the current day.",
        "input_schema": {
            "type": "object",
            "properties": {
                "item_id": {"type": "string", "enum": ["standard", "premium"]},
                "price_per_kg": {"type": "number", "minimum": 0.01, "maximum": 100.0},
            },
            "required": ["item_id", "price_per_kg"],
        },
    },
    {
        "name": "advance_day",
        "description": "Harvest, fulfill contracts, sell demand, and move to the next day.",
        "input_schema": {"type": "object", "properties": {}},
    },
    {
        "name": "finish_episode",
        "description": "End the episode and return the final reward.",
        "input_schema": {"type": "object", "properties": {}},
    },
]


OPENAI_TOOL_SCHEMAS = [
    {
        "type": "function",
        "function": {
            "name": tool["name"],
            "description": tool["description"],
            "parameters": tool["input_schema"],
        },
    }
    for tool in TOOL_SCHEMAS
]


def _call_ors_tool(session, tool_name: str, arguments: dict) -> tuple[str, bool, float]:
    result = session.call_tool(tool_name, arguments)
    return "\n".join(block.text for block in result.blocks), result.finished, result.reward


def _build_initial_messages(prompt_text: str) -> list[dict]:
    return [
        {"role": "user", "content": prompt_text},
        {"role": "user", "content": STRATEGY_HINT},
    ]


def _require_env(name: str) -> str:
    value = os.getenv(name)
    if not value:
        print(f"ERROR: {name} is not set.", file=sys.stderr)
        sys.exit(1)
    return value


def _anthropic_client():
    import anthropic

    api_key = _require_env("ANTHROPIC_API_KEY")
    return anthropic.Anthropic(api_key=api_key)


def _azure_openai_client():
    from openai import AzureOpenAI

    endpoint = _require_env("AZURE_OPENAI_ENDPOINT")
    deployment = _require_env("AZURE_OPENAI_DEPLOYMENT")
    api_key = os.getenv("AZURE_OPENAI_API_KEY")
    bearer_token = os.getenv("AZURE_OPENAI_TOKEN")
    if not api_key and not bearer_token:
        print("ERROR: Set AZURE_OPENAI_API_KEY or AZURE_OPENAI_TOKEN.", file=sys.stderr)
        sys.exit(1)

    kwargs = {
        "azure_endpoint": endpoint,
        "api_version": AZURE_OPENAI_API_VERSION,
    }
    if bearer_token:
        kwargs["azure_ad_token"] = bearer_token
    else:
        kwargs["api_key"] = api_key
    return AzureOpenAI(**kwargs), deployment


def _run_agent_anthropic(session, prompt_text: str) -> float:
    llm = _anthropic_client()
    messages = _build_initial_messages(prompt_text)
    final_reward = 0.0

    for turn in range(1, MAX_TURNS + 1):
        print(f"\nTurn {turn}")
        response = llm.messages.create(
            model=ANTHROPIC_MODEL,
            max_tokens=4096,
            tools=TOOL_SCHEMAS,
            messages=messages,
        )
        messages.append({"role": "assistant", "content": response.content})

        tool_use_blocks = [block for block in response.content if block.type == "tool_use"]
        if not tool_use_blocks:
            print("Agent stopped without calling a tool.")
            break

        tool_results = []
        for block in tool_use_blocks:
            arguments = block.input if isinstance(block.input, dict) else {}
            print(f"[TOOL] {block.name}({arguments})")
            result_text, finished, reward = _call_ors_tool(session, block.name, arguments)
            print(result_text)
            tool_results.append({"type": "tool_result", "tool_use_id": block.id, "content": result_text})
            if finished:
                final_reward = reward
                messages.append({"role": "user", "content": tool_results})
                return final_reward

        messages.append({"role": "user", "content": tool_results})

    return final_reward


def _run_agent_azure_openai(session, prompt_text: str) -> float:
    client, deployment = _azure_openai_client()
    messages = [
        {"role": "system", "content": STRATEGY_HINT},
        {"role": "user", "content": prompt_text},
    ]
    final_reward = 0.0

    for turn in range(1, MAX_TURNS + 1):
        print(f"\nTurn {turn}")
        response = client.chat.completions.create(
            model=deployment,
            messages=messages,
            tools=OPENAI_TOOL_SCHEMAS,
            tool_choice="auto",
            max_tokens=4096,
        )
        message = response.choices[0].message
        if message.content:
            print(f"[LLM] {message.content}")

        tool_calls = message.tool_calls or []
        messages.append(
            {
                "role": "assistant",
                "content": message.content or "",
                "tool_calls": [
                    {
                        "id": tool_call.id,
                        "type": tool_call.type,
                        "function": {
                            "name": tool_call.function.name,
                            "arguments": tool_call.function.arguments,
                        },
                    }
                    for tool_call in tool_calls
                ],
            }
        )
        if not tool_calls:
            print("Agent stopped without calling a tool.")
            break

        for tool_call in tool_calls:
            arguments = json.loads(tool_call.function.arguments or "{}")
            print(f"[TOOL] {tool_call.function.name}({arguments})")
            result_text, finished, reward = _call_ors_tool(session, tool_call.function.name, arguments)
            print(result_text)
            messages.append(
                {
                    "role": "tool",
                    "tool_call_id": tool_call.id,
                    "content": result_text,
                }
            )
            if finished:
                final_reward = reward
                return final_reward

    return final_reward


def run_agent() -> float:
    env = ORS(base_url=ORS_BASE_URL).environment("minicoffeeenv")
    task = env.list_tasks(split="train")[0]

    with env.session(task=task) as session:
        prompt_text = "\n".join(block.text for block in session.get_prompt())
        if LLM_PROVIDER == "anthropic":
            return _run_agent_anthropic(session, prompt_text)
        if LLM_PROVIDER in {"azure_openai", "azure-openai"}:
            return _run_agent_azure_openai(session, prompt_text)

        print(
            "ERROR: MINI_COFFEE_LLM_PROVIDER must be 'anthropic' or 'azure_openai'.",
            file=sys.stderr,
        )
        sys.exit(1)


if __name__ == "__main__":
    reward = run_agent()
    print(f"\nFinal reward: {reward:.4f}")
