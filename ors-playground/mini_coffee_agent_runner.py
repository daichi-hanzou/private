from __future__ import annotations

import os
import sys

import anthropic
from dotenv import load_dotenv
from ors.client import ORS


load_dotenv()

ORS_BASE_URL = os.getenv("MINI_COFFEE_ORS_URL", "http://localhost:8082")
MODEL = os.getenv("ANTHROPIC_MODEL", "claude-opus-4-8")
MAX_TURNS = 60

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


def _call_ors_tool(session, tool_name: str, arguments: dict) -> tuple[str, bool, float]:
    result = session.call_tool(tool_name, arguments)
    return "\n".join(block.text for block in result.blocks), result.finished, result.reward


def run_agent() -> float:
    api_key = os.getenv("ANTHROPIC_API_KEY")
    if not api_key:
        print("ERROR: ANTHROPIC_API_KEY is not set.", file=sys.stderr)
        sys.exit(1)

    llm = anthropic.Anthropic(api_key=api_key)
    env = ORS(base_url=ORS_BASE_URL).environment("minicoffeeenv")
    task = env.list_tasks(split="train")[0]

    final_reward = 0.0
    with env.session(task=task) as session:
        prompt_text = "\n".join(block.text for block in session.get_prompt())
        messages: list[dict] = [
            {"role": "user", "content": prompt_text},
            {
                "role": "user",
                "content": (
                    "Balance cheap but risky direct procurement against immediate trader inventory. "
                    "Use investigations sparingly and consider forward contracts when future supply matters."
                ),
            },
        ]

        for turn in range(1, MAX_TURNS + 1):
            print(f"\nTurn {turn}")
            response = llm.messages.create(
                model=MODEL,
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


if __name__ == "__main__":
    reward = run_agent()
    print(f"\nFinal reward: {reward:.4f}")
