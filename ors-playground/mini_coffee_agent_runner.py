from __future__ import annotations

import os
import sys

import anthropic
from dotenv import load_dotenv
from ors.client import ORS


load_dotenv()

ORS_BASE_URL = "http://localhost:8082"
MODEL = os.getenv("ANTHROPIC_MODEL", "claude-opus-4-8")
MAX_TURNS = 40

TOOL_SCHEMAS: list[dict] = [
    {
        "name": "view_state",
        "description": "Inspect the current cash, inventory, prices, and recent results.",
        "input_schema": {"type": "object", "properties": {}},
    },
    {
        "name": "buy_inventory",
        "description": "Buy coffee inventory from the wholesaler at today's wholesale price.",
        "input_schema": {
            "type": "object",
            "properties": {
                "item_id": {
                    "type": "string",
                    "enum": ["standard", "premium"],
                },
                "quantity_kg": {
                    "type": "integer",
                    "minimum": 1,
                    "maximum": 100,
                },
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
                "item_id": {
                    "type": "string",
                    "enum": ["standard", "premium"],
                },
                "price_per_kg": {
                    "type": "number",
                    "minimum": 0.01,
                    "maximum": 100.0,
                },
            },
            "required": ["item_id", "price_per_kg"],
        },
    },
    {
        "name": "advance_day",
        "description": "Simulate the current business day and move to the next day.",
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
    text = "\n".join(block.text for block in result.blocks)
    return text, result.finished, result.reward


def run_agent() -> float:
    api_key = os.getenv("ANTHROPIC_API_KEY")
    if not api_key:
        print("ERROR: ANTHROPIC_API_KEY is not set.", file=sys.stderr)
        sys.exit(1)

    llm = anthropic.Anthropic(api_key=api_key)
    ors_client = ORS(base_url=ORS_BASE_URL)

    env = ors_client.environment("minicoffeeenv")
    task = env.list_tasks(split="train")[0]
    task_spec = task.task_spec

    print(f"Task: {task_spec['id']} — {task_spec['description']}")
    print(f"Model: {MODEL}")
    print("-" * 60)

    final_reward = 0.0

    with env.session(task=task) as session:
        prompt_blocks = session.get_prompt()
        prompt_text = "\n".join(block.text for block in prompt_blocks)
        strategy_hint = (
            "Run the business for the full 7-day horizon. "
            "Use the tools to inspect state, buy inventory, set prices, and advance the day. "
            "Do not stop early. Call finish_episode only after the horizon is complete "
            "or if continuing is impossible."
        )

        print("PROMPT:")
        print(prompt_text)
        print("-" * 60)

        messages: list[dict] = [
            {"role": "user", "content": prompt_text},
            {"role": "user", "content": strategy_hint},
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

            for block in response.content:
                if block.type == "text" and block.text:
                    print(f"[LLM] {block.text}")

            tool_use_blocks = [block for block in response.content if block.type == "tool_use"]
            if not tool_use_blocks:
                print("Agent stopped without calling a tool.")
                break

            tool_results = []
            episode_done = False

            for block in tool_use_blocks:
                tool_name = block.name
                arguments = block.input if isinstance(block.input, dict) else {}
                print(f"[TOOL] {tool_name}({arguments})")

                result_text, finished, reward = _call_ors_tool(
                    session, tool_name, arguments
                )
                print(result_text)

                tool_results.append(
                    {
                        "type": "tool_result",
                        "tool_use_id": block.id,
                        "content": result_text,
                    }
                )

                if finished:
                    final_reward = reward
                    episode_done = True

            messages.append({"role": "user", "content": tool_results})

            if episode_done:
                print("-" * 60)
                print(f"Episode complete. Reward: {final_reward:.4f}")
                break
        else:
            print(f"Reached max turns ({MAX_TURNS}).")

    return final_reward


if __name__ == "__main__":
    reward = run_agent()
    print(f"\nFinal reward: {reward:.4f}")
