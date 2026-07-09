from __future__ import annotations

import json
import os
import re
import sys
import time
from pathlib import Path

from dotenv import load_dotenv
from ors.client import ORS

from mini_coffee_event_logger import EventLogger
from mini_coffee_env import FARMER_PROFILES


load_dotenv(Path(__file__).with_name(".env"))

ORS_BASE_URL = os.getenv("MINI_COFFEE_ORS_URL", "http://localhost:8082")
LLM_PROVIDER = os.getenv("MINI_COFFEE_LLM_PROVIDER", "anthropic").lower()
ANTHROPIC_MODEL = os.getenv("ANTHROPIC_MODEL", "claude-opus-4-8")
OPENAI_MODEL = os.getenv("OPENAI_MODEL", "gpt-5")
AZURE_OPENAI_DEPLOYMENT = os.getenv("AZURE_OPENAI_DEPLOYMENT", "")
AZURE_OPENAI_API_VERSION = os.getenv("AZURE_OPENAI_API_VERSION", "2024-10-21")
TOTAL_DAYS = int(os.getenv("MINI_COFFEE_TOTAL_DAYS", "20"))
FARMER_IDS = list(FARMER_PROFILES)
MAX_TURNS = 90
AUTO_FINISH_MARKER = "horizon is complete. Call finish_episode."
# Frozen for the v2 calibration. Changing this hint changes the LLM treatment
# and requires rerunning robot calibration and LLM smoke comparisons.
STRATEGY_HINT = (
    "Balance cheap but risky direct procurement against immediate trader inventory. "
    "Use investigations sparingly and consider forward contracts when future supply matters. "
    "On the first turn, avoid waiting without action. "
    "Before any tool call on every turn, first write a brief 1-3 sentence decision note that explains "
    "your current view of inventory, supply risk, and the next action you will take."
)


def _new_agent_logger(provider: str, deployment: str = "") -> EventLogger:
    log_dir = Path(
        os.getenv(
            "MINI_COFFEE_AGENT_LOG_DIR",
            str(Path(__file__).resolve().parent / "workspace" / "output" / "mini_coffee_agent_logs"),
        )
    )
    log_dir.mkdir(parents=True, exist_ok=True)
    stamp = time.strftime("%Y%m%d_%H%M%S")
    suffix = deployment or provider
    return EventLogger(str(log_dir / f"agent_{stamp}_{suffix}.jsonl"))


def _anthropic_block_to_dict(block) -> dict:
    if block.type == "text":
        return {"type": "text", "text": block.text}
    if block.type == "tool_use":
        return {"type": "tool_use", "id": block.id, "name": block.name, "input": block.input}
    return {"type": block.type, "repr": repr(block)}


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
            "properties": {"farmer_id": {"type": "string", "enum": FARMER_IDS}},
            "required": ["farmer_id"],
        },
    },
    {
        "name": "buy_spot_direct",
        "description": "Prepay a spot order from a farmer. Delivery is processed on advance_day.",
        "input_schema": {
            "type": "object",
            "properties": {
                "farmer_id": {"type": "string", "enum": FARMER_IDS},
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
                "farmer_id": {"type": "string", "enum": FARMER_IDS},
                "item_id": {"type": "string", "enum": ["standard", "premium"]},
                "quantity_kg": {"type": "integer", "minimum": 1, "maximum": 100},
                "delivery_day": {"type": "integer", "minimum": 2, "maximum": TOTAL_DAYS},
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

assert all(not tool["name"].startswith("debug_") for tool in TOOL_SCHEMAS)
assert all(not tool["function"]["name"].startswith("debug_") for tool in OPENAI_TOOL_SCHEMAS)

PROMPT_DAYS_RE = re.compile(r"\bfor\s+(\d+)\s+days\b", re.IGNORECASE)


def _call_ors_tool(session, tool_name: str, arguments: dict) -> tuple[str, bool, float]:
    result = session.call_tool(tool_name, arguments)
    return "\n".join(block.text for block in result.blocks), result.finished, result.reward


def _prompt_total_days(prompt_text: str) -> int:
    match = PROMPT_DAYS_RE.search(prompt_text)
    if not match:
        raise RuntimeError("Could not find the episode horizon in the environment prompt.")
    return int(match.group(1))


def _validate_prompt_total_days(prompt_text: str) -> None:
    prompt_days = _prompt_total_days(prompt_text)
    if prompt_days != TOTAL_DAYS:
        raise RuntimeError(
            f"Prompt horizon mismatch: runner TOTAL_DAYS={TOTAL_DAYS}, prompt says {prompt_days} days."
        )


def _print_llm_note(text: str) -> None:
    note = (text or "").strip()
    if not note:
        return
    for line in note.splitlines():
        cleaned = line.strip()
        if cleaned:
            print(f"[LLM] {cleaned}")


def _auto_continue_after_no_tool_call(session, turn: int) -> tuple[str, bool, float]:
    if turn == 1:
        print("[AUTO] No tool call detected on turn 1. Viewing state automatically.")
        result_text, finished, reward = _call_ors_tool(session, "view_state", {})
        print(result_text)
        return result_text, finished, reward

    print("[AUTO] No tool call detected. Advancing the simulation automatically.")
    result_text, finished, reward = _call_ors_tool(session, "advance_day", {})
    print(result_text)
    if finished:
        return result_text, finished, reward
    if AUTO_FINISH_MARKER not in result_text:
        return result_text, False, reward

    print("[AUTO] Horizon complete. Finishing the episode automatically.")
    finish_text, finished, reward = _call_ors_tool(session, "finish_episode", {})
    print(finish_text)
    combined = f"{result_text}\n\n{finish_text}"
    return combined, finished, reward


def _force_finish_after_max_turns(session, agent_logger) -> float:
    final_reward = 0.0
    for step in range(1, TOTAL_DAYS + 2):
        result_text, finished, reward = _call_ors_tool(session, "advance_day", {})
        print(f"[AUTO] max_turns cleanup advance_day step {step}")
        print(result_text)
        agent_logger.emit(
            "tool_result",
            turn=MAX_TURNS,
            tool="advance_day",
            tool_call_id=f"max-turns-advance-{step}",
            result=result_text,
            finished=finished,
            reward=reward,
            reason="max_turns",
        )
        final_reward = reward
        if finished:
            return final_reward
        if AUTO_FINISH_MARKER in result_text:
            break

    finish_text, finished, reward = _call_ors_tool(session, "finish_episode", {})
    print("[AUTO] max_turns cleanup finish_episode")
    print(finish_text)
    agent_logger.emit(
        "tool_result",
        turn=MAX_TURNS,
        tool="finish_episode",
        tool_call_id="max-turns-finish",
        result=finish_text,
        finished=finished,
        reward=reward,
        reason="max_turns",
    )
    return reward


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
    from azure.identity import DefaultAzureCredential, get_bearer_token_provider
    from openai import AzureOpenAI

    endpoint = _require_env("AZURE_OPENAI_ENDPOINT")
    deployment = _require_env("AZURE_OPENAI_DEPLOYMENT")
    api_key = os.getenv("AZURE_OPENAI_API_KEY")
    bearer_token = os.getenv("AZURE_OPENAI_TOKEN")
    use_default_credential = os.getenv("AZURE_OPENAI_USE_DEFAULT_CREDENTIAL", "false").lower() in {
        "1",
        "true",
        "yes",
    }

    kwargs = {
        "azure_endpoint": endpoint,
        "api_version": AZURE_OPENAI_API_VERSION,
    }
    if use_default_credential:
        credential = DefaultAzureCredential()
        kwargs["azure_ad_token_provider"] = get_bearer_token_provider(
            credential,
            "https://cognitiveservices.azure.com/.default",
        )
    elif bearer_token:
        kwargs["azure_ad_token"] = bearer_token
    elif api_key:
        kwargs["api_key"] = api_key
    else:
        print(
            "ERROR: Set AZURE_OPENAI_API_KEY, AZURE_OPENAI_TOKEN, "
            "or AZURE_OPENAI_USE_DEFAULT_CREDENTIAL=true.",
            file=sys.stderr,
        )
        sys.exit(1)
    return AzureOpenAI(**kwargs), deployment


def _openai_client():
    from openai import OpenAI

    kwargs = {"api_key": _require_env("OPENAI_API_KEY")}
    base_url = os.getenv("OPENAI_BASE_URL")
    organization = os.getenv("OPENAI_ORG_ID")
    project = os.getenv("OPENAI_PROJECT")
    if base_url:
        kwargs["base_url"] = base_url
    if organization:
        kwargs["organization"] = organization
    if project:
        kwargs["project"] = project
    return OpenAI(**kwargs), OPENAI_MODEL


def _run_agent_anthropic(session, prompt_text: str) -> float:
    llm = _anthropic_client()
    agent_logger = _new_agent_logger("anthropic", ANTHROPIC_MODEL)
    agent_logger.emit(
        "agent_start",
        provider="anthropic",
        model=ANTHROPIC_MODEL,
        total_days=TOTAL_DAYS,
        max_turns=MAX_TURNS,
        prompt=prompt_text,
        strategy_hint=STRATEGY_HINT,
    )
    messages = _build_initial_messages(prompt_text)
    final_reward = 0.0

    try:
        for turn in range(1, MAX_TURNS + 1):
            print(f"\nTurn {turn}")
            response = llm.messages.create(
                model=ANTHROPIC_MODEL,
                max_tokens=4096,
                tools=TOOL_SCHEMAS,
                messages=messages,
            )
            agent_logger.emit(
                "llm_response",
                turn=turn,
                content=[_anthropic_block_to_dict(block) for block in response.content],
                stop_reason=response.stop_reason,
            )
            messages.append({"role": "assistant", "content": response.content})
            assistant_text = "\n".join(
                block.text.strip()
                for block in response.content
                if getattr(block, "type", None) == "text" and getattr(block, "text", "").strip()
            )
            _print_llm_note(assistant_text)

            tool_use_blocks = [block for block in response.content if block.type == "tool_use"]
            if not tool_use_blocks:
                result_text, finished, reward = _auto_continue_after_no_tool_call(session, turn)
                agent_logger.emit(
                    "tool_result",
                    turn=turn,
                    tool="auto_continue_after_no_tool_call",
                    tool_call_id=f"auto-turn-{turn}",
                    result=result_text,
                    finished=finished,
                    reward=reward,
                )
                messages.append(
                    {
                        "role": "user",
                        "content": (
                            "System fallback: you did not call a tool, so the runner automatically "
                            f"continued the environment.\n{result_text}"
                        ),
                    }
                )
                if finished:
                    final_reward = reward
                    agent_logger.emit("agent_stop", turn=turn, reason="finished", final_reward=final_reward)
                    return final_reward
                continue

            tool_results = []
            for block in tool_use_blocks:
                arguments = block.input if isinstance(block.input, dict) else {}
                print(f"[TOOL] {block.name}({arguments})")
                agent_logger.emit("tool_call", turn=turn, tool=block.name, arguments=arguments, tool_call_id=block.id)
                result_text, finished, reward = _call_ors_tool(session, block.name, arguments)
                print(result_text)
                agent_logger.emit(
                    "tool_result",
                    turn=turn,
                    tool=block.name,
                    tool_call_id=block.id,
                    result=result_text,
                    finished=finished,
                    reward=reward,
                )
                tool_results.append({"type": "tool_result", "tool_use_id": block.id, "content": result_text})
                if finished:
                    final_reward = reward
                    messages.append({"role": "user", "content": tool_results})
                    agent_logger.emit("agent_stop", turn=turn, reason="finished", final_reward=final_reward)
                    return final_reward

            messages.append({"role": "user", "content": tool_results})

        final_reward = _force_finish_after_max_turns(session, agent_logger)
        agent_logger.emit("agent_stop", turn=MAX_TURNS, reason="max_turns", final_reward=final_reward)
        return final_reward
    finally:
        agent_logger.close()


def _run_agent_openai_chat(session, prompt_text: str, client, model: str, provider: str) -> float:
    agent_logger = _new_agent_logger(provider, model)
    start_event = {
        "provider": provider,
        "model": model,
        "total_days": TOTAL_DAYS,
        "max_turns": MAX_TURNS,
        "prompt": prompt_text,
        "strategy_hint": STRATEGY_HINT,
    }
    if provider == "azure_openai":
        start_event["deployment"] = model
    agent_logger.emit("agent_start", **start_event)
    messages = [
        {"role": "system", "content": STRATEGY_HINT},
        {"role": "user", "content": prompt_text},
    ]
    final_reward = 0.0

    try:
        for turn in range(1, MAX_TURNS + 1):
            print(f"\nTurn {turn}")
            response = client.chat.completions.create(
                model=model,
                messages=messages,
                tools=OPENAI_TOOL_SCHEMAS,
                tool_choice="auto",
                max_completion_tokens=4096,
            )
            message = response.choices[0].message
            _print_llm_note(message.content or "")

            tool_calls = message.tool_calls or []
            serialized_tool_calls = [
                {
                    "id": tool_call.id,
                    "type": tool_call.type,
                    "function": {
                        "name": tool_call.function.name,
                        "arguments": tool_call.function.arguments,
                    },
                }
                for tool_call in tool_calls
            ]
            agent_logger.emit(
                "llm_response",
                turn=turn,
                content=message.content or "",
                tool_calls=serialized_tool_calls,
                finish_reason=response.choices[0].finish_reason,
            )
            assistant_message = {
                "role": "assistant",
                "content": message.content or "",
            }
            if serialized_tool_calls:
                assistant_message["tool_calls"] = serialized_tool_calls
            messages.append(assistant_message)
            if not tool_calls:
                result_text, finished, reward = _auto_continue_after_no_tool_call(session, turn)
                agent_logger.emit(
                    "tool_result",
                    turn=turn,
                    tool="auto_continue_after_no_tool_call",
                    tool_call_id=f"auto-turn-{turn}",
                    result=result_text,
                    finished=finished,
                    reward=reward,
                )
                messages.append(
                    {
                        "role": "user",
                        "content": (
                            "System fallback: you did not call a tool, so the runner automatically "
                            f"continued the environment.\n{result_text}"
                        ),
                    }
                )
                if finished:
                    final_reward = reward
                    agent_logger.emit("agent_stop", turn=turn, reason="finished", final_reward=final_reward)
                    return final_reward
                continue

            for tool_call in tool_calls:
                arguments = json.loads(tool_call.function.arguments or "{}")
                print(f"[TOOL] {tool_call.function.name}({arguments})")
                agent_logger.emit(
                    "tool_call",
                    turn=turn,
                    tool=tool_call.function.name,
                    arguments=arguments,
                    tool_call_id=tool_call.id,
                )
                result_text, finished, reward = _call_ors_tool(session, tool_call.function.name, arguments)
                print(result_text)
                agent_logger.emit(
                    "tool_result",
                    turn=turn,
                    tool=tool_call.function.name,
                    tool_call_id=tool_call.id,
                    result=result_text,
                    finished=finished,
                    reward=reward,
                )
                messages.append(
                    {
                        "role": "tool",
                        "tool_call_id": tool_call.id,
                        "content": result_text,
                    }
                )
                if finished:
                    final_reward = reward
                    agent_logger.emit("agent_stop", turn=turn, reason="finished", final_reward=final_reward)
                    return final_reward

        final_reward = _force_finish_after_max_turns(session, agent_logger)
        agent_logger.emit("agent_stop", turn=MAX_TURNS, reason="max_turns", final_reward=final_reward)
        return final_reward
    finally:
        agent_logger.close()


def _run_agent_openai(session, prompt_text: str) -> float:
    client, model = _openai_client()
    return _run_agent_openai_chat(session, prompt_text, client, model, "openai")


def _run_agent_azure_openai(session, prompt_text: str) -> float:
    client, deployment = _azure_openai_client()
    return _run_agent_openai_chat(session, prompt_text, client, deployment, "azure_openai")


def run_agent() -> float:
    client = ORS(base_url=ORS_BASE_URL)
    try:
        env = client.environment("minicoffeeenv")
        task = env.list_tasks(split="train")[0]

        with env.session(task=task) as session:
            prompt_text = "\n".join(block.text for block in session.get_prompt())
            _validate_prompt_total_days(prompt_text)
            if LLM_PROVIDER == "anthropic":
                return _run_agent_anthropic(session, prompt_text)
            if LLM_PROVIDER == "openai":
                return _run_agent_openai(session, prompt_text)
            if LLM_PROVIDER in {"azure_openai", "azure-openai"}:
                return _run_agent_azure_openai(session, prompt_text)

            print(
                "ERROR: MINI_COFFEE_LLM_PROVIDER must be 'anthropic', 'openai', or 'azure_openai'.",
                file=sys.stderr,
            )
            sys.exit(1)
    finally:
        client.close()


if __name__ == "__main__":
    reward = run_agent()
    print(f"\nFinal reward: {reward:.4f}")
