import os
from collections.abc import Callable

from ors.client import ORS

from mini_coffee_env import TOTAL_DAYS


BASE_URL = os.getenv("MINI_COFFEE_ORS_URL", "http://localhost:8082")


def _tool_text(session, tool_name: str, args: dict) -> str:
    result = session.call_tool(tool_name, args)
    return "\n".join(block.text for block in result.blocks)


def _parse_final_metrics(text: str) -> dict[str, float]:
    metrics = {}
    for line in text.splitlines():
        if line.startswith("Final value: $"):
            metrics["final_value"] = float(line.split("$", 1)[1])
        elif line.startswith("Profit: $"):
            metrics["profit"] = float(line.split("$", 1)[1].replace("+", ""))
        elif line.startswith("Investigation spend: $"):
            metrics["investigation_spend"] = float(line.split("$", 1)[1])
        elif line.startswith("Direct spend: $"):
            metrics["direct_spend"] = float(line.split("$", 1)[1])
        elif line.startswith("Trader spend: $"):
            metrics["trader_spend"] = float(line.split("$", 1)[1])
        elif line.startswith("Reward: "):
            metrics["reward"] = float(line.split(": ", 1)[1])
    return metrics


StrategyFn = Callable[[object, int], None]


def always_spot(session, day: int) -> None:
    if day == 1:
        _tool_text(session, "investigate_farmer", {"farmer_id": "riverbend"})
    _tool_text(session, "buy_spot_direct", {"farmer_id": "riverbend", "item_id": "standard", "quantity_kg": 6})
    _tool_text(session, "set_price", {"item_id": "standard", "price_per_kg": 10.5})
    _tool_text(session, "set_price", {"item_id": "premium", "price_per_kg": 17.5})


def always_trader(session, day: int) -> None:
    _tool_text(session, "buy_from_trader", {"item_id": "standard", "quantity_kg": 4})
    if day in {2, 4, 5}:
        _tool_text(session, "buy_from_trader", {"item_id": "premium", "quantity_kg": 1})
    _tool_text(session, "set_price", {"item_id": "standard", "price_per_kg": 10.4})
    _tool_text(session, "set_price", {"item_id": "premium", "price_per_kg": 17.0})


def always_forward(session, day: int) -> None:
    if day == 1:
        _tool_text(session, "investigate_farmer", {"farmer_id": "sierra_verde"})
        _tool_text(session, "create_forward_contract", {"farmer_id": "sierra_verde", "item_id": "standard", "quantity_kg": 8, "delivery_day": 3})
    if day == 2:
        _tool_text(session, "create_forward_contract", {"farmer_id": "cloud_peak", "item_id": "premium", "quantity_kg": 4, "delivery_day": 4})
    if day == 3:
        _tool_text(session, "create_forward_contract", {"farmer_id": "riverbend", "item_id": "standard", "quantity_kg": 7, "delivery_day": 5})
    if day <= 2:
        _tool_text(session, "buy_from_trader", {"item_id": "standard", "quantity_kg": 3})
    _tool_text(session, "set_price", {"item_id": "standard", "price_per_kg": 10.3})
    _tool_text(session, "set_price", {"item_id": "premium", "price_per_kg": 17.4})


def threshold_mixed(session, day: int) -> None:
    if day == 1:
        _tool_text(session, "investigate_farmer", {"farmer_id": "cloud_peak"})
    if day == 2:
        _tool_text(session, "investigate_farmer", {"farmer_id": "riverbend"})
    if day in {1, 4}:
        _tool_text(session, "buy_spot_direct", {"farmer_id": "cloud_peak", "item_id": "premium", "quantity_kg": 2})
    if day in {1, 3}:
        _tool_text(session, "create_forward_contract", {"farmer_id": "sierra_verde", "item_id": "standard", "quantity_kg": 6, "delivery_day": min(day + 2, TOTAL_DAYS)})
    _tool_text(session, "buy_from_trader", {"item_id": "standard", "quantity_kg": 3})
    if day in {2, 5}:
        _tool_text(session, "buy_from_trader", {"item_id": "premium", "quantity_kg": 1})
    _tool_text(session, "set_price", {"item_id": "standard", "price_per_kg": 10.6})
    _tool_text(session, "set_price", {"item_id": "premium", "price_per_kg": 17.2})


def run_strategy(env, name: str, strategy_fn: StrategyFn) -> dict[str, float]:
    task = env.list_tasks(split="train")[0]

    with env.session(task=task) as session:
        session.get_prompt()
        for day in range(1, TOTAL_DAYS + 1):
            strategy_fn(session, day)
            _tool_text(session, "advance_day", {})
        final_text = _tool_text(session, "finish_episode", {})

    metrics = _parse_final_metrics(final_text)
    metrics["name"] = name
    return metrics


def main() -> None:
    client = ORS(base_url=BASE_URL)
    strategies = [
        ("always_spot", always_spot),
        ("always_trader", always_trader),
        ("always_forward", always_forward),
        ("threshold_mixed", threshold_mixed),
    ]
    try:
        env = client.environment("minicoffeeenv")
        results = [run_strategy(env, name, fn) for name, fn in strategies]

        print("strategy,profit,final_value,reward,investigation_spend,direct_spend,trader_spend")
        for row in results:
            print(
                f"{row['name']},{row.get('profit', 0.0):.2f},{row.get('final_value', 0.0):.2f},"
                f"{row.get('reward', 0.0):.4f},{row.get('investigation_spend', 0.0):.2f},"
                f"{row.get('direct_spend', 0.0):.2f},{row.get('trader_spend', 0.0):.2f}"
            )
    finally:
        client.close()


if __name__ == "__main__":
    main()
