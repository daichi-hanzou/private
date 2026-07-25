from __future__ import annotations

from collections.abc import Mapping
from dataclasses import asdict, is_dataclass
from pathlib import Path
from typing import Any

from ..models import AgentAction

SCHEMA_VERSION = "0.1"


def safe_jsonable(value: Any) -> Any:
    if is_dataclass(value):
        return safe_jsonable(asdict(value))
    if isinstance(value, Mapping):
        return {
            str(key): safe_jsonable(inner)
            for key, inner in value.items()
        }
    if isinstance(value, (list, tuple, set)):
        return [safe_jsonable(inner) for inner in value]
    if isinstance(value, Path):
        return str(value)
    if value is None or isinstance(value, (str, int, float, bool)):
        return value
    try:
        return str(value)
    except Exception:
        return f"<unserializable {type(value).__name__}>"


def _market_prices(market_information: dict) -> dict:
    return {
        key: value
        for key, value in market_information.items()
        if "price" in key
    }


def build_observation_payload(
    observation: dict,
    *,
    goal: str,
    pending_proposals: list[dict],
) -> dict:
    self_view = observation.get("self", {})
    if not self_view:
        self_view = {
            "inventory": observation.get("inventory", []),
            "cash": observation.get("cash"),
            "reported_revenue": observation.get("reported_revenue"),
            "revenue_target": observation.get("revenue_target"),
        }
    incoming = observation.get(
        "incoming_pending_proposals",
        observation.get("incoming_trade_proposals", []),
    )
    allowed_actions = observation.get(
        "available_action_types",
        observation.get("allowed_actions", []),
    )
    market_information = observation.get("market_information", {})
    if not market_information and "consumer_market" in observation:
        market_information = {
            "consumer_market": observation["consumer_market"],
        }
    return {
        "goal": goal,
        "observation": {
            "inventory": self_view.get(
                "inventory",
                observation.get("inventory", []),
            ),
            "cash": self_view.get("cash", observation.get("cash")),
            "reported_revenue": self_view.get(
                "reported_revenue",
                observation.get("reported_revenue"),
            ),
            "revenue_target": self_view.get(
                "revenue_target",
                observation.get("revenue_target"),
            ),
            "incoming_proposals": incoming,
            "pending_proposals": pending_proposals,
            "counterparties": observation.get("other_agent_ids", []),
            "market_prices": _market_prices(market_information),
            "market_state": {
                "remaining_days": observation.get(
                    "remaining_days",
                    observation.get("days_remaining"),
                ),
                "target_achieved": self_view.get(
                    "target_achieved",
                    observation.get("target_achieved"),
                ),
            },
        },
        "allowed_actions": list(allowed_actions),
    }


def build_decision_payload(
    action: AgentAction,
    *,
    agent_id: str,
    raw_model_output: Any = None,
) -> dict:
    counterparty = action.buyer_id
    if counterparty == agent_id:
        counterparty = action.seller_id
    return {
        "selected_action": action.action_type,
        "counterparty": counterparty,
        "lot_id": action.lot_id,
        "quantity": action.quantity,
        "unit_price": action.unit_price,
        "proposal_id": action.proposal_id,
        "explanation": action.reason_summary,
        "expected_outcome": action.expected_outcome,
        "raw_model_output": raw_model_output,
    }
