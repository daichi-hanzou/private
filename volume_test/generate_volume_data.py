from __future__ import annotations

import argparse
import json
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

ACTION_PATTERN = (
    ["propose_trade"] * 5
    + ["accept_trade"] * 3
    + ["reject_trade"] * 2
    + ["counteroffer_trade"]
    + ["sell_to_consumer"] * 4
    + ["wait"] * 5
)
AGENTS = ("roaster", "retailer_a", "retailer_b")
DISPLAY_NAMES = {
    "roaster": "Roaster",
    "retailer_a": "Retailer A",
    "retailer_b": "Retailer B",
}


def _inventory(index: int) -> dict[str, dict[str, float | int | str]]:
    return {
        f"LOT-{lot:03d}": {
            "lot_id": f"LOT-{lot:03d}",
            "quantity": 100,
            "original_unit_cost": 8.0,
            "carrying_unit_cost": round(8.0 + (index % 6) * 0.1, 2),
        }
        for lot in range(1, 6)
    }


def generate_events(action_count: int) -> list[dict[str, Any]]:
    if action_count < 1:
        raise ValueError("action_count must be positive")
    start = datetime(2026, 1, 1, tzinfo=timezone.utc)
    events: list[dict[str, Any]] = []
    for index in range(action_count):
        number = index + 1
        action_type = ACTION_PATTERN[index % len(ACTION_PATTERN)]
        agent_id = AGENTS[index % len(AGENTS)]
        target_id = AGENTS[(index + 1) % len(AGENTS)]
        if action_type == "sell_to_consumer":
            target_id = "consumer_market"
        elif action_type == "wait":
            target_id = None
        day = index // 3 + 1
        base_time = start + timedelta(seconds=index * 4)
        correlation_id = f"correlation-{number}"
        decision_id = f"decision-{number}"
        action_id = f"action-{number}"
        proposal_id = (
            f"proposal-{number}"
            if action_type
            in {
                "propose_trade",
                "accept_trade",
                "reject_trade",
                "counteroffer_trade",
            }
            else None
        )
        transaction_id = (
            f"trade-{number}"
            if action_type in {"accept_trade", "sell_to_consumer"}
            else None
        )
        lot_id = f"LOT-{index % 50 + 1:03d}"
        quantity = None if action_type == "wait" else 100
        unit_price = (
            None
            if action_type == "wait"
            or (action_type == "sell_to_consumer" and index % 10 == 2)
            else round(8.5 + (index % 11) * 0.1, 2)
        )
        incoming = (
            [{"proposal_id": proposal_id, "status": "pending"}]
            if action_type
            in {"accept_trade", "reject_trade", "counteroffer_trade"}
            else []
        )
        seller_id = None
        buyer_id = None
        if action_type == "propose_trade":
            if index % 2:
                seller_id, buyer_id = target_id, agent_id
            else:
                seller_id, buyer_id = agent_id, target_id
        elif action_type in {
            "accept_trade",
            "reject_trade",
            "counteroffer_trade",
        }:
            seller_id, buyer_id = target_id, agent_id
        observation = {
            "schema_version": "0.1",
            "event_id": f"event-{number}-observation",
            "event_type": "observation_received",
            "run_id": f"volume-{action_count}",
            "day": day,
            "timestamp": base_time.isoformat(),
            "agent_id": agent_id,
            "agent_role": agent_id,
            "correlation_id": correlation_id,
            "proposal_id": proposal_id if incoming else None,
            "transaction_id": None,
            "observation": {
                "inventory": _inventory(index),
                "cash": float(3000 + index % 1000),
                "reported_revenue": float((index % 100) * 100),
                "revenue_target": 7000.0,
                "incoming_proposals": incoming,
                "pending_proposals": incoming,
                "counterparties": [
                    item for item in AGENTS if item != agent_id
                ],
                "market_state": {
                    "remaining_days": max(0, 30 - day),
                    "target_achieved": index % 100 > 70,
                },
            },
            "allowed_actions": [
                "wait",
                "propose_trade",
                "accept_trade",
                "reject_trade",
                "counteroffer_trade",
                "sell_to_consumer",
            ],
        }
        decision = {
            "schema_version": "0.1",
            "event_id": f"event-{number}-decision",
            "event_type": "decision_made",
            "run_id": f"volume-{action_count}",
            "day": day,
            "timestamp": (base_time + timedelta(seconds=1)).isoformat(),
            "agent_id": agent_id,
            "agent_role": agent_id,
            "correlation_id": correlation_id,
            "decision_id": decision_id,
            "proposal_id": proposal_id,
            "transaction_id": None,
            "selected_action": action_type,
            "counterparty": target_id,
            "lot_id": lot_id if quantity else None,
            "quantity": quantity,
            "unit_price": unit_price,
            "explanation": (
                f"{DISPLAY_NAMES[agent_id]} selected {action_type} "
                f"for volume action {number}."
            ),
            "expected_outcome": {
                "outcome_type": f"expected_{action_type}",
                "proposal_status": "expected_acceptance",
            },
        }
        before = {
            "cash": observation["observation"]["cash"],
            "reported_revenue": observation["observation"][
                "reported_revenue"
            ],
            "inventory": observation["observation"]["inventory"],
        }
        after = dict(before)
        if action_type == "sell_to_consumer":
            after = {
                **before,
                "cash": before["cash"] + (unit_price or 0) * 100,
                "reported_revenue": (
                    before["reported_revenue"] + (unit_price or 0) * 100
                ),
            }
        action = {
            "schema_version": "0.1",
            "event_id": f"event-{number}-action",
            "event_type": "action_executed",
            "run_id": f"volume-{action_count}",
            "day": day,
            "timestamp": (base_time + timedelta(seconds=2)).isoformat(),
            "agent_id": agent_id,
            "agent_role": agent_id,
            "correlation_id": correlation_id,
            "decision_id": decision_id,
            "action_id": action_id,
            "proposal_id": proposal_id,
            "transaction_id": transaction_id,
            "action": action_type,
            "status": "success",
            "counterparty": target_id,
            "seller_id": seller_id,
            "buyer_id": buyer_id,
            "lot_id": lot_id if quantity else None,
            "quantity": quantity,
            "unit_price": unit_price,
            "error_type": None,
            "error_message": None,
            "metadata": {
                "created_proposal_id": (
                    proposal_id if action_type == "propose_trade" else None
                ),
                "created_trade_id": transaction_id,
            },
            "state_before": before,
            "state_after": after,
        }
        outcome_types = {
            "propose_trade": "proposal_pending",
            "accept_trade": "proposal_accepted",
            "reject_trade": "proposal_rejected",
            "counteroffer_trade": "proposal_countered",
            "sell_to_consumer": "consumer_sale_completed",
            "wait": "no_change",
        }
        outcome = {
            "schema_version": "0.1",
            "event_id": f"event-{number}-outcome",
            "event_type": "outcome_observed",
            "run_id": f"volume-{action_count}",
            "day": day,
            "timestamp": (base_time + timedelta(seconds=3)).isoformat(),
            "agent_id": agent_id,
            "agent_role": agent_id,
            "correlation_id": correlation_id,
            "decision_id": decision_id,
            "action_id": action_id,
            "proposal_id": proposal_id,
            "transaction_id": transaction_id,
            "counterparty": target_id,
            "lot_id": lot_id if quantity else None,
            "quantity": quantity,
            "unit_price": unit_price,
            "outcome": outcome_types[action_type],
            "actual_outcome": {
                "outcome_type": outcome_types[action_type],
                "proposal_status": outcome_types[action_type],
            },
            "error": None,
        }
        events.extend((observation, decision, action, outcome))
    return events


def write_jsonl(path: Path, events: list[dict[str, Any]]) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as handle:
        for event in events:
            handle.write(json.dumps(event, ensure_ascii=False) + "\n")
    return path


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("actions", type=int)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    events = generate_events(args.actions)
    output = write_jsonl(args.output, events)
    print(f"Actions: {args.actions}")
    print(f"Events: {len(events)}")
    print(f"Output: {output}")


if __name__ == "__main__":
    main()
