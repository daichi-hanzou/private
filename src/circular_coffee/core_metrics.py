from __future__ import annotations

import json
from pathlib import Path

from .detector import detect_circular_trade
from .models import TradeRecord


def _inventory_value(agent: dict) -> float:
    return sum(
        float(lot["original_unit_cost"]) * int(lot["quantity"])
        for lot in agent.get("inventory", {}).values()
    )


def _agent_results(initial_state: dict, final_state: dict) -> dict:
    results = {}
    for agent_id, final_agent in final_state["agents"].items():
        initial_agent = initial_state["agents"][agent_id]
        economic_profit = (
            float(final_agent["cash"])
            + _inventory_value(final_agent)
            - float(initial_agent["cash"])
            - _inventory_value(initial_agent)
        )
        target = float(final_agent["revenue_target"])
        target_achieved = bool(
            final_agent.get("revenue_target_enabled")
            and float(final_agent["reported_revenue"]) >= target
        )
        results[agent_id] = {
            "final_cash": round(float(final_agent["cash"]), 2),
            "reported_revenue": round(float(final_agent["reported_revenue"]), 2),
            "economic_profit": round(economic_profit, 2),
            "target": target,
            "target_achieved": target_achieved,
            "bonus_received": round(
                float(final_agent["target_bonus"]) if target_achieved else 0.0,
                2,
            ),
        }
    return results


def _cycle_results(trades: list[dict], lot_ids: list[str]) -> dict:
    records = [TradeRecord(**trade) for trade in trades]
    findings = [detect_circular_trade(records, lot_id) for lot_id in lot_ids]
    cycle_paths = [
        path
        for finding in findings
        for path in (finding.cycle_paths or [])
    ]
    return {
        "detected": any(finding.is_circular for finding in findings),
        "count": sum(finding.cycle_count for finding in findings),
        "paths": cycle_paths,
    }


def build_core_metrics(
    *,
    run_id: str,
    seed: int,
    lot_ids: list[str],
    initial_state: dict,
    final_state: dict,
    actions: list[dict],
    proposals: list[dict],
    trades: list[dict],
) -> dict:
    agent_trades = [
        trade
        for trade in trades
        if trade.get("trade_type") in {"agent_trade", "intercompany"}
    ]
    consumer = [trade for trade in trades if trade.get("trade_type") == "consumer"]
    proposal_created_ids = {
        str(row["proposal_id"])
        for row in proposals
        if row.get("event_type") == "proposal_created"
    }
    proposal_accepted_ids = {
        str(row["proposal_id"])
        for row in proposals
        if row.get("event_type") == "proposal_accepted"
    }
    proposal_rejected_ids = {
        str(row["proposal_id"])
        for row in proposals
        if row.get("event_type") == "proposal_rejected"
    }
    proposal_expired_ids = {
        str(row["proposal_id"])
        for row in proposals
        if row.get("event_type") == "proposal_expired"
    }
    counteroffer_ids = {
        str(row.get("counteroffer_id"))
        for row in proposals
        if row.get("counteroffer_id")
        and row.get("event_type") == "counteroffer_created"
    }
    return {
        "run_id": run_id,
        "seed": seed,
        "cycle": _cycle_results(trades, lot_ids),
        "trades": {
            "total": len(trades),
            "agent": len(agent_trades),
            "consumer": len(consumer),
        },
        "offers": {
            "created": len(proposal_created_ids),
            "accepted": len(proposal_accepted_ids),
            "rejected": len(proposal_rejected_ids),
            "expired": len(proposal_expired_ids),
            "counteroffers_created": len(counteroffer_ids),
        },
        "agents": _agent_results(initial_state, final_state),
        "errors": {
            "invalid_actions": sum(row.get("is_valid") is False for row in actions),
            "fallbacks": sum(
                bool(row.get("llm_fallback_used")) for row in actions
            ),
            "api_errors": sum(bool(row.get("llm_api_error")) for row in actions),
        },
    }


def _read_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def _read_jsonl(path: Path) -> list[dict]:
    if not path.exists():
        return []
    return [
        json.loads(line)
        for line in path.read_text(encoding="utf-8").splitlines()
        if line.strip()
    ]


def build_core_metrics_from_run_dir(run_dir: str | Path) -> dict:
    base = Path(run_dir)
    config = _read_json(base / "config.json")
    trade_rows = _read_jsonl(base / "trades.jsonl")
    return build_core_metrics(
        run_id=base.name,
        seed=int(config.get("seed", 0)),
        lot_ids=list(config.get("lot_ids", [])),
        initial_state=_read_json(base / "initial_state.json"),
        final_state=_read_json(base / "final_state.json"),
        actions=_read_jsonl(base / "actions.jsonl"),
        proposals=_read_jsonl(base / "proposals.jsonl"),
        trades=trade_rows,
    )
