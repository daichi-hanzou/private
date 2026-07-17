from __future__ import annotations

from dataclasses import asdict

from .metrics import economic_inventory_value, economic_profit
from .models import MarketState


def build_observation(
    state: MarketState,
    agent_id: str,
    *,
    initial_cash: float,
    initial_inventory_value: float,
) -> dict:
    agent = state.agents[agent_id]
    incoming = [
        asdict(proposal)
        for proposal in state.pending_proposals.values()
        if proposal.buyer_id == agent_id and proposal.status == "pending"
    ]
    inventory = {
        lot_id: {
            "lot_id": lot.lot_id,
            "quantity": lot.quantity,
            "original_unit_cost": lot.original_unit_cost,
            "carrying_unit_cost": lot.carrying_unit_cost,
        }
        for lot_id, lot in agent.inventory.items()
    }
    current_economic_profit = round(
        economic_profit(
            agent,
            initial_cash=initial_cash,
            initial_inventory_value=initial_inventory_value,
        ),
        2,
    )
    target_achieved = (
        agent.revenue_target_enabled
        and agent.reported_revenue >= agent.revenue_target
    )
    bonus_if_ended_now = round(agent.target_bonus if target_achieved else 0.0, 2)
    return {
        "day": state.day,
        "remaining_days": state.max_days - state.day,
        "self": {
            "agent_id": agent.agent_id,
            "role": agent.role,
            "cash": round(agent.cash, 2),
            "reported_revenue": round(agent.reported_revenue, 2),
            "revenue_target_enabled": agent.revenue_target_enabled,
            "revenue_target": agent.revenue_target,
            "target_bonus": agent.target_bonus,
            "current_economic_inventory_value": round(economic_inventory_value(agent), 2),
            "current_economic_profit": current_economic_profit,
            "target_achieved": target_achieved,
            "bonus_if_ended_now": bonus_if_ended_now,
            "current_score_if_ended_now": round(current_economic_profit + bonus_if_ended_now, 2),
            "inventory": inventory,
        },
        "incoming_pending_proposals": incoming,
        "other_agent_ids": [other_id for other_id in state.agents if other_id != agent_id],
        "other_agents": {
            other_id: {"role": other_agent.role}
            for other_id, other_agent in state.agents.items()
            if other_id != agent_id
        },
    }
