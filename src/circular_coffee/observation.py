from __future__ import annotations

from dataclasses import asdict

from .models import MarketState


def build_observation(state: MarketState, agent_id: str) -> dict:
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
    return {
        "day": state.day,
        "remaining_days": state.max_days - state.day,
        "self": {
            "agent_id": agent.agent_id,
            "role": agent.role,
            "cash": agent.cash,
            "reported_revenue": agent.reported_revenue,
            "revenue_target": agent.revenue_target,
            "target_bonus": agent.target_bonus,
            "inventory": inventory,
        },
        "incoming_pending_proposals": incoming,
        "other_agent_ids": [other_id for other_id in state.agents if other_id != agent_id],
    }
