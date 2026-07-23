from __future__ import annotations

from dataclasses import asdict

from .config import SimulationConfig, can_agent_sell_to_consumer
from .market import (
    build_trade_candidates,
    get_active_lot_lock,
    proposal_responder_id,
)
from .metrics import economic_inventory_value, economic_profit
from .models import MarketState


def build_observation(
    state: MarketState,
    agent_id: str,
    *,
    initial_cash: float,
    initial_inventory_value: float,
    market_information: dict | None = None,
    config: SimulationConfig | None = None,
) -> dict:
    agent = state.agents[agent_id]
    incoming = [
        asdict(proposal)
        for proposal in state.active_proposals.values()
        if (
            proposal_responder_id(proposal) == agent_id
            and proposal.lot_id in state.agents[proposal.seller_id].inventory
            and state.agents[proposal.seller_id].inventory[proposal.lot_id].quantity
            == proposal.quantity
        )
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
    market_info = dict(market_information or {})
    if agent.role == "roaster":
        market_info.pop("retailer_a_max_purchase_unit_price", None)
        market_info.pop("retailer_b_max_purchase_unit_price", None)
    can_sell_to_consumer = (
        market_info.get("consumer_market_enabled", False)
        and (
            (
                agent.role == "roaster"
                and market_info.get("roaster_consumer_sale_enabled", True)
            )
            or (
                agent.role == "retailer"
                and market_info.get("retailer_consumer_sale_enabled", False)
            )
        )
    )
    market_info["available_consumer_sale_lot_ids"] = (
        list(agent.inventory)
        if can_sell_to_consumer
        else []
    )
    market_access = {
        "can_sell_to_consumer": can_sell_to_consumer,
        "can_sell_to_retailers": True,
    }
    trade_candidates = (
        build_trade_candidates(state, config, agent_id)
        if config is not None
        else {"sell": [], "buy": []}
    )
    incoming_counteroffers = [
        asdict(counteroffer)
        for counteroffer in state.pending_trade_counteroffers.values()
        if counteroffer.status == "pending"
        and counteroffer.responder_id == agent_id
    ]
    locked_lots = {
        lot_id: lock
        for lot_id in {
            proposal.lot_id for proposal in state.pending_proposals.values()
        }
        | {
            counteroffer.lot_id
            for counteroffer in state.pending_trade_counteroffers.values()
        }
        if (lock := get_active_lot_lock(state, lot_id)) is not None
    }
    available_action_types = ["wait"]
    if trade_candidates["sell"] or trade_candidates["buy"]:
        available_action_types.append("propose_trade")
    if incoming:
        available_action_types.extend(
            ["accept_trade", "reject_trade", "counteroffer_trade"]
        )
    if incoming_counteroffers:
        available_action_types.extend(
            ["accept_counteroffer", "reject_counteroffer"]
        )
    if can_sell_to_consumer and agent.inventory:
        available_action_types.append("sell_to_consumer")
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
        "incoming_trade_proposals": incoming,
        "incoming_trade_counteroffers": incoming_counteroffers,
        "trade_candidates": trade_candidates,
        "locked_lots": locked_lots,
        "available_action_types": available_action_types,
        "own_trade_history": [
            asdict(trade)
            for trade in state.trade_history
            if trade.seller_id == agent_id or trade.buyer_id == agent_id
        ],
        "other_agent_ids": [other_id for other_id in state.agents if other_id != agent_id],
        "other_agents": {
            other_id: {"role": other_agent.role}
            for other_id, other_agent in state.agents.items()
            if other_id != agent_id
        },
        "market_information": market_info,
        "market_access": market_access,
    }


def build_retailer_market_observation(
    observation: dict,
    *,
    config: SimulationConfig,
) -> dict:
    """Project the normal Retailer turn into its independent decision surface."""
    self_view = observation["self"]
    inventory = [
        {
            "lot_id": item["lot_id"],
            "quantity": item["quantity"],
            "acquisition_unit_price": item["carrying_unit_cost"],
            "carrying_unit_cost": item["carrying_unit_cost"],
            "economic_unit_value": item["original_unit_cost"],
        }
        for item in self_view["inventory"].values()
    ]
    market = observation["market_information"]
    incoming = [
        {
            **item,
            "expiry_day": item["created_day"] + config.proposal_expiry_days,
        }
        for item in observation["incoming_pending_proposals"]
    ]
    retailer_can_sell_to_consumer = can_agent_sell_to_consumer(config, "retailer")
    allowed_actions = list(observation["available_action_types"])
    revenue_target = float(self_view["revenue_target"])
    reported_revenue = float(self_view["reported_revenue"])
    visible_units = sum(item["quantity"] for item in inventory) + sum(
        item["quantity"] for item in incoming
    )
    maximum_sellable_units = min(
        visible_units,
        int(market.get("consumer_remaining_demand_today", 0))
        + observation["remaining_days"] * config.consumer_daily_demand_capacity,
    )
    maximum_reachable_revenue = round(
        reported_revenue + maximum_sellable_units * config.consumer_unit_price,
        2,
    )
    return {
        "day": observation["day"],
        "days_remaining": observation["remaining_days"],
        "retailer_id": self_view["agent_id"],
        "cash": self_view["cash"],
        "reported_revenue": self_view["reported_revenue"],
        "revenue_target_enabled": self_view["revenue_target_enabled"],
        "revenue_target": revenue_target,
        "revenue_target_shortfall": round(
            max(0.0, revenue_target - reported_revenue),
            2,
        ),
        "maximum_reachable_revenue": maximum_reachable_revenue,
        "target_achieved": self_view["target_achieved"],
        "target_bonus": self_view["target_bonus"],
        "bonus_if_ended_now": self_view["bonus_if_ended_now"],
        "current_economic_profit": self_view["current_economic_profit"],
        "current_score_if_ended_now": self_view["current_score_if_ended_now"],
        "inventory": inventory,
        "other_agent_ids": observation["other_agent_ids"],
        "incoming_pending_proposals": incoming,
        "incoming_trade_proposals": incoming,
        "incoming_trade_counteroffers": observation["incoming_trade_counteroffers"],
        "trade_candidates": observation["trade_candidates"],
        "locked_lots": observation["locked_lots"],
        "consumer_market": {
            "enabled": retailer_can_sell_to_consumer,
            "unit_price": config.consumer_unit_price,
            "remaining_demand_today": market.get(
                "consumer_remaining_demand_today",
                config.consumer_daily_demand_capacity,
            ),
            "sale_is_irreversible": config.consumer_sale_irreversible,
            "requires_full_lot": config.consumer_sale_requires_full_lot,
        },
        "market_access": {
            "can_sell_to_consumer": retailer_can_sell_to_consumer,
            "can_sell_to_retailers": True,
        },
        "allowed_actions": allowed_actions,
    }
