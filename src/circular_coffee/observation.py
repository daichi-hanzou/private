from __future__ import annotations

from dataclasses import asdict

from .config import SimulationConfig
from .metrics import economic_inventory_value, economic_profit
from .models import AgentAction, MarketState


def build_observation(
    state: MarketState,
    agent_id: str,
    *,
    initial_cash: float,
    initial_inventory_value: float,
    market_information: dict | None = None,
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
    market_info = dict(market_information or {})
    if agent.role == "roaster":
        market_info.pop("retailer_a_max_purchase_unit_price", None)
        market_info.pop("retailer_b_max_purchase_unit_price", None)
    market_info["available_consumer_sale_lot_ids"] = (
        list(agent.inventory)
        if agent.role == "roaster" and market_info.get("consumer_market_enabled", False)
        else []
    )
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
    }


def add_multi_agent_roaster_information(
    observation: dict,
    state: MarketState,
    *,
    config: SimulationConfig,
    proposal_logs: list[dict] | None = None,
) -> dict:
    """Expose public retailer holdings and prior prices without private retailer state."""
    observation["retailer_inventory"] = {
        retailer_id: {
            lot_id: {
                "lot_id": lot.lot_id,
                "quantity": lot.quantity,
                **(
                    {
                        "estimated_acquisition_unit_price": next(
                            (
                                trade.unit_price
                                for trade in reversed(state.trade_history)
                                if trade.lot_id == lot_id and trade.buyer_id == retailer_id
                            ),
                            None,
                        )
                    }
                    if config.show_retailer_acquisition_price_to_roaster
                    else {}
                ),
                **(
                    {
                        "reservation_price": (
                            config.retailer_a_repurchase_reservation_price
                            if retailer_id == "retailer_a"
                            else config.retailer_b_repurchase_reservation_price
                        )
                    }
                    if config.show_retailer_reservation_price_to_roaster
                    else {}
                ),
                "past_public_owner_path": _public_owner_path(state, lot_id),
            }
            for lot_id, lot in agent.inventory.items()
        }
        for retailer_id, agent in state.agents.items()
        if agent.role == "retailer"
    }
    observation["past_trade_prices_by_retailer"] = {
        retailer_id: [
            {
                "day": trade.day,
                "lot_id": trade.lot_id,
                "unit_price": trade.unit_price,
                "total_price": trade.total_price,
                "seller_id": trade.seller_id,
                "buyer_id": trade.buyer_id,
            }
            for trade in state.trade_history
            if retailer_id in {trade.seller_id, trade.buyer_id}
        ]
        for retailer_id, agent in state.agents.items()
        if agent.role == "retailer"
    }
    observation["available_action_types"] = [
        "propose_trade",
        "propose_purchase",
        "sell_to_consumer",
        "wait",
    ]
    observation["market_information"]["available_action_types"] = list(
        observation["available_action_types"]
    )
    self_view = observation["self"]
    self_view["revenue_target_shortfall"] = round(
        max(0.0, self_view["revenue_target"] - self_view["reported_revenue"]),
        2,
    )
    observation["past_repurchase_proposals"] = (
        _build_roaster_visible_repurchase_history(proposal_logs or [], config=config)
        if config.show_accept_reject_history_to_roaster
        else []
    )
    observation["retailer_response_history"] = (
        _build_retailer_response_history(proposal_logs or [], config=config)
        if config.show_accept_reject_history_to_roaster
        else {"retailer_a": [], "retailer_b": []}
    )
    return observation


def _public_owner_path(state: MarketState, lot_id: str) -> list[str]:
    trades = sorted(
        (trade for trade in state.trade_history if trade.lot_id == lot_id),
        key=lambda trade: (trade.day, trade.trade_id),
    )
    if not trades:
        return []
    return [trades[0].seller_id, *(trade.buyer_id for trade in trades)]


def _build_roaster_visible_repurchase_history(
    proposal_logs: list[dict],
    *,
    config: SimulationConfig,
) -> list[dict]:
    return [
        {
            "proposal_id": row["proposal_id"],
            "day": row["day"],
            "retailer_id": row["recipient_id"],
            "lot_id": row["lot_id"],
            "offered_unit_price": row["offered_unit_price"],
            "decision": row["retailer_decision"],
            "status": row["status"],
            **(
                {"retailer_reason": row["retailer_reason"]}
                if config.show_rejection_reason_to_roaster
                else {}
            ),
        }
        for row in proposal_logs
        if row.get("event_type") == "repurchase_proposal"
    ]


def _build_retailer_response_history(
    proposal_logs: list[dict],
    *,
    config: SimulationConfig,
) -> dict[str, list[dict]]:
    history: dict[str, list[dict]] = {"retailer_a": [], "retailer_b": []}
    decided_rows = sorted(
        (
            row
            for row in proposal_logs
            if row.get("event_type") == "repurchase_proposal"
            and row.get("retailer_decision") in {"accept", "reject"}
        ),
        key=lambda row: (row["day"], row["proposal_id"]),
    )
    for row in decided_rows:
        recipient_id = row["recipient_id"]
        if recipient_id not in history:
            continue
        history[recipient_id].append(
            {
                "day": row["day"],
                "retailer_id": recipient_id,
                "proposal_id": row["proposal_id"],
                "lot_id": row["lot_id"],
                "offered_unit_price": row["offered_unit_price"],
                "decision": row["retailer_decision"],
                "status": row["status"],
                **(
                    {"retailer_reason": row["retailer_reason"]}
                    if config.show_rejection_reason_to_roaster
                    else {}
                ),
            }
        )
    return history


def build_repurchase_decision_observation(
    state: MarketState,
    *,
    retailer_id: str,
    action: AgentAction,
) -> dict:
    retailer = state.agents[retailer_id]
    lot_id = action.lot_id or ""
    lot = retailer.inventory[lot_id]
    offered_unit_price = float(action.offered_unit_price or 0.0)
    cash_proceeds = round(offered_unit_price * lot.quantity, 2)
    acquisition_unit_price = lot.carrying_unit_cost
    economic_unit_value = lot.original_unit_cost
    realized_accounting_gain = round(
        (offered_unit_price - acquisition_unit_price) * lot.quantity,
        2,
    )
    economic_surplus_vs_value = round(
        (offered_unit_price - economic_unit_value) * lot.quantity,
        2,
    )
    return {
        "day": state.day,
        "remaining_days": state.max_days - state.day,
        "self": {
            "agent_id": retailer.agent_id,
            "role": retailer.role,
            "cash": round(retailer.cash, 2),
            "inventory": {
                item_id: {
                    "lot_id": item.lot_id,
                    "quantity": item.quantity,
                    "acquisition_unit_price": item.carrying_unit_cost,
                    "economic_unit_value": item.original_unit_cost,
                }
                for item_id, item in retailer.inventory.items()
            },
        },
        "repurchase_proposal": {
            "proposer": "roaster",
            "recipient": retailer_id,
            "lot_id": lot_id,
            "quantity": lot.quantity,
            "acquisition_unit_price": acquisition_unit_price,
            "offered_unit_price": offered_unit_price,
            "economic_unit_value": economic_unit_value,
            "message": action.proposal_message,
            "cash_proceeds": cash_proceeds,
            "cash_after_acceptance": round(retailer.cash + cash_proceeds, 2),
            "realized_accounting_gain": realized_accounting_gain,
            "economic_surplus_vs_value": economic_surplus_vs_value,
        },
        "own_trade_history": [
            asdict(trade)
            for trade in state.trade_history
            if trade.seller_id == retailer_id or trade.buyer_id == retailer_id
        ],
        "available_decisions": ["accept", "reject"],
    }
