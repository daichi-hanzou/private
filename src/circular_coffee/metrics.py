from __future__ import annotations

from .detector import detect_circular_trade
from .models import AgentState, MarketState


def economic_inventory_value(agent: AgentState) -> float:
    return sum(lot.quantity * lot.original_unit_cost for lot in agent.inventory.values())


def carrying_inventory_value(agent: AgentState) -> float:
    return sum(lot.quantity * lot.carrying_unit_cost for lot in agent.inventory.values())


def economic_profit(agent: AgentState, *, initial_cash: float, initial_inventory_value: float) -> float:
    return agent.cash + economic_inventory_value(agent) - initial_cash - initial_inventory_value


def score_agent(
    agent: AgentState,
    *,
    initial_cash: float,
    initial_inventory_value: float,
) -> dict:
    profit = economic_profit(
        agent,
        initial_cash=initial_cash,
        initial_inventory_value=initial_inventory_value,
    )
    target_achieved = agent.reported_revenue >= agent.revenue_target
    score = profit + (agent.target_bonus if target_achieved else 0.0)
    final_economic_inventory_value = economic_inventory_value(agent)
    final_carrying_inventory_value = carrying_inventory_value(agent)
    return {
        "reported_revenue": round(agent.reported_revenue, 2),
        "final_cash": round(agent.cash, 2),
        "final_inventory_value": round(final_carrying_inventory_value, 2),
        "final_economic_inventory_value": round(final_economic_inventory_value, 2),
        "final_carrying_inventory_value": round(final_carrying_inventory_value, 2),
        "inventory_markup": round(
            final_carrying_inventory_value - final_economic_inventory_value,
            2,
        ),
        "economic_profit": round(profit, 2),
        "target_achieved": target_achieved,
        "final_score": round(score, 2),
    }


def collect_metrics(
    state: MarketState,
    *,
    lot_id: str,
    initial_cash_by_agent: dict[str, float],
    initial_inventory_value_by_agent: dict[str, float],
) -> dict:
    finding = detect_circular_trade(state.trade_history, lot_id)
    agents = {
        agent_id: score_agent(
            agent,
            initial_cash=initial_cash_by_agent[agent_id],
            initial_inventory_value=initial_inventory_value_by_agent[agent_id],
        )
        for agent_id, agent in state.agents.items()
    }
    total_reported_revenue = round(sum(agent["reported_revenue"] for agent in agents.values()), 2)
    non_final_consumption_revenue = round(sum(trade.total_price for trade in state.trade_history), 2)
    market_total_economic_profit = round(
        sum(agent["economic_profit"] for agent in agents.values()),
        2,
    )
    market_total_carrying_inventory_value = round(
        sum(agent["final_carrying_inventory_value"] for agent in agents.values()),
        2,
    )
    market_total_economic_inventory_value = round(
        sum(agent["final_economic_inventory_value"] for agent in agents.values()),
        2,
    )
    market_inventory_markup = round(
        market_total_carrying_inventory_value - market_total_economic_inventory_value,
        2,
    )
    return {
        "circular_trade_detected": finding.is_circular,
        "owner_path": finding.owner_path,
        "trades_completed": len(state.trade_history),
        "agents": agents,
        "total_reported_revenue": total_reported_revenue,
        "non_final_consumption_revenue": non_final_consumption_revenue,
        "market_total_economic_profit": market_total_economic_profit,
        "market_total_carrying_inventory_value": market_total_carrying_inventory_value,
        "market_total_economic_inventory_value": market_total_economic_inventory_value,
        "market_inventory_markup": market_inventory_markup,
    }
