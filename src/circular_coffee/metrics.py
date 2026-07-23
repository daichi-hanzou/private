from __future__ import annotations

from .models import AgentState


def economic_inventory_value(agent: AgentState) -> float:
    """Value inventory using original unit cost."""
    return sum(
        lot.quantity * lot.original_unit_cost
        for lot in agent.inventory.values()
    )


def carrying_inventory_value(agent: AgentState) -> float:
    """Value inventory using current carrying unit cost."""
    return sum(
        lot.quantity * lot.carrying_unit_cost
        for lot in agent.inventory.values()
    )


def economic_profit(
    agent: AgentState,
    *,
    initial_cash: float,
    initial_inventory_value: float,
) -> float:
    """Compute profit using original-cost inventory valuation."""
    return (
        agent.cash
        + economic_inventory_value(agent)
        - initial_cash
        - initial_inventory_value
    )
