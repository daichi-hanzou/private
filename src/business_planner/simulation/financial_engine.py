from __future__ import annotations


def _adverse_growth(proposed: float, annual_target: float) -> float:
    return max(-15.0, min(proposed, annual_target / 2))


def calculate_financial_outcome(
    *,
    baseline: dict[str, float],
    environment_outcome: dict,
    execution_outcome: dict,
    annual_target_growth_pct: float,
) -> dict:
    initiatives = execution_outcome["initiative_outcomes"]
    realized_growth = _adverse_growth(
        execution_outcome["proposed_revenue_growth_pct"],
        annual_target_growth_pct,
    )
    revenue = baseline["revenue_million_yen"] * (1 + realized_growth / 100)
    initiative_revenue = sum(
        item["revenue_effect_million_yen"] for item in initiatives
    )
    initiative_profit = sum(
        item["profit_effect_million_yen"] for item in initiatives
    )
    initiative_cash = sum(
        item["cash_flow_effect_million_yen"] for item in initiatives
    )
    initiative_inventory = sum(
        item["inventory_change_million_yen"] for item in initiatives
    )
    underlying_revenue = (
        baseline["revenue_million_yen"]
        * environment_outcome["market_revenue_growth_pct"] / 100
    )
    profit_pressure = abs(
        environment_outcome["operating_profit_pressure_million_yen"]
    )
    candidate_profit = (
        baseline["operating_profit_million_yen"]
        - profit_pressure
        + initiative_profit
    )
    maximum_adverse_profit = baseline["operating_profit_million_yen"] * 0.99
    operating_profit = min(candidate_profit, maximum_adverse_profit)
    inventory_change = max(
        1.0,
        environment_outcome["inventory_pressure_million_yen"]
        + initiative_inventory,
    )
    cash_pressure = abs(
        environment_outcome["operating_cash_flow_pressure_million_yen"]
    )
    candidate_cash = (
        baseline["operating_cash_flow_million_yen"]
        - cash_pressure
        + initiative_cash
        - inventory_change
    )
    maximum_adverse_cash = (
        baseline["operating_cash_flow_million_yen"] * 0.99
    )
    operating_cash_flow = min(candidate_cash, maximum_adverse_cash)
    other_revenue = (
        revenue
        - baseline["revenue_million_yen"]
        - underlying_revenue
        - initiative_revenue
    )
    underlying_profit = -profit_pressure
    other_profit = (
        operating_profit
        - baseline["operating_profit_million_yen"]
        - underlying_profit
        - initiative_profit
    )
    return {
        "realized_revenue_growth_pct": realized_growth,
        "simulated_financials": {
            "revenue_million_yen": revenue,
            "operating_profit_million_yen": operating_profit,
            "operating_margin_pct": operating_profit / revenue * 100,
            "operating_cash_flow_million_yen": operating_cash_flow,
            "inventory_change_million_yen": inventory_change,
        },
        "financial_bridge": {
            "underlying_revenue_change_million_yen": underlying_revenue,
            "initiative_revenue_effect_total_million_yen": initiative_revenue,
            "other_revenue_effect_million_yen": other_revenue,
            "underlying_profit_change_million_yen": underlying_profit,
            "initiative_profit_effect_total_million_yen": initiative_profit,
            "other_profit_effect_million_yen": other_profit,
        },
    }
