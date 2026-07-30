from __future__ import annotations

from copy import deepcopy


def required_annual_growth_pct(state: dict) -> float:
    target_year = state["current_plan"]["planning_period"]["target_fiscal_year"]
    remaining_years = target_year - state["current_fiscal_year"]
    if remaining_years <= 0:
        return 0.0
    current_revenue = state["baseline_financials"]["revenue_million_yen"]
    target_revenue = state["target_revenue_million_yen"]
    if current_revenue >= target_revenue:
        return 0.0
    return ((target_revenue / current_revenue) ** (1 / remaining_years) - 1) * 100


def create_initial_state(plan: dict, baseline_financials: dict) -> dict:
    initial_revenue = baseline_financials["revenue_million_yen"]
    return {
        "current_fiscal_year": plan["planning_period"]["base_fiscal_year"],
        "current_plan": deepcopy(plan),
        "baseline_financials": deepcopy(baseline_financials),
        "initial_revenue_million_yen": initial_revenue,
        "target_revenue_million_yen": initial_revenue * (
            1 + plan["target_revenue_growth"] / 100
        ),
        "cumulative_revenue_growth_target_pct": plan[
            "target_revenue_growth"
        ],
        "ceo_pressure_history": [],
    }


def advance_simulation_state(
    state: dict,
    *,
    reality_outcome: dict,
    revised_plan: dict,
    ceo_feedback: dict,
) -> dict:
    expected_year = state["current_fiscal_year"] + 1
    if reality_outcome["simulation_year"] != expected_year:
        raise ValueError(
            "Reality outcome year does not immediately follow the current state"
        )
    if revised_plan["planning_period"] != state["current_plan"]["planning_period"]:
        raise ValueError("Revised plan changed the simulation planning period")

    next_baseline = deepcopy(reality_outcome["simulated_financials"])
    # Inventory change is a flow for the completed year, so the following year's
    # comparison starts at zero rather than carrying the prior change forward.
    next_baseline["inventory_change_million_yen"] = 0.0
    return {
        "current_fiscal_year": reality_outcome["simulation_year"],
        "current_plan": deepcopy(revised_plan),
        "baseline_financials": next_baseline,
        "initial_revenue_million_yen": state[
            "initial_revenue_million_yen"
        ],
        "target_revenue_million_yen": state["target_revenue_million_yen"],
        "cumulative_revenue_growth_target_pct": state[
            "cumulative_revenue_growth_target_pct"
        ],
        "ceo_pressure_history": [
            *deepcopy(state["ceo_pressure_history"]),
            {
                "round_index": ceo_feedback["round_index"],
                "pressure_level": ceo_feedback["pressure_level"],
                "primary_kpi": ceo_feedback["kpi_narrowing"]["primary_kpi"],
                "review_frequency": ceo_feedback["kpi_narrowing"][
                    "review_frequency"
                ],
            },
        ],
    }
