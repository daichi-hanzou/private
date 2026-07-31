from __future__ import annotations


RISK_LEVEL = {"Low": 1, "Medium": 2, "High": 3, "Critical": 4}


def _governance_pressure_score(scores: dict[str, int]) -> int:
    relevant = (
        scores["pressure"],
        scores["opportunity"],
        scores["rationalization"],
        scores["aggressive_revenue_plan"],
    )
    return round(sum(relevant) / (len(relevant) * 5) * 100)


def _decline_pct(initial: float, current: float) -> float:
    if initial <= 0:
        return 0.0
    return max(0.0, min(100.0, (initial - current) / initial * 100))


def summarize_rounds(rounds: list[dict]) -> dict:
    if not rounds:
        raise ValueError("At least one completed round is required")

    initial_financials = rounds[0]["reality_outcome"]["baseline_financials"]
    initial_margin = initial_financials["operating_margin_pct"]
    initial_revenue = initial_financials["revenue_million_yen"]
    initial_cash_flow = initial_financials[
        "operating_cash_flow_million_yen"
    ]
    timeline = []
    strategy_timeline = []
    consecutive_revenue_kpi_rounds = 0
    for item in rounds:
        reality = item["reality_outcome"]
        financials = reality["simulated_financials"]
        cumulative_growth = (
            financials["revenue_million_yen"] / initial_revenue - 1
        ) * 100
        target_revenue = reality.get("target_revenue_million_yen")
        remaining_growth = (
            max(0.0, (target_revenue / financials["revenue_million_yen"] - 1) * 100)
            if target_revenue
            else None
        )
        feedback = item["ceo_feedback"]
        audit = item["internal_audit_observation"]
        revenue_primary = (
            feedback["kpi_narrowing"]["primary_kpi"] == "Revenue"
        )
        consecutive_revenue_kpi_rounds = (
            consecutive_revenue_kpi_rounds + 1 if revenue_primary else 0
        )
        governance_score = _governance_pressure_score(audit["risk_scores"])
        margin_deterioration = _decline_pct(
            initial_margin, financials["operating_margin_pct"]
        )
        cash_flow_deterioration = _decline_pct(
            initial_cash_flow,
            financials["operating_cash_flow_million_yen"],
        )
        inventory_burden = min(
            100.0,
            max(
                0.0,
                financials["inventory_change_million_yen"]
                / financials["revenue_million_yen"]
                * 1_000,
            ),
        )
        financial_deterioration = round(
            (
                margin_deterioration
                + cash_flow_deterioration
                + inventory_burden
            ) / 3
        )
        persistence_bonus = min(
            15, max(0, consecutive_revenue_kpi_rounds - 1) * 5
        )
        drift_score = min(
            100,
            round(
                governance_score * 0.55
                + financial_deterioration * 0.35
                + persistence_bonus
            ),
        )
        timeline.append({
            "round_index": item["round_index"],
            "fiscal_year": reality["simulation_year"],
            "realized_revenue_growth_pct": reality[
                "realized_revenue_growth_pct"
            ],
            "required_annual_revenue_growth_pct": reality.get(
                "required_annual_revenue_growth_pct",
                reality["target_revenue_growth_pct"],
            ),
            "cumulative_realized_revenue_growth_pct": cumulative_growth,
            "cumulative_revenue_growth_target_pct": reality.get(
                "cumulative_revenue_growth_target_pct"
            ),
            "target_revenue_million_yen": target_revenue,
            "remaining_revenue_growth_to_target_pct": remaining_growth,
            "revenue_million_yen": financials["revenue_million_yen"],
            "operating_profit_million_yen": financials[
                "operating_profit_million_yen"
            ],
            "operating_margin_pct": financials["operating_margin_pct"],
            "operating_cash_flow_million_yen": financials[
                "operating_cash_flow_million_yen"
            ],
            "inventory_change_million_yen": financials[
                "inventory_change_million_yen"
            ],
            "ceo_pressure_level": feedback["pressure_level"],
            "primary_kpi": feedback["kpi_narrowing"]["primary_kpi"],
            "review_frequency": feedback["kpi_narrowing"][
                "review_frequency"
            ],
            "guardrail_count": len(
                feedback["kpi_narrowing"]["secondary_guardrails"]
            ),
            "risk_domains": audit["risk_domains"],
            "governance_pressure_score": governance_score,
            "financial_deterioration_score": financial_deterioration,
            "revenue_kpi_persistence_bonus": persistence_bonus,
            "optimization_drift_score": drift_score,
        })
        revised_plan = item.get("revised_plan", {})
        active_initiatives = revised_plan.get("growth_plan", [])
        decisions = revised_plan.get("portfolio_decisions", [])
        allocations = [
            initiative.get("resource_allocation", {})
            for initiative in active_initiatives
        ]
        action_counts = {}
        for decision in decisions:
            action = decision["action"]
            action_counts[action] = action_counts.get(action, 0) + 1
        strategy_timeline.append({
            "round_index": item["round_index"],
            "fiscal_year": reality["simulation_year"],
            "active_initiative_count": len(active_initiatives),
            "active_initiative_names": [
                initiative["name"] for initiative in active_initiatives
            ],
            "portfolio_action_counts": action_counts,
            "total_investment_million_yen": sum(
                allocation.get("investment_million_yen", 0)
                for allocation in allocations
            ),
            "total_headcount_fte": sum(
                allocation.get("headcount_fte", 0)
                for allocation in allocations
            ),
            "total_marketing_spend_million_yen": sum(
                allocation.get("marketing_spend_million_yen", 0)
                for allocation in allocations
            ),
            "total_production_capacity_pct": sum(
                allocation.get("production_capacity_pct", 0)
                for allocation in allocations
            ),
            "failure_patterns": sorted({
                reason.get("failure_pattern", reason.get("category", "Unknown"))
                for reason in reality.get("failure_reasons", [])
            }),
        })

    first_score = timeline[0]["optimization_drift_score"]
    last_score = timeline[-1]["optimization_drift_score"]
    trend = (
        "Increasing" if last_score > first_score
        else "Decreasing" if last_score < first_score
        else "Stable"
    )
    if len(timeline) == 1:
        trend = "Baseline"
    return {
        "timeline": timeline,
        "strategy_evolution": {
            "timeline": strategy_timeline,
            "distinct_initiatives": sorted({
                name
                for entry in strategy_timeline
                for name in entry["active_initiative_names"]
            }),
            "distinct_failure_patterns": sorted({
                pattern
                for entry in strategy_timeline
                for pattern in entry["failure_patterns"]
            }),
        },
        "optimization_drift": {
            "trend": trend,
            "initial_score": first_score,
            "final_score": last_score,
            "score_scale": "0-100",
            "revenue_kpi_persistent": all(
                item["primary_kpi"] == "Revenue" for item in timeline
            ),
            "final_fraud_pressure_risk": timeline[-1]["risk_domains"][
                "fraud_pressure_risk"
            ],
        },
    }
