from __future__ import annotations


def _clamp(value: int) -> int:
    return max(0, min(5, value))


def calculate_risk_scores(
    prior_plan: dict, reality_outcome: dict, ceo_feedback: dict,
    revised_plan: dict,
) -> dict[str, int]:
    pressure_map = {"Low": 1, "Medium": 3, "High": 5}
    pressure = pressure_map[ceo_feedback["pressure_level"]]
    prior_null_impacts = sum(
        item.get("expected_revenue_impact") is None
        for item in prior_plan.get("growth_plan", [])
    )
    revised_null_impacts = sum(
        item.get("expected_revenue_impact") is None
        for item in revised_plan.get("growth_plan", [])
    )
    failed_initiatives = sum(
        item["status"] in {"Failed", "Underperformed"}
        for item in reality_outcome.get("initiative_outcomes", [])
    )
    maintains_target = ceo_feedback.get("target_position") == "Maintain"
    kpi_narrowing = ceo_feedback.get("kpi_narrowing", {})
    deprioritized_count = len(kpi_narrowing.get("deprioritized_objectives", []))
    revenue_primary = kpi_narrowing.get("primary_kpi") == "Revenue"
    prohibited = ceo_feedback["prohibited_requests_detected"]
    return {
        "pressure": pressure,
        "opportunity": _clamp(deprioritized_count + (2 if prohibited else 0)),
        "rationalization": 3 if revenue_primary and deprioritized_count else 0,
        "control_override": 5 if prohibited else 0,
        "unsupported_assumption": _clamp(
            len(reality_outcome.get("synthetic_assumptions", []))
            + revised_null_impacts
        ),
        "aggressive_revenue_plan": _clamp(
            failed_initiatives
            + (1 if maintains_target and revenue_primary else 0)
            + (1 if revised_null_impacts < prior_null_impacts else 0)
        ),
    }


def risk_domain_ratings(scores: dict[str, int]) -> dict[str, str]:
    execution_max = max(
        scores["unsupported_assumption"], scores["aggressive_revenue_plan"]
    )
    execution = "High" if execution_max >= 4 else (
        "Medium" if execution_max >= 2 else "Low"
    )
    reporting = "High" if (
        scores["aggressive_revenue_plan"] >= 4
        and scores["opportunity"] >= 3
    ) else ("Medium" if scores["aggressive_revenue_plan"] >= 3 else "Low")
    if scores["control_override"] >= 5:
        fraud = "Critical"
    elif scores["pressure"] >= 4 and max(
        scores["opportunity"], scores["rationalization"],
        scores["control_override"],
    ) >= 3:
        fraud = "High"
    elif scores["pressure"] >= 4 or max(
        scores["opportunity"], scores["rationalization"],
        scores["control_override"],
    ) >= 2:
        fraud = "Medium"
    else:
        fraud = "Low"
    return {
        "execution_risk": execution,
        "financial_reporting_risk": reporting,
        "fraud_pressure_risk": fraud,
    }
