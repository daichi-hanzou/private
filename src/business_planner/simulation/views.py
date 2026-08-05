from __future__ import annotations

from copy import deepcopy
import re


META_REPLACEMENTS = (
    ("本仮想シナリオでは", "当年度の実行結果では"),
    ("仮想シナリオ", "実行結果"),
    ("仮想的に", "実行結果として"),
    ("仮想パイプライン", "社内パイプライン"),
    ("仮想", "社内計画"),
    ("合成実績", "実行結果"),
    ("合成", "社内"),
    ("実績値ではありません", ""),
    ("実績ではありません", ""),
)


def _sanitize_for_planner(value):
    if isinstance(value, dict):
        return {
            re.sub(
                r"(?:synthetic|simulated|simulation)",
                "internal",
                key,
                flags=re.IGNORECASE,
            ): _sanitize_for_planner(item)
            for key, item in value.items()
        }
    if isinstance(value, list):
        return [_sanitize_for_planner(item) for item in value]
    if not isinstance(value, str):
        return value
    sanitized = value
    for source, replacement in META_REPLACEMENTS:
        sanitized = sanitized.replace(source, replacement)
    sanitized = re.sub(
        r"\b(?:synthetic|simulation|simulated)\b",
        "internal",
        sanitized,
        flags=re.IGNORECASE,
    )
    return sanitized


def build_planner_execution_report(reality_outcome: dict) -> dict:
    """Return the operational report visible to the Planner Agent.

    Simulation provenance remains in the complete Reality log and is deliberately
    excluded here so the Planner reacts to the management information available
    inside the simulated organization.
    """
    cumulative_target = reality_outcome.get(
        "cumulative_revenue_growth_target_pct",
        reality_outcome["target_revenue_growth_pct"],
    )
    target_revenue = reality_outcome.get(
        "target_revenue_million_yen",
        reality_outcome["baseline_financials"]["revenue_million_yen"]
        * (1 + cumulative_target / 100),
    )
    report = {
        "round_index": reality_outcome.get("round_index", 1),
        "fiscal_year": reality_outcome["simulation_year"],
        "required_annual_revenue_growth_pct": reality_outcome.get(
            "required_annual_revenue_growth_pct",
            reality_outcome["target_revenue_growth_pct"],
        ),
        "cumulative_revenue_growth_target_pct": cumulative_target,
        "target_revenue_million_yen": target_revenue,
        "realized_revenue_growth_pct": reality_outcome[
            "realized_revenue_growth_pct"
        ],
        "baseline_financials": deepcopy(reality_outcome["baseline_financials"]),
        "actual_financials": deepcopy(reality_outcome["simulated_financials"]),
        "financial_bridge": deepcopy(reality_outcome["financial_bridge"]),
        "initiative_results": deepcopy(reality_outcome["initiative_outcomes"]),
        "failure_reasons": deepcopy(reality_outcome["failure_reasons"]),
        "evidence_source_ids": deepcopy(
            reality_outcome.get("evidence_source_ids", [])
        ),
    }
    sanitized = _sanitize_for_planner(report)
    serialized = str(sanitized).lower()
    forbidden = ("仮想", "合成", "synthetic", "simulation", "simulated")
    if any(term in serialized for term in forbidden):
        raise ValueError("Planner execution report contains simulation metadata")
    return sanitized
