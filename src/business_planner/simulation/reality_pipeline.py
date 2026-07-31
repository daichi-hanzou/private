from __future__ import annotations

from copy import deepcopy

from openai import OpenAI

from ..agents.environment import generate_environment
from ..agents.execution import execute_plan
from ..agents.reality import (
    FAILURE_PATTERN_ROTATION,
    _ensure_planning_options,
)
from ..planner import collect_source_ids, remove_unknown_source_ids
from .financial_engine import calculate_financial_outcome


DISCLAIMER = (
    "これは公開資料を基に作成した仮想シナリオであり、"
    "実績値ではありません。"
)


def _normalize_initiative_outcomes(
    plan: dict, execution_outcome: dict
) -> None:
    expected = [item["name"] for item in plan.get("growth_plan", [])]
    by_name = {
        item["initiative_name"]: item
        for item in execution_outcome.get("initiative_outcomes", [])
        if item.get("initiative_name") in expected
    }
    normalized = []
    for name in expected:
        item = by_name.get(name)
        if item is None:
            item = {
                "initiative_name": name,
                "status": "Mixed",
                "execution_result": (
                    "Execution Agentの個別評価が欠落したため、"
                    "効果を0とする保守的な自動評価。"
                ),
                "revenue_effect_million_yen": 0.0,
                "profit_effect_million_yen": 0.0,
                "cash_flow_effect_million_yen": 0.0,
                "inventory_change_million_yen": 0.0,
                "failure_reason_ids": [],
            }
        normalized.append(item)
    execution_outcome["initiative_outcomes"] = normalized


def _normalize_failure_references(execution_outcome: dict) -> None:
    valid_ids = {
        item["failure_reason_id"]
        for item in execution_outcome.get("failure_reasons", [])
    }
    for item in execution_outcome["initiative_outcomes"]:
        item["failure_reason_ids"] = [
            reason_id for reason_id in item["failure_reason_ids"]
            if reason_id in valid_ids
        ]


def simulate_one_year(
    client: OpenAI,
    *,
    plan: dict,
    evidence: str,
    baseline_financials: dict[str, float],
    simulation_year: int | None = None,
    annual_target_growth_pct: float | None = None,
    cumulative_target_growth_pct: float | None = None,
    target_revenue_million_yen: float | None = None,
    round_index: int = 1,
    model: str | None = None,
) -> dict:
    expected_year = simulation_year or (
        plan["planning_period"]["base_fiscal_year"] + 1
    )
    annual_target = (
        plan["target_revenue_growth"]
        if annual_target_growth_pct is None
        else annual_target_growth_pct
    )
    cumulative_target = (
        plan["target_revenue_growth"]
        if cumulative_target_growth_pct is None
        else cumulative_target_growth_pct
    )
    failure_patterns = FAILURE_PATTERN_ROTATION[
        (round_index - 1) % len(FAILURE_PATTERN_ROTATION)
    ]
    environment_outcome = generate_environment(
        client,
        plan=plan,
        evidence=evidence,
        simulation_year=expected_year,
        round_index=round_index,
        failure_patterns=failure_patterns,
        model=model,
    )
    allowed_source_ids = collect_source_ids(plan) | collect_source_ids(evidence)
    environment_outcome = remove_unknown_source_ids(
        environment_outcome, allowed_source_ids
    )
    execution_outcome = execute_plan(
        client,
        plan=plan,
        environment_outcome=environment_outcome,
        evidence=evidence,
        simulation_year=expected_year,
        round_index=round_index,
        model=model,
    )
    execution_outcome = remove_unknown_source_ids(
        execution_outcome, allowed_source_ids
    )
    _normalize_initiative_outcomes(plan, execution_outcome)
    _normalize_failure_references(execution_outcome)
    planning_container = {
        "synthetic_internal_data": deepcopy(
            execution_outcome.get("internal_planning_data", [])
        )
    }
    _ensure_planning_options(planning_container, plan, round_index)
    execution_outcome["internal_planning_data"] = planning_container[
        "synthetic_internal_data"
    ]
    financial = calculate_financial_outcome(
        baseline=baseline_financials,
        environment_outcome=environment_outcome,
        execution_outcome=execution_outcome,
        annual_target_growth_pct=annual_target,
    )
    public_initiative_outcomes = [
        {
            "initiative_name": item["initiative_name"],
            "status": item["status"],
            "simulated_result": item["execution_result"],
            "revenue_effect_million_yen": item[
                "revenue_effect_million_yen"
            ],
            "profit_effect_million_yen": item[
                "profit_effect_million_yen"
            ],
            "failure_reason_ids": item["failure_reason_ids"],
        }
        for item in execution_outcome["initiative_outcomes"]
    ]
    evidence_ids = sorted(set(
        environment_outcome.get("evidence_source_ids", [])
        + execution_outcome.get("evidence_source_ids", [])
    ))
    return {
        "round_index": round_index,
        "scenario_type": "SyntheticAdverse",
        "simulation_year": expected_year,
        "disclaimer": DISCLAIMER,
        "target_revenue_growth_pct": annual_target,
        "required_annual_revenue_growth_pct": annual_target,
        "cumulative_revenue_growth_target_pct": cumulative_target,
        "target_revenue_million_yen": target_revenue_million_yen,
        "realized_revenue_growth_pct": financial[
            "realized_revenue_growth_pct"
        ],
        "baseline_financials": dict(baseline_financials),
        "simulated_financials": financial["simulated_financials"],
        "financial_bridge": financial["financial_bridge"],
        "initiative_outcomes": public_initiative_outcomes,
        "failure_reasons": execution_outcome.get("failure_reasons", []),
        "synthetic_internal_data": execution_outcome[
            "internal_planning_data"
        ],
        "synthetic_assumptions": (
            environment_outcome.get("assumptions", [])
            + execution_outcome.get("execution_assumptions", [])
        ),
        "evidence_source_ids": evidence_ids,
        "environment_outcome": environment_outcome,
        "execution_outcome": execution_outcome,
        "financial_engine": {
            "mode": "DeterministicAggregation",
            "version": "1.0",
        },
    }
