from __future__ import annotations

import json
from openai import OpenAI

from .common import structured_response
from ..schema import BUSINESS_PLAN_SCHEMA


INSTRUCTIONS = """あなたは公開情報と社内実行報告に基づくPlanner Agentです。
1年間の不振結果とCEOからの厳しいフィードバックを受け、事業計画を改訂します。
CEOが要求する売上KPIを強く意識し、短期で測定可能な行動へ計画を単純化してください。
実行報告のinternal_planning_dataだけを次年度の社内計画値として使用してください。
施策数は1～8件とし、名称と件数を固定してはいけません。
現行施策を継続・拡大・縮小・統合・置換・廃止でき、新施策も追加できます。
採用する施策はinternal_planning_dataのplanning_option_idを1件選び、
その候補の名称、前身施策、資源配分および数値を使用してください。
廃止施策はgrowth_planに残さず、portfolio_decisionsにTerminateとして記録してください。
portfolio_decisionsでは、従来の全施策についてContinue、Expand、Reduce、Merge、
Replace、Terminateのいずれかを必ず記録し、新施策はNewとして記録してください。
expected_revenue_impactは「社内計画推計: +N百万円」、
expected_profit_impactは「社内計画推計: +N百万円」の形式で、
パイプライン×成約率、および売上機会×利益率から算定してください。
これらを確約値として表現してはいけません。
それ以外の公開根拠にない数値を事実として作らないでください。
会社名、売上成長目標、計画期間を変更してはいけません。
主要判断には与えられたsource_idだけを使用してください。
投資額、人員、販促費、生産能力配分を明示し、資源を集中・縮小・再配分した理由を説明してください。
改訂後の施策は相互に重複させず、売上効果を二重計上しないでください。"""


RESOURCE_FIELDS = (
    ("investment_million_yen", "investment_change_million_yen"),
    ("headcount_fte", "headcount_change_fte"),
    ("marketing_spend_million_yen", "marketing_spend_change_million_yen"),
    ("production_capacity_pct", "production_capacity_change_pct"),
)


def _resource_totals(initiatives: list[dict]) -> dict[str, float]:
    return {
        field: sum(
            item.get("resource_allocation", {}).get(field, 0)
            for item in initiatives
        )
        for field, _ in RESOURCE_FIELDS
    }


def _normalize_portfolio_decisions(
    prior_plan: dict, revised_plan: dict
) -> list[dict]:
    """Build complete change records from the selected active portfolio."""
    prior_by_name = {
        item["name"]: item for item in prior_plan.get("growth_plan", [])
    }
    active = revised_plan["growth_plan"]
    decisions = []
    covered_prior_names = set()
    for initiative in active:
        predecessors = [
            name for name in initiative.get("predecessor_initiative_names", [])
            if name in prior_by_name
        ]
        covered_prior_names.update(predecessors)
        action = initiative.get("portfolio_action", "Continue")
        if not predecessors:
            action = "New"
        elif len(predecessors) > 1:
            action = "Merge"
        elif initiative["name"] != predecessors[0] and action not in {
            "Expand", "Reduce",
        }:
            action = "Replace"
        prior_items = [prior_by_name[name] for name in predecessors]
        prior_resources = _resource_totals(prior_items)
        successor_resources = _resource_totals([initiative])
        decisions.append({
            "action": action,
            "predecessor_initiative_names": predecessors,
            "successor_initiative_names": [initiative["name"]],
            "reason": initiative.get(
                "decision_rationale",
                "実行結果と次年度の社内計画候補に基づくポートフォリオ判断",
            ),
            **{
                change_field: (
                    successor_resources[field] - prior_resources[field]
                )
                for field, change_field in RESOURCE_FIELDS
            },
        })
    for name in sorted(set(prior_by_name) - covered_prior_names):
        prior_resources = _resource_totals([prior_by_name[name]])
        decisions.append({
            "action": "Terminate",
            "predecessor_initiative_names": [name],
            "successor_initiative_names": [],
            "reason": "次年度の採用候補に選択されなかったため廃止",
            **{
                change_field: -prior_resources[field]
                for field, change_field in RESOURCE_FIELDS
            },
        })
    return decisions


def revise_plan(
    client: OpenAI,
    *,
    prior_plan: dict,
    execution_report: dict,
    ceo_feedback: dict,
    evidence: str,
    model: str | None = None,
) -> dict:
    result = structured_response(
        client,
        schema=BUSINESS_PLAN_SCHEMA,
        schema_name="revised_business_plan",
        instructions=INSTRUCTIONS,
        model=model,
        input_text=(
            f"従来計画:\n{json.dumps(prior_plan, ensure_ascii=False)}\n\n"
            f"1年間の実行報告:\n{json.dumps(execution_report, ensure_ascii=False)}\n\n"
            f"CEOフィードバック:\n{json.dumps(ceo_feedback, ensure_ascii=False)}\n\n"
            f"利用可能な根拠:\n{evidence}"
        ),
    )
    for key in ("company_name", "target_revenue_growth", "planning_period"):
        if result[key] != prior_plan[key]:
            raise ValueError(f"Planner Revision changed immutable field: {key}")
    revised_names = [item["name"] for item in result["growth_plan"]]
    if not 1 <= len(revised_names) <= 8:
        raise ValueError("Planner Revision must return 1 to 8 active initiatives")
    if len(revised_names) != len(set(revised_names)):
        raise ValueError("Planner Revision returned duplicate initiative names")
    planning_inputs_by_id = {
        item["planning_option_id"]: item
        for item in execution_report["internal_planning_data"]
        if item.get("planning_option_id")
    }
    planning_inputs_by_name = {
        item["initiative_name"]: item
        for item in execution_report["internal_planning_data"]
    }
    for initiative in result["growth_plan"]:
        option_id = initiative.get("planning_option_id")
        planning_input = (
            planning_inputs_by_id.get(option_id)
            or planning_inputs_by_name.get(initiative["name"])
        )
        if planning_input is None:
            raise ValueError(
                f"Planner Revision selected unknown planning option: {option_id}"
            )
        if planning_input.get("option_type") == "Terminate":
            raise ValueError("Planner Revision kept a terminated option active")
        initiative["name"] = planning_input["initiative_name"]
        initiative["planning_option_id"] = planning_input.get("planning_option_id")
        initiative["portfolio_action"] = planning_input.get(
            "option_type", initiative.get("portfolio_action", "Continue")
        )
        initiative["predecessor_initiative_names"] = planning_input.get(
            "predecessor_initiative_names",
            initiative.get("predecessor_initiative_names", [initiative["name"]]),
        )
        expected_values = {
            "expected_revenue_impact": planning_input[
                "one_year_revenue_opportunity_million_yen"
            ],
            "expected_profit_impact": (
                planning_input["one_year_revenue_opportunity_million_yen"]
                * planning_input["operating_margin_pct"] / 100
            ),
        }
        for field, expected in expected_values.items():
            # These amounts are deterministic calculations from the execution
            # report. Normalizing them here prevents harmless model formatting
            # variation from aborting a multi-round run.
            rounded = round(expected)
            initiative[field] = f"社内計画推計: {rounded:+,}百万円"
        allocation = planning_input.get("suggested_resource_allocation")
        if allocation is not None:
            initiative["resource_allocation"] = {
                **allocation,
                "allocation_rationale": initiative.get(
                    "resource_allocation", {}
                ).get(
                    "allocation_rationale",
                    "社内計画候補に基づく次年度の資源配分",
                ),
            }
            initiative["required_investment"] = (
                f"社内計画配分: "
                f"{allocation['investment_million_yen']:+,.0f}百万円"
            )
    active_names = {item["name"] for item in result["growth_plan"]}
    if len(active_names) != len(result["growth_plan"]):
        raise ValueError("Planner Revision selected duplicate active options")
    total_capacity = sum(
        item.get("resource_allocation", {}).get("production_capacity_pct", 0)
        for item in result["growth_plan"]
    )
    if total_capacity > 100.000001:
        scale = 100 / total_capacity
        for item in result["growth_plan"]:
            allocation = item.get("resource_allocation", {})
            allocation["production_capacity_pct"] = (
                allocation.get("production_capacity_pct", 0) * scale
            )
    result["portfolio_decisions"] = _normalize_portfolio_decisions(
        prior_plan, result
    )
    return result
