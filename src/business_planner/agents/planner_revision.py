from __future__ import annotations

import json
from openai import OpenAI

from .common import structured_response
from ..executive_principles import principles_instructions
from ..schema import BUSINESS_PLAN_SCHEMA


INSTRUCTIONS = """あなたは公開情報と社内実行報告に基づくPlanner Agentです。
1年間の実行結果とCEOからのフィードバックを受け、事業計画を改訂します。
CEOが要求する売上KPIを考慮し、目標達成に向けた施策と資源配分を設計してください。
次年度の施策は、従来計画、1年間の実行報告、CEOフィードバックおよび
根拠資料に基づき、Planner自身が設計してください。
新規施策の追加に加え、従来施策の継続・拡大・縮小・統合・置換・廃止が可能です。
expected_revenue_impactには、その個別施策による次年度1年間の増分売上を
「Planner自主推計: +N百万円」の形式で記載してください。
expected_profit_impactには、その個別施策による次年度1年間の増分営業利益を
「Planner自主推計: +N百万円」の形式で記載してください。
decision_rationaleには、推計の計算方法、主要な前提、必要な資源、および
従来施策との関係を説明してください。
施策数は1～8件とし、名称と件数を固定してはいけません。
現行施策を継続・拡大・縮小・統合・置換・廃止でき、新施策も追加できます。
廃止施策はgrowth_planに残さず、portfolio_decisionsにTerminateとして記録してください。
portfolio_decisionsでは、従来の全施策についてContinue、Expand、Reduce、Merge、
Replace、Terminateのいずれかを必ず記録し、新施策はNewとして記録してください。
会社名、売上成長目標、計画期間を変更してはいけません。
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
    executive_principles: dict | None = None,
) -> dict:
    result = structured_response(
        client,
        schema=BUSINESS_PLAN_SCHEMA,
        schema_name="revised_business_plan",
        instructions=principles_instructions(executive_principles) + INSTRUCTIONS,
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
