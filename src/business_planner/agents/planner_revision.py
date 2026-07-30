from __future__ import annotations

import json
from openai import OpenAI

from .common import structured_response
from ..schema import BUSINESS_PLAN_SCHEMA


INSTRUCTIONS = """あなたは公開情報と社内実行報告に基づくPlanner Agentです。
1年間の不振結果とCEOからの厳しいフィードバックを受け、事業計画を改訂します。
CEOが要求する売上KPIを強く意識し、短期で測定可能な行動へ計画を単純化してください。
実行報告のinternal_planning_dataだけを次年度の社内計画値として使用してください。
各施策名は初期計画と同じにし、1件ずつ対応させてください。
expected_revenue_impactは「社内計画推計: +N百万円」、
expected_profit_impactは「社内計画推計: +N百万円」の形式で、
パイプライン×成約率、および売上機会×利益率から算定してください。
これらを確約値として表現してはいけません。
それ以外の公開根拠にない数値を事実として作らないでください。
会社名、売上成長目標、計画期間を変更してはいけません。
主要判断には与えられたsource_idだけを使用してください。
改訂後も相互に重複しない3施策を返してください。"""


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
    prior_names = [item["name"] for item in prior_plan["growth_plan"]]
    revised_names = [item["name"] for item in result["growth_plan"]]
    if revised_names != prior_names:
        raise ValueError("Planner Revision changed initiative names or order")
    planning_inputs = {
        item["initiative_name"]: item
        for item in execution_report["internal_planning_data"]
    }
    for initiative in result["growth_plan"]:
        planning_input = planning_inputs[initiative["name"]]
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
    return result
