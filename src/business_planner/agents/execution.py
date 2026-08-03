from __future__ import annotations

from copy import deepcopy
import json

from openai import OpenAI

from .common import structured_response
from ..simulation.schemas import FAILURE_PATTERNS, REALITY_OUTCOME_SCHEMA


PLANNING_DATA_SCHEMA = deepcopy(
    REALITY_OUTCOME_SCHEMA["properties"]["synthetic_internal_data"]
)

EXECUTION_OUTCOME_SCHEMA = {
    "type": "object",
    "additionalProperties": False,
    "required": [
        "round_index", "simulation_year", "proposed_revenue_growth_pct",
        "initiative_outcomes", "failure_reasons", "internal_planning_data",
        "execution_assumptions", "evidence_source_ids",
    ],
    "properties": {
        "round_index": {"type": "integer"},
        "simulation_year": {"type": "integer"},
        "proposed_revenue_growth_pct": {
            "type": "number", "minimum": -15, "maximum": 25
        },
        "initiative_outcomes": {
            "type": "array",
            "items": {
                "type": "object",
                "additionalProperties": False,
                "required": [
                    "initiative_name", "status", "execution_result",
                    "revenue_effect_million_yen",
                    "profit_effect_million_yen",
                    "cash_flow_effect_million_yen",
                    "inventory_change_million_yen",
                    "failure_reason_ids",
                ],
                "properties": {
                    "initiative_name": {"type": "string"},
                    "status": {
                        "type": "string",
                        "enum": ["Failed", "Underperformed", "Mixed"],
                    },
                    "execution_result": {"type": "string"},
                    "revenue_effect_million_yen": {"type": "number"},
                    "profit_effect_million_yen": {"type": "number"},
                    "cash_flow_effect_million_yen": {"type": "number"},
                    "inventory_change_million_yen": {"type": "number"},
                    "failure_reason_ids": {
                        "type": "array", "items": {"type": "string"}
                    },
                },
            },
        },
        "failure_reasons": {
            "type": "array",
            "items": {
                "type": "object",
                "additionalProperties": False,
                "required": [
                    "failure_reason_id", "category", "failure_pattern",
                    "proximate_cause", "root_cause", "financial_effect",
                    "evidence_source_ids",
                ],
                "properties": {
                    "failure_reason_id": {"type": "string"},
                    "category": {"type": "string"},
                    "failure_pattern": {
                        "type": "string", "enum": FAILURE_PATTERNS
                    },
                    "proximate_cause": {"type": "string"},
                    "root_cause": {"type": "string"},
                    "financial_effect": {"type": "string"},
                    "evidence_source_ids": {
                        "type": "array", "items": {"type": "string"}
                    },
                },
            },
        },
        "internal_planning_data": PLANNING_DATA_SCHEMA,
        "execution_assumptions": {
            "type": "array", "items": {"type": "string"}
        },
        "evidence_source_ids": {"type": "array", "items": {"type": "string"}},
    },
}


INSTRUCTIONS = """あなたはExecution Agentです。
Plannerが明示した施策を、与えられた外部環境の下で1年間通常実行した結果を評価します。
各施策の売上、利益、キャッシュ、在庫への効果と、次年度の社内計画候補を作成してください。

最重要ルール:
- 社内の実行行動は、Plannerの施策、目標、KPI、資源配分および判断理由から因果的に導く。
- Plannerの戦略や目標圧力が現場で問題のある実行へ具体化し得る場合は、
  その実行内容と結果をフィクションとして作成してよい。
- 年度や失敗パターンだけを理由に行動をあらかじめ決めず、Plannerの戦略とのつながりを
  simulated_resultおよびexecution_assumptionsで明確にする。
- シナリオを劇的にする目的だけで、Plannerの戦略と無関係な社内行動を追加しない。
- failure_patternはEnvironment Agentの外部ショックまたは通常の実行失敗に限定する。
- 現行の全施策を1件ずつ評価する。
- 次年度候補には現行施策の候補と、通常の縮小・置換・廃止候補を含める。
- source_idは与えられたものだけを使用する。
- 数値は合成社内仮定であり、実績として表現しない。"""


def execute_plan(
    client: OpenAI,
    *,
    plan: dict,
    environment_outcome: dict,
    evidence: str,
    simulation_year: int,
    round_index: int,
    model: str | None = None,
) -> dict:
    result = structured_response(
        client,
        schema=EXECUTION_OUTCOME_SCHEMA,
        schema_name="execution_outcome",
        instructions=INSTRUCTIONS,
        model=model,
        input_text=(
            f"round_index: {round_index}\n"
            f"simulation_year: {simulation_year}\n\n"
            f"Plannerの計画:\n{json.dumps(plan, ensure_ascii=False)}\n\n"
            f"外部環境:\n{json.dumps(environment_outcome, ensure_ascii=False)}\n\n"
            f"公開根拠:\n{evidence}"
        ),
    )
    result["round_index"] = round_index
    result["simulation_year"] = simulation_year
    return result
