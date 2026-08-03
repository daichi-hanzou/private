from __future__ import annotations

import json

from openai import OpenAI

from .common import structured_response
from ..simulation.schemas import FAILURE_PATTERNS


ENVIRONMENT_OUTCOME_SCHEMA = {
    "type": "object",
    "additionalProperties": False,
    "required": [
        "round_index", "simulation_year", "external_shocks",
        "market_revenue_growth_pct",
        "operating_profit_pressure_million_yen",
        "operating_cash_flow_pressure_million_yen",
        "inventory_pressure_million_yen",
        "assumptions", "evidence_source_ids",
    ],
    "properties": {
        "round_index": {"type": "integer"},
        "simulation_year": {"type": "integer"},
        "external_shocks": {
            "type": "array",
            "minItems": 1,
            "items": {
                "type": "object",
                "additionalProperties": False,
                "required": [
                    "failure_pattern", "severity", "description",
                    "financial_transmission", "evidence_source_ids",
                ],
                "properties": {
                    "failure_pattern": {
                        "type": "string", "enum": FAILURE_PATTERNS
                    },
                    "severity": {
                        "type": "string", "enum": ["Low", "Medium", "High"]
                    },
                    "description": {"type": "string"},
                    "financial_transmission": {"type": "string"},
                    "evidence_source_ids": {
                        "type": "array", "items": {"type": "string"}
                    },
                },
            },
        },
        "market_revenue_growth_pct": {
            "type": "number", "minimum": -15, "maximum": 10
        },
        "operating_profit_pressure_million_yen": {
            "type": "number", "minimum": 0
        },
        "operating_cash_flow_pressure_million_yen": {
            "type": "number", "minimum": 0
        },
        "inventory_pressure_million_yen": {
            "type": "number", "minimum": 0
        },
        "assumptions": {"type": "array", "items": {"type": "string"}},
        "evidence_source_ids": {"type": "array", "items": {"type": "string"}},
    },
}


INSTRUCTIONS = """あなたはEnvironment Agentです。
事業計画が実行される1年間の外部環境だけを生成してください。
需要、競争、為替、供給網、コスト、規制、技術成熟、顧客採用、品質イベントなどを扱います。

役割境界:
- CEO、Planner、社是、評価制度、報酬制度、社内の意図を推測しない。
- 社内の意思決定や実行行動は描写せず、外部環境だけを作成する。
- Plannerが取るべき施策を提案しない。
- 実績値または将来予測として表現しない。

公開資料のリスクを起点とする合成外部シナリオとして、数値仮定と根拠を区別してください。
source_idは与えられたものだけを使用してください。"""


def generate_environment(
    client: OpenAI,
    *,
    plan: dict,
    evidence: str,
    simulation_year: int,
    round_index: int,
    failure_patterns: tuple[str, ...],
    model: str | None = None,
) -> dict:
    result = structured_response(
        client,
        schema=ENVIRONMENT_OUTCOME_SCHEMA,
        schema_name="environment_outcome",
        instructions=INSTRUCTIONS,
        model=model,
        input_text=(
            f"round_index: {round_index}\n"
            f"simulation_year: {simulation_year}\n"
            f"重点外部失敗パターン: "
            f"{json.dumps(failure_patterns, ensure_ascii=False)}\n\n"
            f"事業領域と施策名:\n"
            f"{json.dumps([item['name'] for item in plan.get('growth_plan', [])], ensure_ascii=False)}\n\n"
            f"公開根拠:\n{evidence}"
        ),
    )
    result["round_index"] = round_index
    result["simulation_year"] = simulation_year
    return result
