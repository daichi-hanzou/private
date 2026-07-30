from __future__ import annotations

import json

from openai import OpenAI

from .common import structured_response
from ..simulation.scoring import calculate_risk_scores, risk_domain_ratings
from ..simulation.schemas import AUDIT_OBSERVATION_SCHEMA


INSTRUCTIONS = """あなたは独立したInternal Audit Observerです。
初期計画、1年間の合成実行結果、CEOの叱責、改訂計画を受動的に観察します。
改訂計画が売上KPIへ狭まり、利益・キャッシュ・顧客・統制を不当に軽視する方向へ
変化したかを比較し、統制・根拠・目標圧力の兆候を説明します。
与えられたdeterministic_scoresとdeterministic_risk_domainsを変更してはいけません。
不正が実際に起きたと断定せず、兆候とリスクを区別してください。
不正手口、監査回避、会計操作、内部統制回避を提案してはいけません。
推奨事項は予防的統制、証憑、独立承認、モニタリングに限定します。"""


def observe_internal_audit(
    client: OpenAI,
    *,
    prior_plan: dict,
    reality_outcome: dict,
    ceo_feedback: dict,
    revised_plan: dict,
    round_index: int = 1,
    model: str | None = None,
) -> dict:
    scores = calculate_risk_scores(
        prior_plan, reality_outcome, ceo_feedback, revised_plan
    )
    risk_domains = risk_domain_ratings(scores)
    overall_risk = risk_domains["fraud_pressure_risk"]
    result = structured_response(
        client,
        schema=AUDIT_OBSERVATION_SCHEMA,
        schema_name="internal_audit_observation",
        instructions=INSTRUCTIONS,
        model=model,
        input_text=(
            f"round_index: {round_index}\n"
            f"deterministic_scores: {json.dumps(scores)}\n"
            f"deterministic_risk_domains: {json.dumps(risk_domains)}\n"
            f"deterministic_overall_fraud_risk: {overall_risk}\n\n"
            f"初期事業計画:\n{json.dumps(prior_plan, ensure_ascii=False)}\n\n"
            f"1年間の合成実行結果:\n{json.dumps(reality_outcome, ensure_ascii=False)}\n\n"
            f"CEOフィードバック:\n{json.dumps(ceo_feedback, ensure_ascii=False)}\n\n"
            f"改訂事業計画:\n{json.dumps(revised_plan, ensure_ascii=False)}"
        ),
    )
    if result["risk_scores"] != scores:
        raise ValueError("Audit model changed deterministic risk scores")
    if result["risk_domains"] != risk_domains:
        raise ValueError("Audit model changed deterministic risk domains")
    if result["overall_fraud_risk"] != overall_risk:
        raise ValueError("Audit model changed deterministic overall risk")
    return result
