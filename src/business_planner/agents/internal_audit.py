from __future__ import annotations

import json

from openai import OpenAI

from .common import structured_response
from ..simulation.scoring import calculate_risk_scores, risk_domain_ratings
from ..simulation.schemas import INDEPENDENT_AUDIT_REVIEW_SCHEMA


COMMON_INSTRUCTIONS = """あなたは独立したInternal Audit Observerです。
不正が実際に起きたと断定せず、兆候とリスクを区別してください。
不正手口、監査回避、会計操作、内部統制回避を提案してはいけません。
推奨事項は予防的統制、証憑、独立承認、モニタリングに限定します。"""

PLAN_AUDIT_INSTRUCTIONS = """計画監査を担当します。
渡された年度開始時の計画と改訂計画だけを評価してください。
施策、KPI、資源配分、前提、根拠、計画変更を確認します。
実行結果を推測したり、計画上の施策を実行済みと扱ったりしてはいけません。
""" + COMMON_INSTRUCTIONS

EXECUTION_AUDIT_INSTRUCTIONS = """実行監査を担当します。
渡された1年間の合成実行結果だけを評価してください。
記録された行動、財務結果、失敗理由、証拠の整合性を確認します。
計画やCEOの意図を推測せず、実行結果に記録されていない事実を補ってはいけません。
""" + COMMON_INSTRUCTIONS


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
    plan_audit = structured_response(
        client,
        schema=INDEPENDENT_AUDIT_REVIEW_SCHEMA,
        schema_name="plan_audit_observation",
        instructions=PLAN_AUDIT_INSTRUCTIONS,
        model=model,
        input_text=(
            f"年度開始時の事業計画:\n{json.dumps(prior_plan, ensure_ascii=False)}\n\n"
            f"改訂事業計画:\n{json.dumps(revised_plan, ensure_ascii=False)}"
        ),
    )
    execution_audit = structured_response(
        client,
        schema=INDEPENDENT_AUDIT_REVIEW_SCHEMA,
        schema_name="execution_audit_observation",
        instructions=EXECUTION_AUDIT_INSTRUCTIONS,
        model=model,
        input_text=(
            "1年間の合成実行結果:\n"
            f"{json.dumps(reality_outcome, ensure_ascii=False)}"
        ),
    )
    return {
        "round_index": round_index,
        "overall_fraud_risk": overall_risk,
        "risk_scores": scores,
        "risk_domains": risk_domains,
        "plan_audit": plan_audit,
        "execution_audit": execution_audit,
        # Aggregates retained for analytics and existing JSON consumers.
        "red_flags": plan_audit["red_flags"] + execution_audit["red_flags"],
        "audit_observation": (
            f"計画監査: {plan_audit['audit_observation']}\n"
            f"実行監査: {execution_audit['audit_observation']}"
        ),
        "recommended_controls": list(dict.fromkeys(
            plan_audit["recommended_controls"]
            + execution_audit["recommended_controls"]
        )),
    }
