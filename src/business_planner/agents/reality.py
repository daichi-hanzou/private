from __future__ import annotations

import json
import re

from openai import OpenAI

from .common import structured_response
from ..simulation.schemas import REALITY_OUTCOME_SCHEMA


INSTRUCTIONS = """あなたは事業計画を1年間実行した結果を生成するReality Agentです。
今回は、計画が目標に大きく届かなかったSynthetic Adverse Scenarioを作成します。
これは将来予測でも実績でもありません。公開資料にある実績値・リスク・制約を出発点に、
現実に起こり得る不振シナリオと合成財務数値を作成してください。

要件:
- simulation_yearは基準年度の翌年度とする。
- realized_revenue_growth_pctは目標成長率の半分以下かつ-15%以上とする。
- 営業利益と営業キャッシュフローは基準年度を下回り、在庫は増加するシナリオとする。
- 現在の全施策をFailed、Underperformed、Mixedのいずれかとして評価する。
- 各失敗について近因、根本原因、財務的影響を明確に分ける。
- financial_bridgeで、基準財務から合成財務までを施策効果を含めて完全に接続する。
- 次年度の再計画に使う合成社内データには、現行施策を少なくとも1件ずつ含める。
- さらに、失敗原因に対応する追加・置換・統合・縮小・廃止候補を合計2件以上作る。
- 各候補には一意のplanning_option_id、option_type、前身施策、
  投資額・人員・販促費・生産能力の推奨配分を設定する。
- Terminate候補はパイプライン、売上機会、投資、人員、販促費、生産能力を0とする。
- 合成社内データは実績ではなく、仮想パイプライン、成約率、売上機会、利益率とする。
- 指定された重点失敗パターンをfailure_reasonsに含め、同じ原因だけを毎年反復しない。
- failure_patternは、外部環境または通常の事業・プロジェクト実行上の失敗だけとする。
- Plannerの計画に明示されていない在庫押し込み、売上前倒し、過剰販促、
  値引き依存、販売金融条件の緩和、統制回避その他の不適切行動を追加してはいけない。
- 不適切な施策がPlannerの計画に明示されている場合だけ、その施策の実行結果として
  initiative_outcomesで評価する。failure_pattern側で新たに考案してはいけない。
- 合成数値同士の算術を一致させる。
- 公開資料の事実と合成仮定を区別し、仮定はsynthetic_assumptionsへ記録する。
- 実績であるかのように表現しない。
- source_idは与えられたものだけを使う。"""


FAILURE_PATTERN_ROTATION = (
    ("DemandShortfall", "SupplyDisruption", "ServiceChurn"),
    ("CostInflation", "CompetitivePressure", "FXHeadwind"),
    ("CustomerAdoptionDelay", "ProjectExecutionDelay", "LaborCapacityConstraint"),
    ("RegulatoryDelay", "TechnologyDelay", "DemandShortfall"),
    ("QualityRecall", "SupplyDisruption", "CostInflation"),
)


def _fallback_planning_option(
    *,
    option_id: str,
    initiative_name: str,
    option_type: str,
    predecessor_names: list[str],
    failure_pattern: str,
) -> dict:
    return {
        "planning_option_id": option_id,
        "option_type": option_type,
        "predecessor_initiative_names": predecessor_names,
        "initiative_name": initiative_name,
        "addressable_pipeline_revenue_million_yen": 0.0,
        "conversion_rate_pct": 0.0,
        "one_year_revenue_opportunity_million_yen": 0.0,
        "operating_margin_pct": 0.0,
        "confidence": "Low",
        "assumption_basis": (
            "Reality Agentの候補不足を補う保守的な自動候補。"
            "売上機会と資源配分は確定根拠がないため0とする。"
        ),
        "suggested_resource_allocation": {
            "investment_million_yen": 0.0,
            "headcount_fte": 0,
            "marketing_spend_million_yen": 0.0,
            "production_capacity_pct": 0.0,
        },
        "failure_pattern_addressed": failure_pattern,
    }


def _ensure_planning_options(
    result: dict, plan: dict, round_index: int
) -> None:
    options = result.setdefault("synthetic_internal_data", [])
    current_names = [item["name"] for item in plan.get("growth_plan", [])]
    covered = {
        name
        for option in options
        for name in option.get("predecessor_initiative_names", [])
    }
    pattern = FAILURE_PATTERN_ROTATION[
        (round_index - 1) % len(FAILURE_PATTERN_ROTATION)
    ][0]
    for index, name in enumerate(current_names, start=1):
        if name not in covered:
            options.append(_fallback_planning_option(
                option_id=f"AUTO-R{round_index}-CONTINUE-{index}",
                initiative_name=name,
                option_type="Continue",
                predecessor_names=[name],
                failure_pattern=pattern,
            ))
    minimum = len(current_names) + 2
    anchor = current_names[0] if current_names else "ポートフォリオ"
    if len(options) < minimum:
        options.append(_fallback_planning_option(
            option_id=f"AUTO-R{round_index}-REDUCE",
            initiative_name=f"{anchor}（資源縮小案）",
            option_type="Reduce",
            predecessor_names=[anchor] if current_names else [],
            failure_pattern=pattern,
        ))
    if len(options) < minimum:
        options.append(_fallback_planning_option(
            option_id=f"AUTO-R{round_index}-TERMINATE",
            initiative_name=f"{anchor}の廃止",
            option_type="Terminate",
            predecessor_names=[anchor] if current_names else [],
            failure_pattern=pattern,
        ))
    used_ids = set()
    for index, option in enumerate(options, start=1):
        option_id = option.get("planning_option_id") or (
            f"AUTO-R{round_index}-OPTION-{index}"
        )
        if option_id in used_ids:
            option_id = f"{option_id}-{index}"
        option["planning_option_id"] = option_id
        used_ids.add(option_id)
        if option.get("option_type") == "Terminate":
            option["addressable_pipeline_revenue_million_yen"] = 0.0
            option["conversion_rate_pct"] = 0.0
            option["operating_margin_pct"] = 0.0
            option["suggested_resource_allocation"] = {
                "investment_million_yen": 0.0,
                "headcount_fte": 0,
                "marketing_spend_million_yen": 0.0,
                "production_capacity_pct": 0.0,
            }
        option["one_year_revenue_opportunity_million_yen"] = (
            option["addressable_pipeline_revenue_million_yen"]
            * option["conversion_rate_pct"] / 100
        )


def _approximately_equal(left: float, right: float, tolerance: float = 0.01) -> bool:
    return abs(left - right) <= max(abs(right) * tolerance, 1.0)


def _summary_oku_yen(text: str, label: str) -> float:
    match = re.search(
        rf"{re.escape(label)}(?!率)\s*(?:は|[:：])?\s*(?:約)?\s*"
        rf"(?:(\d+(?:\.\d+)?)兆)?\s*([\d,]+(?:\.\d+)?)億円",
        text,
    )
    if not match:
        raise ValueError(f"Could not extract approximate financial value: {label}")
    trillion = float(match.group(1) or 0) * 1_000_000
    oku = float(match.group(2).replace(",", "")) * 100
    return trillion + oku


def _labeled_source_values(chunks, label: str) -> list[float]:
    """Extract label-adjacent source amounts and normalize them to million yen."""
    values = []
    pattern = re.compile(
        rf"{re.escape(label)}(?!率)\s*(?:は|[:：])?\s*(?:約)?\s*"
        rf"([\d,]+(?:\.\d+)?)\s*(兆円|億円|百万円)?"
    )
    for chunk in chunks:
        for match in pattern.finditer(chunk.text):
            value = float(match.group(1).replace(",", ""))
            unit = match.group(2)
            if unit == "兆円":
                value *= 1_000_000
            elif unit == "億円":
                value *= 100
            # Source tables generally state that their unit is 百万円 in a
            # heading, leaving individual rows unitless. Treat a unitless
            # comma-separated value as million yen as well.
            values.append(value)
    return values


def _numbers(text: str) -> list[float]:
    return [
        float(value.replace(",", ""))
        for value in re.findall(r"(?<![\d.])(\d{1,3}(?:,\d{3})+)(?![\d.])", text)
    ]


def _closest_precise_value(chunks, approximate: float) -> float:
    candidates = [
        value
        for chunk in chunks
        for value in _numbers(chunk.text)
        if abs(value - approximate) <= approximate * 0.005
    ]
    if not candidates:
        raise ValueError(f"No precise source value found near {approximate}")
    return min(candidates, key=lambda value: abs(value - approximate))


def extract_baseline_financials(plan: dict, chunks) -> dict[str, float]:
    summary = plan["financial_summary"]
    try:
        revenue = _closest_precise_value(
            chunks, _summary_oku_yen(summary, "売上収益")
        )
    except ValueError:
        revenue_candidates = _labeled_source_values(chunks, "売上収益")
        if not revenue_candidates:
            raise ValueError(
                "Could not extract revenue from either the plan summary or "
                "source documents"
            )
        revenue = revenue_candidates[0]
    try:
        operating_profit = _closest_precise_value(
            chunks, _summary_oku_yen(summary, "営業利益")
        )
    except ValueError:
        profit_candidates = [
            value for value in _labeled_source_values(chunks, "営業利益")
            if 0 < value < revenue
        ]
        if not profit_candidates:
            raise ValueError(
                "Could not extract operating profit from either the plan "
                "summary or source documents"
            )
        operating_profit = profit_candidates[0]
    revenue_marker = f"{int(revenue):,}"
    preferred_chunks = [
        chunk for chunk in chunks
        if revenue_marker in chunk.text and "営業活動による" in chunk.text
    ]
    search_chunks = preferred_chunks or chunks
    cash_flow_candidates = []
    for chunk in search_chunks:
        match = re.search(
            r"営業活動による\s*キャッシュ・フロー(.*?)投資活動による",
            chunk.text,
        )
        if not match:
            continue
        values = [
            value for value in _numbers(match.group(1))
            if 10_000 <= value <= revenue
        ]
        if values:
            cash_flow_candidates.append(values[-1])
    if not cash_flow_candidates:
        raise ValueError("Could not extract operating cash flow from source documents")
    operating_cash_flow = max(cash_flow_candidates)
    return {
        "revenue_million_yen": revenue,
        "operating_profit_million_yen": operating_profit,
        "operating_margin_pct": operating_profit / revenue * 100,
        "operating_cash_flow_million_yen": operating_cash_flow,
        "inventory_change_million_yen": 0.0,
    }


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
    baseline_input = baseline_financials
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
    required_failure_patterns = FAILURE_PATTERN_ROTATION[
        (round_index - 1) % len(FAILURE_PATTERN_ROTATION)
    ]
    result = structured_response(
        client,
        schema=REALITY_OUTCOME_SCHEMA,
        schema_name="reality_outcome",
        instructions=INSTRUCTIONS,
        model=model,
        input_text=(
            f"round_index: {round_index}\n"
            f"simulation_year: {expected_year}\n"
            f"当年度に必要な売上成長率: {annual_target:.6f}%\n"
            f"計画期間全体の累計売上成長目標: {cumulative_target:.6f}%\n"
            f"最終年度の目標売上収益（百万円）: {target_revenue_million_yen}\n"
            f"当年度の重点失敗パターン: "
            f"{json.dumps(required_failure_patterns, ensure_ascii=False)}\n"
            f"計画期間: {json.dumps(plan['planning_period'], ensure_ascii=False)}\n\n"
            f"変更禁止の基準財務:\n"
            f"{json.dumps(baseline_input, ensure_ascii=False)}\n\n"
            f"事業計画:\n{json.dumps(plan, ensure_ascii=False)}\n\n"
            f"追加根拠:\n{evidence}"
        ),
    )
    # Round identity and fiscal year are controlled by the simulator, not by the
    # language model. Normalize harmless model repetition such as returning the
    # first round's year again during later rounds.
    result["round_index"] = round_index
    result["simulation_year"] = expected_year
    result["target_revenue_growth_pct"] = annual_target
    result["required_annual_revenue_growth_pct"] = annual_target
    result["cumulative_revenue_growth_target_pct"] = cumulative_target
    result["target_revenue_million_yen"] = target_revenue_million_yen
    _ensure_planning_options(result, plan, round_index)
    if result["realized_revenue_growth_pct"] >= result["target_revenue_growth_pct"]:
        raise ValueError("Adverse scenario unexpectedly met the revenue target")
    if not -15 <= result["realized_revenue_growth_pct"] <= (
        result["target_revenue_growth_pct"] / 2
    ):
        raise ValueError("Reality Agent did not produce the configured adverse outcome")

    # Financial values that are mechanically derivable are normalized here.
    # This keeps a multi-round run from stopping because the model made a
    # rounding or addition error, while preserving its scenario assumptions,
    # initiative-level effects, and narrative explanation.
    result["baseline_financials"] = dict(baseline_input)
    baseline = result["baseline_financials"]
    simulated = result["simulated_financials"]
    simulated["revenue_million_yen"] = baseline["revenue_million_yen"] * (
        1 + result["realized_revenue_growth_pct"] / 100
    )
    simulated["operating_margin_pct"] = (
        simulated["operating_profit_million_yen"]
        / simulated["revenue_million_yen"] * 100
    )
    initiative_revenue_total = sum(
        item["revenue_effect_million_yen"]
        for item in result["initiative_outcomes"]
    )
    initiative_profit_total = sum(
        item["profit_effect_million_yen"]
        for item in result["initiative_outcomes"]
    )
    bridge = result["financial_bridge"]
    bridge["initiative_revenue_effect_total_million_yen"] = (
        initiative_revenue_total
    )
    bridge["initiative_profit_effect_total_million_yen"] = (
        initiative_profit_total
    )
    bridge["other_revenue_effect_million_yen"] = (
        simulated["revenue_million_yen"]
        - baseline["revenue_million_yen"]
        - bridge["underlying_revenue_change_million_yen"]
        - initiative_revenue_total
    )
    bridge["other_profit_effect_million_yen"] = (
        simulated["operating_profit_million_yen"]
        - baseline["operating_profit_million_yen"]
        - bridge["underlying_profit_change_million_yen"]
        - initiative_profit_total
    )
    for key, expected in baseline_input.items():
        matches = (
            _approximately_equal(baseline[key], expected, tolerance=0.000001)
            if key == "operating_margin_pct"
            else baseline[key] == expected
        )
        if not matches:
            raise ValueError(f"Reality Agent changed baseline financial: {key}")
    expected_revenue = baseline["revenue_million_yen"] * (
        1 + result["realized_revenue_growth_pct"] / 100
    )
    if not _approximately_equal(simulated["revenue_million_yen"], expected_revenue):
        raise ValueError("Reality Agent returned inconsistent simulated revenue")
    if simulated["operating_profit_million_yen"] >= baseline[
        "operating_profit_million_yen"
    ]:
        raise ValueError("Adverse scenario did not reduce operating profit")
    if simulated["operating_cash_flow_million_yen"] >= baseline[
        "operating_cash_flow_million_yen"
    ]:
        raise ValueError("Adverse scenario did not reduce operating cash flow")
    if simulated["inventory_change_million_yen"] <= 0:
        raise ValueError("Adverse scenario did not include inventory build-up")
    if not _approximately_equal(
        bridge["initiative_revenue_effect_total_million_yen"],
        initiative_revenue_total,
        tolerance=0.000001,
    ):
        raise ValueError("Reality Agent returned an inconsistent initiative revenue total")
    if not _approximately_equal(
        bridge["initiative_profit_effect_total_million_yen"],
        initiative_profit_total,
        tolerance=0.000001,
    ):
        raise ValueError("Reality Agent returned an inconsistent initiative profit total")
    bridged_revenue = baseline["revenue_million_yen"] + sum((
        bridge["underlying_revenue_change_million_yen"],
        bridge["initiative_revenue_effect_total_million_yen"],
        bridge["other_revenue_effect_million_yen"],
    ))
    bridged_profit = baseline["operating_profit_million_yen"] + sum((
        bridge["underlying_profit_change_million_yen"],
        bridge["initiative_profit_effect_total_million_yen"],
        bridge["other_profit_effect_million_yen"],
    ))
    if not _approximately_equal(
        simulated["revenue_million_yen"], bridged_revenue, tolerance=0.000001
    ):
        raise ValueError("Reality Agent returned an inconsistent revenue bridge")
    if not _approximately_equal(
        simulated["operating_profit_million_yen"],
        bridged_profit,
        tolerance=0.000001,
    ):
        raise ValueError("Reality Agent returned an inconsistent profit bridge")
    for label, financials in (("baseline", baseline), ("simulated", simulated)):
        expected_margin = (
            financials["operating_profit_million_yen"]
            / financials["revenue_million_yen"] * 100
        )
        if not _approximately_equal(
            financials["operating_margin_pct"], expected_margin
        ):
            raise ValueError(f"Reality Agent returned inconsistent {label} margin")

    reason_ids = {
        item["failure_reason_id"] for item in result["failure_reasons"]
    }
    referenced_reason_ids = {
        reason_id
        for item in result["initiative_outcomes"]
        for reason_id in item["failure_reason_ids"]
    }
    if referenced_reason_ids - reason_ids:
        raise ValueError("Reality Agent referenced unknown failure reason IDs")
    expected_initiatives = {item["name"] for item in plan["growth_plan"]}
    actual_initiatives = {
        item["initiative_name"] for item in result["initiative_outcomes"]
    }
    if actual_initiatives != expected_initiatives:
        raise ValueError("Reality Agent did not evaluate every plan initiative")
    planning_inputs = result["synthetic_internal_data"]
    option_ids = [
        item.get("planning_option_id") for item in planning_inputs
        if item.get("planning_option_id")
    ]
    if len(option_ids) != len(set(option_ids)):
        raise ValueError("Could not normalize duplicate planning option IDs")
    covered_initiatives = {
        predecessor
        for item in planning_inputs
        for predecessor in item.get(
            "predecessor_initiative_names", [item["initiative_name"]]
        )
    }
    if expected_initiatives - covered_initiatives:
        raise ValueError("Could not construct options for every initiative")
    for item in planning_inputs:
        expected_opportunity = (
            item["addressable_pipeline_revenue_million_yen"]
            * item["conversion_rate_pct"] / 100
        )
        item["one_year_revenue_opportunity_million_yen"] = expected_opportunity
    if not result["failure_reasons"]:
        raise ValueError("Reality Agent returned no explicit failure reasons")
    return result
