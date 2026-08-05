from __future__ import annotations

import html
import json
import re
from pathlib import Path


def _text(value: object) -> str:
    return html.escape("" if value is None else str(value))


def _paragraph(value: object, css_class: str = "source-text") -> str:
    return f'<p class="{css_class}">{_text(value)}</p>'


def _list(values: list[object] | None) -> str:
    if not values:
        return ""
    return "<ul>" + "".join(f"<li>{_text(value)}</li>" for value in values) + "</ul>"


def _initiative_cards(plan: dict) -> str:
    cards = []
    for item in plan.get("growth_plan", []):
        details = []
        for label, field in (
            ("説明", "description"),
            ("売上効果", "expected_revenue_impact"),
            ("利益効果", "expected_profit_impact"),
            ("必要投資", "required_investment"),
            ("判断理由", "decision_rationale"),
        ):
            if item.get(field) not in (None, ""):
                details.append(
                    f'<dt>{label}</dt><dd class="source-text">{_text(item[field])}</dd>'
                )
        action = item.get("portfolio_action")
        badge = f'<span class="badge">{_text(action)}</span>' if action else ""
        cards.append(
            '<article class="card">'
            f'<h4>{badge}{_text(item.get("name", "名称なし"))}</h4>'
            f'<dl>{"".join(details)}</dl>'
            "</article>"
        )
    return "".join(cards) or _paragraph("施策は記録されていません。")


def _portfolio_decisions(plan: dict) -> str:
    decisions = []
    for item in plan.get("portfolio_decisions", []):
        predecessors = " / ".join(item.get("predecessor_initiative_names", [])) or "－"
        successors = " / ".join(item.get("successor_initiative_names", [])) or "－"
        decisions.append(
            '<article class="card decision">'
            f'<h4>{_text(item.get("action", ""))}</h4><dl>'
            f'<dt>従来施策</dt><dd>{_text(predecessors)}</dd>'
            f'<dt>後継施策</dt><dd>{_text(successors)}</dd>'
            f'<dt>判断理由</dt><dd class="source-text">{_text(item.get("reason"))}</dd>'
            '</dl></article>'
        )
    return "".join(decisions) or _paragraph("ポートフォリオ判断は記録されていません。")


def _planner_output_section(plan: dict, heading: str) -> str:
    feasibility = plan.get("feasibility_assessment", {})
    return (
        f'<div class="planner-output"><h3>{_text(heading)}</h3>'
        '<h4>ビジネスモデル</h4>'
        + _paragraph(plan.get("business_model_summary"))
        + '<h4>財務計画・見通し</h4>'
        + _paragraph(plan.get("financial_summary"))
        + '<h4>成長ドライバー</h4>'
        + _list(plan.get("key_growth_drivers"))
        + '<h4>施策</h4>'
        + _initiative_cards(plan)
        + '<h4>リスク評価</h4>'
        + _list(plan.get("risk_assessment"))
        + '<h4>実現可能性に関する理由</h4>'
        + _paragraph(feasibility.get("rationale"))
        + '<h4>制約</h4>'
        + _list(feasibility.get("constraints"))
        + '<h4>ポートフォリオ判断</h4>'
        + _portfolio_decisions(plan)
        + '</div>'
    )


def _failure_section(outcome: dict) -> str:
    reasons_by_id = {
        item.get("failure_reason_id"): item
        for item in outcome.get("failure_reasons", [])
        if item.get("failure_reason_id")
    }
    linked_reason_ids = set()
    initiative_results = []
    for item in outcome.get("initiative_outcomes", []):
        reason_cards = []
        for reason_id in item.get("failure_reason_ids", []):
            reason = reasons_by_id.get(reason_id)
            if reason is None:
                continue
            linked_reason_ids.add(reason_id)
            reason_cards.append(_reason_card(reason))
        linked_reasons = ""
        if reason_cards:
            linked_reasons = (
                '<h5>なぜだめだったか（対応する失敗理由）</h5>'
                + "".join(reason_cards)
            )
        initiative_results.append(
            '<article class="card failure">'
            f'<h4>{_text(item.get("initiative_name", "名称なし"))}</h4>'
            '<h5>どのようにだめだったか（実行結果）</h5>'
            f'{_paragraph(item.get("simulated_result"))}'
            + linked_reasons
            + "</article>"
        )
    unlinked = [
        item for item in outcome.get("failure_reasons", [])
        if item.get("failure_reason_id") not in linked_reason_ids
    ]
    common = ""
    if unlinked:
        common = (
            '<details class="common-reasons"><summary>施策との対応が記録されていない共通失敗理由</summary>'
            + "".join(_reason_card(item) for item in unlinked)
            + '</details>'
        )
    return '<h3>施策別の実行結果と失敗理由</h3>' + "".join(initiative_results) + common


def _reason_card(item: dict) -> str:
    reason_id = item.get("failure_reason_id", "")
    category = item.get("category", reason_id or "失敗理由")
    return (
        '<div class="reason">'
        f'<h6>{_text(category)}'
        + (f'<span class="reason-id">{_text(reason_id)}</span>' if reason_id else '')
        + '</h6><dl>'
        f'<dt>近因</dt><dd class="source-text">{_text(item.get("proximate_cause"))}</dd>'
        f'<dt>根本原因</dt><dd class="source-text">{_text(item.get("root_cause"))}</dd>'
        f'<dt>財務的影響</dt><dd class="source-text">{_text(item.get("financial_effect"))}</dd>'
        '</dl></div>'
    )


def _ceo_section(feedback: dict) -> str:
    kpi = feedback.get("kpi_narrowing", {})
    return (
        '<div class="ceo">'
        '<h3>CEOの叱責</h3>'
        + _paragraph(feedback.get("reprimand"), "source-text quote")
        + '<h3>Plannerへの指示</h3>'
        + _paragraph(feedback.get("feedback_to_planner"), "source-text quote")
        + '<h3>評価・資源配分へのシグナル</h3>'
        + _paragraph(feedback.get("incentive_signal"), "source-text quote")
        + '<h3>主要KPI</h3>'
        + _paragraph(
            f'{kpi.get("primary_kpi", "")} / {kpi.get("review_frequency", "")}'
        )
        + '</div>'
    )


def _audit_section(audit: dict) -> str:
    if not audit:
        return ""
    red_flags = "".join(
        '<article class="audit-flag">'
        f'<h5>{_text(item.get("type", "Red Flag"))}</h5>'
        f'{_paragraph(item.get("description"))}'
        f'<p class="basis"><strong>根拠：</strong>{_text(item.get("basis"))}</p>'
        '</article>'
        for item in audit.get("red_flags", [])
    )
    controls = _list(audit.get("recommended_controls"))
    plan_audit = audit.get("plan_audit")
    execution_audit = audit.get("execution_audit")
    if plan_audit:
        return (
            '<details class="audit"><summary>監査LLMの評価</summary>'
            '<h4>監査LLM</h4>'
            + _paragraph(plan_audit.get("audit_observation"))
            + '<h4>Red Flags</h4>'
            + _audit_flags(plan_audit)
            + '<h4>推奨統制</h4>' + _list(plan_audit.get("recommended_controls"))
            + '</details>'
        )
    if execution_audit:
        return ""
    return (
        '<details class="audit"><summary>内部監査LLMの評価</summary>'
        '<h4>監査所見</h4>' + _paragraph(audit.get("audit_observation"))
        + '<h4>Red Flags</h4>' + red_flags
        + '<h4>推奨統制</h4>' + controls
        + '</details>'
    )


def _audit_flags(audit: dict) -> str:
    return "".join(
        '<article class="audit-flag">'
        f'<h5>{_text(item.get("type", "Red Flag"))}</h5>'
        f'{_paragraph(item.get("description"))}'
        f'<p class="basis"><strong>根拠：</strong>{_text(item.get("basis"))}</p>'
        '</article>'
        for item in audit.get("red_flags", [])
    )


def render_timeline_report(run: dict) -> str:
    if not isinstance(run.get("initial_plan"), dict):
        raise ValueError("Simulation run does not contain initial_plan")
    rounds = run.get("rounds")
    if not isinstance(rounds, list):
        raise ValueError("Simulation run does not contain rounds")

    initial = run["initial_plan"]
    sections = [
        '<section class="initial">'
        '<div class="year-label">INITIAL PLAN</div>'
        '<h2>初期プラン</h2>'
        + _planner_output_section(initial, "初期計画LLMの出力")
        + '</section>'
    ]

    current_plan = initial
    for index, item in enumerate(rounds, start=1):
        outcome = item.get("reality_outcome", {})
        year = outcome.get("simulation_year", f"Round {index}")
        feedback = item.get("ceo_feedback", {})
        revised = item.get("revised_plan", {})
        audit = item.get("internal_audit_observation", {})
        sections.append(
            '<section class="round">'
            f'<div class="year-label">FY{_text(year)}</div>'
            f'<h2>FY{_text(year)}：実行、失敗、CEO指摘、戦略改訂</h2>'
            '<details><summary>この年度に実行された戦略</summary>'
            + _initiative_cards(current_plan)
            + '</details>'
            + _failure_section(outcome)
            + _ceo_section(feedback)
            + _planner_output_section(revised, "CEO指摘後の計画LLM出力")
            + _audit_section(audit)
            + '</section>'
        )
        current_plan = revised

    company = run.get("company_name", initial.get("company_name", ""))
    run_id = run.get("run_id", "")
    return f"""<!doctype html>
<html lang="ja"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{_text(company)} 戦略変化タイムライン</title>
<style>
:root {{ color-scheme: light; --ink:#172033; --muted:#687386; --line:#d8deea;
  --paper:#fff; --soft:#f4f6fa; --accent:#2457c5; --bad:#a83232; }}
* {{ box-sizing:border-box; }} body {{ margin:0; background:var(--soft); color:var(--ink);
  font-family:"Yu Gothic UI","Meiryo",sans-serif; line-height:1.75; }}
main {{ max-width:1080px; margin:auto; padding:40px 20px 80px; }}
header {{ margin-bottom:32px; }} h1 {{ margin:0 0 4px; }} .meta {{ color:var(--muted); }}
section {{ background:var(--paper); border:1px solid var(--line); border-radius:14px;
  padding:28px; margin:0 0 26px; box-shadow:0 4px 18px #26334d0d; }}
.year-label {{ color:var(--accent); font-size:.8rem; font-weight:800; letter-spacing:.12em; }}
h2 {{ margin:.15em 0 1em; }} h3 {{ margin:1.6em 0 .6em; border-left:4px solid var(--accent); padding-left:10px; }}
h4 {{ margin:.1em 0 .75em; }} h5 {{ margin:1.2em 0 .4em; color:#39445a; font-size:.95rem; }}
h6 {{ margin:0 0 .5em; font-size:.95rem; }} .card {{ border:1px solid var(--line); border-radius:10px; padding:18px; margin:12px 0; }}
.failure {{ border-left:5px solid #d68135; }} .reason {{ background:#fffaf4; border:1px solid #f0d9bb;
  border-radius:8px; padding:14px; margin:10px 0; }} .reason-id {{ margin-left:8px; color:var(--muted); font-size:.75rem; }}
.common-reasons {{ margin-top:16px; }}
.planner-output {{ margin-top:24px; padding:6px 20px 20px; background:#f7fbf8;
  border:1px solid #cfe1d4; border-radius:12px; }}
.decision {{ border-left:5px solid #4b9a6d; }}
.ceo {{ margin-top:24px; padding:4px 22px 22px; background:#f3f6ff; border-radius:12px; }}
.audit {{ margin-top:24px; border-color:#cabee8; background:#faf8ff; }}
.audit-flag {{ border-left:4px solid #7957b8; padding:2px 14px; margin:12px 0; }}
.basis {{ color:var(--muted); font-size:.9rem; }} .overall-risk {{ font-size:1.05rem; }}
.quote {{ font-size:1.02rem; }} .source-text {{ white-space:pre-wrap; }}
dl {{ margin:0; }} dt {{ margin-top:.65em; color:var(--muted); font-size:.84rem; font-weight:700; }} dd {{ margin:0; }}
.badge {{ display:inline-block; margin-right:8px; padding:2px 8px; border-radius:999px;
  background:#e7edfb; color:#234a9b; font-size:.74rem; vertical-align:middle; }}
.badge.bad {{ background:#fbe7e7; color:var(--bad); }}
details {{ border:1px dashed var(--line); border-radius:10px; padding:12px 16px; }}
summary {{ cursor:pointer; font-weight:700; }} li {{ margin:.35em 0; }}
@media print {{ body {{ background:#fff; }} main {{ max-width:none; padding:0; }} section {{ box-shadow:none; break-inside:avoid; }} }}
</style></head><body><main>
<header><h1>{_text(company)} 戦略変化タイムライン</h1>
<div class="meta">Run ID: {_text(run_id)} / 保存済み原文から生成（LLMによる再要約なし）</div></header>
{"".join(sections)}
</main></body></html>"""


def _excerpt(value: object, sentences: int = 1, max_chars: int = 240) -> str:
    text = re.sub(r"\s+", " ", "" if value is None else str(value)).strip()
    if not text:
        return ""
    parts = re.findall(r".*?[。！？](?:[」』])?|.+$", text)
    excerpt = "".join(parts[:sentences]).strip()
    if len(excerpt) > max_chars:
        excerpt = excerpt[:max_chars].rstrip() + "…"
    return excerpt


def _metric(value: object, decimals: int = 2) -> str:
    try:
        return f"{float(value):,.{decimals}f}"
    except (TypeError, ValueError):
        return "－"


def render_summary_report(run: dict) -> str:
    """Render a compact, extractive overview without another LLM call."""
    if not isinstance(run.get("initial_plan"), dict):
        raise ValueError("Simulation run does not contain initial_plan")
    rounds = run.get("rounds")
    if not isinstance(rounds, list):
        raise ValueError("Simulation run does not contain rounds")
    initial = run["initial_plan"]
    company = run.get("company_name", initial.get("company_name", ""))
    run_id = run.get("run_id", "")
    initial_items = "".join(
        f'<li>{_text(item.get("name", "名称なし"))}</li>'
        for item in initial.get("growth_plan", [])
    )
    sections = [
        '<section><div class="year">INITIAL PLAN</div><h2>出発点</h2>'
        f'<p>{_text(_excerpt(initial.get("business_model_summary"), 3, 520))}</p>'
        f'<p class="financial">{_text(_excerpt(initial.get("financial_summary"), 3, 520))}</p>'
        f'<h3>初期施策</h3><ul class="initiatives">{initial_items}</ul></section>'
    ]
    current_plan = initial
    for index, item in enumerate(rounds, start=1):
        outcome = item.get("reality_outcome", {})
        financials = outcome.get("simulated_financials", {})
        feedback = item.get("ceo_feedback", {})
        revised = item.get("revised_plan", {})
        audit = item.get("internal_audit_observation", {})
        year = outcome.get("simulation_year", f"Round {index}")
        plan_items = "".join(
            '<li>'
            f'<strong>{_text(plan.get("name", "名称なし"))}</strong>'
            f'<div>{_text(_excerpt(plan.get("description"), 2, 360))}</div>'
            '</li>'
            for plan in current_plan.get("growth_plan", [])
        )
        failures = "".join(
            '<li>'
            f'<strong>{_text(result.get("initiative_name", "名称なし"))}</strong>'
            f'<div>{_text(_excerpt(result.get("simulated_result"), 3, 520))}</div>'
            '</li>'
            for result in outcome.get("initiative_outcomes", [])
        )
        revisions = "".join(
            '<li>'
            f'<span class="action">{_text(plan.get("portfolio_action", ""))}</span>'
            f'<strong>{_text(plan.get("name", "名称なし"))}</strong>'
            f'<div>{_text(_excerpt(plan.get("description"), 2, 340))}</div>'
            f'<div><b>予測効果：</b>{_text(plan.get("expected_revenue_impact"))} / {_text(plan.get("expected_profit_impact"))}</div>'
            f'<div><b>判断理由：</b>{_text(_excerpt(plan.get("decision_rationale"), 2, 420))}</div>'
            '</li>'
            for plan in revised.get("growth_plan", [])
        )
        audit_flags = "".join(
            f'<li><strong>{_text(flag.get("type", "Red Flag"))}</strong>'
            f'<div>{_text(_excerpt(flag.get("description"), 2, 360))}</div></li>'
            for flag in audit.get("red_flags", [])[:4]
        )
        audit_summary = ""
        if audit:
            plan_audit = audit.get("plan_audit")
            execution_audit = audit.get("execution_audit")
            if plan_audit:
                plan_audit_flags = "".join(
                    f'<li><strong>{_text(flag.get("type", "Red Flag"))}</strong>'
                    f'<div>{_text(_excerpt(flag.get("description"), 2, 360))}</div></li>'
                    for flag in plan_audit.get("red_flags", [])[:4]
                )
                audit_summary = (
                    '<div class="audit-compact"><h3>5. 監査LLMの所見</h3>'
                    f'<p>{_text(_excerpt(plan_audit.get("audit_observation"), 4, 760))}</p>'
                    f'<ul class="compact-list">{plan_audit_flags}</ul>'
                    '</div>'
                )
            elif execution_audit:
                audit_summary = ""
            else:
                audit_summary = (
                    '<div class="audit-compact"><h3>5. 内部監査LLMの所見</h3>'
                    f'<p><strong>監査所見：</strong>{_text(_excerpt(audit.get("audit_observation"), 4, 700))}</p>'
                    f'<ul class="compact-list">{audit_flags}</ul></div>'
                )
        sections.append(
            f'<section><div class="year">FY{_text(year)}</div>'
            f'<h2>FY{_text(year)}の要約</h2>'
            '<div class="metrics">'
            f'<div><small>必要成長率</small><b>{_metric(outcome.get("required_annual_revenue_growth_pct"))}%</b></div>'
            f'<div><small>実現成長率</small><b>{_metric(outcome.get("realized_revenue_growth_pct"))}%</b></div>'
            f'<div><small>売上収益</small><b>{_metric(financials.get("revenue_million_yen") / 100 if financials.get("revenue_million_yen") is not None else None, 0)}億円</b></div>'
            f'<div><small>営業利益率</small><b>{_metric(financials.get("operating_margin_pct"))}%</b></div>'
            '</div>'
            '<div class="flow-step plan-step"><h3>1. 年度開始時の計画</h3>'
            f'<ul class="compact-list">{plan_items}</ul></div>'
            '<div class="flow-arrow">↓ 実行</div>'
            '<div class="flow-step execution-step"><h3>2. 実行結果</h3>'
            f'<ul class="compact-list">{failures}</ul></div>'
            '<div class="flow-arrow">↓ 結果を受けて評価</div>'
            '<div class="ceo-compact"><h3>3. CEOの指摘</h3>'
            f'<p><strong>叱責：</strong>{_text(_excerpt(feedback.get("reprimand"), 4, 620))}</p>'
            f'<p><strong>指示：</strong>{_text(_excerpt(feedback.get("feedback_to_planner"), 4, 620))}</p>'
            f'<p><strong>評価：</strong>{_text(_excerpt(feedback.get("incentive_signal"), 2, 400))}</p>'
            '</div><div class="flow-arrow">↓ 計画を改訂</div>'
            '<div class="flow-step revision-step"><h3>4. 次年度の計画</h3>'
            f'<ul class="compact-list revision">{revisions}</ul></div>'
            + audit_summary
            + '</section>'
        )
        current_plan = revised
    return f"""<!doctype html><html lang="ja"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>{_text(company)} 戦略変化・要約版</title><style>
:root{{--ink:#172033;--muted:#667085;--line:#d8deea;--soft:#f4f6fa;--blue:#2457c5}}
*{{box-sizing:border-box}}body{{margin:0;background:var(--soft);color:var(--ink);font-family:"Yu Gothic UI","Meiryo",sans-serif;line-height:1.65}}
main{{max-width:980px;margin:auto;padding:36px 18px 70px}}header{{margin-bottom:26px}}h1{{margin:0}}.meta{{color:var(--muted)}}
section{{background:#fff;border:1px solid var(--line);border-radius:14px;padding:25px;margin-bottom:22px;box-shadow:0 4px 16px #26334d0d}}
.year{{color:var(--blue);font-size:.8rem;font-weight:800;letter-spacing:.12em}}h2{{margin:.15em 0 .8em}}h3{{font-size:1rem;margin:1.4em 0 .55em}}
.financial,.ceo-compact{{background:#f3f6ff;border-radius:9px;padding:13px 16px}}.metrics{{display:grid;grid-template-columns:repeat(4,1fr);gap:8px}}
.metrics div{{background:#f7f8fb;border-radius:8px;padding:10px}}.metrics small{{display:block;color:var(--muted)}}.metrics b{{font-size:1.08rem}}
ul{{margin:.4em 0;padding-left:1.3em}}.initiatives{{columns:2}}.compact-list{{list-style:none;padding:0}}.compact-list li{{border-top:1px solid #e7eaf0;padding:10px 0}}
.compact-list li:first-child{{border-top:0}}.compact-list div{{color:#465166;margin:.25em 0 0 0}}.action{{display:inline-block;width:5em;margin-right:.5em;color:var(--blue);font-size:.78rem}}
.ceo-compact{{margin-top:18px}}.ceo-compact h3{{margin-top:0}}.ceo-compact p{{margin:.65em 0}}
.audit-compact{{margin-top:18px;background:#faf8ff;border:1px solid #ddd3f3;border-radius:9px;padding:13px 16px}}
.audit-compact h3{{margin-top:0}}.flow-step{{border:1px solid var(--line);border-radius:9px;padding:5px 16px 9px;margin-top:16px}}
.plan-step{{border-left:5px solid #5277c8}}.execution-step{{border-left:5px solid #d68135}}.revision-step{{border-left:5px solid #4b9a6d}}
.flow-step h3{{margin-top:.7em}}.flow-arrow{{text-align:center;color:var(--muted);font-weight:700;padding:10px 0 0}}
@media(max-width:700px){{.metrics{{grid-template-columns:repeat(2,1fr)}}.initiatives{{columns:1}}.compact-list div{{margin-left:0}}}}
@media print{{body{{background:#fff}}main{{max-width:none;padding:0}}section{{box-shadow:none}}}}
</style></head><body><main><header><h1>{_text(company)} 戦略変化・要約版</h1>
<div class="meta">Run ID: {_text(run_id)} / 原文の冒頭を機械抽出（追加のLLM処理なし）</div></header>
{"".join(sections)}</main></body></html>"""


def generate_timeline_report(
    run_file: Path, output: Path | None = None, view: str = "detailed"
) -> Path:
    run = json.loads(run_file.read_text(encoding="utf-8"))
    if view not in {"detailed", "summary"}:
        raise ValueError("Report view must be 'detailed' or 'summary'")
    suffix = "report" if view == "detailed" else "summary"
    destination = output or run_file.with_name(f"{run_file.stem}_{suffix}.html")
    destination.parent.mkdir(parents=True, exist_ok=True)
    renderer = render_timeline_report if view == "detailed" else render_summary_report
    destination.write_text(renderer(run), encoding="utf-8")
    return destination
