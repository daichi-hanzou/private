from __future__ import annotations

import html
import json
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


def _failure_section(outcome: dict) -> str:
    initiative_results = []
    for item in outcome.get("initiative_outcomes", []):
        initiative_results.append(
            '<article class="card failure">'
            f'<h4><span class="badge bad">{_text(item.get("status"))}</span>'
            f'{_text(item.get("initiative_name", "名称なし"))}</h4>'
            f'{_paragraph(item.get("simulated_result"))}'
            "</article>"
        )
    reasons = []
    for item in outcome.get("failure_reasons", []):
        reasons.append(
            '<article class="card reason">'
            f'<h4>{_text(item.get("category", item.get("failure_reason_id", "失敗理由")))}</h4>'
            '<dl>'
            f'<dt>近因</dt><dd class="source-text">{_text(item.get("proximate_cause"))}</dd>'
            f'<dt>根本原因</dt><dd class="source-text">{_text(item.get("root_cause"))}</dd>'
            f'<dt>財務的影響</dt><dd class="source-text">{_text(item.get("financial_effect"))}</dd>'
            '</dl></article>'
        )
    return (
        '<h3>施策がどのようにだめだったか</h3>'
        + "".join(initiative_results)
        + '<h3>失敗理由</h3>'
        + "".join(reasons)
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
        '<h3>ビジネスモデル</h3>'
        + _paragraph(initial.get("business_model_summary"))
        + '<h3>財務計画</h3>'
        + _paragraph(initial.get("financial_summary"))
        + '<h3>成長ドライバー</h3>'
        + _list(initial.get("key_growth_drivers"))
        + '<h3>初期施策</h3>'
        + _initiative_cards(initial)
        + '</section>'
    ]

    current_plan = initial
    for index, item in enumerate(rounds, start=1):
        outcome = item.get("reality_outcome", {})
        year = outcome.get("simulation_year", f"Round {index}")
        feedback = item.get("ceo_feedback", {})
        revised = item.get("revised_plan", {})
        sections.append(
            '<section class="round">'
            f'<div class="year-label">FY{_text(year)}</div>'
            f'<h2>FY{_text(year)}：実行、失敗、CEO指摘、戦略改訂</h2>'
            '<details><summary>この年度に実行された戦略</summary>'
            + _initiative_cards(current_plan)
            + '</details>'
            + _failure_section(outcome)
            + _ceo_section(feedback)
            + '<h3>CEO指摘後の改訂戦略</h3>'
            + _paragraph(revised.get("business_model_summary"))
            + _initiative_cards(revised)
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
h4 {{ margin:.1em 0 .75em; }} .card {{ border:1px solid var(--line); border-radius:10px; padding:18px; margin:12px 0; }}
.failure {{ border-left:5px solid #d68135; }} .reason {{ background:#fffaf4; }}
.ceo {{ margin-top:24px; padding:4px 22px 22px; background:#f3f6ff; border-radius:12px; }}
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


def generate_timeline_report(run_file: Path, output: Path | None = None) -> Path:
    run = json.loads(run_file.read_text(encoding="utf-8"))
    destination = output or run_file.with_name(f"{run_file.stem}_report.html")
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(render_timeline_report(run), encoding="utf-8")
    return destination
