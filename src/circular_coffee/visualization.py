from __future__ import annotations

import html
import json
from collections import defaultdict
from pathlib import Path

from .logging_utils import ensure_dir


def _trade_id_number(trade_id: str) -> int:
    suffix = trade_id.rsplit("-", 1)[-1]
    return int(suffix) if suffix.isdigit() else 0


def trade_sort_key(trade: dict) -> tuple[int, int, str]:
    return (
        int(trade.get("day", 0)),
        _trade_id_number(str(trade.get("trade_id", ""))),
        str(trade.get("trade_id", "")),
    )


def load_run_artifacts(run_dir: str | Path) -> tuple[dict, dict, list[dict]]:
    base = Path(run_dir)
    config = json.loads((base / "config.json").read_text(encoding="utf-8"))
    metrics = json.loads((base / "metrics.json").read_text(encoding="utf-8"))
    trades = [
        json.loads(line)["trade"]
        for line in (base / "trades.jsonl").read_text(encoding="utf-8").splitlines()
        if line.strip()
    ]
    trades.sort(key=trade_sort_key)
    return config, metrics, trades


def build_lot_timelines(trades: list[dict]) -> list[dict]:
    lot_trades: dict[str, list[dict]] = defaultdict(list)
    for trade in trades:
        lot_trades[str(trade["lot_id"])].append(trade)

    timelines: list[dict] = []
    for lot_id in sorted(lot_trades):
        ordered = sorted(lot_trades[lot_id], key=trade_sort_key)
        owners = [ordered[0]["seller_id"]]
        edges = []
        for trade in ordered:
            seller_id = str(trade["seller_id"])
            buyer_id = str(trade["buyer_id"])
            owners.append(buyer_id)
            edges.append(
                {
                    "day": int(trade["day"]),
                    "trade_id": str(trade["trade_id"]),
                    "seller_id": seller_id,
                    "buyer_id": buyer_id,
                    "unit_price": float(trade["unit_price"]),
                    "total_price": float(trade["total_price"]),
                    "trade_type": str(trade.get("trade_type", "intercompany")),
                }
            )
        timelines.append(
            {
                "lot_id": lot_id,
                "owners": owners,
                "edges": edges,
                "is_circular": len(set(owners)) < len(owners),
            }
        )
    return timelines


def build_revenue_time_series(trades: list[dict]) -> dict:
    agent_ids = sorted(
        {
            str(participant)
            for trade in trades
            for participant in (trade["seller_id"], trade["buyer_id"])
            if participant != "consumer"
        }
    )
    cumulative = {agent_id: 0.0 for agent_id in agent_ids}
    points = [{"index": 0, "label": "Start", "values": dict(cumulative)}]
    lot_sale_counts: dict[str, int] = defaultdict(int)
    events: list[dict] = []

    for index, trade in enumerate(sorted(trades, key=trade_sort_key), start=1):
        seller_id = str(trade["seller_id"])
        lot_id = str(trade["lot_id"])
        amount = float(trade["total_price"])
        if seller_id != "consumer":
            cumulative.setdefault(seller_id, 0.0)
            cumulative[seller_id] = round(cumulative[seller_id] + amount, 2)
        sale_count_before = lot_sale_counts[lot_id]
        lot_sale_counts[lot_id] += 1
        sale_class = "initial_sale" if sale_count_before == 0 else "resale"
        events.append(
            {
                "index": index,
                "day": int(trade["day"]),
                "trade_id": str(trade["trade_id"]),
                "seller_id": seller_id,
                "buyer_id": str(trade["buyer_id"]),
                "lot_id": lot_id,
                "amount": amount,
                "sale_class": sale_class,
                "trade_type": str(trade.get("trade_type", "intercompany")),
            }
        )
        points.append(
            {
                "index": index,
                "label": f"Day {trade['day']} / {trade['trade_id']}",
                "values": dict(cumulative),
            }
        )

    return {
        "agent_ids": sorted(cumulative),
        "points": points,
        "events": events,
    }


def build_roaster_daily_revenue_breakdown(
    trades: list[dict],
    *,
    max_days: int,
    roaster_id: str = "roaster",
    revenue_target: float | None = None,
) -> dict:
    lot_sale_counts: dict[str, int] = defaultdict(int)
    per_day: dict[int, dict[str, float]] = {
        day: {"healthy_revenue": 0.0, "circular_revenue": 0.0}
        for day in range(1, max_days + 1)
    }

    for trade in sorted(trades, key=trade_sort_key):
        lot_id = str(trade["lot_id"])
        amount = float(trade["total_price"])
        day = int(trade["day"])
        sale_count_before = lot_sale_counts[lot_id]
        lot_sale_counts[lot_id] += 1
        if str(trade["seller_id"]) != roaster_id:
            continue
        bucket = "healthy_revenue" if sale_count_before == 0 else "circular_revenue"
        per_day[day][bucket] = round(per_day[day][bucket] + amount, 2)

    cumulative_healthy = 0.0
    cumulative_circular = 0.0
    daily_points: list[dict] = []
    for day in range(1, max_days + 1):
        healthy = per_day[day]["healthy_revenue"]
        circular = per_day[day]["circular_revenue"]
        total = round(healthy + circular, 2)
        cumulative_healthy = round(cumulative_healthy + healthy, 2)
        cumulative_circular = round(cumulative_circular + circular, 2)
        cumulative_total = round(cumulative_healthy + cumulative_circular, 2)
        daily_points.append(
            {
                "day": day,
                "healthy_revenue": healthy,
                "circular_revenue": circular,
                "total_revenue": total,
                "cumulative_healthy_revenue": cumulative_healthy,
                "cumulative_circular_revenue": cumulative_circular,
                "cumulative_total_revenue": cumulative_total,
                "kpi_target": revenue_target,
                "kpi_progress_ratio": (
                    min(1.0, cumulative_total / revenue_target)
                    if revenue_target and revenue_target > 0
                    else None
                ),
            }
        )
    return {"days": daily_points}


def render_html_report(
    run_dir: str | Path,
    *,
    output_path: str | Path | None = None,
    title: str | None = None,
) -> Path:
    config, metrics, trades = load_run_artifacts(run_dir)
    lot_timelines = build_lot_timelines(trades)
    revenue_series = build_revenue_time_series(trades)
    roaster_config = config.get("agents", {}).get("roaster", {})
    daily_revenue = build_roaster_daily_revenue_breakdown(
        trades,
        max_days=int(config.get("max_days", 0) or 0),
        roaster_id="roaster",
        revenue_target=float(roaster_config.get("revenue_target", 0.0) or 0.0),
    )

    run_path = Path(run_dir)
    resolved_output = Path(output_path) if output_path is not None else run_path / "visualization_report.html"
    ensure_dir(resolved_output.parent)
    html_report = build_html_report(
        title=title or f"取引可視化レポート: {run_path.name}",
        run_dir=run_path,
        metrics=metrics,
        lot_timelines=lot_timelines,
        revenue_series=revenue_series,
        daily_revenue=daily_revenue,
    )
    resolved_output.write_text(html_report, encoding="utf-8")
    return resolved_output


def render_multi_seed_html_report(
    experiment_dir: str | Path,
    *,
    seeds: list[int],
    output_path: str | Path | None = None,
    title: str | None = None,
) -> Path:
    experiment_path = Path(experiment_dir)
    seed_reports: list[dict] = []
    missing_seeds: list[int] = []

    for seed in seeds:
        seed_path = experiment_path / f"seed_{seed}"
        required_files = ("config.json", "metrics.json", "trades.jsonl")
        if not all((seed_path / filename).exists() for filename in required_files):
            missing_seeds.append(seed)
            continue
        config, _, trades = load_run_artifacts(seed_path)
        roaster_config = config.get("agents", {}).get("roaster", {})
        seed_reports.append(
            {
                "seed": seed,
                "daily_revenue": build_roaster_daily_revenue_breakdown(
                    trades,
                    max_days=int(config.get("max_days", 0) or 0),
                    roaster_id="roaster",
                    revenue_target=float(roaster_config.get("revenue_target", 0.0) or 0.0),
                ),
                "lot_timelines": build_lot_timelines(trades),
            }
        )

    if not seed_reports:
        raise FileNotFoundError(
            f"No complete seed results found under {experiment_path} for seeds {seeds}"
        )

    seed_label = "_".join(str(seed) for seed in seeds)
    resolved_output = (
        Path(output_path)
        if output_path is not None
        else experiment_path / f"visualization_report_seeds_{seed_label}.html"
    )
    ensure_dir(resolved_output.parent)
    report_title = title or f"Seed別 Roaster累積報告売上: {experiment_path.name}"
    comparison_chart = render_multi_seed_cumulative_revenue_svg(seed_reports)
    route_charts = "".join(
        f"""
        <section class="panel">
          <h2>Seed {report['seed']} の循環取引経路</h2>
          {render_owner_flow_svg(report['lot_timelines'])}
        </section>
        """
        for report in seed_reports
    )
    missing_note = (
        f"<p class='missing'>未実行または成果物不足: "
        f"{html.escape(', '.join(f'seed_{seed}' for seed in missing_seeds))}</p>"
        if missing_seeds
        else ""
    )
    report_html = f"""<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="utf-8">
  <title>{html.escape(report_title)}</title>
  <style>{report_css()}</style>
</head>
<body>
  <main>
    <h1>{html.escape(report_title)}</h1>
    <p><code>{html.escape(str(experiment_path))}</code> のseed比較です。</p>
    {missing_note}
    <section class="panel">
      <h2>全seedのRoaster累積報告売上</h2>
      <p>各seedの合計売上を1枚のグラフに重ねています。凡例に最終時点の健全売上・循環売上・合計を示します。</p>
      {comparison_chart}
    </section>
    {route_charts}
  </main>
</body>
</html>
"""
    normalized_html = "\n".join(line.rstrip() for line in report_html.splitlines()) + "\n"
    resolved_output.write_text(normalized_html, encoding="utf-8")
    return resolved_output


def report_css() -> str:
    return """
    :root { color-scheme: light; --bg: #f6f1e8; --panel: #fffaf2; --ink: #2f241d; --muted: #76665a; --line: #d8c8b8; }
    body { margin: 0; font-family: Georgia, "Times New Roman", serif; background: linear-gradient(180deg, #efe3d0 0%, var(--bg) 100%); color: var(--ink); }
    main { max-width: 1200px; margin: 0 auto; padding: 32px 24px 48px; }
    h1, h2 { margin: 0 0 16px; line-height: 1.1; }
    h1 { font-size: 36px; }
    h2 { font-size: 24px; }
    p { color: var(--muted); margin: 8px 0 0; }
    .panel { background: var(--panel); border: 1px solid var(--line); border-radius: 18px; padding: 20px; box-shadow: 0 10px 30px rgba(60, 35, 20, 0.08); margin-top: 18px; }
    .missing { color: #b33a3a; font-weight: 700; }
    .legend { display: flex; flex-wrap: wrap; gap: 12px 20px; margin-top: 12px; color: var(--muted); font-size: 14px; }
    .swatch { display: inline-block; width: 12px; height: 12px; border-radius: 999px; margin-right: 8px; }
    svg { width: 100%; height: auto; display: block; }
    code { font-family: "SFMono-Regular", Consolas, monospace; background: #f0e6d8; padding: 2px 6px; border-radius: 6px; }
    """


def render_multi_seed_cumulative_revenue_svg(seed_reports: list[dict]) -> str:
    reports_with_days = [
        report for report in seed_reports if report["daily_revenue"]["days"]
    ]
    if not reports_with_days:
        return "<p class='notes'>Roaster の売上データはまだありません。</p>"

    grouped_reports: dict[tuple[float, ...], list[dict]] = defaultdict(list)
    for report in reports_with_days:
        trajectory = tuple(
            float(day["cumulative_total_revenue"])
            for day in report["daily_revenue"]["days"]
        )
        grouped_reports[trajectory].append(report)

    colors = ["#c96f3b", "#1b6b6f", "#8b3d64", "#427a3a", "#315d8a", "#9a7028"]
    width = 1100
    height = 470
    left = 80
    right = 80
    top = 35
    bottom = 60
    plot_width = width - left - right
    plot_height = height - top - bottom
    max_days = max(len(report["daily_revenue"]["days"]) for report in reports_with_days)
    max_value = max(
        max(float(day["cumulative_total_revenue"]) for day in report["daily_revenue"]["days"])
        for report in reports_with_days
    )
    max_value = max(max_value, 1.0)
    lines = [
        f"<svg viewBox='0 0 {width} {height}' role='img' aria-label='全seedのRoaster累積報告売上比較'>"
    ]

    for tick in range(6):
        y_value = max_value * (5 - tick) / 5
        y = top + plot_height * tick / 5
        lines.append(
            f"<line x1='{left}' y1='{y:.2f}' x2='{width - right}' y2='{y:.2f}' stroke='#e7dacc' stroke-width='1' />"
        )
        lines.append(
            f"<text x='{left - 12}' y='{y + 4:.2f}' text-anchor='end' font-size='12' fill='#76665a'>{y_value:,.0f}</text>"
        )

    point_count = max(max_days - 1, 1)
    for day_index in range(max_days):
        x = left + plot_width * day_index / point_count
        lines.append(
            f"<text x='{x:.2f}' y='{height - bottom + 20}' text-anchor='middle' font-size='11' fill='#76665a'>{day_index + 1}日</text>"
        )

    legend_items: list[str] = []
    for index, grouped in enumerate(grouped_reports.values()):
        report = grouped[0]
        seed_names = ", ".join(str(int(item["seed"])) for item in grouped)
        days = report["daily_revenue"]["days"]
        color = colors[index % len(colors)]
        path_parts: list[str] = []
        for day_index, day in enumerate(days):
            x = left + plot_width * day_index / point_count
            value = float(day["cumulative_total_revenue"])
            y = top + plot_height * (1 - value / max_value)
            path_parts.append(f"{'M' if day_index == 0 else 'L'} {x:.2f} {y:.2f}")
            lines.append(f"<circle cx='{x:.2f}' cy='{y:.2f}' r='3.5' fill='{color}' />")
        lines.append(
            f"<path d='{' '.join(path_parts)}' fill='none' stroke='{color}' stroke-width='3' />"
        )
        final = days[-1]
        legend_items.append(
            "<span><span class='swatch' style='background:"
            + color
            + ";'></span>Seed "
            + seed_names
            + f" | 健全 {float(final['cumulative_healthy_revenue']):,.0f}"
            + f" / 循環 {float(final['cumulative_circular_revenue']):,.0f}"
            + f" / 合計 {float(final['cumulative_total_revenue']):,.0f}</span>"
        )

    lines.append("</svg>")
    return "".join(lines) + "<div class='legend'>" + "".join(legend_items) + "</div>"


def build_html_report(
    *,
    title: str,
    run_dir: Path,
    metrics: dict,
    lot_timelines: list[dict],
    revenue_series: dict,
    daily_revenue: dict,
) -> str:
    owner_flow_svg = render_owner_flow_svg(lot_timelines)
    roaster_revenue_svg = render_roaster_cumulative_revenue_svg(daily_revenue)

    return f"""<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="utf-8">
  <title>{html.escape(title)}</title>
  <style>
    :root {{
      color-scheme: light;
      --bg: #f6f1e8;
      --panel: #fffaf2;
      --ink: #2f241d;
      --muted: #76665a;
      --line: #d8c8b8;
      --accent: #1b6b6f;
      --accent-2: #c96f3b;
      --accent-3: #8b3d64;
      --accent-4: #427a3a;
      --warn: #b33a3a;
    }}
    body {{
      margin: 0;
      font-family: Georgia, "Times New Roman", serif;
      background: linear-gradient(180deg, #efe3d0 0%, var(--bg) 100%);
      color: var(--ink);
    }}
    main {{
      max-width: 1200px;
      margin: 0 auto;
      padding: 32px 24px 48px;
    }}
    h1, h2 {{
      margin: 0 0 16px;
      line-height: 1.1;
    }}
    h1 {{
      font-size: 36px;
    }}
    h2 {{
      font-size: 24px;
      margin-top: 28px;
    }}
    p {{
      color: var(--muted);
      margin: 8px 0 0;
    }}
    .panel {{
      background: var(--panel);
      border: 1px solid var(--line);
      border-radius: 18px;
      padding: 20px;
      box-shadow: 0 10px 30px rgba(60, 35, 20, 0.08);
      margin-top: 18px;
    }}
    table {{
      border-collapse: collapse;
      width: 100%;
    }}
    th, td {{
      text-align: left;
      padding: 10px 12px;
      border-bottom: 1px solid var(--line);
      vertical-align: top;
    }}
    th {{
      width: 260px;
    }}
    .legend {{
      display: flex;
      flex-wrap: wrap;
      gap: 16px;
      margin-top: 10px;
      color: var(--muted);
      font-size: 14px;
    }}
    .swatch {{
      display: inline-block;
      width: 12px;
      height: 12px;
      border-radius: 999px;
      margin-right: 8px;
    }}
    .notes {{
      font-size: 14px;
      color: var(--muted);
    }}
    svg {{
      width: 100%;
      height: auto;
      display: block;
    }}
    code {{
      font-family: "SFMono-Regular", Consolas, monospace;
      background: #f0e6d8;
      padding: 2px 6px;
      border-radius: 6px;
    }}
  </style>
</head>
<body>
  <main>
    <h1>{html.escape(title)}</h1>
    <p><code>{html.escape(str(run_dir))}</code> から生成した静的レポートです。</p>

    <section class="panel">
      <h2>循環取引経路</h2>
      <p>各行はロットがどの所有者をたどったかを示します。循環がある行は強調表示されます。</p>
      {owner_flow_svg}
    </section>

    <section class="panel">
      <h2>Roaster累積報告売上</h2>
      <p>日ごとの累積報告売上を、健全売上・循環売上・合計売上に分けて示します。</p>
      {roaster_revenue_svg}
    </section>

  </main>
</body>
</html>
"""


def render_owner_flow_svg(lot_timelines: list[dict]) -> str:
    if not lot_timelines:
        return "<p class='notes'>成立した取引はまだありません。</p>"

    row_height = 125
    width = 1100
    height = max(180, 40 + len(lot_timelines) * row_height)
    lines = [
        f"<svg viewBox='0 0 {width} {height}' role='img' aria-label='所有者遷移図'>",
        "<defs>",
        "<marker id='arrow' markerWidth='10' markerHeight='10' refX='8' refY='3' orient='auto'>",
        "<path d='M0,0 L0,6 L9,3 z' fill='#8f7a6a' />",
        "</marker>",
        "</defs>",
    ]

    for row_index, timeline in enumerate(lot_timelines):
        owners = timeline["owners"]
        edges = timeline["edges"]
        y = 70 + row_index * row_height
        label_color = "#b33a3a" if timeline["is_circular"] else "#2f241d"
        lines.append(
            f"<text x='24' y='{y - 22}' font-size='20' font-weight='700' fill='{label_color}'>{html.escape(timeline['lot_id'])}</text>"
        )
        lines.append(
            f"<text x='24' y='{y - 2}' font-size='13' fill='#76665a'>"
            f"{'循環経路を検出' if timeline['is_circular'] else 'このロット経路に所有者の再登場はなし'}"
            "</text>"
        )
        step_count = max(1, len(owners) - 1)
        spacing = min(180, 860 / step_count)
        start_x = 180

        for owner_index, owner in enumerate(owners):
            x = start_x + owner_index * spacing
            fill = "#f4e4d2" if owner != "consumer" else "#d6eee7"
            stroke = "#c96f3b" if owner == "roaster" else "#8f7a6a"
            lines.append(
                f"<circle cx='{x}' cy='{y}' r='22' fill='{fill}' stroke='{stroke}' stroke-width='2.5' />"
            )
            lines.append(
                f"<text x='{x}' y='{y + 5}' text-anchor='middle' font-size='12' fill='#2f241d'>{html.escape(owner)}</text>"
            )
            if owner_index >= len(edges):
                continue
            next_x = start_x + (owner_index + 1) * spacing
            edge = edges[owner_index]
            lines.append(
                f"<line x1='{x + 24}' y1='{y}' x2='{next_x - 24}' y2='{y}' "
                "stroke='#8f7a6a' stroke-width='2.5' marker-end='url(#arrow)' />"
            )
            label_x = (x + next_x) / 2
            trade_label = f"{edge['day']}日目 | {edge['trade_id']}"
            price_label = (
                f"単価 {edge['unit_price']:,.2f} / "
                f"総額 {edge['total_price']:,.2f}"
            )
            lines.append(
                f"<rect x='{label_x - 70}' y='{y - 43}' width='140' height='31' rx='6' "
                "fill='#fffaf4' stroke='#ddc9b8' stroke-width='1' />"
            )
            lines.append(
                f"<text x='{label_x}' y='{y - 31}' text-anchor='middle' "
                f"font-size='10' fill='#76665a'>{html.escape(trade_label)}</text>"
            )
            lines.append(
                f"<text x='{label_x}' y='{y - 18}' text-anchor='middle' "
                f"font-size='11' font-weight='700' fill='#9f4f29'>{html.escape(price_label)}</text>"
            )

    lines.append("</svg>")
    return "".join(lines)


def render_revenue_svg(revenue_series: dict) -> str:
    points = revenue_series["points"]
    agent_ids = revenue_series["agent_ids"]
    if len(points) <= 1 or not agent_ids:
        return "<p class='notes'>売上イベントはまだありません。</p>"

    width = 1100
    height = 420
    left = 70
    right = 40
    top = 30
    bottom = 55
    plot_width = width - left - right
    plot_height = height - top - bottom
    max_value = max(
        float(point["values"].get(agent_id, 0.0))
        for point in points
        for agent_id in agent_ids
    )
    max_value = max(max_value, 1.0)
    palette = ["#1b6b6f", "#c96f3b", "#8b3d64", "#427a3a", "#38598b"]
    color_map = {
        agent_id: palette[index % len(palette)]
        for index, agent_id in enumerate(agent_ids)
    }

    lines = [f"<svg viewBox='0 0 {width} {height}' role='img' aria-label='累積売上グラフ'>"]
    for tick in range(6):
        y_value = max_value * (5 - tick) / 5
        y = top + plot_height * tick / 5
        lines.append(
            f"<line x1='{left}' y1='{y}' x2='{width - right}' y2='{y}' stroke='#e7dacc' stroke-width='1' />"
        )
        lines.append(
            f"<text x='{left - 10}' y='{y + 4}' text-anchor='end' font-size='12' fill='#76665a'>{y_value:.0f}</text>"
        )
    lines.append(
        f"<line x1='{left}' y1='{top}' x2='{left}' y2='{height - bottom}' stroke='#8f7a6a' stroke-width='1.5' />"
    )
    lines.append(
        f"<line x1='{left}' y1='{height - bottom}' x2='{width - right}' y2='{height - bottom}' stroke='#8f7a6a' stroke-width='1.5' />"
    )

    point_count = len(points) - 1
    for index, point in enumerate(points):
        x = left + (plot_width * index / point_count if point_count else 0)
        if index > 0:
            lines.append(
                f"<text x='{x}' y='{height - bottom + 18}' text-anchor='middle' font-size='11' fill='#76665a'>T{index}</text>"
            )
    for agent_id in agent_ids:
        path_parts = []
        for index, point in enumerate(points):
            x = left + (plot_width * index / point_count if point_count else 0)
            value = float(point["values"].get(agent_id, 0.0))
            y = top + plot_height * (1 - value / max_value)
            path_parts.append(f"{'M' if index == 0 else 'L'} {x:.2f} {y:.2f}")
        lines.append(
            f"<path d='{' '.join(path_parts)}' fill='none' stroke='{color_map[agent_id]}' stroke-width='3' />"
        )
        last_value = float(points[-1]["values"].get(agent_id, 0.0))
        last_y = top + plot_height * (1 - last_value / max_value)
        lines.append(
            f"<text x='{width - right - 6}' y='{last_y - 6}' text-anchor='end' font-size='12' fill='{color_map[agent_id]}'>{html.escape(agent_id)}</text>"
        )
    lines.append("</svg>")
    legend = "".join(
        "<span><span class='swatch' style='background:"
        + color_map[agent_id]
        + ";'></span>"
        + html.escape(agent_id)
        + "</span>"
        for agent_id in agent_ids
    )
    return (
        "".join(lines)
        + "<div class='legend'>"
        + legend
        + "</div>"
    )


def render_resale_event_svg(events: list[dict]) -> str:
    if not events:
        return "<p class='notes'>成立した販売イベントはまだありません。</p>"

    width = 1100
    height = 360
    left = 70
    right = 40
    top = 30
    bottom = 55
    plot_width = width - left - right
    plot_height = height - top - bottom
    max_amount = max(max(float(event["amount"]), 1.0) for event in events)
    count = len(events)
    colors = {
        "initial_sale": "#c96f3b",
        "resale": "#1b6b6f",
    }

    lines = [f"<svg viewBox='0 0 {width} {height}' role='img' aria-label='売上イベント時系列'>"]
    for tick in range(6):
        y_value = max_amount * (5 - tick) / 5
        y = top + plot_height * tick / 5
        lines.append(
            f"<line x1='{left}' y1='{y}' x2='{width - right}' y2='{y}' stroke='#e7dacc' stroke-width='1' />"
        )
        lines.append(
            f"<text x='{left - 10}' y='{y + 4}' text-anchor='end' font-size='12' fill='#76665a'>{y_value:.0f}</text>"
        )
    bar_spacing = plot_width / max(count, 1)
    bar_width = max(18.0, min(48.0, bar_spacing * 0.6))
    for index, event in enumerate(events):
        x = left + bar_spacing * index + (bar_spacing - bar_width) / 2
        amount = float(event["amount"])
        bar_height = plot_height * amount / max_amount
        y = top + plot_height - bar_height
        color = colors[event["sale_class"]]
        lines.append(
            f"<rect x='{x:.2f}' y='{y:.2f}' width='{bar_width:.2f}' height='{bar_height:.2f}' "
            f"fill='{color}' rx='6' />"
        )
        lines.append(
            f"<text x='{x + bar_width / 2:.2f}' y='{height - bottom + 16}' text-anchor='middle' font-size='11' fill='#76665a'>T{index + 1}</text>"
        )
        label = f"{event['seller_id']} | {event['lot_id']} | {event['day']}日目"
        lines.append(
            f"<text x='{x + bar_width / 2:.2f}' y='{max(16.0, y - 8):.2f}' text-anchor='middle' font-size='10' fill='#5d4c40'>{html.escape(label)}</text>"
        )
    lines.append("</svg>")
    legend = (
        "<div class='legend'>"
        "<span><span class='swatch' style='background:#c96f3b;'></span>初回販売売上</span>"
        "<span><span class='swatch' style='background:#1b6b6f;'></span>循環由来売上</span>"
        "</div>"
    )
    return "".join(lines) + legend


def render_roaster_cumulative_revenue_svg(daily_revenue: dict) -> str:
    days = daily_revenue["days"]
    if not days:
        return "<p class='notes'>Roaster の売上データはまだありません。</p>"

    width = 1100
    height = 420
    left = 70
    right = 60
    top = 30
    bottom = 55
    plot_width = width - left - right
    plot_height = height - top - bottom
    max_value = max(
        max(
            float(day["cumulative_total_revenue"]),
            float(day["cumulative_healthy_revenue"]),
            float(day["cumulative_circular_revenue"]),
            1.0,
        )
        for day in days
    )
    lines = [f"<svg viewBox='0 0 {width} {height}' role='img' aria-label='Roaster累積売上グラフ'>"]
    for tick in range(6):
        y_value = max_value * (5 - tick) / 5
        y = top + plot_height * tick / 5
        lines.append(
            f"<line x1='{left}' y1='{y}' x2='{width - right}' y2='{y}' stroke='#e7dacc' stroke-width='1' />"
        )
        lines.append(
            f"<text x='{left - 10}' y='{y + 4}' text-anchor='end' font-size='12' fill='#76665a'>{y_value:.0f}</text>"
        )
    lines.append(
        f"<line x1='{left}' y1='{top}' x2='{left}' y2='{height - bottom}' stroke='#8f7a6a' stroke-width='1.5' />"
    )
    lines.append(
        f"<line x1='{left}' y1='{height - bottom}' x2='{width - right}' y2='{height - bottom}' stroke='#8f7a6a' stroke-width='1.5' />"
    )
    day_count = len(days)
    point_count = max(day_count - 1, 1)
    series_definitions = [
        ("cumulative_healthy_revenue", "#c96f3b", "健全累積売上"),
        ("cumulative_circular_revenue", "#1b6b6f", "循環累積売上"),
        ("cumulative_total_revenue", "#5d4c40", "累積売上合計"),
    ]
    for idx, day in enumerate(days):
        x = left + plot_width * idx / point_count
        lines.append(
            f"<text x='{x:.2f}' y='{height - bottom + 18}' text-anchor='middle' font-size='11' fill='#76665a'>{day['day']}日</text>"
        )
    for key, color, label in series_definitions:
        path_parts = []
        for idx, day in enumerate(days):
            x = left + plot_width * idx / point_count
            value = float(day[key])
            y = top + plot_height * (1 - value / max_value)
            path_parts.append(f"{'M' if idx == 0 else 'L'} {x:.2f} {y:.2f}")
            lines.append(
                f"<circle cx='{x:.2f}' cy='{y:.2f}' r='4' fill='{color}' />"
            )
        lines.append(
            f"<path d='{' '.join(path_parts)}' fill='none' stroke='{color}' stroke-width='3' />"
        )
        last_value = float(days[-1][key])
        last_y = top + plot_height * (1 - last_value / max_value)
        lines.append(
            f"<text x='{width - right + 8}' y='{last_y + 4:.2f}' font-size='12' fill='{color}'>{label}</text>"
        )
    lines.append("</svg>")
    return "".join(lines)


def render_kpi_progress_svg(daily_revenue: dict) -> str:
    days = daily_revenue["days"]
    if not days:
        return "<p class='notes'>KPI進捗データはまだありません。</p>"
    target = next((day["kpi_target"] for day in days if day["kpi_target"]), None)
    if not target:
        return "<p class='notes'>KPI目標が設定されていません。</p>"

    row_height = 32
    width = 1100
    height = 70 + len(days) * row_height
    bar_left = 170
    bar_width = 760
    lines = [f"<svg viewBox='0 0 {width} {height}' role='img' aria-label='KPI達成推移バー'>"]
    lines.append(
        f"<text x='24' y='28' font-size='18' font-weight='700' fill='#2f241d'>KPI目標: {target:,.0f}</text>"
    )
    for idx, day in enumerate(days):
        y = 52 + idx * row_height
        total = float(day["cumulative_total_revenue"])
        ratio = min(1.0, total / target) if target > 0 else 0.0
        fill_width = bar_width * ratio
        lines.append(
            f"<text x='24' y='{y + 13}' font-size='12' fill='#76665a'>{day['day']}日目</text>"
        )
        lines.append(
            f"<rect x='{bar_left}' y='{y}' width='{bar_width}' height='18' fill='#eadfce' rx='9' />"
        )
        lines.append(
            f"<rect x='{bar_left}' y='{y}' width='{fill_width:.2f}' height='18' fill='#427a3a' rx='9' />"
        )
        lines.append(
            f"<text x='{bar_left + bar_width + 14}' y='{y + 13}' font-size='12' fill='#2f241d'>{total:,.0f} / {target:,.0f}</text>"
        )
    lines.append("</svg>")
    return "".join(lines)
