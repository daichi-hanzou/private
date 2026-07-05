from __future__ import annotations

import argparse
import json
from html import escape
from pathlib import Path


def load_events(path: Path) -> list[dict]:
    return [json.loads(line) for line in path.read_text().splitlines() if line.strip()]


def latest_log(log_dir: Path) -> Path:
    candidates = sorted(log_dir.glob("mini_coffee_*.jsonl"), key=lambda p: p.stat().st_mtime)
    if not candidates:
        raise FileNotFoundError(f"No JSONL logs found in {log_dir}")
    return candidates[-1]


def svg_line_chart(title: str, labels: list[str], series: list[tuple[str, list[float], str]]) -> str:
    width, height = 560, 240
    left, right, top, bottom = 48, 18, 24, 32
    plot_w = width - left - right
    plot_h = height - top - bottom
    values = [value for _, data, _ in series for value in data]
    vmin = min(values) if values else 0.0
    vmax = max(values) if values else 1.0
    if vmin == vmax:
        vmin -= 1.0
        vmax += 1.0

    def x_pos(idx: int) -> float:
        if len(labels) <= 1:
            return left + plot_w / 2
        return left + (plot_w * idx / (len(labels) - 1))

    def y_pos(value: float) -> float:
        return top + plot_h - ((value - vmin) / (vmax - vmin) * plot_h)

    lines = [
        f'<svg viewBox="0 0 {width} {height}" class="chart">',
        f'<text x="{left}" y="16" class="chart-title">{escape(title)}</text>',
        f'<line x1="{left}" y1="{top}" x2="{left}" y2="{top + plot_h}" class="axis" />',
        f'<line x1="{left}" y1="{top + plot_h}" x2="{left + plot_w}" y2="{top + plot_h}" class="axis" />',
    ]

    for tick in range(5):
        ratio = tick / 4
        value = vmax - (vmax - vmin) * ratio
        y = top + plot_h * ratio
        lines.append(f'<line x1="{left}" y1="{y:.1f}" x2="{left + plot_w}" y2="{y:.1f}" class="grid" />')
        lines.append(f'<text x="{left - 8}" y="{y + 4:.1f}" class="tick tick-left">${value:.0f}</text>')

    for idx, label in enumerate(labels):
        x = x_pos(idx)
        lines.append(f'<text x="{x:.1f}" y="{height - 8}" class="tick tick-center">{escape(label)}</text>')

    legend_x = left
    for name, data, color in series:
        points = " ".join(f"{x_pos(i):.1f},{y_pos(v):.1f}" for i, v in enumerate(data))
        lines.append(f'<polyline fill="none" stroke="{color}" stroke-width="3" points="{points}" />')
        lines.append(f'<rect x="{legend_x}" y="{height - 24}" width="10" height="10" fill="{color}" rx="2" />')
        lines.append(f'<text x="{legend_x + 16}" y="{height - 15}" class="legend">{escape(name)}</text>')
        legend_x += 132

    lines.append("</svg>")
    return "\n".join(lines)


def svg_bar_chart(title: str, labels: list[str], series: list[tuple[str, list[float], str]]) -> str:
    width, height = 560, 240
    left, right, top, bottom = 48, 18, 24, 32
    plot_w = width - left - right
    plot_h = height - top - bottom
    values = [value for _, data, _ in series for value in data]
    vmax = max(values) if values else 1.0
    vmax = max(1.0, vmax)
    group_width = plot_w / max(1, len(labels))
    bar_width = group_width / max(1, len(series) + 0.5)

    lines = [
        f'<svg viewBox="0 0 {width} {height}" class="chart">',
        f'<text x="{left}" y="16" class="chart-title">{escape(title)}</text>',
        f'<line x1="{left}" y1="{top}" x2="{left}" y2="{top + plot_h}" class="axis" />',
        f'<line x1="{left}" y1="{top + plot_h}" x2="{left + plot_w}" y2="{top + plot_h}" class="axis" />',
    ]

    for tick in range(5):
        ratio = tick / 4
        value = vmax - vmax * ratio
        y = top + plot_h * ratio
        lines.append(f'<line x1="{left}" y1="{y:.1f}" x2="{left + plot_w}" y2="{y:.1f}" class="grid" />')
        lines.append(f'<text x="{left - 8}" y="{y + 4:.1f}" class="tick tick-left">${value:.0f}</text>')

    for idx, label in enumerate(labels):
        base_x = left + idx * group_width
        lines.append(f'<text x="{base_x + group_width / 2:.1f}" y="{height - 8}" class="tick tick-center">{escape(label)}</text>')
        for offset, (name, data, color) in enumerate(series):
            value = data[idx]
            bar_h = plot_h * (value / vmax)
            x = base_x + 8 + offset * bar_width
            y = top + plot_h - bar_h
            lines.append(
                f'<rect x="{x:.1f}" y="{y:.1f}" width="{bar_width - 8:.1f}" height="{bar_h:.1f}" fill="{color}" rx="4" />'
            )

    legend_x = left
    for name, _, color in series:
        lines.append(f'<rect x="{legend_x}" y="{height - 24}" width="10" height="10" fill="{color}" rx="2" />')
        lines.append(f'<text x="{legend_x + 16}" y="{height - 15}" class="legend">{escape(name)}</text>')
        legend_x += 132

    lines.append("</svg>")
    return "\n".join(lines)


def build_dashboard(events: list[dict], source_path: Path) -> str:
    start = next(event for event in events if event["type"] == "run_start")
    days = [event for event in events if event["type"] == "day_end"]
    end = next((event for event in events if event["type"] == "run_end"), None)

    labels = ["Start"] + [f"D{event['day']}" for event in days]
    cash_series = [start["snapshot"]["cash"]] + [event["snapshot"]["cash"] for event in days]
    terminal_series = [start["snapshot"]["estimated_terminal_value"]] + [
        event["snapshot"]["estimated_terminal_value"] for event in days
    ]
    standard_inventory = [start["snapshot"]["inventory"]["standard"]] + [
        event["snapshot"]["inventory"]["standard"] for event in days
    ]
    premium_inventory = [start["snapshot"]["inventory"]["premium"]] + [
        event["snapshot"]["inventory"]["premium"] for event in days
    ]
    revenue = [event["revenue"] for event in days]
    profit = [event["profit"] for event in days]
    holding = [event["holding_cost"] for event in days]
    investigate_spend = [start["snapshot"]["metrics"]["investigation_spend"]] + [
        event["snapshot"]["metrics"]["investigation_spend"] for event in days
    ]
    direct_spend = [start["snapshot"]["metrics"]["direct_spend"]] + [
        event["snapshot"]["metrics"]["direct_spend"] for event in days
    ]
    trader_spend = [start["snapshot"]["metrics"]["trader_spend"]] + [
        event["snapshot"]["metrics"]["trader_spend"] for event in days
    ]
    sold_standard = [event["sold_units"]["standard"] for event in days]
    sold_premium = [event["sold_units"]["premium"] for event in days]

    summary_rows = []
    if end:
        summary_rows.extend(
            [
                ("Final value", f"${end['final_value']:.2f}"),
                ("Profit", f"${end['profit']:.2f}"),
                ("Reward", f"{end['reward']:.4f}"),
                ("Log file", str(source_path)),
            ]
        )
    trader_value = end.get("trader_value_analysis", {}) if end else {}

    event_rows = []
    for event in days:
        details = "<br>".join(escape(text) for text in event["events"]) if event["events"] else "No contract events"
        event_rows.append(
            f"<tr><td>Day {event['day']}</td><td>${event['revenue']:.2f}</td><td>${event['profit']:.2f}</td><td>{details}</td></tr>"
        )

    cards = "".join(
        f'<div class="card"><div class="metric-label">{escape(label)}</div><div class="metric-value">{escape(value)}</div></div>'
        for label, value in summary_rows
    )

    charts = [
        svg_line_chart(
            "Cash And Estimated Terminal Value",
            labels,
            [
                ("Cash", cash_series, "#0f766e"),
                ("Est. terminal value", terminal_series, "#d97706"),
            ],
        ),
        svg_line_chart(
            "Retail Inventory By Item",
            labels,
            [
                ("Standard", standard_inventory, "#2563eb"),
                ("Premium", premium_inventory, "#dc2626"),
            ],
        ),
        svg_bar_chart(
            "Daily Revenue, Profit, Holding Cost",
            [f"D{event['day']}" for event in days],
            [
                ("Revenue", revenue, "#16a34a"),
                ("Profit", profit, "#0f766e"),
                ("Holding", holding, "#9ca3af"),
            ],
        ),
        svg_line_chart(
            "Cumulative Spend By Channel",
            labels,
            [
                ("Investigate", investigate_spend, "#7c3aed"),
                ("Direct", direct_spend, "#2563eb"),
                ("Trader", trader_spend, "#ea580c"),
            ],
        ),
        svg_bar_chart(
            "Units Sold By Day",
            [f"D{event['day']}" for event in days],
            [
                ("Standard", sold_standard, "#2563eb"),
                ("Premium", sold_premium, "#dc2626"),
            ],
        ),
    ]

    trader_rows = []
    if trader_value:
        trader_rows = [
            ("Direct fill rate", f"{trader_value['direct_fill_rate']:.2%}"),
            ("Direct shortfall qty", f"{trader_value['direct_shortfall_qty']:.0f} kg"),
            ("Direct late qty", f"{trader_value['direct_late_qty']:.0f} kg"),
            ("Direct refund value", f"${trader_value['direct_refund_value']:.2f}"),
            ("Trader guaranteed qty", f"{trader_value['trader_guaranteed_qty']:.0f} kg"),
            ("Trader purchase share", f"{trader_value['trader_purchase_share']:.2%}"),
            ("Risk transferred to trader (proxy)", f"{trader_value['risk_transfer_proxy_qty']:.0f} kg"),
            ("Potential stockout buffer (proxy)", f"{trader_value['avoided_stockout_proxy_qty']:.0f} kg"),
        ]

    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Mini Coffee Charts</title>
  <style>
    :root {{
      --bg: #f7f3ea;
      --panel: #fffdf8;
      --ink: #1f2937;
      --muted: #6b7280;
      --border: #d6d3d1;
      --grid: #e7e5e4;
    }}
    body {{
      margin: 0;
      font-family: Georgia, "Times New Roman", serif;
      background: radial-gradient(circle at top left, #efe7d3, var(--bg) 42%);
      color: var(--ink);
    }}
    .page {{
      max-width: 1180px;
      margin: 0 auto;
      padding: 28px 20px 56px;
    }}
    h1 {{
      margin: 0 0 8px;
      font-size: 36px;
    }}
    .sub {{
      color: var(--muted);
      margin-bottom: 20px;
    }}
    .cards {{
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 12px;
      margin-bottom: 18px;
    }}
    .card, .panel {{
      background: var(--panel);
      border: 1px solid var(--border);
      border-radius: 16px;
      box-shadow: 0 6px 20px rgba(0, 0, 0, 0.05);
    }}
    .card {{
      padding: 14px 16px;
    }}
    .metric-label {{
      color: var(--muted);
      font-size: 13px;
      text-transform: uppercase;
      letter-spacing: 0.08em;
    }}
    .metric-value {{
      margin-top: 8px;
      font-size: 28px;
    }}
    .chart-grid {{
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
      gap: 14px;
    }}
    .panel {{
      padding: 10px 10px 14px;
    }}
    .chart {{
      width: 100%;
      height: auto;
    }}
    .chart-title {{
      font-size: 14px;
      font-weight: 700;
      fill: var(--ink);
    }}
    .axis {{
      stroke: #78716c;
      stroke-width: 1;
    }}
    .grid {{
      stroke: var(--grid);
      stroke-width: 1;
    }}
    .tick {{
      font-size: 11px;
      fill: var(--muted);
    }}
    .tick-left {{ text-anchor: end; }}
    .tick-center {{ text-anchor: middle; }}
    .legend {{
      font-size: 12px;
      fill: var(--muted);
    }}
    table {{
      width: 100%;
      border-collapse: collapse;
      font-size: 14px;
    }}
    th, td {{
      border-top: 1px solid var(--border);
      padding: 10px 8px;
      text-align: left;
      vertical-align: top;
    }}
    th {{
      color: var(--muted);
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 0.06em;
    }}
  </style>
</head>
<body>
  <div class="page">
    <h1>Mini Coffee Run Dashboard</h1>
    <div class="sub">Static replay from JSONL log. No live watcher required.</div>
    <div class="cards">{cards}</div>
    <div class="chart-grid">
      {''.join(f'<div class="panel">{chart}</div>' for chart in charts)}
    </div>
    <div class="panel" style="margin-top: 14px; padding: 16px;">
      <h2 style="margin: 0 0 12px;">Trader Value Analysis</h2>
      <table>
        <tbody>
          {''.join(f"<tr><th>{escape(label)}</th><td>{escape(value)}</td></tr>" for label, value in trader_rows)}
        </tbody>
      </table>
    </div>
    <div class="panel" style="margin-top: 14px; padding: 16px;">
      <table>
        <thead>
          <tr><th>Day</th><th>Revenue</th><th>Profit</th><th>Contract Events</th></tr>
        </thead>
        <tbody>
          {''.join(event_rows)}
        </tbody>
      </table>
    </div>
  </div>
</body>
</html>
"""


def main() -> None:
    default_log_dir = Path(__file__).resolve().parent / "workspace" / "output" / "mini_coffee_runs"
    parser = argparse.ArgumentParser()
    parser.add_argument("--log", type=Path, help="Path to one JSONL run log.")
    parser.add_argument("--latest", action="store_true", help="Render the newest JSONL log in the default log directory.")
    parser.add_argument("--output", type=Path, help="Destination HTML path.")
    args = parser.parse_args()

    log_path = args.log or latest_log(default_log_dir)
    events = load_events(log_path)
    html = build_dashboard(events, log_path)
    output_path = args.output or log_path.with_suffix(".html")
    output_path.write_text(html)
    print(output_path)


if __name__ == "__main__":
    main()
