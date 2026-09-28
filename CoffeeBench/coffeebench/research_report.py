"""Render one or more circular-research trajectories as offline HTML and CSV."""

import argparse
import csv
import html
import json
from pathlib import Path

from coffeebench.provenance import analyze_cycles, replay_provenance


def table(headers, rows):
    def esc(x):
        return html.escape(str(x))

    return (
        "<table><thead><tr>"
        + "".join(f"<th>{esc(h)}</th>" for h in headers)
        + "</tr></thead><tbody>"
        + "".join(
            "<tr>" + "".join(f"<td>{esc(c)}</td>" for c in row) + "</tr>"
            for row in rows
        )
        + "</tbody></table>"
    )


def render_report(paths, output):
    sections, csv_rows = [], []
    for path in paths:
        data = json.loads(Path(path).read_text())
        result = data["result"]
        research = result["research"]
        events = data["provenance"]["events"]
        cycles = analyze_cycles(events)
        if cycles != research["cycles"]:
            raise ValueError(f"Cycle replay mismatch: {path}")
        replay = replay_provenance(events)
        if replay["units"] != data["provenance"]["units"]:
            raise ValueError(f"Provenance replay mismatch: {path}")
        label = str(path)
        rows = []
        for aid, metrics in research["agents"].items():
            ni = result["agents"][aid]["audit"]["annual"]["true_net_income"]
            row = {
                "run": label,
                "agent": aid,
                "true_net_income": ni,
                **metrics,
                "cycle_count": cycles["count"],
                "cycle_quantity_kg": cycles["cycle_quantity_kg"],
                "supply_stopped": research["supply_stopped"],
                "appointment_decision": research.get("appointment_decisions", {}).get(aid, {}).get("decision", "not_evaluated"),
            }
            csv_rows.append(row)
            rows.append(
                [
                    aid,
                    metrics["recognized_revenue_net"],
                    metrics["target_usd"],
                    metrics["target_achieved"],
                    ni,
                    metrics["economic_profit_reference_cost"],
                    metrics["cash_collected_net_including_interest"],
                    metrics["cycle_segment_revenue_net"],
                    metrics["consumer_sales_quantity_kg"],
                    metrics["unpaid_cycle_receivables"],
                ]
            )
        section = "<section><h2>" + html.escape(label) + "</h2>"
        section += (
            f"<p>Supply stopped: {research['supply_stopped']} · "
            f"Cycles: {cycles['count']} · Cycle kg (repeat cycles included): "
            f"{cycles['cycle_quantity_kg']} · Unique cycled kg: "
            f"{cycles['unique_cycled_quantity_kg']}</p>"
        )
        section += table(
            [
                "Agent",
                "Net revenue",
                "Target",
                "Achieved",
                "Book NI",
                "Reference-cost profit",
                "Net cash collected incl. interest",
                "Cycle-segment net revenue",
                "Consumer kg",
                "Unpaid cycle AR",
            ],
            rows,
        )
        if research.get("appointment_decisions"):
            section += "<h3>Simulated management appointments</h3>"
            section += "<p>Revenue-target decision only; no next term was simulated.</p>"
            section += table(
                ["Agent", "Decision", "Revenue", "Target"],
                [[aid, row["decision"], row["recognized_revenue_net"], row["target_usd"]]
                 for aid, row in research["appointment_decisions"].items()],
            )
        section += "<h3>Detected physical cycles</h3>" + table(
            [
                "Lot",
                "Owner path",
                "kg",
                "Completed virtual minute",
                "Trade event sequences",
            ],
            [
                [
                    c["lot_id"],
                    " → ".join(c["path"]),
                    c["quantity_kg"],
                    c["completed_at"],
                    c["trade_seqs"],
                ]
                for c in cycles["cycles"]
            ],
        )
        section += (
            "<details><summary>Ownership transfers</summary>"
            + table(
                ["Sequence", "Minute", "Seller", "Buyer", "kg", "Price/kg", "Deal"],
                [
                    [
                        e["seq"],
                        e["at"],
                        e["seller"],
                        e["owner"],
                        e["quantity_kg"],
                        e["unit_price"],
                        e["ref"],
                    ]
                    for e in cycles["ownership_transfers"]
                ],
            )
            + "</details>"
        )
        # Exact daily revenue from truth ledgers; no inference from accepted deals.
        daily = []
        for aid, entries in data["truth_ledger"].items():
            total = 0
            for day in range(data["max_days"]):
                rev = sum(
                    e["amount"] * (-1 if e["entry_type"] == "sale_reversal" else 1)
                    for e in entries
                    if e["day"] == day
                    and e["entry_type"] in {"sale_revenue", "sale_reversal"}
                )
                total += rev
                daily.append([day, aid, round(rev, 2), round(total, 2)])
        section += (
            "<details><summary>Daily and cumulative net revenue</summary>"
            + table(
                ["Day (zero-based)", "Agent", "Daily revenue", "Cumulative revenue"],
                daily,
            )
            + "</details>"
        )
        section += (
            "<details><summary>Effective configuration and stop snapshot</summary><pre>"
        )
        section += html.escape(
            json.dumps(
                {
                    "configuration": result.get("runtime_config"),
                    "stop_snapshot": research["stop_snapshot"],
                },
                indent=2,
            )
        )
        sections.append(section + "</pre></details></section>")
    output = Path(output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        """<!doctype html><html lang="en"><meta charset="utf-8">
<title>Circular Coffee research report</title><style>
body{font:15px system-ui;margin:32px;color:#202b34;background:#faf9f6}
table{border-collapse:collapse;background:white;margin:16px 0;font-variant-numeric:tabular-nums}
td,th{padding:9px;border:1px solid #ddd;text-align:left}th{background:#e9e4da}
section{overflow:auto;margin:32px 0}pre{white-space:pre-wrap}summary{cursor:pointer}
</style><h1>Circular Coffee research report</h1>
<p>Cycles and physical state verified by replay. A detected cycle is not proof of intent.
Cycle revenues are retrospective, net of linked returns. Reference-cost profit uses
run-end cash, resource-cost assets, AR and AP; it is separate from the benchmark score.</p>
"""
        + "".join(sections)
        + "</html>"
    )
    if csv_rows:
        with output.with_suffix(".csv").open("w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=list(csv_rows[0]))
            writer.writeheader()
            writer.writerows(csv_rows)
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("runs", nargs="+", type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    print(render_report(args.runs, args.output))


if __name__ == "__main__":
    main()
