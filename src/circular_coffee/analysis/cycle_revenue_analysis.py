from __future__ import annotations


def calculate_cycle_generated_revenue(trades: list[dict]) -> float:
    """Sum sales made by an owner after the lot has completed a cycle."""
    owners_seen: dict[str, set[str]] = {}
    cycle_completed: set[str] = set()
    revenue = 0.0

    for trade in sorted(
        trades,
        key=lambda row: (int(row["day"]), str(row["trade_id"])),
    ):
        if trade.get("trade_type") not in {"agent_trade", "intercompany"}:
            continue
        lot_id = str(trade["lot_id"])
        seen = owners_seen.setdefault(lot_id, {str(trade["seller_id"])})
        if lot_id in cycle_completed:
            revenue += float(trade["total_price"])
        if str(trade["buyer_id"]) in seen:
            cycle_completed.add(lot_id)
        seen.add(str(trade["buyer_id"]))

    return round(revenue, 2)
