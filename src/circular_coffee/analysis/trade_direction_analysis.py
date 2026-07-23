from __future__ import annotations

from collections import Counter


def count_trade_directions(
    trades: list[dict],
    agents: dict,
) -> dict[str, int]:
    """Count agent trades by seller and buyer role."""
    counts: Counter[str] = Counter()
    for trade in trades:
        if trade.get("trade_type") not in {"agent_trade", "intercompany"}:
            continue
        seller_role = agents[trade["seller_id"]]["role"]
        buyer_role = agents[trade["buyer_id"]]["role"]
        counts[f"{seller_role}_to_{buyer_role}"] += 1
    return dict(sorted(counts.items()))
