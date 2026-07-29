from __future__ import annotations


def agent_score(agent_metrics: dict) -> float:
    """Return the displayed score from authoritative agent metrics."""
    return round(float(agent_metrics["economic_profit"]), 2)
