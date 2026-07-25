from __future__ import annotations

from typing import Any

from agentledger.normalizer import AuditEventNormalizer, humanize


def _agent_name(agent_id: str) -> str:
    return {
        "roaster": "Roaster",
        "retailer_a": "Retailer A",
        "retailer_b": "Retailer B",
    }.get(agent_id, humanize(agent_id))


def _trade_summary(raw: dict[str, Any]) -> str:
    actor = _agent_name(raw.get("agent_id") or "agent")
    target = _agent_name(raw.get("counterparty") or "counterparty")
    lot = raw.get("lot_id") or "unspecified inventory"
    quantity = raw.get("quantity")
    price = raw.get("unit_price")
    details = [str(lot)]
    if quantity is not None:
        details.append(f"quantity {quantity}")
    if price is not None:
        details.append(f"unit price {float(price):.2f}")
    return f"{actor} proposed to {target}: {', '.join(details)}"


def _counteroffer_summary(raw: dict[str, Any]) -> str:
    actor = _agent_name(raw.get("agent_id") or "agent")
    proposal = raw.get("proposal_id") or "the case"
    price = raw.get("unit_price")
    suffix = f" at unit price {float(price):.2f}" if price is not None else ""
    return f"{actor} counteroffered for {proposal}{suffix}"


class CoffeeBenchAuditAdapter(AuditEventNormalizer):
    def __init__(self) -> None:
        super().__init__(
            agent_name=_agent_name,
            action_formatters={
                "propose_trade": _trade_summary,
                "counteroffer_trade": _counteroffer_summary,
            },
        )
