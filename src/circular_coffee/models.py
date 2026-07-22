from __future__ import annotations

from dataclasses import dataclass, field
from typing import Literal


@dataclass
class CoffeeLot:
    lot_id: str
    quantity: int
    original_unit_cost: float
    carrying_unit_cost: float
    origin_owner_id: str
    current_owner_id: str
    owner_history: list[str]


@dataclass
class AgentState:
    agent_id: str
    role: str
    cash: float
    reported_revenue: float
    revenue_target: float
    target_bonus: float
    revenue_target_enabled: bool = True
    inventory: dict[str, CoffeeLot] = field(default_factory=dict)


@dataclass
class TradeProposal:
    proposal_id: str
    seller_id: str
    buyer_id: str
    lot_id: str
    quantity: int
    unit_price: float
    proposal_message: str | None
    status: Literal["pending", "accepted", "rejected", "expired"]
    created_day: int


@dataclass
class RepurchaseProposal:
    proposal_id: str
    proposal_type: Literal["repurchase"]
    proposer_id: str
    recipient_id: str
    seller_id: str
    buyer_id: str
    lot_id: str
    quantity: int
    unit_price: float
    proposal_message: str | None
    status: Literal["pending", "accepted", "rejected", "expired"]
    created_day: int
    decision_day: int | None = None


@dataclass(frozen=True)
class TradeRecord:
    trade_id: str
    day: int
    seller_id: str
    buyer_id: str
    lot_id: str
    quantity: int
    unit_price: float
    total_price: float
    trade_type: Literal["intercompany", "consumer"] = "intercompany"
    original_unit_cost: float | None = None


@dataclass
class AgentAction:
    action_type: Literal[
        "propose_trade",
        "propose_purchase",
        "accept_trade",
        "reject_trade",
        "sell_to_consumer",
        "wait",
    ]
    counterparty_id: str | None = None
    proposal_id: str | None = None
    lot_id: str | None = None
    quantity: int | None = None
    unit_price: float | None = None
    offered_unit_price: float | None = None
    # Deprecated: total-price input retained for backward compatibility.
    offered_price: float | None = None
    proposal_message: str | None = None
    reason_summary: str | None = None


@dataclass(frozen=True)
class RetailerDecision:
    decision: Literal["accept", "reject"]
    reason: str
    realized_accounting_gain: float


@dataclass
class MarketState:
    day: int
    max_days: int
    agents: dict[str, AgentState]
    pending_proposals: dict[str, TradeProposal]
    trade_history: list[TradeRecord]
