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
    revenue_target_enabled: bool = True
    inventory: dict[str, CoffeeLot] = field(default_factory=dict)


@dataclass
class TradeProposal:
    proposal_id: str
    initiator_id: str
    seller_id: str
    buyer_id: str
    lot_id: str
    quantity: int
    unit_price: float
    proposal_message: str | None
    status: Literal["pending", "accepted", "rejected", "expired", "cancelled"]
    created_day: int
    decision_day: int | None = None
    closed_day: int | None = None
    close_reason: str | None = None


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
    initiator_id: str | None = None
    proposal_id: str | None = None
    trade_type: Literal["agent_trade", "intercompany", "consumer"] = "agent_trade"
    original_unit_cost: float | None = None
    final_consumption: bool = False


@dataclass
class AgentAction:
    action_type: Literal[
        "propose_trade",
        "accept_trade",
        "reject_trade",
        "accept_counteroffer",
        "reject_counteroffer",
        "counteroffer_trade",
        "sell_to_consumer",
        "wait",
    ]
    seller_id: str | None = None
    buyer_id: str | None = None
    proposal_id: str | None = None
    counteroffer_id: str | None = None
    lot_id: str | None = None
    quantity: int | None = None
    unit_price: float | None = None
    proposal_message: str | None = None
    reason_summary: str | None = None


@dataclass
class CommunicationAction:
    action_type: Literal["send_message", "no_message"]
    recipient_id: str | None = None
    message: str | None = None
    related_lot_id: str | None = None
    related_proposal_id: str | None = None
    reason_summary: str | None = None


@dataclass
class TradeCounteroffer:
    counteroffer_id: str
    proposal_id: str
    initiator_id: str
    responder_id: str
    seller_id: str
    buyer_id: str
    lot_id: str
    quantity: int
    unit_price: float
    status: Literal[
        "pending",
        "accepted",
        "rejected",
        "expired",
        "superseded",
        "cancelled",
        "invalidated",
    ]
    created_day: int
    decision_day: int | None = None
    close_reason: str | None = None


@dataclass(frozen=True)
class MessageRecord:
    message_id: str
    day: int
    sender_id: str
    recipient_id: str
    message: str
    related_proposal_id: str | None = None
    related_lot_id: str | None = None


@dataclass
class MarketState:
    day: int
    max_days: int
    agents: dict[str, AgentState]
    pending_proposals: dict[str, TradeProposal]
    trade_history: list[TradeRecord]
    pending_trade_counteroffers: dict[str, TradeCounteroffer] = field(default_factory=dict)
    consumer_market: ConsumerMarketState | None = None
    messages: list[MessageRecord] = field(default_factory=list)

    @property
    def active_proposals(self) -> dict[str, TradeProposal]:
        return {
            proposal_id: proposal
            for proposal_id, proposal in self.pending_proposals.items()
            if proposal.status == "pending"
        }

    @property
    def proposal_history(self) -> dict[str, TradeProposal]:
        return {
            proposal_id: proposal
            for proposal_id, proposal in self.pending_proposals.items()
            if proposal.status != "pending"
        }


@dataclass
class ConsumerMarketState:
    daily_capacity: int
    remaining_capacity_by_day: dict[int, int] = field(default_factory=dict)
    consumed_lot_ids: list[str] = field(default_factory=list)


@dataclass(frozen=True)
class ConsumerSaleResult:
    status: Literal["accepted", "rejected"]
    seller_id: str
    buyer_id: str
    lot_id: str
    quantity_sold: int
    unit_price: float
    total_revenue: float
    remaining_daily_demand: int
    reason: str
    trade: TradeRecord | None = None
