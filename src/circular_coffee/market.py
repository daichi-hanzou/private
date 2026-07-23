from __future__ import annotations

import math
from dataclasses import asdict
from typing import TYPE_CHECKING

from .models import (
    AgentAction,
    CommunicationAction,
    ConsumerMarketState,
    ConsumerSaleResult,
    MarketState,
    MessageRecord,
    TradeCounteroffer,
    TradeProposal,
    TradeRecord,
)

if TYPE_CHECKING:
    from .config import SimulationConfig


class InvalidActionError(ValueError):
    pass


def validate_communication_action(
    state: MarketState,
    config: SimulationConfig,
    *,
    sender_id: str,
    action: CommunicationAction,
) -> None:
    sender = _get_agent(state, sender_id)
    if action.action_type == "no_message":
        return
    if action.action_type != "send_message":
        raise InvalidActionError("unsupported_communication_action")
    if not config.communication_enabled:
        raise InvalidActionError("communication_disabled")
    if action.recipient_id is None:
        raise InvalidActionError("missing_message_recipient")
    recipient = _get_agent(state, action.recipient_id)
    if sender_id == action.recipient_id:
        raise InvalidActionError("cannot_send_message_to_self")
    channel = f"{sender.role}_to_{recipient.role}"
    if not config.message_channels.get(channel, False):
        raise InvalidActionError("message_channel_not_allowed")
    text = (action.message or "").strip()
    if not text:
        raise InvalidActionError("message_cannot_be_empty")
    if len(text) > config.max_message_length:
        raise InvalidActionError("message_exceeds_maximum_length")
    sent_today = sum(
        1
        for item in state.messages
        if item.sender_id == sender_id and item.day == state.day
    )
    if sent_today >= config.max_messages_per_agent_per_day:
        raise InvalidActionError("daily_message_limit_reached")
    if (
        action.related_lot_id is not None
        and action.related_lot_id not in config.lot_ids
    ):
        raise InvalidActionError("unknown_related_lot")
    if (
        action.related_proposal_id is not None
        and action.related_proposal_id not in state.pending_proposals
    ):
        raise InvalidActionError("unknown_related_proposal")


def commit_messages(
    state: MarketState,
    config: SimulationConfig,
    decisions: list[tuple[str, CommunicationAction]],
) -> list[MessageRecord]:
    records: list[MessageRecord] = []
    for sender_id, action in sorted(decisions, key=lambda item: item[0]):
        validate_communication_action(
            state,
            config,
            sender_id=sender_id,
            action=action,
        )
        if action.action_type == "no_message":
            continue
        record = MessageRecord(
            message_id=f"message-{len(state.messages) + 1}",
            day=state.day,
            sender_id=sender_id,
            recipient_id=action.recipient_id or "",
            message=(action.message or "").strip(),
            related_proposal_id=action.related_proposal_id,
            related_lot_id=action.related_lot_id,
        )
        state.messages.append(record)
        records.append(record)
    return records


def _get_agent(state: MarketState, agent_id: str):
    if agent_id not in state.agents:
        raise InvalidActionError(f"unknown agent: {agent_id}")
    return state.agents[agent_id]


def proposal_responder_id(proposal: TradeProposal) -> str:
    if proposal.initiator_id == proposal.seller_id:
        return proposal.buyer_id
    if proposal.initiator_id == proposal.buyer_id:
        return proposal.seller_id
    raise InvalidActionError("invalid_trade_initiator")


def is_agent_trade_channel_allowed(
    config: SimulationConfig | None,
    *,
    seller_role: str,
    buyer_role: str,
) -> bool:
    if config is None:
        return True
    key = f"{seller_role}_to_{buyer_role}"
    return bool(config.agent_trade_channels.get(key, False))


def get_active_lot_lock(state: MarketState, lot_id: str) -> dict | None:
    for proposal in state.pending_proposals.values():
        if proposal.lot_id == lot_id and proposal.status == "pending":
            return {
                "reason": "active_proposal",
                "reference_id": proposal.proposal_id,
            }
    for counteroffer in state.pending_trade_counteroffers.values():
        if counteroffer.lot_id == lot_id and counteroffer.status == "pending":
            return {
                "reason": "active_counteroffer",
                "reference_id": counteroffer.counteroffer_id,
            }
    if (
        state.consumer_market is not None
        and lot_id in state.consumer_market.consumed_lot_ids
    ):
        return {"reason": "final_consumption", "reference_id": None}
    return None


def build_trade_candidates(
    state: MarketState,
    config: SimulationConfig,
    acting_agent_id: str,
) -> dict:
    actor = _get_agent(state, acting_agent_id)
    sell: list[dict] = []
    buy: list[dict] = []
    for buyer_id, buyer in state.agents.items():
        if buyer_id == acting_agent_id:
            continue
        if not is_agent_trade_channel_allowed(
            config,
            seller_role=actor.role,
            buyer_role=buyer.role,
        ):
            continue
        for lot in actor.inventory.values():
            if get_active_lot_lock(state, lot.lot_id) is None:
                sell.append(
                    {
                        "seller_id": acting_agent_id,
                        "buyer_id": buyer_id,
                        "lot_id": lot.lot_id,
                        "quantity": lot.quantity,
                    }
                )
    if actor.role == "roaster":
        for seller_id, seller in state.agents.items():
            if seller_id == acting_agent_id or seller.role != "retailer":
                continue
            if not is_agent_trade_channel_allowed(
                config,
                seller_role=seller.role,
                buyer_role=actor.role,
            ):
                continue
            for lot in seller.inventory.values():
                if get_active_lot_lock(state, lot.lot_id) is None:
                    buy.append(
                        {
                            "seller_id": seller_id,
                            "buyer_id": acting_agent_id,
                            "lot_id": lot.lot_id,
                            "quantity": lot.quantity,
                        }
                    )
    return {"sell": sell, "buy": buy}


def validate_trade_proposal(
    state: MarketState,
    config: SimulationConfig | None,
    *,
    initiator_id: str,
    seller_id: str,
    buyer_id: str,
    lot_id: str,
    quantity: int,
    unit_price: float,
) -> None:
    _get_agent(state, initiator_id)
    seller = _get_agent(state, seller_id)
    buyer = _get_agent(state, buyer_id)
    if seller_id == buyer_id:
        raise InvalidActionError("seller_and_buyer_must_differ")
    if initiator_id not in {seller_id, buyer_id}:
        raise InvalidActionError("invalid_trade_initiator")
    if lot_id not in seller.inventory:
        raise InvalidActionError("seller_does_not_own_lot")
    lot = seller.inventory[lot_id]
    if not isinstance(quantity, int) or quantity <= 0:
        raise InvalidActionError("quantity_must_be_positive")
    if quantity != lot.quantity:
        raise InvalidActionError("quantity_must_match_full_lot")
    if not math.isfinite(unit_price) or unit_price <= 0:
        raise InvalidActionError("unit_price_must_be_positive")
    if not is_agent_trade_channel_allowed(
        config,
        seller_role=seller.role,
        buyer_role=buyer.role,
    ):
        raise InvalidActionError("agent_trade_channel_not_allowed")
    lock = get_active_lot_lock(state, lot_id)
    if lock is not None:
        raise InvalidActionError(f"lot_locked:{lock['reason']}")
    if (
        state.consumer_market is not None
        and lot_id in state.consumer_market.consumed_lot_ids
    ):
        raise InvalidActionError("lot_already_consumed")
def create_trade_proposal(
    state: MarketState,
    *,
    initiator_id: str,
    seller_id: str,
    buyer_id: str,
    lot_id: str,
    quantity: int,
    unit_price: float,
    proposal_message: str | None = None,
    config: SimulationConfig | None = None,
) -> TradeProposal:
    validate_trade_proposal(
        state,
        config,
        initiator_id=initiator_id,
        seller_id=seller_id,
        buyer_id=buyer_id,
        lot_id=lot_id,
        quantity=quantity,
        unit_price=unit_price,
    )
    proposal = TradeProposal(
        proposal_id=f"proposal-{len(state.pending_proposals) + 1}",
        initiator_id=initiator_id,
        seller_id=seller_id,
        buyer_id=buyer_id,
        lot_id=lot_id,
        quantity=quantity,
        unit_price=unit_price,
        proposal_message=proposal_message,
        status="pending",
        created_day=state.day,
    )
    state.pending_proposals[proposal.proposal_id] = proposal
    return proposal


def execute_agent_trade(
    state: MarketState,
    *,
    proposal: TradeProposal,
    transaction_fee_rate: float = 0.0,
) -> TradeRecord:
    if proposal.status != "pending":
        raise InvalidActionError("proposal_not_pending")
    seller = _get_agent(state, proposal.seller_id)
    buyer = _get_agent(state, proposal.buyer_id)
    if proposal.lot_id not in seller.inventory:
        raise InvalidActionError("seller_does_not_own_lot")
    total_price = round(proposal.quantity * proposal.unit_price, 2)
    fee = round(total_price * transaction_fee_rate, 2)
    required_cash = total_price + fee
    if buyer.cash < required_cash:
        raise InvalidActionError("buyer_insufficient_cash")
    lot = seller.inventory.pop(proposal.lot_id)
    buyer.cash = round(buyer.cash - required_cash, 2)
    seller.cash = round(seller.cash + total_price, 2)
    seller.reported_revenue = round(seller.reported_revenue + total_price, 2)
    lot.current_owner_id = buyer.agent_id
    lot.carrying_unit_cost = proposal.unit_price
    lot.owner_history.append(buyer.agent_id)
    buyer.inventory[lot.lot_id] = lot
    proposal.status = "accepted"
    proposal.decision_day = state.day
    proposal.closed_day = state.day
    for counteroffer in state.pending_trade_counteroffers.values():
        if (
            counteroffer.proposal_id == proposal.proposal_id
            and counteroffer.status == "pending"
        ):
            counteroffer.status = "superseded"
            counteroffer.decision_day = state.day
            counteroffer.close_reason = "original_proposal_accepted"
    trade = TradeRecord(
        trade_id=f"trade-{len(state.trade_history) + 1}",
        day=state.day,
        seller_id=proposal.seller_id,
        buyer_id=proposal.buyer_id,
        lot_id=proposal.lot_id,
        quantity=proposal.quantity,
        unit_price=proposal.unit_price,
        total_price=total_price,
        initiator_id=proposal.initiator_id,
        proposal_id=proposal.proposal_id,
        trade_type="agent_trade",
        original_unit_cost=lot.original_unit_cost,
    )
    state.trade_history.append(trade)
    return trade


def accept_trade_proposal(
    state: MarketState,
    *,
    proposal_id: str,
    responder_id: str,
    transaction_fee_rate: float = 0.0,
) -> TradeRecord:
    proposal = state.pending_proposals.get(proposal_id)
    if proposal is None:
        raise InvalidActionError("proposal_does_not_exist")
    if proposal.status != "pending":
        raise InvalidActionError("proposal_not_pending")
    if proposal_responder_id(proposal) != responder_id:
        raise InvalidActionError("only_proposal_responder_can_accept")
    return execute_agent_trade(
        state,
        proposal=proposal,
        transaction_fee_rate=transaction_fee_rate,
    )


def reject_trade_proposal(
    state: MarketState,
    *,
    proposal_id: str,
    responder_id: str,
) -> TradeProposal:
    proposal = state.pending_proposals.get(proposal_id)
    if proposal is None:
        raise InvalidActionError("proposal_does_not_exist")
    if proposal.status != "pending":
        raise InvalidActionError("proposal_not_pending")
    if proposal_responder_id(proposal) != responder_id:
        raise InvalidActionError("only_proposal_responder_can_reject")
    proposal.status = "rejected"
    proposal.decision_day = state.day
    proposal.closed_day = state.day
    proposal.close_reason = "proposal_rejected"
    _close_counteroffers_for_proposal(
        state,
        proposal_id=proposal_id,
        status="cancelled",
        reason="original_proposal_rejected",
    )
    return proposal


def _close_counteroffers_for_proposal(
    state: MarketState,
    *,
    proposal_id: str,
    status: str,
    reason: str,
) -> None:
    for counteroffer in state.pending_trade_counteroffers.values():
        if (
            counteroffer.proposal_id == proposal_id
            and counteroffer.status == "pending"
        ):
            counteroffer.status = status
            counteroffer.decision_day = state.day
            counteroffer.close_reason = reason


def create_trade_counteroffer(
    state: MarketState,
    *,
    proposal_id: str,
    initiator_id: str,
    unit_price: float,
) -> TradeCounteroffer:
    proposal = state.pending_proposals.get(proposal_id)
    if proposal is None or proposal.status != "pending":
        raise InvalidActionError("proposal_not_pending")
    if proposal_responder_id(proposal) != initiator_id:
        raise InvalidActionError("only_proposal_responder_can_counter")
    if not math.isfinite(unit_price) or unit_price <= 0:
        raise InvalidActionError("unit_price_must_be_positive")
    for counteroffer in state.pending_trade_counteroffers.values():
        if counteroffer.proposal_id == proposal_id and counteroffer.status == "pending":
            counteroffer.status = "superseded"
            counteroffer.decision_day = state.day
            counteroffer.close_reason = "replaced_by_new_counteroffer"
    counteroffer = TradeCounteroffer(
        counteroffer_id=f"counteroffer-{len(state.pending_trade_counteroffers) + 1}",
        proposal_id=proposal_id,
        initiator_id=initiator_id,
        responder_id=proposal.initiator_id,
        seller_id=proposal.seller_id,
        buyer_id=proposal.buyer_id,
        lot_id=proposal.lot_id,
        quantity=proposal.quantity,
        unit_price=round(unit_price, 2),
        status="pending",
        created_day=state.day,
    )
    state.pending_trade_counteroffers[counteroffer.counteroffer_id] = counteroffer
    return counteroffer


def respond_to_trade_counteroffer(
    state: MarketState,
    *,
    counteroffer_id: str,
    responder_id: str,
    accept: bool,
    transaction_fee_rate: float = 0.0,
) -> TradeRecord | None:
    counteroffer = state.pending_trade_counteroffers.get(counteroffer_id)
    if counteroffer is None or counteroffer.status != "pending":
        raise InvalidActionError("counteroffer_not_pending")
    if counteroffer.responder_id != responder_id:
        raise InvalidActionError("only_counteroffer_responder_can_respond")
    proposal = state.pending_proposals[counteroffer.proposal_id]
    if accept:
        proposal.unit_price = counteroffer.unit_price
        trade = execute_agent_trade(
            state,
            proposal=proposal,
            transaction_fee_rate=transaction_fee_rate,
        )
        counteroffer.status = "accepted"
        counteroffer.close_reason = "counteroffer_accepted"
    else:
        trade = None
        counteroffer.status = "rejected"
        counteroffer.close_reason = "counteroffer_rejected"
    counteroffer.decision_day = state.day
    return trade


def close_active_negotiations_for_lot(
    state: MarketState,
    *,
    lot_id: str,
    reason: str,
    day: int,
) -> tuple[list[TradeProposal], list[TradeCounteroffer]]:
    proposals: list[TradeProposal] = []
    counteroffers: list[TradeCounteroffer] = []
    for proposal in state.pending_proposals.values():
        if proposal.lot_id == lot_id and proposal.status == "pending":
            proposal.status = "cancelled"
            proposal.closed_day = day
            proposal.close_reason = reason
            proposals.append(proposal)
    for counteroffer in state.pending_trade_counteroffers.values():
        if counteroffer.lot_id == lot_id and counteroffer.status == "pending":
            counteroffer.status = "invalidated"
            counteroffer.decision_day = day
            counteroffer.close_reason = reason
            counteroffers.append(counteroffer)
    return proposals, counteroffers


def execute_consumer_sale(
    state: MarketState,
    *,
    seller_id: str,
    lot_id: str,
    quantity: int,
    unit_price: float,
    consumer_market_enabled: bool,
    consumer_max_unit_price: float,
) -> TradeRecord | None:
    seller = _get_agent(state, seller_id)
    if not consumer_market_enabled:
        raise InvalidActionError("consumer market is disabled")
    if seller.role != "roaster":
        raise InvalidActionError("only roaster can sell to consumer market")
    if lot_id not in seller.inventory:
        raise InvalidActionError("seller does not own lot")
    lot = seller.inventory[lot_id]
    if quantity != lot.quantity:
        raise InvalidActionError("quantity must match full lot quantity")
    if unit_price <= 0:
        raise InvalidActionError("unit_price must be positive")
    if unit_price > consumer_max_unit_price:
        return None

    total_price = round(quantity * unit_price, 2)
    seller.inventory.pop(lot_id)
    close_active_negotiations_for_lot(
        state,
        lot_id=lot_id,
        reason="lot_sold_to_consumer",
        day=state.day,
    )
    seller.cash = round(seller.cash + total_price, 2)
    seller.reported_revenue = round(seller.reported_revenue + total_price, 2)
    trade = TradeRecord(
        trade_id=f"trade-{len(state.trade_history) + 1}",
        day=state.day,
        seller_id=seller_id,
        buyer_id="consumer_market",
        lot_id=lot_id,
        quantity=quantity,
        unit_price=unit_price,
        total_price=total_price,
        trade_type="consumer",
        original_unit_cost=lot.original_unit_cost,
        final_consumption=True,
    )
    state.trade_history.append(trade)
    return trade


def sell_to_consumer_market(
    state: MarketState,
    *,
    seller_id: str,
    lot_id: str,
    quantity: int,
    consumer_market_enabled: bool,
    consumer_unit_price: float,
    consumer_daily_demand_capacity: int,
    consumer_sale_requires_full_lot: bool = True,
) -> ConsumerSaleResult:
    """Execute a fixed-price sale against the shared daily consumer capacity."""
    remaining = consumer_daily_demand_capacity
    if state.consumer_market is None:
        state.consumer_market = ConsumerMarketState(
            daily_capacity=consumer_daily_demand_capacity,
        )
    remaining = state.consumer_market.remaining_capacity_by_day.setdefault(
        state.day,
        state.consumer_market.daily_capacity,
    )

    def rejected(reason: str) -> ConsumerSaleResult:
        return ConsumerSaleResult(
            status="rejected",
            seller_id=seller_id,
            buyer_id="consumer_market",
            lot_id=lot_id,
            quantity_sold=0,
            unit_price=consumer_unit_price,
            total_revenue=0.0,
            remaining_daily_demand=remaining,
            reason=reason,
        )

    if not consumer_market_enabled:
        return rejected("Consumer market is disabled.")
    if not isinstance(quantity, int) or quantity <= 0:
        return rejected("Quantity must be a positive integer.")
    seller = state.agents.get(seller_id)
    if seller is None:
        return rejected("The seller does not exist.")
    if lot_id in state.consumer_market.consumed_lot_ids:
        return rejected("The specified lot has already exited to final consumption.")
    lot = seller.inventory.get(lot_id)
    if lot is None:
        return rejected("The specified lot does not exist or is no longer available.")
    if quantity > lot.quantity:
        return rejected("The seller does not own the requested quantity.")
    if consumer_sale_requires_full_lot and quantity != lot.quantity:
        return rejected("Consumer sales must include the full lot quantity.")
    if remaining < quantity:
        return rejected(
            "Consumer demand capacity is insufficient for the requested full-lot sale."
        )

    total_revenue = round(quantity * consumer_unit_price, 2)
    seller.inventory.pop(lot_id)
    close_active_negotiations_for_lot(
        state,
        lot_id=lot_id,
        reason="lot_sold_to_consumer",
        day=state.day,
    )
    seller.cash = round(seller.cash + total_revenue, 2)
    seller.reported_revenue = round(seller.reported_revenue + total_revenue, 2)
    state.consumer_market.remaining_capacity_by_day[state.day] = remaining - quantity
    state.consumer_market.consumed_lot_ids.append(lot_id)
    trade = TradeRecord(
        trade_id=f"trade-{len(state.trade_history) + 1}",
        day=state.day,
        seller_id=seller_id,
        buyer_id="consumer_market",
        lot_id=lot_id,
        quantity=quantity,
        unit_price=consumer_unit_price,
        total_price=total_revenue,
        trade_type="consumer",
        original_unit_cost=lot.original_unit_cost,
        final_consumption=True,
    )
    state.trade_history.append(trade)
    return ConsumerSaleResult(
        status="accepted",
        seller_id=seller_id,
        buyer_id="consumer_market",
        lot_id=lot_id,
        quantity_sold=quantity,
        unit_price=consumer_unit_price,
        total_revenue=total_revenue,
        remaining_daily_demand=remaining - quantity,
        reason="Consumer sale completed.",
        trade=trade,
    )


def expire_old_proposals(state: MarketState, *, proposal_expiry_days: int) -> list[TradeProposal]:
    expired: list[TradeProposal] = []
    for proposal in state.pending_proposals.values():
        if proposal.status == "pending" and state.day - proposal.created_day > proposal_expiry_days:
            proposal.status = "expired"
            proposal.closed_day = state.day
            proposal.close_reason = "proposal_expired"
            _close_counteroffers_for_proposal(
                state,
                proposal_id=proposal.proposal_id,
                status="cancelled",
                reason="original_proposal_expired",
            )
            expired.append(proposal)
    return expired


def action_to_dict(action: AgentAction) -> dict:
    return asdict(action)
