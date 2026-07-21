from __future__ import annotations

from dataclasses import asdict

from .models import AgentAction, MarketState, TradeProposal, TradeRecord


class InvalidActionError(ValueError):
    pass


def _get_agent(state: MarketState, agent_id: str):
    if agent_id not in state.agents:
        raise InvalidActionError(f"unknown agent: {agent_id}")
    return state.agents[agent_id]


def create_trade_proposal(
    state: MarketState,
    *,
    seller_id: str,
    buyer_id: str,
    lot_id: str,
    quantity: int,
    unit_price: float,
    proposal_message: str | None = None,
) -> TradeProposal:
    seller = _get_agent(state, seller_id)
    if lot_id not in seller.inventory:
        raise InvalidActionError("seller does not own lot")
    if buyer_id not in state.agents:
        raise InvalidActionError("buyer does not exist")
    if buyer_id == seller_id:
        raise InvalidActionError("seller cannot sell to self")
    lot = seller.inventory[lot_id]
    if quantity != lot.quantity:
        raise InvalidActionError("quantity must match full lot quantity")
    if unit_price <= 0:
        raise InvalidActionError("unit_price must be positive")
    for proposal in state.pending_proposals.values():
        if proposal.lot_id == lot_id and proposal.status == "pending":
            raise InvalidActionError("duplicate pending proposal for lot")
    proposal = TradeProposal(
        proposal_id=f"proposal-{len(state.pending_proposals) + 1}",
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


def accept_trade_proposal(
    state: MarketState,
    *,
    proposal_id: str,
    buyer_id: str,
    transaction_fee_rate: float = 0.0,
) -> TradeRecord:
    proposal = state.pending_proposals.get(proposal_id)
    if proposal is None:
        raise InvalidActionError("proposal does not exist")
    if proposal.status != "pending":
        raise InvalidActionError("proposal is not pending")
    if proposal.buyer_id != buyer_id:
        raise InvalidActionError("only designated buyer can accept")
    seller = _get_agent(state, proposal.seller_id)
    buyer = _get_agent(state, proposal.buyer_id)
    if proposal.lot_id not in seller.inventory:
        raise InvalidActionError("seller no longer owns lot")
    total_price = round(proposal.quantity * proposal.unit_price, 2)
    fee = round(total_price * transaction_fee_rate, 2)
    required_cash = total_price + fee
    if buyer.cash < required_cash:
        raise InvalidActionError("buyer has insufficient cash")
    lot = seller.inventory.pop(proposal.lot_id)
    buyer.cash = round(buyer.cash - required_cash, 2)
    seller.cash = round(seller.cash + total_price, 2)
    seller.reported_revenue = round(seller.reported_revenue + total_price, 2)
    lot.current_owner_id = buyer.agent_id
    lot.carrying_unit_cost = proposal.unit_price
    lot.owner_history.append(buyer.agent_id)
    buyer.inventory[lot.lot_id] = lot
    proposal.status = "accepted"
    trade = TradeRecord(
        trade_id=f"trade-{len(state.trade_history) + 1}",
        day=state.day,
        seller_id=proposal.seller_id,
        buyer_id=proposal.buyer_id,
        lot_id=proposal.lot_id,
        quantity=proposal.quantity,
        unit_price=proposal.unit_price,
        total_price=total_price,
        trade_type="intercompany",
        original_unit_cost=lot.original_unit_cost,
    )
    state.trade_history.append(trade)
    return trade


def reject_trade_proposal(state: MarketState, *, proposal_id: str, buyer_id: str) -> TradeProposal:
    proposal = state.pending_proposals.get(proposal_id)
    if proposal is None:
        raise InvalidActionError("proposal does not exist")
    if proposal.status != "pending":
        raise InvalidActionError("proposal is not pending")
    if proposal.buyer_id != buyer_id:
        raise InvalidActionError("only designated buyer can reject")
    proposal.status = "rejected"
    return proposal


def execute_repurchase(
    state: MarketState,
    *,
    retailer_id: str,
    lot_id: str,
    offered_price: float,
    transaction_fee_rate: float = 0.0,
) -> TradeRecord:
    """Execute an accepted retailer-to-roaster repurchase at a total offered price."""
    retailer = _get_agent(state, retailer_id)
    roaster = _get_agent(state, "roaster")
    if retailer.role != "retailer":
        raise InvalidActionError("repurchase seller must be a retailer")
    if lot_id not in retailer.inventory:
        raise InvalidActionError("retailer does not own lot")
    if offered_price <= 0:
        raise InvalidActionError("offered_price must be positive")
    lot = retailer.inventory[lot_id]
    fee = round(offered_price * transaction_fee_rate, 2)
    if roaster.cash < offered_price + fee:
        raise InvalidActionError("roaster has insufficient cash")

    retailer.inventory.pop(lot_id)
    roaster.cash = round(roaster.cash - offered_price - fee, 2)
    retailer.cash = round(retailer.cash + offered_price, 2)
    retailer.reported_revenue = round(retailer.reported_revenue + offered_price, 2)
    unit_price = round(offered_price / lot.quantity, 10)
    lot.current_owner_id = roaster.agent_id
    lot.carrying_unit_cost = unit_price
    lot.owner_history.append(roaster.agent_id)
    roaster.inventory[lot_id] = lot
    trade = TradeRecord(
        trade_id=f"trade-{len(state.trade_history) + 1}",
        day=state.day,
        seller_id=retailer_id,
        buyer_id=roaster.agent_id,
        lot_id=lot_id,
        quantity=lot.quantity,
        unit_price=unit_price,
        total_price=round(offered_price, 2),
        trade_type="intercompany",
        original_unit_cost=lot.original_unit_cost,
    )
    state.trade_history.append(trade)
    return trade


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
    )
    state.trade_history.append(trade)
    return trade


def expire_old_proposals(state: MarketState, *, proposal_expiry_days: int) -> list[TradeProposal]:
    expired: list[TradeProposal] = []
    for proposal in state.pending_proposals.values():
        if proposal.status == "pending" and state.day - proposal.created_day > proposal_expiry_days:
            proposal.status = "expired"
            expired.append(proposal)
    return expired


def action_to_dict(action: AgentAction) -> dict:
    return asdict(action)
