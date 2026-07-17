from circular_coffee.config import build_default_config, create_initial_market_state
from circular_coffee.market import (
    InvalidActionError,
    accept_trade_proposal,
    create_trade_proposal,
)


def test_create_valid_trade_proposal() -> None:
    state = create_initial_market_state(build_default_config())
    proposal = create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
        proposal_message="Please review this offer.",
    )
    assert proposal.status == "pending"
    assert proposal.buyer_id == "retailer_a"
    assert proposal.proposal_message == "Please review this offer."


def test_cannot_propose_unowned_lot() -> None:
    state = create_initial_market_state(build_default_config())
    try:
        create_trade_proposal(
            state,
            seller_id="retailer_a",
            buyer_id="retailer_b",
            lot_id="LOT-001",
            quantity=100,
            unit_price=10.0,
        )
    except InvalidActionError as exc:
        assert "does not own" in str(exc)
    else:
        raise AssertionError("expected InvalidActionError")


def test_cannot_sell_to_self() -> None:
    state = create_initial_market_state(build_default_config())
    try:
        create_trade_proposal(
            state,
            seller_id="roaster",
            buyer_id="roaster",
            lot_id="LOT-001",
            quantity=100,
            unit_price=10.0,
        )
    except InvalidActionError as exc:
        assert "self" in str(exc)
    else:
        raise AssertionError("expected InvalidActionError")


def test_accept_fails_with_insufficient_cash() -> None:
    config = build_default_config()
    state = create_initial_market_state(config)
    state.agents["retailer_a"].cash = 100.0
    proposal = create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
    )
    try:
        accept_trade_proposal(state, proposal_id=proposal.proposal_id, buyer_id="retailer_a")
    except InvalidActionError as exc:
        assert "insufficient cash" in str(exc)
    else:
        raise AssertionError("expected InvalidActionError")


def test_accept_moves_cash_and_revenue_and_inventory() -> None:
    state = create_initial_market_state(build_default_config())
    proposal = create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
    )
    trade = accept_trade_proposal(state, proposal_id=proposal.proposal_id, buyer_id="retailer_a")
    assert trade.total_price == 1000.0
    assert state.agents["roaster"].cash == 4000.0
    assert state.agents["retailer_a"].cash == 2000.0
    assert state.agents["roaster"].reported_revenue == 1000.0
    assert "LOT-001" not in state.agents["roaster"].inventory
    assert state.agents["retailer_a"].inventory["LOT-001"].current_owner_id == "retailer_a"
    assert state.agents["retailer_a"].inventory["LOT-001"].carrying_unit_cost == 10.0


def test_cannot_double_accept_same_proposal() -> None:
    state = create_initial_market_state(build_default_config())
    proposal = create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
    )
    accept_trade_proposal(state, proposal_id=proposal.proposal_id, buyer_id="retailer_a")
    try:
        accept_trade_proposal(state, proposal_id=proposal.proposal_id, buyer_id="retailer_a")
    except InvalidActionError as exc:
        assert "not pending" in str(exc)
    else:
        raise AssertionError("expected InvalidActionError")
