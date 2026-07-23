from circular_coffee.config import build_default_config, create_initial_market_state
from circular_coffee.market import (
    InvalidActionError,
    accept_trade_proposal,
    create_trade_proposal,
    execute_consumer_sale,
)


def test_create_valid_trade_proposal() -> None:
    state = create_initial_market_state(build_default_config())
    proposal = create_trade_proposal(
        state,
        initiator_id="roaster",
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
            initiator_id="retailer_a",
            seller_id="retailer_a",
            buyer_id="retailer_b",
            lot_id="LOT-001",
            quantity=100,
            unit_price=10.0,
        )
    except InvalidActionError as exc:
        assert str(exc) == "seller_does_not_own_lot"
    else:
        raise AssertionError("expected InvalidActionError")


def test_cannot_sell_to_self() -> None:
    state = create_initial_market_state(build_default_config())
    try:
        create_trade_proposal(
            state,
            initiator_id="roaster",
            seller_id="roaster",
            buyer_id="roaster",
            lot_id="LOT-001",
            quantity=100,
            unit_price=10.0,
        )
    except InvalidActionError as exc:
        assert str(exc) == "seller_and_buyer_must_differ"
    else:
        raise AssertionError("expected InvalidActionError")


def test_accept_fails_with_insufficient_cash() -> None:
    config = build_default_config()
    state = create_initial_market_state(config)
    state.agents["retailer_a"].cash = 100.0
    proposal = create_trade_proposal(
        state,
        initiator_id="roaster",
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
    )
    try:
        accept_trade_proposal(
            state,
            proposal_id=proposal.proposal_id,
            responder_id="retailer_a",
        )
    except InvalidActionError as exc:
        assert str(exc) == "buyer_insufficient_cash"
    else:
        raise AssertionError("expected InvalidActionError")


def test_accept_moves_cash_and_revenue_and_inventory() -> None:
    state = create_initial_market_state(build_default_config())
    proposal = create_trade_proposal(
        state,
        initiator_id="roaster",
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
    )
    trade = accept_trade_proposal(
        state,
        proposal_id=proposal.proposal_id,
        responder_id="retailer_a",
    )
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
        initiator_id="roaster",
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
    )
    accept_trade_proposal(
        state,
        proposal_id=proposal.proposal_id,
        responder_id="retailer_a",
    )
    try:
        accept_trade_proposal(
            state,
            proposal_id=proposal.proposal_id,
            responder_id="retailer_a",
        )
    except InvalidActionError as exc:
        assert str(exc) == "proposal_not_pending"
    else:
        raise AssertionError("expected InvalidActionError")


def test_consumer_sale_completes_and_removes_lot() -> None:
    config = build_default_config(
        experiment_condition="multi_strategy",
        consumer_market_enabled=True,
        consumer_max_unit_price=9.5,
        lot_ids=["LOT-001"],
    )
    state = create_initial_market_state(config)
    trade = execute_consumer_sale(
        state,
        seller_id="roaster",
        lot_id="LOT-001",
        quantity=100,
        unit_price=9.0,
        consumer_market_enabled=config.consumer_market_enabled,
        consumer_max_unit_price=config.consumer_max_unit_price,
    )
    assert trade is not None
    assert trade.trade_type == "consumer"
    assert state.agents["roaster"].reported_revenue == 900.0
    assert state.agents["roaster"].cash == 3900.0
    assert "LOT-001" not in state.agents["roaster"].inventory


def test_consumer_sale_above_price_limit_does_not_change_state() -> None:
    config = build_default_config(
        experiment_condition="multi_strategy",
        consumer_market_enabled=True,
        consumer_max_unit_price=9.5,
        lot_ids=["LOT-001"],
    )
    state = create_initial_market_state(config)
    trade = execute_consumer_sale(
        state,
        seller_id="roaster",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
        consumer_market_enabled=config.consumer_market_enabled,
        consumer_max_unit_price=config.consumer_max_unit_price,
    )
    assert trade is None
    assert state.agents["roaster"].reported_revenue == 0.0
    assert state.agents["roaster"].cash == 3000.0
    assert "LOT-001" in state.agents["roaster"].inventory
