from circular_coffee.config import (
    build_experiment_config,
    build_market_information,
    create_initial_market_state,
)
from circular_coffee.market import (
    accept_trade_proposal,
    create_trade_counteroffer,
    create_trade_proposal,
)
from circular_coffee.metrics import economic_inventory_value
from circular_coffee.observation import build_observation


def _config():
    return build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        lot_ids=["LOT-001"],
    )


def _observe(state, config, agent_id):
    agent = state.agents[agent_id]
    return build_observation(
        state,
        agent_id,
        initial_cash=agent.cash,
        initial_inventory_value=economic_inventory_value(agent),
        market_information=build_market_information(config, state),
        config=config,
    )


def test_observation_hides_internal_ownership_fields() -> None:
    config = _config()
    state = create_initial_market_state(config)

    lot = _observe(state, config, "roaster")["self"]["inventory"]["LOT-001"]

    assert "origin_owner_id" not in lot
    assert "current_owner_id" not in lot
    assert "owner_history" not in lot


def test_proposal_is_visible_only_to_responder() -> None:
    config = _config()
    state = create_initial_market_state(config)
    proposal = create_trade_proposal(
        state,
        config=config,
        initiator_id="roaster",
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=9.0,
        proposal_message="Public negotiation message.",
    )

    retailer_view = _observe(state, config, "retailer_a")
    other_view = _observe(state, config, "retailer_b")

    assert retailer_view["incoming_trade_proposals"][0]["proposal_id"] == proposal.proposal_id
    assert (
        retailer_view["incoming_trade_proposals"][0]["proposal_message"]
        == "Public negotiation message."
    )
    assert other_view["incoming_trade_proposals"] == []


def test_buyer_initiated_proposal_is_visible_to_seller() -> None:
    config = _config()
    state = create_initial_market_state(config)
    initial = create_trade_proposal(
        state,
        config=config,
        initiator_id="roaster",
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=9.0,
    )
    accept_trade_proposal(
        state,
        proposal_id=initial.proposal_id,
        responder_id="retailer_a",
    )
    proposal = create_trade_proposal(
        state,
        config=config,
        initiator_id="roaster",
        seller_id="retailer_a",
        buyer_id="roaster",
        lot_id="LOT-001",
        quantity=100,
        unit_price=9.1,
    )

    retailer_view = _observe(state, config, "retailer_a")

    assert retailer_view["incoming_trade_proposals"][0]["proposal_id"] == proposal.proposal_id
    assert "accept_trade" in retailer_view["available_action_types"]


def test_counteroffer_visibility_and_lot_lock_are_common() -> None:
    config = _config()
    state = create_initial_market_state(config)
    proposal = create_trade_proposal(
        state,
        config=config,
        initiator_id="roaster",
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=9.0,
    )
    counteroffer = create_trade_counteroffer(
        state,
        proposal_id=proposal.proposal_id,
        initiator_id="retailer_a",
        unit_price=9.2,
    )

    roaster_view = _observe(state, config, "roaster")

    assert roaster_view["incoming_trade_counteroffers"][0]["counteroffer_id"] == (
        counteroffer.counteroffer_id
    )
    assert "accept_counteroffer" in roaster_view["available_action_types"]
    assert roaster_view["locked_lots"]["LOT-001"]["reference_id"] == proposal.proposal_id


def test_inventory_costs_remain_visible_after_transfer() -> None:
    config = _config()
    state = create_initial_market_state(config)
    proposal = create_trade_proposal(
        state,
        config=config,
        initiator_id="roaster",
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.1,
    )
    accept_trade_proposal(
        state,
        proposal_id=proposal.proposal_id,
        responder_id="retailer_a",
    )

    lot = _observe(state, config, "retailer_a")["self"]["inventory"]["LOT-001"]

    assert lot["original_unit_cost"] == 8.0
    assert lot["carrying_unit_cost"] == 10.1
