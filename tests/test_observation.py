from circular_coffee.config import build_default_config, create_initial_market_state
from circular_coffee.market import accept_trade_proposal, create_trade_proposal
from circular_coffee.observation import build_observation


def test_observation_hides_owner_history_fields() -> None:
    state = create_initial_market_state(build_default_config())
    observation = build_observation(state, "roaster")
    lot = observation["self"]["inventory"]["LOT-001"]
    assert "origin_owner_id" not in lot
    assert "current_owner_id" not in lot
    assert "owner_history" not in lot


def test_proposal_message_visible_to_designated_buyer() -> None:
    state = create_initial_market_state(build_default_config())
    create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
        proposal_message="Can offer quick delivery on this lot.",
    )
    observation = build_observation(state, "retailer_a")
    assert observation["incoming_pending_proposals"][0]["proposal_message"] == (
        "Can offer quick delivery on this lot."
    )


def test_proposal_message_hidden_from_other_agents() -> None:
    state = create_initial_market_state(build_default_config())
    create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.0,
        proposal_message="Can offer quick delivery on this lot.",
    )
    observation = build_observation(state, "retailer_b")
    assert observation["incoming_pending_proposals"] == []


def test_inventory_value_uses_purchase_price_after_transfer() -> None:
    state = create_initial_market_state(build_default_config())
    proposal = create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.1,
    )
    accept_trade_proposal(state, proposal_id=proposal.proposal_id, buyer_id="retailer_a")
    observation = build_observation(state, "retailer_a")
    lot = observation["self"]["inventory"]["LOT-001"]
    assert lot["original_unit_cost"] == 8.0
    assert lot["carrying_unit_cost"] == 10.1
