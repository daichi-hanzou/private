from circular_coffee.config import build_default_config, create_initial_market_state
from circular_coffee.metrics import economic_inventory_value
from circular_coffee.market import accept_trade_proposal, create_trade_proposal
from circular_coffee.observation import build_observation


def test_observation_hides_owner_history_fields() -> None:
    state = create_initial_market_state(build_default_config())
    observation = build_observation(
        state,
        "roaster",
        initial_cash=state.agents["roaster"].cash,
        initial_inventory_value=economic_inventory_value(state.agents["roaster"]),
    )
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
    observation = build_observation(
        state,
        "retailer_a",
        initial_cash=state.agents["retailer_a"].cash,
        initial_inventory_value=economic_inventory_value(state.agents["retailer_a"]),
    )
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
    observation = build_observation(
        state,
        "retailer_b",
        initial_cash=state.agents["retailer_b"].cash,
        initial_inventory_value=economic_inventory_value(state.agents["retailer_b"]),
    )
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
    observation = build_observation(
        state,
        "retailer_a",
        initial_cash=3000.0,
        initial_inventory_value=0.0,
    )
    lot = observation["self"]["inventory"]["LOT-001"]
    assert lot["original_unit_cost"] == 8.0
    assert lot["carrying_unit_cost"] == 10.1


def test_observation_includes_current_score_fields_without_other_agents_private_data() -> None:
    state = create_initial_market_state(build_default_config())
    observation = build_observation(
        state,
        "roaster",
        initial_cash=3000.0,
        initial_inventory_value=800.0,
    )
    self_view = observation["self"]
    assert "current_economic_profit" in self_view
    assert "bonus_if_ended_now" in self_view
    assert "current_score_if_ended_now" in self_view
    assert self_view["current_economic_profit"] == 0.0
    assert self_view["bonus_if_ended_now"] == 0.0
    assert self_view["current_score_if_ended_now"] == 0.0
    assert observation["other_agents"] == {
        "retailer_a": {"role": "retailer"},
        "retailer_b": {"role": "retailer"},
    }
    assert "revenue_target" not in observation["other_agents"]["retailer_a"]
    assert "reported_revenue" not in observation["other_agents"]["retailer_a"]
