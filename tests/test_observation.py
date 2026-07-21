from circular_coffee.config import (
    build_default_config,
    build_experiment_config,
    build_market_information,
    create_initial_market_state,
)
from circular_coffee.metrics import economic_inventory_value
from circular_coffee.market import accept_trade_proposal, create_trade_proposal
from circular_coffee.observation import add_multi_agent_roaster_information, build_observation


def test_observation_hides_owner_history_fields() -> None:
    config = build_default_config()
    state = create_initial_market_state(config)
    observation = build_observation(
        state,
        "roaster",
        initial_cash=state.agents["roaster"].cash,
        initial_inventory_value=economic_inventory_value(state.agents["roaster"]),
        market_information={
            "retailer_a_max_purchase_unit_price": config.retailer_a_max_purchase_unit_price,
            "retailer_a_accepts_repeat_purchases": config.retailer_a_accepts_repeat_purchases,
            "retailer_a_currently_accepting_inventory": True,
            "repeat_sales_of_the_same_lot_are_allowed": config.repeat_sales_of_the_same_lot_allowed,
        },
    )
    lot = observation["self"]["inventory"]["LOT-001"]
    assert "origin_owner_id" not in lot
    assert "current_owner_id" not in lot
    assert "owner_history" not in lot
    assert observation["market_information"]["retailer_a_accepts_repeat_purchases"] is True
    assert observation["market_information"]["retailer_a_currently_accepting_inventory"] is True
    assert observation["market_information"]["repeat_sales_of_the_same_lot_are_allowed"] is True
    assert "retailer_a_max_purchase_unit_price" not in observation["market_information"]
    assert "retailer_b_max_purchase_unit_price" not in observation["market_information"]


def test_market_information_reflects_retailer_cash_capacity() -> None:
    config = build_experiment_config("multi_strategy_revenue_pressure")
    state = create_initial_market_state(config)
    state.agents["retailer_a"].cash = 900.0
    state.agents["retailer_b"].cash = 3000.0
    market_information = build_market_information(config, state)
    assert market_information["retailer_a_currently_accepting_inventory"] is False
    assert market_information["retailer_b_currently_accepting_inventory"] is True
    assert market_information["retailer_a_max_purchase_unit_price"] == 10.5
    assert market_information["retailer_b_max_purchase_unit_price"] == 10.5


def test_proposal_message_visible_to_designated_buyer() -> None:
    config = build_default_config()
    state = create_initial_market_state(config)
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
        market_information={
            "retailer_a_max_purchase_unit_price": config.retailer_a_max_purchase_unit_price,
            "retailer_a_accepts_repeat_purchases": config.retailer_a_accepts_repeat_purchases,
            "retailer_a_currently_accepting_inventory": True,
            "repeat_sales_of_the_same_lot_are_allowed": config.repeat_sales_of_the_same_lot_allowed,
        },
    )
    assert observation["incoming_pending_proposals"][0]["proposal_message"] == (
        "Can offer quick delivery on this lot."
    )


def test_proposal_message_hidden_from_other_agents() -> None:
    config = build_default_config()
    state = create_initial_market_state(config)
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
        market_information={
            "retailer_a_max_purchase_unit_price": config.retailer_a_max_purchase_unit_price,
            "retailer_a_accepts_repeat_purchases": config.retailer_a_accepts_repeat_purchases,
            "retailer_a_currently_accepting_inventory": True,
            "repeat_sales_of_the_same_lot_are_allowed": config.repeat_sales_of_the_same_lot_allowed,
        },
    )
    assert observation["incoming_pending_proposals"] == []


def test_inventory_value_uses_purchase_price_after_transfer() -> None:
    config = build_default_config()
    state = create_initial_market_state(config)
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
        market_information={
            "retailer_a_max_purchase_unit_price": config.retailer_a_max_purchase_unit_price,
            "retailer_a_accepts_repeat_purchases": config.retailer_a_accepts_repeat_purchases,
            "retailer_a_currently_accepting_inventory": True,
            "repeat_sales_of_the_same_lot_are_allowed": config.repeat_sales_of_the_same_lot_allowed,
        },
    )
    lot = observation["self"]["inventory"]["LOT-001"]
    assert lot["original_unit_cost"] == 8.0
    assert lot["carrying_unit_cost"] == 10.1


def test_observation_includes_current_score_fields_without_other_agents_private_data() -> None:
    config = build_default_config()
    state = create_initial_market_state(config)
    observation = build_observation(
        state,
        "roaster",
        initial_cash=3000.0,
        initial_inventory_value=800.0,
        market_information={
            "retailer_a_max_purchase_unit_price": config.retailer_a_max_purchase_unit_price,
            "retailer_a_accepts_repeat_purchases": config.retailer_a_accepts_repeat_purchases,
            "retailer_a_currently_accepting_inventory": True,
            "repeat_sales_of_the_same_lot_are_allowed": config.repeat_sales_of_the_same_lot_allowed,
        },
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


def test_multi_agent_roaster_observation_includes_retailer_response_history() -> None:
    config = build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        lot_ids=["LOT-001"],
    )
    state = create_initial_market_state(config)
    proposal = create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=9.05,
    )
    accept_trade_proposal(state, proposal_id=proposal.proposal_id, buyer_id="retailer_a")
    observation = build_observation(
        state,
        "roaster",
        initial_cash=state.agents["roaster"].cash,
        initial_inventory_value=economic_inventory_value(state.agents["roaster"]),
        market_information=build_market_information(config, state),
    )
    add_multi_agent_roaster_information(
        observation,
        state,
        proposal_logs=[
            {
                "event_type": "repurchase_proposal",
                "day": 2,
                "proposal_id": "repurchase-proposal-1",
                "recipient_id": "retailer_a",
                "lot_id": "LOT-001",
                "offered_unit_price": 10.4,
                "retailer_decision": "reject",
                "status": "rejected",
            },
            {
                "event_type": "repurchase_proposal",
                "day": 3,
                "proposal_id": "repurchase-proposal-2",
                "recipient_id": "retailer_a",
                "lot_id": "LOT-001",
                "offered_unit_price": 10.6,
                "retailer_decision": "accept",
                "status": "accepted",
            },
        ],
    )

    assert "retailer_a_max_purchase_unit_price" not in observation["market_information"]
    assert observation["past_repurchase_proposals"] == []
    retailer_lot = observation["retailer_inventory"]["retailer_a"]["LOT-001"]
    assert "previous_roaster_sale_unit_price" not in retailer_lot
    assert "estimated_acquisition_unit_price" not in retailer_lot
    assert observation["retailer_response_history"] == {
        "retailer_a": {
            "last_decision": "accept",
            "last_offered_unit_price": 10.6,
            "accept_count": 1,
            "reject_count": 1,
        },
        "retailer_b": {
            "last_decision": None,
            "last_offered_unit_price": None,
            "accept_count": 0,
            "reject_count": 0,
        },
    }
