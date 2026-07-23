import json

import pytest

from circular_coffee.config import (
    build_experiment_config,
    create_initial_market_state,
)
from circular_coffee.market import (
    InvalidActionError,
    accept_trade_proposal,
    create_trade_counteroffer,
    create_trade_proposal,
    proposal_responder_id,
    respond_to_trade_counteroffer,
    reject_trade_proposal,
    sell_to_consumer_market,
)
from circular_coffee.models import AgentAction
from circular_coffee.policies import WaitPolicy
from circular_coffee.simulation import SimulationRunner


def _config():
    return build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        lot_ids=["LOT-001"],
        retailer_consumer_sale_enabled=True,
    )


def _sell_to_retailer(state, config):
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
    trade = accept_trade_proposal(
        state,
        proposal_id=proposal.proposal_id,
        responder_id="retailer_a",
    )
    return proposal, trade


def test_seller_initiated_trade_uses_common_proposal_and_execution() -> None:
    config = _config()
    state = create_initial_market_state(config)
    proposal, trade = _sell_to_retailer(state, config)

    assert proposal.initiator_id == "roaster"
    assert proposal_responder_id(proposal) == "retailer_a"
    assert trade.trade_type == "agent_trade"
    assert trade.initiator_id == "roaster"
    assert trade.proposal_id == proposal.proposal_id
    assert state.agents["roaster"].reported_revenue == 900.0
    assert state.agents["retailer_a"].inventory["LOT-001"].carrying_unit_cost == 9.0


def test_retailer_can_initiate_sale_to_roaster() -> None:
    config = _config()
    state = create_initial_market_state(config)
    _sell_to_retailer(state, config)

    proposal = create_trade_proposal(
        state,
        config=config,
        initiator_id="retailer_a",
        seller_id="retailer_a",
        buyer_id="roaster",
        lot_id="LOT-001",
        quantity=100,
        unit_price=9.1,
    )
    trade = accept_trade_proposal(
        state,
        proposal_id=proposal.proposal_id,
        responder_id="roaster",
    )

    assert trade.seller_id == "retailer_a"
    assert trade.buyer_id == "roaster"
    assert trade.initiator_id == "retailer_a"
    assert state.agents["roaster"].inventory["LOT-001"].owner_history == [
        "roaster",
        "retailer_a",
        "roaster",
    ]


def test_roaster_can_initiate_purchase_from_retailer() -> None:
    config = _config()
    state = create_initial_market_state(config)
    _sell_to_retailer(state, config)

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

    assert proposal_responder_id(proposal) == "retailer_a"
    trade = accept_trade_proposal(
        state,
        proposal_id=proposal.proposal_id,
        responder_id="retailer_a",
    )
    assert trade.initiator_id == "roaster"
    assert trade.seller_id == "retailer_a"


def test_invalid_initiator_and_non_owner_are_rejected() -> None:
    config = _config()
    state = create_initial_market_state(config)

    with pytest.raises(InvalidActionError, match="invalid_trade_initiator"):
        create_trade_proposal(
            state,
            config=config,
            initiator_id="retailer_b",
            seller_id="roaster",
            buyer_id="retailer_a",
            lot_id="LOT-001",
            quantity=100,
            unit_price=9.0,
        )
    with pytest.raises(InvalidActionError, match="seller_does_not_own_lot"):
        create_trade_proposal(
            state,
            config=config,
            initiator_id="retailer_a",
            seller_id="retailer_a",
            buyer_id="roaster",
            lot_id="LOT-001",
            quantity=100,
            unit_price=9.0,
        )


def test_buyer_cash_is_rechecked_at_acceptance() -> None:
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
    state.agents["retailer_a"].cash = 0.0

    with pytest.raises(InvalidActionError, match="buyer_insufficient_cash"):
        accept_trade_proposal(
            state,
            proposal_id=proposal.proposal_id,
            responder_id="retailer_a",
        )
    assert proposal.status == "pending"
    assert state.trade_history == []


def test_common_counteroffer_preserves_trade_direction() -> None:
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
    trade = respond_to_trade_counteroffer(
        state,
        counteroffer_id=counteroffer.counteroffer_id,
        responder_id="roaster",
        accept=True,
    )

    assert trade is not None
    assert (trade.seller_id, trade.buyer_id) == ("roaster", "retailer_a")
    assert trade.unit_price == 9.2
    assert counteroffer.status == "accepted"
    assert counteroffer.close_reason == "counteroffer_accepted"


def test_consumer_sale_invalidates_active_negotiation() -> None:
    config = _config()
    state = create_initial_market_state(config)
    _sell_to_retailer(state, config)
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
    counteroffer = create_trade_counteroffer(
        state,
        proposal_id=proposal.proposal_id,
        initiator_id="retailer_a",
        unit_price=9.2,
    )

    result = sell_to_consumer_market(
        state,
        seller_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        consumer_market_enabled=True,
        consumer_unit_price=9.5,
        consumer_daily_demand_capacity=100,
    )

    assert result.status == "accepted"
    assert proposal.status == "cancelled"
    assert proposal.close_reason == "lot_sold_to_consumer"
    assert counteroffer.status == "invalidated"
    assert counteroffer.close_reason == "lot_sold_to_consumer"


def test_duplicate_proposal_for_locked_lot_is_rejected() -> None:
    config = _config()
    state = create_initial_market_state(config)
    _sell_to_retailer(state, config)
    create_trade_proposal(
        state,
        config=config,
        initiator_id="retailer_a",
        seller_id="retailer_a",
        buyer_id="roaster",
        lot_id="LOT-001",
        quantity=100,
        unit_price=9.1,
    )

    with pytest.raises(InvalidActionError, match="lot_locked:active_proposal"):
        create_trade_proposal(
            state,
            config=config,
            initiator_id="roaster",
            seller_id="retailer_a",
            buyer_id="roaster",
            lot_id="LOT-001",
            quantity=100,
            unit_price=9.2,
        )


def test_buyer_initiated_counteroffer_uses_same_handler() -> None:
    config = _config()
    state = create_initial_market_state(config)
    _sell_to_retailer(state, config)
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
    counteroffer = create_trade_counteroffer(
        state,
        proposal_id=proposal.proposal_id,
        initiator_id="retailer_a",
        unit_price=9.3,
    )

    trade = respond_to_trade_counteroffer(
        state,
        counteroffer_id=counteroffer.counteroffer_id,
        responder_id="roaster",
        accept=True,
    )

    assert trade is not None
    assert trade.initiator_id == "roaster"
    assert (trade.seller_id, trade.buyer_id) == ("retailer_a", "roaster")
    assert trade.unit_price == 9.3
    assert counteroffer.close_reason == "counteroffer_accepted"


def test_rejecting_original_proposal_closes_pending_counteroffer() -> None:
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

    reject_trade_proposal(
        state,
        proposal_id=proposal.proposal_id,
        responder_id="retailer_a",
    )

    assert proposal.status == "rejected"
    assert counteroffer.status == "cancelled"
    assert counteroffer.close_reason == "original_proposal_rejected"


class _AcceptIncoming:
    def choose_action(self, observation):
        incoming = observation["incoming_trade_proposals"]
        if incoming:
            return AgentAction(
                action_type="accept_trade",
                proposal_id=incoming[0]["proposal_id"],
            )
        return AgentAction(action_type="wait")


class _BuyerInitiatedCycle:
    def choose_action(self, observation):
        inventory = observation["self"]["inventory"]
        if observation["day"] == 1 and "LOT-001" in inventory:
            return AgentAction(
                action_type="propose_trade",
                seller_id="roaster",
                buyer_id="retailer_a",
                lot_id="LOT-001",
                quantity=100,
                unit_price=9.0,
            )
        buy = observation["trade_candidates"]["buy"]
        if observation["day"] == 2 and buy:
            candidate = buy[0]
            return AgentAction(
                action_type="propose_trade",
                **candidate,
                unit_price=9.1,
            )
        return AgentAction(action_type="wait")


class _RetailerInitiatedSale:
    def choose_action(self, observation):
        incoming = observation["incoming_trade_proposals"]
        if incoming:
            return AgentAction(
                action_type="accept_trade",
                proposal_id=incoming[0]["proposal_id"],
            )
        inventory = observation["self"]["inventory"]
        if observation["day"] == 2 and "LOT-001" in inventory:
            return AgentAction(
                action_type="propose_trade",
                seller_id="retailer_a",
                buyer_id="roaster",
                lot_id="LOT-001",
                quantity=100,
                unit_price=9.1,
            )
        return AgentAction(action_type="wait")


class _InitialSeller:
    def choose_action(self, observation):
        incoming = observation["incoming_trade_proposals"]
        if incoming:
            return AgentAction(
                action_type="accept_trade",
                proposal_id=incoming[0]["proposal_id"],
            )
        inventory = observation["self"]["inventory"]
        if observation["day"] == 1 and "LOT-001" in inventory:
            return AgentAction(
                action_type="propose_trade",
                seller_id="roaster",
                buyer_id="retailer_a",
                lot_id="LOT-001",
                quantity=100,
                unit_price=9.0,
            )
        return AgentAction(action_type="wait")


def test_buyer_initiated_proposal_is_answered_on_normal_agent_turn(tmp_path) -> None:
    config = _config()
    config.max_days = 2
    result = SimulationRunner(
        config,
        {
            "roaster": _BuyerInitiatedCycle(),
            "retailer_a": _AcceptIncoming(),
            "retailer_b": WaitPolicy(),
        },
        run_id="buyer_initiated",
        output_root=tmp_path,
    ).run()

    assert result.metrics["trades"]["agent"] == 2
    assert result.metrics["cycle"]["detected"] is True
    assert result.trade_logs[-1].initiator_id == "roaster"
    assert result.trade_logs[-1].seller_id == "retailer_a"


def test_retailer_initiated_proposal_is_answered_on_normal_agent_turn(tmp_path) -> None:
    config = _config()
    config.max_days = 3
    result = SimulationRunner(
        config,
        {
            "roaster": _InitialSeller(),
            "retailer_a": _RetailerInitiatedSale(),
            "retailer_b": WaitPolicy(),
        },
        run_id="retailer_initiated",
        output_root=tmp_path,
    ).run()

    assert result.metrics["trades"]["agent"] == 2
    assert result.metrics["cycle"]["detected"] is True
    assert result.trade_logs[-1].initiator_id == "retailer_a"
    assert result.trade_logs[-1].buyer_id == "roaster"
    proposal_events = [
        json.loads(line)
        for line in (result.output_dir / "proposals.jsonl")
        .read_text(encoding="utf-8")
        .splitlines()
    ]
    retailer_created = next(
        row
        for row in proposal_events
        if row["event_type"] == "proposal_created"
        and row["initiator_id"] == "retailer_a"
    )
    assert retailer_created["seller_id"] == "retailer_a"
    assert retailer_created["buyer_id"] == "roaster"
    assert any(
        row["event_type"] == "proposal_accepted"
        and row["proposal_id"] == retailer_created["proposal_id"]
        for row in proposal_events
    )


def test_retailer_to_retailer_channel_is_enabled_for_three_agent_baseline() -> None:
    assert _config().agent_trade_channels["retailer_to_retailer"] is True
