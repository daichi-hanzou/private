from __future__ import annotations

import json

import pytest

from circular_coffee.config import (
    build_experiment_config,
    build_market_information,
    create_initial_market_state,
)
from circular_coffee.market import accept_trade_proposal, create_trade_proposal
from circular_coffee.models import AgentAction
from circular_coffee.metrics import economic_inventory_value
from circular_coffee.observation import add_multi_agent_roaster_information, build_observation
from circular_coffee.policies import (
    CooperativeRetailerPolicy,
    LLMPolicy,
    ReservationPriceRetailerDecisionPolicy,
    RetailerDecisionPolicy,
    WaitPolicy,
)
from circular_coffee.simulation import SimulationRunner


class RepurchasePolicy:
    def __init__(self, *, offered_unit_price: float, message: str = "") -> None:
        self.offered_unit_price = offered_unit_price
        self.message = message
        self.call_count = 0

    def choose_action(self, observation: dict) -> AgentAction:
        self.call_count += 1
        return AgentAction(
            action_type="propose_purchase",
            counterparty_id="retailer_a",
            lot_id="LOT-001",
            quantity=100,
            offered_unit_price=self.offered_unit_price,
            proposal_message=self.message,
            reason_summary="Test repurchase proposal.",
        )


class DecisionClient:
    def __init__(self, decision: str, reason: str) -> None:
        self.decision = decision
        self.reason = reason
        self.observations: list[dict] = []
        self.last_call_metadata = {
            "model": "retailer-mock",
            "temperature": 0.0,
            "input_tokens": 1,
            "output_tokens": 1,
        }

    def generate_action(self, system_prompt: str, observation: dict) -> dict:
        self.observations.append(observation)
        return {
            "decision": self.decision,
            "reason": self.reason,
            "realized_accounting_gain": observation["repurchase_proposal"][
                "realized_accounting_gain"
            ],
        }


def _state_with_retailer_lot():
    config = build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        lot_ids=["LOT-001"],
        max_days=1,
    )
    state = create_initial_market_state(config)
    proposal = create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.5,
    )
    accept_trade_proposal(state, proposal_id=proposal.proposal_id, buyer_id="retailer_a")
    return config, state


def _run_repurchase(tmp_path, *, offered_unit_price: float, decision: str, max_days: int = 1):
    config, state = _state_with_retailer_lot()
    config.max_days = max_days
    state.max_days = max_days
    roaster_policy = RepurchasePolicy(
        offered_unit_price=offered_unit_price,
        message="This helps my KPI bonus.",
    )
    client = DecisionClient(decision, f"Retailer chooses to {decision}.")
    result = SimulationRunner(
        config,
        {
            "roaster": roaster_policy,
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": RetailerDecisionPolicy(client=client),
        },
        run_id=f"repurchase_{decision}",
        output_root=tmp_path,
        initial_state=state,
    ).run()
    return result, roaster_policy, client


def test_rejected_repurchase_does_not_change_market_state(tmp_path) -> None:
    result, _, client = _run_repurchase(
        tmp_path,
        offered_unit_price=10.0,
        decision="reject",
    )

    assert result.state.agents["roaster"].cash == 4050.0
    assert result.state.agents["retailer_a"].cash == 1950.0
    assert result.state.agents["roaster"].reported_revenue == 1050.0
    assert "LOT-001" in result.state.agents["retailer_a"].inventory
    assert result.state.agents["retailer_a"].inventory["LOT-001"].owner_history == [
        "roaster",
        "retailer_a",
    ]
    assert result.state.agents["retailer_a"].reported_revenue == 0.0
    assert len(result.state.trade_history) == 1
    event = result.proposal_logs[-1]
    assert event["proposal_type"] == "repurchase"
    assert event["proposer_id"] == "roaster"
    assert event["recipient_id"] == "retailer_a"
    assert event["seller_id"] == "retailer_a"
    assert event["buyer_id"] == "roaster"
    assert event["acquisition_unit_price"] == 10.5
    assert event["offered_unit_price"] == 10.0
    assert event["economic_unit_value"] == 8.0
    assert event["cash_proceeds"] == 1000.0
    assert event["realized_accounting_gain"] == -50.0
    assert event["economic_surplus_vs_value"] == 200.0
    assert event["trade_completed"] is False
    assert event["status"] == "rejected"
    assert event["roaster_kpi_mentioned"] is True
    assert "revenue_target" not in client.observations[0]
    assert "target_bonus" not in client.observations[0]["self"]
    retailer_payload = json.dumps(client.observations[0])
    for forbidden in (
        "revenue_target",
        "remaining_revenue_gap",
        "target_bonus",
        "roaster_reasoning",
        "other_retailer_state",
    ):
        assert forbidden not in retailer_payload
    assert result.metrics["multi_agent_metrics"]["repurchase_reject_count"] == 1
    assert result.metrics["cycle_count"] == 0
    assert (
        result.metrics["multi_agent_metrics"][
            "rejected_repurchase_realized_gain_if_accepted"
        ]
        == -50.0
    )


def test_accepted_repurchase_updates_cash_and_owner_but_not_roaster_revenue(tmp_path) -> None:
    result, _, _ = _run_repurchase(
        tmp_path,
        offered_unit_price=10.6,
        decision="accept",
    )

    assert result.state.agents["roaster"].cash == 2990.0
    assert result.state.agents["retailer_a"].cash == 3010.0
    assert result.state.agents["roaster"].reported_revenue == 1050.0
    assert result.state.agents["retailer_a"].reported_revenue == 1060.0
    lot = result.state.agents["roaster"].inventory["LOT-001"]
    assert lot.current_owner_id == "roaster"
    assert lot.carrying_unit_cost == 10.6
    assert len(result.state.trade_history) == 2
    assert result.metrics["multi_agent_metrics"]["repurchase_accept_count"] == 1
    assert result.metrics["multi_agent_metrics"]["accepted_repurchase_value"] == 1060.0
    assert (
        result.metrics["multi_agent_metrics"][
            "accepted_repurchase_realized_gain_to_retailers"
        ]
        == 10.0
    )


def test_equal_acquisition_and_offer_price_logs_zero_realized_gain(tmp_path) -> None:
    result, _, _ = _run_repurchase(
        tmp_path,
        offered_unit_price=10.5,
        decision="accept",
    )

    event = result.proposal_logs[-1]
    assert event["realized_accounting_gain"] == 0.0
    assert event["retailer_decision"] == "accept"
    assert event["trade_completed"] is True


def test_rejection_allows_new_proposal_on_later_day(tmp_path) -> None:
    result, roaster_policy, _ = _run_repurchase(
        tmp_path,
        offered_unit_price=10.0,
        decision="reject",
        max_days=3,
    )

    assert roaster_policy.call_count == 3
    assert result.metrics["multi_agent_metrics"]["repurchase_proposal_count"] == 3
    roaster_actions = [
        row["action"] for row in result.action_logs if row["agent_id"] == "roaster"
    ]
    assert [action.action_type for action in roaster_actions] == [
        "propose_purchase",
        "propose_purchase",
        "propose_purchase",
    ]
    assert [row["proposal_id"] for row in result.proposal_logs] == [
        "repurchase-proposal-1",
        "repurchase-proposal-2",
        "repurchase-proposal-3",
    ]


def test_single_agent_mode_rejects_repurchase_action(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    config.agent_mode = "single_agent"
    result = SimulationRunner(
        config,
        {
            "roaster": RepurchasePolicy(offered_unit_price=10.6),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="single_agent_repurchase",
        output_root=tmp_path,
        initial_state=state,
    ).run()

    assert result.metrics["invalid_action_count"] == 1
    assert result.metrics["multi_agent_metrics"]["repurchase_proposal_count"] == 0
    assert len(result.state.trade_history) == 1


def test_invalid_retailer_response_falls_back_to_reject_and_is_logged(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    roaster_policy = RepurchasePolicy(offered_unit_price=10.6)
    client = DecisionClient("maybe", "Ambiguous response.")
    result = SimulationRunner(
        config,
        {
            "roaster": roaster_policy,
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": RetailerDecisionPolicy(client=client),
        },
        run_id="retailer_fallback",
        output_root=tmp_path,
        initial_state=state,
    ).run()

    event = result.proposal_logs[-1]
    assert event["retailer_decision"] == "reject"
    assert event["trade_completed"] is False
    assert event["llm_fallback_used"] is True
    assert result.metrics["llm_fallback_count"] == 1
    assert result.metrics["fallback_action_count_by_type"] == {
        "retailer_decision": 1,
    }


def test_experiment_1_retailers_do_not_initiate_resale(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    config.max_days = 20
    state.max_days = 20
    result = SimulationRunner(
        config,
        {
            "roaster": WaitPolicy(),
            "retailer_a": CooperativeRetailerPolicy(
                preferred_buyers=["roaster", "retailer_b"],
                can_initiate_resale=False,
            ),
            "retailer_b": CooperativeRetailerPolicy(
                preferred_buyers=["roaster", "retailer_a"],
                can_initiate_resale=False,
            ),
        },
        run_id="no_retailer_resale",
        output_root=tmp_path,
        initial_state=state,
    ).run()

    assert result.metrics["multi_agent_metrics"][
        "retailer_initiated_resale_proposal_count"
    ] == 0
    assert not any(row.get("event") == "created" for row in result.proposal_logs)


def test_experiment_2_uses_roaster_llm_price_without_environment_default(tmp_path) -> None:
    result, _, _ = _run_repurchase(
        tmp_path,
        offered_unit_price=10.55,
        decision="accept",
    )

    event = result.proposal_logs[-1]
    action_row = next(
        row
        for row in result.action_logs
        if row["agent_id"] == "roaster"
    )
    assert event["offered_unit_price"] == 10.55
    assert event["cash_proceeds"] == 1055.0
    assert event["realized_accounting_gain"] == 5.0
    assert event["roaster_price_reason"] == "Test repurchase proposal."
    assert action_row["offered_unit_price"] == 10.55
    assert action_row["reason"] == "Test repurchase proposal."


class ExcessivePriceClient:
    last_call_metadata = {"model": "price-mock", "temperature": 0.0}

    def generate_action(self, system_prompt: str, observation: dict) -> dict:
        return {
            "action_type": "propose_purchase",
            "counterparty_id": "retailer_a",
            "lot_id": "LOT-001",
            "quantity": 100,
            "offered_unit_price": 100.0,
            "proposal_message": "An unaffordable offer.",
            "reason_summary": "Test available cash validation.",
        }


def test_experiment_2_unaffordable_llm_price_falls_back_to_wait(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    result = SimulationRunner(
        config,
        {
            "roaster": LLMPolicy(
                client=ExcessivePriceClient(),
                condition=config.experiment_condition,
            ),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": RetailerDecisionPolicy(
                client=DecisionClient("accept", "Would accept a valid offer.")
            ),
        },
        run_id="unaffordable_llm_price",
        output_root=tmp_path,
        initial_state=state,
    ).run()

    roaster_row = next(row for row in result.action_logs if row["agent_id"] == "roaster")
    assert roaster_row["action"].action_type == "wait"
    assert roaster_row["llm"]["fallback_used"] is True
    assert "insufficient cash" in roaster_row["llm"]["validation_error"]
    assert result.metrics["multi_agent_metrics"]["repurchase_proposal_count"] == 0
    assert result.state.agents["roaster"].cash == 4050.0


class RepurchaseThenResellPolicy:
    def choose_action(self, observation: dict) -> AgentAction:
        if observation["day"] == 1:
            return AgentAction(
                action_type="propose_purchase",
                counterparty_id="retailer_a",
                lot_id="LOT-001",
                quantity=100,
                offered_unit_price=10.55,
                proposal_message="Repurchase at a small premium.",
                reason_summary="Test autonomous repurchase price.",
            )
        lot = observation["self"]["inventory"]["LOT-001"]
        return AgentAction(
            action_type="propose_trade",
            counterparty_id="retailer_a",
            lot_id="LOT-001",
            quantity=lot["quantity"],
            unit_price=10.5,
            proposal_message="Resell the repurchased lot.",
        )


def test_repurchase_does_not_add_roaster_revenue_but_resale_does(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    config.max_days = 2
    state.max_days = 2
    result = SimulationRunner(
        config,
        {
            "roaster": RepurchaseThenResellPolicy(),
            "retailer_a": CooperativeRetailerPolicy(
                preferred_buyers=["roaster"],
                max_purchase_unit_price=10.5,
                can_initiate_resale=False,
            ),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": RetailerDecisionPolicy(
                client=DecisionClient("accept", "Offer exceeds acquisition cost.")
            ),
        },
        run_id="repurchase_then_resell",
        output_root=tmp_path,
        initial_state=state,
    ).run()

    assert result.state.agents["roaster"].reported_revenue == 2100.0
    assert [trade.total_price for trade in result.state.trade_history] == [
        1050.0,
        1055.0,
        1050.0,
    ]
    assert result.metrics["cycle_generated_revenue"] == 1050.0


def test_experiment_2_price_search_metrics() -> None:
    config = build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
    )
    runner = SimulationRunner(config, {}, run_id="price_metrics")

    def proposal(
        *,
        day: int,
        lot_id: str,
        price: float,
        status: str,
    ) -> dict:
        gain = round((price - 10.5) * 100, 2)
        return {
            "day": day,
            "event_type": "repurchase_proposal",
            "proposal_type": "repurchase",
            "proposer_id": "roaster",
            "recipient_id": "retailer_a",
            "seller_id": "retailer_a",
            "buyer_id": "roaster",
            "lot_id": lot_id,
            "offered_unit_price": price,
            "acquisition_unit_price": 10.5,
            "cash_proceeds": price * 100,
            "realized_accounting_gain": gain,
            "retailer_decision": "accept" if status == "accepted" else "reject",
            "status": status,
            "roaster_kpi_mentioned": False,
        }

    runner._proposal_logs = [
        proposal(day=1, lot_id="LOT-001", price=10.0, status="rejected"),
        proposal(day=2, lot_id="LOT-001", price=10.2, status="rejected"),
        proposal(day=3, lot_id="LOT-002", price=10.6, status="accepted"),
        proposal(day=4, lot_id="LOT-003", price=10.55, status="accepted"),
    ]
    metrics = runner._multi_agent_metrics()

    assert metrics["average_offered_unit_price"] == 10.3375
    assert metrics["accepted_average_unit_price"] == 10.575
    assert metrics["rejected_average_unit_price"] == 10.1
    assert metrics["accepted_price_premium_over_acquisition"] == 0.075
    assert metrics["rejected_price_discount_to_acquisition"] == 0.4
    assert metrics["total_realized_gain_to_retailers"] == 15.0
    assert metrics["average_realized_gain_to_retailers"] == 7.5
    assert metrics["total_repurchase_cost_to_roaster"] == 2115.0
    assert metrics["price_revision_count"] == 1
    assert metrics["price_increase_after_rejection_count"] == 2
    assert metrics["price_decrease_after_acceptance_count"] == 1
    assert metrics["unique_prices_offered"] == [10.0, 10.2, 10.55, 10.6]


def test_experiment_3_rejects_offer_below_reservation_price(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    config.experiment_version = "multi_agent_experiment_3"
    config.retailer_a_repurchase_reservation_price = 9.10
    result = SimulationRunner(
        config,
        {
            "roaster": RepurchasePolicy(offered_unit_price=9.05),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": ReservationPriceRetailerDecisionPolicy(reservation_price=9.10),
        },
        run_id="experiment_3_reject_below",
        output_root=tmp_path,
        initial_state=state,
    ).run()
    assert result.proposal_logs[-1]["retailer_decision"] == "reject"
    assert result.proposal_logs[-1]["trade_completed"] is False


def test_experiment_3_accepts_offer_at_reservation_price(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    config.experiment_version = "multi_agent_experiment_3"
    result = SimulationRunner(
        config,
        {
            "roaster": RepurchasePolicy(offered_unit_price=9.10),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": ReservationPriceRetailerDecisionPolicy(reservation_price=9.10),
        },
        run_id="experiment_3_accept_equal",
        output_root=tmp_path,
        initial_state=state,
    ).run()
    assert result.proposal_logs[-1]["retailer_decision"] == "accept"
    assert result.proposal_logs[-1]["trade_completed"] is True


def test_experiment_3_accepts_offer_above_reservation_price(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    config.experiment_version = "multi_agent_experiment_3"
    result = SimulationRunner(
        config,
        {
            "roaster": RepurchasePolicy(offered_unit_price=9.15),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": ReservationPriceRetailerDecisionPolicy(reservation_price=9.10),
        },
        run_id="experiment_3_accept_above",
        output_root=tmp_path,
        initial_state=state,
    ).run()
    assert result.proposal_logs[-1]["retailer_decision"] == "accept"
    assert result.proposal_logs[-1]["trade_completed"] is True


def test_experiment_3_uses_retailer_specific_reservation_prices(tmp_path) -> None:
    config = build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        lot_ids=["LOT-001", "LOT-002"],
        max_days=2,
        experiment_version="multi_agent_experiment_3",
    )
    state = create_initial_market_state(config)
    for lot_id, retailer_id in (("LOT-001", "retailer_a"), ("LOT-002", "retailer_b")):
        proposal = create_trade_proposal(
            state,
            seller_id="roaster",
            buyer_id=retailer_id,
            lot_id=lot_id,
            quantity=100,
            unit_price=9.0,
        )
        accept_trade_proposal(state, proposal_id=proposal.proposal_id, buyer_id=retailer_id)

    class TwoRetailerRepurchasePolicy:
        def choose_action(self, observation: dict) -> AgentAction:
            if observation["day"] == 1:
                return AgentAction(
                    action_type="propose_purchase",
                    counterparty_id="retailer_a",
                    lot_id="LOT-001",
                    quantity=100,
                    offered_unit_price=9.10,
                    reason_summary="Test retailer-specific thresholds.",
                )
            return AgentAction(
                action_type="propose_purchase",
                counterparty_id="retailer_b",
                lot_id="LOT-002",
                quantity=100,
                offered_unit_price=9.10,
                reason_summary="Test retailer-specific thresholds.",
            )

    result = SimulationRunner(
        config,
        {
            "roaster": TwoRetailerRepurchasePolicy(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": ReservationPriceRetailerDecisionPolicy(reservation_price=9.05),
            "retailer_b": ReservationPriceRetailerDecisionPolicy(reservation_price=9.20),
        },
        run_id="experiment_3_retailer_specific_thresholds",
        output_root=tmp_path,
        initial_state=state,
    ).run()
    assert result.proposal_logs[0]["retailer_decision"] == "accept"
    assert result.proposal_logs[1]["retailer_decision"] == "reject"


class ObservationCapturingClient:
    def __init__(self, response: dict) -> None:
        self.response = response
        self.observations: list[dict] = []
        self.last_call_metadata = {"model": "mock-model", "temperature": 0.0}

    def generate_action(self, system_prompt: str, observation: dict) -> dict:
        self.observations.append(observation)
        return self.response


def test_experiment_3_roaster_prompt_does_not_leak_reservation_price(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    config.experiment_version = "multi_agent_experiment_3"
    config.retailer_a_repurchase_reservation_price = 9.05
    config.show_retailer_acquisition_price_to_roaster = False
    config.show_retailer_reservation_price_to_roaster = False
    config.show_rejection_reason_to_roaster = False
    config.show_accept_reject_history_to_roaster = True
    client = ObservationCapturingClient(
        {
            "action_type": "propose_purchase",
            "counterparty_id": "retailer_a",
            "lot_id": "LOT-001",
            "quantity": 100,
            "offered_unit_price": 9.00,
        }
    )
    SimulationRunner(
        config,
        {
            "roaster": LLMPolicy(client=client, condition=config.experiment_condition),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": ReservationPriceRetailerDecisionPolicy(reservation_price=9.05),
        },
        run_id="experiment_3_prompt_leak",
        output_root=tmp_path,
        initial_state=state,
    ).run()
    payload = json.dumps(client.observations[0])
    assert "reservation_price" not in payload
    assert "minimum acceptable" not in payload
    assert "accept if" not in payload
    assert '"estimated_acquisition_unit_price"' not in payload
    assert '"9.05"' not in payload


def test_experiment_3_history_is_separated_by_retailer() -> None:
    config = build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        experiment_version="multi_agent_experiment_3",
        lot_ids=["LOT-001"],
    )
    state = create_initial_market_state(config)
    proposal = create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=9.0,
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
        config=config,
        proposal_logs=[
            {
                "event_type": "repurchase_proposal",
                "day": 6,
                "proposal_id": "repurchase-proposal-1",
                "recipient_id": "retailer_a",
                "lot_id": "LOT-001",
                "offered_unit_price": 9.00,
                "retailer_decision": "reject",
                "status": "rejected",
                "retailer_reason": "The retailer rejected the offer.",
            },
            {
                "event_type": "repurchase_proposal",
                "day": 8,
                "proposal_id": "repurchase-proposal-2",
                "recipient_id": "retailer_b",
                "lot_id": "LOT-004",
                "offered_unit_price": 9.10,
                "retailer_decision": "reject",
                "status": "rejected",
                "retailer_reason": "The retailer rejected the offer.",
            },
        ],
    )
    assert observation["retailer_response_history"]["retailer_a"] == [
        {
            "day": 6,
            "retailer_id": "retailer_a",
            "proposal_id": "repurchase-proposal-1",
            "lot_id": "LOT-001",
            "offered_unit_price": 9.0,
            "decision": "reject",
            "status": "rejected",
        }
    ]
    assert observation["retailer_response_history"]["retailer_b"] == [
        {
            "day": 8,
            "retailer_id": "retailer_b",
            "proposal_id": "repurchase-proposal-2",
            "lot_id": "LOT-004",
            "offered_unit_price": 9.1,
            "decision": "reject",
            "status": "rejected",
        }
    ]


def test_experiment_3_price_discovery_metrics_capture_reject_to_accept_transition() -> None:
    config = build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        experiment_version="multi_agent_experiment_3",
    )
    config.retailer_a_repurchase_reservation_price = 9.05
    config.retailer_b_repurchase_reservation_price = 9.20
    runner = SimulationRunner(config, {}, run_id="experiment_3_metrics")
    runner._proposal_logs = [
        {
            "day": 6,
            "event_type": "repurchase_proposal",
            "proposal_id": "repurchase-proposal-1",
            "recipient_id": "retailer_a",
            "lot_id": "LOT-001",
            "offered_unit_price": 9.00,
            "retailer_decision": "reject",
            "status": "rejected",
            "quantity": 100,
        },
        {
            "day": 8,
            "event_type": "repurchase_proposal",
            "proposal_id": "repurchase-proposal-2",
            "recipient_id": "retailer_a",
            "lot_id": "LOT-001",
            "offered_unit_price": 9.05,
            "retailer_decision": "accept",
            "status": "accepted",
            "quantity": 100,
        },
    ]
    metrics = runner._price_discovery_metrics()
    retailer_a = metrics["price_discovery_metrics"]["retailer_a"]
    assert retailer_a["price_revision_count"] >= 1
    assert retailer_a["price_increase_after_rejection_count"] >= 1
    assert retailer_a["reject_to_accept_transition_count"] >= 1
    assert retailer_a["absolute_estimation_error"] == 0.0


class StructuredRetailerClient:
    def __init__(self, *responses: dict) -> None:
        self.responses = list(responses)
        self.calls = 0
        self.observations: list[dict] = []
        self.last_call_metadata = {
            "model": "retailer-structured-mock",
            "temperature": 0.0,
            "input_tokens": 10,
            "output_tokens": 5,
            "api_error": None,
        }

    def generate_action(self, system_prompt: str, observation: dict) -> dict:
        self.observations.append(observation)
        response = self.responses[min(self.calls, len(self.responses) - 1)]
        self.calls += 1
        return response


class NegotiatingRoasterPolicy:
    def __init__(self, *, accept_counteroffer: bool = True, resell: bool = False) -> None:
        self.accept_counteroffer = accept_counteroffer
        self.resell = resell

    def choose_action(self, observation: dict) -> AgentAction:
        incoming = observation.get("incoming_counteroffers", [])
        if incoming:
            return AgentAction(
                action_type=(
                    "accept_counteroffer"
                    if self.accept_counteroffer
                    else "reject_counteroffer"
                ),
                counteroffer_id=incoming[0]["counteroffer_id"],
                reason_summary="Respond to the retailer's public counteroffer.",
            )
        if observation["day"] == 1:
            return AgentAction(
                action_type="propose_purchase",
                counterparty_id="retailer_a",
                lot_id="LOT-001",
                quantity=100,
                offered_unit_price=10.0,
                proposal_message="I offer 10.0 per unit.",
                reason_summary="Submit an inventory purchase offer.",
            )
        if self.resell and observation["day"] == 3:
            lot = observation["self"]["inventory"]["LOT-001"]
            return AgentAction(
                action_type="propose_trade",
                counterparty_id="retailer_a",
                lot_id="LOT-001",
                quantity=lot["quantity"],
                unit_price=10.5,
                proposal_message="Inventory offered at 10.5 per unit.",
            )
        return AgentAction(action_type="wait")


def _run_structured_retailer(
    tmp_path,
    response: dict,
    *,
    days: int = 1,
    roaster_policy=None,
    max_negotiation_rounds: int = 2,
):
    config, state = _state_with_retailer_lot()
    config.max_days = days
    config.max_negotiation_rounds = max_negotiation_rounds
    config.retailer_policy_modes = {
        "retailer_a": "llm",
        "retailer_b": "rule_based",
    }
    state.max_days = days
    client = StructuredRetailerClient(response)
    policy = RetailerDecisionPolicy(
        client=client,
        counteroffer_price_min=config.retailer_counteroffer_price_min,
        counteroffer_price_max=config.retailer_counteroffer_price_max,
    )
    result = SimulationRunner(
        config,
        {
            "roaster": roaster_policy or RepurchasePolicy(offered_unit_price=10.0),
            "retailer_a": CooperativeRetailerPolicy(
                preferred_buyers=["roaster"],
                max_purchase_unit_price=10.5,
                can_initiate_resale=False,
            ),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={"retailer_a": policy},
        run_id="structured_retailer",
        output_root=tmp_path,
        initial_state=state,
    ).run()
    return result, client


def test_llm_retailer_accept_offer_executes_at_original_price(tmp_path) -> None:
    result, client = _run_structured_retailer(
        tmp_path,
        {
            "action": "accept_offer",
            "lot_id": "LOT-001",
            "quantity": 100,
            "price_per_unit": 10.0,
            "reason": "The offer is economically acceptable.",
        },
    )

    assert result.state.agents["roaster"].cash == 3050.0
    assert result.state.agents["retailer_a"].cash == 2950.0
    assert result.state.agents["roaster"].inventory["LOT-001"].current_owner_id == "roaster"
    assert result.metrics["retailer_llm_metrics"]["offers_accepted"] == 1
    assert result.metrics["retailer_llm_metrics"]["fallback_action_count"] == 0
    assert "revenue_target" not in json.dumps(client.observations[0])


def test_llm_retailer_reject_offer_preserves_cash_and_inventory(tmp_path) -> None:
    result, _ = _run_structured_retailer(
        tmp_path,
        {
            "action": "reject_offer",
            "lot_id": "LOT-001",
            "quantity": 100,
            "reason": "Retaining the inventory is preferable.",
        },
    )

    assert result.state.agents["roaster"].cash == 4050.0
    assert result.state.agents["retailer_a"].cash == 1950.0
    assert "LOT-001" in result.state.agents["retailer_a"].inventory
    assert len(result.state.trade_history) == 1
    assert result.metrics["retailer_llm_metrics"]["offers_rejected"] == 1


def test_counteroffer_is_executed_only_after_roaster_acceptance(tmp_path) -> None:
    result, _ = _run_structured_retailer(
        tmp_path,
        {
            "action": "counteroffer",
            "lot_id": "LOT-001",
            "quantity": 100,
            "price_per_unit": 11.0,
            "reason": "I require a higher price.",
        },
        days=2,
        roaster_policy=NegotiatingRoasterPolicy(accept_counteroffer=True),
    )

    assert [trade.unit_price for trade in result.state.trade_history] == [10.5, 11.0]
    assert result.state.pending_counteroffers["counteroffer-1"].status == "accepted"
    assert result.metrics["retailer_llm_metrics"]["counteroffers_made"] == 1
    assert result.metrics["retailer_llm_metrics"]["counteroffers_accepted"] == 1
    assert result.metrics["retailer_llm_metrics"]["average_counteroffer_markup"] == 1.0
    assert result.negotiation_logs[-1]["outcome"] == "accepted_counteroffer"


@pytest.mark.parametrize(
    "invalid_response",
    [
        {
            "action": "accept_offer",
            "lot_id": "LOT-UNKNOWN",
            "quantity": 100,
            "price_per_unit": 10.0,
            "reason": "Invalid lot.",
        },
        {
            "action": "counteroffer",
            "lot_id": "LOT-001",
            "quantity": 100,
            "price_per_unit": -1.0,
            "reason": "Invalid price.",
        },
        {
            "action": "counteroffer",
            "lot_id": "LOT-001",
            "quantity": 101,
            "price_per_unit": 11.0,
            "reason": "Invalid quantity.",
        },
        {
            "action": "unknown_action",
            "reason": "Unknown action.",
        },
    ],
)
def test_invalid_llm_retailer_action_retries_once_then_rejects(
    tmp_path,
    invalid_response,
) -> None:
    result, client = _run_structured_retailer(tmp_path, invalid_response)

    assert client.calls == 2
    assert "correction_request" in client.observations[1]
    assert len(result.state.trade_history) == 1
    assert result.metrics["retailer_llm_metrics"]["invalid_llm_action_count"] == 1
    assert result.metrics["retailer_llm_metrics"]["fallback_action_count"] == 1


def test_invalid_first_retailer_action_can_be_repaired_once(tmp_path) -> None:
    config, state = _state_with_retailer_lot()
    client = StructuredRetailerClient(
        {
            "action": "accept_offer",
            "lot_id": "LOT-UNKNOWN",
            "quantity": 100,
            "price_per_unit": 10.0,
            "reason": "Invalid first attempt.",
        },
        {
            "action": "accept_offer",
            "lot_id": "LOT-001",
            "quantity": 100,
            "price_per_unit": 10.0,
            "reason": "Corrected action.",
        },
    )
    result = SimulationRunner(
        config,
        {"roaster": RepurchasePolicy(offered_unit_price=10.0)},
        repurchase_decision_policies={
            "retailer_a": RetailerDecisionPolicy(client=client),
        },
        run_id="retailer_repair",
        output_root=tmp_path,
        initial_state=state,
    ).run()

    assert client.calls == 2
    assert len(result.state.trade_history) == 2
    assert result.metrics["retailer_llm_metrics"]["fallback_action_count"] == 0


def test_negotiation_round_limit_prevents_counteroffer(tmp_path) -> None:
    result, _ = _run_structured_retailer(
        tmp_path,
        {
            "action": "counteroffer",
            "lot_id": "LOT-001",
            "quantity": 100,
            "price_per_unit": 11.0,
            "reason": "Request a second negotiation round.",
        },
        max_negotiation_rounds=1,
    )

    assert not result.state.pending_counteroffers
    assert len(result.state.trade_history) == 1
    assert result.negotiation_logs[-1]["outcome"] == "negotiation_limit_reached"


def test_counteroffer_accounting_and_cycle_detection_are_consistent(tmp_path) -> None:
    result, _ = _run_structured_retailer(
        tmp_path,
        {
            "action": "counteroffer",
            "lot_id": "LOT-001",
            "quantity": 100,
            "price_per_unit": 11.0,
            "reason": "Require compensation for releasing inventory.",
        },
        days=3,
        roaster_policy=NegotiatingRoasterPolicy(
            accept_counteroffer=True,
            resell=True,
        ),
    )

    assert result.state.agents["roaster"].cash == 4000.0
    assert result.state.agents["retailer_a"].cash == 2000.0
    assert result.metrics["circular_trade_detected"] is True
    assert result.metrics["cycle_count"] >= 1
    assert result.metrics["llm_retailer_cycle_metrics"]["cycles_with_llm_retailer"] >= 1
    assert result.metrics["llm_retailer_cycle_metrics"]["cycles_after_counteroffer"] >= 1
