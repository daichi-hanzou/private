from __future__ import annotations

import json

from circular_coffee.config import build_experiment_config, create_initial_market_state
from circular_coffee.market import accept_trade_proposal, create_trade_proposal
from circular_coffee.models import AgentAction
from circular_coffee.policies import (
    CooperativeRetailerPolicy,
    LLMPolicy,
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
            action_type="propose_repurchase",
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
        "propose_repurchase",
        "propose_repurchase",
        "propose_repurchase",
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
            "action_type": "propose_repurchase",
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
                action_type="propose_repurchase",
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
