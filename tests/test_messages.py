from __future__ import annotations

import json

import pytest

from circular_coffee.config import build_experiment_config, create_initial_market_state
from circular_coffee.core_metrics import build_core_metrics_from_run_dir
from circular_coffee.llm_clients import COMMUNICATION_ACTION_JSON_SCHEMA
from circular_coffee.market import (
    InvalidActionError,
    commit_messages,
    validate_communication_action,
)
from circular_coffee.models import AgentAction, CommunicationAction
from circular_coffee.policies import WaitPolicy
from circular_coffee.simulation import SimulationRunner


class FixedCommunicationPolicy:
    def __init__(self, action: CommunicationAction):
        self.action = action
        self.observations: list[dict] = []

    def choose_communication_action(self, observation: dict) -> CommunicationAction:
        self.observations.append(observation)
        return self.action


class CapturingEconomicPolicy:
    def __init__(self, action: AgentAction | None = None):
        self.action = action or AgentAction(action_type="wait")
        self.observations: list[dict] = []

    def choose_action(self, observation: dict) -> AgentAction:
        self.observations.append(observation)
        return self.action


def _communication_config(**overrides):
    return build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        communication_enabled=True,
        lot_ids=["LOT-001"],
        **overrides,
    )


def test_communication_schema_is_separate_from_economic_actions() -> None:
    branches = COMMUNICATION_ACTION_JSON_SCHEMA["schema"]["properties"]["action"][
        "anyOf"
    ]
    action_types = {
        branch["properties"]["action_type"]["enum"][0] for branch in branches
    }

    assert action_types == {"send_message", "no_message"}


def test_message_and_economic_action_can_run_on_same_day(tmp_path) -> None:
    config = _communication_config(max_days=1)
    communication = FixedCommunicationPolicy(
        CommunicationAction(
            action_type="send_message",
            recipient_id="retailer_a",
            message="I may purchase LOT-001 later.",
            related_lot_id="LOT-001",
        )
    )
    roaster_economic = CapturingEconomicPolicy(
        AgentAction(
            action_type="propose_trade",
            seller_id="roaster",
            buyer_id="retailer_a",
            lot_id="LOT-001",
            quantity=100,
            unit_price=9.0,
        )
    )
    retailer_economic = CapturingEconomicPolicy()

    result = SimulationRunner(
        config,
        {
            "roaster": roaster_economic,
            "retailer_a": retailer_economic,
            "retailer_b": WaitPolicy(),
        },
        communication_policies={"roaster": communication},
        run_id="same_day_message_and_trade",
        output_root=tmp_path,
    ).run()

    assert len(result.message_logs) == 1
    assert result.metrics["offers"]["created"] == 1
    assert len(result.action_logs) == 3
    assert len(result.communication_action_logs) == 3
    assert retailer_economic.observations[0]["new_messages_today"][0]["message"] == (
        "I may purchase LOT-001 later."
    )


def test_communication_decisions_are_simultaneous_and_delivered_before_economic_phase(
    tmp_path,
) -> None:
    config = _communication_config(max_days=1)
    roaster_communication = FixedCommunicationPolicy(
        CommunicationAction(
            action_type="send_message",
            recipient_id="retailer_a",
            message="from roaster",
        )
    )
    retailer_communication = FixedCommunicationPolicy(
        CommunicationAction(
            action_type="send_message",
            recipient_id="roaster",
            message="from retailer",
        )
    )
    roaster_economic = CapturingEconomicPolicy()
    retailer_economic = CapturingEconomicPolicy()

    SimulationRunner(
        config,
        {
            "roaster": roaster_economic,
            "retailer_a": retailer_economic,
            "retailer_b": WaitPolicy(),
        },
        communication_policies={
            "roaster": roaster_communication,
            "retailer_a": retailer_communication,
        },
        run_id="simultaneous_messages",
        output_root=tmp_path,
    ).run()

    assert roaster_communication.observations[0]["recent_incoming_messages"] == []
    assert retailer_communication.observations[0]["recent_incoming_messages"] == []
    assert roaster_economic.observations[0]["new_messages_today"][0]["message"] == (
        "from retailer"
    )
    assert retailer_economic.observations[0]["new_messages_today"][0]["message"] == (
        "from roaster"
    )


def test_invalid_message_does_not_remove_economic_action(tmp_path) -> None:
    config = _communication_config(max_days=1)
    invalid_communication = FixedCommunicationPolicy(
        CommunicationAction(
            action_type="send_message",
            recipient_id="unknown",
            message="invalid recipient",
        )
    )

    result = SimulationRunner(
        config,
        {
            "roaster": WaitPolicy(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        communication_policies={"roaster": invalid_communication},
        run_id="invalid_message",
        output_root=tmp_path,
    ).run()

    row = next(
        item
        for item in result.communication_action_logs
        if item["agent_id"] == "roaster"
    )
    assert row["is_valid"] is False
    assert row["executed_action"] == "no_message"
    assert row["llm_fallback_used"] is True
    assert result.message_logs == []
    assert len(result.action_logs) == 3
    assert result.metrics["errors"]["invalid_actions"] == 0


def test_no_message_creates_no_record(tmp_path) -> None:
    config = _communication_config(max_days=1)
    result = SimulationRunner(
        config,
        {
            "roaster": WaitPolicy(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        communication_policies={
            "roaster": FixedCommunicationPolicy(
                CommunicationAction(action_type="no_message")
            )
        },
        run_id="no_message",
        output_root=tmp_path,
    ).run()

    assert result.message_logs == []
    assert all(
        row["executed_action"] == "no_message"
        for row in result.communication_action_logs
    )


def test_message_limits_and_related_ids_are_validated() -> None:
    config = _communication_config(max_message_length=5)
    state = create_initial_market_state(config)
    state.day = 1

    with pytest.raises(InvalidActionError, match="message_exceeds_maximum_length"):
        validate_communication_action(
            state,
            config,
            sender_id="roaster",
            action=CommunicationAction(
                action_type="send_message",
                recipient_id="retailer_a",
                message="123456",
            ),
        )
    with pytest.raises(InvalidActionError, match="unknown_related_lot"):
        validate_communication_action(
            state,
            config,
            sender_id="roaster",
            action=CommunicationAction(
                action_type="send_message",
                recipient_id="retailer_a",
                message="valid",
                related_lot_id="UNKNOWN",
            ),
        )

    commit_messages(
        state,
        config,
        [
            (
                "roaster",
                CommunicationAction(
                    action_type="send_message",
                    recipient_id="retailer_a",
                    message="valid",
                ),
            )
        ],
    )
    with pytest.raises(InvalidActionError, match="daily_message_limit_reached"):
        validate_communication_action(
            state,
            config,
            sender_id="roaster",
            action=CommunicationAction(
                action_type="send_message",
                recipient_id="retailer_b",
                message="again",
            ),
        )


def test_message_body_has_single_log_source_and_metrics_are_independent(
    tmp_path,
) -> None:
    config = _communication_config(max_days=1)
    message_text = "Body stored only in messages.jsonl."
    result = SimulationRunner(
        config,
        {
            "roaster": WaitPolicy(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        communication_policies={
            "roaster": FixedCommunicationPolicy(
                CommunicationAction(
                    action_type="send_message",
                    recipient_id="retailer_a",
                    message=message_text,
                )
            )
        },
        run_id="message_single_source",
        output_root=tmp_path,
    ).run()

    messages_path = result.output_dir / "messages.jsonl"
    assert message_text in messages_path.read_text()
    for filename in (
        "actions.jsonl",
        "communication_actions.jsonl",
        "proposals.jsonl",
        "negotiations.jsonl",
        "trades.jsonl",
        "metrics.json",
        "final_state.json",
    ):
        assert message_text not in (result.output_dir / filename).read_text()

    expected_metrics = json.loads((result.output_dir / "metrics.json").read_text())
    messages_path.unlink()
    assert build_core_metrics_from_run_dir(result.output_dir) == expected_metrics


def test_baseline_and_communication_have_same_economic_action_capacity(
    tmp_path,
) -> None:
    baseline = build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        max_days=2,
    )
    enabled = _communication_config(max_days=2)
    policies = {
        "roaster": WaitPolicy(),
        "retailer_a": WaitPolicy(),
        "retailer_b": WaitPolicy(),
    }

    baseline_result = SimulationRunner(
        baseline,
        policies,
        run_id="baseline_capacity",
        output_root=tmp_path,
    ).run()
    enabled_result = SimulationRunner(
        enabled,
        policies,
        run_id="communication_capacity",
        output_root=tmp_path,
    ).run()

    assert len(baseline_result.action_logs) == 6
    assert len(enabled_result.action_logs) == 6
    assert baseline_result.communication_action_logs == []
    assert len(enabled_result.communication_action_logs) == 6
