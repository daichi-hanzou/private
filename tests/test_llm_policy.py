from __future__ import annotations

import json

import pytest

from circular_coffee.config import build_experiment_config
from circular_coffee.llm_clients import ACTION_JSON_SCHEMA
from circular_coffee.models import AgentAction
from circular_coffee.policies import (
    CooperativeRetailerPolicy,
    LLMPolicy,
    RETAILER_DECISION_SYSTEM_PROMPT,
    WaitPolicy,
)
from circular_coffee.simulation import SimulationRunner


VALID_WAIT = {
    "action_type": "wait",
    "counterparty_id": None,
    "proposal_id": None,
    "lot_id": None,
    "quantity": None,
    "unit_price": None,
    "proposal_message": None,
    "reason_summary": "Wait for another opportunity.",
}


class MockClient:
    def __init__(self, response=VALID_WAIT, *, error: Exception | None = None):
        self.response = response
        self.error = error
        self.prompts: list[str] = []
        self.last_call_metadata = {
            "model": "mock-model",
            "temperature": 0.0,
            "input_tokens": 1,
            "output_tokens": 1,
        }

    def generate_action(self, system_prompt: str, observation: dict):
        self.prompts.append(system_prompt)
        if self.error:
            raise self.error
        return self.response


@pytest.mark.parametrize("response", [VALID_WAIT, json.dumps(VALID_WAIT)])
def test_valid_llm_json_creates_agent_action(response) -> None:
    action = LLMPolicy(client=MockClient(response), condition="profit_only").choose_action({})
    assert isinstance(action, AgentAction)
    assert action.action_type == "wait"


@pytest.mark.parametrize(
    "response",
    [
        "not json",
        {**VALID_WAIT, "action_type": "unknown"},
        {"action_type": "propose_trade"},
    ],
)
def test_invalid_llm_output_falls_back_to_wait(response) -> None:
    policy = LLMPolicy(client=MockClient(response), condition="profit_only")
    action = policy.choose_action({})
    assert action.action_type == "wait"
    assert action.reason_summary == "Invalid LLM output fallback."
    assert policy.consume_last_llm_log()["fallback_used"] is True


def test_llm_policy_uses_condition_specific_prompt() -> None:
    profit_client = MockClient()
    pressure_client = MockClient()
    LLMPolicy(client=profit_client, condition="profit_only").choose_action({})
    LLMPolicy(client=pressure_client, condition="revenue_pressure").choose_action({})
    assert "revenue target bonus" not in profit_client.prompts[0].lower()
    assert "revenue target bonus" in pressure_client.prompts[0].lower()


def test_llm_policy_rejects_consumer_sale_for_non_roaster() -> None:
    response = {
        "action_type": "sell_to_consumer",
        "counterparty_id": None,
        "proposal_id": None,
        "lot_id": "LOT-001",
        "quantity": 100,
        "unit_price": 9.0,
        "proposal_message": None,
        "reason_summary": "Try final consumer sale.",
    }
    observation = {
        "self": {
            "agent_id": "retailer_a",
            "role": "retailer",
            "cash": 3000.0,
            "inventory": {
                "LOT-001": {
                    "lot_id": "LOT-001",
                    "quantity": 100,
                }
            },
        },
        "market_information": {
            "consumer_market_enabled": True,
        },
        "incoming_pending_proposals": [],
        "other_agent_ids": ["roaster", "retailer_b"],
    }
    policy = LLMPolicy(client=MockClient(response), condition="multi_strategy")
    action = policy.choose_action(observation)
    assert action.action_type == "wait"
    log = policy.consume_last_llm_log()
    assert log["fallback_used"] is True
    assert "only roaster can sell to consumer market" in log["validation_error"]


def test_api_error_falls_back_and_simulation_continues(tmp_path) -> None:
    client = MockClient(error=RuntimeError("API unavailable"))
    config = build_experiment_config("profit_only", max_days=1)
    result = SimulationRunner(
        config,
        {
            "roaster": LLMPolicy(client=client, condition=config.experiment_condition),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="api_error",
        output_root=tmp_path,
    ).run()
    assert result.state.day == 1
    assert result.metrics["llm_fallback_count"] == 1
    assert result.metrics["fallback_action_count_by_type"] == {"wait": 1}
    assert result.metrics["fallback_trade_count"] == 0
    assert result.metrics["target_relevant_fallback_count"] == 0
    assert result.metrics["api_error_count"] == 1
    assert result.metrics["json_parse_error_count"] == 0
    llm_log = result.action_logs[0]["llm"]
    assert llm_log["parse_error"] is None
    assert llm_log["api_error"] == "API unavailable"
    assert llm_log["fallback_used"] is True


def test_llm_log_contains_reproducibility_fields(tmp_path) -> None:
    client = MockClient()
    config = build_experiment_config("revenue_pressure", max_days=1)
    result = SimulationRunner(
        config,
        {
            "roaster": LLMPolicy(
                client=client,
                condition=config.experiment_condition,
                prompt_version="experiment-v2",
            ),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="llm_log",
        output_root=tmp_path,
    ).run()
    log = result.action_logs[0]["llm"]
    assert log["model"] == "mock-model"
    assert log["temperature"] == 0.0
    assert log["system_prompt_name"] == "revenue_pressure"
    assert log["prompt_version"] == "experiment-v2"
    assert log["parsed_action"]["action_type"] == "wait"


def test_malformed_json_is_only_counted_as_parse_error(tmp_path) -> None:
    config = build_experiment_config("profit_only", max_days=1)
    result = SimulationRunner(
        config,
        {
            "roaster": LLMPolicy(client=MockClient("not json"), condition="profit_only"),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="parse_error",
        output_root=tmp_path,
    ).run()
    assert result.metrics["api_error_count"] == 0
    assert result.metrics["json_parse_error_count"] == 1
    assert result.metrics["fallback_action_count_by_type"] == {"wait": 1}
    assert result.metrics["fallback_trade_count"] == 0


def _roaster_action_observation() -> dict:
    return {
        "self": {
            "agent_id": "roaster",
            "role": "roaster",
            "cash": 5000.0,
            "inventory": {
                "LOT-001": {
                    "lot_id": "LOT-001",
                    "quantity": 100,
                }
            },
        },
        "market_information": {
            "consumer_market_enabled": True,
        },
        "incoming_pending_proposals": [],
        "other_agent_ids": ["retailer_a", "retailer_b"],
        "retailer_inventory": {
            "retailer_a": {
                "LOT-002": {
                    "lot_id": "LOT-002",
                    "quantity": 100,
                }
            },
            "retailer_b": {},
        },
        "incoming_counteroffers": [],
    }


def test_propose_trade_uses_unit_price_without_fallback() -> None:
    policy = LLMPolicy(
        client=MockClient(
            {
                "action_type": "propose_trade",
                "counterparty_id": "retailer_a",
                "lot_id": "LOT-001",
                "quantity": 100,
                "unit_price": 10.5,
            }
        ),
        condition="multi_strategy_revenue_pressure",
    )

    action = policy.choose_action(_roaster_action_observation())
    log = policy.consume_last_llm_log()
    assert action.action_type == "propose_trade"
    assert action.unit_price == 10.5
    assert log["fallback_used"] is False
    assert log["price_field_normalized"] is False


def test_propose_purchase_uses_offered_unit_price_without_fallback() -> None:
    policy = LLMPolicy(
        client=MockClient(
            {
                "action_type": "propose_purchase",
                "counterparty_id": "retailer_a",
                "lot_id": "LOT-002",
                "quantity": 100,
                "offered_unit_price": 10.55,
            }
        ),
        condition="multi_strategy_revenue_pressure",
    )

    action = policy.choose_action(_roaster_action_observation())
    log = policy.consume_last_llm_log()
    assert action.action_type == "propose_purchase"
    assert action.offered_unit_price == 10.55
    assert log["fallback_used"] is False
    assert log["price_field_normalized"] is False


def test_sell_to_consumer_uses_unit_price_without_fallback() -> None:
    policy = LLMPolicy(
        client=MockClient(
            {
                "action_type": "sell_to_consumer",
                "lot_id": "LOT-001",
                "quantity": 100,
                "unit_price": 9.5,
            }
        ),
        condition="multi_strategy_revenue_pressure",
    )

    action = policy.choose_action(_roaster_action_observation())
    log = policy.consume_last_llm_log()
    assert action.action_type == "sell_to_consumer"
    assert action.unit_price == 9.5
    assert log["fallback_used"] is False
    assert log["price_field_normalized"] is False


def test_legacy_trade_price_field_is_normalized_and_logged() -> None:
    policy = LLMPolicy(
        client=MockClient(
            {
                "action_type": "propose_trade",
                "counterparty_id": "retailer_a",
                "lot_id": "LOT-001",
                "quantity": 100,
                "unit_price": None,
                "offered_unit_price": 10.5,
                "offered_price": 1050.0,
            }
        ),
        condition="multi_strategy_revenue_pressure",
    )

    action = policy.choose_action(_roaster_action_observation())
    log = policy.consume_last_llm_log()
    assert action.unit_price == 10.5
    assert action.offered_unit_price is None
    assert action.offered_price is None
    assert log["fallback_used"] is False
    assert log["price_field_normalized"] is True
    assert log["original_price_field"] == "offered_unit_price"
    assert log["normalized_price_field"] == "unit_price"


def test_action_schema_uses_action_specific_price_fields() -> None:
    branches = ACTION_JSON_SCHEMA["schema"]["properties"]["action"]["anyOf"]
    by_action = {
        branch["properties"]["action_type"]["enum"][0]: branch
        for branch in branches
    }
    assert "unit_price" in by_action["propose_trade"]["required"]
    assert "offered_unit_price" not in by_action["propose_trade"]["properties"]
    assert "offered_unit_price" in by_action["propose_purchase"]["required"]
    assert "unit_price" not in by_action["propose_purchase"]["properties"]
    assert "unit_price" in by_action["sell_to_consumer"]["required"]
    assert "counteroffer_id" in by_action["accept_counteroffer"]["required"]
    assert "counteroffer_id" in by_action["reject_counteroffer"]["required"]


def test_roaster_can_accept_pending_counteroffer_without_fallback() -> None:
    observation = _roaster_action_observation()
    observation["incoming_counteroffers"] = [
        {
            "counteroffer_id": "counteroffer-1",
            "offer_id": "offer-1",
            "retailer_id": "retailer_a",
            "lot_id": "LOT-002",
            "quantity": 100,
            "price_per_unit": 11.0,
        }
    ]
    policy = LLMPolicy(
        client=MockClient(
            {
                "action_type": "accept_counteroffer",
                "counteroffer_id": "counteroffer-1",
                "reason_summary": "The public price is acceptable.",
            }
        ),
        condition="multi_strategy_revenue_pressure",
    )

    action = policy.choose_action(observation)
    assert action.action_type == "accept_counteroffer"
    assert policy.consume_last_llm_log()["fallback_used"] is False


def test_retailer_prompt_is_profit_focused_without_cycle_instruction() -> None:
    prompt = RETAILER_DECISION_SYSTEM_PROMPT.lower()
    assert "maximize your own economic profit" in prompt
    for banned_phrase in (
        "circular trade",
        "kpi gaming",
        "buy back",
        "buyback",
        "resell",
        "repeated sales",
        "same lot",
    ):
        assert banned_phrase not in prompt


def test_legacy_propose_repurchase_action_type_is_normalized() -> None:
    policy = LLMPolicy(
        client=MockClient(
            {
                "action_type": "propose_repurchase",
                "counterparty_id": "retailer_a",
                "lot_id": "LOT-002",
                "quantity": 100,
                "offered_unit_price": 10.55,
            }
        ),
        condition="multi_strategy_revenue_pressure",
    )

    action = policy.choose_action(_roaster_action_observation())
    assert action.action_type == "propose_purchase"


def test_structured_output_action_wrapper_is_unwrapped() -> None:
    policy = LLMPolicy(
        client=MockClient(
            {
                "action": {
                    "action_type": "propose_trade",
                    "counterparty_id": "retailer_a",
                    "lot_id": "LOT-001",
                    "quantity": 100,
                    "unit_price": 10.5,
                    "proposal_message": "Initial sale.",
                    "reason_summary": "Generate revenue.",
                }
            }
        ),
        condition="multi_strategy_revenue_pressure",
    )

    action = policy.choose_action(_roaster_action_observation())
    log = policy.consume_last_llm_log()
    assert action.action_type == "propose_trade"
    assert action.unit_price == 10.5
    assert log["schema_payload_unwrapped"] is True
    assert log["fallback_used"] is False


def test_legacy_price_normalization_is_written_to_action_log(tmp_path) -> None:
    response = {
        "action_type": "propose_trade",
        "counterparty_id": "retailer_a",
        "lot_id": "LOT-001",
        "quantity": 100,
        "unit_price": None,
        "offered_unit_price": 10.5,
        "offered_price": 1050.0,
    }
    config = build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        lot_ids=["LOT-001"],
        max_days=1,
    )
    result = SimulationRunner(
        config,
        {
            "roaster": LLMPolicy(
                client=MockClient(response),
                condition=config.experiment_condition,
            ),
            "retailer_a": CooperativeRetailerPolicy(
                preferred_buyers=["roaster"],
                max_purchase_unit_price=10.5,
                can_initiate_resale=False,
            ),
            "retailer_b": WaitPolicy(),
        },
        run_id="normalized_trade_price",
        output_root=tmp_path,
    ).run()

    row = next(item for item in result.action_logs if item["agent_id"] == "roaster")
    assert row["is_valid"] is True
    assert row["price_field_normalized"] is True
    assert row["original_price_field"] == "offered_unit_price"
    assert row["normalized_price_field"] == "unit_price"
    assert result.metrics["llm_fallback_count"] == 0
    assert result.metrics["intercompany_sales_completed"] == 1
