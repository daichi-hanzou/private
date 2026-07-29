from __future__ import annotations

import json

import pytest

from circular_coffee.config import build_experiment_config
from circular_coffee.llm_clients import (
    ACTION_JSON_SCHEMA,
    RETAILER_MARKET_ACTION_JSON_SCHEMA,
)
from circular_coffee.models import AgentAction
from circular_coffee.policies import LLMPolicy, WaitPolicy
from circular_coffee.policies import (
    MULTI_STRATEGY_LLM_SYSTEM_PROMPT,
    RETAILER_MARKET_SYSTEM_PROMPT,
)
from circular_coffee.simulation import SimulationRunner


def test_retailer_prompt_does_not_directly_instruct_profit_pursuit() -> None:
    assert "profit" not in RETAILER_MARKET_SYSTEM_PROMPT.lower()
    assert (
        "your only kpi is your own reported-revenue target"
        in RETAILER_MARKET_SYSTEM_PROMPT.lower()
    )
    assert "additional kpi" not in RETAILER_MARKET_SYSTEM_PROMPT.lower()


class MockClient:
    def __init__(self, response, *, error: Exception | None = None):
        self.response = response
        self.error = error
        self.last_call_metadata = {
            "model": "mock-model",
            "temperature": 0.0,
            "input_tokens": 1,
            "output_tokens": 1,
        }

    def generate_action(self, system_prompt: str, observation: dict):
        if self.error is not None:
            raise self.error
        return self.response


def _observation() -> dict:
    return {
        "self": {
            "agent_id": "roaster",
            "role": "roaster",
            "cash": 5000.0,
            "inventory": {"LOT-001": {"lot_id": "LOT-001", "quantity": 100}},
        },
        "market_information": {"consumer_market_enabled": False},
        "market_access": {"can_sell_to_consumer": False},
        "available_action_types": ["propose_trade", "wait"],
        "incoming_pending_proposals": [],
        "incoming_trade_counteroffers": [],
        "trade_candidates": {
            "sell": [
                {
                    "seller_id": "roaster",
                    "buyer_id": "retailer_a",
                    "lot_id": "LOT-001",
                    "quantity": 100,
                }
            ],
            "buy": [],
        },
    }


@pytest.mark.parametrize(
    "response",
    [
        {"action_type": "wait", "reason_summary": "Wait."},
        json.dumps({"action_type": "wait", "reason_summary": "Wait."}),
    ],
)
def test_valid_wait_creates_agent_action(response) -> None:
    action = LLMPolicy(client=MockClient(response), condition="profit_only").choose_action({})
    assert isinstance(action, AgentAction)
    assert action.action_type == "wait"


def test_common_trade_action_uses_explicit_direction() -> None:
    response = {
        "action_type": "propose_trade",
        "seller_id": "roaster",
        "buyer_id": "retailer_a",
        "lot_id": "LOT-001",
        "quantity": 100,
        "unit_price": 10.5,
        "proposal_message": "Offer.",
        "reason_summary": "Generate revenue.",
    }
    policy = LLMPolicy(
        client=MockClient(response),
        condition="multi_strategy_revenue_pressure",
    )

    action = policy.choose_action(_observation())

    assert action.seller_id == "roaster"
    assert action.buyer_id == "retailer_a"
    assert action.unit_price == 10.5
    assert policy.consume_last_llm_log()["fallback_used"] is False


def test_trade_outside_candidates_falls_back() -> None:
    response = {
        "action_type": "propose_trade",
        "seller_id": "retailer_a",
        "buyer_id": "roaster",
        "lot_id": "LOT-001",
        "quantity": 100,
        "unit_price": 10.5,
    }
    policy = LLMPolicy(client=MockClient(response), condition="multi_strategy")

    action = policy.choose_action(_observation())

    assert action.action_type == "wait"
    assert "available candidate" in policy.consume_last_llm_log()["validation_error"]


def test_common_action_schema_has_no_special_purchase_action() -> None:
    branches = ACTION_JSON_SCHEMA["schema"]["properties"]["action"]["anyOf"]
    by_action = {
        branch["properties"]["action_type"]["enum"][0]: branch
        for branch in branches
    }

    assert "propose_purchase" not in by_action
    assert {
        "seller_id",
        "buyer_id",
        "lot_id",
        "quantity",
        "unit_price",
    }.issubset(by_action["propose_trade"]["required"])
    assert "counteroffer_trade" in by_action
    assert "accept_counteroffer" in by_action
    assert "reject_counteroffer" in by_action
    assert "hold_inventory" not in by_action

    retailer_branches = RETAILER_MARKET_ACTION_JSON_SCHEMA["schema"]["properties"][
        "action"
    ]["anyOf"]
    retailer_actions = {
        branch["properties"]["action"]["enum"][0] for branch in retailer_branches
    }
    assert "hold_inventory" not in retailer_actions


def test_legacy_llm_hold_is_normalized_without_fallback() -> None:
    policy = LLMPolicy(
        client=MockClient(
            {
                "action_type": "hold_inventory",
                "reason_summary": "Keep inventory unchanged.",
            }
        ),
        condition="profit_only",
    )

    action = policy.choose_action(_observation())
    log = policy.consume_last_llm_log()

    assert action.action_type == "wait"
    assert log["legacy_action_normalized"] is True
    assert log["fallback_used"] is False
    assert log["validation_error"] is None


def test_new_prompts_do_not_display_legacy_hold_action() -> None:
    assert "hold_inventory" not in MULTI_STRATEGY_LLM_SYSTEM_PROMPT
    assert "hold_inventory" not in RETAILER_MARKET_SYSTEM_PROMPT


def test_api_error_falls_back_and_simulation_continues(tmp_path) -> None:
    config = build_experiment_config("profit_only", max_days=1)
    result = SimulationRunner(
        config,
        {
            "roaster": LLMPolicy(
                client=MockClient({}, error=RuntimeError("API unavailable")),
                condition="profit_only",
            ),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="api_error",
        output_root=tmp_path,
    ).run()

    assert result.metrics["errors"]["fallbacks"] == 1
    assert result.metrics["errors"]["api_errors"] == 1
    assert result.action_logs[0]["requested_action"].action_type == "wait"
