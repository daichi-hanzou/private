from __future__ import annotations

import json

import pytest

from circular_coffee.config import build_experiment_config
from circular_coffee.models import AgentAction
from circular_coffee.policies import LLMPolicy, WaitPolicy
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
