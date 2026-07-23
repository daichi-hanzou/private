import json

from circular_coffee.config import build_experiment_config
from circular_coffee.policies import CooperativeRetailerPolicy, LLMPolicy
from circular_coffee.simulation import SimulationRunner


class InitiatingMockClient:
    last_call_metadata = {"model": "initiating-mock", "temperature": None}

    def generate_action(self, system_prompt: str, observation: dict) -> str:
        incoming = observation["incoming_pending_proposals"]
        if incoming:
            action = {
                "action_type": "accept_trade",
                "proposal_id": incoming[0]["proposal_id"],
            }
        elif observation["self"]["inventory"] and observation["day"] == 1:
            lot = next(iter(observation["self"]["inventory"].values()))
            action = {
                "action_type": "propose_trade",
                "seller_id": "roaster",
                "buyer_id": "retailer_a",
                "lot_id": lot["lot_id"],
                "quantity": lot["quantity"],
                "unit_price": 10.0,
            }
        else:
            action = {"action_type": "wait"}
        return json.dumps(action)


def test_roaster_initiated_sequence_can_return_lot_to_roaster(tmp_path) -> None:
    config = build_experiment_config("revenue_pressure", max_days=4)
    result = SimulationRunner(
        config,
        {
            "roaster": LLMPolicy(
                client=InitiatingMockClient(),
                condition=config.experiment_condition,
            ),
            "retailer_a": CooperativeRetailerPolicy(
                preferred_buyers=["retailer_b", "roaster"],
            ),
            "retailer_b": CooperativeRetailerPolicy(
                preferred_buyers=["roaster", "retailer_a"],
            ),
        },
        run_id="mock_llm_cycle",
        output_root=tmp_path,
    ).run()
    assert result.metrics["cycle"]["detected"] is True
    assert result.metrics["cycle"]["paths"] == [[
        "roaster",
        "retailer_a",
        "retailer_b",
        "roaster",
    ]]
