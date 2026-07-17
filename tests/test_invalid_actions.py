from circular_coffee.config import build_default_config
from circular_coffee.models import AgentAction
from circular_coffee.policies import AgentPolicy, WaitPolicy
from circular_coffee.simulation import SimulationRunner


class InvalidPolicy:
    def choose_action(self, observation: dict) -> AgentAction:
        return AgentAction(
            action_type="accept_trade",
            proposal_id="missing-proposal",
            reason_summary="Invalid action for test.",
        )


def test_invalid_action_does_not_stop_simulation() -> None:
    config = build_default_config(max_days=2)
    policies: dict[str, AgentPolicy] = {
        "roaster": InvalidPolicy(),
        "retailer_a": WaitPolicy(),
        "retailer_b": WaitPolicy(),
    }
    result = SimulationRunner(config, policies, run_id="test_invalid_action").run()
    assert result.state.day == 2


def test_invalid_action_is_logged() -> None:
    config = build_default_config(max_days=1)
    policies = {
        "roaster": InvalidPolicy(),
        "retailer_a": WaitPolicy(),
        "retailer_b": WaitPolicy(),
    }
    result = SimulationRunner(config, policies, run_id="test_invalid_log").run()
    invalid_rows = [row for row in result.action_logs if row["agent_id"] == "roaster"]
    assert invalid_rows[0]["is_valid"] is False
    assert invalid_rows[0]["error"] is not None


def test_invalid_action_behaves_like_wait() -> None:
    config = build_default_config(max_days=1)
    policies = {
        "roaster": InvalidPolicy(),
        "retailer_a": WaitPolicy(),
        "retailer_b": WaitPolicy(),
    }
    result = SimulationRunner(config, policies, run_id="test_invalid_wait").run()
    assert result.metrics["trades_completed"] == 0
    assert result.state.agents["roaster"].inventory["LOT-001"].current_owner_id == "roaster"
