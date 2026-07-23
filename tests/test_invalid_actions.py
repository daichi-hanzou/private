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


class RaisingPolicy:
    def choose_action(self, observation: dict) -> AgentAction:
        raise RuntimeError("policy failed")


class LegacyHoldPolicy:
    def choose_action(self, observation: dict) -> AgentAction:
        return AgentAction(
            action_type="hold_inventory",  # type: ignore[arg-type]
            reason_summary="Legacy no-action response.",
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
    assert invalid_rows[0]["error_reason"] is not None


def test_invalid_action_behaves_like_wait() -> None:
    config = build_default_config(max_days=1)
    policies = {
        "roaster": InvalidPolicy(),
        "retailer_a": WaitPolicy(),
        "retailer_b": WaitPolicy(),
    }
    result = SimulationRunner(config, policies, run_id="test_invalid_wait").run()
    assert result.metrics["trades"]["total"] == 0
    assert result.state.agents["roaster"].inventory["LOT-001"].current_owner_id == "roaster"


def test_policy_error_creates_one_action_row_for_the_turn() -> None:
    config = build_default_config(max_days=1)
    result = SimulationRunner(
        config,
        {
            "roaster": RaisingPolicy(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="test_policy_error",
    ).run()

    rows = [row for row in result.action_logs if row["agent_id"] == "roaster"]
    assert len(rows) == 1
    assert rows[0]["requested_action"].action_type == "wait"
    assert rows[0]["is_valid"] is True
    assert rows[0]["policy_error"] is True
    assert rows[0]["llm_fallback_used"] is True
    assert result.metrics["errors"]["invalid_actions"] == 0
    assert result.metrics["errors"]["fallbacks"] == 1


def test_wait_is_valid_with_and_without_inventory() -> None:
    config = build_default_config(max_days=1)
    result = SimulationRunner(
        config,
        {
            "roaster": WaitPolicy(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="wait_inventory_independent",
    ).run()

    rows = {row["agent_id"]: row for row in result.action_logs}
    assert "LOT-001" in result.state.agents["roaster"].inventory
    assert result.state.agents["retailer_a"].inventory == {}
    assert rows["roaster"]["requested_action"].action_type == "wait"
    assert rows["retailer_a"]["requested_action"].action_type == "wait"
    assert rows["roaster"]["is_valid"] is True
    assert rows["retailer_a"]["is_valid"] is True
    assert result.metrics["errors"]["invalid_actions"] == 0


def test_legacy_hold_is_normalized_without_invalid_or_fallback() -> None:
    config = build_default_config(max_days=1)
    result = SimulationRunner(
        config,
        {
            "roaster": LegacyHoldPolicy(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="legacy_hold_normalization",
    ).run()

    row = next(item for item in result.action_logs if item["agent_id"] == "roaster")
    assert row["requested_action"].action_type == "wait"
    assert row["legacy_action_normalized"] is True
    assert row["is_valid"] is True
    assert row["llm_fallback_used"] is False
    assert result.metrics["errors"]["invalid_actions"] == 0
    assert result.metrics["errors"]["fallbacks"] == 0
