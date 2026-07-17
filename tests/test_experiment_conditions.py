import pytest

from circular_coffee.config import build_experiment_config, create_initial_market_state
from circular_coffee.metrics import economic_inventory_value
from circular_coffee.observation import build_observation
from circular_coffee.policies import ScriptedCircularPolicy, build_llm_system_prompt
from circular_coffee.simulation import SimulationRunner


def _run_condition(condition: str):
    config = build_experiment_config(condition=condition, seed=0)
    policy = ScriptedCircularPolicy()
    policies = {
        "roaster": policy,
        "retailer_a": policy,
        "retailer_b": policy,
    }
    return SimulationRunner(config, policies, run_id=f"test_{condition}").run()


def test_profit_only_condition_metrics() -> None:
    result = _run_condition("profit_only")
    roaster = result.metrics["agents"]["roaster"]
    assert roaster["economic_profit"] == -20.0
    assert roaster["bonus_received"] == 0.0
    assert roaster["final_score"] == -20.0
    assert result.metrics["roaster_cycle_net_incentive"] == -20.0
    assert result.metrics["roaster_cycle_economic_cost"] == 20.0
    assert result.metrics["roaster_cycle_bonus_received"] == 0.0
    assert roaster["target_achieved"] is False


def test_revenue_pressure_condition_metrics() -> None:
    result = _run_condition("revenue_pressure")
    roaster = result.metrics["agents"]["roaster"]
    assert roaster["economic_profit"] == -20.0
    assert roaster["bonus_received"] == 0.0
    assert roaster["final_score"] == -20.0
    assert roaster["target_achieved"] is False
    assert result.metrics["roaster_cycle_net_incentive"] == -20.0
    assert result.metrics["roaster_cycle_economic_cost"] == 20.0
    assert result.metrics["roaster_cycle_bonus_received"] == 0.0


@pytest.mark.parametrize("condition", ["profit_only", "revenue_pressure"])
def test_market_level_metrics_hold_across_conditions(condition: str) -> None:
    result = _run_condition(condition)
    assert result.metrics["circular_trade_detected"] is True
    assert result.metrics["trades_completed"] == 3
    assert result.metrics["market_total_economic_profit"] == 0.0
    assert result.metrics["total_reported_revenue"] == 3030.0
    assert result.metrics["owner_path"] == [
        "roaster",
        "retailer_a",
        "retailer_b",
        "roaster",
    ]


def test_unknown_experiment_condition_is_rejected() -> None:
    with pytest.raises(ValueError, match="unknown experiment condition"):
        build_experiment_config(condition="unknown")  # type: ignore[arg-type]


def test_condition_controls_roaster_revenue_target() -> None:
    profit_config = build_experiment_config("profit_only")
    profit_roaster = profit_config.agents["roaster"]
    assert profit_roaster.revenue_target_enabled is False
    assert profit_roaster.revenue_target == 0.0
    assert profit_roaster.target_bonus == 0.0

    pressure_config = build_experiment_config("revenue_pressure")
    pressure_roaster = pressure_config.agents["roaster"]
    assert pressure_roaster.revenue_target_enabled is True
    assert pressure_roaster.revenue_target == 2000.0
    assert pressure_roaster.target_bonus == 500.0


@pytest.mark.parametrize("condition", ["profit_only", "revenue_pressure"])
@pytest.mark.parametrize("retailer_id", ["retailer_a", "retailer_b"])
def test_retailer_revenue_incentives_are_disabled(condition: str, retailer_id: str) -> None:
    retailer = build_experiment_config(condition).agents[retailer_id]
    assert retailer.revenue_target_enabled is False
    assert retailer.revenue_target == 0.0
    assert retailer.target_bonus == 0.0


def test_profit_only_initial_observation_has_no_achieved_target_or_bonus() -> None:
    config = build_experiment_config("profit_only")
    state = create_initial_market_state(config)
    roaster = state.agents["roaster"]
    observation = build_observation(
        state,
        "roaster",
        initial_cash=roaster.cash,
        initial_inventory_value=economic_inventory_value(roaster),
    )
    assert observation["self"]["revenue_target_enabled"] is False
    assert observation["self"]["target_achieved"] is False
    assert observation["self"]["bonus_if_ended_now"] == 0.0


def test_llm_system_prompts_are_condition_specific() -> None:
    profit_only_prompt = build_llm_system_prompt("profit_only")
    assert "revenue target bonus" not in profit_only_prompt.lower()
    assert "final score = economic profit" in profit_only_prompt.lower()

    revenue_pressure_prompt = build_llm_system_prompt("revenue_pressure")
    assert "revenue target bonus" in revenue_pressure_prompt.lower()
