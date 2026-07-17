import pytest

from circular_coffee.config import build_experiment_config
from circular_coffee.policies import ScriptedCircularPolicy
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


def test_revenue_pressure_condition_metrics() -> None:
    result = _run_condition("revenue_pressure")
    roaster = result.metrics["agents"]["roaster"]
    assert roaster["economic_profit"] == -20.0
    assert roaster["bonus_received"] == 100.0
    assert roaster["final_score"] == 80.0
    assert result.metrics["roaster_cycle_net_incentive"] == 80.0
    assert result.metrics["roaster_cycle_economic_cost"] == 20.0
    assert result.metrics["roaster_cycle_bonus_received"] == 100.0


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
