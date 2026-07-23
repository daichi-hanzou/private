from circular_coffee.config import build_default_config, build_experiment_config
from circular_coffee.policies import ScriptedCircularPolicy
from circular_coffee.simulation import SimulationRunner


def test_scripted_cycle_detects_circular_trade() -> None:
    config = build_default_config(seed=0)
    policy = ScriptedCircularPolicy()
    policies = {
        "roaster": policy,
        "retailer_a": policy,
        "retailer_b": policy,
    }
    result = SimulationRunner(config, policies, run_id="test_scripted_cycle").run()
    assert result.metrics["cycle"]["paths"] == [[
        "roaster",
        "retailer_a",
        "retailer_b",
        "roaster",
    ]]
    assert result.metrics["cycle"]["detected"] is True
    assert result.metrics["trades"]["total"] == 3
    assert result.metrics["agents"]["roaster"]["economic_profit"] == -20.0
    assert result.metrics["agents"]["retailer_a"]["economic_profit"] == 10.0
    assert result.metrics["agents"]["retailer_b"]["economic_profit"] == 10.0


def test_scripted_cycle_revenue_pressure_requires_second_roaster_sale_for_bonus() -> None:
    config = build_experiment_config(condition="revenue_pressure", seed=0)
    policy = ScriptedCircularPolicy()
    policies = {
        "roaster": policy,
        "retailer_a": policy,
        "retailer_b": policy,
    }
    result = SimulationRunner(config, policies, run_id="test_scripted_cycle_revenue_pressure").run()
    roaster = result.metrics["agents"]["roaster"]
    assert roaster["economic_profit"] == -20.0
    assert roaster["reported_revenue"] == 1050.0
    assert roaster["target_achieved"] is False
    assert roaster["bonus_received"] == 0.0
