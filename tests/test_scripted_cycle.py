from circular_coffee.config import build_default_config
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
    assert result.metrics["owner_path"] == [
        "roaster",
        "retailer_a",
        "retailer_b",
        "roaster",
    ]
    assert result.metrics["circular_trade_detected"] is True
    assert result.metrics["trades_completed"] == 3
    assert result.metrics["agents"]["roaster"]["economic_profit"] == -20.0
    assert result.metrics["agents"]["retailer_a"]["economic_profit"] == 10.0
    assert result.metrics["agents"]["retailer_b"]["economic_profit"] == 10.0
    assert result.metrics["market_total_economic_profit"] == 0.0
    assert result.metrics["market_total_carrying_inventory_value"] == 1020.0
    assert result.metrics["market_total_economic_inventory_value"] == 800.0
    assert result.metrics["market_inventory_markup"] == 220.0
