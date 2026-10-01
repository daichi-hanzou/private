import asyncio
from pathlib import Path
from types import SimpleNamespace
import pytest
from coffeebench import main, environment
from coffeebench.config import RunConfig
from tools.inspect_reciprocal_trades import inspect

ROOT = Path(__file__).resolve().parents[1]

@pytest.mark.parametrize("condition", ["profit_normal", "profit_zero", "revenue_normal", "revenue_zero"])
def test_twelve_days_offline(condition, monkeypatch, tmp_path):
    config = ROOT / "experiments/minimal" / (condition + ".toml")
    monkeypatch.setattr(environment, "CONSUMER_DEMAND_ENABLED", True)
    monkeypatch.chdir(tmp_path)
    args = SimpleNamespace(config=str(config), model="passive", models=None,
                           seed=0, max_days=None, main_agent=None)
    env, path, _ = main.build_run(args)
    env.verbose = False
    asyncio.run(env.run())
    env.save_trajectory(path)
    if condition.endswith("zero"):
        assert env.consumer_sales_log == []
    else:
        assert sum(s["qty"] for s in env.consumer_sales_log) > 0
    for aid, agent in env.agents.items():
        expected = main._build_score_framing(RunConfig.from_toml(config).kpi[aid], 12)
        assert expected in agent.system_prompt


def test_boolean_validation():
    with pytest.raises(ValueError):
        RunConfig(name="bad", economy={"consumer_demand_enabled": "false"}).apply_economy_overrides()


def test_reciprocal_screen_excludes_lost_and_other_items():
    def e(seller, buyer, item="coffee", type="deal_delivered"):
        return dict(type=type, seller=seller, buyer=buyer, item_id=item, received_qty=2)
    assert inspect([e("a", "b"), e("b", "a", type="delivery_lost")])["reciprocal_pair_count"] == 0
    assert inspect([e("a", "b"), e("b", "a", item="other")])["reciprocal_pair_count"] == 0
    assert inspect([e("a", "b"), e("b", "a")])["reciprocal_pair_count"] == 1
