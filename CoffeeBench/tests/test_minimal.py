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


def test_demand_change_boundary_and_reset(monkeypatch, tmp_path):
    config = ROOT / 'experiments/minimal/revenue_demand20_day4_25days_azure.toml'
    c = RunConfig.from_toml(config)
    assert c.max_days == 25 and c.default_model == 'azure:low'
    for name in ['CONSUMER_DEMAND_ENABLED','CONSUMER_DEMAND_CHANGE_DAY','CONSUMER_DEMAND_MULTIPLIER']:
        monkeypatch.setattr(environment, name, getattr(environment, name))
    monkeypatch.chdir(tmp_path)
    env, _, _ = main.build_run(SimpleNamespace(config=str(config), model='passive',
                              models=None,seed=0,max_days=None,main_agent=None))
    env.verbose=False
    monkeypatch.setattr(env, '_market_demand', lambda *args, **kwargs: 100)
    monkeypatch.setattr(env, '_shop_shares', lambda item, active: {a[0]: 0.5 for a in active})
    def sales(day):
        for aid in ['retailer_A', 'retailer_B']:
            ba=env.business_apps[aid]
            for item_id, item in env.marketplace.items.items():
                if item.retail_reservation_price is not None:
                    difference = 1000 - ba.inventory.get(item_id, 0)
                    env.provenance.create(aid, item_id, difference, float(difference))
                    ba.inventory[item_id]=1000
                    ba.inventory_total_cost[item_id]=1000.0
        return env._run_consumer_sales(day)
    normal=sales(2)
    reduced=sales(3)
    assert normal and reduced
    assert len(normal)==len(reduced)
    for before,after in zip(normal,reduced):
        assert after['elastic_qty']==round(before['elastic_qty']*0.2)
        assert after['floor_qty']==round(before['floor_qty']*0.2)
    final = sales(24)
    assert [s['qty'] for s in final] == [s['qty'] for s in reduced]
    RunConfig(name='reset').apply_economy_overrides()
    assert environment.CONSUMER_DEMAND_CHANGE_DAY is None
    assert environment.CONSUMER_DEMAND_MULTIPLIER==1.0
