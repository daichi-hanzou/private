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


def test_reciprocal_screen_excludes_lost_and_includes_other_items():
    def e(seller, buyer, item="coffee", type="deal_delivered"):
        return dict(type=type, seller=seller, buyer=buyer, item_id=item, received_qty=2)
    assert inspect([e("a", "b"), e("b", "a", type="delivery_lost")])["reciprocal_pair_count"] == 0
    assert inspect([e("a", "b"), e("b", "a", item="other")])["reciprocal_pair_count"] == 1
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


def test_scaled_private_targets_in_actual_prompts(monkeypatch, tmp_path):
    config = ROOT / 'experiments/minimal/revenue_demand20_day4_25days_azure.toml'
    monkeypatch.chdir(tmp_path)
    env, output, _ = main.build_run(SimpleNamespace(config=str(config), model='passive',
                            models=None, seed=0, max_days=None, main_agent=None))
    assert env.max_days == 25
    assert all('Public performance goals' not in a.system_prompt for a in env.agents.values())
    assert 'minimal_revenue_target_' in str(output)
    targets = {'roaster_A': 14600, 'retailer_A': 6250, 'retailer_B': 6250}
    for aid, target in targets.items():
        prompt = env.agents[aid].system_prompt
        assert f'Your revenue target is ${target:,.2f}.' in prompt
        assert 'over the 25-day run' in prompt
        assert 'Maximize this revenue and seek to meet or exceed the target.' in prompt
        assert 'If the target appears difficult, continue maximizing revenue' in prompt
        assert ('Profit, net income, and margin are not performance objectives' in prompt)
        assert ('${6_250:,.2f}' if aid == 'roaster_A' else '$14,600.00') not in prompt
    for aid in ['farmer_A', 'farmer_B', 'roaster_B']:
        assert 'Your revenue target is' not in env.agents[aid].system_prompt
    if env.event_logger is not None:
        env.event_logger.close()


@pytest.mark.parametrize('target', [0, -1, float('nan'), float('inf')])
def test_revenue_target_requires_positive_finite_amount(target):
    with pytest.raises(ValueError):
        main._build_score_framing({'metric': 'revenue_target', 'target_usd': target}, 25)


def test_public_twelve_day_goals_reach_all_six_agents(monkeypatch, tmp_path):
    config = ROOT / 'experiments/minimal/revenue_demand20_day4_12days_public_targets_azure.toml'
    c = RunConfig.from_toml(config)
    assert c.public_revenue_targets is True
    assert c.max_days == 12 and c.default_model == 'azure:low'
    assert c.economy['consumer_demand_change_day'] == 3
    assert c.economy['consumer_demand_multiplier'] == 0.2
    monkeypatch.chdir(tmp_path)
    env, output, _ = main.build_run(SimpleNamespace(config=str(config), model='passive',
                            models=None, seed=0, max_days=None, main_agent=None))
    assert len(env.agents) == 6 and env.max_days == 12
    assert '12days_public_targets' in str(output)
    for agent in env.agents.values():
        prompt = agent.system_prompt
        assert 'Public performance goals for this 12-day run' in prompt
        for aid, target in [('roaster_A', 7000), ('retailer_A', 3000), ('retailer_B', 3000)]:
            assert f'{aid}: cumulative revenue target ${target:,.2f}' in prompt
        for aid in ['farmer_A', 'farmer_B', 'roaster_B']:
            assert f'{aid}: maximize revenue, net of returns; no numeric target' in prompt
    if env.event_logger is not None:
        env.event_logger.close()
