import asyncio
import json
from pathlib import Path
from types import SimpleNamespace

import pytest
from coffeebench import main, environment
from coffeebench.config import RunConfig
from coffeebench.provenance import Provenance, replay_provenance
from coffeebench.typings import Deal

ROOT = Path(__file__).resolve().parents[1]
PRESET = ROOT / 'experiments/minimal/revenue_demand20_day4_30days_lot_expiry_public_targets_azure.toml'


@pytest.fixture
def env(monkeypatch, tmp_path):
    for key in ('INVENTORY_DECAY_MODE', 'INVENTORY_SPOILAGE_PER_DAY',
                'GREEN_SHELF_LIFE_DAYS', 'ROASTED_SHELF_LIFE_DAYS',
                'CONSUMER_DEMAND_CHANGE_DAY', 'CONSUMER_DEMAND_MULTIPLIER',
                'CONSUMER_DEMAND_ENABLED'):
        monkeypatch.setattr(environment, key, getattr(environment, key))
    monkeypatch.chdir(tmp_path)
    e, _, _ = main.build_run(SimpleNamespace(config=str(PRESET), model='passive',
                             models=None, seed=0, max_days=None, main_agent=None))
    e.verbose = False
    monkeypatch.setattr(environment, 'DELIVERY_LOSS_PROB', 0)
    monkeypatch.setattr(environment, 'DELIVERY_DELAY_PROB', 0)
    return e


def test_dates_transfer_split_return_and_roast_parent_cap():
    now = [0]
    p = Provenance(lambda: now[0], shelf_life_days={'green': 30, 'roasted': 7})
    ids = p.create('a', 'green', 10, 20)
    assert p.expiry_day(ids[0]) == 29
    p.move(ids[:3], 'b', 'on_hand', kind='trade', seller='a')
    p.move(ids[:1], 'a', 'on_hand', kind='return')
    assert {p.expiry_day(u) for u in ids} == {29}
    now[0] = 27 * 1440
    roast = p.create('a', 'roasted', 2, 9, parents=ids[:1])
    assert p.expiry_day(roast[0]) == 29  # does not extend almost-expired green
    assert not p.expired(roast, 28)
    assert p.expired(roast, 29) == roast
    assert replay_provenance(p.events) == {'units': p.units, 'lots': p.lots}


def test_exact_deadline_writeoff_and_no_daily_decay(env):
    b = env.business_apps['retailer_A']
    before = dict(b.inventory)
    cost = sum(b.inventory_total_cost.values())
    equity = b._compute_true_equity()
    for day in range(6):
        assert env._apply_spoilage(day) == {}
    assert b.inventory == before
    result = env._apply_spoilage(6)
    assert result['retailer_A']['spoiled_units'] == {k: q for k, q in before.items() if q}
    assert result['retailer_A']['spoiled_value'] == pytest.approx(cost)
    assert b._compute_true_equity() == pytest.approx(equity - cost)
    assert env._apply_spoilage(6) == {}  # no duplicate writeoff
    env.provenance.assert_consistent(env)


def test_production_completion_clock_and_roast_clock(env):
    farmer = env.business_apps['farmer_A']
    assert farmer.produce_item('green_coffee_kg', 10)['status'] == 'success'
    batch = env._pending_production[-1]
    assert env.provenance.expiry_day(batch['unit_ids'][0]) == batch['ready_day'] + 29
    roaster = env.business_apps['roaster_A']
    env.provenance.create('roaster_A', 'green_coffee_kg', 10, 20)
    roaster.inventory['green_coffee_kg'] = 10
    roaster.inventory_total_cost['green_coffee_kg'] = 20
    assert roaster.roast('green_coffee_kg', 10)['status'] == 'success'
    roast = env._pending_roasting[-1]
    assert env.provenance.expiry_day(roast['unit_ids'][0]) == 6
    equity = roaster._compute_true_equity()
    cost = roast['output_total_cost']
    # Force long processing: expired WIP must never materialize later.
    roast['ready_day'] = 10
    env._apply_spoilage(6)
    assert not env._pending_roasting
    env._materialize_pending_roasting(10)
    assert all(env.provenance.units[u]['state'] == 'expired' for u in roast['unit_ids'])
    losses = sum(e.amount for e in env.truth_ledger['roaster_A']
                 if e.entry_type == 'spoilage_expense')
    assert losses >= cost
    assert roaster._compute_true_equity() == pytest.approx(equity - losses)
    env.provenance.assert_consistent(env)


def test_mixed_expiry_shipment_cancel_no_revenue_valid_stock_restored(env):
    item = 'roasted_coffee_kg'
    seller = env.business_apps['retailer_A']
    old_qty = seller.inventory[item]
    env.time_manager.virtual_min = 3 * 1440 + 540
    fresh = env.provenance.create('retailer_A', item, 5, 100)
    seller.inventory[item] += 5
    seller.inventory_total_cost[item] += 100
    env.time_manager.virtual_min = 6 * 1440 + 540
    qty = old_qty + 2
    d = Deal('expiry-test', '', '', 'retailer_A', 'retailer_B', item,
             qty, 10, qty * 10, 30, 6 * 1440, 7 * 1440 + 540, '')
    env.marketplace.deals.append(d)
    env._on_deal_accepted(d)
    equity = seller._compute_true_equity()
    reserved_unit_cost = d._reserved_seller_cogs / qty
    env._apply_spoilage(6)
    assert env._pending_shipments == []
    assert seller.inventory[item] == 5
    assert all(env.provenance.units[u]['state'] == 'on_hand' for u in fresh)
    assert seller.inventory_total_cost[item] == pytest.approx(5 * reserved_unit_cost)
    # Other roasted item also expires; verify commodity writeoff separately.
    costs = [e.amount for e in env.truth_ledger['retailer_A']
             if e.entry_type == 'spoilage_expense']
    assert seller._compute_true_equity() == pytest.approx(equity - sum(costs))
    env._process_deliveries(7)
    assert not d.invoice_id
    assert not any(e.entry_type == 'sale_revenue' for e in env.truth_ledger['retailer_A'])
    env.provenance.assert_consistent(env)


def test_agent_deadlines_and_prompt(env):
    assert 'Lot deadlines' in env._format_observation('retailer_A', 0, None)
    prompt = env.agents['retailer_A'].system_prompt
    assert 'Lot expiry replaces daily' in prompt
    assert '0.5%/day' not in prompt
    listing = env.business_apps['retailer_A'].post_listing('roasted_coffee_kg', 2, 20)
    assert listing['status'] == 'success'
    rows = env.marketplace.listings_dump()
    assert rows[-1]['available_lots'][0]['expiry_day'] == 6


@pytest.mark.parametrize('key,value', [('inventory_decay_mode','bad'),
    ('green_shelf_life_days',0), ('roasted_shelf_life_days',True),
    ('roasted_shelf_life_days',7.5)])
def test_invalid_config(key, value):
    with pytest.raises(ValueError):
        RunConfig(name='invalid', economy={key:value}).apply_economy_overrides()


def test_config_reset_and_control(monkeypatch):
    for name in ('INVENTORY_DECAY_MODE','GREEN_SHELF_LIFE_DAYS','ROASTED_SHELF_LIFE_DAYS'):
        monkeypatch.setattr(environment, name, getattr(environment, name))
    cfg = RunConfig.from_toml(PRESET)
    cfg.apply_economy_overrides()
    assert environment.INVENTORY_DECAY_MODE == 'lot_expiry'
    control = RunConfig.from_toml(str(PRESET).replace('lot_expiry','daily_rate'))
    control.apply_economy_overrides()
    assert environment.INVENTORY_DECAY_MODE == 'daily_rate'
    assert control.max_days == cfg.max_days == 30
    assert control.kpi == cfg.kpi
    assert cfg.kpi['retailer_A']['target_usd'] == 5250
    assert '0.5%/day' in main._operational_mechanics_block()


def test_thirty_days_offline(env, tmp_path, monkeypatch):
    # Make demand zero for this boundary test so every original lot survives to expiry.
    monkeypatch.setattr(environment, 'CONSUMER_DEMAND_ENABLED', False)
    asyncio.run(env.run())
    env.save_trajectory(str(tmp_path/'expiry.json'))
    data = json.loads((tmp_path/'expiry.json').read_text())
    assert data['inventory_policy']['mode'] == 'lot_expiry'
    expired = [e for e in data['provenance']['events'] if e['kind'] == 'lot_expired']
    assert {e['expiry_processed_day'] for e in expired} == {6, 29}
    assert all(u['state'] == 'expired' for u in data['provenance']['units'].values())
    env.provenance.assert_consistent(env)


def test_delivered_and_returned_stock_retains_deadline(env):
    item = 'roasted_coffee_kg'
    d = Deal('return-expiry', '', '', 'retailer_A', 'retailer_B', item,
             5, 10, 50, 30, 0, 1440 + 540, '')
    env.marketplace.deals.append(d)
    env._on_deal_accepted(d)
    env.time_manager.virtual_min = 1440 + 540
    env._process_deliveries(1)
    assert all(env.provenance.expiry_day(u) == 6 for u in d.unit_ids)
    result = env.business_apps['retailer_B'].return_shipment(d.invoice_id, 2)
    assert result['status'] == 'success'
    env._apply_spoilage(6)
    assert all(env.provenance.units[u]['state'] == 'expired' for u in d.unit_ids)
    env.provenance.assert_consistent(env)


def test_expiry_after_consumer_sale_and_end_run_keeps_unexpired(env, monkeypatch):
    observed = []
    def sell(day):
        observed.append(env.business_apps['retailer_A'].inventory['roasted_coffee_kg'])
        return []
    monkeypatch.setattr(env, '_run_consumer_sales', sell)
    env.time_manager.virtual_min = 6 * 1440 + 1140
    env._handle_eod_mechanics(6)
    assert observed[0] > 0
    assert env.business_apps['retailer_A'].inventory['roasted_coffee_kg'] == 0
    env.time_manager.virtual_min = 28 * 1440 + 540
    ids = env.provenance.create('retailer_A', 'roasted_coffee_kg', 3, 30)
    ba = env.business_apps['retailer_A']
    ba.inventory['roasted_coffee_kg'] = 3
    ba.inventory_total_cost['roasted_coffee_kg'] = 30
    env._apply_spoilage(29)
    assert ba.inventory['roasted_coffee_kg'] == 3
    assert env.provenance.expiry_day(ids[0]) == 34
    env.provenance.assert_consistent(env)
