from pathlib import Path
from types import SimpleNamespace
import json
import pytest
from coffeebench import main, environment
from coffeebench.provenance import Provenance, analyze_cycles, replay_provenance
from coffeebench.typings import Deal


def test_partial_three_party_cycles_and_terminal_units():
    p = Provenance(lambda: 0)
    ids = p.create('a', 'coffee', 10, 20)
    def trade(units, seller, buyer):
        p.move(units, buyer, 'on_hand', kind='trade', seller=seller)
    trade(ids, 'a', 'b')
    p.withdraw('b', 'coffee', 2, 'consumed', kind='consumer')
    trade(ids[2:], 'b', 'c')
    trade(ids[2:5], 'c', 'a')
    result = analyze_cycles(p.events)
    assert result['unique_cycled_quantity_kg'] == 3
    assert result['cycles'][0]['path'] == ['a', 'b', 'c', 'a']
    assert len(p.select('a', 'coffee', 3)) == 3
    with pytest.raises(ValueError):
        p.select('b', 'coffee', 1)
    assert replay_provenance(p.events) == {'units': p.units, 'lots': p.lots}


def test_different_lots_and_returns_are_not_cycles():
    p = Provenance(lambda: 0)
    a = p.create('a', 'coffee', 4, 4)
    b = p.create('b', 'coffee', 4, 4)
    p.move(a, 'b', 'on_hand', kind='trade', seller='a')
    p.move(b, 'a', 'on_hand', kind='trade', seller='b')
    assert not analyze_cycles(p.events)['detected']
    p.move(a, 'a', 'on_hand', kind='return')
    assert not analyze_cycles(p.events)['detected']
    p.move(a, 'b', 'on_hand', kind='trade', seller='a')
    assert not analyze_cycles(p.events)['detected']


@pytest.fixture
def env(monkeypatch, tmp_path):
    config = Path(__file__).resolve().parents[1] / 'experiments/minimal/revenue_zero.toml'
    monkeypatch.chdir(tmp_path)
    e, _, _ = main.build_run(SimpleNamespace(config=str(config), model='passive',
                             models=None, seed=0, max_days=None, main_agent=None))
    e.verbose = False
    monkeypatch.setattr(environment, 'DELIVERY_LOSS_PROB', 0)
    monkeypatch.setattr(environment, 'DELIVERY_DELAY_PROB', 0)
    return e


def trade(e, seller, buyer, item, qty):
    deal = Deal(str(len(e.marketplace.deals)), '', '', seller, buyer, item,
                qty, 10, qty*10, 30, 0, 0, '')
    e.marketplace.deals.append(deal)
    e._on_deal_accepted(deal)
    e.provenance.assert_consistent(e)
    e._process_deliveries(0)
    e.provenance.assert_consistent(e)
    return deal


def test_live_trade_return_and_output(env, tmp_path):
    item = 'roasted_coffee_kg'
    # Empty B through the audit as well as the books, so FIFO buys back A's units.
    b = env.business_apps['retailer_B']
    env.provenance.withdraw('retailer_B', item, b.inventory[item], 'consumed',
                            kind='consumer', total_price=0)
    b.inventory[item] = 0
    b.inventory_total_cost[item] = 0
    first = trade(env, 'retailer_A', 'retailer_B', item, 10)
    returned = b.return_shipment(first.invoice_id, 2)
    assert returned['status'] == 'success'
    assert not analyze_cycles(env.provenance.events)['detected']
    trade(env, 'retailer_B', 'retailer_A', item, 5)
    assert analyze_cycles(env.provenance.events)['unique_cycled_quantity_kg'] == 5
    env.provenance.assert_consistent(env)
    env.save_trajectory(str(tmp_path / 'run.json'))
    data = json.loads((tmp_path / 'run.json').read_text())
    assert data['lot_cycles']['detected']
    assert len(data['deal_unit_ids'][first.id]) == 10
    assert data['provenance']['lots']


def test_production_roasting_loss_and_bankruptcy(env, monkeypatch):
    farmer = env.business_apps['farmer_A']
    assert farmer.produce_item('green_coffee_kg', 10)['status'] == 'success'
    env.provenance.assert_consistent(env)
    env._materialize_pending_production(2)
    env.provenance.assert_consistent(env)
    trade(env, 'farmer_A', 'roaster_A', 'green_coffee_kg', 20)
    roaster = env.business_apps['roaster_A']
    assert roaster.roast('green_coffee_kg', 10)['status'] == 'success'
    env.provenance.assert_consistent(env)
    env._materialize_pending_roasting(2)
    env.provenance.assert_consistent(env)
    assert any(l['parent_units'] for l in env.provenance.lots.values())
    assert roaster.roast('green_coffee_kg', 5)['status'] == 'success'
    env._destroy_pending_for('roaster_A', 0)
    env.provenance.assert_consistent(env)
    monkeypatch.setattr(environment, 'DELIVERY_LOSS_PROB', 1)
    lost = trade(env, 'farmer_A', 'roaster_A', 'green_coffee_kg', 5)
    assert all(env.provenance.units[u]['state'] == 'lost' for u in lost.unit_ids)
