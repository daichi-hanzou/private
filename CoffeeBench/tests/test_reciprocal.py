from coffeebench.provenance import Provenance, analyze_cycles
from coffeebench.reciprocal import analyze_reciprocal
from coffeebench.lot_report import build_report_data


def run_with_different_lots():
    clock = [0]
    p = Provenance(lambda: clock[0])
    a = p.create('A', 'coffee', 5, 10)
    b = p.create('B', 'coffee', 3, 10)
    clock[0] = 1440
    p.move(a, 'B', 'on_hand', 'd1', kind='trade', seller='A', unit_price=20)
    clock[0] = 4320
    p.move(b, 'A', 'on_hand', 'd2', kind='trade', seller='B', unit_price=30)
    return p, b, clock


def test_different_lots_qualify_with_dates_and_amounts():
    p, _, _ = run_with_different_lots()
    run = {'provenance': p.snapshot(), 'final_day': 4}
    assert not analyze_cycles(p.events)['detected']
    result = analyze_reciprocal(run)
    assert result['pair_count'] == 1 and result['trade_count'] == 2
    g = result['groups'][0]
    assert g['day'] == 4
    assert [d['quantity_kg'] for d in g['directions']] == [5,3]
    assert [d['revenue_net'] for d in g['directions']] == [100,90]
    assert build_report_data(run)['reciprocal'] == result


def test_partial_and_full_returns():
    p, ids, clock = run_with_different_lots()
    clock[0] = 5760
    p.move(ids[:1], 'B', 'on_hand', 'd2', kind='return')
    assert analyze_reciprocal({'provenance':p.snapshot()})['groups'][0]['directions'][1]['quantity_kg'] == 2
    p.move(ids[1:], 'B', 'on_hand', 'd2', kind='return')
    assert not analyze_reciprocal({'provenance':p.snapshot()})['detected']


def test_no_lot_history_delivery_status_and_different_items():
    def d(id, seller, buyer, item='coffee', status='delivered'):
        return dict(id=id,seller_id=seller,buyer_id=buyer,item_id=item,status=status,
                    qty=2,unit_price=5,delivery_at=1440,returned_qty=0)
    first = d('d1','A','B')
    for second in [d('d2','A','B'),d('d2','B','A',status='pending'),d('d2','B','A',status='lost')]:
        assert not analyze_reciprocal({'marketplace':{'deals':[first,second]}})['detected']
    run = {'marketplace':{'deals':[first,d('d2','B','A')]}}
    assert analyze_reciprocal(run)['detected']
    assert build_report_data(run)['reciprocal']['detected']


def test_multiple_items_merge_into_one_pair_and_keep_details():
    clock = [0]
    p = Provenance(lambda: clock[0])
    a = p.create('A', 'green_coffee', 5, 10)
    b = p.create('B', 'roasted_coffee', 3, 10)
    c = p.create('A', 'specialty', 2, 10)
    clock[0] = 1440
    p.move(a, 'B', 'on_hand', 'd1', kind='trade', seller='A', unit_price=20)
    clock[0] = 4320
    p.move(b, 'A', 'on_hand', 'd2', kind='trade', seller='B', unit_price=30)
    clock[0] = 5760
    p.move(c, 'B', 'on_hand', 'd3', kind='trade', seller='A', unit_price=40)
    result = analyze_reciprocal({'provenance':p.snapshot()})
    assert result['pair_count'] == 1 and result['trade_count'] == 3
    g = result['groups'][0]
    assert g['day'] == 4 and g['last_trade_day'] == 5
    assert g['item_ids'] == ['green_coffee', 'roasted_coffee', 'specialty']
    assert [t['item_id'] for t in g['trades']] == ['green_coffee', 'roasted_coffee', 'specialty']
    assert [d['revenue_net'] for d in g['directions']] == [180,90]
    assert not analyze_cycles(p.events)['detected']


def test_different_items_without_lot_history():
    deals = [dict(id='a', seller_id='A', buyer_id='B', item_id='coffee',
                  qty=2, unit_price=5, status='delivered', delivery_at=1440),
             dict(id='b', seller_id='B', buyer_id='A', item_id='tea',
                  qty=3, unit_price=10, status='delivered', delivery_at=2880)]
    g = analyze_reciprocal({'marketplace':{'deals':deals}})['groups'][0]
    assert g['item_ids'] == ['coffee','tea'] and g['day'] == 3
