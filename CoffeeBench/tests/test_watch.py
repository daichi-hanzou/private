import pytest
from coffeebench.watch import _stream


@pytest.mark.parametrize('actions,messages', [(True, False), (False, True)])
def test_consumer_sale_without_obsolete_boost(actions, messages, monkeypatch, capsys):
    monkeypatch.setattr(_stream, 'seen', 0, raising=False)
    events = [dict(type='consumer_sale', day=3, shop_id='retailer_A',
                   item_id='roasted_coffee_kg', qty=5, unit_price=18,
                   total_price=90, elastic_qty=3, floor_qty=2,
                   market_demand=10, share=0.5)]
    _stream(events, actions, messages)
    output = capsys.readouterr().out
    assert 'retailer_A sold roasted_coffee_kg x5 @ $18.00' in output
    assert 'boost' not in output
    assert _stream.seen == 1
