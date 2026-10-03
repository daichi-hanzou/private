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


def test_deal_day_derived_from_timestamp(monkeypatch, capsys):
    monkeypatch.setattr(_stream, 'seen', 0, raising=False)
    _stream([dict(type='deal_accepted', deal_at=3*1440+600,
                  seller='A', buyer='B', item_id='coffee', qty=2, unit_price=10)], True, False)
    assert '[day 3] DEAL: A -> B' in capsys.readouterr().out


def test_read_utf8_log_under_cp932_default(monkeypatch, tmp_path):
    import builtins
    import json
    from coffeebench.watch import _read_events
    path = tmp_path / 'run.events.jsonl'
    event = {'type': 'agent_step', 'action_input': {'body': '商品を購入します ☕'}}
    path.write_text(json.dumps(event, ensure_ascii=False)+'\n', encoding='utf-8')
    original = builtins.open
    def cp932_open(file, mode='r', *args, **kwargs):
        if 'b' not in mode and 'encoding' not in kwargs:
            kwargs['encoding'] = 'cp932'
        return original(file, mode, *args, **kwargs)
    monkeypatch.setattr(builtins, 'open', cp932_open)
    assert _read_events(str(path)) == [event]
