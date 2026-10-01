import json
from coffeebench.lot_report import build_report_data, write_report
from coffeebench.provenance import Provenance


def sample():
    clock = [0]
    p = Provenance(lambda: clock[0])
    ids = p.create('A', 'coffee', 3, 10)
    clock[0] = 1440 + 600
    p.move(ids, 'B', 'on_hand', 'd1', kind='trade', seller='A', unit_price=10)
    clock[0] = 3*1440 + 600
    p.move(ids[:2], 'A', 'on_hand', 'd2', kind='trade', seller='B', unit_price=12)
    return dict(final_day=4, provenance=p.snapshot(), truth_ledger={
        'A': [dict(day=1, entry_type='sale_revenue', amount=30, counterparty='B'),
              dict(day=2, entry_type='sale_revenue', amount=5, counterparty='consumer'),
              dict(day=3, entry_type='sale_reversal', amount=10, counterparty='B'),
              dict(day=3, entry_type='cash_in', amount=30, counterparty='B')],
        'B': [dict(day=3, entry_type='sale_revenue', amount=24, counterparty='A')]},
        marketplace=dict(deals=[dict(id='d1', deal_at=600, listing_id='l1', offer_id='o1'),
                                dict(id='d2', deal_at=2*1440+600)], messages=[
            dict(id='m2', sender_id='B', recipient_id='A', sent_at=2*1440+500,
                 title='返売', body='</script><script>alert(1)</script>', ref_deal_id='d2'),
            dict(id='m1', sender_id='A', recipient_id='B', sent_at=500,
                 title='販売', body='3kgあります', ref_listing_id='l1')]))


def test_revenue_dates_and_transaction_links():
    report = build_report_data(sample())
    assert report['series']['A']['daily'] == [0,30,5,-10,0]
    assert report['series']['A']['cumulative'] == [0,30,35,25,25]
    assert report['series']['A']['b2b'] == [0,30,0,-10,0]
    assert report['series']['A']['consumer'] == [0,0,5,0,0]
    c = report['cycles']['cycles'][0]
    assert c['day'] == 4 and c['quantity_kg'] == 2
    assert c['trades'][0]['day'] == 2
    assert c['trades'][0]['accepted_at'] == 600
    assert c['trades'][0]['listing_id'] == 'l1'
    assert [m['id'] for m in report['messages']] == ['m1','m2']


def test_html_embeds_full_messages_safely(tmp_path):
    out = tmp_path/'report.html'
    write_report(sample(), out)
    html = out.read_text()
    assert '</script><script>alert(1)' not in html
    payload = html.split('<script id="data" type="application/json">')[1].split('</script>')[0]
    assert json.loads(payload)['messages'][1]['body'] == '</script><script>alert(1)</script>'
    assert '__REPORT_DATA__' not in html
