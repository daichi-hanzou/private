"""Offline, self-contained report linking revenue, physical cycles and messages."""
import json
from pathlib import Path
from coffeebench.provenance import analyze_cycles


def build_report_data(run):
    if 'provenance' not in run:
        raise ValueError('Lot history is missing; re-run with lot tracking enabled.')
    ledger = run.get('truth_ledger', {})
    days = max([run.get('final_day', 0) + 1] +
               [e['day'] + 1 for entries in ledger.values() for e in entries])
    series = {}
    for aid, entries in ledger.items():
        daily, consumer, b2b = [0.0]*days, [0.0]*days, [0.0]*days
        for e in entries:
            if e['entry_type'] not in {'sale_revenue', 'sale_reversal'}:
                continue
            amount = e['amount'] * (-1 if e['entry_type'] == 'sale_reversal' else 1)
            daily[e['day']] += amount
            (consumer if e.get('counterparty') == 'consumer' else b2b)[e['day']] += amount
        total, cumulative = 0, []
        for amount in daily:
            total += amount
            cumulative.append(round(total, 2))
        series[aid] = dict(daily=daily, cumulative=cumulative, consumer=consumer, b2b=b2b)
    marketplace = run.get('marketplace', {})
    deals = {d['id']: d for d in marketplace.get('deals', [])}
    events = {e['seq']: e for e in run['provenance']['events']}
    cycles = analyze_cycles(list(events.values()))
    for c in cycles['cycles']:
        c['day'] = c['completed_at'] // 1440 + 1
        c['trades'] = []
        for seq in c['trade_seqs']:
            e = events[seq]
            d = deals.get(e.get('ref'), {})
            c['trades'].append(dict(seq=seq, deal_id=e.get('ref'), at=e['at'],
                day=e['at']//1440+1, seller=e['seller'], buyer=e['owner'],
                quantity_kg=len(e['unit_ids']), unit_price=e.get('unit_price'),
                accepted_at=d.get('deal_at'), listing_id=d.get('listing_id'),
                offer_id=d.get('offer_id')))
    return dict(days=days, series=series, cycles=cycles,
                messages=sorted(marketplace.get('messages', []), key=lambda m: m['sent_at']))


def write_report(run, destination):
    data = json.dumps(build_report_data(run), ensure_ascii=False).replace('<', '\\u003c').replace('&', '\\u0026')
    template = Path(__file__).with_name('lot_report.html').read_text()
    Path(destination).write_text(template.replace('__REPORT_DATA__', data), encoding='utf-8')
