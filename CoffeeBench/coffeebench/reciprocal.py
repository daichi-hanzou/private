"""Same-item reciprocal delivered sales, independent of lot identity."""
from collections import defaultdict


def analyze_reciprocal(run):
    deals = {d['id']: d for d in run.get('marketplace', {}).get('deals', [])}
    events = run.get('provenance', {}).get('events')
    trades = []
    if events is not None:
        item_by_unit = {u["unit_id"]: u["item_id"] for e in events
                        if e["kind"] == "create" for u in e["units"]}
        returned = defaultdict(set)
        for e in events:
            if e['kind'] == 'return':
                returned[e.get('ref')].update(e['unit_ids'])
        for e in events:
            if e['kind'] != 'trade':
                continue
            d = deals.get(e.get('ref'), {})
            qty = len(set(e['unit_ids']) - returned[e.get('ref')])
            trades.append(dict(deal_id=e.get('ref'), item_id=e.get('item_id') or d.get('item_id') or item_by_unit[e['unit_ids'][0]],
                seller=e['seller'], buyer=e['owner'], at=e['at'], quantity_kg=qty,
                delivered_quantity_kg=len(e['unit_ids']), unit_price=e.get('unit_price'),
                accepted_at=d.get('deal_at'), listing_id=d.get('listing_id'),
                offer_id=d.get('offer_id')))
    else:
        for d in deals.values():
            if d.get('status') != 'delivered' or d.get('delivery_at') is None:
                continue
            delivered = d.get('received_qty', d['qty'])
            trades.append(dict(deal_id=d['id'], item_id=d['item_id'],
                seller=d['seller_id'], buyer=d['buyer_id'], at=d['delivery_at'],
                quantity_kg=max(0, delivered-d.get('returned_qty', 0)),
                delivered_quantity_kg=delivered, unit_price=d.get('unit_price'),
                accepted_at=d.get('deal_at'), listing_id=d.get('listing_id'),
                offer_id=d.get('offer_id')))
    pairs = defaultdict(list)
    for t in trades:
        if t['quantity_kg'] <= 0 or t['seller'] == t['buyer']:
            continue
        t['day'] = int(t['at']//1440)+1
        t['revenue_net'] = (t['quantity_kg']*t['unit_price']
                            if t['unit_price'] is not None else None)
        pairs[(t['item_id'], *sorted((t['seller'], t['buyer'])))].append(t)
    groups = []
    for (item, a, b), ts in sorted(pairs.items()):
        ts.sort(key=lambda t: t['at'])
        if {t['seller'] for t in ts} != {a, b}:
            continue
        first = max(min(t['at'] for t in ts if t['seller'] == seller) for seller in (a,b))
        directions = []
        for seller, buyer in ((a,b),(b,a)):
            selected = [t for t in ts if t['seller'] == seller]
            directions.append(dict(seller=seller, buyer=buyer, trade_count=len(selected),
                quantity_kg=sum(t['quantity_kg'] for t in selected),
                revenue_net=(sum(t['revenue_net'] for t in selected)
                             if all(t['revenue_net'] is not None for t in selected) else None)))
        groups.append(dict(item_id=item, agents=[a,b], day=int(first//1440)+1,
            first_reciprocal_at=first, last_trade_day=ts[-1]['day'],
            directions=directions, trades=ts))
    return dict(detected=bool(groups), pair_count=len(groups), groups=groups,
        trade_count=sum(len(g['trades']) for g in groups),
        limitation='Same-item two-firm reciprocal deliveries, net of recorded returns at analysis time. Different lots qualify. Whole-run grouping does not prove coordinated intent, accounting impropriety, or a matched swap; three-party-only cycles are excluded. Retrospective dates may change after returns.')
