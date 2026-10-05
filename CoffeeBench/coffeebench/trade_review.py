"""Deterministic delivery evidence and readable trade-review HTML."""
import json
from collections import defaultdict
from pathlib import Path


def enrich_deals(deals, events=None):
    deliveries=defaultdict(list)
    returns=defaultdict(set)
    for e in events or []:
        if e['kind']=='trade': deliveries[e.get('ref')].append(e)
        if e['kind']=='return': returns[e.get('ref')].update(e['unit_ids'])
    result=[]
    for source in deals:
        d=dict(source)
        if events is None and 'delivery_facts' in d:
            result.append(d)
            continue
        actual=deliveries.get(d['id'],[])
        if actual:
            quantity=sum(len(e['unit_ids']) for e in actual)
            at=min(e['at'] for e in actual)
            evidence='provenance'
        elif d.get('status')=='delivered':
            quantity=d.get('received_qty',d.get('qty'))
            at=d.get('delivery_at')
            evidence='deal_status'
        else:
            quantity=None
            at=None
            evidence='not_confirmed'
        returned=max(d.get('returned_qty',0) or 0,len(returns.get(d['id'],set())))
        price=d.get('unit_price')
        net=max(0,quantity-returned) if quantity is not None else None
        d['delivery_facts']=dict(confirmed=quantity is not None and quantity>0,
            evidence=evidence,delivered_qty=quantity,returned_qty=returned,net_qty=net,
            actual_at=at,actual_day=int(at//1440)+1 if at is not None else None,
            delivered_amount=quantity*price if quantity is not None and price is not None else None,
            net_amount=net*price if net is not None and price is not None else None)
        result.append(d)
    return result


def execution_facts(deals):
    normalized=[d if 'delivery_facts' in d else enrich_deals([d])[0] for d in deals]
    delivered=[d for d in normalized if d['delivery_facts']['confirmed']]
    sellers={d['seller_id'] for d in delivered}
    state=('both_sides_delivered' if len(sellers)==2 else
           'one_side_delivered' if len(sellers)==1 else 'no_confirmed_delivery')
    return dict(delivery_state=state,delivered_deal_ids=[d['id'] for d in delivered],
                contract_count=len(deals),
                unconfirmed_deal_ids=[d['id'] for d in normalized if not d['delivery_facts']['confirmed']],
                returned_deal_ids=[d['id'] for d in normalized if d['delivery_facts']['returned_qty']>0],
                note='配送の事実と返品を分けた集計。未確認は未配送の断定ではありません。')


def write_html(report,path):
    # Never interpolate evidence into markup. The browser uses textContent.
    from coffeebench.trade_translation import attach_for_display
    payload=json.dumps(attach_for_display(report),ensure_ascii=False).replace('&','\\u0026').replace('<','\\u003c')
    template=Path(__file__).with_name('trade_review.html').read_text(encoding='utf-8')
    Path(path).write_text(template.replace('__REPORT_DATA__',payload),encoding='utf-8')
