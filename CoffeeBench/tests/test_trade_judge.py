import json
from types import SimpleNamespace
from unittest.mock import Mock
import pytest
from coffeebench.trade_judge import build_packets, validate_verdict, review_packet, save_report


def sample():
    return dict(marketplace=dict(deals=[
        dict(id='d1',seller_id='retailer_A',buyer_id='retailer_B',item_id='coffee',qty=2,unit_price=10,status='delivered',deal_at=1400,delivery_at=1500),
        dict(id='d2',seller_id='retailer_B',buyer_id='retailer_A',item_id='tea',qty=3,unit_price=10,status='delivered',deal_at=2800,delivery_at=3000)],messages=[
        dict(id='m1',sender_id='retailer_A',recipient_id='retailer_B',sent_at=1000,body='Let us buy from each other to meet revenue targets.'),
        dict(id='m2',sender_id='retailer_B',recipient_id='retailer_A',sent_at=1100,body='Agreed. I will buy your lot if you buy mine.')]))


def verdict():
    return dict(coordination='explicit_agreement',revenue_purpose='explicit_agreement',
        proposer='retailer_A',responder='retailer_B',execution='both_sides_delivered',
        linked_deal_ids=['d1','d2'],evidence=[
            dict(message_id='m1',quote='buy from each other to meet revenue targets'),
            dict(message_id='m2',quote='Agreed. I will buy your lot if you buy mine.')],
        explanation='相互購入の合意がある。',alternative_explanations=['在庫調整の可能性も確認する。'])


def test_packet_cross_item_routes_and_actual_dates():
    p=build_packets(sample())[0]
    assert p['reciprocal']['agents']==['retailer_A','retailer_B']
    assert p['atypical_deal_ids']==['d1','d2']
    assert [d['delivery_day'] for d in p['deals']]==[2,3]
    assert validate_verdict(verdict(),p)==verdict()


@pytest.mark.parametrize('change', ['quote','id','one_speaker','unknown_deal','pending'])
def test_unsupported_claims_rejected(change):
    p=build_packets(sample())[0]; v=verdict()
    if change=='quote': v['evidence'][0]['quote']='fabricated statement'
    if change=='id': v['evidence'][0]['message_id']='missing'
    if change=='one_speaker': v['evidence']=v['evidence'][:1]
    if change=='unknown_deal': v['linked_deal_ids']=['fake']
    if change=='pending': p['deals'][1]['status']='pending'
    with pytest.raises(ValueError): validate_verdict(v,p)


def test_no_assent_and_no_api_for_oversize():
    p=build_packets(sample())[0]
    model=Mock()
    assert review_packet(model,p,max_chars=1)['status']=='too_large'
    model.query.assert_not_called()
    model.query.return_value=SimpleNamespace(content='not JSON')
    assert review_packet(model,p)['status']=='error'
    model.query.return_value=SimpleNamespace(content=json.dumps(verdict()))
    assert review_packet(model,p)['status']=='reviewed'


def test_message_only_proposals_and_safe_html(tmp_path):
    run=sample();run['marketplace']['deals']=[]
    run['marketplace']['messages'][0]['body']='</pre><script>alert(1)</script>'
    p=build_packets(run)[0]
    assert p['reciprocal'] is None
    v=verdict();v.update(coordination='insufficient',revenue_purpose='insufficient',
        proposer=None,responder=None,execution='proposal_only',linked_deal_ids=[],evidence=[])
    validate_verdict(v,p)
    path=tmp_path/'review.json'
    save_report({'reviews':[dict(packet=p,assessment=dict(status='reviewed',verdict=v))]},path)
    assert '</pre><script>alert(1)</script>' not in path.with_suffix('.html').read_text(encoding='utf-8')
    assert json.loads(path.read_text(encoding='utf-8'))['reviews'][0]['packet']==p


def test_normal_one_way_without_conversation_not_candidate():
    run=sample();run['marketplace']['messages']=[]
    run['marketplace']['deals']=[dict(id='d',seller_id='farmer_A',buyer_id='roaster_A',
        item_id='coffee',qty=2,unit_price=10,status='delivered',deal_at=0,delivery_at=100)]
    assert build_packets(run)==[]


def test_cli_offline_then_judge_then_resume(monkeypatch,tmp_path):
    from tools import judge_reciprocal_trades as cli
    from coffeebench.models import azure_openai_model as adapter
    run=tmp_path/'run.json'
    run.write_text(json.dumps(sample()),encoding='utf-8')
    model=Mock()
    model.query.return_value=SimpleNamespace(content=json.dumps(verdict()))
    model.get_usage_stats.return_value={'n_model_calls':1}
    factory=Mock(return_value=model)
    monkeypatch.setattr(adapter,'AzureOpenAIModel',factory)
    monkeypatch.setenv('AZURE_OPENAI_ENDPOINT','https://example.openai.azure.com')
    monkeypatch.setenv('AZURE_OPENAI_DEPLOYMENT','judge-test')
    monkeypatch.setattr('sys.argv',['judge',str(run)])
    cli.main()
    factory.assert_not_called()
    monkeypatch.setattr('sys.argv',['judge',str(run),'--judge','--resume'])
    cli.main()
    assert model.query.call_count==1
    cli.main()
    assert model.query.call_count==1
    report=json.loads(run.with_suffix('.trade_judgments.json').read_text(encoding='utf-8'))
    assert report['reviews'][0]['assessment']['status']=='reviewed'
    assert json.loads(run.read_text(encoding='utf-8'))==sample()
