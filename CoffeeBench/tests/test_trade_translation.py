import copy
import json
from types import SimpleNamespace
from unittest.mock import Mock
from coffeebench.trade_translation import attach_for_display,content_key,translate_batch
from coffeebench.trade_review import write_html


def test_translation_is_display_only_and_content_key_changes():
    m=dict(id='m1',title='Trade',body='Buy 23 kg at $30 NET7.')
    key=content_key(m)
    report=dict(reviews=[dict(packet=dict(messages=[m]),assessment=dict(status='not_reviewed'))],
                translations_ja={key:dict(status='translated',title_ja='取引',body_ja='23 kgを$30 NET7で購入。')})
    original=copy.deepcopy(report)
    view=attach_for_display(report)
    assert view['reviews'][0]['packet']['messages'][0]['display_translation_ja']['body_ja'].startswith('23')
    assert report==original
    m['body']='Changed body'
    assert 'display_translation_ja' not in attach_for_display(report)['reviews'][0]['packet']['messages'][0]


def test_batch_checks_numbers_and_ids_and_retains_failed_text():
    model=Mock()
    batch=[dict(key='a',title='Trade',body='Buy 23 kg at $30 NET7 on lst_abc.')]
    model.query.return_value=SimpleNamespace(content=json.dumps({'translations':[
        dict(key='a',title_ja='取引',body_ja='lst_abcで23 kgを$30 NET7で購入。')]}))
    assert translate_batch(model,batch)['a']['status']=='translated'
    model.query.return_value=SimpleNamespace(content=json.dumps({'translations':[
        dict(key='a',title_ja='取引',body_ja='lst_abcで24 kgを$30 NET7で購入。')]}))
    result=translate_batch(model,batch)['a']
    assert result['status']=='needs_review' and '24' in result['body_ja']
    model.query.return_value=SimpleNamespace(content='malformed')
    assert translate_batch(model,batch)['a']['status']=='error'


def test_translation_html_escaping(tmp_path):
    m=dict(id='m',title='',body='Hello')
    report=dict(reviews=[dict(packet={'messages':[m]})],translations_ja={content_key(m):dict(
        status='translated',body_ja='</script><script>alert(1)</script>',title_ja='')})
    path=tmp_path/'report.html';write_html(report,path)
    text=path.read_text(encoding='utf-8')
    assert '</script><script>alert(1)' not in text
    payload=text.split('<script id="data" type="application/json">')[1].split('</script>')[0]
    assert json.loads(payload)['reviews'][0]['packet']['messages'][0]['display_translation_ja']['body_ja'].startswith('</script>')


def test_cli_resume_does_not_call_model_twice(tmp_path,monkeypatch):
    from tools.translate_trade_review import main
    from coffeebench.models import azure_openai_model as adapter
    m=dict(id='m',title='Hello',body='Thank you.')
    source=tmp_path/'report.json'
    source.write_text(json.dumps(dict(reviews=[dict(packet=dict(messages=[m]),assessment=dict(status='not_reviewed'))])),encoding='utf-8')
    model=Mock()
    model.query.return_value=SimpleNamespace(content=json.dumps({'translations':[
        dict(key=content_key(m),title_ja='こんにちは',body_ja='ありがとう。')]}))
    model.get_usage_stats.return_value={}
    factory=Mock(return_value=model)
    monkeypatch.setattr(adapter,'AzureOpenAIModel',factory)
    monkeypatch.setattr('sys.argv',['translate',str(source),'--resume'])
    main();main()
    assert model.query.call_count==1 and factory.call_count==1
    result=json.loads(source.with_name('report.ja.json').read_text(encoding='utf-8'))
    assert result['reviews'][0]['packet']['messages'][0]['body']=='Thank you.'
    assert result['translations_ja'][content_key(m)]['body_ja']=='ありがとう。'
