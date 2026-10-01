import asyncio
from types import SimpleNamespace as NS
from unittest.mock import Mock
import httpx
import pytest
from coffeebench.models import get_model
from coffeebench.models import azure_openai_model as adapter
from coffeebench.models.types import ToolSpec
from coffeebench import main
from pathlib import Path

@pytest.fixture
def azure(monkeypatch):
    monkeypatch.setenv('AZURE_OPENAI_ENDPOINT', 'https://example.openai.azure.com')
    monkeypatch.setenv('AZURE_OPENAI_API_VERSION', '2024-10-21')
    monkeypatch.setenv('AZURE_OPENAI_DEPLOYMENT', 'my-deployment')
    monkeypatch.delenv('AZURE_OPENAI_MODEL', raising=False)
    credential = Mock()
    credential.get_token.return_value = NS(token='test-token', expires_on=9999999999)
    monkeypatch.setattr(adapter, 'DefaultAzureCredential', Mock(return_value=credential))
    return credential


def test_chat_request_and_refresh(azure):
    model = get_model('azure:low')
    model.client.close()
    requests = []
    def handle(req):
        requests.append(req)
        return httpx.Response(200, json={'id':'c1','object':'chat.completion','created':0,
            'model':'my-deployment','choices':[{'index':0,'finish_reason':'tool_calls',
            'message':{'role':'assistant','content':None,'tool_calls':[{'id':'t1','type':'function',
            'function':{'name':'wait_for_next_day','arguments':'{}'}}]}}],
            'usage':{'prompt_tokens':20,'completion_tokens':5,'total_tokens':25}})
    model.client = adapter.AzureOpenAI(azure_endpoint='https://example.openai.azure.com',
        api_version='2024-10-21',azure_ad_token_provider=model._token_provider,
        http_client=httpx.Client(transport=httpx.MockTransport(handle)))
    spec = ToolSpec('wait_for_next_day','wait',{'type':'object'})
    try:
        first = model.query([{'role':'user','content':'go'}],[spec])
        azure.get_token.return_value.expires_on = 0
        azure.get_token.return_value = NS(token='renewed-token',expires_on=9999999999)
        model.query([{'role':'assistant','content':'','tool_calls':first.tool_calls},
                     {'role':'tool','tool_call_id':'t1','content':'done'}],[spec])
        model.summarize('summarize','history')
        assert requests[0].url.path == '/openai/deployments/my-deployment/chat/completions'
        assert requests[0].url.params['api-version'] == '2024-10-21'
        assert requests[1].headers['Authorization'] == 'Bearer renewed-token'
        assert 'api-key' not in requests[0].headers
        import json
        body = json.loads(requests[1].content)
        assert body['messages'][0]['tool_calls'][0]['id'] == 't1'
        assert body['tools'][0]['function']['name'] == 'wait_for_next_day'
        assert body['reasoning_effort'] == 'low'
        assert 'input' not in body
        assert model.get_usage_stats()['model_cost'] is None
    finally:
        model.client.close()


@pytest.mark.parametrize("preset,days", [("revenue_zero",12), ("revenue_demand20_day4_25days_azure",25)])
def test_azure_react_twelve_days(azure, monkeypatch, tmp_path,preset,days):
    client = Mock()
    client.chat.completions.create.return_value = NS(
        usage=NS(prompt_tokens=10,completion_tokens=2),
        choices=[NS(finish_reason='tool_calls',message=NS(content='',tool_calls=[
            NS(id='wait',function=NS(name='wait_for_next_day',arguments='{}'))]))])
    monkeypatch.setattr(adapter,'AzureOpenAI',Mock(return_value=client))
    config = Path(__file__).resolve().parents[1]/'experiments/minimal'/f'{preset}.toml'
    monkeypatch.chdir(tmp_path)
    from coffeebench import environment
    monkeypatch.setattr(environment,'CONSUMER_DEMAND_ENABLED',True)
    env, _, _ = main.build_run(NS(config=str(config),model='azure:low',models=None,
                                seed=0,max_days=None,main_agent=None))
    env.verbose=False
    result=asyncio.run(env.run())
    assert env.max_days == days
    if preset == "revenue_zero":
        assert env.consumer_sales_log == []
    else:
        assert env.consumer_sales_log
    assert all(a['usage']['cost'] is None for a in result['agents'].values())
    assert all(a.model.n_calls >= days for a in env.agents.values())
