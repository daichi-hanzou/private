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
    monkeypatch.delenv('AZURE_OPENAI_API_VERSION', raising=False)
    monkeypatch.setenv('AZURE_OPENAI_DEPLOYMENT', 'my-deployment')
    monkeypatch.delenv('AZURE_OPENAI_MODEL', raising=False)
    credential = Mock()
    credential.get_token.return_value = NS(token='test-token', expires_on=9999999999)
    monkeypatch.setattr(adapter, 'DefaultAzureCredential', Mock(return_value=credential))
    return credential


@pytest.mark.parametrize('effort', ['low', 'off'])
def test_responses_request_and_refresh(azure, effort):
    model = get_model(f'azure:{effort}')
    model.client.close()
    requests = []
    def handle(req):
        requests.append(req)
        return httpx.Response(200, json={'id':'r1','object':'response','created_at':0,
            'model':'my-deployment','status':'completed','output':[
                {'id':'rs1','type':'reasoning','summary':[{'type':'summary_text','text':'plan'}]},
                {'id':'fc1','type':'function_call','call_id':'t1',
                 'name':'wait_for_next_day','arguments':'{}'}],
            'usage':{'input_tokens':20,'output_tokens':5,'total_tokens':25}})
    model.client = adapter.OpenAI(base_url='https://example.openai.azure.com/openai/v1/',
        api_key=model._token_provider,
        http_client=httpx.Client(transport=httpx.MockTransport(handle)))
    spec = ToolSpec('wait_for_next_day','wait',{'type':'object'})
    try:
        first = model.query([{'role':'user','content':'go'}],[spec])
        azure.get_token.return_value.expires_on = 0
        azure.get_token.return_value = NS(token='renewed-token',expires_on=9999999999)
        model.query([{'role':'assistant','_raw':first.raw},
                     {'role':'tool','tool_call_id':'t1','content':'done'}],[spec])
        model.summarize('summarize','history')
        assert requests[0].url.path == '/openai/v1/responses'
        assert 'api-version' not in requests[0].url.params
        assert requests[1].headers['Authorization'] == 'Bearer renewed-token'
        assert 'api-key' not in requests[0].headers
        import json
        body = json.loads(requests[1].content)
        assert body['input'][0]['type'] == 'reasoning'
        assert body['input'][1]['call_id'] == body['input'][2]['call_id'] == 't1'
        assert body['tools'][0]['name'] == 'wait_for_next_day'
        for request in requests:
            payload = json.loads(request.content)
            if effort == 'off':
                assert 'reasoning' not in payload
            else:
                assert payload['reasoning']['effort'] == 'low'
        assert 'messages' not in body and 'reasoning_effort' not in body
        assert model.get_usage_stats()['model_cost'] is None
    finally:
        model.client.close()


@pytest.mark.parametrize("preset,days", [("revenue_zero",12), ("revenue_demand20_day4_25days_azure",25), ("revenue_demand20_day4_12days_public_targets_azure",12)])
def test_azure_react_twelve_days(azure, monkeypatch, tmp_path,preset,days):
    client = Mock()
    client.responses.create.return_value = NS(
        usage=NS(input_tokens=10,output_tokens=2), status='completed',
        output=[NS(type='function_call', id='fc1', call_id='wait',
                   name='wait_for_next_day', arguments='{}')])
    monkeypatch.setattr(adapter,'OpenAI',Mock(return_value=client))
    config = Path(__file__).resolve().parents[1]/'experiments/minimal'/f'{preset}.toml'
    monkeypatch.chdir(tmp_path)
    from coffeebench import environment
    monkeypatch.setattr(environment,'CONSUMER_DEMAND_ENABLED',True)
    env, _, _ = main.build_run(NS(config=str(config),model='azure:low',models=None,
                                seed=0,max_days=None,main_agent=None))
    env.verbose=False
    result=asyncio.run(env.run())
    assert env.max_days == days
    assert len(env.agents) == 6
    assert all(a.model.model == "azure" for a in env.agents.values())
    assert all(c.kwargs["reasoning"]["effort"] == "low" for c in client.responses.create.call_args_list)
    if preset == "revenue_zero":
        assert env.consumer_sales_log == []
    else:
        assert env.consumer_sales_log
    assert all(a['usage']['cost'] is None for a in result['agents'].values())
    assert all(a.model.n_calls >= days for a in env.agents.values())


def cli_timeout():
    import subprocess
    error = adapter.CredentialUnavailableError('Failed to invoke the Azure CLI')
    error.__cause__ = subprocess.TimeoutExpired(['az', 'account', 'get-access-token'], 60)
    return error


def test_initial_cli_timeout_retries_then_succeeds(azure, monkeypatch):
    sleep = Mock()
    monkeypatch.setattr(adapter.time, 'sleep', sleep)
    token = NS(token='test-token', expires_on=9999999999)
    azure.get_token.side_effect = [cli_timeout(), cli_timeout(), token]
    model = get_model('azure:low')
    try:
        adapter.DefaultAzureCredential.assert_called_once_with(process_timeout=60)
        assert model._token_provider() == 'test-token'
        assert azure.get_token.call_count == 3
        assert [c.args[0] for c in sleep.call_args_list] == [2, 4]
    finally:
        model.client.close()


def test_refresh_cli_timeout_retries_without_losing_cached_token(azure, monkeypatch):
    monkeypatch.setattr(adapter.time, 'sleep', Mock())
    model = get_model('azure:low')
    try:
        model._token.expires_on = 0
        azure.get_token.side_effect = [cli_timeout(), NS(token='renewed', expires_on=9999999999)]
        assert model._token_provider() == 'renewed'
        assert model._token_provider() == 'renewed'
        assert azure.get_token.call_count == 3  # initial + two refresh attempts
    finally:
        model.client.close()


def test_cli_timeout_exhaustion_is_bounded(azure, monkeypatch):
    sleep = Mock()
    monkeypatch.setattr(adapter.time, 'sleep', sleep)
    model = get_model('azure:low')
    try:
        cached = model._token
        cached.expires_on = 0
        error = cli_timeout()
        azure.get_token.side_effect = error
        with pytest.raises(adapter.CredentialUnavailableError) as caught:
            model._token_provider()
        assert caught.value is error
        assert model._token is cached
        assert azure.get_token.call_count == 4  # initial + three attempts
        assert sleep.call_count == 2
    finally:
        model.client.close()


@pytest.mark.parametrize('error', [
    adapter.CredentialUnavailableError('Please run az login'),
    adapter.CredentialUnavailableError('Failed to invoke the Azure CLI'),
    ValueError('configuration error'),
])
def test_non_timeout_auth_errors_are_not_retried(azure, monkeypatch, error):
    sleep = Mock()
    monkeypatch.setattr(adapter.time, 'sleep', sleep)
    azure.get_token.side_effect = error
    with pytest.raises(type(error)) as caught:
        get_model('azure:low')
    assert caught.value is error
    assert azure.get_token.call_count == 1
    sleep.assert_not_called()
