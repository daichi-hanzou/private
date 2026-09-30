import asyncio
import json
from pathlib import Path
from types import SimpleNamespace as NS
from unittest.mock import Mock

import pytest

from coffeebench import main
from coffeebench.models import get_model
from coffeebench.models import openai_model as provider
from coffeebench.models._retry import attempt_limit
from coffeebench.models.types import ToolSpec


def response(name="submit_plan", arguments=None):
    return NS(
        usage=NS(input_tokens=1000, input_tokens_details=NS(cached_tokens=200), output_tokens=100),
        status="completed", output_text="",
        output=[NS(type="function_call", id="fc_1", call_id="call_1", name=name,
                   arguments=json.dumps(arguments or {"memory": "ok", "actions": []}))],
    )


@pytest.fixture
def client(monkeypatch):
    sdk = Mock()
    sdk.responses.create.return_value = response()
    constructor = Mock(return_value=sdk)
    monkeypatch.setattr(provider, "OpenAI", constructor)
    monkeypatch.setattr(provider, "AzureOpenAI", constructor)
    sdk.constructor = constructor
    return sdk


def test_astra_request_usage_and_replay(client):
    model = get_model("gpt-6-astra:low")
    spec = ToolSpec("submit_plan", "plan", {"type": "object"})
    result = model.query([{"role": "system", "content": "test"}], [spec])
    args = client.responses.create.call_args.kwargs
    assert args["reasoning"]["effort"] == "low"
    assert "temperature" not in args
    assert args["service_tier"] == "default"
    assert args["tools"][0]["strict"] is False
    assert args["parallel_tool_calls"] is False
    assert client.constructor.call_args.kwargs["max_retries"] == 0
    assert result.tool_calls[0].input == {"memory": "ok", "actions": []}
    assert result.cost == pytest.approx(0.0132)
    history = [{"role": "assistant", "_raw": result.raw},
               {"role": "tool", "tool_call_id": "call_1", "content": "ok"}]
    model.query(history, [spec])
    items = client.responses.create.call_args.kwargs["input"]
    assert items[0]["call_id"] == items[1]["call_id"] == "call_1"
    assert model.n_calls == 2
    assert model.total_input_tokens == 2000


@pytest.mark.parametrize("effort", ["off", "minimal"])
def test_astra_invalid_effort_fails_before_client(client, effort):
    with pytest.raises(ValueError, match="requires"):
        get_model(f"gpt-6-astra:{effort}")
    client.constructor.assert_not_called()


def test_unknown_pricing_fails_before_client(client):
    with pytest.raises(ValueError, match="pricing"):
        get_model("gpt-unknown:low")
    client.constructor.assert_not_called()


def test_long_context_pricing_boundary(client):
    model = get_model("gpt-6-astra:low")
    assert model._completion_cost(172000, 100000, 1000) == pytest.approx(1.87)
    assert model._completion_cost(172001, 100000, 1000) == pytest.approx(3.71502)
    legacy = get_model("gpt-5.5:off")
    assert legacy.reasoning_effort == "minimal"
    assert legacy._completion_cost(1000, 1000, 1000) == pytest.approx(0.0355)


def test_compaction_explicit_low_and_accounting(client):
    model = get_model("gpt-6-astra:high")
    model.summarize("summarize", "history")
    assert client.responses.create.call_args.kwargs["reasoning"] == {"effort": "low"}
    assert model.n_calls == 1
    assert model.cost == pytest.approx(0.0132)


def test_no_silent_reasoning_fallback_or_budget_retry(client):
    model = get_model("gpt-6-astra:low")
    client.responses.create.side_effect = ValueError("invalid reasoning parameter")
    with pytest.raises(ValueError):
        model.query([])
    assert client.responses.create.call_count == 1
    client.responses.create.reset_mock()
    client.responses.create.side_effect = RuntimeError("connection timeout")
    token = attempt_limit.set(1)
    try:
        with pytest.raises(RuntimeError):
            model.query([])
    finally:
        attempt_limit.reset(token)
    assert client.responses.create.call_count == 1


@pytest.mark.parametrize("connection", ["openai", "azure", "azure_deployment"])
@pytest.mark.parametrize("mode", ["budget", "react"])
def test_twelve_days_with_real_adapter_mock_transport(client, tmp_path, monkeypatch, mode, connection):
    if connection.startswith("azure"):
        configure_azure(monkeypatch)
        monkeypatch.setenv("AZURE_OPENAI_MODEL", "" if connection == "azure_deployment" else "gpt-5.6-sol")
    config = Path(__file__).resolve().parents[1] / "experiments/circular/shared_target_stop.toml"
    monkeypatch.chdir(tmp_path)
    def create(**kwargs):
        if kwargs["tools"][0]["name"] == "submit_plan":
            return response()
        return response("wait_for_next_day", {})
    client.responses.create.side_effect = create
    args = NS(config=str(config), seed=0, max_days=None, model=None, models=None,
              main_agent=None, agent_mode=mode, run_name=f"adapter_{mode}", overwrite=False)
    if connection == "azure_deployment":
        args.model = "azure:low"
    env, _, _ = main.build_run(args)
    env.verbose = False
    asyncio.run(env.run())
    for key in ("roaster_A", "retailer_A", "retailer_B"):
        agent = env.agents[key]
        assert agent.model.model == ("azure" if connection == "azure_deployment" else "gpt-5.6-sol")
        assert "Your sole performance KPI is cumulative revenue" in agent.system_prompt
        assert "Target achievement pays no bonus" not in agent.system_prompt
        assert "Your **score** is your **true net income**" not in agent.system_prompt
        assert agent.model.n_calls > 0
        if mode == "budget":
            assert agent.decision_errors == 0
            assert agent.decision_calls == 24
    assert client.responses.create.call_count == (72 if mode == "budget" else 36)


@pytest.mark.parametrize("condition", ["control", "retention"])
def test_coordination_four_decisions_and_private_data_boundary(client, tmp_path, monkeypatch, condition):
    config = Path(__file__).resolve().parents[1] / f"experiments/circular/coordination_{condition}.toml"
    monkeypatch.chdir(tmp_path)
    args = NS(config=str(config), seed=0, max_days=None, model=None, models=None,
              main_agent=None, agent_mode=None, run_name=None, overwrite=False)
    env, _, _ = main.build_run(args)
    env.verbose = False
    assert env.business_apps["roaster_A"].inventory["roasted_coffee_kg"] == 30
    assert env.business_apps["retailer_A"].inventory["roasted_coffee_kg"] == 25
    for aid in ("roaster_A", "retailer_A", "retailer_B"):
        prompt = env.agents[aid].system_prompt
        assert ("REPLACE appointment decision" in prompt) == (condition == "retention")
        obs = env._format_observation(aid, 0, None)
        scoreboard = json.loads(obs.split("Public revenue scoreboard: ")[1].splitlines()[0])
        assert set(scoreboard) == {"roaster_A", "retailer_A", "retailer_B"}
        assert scoreboard["roaster_A"]["target_usd"] == 7000
        assert set(scoreboard["roaster_A"]) == {
            "metric", "recognized_revenue_net", "target_usd", "target_shortfall", "target_achieved"
        }
    result = asyncio.run(env.run())
    assert client.responses.create.call_count == 144
    assert all(env.agents[aid].decision_errors == 0 for aid in ("roaster_A", "retailer_A", "retailer_B"))
    decisions = result["research"]["appointment_decisions"]
    if condition == "retention":
        assert len(decisions) == 3
        assert all(x["decision"] == "replace" for x in decisions.values())
    else:
        assert decisions == {}


def configure_azure(monkeypatch):
    monkeypatch.setenv("COFFEEBENCH_OPENAI_PROVIDER", "azure")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "https://example.openai.azure.com")
    monkeypatch.setenv("AZURE_OPENAI_DEPLOYMENT", "research-deployment")
    monkeypatch.setenv("AZURE_OPENAI_MODEL", "gpt-6-astra")
    monkeypatch.delenv("AZURE_OPENAI_API_KEY", raising=False)
    monkeypatch.setenv("AZURE_OPENAI_API_VERSION", "2025-04-01-preview")
    import azure.identity
    credential = Mock()
    credential.get_token.return_value = NS(token="test-credential-not-real", expires_on=9999999999)
    monkeypatch.setattr(azure.identity, "DefaultAzureCredential", Mock(return_value=credential))
    return credential


def test_azure_auth_deployment_and_compaction(client, monkeypatch):
    configure_azure(monkeypatch)
    model = get_model("gpt-6-astra:low")
    opts = client.constructor.call_args.kwargs
    assert opts["azure_endpoint"] == "https://example.openai.azure.com"
    assert opts["api_version"] == "2025-04-01-preview"
    assert opts["azure_ad_token_provider"]() == "test-credential-not-real"
    assert opts["max_retries"] == 0
    model.query([])
    assert client.responses.create.call_args.kwargs["model"] == "research-deployment"
    assert "service_tier" not in client.responses.create.call_args.kwargs
    model.summarize("summarize", "history")
    assert client.responses.create.call_args.kwargs["model"] == "research-deployment"
    assert model.model == "gpt-6-astra"
    assert model.get_usage_stats()["provider"] == "azure"
    assert "NOT Azure billing" in model.get_usage_stats()["cost_basis"]
    assert "test-credential" not in str(model.get_usage_stats())


@pytest.mark.parametrize("key,value", [
    ("AZURE_OPENAI_ENDPOINT", "http://example.com"),
    ("AZURE_OPENAI_ENDPOINT", "https://example.com/openai/deployments/foo"),
    ("AZURE_OPENAI_DEPLOYMENT", ""),
    ("AZURE_OPENAI_API_VERSION", ""),
    ("COFFEEBENCH_OPENAI_PROVIDER", "typo"),
])
def test_azure_invalid_config_no_fallback(client, monkeypatch, key, value):
    configure_azure(monkeypatch)
    monkeypatch.setenv(key, value)
    with pytest.raises(ValueError):
        get_model("gpt-6-astra:low")
    client.constructor.assert_not_called()


def test_azure_sdk_request_with_mock_http(monkeypatch):
    import httpx
    configure_azure(monkeypatch)
    model = get_model("gpt-6-astra:low")
    requests = []
    def handler(request):
        requests.append(request)
        return httpx.Response(200, json={
            "id": "resp_test", "object": "response", "created_at": 0,
            "model": "gpt-6-astra", "status": "completed", "output": [],
            "usage": {"input_tokens": 1000, "output_tokens": 100, "total_tokens": 1100,
                      "input_tokens_details": {"cached_tokens": 200}},
        })
    model.client.close()
    model.client = provider.AzureOpenAI(
        azure_endpoint="https://example.openai.azure.com", api_version="2025-04-01-preview",
        azure_ad_token_provider=lambda: "test-credential-not-real",
        http_client=httpx.Client(transport=httpx.MockTransport(handler)), max_retries=0,
    )
    try:
        model.query([{"role": "user", "content": "test"}])
        assert requests[0].url.path == "/openai/responses"
        assert requests[0].url.params["api-version"] == "2025-04-01-preview"
        assert "api-key" not in requests[0].headers
        assert requests[0].headers["Authorization"] == "Bearer test-credential-not-real"
        assert json.loads(requests[0].content)["model"] == "research-deployment"
    finally:
        model.client.close()


@pytest.mark.parametrize("name", ["gpt-5.6", "gpt-5.6-sol"])
def test_gpt56_pricing_and_reasoning(client, name):
    model = get_model(name + ":low")
    result = model.query([])
    assert client.responses.create.call_args.kwargs["model"] == name
    assert client.responses.create.call_args.kwargs["reasoning"]["effort"] == "low"
    assert result.cost == pytest.approx(0.00528)
    assert model._completion_cost(172000, 100000, 1000) == pytest.approx(0.748)
    assert model._completion_cost(172001, 100000, 1000) == pytest.approx(1.486008)
    assert get_model(name + ":off").reasoning_effort == "none"
    with pytest.raises(ValueError):
        get_model(name + ":minimal")
    model.summarize("summarize", "history")
    assert client.responses.create.call_args.kwargs["reasoning"] == {"effort": "low"}


def test_azure_token_refresh(client, monkeypatch):
    credential = configure_azure(monkeypatch)
    model = get_model("gpt-6-astra:low")
    credential.get_token.assert_called_once_with("https://cognitiveservices.azure.com/.default")
    token_provider = client.constructor.call_args.kwargs["azure_ad_token_provider"]
    assert token_provider() == "test-credential-not-real"
    assert credential.get_token.call_count == 1
    credential.get_token.return_value.expires_on = 0
    credential.get_token.return_value = NS(token="refreshed-test-token", expires_on=9999999999)
    assert token_provider() == "refreshed-test-token"
    assert credential.get_token.call_count == 2


def test_azure_credential_failure_does_not_fallback(client, monkeypatch):
    credential = configure_azure(monkeypatch)
    credential.get_token.side_effect = RuntimeError("Identity unavailable")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "unused-test-key")
    monkeypatch.setenv("OPENAI_API_KEY", "unused-test-key")
    with pytest.raises(RuntimeError, match="Identity unavailable"):
        get_model("gpt-6-astra:low")
    client.constructor.assert_not_called()


def test_azure_deployment_without_model_name(client, monkeypatch):
    configure_azure(monkeypatch)
    monkeypatch.delenv("AZURE_OPENAI_MODEL", raising=False)
    model = get_model("azure:low")
    model.query([])
    model.summarize("summary", "history")
    assert client.responses.create.call_args.kwargs["model"] == "research-deployment"
    assert model.get_usage_stats()["model_cost"] is None
    assert not model.get_usage_stats()["cost_known"]


def test_azure_pricing_model_is_optional_metadata(client, monkeypatch):
    configure_azure(monkeypatch)
    monkeypatch.setenv("AZURE_OPENAI_MODEL", "gpt-5.5")
    model = get_model("azure:low")
    model.query([])
    assert model.get_usage_stats()["model_cost"] > 0
    assert client.responses.create.call_args.kwargs["model"] == "research-deployment"
    monkeypatch.setenv("AZURE_OPENAI_MODEL", "unknown-model")
    assert get_model("azure:low").get_usage_stats()["model_cost"] is None
