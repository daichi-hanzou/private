from types import SimpleNamespace

from circular_coffee.llm_clients import ACTION_JSON_SCHEMA, OpenAIClient
from circular_coffee.models import AgentAction


class FakeCompletions:
    def __init__(self):
        self.request = None

    def create(self, **kwargs):
        self.request = kwargs
        return SimpleNamespace(
            choices=[SimpleNamespace(message=SimpleNamespace(content='{"action_type":"wait"}'))],
            usage=SimpleNamespace(prompt_tokens=12, completion_tokens=3),
        )


def test_action_schema_is_strict_and_matches_agent_action() -> None:
    schema = ACTION_JSON_SCHEMA["schema"]
    assert ACTION_JSON_SCHEMA["strict"] is True
    assert schema["type"] == "object"
    assert schema["additionalProperties"] is False
    assert schema["required"] == ["action"]
    for branch in schema["properties"]["action"]["anyOf"]:
        assert branch["additionalProperties"] is False
        assert set(branch["properties"]).issubset(AgentAction.__dataclass_fields__)
        assert set(branch["required"]) == set(branch["properties"])


def test_openai_client_requests_json_schema_and_records_metadata() -> None:
    completions = FakeCompletions()
    sdk = SimpleNamespace(chat=SimpleNamespace(completions=completions))
    client = OpenAIClient(
        model="mock-model",
        temperature=0.0,
        seed=7,
        supports_seed=True,
        client=sdk,
    )
    raw = client.generate_action("system prompt", {"day": 1})
    assert raw == '{"action_type":"wait"}'
    assert completions.request["response_format"]["type"] == "json_schema"
    assert completions.request["seed"] == 7
    assert client.last_call_metadata["input_tokens"] == 12
    assert client.last_call_metadata["output_tokens"] == 3
    assert "api_key" not in client.last_call_metadata


def test_optional_temperature_and_seed_are_omitted_by_default() -> None:
    completions = FakeCompletions()
    sdk = SimpleNamespace(chat=SimpleNamespace(completions=completions))
    client = OpenAIClient(model="gpt-5-test", seed=7, client=sdk)
    client.generate_action("system prompt", {"day": 1})
    assert "temperature" not in completions.request
    assert "seed" not in completions.request
    assert client.last_call_metadata["seed_requested"] == 7
    assert client.last_call_metadata["seed_sent"] is None
