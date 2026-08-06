import json
from pathlib import Path

import pytest

from agentledger.bundles import build_action_bundles, outcome_status
from agentledger.html import render_explorer
from agentledger.ingestion import read_jsonl
from agentledger.normalizer import normalize_events
from agentledger.models import IngestionResult


EXAMPLES = Path(__file__).parents[1] / "examples"


def _load_example(name: str):
    ingestion = read_jsonl(EXAMPLES / name)
    return ingestion, normalize_events(ingestion.events)


def _explorer_actions(html: str) -> list[dict]:
    prefix = "window.AGENT_LEDGER="
    payload = html.split(prefix, 1)[1].split(";</script>", 1)[0]
    return json.loads(payload)["actions"]


def test_minimal_pending_audit_flow() -> None:
    ingestion, events = _load_example("minimal_audit_pending.jsonl")

    assert ingestion.loaded == 3
    assert ingestion.issues == []
    assert len(events) == 3

    bundles = build_action_bundles(events)
    assert len(bundles) == 1
    bundle = bundles[0]
    assert bundle.observation is not None
    assert bundle.decision is not None
    assert bundle.action is not None
    assert bundle.outcome is None
    assert outcome_status(bundle) == "pending"

    context = bundle.execution_context
    assert context.model_name == "example-agent-model"
    assert context.model_version == "2026-08"
    assert context.prompt_hash == "sha256:prompt-example-001"
    assert context.tool_version == "calendar-tool-1.0.0"
    assert context.config_hash == "sha256:config-example-001"
    assert context.git_commit == "0123456789abcdef"
    assert context.environment == {
        "name": "local",
        "timezone": "Asia/Tokyo",
    }


def test_minimal_confirmed_audit_flow() -> None:
    ingestion, events = _load_example("minimal_audit_confirmed.jsonl")

    assert ingestion.issues == []
    assert len(events) == 5

    bundles = build_action_bundles(events)
    assert len(bundles) == 1
    bundle = bundles[0]
    assert bundle.outcome is not None
    assert outcome_status(bundle) == "confirmed"
    assert len(bundle.human_interventions) == 1

    intervention = bundle.human_interventions[0]
    assert intervention.intervention_type == "accept"
    assert intervention.actor == "calendar_owner"
    assert intervention.reason == (
        "The selected time and event details are correct."
    )

    assert bundle.observation.event_id == "event-observation-001"
    assert bundle.decision.event_id == "event-decision-001"
    assert bundle.decision.decision_id == bundle.action.decision_id
    assert bundle.action.action_id == "action-calendar-001"
    assert bundle.outcome.action_id == bundle.action.action_id
    assert intervention.related_action_id == bundle.action.action_id
    assert {
        event.correlation_id for event in bundle.events
    } == {"correlation-calendar-001"}


@pytest.mark.parametrize(
    ("filename", "expected_status", "extra_text"),
    [
        ("minimal_audit_pending.jsonl", "pending", "Execution Context"),
        (
            "minimal_audit_confirmed.jsonl",
            "confirmed",
            "Human Intervention",
        ),
    ],
)
def test_minimal_audit_explorer(
    filename: str,
    expected_status: str,
    extra_text: str,
) -> None:
    ingestion, events = _load_example(filename)

    html = render_explorer(
        events,
        ingestion=ingestion,
        source_path=EXAMPLES / filename,
    )

    for text in (
        "AgentLedger",
        "Observation",
        "Decision",
        "Action",
        "Outcome",
        "Execution Context",
        extra_text,
        expected_status,
    ):
        assert text in html


def test_calendar_observation_payload_and_dynamic_rendering() -> None:
    ingestion, events = _load_example("minimal_audit_pending.jsonl")
    html = render_explorer(events, ingestion=ingestion)
    observation = _explorer_actions(html)[0]["observation"]

    assert observation["data"] == {
        "requested_title": "Project check-in",
        "requested_date": "2026-08-06",
        "available_slots": ["14:00", "15:00"],
        "request_context": {
            "duration_minutes": 30,
            "timezone": "Asia/Tokyo",
        },
    }
    assert observation["allowed_actions"] == [
        "create_calendar_event",
        "request_clarification",
    ]
    assert "inventory" not in observation
    assert "cash" not in observation
    assert "reported_revenue" not in observation
    assert 'jsonField("Inventory"' not in html
    assert "Object.entries(value.data" in html
    assert 'observationField("allowed_actions"' in html
    assert 'typeof value==="object"?jsonField' in html


def test_observation_payload_keeps_coffeebench_keys_as_dynamic_data() -> None:
    raw = [
        {
            "event_id": "observation-1",
            "event_type": "observation_received",
            "correlation_id": "correlation-1",
            "observation": {
                "inventory": {"LOT-001": {"quantity": 10}},
                "cash": 100,
                "reported_revenue": 25,
            },
        },
        {
            "event_id": "decision-1",
            "event_type": "decision_made",
            "correlation_id": "correlation-1",
            "decision_id": "decision-1",
            "selected_action": "wait",
        },
        {
            "event_id": "action-1",
            "event_type": "action_executed",
            "correlation_id": "correlation-1",
            "decision_id": "decision-1",
            "action_id": "action-1",
            "action": "wait",
        },
    ]
    html = render_explorer(
        normalize_events(raw),
        ingestion=IngestionResult(raw, []),
    )

    assert _explorer_actions(html)[0]["observation"]["data"] == {
        "inventory": {"LOT-001": {"quantity": 10}},
        "cash": 100,
        "reported_revenue": 25,
    }


def test_empty_observation_renders_only_not_recorded_state() -> None:
    raw = [
        {
            "event_id": "observation-1",
            "event_type": "observation_received",
            "correlation_id": "correlation-1",
        },
        {
            "event_id": "action-1",
            "event_type": "action_executed",
            "correlation_id": "correlation-1",
            "action_id": "action-1",
            "action": "wait",
        },
    ]
    html = render_explorer(
        normalize_events(raw),
        ingestion=IngestionResult(raw, []),
    )
    observation = _explorer_actions(html)[0]["observation"]

    assert observation["data"] == {}
    assert observation["allowed_actions"] is None
    assert (
        "if(!fields.length)return section(\"Observation\","
        "'<div class=\"empty-state\">Not recorded</div>')"
    ) in html
