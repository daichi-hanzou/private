from __future__ import annotations

import json

from agentledger.cases import decision_case, related_case
from agentledger.bundles import (
    build_action_bundles,
    observed_at,
    outcome_status,
)
from agentledger.display import assign_display_ids
from agentledger.html import render_explorer
from agentledger.ingestion import read_jsonl
from agentledger.models import IngestionResult
from agentledger.normalizer import normalize_events
from agentledger.query import AuditQuery, filter_events
from agentledger.sequence import build_action_sequence, build_sequence


def _raw_events() -> list[dict]:
    return [
        {
            "event_id": "event-1",
            "event_type": "observation_received",
            "run_id": "seed_0",
            "day": 1,
            "timestamp": "2026-01-01T00:00:00+00:00",
            "agent_id": "roaster",
            "correlation_id": "corr-1",
            "observation": {
                "cash": 3000,
                "inventory": {"LOT-001": {}},
                "incoming_proposals": [],
            },
        },
        {
            "event_id": "event-2",
            "event_type": "decision_made",
            "run_id": "seed_0",
            "day": 1,
            "timestamp": "2026-01-01T00:00:01+00:00",
            "agent_id": "roaster",
            "correlation_id": "corr-1",
            "decision_id": "decision-1",
            "selected_action": "propose_trade",
            "explanation": "Preserve margin while advancing revenue.",
        },
        {
            "event_id": "event-3",
            "event_type": "action_executed",
            "run_id": "seed_0",
            "day": 1,
            "timestamp": "2026-01-01T00:00:02+00:00",
            "agent_id": "roaster",
            "counterparty": "retailer_a",
            "correlation_id": "corr-1",
            "decision_id": "decision-1",
            "action_id": "action-1",
            "action": "propose_trade",
            "proposal_id": "proposal-1",
            "lot_id": "LOT-001",
            "quantity": 100,
            "unit_price": 9.4,
            "status": "success",
        },
        {
            "event_id": "event-4",
            "event_type": "observation_received",
            "run_id": "seed_0",
            "day": 1,
            "timestamp": "2026-01-01T00:00:03+00:00",
            "agent_id": "retailer_a",
            "correlation_id": "corr-2",
            "observation": {
                "incoming_proposals": [{"proposal_id": "proposal-1"}]
            },
        },
        {
            "event_id": "event-5",
            "event_type": "decision_made",
            "run_id": "seed_0",
            "day": 1,
            "timestamp": "2026-01-01T00:00:04+00:00",
            "agent_id": "retailer_a",
            "correlation_id": "corr-2",
            "decision_id": "decision-2",
            "selected_action": "counteroffer_trade",
            "proposal_id": "proposal-1",
            "explanation": "A lower price improves the expected margin.",
        },
        {
            "event_id": "event-6",
            "event_type": "action_executed",
            "run_id": "seed_0",
            "day": 1,
            "timestamp": "2026-01-01T00:00:05+00:00",
            "agent_id": "retailer_a",
            "counterparty": "roaster",
            "correlation_id": "corr-2",
            "decision_id": "decision-2",
            "action_id": "action-2",
            "action": "counteroffer_trade",
            "proposal_id": "proposal-1",
            "unit_price": 9.0,
            "status": "success",
        },
        {
            "event_id": "event-7",
            "event_type": "outcome_observed",
            "run_id": "seed_0",
            "day": 1,
            "timestamp": "2026-01-01T00:00:06+00:00",
            "agent_id": "retailer_a",
            "counterparty": "roaster",
            "correlation_id": "corr-2",
            "decision_id": "decision-2",
            "proposal_id": "proposal-1",
            "outcome": "countered",
        },
        {
            "event_id": "event-8",
            "event_type": "decision_made",
            "run_id": "seed_0",
            "day": 1,
            "timestamp": "2026-01-01T00:00:07+00:00",
            "agent_id": "retailer_b",
            "correlation_id": "unrelated",
            "decision_id": "decision-x",
            "selected_action": "wait",
        },
    ]


def test_jsonl_reader_skips_empty_and_malformed_lines(tmp_path) -> None:
    path = tmp_path / "audit.jsonl"
    path.write_text(
        json.dumps(_raw_events()[0]) + "\n\n{bad json\n"
        + json.dumps({"event_id": "minimal"})
        + "\n",
        encoding="utf-8",
    )
    result = read_jsonl(path)

    assert result.loaded == 2
    assert result.skipped == 1
    assert result.empty_lines == 1
    assert "_agentledger_source_line" not in result.events[0]


def test_normalization_preserves_raw_and_handles_missing_fields() -> None:
    raw = {"event_id": "minimal", "explanation": "Original text"}
    event = normalize_events([raw])[0]

    assert event.event_type == "unknown"
    assert event.raw_event == raw
    assert event.summary


def test_duplicate_event_ids_receive_unique_normalized_ids() -> None:
    raw = {"event_id": "duplicate", "event_type": "custom"}
    events = normalize_events([raw, raw])

    assert [event.event_id for event in events] == [
        "duplicate",
        "duplicate#2",
    ]
    assert events[1].raw_event["event_id"] == "duplicate"


def test_normalizes_decision_action_and_proposal_case() -> None:
    events = normalize_events(_raw_events())
    decision = events[1]
    action = events[2]

    assert decision.explanation == "Preserve margin while advancing revenue."
    assert action.action_type == "propose_trade"
    assert action.case_type == "trade_proposal"
    assert action.case_id == "proposal-1"
    assert action.action_parameters["unit_price"] == 9.4


def test_display_ids_are_stable_and_sequential() -> None:
    events = normalize_events(_raw_events())
    ids = assign_display_ids(events)

    assert ids.events["event-1"] == "E1"
    assert ids.decisions["decision-1"] == "D1"
    assert ids.decisions["decision-2"] == "D2"
    assert ids.cases["proposal-1"] == "P1"
    assert ids.actions["action-1"] == "A1"


def test_filters_and_case_insensitive_keyword_search() -> None:
    events = normalize_events(_raw_events())

    assert len(filter_events(events, AuditQuery(agent="roaster"))) == 3
    assert len(
        filter_events(events, AuditQuery(event_type="action_executed"))
    ) == 2
    assert len(
        filter_events(events, AuditQuery(proposal_id="proposal-1"))
    ) == 5
    searched = filter_events(events, AuditQuery(keyword="EXPECTED MARGIN"))
    assert [event.event_id for event in searched] == ["event-5"]


def test_proposal_case_includes_related_turns_not_unrelated_agent() -> None:
    events = normalize_events(_raw_events())
    case = related_case(events, "proposal-1")
    event_ids = {event.event_id for event in case}

    assert {"event-1", "event-2", "event-3"}.issubset(event_ids)
    assert {"event-4", "event-5", "event-6", "event-7"}.issubset(event_ids)
    assert "event-8" not in event_ids


def test_decision_case_links_only_same_decision_turn() -> None:
    events = normalize_events(_raw_events())
    case = decision_case(events, "decision-2")

    assert {event.event_id for event in case} == {
        "event-4",
        "event-5",
        "event-6",
        "event-7",
    }


def test_business_and_technical_sequences_use_selected_participants() -> None:
    case = related_case(normalize_events(_raw_events()), "proposal-1")
    ids = assign_display_ids(case)
    business = build_sequence(case, view="business", display_ids=ids)
    technical = build_sequence(case, view="technical", display_ids=ids)

    assert "participant env as Environment" not in business
    assert "roaster->>retailer_a" in business
    assert "retailer_a->>roaster" in business
    assert "Decision D1" in business
    assert "P1 registered" in business
    assert "retailer_b" not in business
    assert "participant env as Environment" in technical
    assert "roaster->>env" in technical
    assert "env->>retailer_a" in technical


def test_action_bundles_join_the_four_event_types() -> None:
    events = normalize_events(_raw_events())
    bundles = build_action_bundles(events)

    assert len(bundles) == 2
    first = bundles[0]
    assert first.observation is not None
    assert first.observation.event_id == "event-1"
    assert first.decision is not None
    assert first.decision.event_id == "event-2"
    assert first.action.event_id == "event-3"
    assert first.outcome is None
    second = bundles[1]
    assert second.observation is not None
    assert second.observation.event_id == "event-4"
    assert second.decision is not None
    assert second.decision.event_id == "event-5"
    assert second.outcome is not None
    assert second.outcome.event_id == "event-7"


def test_action_sequence_contains_only_the_selected_bundle() -> None:
    bundle = build_action_bundles(normalize_events(_raw_events()))[0]
    content = build_action_sequence(bundle, action_display_id="A1")

    assert "Observation" in content
    assert "Decision - Propose Trade" in content
    assert "[A1] Propose Trade" in content
    assert "participant env" not in content
    assert "Environment" not in content
    assert "roaster->>retailer_a" in content
    assert "Accept Trade" not in content
    assert "Counteroffer Trade" not in content


def test_explorer_contains_action_list_detail_and_sequence() -> None:
    raw = _raw_events()
    events = normalize_events(raw)
    content = render_explorer(
        events,
        ingestion=IngestionResult(raw, []),
        source_path="audit.jsonl",
    )

    assert "Action List" in content
    assert "Action Detail" in content
    assert "<h3>Observation</h3>" not in content
    assert 'section("Observation"' in content
    assert 'section("Decision"' in content
    assert 'section("Action"' in content
    assert 'section("Outcome"' in content
    assert "Sequence" in content
    assert "View raw action JSON" in content
    assert "Preserve margin while advancing revenue." in content
    assert "sequenceDiagram" in content
    assert "proposal-1" in content
    assert '"relation_ids"' in content
    assert '"correlation_id"' in content


def test_action_list_has_requested_columns_and_filters() -> None:
    raw = _raw_events()
    content = render_explorer(
        normalize_events(raw),
        ingestion=IngestionResult(raw, []),
    )

    assert 'class="filters"' not in content
    assert 'class="action-toolbar"' in content
    assert 'class="column-labels"' in content
    assert 'class="column-filters"' in content
    assert 'data-column-filter="day"' in content
    assert 'data-column-filter="action_id"' in content
    assert 'data-column-filter="agent"' in content
    assert 'data-column-filter="action_type"' in content
    assert 'data-column-filter="target"' in content
    assert 'data-column-filter="summary"' in content
    assert 'id="clear-filters"' in content
    assert 'id="search"' in content
    assert "<th>Action ID</th>" in content
    assert "<th>Agent</th>" in content
    assert "<th>Action</th>" in content
    assert "Target</th>" in content
    assert "Summary</th>" in content


def test_html_uses_action_filters_and_removes_event_ui() -> None:
    raw = _raw_events()
    content = render_explorer(
        normalize_events(raw),
        ingestion=IngestionResult(raw, []),
    )

    assert "Object.entries(state.columnFilters).every" in content
    assert "action.search_text.includes(query)" in content
    assert "active-filter" in content
    assert "visible.length} / ${data.actions.length}" in content
    assert "No action matches the current filters." in content
    assert "No sequence to display." in content
    assert 'control.value=""' in content
    assert "Event List" not in content
    assert "Event Detail" not in content
    assert "Related Case / Decision" not in content
    assert "Audit ID mapping" not in content
    assert "Ingestion details" not in content
    assert 'data-view="business"' not in content
    assert 'data-view="technical"' not in content
    assert "Technical" not in content
    assert 'data-column-filter="related"' not in content
    assert 'data-column-filter="status"' not in content


def test_human_intervention_is_attached_and_rendered_separately() -> None:
    raw = _raw_events()
    raw.append(
        {
            "event_id": "human-1",
            "event_type": "human_intervention",
            "timestamp": "2026-01-01T00:00:03+00:00",
            "intervention_type": "modify",
            "related_action_id": "action-1",
            "performed_at": "2026-01-01T00:00:02.500000+00:00",
            "actor": "human",
            "before": {"unit_price": 9.4},
            "after": {"unit_price": 9.2},
            "reason": "Operator reduced the price.",
            "input_method": "ui",
        }
    )
    events = normalize_events(raw)
    bundle = build_action_bundles(events)[0]

    assert len(bundle.human_interventions) == 1
    assert bundle.human_interventions[0].intervention_type == "modify"
    sequence = build_action_sequence(bundle, action_display_id="A1")
    assert "Human intervention - Modify" in sequence
    content = render_explorer(
        events,
        ingestion=IngestionResult(raw, []),
    )
    assert 'section("Human Intervention"' in content
    assert "Operator reduced the price." in content
    assert '"related_action_id": "action-1"' in content


def test_explorer_hides_empty_human_intervention_section() -> None:
    content = render_explorer(
        normalize_events(_raw_events()),
        ingestion=IngestionResult(_raw_events(), []),
    )

    assert 'if(!values?.length)return ""' in content
    assert '"human_interventions": []' in content


def test_latest_asynchronous_outcome_is_used() -> None:
    raw = _raw_events()
    raw.extend(
        [
            {
                "event_id": "outcome-pending",
                "event_type": "outcome_observed",
                "timestamp": "2026-01-01T00:00:10+00:00",
                "observed_at": "2026-01-02T10:00:00+00:00",
                "action_id": "action-1",
                "status": "pending",
            },
            {
                "event_id": "outcome-confirmed",
                "event_type": "outcome_observed",
                "timestamp": "2026-01-01T00:00:09+00:00",
                "observed_at": "2026-01-03T14:00:00+00:00",
                "action_id": "action-1",
                "status": "confirmed",
            },
        ]
    )
    bundle = build_action_bundles(normalize_events(raw))[0]

    assert bundle.outcome is not None
    assert bundle.outcome.event_id == "outcome-confirmed"
    assert outcome_status(bundle) == "confirmed"
    assert observed_at(bundle) == "2026-01-03T14:00:00+00:00"


def test_pending_and_unknown_outcome_statuses() -> None:
    no_outcome = build_action_bundles(normalize_events(_raw_events()))[0]
    assert outcome_status(no_outcome) == "pending"

    raw = _raw_events()
    raw.append(
        {
            "event_id": "outcome-unknown",
            "event_type": "outcome_observed",
            "timestamp": "2026-01-02T00:00:00+00:00",
            "action_id": "action-1",
            "status": "vendor_specific_status",
        }
    )
    unknown = build_action_bundles(normalize_events(raw))[0]
    assert outcome_status(unknown) == "unknown"


def test_legacy_outcome_without_status_defaults_to_confirmed() -> None:
    bundle = build_action_bundles(normalize_events(_raw_events()))[1]

    assert bundle.outcome is not None
    assert "status" not in bundle.outcome.raw_event
    assert outcome_status(bundle) == "confirmed"


def test_execution_context_is_rendered_and_empty_context_is_collapsed() -> None:
    raw = _raw_events()
    raw[2]["execution_context"] = {
        "model_name": "gpt-test",
        "model_version": "2026-07",
        "prompt_hash": "prompt-abc",
        "tool_version": "tool-2",
        "config_hash": "config-def",
        "git_commit": "deadbeef",
        "environment": {"region": "local"},
    }
    events = normalize_events(raw)
    bundle = build_action_bundles(events)[0]

    assert bundle.execution_context.model_name == "gpt-test"
    assert not bundle.execution_context.is_empty
    content = render_explorer(
        events,
        ingestion=IngestionResult(raw, []),
    )
    assert "Execution Context" in content
    assert "gpt-test" in content
    assert 'class="bundle-section execution-context"' in content
    assert '"has_execution_context": true' in content

    legacy = build_action_bundles(normalize_events(_raw_events()))[0]
    assert legacy.execution_context.is_empty
