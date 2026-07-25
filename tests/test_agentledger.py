from __future__ import annotations

import json

from agentledger.cases import decision_case, related_case
from agentledger.display import assign_display_ids
from agentledger.html import render_explorer
from agentledger.ingestion import read_jsonl
from agentledger.models import IngestionResult
from agentledger.normalizer import normalize_events
from agentledger.query import AuditQuery, filter_events
from agentledger.sequence import build_sequence


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


def test_explorer_contains_event_list_detail_case_sequence_and_raw_json() -> None:
    raw = _raw_events()
    events = normalize_events(raw)
    content = render_explorer(
        events,
        ingestion=IngestionResult(raw, []),
        source_path="audit.jsonl",
    )

    assert "Event List" in content
    assert "Event Detail" in content
    assert "Related Case / Decision" in content
    assert "Partial Sequence View" in content
    assert "View raw event JSON" in content
    assert "Audit ID mapping" in content
    assert "Preserve margin while advancing revenue." in content
    assert "sequenceDiagram" in content
    assert "proposal-1" in content


def test_html_uses_column_filters_and_keeps_view_switch() -> None:
    raw = _raw_events()
    content = render_explorer(
        normalize_events(raw),
        ingestion=IngestionResult(raw, []),
    )

    assert 'class="filters"' not in content
    assert 'class="event-toolbar"' in content
    assert 'class="column-labels"' in content
    assert 'class="column-filters"' in content
    assert 'data-column-filter="day"' in content
    assert 'data-column-filter="event"' in content
    assert 'data-column-filter="agent"' in content
    assert 'data-column-filter="event_type"' in content
    assert 'data-column-filter="summary"' in content
    assert 'data-column-filter="related"' in content
    assert 'data-column-filter="status"' in content
    assert 'id="clear-filters"' in content
    assert 'data-view="business"' in content
    assert 'data-view="technical"' in content
    assert 'id="search"' in content


def test_html_column_filter_logic_handles_and_clear_and_empty_results() -> None:
    raw = _raw_events()
    content = render_explorer(
        normalize_events(raw),
        ingestion=IngestionResult(raw, []),
    )

    assert "Object.entries(state.columnFilters).every" in content
    assert "event.action_type?event.action_label:null" in content
    assert "active-filter" in content
    assert "visible.length} / ${data.events.length}" in content
    assert "No event matches the current filters." in content
    assert "No related events to display." in content
    assert "No sequence to display." in content
    assert 'control.value=""' in content
