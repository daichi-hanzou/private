from __future__ import annotations

import pytest

from agentledger.bundles import build_action_bundles, outcome_status
from agentledger.html import render_explorer
from agentledger.ingestion import read_jsonl
from agentledger.normalizer import normalize_events
from calendar_agent.agent import CalendarAgent
from calendar_agent.audit import (
    AuditFileError,
    read_complete_jsonl,
    write_jsonl,
)
from calendar_agent.models import CalendarRequest


def _request(
    *,
    slots: list[str] | None = None,
    requires_approval: bool = True,
) -> CalendarRequest:
    return CalendarRequest(
        title="Dental appointment",
        requested_date="2026-08-12",
        preferred_period="afternoon",
        duration_minutes=60,
        available_slots=slots if slots is not None else ["15:00", "13:00"],
        requires_approval=requires_approval,
    )


def _pending_events() -> list[dict]:
    return CalendarAgent().propose(_request())


def _resolve(
    events: list[dict],
    resolution: str,
) -> list[dict]:
    return CalendarAgent().resolve(
        events,
        action_id=events[2]["action_id"],
        resolution=resolution,
        actor="daichi",
        reason=resolution.title(),
    )


def test_approval_required_propose_is_pending() -> None:
    raw = _pending_events()
    events = normalize_events(raw)

    assert len(raw) == 3
    assert [event["event_type"] for event in raw] == [
        "observation_received",
        "decision_made",
        "action_executed",
    ]
    assert raw[2]["action_parameters"]["start"] == (
        "2026-08-12T13:00"
    )
    assert raw[2]["status"] == "awaiting_approval"
    assert len({event["event_id"] for event in raw}) == 3
    assert len({event["run_id"] for event in raw}) == 1
    assert len({event["correlation_id"] for event in raw}) == 1

    bundle = build_action_bundles(events)[0]
    assert bundle.observation is not None
    assert bundle.decision is not None
    assert bundle.outcome is None
    assert bundle.human_interventions == ()
    assert outcome_status(bundle) == "pending"


def test_approval_not_required_propose_is_confirmed() -> None:
    raw = CalendarAgent().propose(_request(requires_approval=False))
    bundle = build_action_bundles(normalize_events(raw))[0]

    assert len(raw) == 4
    assert not any(
        event["event_type"] == "human_intervention" for event in raw
    )
    assert raw[-1]["event_type"] == "outcome_observed"
    assert raw[-1]["status"] == "confirmed"
    assert outcome_status(bundle) == "confirmed"


def test_approve_appends_intervention_and_confirmed_outcome() -> None:
    pending = _pending_events()
    resolved = _resolve(pending, "approve")
    bundle = build_action_bundles(normalize_events(resolved))[0]

    assert len(resolved) == 5
    intervention, outcome = resolved[-2:]
    assert intervention["intervention_type"] == "accept"
    assert intervention["actor"] == "daichi"
    assert intervention["before"] == {"status": "awaiting_approval"}
    assert intervention["after"] == {"status": "approved"}
    assert outcome["status"] == "confirmed"
    assert outcome["actual_outcome"]["outcome_type"] == (
        "calendar_event_created"
    )
    assert outcome_status(bundle) == "confirmed"
    assert len(bundle.human_interventions) == 1
    assert bundle.outcome is not None
    assert bundle.outcome.action_id == bundle.action.action_id

    action = pending[2]
    assert {
        intervention["run_id"],
        outcome["run_id"],
    } == {action["run_id"]}
    assert {
        intervention["correlation_id"],
        outcome["correlation_id"],
    } == {action["correlation_id"]}
    assert {
        intervention["decision_id"],
        outcome["decision_id"],
    } == {action["decision_id"]}
    assert {
        intervention["action_id"],
        intervention["related_action_id"],
        outcome["action_id"],
    } == {action["action_id"]}
    assert len({event["event_id"] for event in resolved}) == 5


def test_reject_appends_intervention_and_contradicted_outcome() -> None:
    resolved = _resolve(_pending_events(), "reject")
    bundle = build_action_bundles(normalize_events(resolved))[0]

    assert len(resolved) == 5
    intervention, outcome = resolved[-2:]
    assert intervention["intervention_type"] == "reject"
    assert intervention["after"] == {"status": "rejected"}
    assert outcome["status"] == "contradicted"
    assert outcome["actual_outcome"] == {
        "outcome_type": "calendar_event_rejected"
    }
    assert outcome_status(bundle) == "contradicted"


@pytest.mark.parametrize("first_resolution", ["approve", "reject"])
def test_resolved_action_cannot_be_approved_again(
    first_resolution: str,
) -> None:
    resolved = _resolve(_pending_events(), first_resolution)

    with pytest.raises(ValueError, match="already has an outcome"):
        _resolve(resolved, "approve")


def test_unknown_action_id_fails() -> None:
    with pytest.raises(ValueError, match="action not found"):
        CalendarAgent().resolve(
            _pending_events(),
            action_id="missing-action",
            resolution="approve",
            actor="daichi",
            reason="Approved",
        )


def test_no_available_slots_requests_clarification() -> None:
    raw = CalendarAgent().propose(_request(slots=[]))
    bundle = build_action_bundles(normalize_events(raw))[0]

    assert len(raw) == 4
    assert raw[1]["selected_action"] == "request_clarification"
    assert raw[2]["action"] == "request_clarification"
    assert raw[-1]["actual_outcome"] == {
        "outcome_type": "clarification_requested"
    }
    assert outcome_status(bundle) == "confirmed"


def test_generated_jsonl_loads_and_renders_in_explorer(tmp_path) -> None:
    pending_path = write_jsonl(
        tmp_path / "pending.jsonl",
        _pending_events(),
    )
    pending_ingestion = read_jsonl(pending_path)
    pending_events = normalize_events(pending_ingestion.events)
    pending_html = render_explorer(
        pending_events,
        ingestion=pending_ingestion,
        source_path=pending_path,
    )

    assert pending_ingestion.loaded == 3
    assert pending_ingestion.issues == []
    assert '"status": "pending"' in pending_html

    approved = _resolve(read_complete_jsonl(pending_path), "approve")
    approved_path = write_jsonl(tmp_path / "approved.jsonl", approved)
    approved_ingestion = read_jsonl(approved_path)
    approved_html = render_explorer(
        normalize_events(approved_ingestion.events),
        ingestion=approved_ingestion,
        source_path=approved_path,
    )
    assert approved_ingestion.loaded == 5
    assert "calendar_event_created" in approved_html
    assert "Human Intervention" in approved_html
    assert '"status": "confirmed"' in approved_html


def test_resolution_refuses_malformed_jsonl(tmp_path) -> None:
    path = write_jsonl(tmp_path / "malformed.jsonl", _pending_events())
    with path.open("a", encoding="utf-8") as handle:
        handle.write("{not-json}\n")

    with pytest.raises(
        AuditFileError,
        match="malformed lines: line 4",
    ):
        read_complete_jsonl(path)
