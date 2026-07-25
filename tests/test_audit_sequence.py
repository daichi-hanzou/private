from __future__ import annotations

from circular_coffee.audit import (
    build_sequence_diagram,
    filter_events,
    load_events,
    mermaid_text,
    participant_id,
    write_sequence_output,
)
from circular_coffee.config import build_default_config, create_initial_market_state
from circular_coffee.models import AgentAction, MarketState
from circular_coffee.policies import WaitPolicy
from circular_coffee.simulation import SimulationRunner


def _events() -> list[dict]:
    return [
        {
            "event_type": "observation_received",
            "run_id": "run-1",
            "day": 1,
            "timestamp": "2026-01-01T00:00:00+00:00",
            "agent_id": "roaster",
            "correlation_id": "corr-1",
            "observation": {
                "inventory": {"LOT-001": {"quantity": 100}},
                "incoming_proposals": [],
            },
            "allowed_actions": ["wait", "propose_trade"],
        },
        {
            "event_type": "decision_made",
            "run_id": "run-1",
            "day": 1,
            "timestamp": "2026-01-01T00:00:01+00:00",
            "agent_id": "roaster",
            "correlation_id": "corr-1",
            "decision_id": "decision-1",
            "selected_action": "propose_trade",
            "reason": "Offer <script>alert: 1;</script>\nfor revenue.",
            "expected_outcome": {"outcome_type": "proposal_accepted"},
            "raw_model_output": "RAW-MODEL-SECRET",
        },
        {
            "event_type": "action_executed",
            "run_id": "run-1",
            "day": 1,
            "timestamp": "2026-01-01T00:00:02+00:00",
            "agent_id": "roaster",
            "correlation_id": "corr-1",
            "decision_id": "decision-1",
            "action_id": "action-1",
            "action": "propose_trade",
            "status": "success",
            "counterparty": "retailer_a",
            "lot_id": "LOT-001",
            "quantity": 100,
            "unit_price": 10.0,
            "proposal_id": "proposal-1",
        },
        {
            "event_type": "outcome_observed",
            "run_id": "run-1",
            "day": 1,
            "timestamp": "2026-01-01T00:00:03+00:00",
            "agent_id": "roaster",
            "decision_id": "decision-1",
            "proposal_id": "proposal-1",
            "actual_outcome": {"outcome_type": "proposal_pending"},
        },
        {
            "event_type": "observation_received",
            "run_id": "run-1",
            "day": 2,
            "timestamp": "2026-01-02T00:00:00+00:00",
            "agent_id": "retailer_a",
            "correlation_id": "corr-2",
            "observation": {
                "inventory": {},
                "incoming_proposals": [],
            },
            "allowed_actions": ["wait"],
        },
        {
            "event_type": "decision_made",
            "run_id": "run-1",
            "day": 2,
            "timestamp": "2026-01-02T00:00:01+00:00",
            "agent_id": "retailer_a",
            "correlation_id": "corr-2",
            "decision_id": "decision-2",
            "selected_action": "wait",
            "explanation": "No offer is visible.",
            "expected_outcome": None,
        },
        {
            "event_type": "action_executed",
            "run_id": "run-1",
            "day": 2,
            "timestamp": "2026-01-02T00:00:02+00:00",
            "agent_id": "retailer_a",
            "correlation_id": "corr-2",
            "decision_id": "decision-2",
            "action_id": "action-2",
            "action": "wait",
            "status": "success",
        },
        {
            "event_type": "outcome_observed",
            "run_id": "run-1",
            "day": 3,
            "timestamp": "2026-01-03T00:00:00+00:00",
            "agent_id": "roaster",
            "decision_id": "decision-1",
            "action_id": "action-1",
            "proposal_id": "proposal-1",
            "actual_outcome": {"outcome_type": "proposal_expired"},
        },
        {
            "event_type": "decision_made",
            "run_id": "run-1",
            "day": 3,
            "timestamp": "2026-01-03T00:00:01+00:00",
            "agent_id": "retailer_b",
            "correlation_id": "unrelated",
            "decision_id": "unrelated",
            "selected_action": "wait",
            "explanation": "Unrelated event.",
        },
    ]


def test_participant_id_is_mermaid_safe() -> None:
    assert participant_id("Retailer A / <script>") == "retailer_a_script"
    assert participant_id("123 Buyer") == "agent_123_buyer"


def test_mermaid_text_escapes_and_truncates() -> None:
    escaped = mermaid_text("<b>unsafe:</b>;\nnext", limit=18)
    assert "<b>" not in escaped
    assert "&lt;b&gt;" in escaped
    assert ":" not in escaped
    assert "&lt;/b&gt;," in escaped
    assert "\n" not in escaped
    assert len(escaped) < 50


def test_sequence_uses_old_reason_and_expected_actual_warning() -> None:
    diagram = build_sequence_diagram(_events())

    assert "sequenceDiagram" in diagram
    assert "participant roaster as Roaster" in diagram
    assert "participant env as Environment" not in diagram
    assert "Offer &lt;script&gt;alert - 1," in diagram
    assert "Expected - proposal_accepted" in diagram
    assert "WARNING - Expected proposal_accepted / Actual proposal_expired" in diagram
    assert "Expected - Not recorded" in diagram
    assert "RAW-MODEL-SECRET" not in diagram


def test_sequence_marks_missing_proposal_observation() -> None:
    diagram = build_sequence_diagram(_events())

    assert "roaster->>retailer_a" in diagram
    assert "Proposal P1 was not visible to Retailer A" in diagram


def test_business_view_links_decision_action_and_preserves_id_mapping(
    tmp_path,
) -> None:
    events = _events()
    diagram = build_sequence_diagram(events)
    output = write_sequence_output(
        tmp_path / "business.html",
        events=events,
        mermaid_source=diagram,
        filters={"proposal_id": "proposal-1"},
    )
    content = output.read_text(encoding="utf-8")

    assert "participant env as Environment" not in diagram
    assert "Decision D1" in diagram
    assert "[D1] Proposal P1" in diagram
    assert "roaster->>retailer_a" in diagram
    assert "P1 registered" in diagram
    assert "proposal-1" not in diagram
    assert "Audit ID mapping" in content
    assert "D1 = decision_id: decision-1" in content
    assert "A1 = action_id: action-1" in content
    assert "P1 = proposal_id: proposal-1" in content


def test_business_view_counteroffer_targets_original_proposer() -> None:
    events = _events()[:4] + [
        {
            "event_type": "decision_made",
            "run_id": "run-1",
            "day": 1,
            "timestamp": "2026-01-01T00:00:04+00:00",
            "agent_id": "retailer_a",
            "correlation_id": "corr-2",
            "decision_id": "decision-2",
            "selected_action": "counteroffer_trade",
            "reason": "A lower purchase price preserves margin.",
        },
        {
            "event_type": "action_executed",
            "run_id": "run-1",
            "day": 1,
            "timestamp": "2026-01-01T00:00:05+00:00",
            "agent_id": "retailer_a",
            "correlation_id": "corr-2",
            "decision_id": "decision-2",
            "action_id": "action-2",
            "action": "counteroffer_trade",
            "counterparty": "roaster",
            "proposal_id": "proposal-1",
            "unit_price": 9.0,
            "status": "success",
        },
        {
            "event_type": "outcome_observed",
            "run_id": "run-1",
            "day": 1,
            "timestamp": "2026-01-01T00:00:06+00:00",
            "agent_id": "retailer_a",
            "decision_id": "decision-2",
            "proposal_id": "proposal-1",
            "actual_outcome": {"outcome_type": "proposal_countered"},
        },
    ]
    diagram = build_sequence_diagram(events)

    assert "retailer_a->>roaster" in diagram
    assert "[D2] Counteroffer for P1" in diagram
    assert "Price 9.00" in diagram
    assert "System changed P1 status to Countered" in diagram


def test_unlinked_action_has_no_decision_reference() -> None:
    events = [
        {
            "event_type": "action_executed",
            "day": 1,
            "agent_id": "roaster",
            "action_id": "orphan-action",
            "action": "propose_trade",
            "counterparty": "retailer_a",
            "proposal_id": "proposal-1",
            "status": "success",
        }
    ]
    diagram = build_sequence_diagram(events)

    assert "[A1] Proposal P1" in diagram
    assert "[D1]" not in diagram
    assert "WARNING - Action could not be linked to a decision" in diagram


def test_technical_view_keeps_environment_flow() -> None:
    diagram = build_sequence_diagram(_events(), view="technical")

    assert "participant env as Environment" in diagram
    assert "roaster->>env: propose_trade" in diagram
    assert "env-->>retailer_a" in diagram


def test_proposal_and_day_filters_select_related_events() -> None:
    proposal_events = filter_events(_events(), proposal_id="proposal-1")
    day_events = filter_events(_events(), day=2)
    proposal_diagram = build_sequence_diagram(proposal_events)

    assert any(
        event.get("decision_id") == "decision-1"
        for event in proposal_events
    )
    assert any(
        event.get("decision_id") == "decision-2"
        for event in proposal_events
    )
    assert all(
        event.get("decision_id") != "unrelated"
        for event in proposal_events
    )
    assert day_events
    assert all(event["day"] == 2 for event in day_events)
    assert "participant roaster as Roaster" in proposal_diagram
    assert "participant retailer_a as Retailer A" in proposal_diagram
    assert "participant retailer_b as Retailer B" not in proposal_diagram


class _CreateThenInvalidate:
    def __init__(self, state: MarketState) -> None:
        self.state = state

    def choose_action(self, observation: dict) -> AgentAction:
        if observation["day"] == 1:
            lot = observation["self"]["inventory"]["LOT-001"]
            return AgentAction(
                action_type="propose_trade",
                seller_id="roaster",
                buyer_id="retailer_a",
                lot_id="LOT-001",
                quantity=lot["quantity"],
                unit_price=10.0,
                reason_summary="Offer inventory to Retailer A.",
                expected_outcome={
                    "outcome_type": "proposal_accepted",
                    "counterparty": "retailer_a",
                    "proposal_status": "accepted",
                },
            )
        if observation["day"] == 2:
            self.state.agents["roaster"].inventory["LOT-001"].quantity = 50
        return AgentAction(action_type="wait")


class _TenDayRunner(SimulationRunner):
    def _ordered_agent_ids(self) -> list[str]:
        if self.state.day == 1:
            return ["retailer_a", "retailer_b", "roaster"]
        return ["roaster", "retailer_a", "retailer_b"]


def test_ten_day_audit_generates_nonempty_sequence_html(tmp_path) -> None:
    config = build_default_config(max_days=10, seed=17)
    config.proposal_expiry_days = 1
    state = create_initial_market_state(config)
    result = _TenDayRunner(
        config,
        {
            "roaster": _CreateThenInvalidate(state),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="sequence_integration",
        output_root=tmp_path,
        initial_state=state,
    ).run()
    events = load_events(result.output_dir / "audit_events.jsonl")
    diagram = build_sequence_diagram(events)
    output = write_sequence_output(
        result.output_dir / "audit_sequence.html",
        events=events,
        mermaid_source=diagram,
        filters={},
    )
    content = output.read_text(encoding="utf-8")

    assert output.stat().st_size > 1000
    assert "sequenceDiagram" in content
    assert "participant roaster as Roaster" in content
    assert "participant env as Environment" not in content
    assert "participant retailer_a as Retailer A" in content
    assert "Propose trade" in content
    assert "Proposal P1" in content
    assert "Offer inventory to Retailer A." in content
    assert "Expected - proposal_accepted" in content
    assert "status to Expired" in content
    assert "Proposal P1 was not visible to Retailer A" in content
