from __future__ import annotations

import json
from datetime import datetime, timezone

from circular_coffee.audit import AuditLogger, load_events, render_events
from circular_coffee.config import build_default_config, create_initial_market_state
from circular_coffee.logging_utils import to_jsonable
from circular_coffee.models import AgentAction, MarketState
from circular_coffee.policies import WaitPolicy
from circular_coffee.simulation import SimulationRunner


class _CreateOffer:
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
                reason_summary="Create a test offer.",
                expected_outcome={
                    "outcome_type": "proposal_accepted",
                    "counterparty": "retailer_a",
                    "proposal_status": "accepted",
                },
            )
        return AgentAction(action_type="wait")


class _InvalidateOfferVisibility:
    def __init__(self, state: MarketState) -> None:
        self.state = state

    def choose_action(self, observation: dict) -> AgentAction:
        if observation["day"] == 2:
            self.state.agents["roaster"].inventory["LOT-001"].quantity = 50
        return AgentAction(action_type="wait")


class _VisibilityGapRunner(SimulationRunner):
    def _ordered_agent_ids(self) -> list[str]:
        if self.state.day == 1:
            return ["retailer_a", "retailer_b", "roaster"]
        return ["roaster", "retailer_a", "retailer_b"]


def test_audit_logger_writes_required_jsonl_fields(tmp_path) -> None:
    logger = AuditLogger(
        tmp_path / "audit.jsonl",
        now=lambda: datetime(2026, 1, 2, 3, 4, tzinfo=timezone.utc),
        event_id_factory=lambda: "event-1",
    )
    logger.reset()
    logger.log_event(
        event_type="decision_made",
        run_id="run-1",
        day=2,
        agent_id="roaster",
        agent_role="roaster",
        correlation_id="correlation-1",
        payload={"unsafe": object()},
    )

    event = json.loads((tmp_path / "audit.jsonl").read_text(encoding="utf-8"))
    assert event["schema_version"] == "0.1"
    assert event["event_id"] == "event-1"
    assert event["timestamp"] == "2026-01-02T03:04:00+00:00"
    assert event["correlation_id"] == "correlation-1"
    assert isinstance(event["unsafe"], str)


def test_audit_io_failure_is_nonfatal(tmp_path, caplog) -> None:
    blocked_parent = tmp_path / "not-a-directory"
    blocked_parent.write_text("file", encoding="utf-8")
    logger = AuditLogger(blocked_parent / "audit.jsonl")

    result = logger.log_event(
        event_type="decision_made",
        run_id="run-1",
        day=1,
        agent_id="roaster",
        agent_role="roaster",
        correlation_id="correlation-1",
    )

    assert result is None
    assert "Failed to append audit event" in caplog.text


def test_runner_audit_uses_one_correlation_per_agent_turn(tmp_path) -> None:
    config = build_default_config(max_days=1)
    result = SimulationRunner(
        config,
        {
            "roaster": WaitPolicy(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="audit_correlation",
        output_root=tmp_path,
    ).run()
    events = load_events(result.output_dir / "audit_events.jsonl")
    roaster_events = [event for event in events if event["agent_id"] == "roaster"]

    assert [event["event_type"] for event in roaster_events] == [
        "observation_received",
        "decision_made",
        "action_executed",
        "outcome_observed",
    ]
    assert len({event["correlation_id"] for event in roaster_events}) == 1


def test_replay_supports_reason_and_explanation_without_showing_raw_output() -> None:
    base = {
        "event_type": "decision_made",
        "agent_id": "roaster",
        "correlation_id": "corr-1",
        "decision_id": "decision-1",
        "selected_action": "wait",
        "expected_outcome": None,
        "raw_model_output": "private debug payload",
    }
    old_output = render_events([{**base, "reason": "Old explanation."}])
    new_output = render_events(
        [{**base, "explanation": "New explanation."}]
    )
    debug_output = render_events(
        [{**base, "explanation": "New explanation."}],
        debug=True,
    )

    assert "Old explanation." in old_output
    assert "New explanation." in new_output
    assert "Expected outcome:\n  Not stated" in new_output
    assert "Actual outcome:\n  Pending" in new_output
    assert "private debug payload" not in new_output
    assert "private debug payload" in debug_output


def test_expected_outcome_is_json_serializable_or_null(tmp_path) -> None:
    logger = AuditLogger(tmp_path / "audit.jsonl")
    logger.reset()
    for index, expected_outcome in enumerate(
        [
            {
                "outcome_type": "proposal_accepted",
                "counterparty": "retailer_a",
                "proposal_status": "accepted",
            },
            None,
        ],
        start=1,
    ):
        logger.log_event(
            event_type="decision_made",
            run_id="run-1",
            day=1,
            agent_id="roaster",
            agent_role="roaster",
            correlation_id=f"corr-{index}",
            payload={"expected_outcome": expected_outcome},
        )

    events = load_events(tmp_path / "audit.jsonl")
    assert events[0]["expected_outcome"]["outcome_type"] == "proposal_accepted"
    assert events[1]["expected_outcome"] is None


def test_replay_warns_when_offer_is_never_visible_then_expires(tmp_path) -> None:
    config = build_default_config(max_days=10, seed=7)
    config.proposal_expiry_days = 1
    state = create_initial_market_state(config)
    runner = _VisibilityGapRunner(
        config,
        {
            "roaster": _CreateOffer(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="audit_visibility_gap",
        output_root=tmp_path,
        initial_state=state,
    )
    runner.policies["roaster"] = _InvalidateOfferVisibility(state)
    original_policy = _CreateOffer()

    class _CreateThenInvalidate:
        def choose_action(self, observation: dict) -> AgentAction:
            if observation["day"] == 1:
                return original_policy.choose_action(observation)
            return _InvalidateOfferVisibility(state).choose_action(observation)

    runner.policies["roaster"] = _CreateThenInvalidate()
    result = runner.run()
    events = load_events(result.output_dir / "audit_events.jsonl")
    event_counts = {
        event_type: sum(
            event["event_type"] == event_type for event in events
        )
        for event_type in {
            "observation_received",
            "decision_made",
            "action_executed",
            "outcome_observed",
        }
    }

    assert event_counts["observation_received"] == 30
    assert event_counts["decision_made"] == 30
    assert event_counts["action_executed"] == 30
    assert event_counts["outcome_observed"] == 31
    created = next(
        event
        for event in events
        if event["event_type"] == "action_executed"
        and event["action"] == "propose_trade"
    )
    retailer_observations = [
        event
        for event in events
        if event["event_type"] == "observation_received"
        and event["agent_id"] == "retailer_a"
    ]
    assert created["status"] == "success"
    assert created["proposal_id"] == "proposal-1"
    assert created["seller_id"] == "roaster"
    assert created["buyer_id"] == "retailer_a"
    decision = next(
        event
        for event in events
        if event["event_type"] == "decision_made"
        and event["agent_id"] == "roaster"
        and event["selected_action"] == "propose_trade"
    )
    assert decision["explanation"] == "Create a test offer."
    assert "reason" not in decision
    assert decision["expected_outcome"]["outcome_type"] == "proposal_accepted"
    assert created["decision_id"] == decision["decision_id"]
    assert created["action_id"]
    assert all(
        observation["observation"]["incoming_proposals"] == []
        for observation in retailer_observations
    )
    assert all(
        not {
            "accept_trade",
            "reject_trade",
            "counteroffer_trade",
        }.intersection(observation["allowed_actions"])
        for observation in retailer_observations
    )
    assert all(
        event["selected_action"] == "wait"
        for event in events
        if event["event_type"] == "decision_made"
        and event["agent_id"] == "retailer_a"
    )
    assert any(
        event["event_type"] == "outcome_observed"
        and event["proposal_id"] == "proposal-1"
        and event["outcome"] == "expired"
        and event["actual_outcome"]["outcome_type"] == "proposal_expired"
        and event["decision_id"] == decision["decision_id"]
        for event in events
    )
    replay = render_events(events)
    assert "Expected outcome:\n  proposal_accepted" in replay
    assert "Actual outcome:\n  proposal_expired" in replay
    assert "Expected and actual outcomes differ" in replay
    assert "was not present in retailer_a's subsequent observation" in replay
    assert "had no accept, reject, or counteroffer action available" in replay


def _result_signature(result) -> dict:
    metrics = to_jsonable(result.metrics)
    metrics.pop("run_id", None)
    return {
        "actions": [
            {
                "day": row["day"],
                "agent_id": row["agent_id"],
                "requested_action": to_jsonable(row["requested_action"]),
                "executed_action": row["executed_action"],
                "is_valid": row["is_valid"],
            }
            for row in result.action_logs
        ],
        "trades": to_jsonable(result.trade_logs),
        "agents": to_jsonable(result.state.agents),
        "metrics": metrics,
    }


def test_ten_day_results_are_identical_with_audit_enabled_or_disabled(
    tmp_path,
) -> None:
    enabled_config = build_default_config(max_days=10, seed=11)
    disabled_config = build_default_config(max_days=10, seed=11)
    policies = {
        "roaster": _CreateOffer(),
        "retailer_a": WaitPolicy(),
        "retailer_b": WaitPolicy(),
    }
    enabled = SimulationRunner(
        enabled_config,
        policies,
        run_id="audit_enabled",
        output_root=tmp_path,
        audit_enabled=True,
    ).run()
    disabled = SimulationRunner(
        disabled_config,
        {
            "roaster": _CreateOffer(),
            "retailer_a": WaitPolicy(),
            "retailer_b": WaitPolicy(),
        },
        run_id="audit_disabled",
        output_root=tmp_path,
        audit_enabled=False,
    ).run()

    assert _result_signature(enabled) == _result_signature(disabled)
    assert (enabled.output_dir / "audit_events.jsonl").is_file()
    assert not (disabled.output_dir / "audit_events.jsonl").exists()
