from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any


def load_events(path: str | Path) -> list[dict[str, Any]]:
    events: list[dict[str, Any]] = []
    with Path(path).open(encoding="utf-8") as handle:
        for line_number, line in enumerate(handle, start=1):
            if not line.strip():
                continue
            try:
                events.append(json.loads(line))
            except json.JSONDecodeError as exc:
                raise ValueError(
                    f"Invalid JSON on line {line_number}: {exc}"
                ) from exc
    return events


def normalize_outcome_type(value: Any) -> str | None:
    if isinstance(value, dict):
        value = value.get("outcome_type") or value.get("proposal_status")
    if not isinstance(value, str) or not value.strip():
        return None
    normalized = re.sub(r"[^a-z0-9]+", "_", value.lower()).strip("_")
    aliases = {
        "accept": "proposal_accepted",
        "accepted": "proposal_accepted",
        "proposal_accept": "proposal_accepted",
        "expire": "proposal_expired",
        "expired": "proposal_expired",
        "proposal_expire": "proposal_expired",
        "reject": "proposal_rejected",
        "rejected": "proposal_rejected",
        "proposal_reject": "proposal_rejected",
        "counter": "proposal_countered",
        "countered": "proposal_countered",
        "pending": "proposal_pending",
    }
    return aliases.get(normalized, normalized)


def _actual_outcome_type(event: dict[str, Any]) -> str | None:
    return normalize_outcome_type(
        event.get("actual_outcome", event.get("outcome"))
    )


def find_delivery_warnings(events: list[dict[str, Any]]) -> list[str]:
    created: dict[str, tuple[int, dict[str, Any]]] = {}
    expired: set[str] = set()

    for index, event in enumerate(events):
        proposal_id = event.get("proposal_id")
        if (
            event.get("event_type") == "action_executed"
            and event.get("action") == "propose_trade"
            and event.get("status") == "success"
            and proposal_id
        ):
            created[proposal_id] = (index, event)
        if (
            event.get("event_type") == "outcome_observed"
            and _actual_outcome_type(event) == "proposal_expired"
            and proposal_id
        ):
            expired.add(proposal_id)

    warnings: list[str] = []
    response_actions = {
        "accept_trade",
        "reject_trade",
        "counteroffer_trade",
    }
    for proposal_id in sorted(created.keys() & expired):
        created_index, created_event = created[proposal_id]
        counterparty = created_event.get("counterparty")
        observations = [
            event
            for event in events[created_index + 1 :]
            if event.get("event_type") == "observation_received"
            and event.get("agent_id") == counterparty
        ]
        visible = any(
            proposal.get("proposal_id") == proposal_id
            for event in observations
            for proposal in event.get("observation", {}).get(
                "incoming_proposals",
                [],
            )
        )
        if visible:
            continue
        warnings.append(
            f"WARNING: Proposal {proposal_id} was created successfully but "
            f"was not present in {counterparty}'s subsequent observation."
        )
        if observations and all(
            not response_actions.intersection(event.get("allowed_actions", []))
            for event in observations
        ):
            warnings.append(
                f"WARNING: {counterparty} had no accept, reject, or "
                "counteroffer action available."
            )
        warnings.append(
            f"WARNING: Proposal {proposal_id} later expired without being "
            "observed by the counterparty."
        )
    return warnings


def _terminal_outcome(
    decision: dict[str, Any],
    action: dict[str, Any] | None,
    events: list[dict[str, Any]],
) -> str | None:
    decision_id = decision.get("decision_id")
    proposal_id = (
        action.get("proposal_id") if action is not None else None
    ) or decision.get("proposal_id")
    outcomes = [
        event
        for event in events
        if event.get("event_type") == "outcome_observed"
        and (
            (decision_id and event.get("decision_id") == decision_id)
            or (proposal_id and event.get("proposal_id") == proposal_id)
        )
    ]
    normalized = [
        outcome
        for event in outcomes
        if (outcome := _actual_outcome_type(event)) is not None
    ]
    terminal = [
        outcome for outcome in normalized if outcome != "proposal_pending"
    ]
    return terminal[-1] if terminal else None


def render_events(
    events: list[dict[str, Any]],
    *,
    debug: bool = False,
) -> str:
    observations = {
        event.get("correlation_id"): event
        for event in events
        if event.get("event_type") == "observation_received"
    }
    actions_by_decision = {
        event.get("decision_id"): event
        for event in events
        if event.get("event_type") == "action_executed"
        and event.get("decision_id")
    }
    actions_by_correlation = {
        event.get("correlation_id"): event
        for event in events
        if event.get("event_type") == "action_executed"
    }
    decisions = [
        event for event in events if event.get("event_type") == "decision_made"
    ]

    lines: list[str] = []
    for number, decision in enumerate(decisions, start=1):
        correlation_id = decision.get("correlation_id")
        decision_id = decision.get("decision_id") or correlation_id
        observation_event = observations.get(correlation_id, {})
        observation = observation_event.get("observation", {})
        action = actions_by_decision.get(decision.get("decision_id"))
        if action is None:
            action = actions_by_correlation.get(correlation_id)
        expected = normalize_outcome_type(decision.get("expected_outcome"))
        actual = _terminal_outcome(decision, action, events)
        explanation = decision.get("explanation") or decision.get("reason")

        lines.extend(
            [
                f"Decision {decision_id or number}",
                f"Agent: {decision.get('agent_id')}",
                f"Goal: {observation_event.get('goal', 'Not stated')}",
                "Observed:",
                f"  - Cash: {observation.get('cash')}",
                f"  - Inventory: {list(observation.get('inventory', {}))}",
                "  - Incoming proposals: "
                f"{[item.get('proposal_id') for item in observation.get('incoming_proposals', [])]}",
                f"  - Allowed actions: {observation_event.get('allowed_actions', [])}",
                "Action:",
                f"  {(action or {}).get('action', decision.get('selected_action'))}",
                "Explanation:",
                f"  {explanation or 'Not stated'}",
                "Expected outcome:",
                f"  {expected or 'Not stated'}",
                "Actual outcome:",
                f"  {actual or 'Pending'}",
            ]
        )
        if expected and actual and expected != actual:
            lines.extend(
                [
                    "Result:",
                    "  WARNING: Expected and actual outcomes differ",
                ]
            )
        if debug and decision.get("raw_model_output") is not None:
            lines.extend(
                [
                    "Raw model output:",
                    f"  {decision['raw_model_output']}",
                ]
            )
        lines.append("")

    warnings = find_delivery_warnings(events)
    if warnings:
        lines.extend(["Audit warnings", *warnings])
    return "\n".join(lines).rstrip()
