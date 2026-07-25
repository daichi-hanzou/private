from __future__ import annotations

from .models import NormalizedAuditEvent


def _observation_contains(event: NormalizedAuditEvent, case_id: str) -> bool:
    return any(
        item.get("proposal_id") == case_id
        or item.get("case_id") == case_id
        for item in event.raw_event.get("observation", {}).get(
            "incoming_proposals",
            [],
        )
    )


def related_case(
    events: list[NormalizedAuditEvent],
    case_id: str,
) -> list[NormalizedAuditEvent]:
    direct = [
        event
        for event in events
        if event.case_id == case_id or _observation_contains(event, case_id)
    ]
    correlations = {
        event.correlation_id for event in direct if event.correlation_id
    }
    decision_ids = {
        event.decision_id for event in direct if event.decision_id
    }
    action_ids = {event.action_id for event in direct if event.action_id}
    return [
        event
        for event in events
        if event in direct
        or (
            event.correlation_id is not None
            and event.correlation_id in correlations
        )
        or (event.decision_id is not None and event.decision_id in decision_ids)
        or (event.action_id is not None and event.action_id in action_ids)
    ]


def decision_case(
    events: list[NormalizedAuditEvent],
    decision_id: str,
) -> list[NormalizedAuditEvent]:
    decisions = [
        event
        for event in events
        if event.event_type == "decision_made"
        and (
            event.decision_id == decision_id
            or (
                event.decision_id is None
                and event.correlation_id == decision_id
            )
        )
    ]
    if not decisions:
        return []
    decision = decisions[0]
    return [
        event
        for event in events
        if event is decision
        or (
            decision.decision_id
            and event.decision_id == decision.decision_id
        )
        or (
            decision.correlation_id
            and event.actor_id == decision.actor_id
            and event.correlation_id == decision.correlation_id
        )
    ]
