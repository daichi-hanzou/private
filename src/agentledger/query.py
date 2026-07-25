from __future__ import annotations

from dataclasses import dataclass

from .models import NormalizedAuditEvent
from .normalizer import searchable_text


@dataclass(frozen=True)
class AuditQuery:
    run_id: str | None = None
    day: int | None = None
    agent: str | None = None
    event_type: str | None = None
    action_type: str | None = None
    case_type: str | None = None
    case_id: str | None = None
    proposal_id: str | None = None
    decision_id: str | None = None
    status: str | None = None
    counterparty: str | None = None
    keyword: str | None = None


def filter_events(
    events: list[NormalizedAuditEvent],
    query: AuditQuery,
) -> list[NormalizedAuditEvent]:
    case_id = query.case_id or query.proposal_id
    result = []
    for event in events:
        if query.run_id and event.run_id != query.run_id:
            continue
        if query.day is not None and event.day != query.day:
            continue
        if query.agent and event.actor_id != query.agent:
            continue
        if query.event_type and event.event_type != query.event_type:
            continue
        if query.action_type and event.action_type != query.action_type:
            continue
        if query.case_type and event.case_type != query.case_type:
            continue
        if case_id and event.case_id != case_id:
            continue
        if query.decision_id and event.decision_id != query.decision_id:
            continue
        if query.status and event.status != query.status:
            continue
        if query.counterparty and event.target_id != query.counterparty:
            continue
        if query.keyword and query.keyword.casefold() not in searchable_text(
            event
        ).casefold():
            continue
        result.append(event)
    return result
