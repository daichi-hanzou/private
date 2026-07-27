from __future__ import annotations

import json
import re
from collections.abc import Callable, Iterable, Mapping
from dataclasses import replace
from datetime import datetime
from typing import Any

from .models import NormalizedAuditEvent

SummaryFormatter = Callable[[dict[str, Any]], str]
NameFormatter = Callable[[str], str]


def humanize(value: str | None) -> str:
    return (value or "Unknown").replace("_", " ").strip().title()


def normalize_outcome(value: Any) -> str | None:
    if isinstance(value, Mapping):
        value = value.get("outcome_type") or value.get("proposal_status")
    if value is None:
        return None
    text = re.sub(r"[^a-z0-9]+", "_", str(value).lower()).strip("_")
    aliases = {
        "accept": "proposal_accepted",
        "accepted": "proposal_accepted",
        "countered": "proposal_countered",
        "expire": "proposal_expired",
        "expired": "proposal_expired",
        "pending": "proposal_pending",
        "reject": "proposal_rejected",
        "rejected": "proposal_rejected",
    }
    return aliases.get(text, text) or None


def _timestamp(value: Any) -> datetime | None:
    if not isinstance(value, str):
        return None
    try:
        return datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return None


def _case_id(raw: dict[str, Any]) -> str | None:
    parameters = raw.get("action_parameters") or {}
    metadata = raw.get("metadata") or {}
    candidates = (
        raw.get("case_id"),
        raw.get("proposal_id"),
        parameters.get("case_id") if isinstance(parameters, Mapping) else None,
        parameters.get("proposal_id")
        if isinstance(parameters, Mapping)
        else None,
        metadata.get("created_proposal_id")
        if isinstance(metadata, Mapping)
        else None,
        metadata.get("accepted_proposal_id")
        if isinstance(metadata, Mapping)
        else None,
    )
    direct = next((str(value) for value in candidates if value), None)
    if direct:
        return direct
    incoming = raw.get("observation", {}).get("incoming_proposals", [])
    incoming_ids = {
        item.get("case_id") or item.get("proposal_id")
        for item in incoming
        if isinstance(item, Mapping)
    } - {None}
    if len(incoming_ids) == 1:
        return str(next(iter(incoming_ids)))
    return None


def _action_parameters(raw: dict[str, Any]) -> dict[str, Any]:
    nested = raw.get("action_parameters")
    result = dict(nested) if isinstance(nested, Mapping) else {}
    for key in (
        "counterparty",
        "lot_id",
        "quantity",
        "unit_price",
        "proposal_id",
        "transaction_id",
    ):
        if raw.get(key) is not None:
            result.setdefault(key, raw[key])
    return result


class AuditEventNormalizer:
    def __init__(
        self,
        *,
        action_formatters: Mapping[str, SummaryFormatter] | None = None,
        agent_name: NameFormatter | None = None,
    ) -> None:
        self.action_formatters = dict(action_formatters or {})
        self.agent_name = agent_name or humanize

    def normalize(
        self,
        raw_event: dict[str, Any],
        *,
        fallback_index: int,
    ) -> NormalizedAuditEvent:
        raw = dict(raw_event)
        event_type = str(raw.get("event_type") or "unknown")
        actor_id = raw.get("agent_id") or raw.get("actor_id")
        target_id = raw.get("counterparty") or raw.get("target_id")
        action_type = raw.get("action") or raw.get("selected_action")
        case_id = _case_id(raw)
        actual = raw.get("actual_outcome", raw.get("outcome"))
        status = raw.get("status") or normalize_outcome(actual)
        parameters = _action_parameters(raw)
        summary = self._summary(
            raw,
            event_type=event_type,
            actor_id=actor_id,
            target_id=target_id,
            action_type=action_type,
            case_id=case_id,
            parameters=parameters,
            status=status,
        )
        return NormalizedAuditEvent(
            event_id=str(raw.get("event_id") or f"line-{fallback_index}"),
            event_type=event_type,
            run_id=raw.get("run_id"),
            timestamp=_timestamp(raw.get("timestamp")),
            action_time=_timestamp(
                raw.get("action_time") or raw.get("executed_at")
            ),
            observed_at=_timestamp(raw.get("observed_at")),
            day=raw.get("day") if isinstance(raw.get("day"), int) else None,
            actor_id=actor_id,
            actor_name=self.agent_name(actor_id) if actor_id else None,
            target_id=target_id,
            target_name=self.agent_name(target_id) if target_id else None,
            decision_id=raw.get("decision_id"),
            action_id=raw.get("action_id"),
            correlation_id=raw.get("correlation_id"),
            case_type=(
                raw.get("case_type")
                or ("trade_proposal" if case_id else None)
            ),
            case_id=case_id,
            action_type=action_type,
            action_parameters=parameters,
            explanation=raw.get("explanation") or raw.get("reason"),
            expected_outcome=normalize_outcome(raw.get("expected_outcome")),
            actual_outcome=actual,
            status=status,
            summary=summary,
            raw_event=raw,
            source_line=fallback_index,
        )

    def _summary(
        self,
        raw: dict[str, Any],
        *,
        event_type: str,
        actor_id: str | None,
        target_id: str | None,
        action_type: str | None,
        case_id: str | None,
        parameters: dict[str, Any],
        status: str | None,
    ) -> str:
        if (
            event_type == "action_executed"
            and action_type in self.action_formatters
        ):
            return self.action_formatters[action_type](raw)
        actor = self.agent_name(actor_id) if actor_id else "System"
        target = self.agent_name(target_id) if target_id else None
        if event_type == "observation_received":
            incoming = raw.get("observation", {}).get(
                "incoming_proposals",
                [],
            )
            return f"{actor} received an observation ({len(incoming)} incoming cases)"
        if event_type == "decision_made":
            return f"{actor} decided to {humanize(action_type).lower()}"
        if event_type == "action_executed":
            details = []
            if target:
                details.append(f"for {target}")
            if parameters.get("lot_id"):
                details.append(str(parameters["lot_id"]))
            if parameters.get("quantity") is not None:
                details.append(f"quantity {parameters['quantity']}")
            if parameters.get("unit_price") is not None:
                details.append(f"price {parameters['unit_price']}")
            suffix = ", ".join(details)
            return (
                f"{actor} executed {humanize(action_type)}"
                + (f": {suffix}" if suffix else "")
            )
        if event_type == "outcome_observed":
            subject = f"Case {case_id}" if case_id else "Action"
            return f"{subject} outcome: {humanize(status)}"
        if event_type == "human_intervention":
            intervention = raw.get("intervention_type")
            related = raw.get("related_action_id") or "unknown action"
            return (
                f"Human intervention {humanize(intervention).lower()} "
                f"for {related}"
            )
        return f"{humanize(event_type)} event by {actor}"


def normalize_events(
    raw_events: Iterable[dict[str, Any]],
    *,
    normalizer: AuditEventNormalizer | None = None,
) -> list[NormalizedAuditEvent]:
    adapter = normalizer or AuditEventNormalizer()
    events = []
    seen_event_ids: dict[str, int] = {}
    for index, raw in enumerate(raw_events, start=1):
        event = adapter.normalize(raw, fallback_index=index)
        occurrence = seen_event_ids.get(event.event_id, 0) + 1
        seen_event_ids[event.event_id] = occurrence
        if occurrence > 1:
            event = replace(
                event,
                event_id=f"{event.event_id}#{occurrence}",
            )
        events.append(event)
    return sorted(
        events,
        key=lambda event: (
            event.timestamp is None,
            event.timestamp or datetime.min,
            event.day or 0,
            event.source_line or 0,
        ),
    )


def searchable_text(event: NormalizedAuditEvent) -> str:
    values = [
        event.summary,
        event.explanation,
        event.expected_outcome,
        event.actual_outcome,
        event.action_type,
        event.actor_id,
        event.actor_name,
        event.target_id,
        event.case_id,
        event.decision_id,
        event.action_id,
        event.event_id,
        json.dumps(event.raw_event, ensure_ascii=False, sort_keys=True),
    ]
    return " ".join(str(value) for value in values if value is not None)
