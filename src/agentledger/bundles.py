from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
from typing import Any

from .models import (
    ExecutionContext,
    HumanInterventionEvent,
    NormalizedAuditEvent,
)
from .normalizer import humanize, normalize_outcome

OUTCOME_STATUSES = {
    "pending",
    "confirmed",
    "failed",
    "contradicted",
    "expired",
    "unknown",
}


@dataclass(frozen=True)
class ActionBundle:
    action: NormalizedAuditEvent
    observation: NormalizedAuditEvent | None = None
    decision: NormalizedAuditEvent | None = None
    outcome: NormalizedAuditEvent | None = None
    human_interventions: tuple[HumanInterventionEvent, ...] = ()
    execution_context: ExecutionContext = ExecutionContext()

    @property
    def events(self) -> list[NormalizedAuditEvent]:
        return [
            event
            for event in (
                self.observation,
                self.decision,
                self.action,
                self.outcome,
            )
            if event is not None
        ]


def _parse_timestamp(value: Any) -> datetime | None:
    if not isinstance(value, str):
        return None
    try:
        return datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return None


def _event_order(event: NormalizedAuditEvent, *, observed: bool) -> tuple:
    raw_time = event.observed_at if observed else None
    timestamp = raw_time or event.timestamp
    return (
        timestamp.timestamp() if timestamp else float("-inf"),
        event.day or 0,
        event.source_line or 0,
    )


def _keep_latest(
    values: dict[str, NormalizedAuditEvent],
    key: str,
    event: NormalizedAuditEvent,
) -> None:
    current = values.get(key)
    if current is None or _event_order(
        event,
        observed=True,
    ) >= _event_order(current, observed=True):
        values[key] = event


def _human_intervention(
    event: NormalizedAuditEvent,
) -> HumanInterventionEvent | None:
    raw = event.raw_event
    related_action_id = raw.get("related_action_id") or event.action_id
    if not related_action_id:
        return None
    return HumanInterventionEvent(
        event_id=event.event_id,
        related_action_id=str(related_action_id),
        intervention_type=str(raw.get("intervention_type") or "unknown"),
        performed_at=_parse_timestamp(raw.get("performed_at"))
        or event.timestamp,
        actor=str(raw.get("actor") or "human"),
        before=raw.get("before"),
        after=raw.get("after"),
        reason=raw.get("reason"),
        input_method=raw.get("input_method"),
        raw_event=raw,
    )


def _execution_context(action: NormalizedAuditEvent) -> ExecutionContext:
    raw = action.raw_event
    metadata = raw.get("metadata")
    value = raw.get("execution_context")
    if value is None and isinstance(metadata, dict):
        value = metadata.get("execution_context")
    return ExecutionContext.from_value(value)


def build_action_bundles(
    events: list[NormalizedAuditEvent],
) -> list[ActionBundle]:
    latest_observation: dict[str, NormalizedAuditEvent] = {}
    decisions: dict[str, NormalizedAuditEvent] = {}
    decision_observations: dict[str, NormalizedAuditEvent] = {}
    action_context: dict[
        str,
        tuple[
            NormalizedAuditEvent | None,
            NormalizedAuditEvent | None,
        ],
    ] = {}
    outcomes_by_action: dict[str, NormalizedAuditEvent] = {}
    outcomes_by_decision: dict[str, NormalizedAuditEvent] = {}
    interventions_by_action: dict[
        str,
        list[HumanInterventionEvent],
    ] = {}

    for event in events:
        if (
            event.event_type == "observation_received"
            and event.correlation_id
        ):
            latest_observation[event.correlation_id] = event
        elif event.event_type == "decision_made":
            if event.decision_id:
                decisions[event.decision_id] = event
                if (
                    event.correlation_id
                    and event.correlation_id in latest_observation
                ):
                    decision_observations[event.decision_id] = (
                        latest_observation[event.correlation_id]
                    )
        elif event.event_type == "action_executed":
            decision = (
                decisions.get(event.decision_id)
                if event.decision_id
                else None
            )
            observation = (
                decision_observations.get(event.decision_id)
                or latest_observation.get(event.correlation_id or "")
                if event.decision_id
                else latest_observation.get(event.correlation_id or "")
            )
            action_context[event.event_id] = (decision, observation)
        if event.event_type == "outcome_observed" and event.action_id:
            _keep_latest(outcomes_by_action, event.action_id, event)
        if event.event_type == "outcome_observed" and event.decision_id:
            _keep_latest(outcomes_by_decision, event.decision_id, event)
        if event.event_type == "human_intervention":
            intervention = _human_intervention(event)
            if intervention:
                interventions_by_action.setdefault(
                    intervention.related_action_id,
                    [],
                ).append(intervention)

    bundles = []
    for action in events:
        if action.event_type != "action_executed":
            continue
        decision, observation = action_context.get(
            action.event_id,
            (None, None),
        )
        outcome = (
            outcomes_by_action.get(action.action_id)
            if action.action_id
            else None
        )
        if outcome is None and action.decision_id:
            outcome = outcomes_by_decision.get(action.decision_id)
        intervention_keys = {action.event_id}
        if action.action_id:
            intervention_keys.add(action.action_id)
        interventions = [
            item
            for key in intervention_keys
            for item in interventions_by_action.get(key, [])
        ]
        interventions.sort(
            key=lambda item: (
                item.performed_at.timestamp()
                if item.performed_at
                else float("-inf"),
                item.event_id,
            )
        )
        bundles.append(
            ActionBundle(
                action=action,
                observation=observation,
                decision=decision,
                outcome=outcome,
                human_interventions=tuple(interventions),
                execution_context=_execution_context(action),
            )
        )
    return bundles


def display_action_ids(
    bundles: list[ActionBundle],
) -> dict[str, str]:
    return {
        bundle.action.event_id: f"A{index}"
        for index, bundle in enumerate(bundles, start=1)
    }


def display_case_ids(
    bundles: list[ActionBundle],
) -> dict[str, str]:
    result: dict[str, str] = {}
    for bundle in bundles:
        if bundle.action.case_id and bundle.action.case_id not in result:
            result[bundle.action.case_id] = f"P{len(result) + 1}"
    return result


def action_summary(
    bundle: ActionBundle,
    case_ids: dict[str, str],
) -> str:
    action = bundle.action
    parameters = action.action_parameters
    quantity = parameters.get("quantity")
    unit_price = parameters.get("unit_price")
    case = case_ids.get(action.case_id or "", action.case_id)
    if action.action_type == "propose_trade":
        direction = trade_direction(bundle)
        details = []
        if quantity is not None:
            details.append(f"{quantity} units")
        if unit_price is not None:
            details.append(f"at {unit_price}")
        return " ".join([direction, *details])
    if action.action_type == "accept_trade":
        return f"Accepted proposal {case or 'Not recorded'}"
    if action.action_type == "reject_trade":
        return f"Rejected proposal {case or 'Not recorded'}"
    if action.action_type == "counteroffer_trade":
        return f"Counteroffered {unit_price} for {case or 'proposal'}"
    if action.action_type == "sell_to_consumer":
        summary = f"Sold {quantity} units to consumers"
        return f"{summary} at {unit_price}" if unit_price is not None else summary
    if action.action_type == "send_message":
        return "Sent a message"
    if action.action_type == "wait":
        return "Waited without taking a market action"
    return action.summary


def trade_direction(bundle: ActionBundle) -> str:
    action = bundle.action
    if action.action_type != "propose_trade":
        return humanize(action.action_type)
    seller_id = action.raw_event.get("seller_id")
    buyer_id = action.raw_event.get("buyer_id")
    if seller_id and buyer_id:
        if action.actor_id == buyer_id:
            return "Offer to Buy"
        if action.actor_id == seller_id:
            return "Offer to Sell"
        return (
            f"Propose Trade: {humanize(seller_id)} sells to "
            f"{humanize(buyer_id)}"
        )
    return "Propose Trade"


def target_summary(
    bundle: ActionBundle,
    case_ids: dict[str, str],
) -> str:
    action = bundle.action
    parts = []
    if action.target_name:
        parts.append(action.target_name)
    elif action.target_id:
        parts.append(humanize(action.target_id))
    proposal_actions = {
        "accept_trade",
        "reject_trade",
        "counteroffer_trade",
    }
    if action.action_type in proposal_actions and action.case_id:
        parts.append(case_ids.get(action.case_id, action.case_id))
    elif action.action_parameters.get("lot_id"):
        parts.append(str(action.action_parameters["lot_id"]))
    elif action.case_id:
        parts.append(case_ids.get(action.case_id, action.case_id))
    return " / ".join(parts) or "-"


def business_state(bundle: ActionBundle) -> str:
    outcome = bundle.outcome
    if outcome is None:
        return "Pending"
    value = normalize_outcome(outcome.actual_outcome) or outcome.status
    return humanize(value)


def outcome_status(bundle: ActionBundle) -> str:
    outcome = bundle.outcome
    if outcome is None:
        return "pending"
    raw_status = outcome.raw_event.get("status")
    if raw_status is None:
        return "confirmed"
    normalized = str(raw_status).strip().lower()
    return normalized if normalized in OUTCOME_STATUSES else "unknown"


def action_time(bundle: ActionBundle) -> str | None:
    if bundle.action.action_time:
        return bundle.action.action_time.isoformat()
    return (
        bundle.action.timestamp.isoformat()
        if bundle.action.timestamp
        else None
    )


def observed_at(bundle: ActionBundle) -> str | None:
    outcome = bundle.outcome
    if outcome is None:
        return None
    if outcome.observed_at:
        return outcome.observed_at.isoformat()
    return outcome.timestamp.isoformat() if outcome.timestamp else None


def execution_result(bundle: ActionBundle) -> str:
    status = str(bundle.action.status or "").lower()
    if status in {"success", "completed"}:
        return "Success"
    if status in {"failed", "failure", "invalid", "error"}:
        return "Failure"
    return humanize(status) if status else "Not recorded"


def expected_outcome(bundle: ActionBundle) -> Any:
    if bundle.decision is None:
        return None
    return bundle.decision.raw_event.get("expected_outcome")
