from __future__ import annotations

import html
import re
from typing import Literal

from .bundles import ActionBundle, business_state, trade_direction
from .display import DisplayIds, assign_display_ids
from .models import NormalizedAuditEvent
from .normalizer import humanize, normalize_outcome

SequenceView = Literal["business", "technical"]


def _safe_id(value: str) -> str:
    result = re.sub(r"[^a-zA-Z0-9_]+", "_", value).strip("_").lower()
    if not result:
        result = "agent"
    return f"agent_{result}" if result[0].isdigit() else result


def _text(value: object) -> str:
    result = re.sub(r"\s+", " ", str(value)).strip()
    result = result.replace(";", ",").replace(":", " -")
    return html.escape(result, quote=True)


def _label(*lines: str) -> str:
    return "<br/>".join(_text(line) for line in lines if line)


def _participants(
    events: list[NormalizedAuditEvent],
) -> dict[str, str]:
    values: list[str] = []
    for event in events:
        for value in (event.actor_id, event.target_id):
            if value and value != "consumer_market" and value not in values:
                values.append(value)
    return {value: _safe_id(value) for value in values}


def _decision_links(
    events: list[NormalizedAuditEvent],
) -> dict[int, int | None]:
    decisions = [
        index
        for index, event in enumerate(events)
        if event.event_type == "decision_made"
    ]
    links: dict[int, int | None] = {}
    used: set[int] = set()
    for index, event in enumerate(events):
        if event.event_type != "action_executed":
            continue
        candidates = [
            decision_index
            for decision_index in decisions
            if events[decision_index].decision_id
            and events[decision_index].decision_id == event.decision_id
        ]
        if not candidates and event.action_id:
            candidates = [
                decision_index
                for decision_index in decisions
                if events[decision_index].action_id == event.action_id
            ]
        if not candidates and event.correlation_id:
            candidates = [
                decision_index
                for decision_index in decisions
                if events[decision_index].actor_id == event.actor_id
                and events[decision_index].correlation_id
                == event.correlation_id
            ]
        if not candidates:
            candidates = [
                decision_index
                for decision_index in decisions
                if decision_index < index
                and decision_index not in used
                and events[decision_index].actor_id == event.actor_id
                and events[decision_index].day == event.day
                and not any(
                    candidate.event_type == "action_executed"
                    and candidate.actor_id == event.actor_id
                    for candidate in events[decision_index + 1 : index]
                )
            ][-1:]
        linked = candidates[0] if len(candidates) == 1 else None
        links[index] = linked
        if linked is not None:
            used.add(linked)
    return links


def _decision_ref(
    event: NormalizedAuditEvent,
    ids: DisplayIds,
) -> str | None:
    key = event.decision_id or (
        event.correlation_id
        if event.event_type == "decision_made"
        else None
    )
    return ids.decisions.get(key) if key else None


def _case_ref(event: NormalizedAuditEvent, ids: DisplayIds) -> str | None:
    return ids.cases.get(event.case_id) if event.case_id else None


def _price(value: object) -> str:
    try:
        return f"{float(value):.2f}"
    except (TypeError, ValueError):
        return str(value)


def _business(
    events: list[NormalizedAuditEvent],
    ids: DisplayIds,
) -> str:
    participants = _participants(events)
    links = _decision_links(events)
    lines = ["sequenceDiagram"]
    for agent_id, mermaid_id in participants.items():
        name = next(
            (
                event.actor_name
                for event in events
                if event.actor_id == agent_id and event.actor_name
            ),
            humanize(agent_id),
        )
        lines.append(f"    participant {mermaid_id} as {_text(name)}")

    for index, event in enumerate(events):
        actor = participants.get(event.actor_id or "")
        if actor is None:
            continue
        if event.event_type == "observation_received":
            observation = event.raw_event.get("observation", {})
            incoming = observation.get("incoming_proposals", [])
            details = [
                f"Day {event.day} Observation",
                f"Incoming proposals: {len(incoming)}",
            ]
            inventory = observation.get("inventory")
            if isinstance(inventory, (dict, list)):
                details.append(f"Inventory: {len(inventory)}")
            if observation.get("cash") is not None:
                details.append(f"Cash: {_price(observation['cash'])}")
            lines.append(f"    Note right of {actor}: {_label(*details)}")
        elif event.event_type == "decision_made":
            decision = _decision_ref(event, ids) or "Unlinked"
            explanation = re.sub(
                r"\s+",
                " ",
                event.explanation or "Not recorded",
            ).strip()
            if len(explanation) > 180:
                explanation = explanation[:177].rstrip() + "..."
            lines.append(
                f"    Note right of {actor}: "
                f"{_label(f'Decision {decision}', f'Action: {humanize(event.action_type)}', f'Why: {explanation}', f'Expected: {event.expected_outcome or "Not recorded"}')}"
            )
        elif event.event_type == "action_executed":
            decision_index = links.get(index)
            decision = (
                _decision_ref(events[decision_index], ids)
                if decision_index is not None
                else None
            )
            case = _case_ref(event, ids)
            action = humanize(event.action_type)
            title = f"[{decision}] {action}" if decision else action
            if case:
                title += f" {case}"
            details = []
            for key, label in (
                ("lot_id", "Lot"),
                ("quantity", "Qty"),
                ("unit_price", "Price"),
            ):
                value = event.action_parameters.get(key)
                if value is not None:
                    details.append(
                        f"{label} {_price(value) if key == 'unit_price' else value}"
                    )
            target = participants.get(event.target_id or "")
            if target:
                lines.append(
                    f"    {actor}->>{target}: "
                    f"{_label(title, ' / '.join(details))}"
                )
            else:
                lines.append(
                    f"    Note over {actor}: "
                    f"{_label(title, 'Destination not recorded')}"
                )
            if not decision:
                lines.append(
                    f"    Note over {actor}: "
                    "Action could not be linked to a decision"
                )
            if case and event.action_type == "propose_trade":
                note_target = f"{actor},{target}" if target else actor
                lines.append(
                    f"    Note over {note_target}: "
                    f"{_label(f'{case} registered', 'Status: Pending')}"
                )
        elif event.event_type == "outcome_observed":
            case = _case_ref(event, ids)
            outcome = normalize_outcome(event.actual_outcome) or event.status
            target = participants.get(event.target_id or "")
            note_target = f"{actor},{target}" if target else actor
            if case:
                lines.append(
                    f"    Note over {note_target}: "
                    f"{_label(f'{case} status: {humanize(outcome)}')}"
                )
            else:
                lines.append(
                    f"    Note over {note_target}: "
                    f"{_label(f'Outcome: {humanize(outcome)}')}"
                )
    return "\n".join(lines)


def _technical(
    events: list[NormalizedAuditEvent],
    ids: DisplayIds,
) -> str:
    participants = _participants(events)
    lines = ["sequenceDiagram", "    participant env as Environment"]
    for agent_id, mermaid_id in participants.items():
        lines.append(
            f"    participant {mermaid_id} as {_text(humanize(agent_id))}"
        )
    for event in events:
        actor = participants.get(event.actor_id or "", "env")
        case = _case_ref(event, ids)
        if event.event_type == "observation_received":
            lines.append(
                f"    env->>{actor}: "
                f"{_text(f'Day {event.day} Observation')}"
            )
        elif event.event_type == "decision_made":
            lines.append(
                f"    Note right of {actor}: "
                f"{_text(f'Decision {_decision_ref(event, ids) or "unlinked"} | {event.action_type}')}"
            )
        elif event.event_type == "action_executed":
            lines.append(
                f"    {actor}->>env: "
                f"{_text(f'{event.action_type} | {case or "no case"}')}"
            )
            if event.target_id in participants:
                lines.append(
                    f"    env->>{participants[event.target_id]}: "
                    f"{_text(f'Route {case or event.action_type}')}"
                )
        elif event.event_type == "outcome_observed":
            lines.append(
                f"    Note over env,{actor}: "
                f"{_text(f'Outcome {event.status or normalize_outcome(event.actual_outcome)}')}"
            )
    return "\n".join(lines)


def build_sequence(
    events: list[NormalizedAuditEvent],
    *,
    view: SequenceView = "business",
    display_ids: DisplayIds | None = None,
) -> str:
    ids = display_ids or assign_display_ids(events)
    if view == "business":
        return _business(events, ids)
    if view == "technical":
        return _technical(events, ids)
    raise ValueError(f"unknown view: {view}")


def build_action_sequence(
    bundle: ActionBundle,
    *,
    action_display_id: str,
) -> str:
    action = bundle.action
    actor_id = _safe_id(action.actor_id or "agent")
    actor_name = action.actor_name or humanize(action.actor_id)
    target_is_agent = bool(
        action.target_id
        and action.target_id != "consumer_market"
        and action.target_id != action.actor_id
    )
    target_id = _safe_id(action.target_id or "")
    target_name = action.target_name or humanize(action.target_id)
    lines = [
        "sequenceDiagram",
        f"    participant {actor_id} as {_text(actor_name)}",
    ]
    if target_is_agent:
        lines.append(
            f"    participant {target_id} as {_text(target_name)}"
        )
    if bundle.observation:
        lines.append(f"    Note right of {actor_id}: Observation")
    if bundle.decision:
        lines.append(
            f"    Note right of {actor_id}: "
            f"{_text(f'Decision: {humanize(action.action_type)}')}"
        )

    parameters = action.action_parameters
    action_label = (
        trade_direction(bundle)
        if action.action_type == "propose_trade"
        else humanize(action.action_type)
    )
    details = [f"[{action_display_id}] {action_label}"]
    if parameters.get("lot_id"):
        details.append(str(parameters["lot_id"]))
    if parameters.get("quantity") is not None:
        details.append(f"{parameters['quantity']} units")
    if parameters.get("unit_price") is not None:
        details.append(f"at {_price(parameters['unit_price'])}")
    if target_is_agent:
        lines.append(
            f"    {actor_id}->>{target_id}: {_text(', '.join(details))}"
        )
    else:
        lines.append(
            f"    Note over {actor_id}: {_text(', '.join(details))}"
        )
    outcome_target = (
        f"{actor_id},{target_id}" if target_is_agent else actor_id
    )
    for intervention in bundle.human_interventions:
        details = [
            f"Human intervention: {humanize(intervention.intervention_type)}"
        ]
        if intervention.reason:
            details.append(f"Reason: {intervention.reason}")
        lines.append(
            f"    Note over {outcome_target}: {_label(*details)}"
        )
    if bundle.outcome:
        lines.append(
            f"    Note over {outcome_target}: "
            f"{_text(f'Outcome: {business_state(bundle)}')}"
        )
    else:
        lines.append(
            f"    Note over {outcome_target}: Outcome: Not recorded"
        )
    return "\n".join(lines)
