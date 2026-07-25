from __future__ import annotations

import html
import re
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Literal

from .replay import normalize_outcome_type

MAX_EXPLANATION_LENGTH = 140
SequenceView = Literal["business", "technical"]


def participant_id(agent_id: str) -> str:
    normalized = re.sub(r"[^a-zA-Z0-9_]+", "_", agent_id).strip("_").lower()
    if not normalized:
        normalized = "agent"
    if normalized[0].isdigit():
        normalized = f"agent_{normalized}"
    return normalized


def participant_label(agent_id: str) -> str:
    known = {
        "roaster": "Roaster",
        "retailer_a": "Retailer A",
        "retailer_b": "Retailer B",
    }
    return known.get(
        agent_id,
        " ".join(part.capitalize() for part in agent_id.split("_")),
    )


def mermaid_text(value: Any, *, limit: int | None = None) -> str:
    text = str(value if value is not None else "")
    text = re.sub(r"\s+", " ", text).strip()
    if limit is not None and len(text) > limit:
        text = text[: max(0, limit - 3)].rstrip() + "..."
    text = (
        text.replace(";", ",")
        .replace(":", " -")
        .replace("%%", "% %")
        .replace("-->>", "⇢")
        .replace("->>", "→")
    )
    return html.escape(text, quote=True)


def _label(*parts: str) -> str:
    return "<br/>".join(mermaid_text(part) for part in parts if part)


def _short_text(value: Any, *, limit: int) -> str:
    text = re.sub(r"\s+", " ", str(value if value is not None else "")).strip()
    if len(text) > limit:
        return text[: max(0, limit - 3)].rstrip() + "..."
    return text


def sort_events(events: list[dict[str, Any]]) -> list[dict[str, Any]]:
    def key(item: tuple[int, dict[str, Any]]) -> tuple:
        index, event = item
        timestamp = event.get("timestamp")
        if timestamp:
            return (0, str(timestamp), index)
        return (1, int(event.get("day") or 0), index)

    return [event for _, event in sorted(enumerate(events), key=key)]


def filter_events(
    events: list[dict[str, Any]],
    *,
    day: int | None = None,
    proposal_id: str | None = None,
    decision_id: str | None = None,
    agent_id: str | None = None,
) -> list[dict[str, Any]]:
    indexed = list(enumerate(events))
    selected_indices = set(range(len(events)))

    if proposal_id is not None:
        direct = {
            index
            for index, event in indexed
            if event.get("proposal_id") == proposal_id
            or any(
                proposal.get("proposal_id") == proposal_id
                for proposal in event.get("observation", {}).get(
                    "incoming_proposals",
                    [],
                )
            )
        }
        creation = next(
            (
                (index, event)
                for index, event in indexed
                if event.get("event_type") == "action_executed"
                and event.get("proposal_id") == proposal_id
                and event.get("action") == "propose_trade"
            ),
            None,
        )
        terminal_indices = [
            index
            for index, event in indexed
            if event.get("event_type") == "outcome_observed"
            and event.get("proposal_id") == proposal_id
            and normalize_outcome_type(
                event.get("actual_outcome", event.get("outcome"))
            )
            not in {None, "proposal_pending"}
        ]
        if creation is not None:
            creation_index, creation_event = creation
            terminal_index = (
                max(terminal_indices)
                if terminal_indices
                else len(events) - 1
            )
            counterparty = creation_event.get("counterparty")
            direct.update(
                index
                for index, event in indexed
                if creation_index < index <= terminal_index
                and event.get("event_type") == "observation_received"
                and event.get("agent_id") == counterparty
            )

        correlation_ids = {
            events[index].get("correlation_id") for index in direct
        } - {None}
        decision_ids = {
            events[index].get("decision_id") for index in direct
        } - {None}
        action_ids = {
            events[index].get("action_id") for index in direct
        } - {None}
        selected_indices = {
            index
            for index, event in indexed
            if index in direct
            or event.get("correlation_id") in correlation_ids
            or event.get("decision_id") in decision_ids
            or event.get("action_id") in action_ids
        }

    if decision_id is not None:
        correlations = {
            event.get("correlation_id")
            for event in events
            if event.get("decision_id") == decision_id
        } - {None}
        selected_indices &= {
            index
            for index, event in indexed
            if event.get("decision_id") == decision_id
            or event.get("correlation_id") in correlations
        }
    if day is not None:
        selected_indices &= {
            index for index, event in indexed if event.get("day") == day
        }
    if agent_id is not None:
        selected_indices &= {
            index
            for index, event in indexed
            if event.get("agent_id") == agent_id
            or event.get("counterparty") == agent_id
        }
    return [
        event for index, event in indexed if index in selected_indices
    ]


def _participant_map(
    events: list[dict[str, Any]],
    *,
    include_default_agents: bool = False,
) -> dict[str, str]:
    agent_ids = (
        ["roaster", "retailer_a", "retailer_b"]
        if include_default_agents
        else []
    )
    for event in events:
        for field in ("agent_id", "counterparty"):
            value = event.get(field)
            if value and value not in agent_ids and value != "consumer_market":
                agent_ids.append(value)
    result: dict[str, str] = {}
    used = {"env"}
    for agent_id in agent_ids:
        candidate = participant_id(agent_id)
        suffix = 2
        while candidate in used:
            candidate = f"{participant_id(agent_id)}_{suffix}"
            suffix += 1
        used.add(candidate)
        result[agent_id] = candidate
    return result


@dataclass(frozen=True)
class DisplayIds:
    decisions: dict[int, str]
    actions: dict[int, str]
    proposals: dict[str, str]
    decision_source: dict[int, str]
    action_source: dict[int, str]


def _display_ids(events: list[dict[str, Any]]) -> DisplayIds:
    decisions: dict[int, str] = {}
    actions: dict[int, str] = {}
    proposals: dict[str, str] = {}
    decision_source: dict[int, str] = {}
    action_source: dict[int, str] = {}

    for index, event in enumerate(events):
        if event.get("event_type") == "decision_made":
            decisions[index] = f"D{len(decisions) + 1}"
            decision_source[index] = str(
                event.get("decision_id")
                or event.get("correlation_id")
                or f"event line {index + 1}"
            )
        if event.get("event_type") == "action_executed":
            actions[index] = f"A{len(actions) + 1}"
            action_source[index] = str(
                event.get("action_id")
                or event.get("correlation_id")
                or f"event line {index + 1}"
            )
        proposal_ids = [event.get("proposal_id")]
        proposal_ids.extend(
            item.get("proposal_id")
            for item in event.get("observation", {}).get(
                "incoming_proposals",
                [],
            )
        )
        for proposal_id in proposal_ids:
            if proposal_id and proposal_id not in proposals:
                proposals[proposal_id] = f"P{len(proposals) + 1}"
    return DisplayIds(
        decisions=decisions,
        actions=actions,
        proposals=proposals,
        decision_source=decision_source,
        action_source=action_source,
    )


def _link_actions_to_decisions(
    events: list[dict[str, Any]],
) -> dict[int, int | None]:
    decision_indices = [
        index
        for index, event in enumerate(events)
        if event.get("event_type") == "decision_made"
    ]
    by_decision_id = {
        event["decision_id"]: index
        for index, event in enumerate(events)
        if event.get("event_type") == "decision_made"
        and event.get("decision_id")
    }
    by_action_id = {
        event["action_id"]: index
        for index, event in enumerate(events)
        if event.get("event_type") == "decision_made"
        and event.get("action_id")
    }
    by_correlation: dict[tuple[str, str], list[int]] = {}
    for index in decision_indices:
        event = events[index]
        key = (event.get("agent_id"), event.get("correlation_id"))
        if all(key):
            by_correlation.setdefault(key, []).append(index)

    links: dict[int, int | None] = {}
    claimed: set[int] = set()
    for action_index, action in enumerate(events):
        if action.get("event_type") != "action_executed":
            continue
        linked = by_decision_id.get(action.get("decision_id"))
        if linked is None:
            linked = by_action_id.get(action.get("action_id"))
        if linked is None:
            candidates = by_correlation.get(
                (action.get("agent_id"), action.get("correlation_id")),
                [],
            )
            if len(candidates) == 1:
                linked = candidates[0]
        if linked is None:
            candidates = [
                index
                for index in decision_indices
                if index < action_index
                and index not in claimed
                and events[index].get("agent_id") == action.get("agent_id")
                and events[index].get("day") == action.get("day")
                and not any(
                    event.get("event_type") == "action_executed"
                    and event.get("agent_id") == action.get("agent_id")
                    for event in events[index + 1 : action_index]
                )
            ]
            if candidates:
                linked = candidates[-1]
        links[action_index] = linked
        if linked is not None:
            claimed.add(linked)
    return links


def _expected_maps(
    events: list[dict[str, Any]],
) -> tuple[dict[str, str], dict[str, str]]:
    expected_by_decision = {
        event["decision_id"]: expected
        for event in events
        if event.get("event_type") == "decision_made"
        and event.get("decision_id")
        and (
            expected := normalize_outcome_type(
                event.get("expected_outcome")
            )
        )
    }
    expected_by_proposal: dict[str, str] = {}
    for event in events:
        proposal_id = event.get("proposal_id")
        decision_id = event.get("decision_id")
        if (
            event.get("event_type") == "action_executed"
            and proposal_id
            and decision_id in expected_by_decision
        ):
            expected_by_proposal[proposal_id] = expected_by_decision[decision_id]
    return expected_by_decision, expected_by_proposal


def _action_name(action: str | None) -> str:
    return {
        "propose_trade": "Propose trade",
        "counteroffer_trade": "Counteroffer",
        "accept_trade": "Accept proposal",
        "reject_trade": "Reject proposal",
        "accept_counteroffer": "Accept counteroffer",
        "reject_counteroffer": "Reject counteroffer",
        "sell_to_consumer": "Sell to consumer",
        "send_message": "Send message",
        "wait": "Wait",
    }.get(action or "", (action or "Unknown action").replace("_", " ").title())


def _price(value: Any) -> str | None:
    if value is None:
        return None
    try:
        return f"{float(value):.2f}"
    except (TypeError, ValueError):
        return str(value)


def _proposal_parties(
    events: list[dict[str, Any]],
) -> dict[str, tuple[str | None, str | None]]:
    parties: dict[str, tuple[str | None, str | None]] = {}
    for event in events:
        proposal_id = event.get("proposal_id")
        if (
            proposal_id
            and event.get("event_type") == "action_executed"
            and event.get("action") == "propose_trade"
        ):
            parties[proposal_id] = (
                event.get("agent_id"),
                event.get("counterparty"),
            )
    return parties


def _action_destination(
    event: dict[str, Any],
    proposal_parties: dict[str, tuple[str | None, str | None]],
) -> str | None:
    counterparty = event.get("counterparty")
    if counterparty and counterparty != "consumer_market":
        return counterparty
    proposal_id = event.get("proposal_id")
    actor = event.get("agent_id")
    seller, buyer = proposal_parties.get(proposal_id, (None, None))
    if actor == seller:
        return buyer
    if actor == buyer:
        return seller
    return None


def _invisible_expired_proposals(events: list[dict[str, Any]]) -> set[str]:
    expired = {
        event["proposal_id"]
        for event in events
        if event.get("proposal_id")
        and event.get("event_type") == "outcome_observed"
        and normalize_outcome_type(
            event.get("actual_outcome", event.get("outcome"))
        )
        == "proposal_expired"
    }
    visible = {
        item.get("proposal_id")
        for event in events
        if event.get("event_type") == "observation_received"
        for item in event.get("observation", {}).get("incoming_proposals", [])
    }
    return expired - visible


def _business_action_label(
    event: dict[str, Any],
    *,
    decision_ref: str | None,
    action_ref: str,
    proposal_ref: str | None,
) -> str:
    prefix = f"[{decision_ref}]" if decision_ref else f"[{action_ref}]"
    action = event.get("action")
    if action == "propose_trade" and proposal_ref:
        title = f"{prefix} Proposal {proposal_ref}"
    elif action == "counteroffer_trade" and proposal_ref:
        title = f"{prefix} Counteroffer for {proposal_ref}"
    elif action in {"accept_trade", "reject_trade"} and proposal_ref:
        title = f"{prefix} {_action_name(action)} {proposal_ref}"
    else:
        title = f"{prefix} {_action_name(action)}"
    details = [
        str(event["lot_id"]) if event.get("lot_id") else "",
        f"Qty {event['quantity']}" if event.get("quantity") is not None else "",
        f"Price {_price(event.get('unit_price'))}"
        if event.get("unit_price") is not None
        else "",
    ]
    detail = " / ".join(item for item in details if item)
    return _label(title, detail)


def _build_business_sequence_diagram(events: list[dict[str, Any]]) -> str:
    participants = _participant_map(events)
    display = _display_ids(events)
    links = _link_actions_to_decisions(events)
    expected_by_decision, expected_by_proposal = _expected_maps(events)
    proposal_parties = _proposal_parties(events)
    invisible_expired = _invisible_expired_proposals(events)
    warned_missing: set[str] = set()
    active_proposals: dict[str, dict[str, Any]] = {}

    lines = ["sequenceDiagram"]
    for agent_id, mermaid_id in participants.items():
        lines.append(
            f"    participant {mermaid_id} as "
            f"{mermaid_text(participant_label(agent_id))}"
        )

    for index, event in enumerate(events):
        event_type = event.get("event_type")
        agent_id = event.get("agent_id")
        actor = participants.get(agent_id)
        if actor is None:
            continue

        if event_type == "observation_received":
            observation = event.get("observation", {})
            incoming = {
                item.get("proposal_id")
                for item in observation.get(
                    "incoming_proposals",
                    [],
                )
            }
            revenue = observation.get("reported_revenue")
            target = observation.get("revenue_target")
            shortfall = None
            if isinstance(revenue, (int, float)) and isinstance(
                target,
                (int, float),
            ):
                shortfall = max(0.0, target - revenue)
            observation_details = [
                f"Incoming proposals: {len(incoming)}",
            ]
            if shortfall is not None:
                observation_details.append(
                    f"Revenue shortfall: {shortfall:,.2f}"
                )
            if observation.get("cash") is not None:
                observation_details.append(
                    f"Available cash: {_price(observation['cash'])}"
                )
            lines.append(
                f"    Note right of {actor}: "
                f"{_label(f'Day {event.get("day")} Observation', *observation_details)}"
            )
            for proposal_id, proposal in active_proposals.items():
                if proposal.get("counterparty") != agent_id:
                    continue
                proposal_ref = display.proposals.get(proposal_id, proposal_id)
                source_id = participants.get(proposal.get("agent_id"), actor)
                if proposal_id in incoming:
                    lines.append(
                        f"    Note over {source_id},{actor}: "
                        f"{_label(f'{proposal_ref} visible to {participant_label(agent_id)}')}"
                    )
                elif (
                    proposal_id in invisible_expired
                    and proposal_id not in warned_missing
                ):
                    lines.append(
                        f"    Note over {source_id},{actor}: "
                        f"{_label(f'WARNING: Proposal {proposal_ref} was not visible to {participant_label(agent_id)}')}"
                    )
                    warned_missing.add(proposal_id)

        elif event_type == "decision_made":
            decision_ref = display.decisions[index]
            explanation = event.get("explanation") or event.get("reason")
            expected = normalize_outcome_type(event.get("expected_outcome"))
            lines.append(
                f"    Note right of {actor}: "
                f"{_label(f'Decision {decision_ref}', f'Action: {_action_name(event.get("selected_action"))}', f'Why: {_short_text(explanation or "Not recorded", limit=MAX_EXPLANATION_LENGTH)}', f'Expected: {expected or "Not recorded"}')}"
            )

        elif event_type == "action_executed":
            action_ref = display.actions[index]
            decision_index = links.get(index)
            decision_ref = (
                display.decisions.get(decision_index)
                if decision_index is not None
                else None
            )
            proposal_id = event.get("proposal_id")
            proposal_ref = display.proposals.get(proposal_id)
            destination_id = _action_destination(event, proposal_parties)
            destination = participants.get(destination_id)
            action = event.get("action")

            if action in {"wait", "sell_to_consumer"}:
                lines.append(
                    f"    Note over {actor}: "
                    f"{_business_action_label(event, decision_ref=decision_ref, action_ref=action_ref, proposal_ref=proposal_ref)}"
                )
            elif destination is not None:
                lines.append(
                    f"    {actor}->>{destination}: "
                    f"{_business_action_label(event, decision_ref=decision_ref, action_ref=action_ref, proposal_ref=proposal_ref)}"
                )
            else:
                lines.append(
                    f"    Note over {actor}: "
                    f"{_label(f'[{decision_ref or action_ref}] {_action_name(action)}', 'WARNING: Action destination unknown')}"
                )
            if decision_ref is None:
                lines.append(
                    f"    Note over {actor}: "
                    "WARNING - Action could not be linked to a decision"
                )

            if (
                action == "propose_trade"
                and event.get("status") == "success"
                and proposal_id
            ):
                active_proposals[proposal_id] = event
                note_target = (
                    f"{actor},{destination}" if destination is not None else actor
                )
                lines.append(
                    f"    Note over {note_target}: "
                    f"{_label(f'{proposal_ref} registered', 'Status: Pending')}"
                )

        elif event_type == "outcome_observed":
            actual = normalize_outcome_type(
                event.get("actual_outcome", event.get("outcome"))
            )
            if actual is None:
                continue
            proposal_id = event.get("proposal_id")
            proposal_ref = display.proposals.get(proposal_id)
            seller, buyer = proposal_parties.get(proposal_id, (None, None))
            target_ids = [
                participants[item]
                for item in (seller, buyer)
                if item in participants
            ]
            note_target = ",".join(dict.fromkeys(target_ids)) or actor
            expected = expected_by_decision.get(event.get("decision_id"))
            if expected is None and proposal_id:
                expected = expected_by_proposal.get(proposal_id)
            if (
                expected
                and actual != "proposal_pending"
                and expected != actual
            ):
                lines.append(
                    f"    Note over {note_target}: "
                    f"{_label(f'WARNING: Expected {expected} / Actual {actual}')}"
                )
            if proposal_ref and actual == "proposal_countered":
                lines.append(
                    f"    Note over {note_target}: "
                    f"{_label(f'System changed {proposal_ref} status to Countered')}"
                )
            elif proposal_ref and actual in {
                "proposal_accepted",
                "proposal_rejected",
                "proposal_expired",
            }:
                status = actual.removeprefix("proposal_").title()
                lines.append(
                    f"    Note over {note_target}: "
                    f"{_label(f'System changed {proposal_ref} status to {status}')}"
                )
                active_proposals.pop(proposal_id, None)
            elif actual not in {"proposal_pending"}:
                lines.append(
                    f"    Note over {note_target}: "
                    f"{_label(f'Actual: {actual}')}"
                )
    return "\n".join(lines)


def _build_technical_sequence_diagram(events: list[dict[str, Any]]) -> str:
    participants = _participant_map(events, include_default_agents=True)
    expected_by_decision, expected_by_proposal = _expected_maps(events)
    invisible_expired = _invisible_expired_proposals(events)
    warned_missing: set[str] = set()
    active_proposals: dict[str, dict[str, Any]] = {}

    lines = ["sequenceDiagram", "    participant env as Environment"]
    for agent_id, mermaid_id in participants.items():
        lines.append(
            f"    participant {mermaid_id} as "
            f"{mermaid_text(participant_label(agent_id))}"
        )

    for event in events:
        event_type = event.get("event_type")
        agent_id = event.get("agent_id")
        actor = participants.get(agent_id, "env")
        day = event.get("day")

        if event_type == "observation_received":
            observation = event.get("observation", {})
            incoming = {
                item.get("proposal_id")
                for item in observation.get("incoming_proposals", [])
            }
            targeted = [
                (proposal_id, proposal)
                for proposal_id, proposal in active_proposals.items()
                if proposal.get("counterparty") == agent_id
            ]
            allowed = ", ".join(event.get("allowed_actions", [])) or "none"
            if targeted:
                for proposal_id, _ in targeted:
                    visibility = (
                        f"includes {proposal_id}"
                        if proposal_id in incoming
                        else f"missing {proposal_id}"
                    )
                    arrow = "->>" if proposal_id in incoming else "-->>"
                    lines.append(
                        f"    env{arrow}{actor}: "
                        f"{mermaid_text(f'Day {day} Observation | {visibility} | Allowed {allowed}')}"
                    )
                    if (
                        proposal_id in invisible_expired
                        and proposal_id not in warned_missing
                        and proposal_id not in incoming
                    ):
                        lines.append(
                            f"    Note over env,{actor}: "
                            "WARNING - Proposal not visible to counterparty"
                        )
                        warned_missing.add(proposal_id)
            else:
                lines.append(
                    f"    env->>{actor}: "
                    f"{mermaid_text(f'Day {day} Observation | Incoming {len(incoming)} | Allowed {allowed}')}"
                )
        elif event_type == "decision_made":
            explanation = event.get("explanation") or event.get("reason")
            expected = normalize_outcome_type(event.get("expected_outcome"))
            decision_id = event.get("decision_id") or event.get(
                "correlation_id",
                "unknown",
            )
            lines.append(
                f"    Note right of {actor}: "
                f"{mermaid_text(f'Decision {decision_id} | Action {event.get("selected_action")} | Explanation {_short_text(explanation or "Not stated", limit=MAX_EXPLANATION_LENGTH)} | Expected {expected or "Not stated"}')}"
            )
        elif event_type == "action_executed":
            label = f"{event.get('action')} | Action {event.get('action_id') or 'unknown'}"
            if event.get("counterparty"):
                label += f" | To {event['counterparty']}"
            if event.get("lot_id"):
                label += f" | Lot {event['lot_id']}"
            if event.get("quantity") is not None:
                label += f" | Qty {event['quantity']}"
            if event.get("unit_price") is not None:
                label += f" | Price {event['unit_price']}"
            if event.get("proposal_id"):
                label += f" | Proposal {event['proposal_id']}"
            lines.append(f"    {actor}->>env: {mermaid_text(label)}")
            if (
                event.get("action") == "propose_trade"
                and event.get("status") == "success"
                and event.get("proposal_id")
            ):
                active_proposals[event["proposal_id"]] = event
                lines.append(
                    f"    Note over env: Proposal "
                    f"{mermaid_text(event['proposal_id'])} created"
                )
        elif event_type == "outcome_observed":
            actual = normalize_outcome_type(
                event.get("actual_outcome", event.get("outcome"))
            )
            if actual is None:
                continue
            proposal_id = event.get("proposal_id")
            expected = expected_by_decision.get(event.get("decision_id"))
            if expected is None and proposal_id:
                expected = expected_by_proposal.get(proposal_id)
            if expected and actual != "proposal_pending" and expected != actual:
                text = f"WARNING - Expected {expected} / Actual {actual}"
            else:
                text = f"Actual {actual}"
            lines.append(f"    Note over {actor},env: {mermaid_text(text)}")
            if proposal_id and actual.startswith("proposal_"):
                lines.append(
                    f"    Note over env: Proposal "
                    f"{mermaid_text(proposal_id)} "
                    f"{mermaid_text(actual.removeprefix('proposal_'))}"
                )
            if actual in {
                "proposal_accepted",
                "proposal_rejected",
                "proposal_expired",
            }:
                active_proposals.pop(proposal_id, None)
    return "\n".join(lines)


def build_sequence_diagram(
    events: list[dict[str, Any]],
    *,
    view: SequenceView = "business",
) -> str:
    ordered = sort_events(events)
    if view == "technical":
        return _build_technical_sequence_diagram(ordered)
    if view != "business":
        raise ValueError(f"unknown sequence view: {view}")
    return _build_business_sequence_diagram(ordered)


def _id_mapping_text(events: list[dict[str, Any]]) -> str:
    ordered = sort_events(events)
    display = _display_ids(ordered)
    links = _link_actions_to_decisions(ordered)
    lines = []
    for index, source in display.decision_source.items():
        source_name = (
            "decision_id"
            if ordered[index].get("decision_id")
            else "correlation_id (legacy)"
        )
        lines.append(
            f"{display.decisions[index]} = {source_name}: {source}"
        )
    for index, source in display.action_source.items():
        if ordered[index].get("action_id"):
            lines.append(
                f"{display.actions[index]} = action_id: {source} | "
                f"action: {ordered[index].get('action')}"
            )
        elif links.get(index) is None:
            lines.append(
                f"{display.actions[index]} = unlinked action event | "
                f"action: {ordered[index].get('action')}"
            )
    lines.extend(
        f"{display_id} = proposal_id: {source}"
        for source, display_id in display.proposals.items()
    )
    return "\n".join(lines) or "No audit IDs in the selected events."


def build_sequence_html(
    events: list[dict[str, Any]],
    *,
    mermaid_source: str,
    filters: dict[str, Any] | None = None,
    view: SequenceView = "business",
    generated_at: datetime | None = None,
) -> str:
    generated_at = generated_at or datetime.now(timezone.utc)
    run_id = next(
        (event.get("run_id") for event in events if event.get("run_id")),
        "unknown",
    )
    days = sorted(
        {event["day"] for event in events if event.get("day") is not None}
    )
    filter_text = ", ".join(
        f"{key}={value}"
        for key, value in (filters or {}).items()
        if value is not None
    ) or "none"
    metadata = {
        "Run ID": run_id,
        "View": view.title(),
        "Days": ", ".join(map(str, days)) or "none",
        "Events": len(events),
        "Filters": filter_text,
        "Generated": generated_at.isoformat(),
    }
    metadata_html = "".join(
        f"<div><span>{html.escape(label)}</span><strong>{html.escape(str(value))}</strong></div>"
        for label, value in metadata.items()
    )
    source = html.escape(mermaid_source)
    id_mapping = html.escape(_id_mapping_text(events))
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>CoffeeBench Agent Audit Sequence</title>
  <style>
    :root {{ --ink: #183028; --paper: #f5f0e6; --accent: #b64b2a; --line: #d7c9b5; }}
    body {{ margin: 0; color: var(--ink); background:
      radial-gradient(circle at 15% 10%, #f3c98b55, transparent 30%),
      linear-gradient(145deg, #fbf8f0, var(--paper)); font-family: Georgia, serif; }}
    main {{ max-width: 1500px; margin: 0 auto; padding: 40px 24px 72px; }}
    h1 {{ margin: 0 0 8px; font-size: clamp(28px, 5vw, 56px); letter-spacing: -0.04em; }}
    .eyebrow {{ color: var(--accent); font: 700 12px/1.2 ui-monospace, monospace;
      text-transform: uppercase; letter-spacing: .18em; }}
    .meta {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
      gap: 1px; margin: 24px 0; border: 1px solid var(--line); background: var(--line); }}
    .meta div {{ background: #fffaf1; padding: 14px; }}
    .meta span {{ display: block; color: #735f4d; font: 11px ui-monospace, monospace;
      text-transform: uppercase; margin-bottom: 6px; }}
    .diagram {{ overflow: auto; padding: 24px; background: #fffdf8;
      border: 1px solid var(--line); box-shadow: 0 18px 50px #5e41251a; }}
    details {{ margin-top: 20px; border-top: 1px solid var(--line); padding-top: 16px; }}
    summary {{ cursor: pointer; color: var(--accent); font-weight: 700; }}
    pre.source {{ white-space: pre-wrap; overflow-wrap: anywhere; padding: 18px;
      background: #182b25; color: #f5ead7; font: 12px/1.55 ui-monospace, monospace; }}
  </style>
</head>
<body>
  <main>
    <div class="eyebrow">CoffeeBench / Agent Ledger</div>
    <h1>Agent Audit Sequence</h1>
    <section class="meta">{metadata_html}</section>
    <section class="diagram"><pre class="mermaid">{source}</pre></section>
    <details><summary>Audit ID mapping</summary><pre class="source">{id_mapping}</pre></details>
    <details><summary>View Mermaid source</summary><pre class="source">{source}</pre></details>
  </main>
  <script type="module">
    import mermaid from "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs";
    mermaid.initialize({{ startOnLoad: true, theme: "base", securityLevel: "strict",
      themeVariables: {{ primaryColor: "#fff4df", primaryTextColor: "#183028",
      primaryBorderColor: "#b64b2a", lineColor: "#715b47", noteBkgColor: "#f9dba7",
      noteTextColor: "#183028", actorBkg: "#e8efe5", actorBorder: "#426b5a" }} }});
  </script>
</body>
</html>
"""


def write_sequence_output(
    output_path: str | Path,
    *,
    events: list[dict[str, Any]],
    mermaid_source: str,
    filters: dict[str, Any] | None = None,
    view: SequenceView = "business",
) -> Path:
    path = Path(output_path)
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.suffix.lower() in {".mmd", ".mermaid"}:
        content = mermaid_source + "\n"
    else:
        content = build_sequence_html(
            events,
            mermaid_source=mermaid_source,
            filters=filters,
            view=view,
        )
    path.write_text(content, encoding="utf-8")
    return path
