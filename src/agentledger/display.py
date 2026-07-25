from __future__ import annotations

from dataclasses import dataclass

from .models import NormalizedAuditEvent


@dataclass(frozen=True)
class DisplayIds:
    events: dict[str, str]
    decisions: dict[str, str]
    decision_correlations: dict[str, str]
    actions: dict[str, str]
    cases: dict[str, str]

    def related(self, event: NormalizedAuditEvent) -> list[str]:
        result = []
        if event.decision_id in self.decisions:
            result.append(self.decisions[event.decision_id])
        elif event.correlation_id in self.decision_correlations:
            result.append(
                self.decision_correlations[event.correlation_id]
            )
        if event.action_id in self.actions:
            result.append(self.actions[event.action_id])
        if event.case_id in self.cases:
            result.append(self.cases[event.case_id])
        return result


def assign_display_ids(
    events: list[NormalizedAuditEvent],
) -> DisplayIds:
    event_ids: dict[str, str] = {}
    decisions: dict[str, str] = {}
    decision_correlations: dict[str, str] = {}
    actions: dict[str, str] = {}
    cases: dict[str, str] = {}
    for event in events:
        event_ids.setdefault(event.event_id, f"E{len(event_ids) + 1}")
        decision_key = event.decision_id or (
            event.correlation_id
            if event.event_type == "decision_made"
            else None
        )
        if decision_key:
            display_id = decisions.setdefault(
                decision_key,
                f"D{len(decisions) + 1}",
            )
            if event.correlation_id:
                decision_correlations[event.correlation_id] = display_id
        if event.action_id and event.action_id not in decisions:
            actions.setdefault(event.action_id, f"A{len(actions) + 1}")
        if event.case_id:
            cases.setdefault(event.case_id, f"P{len(cases) + 1}")
    return DisplayIds(
        event_ids,
        decisions,
        decision_correlations,
        actions,
        cases,
    )
