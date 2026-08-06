from __future__ import annotations

import hashlib
import json
import subprocess
from typing import Any

from .models import CalendarRequest, CalendarResult


class CalendarAgent:
    """A local rule-based agent that emits AgentLedger-compatible events."""

    version = "0.1.0"
    tool_version = "local-calendar-tool-0.1.0"

    def run(self, request: CalendarRequest) -> list[dict[str, Any]]:
        return self.propose(request)

    def propose(self, request: CalendarRequest) -> list[dict[str, Any]]:
        identifiers = self._identifiers(request)
        events = [self._observation(request, identifiers)]
        selected_action = (
            "create_calendar_event"
            if request.available_slots
            else "request_clarification"
        )
        events.append(self._decision(request, identifiers, selected_action))
        result = self._execute(request, identifiers, selected_action)
        events.append(
            self._action(request, identifiers, selected_action, result)
        )
        if not request.requires_approval or result is None:
            events.append(
                self._outcome(
                    request,
                    identifiers,
                    selected_action,
                    result,
                )
            )
        return events

    def resolve(
        self,
        events: list[dict[str, Any]],
        *,
        action_id: str,
        resolution: str,
        actor: str,
        reason: str,
    ) -> list[dict[str, Any]]:
        if resolution not in {"approve", "reject"}:
            raise ValueError(f"unsupported resolution: {resolution}")
        matches = [
            event
            for event in events
            if event.get("event_type") == "action_executed"
            and event.get("action_id") == action_id
        ]
        if not matches:
            raise ValueError(f"action not found: {action_id}")
        if len(matches) > 1:
            raise ValueError(f"multiple actions found: {action_id}")
        if any(
            event.get("event_type") == "outcome_observed"
            and event.get("action_id") == action_id
            for event in events
        ):
            raise ValueError(f"action already has an outcome: {action_id}")
        if any(
            event.get("event_type") == "human_intervention"
            and (
                event.get("related_action_id") == action_id
                or event.get("action_id") == action_id
            )
            for event in events
        ):
            raise ValueError(
                f"action already has a human intervention: {action_id}"
            )
        action = matches[0]
        required = {
            "run_id",
            "correlation_id",
            "decision_id",
            "action_id",
        }
        missing = sorted(key for key in required if not action.get(key))
        if missing:
            raise ValueError(
                "action is missing required IDs: " + ", ".join(missing)
            )
        appended = self._resolution_events(
            action,
            resolution=resolution,
            actor=actor,
            reason=reason,
        )
        existing_ids = {
            str(event.get("event_id"))
            for event in events
            if event.get("event_id")
        }
        if any(event["event_id"] in existing_ids for event in appended):
            raise ValueError("resolution event ID already exists")
        return [*events, *appended]

    def _identifiers(self, request: CalendarRequest) -> dict[str, str]:
        value = json.dumps(
            {
                "title": request.title,
                "requested_date": request.requested_date,
                "preferred_period": request.preferred_period,
                "duration_minutes": request.duration_minutes,
                "available_slots": request.available_slots,
                "requires_approval": request.requires_approval,
            },
            sort_keys=True,
            separators=(",", ":"),
        )
        digest = hashlib.sha256(value.encode()).hexdigest()[:12]
        return {
            "run_id": f"calendar-run-{digest}",
            "correlation_id": f"calendar-request-{digest}",
            "decision_id": f"calendar-decision-{digest}",
            "action_id": f"calendar-action-{digest}",
            "calendar_event_id": f"calendar-event-{digest}",
        }

    def _base(
        self,
        event_type: str,
        event_name: str,
        identifiers: dict[str, str],
    ) -> dict[str, Any]:
        return {
            "schema_version": "0.1",
            "event_id": f"{event_name}-{identifiers['run_id']}",
            "event_type": event_type,
            "run_id": identifiers["run_id"],
            "agent_id": "calendar_agent",
            "correlation_id": identifiers["correlation_id"],
        }

    def _observation(
        self,
        request: CalendarRequest,
        identifiers: dict[str, str],
    ) -> dict[str, Any]:
        event = self._base(
            "observation_received", "observation", identifiers
        )
        event.update(
            {
                "observation": {
                    "requested_title": request.title,
                    "requested_date": request.requested_date,
                    "preferred_period": request.preferred_period,
                    "duration_minutes": request.duration_minutes,
                    "available_slots": request.available_slots,
                    "requires_approval": request.requires_approval,
                },
                "allowed_actions": [
                    "create_calendar_event",
                    "request_clarification",
                ],
            }
        )
        return event

    def _decision(
        self,
        request: CalendarRequest,
        identifiers: dict[str, str],
        selected_action: str,
    ) -> dict[str, Any]:
        event = self._base("decision_made", "decision", identifiers)
        if selected_action == "create_calendar_event":
            explanation = (
                "Selected the earliest available time slot: "
                f"{min(request.available_slots)}."
            )
            expected = (
                "approval_required"
                if request.requires_approval
                else "event_created"
            )
        else:
            explanation = (
                "No available time slots were provided; additional "
                "information is required."
            )
            expected = "clarification_requested"
        event.update(
            {
                "decision_id": identifiers["decision_id"],
                "selected_action": selected_action,
                "explanation": explanation,
                "expected_outcome": {"outcome_type": expected},
            }
        )
        return event

    def _execute(
        self,
        request: CalendarRequest,
        identifiers: dict[str, str],
        selected_action: str,
    ) -> CalendarResult | None:
        if selected_action != "create_calendar_event":
            return None
        return CalendarResult(
            event_id=identifiers["calendar_event_id"],
            title=request.title,
            start=self._calendar_start(
                request.requested_date,
                min(request.available_slots),
            ),
            duration_minutes=request.duration_minutes,
        )

    @staticmethod
    def _calendar_start(requested_date: str, slot: str) -> str:
        if "T" in slot or slot.startswith(requested_date):
            return slot
        return f"{requested_date}T{slot}"

    def _action(
        self,
        request: CalendarRequest,
        identifiers: dict[str, str],
        selected_action: str,
        result: CalendarResult | None,
    ) -> dict[str, Any]:
        event = self._base("action_executed", "action", identifiers)
        parameters: dict[str, Any] = {}
        if result is not None:
            parameters = {
                "title": result.title,
                "start": result.start,
                "duration_minutes": result.duration_minutes,
            }
        event.update(
            {
                "decision_id": identifiers["decision_id"],
                "action_id": identifiers["action_id"],
                "action": selected_action,
                "action_parameters": parameters,
                "status": (
                    "awaiting_approval"
                    if request.requires_approval and result is not None
                    else "success"
                ),
                "execution_context": self._execution_context(),
            }
        )
        if result is not None:
            event["metadata"] = {"calendar_event_id": result.event_id}
        return event

    def _execution_context(self) -> dict[str, Any]:
        context: dict[str, Any] = {
            "model_name": "rule-based-calendar-agent",
            "model_version": self.version,
            "tool_version": self.tool_version,
            "config_hash": "sha256:"
            + hashlib.sha256(
                b"calendar-agent-rules-v0.1.0"
            ).hexdigest(),
            "environment": {
                "runtime": "local",
                "external_services": False,
            },
        }
        commit = self._git_commit()
        if commit:
            context["git_commit"] = commit
        return context

    @staticmethod
    def _git_commit() -> str | None:
        try:
            result = subprocess.run(
                ["git", "rev-parse", "HEAD"],
                capture_output=True,
                check=True,
                text=True,
                timeout=1,
            )
        except (OSError, subprocess.SubprocessError):
            return None
        return result.stdout.strip() or None

    def _resolution_events(
        self,
        action: dict[str, Any],
        *,
        resolution: str,
        actor: str,
        reason: str,
    ) -> list[dict[str, Any]]:
        accepted = resolution == "approve"
        suffix = "approved" if accepted else "rejected"
        shared = {
            "schema_version": "0.1",
            "run_id": action["run_id"],
            "agent_id": "calendar_agent",
            "correlation_id": action["correlation_id"],
            "decision_id": action["decision_id"],
            "action_id": action["action_id"],
        }
        intervention = {
            **shared,
            "event_id": (
                f"human-intervention-{suffix}-{action['action_id']}"
            ),
            "event_type": "human_intervention",
            "related_action_id": action["action_id"],
            "intervention_type": "accept" if accepted else "reject",
            "actor": actor,
            "reason": reason,
            "before": {"status": "awaiting_approval"},
            "after": {"status": suffix},
            "input_method": "calendar_agent_cli",
        }
        parameters = action.get("action_parameters")
        parameters = parameters if isinstance(parameters, dict) else {}
        metadata = action.get("metadata")
        metadata = metadata if isinstance(metadata, dict) else {}
        if accepted:
            actual_outcome = {
                "outcome_type": "calendar_event_created",
                "event_id": metadata.get("calendar_event_id"),
                "title": parameters.get("title"),
                "start": parameters.get("start"),
                "duration_minutes": parameters.get("duration_minutes"),
            }
        else:
            actual_outcome = {
                "outcome_type": "calendar_event_rejected"
            }
        outcome = {
            **shared,
            "event_id": f"outcome-{suffix}-{action['action_id']}",
            "event_type": "outcome_observed",
            "status": "confirmed" if accepted else "contradicted",
            "actual_outcome": actual_outcome,
        }
        return [intervention, outcome]

    def _outcome(
        self,
        request: CalendarRequest,
        identifiers: dict[str, str],
        selected_action: str,
        result: CalendarResult | None,
    ) -> dict[str, Any]:
        event = self._base("outcome_observed", "outcome", identifiers)
        actual_outcome: dict[str, Any]
        if result is None:
            actual_outcome = {"outcome_type": "clarification_requested"}
        else:
            actual_outcome = {
                "outcome_type": "calendar_event_created",
                "event_id": result.event_id,
                "title": result.title,
                "start": result.start,
                "duration_minutes": result.duration_minutes,
            }
        event.update(
            {
                "decision_id": identifiers["decision_id"],
                "action_id": identifiers["action_id"],
                "status": "confirmed",
                "actual_outcome": actual_outcome,
            }
        )
        return event
