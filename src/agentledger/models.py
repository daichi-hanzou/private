from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any


@dataclass(frozen=True)
class NormalizedAuditEvent:
    event_id: str
    event_type: str
    run_id: str | None = None
    timestamp: datetime | None = None
    action_time: datetime | None = None
    observed_at: datetime | None = None
    day: int | None = None
    actor_id: str | None = None
    actor_name: str | None = None
    target_id: str | None = None
    target_name: str | None = None
    decision_id: str | None = None
    action_id: str | None = None
    correlation_id: str | None = None
    case_type: str | None = None
    case_id: str | None = None
    action_type: str | None = None
    action_parameters: dict[str, Any] = field(default_factory=dict)
    explanation: str | None = None
    expected_outcome: str | None = None
    actual_outcome: Any = None
    status: str | None = None
    summary: str = ""
    raw_event: dict[str, Any] = field(default_factory=dict)
    source_line: int | None = None


@dataclass(frozen=True)
class HumanInterventionEvent:
    event_id: str
    related_action_id: str
    intervention_type: str
    performed_at: datetime | None = None
    actor: str = "human"
    before: Any = None
    after: Any = None
    reason: str | None = None
    input_method: str | None = None
    raw_event: dict[str, Any] = field(default_factory=dict)


@dataclass(frozen=True)
class ExecutionContext:
    model_name: str | None = None
    model_version: str | None = None
    prompt_hash: str | None = None
    tool_version: str | None = None
    config_hash: str | None = None
    git_commit: str | None = None
    environment: Any = None

    @classmethod
    def from_value(cls, value: Any) -> ExecutionContext:
        if not isinstance(value, dict):
            return cls()
        return cls(
            model_name=value.get("model_name"),
            model_version=value.get("model_version"),
            prompt_hash=value.get("prompt_hash"),
            tool_version=value.get("tool_version"),
            config_hash=value.get("config_hash"),
            git_commit=value.get("git_commit"),
            environment=value.get("environment"),
        )

    def as_dict(self) -> dict[str, Any]:
        return {
            "model_name": self.model_name,
            "model_version": self.model_version,
            "prompt_hash": self.prompt_hash,
            "tool_version": self.tool_version,
            "config_hash": self.config_hash,
            "git_commit": self.git_commit,
            "environment": self.environment,
        }

    @property
    def is_empty(self) -> bool:
        return not any(value is not None for value in self.as_dict().values())


@dataclass(frozen=True)
class IngestionIssue:
    line: int
    message: str


@dataclass(frozen=True)
class IngestionResult:
    events: list[dict[str, Any]]
    issues: list[IngestionIssue]
    empty_lines: int = 0

    @property
    def loaded(self) -> int:
        return len(self.events)

    @property
    def skipped(self) -> int:
        return len(self.issues)
