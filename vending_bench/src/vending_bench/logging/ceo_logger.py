"""
Logger for CEO actions and reviews (Phase 2).
"""

from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.governance.ceo_agent import CEOReview


class CEOActionLogger:
    """
    Logs CEO review actions and decisions.
    Output format: JSONL (one JSON object per line).
    """

    def __init__(self, output_file: str | Path):
        """
        Initialize CEO action logger.

        Args:
            output_file: Path to output JSONL file
        """
        self.output_file = Path(output_file)
        self.output_file.parent.mkdir(parents=True, exist_ok=True)

        # Create/clear file
        with open(self.output_file, "w") as f:
            pass

    def log_review(
        self,
        day: int,
        action_type: str,
        operator_proposal: dict[str, Any],
        review: CEOReview,
    ) -> None:
        """
        Log a CEO review action.

        Args:
            day: Current simulation day
            action_type: Type of action being reviewed
            operator_proposal: Operator's proposed action
            review: CEO's review result
        """
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "day": day,
            "action_type": action_type,
            "operator_proposal": operator_proposal,
            "decision": review.decision.value,
            "reason": review.reason,
            "suggested_changes": review.suggested_changes,
        }

        self._write_log(log_entry)

    def log_intervention(
        self,
        day: int,
        intervention_type: str,
        reason: str,
        actions_taken: list[str],
    ) -> None:
        """
        Log a CEO intervention.

        Args:
            day: Current simulation day
            intervention_type: Type of intervention
            reason: Reason for intervention
            actions_taken: List of actions taken
        """
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "day": day,
            "intervention_type": intervention_type,
            "reason": reason,
            "actions_taken": actions_taken,
        }

        self._write_log(log_entry)

    def _write_log(self, log_entry: dict[str, Any]) -> None:
        """Write a log entry to file."""
        with open(self.output_file, "a") as f:
            f.write(json.dumps(log_entry) + "\n")
