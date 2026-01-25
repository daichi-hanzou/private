"""
Logger for anomaly detections (Phase 2).
"""

from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.governance.anomaly_detector import AnomalySignals


class AnomalyLogger:
    """
    Logs anomaly detections and signals.
    Output format: JSONL (one JSON object per line).
    """

    def __init__(self, output_file: str | Path):
        """
        Initialize anomaly logger.

        Args:
            output_file: Path to output JSONL file
        """
        self.output_file = Path(output_file)
        self.output_file.parent.mkdir(parents=True, exist_ok=True)

        # Create/clear file
        with open(self.output_file, "w") as f:
            pass

    def log_anomaly_detection(
        self,
        day: int,
        signals: AnomalySignals,
        meltdown_detected: bool,
        meltdown_reasons: list[str],
        intervention_triggered: bool,
    ) -> None:
        """
        Log an anomaly detection event.

        Args:
            day: Current simulation day
            signals: Current anomaly signals
            meltdown_detected: Whether meltdown was detected
            meltdown_reasons: Reasons for meltdown (if detected)
            intervention_triggered: Whether CEO intervention was triggered
        """
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "day": day,
            "signals": signals.to_dict(),
            "meltdown_detected": meltdown_detected,
            "meltdown_reasons": meltdown_reasons,
            "intervention_triggered": intervention_triggered,
        }

        self._write_log(log_entry)

    def log_daily_signals(
        self,
        day: int,
        signals: AnomalySignals,
    ) -> None:
        """
        Log daily anomaly signals (for tracking trends).

        Args:
            day: Current simulation day
            signals: Current anomaly signals
        """
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "day": day,
            "type": "daily_signals",
            "signals": signals.to_dict(),
        }

        self._write_log(log_entry)

    def _write_log(self, log_entry: dict[str, Any]) -> None:
        """Write a log entry to file."""
        with open(self.output_file, "a") as f:
            f.write(json.dumps(log_entry) + "\n")
