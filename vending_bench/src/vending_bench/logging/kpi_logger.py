"""
Logger for KPI compliance tracking (Phase 2).
"""

from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.governance.kpi import KPIMetrics


class KPILogger:
    """
    Logs KPI metrics and compliance.
    Output format: JSONL (one JSON object per line).
    """

    def __init__(self, output_file: str | Path):
        """
        Initialize KPI logger.

        Args:
            output_file: Path to output JSONL file
        """
        self.output_file = Path(output_file)
        self.output_file.parent.mkdir(parents=True, exist_ok=True)

        # Create/clear file
        with open(self.output_file, "w") as f:
            pass

    def log_daily_kpi(
        self,
        day: int,
        date: str,
        metrics: KPIMetrics,
    ) -> None:
        """
        Log daily KPI metrics.

        Args:
            day: Current simulation day
            date: Current date (ISO format)
            metrics: KPI metrics for the day
        """
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "day": day,
            "date": date,
            "kpi_metrics": metrics.to_dict(),
        }

        self._write_log(log_entry)

    def log_kpi_violation(
        self,
        day: int,
        violation_type: str,
        description: str,
        severity: str = "medium",
    ) -> None:
        """
        Log a KPI violation.

        Args:
            day: Current simulation day
            violation_type: Type of violation
            description: Description of the violation
            severity: Severity level (low/medium/high)
        """
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "day": day,
            "type": "violation",
            "violation_type": violation_type,
            "description": description,
            "severity": severity,
        }

        self._write_log(log_entry)

    def _write_log(self, log_entry: dict[str, Any]) -> None:
        """Write a log entry to file."""
        with open(self.output_file, "a") as f:
            f.write(json.dumps(log_entry) + "\n")
