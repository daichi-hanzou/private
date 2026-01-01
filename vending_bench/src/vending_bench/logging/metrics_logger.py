"""
Metrics logger for Vending-Bench.
Records daily metrics for analysis and visualization.
"""

from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.environment.state import DailyMetrics


class MetricsLogger:
    """
    Logs daily metrics for performance tracking.

    Creates a JSONL file with one entry per day, including:
    - Units sold
    - Revenue
    - Inventory status
    - Net worth
    - Tool usage
    """

    def __init__(self, output_path: Path | str | None = None) -> None:
        self.output_path = Path(output_path) if output_path else None
        self.entries: list[dict[str, Any]] = []
        self._file_handle = None

    def start(self) -> None:
        """Start logging."""
        if self.output_path:
            self.output_path.parent.mkdir(parents=True, exist_ok=True)
            self._file_handle = open(self.output_path, "w")

    def stop(self) -> None:
        """Stop logging."""
        if self._file_handle:
            self._file_handle.close()
            self._file_handle = None

    def log_daily_metrics(self, metrics: DailyMetrics) -> None:
        """Log daily metrics."""
        entry = metrics.to_dict()
        entry["timestamp_real"] = datetime.now().isoformat()

        self.entries.append(entry)
        self._write_entry(entry)

    def log_snapshot(
        self,
        day_number: int,
        data: dict[str, Any],
    ) -> None:
        """Log a custom snapshot."""
        entry = {
            "day_number": day_number,
            "timestamp_real": datetime.now().isoformat(),
            **data,
        }

        self.entries.append(entry)
        self._write_entry(entry)

    def _write_entry(self, entry: dict[str, Any]) -> None:
        """Write entry to file."""
        if self._file_handle:
            self._file_handle.write(json.dumps(entry) + "\n")
            self._file_handle.flush()

    def get_metrics_by_day(self, day_number: int) -> dict[str, Any] | None:
        """Get metrics for a specific day."""
        for entry in self.entries:
            if entry.get("day_number") == day_number:
                return entry
        return None

    def get_time_series(self, metric_name: str) -> list[tuple[int, float]]:
        """
        Get time series data for a specific metric.

        Returns list of (day_number, value) tuples.
        """
        series = []
        for entry in self.entries:
            if metric_name in entry:
                series.append((entry["day_number"], entry[metric_name]))
        return series

    def get_summary_statistics(self) -> dict[str, Any]:
        """Calculate summary statistics from all entries."""
        if not self.entries:
            return {}

        # Extract metrics
        net_worths = [e["net_worth"] for e in self.entries if "net_worth" in e]
        units_sold = [e["units_sold"] for e in self.entries if "units_sold" in e]
        revenues = [e["revenue"] for e in self.entries if "revenue" in e]

        def safe_stats(values: list[float]) -> dict[str, float]:
            if not values:
                return {"min": 0, "max": 0, "mean": 0, "total": 0}
            return {
                "min": min(values),
                "max": max(values),
                "mean": sum(values) / len(values),
                "total": sum(values),
            }

        return {
            "days_recorded": len(self.entries),
            "net_worth": safe_stats(net_worths),
            "units_sold": safe_stats(units_sold),
            "revenue": safe_stats(revenues),
        }

    def to_dataframe(self) -> Any:
        """
        Convert to pandas DataFrame if pandas is available.

        Returns None if pandas is not installed.
        """
        try:
            import pandas as pd
            return pd.DataFrame(self.entries)
        except ImportError:
            return None

    def save(self, path: Path | str) -> None:
        """Save all entries to a file."""
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w") as f:
            for entry in self.entries:
                f.write(json.dumps(entry) + "\n")

    def export_csv(self, path: Path | str) -> None:
        """Export to CSV format."""
        df = self.to_dataframe()
        if df is not None:
            df.to_csv(path, index=False)
        else:
            # Manual CSV export
            path = Path(path)
            if not self.entries:
                return

            keys = list(self.entries[0].keys())
            with open(path, "w") as f:
                f.write(",".join(keys) + "\n")
                for entry in self.entries:
                    values = [str(entry.get(k, "")) for k in keys]
                    f.write(",".join(values) + "\n")
