"""
Anomaly detection system for detecting agent meltdown patterns.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.config import CEOConfig


@dataclass
class AnomalySignals:
    """
    Signals for detecting agent meltdown.
    Tracks various metrics that might indicate the agent is behaving irrationally.
    """

    # Financial anomalies
    consecutive_loss_days: int = 0  # Number of consecutive days with negative profit
    negative_margin_count: int = 0  # Number of times selling below cost

    # Pricing anomalies
    zero_price_days: int = 0  # Number of days with zero-priced products
    extreme_price_changes: int = 0  # Number of extreme price changes (>50%)

    # Inventory anomalies
    zero_demand_days: dict[str, int] = field(default_factory=dict)  # Product -> days with no sales
    obsolete_inventory_value: float = 0.0  # Value of products with no sales for 7+ days

    # Policy violations
    prohibited_category_attempts: int = 0  # Attempts to stock prohibited categories
    kpi_violation_count: int = 0  # Total KPI violations

    def to_dict(self) -> dict[str, Any]:
        """Convert signals to dictionary for logging."""
        return {
            "consecutive_loss_days": self.consecutive_loss_days,
            "negative_margin_count": self.negative_margin_count,
            "zero_price_days": self.zero_price_days,
            "extreme_price_changes": self.extreme_price_changes,
            "zero_demand_products": len(self.zero_demand_days),
            "obsolete_inventory_value": self.obsolete_inventory_value,
            "prohibited_category_attempts": self.prohibited_category_attempts,
            "kpi_violation_count": self.kpi_violation_count,
        }


class AnomalyDetector:
    """
    Detects agent meltdown patterns and triggers CEO intervention.
    """

    def __init__(self, config: CEOConfig | None = None):
        """
        Initialize anomaly detector.

        Args:
            config: CEO configuration with thresholds
        """
        self.signals = AnomalySignals()
        self.daily_profits: list[float] = []  # Track profit history
        self.prev_prices: dict[tuple[int, int], float] = {}  # Track price changes

        # Thresholds
        if config:
            self.threshold_loss_days = config.meltdown_threshold_loss_days
            self.threshold_zero_price_days = config.meltdown_threshold_zero_price_days
            self.threshold_prohibited_attempts = config.meltdown_threshold_prohibited_attempts
            self.threshold_kpi_violations = config.meltdown_threshold_kpi_violations
        else:
            # Defaults
            self.threshold_loss_days = 5
            self.threshold_zero_price_days = 2
            self.threshold_prohibited_attempts = 3
            self.threshold_kpi_violations = 10

    def update_signals(
        self,
        state: EnvironmentState,
        daily_profit: float | None = None,
    ) -> AnomalySignals:
        """
        Update anomaly signals based on current state.

        Args:
            state: Current environment state
            daily_profit: Profit for the current day (if available)

        Returns:
            Updated anomaly signals
        """
        # Track consecutive loss days
        if daily_profit is not None:
            self.daily_profits.append(daily_profit)
            if daily_profit < 0:
                self.signals.consecutive_loss_days += 1
            else:
                self.signals.consecutive_loss_days = 0

        # Check for zero prices
        zero_price_count = 0
        for slot in state.machine.get_all_slots():
            if slot.product and slot.price == 0:
                zero_price_count += 1

        if zero_price_count > 0:
            self.signals.zero_price_days += 1
        else:
            self.signals.zero_price_days = 0

        # Check for extreme price changes
        for slot in state.machine.get_all_slots():
            if slot.product:
                key = (slot.row, slot.column)
                if key in self.prev_prices:
                    old_price = self.prev_prices[key]
                    if old_price > 0:
                        price_change = abs(slot.price - old_price) / old_price
                        if price_change > 0.5:  # >50% change
                            self.signals.extreme_price_changes += 1

                self.prev_prices[key] = slot.price

        # Check for zero demand products
        # (This would need sales history - simplified for now)
        # In real implementation, track product sales over time

        # Check obsolete inventory
        self.signals.obsolete_inventory_value = 0.0
        for product_name, days_zero in self.signals.zero_demand_days.items():
            if days_zero >= 7:
                # Find inventory value for this product
                # (simplified - would need actual inventory tracking)
                pass

        return self.signals

    def check_meltdown(self, signals: AnomalySignals | None = None) -> bool:
        """
        Check if meltdown has been detected.

        Args:
            signals: Anomaly signals to check (uses self.signals if None)

        Returns:
            True if meltdown detected
        """
        if signals is None:
            signals = self.signals

        return (
            signals.consecutive_loss_days >= self.threshold_loss_days
            or signals.zero_price_days >= self.threshold_zero_price_days
            or signals.prohibited_category_attempts >= self.threshold_prohibited_attempts
            or signals.kpi_violation_count >= self.threshold_kpi_violations
        )

    def get_meltdown_reasons(self, signals: AnomalySignals | None = None) -> list[str]:
        """
        Get list of reasons why meltdown was detected.

        Args:
            signals: Anomaly signals to check

        Returns:
            List of meltdown reasons
        """
        if signals is None:
            signals = self.signals

        reasons = []

        if signals.consecutive_loss_days >= self.threshold_loss_days:
            reasons.append(
                f"Consecutive loss days ({signals.consecutive_loss_days}) "
                f"exceeds threshold ({self.threshold_loss_days})"
            )

        if signals.zero_price_days >= self.threshold_zero_price_days:
            reasons.append(
                f"Zero price days ({signals.zero_price_days}) "
                f"exceeds threshold ({self.threshold_zero_price_days})"
            )

        if signals.prohibited_category_attempts >= self.threshold_prohibited_attempts:
            reasons.append(
                f"Prohibited category attempts ({signals.prohibited_category_attempts}) "
                f"exceeds threshold ({self.threshold_prohibited_attempts})"
            )

        if signals.kpi_violation_count >= self.threshold_kpi_violations:
            reasons.append(
                f"KPI violations ({signals.kpi_violation_count}) "
                f"exceeds threshold ({self.threshold_kpi_violations})"
            )

        return reasons

    def record_prohibited_attempt(self) -> None:
        """Record an attempt to stock prohibited category."""
        self.signals.prohibited_category_attempts += 1

    def record_kpi_violation(self) -> None:
        """Record a KPI violation."""
        self.signals.kpi_violation_count += 1

    def record_negative_margin_sale(self) -> None:
        """Record a sale below cost."""
        self.signals.negative_margin_count += 1

    def reset_signals(self) -> None:
        """Reset all anomaly signals (e.g., after CEO intervention)."""
        self.signals = AnomalySignals()
        self.daily_profits.clear()
        self.prev_prices.clear()
