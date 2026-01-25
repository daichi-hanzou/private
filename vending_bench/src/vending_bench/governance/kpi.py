"""
KPI (Key Performance Indicators) system for CEO governance.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.config import CEOKPIConfig


@dataclass
class CEOKPIs:
    """
    Key Performance Indicators for CEO governance.
    These define the business rules and constraints for the vending machine operation.
    """

    # Profit targets
    target_daily_profit: float = 10.0  # Target daily profit in dollars
    min_margin_rate: float = 0.30  # Minimum profit margin (30%)

    # Pricing policies
    max_discount_rate: float = 0.20  # Maximum discount rate (20%)
    min_price_multiplier: float = 1.05  # Price must be at least cost × 1.05

    # Inventory management
    inventory_turnover_target: int = 7  # Target inventory turnover in days
    max_inventory_value: float = 300.0  # Maximum total inventory value

    # Category restrictions
    prohibited_categories: list[str] = field(
        default_factory=lambda: [
            "alcohol",
            "tobacco",
            "medicine",
            "perishable_food",
            "electronics",
        ]
    )
    allowed_categories: list[str] = field(
        default_factory=lambda: [
            "beverage",
            "snack",
            "candy",
            "energy_drink",
        ]
    )

    # Risk management
    max_single_order_value: float = 150.0  # Maximum value for a single order
    min_cash_reserve: float = 50.0  # Minimum cash reserve to maintain

    @classmethod
    def from_config(cls, config: CEOKPIConfig) -> CEOKPIs:
        """Create KPIs from configuration."""
        return cls(
            target_daily_profit=config.target_daily_profit,
            min_margin_rate=config.min_margin_rate,
            max_discount_rate=config.max_discount_rate,
            min_price_multiplier=config.min_price_multiplier,
            inventory_turnover_target=config.inventory_turnover_target,
            max_inventory_value=config.max_inventory_value,
            prohibited_categories=config.prohibited_categories,
            allowed_categories=config.allowed_categories,
            max_single_order_value=config.max_single_order_value,
            min_cash_reserve=config.min_cash_reserve,
        )

    def check_price_compliant(self, price: float, cost: float) -> bool:
        """
        Check if a price meets minimum margin requirements.

        Args:
            price: Proposed selling price
            cost: Product cost

        Returns:
            True if price is compliant with KPIs
        """
        if cost == 0:
            return price > 0

        return price >= cost * self.min_price_multiplier

    def check_margin_rate(self, price: float, cost: float) -> float:
        """
        Calculate profit margin rate.

        Args:
            price: Selling price
            cost: Product cost

        Returns:
            Margin rate (0-1 scale)
        """
        if price == 0:
            return 0.0

        return (price - cost) / price

    def check_discount_rate(self, old_price: float, new_price: float) -> float:
        """
        Calculate discount rate.

        Args:
            old_price: Original price
            new_price: New price

        Returns:
            Discount rate (0-1 scale)
        """
        if old_price == 0:
            return 0.0

        return (old_price - new_price) / old_price

    def check_category_allowed(self, category: str) -> bool:
        """
        Check if a product category is allowed.

        Args:
            category: Product category

        Returns:
            True if category is allowed
        """
        category_lower = category.lower()

        # Check prohibited list
        if any(prohibited in category_lower for prohibited in self.prohibited_categories):
            return False

        # If allowed list is specified, check it
        if self.allowed_categories:
            return any(allowed in category_lower for allowed in self.allowed_categories)

        return True

    def calculate_compliance_score(self, metrics: dict[str, float]) -> float:
        """
        Calculate overall KPI compliance score (0-1).

        Args:
            metrics: Dictionary of performance metrics

        Returns:
            Compliance score from 0 (worst) to 1 (best)
        """
        scores = []

        # Profit compliance
        if "daily_profit" in metrics:
            profit_score = min(1.0, metrics["daily_profit"] / self.target_daily_profit)
            scores.append(max(0.0, profit_score))

        # Margin compliance
        if "margin_rate" in metrics:
            margin_score = 1.0 if metrics["margin_rate"] >= self.min_margin_rate else 0.5
            scores.append(margin_score)

        # Inventory compliance
        if "inventory_value" in metrics:
            if metrics["inventory_value"] <= self.max_inventory_value:
                scores.append(1.0)
            else:
                excess_ratio = metrics["inventory_value"] / self.max_inventory_value
                scores.append(max(0.0, 2.0 - excess_ratio))

        # Cash reserve compliance
        if "cash_balance" in metrics:
            if metrics["cash_balance"] >= self.min_cash_reserve:
                scores.append(1.0)
            else:
                reserve_ratio = metrics["cash_balance"] / self.min_cash_reserve
                scores.append(max(0.0, reserve_ratio))

        return sum(scores) / len(scores) if scores else 1.0

    def to_dict(self) -> dict[str, Any]:
        """Convert KPIs to dictionary."""
        return {
            "target_daily_profit": self.target_daily_profit,
            "min_margin_rate": self.min_margin_rate,
            "max_discount_rate": self.max_discount_rate,
            "min_price_multiplier": self.min_price_multiplier,
            "inventory_turnover_target": self.inventory_turnover_target,
            "max_inventory_value": self.max_inventory_value,
            "prohibited_categories": self.prohibited_categories,
            "allowed_categories": self.allowed_categories,
            "max_single_order_value": self.max_single_order_value,
            "min_cash_reserve": self.min_cash_reserve,
        }


@dataclass
class KPIMetrics:
    """Daily KPI metrics for tracking compliance."""

    day: int
    daily_profit: float
    margin_rate: float
    inventory_value: float
    cash_balance: float
    compliance_score: float
    violations: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        """Convert metrics to dictionary for logging."""
        return {
            "day": self.day,
            "daily_profit": self.daily_profit,
            "margin_rate": self.margin_rate,
            "inventory_value": self.inventory_value,
            "cash_balance": self.cash_balance,
            "compliance_score": self.compliance_score,
            "violations": self.violations,
        }
