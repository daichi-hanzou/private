"""
Scoring for Vending-Bench.
Calculates net worth and tracks performance metrics.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import date
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState, DailyMetrics


@dataclass
class RunResult:
    """Final result of a benchmark run."""

    # Primary score
    net_worth: float

    # Component breakdown
    money_balance: float
    machine_cash: float
    storage_value: float
    machine_inventory_value: float

    # Run statistics
    total_days: int
    total_messages: int
    total_tool_calls: int
    total_units_sold: int
    total_revenue: float

    # Termination info
    termination_reason: str
    bankrupt: bool

    # Performance tracking
    sales_stop_day: int | None
    last_sale_day: int
    days_until_sales_stop: int | None

    # Daily metrics
    daily_metrics: list[dict[str, Any]] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        return {
            "net_worth": self.net_worth,
            "money_balance": self.money_balance,
            "machine_cash": self.machine_cash,
            "storage_value": self.storage_value,
            "machine_inventory_value": self.machine_inventory_value,
            "total_days": self.total_days,
            "total_messages": self.total_messages,
            "total_tool_calls": self.total_tool_calls,
            "total_units_sold": self.total_units_sold,
            "total_revenue": self.total_revenue,
            "termination_reason": self.termination_reason,
            "bankrupt": self.bankrupt,
            "sales_stop_day": self.sales_stop_day,
            "last_sale_day": self.last_sale_day,
            "days_until_sales_stop": self.days_until_sales_stop,
        }

    def format_summary(self) -> str:
        """Format a human-readable summary."""
        lines = [
            "=" * 60,
            "VENDING-BENCH RUN RESULTS",
            "=" * 60,
            "",
            "PRIMARY SCORE",
            "-" * 40,
            f"  Net Worth: ${self.net_worth:.2f}",
            "",
            "SCORE BREAKDOWN",
            "-" * 40,
            f"  Money Balance:          ${self.money_balance:.2f}",
            f"  Cash in Machine:        ${self.machine_cash:.2f}",
            f"  Storage Inventory:      ${self.storage_value:.2f}",
            f"  Machine Inventory:      ${self.machine_inventory_value:.2f}",
            "",
            "RUN STATISTICS",
            "-" * 40,
            f"  Total Days:             {self.total_days}",
            f"  Total Messages:         {self.total_messages}",
            f"  Total Tool Calls:       {self.total_tool_calls}",
            f"  Total Units Sold:       {self.total_units_sold}",
            f"  Total Revenue:          ${self.total_revenue:.2f}",
            "",
            "TERMINATION",
            "-" * 40,
            f"  Reason:                 {self.termination_reason}",
            f"  Bankrupt:               {'Yes' if self.bankrupt else 'No'}",
            "",
        ]

        if self.sales_stop_day:
            lines.extend([
                "PERFORMANCE TRACKING",
                "-" * 40,
                f"  Last Sale Day:          {self.last_sale_day}",
                f"  Sales Stop Day:         {self.sales_stop_day}",
                f"  Days Until Stop:        {self.days_until_sales_stop}",
                "",
            ])

        lines.append("=" * 60)
        return "\n".join(lines)


class Scorer:
    """
    Calculates scores for Vending-Bench runs.

    From the paper (Section 2.4):
    "The primary score of the agent is its net worth at the end of the game, i.e. a sum of:
    - The cash at hand
    - The cash not emptied from the vending machine
    - The value of the unsold products purchased and currently in the inventory
      or in the vending machine (based on the wholesale purchase price)"
    """

    def calculate_net_worth(self, state: EnvironmentState) -> float:
        """
        Calculate net worth from current state.

        Net Worth = Money Balance + Machine Cash + Storage Value + Machine Inventory Value
        """
        return (
            state.account.balance
            + state.machine.cash_box
            + state.storage.get_total_value()
            + state.machine.get_total_inventory_value()
        )

    def calculate_final_result(
        self,
        state: EnvironmentState,
        termination_reason: str,
    ) -> RunResult:
        """
        Calculate final result after a run completes.

        Args:
            state: Final environment state
            termination_reason: Why the run ended

        Returns:
            Complete run result with all metrics
        """
        # Finalize current day's metrics if not done
        state.finalize_day_metrics()

        # Calculate totals from daily metrics
        total_units_sold = sum(m.units_sold for m in state.daily_metrics)
        total_revenue = sum(m.revenue for m in state.daily_metrics)

        # Determine sales stop
        sales_stop_day = state.sales_stop_day
        if sales_stop_day is None and state.last_sale_day > 0:
            # Check if sales stopped near the end
            if state.clock.day_number - state.last_sale_day >= 5:
                sales_stop_day = state.last_sale_day

        days_until_sales_stop = None
        if sales_stop_day:
            days_until_sales_stop = sales_stop_day

        return RunResult(
            net_worth=self.calculate_net_worth(state),
            money_balance=state.account.balance,
            machine_cash=state.machine.cash_box,
            storage_value=state.storage.get_total_value(),
            machine_inventory_value=state.machine.get_total_inventory_value(),
            total_days=state.clock.day_number,
            total_messages=state.message_count,
            total_tool_calls=state.total_tool_calls,
            total_units_sold=total_units_sold,
            total_revenue=total_revenue,
            termination_reason=termination_reason,
            bankrupt=state.account.is_bankrupt(),
            sales_stop_day=sales_stop_day,
            last_sale_day=state.last_sale_day,
            days_until_sales_stop=days_until_sales_stop,
            daily_metrics=[m.to_dict() for m in state.daily_metrics],
        )

    def get_daily_snapshot(self, state: EnvironmentState) -> dict[str, Any]:
        """Get a snapshot of current metrics for daily tracking."""
        return {
            "day_number": state.clock.day_number,
            "date": state.clock.current_date.isoformat(),
            "net_worth": self.calculate_net_worth(state),
            "money_balance": state.account.balance,
            "machine_cash": state.machine.cash_box,
            "storage_value": state.storage.get_total_value(),
            "machine_inventory_value": state.machine.get_total_inventory_value(),
            "products_in_machine": sum(s.quantity for s in state.machine.get_all_slots()),
            "unique_products": state.machine.get_unique_product_count(),
            "storage_items": state.storage.get_total_items(),
            "message_count": state.message_count,
            "tool_calls": state.total_tool_calls,
        }
