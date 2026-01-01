"""
Environment state aggregator for Vending-Bench.
Combines all environment components into a single state object.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import date
from typing import Any, TYPE_CHECKING

from vending_bench.environment.clock import SimulationClock
from vending_bench.environment.vending_machine import VendingMachine
from vending_bench.environment.storage import Storage
from vending_bench.environment.account import Account

if TYPE_CHECKING:
    from vending_bench.config import Config
    from vending_bench.simulation.email_system import EmailSystem
    from vending_bench.memory.scratchpad import Scratchpad
    from vending_bench.memory.kv_store import KeyValueStore
    from vending_bench.memory.vector_db import VectorDB


@dataclass
class DailyMetrics:
    """Metrics for a single day."""

    day_number: int
    date: date
    units_sold: int = 0
    revenue: float = 0.0
    daily_fee_paid: bool = True
    money_balance: float = 0.0
    machine_cash: float = 0.0
    storage_value: float = 0.0
    machine_inventory_value: float = 0.0
    net_worth: float = 0.0
    tool_calls: int = 0
    products_in_machine: int = 0
    unique_products: int = 0

    def to_dict(self) -> dict[str, Any]:
        return {
            "day_number": self.day_number,
            "date": self.date.isoformat(),
            "units_sold": self.units_sold,
            "revenue": self.revenue,
            "daily_fee_paid": self.daily_fee_paid,
            "money_balance": self.money_balance,
            "machine_cash": self.machine_cash,
            "storage_value": self.storage_value,
            "machine_inventory_value": self.machine_inventory_value,
            "net_worth": self.net_worth,
            "tool_calls": self.tool_calls,
            "products_in_machine": self.products_in_machine,
            "unique_products": self.unique_products,
        }


@dataclass
class EnvironmentState:
    """
    Complete state of the Vending-Bench environment.

    Aggregates:
    - Simulation clock (time management)
    - Vending machine (inventory, prices, cash box)
    - Storage (warehouse inventory)
    - Account (money balance)
    - Email system (communication)
    - Memory stores (scratchpad, KV, vector DB)
    - Daily metrics
    """

    clock: SimulationClock
    machine: VendingMachine
    storage: Storage
    account: Account

    # Optional components (set after initialization)
    email_system: EmailSystem | None = None
    scratchpad: Scratchpad | None = None
    kv_store: KeyValueStore | None = None
    vector_db: VectorDB | None = None

    # Metrics tracking
    daily_metrics: list[DailyMetrics] = field(default_factory=list)
    current_day_metrics: DailyMetrics | None = None
    total_tool_calls: int = 0
    message_count: int = 0

    # Sales tracking
    last_sale_day: int = 0
    sales_stop_day: int | None = None

    @classmethod
    def create(cls, config: Config) -> EnvironmentState:
        """Create a new environment state from configuration."""
        from vending_bench.simulation.email_system import EmailSystem
        from vending_bench.memory.scratchpad import Scratchpad
        from vending_bench.memory.kv_store import KeyValueStore
        from vending_bench.memory.vector_db import VectorDB

        env_config = config.environment
        machine_config = env_config.machine

        clock = SimulationClock.create(
            start_date=config.get_start_date(),
            start_hour=env_config.start_hour,
        )

        machine = VendingMachine.create(
            rows=machine_config.rows,
            slots_per_row=machine_config.slots_per_row,
            small_rows=machine_config.small_rows,
            large_rows=machine_config.large_rows,
        )

        storage = Storage()
        account = Account.create(initial_balance=env_config.initial_balance)

        state = cls(
            clock=clock,
            machine=machine,
            storage=storage,
            account=account,
        )

        # Initialize optional components
        state.email_system = EmailSystem()
        state.scratchpad = Scratchpad()
        state.kv_store = KeyValueStore()
        state.vector_db = VectorDB.create(config.embedding)

        # Initialize first day metrics
        state.start_new_day()

        return state

    def start_new_day(self) -> None:
        """Start tracking metrics for a new day."""
        if self.current_day_metrics:
            self.finalize_day_metrics()

        self.current_day_metrics = DailyMetrics(
            day_number=self.clock.day_number,
            date=self.clock.current_date,
            money_balance=self.account.balance,
            machine_cash=self.machine.cash_box,
            storage_value=self.storage.get_total_value(),
            machine_inventory_value=self.machine.get_total_inventory_value(),
        )

    def finalize_day_metrics(self) -> None:
        """Finalize and store the current day's metrics."""
        if not self.current_day_metrics:
            return

        # Update final values
        self.current_day_metrics.money_balance = self.account.balance
        self.current_day_metrics.machine_cash = self.machine.cash_box
        self.current_day_metrics.storage_value = self.storage.get_total_value()
        self.current_day_metrics.machine_inventory_value = self.machine.get_total_inventory_value()
        self.current_day_metrics.net_worth = self.calculate_net_worth()
        self.current_day_metrics.products_in_machine = sum(
            s.quantity for s in self.machine.get_all_slots()
        )
        self.current_day_metrics.unique_products = self.machine.get_unique_product_count()

        self.daily_metrics.append(self.current_day_metrics)

    def record_sale(self, units: int, revenue: float) -> None:
        """Record a sale in the current day's metrics."""
        if self.current_day_metrics:
            self.current_day_metrics.units_sold += units
            self.current_day_metrics.revenue += revenue
            if units > 0:
                self.last_sale_day = self.clock.day_number

    def record_tool_call(self) -> None:
        """Record a tool call."""
        self.total_tool_calls += 1
        if self.current_day_metrics:
            self.current_day_metrics.tool_calls += 1

    def record_daily_fee_result(self, paid: bool) -> None:
        """Record whether daily fee was paid."""
        if self.current_day_metrics:
            self.current_day_metrics.daily_fee_paid = paid

    def check_sales_stopped(self) -> bool:
        """Check if sales have stopped (for tracking purposes)."""
        if self.sales_stop_day is None:
            # Check if no sales in last N days (simplified check)
            days_without_sales = self.clock.day_number - self.last_sale_day
            if days_without_sales >= 7 and self.last_sale_day > 0:
                self.sales_stop_day = self.last_sale_day
                return True
        return self.sales_stop_day is not None

    def calculate_net_worth(self) -> float:
        """
        Calculate net worth as defined in the paper:
        - Cash at hand (money balance)
        - Cash in vending machine (not yet collected)
        - Value of unsold products (at wholesale price)
        """
        return (
            self.account.balance
            + self.machine.cash_box
            + self.storage.get_total_value()
            + self.machine.get_total_inventory_value()
        )

    def is_terminated(self, max_messages: int, bankruptcy_threshold: int) -> tuple[bool, str]:
        """
        Check if simulation should terminate.

        Returns (is_terminated, reason).
        """
        if self.message_count >= max_messages:
            return True, f"Reached maximum messages ({max_messages})"

        if self.account.is_bankrupt(threshold=bankruptcy_threshold):
            return True, f"Bankrupt: failed to pay daily fee for {bankruptcy_threshold} consecutive days"

        return False, ""

    def get_summary(self) -> dict[str, Any]:
        """Get a summary of current state."""
        return {
            "day": self.clock.day_number,
            "datetime": self.clock.format_datetime(),
            "money_balance": self.account.balance,
            "machine_cash": self.machine.cash_box,
            "storage_items": self.storage.get_total_items(),
            "storage_value": self.storage.get_total_value(),
            "machine_items": sum(s.quantity for s in self.machine.get_all_slots()),
            "machine_inventory_value": self.machine.get_total_inventory_value(),
            "net_worth": self.calculate_net_worth(),
            "message_count": self.message_count,
            "total_tool_calls": self.total_tool_calls,
            "consecutive_fee_failures": self.account.consecutive_fee_failures,
        }

    def to_dict(self) -> dict[str, Any]:
        """Convert full state to dictionary."""
        return {
            "clock": {
                "date": self.clock.current_date.isoformat(),
                "time": self.clock.format_time(),
                "day_number": self.clock.day_number,
            },
            "account": self.account.to_dict(),
            "machine": self.machine.get_inventory(),
            "storage": self.storage.to_dict(),
            "metrics": {
                "message_count": self.message_count,
                "total_tool_calls": self.total_tool_calls,
                "net_worth": self.calculate_net_worth(),
                "last_sale_day": self.last_sale_day,
                "sales_stop_day": self.sales_stop_day,
            },
        }
