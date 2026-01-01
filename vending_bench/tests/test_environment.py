"""Tests for environment components."""

import pytest
from datetime import date, timedelta

from vending_bench.environment.clock import SimulationClock
from vending_bench.environment.vending_machine import VendingMachine, Product, Slot, SlotSize
from vending_bench.environment.storage import Storage
from vending_bench.environment.account import Account, TransactionType


class TestSimulationClock:
    """Tests for SimulationClock."""

    def test_create(self):
        """Test clock creation."""
        clock = SimulationClock.create(start_date=date(2025, 1, 1), start_hour=8)
        assert clock.current_date == date(2025, 1, 1)
        assert clock.current_hour == 8
        assert clock.current_minute == 0
        assert clock.day_number == 1

    def test_advance_time_within_day(self, clock: SimulationClock):
        """Test advancing time within the same day."""
        new_day = clock.advance_time(60)  # 1 hour
        assert not new_day
        assert clock.current_hour == 9
        assert clock.current_minute == 0

    def test_advance_time_across_day(self, clock: SimulationClock):
        """Test advancing time across day boundary."""
        new_day = clock.advance_time(18 * 60)  # 18 hours (8 AM -> 2 AM next day)
        assert new_day
        assert clock.day_number == 2
        assert clock.current_hour == 2

    def test_advance_to_next_day(self, clock: SimulationClock):
        """Test jumping to next day."""
        clock.advance_to_next_day()
        assert clock.day_number == 2
        assert clock.current_hour == 8
        assert clock.current_minute == 0

    def test_get_day_of_week(self, clock: SimulationClock):
        """Test getting day of week."""
        # 2025-01-01 is Wednesday (2)
        assert clock.get_day_of_week() == 2


class TestVendingMachine:
    """Tests for VendingMachine."""

    def test_create(self, vending_machine: VendingMachine):
        """Test machine creation."""
        assert len(vending_machine.slots) == 4
        assert len(vending_machine.slots[0]) == 3
        assert vending_machine.cash_box == 0.0

    def test_slot_sizes(self, vending_machine: VendingMachine):
        """Test slot size assignment."""
        # Rows 0-1 should be small
        assert vending_machine.slots[0][0].size == SlotSize.SMALL
        assert vending_machine.slots[1][0].size == SlotSize.SMALL
        # Rows 2-3 should be large
        assert vending_machine.slots[2][0].size == SlotSize.LARGE
        assert vending_machine.slots[3][0].size == SlotSize.LARGE

    def test_stock_product(self, vending_machine: VendingMachine, sample_product_small: Product):
        """Test stocking a product."""
        stocked = vending_machine.stock_product(sample_product_small, 5)
        assert stocked == 5

        # Verify slot
        slot = vending_machine.find_slot_for_product(sample_product_small)
        assert slot is not None
        assert slot.quantity == 5
        assert slot.product.name == "Coca-Cola"

    def test_stock_wrong_size(self, vending_machine: VendingMachine, sample_product_large: Product):
        """Test that large products don't go in small slots."""
        # Fill large rows first
        for _ in range(6):  # 6 large slots
            vending_machine.stock_product(sample_product_large, 10)

        # Now large product should not fit in small slots
        stocked = vending_machine.stock_product(sample_product_large, 5)
        # Should only stock what fits
        assert stocked <= 6 * 10  # Max capacity of large slots

    def test_set_price(self, vending_machine: VendingMachine, sample_product_small: Product):
        """Test setting price."""
        vending_machine.stock_product(sample_product_small, 5)
        updated = vending_machine.set_price("Coca-Cola", 1.50)
        assert updated == 1

        slot = vending_machine.find_slot_for_product(sample_product_small)
        assert slot.price == 1.50

    def test_sell_from_slot(self, vending_machine: VendingMachine, sample_product_small: Product):
        """Test selling products."""
        vending_machine.stock_product(sample_product_small, 5)
        vending_machine.set_price("Coca-Cola", 1.50)

        sold, revenue = vending_machine.sell_from_slot(0, 0, 2)

        assert sold == 2
        assert revenue == 3.00
        assert vending_machine.cash_box == 3.00

    def test_collect_cash(self, vending_machine: VendingMachine, sample_product_small: Product):
        """Test collecting cash."""
        vending_machine.stock_product(sample_product_small, 5)
        vending_machine.set_price("Coca-Cola", 1.50)
        vending_machine.sell_from_slot(0, 0, 2)

        collected = vending_machine.collect_cash()
        assert collected == 3.00
        assert vending_machine.cash_box == 0.0


class TestStorage:
    """Tests for Storage."""

    def test_add_product(self, storage: Storage):
        """Test adding products."""
        assert storage.get_quantity("Coca-Cola") == 20
        assert storage.get_quantity("Red Bull") == 15

    def test_remove_product(self, storage: Storage):
        """Test removing products."""
        removed = storage.remove_product("Coca-Cola", 5)
        assert removed == 5
        assert storage.get_quantity("Coca-Cola") == 15

    def test_remove_more_than_available(self, storage: Storage):
        """Test removing more than available."""
        removed = storage.remove_product("Coca-Cola", 100)
        assert removed == 20  # Only had 20
        assert storage.get_quantity("Coca-Cola") == 0

    def test_get_total_value(self, storage: Storage):
        """Test calculating total value."""
        # 20 * 0.75 + 15 * 1.95 + 10 * 1.25 = 15 + 29.25 + 12.5 = 56.75
        assert storage.get_total_value() == pytest.approx(56.75, rel=0.01)


class TestAccount:
    """Tests for Account."""

    def test_create(self, account: Account):
        """Test account creation."""
        assert account.balance == 500.0
        assert len(account.transactions) == 1  # Initial balance

    def test_debit(self, account: Account):
        """Test debiting money."""
        success = account.debit(
            amount=100.0,
            transaction_type=TransactionType.PURCHASE,
            description="Test purchase",
            current_date=date(2025, 1, 1),
            day_number=1,
        )
        assert success
        assert account.balance == 400.0

    def test_debit_insufficient_funds(self, account: Account):
        """Test debiting with insufficient funds."""
        success = account.debit(
            amount=600.0,
            transaction_type=TransactionType.PURCHASE,
            description="Test purchase",
            current_date=date(2025, 1, 1),
            day_number=1,
        )
        assert not success
        assert account.balance == 500.0  # Unchanged

    def test_pay_daily_fee(self, account: Account):
        """Test paying daily fee."""
        success = account.pay_daily_fee(
            fee=2.0,
            current_date=date(2025, 1, 1),
            day_number=1,
        )
        assert success
        assert account.balance == 498.0
        assert account.consecutive_fee_failures == 0

    def test_pay_daily_fee_failure(self, account: Account):
        """Test failing to pay daily fee."""
        # Drain account first
        account.balance = 1.0

        success = account.pay_daily_fee(
            fee=2.0,
            current_date=date(2025, 1, 1),
            day_number=1,
        )
        assert not success
        assert account.consecutive_fee_failures == 1

    def test_bankruptcy(self, account: Account):
        """Test bankruptcy detection."""
        account.balance = 0.0

        # Fail to pay for 10 consecutive days
        for day in range(10):
            account.pay_daily_fee(2.0, date(2025, 1, 1 + day), day + 1)

        assert account.is_bankrupt(threshold=10)
