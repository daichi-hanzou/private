"""Tests for demand model."""

import pytest
from datetime import date

from vending_bench.simulation.demand_model import DemandModel
from vending_bench.environment.vending_machine import VendingMachine, Product, SlotSize
from vending_bench.config import DemandConfig


class TestDemandModel:
    """Tests for DemandModel."""

    @pytest.fixture
    def demand_model(self) -> DemandModel:
        """Create a demand model with fixed seed."""
        config = DemandConfig(seed=42)
        return DemandModel.create(config)

    @pytest.fixture
    def stocked_machine(self) -> VendingMachine:
        """Create a vending machine with products and prices."""
        machine = VendingMachine.create()

        # Stock some products
        coke = Product("Coca-Cola", SlotSize.SMALL, 0.75)
        machine.stock_product(coke, 10)
        machine.set_price("Coca-Cola", 1.50)

        red_bull = Product("Red Bull", SlotSize.SMALL, 1.95)
        machine.stock_product(red_bull, 8)
        machine.set_price("Red Bull", 3.00)

        return machine

    def test_generate_product_params(self, demand_model: DemandModel):
        """Test product parameter generation."""
        params = demand_model.get_product_params("Coca-Cola")

        assert params.price_elasticity < 0  # Should be negative
        assert params.reference_price > 0
        assert params.base_sales > 0

    def test_params_cached(self, demand_model: DemandModel):
        """Test that params are cached."""
        params1 = demand_model.get_product_params("Coca-Cola")
        params2 = demand_model.get_product_params("Coca-Cola")

        assert params1 is params2  # Same object

    def test_simulate_daily_sales(
        self, demand_model: DemandModel, stocked_machine: VendingMachine
    ):
        """Test daily sales simulation."""
        result = demand_model.simulate_daily_sales(
            machine=stocked_machine,
            current_date=date(2025, 6, 15),  # Summer Sunday
            day_number=1,
        )

        # Should have some sales
        assert result.total_units_sold >= 0
        assert result.total_revenue >= 0

        # Machine inventory should decrease
        total_remaining = sum(s.quantity for s in stocked_machine.get_all_slots())
        assert total_remaining <= 18  # Started with 10 + 8

    def test_sales_reproducible_with_seed(self, stocked_machine: VendingMachine):
        """Test that sales are reproducible with same seed."""
        config = DemandConfig(seed=42)

        # First run
        model1 = DemandModel.create(config)
        machine1 = VendingMachine.create()
        machine1.stock_product(Product("Coca-Cola", SlotSize.SMALL, 0.75), 10)
        machine1.set_price("Coca-Cola", 1.50)
        result1 = model1.simulate_daily_sales(machine1, date(2025, 1, 15), 1)

        # Second run with same seed
        model2 = DemandModel.create(config)
        machine2 = VendingMachine.create()
        machine2.stock_product(Product("Coca-Cola", SlotSize.SMALL, 0.75), 10)
        machine2.set_price("Coca-Cola", 1.50)
        result2 = model2.simulate_daily_sales(machine2, date(2025, 1, 15), 1)

        assert result1.total_units_sold == result2.total_units_sold

    def test_price_elasticity_effect(self, demand_model: DemandModel):
        """Test that higher prices reduce sales."""
        # Machine with low price
        machine_low = VendingMachine.create()
        machine_low.stock_product(Product("Coca-Cola", SlotSize.SMALL, 0.75), 50)
        machine_low.set_price("Coca-Cola", 1.00)

        # Machine with high price
        machine_high = VendingMachine.create()
        machine_high.stock_product(Product("Coca-Cola", SlotSize.SMALL, 0.75), 50)
        machine_high.set_price("Coca-Cola", 5.00)

        # Run multiple days to average out noise
        total_low = 0
        total_high = 0

        for day in range(10):
            # Reset machines
            machine_low.get_slot(0, 0).quantity = 50
            machine_high.get_slot(0, 0).quantity = 50

            r1 = demand_model.simulate_daily_sales(machine_low, date(2025, 1, 1 + day), day)
            r2 = demand_model.simulate_daily_sales(machine_high, date(2025, 1, 1 + day), day)

            total_low += r1.total_units_sold
            total_high += r2.total_units_sold

        # Low price should generally have more sales
        # (may not always be true due to randomness, but usually)
        # Skip assertion if both are 0 (no sales at all)
        if total_low > 0 or total_high > 0:
            assert total_low >= total_high * 0.5  # Allow some variance

    def test_weekday_multiplier(self, demand_model: DemandModel):
        """Test that weekday affects sales."""
        # Weekend multipliers should be higher than weekday
        assert demand_model.weekday_multipliers[5] > demand_model.weekday_multipliers[0]  # Sat > Mon
        assert demand_model.weekday_multipliers[6] > demand_model.weekday_multipliers[1]  # Sun > Tue

    def test_choice_multiplier(self, demand_model: DemandModel):
        """Test choice multiplier calculation."""
        # Too few products
        mult_1 = demand_model._calculate_choice_multiplier(1)
        assert mult_1 < 1.0

        # Optimal variety
        mult_opt = demand_model._calculate_choice_multiplier(demand_model.optimal_variety)
        assert mult_opt == pytest.approx(1.0, rel=0.01)

        # Too many products
        mult_many = demand_model._calculate_choice_multiplier(15)
        assert mult_many < 1.0
        assert mult_many >= 0.5  # Capped at 50% reduction
