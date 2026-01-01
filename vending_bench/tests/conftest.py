"""Pytest fixtures for Vending-Bench tests."""

import pytest
from datetime import date

from vending_bench.config import Config
from vending_bench.environment.state import EnvironmentState
from vending_bench.environment.vending_machine import VendingMachine, Product, SlotSize
from vending_bench.environment.storage import Storage
from vending_bench.environment.account import Account
from vending_bench.environment.clock import SimulationClock


@pytest.fixture
def config() -> Config:
    """Create default test configuration."""
    return Config.default()


@pytest.fixture
def clock() -> SimulationClock:
    """Create a simulation clock."""
    return SimulationClock.create(start_date=date(2025, 1, 1), start_hour=8)


@pytest.fixture
def vending_machine() -> VendingMachine:
    """Create a test vending machine."""
    return VendingMachine.create(
        rows=4,
        slots_per_row=3,
        small_rows=[0, 1],
        large_rows=[2, 3],
    )


@pytest.fixture
def storage() -> Storage:
    """Create a test storage."""
    storage = Storage()
    # Add some test products
    storage.add_product("Coca-Cola", 20, 0.75, SlotSize.SMALL)
    storage.add_product("Red Bull", 15, 1.95, SlotSize.SMALL)
    storage.add_product("Gatorade", 10, 1.25, SlotSize.LARGE)
    return storage


@pytest.fixture
def account() -> Account:
    """Create a test account."""
    return Account.create(initial_balance=500.0)


@pytest.fixture
def env_state(config: Config) -> EnvironmentState:
    """Create a full environment state."""
    return EnvironmentState.create(config)


@pytest.fixture
def sample_product_small() -> Product:
    """Create a sample small product."""
    return Product(name="Coca-Cola", size=SlotSize.SMALL, wholesale_price=0.75)


@pytest.fixture
def sample_product_large() -> Product:
    """Create a sample large product."""
    return Product(name="Gatorade", size=SlotSize.LARGE, wholesale_price=1.25)
