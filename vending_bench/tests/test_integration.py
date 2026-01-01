"""Integration tests for Vending-Bench."""

import pytest
from datetime import date

from vending_bench.config import Config
from vending_bench.environment.state import EnvironmentState
from vending_bench.environment.vending_machine import SlotSize
from vending_bench.simulation.supplier import SupplierSimulator
from vending_bench.simulation.demand_model import DemandModel
from vending_bench.tools.base import ToolRegistry
from vending_bench.tools.main_agent_tools import create_main_agent_tools
from vending_bench.tools.sub_agent_tools import create_sub_agent_tools
from vending_bench.tools.memory_tools import create_memory_tools
from vending_bench.agent.sub_agent import SubAgent
from vending_bench.scoring.scorer import Scorer


class TestFullWorkflow:
    """Integration test for full purchase -> delivery -> stock -> sell workflow."""

    @pytest.fixture
    def config(self) -> Config:
        """Create test configuration."""
        config = Config.default()
        config.demand.seed = 42
        return config

    @pytest.fixture
    def state(self, config: Config) -> EnvironmentState:
        """Create environment state."""
        return EnvironmentState.create(config)

    @pytest.fixture
    def supplier_sim(self, config: Config) -> SupplierSimulator:
        """Create supplier simulator."""
        return SupplierSimulator.create(config.supplier, seed=42)

    @pytest.fixture
    def demand_model(self, config: Config) -> DemandModel:
        """Create demand model."""
        return DemandModel.create(config.demand)

    @pytest.fixture
    def sub_agent(self) -> SubAgent:
        """Create sub-agent."""
        return SubAgent()

    @pytest.fixture
    def scorer(self) -> Scorer:
        """Create scorer."""
        return Scorer()

    def test_full_workflow(
        self,
        state: EnvironmentState,
        config: Config,
        supplier_sim: SupplierSimulator,
        demand_model: DemandModel,
        sub_agent: SubAgent,
        scorer: Scorer,
    ):
        """Test complete workflow: order -> delivery -> stock -> sell -> collect."""
        initial_balance = state.account.balance
        assert initial_balance == 500.0

        # Step 1: Send order email
        order_body = """Hello,

I would like to order:
- Coca-Cola: 20 units
- Red Bull: 15 units

Delivery Address: 123 Vending Street, Business District, CA 90210
Account: VB-001234567

Thanks!"""

        state.email_system.send_email(
            recipient="orders@beverageworld.example.com",
            subject="Product Order",
            body=order_body,
            timestamp=state.clock.get_datetime(),
            day_number=state.clock.day_number,
        )

        # Process outgoing email
        supplier_sim.process_outgoing_email(
            email_system=state.email_system,
            sender_email=state.email_system.agent_email,
            recipient_email="orders@beverageworld.example.com",
            subject="Product Order",
            body=order_body,
            current_date=state.clock.current_date,
            day_number=state.clock.day_number,
        )

        # Verify order was placed
        assert len(supplier_sim.pending_orders) == 1
        order = supplier_sim.pending_orders[0]
        assert order.status.value == "confirmed"

        # Step 2: Advance days until delivery
        for day in range(5):
            state.clock.advance_to_next_day()

            # Pay daily fee
            state.account.pay_daily_fee(
                fee=config.environment.daily_fee,
                current_date=state.clock.current_date,
                day_number=state.clock.day_number,
            )

            # Generate supplier responses
            supplier_sim.generate_daily_responses(
                email_system=state.email_system,
                current_date=state.clock.current_date,
                day_number=state.clock.day_number,
            )

            # Process deliveries
            deliveries = supplier_sim.process_deliveries(
                email_system=state.email_system,
                current_date=state.clock.current_date,
                day_number=state.clock.day_number,
            )

            if deliveries:
                # Add to storage and charge account
                for delivered_order in deliveries:
                    for item in delivered_order.items:
                        product_info = delivered_order.supplier.get_product(item.product_name)
                        size = product_info.size if product_info else SlotSize.SMALL
                        state.storage.add_product(
                            name=item.product_name,
                            quantity=item.quantity,
                            purchase_price=item.unit_price,
                            size=size,
                        )

                    state.account.make_purchase(
                        amount=delivered_order.total_amount,
                        supplier=delivered_order.supplier.name,
                        items=", ".join(f"{i.product_name}" for i in delivered_order.items),
                        current_date=state.clock.current_date,
                        day_number=state.clock.day_number,
                    )
                break

        # Verify products in storage
        assert state.storage.get_total_items() > 0, "No products delivered to storage"

        # Step 3: Stock products using sub-agent
        result = sub_agent.execute_instruction(
            instruction="Stock 10 units of Coca-Cola in the vending machine",
            state=state,
            config=config,
        )
        assert result.success, f"Stocking failed: {result.output}"

        # Verify products in machine
        machine_items = sum(s.quantity for s in state.machine.get_all_slots())
        assert machine_items > 0, "No products stocked in machine"

        # Step 4: Set prices
        result = sub_agent.execute_instruction(
            instruction="Set the price of Coca-Cola to $1.50",
            state=state,
            config=config,
        )
        assert result.success, f"Price setting failed: {result.output}"

        # Verify price set
        prices = state.machine.get_products_with_prices()
        assert "Coca-Cola" in prices
        assert prices["Coca-Cola"] == 1.50

        # Step 5: Simulate sales
        sales_result = demand_model.simulate_daily_sales(
            machine=state.machine,
            current_date=state.clock.current_date,
            day_number=state.clock.day_number,
        )

        # Record sales
        state.record_sale(sales_result.total_units_sold, sales_result.total_revenue)

        # Machine should have cash if there were sales
        if sales_result.total_units_sold > 0:
            assert state.machine.cash_box > 0

        # Step 6: Collect cash
        if state.machine.cash_box > 0:
            result = sub_agent.execute_instruction(
                instruction="Collect cash from the vending machine",
                state=state,
                config=config,
            )
            assert result.success

            # Cash should now be in account
            assert state.machine.cash_box == 0

        # Step 7: Calculate net worth
        net_worth = scorer.calculate_net_worth(state)

        # Net worth should include all assets
        expected_net_worth = (
            state.account.balance
            + state.machine.cash_box
            + state.storage.get_total_value()
            + state.machine.get_total_inventory_value()
        )
        assert net_worth == pytest.approx(expected_net_worth, rel=0.01)

        print(f"\nFinal State:")
        print(f"  Days elapsed: {state.clock.day_number}")
        print(f"  Account balance: ${state.account.balance:.2f}")
        print(f"  Storage value: ${state.storage.get_total_value():.2f}")
        print(f"  Machine inventory value: ${state.machine.get_total_inventory_value():.2f}")
        print(f"  Net worth: ${net_worth:.2f}")

    def test_reproducibility(self, config: Config):
        """Test that simulation is reproducible with same seed."""
        config.demand.seed = 12345

        # Run 1
        state1 = EnvironmentState.create(config)
        demand1 = DemandModel.create(config.demand)

        # Add some products
        state1.storage.add_product("Coca-Cola", 20, 0.75, SlotSize.SMALL)
        state1.machine.stock_product(
            state1.storage.get_product("Coca-Cola").product, 10
        )
        state1.machine.set_price("Coca-Cola", 1.50)

        # Simulate day
        result1 = demand1.simulate_daily_sales(
            machine=state1.machine,
            current_date=state1.clock.current_date,
            day_number=state1.clock.day_number,
        )

        # Run 2 with same seed
        state2 = EnvironmentState.create(config)
        demand2 = DemandModel.create(config.demand)

        state2.storage.add_product("Coca-Cola", 20, 0.75, SlotSize.SMALL)
        state2.machine.stock_product(
            state2.storage.get_product("Coca-Cola").product, 10
        )
        state2.machine.set_price("Coca-Cola", 1.50)

        result2 = demand2.simulate_daily_sales(
            machine=state2.machine,
            current_date=state2.clock.current_date,
            day_number=state2.clock.day_number,
        )

        # Results should be identical
        assert result1.total_units_sold == result2.total_units_sold
        assert result1.total_revenue == result2.total_revenue


class TestToolExecution:
    """Test tool execution."""

    @pytest.fixture
    def tool_registry(self) -> ToolRegistry:
        """Create tool registry with all tools."""
        registry = ToolRegistry()
        registry.register_many(create_main_agent_tools())
        registry.register_many(create_memory_tools())
        return registry

    def test_get_money_balance(
        self, tool_registry: ToolRegistry, state: EnvironmentState, config: Config
    ):
        """Test get_money_balance tool."""
        result = tool_registry.execute("get_money_balance", state, config)
        assert result.success
        assert "$500.00" in result.output

    def test_memory_tools(
        self, tool_registry: ToolRegistry, state: EnvironmentState, config: Config
    ):
        """Test memory tools."""
        # Write to scratchpad
        result = tool_registry.execute(
            "write_scratchpad",
            state,
            config,
            content="Test note",
        )
        assert result.success

        # Read from scratchpad
        result = tool_registry.execute("read_scratchpad", state, config)
        assert result.success
        assert "Test note" in result.output

        # Set KV value
        result = tool_registry.execute(
            "set_kv_value",
            state,
            config,
            key="supplier_email",
            value="test@example.com",
        )
        assert result.success

        # Get KV value
        result = tool_registry.execute(
            "get_kv_value",
            state,
            config,
            key="supplier_email",
        )
        assert result.success
        assert "test@example.com" in result.output

    @pytest.fixture
    def state(self, config: Config) -> EnvironmentState:
        return EnvironmentState.create(config)

    @pytest.fixture
    def config(self) -> Config:
        return Config.default()


class TestNetWorthCalculation:
    """Test net worth calculation."""

    def test_initial_net_worth(self, config: Config):
        """Test net worth at start equals initial balance."""
        state = EnvironmentState.create(config)
        scorer = Scorer()

        net_worth = scorer.calculate_net_worth(state)
        assert net_worth == config.environment.initial_balance

    def test_net_worth_with_inventory(self, config: Config):
        """Test net worth includes inventory value."""
        state = EnvironmentState.create(config)
        scorer = Scorer()

        # Add inventory to storage
        state.storage.add_product("Coca-Cola", 20, 0.75, SlotSize.SMALL)

        net_worth = scorer.calculate_net_worth(state)

        # Net worth should be initial balance + inventory value
        expected = 500.0 + (20 * 0.75)
        assert net_worth == pytest.approx(expected, rel=0.01)

    def test_net_worth_with_machine_cash(self, config: Config):
        """Test net worth includes uncollected machine cash."""
        state = EnvironmentState.create(config)
        scorer = Scorer()

        # Add cash to machine
        state.machine.cash_box = 50.0

        net_worth = scorer.calculate_net_worth(state)
        assert net_worth == 500.0 + 50.0
