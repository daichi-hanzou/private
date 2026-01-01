"""
Rule-based agent for Vending-Bench.
A simple deterministic agent for testing and baseline comparisons.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from vending_bench.agent.base import BaseAgent, AgentAction

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.config import Config
    from vending_bench.tools.base import ToolResult


class RuleBasedAgent(BaseAgent):
    """
    A simple rule-based agent for testing.

    Follows a fixed strategy:
    1. Check balance
    2. Search for suppliers
    3. Order products
    4. Wait for delivery
    5. Stock products
    6. Set prices
    7. Collect cash
    8. Repeat
    """

    def __init__(self) -> None:
        super().__init__()
        self.phase = "init"
        self.day_actions_taken = 0
        self.suppliers_contacted = set()
        self.products_ordered = False
        self.products_stocked = False
        self.prices_set = False
        self.last_result: str = ""

    def think(
        self,
        state: EnvironmentState,
        config: Config,
        observation: str,
    ) -> AgentAction:
        """Decide next action based on current state and phase."""
        self.add_message("user", observation)

        # Track the observation
        self.last_result = observation

        # Get current status
        balance = state.account.balance
        storage_items = state.storage.get_total_items()
        machine_items = sum(s.quantity for s in state.machine.get_all_slots())
        machine_cash = state.machine.cash_box
        has_unread_emails = (
            state.email_system.get_unread_count() > 0 if state.email_system else False
        )

        # Decision tree
        action = self._decide_action(
            state, config, balance, storage_items, machine_items, machine_cash, has_unread_emails
        )

        self.day_actions_taken += 1
        return action

    def _decide_action(
        self,
        state: EnvironmentState,
        config: Config,
        balance: float,
        storage_items: int,
        machine_items: int,
        machine_cash: float,
        has_unread_emails: bool,
    ) -> AgentAction:
        """Core decision logic."""

        # Priority 1: Collect cash if significant amount in machine
        if machine_cash >= 50:
            return AgentAction(
                tool_name="run_sub_agent",
                arguments={"instruction": "Collect all cash from the vending machine"},
                reasoning="Collecting accumulated cash from machine",
            )

        # Priority 2: Check unread emails (might have delivery notifications)
        if has_unread_emails and state.email_system:
            unread = [
                e for e in state.email_system.inbox
                if e.status.value == "unread"
            ]
            if unread:
                return AgentAction(
                    tool_name="read_email",
                    arguments={"email_id": unread[0].id},
                    reasoning="Reading unread email",
                )

        # Priority 3: Stock products if in storage but machine has room
        if storage_items > 0 and machine_items < 12 * 5:  # Less than half capacity
            products = state.storage.get_product_names()
            if products:
                product = products[0]
                qty = min(state.storage.get_quantity(product), 10)
                return AgentAction(
                    tool_name="run_sub_agent",
                    arguments={
                        "instruction": f"Stock {qty} units of {product} from storage to the vending machine"
                    },
                    reasoning=f"Stocking {product} in machine",
                )

        # Priority 4: Set prices if products in machine without prices
        products_without_prices = []
        for slot in state.machine.get_all_slots():
            if slot.product and slot.quantity > 0 and slot.price <= 0:
                products_without_prices.append(slot.product.name)

        if products_without_prices:
            product = products_without_prices[0]
            # Simple pricing: 1.5x wholesale price
            storage_item = state.storage.get_product(product)
            if storage_item:
                price = storage_item.purchase_price * 1.5
            else:
                price = 2.00  # Default price
            return AgentAction(
                tool_name="run_sub_agent",
                arguments={"instruction": f"Set the price of {product} to ${price:.2f}"},
                reasoning=f"Setting price for {product}",
            )

        # Priority 5: Order products if low on inventory and have money
        total_inventory = storage_items + machine_items
        if total_inventory < 30 and balance >= 100 and not self.products_ordered:
            # First, search for suppliers if not done
            if not self.suppliers_contacted:
                return AgentAction(
                    tool_name="ai_web_search",
                    arguments={"query": "wholesale beverage and snack suppliers"},
                    reasoning="Looking for suppliers",
                )

            # Send order email
            from vending_bench.simulation.supplier import DEFAULT_SUPPLIERS
            if DEFAULT_SUPPLIERS:
                supplier = DEFAULT_SUPPLIERS[0]
                order_text = f"""Hello,

I would like to order the following items:
- Coca-Cola: 20 units
- Red Bull: 15 units
- Lay's Chips: 15 units

Please deliver to: {state.email_system.delivery_address if state.email_system else '123 Main St'}
Billing Account: {state.account.account_number}

Thank you,
{state.email_system.agent_name if state.email_system else 'Agent'}"""

                self.products_ordered = True
                return AgentAction(
                    tool_name="send_email",
                    arguments={
                        "recipient": supplier.email,
                        "subject": "Product Order Request",
                        "body": order_text,
                    },
                    reasoning="Ordering products from supplier",
                )

        # Priority 6: Check current status periodically
        if self.day_actions_taken % 5 == 0:
            return AgentAction(
                tool_name="get_money_balance",
                arguments={},
                reasoning="Checking financial status",
            )

        # Default: Wait for next day if nothing else to do
        # Reset daily tracking
        self.day_actions_taken = 0
        if self.products_ordered:
            self.products_ordered = False  # Reset for next cycle
            self.suppliers_contacted.clear()

        return AgentAction(
            tool_name="wait_for_next_day",
            arguments={},
            reasoning="Nothing urgent to do, advancing to next day",
        )

    def receive_tool_result(
        self,
        action: AgentAction,
        result: ToolResult,
    ) -> None:
        """Process tool result."""
        self.add_message(
            "tool",
            result.output,
            tool_name=action.tool_name,
        )

        # Update internal state based on results
        if action.tool_name == "ai_web_search":
            self.suppliers_contacted.add("searched")

        # Track successful actions
        if result.success:
            if "stock" in action.tool_name.lower() or (
                action.tool_name == "run_sub_agent"
                and "stock" in action.arguments.get("instruction", "").lower()
            ):
                self.products_stocked = True
            elif "price" in action.tool_name.lower() or (
                action.tool_name == "run_sub_agent"
                and "price" in action.arguments.get("instruction", "").lower()
            ):
                self.prices_set = True
