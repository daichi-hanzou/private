"""
Sub-agent for Vending-Bench.
Handles physical tasks at the vending machine based on instructions.
"""

from __future__ import annotations

import re
from typing import Any, TYPE_CHECKING

from vending_bench.tools.base import ToolResult, ToolRegistry
from vending_bench.tools.sub_agent_tools import create_sub_agent_tools

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.config import Config


class SubAgent:
    """
    Sub-agent for physical tasks.

    The sub-agent receives natural language instructions and executes
    the appropriate tools to complete tasks at the vending machine.
    """

    def __init__(self) -> None:
        self.tool_registry = ToolRegistry()
        self.tool_registry.register_many(create_sub_agent_tools())
        self.last_action_results: list[dict[str, Any]] = []

    def get_available_tools(self) -> list[str]:
        """Get list of available tool names."""
        return self.tool_registry.list_tools()

    def get_specs(self) -> str:
        """Get specifications of the sub-agent."""
        lines = [
            "Sub-Agent for Physical Tasks",
            "=" * 40,
            "",
            "Available Tools:",
        ]

        for tool in self.tool_registry.get_all_tools():
            lines.append(f"\n{tool.name}:")
            lines.append(f"  {tool.description}")

        return "\n".join(lines)

    def execute_instruction(
        self,
        instruction: str,
        state: EnvironmentState,
        config: Config,
    ) -> ToolResult:
        """
        Execute an instruction by parsing it and calling appropriate tools.

        Args:
            instruction: Natural language instruction
            state: Current environment state
            config: Configuration

        Returns:
            Combined result of all tool executions
        """
        self.last_action_results.clear()

        # Parse instruction and determine actions
        actions = self._parse_instruction(instruction, state)

        if not actions:
            return ToolResult.error_result(
                f"Could not understand instruction: {instruction}\n"
                f"Available actions: stock products, collect cash, set prices, check inventory"
            )

        results = []
        all_success = True

        for tool_name, kwargs in actions:
            result = self.tool_registry.execute(tool_name, state, config, **kwargs)
            results.append(result)
            self.last_action_results.append({
                "tool": tool_name,
                "args": kwargs,
                "success": result.success,
                "output": result.output,
            })

            if not result.success:
                all_success = False

        # Combine results
        combined_output = "\n\n".join(r.output for r in results)

        if all_success:
            return ToolResult.success_result(
                f"Completed {len(actions)} action(s):\n\n{combined_output}",
                {"actions": len(actions), "results": self.last_action_results},
            )
        else:
            return ToolResult(
                success=False,
                output=f"Some actions failed:\n\n{combined_output}",
                data={"actions": len(actions), "results": self.last_action_results},
            )

    def _parse_instruction(
        self,
        instruction: str,
        state: EnvironmentState,
    ) -> list[tuple[str, dict[str, Any]]]:
        """
        Parse natural language instruction into tool calls.

        Returns list of (tool_name, kwargs) tuples.
        """
        actions = []
        instruction_lower = instruction.lower()

        # Detect stocking instructions
        if any(word in instruction_lower for word in ["stock", "load", "fill", "add", "put"]):
            stock_action = self._parse_stock_instruction(instruction, state)
            if stock_action:
                actions.append(stock_action)

        # Detect cash collection
        if any(word in instruction_lower for word in ["collect", "cash", "empty", "retrieve money"]):
            actions.append(("collect_cash_from_machine", {}))

        # Detect price setting
        if any(word in instruction_lower for word in ["price", "pricing", "set price", "cost"]):
            price_actions = self._parse_price_instruction(instruction, state)
            actions.extend(price_actions)

        # Detect inventory check
        if any(word in instruction_lower for word in ["inventory", "check", "status", "view"]):
            if "storage" not in instruction_lower:  # Don't trigger for storage checks
                actions.append(("get_machine_inventory", {}))

        return actions

    def _parse_stock_instruction(
        self,
        instruction: str,
        state: EnvironmentState,
    ) -> tuple[str, dict[str, Any]] | None:
        """Parse a stocking instruction."""
        # Try to find product name and quantity
        instruction_lower = instruction.lower()

        # Get available products from storage
        storage_products = state.storage.get_product_names()

        # Find product mentioned in instruction
        found_product = None
        for product in storage_products:
            if product.lower() in instruction_lower:
                found_product = product
                break

        if not found_product:
            # If no specific product, try to stock first available
            if storage_products:
                found_product = storage_products[0]
            else:
                return None

        # Extract quantity
        quantity_match = re.search(r"(\d+)\s*(?:units?|items?|pcs?|pieces?)?", instruction_lower)
        if quantity_match:
            quantity = int(quantity_match.group(1))
        else:
            # Default quantity
            quantity = min(10, state.storage.get_quantity(found_product))

        if quantity <= 0:
            return None

        return ("stock_products_from_storage_to_machine", {
            "product_name": found_product,
            "quantity": quantity,
        })

    def _parse_price_instruction(
        self,
        instruction: str,
        state: EnvironmentState,
    ) -> list[tuple[str, dict[str, Any]]]:
        """Parse price setting instruction."""
        actions = []
        instruction_lower = instruction.lower()

        # Get products in machine
        machine_products = state.machine.get_product_quantities()

        # Try to find specific product and price
        price_match = re.search(r"\$?(\d+\.?\d*)", instruction)
        price = float(price_match.group(1)) if price_match else None

        # Find product mentioned
        found_product = None
        for product in machine_products.keys():
            if product.lower() in instruction_lower:
                found_product = product
                break

        if found_product and price:
            actions.append(("set_prices", {
                "product_name": found_product,
                "price": price,
            }))
        elif price and not found_product:
            # Set price for all products
            for product in machine_products.keys():
                actions.append(("set_prices", {
                    "product_name": product,
                    "price": price,
                }))
        elif found_product and not price:
            # Get storage price and add markup
            storage_item = state.storage.get_product(found_product)
            if storage_item:
                suggested_price = storage_item.purchase_price * 1.5
            else:
                suggested_price = 2.00
            actions.append(("set_prices", {
                "product_name": found_product,
                "price": suggested_price,
            }))

        return actions

    def chat(self, question: str, state: EnvironmentState) -> str:
        """
        Answer a question about the sub-agent's activities or the machine status.

        Args:
            question: Question from the main agent
            state: Current environment state

        Returns:
            Answer string
        """
        question_lower = question.lower()

        # Questions about last actions
        if any(word in question_lower for word in ["what did", "result", "happen", "status"]):
            if self.last_action_results:
                lines = ["My last actions:"]
                for result in self.last_action_results:
                    status = "Success" if result["success"] else "Failed"
                    lines.append(f"- {result['tool']}: {status}")
                    lines.append(f"  {result['output'][:200]}")
                return "\n".join(lines)
            else:
                return "I haven't performed any actions yet."

        # Questions about inventory
        if any(word in question_lower for word in ["inventory", "stock", "products", "machine"]):
            inventory = state.machine.get_inventory()
            lines = [f"Vending machine has {inventory['total_items']} items:"]
            products = state.machine.get_product_quantities()
            for product, qty in products.items():
                price = state.machine.get_products_with_prices().get(product, 0)
                lines.append(f"  {product}: {qty} units @ ${price:.2f}")
            lines.append(f"Cash in machine: ${inventory['cash_box']:.2f}")
            return "\n".join(lines)

        # Questions about capabilities
        if any(word in question_lower for word in ["can you", "able", "capability", "do"]):
            return self.get_specs()

        # Default response
        return (
            "I can help with physical tasks at the vending machine:\n"
            "- Stocking products from storage\n"
            "- Collecting cash\n"
            "- Setting prices\n"
            "- Checking inventory\n\n"
            "What would you like me to do?"
        )
