"""
Sub-agent tools for Vending-Bench.
Tools for physical operations at the vending machine.
"""

from __future__ import annotations

from typing import Any, TYPE_CHECKING

from vending_bench.tools.base import BaseTool, ToolResult
from vending_bench.environment.vending_machine import SlotSize

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.config import Config


class StockProductsTool(BaseTool):
    """Stock products from storage to vending machine."""

    @property
    def name(self) -> str:
        return "stock_products_from_storage_to_machine"

    @property
    def description(self) -> str:
        return "Move products from storage warehouse to the vending machine slots."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "product_name": {
                    "type": "string",
                    "description": "Name of the product to stock",
                },
                "quantity": {
                    "type": "integer",
                    "description": "Number of units to stock",
                },
                "row": {
                    "type": "integer",
                    "description": "Target row in vending machine (optional)",
                },
                "column": {
                    "type": "integer",
                    "description": "Target column in vending machine (optional)",
                },
            },
            "required": ["product_name", "quantity"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        product_name: str = "",
        quantity: int = 0,
        row: int | None = None,
        column: int | None = None,
        **kwargs: Any,
    ) -> ToolResult:
        # Check storage
        storage_item = state.storage.get_product(product_name)
        if storage_item is None:
            return ToolResult.error_result(
                f"Product '{product_name}' not found in storage. "
                f"Available products: {', '.join(state.storage.get_product_names())}"
            )

        if storage_item.quantity < quantity:
            return ToolResult.error_result(
                f"Not enough '{product_name}' in storage. "
                f"Available: {storage_item.quantity}, Requested: {quantity}"
            )

        # Get product info
        product = storage_item.product

        # Stock in machine
        if row is not None and column is not None:
            stocked = state.machine.stock_product(product, quantity, row, column)
        else:
            stocked = state.machine.stock_product(product, quantity)

        if stocked == 0:
            return ToolResult.error_result(
                f"Could not stock '{product_name}'. "
                "No suitable slot available (check product size and slot capacity)."
            )

        # Remove from storage
        state.storage.remove_product(product_name, stocked)

        output = f"Successfully stocked {stocked} units of '{product_name}' in the vending machine."
        if stocked < quantity:
            output += f" (Only {stocked} of {quantity} requested could be stocked due to capacity.)"

        return ToolResult.success_result(
            output,
            {"product": product_name, "quantity_stocked": stocked},
        )


class CollectCashTool(BaseTool):
    """Collect cash from the vending machine."""

    @property
    def name(self) -> str:
        return "collect_cash_from_machine"

    @property
    def description(self) -> str:
        return "Collect all cash from the vending machine's cash box and transfer to your account."

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        cash_amount = state.machine.collect_cash()

        if cash_amount <= 0:
            return ToolResult.success_result(
                "No cash to collect. The cash box is empty.",
                {"amount_collected": 0},
            )

        # Credit to account
        state.account.collect_cash(
            amount=cash_amount,
            current_date=state.clock.current_date,
            day_number=state.clock.day_number,
        )

        output = (
            f"Collected ${cash_amount:.2f} from the vending machine.\n"
            f"New balance: ${state.account.balance:.2f}"
        )

        return ToolResult.success_result(
            output,
            {"amount_collected": cash_amount, "new_balance": state.account.balance},
        )


class SetPricesTool(BaseTool):
    """Set prices for products in the vending machine."""

    @property
    def name(self) -> str:
        return "set_prices"

    @property
    def description(self) -> str:
        return "Set the retail price for a product in the vending machine."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "product_name": {
                    "type": "string",
                    "description": "Name of the product",
                },
                "price": {
                    "type": "number",
                    "description": "New retail price in dollars",
                },
            },
            "required": ["product_name", "price"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        product_name: str = "",
        price: float = 0.0,
        **kwargs: Any,
    ) -> ToolResult:
        if price <= 0:
            return ToolResult.error_result("Price must be greater than 0")

        slots_updated = state.machine.set_price(product_name, price)

        if slots_updated == 0:
            # Check if product exists in machine
            products_in_machine = state.machine.get_product_quantities()
            if product_name not in products_in_machine:
                return ToolResult.error_result(
                    f"Product '{product_name}' not found in vending machine. "
                    f"Products available: {', '.join(products_in_machine.keys()) or 'None'}"
                )

        output = f"Set price of '{product_name}' to ${price:.2f} ({slots_updated} slot(s) updated)"
        return ToolResult.success_result(
            output,
            {"product": product_name, "price": price, "slots_updated": slots_updated},
        )


class GetMachineInventoryTool(BaseTool):
    """Get vending machine inventory."""

    @property
    def name(self) -> str:
        return "get_machine_inventory"

    @property
    def description(self) -> str:
        return "View current inventory, prices, and slot status of the vending machine."

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        inventory = state.machine.get_inventory()

        lines = [
            "Vending Machine Inventory:",
            "=" * 50,
            "",
        ]

        # Group by row
        slots_by_row: dict[int, list] = {}
        for slot_info in inventory["slots"]:
            row = int(slot_info["position"][1])  # Extract row from "[row,col]"
            if row not in slots_by_row:
                slots_by_row[row] = []
            slots_by_row[row].append(slot_info)

        for row_idx in sorted(slots_by_row.keys()):
            row_slots = slots_by_row[row_idx]
            size = row_slots[0]["size"]
            lines.append(f"Row {row_idx} ({size} items):")

            for slot in row_slots:
                if slot["product"]:
                    lines.append(
                        f"  {slot['position']}: {slot['product']} - "
                        f"{slot['quantity']} units @ ${slot['price']:.2f}"
                    )
                else:
                    lines.append(f"  {slot['position']}: [Empty]")

            lines.append("")

        lines.extend(
            [
                "-" * 50,
                f"Total Items: {inventory['total_items']}",
                f"Cash in Machine: ${inventory['cash_box']:.2f}",
                f"Inventory Value: ${state.machine.get_total_inventory_value():.2f}",
            ]
        )

        output = "\n".join(lines)
        return ToolResult.success_result(output, {"inventory": inventory})


def create_sub_agent_tools() -> list[BaseTool]:
    """Create all sub-agent tools."""
    return [
        StockProductsTool(),
        CollectCashTool(),
        SetPricesTool(),
        GetMachineInventoryTool(),
    ]
