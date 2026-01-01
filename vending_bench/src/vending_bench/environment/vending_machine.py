"""
Vending machine model for Vending-Bench.
Handles slots, inventory, pricing, and cash collection.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class SlotSize(Enum):
    """Size category for vending machine slots."""

    SMALL = "small"
    LARGE = "large"


@dataclass
class Product:
    """Represents a product that can be sold."""

    name: str
    size: SlotSize
    wholesale_price: float  # Purchase price from supplier

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "size": self.size.value,
            "wholesale_price": self.wholesale_price,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> Product:
        return cls(
            name=data["name"],
            size=SlotSize(data["size"]),
            wholesale_price=data["wholesale_price"],
        )


@dataclass
class Slot:
    """A single slot in the vending machine."""

    row: int
    column: int
    size: SlotSize
    product: Product | None = None
    quantity: int = 0
    price: float = 0.0
    max_capacity: int = 10  # Default capacity per slot

    def is_empty(self) -> bool:
        """Check if slot has no products."""
        return self.quantity == 0

    def can_stock(self, product: Product, quantity: int) -> bool:
        """Check if product can be stocked in this slot."""
        if product.size != self.size:
            return False
        if self.product is not None and self.product.name != product.name:
            return False  # Already has different product
        return self.quantity + quantity <= self.max_capacity

    def stock(self, product: Product, quantity: int) -> int:
        """
        Stock products in this slot.
        Returns the number of items actually stocked.
        """
        if not self.can_stock(product, quantity):
            return 0

        actual_quantity = min(quantity, self.max_capacity - self.quantity)
        self.product = product
        self.quantity += actual_quantity
        return actual_quantity

    def sell(self, quantity: int = 1) -> tuple[int, float]:
        """
        Sell products from this slot.
        Returns (quantity_sold, revenue).
        """
        if self.quantity == 0 or self.price <= 0:
            return 0, 0.0

        actual_sold = min(quantity, self.quantity)
        revenue = actual_sold * self.price
        self.quantity -= actual_sold

        if self.quantity == 0:
            self.product = None  # Clear product when empty

        return actual_sold, revenue

    def to_dict(self) -> dict[str, Any]:
        return {
            "row": self.row,
            "column": self.column,
            "size": self.size.value,
            "product": self.product.to_dict() if self.product else None,
            "quantity": self.quantity,
            "price": self.price,
            "max_capacity": self.max_capacity,
        }


@dataclass
class VendingMachine:
    """
    Vending machine with multiple slots.

    Configuration from paper:
    - 4 rows x 3 columns = 12 slots
    - 2 rows for small items, 2 rows for large items
    """

    slots: list[list[Slot]] = field(default_factory=list)
    cash_box: float = 0.0  # Cash collected from sales, not yet retrieved

    @classmethod
    def create(
        cls,
        rows: int = 4,
        slots_per_row: int = 3,
        small_rows: list[int] | None = None,
        large_rows: list[int] | None = None,
    ) -> VendingMachine:
        """Create a new vending machine with the specified configuration."""
        small_rows = small_rows or [0, 1]
        large_rows = large_rows or [2, 3]

        slots = []
        for row_idx in range(rows):
            row = []
            size = SlotSize.SMALL if row_idx in small_rows else SlotSize.LARGE
            for col_idx in range(slots_per_row):
                row.append(Slot(row=row_idx, column=col_idx, size=size))
            slots.append(row)

        return cls(slots=slots)

    def get_slot(self, row: int, column: int) -> Slot | None:
        """Get a specific slot by position."""
        if 0 <= row < len(self.slots) and 0 <= column < len(self.slots[row]):
            return self.slots[row][column]
        return None

    def get_all_slots(self) -> list[Slot]:
        """Get all slots as a flat list."""
        return [slot for row in self.slots for slot in row]

    def find_slot_for_product(self, product: Product) -> Slot | None:
        """Find a slot that can accept the given product."""
        # First, try to find a slot with the same product
        for slot in self.get_all_slots():
            if slot.product and slot.product.name == product.name:
                if slot.can_stock(product, 1):
                    return slot

        # Then, try to find an empty slot of the right size
        for slot in self.get_all_slots():
            if slot.is_empty() and slot.size == product.size:
                return slot

        return None

    def stock_product(
        self, product: Product, quantity: int, row: int | None = None, column: int | None = None
    ) -> int:
        """
        Stock a product in the machine.

        If row/column specified, stock in that slot.
        Otherwise, find an appropriate slot automatically.

        Returns the number of items actually stocked.
        """
        if row is not None and column is not None:
            slot = self.get_slot(row, column)
            if slot and slot.can_stock(product, quantity):
                return slot.stock(product, quantity)
            return 0

        # Auto-find slot
        total_stocked = 0
        remaining = quantity

        while remaining > 0:
            slot = self.find_slot_for_product(product)
            if not slot:
                break
            stocked = slot.stock(product, remaining)
            total_stocked += stocked
            remaining -= stocked

        return total_stocked

    def set_price(self, product_name: str, price: float) -> int:
        """
        Set price for a product across all slots containing it.
        Returns the number of slots updated.
        """
        count = 0
        for slot in self.get_all_slots():
            if slot.product and slot.product.name == product_name:
                slot.price = price
                count += 1
        return count

    def set_slot_price(self, row: int, column: int, price: float) -> bool:
        """Set price for a specific slot."""
        slot = self.get_slot(row, column)
        if slot:
            slot.price = price
            return True
        return False

    def sell_from_slot(self, row: int, column: int, quantity: int = 1) -> tuple[int, float]:
        """
        Sell products from a specific slot.
        Returns (quantity_sold, revenue).
        """
        slot = self.get_slot(row, column)
        if not slot:
            return 0, 0.0

        sold, revenue = slot.sell(quantity)
        self.cash_box += revenue
        return sold, revenue

    def collect_cash(self) -> float:
        """
        Collect all cash from the machine's cash box.
        Returns the amount collected.
        """
        amount = self.cash_box
        self.cash_box = 0.0
        return amount

    def get_inventory(self) -> dict[str, Any]:
        """Get current inventory status."""
        inventory = []
        for slot in self.get_all_slots():
            inventory.append(
                {
                    "position": f"[{slot.row},{slot.column}]",
                    "size": slot.size.value,
                    "product": slot.product.name if slot.product else None,
                    "quantity": slot.quantity,
                    "price": slot.price,
                }
            )
        return {
            "slots": inventory,
            "cash_box": self.cash_box,
            "total_items": sum(s.quantity for s in self.get_all_slots()),
        }

    def get_products_with_prices(self) -> dict[str, float]:
        """Get all products currently in machine with their prices."""
        products = {}
        for slot in self.get_all_slots():
            if slot.product and slot.quantity > 0:
                products[slot.product.name] = slot.price
        return products

    def get_product_quantities(self) -> dict[str, int]:
        """Get quantities of each product in the machine."""
        quantities: dict[str, int] = {}
        for slot in self.get_all_slots():
            if slot.product and slot.quantity > 0:
                name = slot.product.name
                quantities[name] = quantities.get(name, 0) + slot.quantity
        return quantities

    def get_unique_product_count(self) -> int:
        """Get number of unique products in the machine."""
        return len(self.get_product_quantities())

    def get_total_inventory_value(self) -> float:
        """Calculate total value of inventory at wholesale prices."""
        total = 0.0
        for slot in self.get_all_slots():
            if slot.product and slot.quantity > 0:
                total += slot.quantity * slot.product.wholesale_price
        return total

    def to_dict(self) -> dict[str, Any]:
        """Convert machine state to dictionary."""
        return {
            "slots": [[slot.to_dict() for slot in row] for row in self.slots],
            "cash_box": self.cash_box,
        }
