"""
Storage (warehouse) model for Vending-Bench.
Handles inventory that hasn't been stocked in the vending machine yet.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

from vending_bench.environment.vending_machine import Product, SlotSize


@dataclass
class StorageItem:
    """An item stored in the warehouse."""

    product: Product
    quantity: int
    purchase_price: float  # Price paid per unit

    @property
    def total_value(self) -> float:
        """Total value at purchase price."""
        return self.quantity * self.purchase_price


@dataclass
class Storage:
    """
    Warehouse storage for products.

    Products are stored here after delivery from suppliers
    before being stocked in the vending machine.
    """

    items: dict[str, StorageItem] = field(default_factory=dict)

    def add_product(
        self,
        name: str,
        quantity: int,
        purchase_price: float,
        size: SlotSize,
    ) -> None:
        """Add products to storage."""
        if name in self.items:
            # Update existing item
            item = self.items[name]
            # Calculate weighted average purchase price
            total_qty = item.quantity + quantity
            if total_qty > 0:
                weighted_price = (
                    item.quantity * item.purchase_price + quantity * purchase_price
                ) / total_qty
            else:
                weighted_price = purchase_price
            item.quantity = total_qty
            item.purchase_price = weighted_price
        else:
            # Add new item
            product = Product(name=name, size=size, wholesale_price=purchase_price)
            self.items[name] = StorageItem(
                product=product,
                quantity=quantity,
                purchase_price=purchase_price,
            )

    def remove_product(self, name: str, quantity: int) -> int:
        """
        Remove products from storage.
        Returns the actual quantity removed.
        """
        if name not in self.items:
            return 0

        item = self.items[name]
        actual_removed = min(quantity, item.quantity)
        item.quantity -= actual_removed

        if item.quantity == 0:
            del self.items[name]

        return actual_removed

    def get_product(self, name: str) -> StorageItem | None:
        """Get a specific product from storage."""
        return self.items.get(name)

    def get_quantity(self, name: str) -> int:
        """Get quantity of a specific product."""
        item = self.items.get(name)
        return item.quantity if item else 0

    def has_product(self, name: str, quantity: int = 1) -> bool:
        """Check if storage has at least the specified quantity of a product."""
        return self.get_quantity(name) >= quantity

    def get_inventory(self) -> list[dict[str, Any]]:
        """Get list of all items in storage."""
        return [
            {
                "name": name,
                "quantity": item.quantity,
                "size": item.product.size.value,
                "unit_price": item.purchase_price,
                "total_value": item.total_value,
            }
            for name, item in sorted(self.items.items())
        ]

    def get_total_value(self) -> float:
        """Calculate total value of all stored items at purchase price."""
        return sum(item.total_value for item in self.items.values())

    def get_total_items(self) -> int:
        """Get total number of items in storage."""
        return sum(item.quantity for item in self.items.values())

    def get_product_names(self) -> list[str]:
        """Get list of all product names in storage."""
        return list(self.items.keys())

    def is_empty(self) -> bool:
        """Check if storage is empty."""
        return len(self.items) == 0

    def to_dict(self) -> dict[str, Any]:
        """Convert storage state to dictionary."""
        return {
            "items": {
                name: {
                    "product": item.product.to_dict(),
                    "quantity": item.quantity,
                    "purchase_price": item.purchase_price,
                }
                for name, item in self.items.items()
            },
            "total_value": self.get_total_value(),
            "total_items": self.get_total_items(),
        }

    def __repr__(self) -> str:
        return f"Storage(items={len(self.items)}, total_value=${self.get_total_value():.2f})"
