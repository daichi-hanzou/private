"""
Supplier simulation for Vending-Bench.
Handles product catalogs, order processing, and delivery simulation.
"""

from __future__ import annotations

import json
import random
import re
from dataclasses import dataclass, field
from datetime import date, datetime, timedelta
from enum import Enum
from pathlib import Path
from typing import Any, TYPE_CHECKING

from vending_bench.environment.vending_machine import SlotSize

if TYPE_CHECKING:
    from vending_bench.config import SupplierConfig
    from vending_bench.simulation.email_system import EmailSystem


class OrderStatus(Enum):
    """Status of an order."""

    PENDING = "pending"
    CONFIRMED = "confirmed"
    SHIPPED = "shipped"
    DELIVERED = "delivered"
    CANCELLED = "cancelled"


@dataclass
class ProductInfo:
    """Information about a product from a supplier."""

    name: str
    wholesale_price: float
    size: SlotSize
    description: str = ""
    min_quantity: int = 1

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "wholesale_price": self.wholesale_price,
            "size": self.size.value,
            "description": self.description,
            "min_quantity": self.min_quantity,
        }


@dataclass
class Supplier:
    """A wholesale supplier."""

    name: str
    email: str
    products: list[ProductInfo]
    location: str = ""
    response_delay_days: int = 1

    def get_product(self, name: str) -> ProductInfo | None:
        """Get product by name (case-insensitive)."""
        name_lower = name.lower()
        for product in self.products:
            if product.name.lower() == name_lower:
                return product
        return None

    def format_catalog(self) -> str:
        """Format product catalog for email."""
        lines = [f"Product Catalog from {self.name}:", ""]
        for p in self.products:
            lines.append(f"- {p.name}")
            lines.append(f"  Price: ${p.wholesale_price:.2f} per unit")
            lines.append(f"  Size: {p.size.value}")
            if p.description:
                lines.append(f"  Description: {p.description}")
            lines.append("")
        return "\n".join(lines)

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "email": self.email,
            "location": self.location,
            "products": [p.to_dict() for p in self.products],
        }


@dataclass
class OrderItem:
    """An item in an order."""

    product_name: str
    quantity: int
    unit_price: float

    @property
    def total_price(self) -> float:
        return self.quantity * self.unit_price


@dataclass
class Order:
    """An order placed with a supplier."""

    id: str
    supplier: Supplier
    items: list[OrderItem]
    delivery_address: str
    account_number: str
    status: OrderStatus = OrderStatus.PENDING
    order_date: date | None = None
    expected_delivery_date: date | None = None
    actual_delivery_date: date | None = None

    @property
    def total_amount(self) -> float:
        return sum(item.total_price for item in self.items)

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "supplier": self.supplier.name,
            "items": [
                {
                    "product": i.product_name,
                    "quantity": i.quantity,
                    "unit_price": i.unit_price,
                }
                for i in self.items
            ],
            "total_amount": self.total_amount,
            "status": self.status.value,
            "order_date": self.order_date.isoformat() if self.order_date else None,
            "expected_delivery_date": (
                self.expected_delivery_date.isoformat()
                if self.expected_delivery_date
                else None
            ),
        }


# Default supplier catalog for offline mode
DEFAULT_SUPPLIERS = [
    Supplier(
        name="BeverageWorld Wholesale",
        email="orders@beverageworld.example.com",
        location="Los Angeles, CA",
        products=[
            ProductInfo("Coca-Cola", 0.75, SlotSize.SMALL, "Classic 12oz can"),
            ProductInfo("Pepsi", 0.75, SlotSize.SMALL, "12oz can"),
            ProductInfo("Red Bull", 1.95, SlotSize.SMALL, "8.4oz energy drink"),
            ProductInfo("Monster Energy", 1.85, SlotSize.LARGE, "16oz energy drink"),
            ProductInfo("Gatorade", 1.25, SlotSize.LARGE, "20oz sports drink"),
            ProductInfo("Bottled Water", 0.45, SlotSize.SMALL, "16.9oz purified water"),
            ProductInfo("Orange Juice", 1.50, SlotSize.LARGE, "15.2oz Tropicana"),
        ],
    ),
    Supplier(
        name="SnackMaster Distribution",
        email="sales@snackmaster.example.com",
        location="San Diego, CA",
        products=[
            ProductInfo("Lay's Chips", 0.85, SlotSize.SMALL, "1oz bag"),
            ProductInfo("Doritos", 0.90, SlotSize.SMALL, "1oz bag"),
            ProductInfo("Snickers", 0.95, SlotSize.SMALL, "Regular size"),
            ProductInfo("M&Ms", 0.90, SlotSize.SMALL, "1.69oz pack"),
            ProductInfo("Nature Valley Granola", 0.80, SlotSize.SMALL, "2-bar pack"),
            ProductInfo("Clif Bar", 1.25, SlotSize.SMALL, "Energy bar"),
        ],
    ),
    Supplier(
        name="QuickVend Supplies",
        email="contact@quickvend.example.com",
        location="Phoenix, AZ",
        products=[
            ProductInfo("Coca-Cola", 0.80, SlotSize.SMALL, "12oz can"),
            ProductInfo("Sprite", 0.80, SlotSize.SMALL, "12oz can"),
            ProductInfo("Dr Pepper", 0.80, SlotSize.SMALL, "12oz can"),
            ProductInfo("Red Bull", 2.00, SlotSize.SMALL, "8.4oz can"),
            ProductInfo("Kind Bar", 1.15, SlotSize.SMALL, "Nut bar"),
            ProductInfo("Trail Mix", 1.35, SlotSize.SMALL, "3oz pack"),
        ],
    ),
]


@dataclass
class SupplierSimulator:
    """
    Simulates supplier interactions.

    Handles:
    - Responding to inquiry emails
    - Processing purchase orders
    - Simulating delivery
    """

    suppliers: dict[str, Supplier] = field(default_factory=dict)
    pending_orders: list[Order] = field(default_factory=list)
    completed_orders: list[Order] = field(default_factory=list)
    delivery_days_min: int = 2
    delivery_days_max: int = 5
    _next_order_id: int = 0
    _rng: random.Random = field(default_factory=lambda: random.Random(42))

    @classmethod
    def create(cls, config: SupplierConfig, seed: int = 42) -> SupplierSimulator:
        """Create supplier simulator from config."""
        sim = cls(
            delivery_days_min=config.delivery_days_min,
            delivery_days_max=config.delivery_days_max,
        )
        sim._rng = random.Random(seed)

        # Load suppliers from catalog file or use defaults
        if config.catalog_file and Path(config.catalog_file).exists():
            sim._load_catalog(config.catalog_file)
        else:
            for supplier in DEFAULT_SUPPLIERS:
                sim.suppliers[supplier.email] = supplier

        return sim

    def _load_catalog(self, catalog_file: str) -> None:
        """Load supplier catalog from JSON file."""
        with open(catalog_file, "r") as f:
            data = json.load(f)

        for s_data in data.get("suppliers", []):
            products = [
                ProductInfo(
                    name=p["name"],
                    wholesale_price=p["wholesale_price"],
                    size=SlotSize(p["size"]),
                    description=p.get("description", ""),
                )
                for p in s_data.get("products", [])
            ]
            supplier = Supplier(
                name=s_data["name"],
                email=s_data["email"],
                products=products,
                location=s_data.get("location", ""),
            )
            self.suppliers[supplier.email] = supplier

    def _generate_order_id(self) -> str:
        """Generate unique order ID."""
        self._next_order_id += 1
        return f"ORD-{self._next_order_id:06d}"

    def get_supplier_by_email(self, email: str) -> Supplier | None:
        """Get supplier by email address."""
        return self.suppliers.get(email)

    def search_suppliers(self, query: str) -> list[Supplier]:
        """Search for suppliers matching query."""
        query_lower = query.lower()
        results = []
        for supplier in self.suppliers.values():
            if (
                query_lower in supplier.name.lower()
                or query_lower in supplier.location.lower()
                or any(query_lower in p.name.lower() for p in supplier.products)
            ):
                results.append(supplier)
        return results

    def process_outgoing_email(
        self,
        email_system: EmailSystem,
        sender_email: str,
        recipient_email: str,
        subject: str,
        body: str,
        current_date: date,
        day_number: int,
    ) -> bool:
        """
        Process an email sent by the agent to a supplier.

        Returns True if a response will be generated.
        """
        supplier = self.get_supplier_by_email(recipient_email)
        if not supplier:
            return False

        # Check if this is an order (contains required fields)
        if self._is_purchase_order(body, email_system):
            return self._process_purchase_order(
                supplier, body, email_system, current_date, day_number
            )

        # Otherwise, it's an inquiry - will generate response on next day
        return True

    def _is_purchase_order(self, body: str, email_system: EmailSystem) -> bool:
        """
        Check if email body contains a valid purchase order.

        Order requirements (from paper):
        - Product names and quantities
        - Delivery address
        - Account number for billing
        """
        body_lower = body.lower()

        # Check for address (look for delivery address reference)
        has_address = (
            email_system.delivery_address.lower() in body_lower
            or "delivery" in body_lower
            and ("address" in body_lower or "street" in body_lower)
        )

        # Check for account number
        has_account = (
            email_system.agent_email in body_lower
            or "account" in body_lower
            or "billing" in body_lower
            or re.search(r"VB-\d+", body)
        )

        # Check for quantity indicators
        has_quantity = bool(re.search(r"\d+\s*(units?|items?|pcs?|pieces?|x\s+)", body_lower))

        return has_address and has_account and has_quantity

    def _parse_order_items(
        self, body: str, supplier: Supplier
    ) -> list[tuple[ProductInfo, int]]:
        """Parse product names and quantities from order email."""
        items = []
        body_lower = body.lower()

        for product in supplier.products:
            product_name_lower = product.name.lower()
            if product_name_lower in body_lower:
                # Try to find quantity near product name
                pattern = rf"(\d+)\s*(?:units?|items?|pcs?|pieces?|x)?\s*(?:of\s+)?{re.escape(product_name_lower)}"
                match = re.search(pattern, body_lower)
                if match:
                    quantity = int(match.group(1))
                else:
                    # Try alternative pattern: "Product Name: 10"
                    pattern = rf"{re.escape(product_name_lower)}[:\s]+(\d+)"
                    match = re.search(pattern, body_lower)
                    if match:
                        quantity = int(match.group(1))
                    else:
                        quantity = 10  # Default quantity

                if quantity > 0:
                    items.append((product, quantity))

        return items

    def _process_purchase_order(
        self,
        supplier: Supplier,
        body: str,
        email_system: EmailSystem,
        current_date: date,
        day_number: int,
    ) -> bool:
        """Process a purchase order from the agent."""
        items = self._parse_order_items(body, supplier)

        if not items:
            return True  # Will send "products not found" response

        order_items = [
            OrderItem(
                product_name=product.name,
                quantity=quantity,
                unit_price=product.wholesale_price,
            )
            for product, quantity in items
        ]

        delivery_days = self._rng.randint(self.delivery_days_min, self.delivery_days_max)

        order = Order(
            id=self._generate_order_id(),
            supplier=supplier,
            items=order_items,
            delivery_address=email_system.delivery_address,
            account_number=email_system.agent_email,
            status=OrderStatus.CONFIRMED,
            order_date=current_date,
            expected_delivery_date=current_date + timedelta(days=delivery_days),
        )

        self.pending_orders.append(order)
        return True

    def generate_daily_responses(
        self,
        email_system: EmailSystem,
        current_date: date,
        day_number: int,
    ) -> list[str]:
        """
        Generate supplier responses for emails sent the previous day.

        Returns list of response subjects for logging.
        """
        responses = []
        yesterday = day_number - 1

        # Get emails sent yesterday
        for email in email_system.outbox:
            if email.day_number != yesterday:
                continue

            supplier = self.get_supplier_by_email(email.recipient)
            if not supplier:
                continue

            # Check if there's already a response
            existing_reply = any(
                e.reply_to == email.id for e in email_system.inbox
            )
            if existing_reply:
                continue

            # Check if this was an order
            order = self._find_order_for_email(email.body, supplier)

            if order:
                # Send order confirmation
                response_body = self._generate_order_confirmation(order, supplier)
                subject = f"Re: {email.subject} - Order Confirmed"
            else:
                # Send catalog/inquiry response
                response_body = self._generate_inquiry_response(email.body, supplier)
                subject = f"Re: {email.subject}"

            email_system.receive_email(
                sender=supplier.email,
                subject=subject,
                body=response_body,
                timestamp=datetime(
                    current_date.year, current_date.month, current_date.day, 9, 0
                ),
                day_number=day_number,
                reply_to=email.id,
            )
            responses.append(subject)

        return responses

    def _find_order_for_email(self, body: str, supplier: Supplier) -> Order | None:
        """Find order matching an email."""
        for order in self.pending_orders:
            if order.supplier.email == supplier.email:
                # Check if any order items match email content
                for item in order.items:
                    if item.product_name.lower() in body.lower():
                        return order
        return None

    def _generate_inquiry_response(self, query_body: str, supplier: Supplier) -> str:
        """Generate response to an inquiry email."""
        lines = [
            f"Dear Customer,",
            "",
            f"Thank you for contacting {supplier.name}!",
            "",
            "Here is our current product catalog:",
            "",
            supplier.format_catalog(),
            "To place an order, please reply with:",
            "1. Product names and quantities",
            "2. Your delivery address",
            "3. Your billing account number",
            "",
            "We typically deliver within 2-5 business days.",
            "",
            "Best regards,",
            f"{supplier.name} Sales Team",
            supplier.email,
        ]
        return "\n".join(lines)

    def _generate_order_confirmation(self, order: Order, supplier: Supplier) -> str:
        """Generate order confirmation email."""
        lines = [
            f"Dear Customer,",
            "",
            f"Thank you for your order! Your order {order.id} has been confirmed.",
            "",
            "Order Details:",
            "-" * 40,
        ]

        for item in order.items:
            lines.append(
                f"  {item.product_name}: {item.quantity} units @ ${item.unit_price:.2f} = ${item.total_price:.2f}"
            )

        lines.extend(
            [
                "-" * 40,
                f"Total: ${order.total_amount:.2f}",
                "",
                f"Delivery Address: {order.delivery_address}",
                f"Expected Delivery: {order.expected_delivery_date.isoformat()}",
                "",
                "Your account will be charged upon shipment.",
                "",
                "Best regards,",
                f"{supplier.name}",
            ]
        )
        return "\n".join(lines)

    def process_deliveries(
        self,
        email_system: EmailSystem,
        current_date: date,
        day_number: int,
    ) -> list[Order]:
        """
        Process deliveries for today.

        Returns list of orders that were delivered.
        """
        delivered = []

        for order in self.pending_orders[:]:  # Copy list to allow modification
            if (
                order.status == OrderStatus.CONFIRMED
                and order.expected_delivery_date
                and order.expected_delivery_date <= current_date
            ):
                order.status = OrderStatus.DELIVERED
                order.actual_delivery_date = current_date

                # Send delivery notification
                email_system.receive_email(
                    sender=order.supplier.email,
                    subject=f"Delivery Notification - Order {order.id}",
                    body=self._generate_delivery_notification(order),
                    timestamp=datetime(
                        current_date.year, current_date.month, current_date.day, 10, 0
                    ),
                    day_number=day_number,
                )

                self.pending_orders.remove(order)
                self.completed_orders.append(order)
                delivered.append(order)

        return delivered

    def _generate_delivery_notification(self, order: Order) -> str:
        """Generate delivery notification email."""
        lines = [
            f"Dear Customer,",
            "",
            f"Great news! Your order {order.id} has been delivered!",
            "",
            "Delivered Items:",
        ]

        for item in order.items:
            lines.append(f"  - {item.product_name}: {item.quantity} units")

        lines.extend(
            [
                "",
                f"The items are now available in your storage inventory.",
                f"Your account has been charged ${order.total_amount:.2f}.",
                "",
                "Thank you for your business!",
                "",
                f"{order.supplier.name}",
            ]
        )
        return "\n".join(lines)

    def get_pending_orders(self) -> list[Order]:
        """Get all pending orders."""
        return self.pending_orders

    def to_dict(self) -> dict[str, Any]:
        """Convert simulator state to dictionary."""
        return {
            "supplier_count": len(self.suppliers),
            "pending_orders": len(self.pending_orders),
            "completed_orders": len(self.completed_orders),
        }
