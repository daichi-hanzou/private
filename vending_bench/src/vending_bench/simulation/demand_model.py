"""
Demand model for Vending-Bench.
Simulates customer purchases using price elasticity of demand.
"""

from __future__ import annotations

import random
from dataclasses import dataclass, field
from datetime import date
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.config import DemandConfig
    from vending_bench.environment.vending_machine import VendingMachine, Slot


@dataclass
class ProductDemandParams:
    """Demand parameters for a single product."""

    price_elasticity: float  # Typically negative (higher price = lower demand)
    reference_price: float  # "Fair" price for the product
    base_sales: float  # Expected daily sales at reference price

    def to_dict(self) -> dict[str, Any]:
        return {
            "price_elasticity": self.price_elasticity,
            "reference_price": self.reference_price,
            "base_sales": self.base_sales,
        }


@dataclass
class DailySalesResult:
    """Result of daily sales simulation."""

    day_number: int
    date: date
    total_units_sold: int
    total_revenue: float
    product_sales: dict[str, int]  # product_name -> units sold
    weather: str

    def to_dict(self) -> dict[str, Any]:
        return {
            "day_number": self.day_number,
            "date": self.date.isoformat(),
            "total_units_sold": self.total_units_sold,
            "total_revenue": self.total_revenue,
            "product_sales": self.product_sales,
            "weather": self.weather,
        }


@dataclass
class DemandModel:
    """
    Simulates customer purchase behavior using price elasticity of demand.

    From the paper (Section 2.2.2):
    1. Generate and cache (price_elasticity, reference_price, base_sales) per product
    2. Calculate sales impact from price deviation
    3. Apply day-of-week and monthly multipliers
    4. Apply weather impact
    5. Apply choice multiplier (product variety bonus/penalty)
    6. Add noise, round, and cap at inventory
    """

    # Demand parameters per product
    # プロダクトごとの価格パラメータ
    # field dataclassで使う特別な初期化ルール
    product_params: dict[str, ProductDemandParams] = field(default_factory=dict)

    # Configuration
    weekday_multipliers: list[float] = field(
        default_factory=lambda: [0.8, 0.85, 0.9, 0.95, 1.1, 1.3, 1.2]
    )
    month_multipliers: list[float] = field(
        default_factory=lambda: [0.7, 0.75, 0.85, 0.95, 1.0, 1.15, 1.2, 1.15, 1.0, 0.95, 0.85, 0.9]
    )
    weather_multipliers: dict[str, float] = field(
        default_factory=lambda: {"sunny": 1.1, "cloudy": 1.0, "rainy": 0.85}
    )
    optimal_variety: int = 6
    variety_penalty_max: float = 0.5
    noise_std: float = 0.1

    _rng: random.Random = field(default_factory=lambda: random.Random(42))

    @classmethod
    def create(cls, config: DemandConfig) -> DemandModel:
        """Create demand model from configuration."""
        model = cls(
            weekday_multipliers=config.weekday_multipliers,
            month_multipliers=config.month_multipliers,
            weather_multipliers=config.weather_multipliers,
            optimal_variety=config.optimal_variety,
            variety_penalty_max=config.variety_penalty_max,
            noise_std=config.noise_std,
        )
        model._rng = random.Random(config.seed)
        return model

    def _generate_product_params(self, product_name: str) -> ProductDemandParams:
        """
        Generate demand parameters for a new product.

        From paper: "GPT-4o generates and caches three values per item"
        MVP: Use random generation with reasonable ranges.
        """
        # Price elasticity: typically between -1 and -3
        # (1% price increase -> 1-3% demand decrease)
        elasticity = self._rng.uniform(-2.5, -1.0)

        # Reference price: depends on product type
        # Using heuristics based on product name
        name_lower = product_name.lower()
        if any(x in name_lower for x in ["red bull", "monster", "energy"]):
            ref_price = self._rng.uniform(2.50, 3.50)
            base_sales = self._rng.uniform(3, 8)
        elif any(x in name_lower for x in ["water", "bottled"]):
            ref_price = self._rng.uniform(1.00, 1.50)
            base_sales = self._rng.uniform(5, 12)
        elif any(x in name_lower for x in ["cola", "coke", "pepsi", "sprite", "soda"]):
            ref_price = self._rng.uniform(1.25, 2.00)
            base_sales = self._rng.uniform(4, 10)
        elif any(x in name_lower for x in ["juice", "gatorade"]):
            ref_price = self._rng.uniform(2.00, 3.00)
            base_sales = self._rng.uniform(2, 6)
        elif any(x in name_lower for x in ["chips", "doritos", "lay"]):
            ref_price = self._rng.uniform(1.50, 2.50)
            base_sales = self._rng.uniform(3, 8)
        elif any(x in name_lower for x in ["bar", "snickers", "candy", "chocolate"]):
            ref_price = self._rng.uniform(1.50, 2.25)
            base_sales = self._rng.uniform(3, 7)
        else:
            ref_price = self._rng.uniform(1.50, 2.50)
            base_sales = self._rng.uniform(2, 6)

        return ProductDemandParams(
            price_elasticity=elasticity,
            reference_price=ref_price,
            base_sales=base_sales,
        )

    def get_product_params(self, product_name: str) -> ProductDemandParams:
        """Get or generate demand parameters for a product."""
        if product_name not in self.product_params:
            self.product_params[product_name] = self._generate_product_params(product_name)
        return self.product_params[product_name]

    def _get_weather(self, current_date: date) -> str:
        """
        Determine weather for a given date.

        MVP: Simple probabilistic model based on month.
        """
        month = current_date.month

        # Summer months more likely to be sunny
        if month in [6, 7, 8]:
            probs = {"sunny": 0.7, "cloudy": 0.2, "rainy": 0.1}
        # Winter months more likely to be cloudy/rainy
        elif month in [11, 12, 1, 2]:
            probs = {"sunny": 0.3, "cloudy": 0.4, "rainy": 0.3}
        else:
            probs = {"sunny": 0.5, "cloudy": 0.35, "rainy": 0.15}

        r = self._rng.random()
        cumulative = 0
        for weather, prob in probs.items():
            cumulative += prob
            if r < cumulative:
                return weather
        return "cloudy"

    def _calculate_choice_multiplier(self, unique_products: int) -> float:
        """
        Calculate choice multiplier based on product variety.

        From paper: "rewards optimal product variety but penalizes excess options,
        capped at 50% reduction"
        """
        if unique_products == 0:
            return 0.0

        optimal = self.optimal_variety

        if unique_products <= optimal:
            # Bonus for approaching optimal variety
            # Linear increase from 0.7 at 1 product to 1.0 at optimal
            return 0.7 + 0.3 * (unique_products / optimal)
        else:
            # Penalty for too many products
            # Up to variety_penalty_max reduction
            excess = unique_products - optimal
            penalty = min(excess * 0.1, self.variety_penalty_max)
            return 1.0 - penalty

    def simulate_daily_sales(
        self,
        machine: VendingMachine,
        current_date: date,
        day_number: int,
    ) -> DailySalesResult:
        """
        Simulate customer purchases for a single day.

        Returns sales results and modifies machine inventory.
        """
        weather = self._get_weather(current_date)

        # Get multipliers
        weekday_mult = self.weekday_multipliers[current_date.weekday()]
        month_mult = self.month_multipliers[current_date.month - 1]
        weather_mult = self.weather_multipliers.get(weather, 1.0)

        # Get unique products for choice multiplier
        unique_products = machine.get_unique_product_count()
        choice_mult = self._calculate_choice_multiplier(unique_products)

        # Combined environmental multiplier
        env_multiplier = weekday_mult * month_mult * weather_mult * choice_mult

        total_units = 0
        total_revenue = 0.0
        product_sales: dict[str, int] = {}

        # Process each slot
        for slot in machine.get_all_slots():
            if slot.quantity == 0 or slot.product is None or slot.price <= 0:
                continue

            product_name = slot.product.name
            current_price = slot.price

            # Get demand parameters
            params = self.get_product_params(product_name)

            # Calculate price impact
            # sales_impact = 1 + elasticity * ((ref_price - current_price) / ref_price)
            price_diff_pct = (params.reference_price - current_price) / params.reference_price
            sales_impact = 1 + params.price_elasticity * (-price_diff_pct)
            # Note: elasticity is negative, so:
            # - price < ref -> price_diff_pct > 0 -> sales_impact > 1 (more sales)
            # - price > ref -> price_diff_pct < 0 -> sales_impact < 1 (fewer sales)

            # Cap sales impact to reasonable range
            sales_impact = max(0.1, min(3.0, sales_impact))

            # Calculate expected sales
            expected_sales = params.base_sales * sales_impact * env_multiplier

            # Add noise
            noise = self._rng.gauss(0, self.noise_std * expected_sales)
            expected_sales = max(0, expected_sales + noise)

            # Round and cap at inventory
            actual_sales = min(int(round(expected_sales)), slot.quantity)

            if actual_sales > 0:
                # Execute sale
                sold, revenue = machine.sell_from_slot(slot.row, slot.column, actual_sales)
                total_units += sold
                total_revenue += revenue

                # Track by product
                product_sales[product_name] = product_sales.get(product_name, 0) + sold

        return DailySalesResult(
            day_number=day_number,
            date=current_date,
            total_units_sold=total_units,
            total_revenue=total_revenue,
            product_sales=product_sales,
            weather=weather,
        )

    def format_sales_report(self, result: DailySalesResult) -> str:
        """Format sales result as a report string."""
        lines = [
            f"Daily Sales Report - Day {result.day_number} ({result.date.isoformat()})",
            f"Weather: {result.weather}",
            "-" * 40,
        ]

        if result.product_sales:
            for product, quantity in sorted(result.product_sales.items()):
                lines.append(f"  {product}: {quantity} units sold")
        else:
            lines.append("  No sales today")

        lines.extend(
            [
                "-" * 40,
                f"Total Units Sold: {result.total_units_sold}",
                f"Total Revenue: ${result.total_revenue:.2f}",
            ]
        )

        return "\n".join(lines)

    def to_dict(self) -> dict[str, Any]:
        """Convert model state to dictionary."""
        return {
            "product_count": len(self.product_params),
            "products": {k: v.to_dict() for k, v in self.product_params.items()},
            "optimal_variety": self.optimal_variety,
        }
