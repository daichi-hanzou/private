from pydantic import BaseModel, Field

from ors import Environment, Server, Split, TextBlock, ToolOutput, tool


INITIAL_CASH = 1_000.0
TOTAL_DAYS = 7
HOLDING_COST_PER_KG = 0.5
SALVAGE_DISCOUNT = 0.5

ITEMS = {
    "standard": {
        "display_name": "Standard Coffee",
        "base_demand": 18.0,
        "reservation_price": 14.0,
        "default_price": 10.0,
        "wholesale_prices": [5.0, 5.0, 6.0, 6.0, 6.0, 7.0, 7.0],
        "festival_boosts": [1.0, 1.0, 1.0, 1.6, 1.6, 1.2, 1.0],
    },
    "premium": {
        "display_name": "Premium Coffee",
        "base_demand": 8.0,
        "reservation_price": 22.0,
        "default_price": 16.0,
        "wholesale_prices": [9.0, 9.0, 10.0, 10.0, 11.0, 11.0, 12.0],
        "festival_boosts": [1.0, 1.0, 1.0, 1.8, 1.8, 1.3, 1.1],
    },
}


class NoParams(BaseModel):
    pass


class BuyInventoryInput(BaseModel):
    item_id: str
    quantity_kg: int = Field(ge=1, le=100)


class SetPriceInput(BaseModel):
    item_id: str
    price_per_kg: float = Field(gt=0.0, le=100.0)


class MiniCoffeeEnv(Environment):
    @classmethod
    def list_splits(cls):
        return [Split(name="train", type="train")]

    @classmethod
    def list_tasks(cls, split: str):
        if split != "train":
            raise ValueError(f"Unknown split: {split}")

        return [
            {
                "id": "mini-coffee-001",
                "description": (
                    "Operate a small coffee retailer for 7 days. "
                    "Choose inventory and prices to maximize profit."
                ),
            }
        ]

    def get_prompt(self):
        self.day_index = 0
        self.cash = INITIAL_CASH
        self.inventory = {item_id: 0 for item_id in ITEMS}
        self.prices = {
            item_id: item["default_price"]
            for item_id, item in ITEMS.items()
        }
        self.history = []

        lines = [
            "You run a small coffee retailer for 7 days.",
            f"Initial cash: ${INITIAL_CASH:,.2f}",
            "",
            "Your goal is to maximize final profit.",
            "The environment hides the exact demand formula.",
            "Demand depends on your price, the item type, and the day.",
            "Some days have temporary demand spikes.",
            "",
            "Tools available:",
            "  view_state      - inspect cash, inventory, current prices, and history",
            "  buy_inventory   - buy kg from the wholesaler at today's wholesale price",
            "  set_price       - set today's retail price for one item",
            "  advance_day     - simulate one full business day",
            "  finish_episode  - end the run and compute reward",
            "",
            "Items:",
        ]

        for item_id, item in ITEMS.items():
            lines.append(
                f"  {item_id}: {item['display_name']}, "
                f"default retail ${item['default_price']:.2f}/kg"
            )

        lines += [
            "",
            "Hint: demand is stronger when your price is lower, "
            "but margin is smaller. Unsold inventory incurs a holding cost.",
        ]

        return [TextBlock(text="\n".join(lines))]

    def _current_day_number(self) -> int:
        return self.day_index + 1

    def _current_wholesale_price(self, item_id: str) -> float:
        price_index = min(self.day_index, TOTAL_DAYS - 1)
        return ITEMS[item_id]["wholesale_prices"][price_index]

    def _inventory_value_at_salvage(self) -> float:
        value = 0.0
        for item_id, quantity in self.inventory.items():
            value += (
                quantity
                * self._current_wholesale_price(item_id)
                * SALVAGE_DISCOUNT
            )
        return value

    def _portfolio_value(self) -> float:
        return self.cash + self._inventory_value_at_salvage()

    def _sales_for_item(self, item_id: str) -> tuple[int, float]:
        item = ITEMS[item_id]
        reservation = item["reservation_price"]
        price = self.prices[item_id]
        inventory = self.inventory[item_id]

        if inventory <= 0:
            return 0, 0.0

        price_factor = max(0.0, 1.0 - (price / reservation))
        boosted_demand = (
            item["base_demand"]
            * item["festival_boosts"][self.day_index]
            * price_factor
        )
        demand_units = int(round(boosted_demand))
        sold_units = min(inventory, demand_units)
        revenue = sold_units * price
        return sold_units, revenue

    def _state_lines(self) -> list[str]:
        lines = [
            f"Day {self._current_day_number()} of {TOTAL_DAYS}",
            f"Cash: ${self.cash:,.2f}",
            f"Estimated terminal value: ${self._portfolio_value():,.2f}",
            "Inventory and pricing:",
        ]

        for item_id, item in ITEMS.items():
            lines.append(
                "  "
                f"{item_id}: {self.inventory[item_id]} kg on hand, "
                f"retail ${self.prices[item_id]:.2f}/kg, "
                f"wholesale today ${self._current_wholesale_price(item_id):.2f}/kg"
            )

        if self.history:
            last_day = self.history[-1]
            lines.append("")
            lines.append("Previous day summary:")
            lines.append(
                f"  Revenue: ${last_day['revenue']:.2f}, "
                f"Holding cost: ${last_day['holding_cost']:.2f}, "
                f"Profit: ${last_day['profit']:.2f}"
            )
            for item_id, sold in last_day["sold_units"].items():
                lines.append(f"  Sold {sold} kg of {item_id}")

        return lines

    @tool
    def view_state(self, params: NoParams) -> ToolOutput:
        """Return the current business state and the latest daily summary."""
        return ToolOutput(
            blocks=[TextBlock(text="\n".join(self._state_lines()))],
            reward=0.0,
            finished=False,
        )

    @tool
    def buy_inventory(self, params: BuyInventoryInput) -> ToolOutput:
        """Buy inventory from the wholesaler at today's wholesale price."""
        item_id = params.item_id
        quantity = params.quantity_kg

        if item_id not in ITEMS:
            return ToolOutput(
                blocks=[TextBlock(text=f"Unknown item_id: {item_id}")],
                reward=0.0,
                finished=False,
            )

        unit_cost = self._current_wholesale_price(item_id)
        total_cost = unit_cost * quantity
        if total_cost > self.cash:
            return ToolOutput(
                blocks=[
                    TextBlock(
                        text=(
                            f"Insufficient cash. Need ${total_cost:.2f}, "
                            f"have ${self.cash:.2f}."
                        )
                    )
                ],
                reward=0.0,
                finished=False,
            )

        self.cash -= total_cost
        self.inventory[item_id] += quantity

        return ToolOutput(
            blocks=[
                TextBlock(
                    text=(
                        f"Bought {quantity} kg of {item_id} "
                        f"at ${unit_cost:.2f}/kg for ${total_cost:.2f}."
                    )
                )
            ],
            reward=0.0,
            finished=False,
        )

    @tool
    def set_price(self, params: SetPriceInput) -> ToolOutput:
        """Update the retail price for one coffee item for the current day."""
        item_id = params.item_id
        if item_id not in ITEMS:
            return ToolOutput(
                blocks=[TextBlock(text=f"Unknown item_id: {item_id}")],
                reward=0.0,
                finished=False,
            )

        self.prices[item_id] = round(params.price_per_kg, 2)
        return ToolOutput(
            blocks=[
                TextBlock(
                    text=(
                        f"Set retail price for {item_id} "
                        f"to ${self.prices[item_id]:.2f}/kg."
                    )
                )
            ],
            reward=0.0,
            finished=False,
        )

    @tool
    def advance_day(self, params: NoParams) -> ToolOutput:
        """Simulate one business day including sales, revenue, and holding cost."""
        if self.day_index >= TOTAL_DAYS:
            return ToolOutput(
                blocks=[TextBlock(text="All days are complete. Call finish_episode.")],
                reward=0.0,
                finished=False,
            )

        sold_units = {}
        total_revenue = 0.0
        for item_id in ITEMS:
            sold, revenue = self._sales_for_item(item_id)
            sold_units[item_id] = sold
            total_revenue += revenue
            self.inventory[item_id] -= sold

        holding_cost = sum(self.inventory.values()) * HOLDING_COST_PER_KG
        self.cash += total_revenue
        self.cash -= holding_cost
        profit = total_revenue - holding_cost

        self.history.append(
            {
                "day": self._current_day_number(),
                "revenue": total_revenue,
                "holding_cost": holding_cost,
                "profit": profit,
                "sold_units": sold_units,
            }
        )

        lines = [
            f"Finished Day {self._current_day_number()}.",
            f"Revenue: ${total_revenue:.2f}",
            f"Holding cost: ${holding_cost:.2f}",
            f"Profit: ${profit:.2f}",
        ]
        for item_id, sold in sold_units.items():
            lines.append(f"  Sold {sold} kg of {item_id}")

        self.day_index += 1

        if self.day_index < TOTAL_DAYS:
            lines.append(f"Next day is Day {self._current_day_number()}.")
        else:
            lines.append("The 7-day horizon is complete. Call finish_episode.")

        return ToolOutput(
            blocks=[TextBlock(text="\n".join(lines))],
            reward=0.0,
            finished=False,
        )

    @tool
    def finish_episode(self, params: NoParams) -> ToolOutput:
        """End the episode, liquidate remaining value, and return the final reward."""
        salvage_value = self._inventory_value_at_salvage()
        final_value = self.cash + salvage_value
        profit = final_value - INITIAL_CASH
        reward = max(0.0, min(1.0, 0.5 + (profit / INITIAL_CASH)))

        lines = [
            "Episode finished.",
            f"Cash: ${self.cash:.2f}",
            f"Salvage value: ${salvage_value:.2f}",
            f"Final value: ${final_value:.2f}",
            f"Profit: ${profit:+.2f}",
            f"Reward: {reward:.4f}",
        ]

        return ToolOutput(
            blocks=[TextBlock(text="\n".join(lines))],
            reward=reward,
            finished=True,
        )


if __name__ == "__main__":
    Server([MiniCoffeeEnv]).run(port=8082)
