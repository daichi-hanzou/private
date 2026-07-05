from __future__ import annotations

import os
from itertools import count

from pydantic import BaseModel, Field

from ors import Environment, Server, Split, TextBlock, ToolOutput, tool


INITIAL_CASH = 1_000.0
TOTAL_DAYS = 7
HOLDING_COST_PER_KG = 0.5
SALVAGE_DISCOUNT = 0.5
INVESTIGATION_COST = 15.0
TRADER_MARGIN = {"standard": 1.8, "premium": 2.2}
TERMINAL_OPEN_CONTRACT_RECOVERY = 0.35

ITEMS = {
    "standard": {
        "display_name": "Standard Coffee",
        "base_demand": 18.0,
        "reservation_price": 14.0,
        "default_price": 10.0,
        "festival_boosts": [1.0, 1.0, 1.0, 1.6, 1.6, 1.2, 1.0],
    },
    "premium": {
        "display_name": "Premium Coffee",
        "base_demand": 8.0,
        "reservation_price": 22.0,
        "default_price": 16.0,
        "festival_boosts": [1.0, 1.0, 1.0, 1.8, 1.8, 1.3, 1.1],
    },
}

FARMER_PROFILES = {
    "sierra_verde": {
        "name": "Sierra Verde Cooperative",
        "public_note": "Large Colombian cooperative with reliable washing stations.",
        "spot_prices": {
            "standard": [4.0, 4.0, 4.5, 4.5, 4.8, 5.0, 5.0],
            "premium": [7.0, 7.0, 7.5, 7.5, 8.0, 8.2, 8.2],
        },
        "harvest_schedule": {
            "standard": [5, 5, 7, 7, 6, 5, 4],
            "premium": [2, 2, 3, 3, 3, 2, 2],
        },
        "starting_cash": 180.0,
        "short_term_obligations": 120.0,
    },
    "cloud_peak": {
        "name": "Cloud Peak Estate",
        "public_note": "Premium-focused farm with volatile financing and excellent cup scores.",
        "spot_prices": {
            "standard": [3.8, 3.8, 4.3, 4.3, 4.6, 4.8, 4.8],
            "premium": [6.2, 6.2, 6.8, 6.8, 7.2, 7.5, 7.5],
        },
        "harvest_schedule": {
            "standard": [2, 2, 3, 3, 3, 2, 2],
            "premium": [3, 3, 4, 4, 4, 3, 3],
        },
        "starting_cash": 75.0,
        "short_term_obligations": 170.0,
    },
    "riverbend": {
        "name": "Riverbend Growers",
        "public_note": "Low-cost producer with uneven delivery discipline but good scale.",
        "spot_prices": {
            "standard": [3.4, 3.4, 3.9, 3.9, 4.1, 4.4, 4.4],
            "premium": [6.0, 6.0, 6.4, 6.4, 6.8, 7.0, 7.0],
        },
        "harvest_schedule": {
            "standard": [6, 6, 7, 8, 8, 7, 6],
            "premium": [1, 1, 2, 2, 2, 1, 1],
        },
        "starting_cash": 115.0,
        "short_term_obligations": 145.0,
    },
}

TRADER_BULLETINS = [
    "Trader desk: supply is ample this week, but direct counterparty quality varies widely.",
    "Trader desk: logistics remain normal; direct default risk is highest in premium micro-lots.",
    "Trader desk: festival demand is building. Consider securing inventory before days 4 and 5.",
    "Trader desk: demand spike is active. Guaranteed inventory is especially valuable today.",
    "Trader desk: demand remains elevated. Premium sell-through is stronger than usual.",
    "Trader desk: market is normalizing. Avoid overstocking high-cost premium beans.",
    "Trader desk: last day of the horizon. Minimize stranded inventory.",
]


class NoParams(BaseModel):
    pass


class InvestigateFarmerInput(BaseModel):
    farmer_id: str


class BuySpotDirectInput(BaseModel):
    farmer_id: str
    item_id: str
    quantity_kg: int = Field(ge=1, le=100)


class ForwardContractInput(BaseModel):
    farmer_id: str
    item_id: str
    quantity_kg: int = Field(ge=1, le=100)
    delivery_day: int = Field(ge=2, le=TOTAL_DAYS)


class BuyTraderInput(BaseModel):
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
                "id": "mini-coffee-merchant-002",
                "description": (
                    "Operate a coffee roaster-retailer for 7 days. Compare direct spot, "
                    "direct forward, and trader procurement under hidden fulfillment risk."
                ),
            }
        ]

    def get_prompt(self):
        self.day_index = 0
        self.cash = INITIAL_CASH
        self.inventory = {item_id: 0 for item_id in ITEMS}
        self.prices = {
            item_id: item["default_price"] for item_id, item in ITEMS.items()
        }
        self.history = []
        self.investigated_farmers: set[str] = set()
        self.contract_id_counter = count(1)
        self.ledger_contracts: list[dict] = []
        self.ledger_deliveries: list[dict] = []
        self.pending_direct_contracts: list[dict] = []
        self.metrics = {
            "investigation_spend": 0.0,
            "direct_spend": 0.0,
            "trader_spend": 0.0,
        }

        self.farmers = {}
        for farmer_id, profile in FARMER_PROFILES.items():
            self.farmers[farmer_id] = {
                "cash": profile["starting_cash"],
                "short_term_obligations": profile["short_term_obligations"],
                "inventory": {item_id: 0 for item_id in ITEMS},
                "harvest_schedule": profile["harvest_schedule"],
                "defaulted": False,
            }

        self.trader = {
            "cash": 700.0,
            "inventory": {"standard": 8, "premium": 3},
            "inbound_contracts": [],
            "committed_sales": 0,
        }
        self._seed_trader_contracts()

        lines = [
            "You run a small coffee roaster-retailer for 7 days.",
            f"Initial cash: ${INITIAL_CASH:,.2f}",
            "",
            "You can procure beans in three ways:",
            "1. Direct spot purchase from a farmer: cheaper, delivered after today's advance_day.",
            "2. Direct forward contract with a farmer: reserve future delivery, but fulfillment is uncertain.",
            "3. Buy from the trader: immediate inventory transfer from trader stock at a markup.",
            "",
            "Farmer internal state is hidden by default. You can investigate a farmer to reveal ledger-based metrics.",
            "The trader already sees the full farmer ledger and maintains its own inbound contracts and inventory.",
            "You do not see future harvest schedules, contract break probabilities, or future trader inbound deliveries.",
            "",
            "Tools available:",
            "  view_state             - inspect cash, inventory, offers, trader inventory, and history",
            "  investigate_farmer     - pay to reveal one farmer's fulfillment and liquidity metrics",
            "  buy_spot_direct        - buy spot from a farmer for next-step delivery",
            "  create_forward_contract - lock a future direct delivery day with a farmer",
            "  buy_from_trader        - buy immediate guaranteed inventory from trader stock",
            "  set_price              - set today's retail price for one item",
            "  advance_day            - harvest, fulfill contracts, sell retail demand, and charge carrying costs",
            "  finish_episode         - end the run and compute reward",
        ]
        return [TextBlock(text="\n".join(lines))]

    def _seed_trader_contracts(self) -> None:
        plans = [
            ("sierra_verde", "standard", 6, 2, 4.2),
            ("cloud_peak", "premium", 4, 3, 6.9),
            ("riverbend", "standard", 8, 4, 4.0),
            ("sierra_verde", "premium", 3, 5, 7.9),
            ("riverbend", "standard", 6, 6, 4.3),
        ]
        for farmer_id, item_id, qty, day, unit_price in plans:
            contract = self._new_contract(
                buyer_kind="trader",
                seller_kind="farmer",
                seller_id=farmer_id,
                item_id=item_id,
                quantity_kg=qty,
                unit_price=unit_price,
                contract_type="forward",
                delivery_day=day,
            )
            self.trader["inbound_contracts"].append(contract)
            self.ledger_contracts.append(contract)

    def _new_contract(
        self,
        buyer_kind: str,
        seller_kind: str,
        seller_id: str,
        item_id: str,
        quantity_kg: int,
        unit_price: float,
        contract_type: str,
        delivery_day: int,
    ) -> dict:
        return {
            "contract_id": f"ct-{next(self.contract_id_counter):03d}",
            "buyer_kind": buyer_kind,
            "seller_kind": seller_kind,
            "seller_id": seller_id,
            "item_id": item_id,
            "quantity_kg": quantity_kg,
            "remaining_qty": quantity_kg,
            "unit_price": unit_price,
            "contract_type": contract_type,
            "order_day": self._current_day_number(),
            "delivery_day": delivery_day,
            "closed": False,
        }

    def _current_day_number(self) -> int:
        return self.day_index + 1

    def _series_value(self, series: list[float]) -> float:
        return series[min(self.day_index, TOTAL_DAYS - 1)]

    def _current_farmer_price(self, farmer_id: str, item_id: str) -> float:
        return self._series_value(FARMER_PROFILES[farmer_id]["spot_prices"][item_id])

    def _current_trader_bulletin(self) -> str:
        return TRADER_BULLETINS[min(self.day_index, TOTAL_DAYS - 1)]

    def _current_trader_price(self, item_id: str) -> float:
        direct_prices = [self._current_farmer_price(fid, item_id) for fid in self.farmers]
        base_price = sum(direct_prices) / len(direct_prices) + TRADER_MARGIN[item_id]
        on_hand = self.trader["inventory"][item_id]
        scarcity_markup = max(0.0, 6 - on_hand) * 0.18
        demand_pressure = self._series_value(ITEMS[item_id]["festival_boosts"]) - 1.0
        return round(base_price + scarcity_markup + demand_pressure * 0.4, 2)

    def _expected_remaining_harvest(self, farmer_id: str, item_id: str) -> int:
        schedule = self.farmers[farmer_id]["harvest_schedule"][item_id]
        return sum(schedule[self.day_index :])

    def _farmer_open_commitments(self, farmer_id: str, item_id: str) -> int:
        total = 0
        for contract in self.ledger_contracts:
            if (
                contract["seller_kind"] == "farmer"
                and contract["seller_id"] == farmer_id
                and contract["item_id"] == item_id
                and not contract["closed"]
            ):
                total += contract["remaining_qty"]
        return total

    def _inventory_value_at_salvage(self) -> float:
        return sum(
            qty * self._current_trader_price(item_id) * SALVAGE_DISCOUNT
            for item_id, qty in self.inventory.items()
        )

    def _portfolio_value(self) -> float:
        return self.cash + self._inventory_value_at_salvage()

    def _sales_for_item(self, item_id: str) -> tuple[int, float]:
        item = ITEMS[item_id]
        price_factor = max(0.0, 1.0 - (self.prices[item_id] / item["reservation_price"]))
        demand = int(
            round(item["base_demand"] * item["festival_boosts"][self.day_index] * price_factor)
        )
        sold = min(self.inventory[item_id], demand)
        return sold, sold * self.prices[item_id]

    def _record_delivery(
        self,
        contract: dict,
        delivered_qty: int,
        processed_day: int,
        refund: float = 0.0,
    ) -> None:
        self.ledger_deliveries.append(
            {
                "contract_id": contract["contract_id"],
                "seller_id": contract["seller_id"],
                "buyer_kind": contract["buyer_kind"],
                "item_id": contract["item_id"],
                "contract_qty": contract["quantity_kg"],
                "delivered_qty": delivered_qty,
                "delivery_day": processed_day,
                "due_day": contract["delivery_day"],
                "on_time": processed_day <= contract["delivery_day"],
                "refund": refund,
            }
        )

    def _farmer_metrics(self, farmer_id: str) -> dict[str, float | bool]:
        contracts = [
            c
            for c in self.ledger_contracts
            if c["seller_kind"] == "farmer"
            and c["seller_id"] == farmer_id
            and c["delivery_day"] <= self._current_day_number()
        ]
        deliveries = [d for d in self.ledger_deliveries if d["seller_id"] == farmer_id]

        contracted_qty = sum(c["quantity_kg"] for c in contracts)
        delivered_qty = sum(d["delivered_qty"] for d in deliveries)
        on_time_delivered_qty = sum(d["delivered_qty"] for d in deliveries if d["on_time"])
        matured_count = len(contracts)
        partial_count = sum(
            1
            for c in contracts
            if sum(d["delivered_qty"] for d in deliveries if d["contract_id"] == c["contract_id"])
            < c["quantity_kg"]
        )
        remaining_harvest = sum(
            self._expected_remaining_harvest(farmer_id, item_id) for item_id in ITEMS
        )
        outstanding_commitments = sum(
            self._farmer_open_commitments(farmer_id, item_id) for item_id in ITEMS
        )
        obligations = max(1.0, self.farmers[farmer_id]["short_term_obligations"])
        recent_cash_ratio = self.farmers[farmer_id]["cash"] / obligations

        return {
            "delivery_rate": delivered_qty / contracted_qty if contracted_qty else 1.0,
            "on_time_delivery_rate": on_time_delivered_qty / contracted_qty if contracted_qty else 1.0,
            "partial_delivery_rate": partial_count / matured_count if matured_count else 0.0,
            "contract_coverage": (
                outstanding_commitments / max(1, remaining_harvest + sum(self.farmers[farmer_id]["inventory"].values()))
            ),
            "recent_cash_ratio": recent_cash_ratio,
            "recent_default_flag": partial_count > 0 and delivered_qty < contracted_qty,
        }

    def _fulfillment_limit(self, farmer_id: str) -> float:
        metrics = self._farmer_metrics(farmer_id)
        cash_ratio = float(metrics["recent_cash_ratio"])
        coverage = float(metrics["contract_coverage"])
        return max(0.55, min(1.0, 0.7 + 0.15 * cash_ratio - 0.1 * max(0.0, coverage - 1.0)))

    def _contract_incentive_multiplier(self, contract: dict) -> float:
        current_spot = self._current_farmer_price(contract["seller_id"], contract["item_id"])
        contracted_price = contract["unit_price"]
        if current_spot <= contracted_price:
            return 1.0
        temptation = min(0.35, (current_spot - contracted_price) / max(0.01, contracted_price))
        if contract["buyer_kind"] == "trader":
            temptation *= 0.5
        return max(0.6, 1.0 - temptation)

    def _harvest_today(self) -> None:
        for farmer_id, farmer in self.farmers.items():
            for item_id in ITEMS:
                farmer["inventory"][item_id] += farmer["harvest_schedule"][item_id][self.day_index]

    def _process_contracts_for_day(self) -> list[str]:
        events = []
        today = self._current_day_number()
        contracts = [
            c
            for c in self.ledger_contracts
            if not c["closed"] and c["delivery_day"] <= today
        ]
        contracts.sort(key=lambda c: (c["delivery_day"], c["buyer_kind"] != "trader"))

        for contract in contracts:
            farmer_id = contract["seller_id"]
            item_id = contract["item_id"]
            farmer = self.farmers[farmer_id]
            available = farmer["inventory"][item_id]
            incentive_multiplier = self._contract_incentive_multiplier(contract)
            deliverable = min(
                contract["remaining_qty"],
                int(available * self._fulfillment_limit(farmer_id) * incentive_multiplier),
            )
            if deliverable > 0:
                farmer["inventory"][item_id] -= deliverable
                farmer["cash"] += deliverable * contract["unit_price"]
                contract["remaining_qty"] -= deliverable

            refund = 0.0
            if contract["buyer_kind"] == "roaster":
                self.inventory[item_id] += deliverable
                if contract["delivery_day"] < today and contract["remaining_qty"] > 0:
                    refund_ratio = min(0.95, max(0.0, self._farmer_metrics(farmer_id)["recent_cash_ratio"]))
                    refund = contract["remaining_qty"] * contract["unit_price"] * refund_ratio
                    self.cash += refund
                    farmer["cash"] = max(0.0, farmer["cash"] - refund)
                    contract["closed"] = True
                    events.append(
                        f"{farmer_id} missed contract {contract['contract_id']}: delivered {deliverable}/{contract['quantity_kg']} kg "
                        f"of {item_id}, late refund ${refund:.2f}."
                    )
                else:
                    status = "on time" if today <= contract["delivery_day"] else "late"
                    events.append(
                        f"{farmer_id} delivered {deliverable}/{contract['quantity_kg']} kg of {item_id} for {contract['contract_id']} ({status})."
                    )
            else:
                self.trader["inventory"][item_id] += deliverable
                events.append(
                    f"Trader inbound {contract['contract_id']} from {farmer_id}: {deliverable}/{contract['quantity_kg']} kg of {item_id}."
                )

            self._record_delivery(contract, deliverable, today, refund=refund)
            if contract["remaining_qty"] <= 0:
                contract["closed"] = True

        return events

    def _state_lines(self) -> list[str]:
        lines = [
            f"Day {self._current_day_number()} of {TOTAL_DAYS}",
            f"Cash: ${self.cash:,.2f}",
            f"Estimated terminal value: ${self._portfolio_value():,.2f}",
            "",
            "Retail inventory and prices:",
        ]
        for item_id in ITEMS:
            lines.append(
                f"  {item_id}: {self.inventory[item_id]} kg on hand, retail ${self.prices[item_id]:.2f}/kg"
            )

        lines += ["", "Direct farmer spot offers today:"]
        for farmer_id, profile in FARMER_PROFILES.items():
            lines.append(
                f"  {farmer_id}: standard ${self._current_farmer_price(farmer_id, 'standard'):.2f}/kg, "
                f"premium ${self._current_farmer_price(farmer_id, 'premium'):.2f}/kg"
            )
            lines.append(f"    Public note: {profile['public_note']}")
            if farmer_id in self.investigated_farmers:
                metrics = self._farmer_metrics(farmer_id)
                lines.append(
                    "    Revealed metrics: "
                    f"delivery_rate={metrics['delivery_rate']:.2f}, "
                    f"on_time_delivery_rate={metrics['on_time_delivery_rate']:.2f}, "
                    f"partial_delivery_rate={metrics['partial_delivery_rate']:.2f}, "
                    f"contract_coverage={metrics['contract_coverage']:.2f}, "
                    f"recent_cash_ratio={metrics['recent_cash_ratio']:.2f}"
                )
            else:
                lines.append("    Revealed metrics: hidden")

        lines += [
            "",
            "Trader desk:",
            f"  {self._current_trader_bulletin()}",
            f"  Trader inventory: standard {self.trader['inventory']['standard']} kg, premium {self.trader['inventory']['premium']} kg",
            f"  Trader offers: standard ${self._current_trader_price('standard'):.2f}/kg, premium ${self._current_trader_price('premium'):.2f}/kg",
            "  Observation policy: only current stock and current offers are visible; future inbound inventory is hidden.",
        ]

        open_forwards = [
            c for c in self.ledger_contracts if c["buyer_kind"] == "roaster" and not c["closed"]
        ]
        if open_forwards:
            lines.append("")
            lines.append("Open direct contracts:")
            for contract in open_forwards:
                lines.append(
                    f"  {contract['contract_id']}: {contract['contract_type']} {contract['seller_id']} -> "
                    f"{contract['quantity_kg']} kg {contract['item_id']} due day {contract['delivery_day']} "
                    f"(remaining {contract['remaining_qty']} kg)"
                )

        if self.history:
            lines.append("")
            lines.append("Previous day summary:")
            last = self.history[-1]
            lines.append(
                f"  Revenue: ${last['revenue']:.2f}, Holding cost: ${last['holding_cost']:.2f}, Profit: ${last['profit']:.2f}"
            )
            for event in last["events"]:
                lines.append(f"  {event}")
        return lines

    @tool
    def view_state(self, params: NoParams) -> ToolOutput:
        """Return cash, inventory, direct offers, trader inventory, and recent history."""
        return ToolOutput(blocks=[TextBlock(text="\n".join(self._state_lines()))], reward=0.0, finished=False)

    @tool
    def investigate_farmer(self, params: InvestigateFarmerInput) -> ToolOutput:
        """Pay to reveal ledger-derived fulfillment and liquidity metrics for one farmer."""
        farmer_id = params.farmer_id
        if farmer_id not in self.farmers:
            return ToolOutput(blocks=[TextBlock(text=f"Unknown farmer_id: {farmer_id}")], reward=0.0, finished=False)
        if farmer_id in self.investigated_farmers:
            metrics = self._farmer_metrics(farmer_id)
            return ToolOutput(
                blocks=[TextBlock(text=f"Farmer already investigated. {metrics}")],
                reward=0.0,
                finished=False,
            )
        if self.cash < INVESTIGATION_COST:
            return ToolOutput(blocks=[TextBlock(text="Insufficient cash to investigate.")], reward=0.0, finished=False)
        self.cash -= INVESTIGATION_COST
        self.metrics["investigation_spend"] += INVESTIGATION_COST
        self.investigated_farmers.add(farmer_id)
        metrics = self._farmer_metrics(farmer_id)
        return ToolOutput(
            blocks=[
                TextBlock(
                    text=(
                        f"Investigation complete for {farmer_id}. "
                        f"delivery_rate={metrics['delivery_rate']:.2f}, "
                        f"on_time_delivery_rate={metrics['on_time_delivery_rate']:.2f}, "
                        f"partial_delivery_rate={metrics['partial_delivery_rate']:.2f}, "
                        f"contract_coverage={metrics['contract_coverage']:.2f}, "
                        f"recent_cash_ratio={metrics['recent_cash_ratio']:.2f}, "
                        f"recent_default_flag={metrics['recent_default_flag']}."
                    )
                )
            ],
            reward=0.0,
            finished=False,
        )

    @tool
    def buy_spot_direct(self, params: BuySpotDirectInput) -> ToolOutput:
        """Prepay a farmer for spot beans that should arrive after advance_day."""
        if params.farmer_id not in self.farmers or params.item_id not in ITEMS:
            return ToolOutput(blocks=[TextBlock(text="Unknown farmer or item.")], reward=0.0, finished=False)
        unit_price = self._current_farmer_price(params.farmer_id, params.item_id)
        total_cost = unit_price * params.quantity_kg
        if total_cost > self.cash:
            return ToolOutput(blocks=[TextBlock(text=f"Insufficient cash. Need ${total_cost:.2f}.")], reward=0.0, finished=False)
        self.cash -= total_cost
        self.metrics["direct_spend"] += total_cost
        contract = self._new_contract(
            buyer_kind="roaster",
            seller_kind="farmer",
            seller_id=params.farmer_id,
            item_id=params.item_id,
            quantity_kg=params.quantity_kg,
            unit_price=unit_price,
            contract_type="spot",
            delivery_day=self._current_day_number(),
        )
        self.ledger_contracts.append(contract)
        return ToolOutput(
            blocks=[TextBlock(text=f"Created spot contract {contract['contract_id']} with {params.farmer_id} for {params.quantity_kg} kg of {params.item_id} at ${unit_price:.2f}/kg.")],
            reward=0.0,
            finished=False,
        )

    @tool
    def create_forward_contract(self, params: ForwardContractInput) -> ToolOutput:
        """Prepay a future direct contract with a farmer for a specified delivery day."""
        if params.farmer_id not in self.farmers or params.item_id not in ITEMS:
            return ToolOutput(blocks=[TextBlock(text="Unknown farmer or item.")], reward=0.0, finished=False)
        if params.delivery_day <= self._current_day_number():
            return ToolOutput(blocks=[TextBlock(text="Forward contracts must target a future day.")], reward=0.0, finished=False)
        unit_price = round(self._current_farmer_price(params.farmer_id, params.item_id) * 0.96, 2)
        total_cost = unit_price * params.quantity_kg
        if total_cost > self.cash:
            return ToolOutput(blocks=[TextBlock(text=f"Insufficient cash. Need ${total_cost:.2f}.")], reward=0.0, finished=False)
        self.cash -= total_cost
        self.metrics["direct_spend"] += total_cost
        contract = self._new_contract(
            buyer_kind="roaster",
            seller_kind="farmer",
            seller_id=params.farmer_id,
            item_id=params.item_id,
            quantity_kg=params.quantity_kg,
            unit_price=unit_price,
            contract_type="forward",
            delivery_day=params.delivery_day,
        )
        self.ledger_contracts.append(contract)
        return ToolOutput(
            blocks=[TextBlock(text=f"Created forward contract {contract['contract_id']} with {params.farmer_id} for day {params.delivery_day}: {params.quantity_kg} kg of {params.item_id} at ${unit_price:.2f}/kg.")],
            reward=0.0,
            finished=False,
        )

    @tool
    def buy_from_trader(self, params: BuyTraderInput) -> ToolOutput:
        """Buy immediate inventory from trader stock at a safer but higher price."""
        if params.item_id not in ITEMS:
            return ToolOutput(blocks=[TextBlock(text=f"Unknown item_id: {params.item_id}")], reward=0.0, finished=False)
        if params.quantity_kg > self.trader["inventory"][params.item_id]:
            return ToolOutput(
                blocks=[TextBlock(text=f"Trader inventory too low. Available {self.trader['inventory'][params.item_id]} kg.")],
                reward=0.0,
                finished=False,
            )
        unit_price = self._current_trader_price(params.item_id)
        total_cost = unit_price * params.quantity_kg
        if total_cost > self.cash:
            return ToolOutput(blocks=[TextBlock(text=f"Insufficient cash. Need ${total_cost:.2f}.")], reward=0.0, finished=False)
        self.cash -= total_cost
        self.metrics["trader_spend"] += total_cost
        self.trader["cash"] += total_cost
        self.trader["inventory"][params.item_id] -= params.quantity_kg
        self.inventory[params.item_id] += params.quantity_kg
        self.trader["committed_sales"] += params.quantity_kg
        return ToolOutput(
            blocks=[TextBlock(text=f"Bought {params.quantity_kg} kg of {params.item_id} from trader at ${unit_price:.2f}/kg. Inventory transferred immediately.")],
            reward=0.0,
            finished=False,
        )

    @tool
    def set_price(self, params: SetPriceInput) -> ToolOutput:
        """Update the retail price for one coffee item for the current day."""
        if params.item_id not in ITEMS:
            return ToolOutput(blocks=[TextBlock(text=f"Unknown item_id: {params.item_id}")], reward=0.0, finished=False)
        self.prices[params.item_id] = round(params.price_per_kg, 2)
        return ToolOutput(blocks=[TextBlock(text=f"Set retail price for {params.item_id} to ${self.prices[params.item_id]:.2f}/kg.")], reward=0.0, finished=False)

    @tool
    def advance_day(self, params: NoParams) -> ToolOutput:
        """Harvest, fulfill due contracts, sell retail demand, and move to the next day."""
        if self.day_index >= TOTAL_DAYS:
            return ToolOutput(blocks=[TextBlock(text="All days are complete. Call finish_episode.")], reward=0.0, finished=False)

        self._harvest_today()
        events = self._process_contracts_for_day()

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
                "events": events,
            }
        )

        lines = [
            f"Finished Day {self._current_day_number()}.",
            f"Revenue: ${total_revenue:.2f}",
            f"Holding cost: ${holding_cost:.2f}",
            f"Profit: ${profit:.2f}",
            "Fulfillment events:",
        ]
        lines.extend(f"  {event}" for event in events or ["  No due contracts today."])
        lines.append("Sales outcomes:")
        for item_id, sold in sold_units.items():
            lines.append(f"  Sold {sold} kg of {item_id}")

        self.day_index += 1
        if self.day_index < TOTAL_DAYS:
            lines.append(f"Next day is Day {self._current_day_number()}.")
        else:
            lines.append("The 7-day horizon is complete. Call finish_episode.")

        return ToolOutput(blocks=[TextBlock(text="\n".join(lines))], reward=0.0, finished=False)

    @tool
    def finish_episode(self, params: NoParams) -> ToolOutput:
        """End the episode, settle remaining value, and return the final reward."""
        open_direct = [
            c for c in self.ledger_contracts if c["buyer_kind"] == "roaster" and not c["closed"]
        ]
        open_contract_recovery = 0.0
        for contract in open_direct:
            recovery = (
                contract["remaining_qty"]
                * contract["unit_price"]
                * TERMINAL_OPEN_CONTRACT_RECOVERY
            )
            open_contract_recovery += recovery
            contract["closed"] = True
        self.cash += open_contract_recovery

        salvage_value = self._inventory_value_at_salvage()
        final_value = self.cash + salvage_value
        profit = final_value - INITIAL_CASH
        reward = max(0.0, min(1.0, 0.5 + (profit / INITIAL_CASH)))
        lines = [
            "Episode finished.",
            f"Cash: ${self.cash:.2f}",
            f"Salvage value: ${salvage_value:.2f}",
            f"Recovery from open direct contracts: ${open_contract_recovery:.2f}",
            f"Final value: ${final_value:.2f}",
            f"Profit: ${profit:+.2f}",
            f"Investigation spend: ${self.metrics['investigation_spend']:.2f}",
            f"Direct spend: ${self.metrics['direct_spend']:.2f}",
            f"Trader spend: ${self.metrics['trader_spend']:.2f}",
            f"Open direct contracts remaining: {len(open_direct)}",
            f"Reward: {reward:.4f}",
        ]
        return ToolOutput(blocks=[TextBlock(text="\n".join(lines))], reward=reward, finished=True)


if __name__ == "__main__":
    Server([MiniCoffeeEnv]).run(port=int(os.getenv("ORS_PORT", "8082")))
