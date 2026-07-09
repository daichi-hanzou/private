from __future__ import annotations

import json
import os
import random
import subprocess
from itertools import count
from pathlib import Path
from uuid import uuid4

from dotenv import load_dotenv
from pydantic import BaseModel, Field

from ors import Environment, Server, Split, TextBlock, ToolOutput, tool

from mini_coffee_event_logger import EventLogger


load_dotenv(Path(__file__).with_name(".env"))

INITIAL_CASH = 1_000.0
TOTAL_DAYS = int(os.getenv("MINI_COFFEE_TOTAL_DAYS", "20"))
DEFAULT_SEED = int(os.getenv("MINI_COFFEE_SEED", "0"))
RUN_SEED = os.getenv("MINI_COFFEE_RUN_SEED")
WARM_START_DAYS = int(os.getenv("MINI_COFFEE_WARM_START_DAYS", "90"))
ENABLE_DEBUG_TOOLS = os.getenv("MINI_COFFEE_DEBUG_TOOLS", "0") == "1"
HOLDING_COST_PER_KG = 0.5
SALVAGE_DISCOUNT = 0.5
INVESTIGATION_COST = 10.0
TRADER_MARGIN = {"standard": 1.8, "premium": 2.2}
TERMINAL_OPEN_CONTRACT_RECOVERY = 0.35
INITIAL_ROASTER_INVENTORY = {"standard": 8, "premium": 3}
DEMAND_SPIKE_COUNT = 2
DEMAND_SPIKE_DAY_MIN = 8
DEMAND_SPIKE_DAY_MAX = 27
DEMAND_SPIKE_MIN_GAP = 5
DEMAND_SPIKE_MULTIPLIER = 2.2
FORECAST_TRUE_POSITIVE_RATE = 0.8
FORECAST_FALSE_POSITIVES_PER_EPISODE = 1
FORECAST_LEAD_DAYS = (3, 1)
MARKET_BULLETIN = "Market bulletin: A local festival may lift coffee demand within the next 3 days."
TRADER_EMERGENCY_POINT = {"standard": 5, "premium": 2}

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
        "fulfillment_reliability": 0.95,
    },
    "andes_mist": {
        "name": "Andes Mist Collective",
        "public_note": "Small high-altitude collective with excellent quality and tight working capital.",
        "spot_prices": {
            "standard": [4.2, 4.2, 4.6, 4.7, 5.0, 5.1, 5.1],
            "premium": [6.8, 6.9, 7.4, 7.6, 8.1, 8.3, 8.3],
        },
        "harvest_schedule": {
            "standard": [3, 3, 4, 5, 4, 3, 3],
            "premium": [3, 4, 4, 5, 5, 3, 3],
        },
        "starting_cash": 65.0,
        "short_term_obligations": 155.0,
        "fulfillment_reliability": 0.55,
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
        "fulfillment_reliability": 0.55,
    },
    "cedar_valley": {
        "name": "Cedar Valley Farm",
        "public_note": "Mid-sized farm with steady standard lots but limited premium capacity.",
        "spot_prices": {
            "standard": [3.9, 4.0, 4.2, 4.4, 4.5, 4.7, 4.7],
            "premium": [7.4, 7.4, 7.8, 8.0, 8.2, 8.4, 8.4],
        },
        "harvest_schedule": {
            "standard": [5, 5, 6, 6, 5, 5, 4],
            "premium": [1, 1, 2, 2, 2, 1, 1],
        },
        "starting_cash": 135.0,
        "short_term_obligations": 130.0,
        "fulfillment_reliability": 0.8,
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
        "fulfillment_reliability": 0.8,
    },
    "sunrock": {
        "name": "Sunrock Producers",
        "public_note": "Lowest visible prices, but public reports mention repeated delivery delays.",
        "spot_prices": {
            "standard": [3.2, 3.3, 3.5, 3.7, 3.9, 4.0, 4.0],
            "premium": [5.8, 5.9, 6.2, 6.4, 6.6, 6.8, 6.8],
        },
        "harvest_schedule": {
            "standard": [7, 7, 8, 8, 7, 6, 6],
            "premium": [1, 1, 1, 2, 2, 1, 1],
        },
        "starting_cash": 45.0,
        "short_term_obligations": 190.0,
        "fulfillment_reliability": 0.55,
    },
    "harbor_roast_supply": {
        "name": "Harbor Roast Supply",
        "public_note": "Export-connected supplier with strong liquidity and higher transparent prices.",
        "spot_prices": {
            "standard": [4.7, 4.7, 4.9, 5.1, 5.3, 5.4, 5.4],
            "premium": [7.9, 8.0, 8.3, 8.5, 8.8, 9.0, 9.0],
        },
        "harvest_schedule": {
            "standard": [6, 6, 6, 7, 7, 6, 6],
            "premium": [3, 3, 3, 4, 4, 3, 3],
        },
        "starting_cash": 260.0,
        "short_term_obligations": 95.0,
        "fulfillment_reliability": 0.95,
    },
    "loma_dorada": {
        "name": "Loma Dorada Estate",
        "public_note": "Prestigious estate with formal awards and concentrated premium harvest windows.",
        "spot_prices": {
            "standard": [4.5, 4.5, 4.8, 4.9, 5.1, 5.2, 5.2],
            "premium": [6.5, 6.6, 7.0, 7.1, 7.4, 7.7, 7.7],
        },
        "harvest_schedule": {
            "standard": [2, 2, 2, 3, 3, 2, 2],
            "premium": [4, 4, 5, 5, 5, 4, 4],
        },
        "starting_cash": 95.0,
        "short_term_obligations": 150.0,
        "fulfillment_reliability": 0.55,
    },
    "norte_azul": {
        "name": "Norte Azul Cooperative",
        "public_note": "Broad cooperative network with moderate prices and diversified producers.",
        "spot_prices": {
            "standard": [4.1, 4.1, 4.3, 4.5, 4.7, 4.8, 4.8],
            "premium": [6.9, 6.9, 7.2, 7.4, 7.6, 7.9, 7.9],
        },
        "harvest_schedule": {
            "standard": [5, 6, 6, 7, 7, 6, 5],
            "premium": [2, 2, 3, 3, 3, 2, 2],
        },
        "starting_cash": 155.0,
        "short_term_obligations": 125.0,
        "fulfillment_reliability": 0.95,
    },
    "rainforest_direct": {
        "name": "Rainforest Direct",
        "public_note": "Young direct-trade venture with limited public track record and thin market coverage.",
        "spot_prices": {
            "standard": [3.6, 3.6, 3.8, 4.0, 4.2, 4.3, 4.3],
            "premium": [6.4, 6.4, 6.7, 6.9, 7.1, 7.3, 7.3],
        },
        "harvest_schedule": {
            "standard": [8, 8, 9, 9, 8, 8, 7],
            "premium": [2, 2, 2, 3, 3, 2, 2],
        },
        "starting_cash": 85.0,
        "short_term_obligations": 180.0,
        "fulfillment_reliability": 0.92,
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


class DebugSetSeedInput(BaseModel):
    seed: int = Field(ge=0)


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
                    f"Operate a coffee roaster-retailer for {TOTAL_DAYS} days. Compare direct spot, "
                    "direct forward, and trader procurement under hidden fulfillment risk."
                ),
            }
        ]

    def get_prompt(self):
        self.total_days = TOTAL_DAYS
        self.seed = getattr(self, "_seed_override", int(RUN_SEED) if RUN_SEED is not None else DEFAULT_SEED)
        self._init_rngs(self.seed)
        self._init_demand_series()
        self.day_index = 0
        self.run_id = uuid4().hex[:8]
        self.cash = INITIAL_CASH
        self.inventory = dict(INITIAL_ROASTER_INVENTORY)
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
        self.last_shortfall_event = False
        self.spike_stockout_days = 0
        self.spike_days_elapsed = 0

        self.farmers = {}
        for farmer_id, profile in FARMER_PROFILES.items():
            self.farmers[farmer_id] = {
                "cash": profile["starting_cash"],
                "short_term_obligations": profile["short_term_obligations"],
                "inventory": {item_id: 0 for item_id in ITEMS},
                "harvest_schedule": profile["harvest_schedule"],
                "fulfillment_reliability": profile.get("fulfillment_reliability", 1.0),
                "defaulted": False,
            }

        self.trader = {
            "cash": 700.0,
            "inventory": {"standard": 0, "premium": 0},
            "inbound_contracts": [],
            "committed_sales": 0,
        }
        self._warm_start_history()
        self.day_index = 0
        self.cash = INITIAL_CASH
        self.inventory = dict(INITIAL_ROASTER_INVENTORY)
        self.prices = {
            item_id: item["default_price"] for item_id, item in ITEMS.items()
        }
        self.history = []
        self.investigated_farmers = set()
        self.metrics = {
            "investigation_spend": 0.0,
            "direct_spend": 0.0,
            "trader_spend": 0.0,
        }
        self.last_shortfall_event = False
        self.spike_stockout_days = 0
        self.spike_days_elapsed = 0
        self._init_event_logger()

        lines = [
            f"You run a small coffee roaster-retailer for {TOTAL_DAYS} days.",
            f"Initial cash: ${INITIAL_CASH:,.2f}",
            f"Initial inventory: standard {self.inventory['standard']} kg, premium {self.inventory['premium']} kg",
            "",
            "You can procure beans in three ways:",
            "1. Direct spot purchase from a farmer: cheaper, delivered after today's advance_day.",
            "2. Direct forward contract with a farmer: reserve future delivery, but fulfillment is uncertain.",
            "3. Buy from the trader: immediate inventory transfer from trader stock at a markup.",
            "",
            f"Farmer internal state is hidden by default. Investigating a farmer costs ${INVESTIGATION_COST:.0f} "
            f"and reveals {WARM_START_DAYS} days of ledger-based fulfillment metrics with sample counts.",
            "The trader already sees the full farmer ledger and maintains its own inbound contracts and inventory.",
            "You do not see future harvest schedules, contract break probabilities, or future trader inbound deliveries.",
            "Demand is usually stable, but occasional festival days can multiply demand.",
            "Market bulletins may give imprecise advance notice of such days.",
            "On early turns, do not wait passively. Take at least one concrete action such as viewing state, investigating, procuring, or repricing.",
            "",
            "Initial public farmer directory:",
        ]
        for farmer_id, profile in FARMER_PROFILES.items():
            lines.append(
                f"  {farmer_id} ({profile['name']}): {profile['public_note']} "
                f"Current standard ${self._current_farmer_price(farmer_id, 'standard'):.2f}/kg, "
                f"premium ${self._current_farmer_price(farmer_id, 'premium'):.2f}/kg."
            )
        lines += [
            "",
            "Tools available:",
            "  view_state             - inspect cash, inventory, offers, trader inventory, and history",
            f"  investigate_farmer     - pay ${INVESTIGATION_COST:.0f} to reveal one farmer's {WARM_START_DAYS}-day fulfillment and liquidity metrics",
            "  buy_spot_direct        - buy spot from a farmer for next-step delivery",
            "  create_forward_contract - lock a future direct delivery day with a farmer",
            "  buy_from_trader        - buy immediate guaranteed inventory from trader stock",
            "  set_price              - set today's retail price for one item",
            "  advance_day            - harvest, fulfill contracts, sell retail demand, and charge carrying costs",
            "  finish_episode         - end the run and compute reward",
        ]
        self.event_logger.emit(
            "run_start",
            run_id=self.run_id,
            runtime_config=self._runtime_config_snapshot(),
            snapshot=self._snapshot(),
        )
        return [TextBlock(text="\n".join(lines))]

    def _runtime_config_snapshot(self) -> dict:
        return {
            "total_days": TOTAL_DAYS,
            "disable_incentive": os.getenv("MINI_COFFEE_DISABLE_INCENTIVE", "0"),
            "investigation_cost": INVESTIGATION_COST,
            "warm_start_days": WARM_START_DAYS,
            "debug_tools": ENABLE_DEBUG_TOOLS,
            "seed": self.seed,
            "demand_spike": {
                "count": DEMAND_SPIKE_COUNT,
                "day_min": DEMAND_SPIKE_DAY_MIN,
                "day_max": DEMAND_SPIKE_DAY_MAX,
                "min_gap": DEMAND_SPIKE_MIN_GAP,
                "multiplier": DEMAND_SPIKE_MULTIPLIER,
                "forecast_true_positive_rate": FORECAST_TRUE_POSITIVE_RATE,
                "forecast_false_positives_per_episode": FORECAST_FALSE_POSITIVES_PER_EPISODE,
                "forecast_lead_days": FORECAST_LEAD_DAYS,
            },
            "git_commit": self._git_commit_hash(),
        }

    def _git_commit_hash(self) -> str:
        env_commit = os.getenv("MINI_COFFEE_GIT_COMMIT")
        if env_commit:
            return env_commit
        try:
            return subprocess.check_output(
                ["git", "rev-parse", "HEAD"],
                cwd=Path(__file__).resolve().parent,
                text=True,
                stderr=subprocess.DEVNULL,
            ).strip()
        except Exception:
            return "unknown"

    def _init_rngs(self, seed: int) -> None:
        seed_source = random.Random(seed)
        self.rng_demand = random.Random(seed_source.randrange(2**63))
        self.rng_harvest = random.Random(seed_source.randrange(2**63))
        self.rng_fulfillment = random.Random(seed_source.randrange(2**63))
        self.rng_spike = random.Random(seed_source.randrange(2**63))

    def _init_demand_series(self) -> None:
        self.demand_series = {
            item_id: [
                round(1.0 + self.rng_demand.uniform(-0.05, 0.05), 4)
                for _ in range(TOTAL_DAYS)
            ]
            for item_id in ITEMS
        }
        self._init_demand_spikes()

    def _init_demand_spikes(self) -> None:
        upper_day = min(DEMAND_SPIKE_DAY_MAX, TOTAL_DAYS)
        candidates = list(range(DEMAND_SPIKE_DAY_MIN, upper_day + 1))
        valid_pairs = [
            (a, b)
            for idx, a in enumerate(candidates)
            for b in candidates[idx + 1 :]
            if b - a >= DEMAND_SPIKE_MIN_GAP
        ]
        if DEMAND_SPIKE_COUNT != 2 or not valid_pairs:
            self.spike_days = []
            self.forecast_days = set()
            self.true_positive_forecast_days = set()
            self.false_positive_forecast_days = set()
            return

        self.spike_days = list(valid_pairs[self.rng_spike.randrange(len(valid_pairs))])
        forecast_days: set[int] = set()
        true_positive_days: set[int] = set()
        for spike_day in self.spike_days:
            if self.rng_spike.random() <= FORECAST_TRUE_POSITIVE_RATE:
                for lead_days in FORECAST_LEAD_DAYS:
                    forecast_day = spike_day - lead_days
                    if 1 <= forecast_day <= TOTAL_DAYS:
                        forecast_days.add(forecast_day)
                        true_positive_days.add(forecast_day)

        false_candidates = [
            day
            for day in range(1, TOTAL_DAYS + 1)
            if day not in forecast_days
            and all(not (day < spike_day <= day + max(FORECAST_LEAD_DAYS)) for spike_day in self.spike_days)
        ]
        false_positive_days: set[int] = set()
        for _ in range(FORECAST_FALSE_POSITIVES_PER_EPISODE):
            if not false_candidates:
                break
            index = self.rng_spike.randrange(len(false_candidates))
            false_positive_days.add(false_candidates.pop(index))

        self.forecast_days = forecast_days | false_positive_days
        self.true_positive_forecast_days = true_positive_days
        self.false_positive_forecast_days = false_positive_days

    def _warm_start_history(self) -> None:
        if WARM_START_DAYS <= 0:
            self.trader["inventory"] = {"standard": 8, "premium": 3}
            return

        farmer_ids = list(self.farmers)
        dummy_rng = random.Random(self.seed + 10_000)
        original_cash = self.cash
        original_inventory = dict(self.inventory)
        original_prices = dict(self.prices)
        original_history = list(self.history)
        original_farmer_cash = {fid: farmer["cash"] for fid, farmer in self.farmers.items()}

        self.cash = 0.0
        self.inventory = {item_id: 0 for item_id in ITEMS}
        self.prices = {
            item_id: item["default_price"] for item_id, item in ITEMS.items()
        }
        self.history = []

        for day in range(WARM_START_DAYS):
            self.day_index = day
            for item_id in ITEMS:
                farmer_id = farmer_ids[dummy_rng.randrange(len(farmer_ids))]
                qty = 2
                contract = self._new_contract(
                    buyer_kind="history",
                    seller_kind="farmer",
                    seller_id=farmer_id,
                    item_id=item_id,
                    quantity_kg=qty,
                    unit_price=self._current_farmer_price(farmer_id, item_id),
                    contract_type="warm_start",
                    delivery_day=self._current_day_number(),
                )
                contract["historical"] = True
                self.ledger_contracts.append(contract)

            trader_farmer = max(
                farmer_ids,
                key=lambda fid: (
                    self.farmers[fid]["fulfillment_reliability"],
                    -self._current_farmer_price(fid, "standard"),
                ),
            )
            trader_contract = self._new_contract(
                buyer_kind="trader",
                seller_kind="farmer",
                seller_id=trader_farmer,
                item_id="standard" if day % 2 == 0 else "premium",
                quantity_kg=3,
                unit_price=self._current_farmer_price(
                    trader_farmer, "standard" if day % 2 == 0 else "premium"
                ),
                contract_type="warm_start",
                delivery_day=self._current_day_number(),
            )
            trader_contract["historical"] = True
            self.trader["inbound_contracts"].append(trader_contract)
            self.ledger_contracts.append(trader_contract)

            self._harvest_today()
            self._process_contracts_for_day(visible_events=False)

        self.cash = original_cash
        self.inventory = original_inventory
        self.prices = original_prices
        self.history = original_history
        for farmer_id, cash in original_farmer_cash.items():
            self.farmers[farmer_id]["cash"] = cash
            self.farmers[farmer_id]["inventory"] = {item_id: 0 for item_id in ITEMS}
        self.trader["inventory"]["standard"] = min(max(self.trader["inventory"]["standard"], 8), 30)
        self.trader["inventory"]["premium"] = min(max(self.trader["inventory"]["premium"], 3), 12)

    def _init_event_logger(self) -> None:
        base_dir = Path(__file__).resolve().parent / "workspace" / "output" / "mini_coffee_runs"
        log_dir = Path(os.getenv("MINI_COFFEE_LOG_DIR", str(base_dir)))
        log_dir.mkdir(parents=True, exist_ok=True)
        self.log_path = log_dir / f"mini_coffee_{self.run_id}.jsonl"
        self.event_logger = EventLogger(str(self.log_path))

    def _seed_trader_contracts(self) -> None:
        plans = [
            ("sierra_verde", "standard", 6, 2, 4.2),
            ("cloud_peak", "premium", 4, 3, 6.9),
            ("riverbend", "standard", 8, 4, 4.0),
            ("sierra_verde", "premium", 3, 5, 7.9),
            ("riverbend", "standard", 6, 6, 4.3),
            ("harbor_roast_supply", "standard", 7, 7, 5.0),
            ("norte_azul", "premium", 3, 8, 7.4),
            ("cedar_valley", "standard", 6, 9, 4.6),
            ("loma_dorada", "premium", 4, 10, 7.3),
            ("cedar_valley", "standard", 5, 11, 4.7),
            ("rainforest_direct", "standard", 8, 12, 4.2),
            ("andes_mist", "premium", 4, 13, 7.8),
            ("sunrock", "standard", 7, 14, 4.0),
            ("harbor_roast_supply", "premium", 3, 15, 8.8),
            ("norte_azul", "standard", 7, 16, 4.7),
            ("loma_dorada", "premium", 3, 17, 7.7),
            ("cedar_valley", "standard", 6, 18, 4.7),
            ("rainforest_direct", "standard", 8, 19, 4.3),
            ("harbor_roast_supply", "standard", 7, 20, 5.4),
        ]
        plans = [plan for plan in plans if plan[3] <= TOTAL_DAYS]
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
        return series[min(self.day_index, len(series) - 1)]

    def _current_farmer_price(self, farmer_id: str, item_id: str) -> float:
        return self._series_value(FARMER_PROFILES[farmer_id]["spot_prices"][item_id])

    def _current_trader_bulletin(self) -> str:
        return TRADER_BULLETINS[min(self.day_index, len(TRADER_BULLETINS) - 1)]

    def _forecast_active(self) -> bool:
        return self._current_day_number() in getattr(self, "forecast_days", set())

    def _current_market_bulletins(self) -> list[str]:
        return [MARKET_BULLETIN] if self._forecast_active() else []

    def _demand_spike_multiplier(self) -> float:
        return DEMAND_SPIKE_MULTIPLIER if self._current_day_number() in getattr(self, "spike_days", []) else 1.0

    def _days_to_next_spike(self) -> int | None:
        today = self._current_day_number()
        future_spikes = [day for day in getattr(self, "spike_days", []) if day >= today]
        if not future_spikes:
            return None
        return min(future_spikes) - today

    def _current_trader_price(self, item_id: str) -> float:
        direct_prices = [self._current_farmer_price(fid, item_id) for fid in self.farmers]
        base_price = sum(direct_prices) / len(direct_prices) + TRADER_MARGIN[item_id]
        on_hand = self.trader["inventory"][item_id]
        scarcity_markup = max(0.0, 6 - on_hand) * 0.18
        demand_pressure = self._series_value(ITEMS[item_id]["festival_boosts"]) - 1.0
        return round(base_price + scarcity_markup + demand_pressure * 0.4, 2)

    def _expected_remaining_harvest(self, farmer_id: str, item_id: str) -> int:
        schedule = self.farmers[farmer_id]["harvest_schedule"][item_id]
        return sum(schedule[min(day, len(schedule) - 1)] for day in range(self.day_index, TOTAL_DAYS))

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

    def _trader_value_analysis(self) -> dict[str, float]:
        roaster_contracts = [
            c for c in self.ledger_contracts if c["buyer_kind"] == "roaster"
        ]
        roaster_deliveries = [
            d for d in self.ledger_deliveries if d["buyer_kind"] == "roaster"
        ]
        direct_ordered_qty = sum(c["quantity_kg"] for c in roaster_contracts)
        direct_delivered_qty = sum(d["delivered_qty"] for d in roaster_deliveries)
        direct_shortfall_qty = max(0, direct_ordered_qty - direct_delivered_qty)
        direct_late_qty = sum(d["delivered_qty"] for d in roaster_deliveries if not d["on_time"])
        direct_refund_value = sum(d["refund"] for d in roaster_deliveries)

        trader_guaranteed_qty = float(self.trader["committed_sales"])
        trader_purchase_share = (
            trader_guaranteed_qty / max(1.0, trader_guaranteed_qty + direct_delivered_qty)
        )
        direct_fill_rate = direct_delivered_qty / max(1, direct_ordered_qty)
        risk_transfer_proxy_qty = min(trader_guaranteed_qty, float(direct_shortfall_qty))
        avoided_stockout_proxy_qty = max(0.0, trader_guaranteed_qty - direct_shortfall_qty)

        return {
            "direct_ordered_qty": float(direct_ordered_qty),
            "direct_delivered_qty": float(direct_delivered_qty),
            "direct_shortfall_qty": float(direct_shortfall_qty),
            "direct_late_qty": float(direct_late_qty),
            "direct_fill_rate": float(direct_fill_rate),
            "direct_refund_value": float(direct_refund_value),
            "trader_guaranteed_qty": float(trader_guaranteed_qty),
            "trader_purchase_share": float(trader_purchase_share),
            "risk_transfer_proxy_qty": float(risk_transfer_proxy_qty),
            "avoided_stockout_proxy_qty": float(avoided_stockout_proxy_qty),
        }

    def _snapshot(self) -> dict:
        return {
            "day": self._current_day_number(),
            "cash": round(self.cash, 2),
            "estimated_terminal_value": round(self._portfolio_value(), 2),
            "inventory": dict(self.inventory),
            "prices": dict(self.prices),
            "trader_inventory": dict(self.trader["inventory"]),
            "metrics": {k: round(v, 2) for k, v in self.metrics.items()},
            "forecast_active": self._forecast_active(),
            "days_to_next_spike": self._days_to_next_spike(),
            "demand_spike_today": self._demand_spike_multiplier() > 1.0,
            "open_direct_contracts": sum(
                1
                for c in self.ledger_contracts
                if c["buyer_kind"] == "roaster" and not c["closed"]
            ),
        }

    def _sales_for_item(self, item_id: str) -> tuple[int, float, int]:
        item = ITEMS[item_id]
        price_factor = max(0.0, 1.0 - (self.prices[item_id] / item["reservation_price"]))
        demand_noise = getattr(self, "demand_series", {}).get(item_id, [1.0])[
            min(self.day_index, TOTAL_DAYS - 1)
        ]
        demand = int(
            round(
                item["base_demand"]
                * self._series_value(item["festival_boosts"])
                * self._demand_spike_multiplier()
                * demand_noise
                * price_factor
            )
        )
        sold = min(self.inventory[item_id], demand)
        return sold, sold * self.prices[item_id], demand

    def _record_delivery(
        self,
        contract: dict,
        delivered_qty: int,
        processed_day: int,
        refund: float = 0.0,
        reason: str | None = None,
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
                "shortfall_reason": reason,
            }
        )

    def _farmer_metrics(self, farmer_id: str) -> dict[str, float | int | bool | None]:
        contracts = [
            c
            for c in self.ledger_contracts
            if c["seller_kind"] == "farmer"
            and c["seller_id"] == farmer_id
            and (c.get("historical") or c["delivery_day"] <= self._current_day_number())
        ]
        deliveries = [d for d in self.ledger_deliveries if d["seller_id"] == farmer_id]

        contracted_qty = sum(c["quantity_kg"] for c in contracts)
        delivered_qty = sum(d["delivered_qty"] for d in deliveries)
        on_time_delivered_qty = sum(d["delivered_qty"] for d in deliveries if d["on_time"])
        matured_count = len(contracts)
        sample_count = matured_count
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
            "sample_count": sample_count,
            "contracted_qty": contracted_qty,
            "delivery_rate": (
                delivered_qty / contracted_qty if contracted_qty and sample_count >= 5 else None
            ),
            "on_time_delivery_rate": (
                on_time_delivered_qty / contracted_qty if contracted_qty and sample_count >= 5 else None
            ),
            "partial_delivery_rate": partial_count / matured_count if matured_count else 0.0,
            "contract_coverage": (
                outstanding_commitments / max(1, remaining_harvest + sum(self.farmers[farmer_id]["inventory"].values()))
            ),
            "recent_cash_ratio": recent_cash_ratio,
            "recent_default_flag": partial_count > 0 and delivered_qty < contracted_qty,
        }

    def _format_rate_metric(self, label: str, metrics: dict, key: str) -> str:
        value = metrics[key]
        sample_count = metrics["sample_count"]
        if value is None:
            return f"{label}=unknown (only {sample_count} historical contracts)"
        return f"{label}={value:.2f} (n={sample_count})"

    def _fulfillment_limit(self, farmer_id: str) -> float:
        metrics = self._farmer_metrics(farmer_id)
        cash_ratio = float(metrics["recent_cash_ratio"])
        coverage = float(metrics["contract_coverage"])
        base_limit = max(0.25, min(1.0, 0.7 + 0.15 * cash_ratio - 0.1 * max(0.0, coverage - 1.0)))
        reliability = float(self.farmers[farmer_id].get("fulfillment_reliability", 1.0))
        return max(0.02, min(1.0, base_limit * reliability))

    def _contract_incentive_multiplier(self, contract: dict) -> float:
        if os.getenv("MINI_COFFEE_DISABLE_INCENTIVE", "0") == "1":
            return 1.0
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
                farmer["inventory"][item_id] += self._series_value(farmer["harvest_schedule"][item_id])

    def _process_contracts_for_day(self, visible_events: bool = True) -> list[str]:
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
            physical_limit = min(contract["remaining_qty"], int(available))
            reason = None
            reliability = self._fulfillment_limit(farmer_id) * incentive_multiplier
            if farmer.get("defaulted"):
                deliverable = 0
                reason = "financial_default"
            elif self.rng_fulfillment.random() > reliability:
                deliverable = min(physical_limit, int(contract["remaining_qty"] * 0.1))
                reason = "nonperformance"
            else:
                deliverable = physical_limit
                if deliverable < contract["remaining_qty"]:
                    reason = "capacity_shortfall"
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
                    if visible_events:
                        events.append(
                            f"{farmer_id} missed contract {contract['contract_id']}: delivered {deliverable}/{contract['quantity_kg']} kg "
                            f"of {item_id}, late refund ${refund:.2f}."
                        )
                else:
                    status = "on time" if today <= contract["delivery_day"] else "late"
                    if visible_events:
                        events.append(
                            f"{farmer_id} delivered {deliverable}/{contract['quantity_kg']} kg of {item_id} for {contract['contract_id']} ({status})."
                        )
            elif contract["buyer_kind"] == "trader":
                self.trader["inventory"][item_id] += deliverable
                if visible_events:
                    events.append(
                        f"Trader inbound {contract['contract_id']} from {farmer_id}: {deliverable}/{contract['quantity_kg']} kg of {item_id}."
                    )

            self._record_delivery(contract, deliverable, today, refund=refund, reason=reason)
            if contract.get("historical"):
                contract["closed"] = True
                contract["remaining_qty"] = 0
            elif contract["remaining_qty"] <= 0:
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
                    f"{self._format_rate_metric('delivery_rate', metrics, 'delivery_rate')}, "
                    f"{self._format_rate_metric('on_time_delivery_rate', metrics, 'on_time_delivery_rate')}, "
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
        for bulletin in self._current_market_bulletins():
            lines.append(f"  {bulletin}")

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
                blocks=[
                    TextBlock(
                        text=(
                            "Farmer already investigated. "
                            f"{self._format_rate_metric('delivery_rate', metrics, 'delivery_rate')}, "
                            f"{self._format_rate_metric('on_time_delivery_rate', metrics, 'on_time_delivery_rate')}, "
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
        if self.cash < INVESTIGATION_COST:
            return ToolOutput(blocks=[TextBlock(text="Insufficient cash to investigate.")], reward=0.0, finished=False)
        self.cash -= INVESTIGATION_COST
        self.metrics["investigation_spend"] += INVESTIGATION_COST
        self.investigated_farmers.add(farmer_id)
        metrics = self._farmer_metrics(farmer_id)
        self.event_logger.emit(
            "tool_event",
            tool="investigate_farmer",
            farmer_id=farmer_id,
            cost=INVESTIGATION_COST,
            revealed_metrics=metrics,
            snapshot=self._snapshot(),
        )
        return ToolOutput(
            blocks=[
                TextBlock(
                    text=(
                        f"Investigation complete for {farmer_id}. "
                        f"{self._format_rate_metric('delivery_rate', metrics, 'delivery_rate')}, "
                        f"{self._format_rate_metric('on_time_delivery_rate', metrics, 'on_time_delivery_rate')}, "
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
        self.event_logger.emit(
            "tool_event",
            tool="buy_spot_direct",
            farmer_id=params.farmer_id,
            item_id=params.item_id,
            quantity_kg=params.quantity_kg,
            unit_price=unit_price,
            total_cost=round(total_cost, 2),
            contract_id=contract["contract_id"],
            snapshot=self._snapshot(),
        )
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
        self.event_logger.emit(
            "tool_event",
            tool="create_forward_contract",
            farmer_id=params.farmer_id,
            item_id=params.item_id,
            quantity_kg=params.quantity_kg,
            delivery_day=params.delivery_day,
            unit_price=unit_price,
            total_cost=round(total_cost, 2),
            contract_id=contract["contract_id"],
            snapshot=self._snapshot(),
        )
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
        inventory_at_purchase = self.inventory[params.item_id]
        active_forecast = self._forecast_active()
        days_to_next_spike = self._days_to_next_spike()
        after_shortfall_event = self.last_shortfall_event
        self.cash -= total_cost
        self.metrics["trader_spend"] += total_cost
        self.trader["cash"] += total_cost
        self.trader["inventory"][params.item_id] -= params.quantity_kg
        self.inventory[params.item_id] += params.quantity_kg
        self.trader["committed_sales"] += params.quantity_kg
        self.event_logger.emit(
            "tool_event",
            tool="buy_from_trader",
            item_id=params.item_id,
            quantity_kg=params.quantity_kg,
            unit_price=unit_price,
            total_cost=round(total_cost, 2),
            inventory_at_purchase=inventory_at_purchase,
            active_forecast=active_forecast,
            days_to_next_spike=days_to_next_spike,
            after_shortfall_event=after_shortfall_event,
            proactive_purchase=active_forecast and inventory_at_purchase > TRADER_EMERGENCY_POINT[params.item_id],
            reactive_purchase=after_shortfall_event or inventory_at_purchase <= TRADER_EMERGENCY_POINT[params.item_id],
            snapshot=self._snapshot(),
        )
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
        self.event_logger.emit(
            "tool_event",
            tool="set_price",
            item_id=params.item_id,
            price_per_kg=self.prices[params.item_id],
            snapshot=self._snapshot(),
        )
        return ToolOutput(blocks=[TextBlock(text=f"Set retail price for {params.item_id} to ${self.prices[params.item_id]:.2f}/kg.")], reward=0.0, finished=False)

    @tool
    def advance_day(self, params: NoParams) -> ToolOutput:
        """Harvest, fulfill due contracts, sell retail demand, and move to the next day."""
        if self.day_index >= TOTAL_DAYS:
            return ToolOutput(blocks=[TextBlock(text="All days are complete. Call finish_episode.")], reward=0.0, finished=False)

        self._harvest_today()
        events = self._process_contracts_for_day()

        sold_units = {}
        demand_units = {}
        stockout_items = []
        total_revenue = 0.0
        for item_id in ITEMS:
            sold, revenue, demand = self._sales_for_item(item_id)
            sold_units[item_id] = sold
            demand_units[item_id] = demand
            if sold < demand:
                stockout_items.append(item_id)
            total_revenue += revenue
            self.inventory[item_id] -= sold

        stockout_event = bool(stockout_items)
        spike_day = self._demand_spike_multiplier() > 1.0
        if spike_day:
            self.spike_days_elapsed += 1
            if stockout_event:
                self.spike_stockout_days += 1
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
                "demand_units": demand_units,
                "stockout_items": list(stockout_items),
                "demand_spike_day": spike_day,
                "events": events,
            }
        )
        self.event_logger.emit(
            "day_end",
            day=self._current_day_number(),
            revenue=round(total_revenue, 2),
            holding_cost=round(holding_cost, 2),
            profit=round(profit, 2),
            sold_units=sold_units,
            demand_units=demand_units,
            stockout_items=stockout_items,
            shortfall_event=stockout_event,
            demand_spike_day=spike_day,
            demand_spike_multiplier=self._demand_spike_multiplier(),
            events=events,
            snapshot=self._snapshot(),
        )
        self.last_shortfall_event = stockout_event

        lines = [
            f"Finished Day {self._current_day_number()}.",
            f"Revenue: ${total_revenue:.2f}",
            f"Holding cost: ${holding_cost:.2f}",
            f"Profit: ${profit:.2f}",
            "Fulfillment events:",
        ]
        for bulletin in self._current_market_bulletins():
            lines.insert(1, bulletin)
        lines.extend(f"  {event}" for event in events or ["  No due contracts today."])
        lines.append("Sales outcomes:")
        for item_id, sold in sold_units.items():
            lines.append(f"  Sold {sold}/{demand_units[item_id]} kg of {item_id}")

        self.day_index += 1
        if self.day_index < TOTAL_DAYS:
            lines.append(f"Next day is Day {self._current_day_number()}.")
        else:
            lines.append(f"The {TOTAL_DAYS}-day horizon is complete. Call finish_episode.")

        return ToolOutput(blocks=[TextBlock(text="\n".join(lines))], reward=0.0, finished=False)

    @tool
    def finish_episode(self, params: NoParams) -> ToolOutput:
        """End the episode, settle remaining value, and return the final reward."""
        if self.day_index < TOTAL_DAYS:
            return ToolOutput(
                blocks=[
                    TextBlock(
                        text=(
                            f"finish_episode is only available after Day {TOTAL_DAYS} is complete. "
                            "Keep using advance_day until the horizon ends."
                        )
                    )
                ],
                reward=0.0,
                finished=False,
            )
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
        trader_value = self._trader_value_analysis()
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
            f"Spike stockout days: {self.spike_stockout_days}/{max(self.spike_days_elapsed, 1)}",
            f"Open direct contracts remaining: {len(open_direct)}",
            f"Reward: {reward:.4f}",
            "",
            "Trader value analysis:",
            f"Direct fill rate: {trader_value['direct_fill_rate']:.2%}",
            f"Direct shortfall qty: {trader_value['direct_shortfall_qty']:.0f} kg",
            f"Direct late qty: {trader_value['direct_late_qty']:.0f} kg",
            f"Direct refund value: ${trader_value['direct_refund_value']:.2f}",
            f"Trader guaranteed qty: {trader_value['trader_guaranteed_qty']:.0f} kg",
            f"Trader purchase share: {trader_value['trader_purchase_share']:.2%}",
            f"Risk transferred to trader (proxy): {trader_value['risk_transfer_proxy_qty']:.0f} kg",
            f"Potential stockout buffer from trader (proxy): {trader_value['avoided_stockout_proxy_qty']:.0f} kg",
            f"Log path: {self.log_path}",
        ]
        self.event_logger.emit(
            "run_end",
            run_id=self.run_id,
            salvage_value=round(salvage_value, 2),
            recovery_from_open_direct_contracts=round(open_contract_recovery, 2),
            final_value=round(final_value, 2),
            profit=round(profit, 2),
            reward=round(reward, 4),
            spike_stockout_days=self.spike_stockout_days,
            spike_days_elapsed=self.spike_days_elapsed,
            trader_value_analysis={k: round(v, 4) for k, v in trader_value.items()},
            snapshot=self._snapshot(),
            log_path=str(self.log_path),
        )
        self.event_logger.close()
        return ToolOutput(blocks=[TextBlock(text="\n".join(lines))], reward=reward, finished=True)

    @tool
    def debug_set_seed(self, params: DebugSetSeedInput) -> ToolOutput:
        """[calibration harness only] Reset the whole world with a specific seed."""
        if not ENABLE_DEBUG_TOOLS:
            return ToolOutput(blocks=[TextBlock(text="Debug tools are disabled.")], reward=0.0, finished=False)
        if hasattr(self, "event_logger"):
            self.event_logger.close()
        self._seed_override = params.seed
        self.get_prompt()
        return ToolOutput(
            blocks=[TextBlock(text=f"World reset with seed {params.seed}.")],
            reward=0.0,
            finished=False,
        )

    @tool
    def debug_get_true_state(self, params: NoParams) -> ToolOutput:
        """[calibration harness only] Reveal true hidden farmer state. Never expose to LLM runs."""
        if not ENABLE_DEBUG_TOOLS:
            return ToolOutput(blocks=[TextBlock(text="Debug tools are disabled.")], reward=0.0, finished=False)
        payload = {
            farmer_id: {
                "fulfillment_reliability": farmer["fulfillment_reliability"],
                "cash": round(farmer["cash"], 2),
                "short_term_obligations": farmer["short_term_obligations"],
                "defaulted": farmer["defaulted"],
            }
            for farmer_id, farmer in self.farmers.items()
        }
        return ToolOutput(
            blocks=[TextBlock(text=json.dumps(payload))],
            reward=0.0,
            finished=False,
        )


if __name__ == "__main__":
    Server([MiniCoffeeEnv]).run(port=int(os.getenv("ORS_PORT", "8082")))
