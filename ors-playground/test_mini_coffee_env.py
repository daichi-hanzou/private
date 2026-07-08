import json
import unittest
from itertools import count

from mini_coffee_env import (
    FARMER_PROFILES,
    INITIAL_CASH,
    INITIAL_ROASTER_INVENTORY,
    ITEMS,
    INVESTIGATION_COST,
    DebugSetSeedInput,
    InvestigateFarmerInput,
    MiniCoffeeEnv,
    NoParams,
    TOTAL_DAYS,
    WARM_START_DAYS,
)
import mini_coffee_env


def make_env(seed: int = 0, warm_days: int = 90) -> MiniCoffeeEnv:
    env = MiniCoffeeEnv()
    env.seed = seed
    env._init_rngs(seed)
    env._init_demand_series()
    env.day_index = 0
    env.run_id = "test"
    env.cash = INITIAL_CASH
    env.inventory = dict(INITIAL_ROASTER_INVENTORY)
    env.prices = {
        item_id: item["default_price"] for item_id, item in ITEMS.items()
    }
    env.history = []
    env.investigated_farmers = set()
    env.contract_id_counter = count(1)
    env.ledger_contracts = []
    env.ledger_deliveries = []
    env.pending_direct_contracts = []
    env.metrics = {
        "investigation_spend": 0.0,
        "direct_spend": 0.0,
        "trader_spend": 0.0,
    }
    env.farmers = {}
    for farmer_id, profile in FARMER_PROFILES.items():
        env.farmers[farmer_id] = {
            "cash": profile["starting_cash"],
            "short_term_obligations": profile["short_term_obligations"],
            "inventory": {item_id: 0 for item_id in ITEMS},
            "harvest_schedule": profile["harvest_schedule"],
            "fulfillment_reliability": profile.get("fulfillment_reliability", 1.0),
            "defaulted": False,
        }
    env.trader = {
        "cash": 700.0,
        "inventory": {"standard": 0, "premium": 0},
        "inbound_contracts": [],
        "committed_sales": 0,
    }
    if warm_days:
        env._warm_start_history()
        env.day_index = 0
    return env


class MiniCoffeeEnvTest(unittest.TestCase):
    def test_zero_sample_rate_is_unknown(self):
        env = make_env(warm_days=0)
        env._init_event_logger()
        metrics = env._farmer_metrics("sierra_verde")
        output = env.investigate_farmer(InvestigateFarmerInput(farmer_id="sierra_verde")).blocks[0].text
        env.event_logger.close()

        self.assertEqual(metrics["sample_count"], 0)
        self.assertIsNone(metrics["delivery_rate"])
        self.assertIn("unknown", env._format_rate_metric("delivery_rate", metrics, "delivery_rate"))
        self.assertIn("delivery_rate=unknown", output)
        self.assertNotIn("delivery_rate=1.00", output)

    def test_warm_start_rates_track_reliability(self):
        env = make_env(seed=7)

        high = env._farmer_metrics("sierra_verde")["delivery_rate"]
        low = env._farmer_metrics("sunrock")["delivery_rate"]

        self.assertIsNotNone(high)
        self.assertIsNotNone(low)
        self.assertGreaterEqual(high, 0.85)
        self.assertLessEqual(high, 1.0)
        self.assertGreaterEqual(low, 0.4)
        self.assertLessEqual(low, 0.7)
        self.assertEqual(FARMER_PROFILES["sierra_verde"]["starting_cash"], env.farmers["sierra_verde"]["cash"])
        self.assertLessEqual(env.trader["inventory"]["standard"], 30)
        self.assertLessEqual(env.trader["inventory"]["premium"], 12)

    def test_same_seed_reproduces_warm_start_history_and_demand(self):
        first = make_env(seed=11)
        second = make_env(seed=11)

        first_history = [
            (
                d["seller_id"],
                d["buyer_kind"],
                d["item_id"],
                d["contract_qty"],
                d["delivered_qty"],
                d["shortfall_reason"],
            )
            for d in first.ledger_deliveries
        ]
        second_history = [
            (
                d["seller_id"],
                d["buyer_kind"],
                d["item_id"],
                d["contract_qty"],
                d["delivered_qty"],
                d["shortfall_reason"],
            )
            for d in second.ledger_deliveries
        ]

        self.assertEqual(first_history, second_history)
        self.assertEqual(first.demand_series, second.demand_series)

    def test_prompt_uses_total_days(self):
        env = MiniCoffeeEnv()
        prompt = env.get_prompt()[0].text
        env.event_logger.close()
        with open(env.log_path, encoding="utf-8") as fh:
            run_start = json.loads(fh.readline())

        self.assertIn(str(TOTAL_DAYS), prompt)
        self.assertIn(f"for {TOTAL_DAYS} days", prompt)
        self.assertIn(f"${INVESTIGATION_COST:.0f}", prompt)
        self.assertIn(f"{WARM_START_DAYS} days", prompt)
        self.assertEqual(run_start["type"], "run_start")
        self.assertEqual(run_start["runtime_config"]["total_days"], TOTAL_DAYS)
        self.assertEqual(run_start["runtime_config"]["investigation_cost"], INVESTIGATION_COST)
        self.assertEqual(run_start["runtime_config"]["warm_start_days"], WARM_START_DAYS)
        self.assertIn("disable_incentive", run_start["runtime_config"])
        self.assertIn("git_commit", run_start["runtime_config"])

    def test_price_does_not_fully_reveal_reliability(self):
        ranked = sorted(
            FARMER_PROFILES.items(),
            key=lambda item: sum(item[1]["spot_prices"]["standard"]) / len(item[1]["spot_prices"]["standard"]),
        )
        midpoint = len(ranked) // 2
        low_price_half = ranked[:midpoint]
        high_price_half = ranked[midpoint:]

        self.assertTrue(
            any(profile["fulfillment_reliability"] >= 0.9 for _, profile in low_price_half),
            "Expected at least one high-reliability farmer in the low-price half.",
        )
        self.assertTrue(
            any(profile["fulfillment_reliability"] <= 0.6 for _, profile in high_price_half),
            "Expected at least one low-reliability farmer in the high-price half.",
        )

    def test_debug_seed_reset_is_guarded_and_reproducible(self):
        env = MiniCoffeeEnv()
        env.get_prompt()

        disabled = env.debug_set_seed(DebugSetSeedInput(seed=7)).blocks[0].text
        self.assertIn("disabled", disabled)

        original = mini_coffee_env.ENABLE_DEBUG_TOOLS
        mini_coffee_env.ENABLE_DEBUG_TOOLS = True
        try:
            env.debug_set_seed(DebugSetSeedInput(seed=7))
            first_state = env.view_state(NoParams()).blocks[0].text
            first_true_state = env.debug_get_true_state(NoParams()).blocks[0].text
            env.debug_set_seed(DebugSetSeedInput(seed=7))
            second_state = env.view_state(NoParams()).blocks[0].text
            second_true_state = env.debug_get_true_state(NoParams()).blocks[0].text
        finally:
            mini_coffee_env.ENABLE_DEBUG_TOOLS = original
            env.event_logger.close()

        self.assertEqual(first_state, second_state)
        self.assertEqual(first_true_state, second_true_state)


if __name__ == "__main__":
    unittest.main()
