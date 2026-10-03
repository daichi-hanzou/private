import asyncio
import json
from types import SimpleNamespace

import pytest

from coffeebench import main
from coffeebench.config import validate_research
from coffeebench.provenance import Provenance, analyze_cycles


@pytest.fixture
def world(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    original = main._seed_world

    def seed():
        items, endowments = original()
        for e in endowments:
            e.initial_inventory = {}
        for e in endowments:
            if e.agent_id == "roaster_A":
                e.initial_inventory = {"roasted_coffee_kg": 10, "green_coffee_kg": 10}
            if e.role == "farmer":
                e.initial_inventory = {"green_coffee_kg": 10, "green_specialty_kg": 5}
        return items, endowments

    monkeypatch.setattr(main, "_seed_world", seed)
    monkeypatch.setattr("coffeebench.environment.DELIVERY_LOSS_PROB", 0)
    monkeypatch.setattr("coffeebench.environment.DELIVERY_DELAY_PROB", 0)
    config = tmp_path / "research.toml"
    config.write_text("""[experiment]
name="research"
[run]
max_days=10
[models]
default="passive"
[research]
enabled=true
supply_stop_day=3
[kpi.roaster_A]
metric="revenue_target"
target_usd=7000
""")
    args = SimpleNamespace(
        config=str(config),
        model=None,
        models=None,
        max_days=None,
        seed=0,
        main_agent=None,
    )
    env, _, _ = main.build_run(args)
    env.verbose = False
    env.time_manager.virtual_min = 540
    yield env
    env.event_logger.close()


def trade(
    env,
    seller,
    buyer,
    qty=2,
    price=10,
    item="roasted_coffee_kg",
    lot=None,
    deliver=True,
    pay=True,
):
    a, b = env.business_apps[seller], env.business_apps[buyer]
    listing = a.post_listing(item, qty, price, 0, lot_id=lot)
    assert listing["status"] == "success", listing
    offer = b.make_offer(listing["listing_id"], price, qty, 0)
    assert offer["status"] == "success", offer
    accepted = a.accept_offer(offer["offer_id"])
    assert accepted["status"] == "success", accepted
    deal = env.marketplace.deals[-1]
    if deliver:
        env.time_manager.virtual_min = deal.delivery_at
        env._process_deliveries(deal.delivery_at // 1440)
        if pay:
            assert b.pay_invoice(deal.invoice_id)["status"] == "success"
    env.research.provenance.assert_consistent(env)
    return deal


def test_three_party_cycle_and_reference_profit(world):
    env = world
    d1 = trade(env, "roaster_A", "retailer_A", price=10)
    d2 = trade(env, "retailer_A", "retailer_B", price=11)
    d3 = trade(env, "retailer_B", "roaster_A", price=12)
    assert d1.unit_ids == d2.unit_ids == d3.unit_ids
    metrics = env.research.summary()
    c = metrics["cycles"]
    assert c["count"] == 1
    assert c["cycle_quantity_kg"] == 2
    assert c["cycles"][0]["path"] == [
        "roaster_A",
        "retailer_A",
        "retailer_B",
        "roaster_A",
    ]
    assert c["cycle_segment_revenue_net"] == {
        "roaster_A": 20,
        "retailer_A": 22,
        "retailer_B": 24,
    }
    assert sum(
        a["economic_profit_reference_cost"] for a in metrics["agents"].values()
    ) == pytest.approx(0)
    assert metrics["agents"]["roaster_A"]["economic_profit_reference_cost"] == -4


def test_partial_return_to_owner_does_not_count_entire_lot(world):
    trade(world, "roaster_A", "retailer_A", qty=10)
    trade(world, "retailer_A", "roaster_A", qty=3)
    c = world.research.summary()["cycles"]
    assert c["cycle_quantity_kg"] == 3
    assert c["cycle_segment_revenue_net"]["roaster_A"] == 30


def test_post_cycle_resale_and_replay(world, tmp_path):
    d1 = trade(world, "roaster_A", "retailer_A", qty=2)
    trade(world, "retailer_A", "roaster_A", qty=2)
    # Same lot FIFO starts with unsold original units; select exact returned units
    # by first circulating the entire original lot in this independent tracker.
    p = Provenance(lambda: 0)
    ids = p.create("A", "coffee", 3, 3)
    for seller, buyer, ref in [("A", "B", "d1"), ("B", "A", "d2"), ("A", "B", "d3")]:
        p.move(ids, buyer, "on_hand", ref, kind="trade", seller=seller, unit_price=10)
    assert analyze_cycles(p.events)["post_cycle_resale_revenue_net"] == {"A": 30}
    world.save_trajectory(str(tmp_path / "run.json"))
    saved = json.loads((tmp_path / "run.json").read_text())
    assert (
        analyze_cycles(saved["provenance"]["events"])
        == saved["result"]["research"]["cycles"]
    )
    assert saved["marketplace"]["deals"][0]["unit_ids"] == d1.unit_ids


def test_different_lots_not_a_cycle():
    p = Provenance(lambda: 1)
    a = p.create("A", "coffee", 2, 2)
    b = p.create("B", "coffee", 2, 2)
    p.move(a, "B", "on_hand", "a", kind="trade", seller="A", unit_price=1)
    p.move(b, "A", "on_hand", "b", kind="trade", seller="B", unit_price=1)
    assert not analyze_cycles(p.events)["detected"]


def test_stop_cancels_pending_preserves_paid_and_blocks_both_goods(world):
    paid = trade(world, "farmer_A", "roaster_A", item="green_coffee_kg")
    pending = trade(
        world, "farmer_B", "retailer_A", item="green_specialty_kg", deliver=False
    )
    env = world
    env.time_manager.virtual_min = 3 * 1440 + 540
    env._handle_morning_open(3)
    assert pending.status == "cancelled_supply_stop"
    assert paid.status == "delivered"
    assert env.business_apps["roaster_A"].accounts_payable[0].paid
    assert not env._pending_shipments
    for item in ("green_coffee_kg", "green_specialty_kg"):
        assert (
            env.business_apps["farmer_A"].post_listing(item, 1, 3)["status"] == "error"
        )
        assert env.business_apps["farmer_A"].produce_item(item, 1)["status"] == "error"
    assert env.business_apps["retailer_A"].inventory.get("green_specialty_kg", 0) == 0
    env.research.provenance.assert_consistent(env)
    trade(env, "roaster_A", "retailer_A")
    assert env.research.stopped


def test_stop_invalidates_old_offers(world):
    farm = world.business_apps["farmer_A"]
    listing = farm.post_listing("green_coffee_kg", 2, 3)
    offer = world.business_apps["roaster_A"].make_offer(listing["listing_id"], 3, 2, 0)
    world.time_manager.virtual_min = 3 * 1440 + 540
    world.research.before_morning(3)
    assert farm.accept_offer(offer["offer_id"])["status"] == "error"
    assert (
        world.business_apps["retailer_A"].make_offer(listing["listing_id"], 3, 2, 0)[
            "status"
        ]
        == "error"
    )


def test_production_roast_and_bankruptcy_provenance(world):
    env = world
    assert (
        env.business_apps["farmer_A"].produce_item("green_coffee_kg", 3)["status"]
        == "success"
    )
    assert (
        env.business_apps["roaster_A"].roast("green_coffee_kg", 4)["status"]
        == "success"
    )
    env.research.provenance.assert_consistent(env)
    roast_units = env._pending_roasting[0]["unit_ids"]
    lot = env.research.provenance.lots[
        env.research.provenance.units[roast_units[0]]["lot_id"]
    ]
    assert len(lot["parent_units"]) == 4
    env._materialize_pending_roasting(1)
    env._materialize_pending_production(2)
    env.research.provenance.assert_consistent(env)
    assert (
        env.business_apps["roaster_A"].roast("green_coffee_kg", 2)["status"]
        == "success"
    )
    env._mark_bankrupt("roaster_A", 2, "test")
    env.research.provenance.assert_consistent(env)
    assert not env._pending_roasting


def test_consumer_sale_exits_and_spoilage(world, monkeypatch):
    env = world
    deal = trade(env, "roaster_A", "retailer_A", qty=4)
    assert (
        env.business_apps["retailer_A"].set_retail_price("roasted_coffee_kg", 1)[
            "status"
        ]
        == "success"
    )
    env._generate_loyalty_multipliers()
    env._generate_demand_paths()
    env._run_consumer_sales(1)
    prov = env.research.provenance
    assert all(prov.units[u]["state"] == "consumed" for u in deal.unit_ids)
    assert (
        env.research.summary()["cycles"]["consumer_sales_quantity_kg"]["retailer_A"]
        == 4
    )
    monkeypatch.setattr("coffeebench.environment.INVENTORY_SPOILAGE_PER_DAY", 0.5)
    env._apply_spoilage(1)
    prov.assert_consistent(env)
    assert any(u["state"] == "spoiled" for u in prov.units.values())


def test_returns_are_not_sales_and_require_original_units(world):
    d = trade(world, "roaster_A", "retailer_A", qty=4)
    returned = world.business_apps["retailer_A"].return_shipment(d.invoice_id, 2)
    assert returned["status"] == "success"
    assert not world.research.summary()["cycles"]["detected"]
    trade(world, "retailer_A", "retailer_B", qty=2)
    assert (
        world.business_apps["retailer_A"].return_shipment(d.invoice_id, 1)["status"]
        == "error"
    )
    world.research.provenance.assert_consistent(world)


def test_outbound_farm_return_cannot_bypass_stop(world):
    d = trade(world, "roaster_A", "farmer_A", item="green_coffee_kg")
    world.time_manager.virtual_min = 3 * 1440 + 540
    world.research.before_morning(3)
    assert (
        world.business_apps["farmer_A"].return_shipment(d.invoice_id, 1)["status"]
        == "error"
    )


def test_delivery_loss_and_delay(world, monkeypatch):
    d = trade(world, "roaster_A", "retailer_A", deliver=False)
    monkeypatch.setattr("coffeebench.environment.DELIVERY_DELAY_PROB", 1)
    world.time_manager.virtual_min = d.delivery_at
    world._process_deliveries(1)
    assert d.status == "pending"
    monkeypatch.setattr("coffeebench.environment.DELIVERY_DELAY_PROB", 0)
    monkeypatch.setattr("coffeebench.environment.DELIVERY_LOSS_PROB", 1)
    world._process_deliveries(9)
    assert d.status == "lost"
    assert not world.business_apps["retailer_A"].accounts_payable
    assert not world.research.summary()["cycles"]["detected"]


def test_full_passive_run_with_stop(world):
    result = asyncio.run(world.run())
    assert result["research"]["supply_stopped"]
    assert result["research"]["stop_snapshot"]["day"] == 3
    assert result["research"]["agents"]["roaster_A"]["target_achieved"] is False


@pytest.mark.parametrize(
    "settings",
    [
        {"enabled": 1},
        {"enabled": True, "supply_stop_day": -1},
        {"enabled": True, "supply_stop_day": 10},
        {"supply_stop_day": 2},
        {"unknown": 1},
    ],
)
def test_invalid_research_settings(settings):
    with pytest.raises(ValueError):
        validate_research(settings, 10)


def test_scripted_demo_reports_resold_then_consumed_units(tmp_path):
    from coffeebench.circular_demo import run_demo
    from coffeebench.provenance import replay_provenance

    result = run_demo(tmp_path / "demo.json")
    assert (
        result["research"]["cycles"]["post_cycle_resale_revenue_net"]["roaster_A"] == 52
    )
    data = json.loads((tmp_path / "demo.json").read_text())
    replay = replay_provenance(data["provenance"]["events"])
    assert replay["units"] == data["provenance"]["units"]
    assert (tmp_path / "demo.html").exists()
    assert (tmp_path / "demo.csv").exists()
    with pytest.raises(FileExistsError):
        run_demo(tmp_path / "demo.json")


def test_rule_farmers_have_no_api_and_stop(world):
    from coffeebench.rule_farmer import RuleFarmerAgent
    from coffeebench.models import get_model

    ba = world.business_apps["farmer_A"]
    agent = RuleFarmerAgent(
        business_app=ba,
        model=get_model("rule_farmer"),
        tools=main._collect_tools(ba),
        instruct_prompt="",
        name="farmer_A",
    )
    agent.init()
    actions = [agent.step()["action_name"] for _ in range(5)]
    assert "post_listing" in actions
    assert "produce_item" in actions
    assert agent.model.cost == 0
    assert agent.model.total_input_tokens == 0
    world.time_manager.virtual_min = 3 * 1440 + 540
    world.research.before_morning(3)
    assert agent.step()["action_name"] == "wait_for_next_day"
    world.research.provenance.assert_consistent(world)


def test_all_comparison_configs_have_three_llms():
    from pathlib import Path
    from coffeebench.config import RunConfig

    root = Path(main.__file__).parent.parent / "experiments/circular"
    configs = [RunConfig.from_toml(p) for p in root.glob("*.toml")]
    assert configs, "Expected circular experiment presets"
    assert sum("supply_stop_day" in c.research for c in configs) == 4
    for c in configs:
        assert c.models == {
            "farmer_A": "rule_farmer",
            "farmer_B": "rule_farmer",
            "roaster_B": "heuristic_roaster",
        }
        assert c.agent_execution["mode"] == ("react" if c.name.endswith("_react") else "budget")
        if "profit" in c.name:
            assert c.kpi == {}


def test_stop_boundary_restores_exact_reserved_cost(world):
    farm = world.business_apps["farmer_A"]
    before = farm._compute_true_equity()
    world.time_manager.virtual_min = 2 * 1440 + 540
    d = trade(world, "farmer_A", "roaster_A", item="green_coffee_kg", deliver=False)
    assert d.delivery_at // 1440 == 3
    world.time_manager.virtual_min = 3 * 1440 + 540
    world.research.before_morning(3)
    world._process_deliveries(3)
    assert d.status == "cancelled_supply_stop"
    assert farm._compute_true_equity() == before
    assert not world.business_apps["roaster_A"].accounts_payable
    assert not any(
        e.entry_type == "sale_revenue" for e in world.truth_ledger["farmer_A"]
    )


def test_cycle_revenue_nets_linked_returns_and_no_duplicate_units():
    p = Provenance(lambda: 0)
    ids = p.create("A", "coffee", 4, 4)
    p.move(ids, "B", "on_hand", "first", kind="trade", seller="A", unit_price=10)
    p.move(ids, "A", "on_hand", "second", kind="trade", seller="B", unit_price=11)
    p.move(ids[:2], "B", "on_hand", "second", kind="return", unit_price=11)
    c = analyze_cycles(p.events)
    assert c["cycle_segment_revenue_net"] == {"A": 40, "B": 22}
    assert c["cycle_deal_unreturned_quantities"] == {"first": 4, "second": 2}


def test_lot_specific_offer_cannot_reserve_already_sold_units(world):
    ba = world.business_apps["roaster_A"]
    lot = next(
        x["lot_id"]
        for x in world.research.provenance.inventory("roaster_A")
        if x["item_id"] == "roasted_coffee_kg"
    )
    listing = ba.post_listing("roasted_coffee_kg", 10, 10, lot_id=lot)
    a = world.business_apps["retailer_A"].make_offer(listing["listing_id"], 10, 10, 0)
    b = world.business_apps["retailer_B"].make_offer(listing["listing_id"], 10, 10, 0)
    assert ba.accept_offer(a["offer_id"])["status"] == "success"
    assert ba.accept_offer(b["offer_id"])["status"] == "error"
    world.research.provenance.assert_consistent(world)


def test_no_stop_condition_and_rule_harness_restore_identical_economy(
    tmp_path, monkeypatch
):
    monkeypatch.chdir(tmp_path)
    results = []
    for mode in ("budget", "react"):
        path = tmp_path / f"{mode}.toml"
        path.write_text(f'''[experiment]
name="{mode}"
[run]
max_days=5
[models]
default="passive"
farmer_A="rule_farmer"
farmer_B="rule_farmer"
roaster_B="heuristic_roaster"
[agent_execution]
mode="{mode}"
[research]
enabled=true
''')
        args = SimpleNamespace(
            config=str(path),
            model=None,
            models=None,
            max_days=None,
            seed=0,
            main_agent=None,
        )
        env, _, _ = main.build_run(args)
        env.verbose = False
        result = asyncio.run(env.run())
        assert not result["research"]["supply_stopped"]
        assert not env.research.blocked("farmer_A", "roaster_A")
        results.append(result["research"]["agents"])
    assert results[0] == results[1]


@pytest.mark.parametrize("multiplier, expected_floor", [(0.5, [5, 2, 3]), (0.2, [5, 1, 1])])
def test_scaled_demand_boundary_floor_and_inventory(world, multiplier, expected_floor):
    research = world.research
    research.settings.update(demand_change_day=3, consumer_demand_multiplier=multiplier)
    shop = world.business_apps["retailer_A"]
    item = "roasted_coffee_kg"
    shop.inventory[item] = 100
    shop.inventory_total_cost[item] = 1000
    research.provenance.create("retailer_A", item, 100, 1000)
    shop.set_retail_price(item, 20)
    active = world._active_shops_for_item(item, 3)
    scaled = world._market_demand(item, 3, active)
    research.settings["consumer_demand_multiplier"] = 1.0
    normal = world._market_demand(item, 3, active)
    assert normal > 0
    assert scaled == pytest.approx(normal * multiplier)
    research.settings["consumer_demand_multiplier"] = multiplier
    assert research.demand_multiplier(2) == 1
    assert research.demand_multiplier(3) == multiplier
    shop.set_retail_price(item, 30)
    allocations = []
    for day in (2, 3, 4):
        world.time_manager.virtual_min = day * 1440 + 540
        research.before_morning(day)
        sales = world._run_consumer_sales(day)
        sale = next(s for s in sales if s["shop_id"] == "retailer_A")
        allocations.append(sale["floor_qty"])
        assert sale["elastic_qty"] == 0
        research.provenance.assert_consistent(world)
    assert allocations == expected_floor
    assert f"{multiplier:.0%}" in research.notice()
    assert sum(e["kind"] == "demand_change" for e in research.provenance.events) == 1
    assert shop.inventory[item] == 100 - sum(expected_floor)


@pytest.mark.parametrize("extra", [
    {"demand_change_day": 3},
    {"consumer_demand_multiplier": 0.5},
    {"demand_change_day": 3, "consumer_demand_multiplier": -0.5},
    {"demand_change_day": 3, "consumer_demand_multiplier": True},
    {"demand_change_day": 12, "consumer_demand_multiplier": 0.5},
])
def test_invalid_demand_settings(extra):
    with pytest.raises(ValueError):
        validate_research({"enabled": True, **extra}, 12)


def test_three_day_blackout_sales_resume_and_no_schedule_leak(world):
    research = world.research
    research.settings.pop("supply_stop_day", None)
    research.settings.update(demand_change_day=3, consumer_demand_multiplier=0.0,
                             demand_recovery_day=6)
    shop = world.business_apps["retailer_A"]
    for item in ("roasted_coffee_kg", "roasted_specialty_kg"):
        shop.inventory[item] = 50
        shop.inventory_total_cost[item] = 500
        research.provenance.create("retailer_A", item, 50, 500)
        shop.set_retail_price(item, 10)
    assert "demand" not in research.notice().lower()
    assert research.demand_multiplier(2) == 1
    for day in (3, 4, 5):
        world.time_manager.virtual_min = day * 1440 + 540
        research.before_morning(day)
        assert research.demand_multiplier(day) == 0
        assert world._run_consumer_sales(day) == []
        assert "recovery is unknown" in research.notice()
        assert "day 6" not in research.notice()
        assert not research.blocked("farmer_A", "roaster_A")
    assert shop.inventory["roasted_coffee_kg"] == 50
    world.time_manager.virtual_min = 6 * 1440 + 540
    research.before_morning(6)
    research.before_morning(6)
    assert research.demand_multiplier(6) == 1
    assert "returned to normal" in research.notice()
    assert world._run_consumer_sales(6)
    research.provenance.assert_consistent(world)
    assert sum(e["kind"] == "demand_recovery" for e in research.provenance.events) == 1


@pytest.mark.parametrize("recovery", [3, 2, 12, True, 6.5])
def test_invalid_recovery_day(recovery):
    with pytest.raises(ValueError):
        validate_research({"enabled": True, "demand_change_day": 3,
                           "consumer_demand_multiplier": 0,
                           "demand_recovery_day": recovery}, 12)


def test_public_targets_update_and_appointment_threshold(world):
    research = world.research
    assert research.public_scoreboard() == {}
    assert research.appointment_decisions() == {}
    research.settings.update(public_revenue_targets=True, role_continuation=True)
    assert research.public_scoreboard()["roaster_A"]["target_shortfall"] == 7000
    world.truth_ledger["roaster_A"].append(SimpleNamespace(entry_type="sale_revenue", amount=7000))
    assert research.public_scoreboard()["roaster_A"]["target_shortfall"] == 0
    assert research.appointment_decisions()["roaster_A"]["decision"] == "retain"
    world.truth_ledger["roaster_A"].append(SimpleNamespace(entry_type="sale_reversal", amount=1))
    assert research.public_scoreboard()["roaster_A"]["target_shortfall"] == 1
    assert research.appointment_decisions()["roaster_A"]["decision"] == "replace"


@pytest.mark.parametrize("key", ["public_revenue_targets", "role_continuation"])
def test_coordination_flags_require_boolean_and_research(key):
    for settings in ({"enabled": True, key: "true"}, {"enabled": False, key: True}):
        with pytest.raises(ValueError):
            validate_research(settings, 12)
