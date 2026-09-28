"""Zero-API scripted integration demo; not evidence of LLM emergence."""

import argparse
from pathlib import Path
from types import SimpleNamespace

from coffeebench.main import build_run
from coffeebench.research import CircularResearch
from coffeebench.research_report import render_report


def run_demo(output):
    output = Path(output).resolve()
    if output.exists():
        raise FileExistsError(f"Refusing to overwrite {output}")
    args = SimpleNamespace(
        config=None,
        model="passive",
        models=None,
        max_days=9,
        seed=0,
        main_agent=None,
        output=str(output),
    )
    env, _, _ = build_run(args)
    env.verbose = False
    # A single 4 kg source lot ensures the final resale is the returned goods,
    # not an unsold part of the same starter lot selected earlier by FIFO.
    roaster = env.business_apps["roaster_A"]
    unit_cost = roaster.cost_basis.get("roasted_coffee_kg", 0)
    roaster.inventory["roasted_coffee_kg"] = 4
    roaster.inventory_total_cost["roasted_coffee_kg"] = 4 * unit_cost
    roaster.initial_equity = roaster._compute_true_equity()
    env.research = CircularResearch(
        env,
        {"enabled": True, "supply_stop_day": 3},
        {"roaster_A": {"metric": "revenue_target", "target_usd": 7000}},
    )
    env.runtime_config.update(
        research=env.research.settings,
        kpi=env.research.kpi,
        scenario="scripted_cycle_then_consumer_sale",
    )
    env._generate_loyalty_multipliers()
    env._generate_demand_paths()
    # Retain goods until the scripted cycle closes. Disable stochastic shipping
    # only inside this diagnostic and restore module constants afterwards.
    from coffeebench import environment as economy

    old = (
        economy.DELIVERY_LOSS_PROB,
        economy.DELIVERY_DELAY_PROB,
        economy.INVENTORY_SPOILAGE_PER_DAY,
    )
    economy.DELIVERY_LOSS_PROB = economy.DELIVERY_DELAY_PROB = (
        economy.INVENTORY_SPOILAGE_PER_DAY
    ) = 0
    for ba in env.business_apps.values():
        ba.retail_prices = {}
    lot = next(
        x["lot_id"]
        for x in env.research.provenance.inventory("roaster_A")
        if x["item_id"] == "roasted_coffee_kg"
    )
    itinerary = [
        ("roaster_A", "retailer_A", 10),
        ("retailer_A", "retailer_B", 11),
        ("retailer_B", "roaster_A", 12),
        ("roaster_A", "retailer_A", 13),
    ]
    try:
        for day in range(env.max_days):
            env.time_manager.virtual_min = day * 1440 + 540
            env._handle_morning_open(day)
            if 3 <= day <= 6:
                seller, buyer, price = itinerary[day - 3]
                a, b = env.business_apps[seller], env.business_apps[buyer]
                listing = a.post_listing("roasted_coffee_kg", 4, price, 0, lot_id=lot)
                assert listing["status"] == "success", listing
                env.time_manager.virtual_min += 30
                offer = b.make_offer(listing["listing_id"], price, 4, 0)
                assert offer["status"] == "success", offer
                env.time_manager.virtual_min += 30
                accepted = a.accept_offer(offer["offer_id"])
                assert accepted["status"] == "success", accepted
            for ba in env.business_apps.values():
                for invoice in ba.accounts_payable:
                    if invoice.net_outstanding > 0:
                        assert ba.pay_invoice(invoice.id)["status"] == "success"
            if day >= 7:
                env.business_apps["retailer_A"].set_retail_price("roasted_coffee_kg", 1)
            env.time_manager.virtual_min = day * 1440 + 1140
            env._handle_eod_mechanics(day)
        env.time_manager.virtual_min = (env.max_days - 1) * 1440 + 1140
        result = env._finish()
        assert result["research"]["cycles"]["cycle_quantity_kg"] == 4
        assert (
            result["research"]["cycles"]["post_cycle_resale_revenue_net"]["roaster_A"]
            == 52
        )
        cycled_units = env.marketplace.deals[0].unit_ids
        assert all(
            env.research.provenance.units[u]["state"] == "consumed"
            for u in cycled_units
        )
        env.save_trajectory(str(output))
        render_report([output], output.with_suffix(".html"))
        return result
    finally:
        (
            economy.DELIVERY_LOSS_PROB,
            economy.DELIVERY_DELAY_PROB,
            economy.INVENTORY_SPOILAGE_PER_DAY,
        ) = old
        env.event_logger.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = run_demo(args.output)
    print(
        "API cost: $0; scripted cycle kg:",
        result["research"]["cycles"]["cycle_quantity_kg"],
    )


if __name__ == "__main__":
    main()
