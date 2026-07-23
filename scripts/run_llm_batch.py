from __future__ import annotations

import argparse
import csv
import os
from pathlib import Path

from run_llm_condition_comparison import (
    CONDITION_CHOICES,
    build_output_root,
    run_condition,
    validate_args,
)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", default=os.environ.get("OPENAI_MODEL"))
    parser.add_argument(
        "--condition",
        choices=CONDITION_CHOICES,
        required=True,
    )
    parser.add_argument("--forced-repurchase-unit-price", type=float, default=None)
    parser.add_argument(
        "--experiment-version",
        choices=(
            "multi_agent_experiment_1",
            "multi_agent_experiment_2",
            "multi_agent_experiment_3",
        ),
        default=None,
    )
    parser.add_argument("--seed", type=int)
    parser.add_argument("--seeds", type=int, nargs="+")
    parser.add_argument("--llm-seed", type=int, default=None)
    parser.add_argument("--agent-order-seed", type=int, default=None)
    parser.add_argument("--temperature", type=float, default=None)
    parser.add_argument("--send-seed", action="store_true")
    parser.add_argument("--prompt-version", default="v1")
    parser.add_argument("--provider", choices=("openai", "azure"), default="openai")
    parser.add_argument("--bonus", type=float, default=None)
    parser.add_argument("--target", type=float, default=None)
    parser.add_argument("--lot-count", type=int, default=None)
    parser.add_argument("--retailer-a-max-purchase-unit-price", type=float, default=None)
    parser.add_argument("--retailer-b-max-purchase-unit-price", type=float, default=None)
    parser.add_argument("--retailer-a-repurchase-reservation-price", type=float, default=None)
    parser.add_argument("--retailer-b-repurchase-reservation-price", type=float, default=None)
    parser.add_argument(
        "--agent-mode",
        choices=("single_agent", "multi_agent"),
        default="single_agent",
    )
    parser.add_argument("--retailer-policy-mode", choices=("rule_based", "llm"), default="rule_based")
    parser.add_argument("--retailer-a-policy-mode", choices=("rule_based", "llm"), default=None)
    parser.add_argument("--retailer-b-policy-mode", choices=("rule_based", "llm"), default=None)
    parser.add_argument("--max-negotiation-rounds", type=int, default=2)
    parser.add_argument("--retailer-counteroffer-price-min", type=float, default=0.01)
    parser.add_argument("--retailer-counteroffer-price-max", type=float, default=100.0)
    parser.add_argument("--hide-retailer-offer-analysis", action="store_true")
    parser.add_argument("--retailer-consumer-sale-enabled", action="store_true")
    parser.add_argument("--disable-roaster-consumer-sale", action="store_true")
    parser.add_argument("--consumer-unit-price", type=float, default=9.5)
    parser.add_argument("--consumer-daily-demand-capacity", type=int, default=100)
    parser.add_argument("--retailer-revenue-target", type=float, default=None)
    parser.add_argument("--retailer-target-bonus", type=float, default=500.0)
    parser.add_argument("--overwrite", action="store_true")
    args = parser.parse_args()
    if not args.model:
        parser.error("--model or OPENAI_MODEL is required")
    if (args.seed is None) == (args.seeds is None):
        parser.error("Specify exactly one of --seed or --seeds")
    validate_args(args)
    seeds = args.seeds if args.seeds is not None else [args.seed]

    rows = []
    for seed in seeds:
        result = run_condition(
            condition=args.condition,
            model=args.model,
            seed=seed,
            llm_seed=args.llm_seed,
            agent_order_seed=args.agent_order_seed,
            temperature=args.temperature,
            provider=args.provider,
            send_seed=args.send_seed,
            prompt_version=args.prompt_version,
            bonus=args.bonus,
            target=args.target,
            lot_count=args.lot_count,
            retailer_a_max_purchase_unit_price=args.retailer_a_max_purchase_unit_price,
            retailer_b_max_purchase_unit_price=args.retailer_b_max_purchase_unit_price,
            experiment_version=args.experiment_version,
            retailer_a_repurchase_reservation_price=args.retailer_a_repurchase_reservation_price,
            retailer_b_repurchase_reservation_price=args.retailer_b_repurchase_reservation_price,
            agent_mode=args.agent_mode,
            forced_repurchase_unit_price=args.forced_repurchase_unit_price,
            retailer_policy_mode=args.retailer_policy_mode,
            retailer_a_policy_mode=args.retailer_a_policy_mode,
            retailer_b_policy_mode=args.retailer_b_policy_mode,
            max_negotiation_rounds=args.max_negotiation_rounds,
            retailer_counteroffer_price_min=args.retailer_counteroffer_price_min,
            retailer_counteroffer_price_max=args.retailer_counteroffer_price_max,
            retailer_show_offer_analysis=not args.hide_retailer_offer_analysis,
            retailer_consumer_sale_enabled=args.retailer_consumer_sale_enabled,
            roaster_consumer_sale_enabled=not args.disable_roaster_consumer_sale,
            consumer_unit_price=args.consumer_unit_price,
            consumer_daily_demand_capacity=args.consumer_daily_demand_capacity,
            retailer_revenue_target=args.retailer_revenue_target,
            retailer_target_bonus=args.retailer_target_bonus,
            overwrite=args.overwrite,
        )
        metrics = result.metrics
        roaster = metrics["agents"]["roaster"]
        retailer_a = metrics["agents"]["retailer_a"]
        retailer_b = metrics["agents"]["retailer_b"]
        configured_target = result.state.agents["roaster"].revenue_target
        configured_bonus = result.state.agents["roaster"].target_bonus
        rows.append(
            {
                "run_id": result.run_id,
                "condition": args.condition,
                "model": args.model,
                "simulation_seed": seed,
                "llm_seed": seed if args.llm_seed is None else args.llm_seed,
                "agent_order_seed": (
                    seed if args.agent_order_seed is None else args.agent_order_seed
                ),
                "temperature": args.temperature,
                "prompt_version": args.prompt_version,
                "configured_roaster_revenue_target": configured_target,
                "configured_roaster_target_bonus": configured_bonus,
                "configured_lot_count": args.lot_count,
                "configured_retailer_a_max_purchase_unit_price": args.retailer_a_max_purchase_unit_price,
                "configured_retailer_b_max_purchase_unit_price": args.retailer_b_max_purchase_unit_price,
                "configured_retailer_a_repurchase_reservation_price": args.retailer_a_repurchase_reservation_price,
                "configured_retailer_b_repurchase_reservation_price": args.retailer_b_repurchase_reservation_price,
                "agent_mode": args.agent_mode,
                "experiment_version": (
                    args.experiment_version
                    or (
                        "multi_agent_experiment_2"
                        if args.agent_mode == "multi_agent"
                        and args.forced_repurchase_unit_price is None
                        else "multi_agent_experiment_1"
                    )
                ),
                "roaster_price_decision_mode": (
                    "llm"
                    if args.agent_mode == "multi_agent"
                    and args.forced_repurchase_unit_price is None
                    else "fixed"
                ),
                "forced_repurchase_unit_price": args.forced_repurchase_unit_price,
                "retailer_a_policy_mode": args.retailer_a_policy_mode or args.retailer_policy_mode,
                "retailer_b_policy_mode": args.retailer_b_policy_mode or args.retailer_policy_mode,
                "retailer_consumer_sale_enabled": args.retailer_consumer_sale_enabled,
                "roaster_consumer_sale_enabled": (
                    not args.disable_roaster_consumer_sale
                ),
                "consumer_unit_price": args.consumer_unit_price,
                "consumer_daily_demand_capacity": args.consumer_daily_demand_capacity,
                "retailer_revenue_target": result.state.agents[
                    "retailer_a"
                ].revenue_target,
                "retailer_target_bonus": result.state.agents[
                    "retailer_a"
                ].target_bonus,
                "cycle_detected": metrics["cycle"]["detected"],
                "cycle_count": metrics["cycle"]["count"],
                "trade_count": metrics["trades"]["total"],
                "agent_trade_count": metrics["trades"]["agent"],
                "consumer_sale_count": metrics["trades"]["consumer"],
                "offer_count": metrics["offers"]["created"],
                "offer_accepted_count": metrics["offers"]["accepted"],
                "offer_rejected_count": metrics["offers"]["rejected"],
                "offer_expired_count": metrics["offers"]["expired"],
                "counteroffer_count": metrics["offers"]["counteroffers_created"],
                "roaster_reported_revenue": roaster["reported_revenue"],
                "roaster_economic_profit": roaster["economic_profit"],
                "roaster_target_achieved": roaster["target_achieved"],
                "retailer_a_reported_revenue": retailer_a["reported_revenue"],
                "retailer_a_economic_profit": retailer_a["economic_profit"],
                "retailer_a_target_achieved": retailer_a["target_achieved"],
                "retailer_b_reported_revenue": retailer_b["reported_revenue"],
                "retailer_b_economic_profit": retailer_b["economic_profit"],
                "retailer_b_target_achieved": retailer_b["target_achieved"],
                "invalid_action_count": metrics["errors"]["invalid_actions"],
                "llm_fallback_count": metrics["errors"]["fallbacks"],
                "api_error_count": metrics["errors"]["api_errors"],
                "output_dir": str(result.output_dir),
            }
        )

    output_root = build_output_root(
        condition=args.condition,
        target=result.state.agents["roaster"].revenue_target,
        bonus=result.state.agents["roaster"].target_bonus,
        lot_count=args.lot_count,
        retailer_a_max_purchase_unit_price=args.retailer_a_max_purchase_unit_price,
        retailer_b_max_purchase_unit_price=args.retailer_b_max_purchase_unit_price,
        experiment_version=args.experiment_version,
        agent_mode=args.agent_mode,
        forced_repurchase_unit_price=args.forced_repurchase_unit_price,
        retailer_policy_modes={
            "retailer_a": args.retailer_a_policy_mode or args.retailer_policy_mode,
            "retailer_b": args.retailer_b_policy_mode or args.retailer_policy_mode,
        },
        retailer_consumer_sale_enabled=args.retailer_consumer_sale_enabled,
        roaster_consumer_sale_enabled=not args.disable_roaster_consumer_sale,
    )
    output_name = (
        "experiment_3_summary.csv"
        if args.experiment_version == "multi_agent_experiment_3"
        else "experiment_2_summary.csv"
        if args.agent_mode == "multi_agent" and args.forced_repurchase_unit_price is None
        else "summary.csv"
    )
    output_path = output_root / output_name
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


if __name__ == "__main__":
    main()
