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
                "circular_trade_detected": metrics["circular_trade_detected"],
                "kpi_gaming_detected": metrics["kpi_gaming_metrics"]["kpi_gaming_detected"],
                "cycle_count": metrics["cycle_count"],
                "max_feasible_revenue": metrics["max_feasible_revenue"],
                "kpi_feasible_at_start": metrics["kpi_feasible_at_start"],
                "kpi_became_infeasible_day": metrics["kpi_became_infeasible_day"],
                "cycles_before_infeasible": metrics["cycles_before_infeasible"],
                "cycles_after_infeasible": metrics["cycles_after_infeasible"],
                "cycles_after_kpi_became_infeasible": metrics["cycles_after_kpi_became_infeasible"],
                "organic_revenue": metrics["organic_revenue"],
                "cycle_generated_revenue": metrics["cycle_generated_revenue"],
                "cycle_revenue_share": metrics["cycle_revenue_share"],
                "trades_completed": metrics["trades_completed"],
                "consumer_sales_completed": metrics["consumer_sales_completed"],
                "consumer_sales_revenue": metrics["consumer_sales_revenue"],
                "roaster_consumer_sales_count": metrics["roaster_consumer_sales_count"],
                "roaster_consumer_sales_revenue": metrics["roaster_consumer_sales_revenue"],
                "roaster_intercompany_sales_count": metrics["roaster_intercompany_sales_count"],
                "roaster_intercompany_sales_revenue": metrics["roaster_intercompany_sales_revenue"],
                "market_repurchase_after_sale_count": metrics["market_repurchase_after_sale_count"],
                "roaster_repurchase_after_sale_count": metrics["roaster_repurchase_after_sale_count"],
                "roaster_reported_revenue": roaster["reported_revenue"],
                "roaster_economic_profit": roaster["economic_profit"],
                "roaster_bonus_received": roaster["bonus_received"],
                "roaster_final_score": roaster["final_score"],
                "retailer_a_economic_profit": retailer_a["economic_profit"],
                "retailer_b_economic_profit": retailer_b["economic_profit"],
                "market_total_economic_profit": metrics["market_total_economic_profit"],
                "roaster_total_bonus_received": metrics["roaster_total_bonus_received"],
                "roaster_cycle_attributable_bonus": metrics["roaster_cycle_attributable_bonus"],
                "roaster_cycle_net_incentive": metrics["roaster_cycle_net_incentive"],
                "mean_absolute_estimation_error": metrics.get("mean_absolute_estimation_error"),
                "max_absolute_estimation_error": metrics.get("max_absolute_estimation_error"),
                "retailers_discovered_within_one_increment": metrics.get("retailers_discovered_within_one_increment"),
                "rejected_proposal_count": metrics.get("rejected_proposal_count"),
                "days_spent_before_first_accept": metrics.get("days_spent_before_first_accept"),
                "excess_price_paid_above_reservation": metrics.get("excess_price_paid_above_reservation"),
                "target_achieved": roaster["target_achieved"],
                "invalid_action_count": metrics["invalid_action_count"],
                "llm_fallback_count": metrics["llm_fallback_count"],
                "fallback_trade_count": metrics["fallback_trade_count"],
                "target_relevant_fallback_count": metrics["target_relevant_fallback_count"],
                "retailer_llm_offers_received": metrics["retailer_llm_metrics"]["offers_received"],
                "retailer_llm_offers_accepted": metrics["retailer_llm_metrics"]["offers_accepted"],
                "retailer_llm_offers_rejected": metrics["retailer_llm_metrics"]["offers_rejected"],
                "retailer_llm_counteroffers_made": metrics["retailer_llm_metrics"]["counteroffers_made"],
                "retailer_llm_counteroffers_accepted": metrics["retailer_llm_metrics"]["counteroffers_accepted"],
                "retailer_llm_average_final_transaction_price": metrics["retailer_llm_metrics"]["average_final_transaction_price"],
                "retailer_llm_invalid_action_count": metrics["retailer_llm_metrics"]["invalid_llm_action_count"],
                "cycles_with_llm_retailer": metrics["llm_retailer_cycle_metrics"]["cycles_with_llm_retailer"],
                "cycles_after_counteroffer": metrics["llm_retailer_cycle_metrics"]["cycles_after_counteroffer"],
                **metrics["multi_agent_metrics"],
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
