from __future__ import annotations

import argparse
import os
import re
from pathlib import Path

from circular_coffee.config import build_experiment_config
from circular_coffee.llm_clients import (
    RETAILER_DECISION_JSON_SCHEMA,
    AzureOpenAIClient,
    OpenAIClient,
)
from circular_coffee.policies import (
    MULTI_AGENT_ROASTER_PROMPT_SUFFIX,
    CooperativeRetailerPolicy,
    LLMPolicy,
    ReservationPriceRetailerDecisionPolicy,
    RetailerDecisionPolicy,
    build_llm_system_prompt,
)
from circular_coffee.simulation import SimulationRunner

CONDITION_CHOICES = (
    "profit_only",
    "revenue_pressure",
    "multi_strategy",
    "multi_strategy_profit_only",
    "multi_strategy_revenue_pressure",
)


def safe_model_name(model: str) -> str:
    return re.sub(r"[^A-Za-z0-9_.-]+", "_", model)


def safe_number_label(value: float) -> str:
    return str(float(value)).replace(".", "_")


def build_lot_ids(lot_count: int) -> list[str]:
    if lot_count <= 0:
        raise ValueError("--lot-count must be positive")
    return [f"LOT-{index:03d}" for index in range(1, lot_count + 1)]


def build_client(
    *,
    model: str,
    temperature: float | None,
    seed: int,
    provider: str,
    send_seed: bool,
    response_schema: dict | None = None,
):
    if provider == "azure":
        return AzureOpenAIClient(
            deployment=model,
            temperature=temperature,
            seed=seed,
            supports_seed=send_seed,
            response_schema=response_schema,
        )
    return OpenAIClient(
        model=model,
        temperature=temperature,
        seed=seed,
        supports_seed=send_seed,
        response_schema=response_schema,
    )


def uses_multi_strategy_market(condition: str) -> bool:
    return condition in {
        "multi_strategy",
        "multi_strategy_profit_only",
        "multi_strategy_revenue_pressure",
    }


def run_condition(
    *,
    condition: str,
    model: str,
    seed: int,
    temperature: float | None,
    provider: str,
    llm_seed: int | None = None,
    agent_order_seed: int | None = None,
    send_seed: bool = False,
    prompt_version: str = "v1",
    bonus: float | None = None,
    target: float | None = None,
    lot_count: int | None = None,
    retailer_a_max_purchase_unit_price: float | None = None,
    retailer_b_max_purchase_unit_price: float | None = None,
    experiment_version: str | None = None,
    retailer_a_repurchase_reservation_price: float | None = None,
    retailer_b_repurchase_reservation_price: float | None = None,
    agent_mode: str = "single_agent",
    forced_repurchase_unit_price: float | None = None,
    retailer_policy_mode: str = "rule_based",
    retailer_a_policy_mode: str | None = None,
    retailer_b_policy_mode: str | None = None,
    max_negotiation_rounds: int = 2,
    retailer_counteroffer_price_min: float = 0.01,
    retailer_counteroffer_price_max: float = 100.0,
    retailer_show_offer_analysis: bool = True,
    overwrite: bool = False,
):
    resolved_experiment_version = experiment_version or (
        "multi_agent_experiment_2"
        if agent_mode == "multi_agent" and forced_repurchase_unit_price is None
        else "multi_agent_experiment_1"
    )
    is_experiment_2 = resolved_experiment_version == "multi_agent_experiment_2"
    is_experiment_3 = resolved_experiment_version == "multi_agent_experiment_3"
    resolved_llm_seed = seed if llm_seed is None else llm_seed
    resolved_agent_order_seed = seed if agent_order_seed is None else agent_order_seed
    retailer_policy_modes = {
        "retailer_a": retailer_a_policy_mode or retailer_policy_mode,
        "retailer_b": retailer_b_policy_mode or retailer_policy_mode,
    }
    config = build_experiment_config(
        condition=condition,
        seed=seed,
        llm_seed=resolved_llm_seed,
        agent_order_seed=resolved_agent_order_seed,
        llm_model_name=model,
        llm_temperature=temperature,
        prompt_version=prompt_version,
        agent_mode=agent_mode,
        forced_repurchase_unit_price=forced_repurchase_unit_price,
        experiment_version=resolved_experiment_version,
        roaster_price_decision_mode="llm" if is_experiment_2 or is_experiment_3 else "fixed",
        log_roaster_price_reason=True,
        retailer_policy_modes=retailer_policy_modes,
        max_negotiation_rounds=max_negotiation_rounds,
        retailer_counteroffer_price_min=retailer_counteroffer_price_min,
        retailer_counteroffer_price_max=retailer_counteroffer_price_max,
        retailer_show_offer_analysis=retailer_show_offer_analysis,
    )
    if bonus is not None:
        config.agents["roaster"].target_bonus = bonus
    if target is not None:
        config.agents["roaster"].revenue_target = target
    if lot_count is not None:
        config.lot_ids = build_lot_ids(lot_count)
    if retailer_a_max_purchase_unit_price is not None:
        config.retailer_a_max_purchase_unit_price = retailer_a_max_purchase_unit_price
    if retailer_b_max_purchase_unit_price is not None:
        config.retailer_b_max_purchase_unit_price = retailer_b_max_purchase_unit_price
    if retailer_a_repurchase_reservation_price is not None:
        config.retailer_a_repurchase_reservation_price = (
            retailer_a_repurchase_reservation_price
        )
    if retailer_b_repurchase_reservation_price is not None:
        config.retailer_b_repurchase_reservation_price = (
            retailer_b_repurchase_reservation_price
        )
    if is_experiment_3:
        config.hidden_retailer_reservation_price = True
        config.show_retailer_acquisition_price_to_roaster = False
        config.show_retailer_reservation_price_to_roaster = False
        config.show_rejection_reason_to_roaster = False
        config.show_accept_reject_history_to_roaster = True
    configured_bonus = config.agents["roaster"].target_bonus
    configured_target = config.agents["roaster"].revenue_target
    configured_lot_count = len(config.lot_ids)
    roaster_client = build_client(
        model=model,
        temperature=temperature,
        seed=resolved_llm_seed,
        provider=provider,
        send_seed=send_seed,
    )
    retailer_a_preferred_buyers = (
        ["roaster", "retailer_b"]
        if uses_multi_strategy_market(condition)
        else ["retailer_b", "roaster"]
    )
    retailer_b_max_purchase_unit_price = (
        config.retailer_b_max_purchase_unit_price
        if uses_multi_strategy_market(condition)
        else None
    )
    policies = {
        "roaster": LLMPolicy(
            client=roaster_client,
            condition=config.experiment_condition,
            system_prompt=(
                build_llm_system_prompt(config.experiment_condition)
                + MULTI_AGENT_ROASTER_PROMPT_SUFFIX
                if agent_mode == "multi_agent"
                else None
            ),
            prompt_version=config.prompt_version,
        ),
        "retailer_a": CooperativeRetailerPolicy(
            preferred_buyers=retailer_a_preferred_buyers,
            max_purchase_unit_price=config.retailer_a_max_purchase_unit_price,
            can_initiate_resale=(
                agent_mode != "multi_agent"
                or config.retailer_can_initiate_resale_to_roaster
            ),
        ),
        "retailer_b": CooperativeRetailerPolicy(
            preferred_buyers=["roaster", "retailer_a"],
            max_purchase_unit_price=retailer_b_max_purchase_unit_price,
            can_initiate_resale=(
                agent_mode != "multi_agent"
                or config.retailer_can_initiate_resale_to_roaster
            ),
        ),
    }
    repurchase_decision_policies = {}
    if agent_mode == "multi_agent":
        repurchase_decision_policies = {}
        for retailer_id in ("retailer_a", "retailer_b"):
            if retailer_policy_modes[retailer_id] == "llm":
                repurchase_decision_policies[retailer_id] = RetailerDecisionPolicy(
                    client=build_client(
                        model=model,
                        temperature=temperature,
                        seed=resolved_llm_seed,
                        provider=provider,
                        send_seed=send_seed,
                        response_schema=RETAILER_DECISION_JSON_SCHEMA,
                    ),
                    prompt_version=config.retailer_prompt_version,
                    counteroffer_price_min=config.retailer_counteroffer_price_min,
                    counteroffer_price_max=config.retailer_counteroffer_price_max,
                )
            else:
                reservation_price = (
                    config.retailer_a_repurchase_reservation_price
                    if retailer_id == "retailer_a"
                    else config.retailer_b_repurchase_reservation_price
                )
                repurchase_decision_policies[retailer_id] = (
                    ReservationPriceRetailerDecisionPolicy(
                        reservation_price=reservation_price,
                        reveal_reason=config.show_rejection_reason_to_roaster,
                    )
                )
    output_root = build_output_root(
        condition=condition,
        bonus=configured_bonus,
        target=configured_target,
        lot_count=lot_count,
        retailer_a_max_purchase_unit_price=retailer_a_max_purchase_unit_price,
        retailer_b_max_purchase_unit_price=retailer_b_max_purchase_unit_price,
        experiment_version=resolved_experiment_version,
        agent_mode=agent_mode,
        forced_repurchase_unit_price=forced_repurchase_unit_price,
        retailer_policy_modes=retailer_policy_modes,
    )
    run_id = f"seed_{seed}"
    output_dir = Path(output_root) / run_id
    if output_dir.exists() and not overwrite:
        raise FileExistsError(
            f"output already exists: {output_dir}. Use --overwrite to replace it."
        )
    print(f"condition: {condition}")
    print(f"target: {configured_target}")
    print(f"bonus: {configured_bonus}")
    print(f"lot_count: {configured_lot_count}")
    print(
        "retailer_a_max_purchase_unit_price: "
        f"{config.retailer_a_max_purchase_unit_price}"
    )
    print(
        "retailer_b_max_purchase_unit_price: "
        f"{config.retailer_b_max_purchase_unit_price}"
    )
    print(f"agent_mode: {agent_mode}")
    print(f"forced_repurchase_unit_price: {forced_repurchase_unit_price}")
    print(f"experiment_version: {config.experiment_version}")
    print(f"retailer_policy_modes: {retailer_policy_modes}")
    print(f"roaster_price_decision_mode: {config.roaster_price_decision_mode}")
    print(f"seed: {seed}")
    print(f"llm_seed: {resolved_llm_seed}")
    print(f"agent_order_seed: {resolved_agent_order_seed}")
    print(f"output_dir: {output_dir}")
    return SimulationRunner(
        config,
        policies,
        run_id=run_id,
        output_root=output_root,
        repurchase_decision_policies=repurchase_decision_policies,
    ).run()


def print_result(
    result,
    *,
    condition: str,
    model: str,
    seed: int,
    bonus: float | None,
    target: float | None,
    lot_count: int | None,
    retailer_a_max_purchase_unit_price: float | None,
    retailer_b_max_purchase_unit_price: float | None,
    agent_mode: str,
    experiment_version: str | None,
    retailer_a_repurchase_reservation_price: float | None,
    retailer_b_repurchase_reservation_price: float | None,
    forced_repurchase_unit_price: float | None,
) -> None:
    metrics = result.metrics
    roaster = metrics["agents"]["roaster"]
    fields = {
        "condition": condition,
        "model": model,
        "seed": seed,
        "configured_roaster_revenue_target": (
            target if target is not None else result.state.agents["roaster"].revenue_target
        ),
        "configured_roaster_target_bonus": (
            bonus if bonus is not None else result.state.agents["roaster"].target_bonus
        ),
        "configured_lot_count": lot_count if lot_count is not None else "default",
        "configured_retailer_a_max_purchase_unit_price": (
            retailer_a_max_purchase_unit_price
            if retailer_a_max_purchase_unit_price is not None
            else "default"
        ),
        "configured_retailer_b_max_purchase_unit_price": (
            retailer_b_max_purchase_unit_price
            if retailer_b_max_purchase_unit_price is not None
            else "default"
        ),
        "agent_mode": agent_mode,
        "experiment_version": experiment_version or "default",
        "forced_repurchase_unit_price": forced_repurchase_unit_price,
        "configured_retailer_a_repurchase_reservation_price": (
            retailer_a_repurchase_reservation_price
            if retailer_a_repurchase_reservation_price is not None
            else "default"
        ),
        "configured_retailer_b_repurchase_reservation_price": (
            retailer_b_repurchase_reservation_price
            if retailer_b_repurchase_reservation_price is not None
            else "default"
        ),
        "circular_trade_detected": metrics["circular_trade_detected"],
        "kpi_gaming_detected": metrics["kpi_gaming_metrics"]["kpi_gaming_detected"],
        "cycle_count": metrics["cycle_count"],
        "max_feasible_revenue": metrics["max_feasible_revenue"],
        "kpi_feasible_at_start": metrics["kpi_feasible_at_start"],
        "kpi_became_infeasible_day": metrics["kpi_became_infeasible_day"],
        "cycles_after_kpi_became_infeasible": metrics["cycles_after_kpi_became_infeasible"],
        "organic_revenue": metrics["organic_revenue"],
        "cycle_generated_revenue": metrics["cycle_generated_revenue"],
        "cycle_revenue_share": metrics["cycle_revenue_share"],
        "trades_completed": metrics["trades_completed"],
        "consumer_sales_completed": metrics["consumer_sales_completed"],
        "consumer_sales_revenue": metrics["consumer_sales_revenue"],
        "roaster_consumer_sales_count": metrics["roaster_consumer_sales_count"],
        "roaster_intercompany_sales_count": metrics["roaster_intercompany_sales_count"],
        "market_repurchase_after_sale_count": metrics["market_repurchase_after_sale_count"],
        "roaster_repurchase_after_sale_count": metrics["roaster_repurchase_after_sale_count"],
        "roaster reported_revenue": roaster["reported_revenue"],
        "roaster economic_profit": roaster["economic_profit"],
        "roaster bonus_received": roaster["bonus_received"],
        "roaster target_achieved": roaster["target_achieved"],
        "roaster final_score": roaster["final_score"],
        "roaster_total_bonus_received": metrics["roaster_total_bonus_received"],
        "roaster_cycle_attributable_bonus": metrics["roaster_cycle_attributable_bonus"],
        "roaster_cycle_net_incentive": metrics["roaster_cycle_net_incentive"],
        "invalid_action_count": metrics["invalid_action_count"],
        "llm_fallback_count": metrics["llm_fallback_count"],
        "fallback_trade_count": metrics["fallback_trade_count"],
        "target_relevant_fallback_count": metrics["target_relevant_fallback_count"],
        "api_error_count": metrics["api_error_count"],
        "json_parse_error_count": metrics["json_parse_error_count"],
        "mean_absolute_estimation_error": metrics.get("mean_absolute_estimation_error"),
        "max_absolute_estimation_error": metrics.get("max_absolute_estimation_error"),
        "retailers_discovered_within_one_increment": metrics.get("retailers_discovered_within_one_increment"),
        "rejected_proposal_count": metrics.get("rejected_proposal_count"),
        "days_spent_before_first_accept": metrics.get("days_spent_before_first_accept"),
        "excess_price_paid_above_reservation": metrics.get("excess_price_paid_above_reservation"),
        "multi_agent_metrics": metrics["multi_agent_metrics"],
    }
    for key, value in fields.items():
        print(f"{key}: {value}")
    print()


def build_output_root(
    *,
    condition: str,
    bonus: float,
    target: float,
    lot_count: int | None = None,
    retailer_a_max_purchase_unit_price: float | None = None,
    retailer_b_max_purchase_unit_price: float | None = None,
    experiment_version: str | None = None,
    agent_mode: str = "single_agent",
    forced_repurchase_unit_price: float | None = None,
    retailer_policy_modes: dict[str, str] | None = None,
) -> Path:
    lot_path = f"lots_{lot_count}" if lot_count is not None else None
    retailer_price_path = None
    if (
        retailer_a_max_purchase_unit_price is not None
        or retailer_b_max_purchase_unit_price is not None
    ):
        retailer_price_path = (
            "retailer_prices_"
            f"a_{safe_number_label(retailer_a_max_purchase_unit_price or 0.0)}"
            f"_b_{safe_number_label(retailer_b_max_purchase_unit_price or 0.0)}"
        )
    if condition in {
        "revenue_pressure",
        "multi_strategy",
        "multi_strategy_revenue_pressure",
    }:
        path = (
            Path("results")
            / condition
            / f"target_{safe_number_label(target)}"
            / f"bonus_{safe_number_label(bonus)}"
        )
        if lot_path is not None:
            path /= lot_path
        if retailer_price_path is not None:
            path /= retailer_price_path
        if agent_mode == "multi_agent":
            path /= "agent_mode_multi_agent"
            if experiment_version == "multi_agent_experiment_3":
                path /= "experiment_3"
            elif forced_repurchase_unit_price is None:
                path /= "experiment_2"
            if retailer_policy_modes is not None:
                path /= (
                    f"retailers_a_{retailer_policy_modes['retailer_a']}"
                    f"_b_{retailer_policy_modes['retailer_b']}"
                )
        if forced_repurchase_unit_price is not None:
            path /= f"forced_repurchase_{safe_number_label(forced_repurchase_unit_price)}"
        return path
    path = Path("results") / condition
    if lot_path is not None:
        path /= lot_path
    if retailer_price_path is not None:
        path /= retailer_price_path
    if agent_mode == "multi_agent":
        path /= "agent_mode_multi_agent"
        if experiment_version == "multi_agent_experiment_3":
            path /= "experiment_3"
        elif forced_repurchase_unit_price is None:
            path /= "experiment_2"
        if retailer_policy_modes is not None:
            path /= (
                f"retailers_a_{retailer_policy_modes['retailer_a']}"
                f"_b_{retailer_policy_modes['retailer_b']}"
            )
    if forced_repurchase_unit_price is not None:
        path /= f"forced_repurchase_{safe_number_label(forced_repurchase_unit_price)}"
    return path


def validate_args(args: argparse.Namespace) -> None:
    if args.condition in {"profit_only", "multi_strategy_profit_only"} and args.bonus is not None:
        raise SystemExit(f"--bonus cannot be used with --condition {args.condition}")
    if args.condition in {"profit_only", "multi_strategy_profit_only"} and args.target is not None:
        raise SystemExit(f"--target cannot be used with --condition {args.condition}")
    if args.lot_count is not None and not uses_multi_strategy_market(args.condition):
        raise SystemExit("--lot-count can only be used with multi_strategy conditions")
    if args.lot_count is not None and args.lot_count <= 0:
        raise SystemExit("--lot-count must be positive")
    if (
        args.retailer_a_max_purchase_unit_price is not None
        and args.retailer_a_max_purchase_unit_price <= 0
    ):
        raise SystemExit("--retailer-a-max-purchase-unit-price must be positive")
    if (
        args.retailer_b_max_purchase_unit_price is not None
        and args.retailer_b_max_purchase_unit_price <= 0
    ):
        raise SystemExit("--retailer-b-max-purchase-unit-price must be positive")
    if (
        args.forced_repurchase_unit_price is not None
        and args.agent_mode != "multi_agent"
    ):
        raise SystemExit("--forced-repurchase-unit-price requires --agent-mode multi_agent")
    if args.experiment_version == "multi_agent_experiment_3" and args.agent_mode != "multi_agent":
        raise SystemExit("--experiment-version multi_agent_experiment_3 requires --agent-mode multi_agent")
    if args.forced_repurchase_unit_price is not None and args.forced_repurchase_unit_price <= 0:
        raise SystemExit("--forced-repurchase-unit-price must be positive")
    if args.condition == "revenue_pressure" and args.bonus is None:
        raise SystemExit(
            f"--bonus is required with --condition {args.condition}"
        )
    for field_name in (
        "retailer_policy_mode",
        "retailer_a_policy_mode",
        "retailer_b_policy_mode",
    ):
        value = getattr(args, field_name, None)
        if value is not None and value not in {"rule_based", "llm"}:
            raise SystemExit(f"--{field_name.replace('_', '-')} must be rule_based or llm")
    if (
        any(
            getattr(args, field_name, None) == "llm"
            for field_name in (
                "retailer_policy_mode",
                "retailer_a_policy_mode",
                "retailer_b_policy_mode",
            )
        )
        and args.agent_mode != "multi_agent"
    ):
        raise SystemExit("LLM Retailer mode requires --agent-mode multi_agent")
    if getattr(args, "max_negotiation_rounds", 2) < 1:
        raise SystemExit("--max-negotiation-rounds must be positive")
    counteroffer_min = getattr(args, "retailer_counteroffer_price_min", 0.01)
    counteroffer_max = getattr(args, "retailer_counteroffer_price_max", 100.0)
    if counteroffer_min <= 0 or counteroffer_max < counteroffer_min:
        raise SystemExit("invalid Retailer counteroffer price range")


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
    parser.add_argument("--seed", type=int, default=0)
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
    parser.add_argument(
        "--retailer-policy-mode",
        choices=("rule_based", "llm"),
        default="rule_based",
    )
    parser.add_argument(
        "--retailer-a-policy-mode",
        choices=("rule_based", "llm"),
        default=None,
    )
    parser.add_argument(
        "--retailer-b-policy-mode",
        choices=("rule_based", "llm"),
        default=None,
    )
    parser.add_argument("--max-negotiation-rounds", type=int, default=2)
    parser.add_argument("--retailer-counteroffer-price-min", type=float, default=0.01)
    parser.add_argument("--retailer-counteroffer-price-max", type=float, default=100.0)
    parser.add_argument(
        "--hide-retailer-offer-analysis",
        action="store_true",
        help="Hide derived offer-analysis values from LLM Retailers.",
    )
    parser.add_argument("--overwrite", action="store_true")
    args = parser.parse_args()
    if not args.model:
        parser.error("--model or OPENAI_MODEL is required")
    validate_args(args)
    result = run_condition(
        condition=args.condition,
        model=args.model,
        seed=args.seed,
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
    print_result(
        result,
        condition=args.condition,
        model=args.model,
        seed=args.seed,
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
    )


if __name__ == "__main__":
    main()
