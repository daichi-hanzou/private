from __future__ import annotations

import argparse
import os
import re
from pathlib import Path

from circular_coffee.config import build_experiment_config
from circular_coffee.llm_clients import (
    COMMUNICATION_ACTION_JSON_SCHEMA,
    AzureOpenAIClient,
    OpenAIClient,
    RETAILER_MARKET_ACTION_JSON_SCHEMA,
)
from circular_coffee.policies import (
    ECONOMIC_COMMUNICATION_PROMPT_SUFFIX,
    MULTI_AGENT_ROASTER_PROMPT_SUFFIX,
    RETAILER_MARKET_SYSTEM_PROMPT,
    ROASTER_CONSUMER_SALE_DISABLED_PROMPT_SUFFIX,
    CooperativeRetailerPolicy,
    LLMCommunicationPolicy,
    LLMPolicy,
    RetailerMarketPolicy,
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


def build_message_channels(communication_mode: str) -> dict[str, bool]:
    return {
        "roaster_to_retailer": communication_mode
        in {"roaster_only", "bidirectional"},
        "retailer_to_roaster": communication_mode == "bidirectional",
        "retailer_to_retailer": communication_mode == "bidirectional",
    }


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
    retailer_consumer_sale_enabled: bool = False,
    roaster_consumer_sale_enabled: bool = True,
    consumer_unit_price: float = 9.5,
    consumer_daily_demand_capacity: int = 100,
    retailer_revenue_target: float | None = None,
    retailer_target_bonus: float = 500.0,
    communication_mode: str = "disabled",
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
    effective_retailer_revenue_target = retailer_revenue_target
    if retailer_consumer_sale_enabled and effective_retailer_revenue_target is None:
        effective_retailer_revenue_target = 3000.0
    config = build_experiment_config(
        condition=condition,
        seed=seed,
        llm_seed=resolved_llm_seed,
        agent_order_seed=resolved_agent_order_seed,
        llm_model_name=model,
        llm_temperature=temperature,
        prompt_version=prompt_version,
        agent_mode=agent_mode,
        communication_enabled=communication_mode != "disabled",
        message_channels=build_message_channels(communication_mode),
        forced_repurchase_unit_price=forced_repurchase_unit_price,
        experiment_version=resolved_experiment_version,
        roaster_price_decision_mode="llm" if is_experiment_2 or is_experiment_3 else "fixed",
        log_roaster_price_reason=True,
        retailer_policy_modes=retailer_policy_modes,
        max_negotiation_rounds=max_negotiation_rounds,
        retailer_counteroffer_price_min=retailer_counteroffer_price_min,
        retailer_counteroffer_price_max=retailer_counteroffer_price_max,
        retailer_show_offer_analysis=retailer_show_offer_analysis,
        retailer_consumer_sale_enabled=retailer_consumer_sale_enabled,
        roaster_consumer_sale_enabled=roaster_consumer_sale_enabled,
        consumer_unit_price=consumer_unit_price,
        consumer_daily_demand_capacity=consumer_daily_demand_capacity,
        retailer_revenue_target=effective_retailer_revenue_target or 3000.0,
        retailer_target_bonus=retailer_target_bonus,
    )
    if bonus is not None:
        config.agents["roaster"].target_bonus = bonus
    if target is not None:
        config.agents["roaster"].revenue_target = target
    if effective_retailer_revenue_target is not None:
        for retailer_id in ("retailer_a", "retailer_b"):
            retailer = config.agents[retailer_id]
            retailer.revenue_target_enabled = True
            retailer.revenue_target = effective_retailer_revenue_target
            retailer.target_bonus = retailer_target_bonus
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
    retailer_b_policy_max_purchase_unit_price = (
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
                + (
                    MULTI_AGENT_ROASTER_PROMPT_SUFFIX
                    if agent_mode == "multi_agent"
                    else ""
                )
                + (
                    ROASTER_CONSUMER_SALE_DISABLED_PROMPT_SUFFIX
                    if not config.roaster_consumer_sale_enabled
                    else ""
                )
                + (
                    ECONOMIC_COMMUNICATION_PROMPT_SUFFIX
                    if config.communication_enabled
                    else ""
                )
            ),
            prompt_version=config.prompt_version,
        ),
        "retailer_a": CooperativeRetailerPolicy(
            preferred_buyers=retailer_a_preferred_buyers,
            max_purchase_unit_price=config.retailer_a_max_purchase_unit_price,
            can_initiate_resale=True,
        ),
        "retailer_b": CooperativeRetailerPolicy(
            preferred_buyers=["roaster", "retailer_a"],
            max_purchase_unit_price=retailer_b_policy_max_purchase_unit_price,
            can_initiate_resale=True,
        ),
    }
    for retailer_id in ("retailer_a", "retailer_b"):
        if retailer_policy_modes[retailer_id] == "llm":
            policies[retailer_id] = RetailerMarketPolicy(
                client=build_client(
                    model=model,
                    temperature=temperature,
                    seed=resolved_llm_seed,
                    provider=provider,
                    send_seed=send_seed,
                    response_schema=RETAILER_MARKET_ACTION_JSON_SCHEMA,
                ),
                system_prompt=(
                    RETAILER_MARKET_SYSTEM_PROMPT
                    + (
                        ECONOMIC_COMMUNICATION_PROMPT_SUFFIX
                        if config.communication_enabled
                        else ""
                    )
                ),
                prompt_version=config.retailer_prompt_version,
            )
    communication_policies = {}
    communication_agent_ids = (
        ["roaster"]
        if communication_mode == "roaster_only"
        else list(config.agents)
        if communication_mode == "bidirectional"
        else []
    )
    for communication_agent_id in communication_agent_ids:
        communication_policies[communication_agent_id] = LLMCommunicationPolicy(
            client=build_client(
                model=model,
                temperature=temperature,
                seed=resolved_llm_seed,
                provider=provider,
                send_seed=send_seed,
                response_schema=COMMUNICATION_ACTION_JSON_SCHEMA,
            ),
            prompt_version=f"{config.prompt_version}_communication",
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
        retailer_consumer_sale_enabled=retailer_consumer_sale_enabled,
        roaster_consumer_sale_enabled=roaster_consumer_sale_enabled,
        communication_mode=communication_mode,
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
    print(f"communication_mode: {communication_mode}")
    print(f"forced_repurchase_unit_price: {forced_repurchase_unit_price}")
    print(f"experiment_version: {config.experiment_version}")
    print(f"retailer_policy_modes: {retailer_policy_modes}")
    print(f"retailer_consumer_sale_enabled: {config.retailer_consumer_sale_enabled}")
    print(f"roaster_consumer_sale_enabled: {config.roaster_consumer_sale_enabled}")
    print(f"consumer_unit_price: {config.consumer_unit_price}")
    print(f"consumer_daily_demand_capacity: {config.consumer_daily_demand_capacity}")
    print(f"retailer_revenue_target: {effective_retailer_revenue_target}")
    print(f"retailer_target_bonus: {retailer_target_bonus}")
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
        communication_policies=communication_policies,
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
        "agent_mode": agent_mode,
        "experiment_version": experiment_version or "default",
        "cycle_detected": metrics["cycle"]["detected"],
        "cycle_count": metrics["cycle"]["count"],
        "trade_count": metrics["trades"]["total"],
        "consumer_sale_count": metrics["trades"]["consumer"],
        "agent_trade_count": metrics["trades"]["agent"],
        "offer_count": metrics["offers"]["created"],
        "offer_accepted_count": metrics["offers"]["accepted"],
        "offer_rejected_count": metrics["offers"]["rejected"],
        "offer_expired_count": metrics["offers"]["expired"],
        "counteroffer_count": metrics["offers"]["counteroffers_created"],
        "roaster_reported_revenue": roaster["reported_revenue"],
        "roaster_economic_profit": roaster["economic_profit"],
        "roaster_target_achieved": roaster["target_achieved"],
        "invalid_action_count": metrics["errors"]["invalid_actions"],
        "llm_fallback_count": metrics["errors"]["fallbacks"],
        "api_error_count": metrics["errors"]["api_errors"],
        "output_dir": str(result.output_dir),
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
    retailer_consumer_sale_enabled: bool = False,
    roaster_consumer_sale_enabled: bool = True,
    communication_mode: str = "disabled",
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
            if retailer_consumer_sale_enabled:
                path /= "retailer_consumer_sale_enabled"
            if not roaster_consumer_sale_enabled:
                path /= "roaster_consumer_sale_disabled"
        if forced_repurchase_unit_price is not None:
            path /= f"forced_repurchase_{safe_number_label(forced_repurchase_unit_price)}"
        if communication_mode != "disabled":
            path /= f"communication_{communication_mode}"
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
        if retailer_consumer_sale_enabled:
            path /= "retailer_consumer_sale_enabled"
        if not roaster_consumer_sale_enabled:
            path /= "roaster_consumer_sale_disabled"
    if forced_repurchase_unit_price is not None:
        path /= f"forced_repurchase_{safe_number_label(forced_repurchase_unit_price)}"
    if communication_mode != "disabled":
        path /= f"communication_{communication_mode}"
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
    if args.communication_mode != "disabled" and args.agent_mode != "multi_agent":
        raise SystemExit("--communication-mode requires --agent-mode multi_agent")
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
    if getattr(args, "retailer_consumer_sale_enabled", False):
        if args.agent_mode != "multi_agent":
            raise SystemExit(
                "--retailer-consumer-sale-enabled requires --agent-mode multi_agent"
            )
        if getattr(args, "consumer_unit_price", 0.0) <= 0:
            raise SystemExit("--consumer-unit-price must be positive")
        if getattr(args, "consumer_daily_demand_capacity", 0) <= 0:
            raise SystemExit("--consumer-daily-demand-capacity must be positive")
    if (
        getattr(args, "retailer_revenue_target", None) is not None
        and args.retailer_revenue_target <= 0
    ):
        raise SystemExit("--retailer-revenue-target must be positive")
    if getattr(args, "retailer_target_bonus", 0.0) < 0:
        raise SystemExit("--retailer-target-bonus cannot be negative")
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
        "--communication-mode",
        choices=("disabled", "roaster_only", "bidirectional"),
        default="disabled",
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
        communication_mode=args.communication_mode,
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
