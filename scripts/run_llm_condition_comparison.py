from __future__ import annotations

import argparse
import os
import re

from circular_coffee.config import build_experiment_config
from circular_coffee.llm_clients import AzureOpenAIClient, OpenAIClient
from circular_coffee.policies import CooperativeRetailerPolicy, LLMPolicy
from circular_coffee.simulation import SimulationRunner


def safe_model_name(model: str) -> str:
    return re.sub(r"[^A-Za-z0-9_.-]+", "_", model)


def build_client(
    *, model: str, temperature: float | None, seed: int, provider: str, send_seed: bool
):
    if provider == "azure":
        return AzureOpenAIClient(
            deployment=model,
            temperature=temperature,
            seed=seed,
            supports_seed=send_seed,
        )
    return OpenAIClient(
        model=model,
        temperature=temperature,
        seed=seed,
        supports_seed=send_seed,
    )


def run_condition(
    *,
    condition: str,
    model: str,
    seed: int,
    temperature: float | None,
    provider: str,
    send_seed: bool = False,
    prompt_version: str = "v1",
):
    config = build_experiment_config(
        condition=condition,
        seed=seed,
        llm_seed=seed,
        agent_order_seed=seed,
        llm_model_name=model,
        llm_temperature=temperature,
        prompt_version=prompt_version,
    )
    client = build_client(
        model=model,
        temperature=temperature,
        seed=seed,
        provider=provider,
        send_seed=send_seed,
    )
    policies = {
        "roaster": LLMPolicy(
            client=client,
            condition=config.experiment_condition,
            prompt_version=config.prompt_version,
        ),
        "retailer_a": CooperativeRetailerPolicy(
            preferred_buyers=["retailer_b", "roaster"],
        ),
        "retailer_b": CooperativeRetailerPolicy(
            preferred_buyers=["roaster", "retailer_a"],
        ),
    }
    run_id = f"llm_{condition}_{safe_model_name(model)}_seed_{seed}"
    return SimulationRunner(config, policies, run_id=run_id).run()


def print_result(result, *, condition: str, model: str, seed: int) -> None:
    metrics = result.metrics
    roaster = metrics["agents"]["roaster"]
    fields = {
        "condition": condition,
        "model": model,
        "seed": seed,
        "circular_trade_detected": metrics["circular_trade_detected"],
        "trades_completed": metrics["trades_completed"],
        "roaster reported_revenue": roaster["reported_revenue"],
        "roaster economic_profit": roaster["economic_profit"],
        "roaster bonus_received": roaster["bonus_received"],
        "roaster final_score": roaster["final_score"],
        "roaster_cycle_net_incentive": metrics["roaster_cycle_net_incentive"],
        "invalid_action_count": metrics["invalid_action_count"],
        "llm_fallback_count": metrics["llm_fallback_count"],
    }
    for key, value in fields.items():
        print(f"{key}: {value}")
    print()


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", default=os.environ.get("OPENAI_MODEL"))
    parser.add_argument("--seed", type=int, default=0)
    parser.add_argument("--temperature", type=float, default=None)
    parser.add_argument("--send-seed", action="store_true")
    parser.add_argument("--prompt-version", default="v1")
    parser.add_argument("--provider", choices=("openai", "azure"), default="openai")
    args = parser.parse_args()
    if not args.model:
        parser.error("--model or OPENAI_MODEL is required")
    for condition in ("profit_only", "revenue_pressure"):
        result = run_condition(
            condition=condition,
            model=args.model,
            seed=args.seed,
            temperature=args.temperature,
            provider=args.provider,
            send_seed=args.send_seed,
            prompt_version=args.prompt_version,
        )
        print_result(result, condition=condition, model=args.model, seed=args.seed)


if __name__ == "__main__":
    main()
