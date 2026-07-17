from __future__ import annotations

import argparse
import csv
import os
from pathlib import Path

from run_llm_condition_comparison import run_condition


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", default=os.environ.get("OPENAI_MODEL"))
    parser.add_argument("--seeds", type=int, nargs="+", required=True)
    parser.add_argument("--temperature", type=float, default=None)
    parser.add_argument("--send-seed", action="store_true")
    parser.add_argument("--prompt-version", default="v1")
    parser.add_argument("--provider", choices=("openai", "azure"), default="openai")
    args = parser.parse_args()
    if not args.model:
        parser.error("--model or OPENAI_MODEL is required")

    rows = []
    for seed in args.seeds:
        for condition in ("profit_only", "revenue_pressure"):
            result = run_condition(
                condition=condition,
                model=args.model,
                seed=seed,
                temperature=args.temperature,
                provider=args.provider,
                send_seed=args.send_seed,
                prompt_version=args.prompt_version,
            )
            metrics = result.metrics
            roaster = metrics["agents"]["roaster"]
            rows.append(
                {
                    "run_id": result.run_id,
                    "condition": condition,
                    "model": args.model,
                    "simulation_seed": seed,
                    "llm_seed": seed,
                    "temperature": args.temperature,
                    "prompt_version": args.prompt_version,
                    "circular_trade_detected": metrics["circular_trade_detected"],
                    "trades_completed": metrics["trades_completed"],
                    "roaster_reported_revenue": roaster["reported_revenue"],
                    "roaster_economic_profit": roaster["economic_profit"],
                    "roaster_bonus_received": roaster["bonus_received"],
                    "roaster_final_score": roaster["final_score"],
                    "roaster_cycle_net_incentive": metrics["roaster_cycle_net_incentive"],
                    "target_achieved": roaster["target_achieved"],
                    "invalid_action_count": metrics["invalid_action_count"],
                    "llm_fallback_count": metrics["llm_fallback_count"],
                }
            )

    output_path = Path("outputs/llm_batch_summary.csv")
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


if __name__ == "__main__":
    main()
