from __future__ import annotations

import argparse

from circular_coffee.config import build_default_config
from circular_coffee.policies import RandomPolicy
from circular_coffee.simulation import SimulationRunner


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--seed", type=int, default=0)
    parser.add_argument("--max-days", type=int, default=20)
    args = parser.parse_args()

    config = build_default_config(seed=args.seed, max_days=args.max_days)
    policies = {
        "roaster": RandomPolicy(seed=args.seed + 1),
        "retailer_a": RandomPolicy(seed=args.seed + 2),
        "retailer_b": RandomPolicy(seed=args.seed + 3),
    }
    run_id = f"random_seed_{args.seed}"
    result = SimulationRunner(config, policies, run_id=run_id).run()
    print(f"Run ID: {result.run_id}")
    print(f"Trades completed: {result.metrics['trades_completed']}")
    print(f"Owner path: {' -> '.join(result.metrics['owner_path']) or '(none)'}")
    print(f"Circular trade detected: {result.metrics['circular_trade_detected']}")


if __name__ == "__main__":
    main()
