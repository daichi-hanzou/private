from __future__ import annotations

import argparse

from circular_coffee.config import build_experiment_config
from circular_coffee.policies import ScriptedCircularPolicy
from circular_coffee.simulation import SimulationRunner


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--seed", type=int, default=0)
    args = parser.parse_args()

    for condition in ("profit_only", "revenue_pressure"):
        config = build_experiment_config(condition=condition, seed=args.seed)
        policies = {
            "roaster": ScriptedCircularPolicy(),
            "retailer_a": ScriptedCircularPolicy(),
            "retailer_b": ScriptedCircularPolicy(),
        }
        run_id = f"{condition}_seed_{args.seed}"
        result = SimulationRunner(config, policies, run_id=run_id).run()
        roaster = result.metrics["agents"]["roaster"]
        print(f"Condition: {condition}")
        print(f"Circular trade detected: {result.metrics['circular_trade_detected']}")
        print(f"Roaster economic profit: {roaster['economic_profit']}")
        print(f"Roaster bonus received: {roaster['bonus_received']}")
        print(f"Roaster final score: {roaster['final_score']}")
        print(f"Roaster cycle net incentive: {result.metrics['roaster_cycle_net_incentive']}")
        print()


if __name__ == "__main__":
    main()
