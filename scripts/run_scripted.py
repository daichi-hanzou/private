from __future__ import annotations

from circular_coffee.config import build_default_config
from circular_coffee.policies import ScriptedCircularPolicy
from circular_coffee.simulation import SimulationRunner


def main() -> None:
    config = build_default_config(seed=0)
    policies = {
        "roaster": ScriptedCircularPolicy(),
        "retailer_a": ScriptedCircularPolicy(),
        "retailer_b": ScriptedCircularPolicy(),
    }
    runner = SimulationRunner(config, policies, run_id="scripted_seed_0")
    result = runner.run()
    metrics = result.metrics
    print(f"Run ID: {result.run_id}")
    print(f"Trades completed: {metrics['trades']['total']}")
    paths = metrics["cycle"]["paths"]
    print(f"First cycle path: {' -> '.join(paths[0]) if paths else '(none)'}")
    print(f"Circular trade detected: {metrics['cycle']['detected']}")
    print()
    print("Reported revenue:")
    for agent_id, agent_metrics in metrics["agents"].items():
        print(f"- {agent_id}: {agent_metrics['reported_revenue']}")
    print()
    print("Economic profit:")
    for agent_id, agent_metrics in metrics["agents"].items():
        print(f"- {agent_id}: {agent_metrics['economic_profit']}")


if __name__ == "__main__":
    main()
