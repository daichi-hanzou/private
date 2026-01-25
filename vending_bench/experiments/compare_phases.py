"""
Compare Phase 1 (baseline) vs Phase 2 (with CEO) performance.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from vending_bench.config import Config


@dataclass
class SimulationResult:
    """Result of a single simulation run."""

    mode: str  # "phase1_baseline" or "phase2_with_ceo"
    seed: int
    max_days: int
    final_net_worth: float
    final_balance: float
    total_revenue: float
    total_units_sold: int
    bankruptcy: bool
    days_survived: int

    # Phase 2 specific metrics
    meltdown_count: int = 0
    below_cost_sales: int = 0
    zero_price_days: int = 0
    ceo_veto_count: int = 0
    ceo_approval_count: int = 0
    ceo_intervention_count: int = 0
    kpi_compliance_rate: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary."""
        return {
            "mode": self.mode,
            "seed": self.seed,
            "max_days": self.max_days,
            "final_net_worth": self.final_net_worth,
            "final_balance": self.final_balance,
            "total_revenue": self.total_revenue,
            "total_units_sold": self.total_units_sold,
            "bankruptcy": self.bankruptcy,
            "days_survived": self.days_survived,
            "meltdown_count": self.meltdown_count,
            "below_cost_sales": self.below_cost_sales,
            "zero_price_days": self.zero_price_days,
            "ceo_veto_count": self.ceo_veto_count,
            "ceo_approval_count": self.ceo_approval_count,
            "ceo_intervention_count": self.ceo_intervention_count,
            "kpi_compliance_rate": self.kpi_compliance_rate,
        }


@dataclass
class ComparisonResult:
    """Result of comparing Phase 1 vs Phase 2."""

    seed: int
    max_days: int
    phase1_result: SimulationResult
    phase2_result: SimulationResult
    improvements: dict[str, float] = field(default_factory=dict)

    def calculate_improvements(self) -> None:
        """Calculate improvement metrics."""
        p1 = self.phase1_result
        p2 = self.phase2_result

        # Net worth improvement
        if p1.final_net_worth > 0:
            self.improvements["net_worth_delta"] = p2.final_net_worth - p1.final_net_worth
            self.improvements["net_worth_improvement_pct"] = (
                (p2.final_net_worth - p1.final_net_worth) / p1.final_net_worth * 100
            )

        # Meltdown reduction
        self.improvements["meltdown_reduction"] = p1.meltdown_count - p2.meltdown_count

        # Below cost sales reduction
        self.improvements["below_cost_reduction"] = (
            p1.below_cost_sales - p2.below_cost_sales
        )

        # Survival improvement
        self.improvements["days_survived_delta"] = p2.days_survived - p1.days_survived

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary."""
        return {
            "seed": self.seed,
            "max_days": self.max_days,
            "phase1": self.phase1_result.to_dict(),
            "phase2": self.phase2_result.to_dict(),
            "improvements": self.improvements,
        }

    def save(self, output_file: str | Path) -> None:
        """Save comparison result to JSON file."""
        output_file = Path(output_file)
        output_file.parent.mkdir(parents=True, exist_ok=True)

        with open(output_file, "w") as f:
            json.dump(self.to_dict(), f, indent=2)

        print(f"Comparison result saved to {output_file}")

    def print_summary(self) -> None:
        """Print a human-readable summary."""
        print("\n" + "=" * 60)
        print("PHASE 1 vs PHASE 2 COMPARISON")
        print("=" * 60)
        print(f"Seed: {self.seed}, Max Days: {self.max_days}")
        print()

        print("PHASE 1 (Baseline):")
        print(f"  Final Net Worth: ${self.phase1_result.final_net_worth:.2f}")
        print(f"  Days Survived: {self.phase1_result.days_survived}")
        print(f"  Total Revenue: ${self.phase1_result.total_revenue:.2f}")
        print(f"  Bankruptcy: {self.phase1_result.bankruptcy}")
        print()

        print("PHASE 2 (With CEO):")
        print(f"  Final Net Worth: ${self.phase2_result.final_net_worth:.2f}")
        print(f"  Days Survived: {self.phase2_result.days_survived}")
        print(f"  Total Revenue: ${self.phase2_result.total_revenue:.2f}")
        print(f"  Bankruptcy: {self.phase2_result.bankruptcy}")
        print(f"  CEO Vetoes: {self.phase2_result.ceo_veto_count}")
        print(f"  CEO Approvals: {self.phase2_result.ceo_approval_count}")
        print(f"  CEO Interventions: {self.phase2_result.ceo_intervention_count}")
        print(f"  KPI Compliance: {self.phase2_result.kpi_compliance_rate*100:.1f}%")
        print()

        print("IMPROVEMENTS:")
        for key, value in self.improvements.items():
            print(f"  {key}: {value:.2f}")
        print("=" * 60 + "\n")


def run_comparison_experiment(
    seed: int,
    max_days: int = 30,
    config_override: dict[str, Any] | None = None,
) -> ComparisonResult:
    """
    Run comparison experiment between Phase 1 and Phase 2.

    Args:
        seed: Random seed for reproducibility
        max_days: Maximum number of days to simulate
        config_override: Optional config overrides

    Returns:
        ComparisonResult with performance comparison
    """
    # This is a stub - actual implementation would run the simulations
    # For now, return a placeholder

    # In full implementation, would:
    # 1. Load config
    # 2. Run Phase 1 simulation
    # 3. Run Phase 2 simulation with same seed
    # 4. Collect metrics
    # 5. Calculate improvements

    print(f"Running comparison experiment with seed={seed}, max_days={max_days}")
    print("Note: This is a stub implementation. Full simulation not yet integrated.")

    # Placeholder results
    phase1 = SimulationResult(
        mode="phase1_baseline",
        seed=seed,
        max_days=max_days,
        final_net_worth=500.0,
        final_balance=480.0,
        total_revenue=100.0,
        total_units_sold=50,
        bankruptcy=False,
        days_survived=max_days,
        meltdown_count=0,
        below_cost_sales=0,
    )

    phase2 = SimulationResult(
        mode="phase2_with_ceo",
        seed=seed,
        max_days=max_days,
        final_net_worth=520.0,
        final_balance=500.0,
        total_revenue=120.0,
        total_units_sold=55,
        bankruptcy=False,
        days_survived=max_days,
        meltdown_count=0,
        below_cost_sales=0,
        ceo_veto_count=2,
        ceo_approval_count=15,
        ceo_intervention_count=0,
        kpi_compliance_rate=0.95,
    )

    result = ComparisonResult(
        seed=seed,
        max_days=max_days,
        phase1_result=phase1,
        phase2_result=phase2,
    )

    result.calculate_improvements()

    return result


def run_multiple_comparisons(
    seeds: list[int],
    max_days: int = 30,
    output_dir: str | Path = "./output/comparisons",
) -> list[ComparisonResult]:
    """
    Run multiple comparison experiments with different seeds.

    Args:
        seeds: List of random seeds
        max_days: Maximum days per simulation
        output_dir: Directory to save results

    Returns:
        List of comparison results
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    results = []

    for seed in seeds:
        print(f"\nRunning comparison with seed {seed}...")
        result = run_comparison_experiment(seed, max_days)
        result.print_summary()

        # Save individual result
        result.save(output_dir / f"comparison_seed_{seed}.json")

        results.append(result)

    # Save aggregate summary
    aggregate = {
        "num_experiments": len(results),
        "seeds": seeds,
        "max_days": max_days,
        "results": [r.to_dict() for r in results],
    }

    with open(output_dir / "aggregate_results.json", "w") as f:
        json.dump(aggregate, f, indent=2)

    print(f"\nAggregate results saved to {output_dir / 'aggregate_results.json'}")

    return results


if __name__ == "__main__":
    # Example usage
    seeds = [42, 43, 44, 45, 46]
    results = run_multiple_comparisons(seeds, max_days=30)

    print("\n" + "=" * 60)
    print("AGGREGATE SUMMARY")
    print("=" * 60)
    print(f"Experiments run: {len(results)}")
    print()

    avg_improvement = sum(r.improvements.get("net_worth_delta", 0) for r in results) / len(
        results
    )
    print(f"Average Net Worth Improvement: ${avg_improvement:.2f}")

    avg_pct = sum(r.improvements.get("net_worth_improvement_pct", 0) for r in results) / len(
        results
    )
    print(f"Average Net Worth Improvement %: {avg_pct:.2f}%")

    print("=" * 60)
