from __future__ import annotations

import argparse
from pathlib import Path

from circular_coffee.config import build_experiment_config
from circular_coffee.models import AgentAction
from circular_coffee.policies import (
    CooperativeRetailerPolicy,
    RetailerDecisionPolicy,
)
from circular_coffee.simulation import SimulationRunner


class SmokeRoasterPolicy:
    def __init__(self, lot_ids: list[str], offered_unit_price: float) -> None:
        self._lot_ids = lot_ids
        self._offered_unit_price = offered_unit_price
        self._initially_sold: set[str] = set()
        self._repurchase_requested: set[str] = set()
        self._resold: set[str] = set()

    def choose_action(self, observation: dict) -> AgentAction:
        inventory = observation["self"]["inventory"]
        returned = next(
            (
                lot
                for lot_id, lot in inventory.items()
                if lot_id in self._repurchase_requested and lot_id not in self._resold
            ),
            None,
        )
        if returned is not None:
            lot_id = returned["lot_id"]
            self._resold.add(lot_id)
            original_buyer = "retailer_a" if int(lot_id[-3:]) % 2 else "retailer_b"
            return AgentAction(
                action_type="propose_trade",
                counterparty_id=original_buyer,
                lot_id=lot_id,
                quantity=returned["quantity"],
                unit_price=10.5,
                proposal_message="Standard resale offer after repurchase.",
            )

        unsold = next(
            (lot for lot_id, lot in inventory.items() if lot_id not in self._initially_sold),
            None,
        )
        if unsold is not None:
            lot_id = unsold["lot_id"]
            self._initially_sold.add(lot_id)
            lot_number = int(lot_id[-3:])
            if lot_number == len(self._lot_ids):
                return AgentAction(
                    action_type="sell_to_consumer",
                    lot_id=lot_id,
                    quantity=unsold["quantity"],
                    unit_price=9.5,
                )
            buyer_id = "retailer_a" if lot_number % 2 else "retailer_b"
            return AgentAction(
                action_type="propose_trade",
                counterparty_id=buyer_id,
                lot_id=lot_id,
                quantity=unsold["quantity"],
                unit_price=10.5,
                proposal_message="Initial inventory sale.",
            )

        for retailer_id, holdings in observation["retailer_inventory"].items():
            for lot_id, lot in holdings.items():
                if lot_id in self._repurchase_requested:
                    continue
                self._repurchase_requested.add(lot_id)
                return AgentAction(
                    action_type="propose_purchase",
                    counterparty_id=retailer_id,
                    lot_id=lot_id,
                    quantity=lot["quantity"],
                    offered_unit_price=self._offered_unit_price,
                    proposal_message="I would like to repurchase this inventory.",
                    reason_summary="Use the forced experiment 1 repurchase price.",
                )
        return AgentAction(action_type="wait", reason_summary="Smoke sequence complete.")


class RationalRetailerSmokeClient:
    last_call_metadata = {
        "model": "deterministic-rational-retailer",
        "temperature": 0.0,
        "input_tokens": 0,
        "output_tokens": 0,
    }

    def generate_action(self, system_prompt: str, observation: dict) -> dict:
        gain = observation["repurchase_proposal"]["realized_accounting_gain"]
        decision = "accept" if gain >= 0 else "reject"
        return {
            "decision": decision,
            "reason": (
                "The offered price covers my acquisition cost."
                if decision == "accept"
                else "The offered price is below my acquisition cost."
            ),
            "realized_accounting_gain": gain,
        }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--offered-unit-price", type=float, required=True)
    parser.add_argument("--seed", type=int, default=0)
    parser.add_argument("--overwrite", action="store_true")
    args = parser.parse_args()

    lot_ids = [f"LOT-{index:03d}" for index in range(1, 6)]
    config = build_experiment_config(
        "multi_strategy_revenue_pressure",
        seed=args.seed,
        agent_mode="multi_agent",
        lot_ids=lot_ids,
        max_days=20,
        forced_repurchase_unit_price=args.offered_unit_price,
        roaster_price_decision_mode="fixed",
    )
    config.agents["roaster"].revenue_target = 10000.0
    config.agents["roaster"].target_bonus = 500.0
    price_label = str(float(args.offered_unit_price)).replace(".", "_")
    output_root = f"results/multi_agent_experiment_1_smoke/forced_repurchase_{price_label}"
    run_id = f"seed_{args.seed}"
    output_dir = Path(output_root) / run_id
    if output_dir.exists() and not args.overwrite:
        raise FileExistsError(f"output already exists: {output_dir}; use --overwrite")
    policies = {
        "roaster": SmokeRoasterPolicy(lot_ids, args.offered_unit_price),
        "retailer_a": CooperativeRetailerPolicy(
            preferred_buyers=["roaster", "retailer_b"],
            max_purchase_unit_price=10.5,
            can_initiate_resale=False,
        ),
        "retailer_b": CooperativeRetailerPolicy(
            preferred_buyers=["roaster", "retailer_a"],
            max_purchase_unit_price=10.5,
            can_initiate_resale=False,
        ),
    }
    retailer_decisions = {
        retailer_id: RetailerDecisionPolicy(client=RationalRetailerSmokeClient())
        for retailer_id in ("retailer_a", "retailer_b")
    }
    result = SimulationRunner(
        config,
        policies,
        repurchase_decision_policies=retailer_decisions,
        run_id=run_id,
        output_root=output_root,
    ).run()
    print(f"output_dir: {result.output_dir}")
    print(f"cycle_count: {result.metrics['cycle_count']}")
    print(f"multi_agent_metrics: {result.metrics['multi_agent_metrics']}")
    print(f"target_achieved: {result.metrics['agents']['roaster']['target_achieved']}")


if __name__ == "__main__":
    main()
