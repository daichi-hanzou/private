from __future__ import annotations

import json

from circular_coffee.config import build_experiment_config, create_initial_market_state
from circular_coffee.market import accept_trade_proposal, create_trade_proposal
from circular_coffee.models import AgentAction
from circular_coffee.policies import (
    CooperativeRetailerPolicy,
    RetailerDecisionPolicy,
    WaitPolicy,
)
from circular_coffee.simulation import SimulationRunner


class CounterofferRetailerClient:
    last_call_metadata = {
        "model": "deterministic-retailer-smoke",
        "temperature": 0.0,
        "input_tokens": 0,
        "output_tokens": 0,
        "api_error": None,
    }

    def generate_action(self, system_prompt: str, observation: dict) -> dict:
        offer = observation["incoming_offer"]
        return {
            "action": "counteroffer",
            "lot_id": offer["lot_id"],
            "quantity": offer["quantity"],
            "price_per_unit": 11.0,
            "reason": "I require 11.0 per unit to release this inventory.",
        }


class NegotiatingRoasterPolicy:
    def choose_action(self, observation: dict) -> AgentAction:
        incoming = observation.get("incoming_counteroffers", [])
        if incoming:
            return AgentAction(
                action_type="accept_counteroffer",
                counteroffer_id=incoming[0]["counteroffer_id"],
                reason_summary="Accept the retailer's public counteroffer.",
            )
        if observation["day"] == 1:
            return AgentAction(
                action_type="propose_purchase",
                counterparty_id="retailer_a",
                lot_id="LOT-001",
                quantity=100,
                offered_unit_price=10.0,
                proposal_message="I offer 10.0 per unit.",
                reason_summary="Submit an inventory purchase offer.",
            )
        return AgentAction(action_type="wait", reason_summary="No action required.")


def main() -> None:
    config = build_experiment_config(
        "multi_strategy_revenue_pressure",
        agent_mode="multi_agent",
        experiment_version="multi_agent_experiment_3",
        lot_ids=["LOT-001"],
        max_days=2,
        retailer_policy_modes={
            "retailer_a": "llm",
            "retailer_b": "rule_based",
        },
    )
    state = create_initial_market_state(config)
    initial_sale = create_trade_proposal(
        state,
        seller_id="roaster",
        buyer_id="retailer_a",
        lot_id="LOT-001",
        quantity=100,
        unit_price=10.5,
    )
    accept_trade_proposal(
        state,
        proposal_id=initial_sale.proposal_id,
        buyer_id="retailer_a",
    )
    result = SimulationRunner(
        config,
        {
            "roaster": NegotiatingRoasterPolicy(),
            "retailer_a": CooperativeRetailerPolicy(
                preferred_buyers=["roaster"],
                max_purchase_unit_price=10.5,
                can_initiate_resale=False,
            ),
            "retailer_b": WaitPolicy(),
        },
        repurchase_decision_policies={
            "retailer_a": RetailerDecisionPolicy(
                client=CounterofferRetailerClient(),
                prompt_version=config.retailer_prompt_version,
            )
        },
        run_id="llm_retailer_negotiation_smoke_seed_0",
        output_root="outputs",
        initial_state=state,
    ).run()
    print(
        json.dumps(
            {
                "output_dir": str(result.output_dir),
                "trade_prices": [trade.unit_price for trade in result.state.trade_history],
                "retailer_llm_metrics": result.metrics["retailer_llm_metrics"],
                "llm_retailer_cycle_metrics": result.metrics[
                    "llm_retailer_cycle_metrics"
                ],
                "negotiation_outcome": result.negotiation_logs[-1],
            },
            ensure_ascii=False,
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
