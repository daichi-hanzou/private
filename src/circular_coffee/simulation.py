from __future__ import annotations

import copy
import math
import random
import re
from dataclasses import dataclass
from pathlib import Path

from .config import SimulationConfig, build_market_information, create_initial_market_state
from .logging_utils import ensure_dir, write_json, write_jsonl
from .market import (
    InvalidActionError,
    accept_trade_proposal,
    create_trade_proposal,
    execute_consumer_sale,
    execute_repurchase,
    expire_old_proposals,
    reject_trade_proposal,
)
from .metrics import collect_metrics, economic_inventory_value
from .models import AgentAction, MarketState, RepurchaseProposal
from .observation import (
    add_multi_agent_roaster_information,
    build_observation,
    build_repurchase_decision_observation,
)
from .policies import (
    AgentPolicy,
    PRICE_LIMIT_REJECTION_REASON,
    RetailerDecisionPolicy,
    WaitPolicy,
    policy_name,
)


@dataclass
class SimulationResult:
    run_id: str
    state: MarketState
    metrics: dict
    action_logs: list[dict]
    proposal_logs: list[dict]
    trade_logs: list[dict]
    output_dir: Path


class SimulationRunner:
    def __init__(
        self,
        config: SimulationConfig,
        policies: dict[str, AgentPolicy],
        *,
        run_id: str,
        output_root: str | Path = "outputs",
        initial_state: MarketState | None = None,
        repurchase_decision_policies: dict[str, RetailerDecisionPolicy] | None = None,
    ):
        self.config = config
        self.policies = policies
        self.repurchase_decision_policies = repurchase_decision_policies or {}
        self.run_id = run_id
        self.output_dir = Path(output_root) / run_id
        self.state = initial_state or create_initial_market_state(config)
        self._rng = random.Random(
            config.agent_order_seed if config.agent_order_seed is not None else config.seed
        )
        self._action_logs: list[dict] = []
        self._proposal_logs: list[dict] = []
        self._trade_logs: list[dict] = []
        self._initial_state_snapshot = copy.deepcopy(self.state)
        self._initial_cash_by_agent = {
            agent_id: agent.cash for agent_id, agent in self.state.agents.items()
        }
        self._initial_inventory_value_by_agent = {
            agent_id: economic_inventory_value(agent) for agent_id, agent in self.state.agents.items()
        }

    def run(self) -> SimulationResult:
        ensure_dir(self.output_dir)
        self._write_static_logs()
        for day in range(1, self.config.max_days + 1):
            self.state.day = day
            for agent_id in self._ordered_agent_ids():
                observation = build_observation(
                    self.state,
                    agent_id,
                    initial_cash=self._initial_cash_by_agent[agent_id],
                    initial_inventory_value=self._initial_inventory_value_by_agent[agent_id],
                    market_information=build_market_information(self.config, self.state),
                )
                if self.config.agent_mode == "multi_agent" and agent_id == "roaster":
                    add_multi_agent_roaster_information(
                        observation,
                        self.state,
                        config=self.config,
                        proposal_logs=self._proposal_logs,
                    )
                chosen_action = self._choose_action(agent_id, observation)
                llm_log = self._consume_llm_log(agent_id)
                self._execute_action(agent_id, observation, chosen_action, llm_log=llm_log)
            expired = expire_old_proposals(
                self.state,
                proposal_expiry_days=self.config.proposal_expiry_days,
            )
            for proposal in expired:
                self._proposal_logs.append(
                    {"event": "expired", "day": day, "proposal": proposal}
                )
        metrics = collect_metrics(
            self.state,
            lot_ids=self.config.lot_ids,
            initial_cash_by_agent=self._initial_cash_by_agent,
            initial_inventory_value_by_agent=self._initial_inventory_value_by_agent,
            lot_quantity=self.config.lot_quantity,
            retailer_a_max_purchase_unit_price=self.config.retailer_a_max_purchase_unit_price,
            retailer_b_max_purchase_unit_price=self.config.retailer_b_max_purchase_unit_price,
            consumer_market_enabled=self.config.consumer_market_enabled,
            consumer_max_unit_price=self.config.consumer_max_unit_price,
        )
        metrics.update(self._quality_metrics())
        self._write_final_logs(metrics)
        return SimulationResult(
            run_id=self.run_id,
            state=self.state,
            metrics=metrics,
            action_logs=self._action_logs,
            proposal_logs=self._proposal_logs,
            trade_logs=self._trade_logs,
            output_dir=self.output_dir,
        )

    def _ordered_agent_ids(self) -> list[str]:
        agent_ids = list(self.state.agents.keys())
        if self.config.agent_order_mode == "random":
            self._rng.shuffle(agent_ids)
        return agent_ids

    def _choose_action(self, agent_id: str, observation: dict) -> AgentAction:
        policy = self.policies.get(agent_id, WaitPolicy())
        try:
            return policy.choose_action(observation)
        except Exception as exc:
            self._action_logs.append(
                {
                    "run_id": self.run_id,
                    "day": self.state.day,
                    "agent_id": agent_id,
                    "observation": observation,
                    "action": {"action_type": "wait"},
                    "is_valid": False,
                    "error": f"policy_error: {exc}",
                }
            )
            return AgentAction(action_type="wait", reason_summary="Policy error fallback.")

    def _execute_action(
        self,
        agent_id: str,
        observation: dict,
        action: AgentAction,
        *,
        llm_log: dict | None = None,
    ) -> None:
        error: str | None = None
        is_valid = True
        sale_completed: bool | None = None
        action_metadata: dict | None = None
        try:
            if action.action_type == "propose_trade":
                proposal = create_trade_proposal(
                    self.state,
                    seller_id=agent_id,
                    buyer_id=self._required(action.counterparty_id, "counterparty_id"),
                    lot_id=self._required(action.lot_id, "lot_id"),
                    quantity=self._required(action.quantity, "quantity"),
                    unit_price=self._required(action.unit_price, "unit_price"),
                    proposal_message=action.proposal_message,
                )
                self._proposal_logs.append(
                    {"event": "created", "day": self.state.day, "proposal": proposal}
                )
            elif action.action_type == "accept_trade":
                proposal_id = self._required(action.proposal_id, "proposal_id")
                trade = accept_trade_proposal(
                    self.state,
                    proposal_id=proposal_id,
                    buyer_id=agent_id,
                    transaction_fee_rate=self.config.transaction_fee_rate,
                )
                proposal = self.state.pending_proposals[proposal_id]
                self._proposal_logs.append(
                    {"event": "accepted", "day": self.state.day, "proposal": proposal}
                )
                self._trade_logs.append({"event": "completed", "day": self.state.day, "trade": trade})
            elif action.action_type == "propose_purchase":
                action_metadata = self._execute_repurchase_proposal(agent_id, action)
            elif action.action_type == "reject_trade":
                proposal = reject_trade_proposal(
                    self.state,
                    proposal_id=self._required(action.proposal_id, "proposal_id"),
                    buyer_id=agent_id,
                )
                self._proposal_logs.append(
                    {"event": "rejected", "day": self.state.day, "proposal": proposal}
                )
            elif action.action_type == "sell_to_consumer":
                trade = execute_consumer_sale(
                    self.state,
                    seller_id=agent_id,
                    lot_id=self._required(action.lot_id, "lot_id"),
                    quantity=self._required(action.quantity, "quantity"),
                    unit_price=self._required(action.unit_price, "unit_price"),
                    consumer_market_enabled=self.config.consumer_market_enabled,
                    consumer_max_unit_price=self.config.consumer_max_unit_price,
                )
                sale_completed = trade is not None
                if trade is not None:
                    self._trade_logs.append({"event": "completed", "day": self.state.day, "trade": trade})
            elif action.action_type == "wait":
                pass
            else:
                raise InvalidActionError(f"unsupported action_type: {action.action_type}")
        except Exception as exc:
            error = str(exc)
            is_valid = False
        row = {
                "run_id": self.run_id,
                "day": self.state.day,
                "agent_id": agent_id,
                "observation": observation,
                "action": action,
                "is_valid": is_valid,
                "error": error,
            }
        if llm_log is not None:
            row["llm"] = llm_log
            row["price_field_normalized"] = bool(
                llm_log.get("price_field_normalized")
            )
            row["original_price_field"] = llm_log.get("original_price_field")
            row["normalized_price_field"] = llm_log.get("normalized_price_field")
            row["schema_payload_unwrapped"] = bool(
                llm_log.get("schema_payload_unwrapped")
            )
        if sale_completed is not None:
            row["sale_completed"] = sale_completed
        if action_metadata is not None:
            row.update(action_metadata)
        if agent_id == "roaster" and action.action_type == "propose_purchase":
            self_view = observation.get("self", {})
            retailer_history = observation.get("retailer_response_history", {}).get(
                action.counterparty_id,
                [],
            )
            row.update(
                {
                    "retailer_id": action.counterparty_id,
                    "lot_id": action.lot_id,
                    "quantity": action.quantity,
                    "offered_unit_price": action.offered_unit_price,
                    "reason": action.reason_summary,
                    "remaining_revenue_gap": self_view.get("revenue_target_shortfall"),
                    "target_bonus": self_view.get("target_bonus"),
                    "available_cash": self_view.get("cash"),
                    "observed_history": retailer_history,
                    "model_name": llm_log.get("model") if llm_log else None,
                    "llm_fallback_used": bool(
                        llm_log and llm_log.get("fallback_used")
                    ),
                    "json_parse_error": bool(
                        llm_log and llm_log.get("parse_error")
                    ),
                }
            )
        self._action_logs.append(row)

    def _execute_repurchase_proposal(self, agent_id: str, action: AgentAction) -> dict:
        if self.config.agent_mode != "multi_agent":
            raise InvalidActionError("propose_purchase requires agent_mode=multi_agent")
        if self.config.repurchase_proposer != "roaster":
            raise InvalidActionError("experiment 1 repurchase_proposer must be roaster")
        if agent_id != "roaster":
            raise InvalidActionError("only roaster can propose a purchase offer")
        retailer_id = self._required(action.counterparty_id, "counterparty_id")
        lot_id = self._required(action.lot_id, "lot_id")
        quantity = self._required(action.quantity, "quantity")
        offered_unit_price = (
            self.config.forced_repurchase_unit_price
            if self.config.forced_repurchase_unit_price is not None
            else action.offered_unit_price
        )
        original_offered_unit_price = offered_unit_price
        if offered_unit_price is None and action.offered_price is not None:
            offered_unit_price = action.offered_price / quantity
        offered_unit_price = self._required(offered_unit_price, "offered_unit_price")
        price_normalized = False
        if (
            self.config.experiment_version == "multi_agent_experiment_3"
            and self.config.repurchase_price_increment > 0
        ):
            offered_unit_price = self._normalize_repurchase_offer_price(offered_unit_price)
            price_normalized = not math.isclose(
                float(offered_unit_price),
                float(original_offered_unit_price),
                abs_tol=1e-9,
            )
        if self.config.experiment_version == "multi_agent_experiment_3":
            if (
                self.config.repurchase_price_min > 0
                and offered_unit_price < self.config.repurchase_price_min
            ):
                raise InvalidActionError("offered_unit_price below repurchase price minimum")
            if (
                self.config.repurchase_price_max > 0
                and offered_unit_price > self.config.repurchase_price_max
            ):
                raise InvalidActionError("offered_unit_price above repurchase price maximum")
        if retailer_id not in self.repurchase_decision_policies:
            raise InvalidActionError("retailer has no independent repurchase decision policy")
        retailer = self.state.agents.get(retailer_id)
        if retailer is None or retailer.role != "retailer":
            raise InvalidActionError("purchase offer recipient must be a retailer")
        if lot_id not in retailer.inventory:
            raise InvalidActionError("retailer does not own proposed lot")
        lot = retailer.inventory[lot_id]
        if quantity != lot.quantity:
            raise InvalidActionError("quantity must match full lot quantity")
        cash_proceeds = round(offered_unit_price * quantity, 2)
        fee = round(cash_proceeds * self.config.transaction_fee_rate, 2)
        if self.state.agents["roaster"].cash < cash_proceeds + fee:
            raise InvalidActionError("roaster has insufficient cash")

        action.offered_unit_price = offered_unit_price
        acquisition_unit_price = lot.carrying_unit_cost
        economic_unit_value = lot.original_unit_cost
        realized_accounting_gain = round(
            (offered_unit_price - acquisition_unit_price) * quantity,
            2,
        )
        economic_surplus_vs_value = round(
            (offered_unit_price - economic_unit_value) * quantity,
            2,
        )
        proposal_id = f"repurchase-proposal-{sum(row.get('event_type') == 'repurchase_proposal' for row in self._proposal_logs) + 1}"
        proposal = RepurchaseProposal(
            proposal_id=proposal_id,
            proposal_type="repurchase",
            proposer_id=self.config.repurchase_proposer,
            recipient_id=retailer_id,
            seller_id=retailer_id,
            buyer_id="roaster",
            lot_id=lot_id,
            quantity=quantity,
            unit_price=offered_unit_price,
            proposal_message=action.proposal_message,
            status="pending",
            created_day=self.state.day,
        )
        if not self._is_valid_experiment_1_repurchase(proposal):
            raise InvalidActionError("invalid experiment 1 repurchase proposal roles")
        decision_observation = build_repurchase_decision_observation(
            self.state,
            retailer_id=retailer_id,
            action=action,
        )
        policy = self.repurchase_decision_policies[retailer_id]
        decision = policy.choose_decision(decision_observation)
        retailer_llm_log = policy.consume_last_llm_log()
        trade = None
        if decision.decision == "accept":
            trade = execute_repurchase(
                self.state,
                retailer_id=retailer_id,
                lot_id=lot_id,
                offered_price=cash_proceeds,
                transaction_fee_rate=self.config.transaction_fee_rate,
            )
            self._trade_logs.append(
                {"event": "completed", "day": self.state.day, "trade": trade}
            )
            proposal.status = "accepted"
        else:
            proposal.status = "rejected"
        proposal.decision_day = self.state.day
        message = action.proposal_message or ""
        self._proposal_logs.append(
            {
                "day": self.state.day,
                "event_type": "repurchase_proposal",
                "proposal_id": proposal.proposal_id,
                "proposal_type": proposal.proposal_type,
                "proposer_id": proposal.proposer_id,
                "recipient_id": proposal.recipient_id,
                "seller_id": proposal.seller_id,
                "buyer_id": proposal.buyer_id,
                "lot_id": proposal.lot_id,
                "quantity": proposal.quantity,
                "acquisition_unit_price": acquisition_unit_price,
                "offered_unit_price": offered_unit_price,
                "economic_unit_value": economic_unit_value,
                "cash_proceeds": cash_proceeds,
                "realized_accounting_gain": realized_accounting_gain,
                "economic_surplus_vs_value": economic_surplus_vs_value,
                "proposal_message": action.proposal_message,
                "roaster_message": action.proposal_message,
                "roaster_price_reason": (
                    action.reason_summary if self.config.log_roaster_price_reason else None
                ),
                "retailer_decision": decision.decision,
                "retailer_reason": decision.reason,
                "reservation_price": self._reservation_price_for_retailer(retailer_id),
                "trade_completed": trade is not None,
                "status": proposal.status,
                "decision_day": proposal.decision_day,
                "roaster_kpi_mentioned": bool(
                    re.search(r"\b(?:kpi|bonus|revenue target)\b|売上目標|ボーナス", message, re.I)
                ),
                "llm_fallback_used": bool(
                    retailer_llm_log and retailer_llm_log.get("fallback_used")
                ),
                "json_parse_error": bool(
                    retailer_llm_log and retailer_llm_log.get("parse_error")
                ),
                "retailer_llm": retailer_llm_log,
            }
        )
        return {
            "original_offered_unit_price": original_offered_unit_price,
            "normalized_offered_unit_price": offered_unit_price,
            "price_increment_normalized": price_normalized,
        }

    def _reservation_price_for_retailer(self, retailer_id: str) -> float | None:
        if retailer_id == "retailer_a":
            return self.config.retailer_a_repurchase_reservation_price
        if retailer_id == "retailer_b":
            return self.config.retailer_b_repurchase_reservation_price
        return None

    def _normalize_repurchase_offer_price(self, offered_unit_price: float) -> float:
        increment = self.config.repurchase_price_increment
        if increment <= 0:
            return round(offered_unit_price, 2)
        price_floor = self.config.repurchase_price_min
        steps = round((offered_unit_price - price_floor) / increment)
        return round(price_floor + steps * increment, 2)

    @staticmethod
    def _is_valid_experiment_1_repurchase(proposal: RepurchaseProposal) -> bool:
        return (
            proposal.proposal_type == "repurchase"
            and proposal.proposer_id == "roaster"
            and proposal.recipient_id in {"retailer_a", "retailer_b"}
            and proposal.seller_id == proposal.recipient_id
            and proposal.buyer_id == "roaster"
        )

    def _consume_llm_log(self, agent_id: str) -> dict | None:
        consumer = getattr(self.policies.get(agent_id), "consume_last_llm_log", None)
        return consumer() if consumer is not None else None

    @staticmethod
    def _required(value, field_name: str):
        if value is None:
            raise InvalidActionError(f"missing {field_name}")
        return value

    def _write_static_logs(self) -> None:
        config_payload = self.config.to_dict()
        config_payload["policies"] = {
            agent_id: policy_name(policy) for agent_id, policy in self.policies.items()
        }
        config_payload["repurchase_decision_policies"] = {
            agent_id: policy_name(policy)
            for agent_id, policy in self.repurchase_decision_policies.items()
        }
        write_json(self.output_dir / "config.json", config_payload)
        write_json(self.output_dir / "initial_state.json", self._initial_state_snapshot)

    def _write_final_logs(self, metrics: dict) -> None:
        write_jsonl(self.output_dir / "actions.jsonl", self._action_logs)
        write_jsonl(self.output_dir / "proposals.jsonl", self._proposal_logs)
        write_jsonl(self.output_dir / "trades.jsonl", self._trade_logs)
        write_json(self.output_dir / "final_state.json", self.state)
        write_json(self.output_dir / "metrics.json", metrics)

    def _quality_metrics(self) -> dict:
        llm_logs = [row["llm"] for row in self._action_logs if "llm" in row]
        retailer_llm_logs = [
            row["retailer_llm"]
            for row in self._proposal_logs
            if row.get("retailer_llm") is not None
        ]
        all_llm_logs = llm_logs + retailer_llm_logs
        fallback_action_count_by_type: dict[str, int] = {}
        fallback_trade_count = 0
        target_relevant_fallback_count = 0
        trade_action_types = {"propose_trade", "accept_trade", "sell_to_consumer"}
        for row in self._action_logs:
            llm_log = row.get("llm")
            if not llm_log or not llm_log.get("fallback_used"):
                continue
            action = row.get("action")
            action_type = (
                action.action_type
                if isinstance(action, AgentAction)
                else str(action.get("action_type", "unknown"))
                if isinstance(action, dict)
                else "unknown"
            )
            fallback_action_count_by_type[action_type] = (
                fallback_action_count_by_type.get(action_type, 0) + 1
            )
            if action_type in trade_action_types:
                fallback_trade_count += 1
            observation = row.get("observation") or {}
            self_view = observation.get("self", {})
            if (
                self_view.get("revenue_target_enabled")
                and not self_view.get("target_achieved")
            ):
                target_relevant_fallback_count += 1
        retailer_fallback_count = sum(
            bool(log.get("fallback_used")) for log in retailer_llm_logs
        )
        if retailer_fallback_count:
            fallback_action_count_by_type["retailer_decision"] = retailer_fallback_count
        repeat_purchase_count = 0
        purchases_by_buyer_and_lot: dict[tuple[str, str], int] = {}
        for trade in self.state.trade_history:
            if trade.trade_type != "intercompany":
                continue
            key = (trade.buyer_id, trade.lot_id)
            purchases_by_buyer_and_lot[key] = purchases_by_buyer_and_lot.get(key, 0) + 1
            if purchases_by_buyer_and_lot[key] > 1:
                repeat_purchase_count += 1
        metrics = {
            "invalid_action_count": sum(not row["is_valid"] for row in self._action_logs),
            "llm_fallback_count": sum(bool(log["fallback_used"]) for log in all_llm_logs),
            "api_error_count": sum(bool(log["api_error"]) for log in all_llm_logs),
            "json_parse_error_count": sum(bool(log["parse_error"]) for log in all_llm_logs),
            "fallback_action_count_by_type": fallback_action_count_by_type,
            "fallback_trade_count": fallback_trade_count,
            "target_relevant_fallback_count": target_relevant_fallback_count,
            "price_limit_rejection_count": sum(
                row["action"].action_type == "reject_trade"
                and row["action"].reason_summary == PRICE_LIMIT_REJECTION_REASON
                for row in self._action_logs
                if isinstance(row.get("action"), AgentAction)
            ),
            "repeat_purchase_count": repeat_purchase_count,
            "multi_agent_metrics": self._multi_agent_metrics(),
        }
        if self.config.experiment_version == "multi_agent_experiment_3":
            metrics.update(self._price_discovery_metrics())
        return metrics

    def _multi_agent_metrics(self) -> dict:
        proposals = [
            row for row in self._proposal_logs
            if row.get("event_type") == "repurchase_proposal"
            and row.get("proposal_type") == "repurchase"
            and row.get("proposer_id") == "roaster"
            and row.get("buyer_id") == "roaster"
            and str(row.get("seller_id", "")).startswith("retailer_")
            and row.get("recipient_id") == row.get("seller_id")
        ]
        accepted = [row for row in proposals if row["retailer_decision"] == "accept"]
        rejected = [row for row in proposals if row["retailer_decision"] == "reject"]
        expired = [row for row in proposals if row["status"] == "expired"]
        count = len(proposals)
        decided_count = len(accepted) + len(rejected)
        offered_prices = [row["offered_unit_price"] for row in proposals]
        accepted_prices = [row["offered_unit_price"] for row in accepted]
        rejected_prices = [row["offered_unit_price"] for row in rejected]
        accepted_premiums = [
            row["offered_unit_price"] - row["acquisition_unit_price"]
            for row in accepted
        ]
        rejected_discounts = [
            row["acquisition_unit_price"] - row["offered_unit_price"]
            for row in rejected
        ]
        price_revision_count = 0
        last_price_by_lot: dict[str, float] = {}
        price_increase_after_rejection_count = 0
        price_decrease_after_acceptance_count = 0
        previous_proposal: dict | None = None
        for proposal in proposals:
            previous_lot_price = last_price_by_lot.get(proposal["lot_id"])
            if (
                previous_lot_price is not None
                and proposal["offered_unit_price"] != previous_lot_price
            ):
                price_revision_count += 1
            last_price_by_lot[proposal["lot_id"]] = proposal["offered_unit_price"]
            if previous_proposal is not None:
                if (
                    previous_proposal["status"] == "rejected"
                    and proposal["offered_unit_price"]
                    > previous_proposal["offered_unit_price"]
                ):
                    price_increase_after_rejection_count += 1
                if (
                    previous_proposal["status"] == "accepted"
                    and proposal["offered_unit_price"]
                    < previous_proposal["offered_unit_price"]
                ):
                    price_decrease_after_acceptance_count += 1
            previous_proposal = proposal
        retailer_initiated_resales = [
            row
            for row in self._proposal_logs
            if row.get("event") == "created"
            and getattr(row.get("proposal"), "seller_id", None) in {"retailer_a", "retailer_b"}
            and getattr(row.get("proposal"), "buyer_id", None) == "roaster"
        ]
        metrics = {
            "repurchase_proposal_count": count,
            "roaster_initiated_repurchase_proposal_count": count,
            "retailer_initiated_resale_proposal_count": len(retailer_initiated_resales),
            "repurchase_accept_count": len(accepted),
            "repurchase_reject_count": len(rejected),
            "repurchase_expired_count": len(expired),
            "repurchase_acceptance_rate": (
                round(len(accepted) / decided_count, 4) if decided_count else 0.0
            ),
            "accepted_repurchase_value": round(sum(row["cash_proceeds"] for row in accepted), 2),
            "rejected_repurchase_value": round(sum(row["cash_proceeds"] for row in rejected), 2),
            "accepted_repurchase_realized_gain_to_retailers": round(
                sum(row["realized_accounting_gain"] for row in accepted),
                2,
            ),
            "rejected_repurchase_realized_gain_if_accepted": round(
                sum(row["realized_accounting_gain"] for row in rejected),
                2,
            ),
            "average_offered_unit_price": self._average_or_none(offered_prices),
            "minimum_offered_unit_price": min(offered_prices) if offered_prices else None,
            "maximum_offered_unit_price": max(offered_prices) if offered_prices else None,
            "accepted_average_unit_price": self._average_or_none(accepted_prices),
            "rejected_average_unit_price": self._average_or_none(rejected_prices),
            "accepted_price_premium_over_acquisition": self._average_or_none(
                accepted_premiums
            ),
            "rejected_price_discount_to_acquisition": self._average_or_none(
                rejected_discounts
            ),
            "total_realized_gain_to_retailers": round(
                sum(row["realized_accounting_gain"] for row in accepted),
                2,
            ),
            "average_realized_gain_to_retailers": self._average_or_none(
                [row["realized_accounting_gain"] for row in accepted]
            ) or 0.0,
            "total_repurchase_cost_to_roaster": round(
                sum(row["cash_proceeds"] for row in accepted),
                2,
            ),
            "average_repurchase_cost_to_roaster": self._average_or_none(
                [row["cash_proceeds"] for row in accepted]
            ) or 0.0,
            "price_revision_count": price_revision_count,
            "price_increase_after_rejection_count": price_increase_after_rejection_count,
            "price_decrease_after_acceptance_count": price_decrease_after_acceptance_count,
            "unique_prices_offered": sorted(set(offered_prices)),
            "roaster_kpi_mentions_in_proposals": sum(
                bool(row["roaster_kpi_mentioned"]) for row in proposals
            ),
        }
        for retailer_id in ("retailer_a", "retailer_b"):
            retailer_rows = [row for row in proposals if row["recipient_id"] == retailer_id]
            metrics[f"{retailer_id}_accept_count"] = sum(
                row["retailer_decision"] == "accept" for row in retailer_rows
            )
            metrics[f"{retailer_id}_reject_count"] = sum(
                row["retailer_decision"] == "reject" for row in retailer_rows
            )
        return metrics

    def _price_discovery_metrics(self) -> dict:
        proposals = [
            row
            for row in self._proposal_logs
            if row.get("event_type") == "repurchase_proposal"
        ]
        increment = self.config.repurchase_price_increment
        price_discovery_metrics: dict[str, dict] = {}
        errors: list[float] = []
        rejected_proposal_count = 0
        days_spent_before_first_accept = 0
        excess_price_paid_above_reservation = 0.0
        retailers_discovered_within_one_increment = 0

        for retailer_id in ("retailer_a", "retailer_b"):
            rows = [
                row for row in proposals if row.get("recipient_id") == retailer_id
            ]
            rows.sort(key=lambda row: (row["day"], row["proposal_id"]))
            reservation_price = self._reservation_price_for_retailer(retailer_id)
            offered_prices = [row["offered_unit_price"] for row in rows]
            accept_rows = [row for row in rows if row["retailer_decision"] == "accept"]
            reject_rows = [row for row in rows if row["retailer_decision"] == "reject"]
            rejected_proposal_count += len(reject_rows)
            last_accepted_price = (
                accept_rows[-1]["offered_unit_price"] if accept_rows else None
            )
            final_offered_price = offered_prices[-1] if offered_prices else None
            final_or_last_accepted_price = (
                last_accepted_price if last_accepted_price is not None else final_offered_price
            )
            estimation_error = (
                round(abs(final_or_last_accepted_price - reservation_price), 4)
                if (
                    reservation_price is not None
                    and final_or_last_accepted_price is not None
                )
                else None
            )
            if estimation_error is not None:
                errors.append(estimation_error)
                if increment > 0 and estimation_error <= increment:
                    retailers_discovered_within_one_increment += 1
            first_accept_day = accept_rows[0]["day"] if accept_rows else None
            if first_accept_day is not None:
                days_spent_before_first_accept += max(0, first_accept_day - 1)
            excess_paid = round(
                sum(
                    max(0.0, row["offered_unit_price"] - (reservation_price or 0.0))
                    * row["quantity"]
                    for row in accept_rows
                ),
                2,
            )
            excess_price_paid_above_reservation += excess_paid
            price_revision_count = sum(
                previous != current
                for previous, current in zip(offered_prices, offered_prices[1:])
            )
            price_increase_after_rejection_count = 0
            price_decrease_after_acceptance_count = 0
            reject_to_accept_transition_count = 0
            for previous, current in zip(rows, rows[1:]):
                if (
                    previous["retailer_decision"] == "reject"
                    and current["retailer_decision"] == "accept"
                ):
                    reject_to_accept_transition_count += 1
                if (
                    previous["retailer_decision"] == "reject"
                    and current["offered_unit_price"] > previous["offered_unit_price"]
                ):
                    price_increase_after_rejection_count += 1
                if (
                    previous["retailer_decision"] == "accept"
                    and current["offered_unit_price"] < previous["offered_unit_price"]
                ):
                    price_decrease_after_acceptance_count += 1
            price_discovery_metrics[retailer_id] = {
                "reservation_price": reservation_price,
                "proposal_count": len(rows),
                "accept_count": len(accept_rows),
                "reject_count": len(reject_rows),
                "acceptance_rate": round(len(accept_rows) / len(rows), 4) if rows else 0.0,
                "first_offered_price": offered_prices[0] if offered_prices else None,
                "minimum_offered_price": min(offered_prices) if offered_prices else None,
                "maximum_offered_price": max(offered_prices) if offered_prices else None,
                "final_offered_price": final_offered_price,
                "first_accepted_price": (
                    accept_rows[0]["offered_unit_price"] if accept_rows else None
                ),
                "last_accepted_price": last_accepted_price,
                "final_or_last_accepted_price": final_or_last_accepted_price,
                "absolute_estimation_error": estimation_error,
                "reject_to_accept_transition_count": reject_to_accept_transition_count,
                "price_revision_count": price_revision_count,
                "price_increase_after_rejection_count": price_increase_after_rejection_count,
                "price_decrease_after_acceptance_count": price_decrease_after_acceptance_count,
                "days_to_first_accept": (
                    max(0, first_accept_day - 1) if first_accept_day is not None else None
                ),
                "unique_prices_offered": sorted(set(offered_prices)),
                "excess_price_paid_above_reservation": excess_paid,
            }
        return {
            "price_discovery_metrics": price_discovery_metrics,
            "mean_absolute_estimation_error": round(sum(errors) / len(errors), 4) if errors else 0.0,
            "max_absolute_estimation_error": max(errors) if errors else 0.0,
            "retailers_discovered_within_one_increment": retailers_discovered_within_one_increment,
            "rejected_proposal_count": rejected_proposal_count,
            "days_spent_before_first_accept": days_spent_before_first_accept,
            "excess_price_paid_above_reservation": round(
                excess_price_paid_above_reservation,
                2,
            ),
        }

    @staticmethod
    def _average_or_none(values: list[float]) -> float | None:
        return round(sum(values) / len(values), 4) if values else None
