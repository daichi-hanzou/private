from __future__ import annotations

import copy
import random
from dataclasses import dataclass
from pathlib import Path

from .config import (
    SimulationConfig,
    build_market_information,
    can_agent_sell_to_consumer,
    create_initial_market_state,
)
from .core_metrics import build_core_metrics
from .logging_utils import ensure_dir, to_jsonable, write_json, write_jsonl
from .market import (
    InvalidActionError,
    accept_trade_proposal,
    create_trade_counteroffer,
    create_trade_proposal,
    execute_consumer_sale,
    expire_old_proposals,
    reject_trade_proposal,
    respond_to_trade_counteroffer,
    sell_to_consumer_market,
)
from .metrics import economic_inventory_value
from .models import AgentAction, MarketState, TradeRecord
from .observation import (
    build_observation,
    build_retailer_market_observation,
)
from .policies import (
    AgentPolicy,
    RetailerMarketPolicy,
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
    negotiation_logs: list[dict]
    trade_logs: list[dict]
    consumer_sale_logs: list[dict]
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
    ):
        self.config = config
        self.policies = policies
        self.run_id = run_id
        self.output_dir = Path(output_root) / run_id
        self.state = initial_state or create_initial_market_state(config)
        self._rng = random.Random(
            config.agent_order_seed if config.agent_order_seed is not None else config.seed
        )
        self._action_logs: list[dict] = []
        self._proposal_logs: list[dict] = []
        self._negotiation_logs: list[dict] = []
        self._trade_logs: list[TradeRecord] = []
        self._policy_errors: dict[str, str] = {}
        self._legacy_action_normalized_agents: set[str] = set()
        self._consumer_sale_logs: list[dict] = []
        self._agent_order_logs: list[dict] = [
            {
                "event_type": "market_channel_configuration",
                "roaster_consumer_sale_enabled": config.roaster_consumer_sale_enabled,
                "retailer_consumer_sale_enabled": config.retailer_consumer_sale_enabled,
            }
        ]
        self._proposal_visibility_counts: dict[str, int] = {}
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
            ordered_agent_ids = self._ordered_agent_ids()
            self._agent_order_logs.append({"day": day, "agent_order": ordered_agent_ids})
            for agent_id in ordered_agent_ids:
                observation = build_observation(
                    self.state,
                    agent_id,
                    initial_cash=self._initial_cash_by_agent[agent_id],
                    initial_inventory_value=self._initial_inventory_value_by_agent[agent_id],
                    market_information=build_market_information(self.config, self.state),
                    config=self.config,
                )
                for proposal in observation["incoming_pending_proposals"]:
                    proposal_id = proposal["proposal_id"]
                    self._proposal_visibility_counts[proposal_id] = (
                        self._proposal_visibility_counts.get(proposal_id, 0) + 1
                    )
                    self._negotiation_logs.append(
                        {
                            "day": day,
                            "event_type": "offer_observed",
                            "offer_id": proposal_id,
                            "agent_id": agent_id,
                            "lot_id": proposal["lot_id"],
                        }
                    )
                policy = self.policies.get(agent_id)
                if isinstance(policy, RetailerMarketPolicy):
                    observation = build_retailer_market_observation(
                        observation,
                        config=self.config,
                    )
                chosen_action = self._choose_action(agent_id, observation)
                llm_log = self._consume_llm_log(agent_id)
                self._execute_action(agent_id, observation, chosen_action, llm_log=llm_log)
            expired = expire_old_proposals(
                self.state,
                proposal_expiry_days=self.config.proposal_expiry_days,
            )
            for proposal in expired:
                self._append_counteroffer_closure_events(proposal.proposal_id)
                times_shown = self._proposal_visibility_counts.get(proposal.proposal_id, 0)
                self._proposal_logs.append(
                    {
                        "event": "expired",
                        "event_type": "offer_expired",
                        "day": day,
                        "proposal_id": proposal.proposal_id,
                        "seller_id": proposal.seller_id,
                        "buyer_id": proposal.buyer_id,
                        "lot_id": proposal.lot_id,
                        "quantity": proposal.quantity,
                        "unit_price": proposal.unit_price,
                        "proposal": proposal,
                        "expired_without_being_observed": times_shown == 0,
                        "times_shown_to_buyer": times_shown,
                        "warning": (
                            "Proposal expired without being shown to its buyer."
                            if times_shown == 0
                            else None
                        ),
                    }
                )
            self._expire_counteroffers()
        authoritative_proposals = self._authoritative_proposal_logs()
        metrics = build_core_metrics(
            run_id=self.run_id,
            seed=self.config.seed,
            lot_ids=self.config.lot_ids,
            initial_state=to_jsonable(self._initial_state_snapshot),
            final_state=to_jsonable(self.state),
            actions=to_jsonable(self._action_logs),
            proposals=authoritative_proposals,
            trades=to_jsonable(self._trade_logs),
        )
        self._write_final_logs(metrics, authoritative_proposals)
        return SimulationResult(
            run_id=self.run_id,
            state=self.state,
            metrics=metrics,
            action_logs=self._action_logs,
            proposal_logs=self._proposal_logs,
            negotiation_logs=self._negotiation_logs,
            trade_logs=self._trade_logs,
            consumer_sale_logs=self._consumer_sale_logs,
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
            action = policy.choose_action(observation)
            if action.action_type == "hold_inventory":
                self._legacy_action_normalized_agents.add(agent_id)
                return AgentAction(
                    action_type="wait",
                    reason_summary=action.reason_summary,
                )
            return action
        except Exception as exc:
            self._policy_errors[agent_id] = str(exc)
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
                seller_id = action.seller_id or agent_id
                proposal = create_trade_proposal(
                    self.state,
                    config=self.config,
                    initiator_id=agent_id,
                    seller_id=seller_id,
                    buyer_id=self._required(action.buyer_id, "buyer_id"),
                    lot_id=self._required(action.lot_id, "lot_id"),
                    quantity=self._required(action.quantity, "quantity"),
                    unit_price=self._required(action.unit_price, "unit_price"),
                    proposal_message=action.proposal_message,
                )
                self._proposal_logs.append(
                    {
                        "event": "created",
                        "event_type": "offer_created",
                        "day": self.state.day,
                        "proposal_id": proposal.proposal_id,
                        "initiator_id": proposal.initiator_id,
                        "seller_id": proposal.seller_id,
                        "buyer_id": proposal.buyer_id,
                        "lot_id": proposal.lot_id,
                        "quantity": proposal.quantity,
                        "unit_price": proposal.unit_price,
                        "proposal": proposal,
                    }
                )
                action_metadata = {"created_proposal_id": proposal.proposal_id}
            elif action.action_type == "accept_trade":
                proposal_id = self._required(action.proposal_id, "proposal_id")
                trade = accept_trade_proposal(
                    self.state,
                    proposal_id=proposal_id,
                    responder_id=agent_id,
                    transaction_fee_rate=self.config.transaction_fee_rate,
                )
                proposal = self.state.pending_proposals[proposal_id]
                self._proposal_logs.append(
                    {
                        "event": "accepted",
                        "event_type": "offer_accepted",
                        "day": self.state.day,
                        "proposal_id": proposal.proposal_id,
                        "seller_id": proposal.seller_id,
                        "buyer_id": proposal.buyer_id,
                        "lot_id": proposal.lot_id,
                        "proposal": proposal,
                    }
                )
                self._trade_logs.append(trade)
                self._append_counteroffer_closure_events(proposal_id)
                action_metadata = {
                    "accepted_proposal_id": proposal_id,
                    "created_trade_id": trade.trade_id,
                }
            elif action.action_type == "accept_counteroffer":
                action_metadata = self._execute_counteroffer_response(
                    agent_id,
                    action,
                    accept=True,
                )
            elif action.action_type == "reject_counteroffer":
                action_metadata = self._execute_counteroffer_response(
                    agent_id,
                    action,
                    accept=False,
                )
            elif action.action_type == "reject_trade":
                proposal = reject_trade_proposal(
                    self.state,
                    proposal_id=self._required(action.proposal_id, "proposal_id"),
                    responder_id=agent_id,
                )
                self._proposal_logs.append(
                    {
                        "event": "rejected",
                        "event_type": "offer_rejected",
                        "day": self.state.day,
                        "proposal_id": proposal.proposal_id,
                        "seller_id": proposal.seller_id,
                        "buyer_id": proposal.buyer_id,
                        "lot_id": proposal.lot_id,
                        "proposal": proposal,
                    }
                )
                self._append_counteroffer_closure_events(proposal.proposal_id)
                action_metadata = {"rejected_proposal_id": proposal.proposal_id}
            elif action.action_type == "counteroffer_trade":
                action_metadata = self._execute_trade_counteroffer(agent_id, action)
            elif action.action_type == "sell_to_consumer":
                actor = self.state.agents[agent_id]
                if not can_agent_sell_to_consumer(self.config, actor.role):
                    reason = (
                        "roaster_consumer_sale_disabled"
                        if actor.role == "roaster"
                        and not self.config.roaster_consumer_sale_enabled
                        else "retailer_consumer_sale_disabled"
                    )
                    raise InvalidActionError(reason)
                if self.config.retailer_consumer_sale_enabled:
                    result = self._execute_shared_consumer_sale(
                        seller_id=agent_id,
                        lot_id=self._required(action.lot_id, "lot_id"),
                        quantity=self._required(action.quantity, "quantity"),
                        decision_reason=action.reason_summary,
                    )
                    sale_completed = result["status"] == "accepted"
                else:
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
                        self._trade_logs.append(trade)
            elif action.action_type == "wait":
                pass
            else:
                raise InvalidActionError(f"unsupported action_type: {action.action_type}")
        except Exception as exc:
            error = str(exc)
            is_valid = False
        policy_error_reason = self._policy_errors.pop(agent_id, None)
        row = {
                "run_id": self.run_id,
                "day": self.state.day,
                "agent_id": agent_id,
                "requested_action": action,
                "executed_action": action.action_type if is_valid else None,
                "is_valid": is_valid,
                "error_reason": error,
                "policy_error": policy_error_reason is not None,
                "policy_error_reason": policy_error_reason,
                "legacy_action_normalized": (
                    agent_id in self._legacy_action_normalized_agents
                    or bool(llm_log and llm_log.get("legacy_action_normalized"))
                ),
                "llm_fallback_used": bool(
                    policy_error_reason is not None
                    or (llm_log and llm_log.get("fallback_used"))
                ),
                "llm_api_error": bool(llm_log and llm_log.get("api_error")),
                "llm_parse_error": bool(llm_log and llm_log.get("parse_error")),
            }
        if not is_valid:
            row["event_type"] = "invalid_action"
            row["action_type"] = action.action_type
            row["reason"] = error
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
            row["llm_fallback_used"] = bool(
                row["llm_fallback_used"]
                or action_metadata.get("retailer_llm_fallback_used")
            )
            row["llm_api_error"] = bool(
                row["llm_api_error"]
                or action_metadata.get("retailer_llm_api_error")
            )
        self._action_logs.append(row)
        self._legacy_action_normalized_agents.discard(agent_id)

    def _execute_shared_consumer_sale(
        self,
        *,
        seller_id: str,
        lot_id: str,
        quantity: int,
        decision_reason: str | None,
        related_offer_id: str | None = None,
    ) -> dict:
        before = self.config.consumer_daily_demand_capacity
        if self.state.consumer_market is not None:
            before = self.state.consumer_market.remaining_capacity_by_day.get(
                self.state.day,
                self.state.consumer_market.daily_capacity,
            )
        result = sell_to_consumer_market(
            self.state,
            seller_id=seller_id,
            lot_id=lot_id,
            quantity=quantity,
            consumer_market_enabled=(
                can_agent_sell_to_consumer(
                    self.config,
                    self.state.agents[seller_id].role,
                )
            ),
            consumer_unit_price=self.config.consumer_unit_price,
            consumer_daily_demand_capacity=self.config.consumer_daily_demand_capacity,
            consumer_sale_requires_full_lot=self.config.consumer_sale_requires_full_lot,
        )
        cancelled_offer_ids: list[str] = []
        if result.status == "accepted":
            if result.trade is not None:
                self._trade_logs.append(result.trade)
            for counteroffer in self.state.pending_trade_counteroffers.values():
                if (
                    counteroffer.lot_id == lot_id
                    and counteroffer.status == "invalidated"
                ):
                    self._proposal_logs.append(
                        {
                            "day": self.state.day,
                            "event_type": "counteroffer_cancelled",
                            "proposal_id": counteroffer.proposal_id,
                            "counteroffer_id": counteroffer.counteroffer_id,
                            "reason": "lot_sold_to_consumer",
                        }
                    )
            if related_offer_id is not None:
                cancelled_offer_ids.append(related_offer_id)
            for proposal in self.state.pending_proposals.values():
                if (
                    proposal.lot_id == lot_id
                    and proposal.status == "cancelled"
                    and proposal.close_reason == "lot_sold_to_consumer"
                ):
                    cancelled_offer_ids.append(proposal.proposal_id)
                    self._proposal_logs.append(
                        {
                            "day": self.state.day,
                            "event": "cancelled",
                            "event_type": "offer_cancelled",
                            "proposal_id": proposal.proposal_id,
                            "initiator_id": proposal.initiator_id,
                            "seller_id": proposal.seller_id,
                            "buyer_id": proposal.buyer_id,
                            "lot_id": proposal.lot_id,
                            "proposal": proposal,
                            "termination_reason": "lot_sold_to_consumer",
                        }
                    )
        log = {
            "day": self.state.day,
            "agent_id": seller_id,
            "action": "sell_to_consumer",
            "lot_id": lot_id,
            "quantity_requested": quantity,
            "quantity_sold": result.quantity_sold,
            "unit_price": result.unit_price,
            "total_revenue": result.total_revenue,
            "remaining_demand_before": before,
            "remaining_demand_after": result.remaining_daily_demand,
            "status": result.status,
            "decision_reason": decision_reason,
            "environment_reason": result.reason,
            "related_offer_id": related_offer_id,
            "cancelled_offer_ids": sorted(set(cancelled_offer_ids)),
        }
        self._consumer_sale_logs.append(log)
        return log

    def _execute_counteroffer_response(
        self,
        agent_id: str,
        action: AgentAction,
        *,
        accept: bool,
    ) -> dict:
        counteroffer_id = self._required(action.counteroffer_id, "counteroffer_id")
        trade = respond_to_trade_counteroffer(
            self.state,
            counteroffer_id=counteroffer_id,
            responder_id=agent_id,
            accept=accept,
            transaction_fee_rate=self.config.transaction_fee_rate,
        )
        counteroffer = self.state.pending_trade_counteroffers[counteroffer_id]
        if trade is not None:
            self._trade_logs.append(trade)
        self._proposal_logs.append(
            {
                "day": self.state.day,
                "event_type": (
                    "counteroffer_accepted"
                    if accept
                    else "counteroffer_rejected"
                ),
                "counteroffer_id": counteroffer_id,
                "proposal_id": counteroffer.proposal_id,
                "trade_id": trade.trade_id if trade is not None else None,
            }
        )
        if trade is not None:
            proposal = self.state.pending_proposals[counteroffer.proposal_id]
            self._proposal_logs.append(
                {
                    "day": self.state.day,
                    "event_type": "offer_accepted",
                    "proposal_id": proposal.proposal_id,
                    "initiator_id": proposal.initiator_id,
                    "seller_id": proposal.seller_id,
                    "buyer_id": proposal.buyer_id,
                    "lot_id": proposal.lot_id,
                }
            )
        return {
            "counteroffer_id": counteroffer_id,
            "negotiation_outcome": "accepted" if accept else "rejected",
            "created_trade_id": trade.trade_id if trade is not None else None,
        }

    def _execute_trade_counteroffer(
        self,
        retailer_id: str,
        action: AgentAction,
    ) -> dict:
        proposal_id = self._required(action.proposal_id, "proposal_id")
        unit_price = self._required(action.unit_price, "unit_price")
        previous_pending_ids = {
            item.counteroffer_id
            for item in self.state.pending_trade_counteroffers.values()
            if item.proposal_id == proposal_id and item.status == "pending"
        }
        counteroffer = create_trade_counteroffer(
            self.state,
            proposal_id=proposal_id,
            initiator_id=retailer_id,
            unit_price=unit_price,
        )
        self._proposal_logs.append(
            {
                "day": self.state.day,
                "event_type": "counteroffer_created",
                "counteroffer_id": counteroffer.counteroffer_id,
                "proposal_id": proposal_id,
                "initiator_id": counteroffer.initiator_id,
                "seller_id": counteroffer.seller_id,
                "buyer_id": counteroffer.buyer_id,
                "lot_id": counteroffer.lot_id,
                "quantity": counteroffer.quantity,
                "unit_price": counteroffer.unit_price,
            }
        )
        for previous_id in previous_pending_ids:
            previous = self.state.pending_trade_counteroffers[previous_id]
            if previous.status == "superseded":
                self._proposal_logs.append(
                    {
                        "day": self.state.day,
                        "event_type": "counteroffer_superseded",
                        "counteroffer_id": previous.counteroffer_id,
                        "proposal_id": previous.proposal_id,
                        "reason": previous.close_reason,
                    }
                )
        return {"counteroffer_id": counteroffer.counteroffer_id}

    def _append_counteroffer_closure_events(self, proposal_id: str) -> None:
        event_names = {
            "superseded": "counteroffer_superseded",
            "cancelled": "counteroffer_cancelled",
            "invalidated": "counteroffer_cancelled",
        }
        logged = {
            (row.get("event_type"), row.get("counteroffer_id"))
            for row in self._proposal_logs
        }
        for counteroffer in self.state.pending_trade_counteroffers.values():
            if counteroffer.proposal_id != proposal_id:
                continue
            event_type = event_names.get(counteroffer.status)
            if event_type is None or (event_type, counteroffer.counteroffer_id) in logged:
                continue
            self._proposal_logs.append(
                {
                    "day": self.state.day,
                    "event_type": event_type,
                    "counteroffer_id": counteroffer.counteroffer_id,
                    "proposal_id": proposal_id,
                    "reason": counteroffer.close_reason,
                }
            )

    def _expire_counteroffers(self) -> None:
        for counteroffer in self.state.pending_trade_counteroffers.values():
            if counteroffer.status != "pending":
                continue
            if (
                self.state.day - counteroffer.created_day
                < self.config.proposal_expiry_days
            ):
                continue
            counteroffer.status = "expired"
            counteroffer.decision_day = self.state.day
            counteroffer.close_reason = "counteroffer_expired"
            self._proposal_logs.append(
                {
                    "day": self.state.day,
                    "event_type": "counteroffer_expired",
                    "counteroffer_id": counteroffer.counteroffer_id,
                    "proposal_id": counteroffer.proposal_id,
                }
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
        write_json(self.output_dir / "config.json", config_payload)
        write_json(self.output_dir / "initial_state.json", self._initial_state_snapshot)

    def _authoritative_proposal_logs(self) -> list[dict]:
        events: list[dict] = []
        event_names = {
            "offer_created": "proposal_created",
            "offer_accepted": "proposal_accepted",
            "offer_rejected": "proposal_rejected",
            "offer_expired": "proposal_expired",
            "offer_cancelled": "proposal_cancelled",
        }

        for row in self._proposal_logs:
            event_type = row.get("event_type")
            if str(event_type).startswith("counteroffer_"):
                events.append(dict(row))
                continue
            normalized_type = event_names.get(str(event_type))
            if normalized_type:
                events.append(self._proposal_event(row, normalized_type))

        return events

    def _proposal_event(self, row: dict, event_type: str) -> dict:
        event = {
            "day": row.get("decision_day", row.get("day")),
            "event_type": event_type,
            "proposal_id": row.get("proposal_id"),
        }
        if event_type == "proposal_created":
            event.update(
                {
                    "initiator_id": row.get("initiator_id", row.get("seller_id")),
                    "seller_id": row.get("seller_id"),
                    "buyer_id": row.get("buyer_id"),
                    "lot_id": row.get("lot_id"),
                    "quantity": row.get("quantity"),
                    "unit_price": row.get("unit_price"),
                    "proposal_message": (
                        row["proposal"].proposal_message
                        if row.get("proposal") is not None
                        else None
                    ),
                }
            )
        if event_type == "proposal_cancelled":
            event["reason"] = row.get(
                "termination_reason",
                row.get("status"),
            )
        if event_type == "proposal_accepted":
            matching_trade = next(
                (
                    trade
                    for trade in reversed(self._trade_logs)
                    if trade.proposal_id == row.get("proposal_id")
                ),
                None,
            )
            event["trade_id"] = matching_trade.trade_id if matching_trade else None
        return event

    def _write_final_logs(
        self,
        metrics: dict,
        authoritative_proposals: list[dict],
    ) -> None:
        write_jsonl(self.output_dir / "actions.jsonl", self._action_logs)
        write_jsonl(
            self.output_dir / "proposals.jsonl",
            authoritative_proposals,
        )
        write_jsonl(self.output_dir / "negotiations.jsonl", self._negotiation_logs)
        write_jsonl(self.output_dir / "trades.jsonl", self._trade_logs)
        write_jsonl(self.output_dir / "consumer_sales.jsonl", self._consumer_sale_logs)
        write_jsonl(self.output_dir / "agent_order.jsonl", self._agent_order_logs)
        final_state = to_jsonable(self.state)
        final_state.pop("trade_history", None)
        write_json(self.output_dir / "final_state.json", final_state)
        write_json(self.output_dir / "metrics.json", metrics)
