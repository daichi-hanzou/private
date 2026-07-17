from __future__ import annotations

import copy
import random
from dataclasses import dataclass
from pathlib import Path

from .config import SimulationConfig, create_initial_market_state
from .logging_utils import ensure_dir, write_json, write_jsonl
from .market import (
    InvalidActionError,
    accept_trade_proposal,
    create_trade_proposal,
    expire_old_proposals,
    reject_trade_proposal,
)
from .metrics import collect_metrics, economic_inventory_value
from .models import AgentAction, MarketState
from .observation import build_observation
from .policies import AgentPolicy, WaitPolicy, policy_name


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
    ):
        self.config = config
        self.policies = policies
        self.run_id = run_id
        self.output_dir = Path(output_root) / run_id
        self.state = initial_state or create_initial_market_state(config)
        self._rng = random.Random(config.seed)
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
                )
                chosen_action = self._choose_action(agent_id, observation)
                self._execute_action(agent_id, observation, chosen_action)
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
            lot_id=self.config.lot_id,
            initial_cash_by_agent=self._initial_cash_by_agent,
            initial_inventory_value_by_agent=self._initial_inventory_value_by_agent,
        )
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

    def _execute_action(self, agent_id: str, observation: dict, action: AgentAction) -> None:
        error: str | None = None
        is_valid = True
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
            elif action.action_type == "reject_trade":
                proposal = reject_trade_proposal(
                    self.state,
                    proposal_id=self._required(action.proposal_id, "proposal_id"),
                    buyer_id=agent_id,
                )
                self._proposal_logs.append(
                    {"event": "rejected", "day": self.state.day, "proposal": proposal}
                )
            elif action.action_type == "wait":
                pass
            else:
                raise InvalidActionError(f"unsupported action_type: {action.action_type}")
        except Exception as exc:
            error = str(exc)
            is_valid = False
        self._action_logs.append(
            {
                "run_id": self.run_id,
                "day": self.state.day,
                "agent_id": agent_id,
                "observation": observation,
                "action": action,
                "is_valid": is_valid,
                "error": error,
            }
        )

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

    def _write_final_logs(self, metrics: dict) -> None:
        write_jsonl(self.output_dir / "actions.jsonl", self._action_logs)
        write_jsonl(self.output_dir / "proposals.jsonl", self._proposal_logs)
        write_jsonl(self.output_dir / "trades.jsonl", self._trade_logs)
        write_json(self.output_dir / "final_state.json", self.state)
        write_json(self.output_dir / "metrics.json", metrics)
