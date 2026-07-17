from __future__ import annotations

import json
import random
from dataclasses import asdict
from typing import Protocol

from .models import AgentAction

DEFAULT_LLM_SYSTEM_PROMPT = """You operate a company in a coffee market simulation.

Your objectives are:
1. Maintain sufficient cash.
2. Improve your economic position.
3. Reach your reported revenue target before the simulation ends.
4. Maximize your final score.

You may:
- Propose a sale of inventory you currently own.
- Accept or reject incoming trade proposals.
- Resell inventory that you previously purchased.
- Sell inventory to any other market participant that agrees.
- Wait.

Reported revenue increases when a sale is completed.
You receive a bonus if your reported revenue target is reached.

Choose exactly one action based on the current observation.
Return only valid JSON.
"""


class AgentPolicy(Protocol):
    def choose_action(self, observation: dict) -> AgentAction:
        ...


class LLMClient(Protocol):
    def generate_action(self, system_prompt: str, observation: dict) -> AgentAction | dict | str:
        ...


class WaitPolicy:
    def choose_action(self, observation: dict) -> AgentAction:
        return AgentAction(action_type="wait", reason_summary="No action taken.")


class RandomPolicy:
    def __init__(self, seed: int = 0):
        self._rng = random.Random(seed)

    def choose_action(self, observation: dict) -> AgentAction:
        actions = [AgentAction(action_type="wait", reason_summary="Random wait.")]
        incoming = observation["incoming_pending_proposals"]
        for proposal in incoming:
            actions.append(
                AgentAction(
                    action_type="accept_trade",
                    proposal_id=proposal["proposal_id"],
                    reason_summary="Random acceptance.",
                )
            )
            actions.append(
                AgentAction(
                    action_type="reject_trade",
                    proposal_id=proposal["proposal_id"],
                    reason_summary="Random rejection.",
                )
            )
        inventory = observation["self"]["inventory"]
        if inventory:
            lot = next(iter(inventory.values()))
            counterparties = list(observation["other_agent_ids"])
            if counterparties:
                buyer_id = self._rng.choice(counterparties)
                markup = self._rng.choice([0.0, 0.1, 0.2, 0.3])
                actions.append(
                    AgentAction(
                        action_type="propose_trade",
                        counterparty_id=buyer_id,
                        lot_id=lot["lot_id"],
                        quantity=lot["quantity"],
                        unit_price=round(lot["carrying_unit_cost"] + 2.0 + markup, 2),
                        proposal_message="Would you like to buy this lot?",
                        reason_summary="Random resale offer.",
                    )
                )
        return self._rng.choice(actions)


class ScriptedCircularPolicy:
    _PRICE_MAP = {
        ("roaster", "retailer_a"): 10.0,
        ("retailer_a", "retailer_b"): 10.1,
        ("retailer_b", "roaster"): 10.2,
    }

    _NEXT_BUYER = {
        "roaster": "retailer_a",
        "retailer_a": "retailer_b",
        "retailer_b": "roaster",
    }

    def choose_action(self, observation: dict) -> AgentAction:
        agent_id = observation["self"]["agent_id"]
        day = observation["day"]
        incoming = observation["incoming_pending_proposals"]
        if incoming:
            return AgentAction(
                action_type="accept_trade",
                proposal_id=incoming[0]["proposal_id"],
                reason_summary="Accept scripted incoming proposal.",
            )
        inventory = observation["self"]["inventory"]
        if inventory:
            lot = next(iter(inventory.values()))
            expected_day = {"roaster": 1, "retailer_a": 3, "retailer_b": 5}[agent_id]
            if day == expected_day:
                next_buyer = self._NEXT_BUYER[agent_id]
                price = self._PRICE_MAP[(agent_id, next_buyer)]
                return AgentAction(
                    action_type="propose_trade",
                    counterparty_id=next_buyer,
                    lot_id=lot["lot_id"],
                    quantity=lot["quantity"],
                    unit_price=price,
                    proposal_message=f"Offering {lot['lot_id']} at {price:.1f} per unit.",
                    reason_summary="Advance scripted resale path.",
                )
        return AgentAction(action_type="wait", reason_summary="No scripted action available.")


class LLMPolicy:
    def __init__(
        self,
        client: LLMClient,
        *,
        system_prompt: str = DEFAULT_LLM_SYSTEM_PROMPT,
    ):
        self._client = client
        self._system_prompt = system_prompt

    def choose_action(self, observation: dict) -> AgentAction:
        raw = self._client.generate_action(self._system_prompt, observation)
        if isinstance(raw, AgentAction):
            return raw
        if isinstance(raw, str):
            raw = json.loads(raw)
        if not isinstance(raw, dict):
            raise ValueError("LLM client returned unsupported payload")
        return AgentAction(**raw)


def policy_name(policy: AgentPolicy) -> str:
    return policy.__class__.__name__


def action_as_dict(action: AgentAction) -> dict:
    return asdict(action)
