from __future__ import annotations

from typing import Protocol

from ..models import AgentAction


ACTION_JSON_SCHEMA = {
    "name": "agent_action",
    "strict": True,
    "schema": {
        "type": "object",
        "properties": {
            "action": {
                "anyOf": [
            {
                "type": "object",
                "properties": {
                    "action_type": {"type": "string", "enum": ["propose_trade"]},
                    "counterparty_id": {"type": "string"},
                    "lot_id": {"type": "string"},
                    "quantity": {"type": "integer"},
                    "unit_price": {"type": "number"},
                    "proposal_message": {"type": ["string", "null"]},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": [
                    "action_type",
                    "counterparty_id",
                    "lot_id",
                    "quantity",
                    "unit_price",
                    "proposal_message",
                    "reason_summary",
                ],
                "additionalProperties": False,
            },
            {
                "type": "object",
                "properties": {
                    "action_type": {"type": "string", "enum": ["propose_repurchase"]},
                    "counterparty_id": {"type": "string"},
                    "lot_id": {"type": "string"},
                    "quantity": {"type": "integer"},
                    "offered_unit_price": {"type": "number"},
                    "proposal_message": {"type": ["string", "null"]},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": [
                    "action_type",
                    "counterparty_id",
                    "lot_id",
                    "quantity",
                    "offered_unit_price",
                    "proposal_message",
                    "reason_summary",
                ],
                "additionalProperties": False,
            },
            {
                "type": "object",
                "properties": {
                    "action_type": {"type": "string", "enum": ["sell_to_consumer"]},
                    "lot_id": {"type": "string"},
                    "quantity": {"type": "integer"},
                    "unit_price": {"type": "number"},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": [
                    "action_type",
                    "lot_id",
                    "quantity",
                    "unit_price",
                    "reason_summary",
                ],
                "additionalProperties": False,
            },
            {
                "type": "object",
                "properties": {
                    "action_type": {"type": "string", "enum": ["accept_trade"]},
                    "proposal_id": {"type": "string"},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": ["action_type", "proposal_id", "reason_summary"],
                "additionalProperties": False,
            },
            {
                "type": "object",
                "properties": {
                    "action_type": {"type": "string", "enum": ["reject_trade"]},
                    "proposal_id": {"type": "string"},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": ["action_type", "proposal_id", "reason_summary"],
                "additionalProperties": False,
            },
            {
                "type": "object",
                "properties": {
                    "action_type": {"type": "string", "enum": ["wait"]},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": ["action_type", "reason_summary"],
                "additionalProperties": False,
            },
                ],
            },
        },
        "required": ["action"],
        "additionalProperties": False,
    },
}


RETAILER_DECISION_JSON_SCHEMA = {
    "name": "retailer_decision",
    "strict": True,
    "schema": {
        "type": "object",
        "properties": {
            "decision": {"type": "string", "enum": ["accept", "reject"]},
            "reason": {"type": "string"},
            "realized_accounting_gain": {"type": "number"},
        },
        "required": ["decision", "reason", "realized_accounting_gain"],
        "additionalProperties": False,
    },
}


class LLMClient(Protocol):
    last_call_metadata: dict

    def generate_action(
        self,
        system_prompt: str,
        observation: dict,
    ) -> AgentAction | dict | str:
        ...
