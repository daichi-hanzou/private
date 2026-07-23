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
                    "seller_id": {"type": "string"},
                    "buyer_id": {"type": "string"},
                    "lot_id": {"type": "string"},
                    "quantity": {"type": "integer"},
                    "unit_price": {"type": "number"},
                    "proposal_message": {"type": ["string", "null"]},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": [
                    "action_type",
                    "seller_id",
                    "buyer_id",
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
                    "action_type": {"type": "string", "enum": ["accept_counteroffer"]},
                    "counteroffer_id": {"type": "string"},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": ["action_type", "counteroffer_id", "reason_summary"],
                "additionalProperties": False,
            },
            {
                "type": "object",
                "properties": {
                    "action_type": {"type": "string", "enum": ["reject_counteroffer"]},
                    "counteroffer_id": {"type": "string"},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": ["action_type", "counteroffer_id", "reason_summary"],
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
                    "action_type": {"type": "string", "enum": ["counteroffer_trade"]},
                    "proposal_id": {"type": "string"},
                    "unit_price": {"type": "number"},
                    "reason_summary": {"type": ["string", "null"]},
                },
                "required": [
                    "action_type",
                    "proposal_id",
                    "unit_price",
                    "reason_summary",
                ],
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


RETAILER_MARKET_ACTION_JSON_SCHEMA = {
    "name": "retailer_market_action",
    "strict": True,
    "schema": {
        "type": "object",
        "properties": {
            "action": {
                "anyOf": [
                    {
                        "type": "object",
                        "properties": {
                            "action": {"type": "string", "enum": ["propose_trade"]},
                            "seller_id": {"type": "string"},
                            "buyer_id": {"type": "string"},
                            "lot_id": {"type": "string"},
                            "quantity": {"type": "integer"},
                            "unit_price": {"type": "number"},
                            "reason": {"type": "string"},
                        },
                        "required": [
                            "action",
                            "seller_id",
                            "buyer_id",
                            "lot_id",
                            "quantity",
                            "unit_price",
                            "reason",
                        ],
                        "additionalProperties": False,
                    },
                    {
                        "type": "object",
                        "properties": {
                            "action": {"type": "string", "enum": ["accept_trade"]},
                            "proposal_id": {"type": "string"},
                            "reason": {"type": "string"},
                        },
                        "required": ["action", "proposal_id", "reason"],
                        "additionalProperties": False,
                    },
                    {
                        "type": "object",
                        "properties": {
                            "action": {"type": "string", "enum": ["reject_trade"]},
                            "proposal_id": {"type": "string"},
                            "reason": {"type": "string"},
                        },
                        "required": ["action", "proposal_id", "reason"],
                        "additionalProperties": False,
                    },
                    {
                        "type": "object",
                        "properties": {
                            "action": {"type": "string", "enum": ["counteroffer_trade"]},
                            "proposal_id": {"type": "string"},
                            "unit_price": {"type": "number"},
                            "reason": {"type": "string"},
                        },
                        "required": ["action", "proposal_id", "unit_price", "reason"],
                        "additionalProperties": False,
                    },
                    {
                        "type": "object",
                        "properties": {
                            "action": {
                                "type": "string",
                                "enum": ["accept_counteroffer"],
                            },
                            "counteroffer_id": {"type": "string"},
                            "reason": {"type": "string"},
                        },
                        "required": ["action", "counteroffer_id", "reason"],
                        "additionalProperties": False,
                    },
                    {
                        "type": "object",
                        "properties": {
                            "action": {
                                "type": "string",
                                "enum": ["reject_counteroffer"],
                            },
                            "counteroffer_id": {"type": "string"},
                            "reason": {"type": "string"},
                        },
                        "required": ["action", "counteroffer_id", "reason"],
                        "additionalProperties": False,
                    },
                    {
                        "type": "object",
                        "properties": {
                            "action": {"type": "string", "enum": ["sell_to_consumer"]},
                            "lot_id": {"type": "string"},
                            "quantity": {"type": "integer"},
                            "reason": {"type": "string"},
                        },
                        "required": ["action", "lot_id", "quantity", "reason"],
                        "additionalProperties": False,
                    },
                    {
                        "type": "object",
                        "properties": {
                            "action": {"type": "string", "enum": ["wait"]},
                            "reason": {"type": "string"},
                        },
                        "required": ["action", "reason"],
                        "additionalProperties": False,
                    },
                ]
            }
        },
        "required": ["action"],
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
