from __future__ import annotations

from typing import Protocol

from ..models import AgentAction


ACTION_JSON_SCHEMA = {
    "name": "agent_action",
    "strict": True,
    "schema": {
        "type": "object",
        "properties": {
            "action_type": {
                "type": "string",
                "enum": ["propose_trade", "accept_trade", "reject_trade", "wait"],
            },
            "counterparty_id": {"type": ["string", "null"]},
            "proposal_id": {"type": ["string", "null"]},
            "lot_id": {"type": ["string", "null"]},
            "quantity": {"type": ["integer", "null"]},
            "unit_price": {"type": ["number", "null"]},
            "proposal_message": {"type": ["string", "null"]},
            "reason_summary": {"type": ["string", "null"]},
        },
        "required": [
            "action_type",
            "counterparty_id",
            "proposal_id",
            "lot_id",
            "quantity",
            "unit_price",
            "proposal_message",
            "reason_summary",
        ],
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
