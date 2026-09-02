from __future__ import annotations

import json
import os
from dataclasses import dataclass
from typing import Any, Protocol

from openai import OpenAI

from .review_models import ReviewChatMessage, ReviewContext, ReviewResult
from mail_to_calendar.taxonomy import CLASSIFICATIONS


REVIEW_SYSTEM_PROMPT = """You audit a mail-classification pipeline.
Decide whether the final system classification materially matches the meaning of the email.
Be critical about false promotions, missed user commitments, bad clarification decisions, notification mistakes, and approval/calendar side effects.
Do not reveal chain-of-thought. Return only the requested JSON object.
If the final judgment is materially sound, use review_status=agree.
If it is materially wrong, use review_status=disagreement.
If evidence is mixed or incomplete, use review_status=uncertain.
Use agree only when suggested_classification and the system's final classification
match after taxonomy normalization. Use disagreement for a definite mismatch.
reason_summary must be short and concrete."""
REVIEW_SYSTEM_PROMPT += """
Explore missed user meaning broadly, but suggested_classification must be exactly one
of: calendar_candidate, transactional, security_notification, ignored, invalid,
clarification_required. Never invent another classification name. transactional means
a user-specific transaction, confirmed fact, financial/contractual state, delivery, or
obligation. Advertising, campaigns, newsletters, and general information are ignored.
Legacy informational and promotion both normalize to ignored, so that alias-only change
is agreement, not a semantic disagreement. Under the private-mailbox policy, grounded
transactional mail is important and user-notifiable; if the original system classified
such a message as ignored, treat that as a meaningful disagreement. Do not classify a
generic newsletter, market update, product introduction, or promotion as transactional.
Apply these canonical examples consistently:
- A PR label, free registration/trial, sales CTA, public seminar invitation, or product
  introduction without a user-specific commitment is ignored.
- An executed investment purchase, card debit, invoice, order confirmation, shipment,
  delivery-state update, or user-specific contract state is transactional.
- A new login, suspicious access, new app connection, security code, password change, or
  account-security warning is security_notification, even when the message recommends an
  action such as resetting a password.
- A generic service outage or maintenance newsletter is ignored unless it represents a
  concrete user-specific transaction, security event, or personal calendar commitment.
Never return or recommend legacy labels such as informational, promotion, promotional,
newsletter, security_alert, or transactional_notice. Map their meaning to the canonical
six-class taxonomy before choosing suggested_classification.
Classification priority is: security_notification; grounded personal calendar commitment;
grounded user-specific transaction/state/obligation; ignored; then invalid or
clarification_required only when their existing definitions truly apply."""


CHAT_SYSTEM_PROMPT = """You discuss one review case with a human reviewer.
Use only the provided case context and visible chat history.
Do not expose chain-of-thought. Answer briefly and directly."""

REVIEW_RESULT_SCHEMA = {
    "type": "object",
    "properties": {
        "review_status": {
            "type": "string", "enum": ["agree", "disagreement", "uncertain"]
        },
        "suggested_classification": {
            "type": "string", "enum": list(CLASSIFICATIONS)
        },
        "issue_type": {"type": "string", "maxLength": 80},
        "confidence": {"type": "number", "minimum": 0, "maximum": 1},
        "reason_summary": {"type": "string", "maxLength": 500},
        "needs_human_review": {"type": "boolean"},
    },
    "required": [
        "review_status", "suggested_classification", "issue_type",
        "confidence", "reason_summary", "needs_human_review",
    ],
    "additionalProperties": False,
}


class ReviewClient(Protocol):
    model_name: str

    def review_case(self, context: ReviewContext) -> ReviewResult: ...

    def chat(
        self,
        context: ReviewContext,
        history: list[ReviewChatMessage],
        user_message: str,
        human_context: dict[str, Any] | None = None,
    ) -> str: ...


@dataclass(frozen=True)
class OpenAIReviewConfig:
    model_name: str
    review_version: str
    api_key: str | None = None
    base_url: str | None = None

    @classmethod
    def from_env(cls) -> "OpenAIReviewConfig":
        model_name = os.environ.get("AGENTLEDGER_REVIEW_MODEL", "").strip()
        if not model_name:
            raise ValueError("AGENTLEDGER_REVIEW_MODEL is required")
        return cls(
            model_name=model_name,
            review_version=os.environ.get("AGENTLEDGER_REVIEW_VERSION", "v2"),
            api_key=os.environ.get("AGENTLEDGER_REVIEW_API_KEY"),
            base_url=os.environ.get("AGENTLEDGER_REVIEW_BASE_URL"),
        )


class OpenAIReviewClient:
    def __init__(self, config: OpenAIReviewConfig) -> None:
        self.model_name = config.model_name
        self.review_version = config.review_version
        arguments = {}
        if config.api_key:
            arguments["api_key"] = config.api_key
        if config.base_url:
            arguments["base_url"] = config.base_url
        self.client = OpenAI(**arguments)

    def review_case(self, context: ReviewContext) -> ReviewResult:
        payload = json.dumps(
            {
                "task": "audit_final_mail_classification",
                "case": context.to_prompt_payload(),
                "output_schema": REVIEW_RESULT_SCHEMA,
            },
            ensure_ascii=False,
        )
        response = self.client.chat.completions.create(
            model=self.model_name,
            response_format={
                "type": "json_schema",
                "json_schema": {
                    "name": "classification_review",
                    "strict": True,
                    "schema": REVIEW_RESULT_SCHEMA,
                },
            },
            messages=[
                {"role": "system", "content": REVIEW_SYSTEM_PROMPT},
                {"role": "user", "content": payload},
            ],
        )
        content = response.choices[0].message.content or "{}"
        return ReviewResult.from_dict(json.loads(content))

    def chat(
        self,
        context: ReviewContext,
        history: list[ReviewChatMessage],
        user_message: str,
        human_context: dict[str, Any] | None = None,
    ) -> str:
        messages = [
            {"role": "system", "content": CHAT_SYSTEM_PROMPT},
            {
                "role": "user",
                "content": json.dumps(
                    {
                        "case": context.to_prompt_payload(),
                        "current_human_review": human_context or {},
                        "instruction": "Use this as the fixed case context for the discussion.",
                    },
                    ensure_ascii=False,
                ),
            },
        ]
        for item in history:
            messages.append({"role": item.role, "content": item.content})
        messages.append({"role": "user", "content": user_message})
        response = self.client.chat.completions.create(
            model=self.model_name,
            messages=messages,
        )
        return (response.choices[0].message.content or "").strip()
