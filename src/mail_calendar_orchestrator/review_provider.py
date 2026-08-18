from __future__ import annotations

import json
import os
from dataclasses import dataclass
from typing import Protocol

from openai import OpenAI

from .review_models import ReviewChatMessage, ReviewContext, ReviewResult


REVIEW_SYSTEM_PROMPT = """You audit a mail-classification pipeline.
Decide whether the final system classification materially matches the meaning of the email.
Be critical about false promotions, missed user commitments, bad clarification decisions, notification mistakes, and approval/calendar side effects.
Do not reveal chain-of-thought. Return only the requested JSON object.
If the final judgment is materially sound, use review_status=agree.
If it is materially wrong, use review_status=disagreement.
If evidence is mixed or incomplete, use review_status=uncertain.
reason_summary must be short and concrete."""


CHAT_SYSTEM_PROMPT = """You discuss one review case with a human reviewer.
Use only the provided case context and visible chat history.
Do not expose chain-of-thought. Answer briefly and directly."""


class ReviewClient(Protocol):
    model_name: str

    def review_case(self, context: ReviewContext) -> ReviewResult: ...

    def chat(
        self,
        context: ReviewContext,
        history: list[ReviewChatMessage],
        user_message: str,
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
            review_version=os.environ.get("AGENTLEDGER_REVIEW_VERSION", "v1"),
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
                "output_schema": {
                    "review_status": "agree | disagreement | uncertain",
                    "suggested_classification": "string",
                    "issue_type": "string",
                    "confidence": 0.0,
                    "reason_summary": "short string",
                    "needs_human_review": True,
                },
            },
            ensure_ascii=False,
        )
        response = self.client.chat.completions.create(
            model=self.model_name,
            temperature=0,
            response_format={"type": "json_object"},
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
    ) -> str:
        messages = [
            {"role": "system", "content": CHAT_SYSTEM_PROMPT},
            {
                "role": "user",
                "content": json.dumps(
                    {
                        "case": context.to_prompt_payload(),
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
            temperature=0,
            messages=messages,
        )
        return (response.choices[0].message.content or "").strip()
