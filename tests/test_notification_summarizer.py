from __future__ import annotations

import pytest

from mail_calendar_orchestrator.notification_summarizer import (
    PROMPT_VERSION,
    SUMMARY_SCHEMA,
    NotificationSummarizer,
    NotificationSummaryInput,
)
from mail_to_calendar.ollama_client import OllamaError


class FakeClient:
    model = "qwen3:8b"

    def __init__(self, response=None, error=None) -> None:
        self.response = response
        self.error = error
        self.calls = []

    def chat(self, *, messages, schema):
        self.calls.append((messages, schema))
        if self.error:
            raise self.error
        return self.response


def value(
    text: str,
    *,
    subject: str = "重要なお知らせ",
    category: str = "transactional",
    date: str | None = None,
    amount: str | None = None,
    action: str | None = None,
) -> NotificationSummaryInput:
    return NotificationSummaryInput(
        subject=subject,
        analysis_text=text,
        final_classification="transactional",
        category=category,
        grounded_date=date,
        grounded_amount=amount,
        existing_action_hint=action,
    )


@pytest.mark.parametrize(
    ("input_value", "response"),
    [
        (
            value(
                "本日、お荷物をお届けいたします。受取日時をご指定ください。",
                subject="お荷物お届けのお知らせ", category="delivery",
                action="受取日時をご指定ください。",
            ),
            {
                "summary": "本日、お荷物が配達される予定です。",
                "action_hint": "受取日時をご指定ください。",
            },
        ),
        (
            value(
                "投資信託の積立購入が約定しました。購入金額は50,000円です。",
                category="investment", amount="50,000円",
            ),
            {
                "summary": "投資信託の積立購入が約定しました。購入金額は50,000円です。",
                "action_hint": None,
            },
        ),
        (
            value(
                "支払期限は2026年8月31日です。前日までに残高をご確認ください。",
                category="payment", date="2026-08-31",
                action="前日までに残高をご確認ください。",
            ),
            {
                "summary": "支払期限は2026-08-31です。",
                "action_hint": "前日までに残高をご確認ください。",
            },
        ),
        (
            value(
                "8月30日23:30から23:59まで予約受付を一時停止します。",
                category="service",
            ),
            {
                "summary": "8月30日23:30から23:59まで予約受付が一時停止されます。",
                "action_hint": None,
            },
        ),
    ],
)
def test_grounded_notification_summaries_are_accepted(input_value, response):
    client = FakeClient(response)
    result = NotificationSummarizer(client).summarize(
        input_value, fallback_summary="fallback", fallback_action_hint=None
    )
    assert result.source == "llm"
    assert result.summary == response["summary"]
    assert result.action_hint == response["action_hint"]
    assert result.model == "qwen3:8b"
    assert result.prompt_version == PROMPT_VERSION
    assert len(client.calls) == 1
    assert client.calls[0][1] == SUMMARY_SCHEMA
    assert "Final classification:" in client.calls[0][0][1]["content"]


@pytest.mark.parametrize(
    "response",
    [
        {"summary": "請求額は99,999円です。", "action_hint": None},
        {"summary": "支払期限は2026-09-30です。", "action_hint": None},
        {"summary": "この請求は支払済みです。", "action_hint": None},
        {"summary": "東京銀行からの請求です。", "action_hint": None},
        {"summary": "請求内容です。", "action_hint": "パスワードを変更してください。"},
    ],
)
def test_ungrounded_facts_or_actions_use_deterministic_fallback(response):
    input_value = value(
        "請求額は1,000円で、支払期限は2026年8月31日です。現在は未払いです。",
        category="payment", date="2026-08-31", amount="1,000円",
    )
    result = NotificationSummarizer(FakeClient(response)).summarize(
        input_value,
        fallback_summary="請求内容をご確認ください。",
        fallback_action_hint=None,
    )
    assert result.source == "deterministic_fallback"
    assert result.summary == "請求内容をご確認ください。"
    assert result.action_hint is None


@pytest.mark.parametrize(
    "client",
    [
        FakeClient({"summary": "要約だけです。"}),
        FakeClient({"summary": 123, "action_hint": None}),
        FakeClient(error=OllamaError("offline")),
    ],
)
def test_schema_and_ollama_failures_are_best_effort(client):
    result = NotificationSummarizer(client).summarize(
        value("配送に関するお知らせです。"),
        fallback_summary="配送に関するお知らせです。",
        fallback_action_hint=None,
    )
    assert result.source == "deterministic_fallback"
    assert result.summary == "配送に関するお知らせです。"
