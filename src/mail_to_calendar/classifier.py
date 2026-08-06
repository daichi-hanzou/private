from __future__ import annotations

import re

from .models import EmailMessage, ImportanceResult


class RuleBasedImportanceClassifier:
    _promotions = ("セール", "キャンペーン", "広告", "メルマガ", "unsubscribe")
    _deadlines = ("提出期限", "締切", "までに", "提出してください", "ご提出")
    _meetings = ("会議", "面談", "打ち合わせ", "ミーティング")
    _appointments = ("予約", "診察", "アポイント")
    _tasks = ("対応をお願いします", "対応してください", "確認してください")
    _request_phrases = (
        "参加をお願いします",
        "参加してください",
        "対応をお願いします",
        "提出してください",
        "ご提出ください",
    )
    _date_pattern = re.compile(
        r"\d{4}年\d{1,2}月\d{1,2}日|\d{1,2}月\d{1,2}日|"
        r"\d{4}-\d{1,2}-\d{1,2}|今日|明日|来週|来月"
    )
    _time_pattern = re.compile(r"\d{1,2}(?::\d{2}|時(?:\d{1,2}分)?)")

    def classify(self, message: EmailMessage) -> ImportanceResult:
        text = f"{message.subject}\n{message.body_text}"
        lowered = text.casefold()
        promotion_hits = [word for word in self._promotions if word in lowered]
        if promotion_hits:
            return ImportanceResult(
                is_important=False,
                score=0.05,
                reasons=[f"promotion indicator: {promotion_hits[0]}"],
                category="promotion",
            )

        score = 0.1
        reasons: list[str] = []
        category = "unknown"
        if any(word in text for word in self._deadlines):
            category = "deadline"
            score += 0.45
            reasons.append("deadline language detected")
        elif any(word in text for word in self._meetings):
            category = "meeting"
            score += 0.4
            reasons.append("meeting language detected")
        elif any(word in text for word in self._appointments):
            category = "appointment"
            score += 0.4
            reasons.append("appointment language detected")
        elif any(word in text for word in self._tasks):
            category = "task"
            score += 0.35
            reasons.append("task request detected")

        if self._date_pattern.search(text):
            score += 0.2
            reasons.append("date expression detected")
        if self._time_pattern.search(text):
            score += 0.1
            reasons.append("time expression detected")
        if any(phrase in text for phrase in self._request_phrases):
            score += 0.15
            reasons.append("direct request detected")
        hint = (message.importance_hint or "").casefold()
        if hint in {"high", "important", "重要"}:
            score += 0.2
            reasons.append("provider importance hint")
        if "important" in {label.casefold() for label in message.labels}:
            score += 0.1
            reasons.append("important label")

        score = round(min(score, 1.0), 2)
        important = score >= 0.5
        if not important and category == "unknown":
            category = "informational"
        if not reasons:
            reasons.append("no importance indicators detected")
        return ImportanceResult(important, score, reasons, category)
