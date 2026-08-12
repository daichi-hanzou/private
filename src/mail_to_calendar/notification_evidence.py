from __future__ import annotations

import re
from dataclasses import dataclass

from .extractor import RuleBasedCalendarExtractor


_DOMAIN = (
    "口座振替", "引き落とし", "引落", "クレジットカード請求", "支払い",
    "請求", "payment", "billing", "debit", "税金", "保険料", "保険更新",
    "行政通知", "納付", "契約更新", "サブスクリプション更新",
)
_OBLIGATION = (
    "残高確認", "残高をご確認", "引き落とします", "引落予定", "振替予定",
    "支払期限", "納付期限", "更新期限", "手続き期限", "本人確認期限",
    "までにお支払い", "お支払いください", "納付してください",
    "手続きしてください", "action required", "payment due", "due date",
)
_AMOUNT = re.compile(r"(?<!\d)(\d{1,3}(?:,\d{3})+|\d+)\s*円")


@dataclass(frozen=True)
class ImportantNotificationEvidence:
    detected: bool
    reason: str | None = None
    grounded_date: str | None = None
    amount_detected: bool = False


def detect_important_notification_evidence(
    text: str, *, base_year: int,
) -> ImportantNotificationEvidence:
    """Detect concrete private payment/deadline obligations, not promotions."""
    folded = text.casefold()
    domain = next((item for item in _DOMAIN if item.casefold() in folded), None)
    obligation = next(
        (item for item in _OBLIGATION if item.casefold() in folded), None
    )
    extractor = RuleBasedCalendarExtractor(base_year=base_year)
    try:
        grounded_date = extractor._extract_date(text)
    except ValueError:
        grounded_date = None
    amount = bool(_AMOUNT.search(text))
    detected = bool(domain and obligation and grounded_date)
    return ImportantNotificationEvidence(
        detected,
        "important_deadline_with_user_obligation" if detected else None,
        grounded_date if detected else None,
        amount,
    )
