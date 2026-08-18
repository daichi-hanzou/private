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
_TRANSACTION_PATTERNS = (
    "約定しました", "約定が成立", "約定内容", "購入が完了", "取引が完了",
    "口座振替予定", "振替予定", "引落予定", "引き落とします",
    "クレジットカード請求", "カード請求額", "請求額", "支払期限",
    "振込完了", "振込を受け付け", "入金しました", "出金しました",
    "納付期限", "納付してください", "保険料", "保険契約更新", "契約更新",
    "注文確定", "注文を承りました", "ご注文ありがとうございます", "注文番号",
    "発送しました", "発送完了", "配達予定", "お届け予定", "配送状況",
    "trade executed", "purchase completed", "payment due", "debit scheduled",
    "order confirmed", "order number", "has shipped", "delivery scheduled",
)
_GENERIC_CONTENT_PATTERNS = (
    "ニュースレター", "メールマガジン", "市況", "マーケット情報",
    "投資情報", "おすすめ商品", "キャンペーン", "セール", "広告",
    "newsletter", "market update", "product introduction", "special offer",
)


@dataclass(frozen=True)
class ImportantNotificationEvidence:
    detected: bool
    reason: str | None = None
    grounded_date: str | None = None
    amount_detected: bool = False


@dataclass(frozen=True)
class TransactionalEvidence:
    detected: bool
    matched_expression: str | None = None


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


def detect_transactional_evidence(text: str) -> TransactionalEvidence:
    """Detect a concrete user transaction/state change, not topic keywords."""
    folded = text.casefold()
    matched = next(
        (item for item in _TRANSACTION_PATTERNS if item.casefold() in folded),
        None,
    )
    if not matched:
        return TransactionalEvidence(False)
    generic_only = any(
        item.casefold() in folded for item in _GENERIC_CONTENT_PATTERNS
    ) and not any(
        marker.casefold() in folded for marker in (
            "注文番号", "請求額", "振替予定", "約定内容", "発送しました",
            "配達予定", "order number", "order confirmed", "trade executed",
        )
    )
    return TransactionalEvidence(not generic_only, matched if not generic_only else None)
