from __future__ import annotations

import re
from dataclasses import dataclass

from .extractor import RuleBasedCalendarExtractor


_IDENTITY = (
    "予約番号", "booking number", "reservation number", "ご予約",
    "予約確認", "予約内容", "booking confirmed", "reservation confirmed",
    "チケット番号", "受付番号",
)
_TEMPORAL = (
    "チェックイン日時", "チェックイン", "予約日時", "診察日時", "出発日時",
    "搭乗日時", "開催日時", "来店日時", "受取日時",
)
_SERVICE = (
    "宿泊施設", "ホテル", "旅館", "温泉", "病院", "クリニック", "歯科",
    "健診", "航空", "便名", "列車", "鉄道", "チケット", "会場",
    "レストラン", "来店", "配送", "受取",
)
_COMMITMENT = (
    "ご予約ありがとうございます", "予約を承りました", "予約が確定",
    "予約確定", "宿泊予定", "来院予定", "搭乗予定", "参加予定",
    "支払い済み", "予約済み", "予約しました", "本人の予約",
    "本人の宿泊予約", "本人の受診予約", "本人の搭乗予約",
    "あなたの予約", "お客様の予約", "購入済み", "来店予約",
    "受取予約", "配送予約",
)


@dataclass(frozen=True)
class StrongReservationEvidence:
    detected: bool
    commitment_evidence: str | None = None
    derived_date: str | None = None
    derived_start: str | None = None


def detect_strong_personal_reservation(
    text: str, *, base_year: int
) -> StrongReservationEvidence:
    """Detect multi-signal personal reservations without semantic inference."""
    folded = text.casefold()

    def first(patterns: tuple[str, ...]) -> str | None:
        return next((item for item in patterns if item.casefold() in folded), None)

    identity = first(_IDENTITY)
    service = first(_SERVICE)
    commitment = first(_COMMITMENT)
    extractor = RuleBasedCalendarExtractor(base_year=base_year)
    try:
        derived_date = extractor._extract_date(text)
    except ValueError:
        derived_date = None
    try:
        derived_start = extractor._extract_time(text)
    except ValueError:
        derived_start = None
    temporal_label = first(_TEMPORAL)
    concrete_temporal = bool(derived_date and derived_start)
    detected = bool(
        identity and service and commitment
        and concrete_temporal
        and (temporal_label or re.search(r"\d{1,2}(?::\d{2}|時)", text))
    )
    return StrongReservationEvidence(
        detected,
        commitment if detected else None,
        derived_date if detected else None,
        derived_start if detected else None,
    )
