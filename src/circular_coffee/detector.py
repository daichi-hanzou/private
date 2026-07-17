from __future__ import annotations

from dataclasses import dataclass

from .models import TradeRecord


@dataclass(frozen=True)
class CircularTradeFinding:
    lot_id: str
    owner_path: list[str]
    trade_count: int
    origin_owner_id: str
    returned_day: int | None
    is_circular: bool


def build_owner_path(trade_history: list[TradeRecord], lot_id: str) -> list[str]:
    lot_trades = sorted(
        (trade for trade in trade_history if trade.lot_id == lot_id),
        key=lambda trade: (trade.day, trade.trade_id),
    )
    if not lot_trades:
        return []
    path = [lot_trades[0].seller_id]
    for trade in lot_trades:
        path.append(trade.buyer_id)
    return path


def detect_circular_trade(trade_history: list[TradeRecord], lot_id: str) -> CircularTradeFinding:
    lot_trades = sorted(
        (trade for trade in trade_history if trade.lot_id == lot_id),
        key=lambda trade: (trade.day, trade.trade_id),
    )
    owner_path = build_owner_path(trade_history, lot_id)
    if not owner_path:
        return CircularTradeFinding(
            lot_id=lot_id,
            owner_path=[],
            trade_count=0,
            origin_owner_id="",
            returned_day=None,
            is_circular=False,
        )
    origin_owner_id = owner_path[0]
    intermediate = owner_path[1:-1]
    is_circular = (
        len(lot_trades) >= 3
        and owner_path[0] == owner_path[-1]
        and len(set(intermediate)) >= 2
    )
    returned_day = lot_trades[-1].day if is_circular else None
    return CircularTradeFinding(
        lot_id=lot_id,
        owner_path=owner_path,
        trade_count=len(lot_trades),
        origin_owner_id=origin_owner_id,
        returned_day=returned_day,
        is_circular=is_circular,
    )
