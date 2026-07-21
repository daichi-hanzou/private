from __future__ import annotations

from dataclasses import dataclass

from .models import TradeRecord


@dataclass(frozen=True)
class CircularTradeFinding:
    """Circular trade finding reconstructed from immutable trade history.

    `first_cycle_start_trade_index` is the 0-based index of the first trade
    belonging to the first detected cycle in sorted trade history.

    `first_cycle_completion_trade_index` is the 0-based index of the trade
    that returns the lot to a previously seen owner and completes the first
    detected cycle in sorted trade history.
    """
    lot_id: str
    owner_path: list[str]
    trade_count: int
    origin_owner_id: str
    returned_day: int | None
    is_circular: bool
    cycle_count: int = 0
    cycle_paths: list[list[str]] | None = None
    first_cycle_start_trade_index: int | None = None
    first_cycle_completion_trade_index: int | None = None


def build_owner_path(trade_history: list[TradeRecord], lot_id: str) -> list[str]:
    lot_trades = sorted(
        (
            trade
            for trade in trade_history
            if trade.lot_id == lot_id and trade.trade_type == "intercompany"
        ),
        key=lambda trade: (trade.day, trade.trade_id),
    )
    if not lot_trades:
        return []
    path = [lot_trades[0].seller_id]
    for trade in lot_trades:
        path.append(trade.buyer_id)
    return path


def compress_consecutive_owners(owner_path: list[str]) -> list[str]:
    """Remove adjacent duplicates so no-op owner updates are ignored."""
    compressed: list[str] = []
    for owner in owner_path:
        if not compressed or compressed[-1] != owner:
            compressed.append(owner)
    return compressed


def compress_owner_path_with_indices(owner_path: list[str]) -> list[tuple[str, int]]:
    """Compress adjacent duplicates while preserving original owner-path indices."""
    compressed: list[tuple[str, int]] = []
    for original_index, owner in enumerate(owner_path):
        if not compressed or compressed[-1][0] != owner:
            compressed.append((owner, original_index))
    return compressed


def find_cycle_paths(owner_path: list[str]) -> tuple[list[list[str]], int | None, int | None]:
    """Return non-overlapping cycles discovered in chronological owner order.

    The algorithm scans the compressed owner path and records the earliest cycle
    completed within the current window. After a cycle is found, scanning
    restarts from the repeated owner so overlapping sub-cycles are not counted
    excessively.
    """
    compressed = compress_owner_path_with_indices(owner_path)
    cycles: list[list[str]] = []
    first_start: int | None = None
    first_completion: int | None = None
    window_start = 0
    seen: dict[str, int] = {}

    for index, (owner, original_owner_index) in enumerate(compressed):
        if owner in seen:
            start_index = seen[owner]
            cycle = [name for name, _ in compressed[start_index : index + 1]]
            cycles.append(cycle)
            if first_start is None:
                start_owner_index = compressed[start_index][1]
                completion_owner_index = original_owner_index
                first_start = start_owner_index
                first_completion = completion_owner_index - 1
            window_start = index
            seen = {owner: index}
            continue
        seen[owner] = index
        if window_start > 0:
            seen = {name: pos for name, pos in seen.items() if pos >= window_start}
    return cycles, first_start, first_completion


def detect_circular_trade(trade_history: list[TradeRecord], lot_id: str) -> CircularTradeFinding:
    lot_trades = sorted(
        (
            trade
            for trade in trade_history
            if trade.lot_id == lot_id and trade.trade_type == "intercompany"
        ),
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
            cycle_paths=[],
        )
    origin_owner_id = owner_path[0]
    cycle_paths, first_start, first_completion = find_cycle_paths(owner_path)
    is_circular = bool(cycle_paths)
    returned_day = None
    if is_circular and first_completion is not None and 0 <= first_completion < len(lot_trades):
        returned_day = lot_trades[first_completion].day
    return CircularTradeFinding(
        lot_id=lot_id,
        owner_path=owner_path,
        trade_count=len(lot_trades),
        origin_owner_id=origin_owner_id,
        returned_day=returned_day,
        is_circular=is_circular,
        cycle_count=len(cycle_paths),
        cycle_paths=cycle_paths,
        first_cycle_start_trade_index=first_start,
        first_cycle_completion_trade_index=first_completion,
    )
