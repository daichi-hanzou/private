from circular_coffee.detector import build_owner_path, compress_consecutive_owners, detect_circular_trade
from circular_coffee.models import TradeRecord


def _trade(day: int, seller: str, buyer: str, lot_id: str = "LOT-001") -> TradeRecord:
    return TradeRecord(
        trade_id=f"trade-{day}-{seller}-{buyer}",
        day=day,
        seller_id=seller,
        buyer_id=buyer,
        lot_id=lot_id,
        quantity=100,
        unit_price=10.0,
        total_price=1000.0,
    )


def test_detects_three_party_circular_trade() -> None:
    history = [
        _trade(1, "roaster", "retailer_a"),
        _trade(2, "retailer_a", "retailer_b"),
        _trade(3, "retailer_b", "roaster"),
    ]
    finding = detect_circular_trade(history, "LOT-001")
    assert build_owner_path(history, "LOT-001") == ["roaster", "retailer_a", "retailer_b", "roaster"]
    assert finding.is_circular is True
    assert finding.cycle_count >= 1
    assert finding.cycle_paths == [["roaster", "retailer_a", "retailer_b", "roaster"]]
    assert finding.first_cycle_start_trade_index == 0
    assert finding.first_cycle_completion_trade_index == 2


def test_two_hop_trade_is_not_circular() -> None:
    history = [_trade(1, "roaster", "retailer_a")]
    finding = detect_circular_trade(history, "LOT-001")
    assert finding.is_circular is False
    assert finding.cycle_count == 0
    assert finding.first_cycle_start_trade_index is None
    assert finding.first_cycle_completion_trade_index is None


def test_two_party_return_is_circular_when_owner_reacquires_lot() -> None:
    history = [
        _trade(1, "roaster", "retailer_a"),
        _trade(2, "retailer_a", "roaster"),
    ]
    finding = detect_circular_trade(history, "LOT-001")
    assert finding.owner_path == ["roaster", "retailer_a", "roaster"]
    assert finding.is_circular is True
    assert finding.cycle_count == 1


def test_does_not_mix_multiple_lots() -> None:
    history = [
        _trade(1, "roaster", "retailer_a", "LOT-001"),
        _trade(2, "retailer_a", "retailer_b", "LOT-002"),
        _trade(3, "retailer_b", "roaster", "LOT-002"),
    ]
    finding = detect_circular_trade(history, "LOT-001")
    assert finding.owner_path == ["roaster", "retailer_a"]
    assert finding.is_circular is False


def test_build_owner_path_sorts_by_day_and_trade_id() -> None:
    history = [
        TradeRecord(
            trade_id="trade-2",
            day=2,
            seller_id="retailer_a",
            buyer_id="retailer_b",
            lot_id="LOT-001",
            quantity=100,
            unit_price=10.1,
            total_price=1010.0,
        ),
        TradeRecord(
            trade_id="trade-1",
            day=1,
            seller_id="roaster",
            buyer_id="retailer_a",
            lot_id="LOT-001",
            quantity=100,
            unit_price=10.0,
            total_price=1000.0,
        ),
        TradeRecord(
            trade_id="trade-3",
            day=3,
            seller_id="retailer_b",
            buyer_id="roaster",
            lot_id="LOT-001",
            quantity=100,
            unit_price=10.2,
            total_price=1020.0,
        ),
    ]
    assert build_owner_path(history, "LOT-001") == [
        "roaster",
        "retailer_a",
        "retailer_b",
        "roaster",
    ]


def test_consecutive_duplicate_owner_is_not_a_cycle() -> None:
    owner_path = ["roaster", "roaster", "retailer_a"]
    assert compress_consecutive_owners(owner_path) == ["roaster", "retailer_a"]


def test_cycle_indices_use_original_trade_history_positions() -> None:
    history = [
        _trade(1, "roaster", "roaster"),
        _trade(2, "roaster", "retailer_a"),
        _trade(3, "retailer_a", "retailer_b"),
        _trade(4, "retailer_b", "roaster"),
    ]
    finding = detect_circular_trade(history, "LOT-001")
    assert finding.is_circular is True
    assert finding.first_cycle_start_trade_index == 0
    assert finding.first_cycle_completion_trade_index == 3
    assert finding.returned_day == 4


def test_multiple_cycles_are_counted_without_overcounting_subcycles() -> None:
    history = [
        _trade(1, "roaster", "retailer_a"),
        _trade(2, "retailer_a", "retailer_b"),
        _trade(3, "retailer_b", "roaster"),
        _trade(4, "roaster", "retailer_a"),
        _trade(5, "retailer_a", "retailer_b"),
        _trade(6, "retailer_b", "roaster"),
    ]
    finding = detect_circular_trade(history, "LOT-001")
    assert finding.is_circular is True
    assert finding.cycle_count >= 2


def test_consumer_trade_is_ignored_by_circular_detector() -> None:
    history = [
        _trade(1, "roaster", "retailer_a"),
        TradeRecord(
            trade_id="trade-2",
            day=2,
            seller_id="retailer_a",
            buyer_id="consumer_market",
            lot_id="LOT-001",
            quantity=100,
            unit_price=9.0,
            total_price=900.0,
            trade_type="consumer",
        ),
    ]
    finding = detect_circular_trade(history, "LOT-001")
    assert finding.owner_path == ["roaster", "retailer_a"]
    assert finding.is_circular is False
