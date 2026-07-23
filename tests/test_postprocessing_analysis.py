from circular_coffee.analysis.cycle_revenue_analysis import (
    calculate_cycle_generated_revenue,
)
from circular_coffee.analysis.trade_direction_analysis import (
    count_trade_directions,
)


def _trade(
    trade_id: str,
    day: int,
    seller: str,
    buyer: str,
    total_price: float,
) -> dict:
    return {
        "trade_id": trade_id,
        "day": day,
        "seller_id": seller,
        "buyer_id": buyer,
        "lot_id": "LOT-001",
        "quantity": 100,
        "unit_price": total_price / 100,
        "total_price": total_price,
        "trade_type": "agent_trade",
    }


def test_cycle_revenue_counts_only_sales_after_cycle_completion() -> None:
    first_sale = _trade("trade-1", 1, "roaster", "retailer_a", 1000.0)
    return_sale = _trade("trade-2", 2, "retailer_a", "roaster", 1010.0)
    repeated_sale = _trade("trade-3", 3, "roaster", "retailer_a", 1020.0)

    assert calculate_cycle_generated_revenue([first_sale]) == 0.0
    assert calculate_cycle_generated_revenue([first_sale, return_sale]) == 0.0
    assert (
        calculate_cycle_generated_revenue(
            [first_sale, return_sale, repeated_sale]
        )
        == 1020.0
    )


def test_trade_direction_analysis_uses_agent_roles() -> None:
    trades = [
        _trade("trade-1", 1, "roaster", "retailer_a", 1000.0),
        _trade("trade-2", 2, "retailer_a", "roaster", 1010.0),
    ]
    agents = {
        "roaster": {"role": "roaster"},
        "retailer_a": {"role": "retailer"},
    }

    assert count_trade_directions(trades, agents) == {
        "retailer_to_roaster": 1,
        "roaster_to_retailer": 1,
    }
