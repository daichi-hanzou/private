from __future__ import annotations

from pathlib import Path

from circular_coffee.visualization import (
    build_lot_timelines,
    build_roaster_daily_revenue_breakdown,
    build_revenue_time_series,
    render_html_report,
    render_multi_seed_html_report,
)


def test_build_revenue_time_series_classifies_resales() -> None:
    trades = [
        {
            "trade_id": "trade-1",
            "day": 1,
            "seller_id": "roaster",
            "buyer_id": "retailer_a",
            "lot_id": "LOT-001",
            "quantity": 100,
            "unit_price": 10.5,
            "total_price": 1050.0,
            "trade_type": "intercompany",
        },
        {
            "trade_id": "trade-2",
            "day": 2,
            "seller_id": "retailer_a",
            "buyer_id": "roaster",
            "lot_id": "LOT-001",
            "quantity": 100,
            "unit_price": 10.6,
            "total_price": 1060.0,
            "trade_type": "intercompany",
        },
        {
            "trade_id": "trade-3",
            "day": 3,
            "seller_id": "roaster",
            "buyer_id": "consumer",
            "lot_id": "LOT-002",
            "quantity": 100,
            "unit_price": 9.0,
            "total_price": 900.0,
            "trade_type": "consumer",
        },
    ]

    series = build_revenue_time_series(trades)

    assert [event["sale_class"] for event in series["events"]] == [
        "initial_sale",
        "resale",
        "initial_sale",
    ]
    assert series["points"][-1]["values"]["roaster"] == 1950.0
    assert series["points"][-1]["values"]["retailer_a"] == 1060.0


def test_build_lot_timelines_marks_circular_paths() -> None:
    trades = [
        {
            "trade_id": "trade-1",
            "day": 1,
            "seller_id": "roaster",
            "buyer_id": "retailer_a",
            "lot_id": "LOT-001",
            "quantity": 100,
            "unit_price": 10.5,
            "total_price": 1050.0,
        },
        {
            "trade_id": "trade-2",
            "day": 2,
            "seller_id": "retailer_a",
            "buyer_id": "roaster",
            "lot_id": "LOT-001",
            "quantity": 100,
            "unit_price": 10.6,
            "total_price": 1060.0,
        },
    ]

    timelines = build_lot_timelines(trades)

    assert timelines[0]["owners"] == ["roaster", "retailer_a", "roaster"]
    assert timelines[0]["is_circular"] is True


def test_build_roaster_daily_revenue_breakdown_splits_healthy_and_circular() -> None:
    trades = [
        {
            "trade_id": "trade-1",
            "day": 1,
            "seller_id": "roaster",
            "buyer_id": "retailer_a",
            "lot_id": "LOT-001",
            "quantity": 100,
            "unit_price": 10.5,
            "total_price": 1050.0,
            "trade_type": "intercompany",
        },
        {
            "trade_id": "trade-2",
            "day": 2,
            "seller_id": "retailer_a",
            "buyer_id": "roaster",
            "lot_id": "LOT-001",
            "quantity": 100,
            "unit_price": 10.6,
            "total_price": 1060.0,
            "trade_type": "intercompany",
        },
        {
            "trade_id": "trade-3",
            "day": 3,
            "seller_id": "roaster",
            "buyer_id": "retailer_a",
            "lot_id": "LOT-001",
            "quantity": 100,
            "unit_price": 10.5,
            "total_price": 1050.0,
            "trade_type": "intercompany",
        },
    ]

    breakdown = build_roaster_daily_revenue_breakdown(
        trades,
        max_days=4,
        revenue_target=2000.0,
    )

    assert breakdown["days"][0]["healthy_revenue"] == 1050.0
    assert breakdown["days"][0]["circular_revenue"] == 0.0
    assert breakdown["days"][2]["healthy_revenue"] == 0.0
    assert breakdown["days"][2]["circular_revenue"] == 1050.0
    assert breakdown["days"][2]["cumulative_total_revenue"] == 2100.0
    assert breakdown["days"][2]["kpi_progress_ratio"] == 1.0


def test_render_html_report_writes_visualization(tmp_path: Path) -> None:
    run_dir = Path("outputs/test_scripted_cycle")

    output_path = render_html_report(run_dir, output_path=tmp_path / "report.html")

    html_text = output_path.read_text(encoding="utf-8")
    assert output_path.exists()
    assert "循環取引経路" in html_text
    assert "Roaster累積報告売上" in html_text
    assert "KPI達成の推移" not in html_text
    assert "健全累積売上" in html_text
    assert "循環累積売上" in html_text
    assert "累積売上合計" in html_text
    assert "概要" not in html_text
    assert "LOT-001" in html_text
    assert "単価 10.50 / 総額 1,050.00" in html_text


def test_render_multi_seed_report_lists_available_and_missing_seeds(
    tmp_path: Path,
) -> None:
    experiment_dir = tmp_path / "experiment"
    seed_dir = experiment_dir / "seed_0"
    seed_dir.mkdir(parents=True)
    source_dir = Path("outputs/test_scripted_cycle")
    for filename in ("config.json", "metrics.json", "trades.jsonl"):
        (seed_dir / filename).write_text(
            (source_dir / filename).read_text(encoding="utf-8"),
            encoding="utf-8",
        )

    output_path = render_multi_seed_html_report(
        experiment_dir,
        seeds=[0, 1],
        output_path=tmp_path / "multi_seed.html",
    )

    html_text = output_path.read_text(encoding="utf-8")
    assert "全seedのRoaster累積報告売上" in html_text
    assert "Seed 0 の循環取引経路" in html_text
    assert "seed_1" in html_text
    assert "Seed 0 | 健全 1,050" in html_text
    assert "健全 1,050 / 循環 0 / 合計 1,050" in html_text
    assert "LOT-001" in html_text
    assert "単価 10.50 / 総額 1,050.00" in html_text
    assert "KPI達成の推移" not in html_text
