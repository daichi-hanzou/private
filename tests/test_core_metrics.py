from __future__ import annotations

import json

from circular_coffee.config import build_experiment_config
from circular_coffee.core_metrics import (
    build_core_metrics,
    build_core_metrics_from_run_dir,
)
from circular_coffee.policies import ScriptedCircularPolicy
from circular_coffee.simulation import SimulationRunner


def test_metrics_file_contains_only_core_research_metrics(tmp_path) -> None:
    config = build_experiment_config("profit_only", seed=7)
    policy = ScriptedCircularPolicy()
    result = SimulationRunner(
        config,
        {
            "roaster": policy,
            "retailer_a": policy,
            "retailer_b": policy,
        },
        run_id="core_metrics",
        output_root=tmp_path,
    ).run()

    run_dir = tmp_path / "core_metrics"
    core = json.loads((run_dir / "metrics.json").read_text(encoding="utf-8"))

    assert set(core) == {
        "run_id",
        "seed",
        "cycle",
        "trades",
        "offers",
        "agents",
        "errors",
    }
    assert core == result.metrics
    assert core["cycle"]["detected"] is True
    assert core["trades"]["total"] == 3
    assert core["trades"]["agent"] == 3
    assert core["agents"]["roaster"]["economic_profit"] == -20.0
    assert "generated_revenue" not in core["cycle"]
    assert "price_discovery_metrics" not in core
    assert "conditional_max_reachable_revenue" not in json.dumps(core)
    assert not (run_dir / "analysis_metrics.json").exists()


def test_core_metrics_can_be_rebuilt_from_raw_artifacts(tmp_path) -> None:
    config = build_experiment_config("profit_only", seed=3)
    policy = ScriptedCircularPolicy()
    result = SimulationRunner(
        config,
        {
            "roaster": policy,
            "retailer_a": policy,
            "retailer_b": policy,
        },
        run_id="rebuild_core_metrics",
        output_root=tmp_path,
    ).run()

    rebuilt = build_core_metrics_from_run_dir(result.output_dir)

    assert rebuilt == result.metrics

    (result.output_dir / "negotiations.jsonl").unlink()
    assert build_core_metrics_from_run_dir(result.output_dir) == result.metrics


def test_authoritative_logs_are_unambiguous(tmp_path) -> None:
    config = build_experiment_config("profit_only", seed=4)
    policy = ScriptedCircularPolicy()
    result = SimulationRunner(
        config,
        {
            "roaster": policy,
            "retailer_a": policy,
            "retailer_b": policy,
        },
        run_id="authoritative_logs",
        output_root=tmp_path,
    ).run()

    trades = [
        json.loads(line)
        for line in (result.output_dir / "trades.jsonl")
        .read_text(encoding="utf-8")
        .splitlines()
    ]
    proposals = [
        json.loads(line)
        for line in (result.output_dir / "proposals.jsonl")
        .read_text(encoding="utf-8")
        .splitlines()
    ]
    final_state = json.loads(
        (result.output_dir / "final_state.json").read_text(encoding="utf-8")
    )

    assert all("trade_id" in trade and "trade" not in trade for trade in trades)
    assert {trade["trade_type"] for trade in trades} == {"agent_trade"}
    assert {row["event_type"] for row in proposals} == {
        "proposal_created",
        "proposal_accepted",
    }
    assert all(
        row.get("trade_id")
        for row in proposals
        if row["event_type"] == "proposal_accepted"
    )
    assert "trade_completed" not in json.dumps(proposals)
    assert "offer_type" not in json.dumps(proposals)
    assert "cash_proceeds" not in json.dumps(proposals)
    assert "trade_history" not in final_state
    assert all("observation" not in row for row in result.action_logs)
    assert all("requested_action" in row for row in result.action_logs)


def test_trade_offer_and_fallback_sources_are_independent(tmp_path) -> None:
    config = build_experiment_config("profit_only", seed=5)
    policy = ScriptedCircularPolicy()
    result = SimulationRunner(
        config,
        {
            "roaster": policy,
            "retailer_a": policy,
            "retailer_b": policy,
        },
        run_id="source_independence",
        output_root=tmp_path,
    ).run()
    initial_state = json.loads(
        (result.output_dir / "initial_state.json").read_text(encoding="utf-8")
    )
    final_state = json.loads(
        (result.output_dir / "final_state.json").read_text(encoding="utf-8")
    )
    first_trade = json.loads(
        (result.output_dir / "trades.jsonl")
        .read_text(encoding="utf-8")
        .splitlines()[0]
    )
    metrics = build_core_metrics(
        run_id="independent",
        seed=5,
        lot_ids=config.lot_ids,
        initial_state=initial_state,
        final_state=final_state,
        actions=[
            {
                "is_valid": True,
                "llm_fallback_used": True,
                "llm_api_error": False,
            }
        ],
        proposals=[
            {"event_type": "proposal_created", "proposal_id": "p1"},
            {"event_type": "proposal_accepted", "proposal_id": "p1"},
            {"event_type": "proposal_accepted", "proposal_id": "p2"},
            {
                "event_type": "proposal_rejected",
                "proposal_id": "p3",
                "retailer_llm": {"fallback_used": True},
            },
        ],
        trades=[first_trade],
    )

    assert metrics["trades"]["total"] == 1
    assert metrics["offers"]["accepted"] == 2
    assert metrics["offers"]["rejected"] == 1
    assert metrics["errors"]["fallbacks"] == 1


def test_legacy_intercompany_trade_type_is_counted_as_agent_trade(tmp_path) -> None:
    config = build_experiment_config("profit_only", seed=6)
    policy = ScriptedCircularPolicy()
    result = SimulationRunner(
        config,
        {
            "roaster": policy,
            "retailer_a": policy,
            "retailer_b": policy,
        },
        run_id="legacy_trade_type",
        output_root=tmp_path,
    ).run()
    initial_state = json.loads(
        (result.output_dir / "initial_state.json").read_text(encoding="utf-8")
    )
    final_state = json.loads(
        (result.output_dir / "final_state.json").read_text(encoding="utf-8")
    )
    trade = json.loads(
        (result.output_dir / "trades.jsonl")
        .read_text(encoding="utf-8")
        .splitlines()[0]
    )
    trade["trade_type"] = "intercompany"

    metrics = build_core_metrics(
        run_id="legacy",
        seed=6,
        lot_ids=config.lot_ids,
        initial_state=initial_state,
        final_state=final_state,
        actions=[],
        proposals=[],
        trades=[trade],
    )

    assert metrics["trades"] == {"total": 1, "agent": 1, "consumer": 0}
