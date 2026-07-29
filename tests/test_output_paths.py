import importlib.util
from pathlib import Path


SCRIPT_PATH = (
    Path(__file__).resolve().parents[1]
    / "scripts"
    / "run_llm_condition_comparison.py"
)
SPEC = importlib.util.spec_from_file_location(
    "run_llm_condition_comparison",
    SCRIPT_PATH,
)
assert SPEC is not None and SPEC.loader is not None
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)
build_output_root = MODULE.build_output_root


def test_long_multi_agent_output_path_is_compacted_for_windows() -> None:
    path = build_output_root(
        condition="multi_strategy_revenue_pressure",
        model="gpt-5.6",
        target=4000.0,
        agent_mode="multi_agent",
        experiment_version="multi_agent_experiment_2",
        retailer_policy_modes={"retailer_a": "llm", "retailer_b": "llm"},
        retailer_consumer_sale_enabled=True,
        roaster_consumer_sale_enabled=False,
        communication_mode="bidirectional",
    )
    assert path.parts[:3] == ("results", "compact", "msrp")
    assert len(str((Path.cwd() / path / "seed_0" / "communication_actions.jsonl").resolve())) < 260


def test_short_output_path_keeps_existing_readable_layout() -> None:
    path = build_output_root(
        condition="profit_only",
        target=0.0,
    )
    assert path == Path("results/profit_only")


def test_model_name_separates_otherwise_identical_runs() -> None:
    gpt_55 = build_output_root(
        condition="revenue_pressure",
        model="gpt-5.5",
        target=7000.0,
    )
    gpt_56 = build_output_root(
        condition="revenue_pressure",
        model="gpt-5.6",
        target=7000.0,
    )
    assert gpt_55 != gpt_56
    assert "model_gpt-5.5" in gpt_55.parts
    assert "model_gpt-5.6" in gpt_56.parts


def test_retailer_revenue_target_separates_output_paths() -> None:
    common = {
        "condition": "multi_strategy_revenue_pressure",
        "model": "gpt-5.5",
        "target": 7000.0,
        "lot_count": 5,
        "agent_mode": "multi_agent",
        "experiment_version": "multi_agent_experiment_2",
        "retailer_policy_modes": {
            "retailer_a": "llm",
            "retailer_b": "llm",
        },
        "retailer_consumer_sale_enabled": True,
        "roaster_consumer_sale_enabled": False,
        "communication_mode": "bidirectional",
    }

    target_3000 = build_output_root(
        **common,
        retailer_revenue_target=3000.0,
    )
    target_4000 = build_output_root(
        **common,
        retailer_revenue_target=4000.0,
    )

    assert target_3000 != target_4000
