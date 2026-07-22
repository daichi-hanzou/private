import pytest

from circular_coffee.config import build_default_config


def test_build_default_config_rejects_unknown_field() -> None:
    with pytest.raises(ValueError, match="unknown config field"):
        build_default_config(does_not_exist=123)


def test_build_default_config_rejects_unknown_agent_mode() -> None:
    with pytest.raises(ValueError, match="unknown agent mode"):
        build_default_config(agent_mode="shared_agent")


def test_llm_price_mode_rejects_forced_repurchase_price() -> None:
    with pytest.raises(ValueError, match="cannot use a forced repurchase price"):
        build_default_config(
            roaster_price_decision_mode="llm",
            forced_repurchase_unit_price=10.0,
        )


def test_build_default_config_rejects_unknown_experiment_version() -> None:
    with pytest.raises(ValueError, match="unknown experiment version"):
        build_default_config(experiment_version="multi_agent_experiment_99")


def test_prompt_version_is_serialized_in_config() -> None:
    config = build_default_config(prompt_version="v2")
    assert config.prompt_version == "v2"
    assert config.to_dict()["prompt_version"] == "v2"


def test_retailer_policy_modes_support_mixed_configuration() -> None:
    config = build_default_config(
        retailer_policy_modes={
            "retailer_a": "llm",
            "retailer_b": "rule_based",
        }
    )
    assert config.retailer_policy_modes == {
        "retailer_a": "llm",
        "retailer_b": "rule_based",
    }


def test_retailer_policy_modes_reject_unknown_mode() -> None:
    with pytest.raises(ValueError, match="unknown retailer policy mode"):
        build_default_config(
            retailer_policy_modes={
                "retailer_a": "cooperative",
                "retailer_b": "rule_based",
            }
        )


def test_retailer_counteroffer_range_must_be_positive_and_ordered() -> None:
    with pytest.raises(ValueError, match="invalid retailer counteroffer price range"):
        build_default_config(
            retailer_counteroffer_price_min=12.0,
            retailer_counteroffer_price_max=11.0,
        )
