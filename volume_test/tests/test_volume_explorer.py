from __future__ import annotations

import pytest

from agentledger.bundles import action_summary, build_action_bundles
from agentledger.bundles import display_case_ids, target_summary
from agentledger.html import render_action_bundles
from agentledger.normalizer import normalize_events
from volume_test.generate_volume_data import (
    ACTION_PATTERN,
    generate_events,
)


@pytest.mark.parametrize("action_count", [100, 1000, 10000])
def test_volume_data_builds_complete_unique_bundles(
    action_count: int,
) -> None:
    raw = generate_events(action_count)
    assert len(raw) == action_count * 4
    assert len({event["event_id"] for event in raw}) == len(raw)

    bundles = build_action_bundles(normalize_events(raw))
    assert len(bundles) == action_count
    assert all(
        all(
            event is not None
            for event in (
                bundle.observation,
                bundle.decision,
                bundle.outcome,
            )
        )
        for bundle in bundles
    )


def test_action_distribution_contains_all_required_actions() -> None:
    assert set(ACTION_PATTERN) == {
        "propose_trade",
        "accept_trade",
        "reject_trade",
        "counteroffer_trade",
        "sell_to_consumer",
        "wait",
    }


def test_consumer_summary_does_not_render_missing_price() -> None:
    bundles = build_action_bundles(normalize_events(generate_events(20)))
    consumer = next(
        bundle
        for bundle in bundles
        if bundle.action.action_type == "sell_to_consumer"
        and bundle.action.action_parameters.get("unit_price") is None
    )
    summary = action_summary(consumer, {})
    assert summary == "Sold 100 units to consumers"
    assert "None" not in summary


def test_purchase_offer_direction_is_visible() -> None:
    bundles = build_action_bundles(normalize_events(generate_events(40)))
    purchase = next(
        bundle
        for bundle in bundles
        if bundle.action.action_type == "propose_trade"
        and bundle.action.raw_event["buyer_id"] == bundle.action.actor_id
    )
    assert action_summary(purchase, {}).startswith("Offer to Buy")


def test_proposal_response_target_uses_proposal_display_id() -> None:
    bundles = build_action_bundles(normalize_events(generate_events(20)))
    case_ids = display_case_ids(bundles)
    accepted = next(
        bundle
        for bundle in bundles
        if bundle.action.action_type == "accept_trade"
    )

    assert target_summary(accepted, case_ids).endswith(
        f"/ {case_ids[accepted.action.case_id]}"
    )


def test_standalone_html_keeps_action_ui_and_performance_hooks() -> None:
    bundles = build_action_bundles(normalize_events(generate_events(100)))
    content = render_action_bundles(bundles)

    assert "Action List" in content
    assert "Observation · Decision · Action · Outcome" in content
    assert "window.AGENT_LEDGER=" in content
    assert "performance.mark(\"agentledger-start\")" in content
    assert "DocumentFragment" in content
    assert "actionById" in content
    assert "agentLedgerBenchmark" in content
    assert "incrementalSearches" in content
    assert "participant env" not in content
    assert "Technical" not in content
