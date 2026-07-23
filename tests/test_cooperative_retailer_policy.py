from circular_coffee.policies import (
    PRICE_LIMIT_REJECTION_REASON,
    CooperativeRetailerPolicy,
)


def _observation(*, cash=3000.0, incoming=None, inventory=None):
    return {
        "day": 1,
        "self": {
            "agent_id": "retailer_a",
            "cash": cash,
            "inventory": inventory or {},
        },
        "incoming_pending_proposals": incoming or [],
        "other_agent_ids": ["roaster", "retailer_b"],
    }


def test_retailer_accepts_affordable_offer() -> None:
    action = CooperativeRetailerPolicy(preferred_buyers=["retailer_b", "roaster"]).choose_action(
        _observation(
            incoming=[
                {
                    "proposal_id": "proposal-1",
                    "quantity": 100,
                    "unit_price": 10.0,
                }
            ]
        )
    )
    assert action.action_type == "accept_trade"


def test_retailer_rejects_offer_without_sufficient_cash() -> None:
    action = CooperativeRetailerPolicy(preferred_buyers=["retailer_b", "roaster"]).choose_action(
        _observation(
            cash=500.0,
            incoming=[
                {
                    "proposal_id": "proposal-1",
                    "quantity": 100,
                    "unit_price": 10.0,
                }
            ],
        )
    )
    assert action.action_type == "reject_trade"


def test_retailer_does_not_duplicate_resale_proposal() -> None:
    policy = CooperativeRetailerPolicy(preferred_buyers=["retailer_b", "roaster"])
    observation = _observation(
        inventory={
            "LOT-001": {
                "lot_id": "LOT-001",
                "quantity": 100,
                "carrying_unit_cost": 10.0,
            }
        }
    )
    assert policy.choose_action(observation).action_type == "propose_trade"
    assert policy.choose_action(observation).action_type == "wait"
    observation["day"] = 5
    assert policy.choose_action(observation).action_type == "propose_trade"


def test_retailer_b_prefers_roaster_as_resale_buyer() -> None:
    policy = CooperativeRetailerPolicy(preferred_buyers=["roaster", "retailer_a"])
    action = policy.choose_action(
        _observation(
            inventory={
                "LOT-001": {
                    "lot_id": "LOT-001",
                    "quantity": 100,
                    "carrying_unit_cost": 10.1,
                }
            }
        )
    )
    assert action.seller_id == "retailer_a"
    assert action.buyer_id == "roaster"


def test_retailer_a_accepts_offer_at_price_limit() -> None:
    action = CooperativeRetailerPolicy(
        preferred_buyers=["retailer_b", "roaster"],
        max_purchase_unit_price=10.5,
    ).choose_action(
        _observation(
            incoming=[
                {
                    "proposal_id": "proposal-1",
                    "seller_id": "roaster",
                    "quantity": 100,
                    "unit_price": 10.5,
                }
            ]
        )
    )
    assert action.action_type == "accept_trade"


def test_retailer_a_rejects_offer_above_price_limit() -> None:
    action = CooperativeRetailerPolicy(
        preferred_buyers=["retailer_b", "roaster"],
        max_purchase_unit_price=10.5,
    ).choose_action(
        _observation(
            incoming=[
                {
                    "proposal_id": "proposal-1",
                    "seller_id": "roaster",
                    "quantity": 100,
                    "unit_price": 10.51,
                }
            ]
        )
    )
    assert action.action_type == "reject_trade"
    assert action.reason_summary == PRICE_LIMIT_REJECTION_REASON
