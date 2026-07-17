from circular_coffee.policies import CooperativeRetailerPolicy


def _observation(*, cash=3000.0, incoming=None, inventory=None):
    return {
        "day": 1,
        "self": {"cash": cash, "inventory": inventory or {}},
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
    assert action.counterparty_id == "roaster"
