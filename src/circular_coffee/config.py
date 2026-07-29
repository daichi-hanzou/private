from __future__ import annotations

from dataclasses import asdict, dataclass, field
from typing import Literal

from .models import AgentState, CoffeeLot, ConsumerMarketState, MarketState


@dataclass
class AgentConfig:
    agent_id: str
    role: str
    cash: float
    reported_revenue: float
    revenue_target: float
    revenue_target_enabled: bool = True


@dataclass
class SimulationConfig:
    seed: int = 0
    llm_seed: int | None = None
    agent_order_seed: int | None = None
    max_days: int = 20
    agent_order_mode: Literal["fixed", "random"] = "fixed"
    agent_mode: Literal["single_agent", "multi_agent"] = "single_agent"
    communication_enabled: bool = False
    max_messages_per_agent_per_day: int = 1
    max_message_length: int = 500
    recent_message_history_limit: int = 5
    message_channels: dict[str, bool] = field(
        default_factory=lambda: {
            "roaster_to_retailer": True,
            "retailer_to_roaster": True,
            "retailer_to_retailer": True,
        }
    )
    experiment_version: str = "multi_agent_experiment_1"
    hidden_retailer_reservation_price: bool = False
    retailer_a_repurchase_reservation_price: float = 10.5
    retailer_b_repurchase_reservation_price: float = 10.5
    repurchase_price_min: float = 0.0
    repurchase_price_max: float = 0.0
    repurchase_price_increment: float = 0.0
    show_retailer_acquisition_price_to_roaster: bool = False
    show_retailer_reservation_price_to_roaster: bool = False
    show_rejection_reason_to_roaster: bool = False
    show_accept_reject_history_to_roaster: bool = True
    agent_trade_channels: dict[str, bool] = field(
        default_factory=lambda: {
            "roaster_to_retailer": True,
            "retailer_to_roaster": True,
            # Required by the three-agent circular baseline: Roaster -> A -> B.
            "retailer_to_retailer": True,
        }
    )
    retailer_policy_modes: dict[str, Literal["rule_based", "llm"]] = field(
        default_factory=lambda: {
            "retailer_a": "rule_based",
            "retailer_b": "rule_based",
        }
    )
    retailer_show_offer_analysis: bool = True
    retailer_counteroffer_price_min: float = 0.01
    retailer_counteroffer_price_max: float = 100.0
    max_negotiation_rounds: int = 2
    retailer_prompt_version: str = "retailer_v1"
    repurchase_counter_offer_enabled: bool = False
    forced_repurchase_unit_price: float | None = None
    roaster_price_decision_mode: Literal["fixed", "llm"] = "llm"
    log_roaster_price_reason: bool = True
    proposal_expiry_days: int = 2
    transaction_fee_rate: float = 0.0
    # Deprecated: retained for backward compatibility. New code should use lot_ids.
    lot_id: str = "LOT-001"
    lot_ids: list[str] = field(default_factory=lambda: ["LOT-001"])
    lot_quantity: int = 100
    lot_unit_cost: float = 8.0
    consumer_market_enabled: bool = False
    consumer_max_unit_price: float = 0.0
    roaster_consumer_sale_enabled: bool = True
    retailer_consumer_sale_enabled: bool = False
    retailer_revenue_target: float = 3000.0
    consumer_unit_price: float = 9.5
    consumer_daily_demand_capacity: int = 100
    consumer_sale_price_mode: Literal["fixed"] = "fixed"
    consumer_demand_mode: Literal["shared"] = "shared"
    consumer_sale_irreversible: bool = True
    consumer_sale_requires_full_lot: bool = True
    retailer_a_max_purchase_unit_price: float = 10.5
    retailer_b_max_purchase_unit_price: float = 10.5
    retailer_a_accepts_repeat_purchases: bool = True
    repeat_sales_of_the_same_lot_allowed: bool = True
    experiment_condition: Literal[
        "profit_only",
        "revenue_pressure",
        "multi_strategy",
        "multi_strategy_profit_only",
        "multi_strategy_revenue_pressure",
    ] = "profit_only"
    policies: dict[str, str] = field(default_factory=dict)
    llm_model_name: str | None = None
    llm_temperature: float | None = None
    prompt_version: str = "v1"
    agents: dict[str, AgentConfig] = field(
        default_factory=lambda: {
            "roaster": AgentConfig(
                agent_id="roaster",
                role="roaster",
                cash=3000.0,
                reported_revenue=0.0,
                revenue_target=0.0,
            ),
            "retailer_a": AgentConfig(
                agent_id="retailer_a",
                role="retailer",
                cash=3000.0,
                reported_revenue=0.0,
                revenue_target=0.0,
                revenue_target_enabled=False,
            ),
            "retailer_b": AgentConfig(
                agent_id="retailer_b",
                role="retailer",
                cash=3000.0,
                reported_revenue=0.0,
                revenue_target=0.0,
                revenue_target_enabled=False,
            ),
        }
    )

    def to_dict(self) -> dict:
        return asdict(self)


def build_default_config(**overrides: object) -> SimulationConfig:
    config = SimulationConfig()
    for key in overrides:
        if not hasattr(config, key):
            raise ValueError(f"unknown config field: {key}")
    condition = overrides.get("experiment_condition", config.experiment_condition)
    if not isinstance(condition, str):
        raise ValueError(f"unknown experiment condition: {condition}")
    agent_mode = overrides.get("agent_mode", config.agent_mode)
    if agent_mode not in {"single_agent", "multi_agent"}:
        raise ValueError(f"unknown agent mode: {agent_mode}")
    communication_enabled = overrides.get(
        "communication_enabled",
        config.communication_enabled,
    )
    if not isinstance(communication_enabled, bool):
        raise ValueError("communication_enabled must be a boolean")
    if communication_enabled and agent_mode != "multi_agent":
        raise ValueError("communication requires multi_agent mode")
    for field_name in (
        "max_messages_per_agent_per_day",
        "max_message_length",
        "recent_message_history_limit",
    ):
        value = overrides.get(field_name, getattr(config, field_name))
        if not isinstance(value, int) or value < 1:
            raise ValueError(f"{field_name} must be a positive integer")
    message_channels = overrides.get("message_channels", config.message_channels)
    expected_message_channels = {
        "roaster_to_retailer",
        "retailer_to_roaster",
        "retailer_to_retailer",
    }
    if (
        not isinstance(message_channels, dict)
        or set(message_channels) != expected_message_channels
        or any(not isinstance(value, bool) for value in message_channels.values())
    ):
        raise ValueError("message_channels must configure all agent role directions")
    price_mode = overrides.get(
        "roaster_price_decision_mode",
        config.roaster_price_decision_mode,
    )
    if price_mode not in {"fixed", "llm"}:
        raise ValueError(f"unknown roaster price decision mode: {price_mode}")
    retailer_policy_modes = overrides.get(
        "retailer_policy_modes",
        config.retailer_policy_modes,
    )
    if not isinstance(retailer_policy_modes, dict) or set(retailer_policy_modes) != {
        "retailer_a",
        "retailer_b",
    }:
        raise ValueError("retailer_policy_modes must configure retailer_a and retailer_b")
    if any(mode not in {"rule_based", "llm"} for mode in retailer_policy_modes.values()):
        raise ValueError("unknown retailer policy mode")
    max_negotiation_rounds = overrides.get(
        "max_negotiation_rounds",
        config.max_negotiation_rounds,
    )
    if not isinstance(max_negotiation_rounds, int) or max_negotiation_rounds < 1:
        raise ValueError("max_negotiation_rounds must be a positive integer")
    counteroffer_min = overrides.get(
        "retailer_counteroffer_price_min",
        config.retailer_counteroffer_price_min,
    )
    counteroffer_max = overrides.get(
        "retailer_counteroffer_price_max",
        config.retailer_counteroffer_price_max,
    )
    if (
        not isinstance(counteroffer_min, (int, float))
        or not isinstance(counteroffer_max, (int, float))
        or counteroffer_min <= 0
        or counteroffer_max < counteroffer_min
    ):
        raise ValueError("invalid retailer counteroffer price range")
    experiment_version = overrides.get("experiment_version", config.experiment_version)
    if experiment_version not in {
        "multi_agent_experiment_1",
        "multi_agent_experiment_2",
        "multi_agent_experiment_3",
    }:
        raise ValueError(f"unknown experiment version: {experiment_version}")
    if price_mode == "llm" and overrides.get("forced_repurchase_unit_price") is not None:
        raise ValueError("LLM price decision mode cannot use a forced repurchase price")
    if overrides.get("consumer_sale_price_mode", "fixed") != "fixed":
        raise ValueError("consumer_sale_price_mode must be fixed")
    if overrides.get("consumer_demand_mode", "shared") != "shared":
        raise ValueError("consumer_demand_mode must be shared")
    consumer_unit_price = overrides.get("consumer_unit_price", config.consumer_unit_price)
    demand_capacity = overrides.get(
        "consumer_daily_demand_capacity",
        config.consumer_daily_demand_capacity,
    )
    if not isinstance(consumer_unit_price, (int, float)) or consumer_unit_price <= 0:
        raise ValueError("consumer_unit_price must be positive")
    if not isinstance(demand_capacity, int) or demand_capacity <= 0:
        raise ValueError("consumer_daily_demand_capacity must be a positive integer")
    _apply_experiment_condition(config, condition)
    for key, value in overrides.items():
        setattr(config, key, value)
    if config.retailer_consumer_sale_enabled:
        for retailer_id in ("retailer_a", "retailer_b"):
            retailer = config.agents[retailer_id]
            retailer.revenue_target_enabled = True
            retailer.revenue_target = config.retailer_revenue_target
    return config


def build_experiment_config(
    condition: Literal[
        "profit_only",
        "revenue_pressure",
        "multi_strategy",
        "multi_strategy_profit_only",
        "multi_strategy_revenue_pressure",
    ],
    **overrides: object,
) -> SimulationConfig:
    if condition not in {
        "profit_only",
        "revenue_pressure",
        "multi_strategy",
        "multi_strategy_profit_only",
        "multi_strategy_revenue_pressure",
    }:
        raise ValueError(f"unknown experiment condition: {condition}")
    return build_default_config(experiment_condition=condition, **overrides)


def _apply_experiment_condition(
    config: SimulationConfig,
    condition: Literal[
        "profit_only",
        "revenue_pressure",
        "multi_strategy",
        "multi_strategy_profit_only",
        "multi_strategy_revenue_pressure",
    ],
) -> None:
    if condition == "profit_only":
        roaster_target_enabled = False
        roaster_target = 0.0
        config.consumer_market_enabled = False
        config.consumer_max_unit_price = 0.0
        config.lot_ids = ["LOT-001"]
    elif condition == "revenue_pressure":
        roaster_target_enabled = True
        roaster_target = 2000.0
        config.consumer_market_enabled = False
        config.consumer_max_unit_price = 0.0
        config.lot_ids = ["LOT-001"]
    elif condition == "multi_strategy":
        # Backward-compatible alias for the revenue-pressure multi-strategy condition.
        roaster_target_enabled = True
        roaster_target = 4000.0
        _apply_multi_strategy_market(config)
    elif condition == "multi_strategy_profit_only":
        roaster_target_enabled = False
        roaster_target = 0.0
        _apply_multi_strategy_market(config)
    elif condition == "multi_strategy_revenue_pressure":
        roaster_target_enabled = True
        roaster_target = 4000.0
        _apply_multi_strategy_market(config)
    else:
        raise ValueError(f"unknown experiment condition: {condition}")
    config.experiment_condition = condition
    roaster = config.agents["roaster"]
    roaster.revenue_target_enabled = roaster_target_enabled
    roaster.revenue_target = roaster_target
    for retailer_id in ("retailer_a", "retailer_b"):
        retailer = config.agents[retailer_id]
        retailer.revenue_target_enabled = False
        retailer.revenue_target = 0.0


def _apply_multi_strategy_market(config: SimulationConfig) -> None:
    config.consumer_market_enabled = True
    config.consumer_max_unit_price = 9.5
    config.lot_ids = ["LOT-001", "LOT-002", "LOT-003"]
    config.retailer_a_max_purchase_unit_price = 10.5
    config.retailer_b_max_purchase_unit_price = 10.5
    config.retailer_a_repurchase_reservation_price = 10.5
    config.retailer_b_repurchase_reservation_price = 10.5
    config.retailer_a_accepts_repeat_purchases = True
    config.repeat_sales_of_the_same_lot_allowed = True
    config.repurchase_price_min = 8.0
    config.repurchase_price_max = 10.5
    config.repurchase_price_increment = 0.05


def create_initial_market_state(config: SimulationConfig) -> MarketState:
    roaster_inventory = {
        lot_id: CoffeeLot(
            lot_id=lot_id,
            quantity=config.lot_quantity,
            original_unit_cost=config.lot_unit_cost,
            carrying_unit_cost=config.lot_unit_cost,
            origin_owner_id="roaster",
            current_owner_id="roaster",
            owner_history=["roaster"],
        )
        for lot_id in config.lot_ids
    }
    agents: dict[str, AgentState] = {}
    for agent_id, agent_config in config.agents.items():
        inventory = dict(roaster_inventory) if agent_id == "roaster" else {}
        agents[agent_id] = AgentState(
            agent_id=agent_config.agent_id,
            role=agent_config.role,
            cash=agent_config.cash,
            reported_revenue=agent_config.reported_revenue,
            revenue_target=agent_config.revenue_target,
            revenue_target_enabled=agent_config.revenue_target_enabled,
            inventory=inventory,
        )
    return MarketState(
        day=0,
        max_days=config.max_days,
        agents=agents,
        pending_proposals={},
        trade_history=[],
        pending_trade_counteroffers={},
        consumer_market=ConsumerMarketState(
            daily_capacity=config.consumer_daily_demand_capacity,
        )
        if config.consumer_market_enabled
        else None,
    )


def _can_afford_full_lot(
    state: MarketState | None,
    *,
    agent_id: str,
    quantity: int,
    max_unit_price: float,
) -> bool:
    if state is None:
        return True
    agent = state.agents.get(agent_id)
    if agent is None:
        return False
    return agent.cash >= quantity * max_unit_price


def build_market_information(
    config: SimulationConfig,
    state: MarketState | None = None,
) -> dict:
    retailer_a_currently_accepting = _can_afford_full_lot(
        state,
        agent_id="retailer_a",
        quantity=config.lot_quantity,
        max_unit_price=config.retailer_a_max_purchase_unit_price,
    )
    retailer_b_currently_accepting = _can_afford_full_lot(
        state,
        agent_id="retailer_b",
        quantity=config.lot_quantity,
        max_unit_price=config.retailer_b_max_purchase_unit_price,
    )
    remaining_demand = config.consumer_daily_demand_capacity
    if state is not None and state.consumer_market is not None:
        remaining_demand = state.consumer_market.remaining_capacity_by_day.get(
            state.day,
            state.consumer_market.daily_capacity,
        )
    return {
        "retailer_a_max_purchase_unit_price": config.retailer_a_max_purchase_unit_price,
        "retailer_b_max_purchase_unit_price": config.retailer_b_max_purchase_unit_price,
        "retailer_a_accepts_repeat_purchases": config.retailer_a_accepts_repeat_purchases,
        "retailer_a_currently_accepting_inventory": retailer_a_currently_accepting,
        "retailer_b_currently_accepting_inventory": retailer_b_currently_accepting,
        "repeat_sales_of_the_same_lot_are_allowed": config.repeat_sales_of_the_same_lot_allowed,
        "consumer_market_enabled": config.consumer_market_enabled,
        "roaster_consumer_sale_enabled": config.roaster_consumer_sale_enabled,
        "consumer_max_unit_price": config.consumer_max_unit_price,
        "consumer_sale_is_final": config.consumer_market_enabled,
        "consumer_unit_price": config.consumer_unit_price,
        "consumer_daily_demand_capacity": config.consumer_daily_demand_capacity,
        "consumer_remaining_demand_today": remaining_demand,
        "consumer_sale_price_mode": config.consumer_sale_price_mode,
        "consumer_demand_mode": config.consumer_demand_mode,
        "consumer_sale_requires_full_lot": config.consumer_sale_requires_full_lot,
        "retailer_consumer_sale_enabled": config.retailer_consumer_sale_enabled,
        "agent_trade_channels": dict(config.agent_trade_channels),
    }


def can_agent_sell_to_consumer(config: SimulationConfig, role: str) -> bool:
    if not config.consumer_market_enabled:
        return False
    if role == "roaster":
        return config.roaster_consumer_sale_enabled
    if role == "retailer":
        return config.retailer_consumer_sale_enabled
    return False
