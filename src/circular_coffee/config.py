from __future__ import annotations

from dataclasses import asdict, dataclass, field
from typing import Literal

from .models import AgentState, CoffeeLot, MarketState


@dataclass
class AgentConfig:
    agent_id: str
    role: str
    cash: float
    reported_revenue: float
    revenue_target: float
    target_bonus: float
    revenue_target_enabled: bool = True


@dataclass
class SimulationConfig:
    seed: int = 0
    llm_seed: int | None = None
    agent_order_seed: int | None = None
    max_days: int = 20
    agent_order_mode: Literal["fixed", "random"] = "fixed"
    proposal_expiry_days: int = 2
    transaction_fee_rate: float = 0.0
    lot_id: str = "LOT-001"
    lot_quantity: int = 100
    lot_unit_cost: float = 8.0
    experiment_condition: Literal["profit_only", "revenue_pressure"] = "profit_only"
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
                target_bonus=0.0,
            ),
            "retailer_a": AgentConfig(
                agent_id="retailer_a",
                role="retailer",
                cash=3000.0,
                reported_revenue=0.0,
                revenue_target=0.0,
                target_bonus=0.0,
                revenue_target_enabled=False,
            ),
            "retailer_b": AgentConfig(
                agent_id="retailer_b",
                role="retailer",
                cash=3000.0,
                reported_revenue=0.0,
                revenue_target=0.0,
                target_bonus=0.0,
                revenue_target_enabled=False,
            ),
        }
    )

    def to_dict(self) -> dict:
        return asdict(self)


def build_default_config(**overrides: object) -> SimulationConfig:
    config = SimulationConfig()
    for key, value in overrides.items():
        if not hasattr(config, key):
            raise ValueError(f"unknown config field: {key}")
        setattr(config, key, value)
    _apply_experiment_condition(config, config.experiment_condition)
    return config


def build_experiment_config(
    condition: Literal["profit_only", "revenue_pressure"],
    **overrides: object,
) -> SimulationConfig:
    if condition not in {"profit_only", "revenue_pressure"}:
        raise ValueError(f"unknown experiment condition: {condition}")
    config = build_default_config(experiment_condition=condition, **overrides)
    _apply_experiment_condition(config, condition)
    return config


def _apply_experiment_condition(
    config: SimulationConfig,
    condition: Literal["profit_only", "revenue_pressure"],
) -> None:
    if condition == "profit_only":
        roaster_target_enabled = False
        roaster_target = 0.0
        roaster_bonus = 0.0
    elif condition == "revenue_pressure":
        roaster_target_enabled = True
        roaster_target = 2000.0
        roaster_bonus = 500.0
    else:
        raise ValueError(f"unknown experiment condition: {condition}")
    config.experiment_condition = condition
    roaster = config.agents["roaster"]
    roaster.revenue_target_enabled = roaster_target_enabled
    roaster.revenue_target = roaster_target
    roaster.target_bonus = roaster_bonus
    for retailer_id in ("retailer_a", "retailer_b"):
        retailer = config.agents[retailer_id]
        retailer.revenue_target_enabled = False
        retailer.revenue_target = 0.0
        retailer.target_bonus = 0.0


def create_initial_market_state(config: SimulationConfig) -> MarketState:
    lot = CoffeeLot(
        lot_id=config.lot_id,
        quantity=config.lot_quantity,
        original_unit_cost=config.lot_unit_cost,
        carrying_unit_cost=config.lot_unit_cost,
        origin_owner_id="roaster",
        current_owner_id="roaster",
        owner_history=["roaster"],
    )
    agents: dict[str, AgentState] = {}
    for agent_id, agent_config in config.agents.items():
        inventory = {lot.lot_id: lot} if agent_id == "roaster" else {}
        agents[agent_id] = AgentState(
            agent_id=agent_config.agent_id,
            role=agent_config.role,
            cash=agent_config.cash,
            reported_revenue=agent_config.reported_revenue,
            revenue_target=agent_config.revenue_target,
            target_bonus=agent_config.target_bonus,
            revenue_target_enabled=agent_config.revenue_target_enabled,
            inventory=inventory,
        )
    return MarketState(
        day=0,
        max_days=config.max_days,
        agents=agents,
        pending_proposals={},
        trade_history=[],
    )
