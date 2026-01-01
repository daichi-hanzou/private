"""
Configuration management for Vending-Bench.
Uses Pydantic for validation and YAML for file loading.
"""

from __future__ import annotations

from datetime import date
from pathlib import Path
from typing import Any, Literal

import yaml
from pydantic import BaseModel, Field


class MachineConfig(BaseModel):
    """Vending machine configuration."""

    rows: int = 4
    slots_per_row: int = 3
    small_rows: list[int] = Field(default_factory=lambda: [0, 1])
    large_rows: list[int] = Field(default_factory=lambda: [2, 3])


class EnvironmentConfig(BaseModel):
    """Environment settings."""

    initial_balance: float = 500.0
    daily_fee: float = 2.0
    machine: MachineConfig = Field(default_factory=MachineConfig)
    start_date: str = "2025-01-01"
    start_hour: int = 8


class AgentConfig(BaseModel):
    """Agent settings."""

    max_messages: int = 2000
    context_tokens: int = 30000
    bankruptcy_threshold: int = 10


class DemandConfig(BaseModel):
    """Demand model settings."""

    seed: int = 42
    weekday_multipliers: list[float] = Field(
        default_factory=lambda: [0.8, 0.85, 0.9, 0.95, 1.1, 1.3, 1.2]
    )
    month_multipliers: list[float] = Field(
        default_factory=lambda: [0.7, 0.75, 0.85, 0.95, 1.0, 1.15, 1.2, 1.15, 1.0, 0.95, 0.85, 0.9]
    )
    weather_multipliers: dict[str, float] = Field(
        default_factory=lambda: {"sunny": 1.1, "cloudy": 1.0, "rainy": 0.85}
    )
    optimal_variety: int = 6
    variety_penalty_max: float = 0.5
    noise_std: float = 0.1


class SupplierConfig(BaseModel):
    """Supplier simulation settings."""

    mode: Literal["fixed", "extended"] = "fixed"
    delivery_days_min: int = 2
    delivery_days_max: int = 5
    catalog_file: str | None = None


class LLMConfig(BaseModel):
    """LLM provider settings."""

    provider: Literal["openai", "anthropic", "mock"] = "mock"
    model: str | None = None
    api_key_env: str | None = None


class EmbeddingConfig(BaseModel):
    """Embedding provider settings."""

    provider: Literal["tfidf", "openai"] = "tfidf"
    openai_model: str = "text-embedding-3-small"


class LoggingConfig(BaseModel):
    """Logging settings."""

    trace_file: str = "trace.jsonl"
    metrics_file: str = "metrics.jsonl"
    output_dir: str = "./output"
    verbose: bool = True


class Config(BaseModel):
    """Main configuration for Vending-Bench."""

    environment: EnvironmentConfig = Field(default_factory=EnvironmentConfig)
    agent: AgentConfig = Field(default_factory=AgentConfig)
    tool_time_costs: dict[str, int] = Field(default_factory=dict)
    demand: DemandConfig = Field(default_factory=DemandConfig)
    supplier: SupplierConfig = Field(default_factory=SupplierConfig)
    llm: LLMConfig = Field(default_factory=LLMConfig)
    embedding: EmbeddingConfig = Field(default_factory=EmbeddingConfig)
    logging: LoggingConfig = Field(default_factory=LoggingConfig)

    @classmethod
    def from_yaml(cls, path: str | Path) -> Config:
        """Load configuration from YAML file."""
        path = Path(path)
        with open(path, "r") as f:
            data = yaml.safe_load(f)
        return cls.model_validate(data or {})

    @classmethod
    def default(cls) -> Config:
        """Create default configuration."""
        return cls(
            tool_time_costs={
                "read_emails": 5,
                "read_email": 5,
                "read_email_inbox": 5,
                "send_email": 25,
                "ai_web_search": 75,
                "get_storage_inventory": 5,
                "check_storage_quantities": 5,
                "list_storage_products": 5,
                "get_money_balance": 5,
                "wait_for_next_day": 0,
                "stock_products_from_storage_to_machine": 75,
                "collect_cash_from_machine": 25,
                "set_prices": 25,
                "get_machine_inventory": 5,
                "write_scratchpad": 5,
                "read_scratchpad": 5,
                "set_kv_value": 5,
                "get_kv_value": 5,
                "delete_kv_value": 5,
                "add_to_vector_db": 5,
                "search_vector_db": 5,
                "sub_agent_specs": 5,
                "run_sub_agent": 300,
                "chat_with_sub_agent": 25,
            }
        )

    def get_tool_time_cost(self, tool_name: str) -> int:
        """Get time cost for a tool in minutes."""
        return self.tool_time_costs.get(tool_name, 5)

    def get_start_date(self) -> date:
        """Parse start date string to date object."""
        return date.fromisoformat(self.environment.start_date)
