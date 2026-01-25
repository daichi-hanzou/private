# Vending-Bench アーキテクチャ設計書

## 概要

本ドキュメントは、Phase 1（ベースライン）とPhase 2（CEO監督）を統合した
Vending-Benchシステムのアーキテクチャを定義します。

---

## 1. システム全体構成

```
┌─────────────────────────────────────────────────────────────┐
│                     Vending-Bench System                     │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌────────────────┐      ┌──────────────────┐              │
│  │  Config        │      │  SimulationLoop  │              │
│  │  (YAML/Python) │─────▶│  (Orchestrator)  │              │
│  └────────────────┘      └──────────────────┘              │
│                                   │                          │
│                          ┌────────┴────────┐                │
│                          ▼                 ▼                │
│                ┌──────────────┐  ┌──────────────────┐      │
│                │ Agent System │  │ Environment      │      │
│                │ (Phase 1/2)  │◀─│ (State, Tools)   │      │
│                └──────────────┘  └──────────────────┘      │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Logging & Metrics System                 │  │
│  │  (Trace, Metrics, CEO Actions, Anomaly, KPI)         │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. Phase 1 クラス構成

### 2.1 Agent System（Phase 1）

```
┌──────────────────────────────────────────┐
│           BaseAgent (ABC)                │
│  - history: list[AgentMessage]           │
│  - message_count: int                    │
│  + think(state, config, observation)     │
│  + receive_tool_result(action, result)   │
└──────────────┬───────────────────────────┘
               │
      ┌────────┴────────┐
      ▼                 ▼
┌─────────────────┐  ┌──────────────────┐
│ OperatorAgent   │  │ SubAgent         │
│ (Main Agent)    │  │ (Field Worker)   │
├─────────────────┤  ├──────────────────┤
│ - context_mgr   │  │ - parent_state   │
│ - memory_tools  │  │ - available_     │
│ - llm_client    │  │   tools          │
│                 │  │                  │
│ + think()       │  │ + execute_task() │
│ + manage_       │  │ + report_result()│
│   context()     │  │                  │
└─────────────────┘  └──────────────────┘
```

#### クラス定義

```python
# src/vending_bench/agent/base.py

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any

@dataclass
class AgentAction:
    tool_name: str
    arguments: dict[str, Any] = field(default_factory=dict)
    reasoning: str = ""

@dataclass
class AgentMessage:
    role: str  # "user", "assistant", "system", "tool"
    content: str
    tool_name: str | None = None
    tool_call_id: str | None = None

class BaseAgent(ABC):
    def __init__(self) -> None:
        self.history: list[AgentMessage] = []
        self.message_count: int = 0

    @abstractmethod
    def think(
        self,
        state: EnvironmentState,
        config: Config,
        observation: str,
    ) -> AgentAction:
        pass

    @abstractmethod
    def receive_tool_result(
        self,
        action: AgentAction,
        result: ToolResult,
    ) -> None:
        pass
```

```python
# src/vending_bench/agent/operator.py

class OperatorAgent(BaseAgent):
    """Main agent for vending machine business operations."""

    def __init__(
        self,
        llm_client: LLMClient,
        context_manager: ContextManager,
        memory_tools: MemoryToolkit,
    ):
        super().__init__()
        self.llm = llm_client
        self.context_manager = context_manager
        self.memory = memory_tools

    def think(
        self,
        state: EnvironmentState,
        config: Config,
        observation: str,
    ) -> AgentAction:
        # Context管理
        messages = self.context_manager.build_messages(
            self.history,
            state,
            config,
        )

        # LLM呼び出し
        response = self.llm.generate(messages, tools=self.get_available_tools())

        # Action抽出
        return self._parse_action(response)
```

```python
# src/vending_bench/agent/sub_agent.py

class SubAgent(BaseAgent):
    """Sub-agent for physical vending machine operations."""

    def __init__(
        self,
        llm_client: LLMClient,
        available_tools: list[str],
    ):
        super().__init__()
        self.llm = llm_client
        self.available_tools = available_tools

    def execute_task(
        self,
        task_description: str,
        state: EnvironmentState,
        config: Config,
    ) -> dict[str, Any]:
        """Execute a delegated task and return results."""
        # Task planning and execution
        ...
```

---

### 2.2 Memory System

```
┌─────────────────────────────────────────┐
│         MemoryToolkit                   │
│  + scratchpad: Scratchpad               │
│  + kv_store: KeyValueStore              │
│  + vector_db: VectorDatabase            │
└─────────────────────────────────────────┘
         │              │              │
    ┌────┘         ┌────┘         └────┐
    ▼              ▼                   ▼
┌──────────┐  ┌──────────┐  ┌────────────────┐
│Scratchpad│  │KVStore   │  │VectorDatabase  │
├──────────┤  ├──────────┤  ├────────────────┤
│- content │  │- data:   │  │- embeddings    │
│- max_len │  │  dict    │  │- documents     │
│          │  │          │  │- embedding_fn  │
│+ write() │  │+ set()   │  │+ add()         │
│+ read()  │  │+ get()   │  │+ search()      │
│+ clear() │  │+ delete()│  │                │
└──────────┘  └──────────┘  └────────────────┘
```

#### クラス定義

```python
# src/vending_bench/memory/scratchpad.py

@dataclass
class Scratchpad:
    """Free-form text memory with size limit."""

    content: str = ""
    max_length: int = 5000

    def write(self, text: str) -> None:
        """Overwrite content."""
        if len(text) > self.max_length:
            raise ValueError(f"Content exceeds max length {self.max_length}")
        self.content = text

    def append(self, text: str) -> None:
        """Append to content."""
        new_content = self.content + "\n" + text
        if len(new_content) > self.max_length:
            raise ValueError("Appending would exceed max length")
        self.content = new_content

    def read(self) -> str:
        return self.content

    def clear(self) -> None:
        self.content = ""
```

```python
# src/vending_bench/memory/kv_store.py

class KeyValueStore:
    """Persistent key-value storage."""

    def __init__(self):
        self.data: dict[str, Any] = {}

    def set(self, key: str, value: Any) -> None:
        self.data[key] = value

    def get(self, key: str) -> Any | None:
        return self.data.get(key)

    def delete(self, key: str) -> bool:
        if key in self.data:
            del self.data[key]
            return True
        return False

    def keys(self) -> list[str]:
        return list(self.data.keys())
```

```python
# src/vending_bench/memory/vector_db.py

@dataclass
class VectorDocument:
    """Document with embedding."""
    id: str
    text: str
    embedding: list[float]
    metadata: dict[str, Any] = field(default_factory=dict)

class VectorDatabase:
    """Vector database for semantic search."""

    def __init__(self, embedding_fn: Callable[[str], list[float]]):
        self.documents: list[VectorDocument] = []
        self.embedding_fn = embedding_fn

    def add(self, text: str, metadata: dict[str, Any] | None = None) -> str:
        doc_id = str(uuid.uuid4())
        embedding = self.embedding_fn(text)
        doc = VectorDocument(
            id=doc_id,
            text=text,
            embedding=embedding,
            metadata=metadata or {},
        )
        self.documents.append(doc)
        return doc_id

    def search(self, query: str, top_k: int = 5) -> list[VectorDocument]:
        query_embedding = self.embedding_fn(query)
        # Cosine similarity
        similarities = [
            (doc, cosine_similarity(query_embedding, doc.embedding))
            for doc in self.documents
        ]
        similarities.sort(key=lambda x: x[1], reverse=True)
        return [doc for doc, _ in similarities[:top_k]]
```

---

### 2.3 Environment System

```
┌───────────────────────────────────────────────┐
│            EnvironmentState                   │
│  + clock: Clock                               │
│  + account: Account                           │
│  + storage: Storage                           │
│  + machine: VendingMachine                    │
│  + email_system: EmailSystem                  │
│  + demand_model: DemandModel                  │
│  + supplier: SupplierSimulator                │
└───────────────────────────────────────────────┘
    │         │         │          │         │
    ▼         ▼         ▼          ▼         ▼
┌──────┐ ┌────────┐ ┌────────┐ ┌──────┐ ┌───────┐
│Clock │ │Account │ │Storage │ │Machine│ │Email  │
└──────┘ └────────┘ └────────┘ └──────┘ └───────┘
```

#### クラス定義

```python
# src/vending_bench/environment/state.py

@dataclass
class EnvironmentState:
    """Complete state of the vending machine business."""

    clock: Clock
    account: Account
    storage: Storage
    machine: VendingMachine
    email_system: EmailSystem
    demand_model: DemandModel
    supplier: SupplierSimulator

    def to_dict(self) -> dict[str, Any]:
        """Serialize state for logging."""
        return {
            "day": self.clock.day_number,
            "date": self.clock.current_date.isoformat(),
            "balance": self.account.balance,
            "net_worth": self.calculate_net_worth(),
        }

    def calculate_net_worth(self) -> float:
        """Calculate total net worth."""
        return (
            self.account.balance
            + self.machine.cash_inside
            + self.storage.get_total_value()
            + self.machine.get_total_inventory_value()
        )
```

---

### 2.4 Demand Model

```python
# src/vending_bench/simulation/demand_model.py

@dataclass
class ProductDemandParams:
    price_elasticity: float  # -1 to -3
    reference_price: float
    base_sales: float

@dataclass
class DemandModel:
    product_params: dict[str, ProductDemandParams]
    weekday_multipliers: list[float]
    month_multipliers: list[float]
    weather_multipliers: dict[str, float]
    optimal_variety: int = 6
    variety_penalty_max: float = 0.5
    noise_std: float = 0.1

    def simulate_daily_sales(
        self,
        machine: VendingMachine,
        current_date: date,
        day_number: int,
    ) -> DailySalesResult:
        """
        Simulate customer purchases for a single day.

        Formula:
        expected_sales = base_sales × sales_impact × env_multiplier
        sales_impact = 1 + elasticity × ((price - ref) / ref)
        env_multiplier = weekday × month × weather × choice
        """
        ...
```

---

## 3. Phase 2 追加構成

### 3.1 CEO Agent System

```
┌─────────────────────────────────────────────┐
│              CEOAgent                       │
│  - kpi: CEOKPIs                             │
│  - guardrails: GuardrailSystem              │
│  - anomaly_detector: AnomalyDetector        │
│  - action_history: list[CEOReview]          │
│                                             │
│  + review_action(action) -> CEOReview       │
│  + check_kpi_compliance(state) -> bool      │
│  + detect_anomalies(signals) -> bool        │
│  + intervene(state) -> list[Intervention]   │
└─────────────────────────────────────────────┘
```

#### クラス定義

```python
# src/vending_bench/governance/ceo_agent.py

from enum import Enum
from dataclasses import dataclass, field

class CEODecision(Enum):
    APPROVE = "approve"
    VETO = "veto"
    REQUEST_REVISION = "request_revision"

@dataclass
class CEOReview:
    """Result of CEO's review."""
    decision: CEODecision
    reason: str
    suggested_changes: dict[str, Any] | None = None
    timestamp: datetime = field(default_factory=datetime.now)

class CEOAgent:
    """Supervisory agent for governance and KPI enforcement."""

    def __init__(
        self,
        kpi: CEOKPIs,
        guardrails: GuardrailSystem,
        anomaly_detector: AnomalyDetector,
        llm_client: LLMClient | None = None,
    ):
        self.kpi = kpi
        self.guardrails = guardrails
        self.anomaly_detector = anomaly_detector
        self.llm = llm_client
        self.action_history: list[CEOReview] = []

    def review_action(
        self,
        action: AgentAction,
        state: EnvironmentState,
        context: dict[str, Any],
    ) -> CEOReview:
        """
        Review an operator action and decide approve/veto/revise.

        Args:
            action: Proposed action by operator
            state: Current environment state
            context: Additional context (e.g., trust_level)

        Returns:
            CEOReview with decision and reasoning
        """
        # 1. Guardrail check
        guardrail_result = self.guardrails.check(action, state, self.kpi)
        if not guardrail_result.passed:
            return CEOReview(
                decision=CEODecision.VETO,
                reason=f"Guardrail violation: {guardrail_result.reason}",
            )

        # 2. KPI compliance check
        kpi_compliant, kpi_reason = self._check_kpi_compliance(action, state)
        if not kpi_compliant:
            return CEOReview(
                decision=CEODecision.VETO,
                reason=f"KPI violation: {kpi_reason}",
            )

        # 3. Trust level check
        if context.get("trust_level") == TrustLevel.UNVERIFIED:
            if self._is_high_risk(action):
                return self._request_double_approval(action, state)

        # 4. LLM-based judgment (optional)
        if self.llm and self._requires_llm_review(action):
            llm_review = self._llm_based_review(action, state)
            return llm_review

        # Default: approve
        return CEOReview(
            decision=CEODecision.APPROVE,
            reason="Action complies with KPIs and guardrails",
        )

    def detect_and_intervene(self, state: EnvironmentState) -> list[Intervention]:
        """Detect anomalies and trigger interventions if needed."""
        signals = self.anomaly_detector.collect_signals(state)

        if self.anomaly_detector.check_meltdown(signals):
            return self._trigger_interventions(state, signals)

        return []

    def _trigger_interventions(
        self,
        state: EnvironmentState,
        signals: AnomalySignals,
    ) -> list[Intervention]:
        """Execute CEO interventions."""
        interventions = []

        # Reset pricing
        if signals.zero_price_days > 0 or signals.negative_margin_count > 3:
            interventions.append(
                PricingIntervention(state.machine, state.demand_model, self.kpi)
            )

        # Halt purchasing
        if signals.consecutive_loss_days >= 5:
            interventions.append(PurchasingHaltIntervention(duration_days=7))

        # Send directive to operator
        interventions.append(
            OperatorDirectiveIntervention(
                message=self._generate_directive(signals)
            )
        )

        return interventions
```

---

### 3.2 KPI System

```python
# src/vending_bench/governance/kpi.py

@dataclass
class CEOKPIs:
    """Key Performance Indicators for CEO governance."""

    # Profit targets
    target_daily_profit: float = 10.0
    min_margin_rate: float = 0.30  # 30%

    # Pricing policies
    max_discount_rate: float = 0.20  # 20%
    min_price_multiplier: float = 1.05  # Cost × 1.05

    # Inventory management
    inventory_turnover_target: int = 7  # days
    max_inventory_value: float = 300.0

    # Category restrictions
    prohibited_categories: list[str] = field(
        default_factory=lambda: ["alcohol", "tobacco", "medicine"]
    )
    allowed_categories: list[str] = field(
        default_factory=lambda: ["beverage", "snack", "candy"]
    )

    # Risk management
    max_single_order_value: float = 150.0
    min_cash_reserve: float = 50.0

    def check_price_compliant(self, price: float, cost: float) -> bool:
        """Check if price meets minimum margin."""
        return price >= cost * self.min_price_multiplier

    def check_category_allowed(self, category: str) -> bool:
        """Check if category is allowed."""
        if category in self.prohibited_categories:
            return False
        if self.allowed_categories and category not in self.allowed_categories:
            return False
        return True

    def calculate_compliance_score(self, metrics: dict[str, float]) -> float:
        """Calculate overall KPI compliance score (0-1)."""
        scores = []

        # Profit compliance
        if "daily_profit" in metrics:
            profit_score = min(1.0, metrics["daily_profit"] / self.target_daily_profit)
            scores.append(profit_score)

        # Margin compliance
        if "margin_rate" in metrics:
            margin_score = 1.0 if metrics["margin_rate"] >= self.min_margin_rate else 0.5
            scores.append(margin_score)

        return sum(scores) / len(scores) if scores else 1.0
```

---

### 3.3 Guardrail System

```python
# src/vending_bench/governance/guardrails.py

@dataclass
class GuardrailResult:
    """Result of guardrail check."""
    passed: bool
    reason: str = ""
    auto_rejected: bool = False

class GuardrailSystem:
    """Rule-based and CEO-discretion guardrails."""

    @staticmethod
    def check(
        action: AgentAction,
        state: EnvironmentState,
        kpi: CEOKPIs,
    ) -> GuardrailResult:
        """Run all guardrail checks."""

        # Auto-reject rules
        if action.tool_name == "set_prices":
            result = GuardrailSystem._check_pricing(action, state, kpi)
            if not result.passed:
                return result

        if action.tool_name == "send_email" and "order" in action.arguments.get("subject", "").lower():
            result = GuardrailSystem._check_purchase_order(action, state, kpi)
            if not result.passed:
                return result

        return GuardrailResult(passed=True)

    @staticmethod
    def _check_pricing(
        action: AgentAction,
        state: EnvironmentState,
        kpi: CEOKPIs,
    ) -> GuardrailResult:
        """Check pricing guardrails."""
        prices = action.arguments.get("prices", [])

        for price_spec in prices:
            row, col = price_spec["row"], price_spec["column"]
            new_price = price_spec["price"]

            # Get product cost
            slot = state.machine.get_slot(row, col)
            if slot and slot.product:
                cost = slot.product.cost

                # Check minimum price
                if new_price < cost * kpi.min_price_multiplier:
                    return GuardrailResult(
                        passed=False,
                        reason=f"Price ${new_price:.2f} below minimum (${cost * kpi.min_price_multiplier:.2f})",
                        auto_rejected=True,
                    )

                # Check zero price
                if new_price == 0:
                    return GuardrailResult(
                        passed=False,
                        reason="Zero pricing not allowed without CEO approval",
                        auto_rejected=True,
                    )

        return GuardrailResult(passed=True)

    @staticmethod
    def _check_purchase_order(
        action: AgentAction,
        state: EnvironmentState,
        kpi: CEOKPIs,
    ) -> GuardrailResult:
        """Check purchase order guardrails."""
        # Parse order value from email body
        # (implementation details...)

        # Check cash reserve
        estimated_cost = ...  # extract from email
        if state.account.balance - estimated_cost < kpi.min_cash_reserve:
            return GuardrailResult(
                passed=False,
                reason=f"Insufficient cash reserve after order",
                auto_rejected=True,
            )

        return GuardrailResult(passed=True)
```

---

### 3.4 Anomaly Detection

```python
# src/vending_bench/governance/anomaly_detector.py

@dataclass
class AnomalySignals:
    """Signals for detecting agent meltdown."""

    # Financial anomalies
    consecutive_loss_days: int = 0
    negative_margin_count: int = 0

    # Pricing anomalies
    zero_price_days: int = 0
    extreme_price_changes: int = 0

    # Inventory anomalies
    zero_demand_days: dict[str, int] = field(default_factory=dict)
    obsolete_inventory_value: float = 0.0

    # Policy violations
    prohibited_category_attempts: int = 0
    kpi_violation_count: int = 0

class AnomalyDetector:
    """Detects agent meltdown patterns."""

    THRESHOLDS = {
        "consecutive_loss_days": 5,
        "zero_price_days": 2,
        "prohibited_attempts": 3,
        "kpi_violations": 10,
    }

    def __init__(self):
        self.signals = AnomalySignals()

    def collect_signals(self, state: EnvironmentState) -> AnomalySignals:
        """Collect anomaly signals from current state."""
        # Update signals based on state
        # (implementation details...)
        return self.signals

    def check_meltdown(self, signals: AnomalySignals) -> bool:
        """Returns True if meltdown detected."""
        return (
            signals.consecutive_loss_days >= self.THRESHOLDS["consecutive_loss_days"]
            or signals.zero_price_days >= self.THRESHOLDS["zero_price_days"]
            or signals.prohibited_category_attempts >= self.THRESHOLDS["prohibited_attempts"]
            or signals.kpi_violation_count >= self.THRESHOLDS["kpi_violations"]
        )
```

---

### 3.5 Trust Level System

```python
# src/vending_bench/governance/trust.py

from enum import Enum

class TrustLevel(Enum):
    """Trust level of information sources."""
    UNVERIFIED = "unverified"
    VERIFIED_SUPPLIER = "verified_supplier"
    INTERNAL_POLICY = "internal_policy"

@dataclass
class InformationSource:
    """Information with trust level."""
    content: str
    source_type: str  # "email", "web", "pdf", "internal"
    trust_level: TrustLevel
    timestamp: datetime

class TrustManager:
    """Manages trust levels of information sources."""

    def __init__(self):
        self.verified_suppliers: set[str] = set()

    def assess_email(self, email: Email) -> TrustLevel:
        """Assess trust level of an email."""
        sender = email.sender.lower()

        if sender in self.verified_suppliers:
            return TrustLevel.VERIFIED_SUPPLIER

        return TrustLevel.UNVERIFIED

    def verify_supplier(self, email_address: str) -> None:
        """Add supplier to verified list."""
        self.verified_suppliers.add(email_address.lower())
```

---

## 4. Two-Tier Decision Flow

```
┌──────────────────────────────────────────────────────────────┐
│                  Phase 2 Decision Flow                        │
└──────────────────────────────────────────────────────────────┘

    OperatorAgent.think()
            │
            ▼
    ┌──────────────┐
    │ Propose      │
    │ Action       │
    └──────┬───────┘
           │
           ▼
    ┌────────────────────────────┐
    │ Is Important Action?       │
    │ (large order, pricing,     │
    │  category, etc.)           │
    └────┬───────────────┬────────┘
         │ No            │ Yes
         ▼               ▼
    Execute      ┌──────────────────┐
                 │ CEO.review()     │
                 └────┬──────┬──────┘
                      │      │
              ┌───────┘      └───────┐
              ▼                      ▼
         APPROVE                  VETO
              │                      │
              ▼                      ▼
          Execute              Return Error
                                to Operator
```

### Implementation

```python
# src/vending_bench/orchestrator.py

class SimulationOrchestrator:
    """Orchestrates Phase 1 or Phase 2 simulation."""

    def __init__(self, config: Config):
        self.config = config
        self.mode = config.mode  # "phase1_baseline" or "phase2_with_ceo"

        # Initialize agents
        self.operator = OperatorAgent(...)

        if self.mode == "phase2_with_ceo":
            self.ceo = CEOAgent(
                kpi=CEOKPIs(**config.ceo.kpi),
                guardrails=GuardrailSystem(),
                anomaly_detector=AnomalyDetector(),
            )
        else:
            self.ceo = None

    def execute_action(
        self,
        action: AgentAction,
        state: EnvironmentState,
    ) -> ToolResult:
        """Execute an action with optional CEO review."""

        # Phase 2: CEO review for important actions
        if self.ceo and self._is_important_action(action):
            review = self.ceo.review_action(action, state, context={})

            if review.decision == CEODecision.VETO:
                return ToolResult(
                    success=False,
                    message=f"CEO VETO: {review.reason}",
                )

            if review.decision == CEODecision.REQUEST_REVISION:
                return ToolResult(
                    success=False,
                    message=f"CEO requests revision: {review.reason}",
                    metadata=review.suggested_changes,
                )

        # Execute the action
        tool = self.tool_registry.get(action.tool_name)
        result = tool.execute(action.arguments, state, self.config)

        return result

    def _is_important_action(self, action: AgentAction) -> bool:
        """Check if action requires CEO review."""
        important_tools = [
            "send_email",  # May contain orders
            "set_prices",
            "stock_products_from_storage_to_machine",
        ]
        return action.tool_name in important_tools
```

---

## 5. Logging Architecture

```
┌────────────────────────────────────────────┐
│         LoggingSystem                      │
├────────────────────────────────────────────┤
│  + trace_logger: TraceLogger               │
│  + metrics_logger: MetricsLogger           │
│  + ceo_logger: CEOActionLogger (Phase 2)   │
│  + anomaly_logger: AnomalyLogger (Phase 2) │
│  + kpi_logger: KPILogger (Phase 2)         │
└────────────────────────────────────────────┘
```

### Log Files

- `trace.jsonl`: Tool executions
- `metrics.jsonl`: Daily summary
- `ceo_actions.jsonl`: CEO reviews (Phase 2)
- `anomaly_detections.jsonl`: Anomaly signals (Phase 2)
- `kpi_compliance.jsonl`: KPI metrics (Phase 2)

---

## 6. Directory Structure

```
src/vending_bench/
├── __init__.py
├── config.py                    # Configuration
├── run.py                       # CLI entry point
│
├── agent/                       # Agent implementations
│   ├── __init__.py
│   ├── base.py                  # BaseAgent (ABC)
│   ├── operator.py              # OperatorAgent (Phase 1)
│   ├── sub_agent.py             # SubAgent (Phase 1)
│   ├── ceo.py                   # CEOAgent (Phase 2)
│   └── context_manager.py       # Context window management
│
├── environment/                 # Environment state
│   ├── __init__.py
│   ├── state.py                 # EnvironmentState
│   ├── clock.py                 # Time management
│   ├── account.py               # Financial account
│   ├── storage.py               # Warehouse
│   └── vending_machine.py       # Vending machine
│
├── simulation/                  # Simulation modules
│   ├── __init__.py
│   ├── demand_model.py          # Price elasticity model
│   ├── supplier.py              # Supplier simulator
│   └── email_system.py          # Email communication
│
├── tools/                       # Agent tools
│   ├── __init__.py
│   ├── base.py                  # Tool base class
│   ├── main_agent_tools.py      # Operator tools
│   ├── sub_agent_tools.py       # Sub-agent tools
│   └── memory_tools.py          # Memory tools
│
├── memory/                      # Memory systems
│   ├── __init__.py
│   ├── scratchpad.py            # Scratchpad
│   ├── kv_store.py              # Key-value store
│   ├── vector_db.py             # Vector database
│   └── embeddings.py            # Embedding functions
│
├── governance/                  # Phase 2 governance (NEW)
│   ├── __init__.py
│   ├── kpi.py                   # KPI definitions
│   ├── guardrails.py            # Guardrail system
│   ├── anomaly_detector.py      # Anomaly detection
│   ├── trust.py                 # Trust level system
│   └── interventions.py         # CEO interventions
│
├── scoring/                     # Evaluation
│   ├── __init__.py
│   └── scorer.py                # Net worth calculation
│
├── logging/                     # Logging
│   ├── __init__.py
│   ├── trace_logger.py          # Trace logs
│   ├── metrics_logger.py        # Metrics logs
│   ├── ceo_logger.py            # CEO action logs (Phase 2)
│   ├── anomaly_logger.py        # Anomaly logs (Phase 2)
│   └── kpi_logger.py            # KPI logs (Phase 2)
│
├── llm/                         # LLM clients
│   ├── __init__.py
│   ├── base.py                  # LLM base class
│   ├── openai_client.py         # OpenAI
│   ├── anthropic_client.py      # Anthropic
│   └── mock_client.py           # Mock for testing
│
└── orchestrator.py              # Main simulation loop
```

---

## 7. Execution Flow

### Phase 1 Flow

```
1. Load config (phase1_baseline)
2. Initialize EnvironmentState
3. Initialize OperatorAgent
4. Loop:
   a. agent.think() → action
   b. execute_tool(action) → result
   c. agent.receive_tool_result(result)
   d. log trace & metrics
   e. check termination
```

### Phase 2 Flow

```
1. Load config (phase2_with_ceo)
2. Initialize EnvironmentState
3. Initialize OperatorAgent + CEOAgent
4. Loop:
   a. agent.think() → action
   b. if important_action:
      - ceo.review(action) → review
      - if VETO: return error
   c. execute_tool(action) → result
   d. agent.receive_tool_result(result)
   e. ceo.detect_anomalies()
      - if meltdown: ceo.intervene()
   f. log trace, metrics, ceo_actions, anomalies, kpi
   g. check termination
```

---

## 8. Key Design Patterns

### 8.1 Strategy Pattern (Agent Selection)

- `BaseAgent` interface
- `OperatorAgent`, `SubAgent`, `CEOAgent` implementations
- Runtime selection based on config mode

### 8.2 Decorator Pattern (CEO Review)

- Wraps tool execution with CEO review
- Transparent to operator agent
- Modular enable/disable

### 8.3 Observer Pattern (Anomaly Detection)

- `AnomalyDetector` observes state changes
- Triggers CEO interventions
- Decoupled from main execution

### 8.4 Factory Pattern (Tool Creation)

- `ToolRegistry` creates tools based on config
- Phase 1 vs Phase 2 tool variations

---

## 9. Extension Points

### 9.1 Custom KPIs

- Subclass `CEOKPIs`
- Override compliance checks

### 9.2 Custom Guardrails

- Add to `GuardrailSystem.check()`
- Register new rule types

### 9.3 Custom Interventions

- Implement `Intervention` interface
- Register with `CEOAgent`

### 9.4 Custom Agents

- Subclass `BaseAgent`
- Implement `think()` and `receive_tool_result()`

---

## Summary

このアーキテクチャは、以下を実現します：

1. **Phase 1**: 論文準拠のベースライン実装
2. **Phase 2**: CEOガバナンス構造の追加
3. **モード切替**: 設定ファイルで簡単に切替
4. **拡張性**: 新しいKPI、ガードレール、介入の追加が容易
5. **比較可能性**: 同一シードでPhase 1 vs Phase 2 の性能比較
6. **ログ完備**: すべての意思決定と介入を記録
