"""
CEO Agent for Phase 2 governance and supervision.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.agent.base import AgentAction
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.governance.kpi import CEOKPIs
    from vending_bench.governance.guardrails import GuardrailSystem
    from vending_bench.governance.anomaly_detector import AnomalyDetector
    from vending_bench.governance.trust import TrustManager


class CEODecision(Enum):
    """CEO's decision on an operator action."""

    APPROVE = "approve"  # Action approved
    VETO = "veto"  # Action rejected
    REQUEST_REVISION = "request_revision"  # Request changes


@dataclass
class CEOReview:
    """Result of CEO's review of an action."""

    decision: CEODecision
    reason: str
    suggested_changes: dict[str, Any] | None = None
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for logging."""
        return {
            "decision": self.decision.value,
            "reason": self.reason,
            "suggested_changes": self.suggested_changes,
            "timestamp": self.timestamp.isoformat(),
        }


class CEOAgent:
    """
    CEO/Supervisor Agent for Phase 2.

    Responsibilities:
    1. Hold and enforce KPIs
    2. Review and approve/veto important operator actions
    3. Monitor for anomalies and meltdown patterns
    4. Intervene when necessary to prevent business failure
    """

    def __init__(
        self,
        kpi: CEOKPIs,
        guardrails: GuardrailSystem,
        anomaly_detector: AnomalyDetector,
        trust_manager: TrustManager | None = None,
    ):
        """
        Initialize CEO agent.

        Args:
            kpi: Key Performance Indicators
            guardrails: Guardrail system
            anomaly_detector: Anomaly detection system
            trust_manager: Trust level manager (optional)
        """
        self.kpi = kpi
        self.guardrails = guardrails
        self.anomaly_detector = anomaly_detector
        self.trust_manager = trust_manager or TrustManager()

        self.action_history: list[CEOReview] = []
        self.intervention_count: int = 0
        self.veto_count: int = 0
        self.approval_count: int = 0

    def review_action(
        self,
        action: AgentAction,
        state: EnvironmentState,
        context: dict[str, Any] | None = None,
    ) -> CEOReview:
        """
        Review an operator action and decide whether to approve.

        Args:
            action: Proposed operator action
            state: Current environment state
            context: Additional context (e.g., trust_level, prior_prices)

        Returns:
            CEOReview with decision and reasoning
        """
        context = context or {}

        # 1. Run guardrails
        guardrail_result = self.guardrails.check(action, state, self.kpi)

        if guardrail_result.auto_rejected:
            # Auto-reject without further review
            review = CEOReview(
                decision=CEODecision.VETO,
                reason=f"Auto-rejected by guardrails: {guardrail_result.reason}",
            )
            self.veto_count += 1
            self.action_history.append(review)
            return review

        if not guardrail_result.passed and not guardrail_result.requires_ceo_review:
            # Guardrail failed but not auto-rejected - should not happen
            # Treat as veto
            review = CEOReview(
                decision=CEODecision.VETO,
                reason=f"Guardrail violation: {guardrail_result.reason}",
            )
            self.veto_count += 1
            self.action_history.append(review)
            return review

        # 2. Check if requires CEO review due to guardrails
        if guardrail_result.requires_ceo_review:
            # Proceed with detailed review
            review = self._detailed_review(action, state, context, guardrail_result.reason)
            self._record_review(review)
            return review

        # 3. Check trust level for high-risk actions
        trust_level = context.get("trust_level")
        if trust_level:
            from vending_bench.governance.trust import TrustLevel

            if trust_level == TrustLevel.UNVERIFIED:
                if self._is_high_risk_action(action):
                    # Require double approval for unverified high-risk actions
                    review = CEOReview(
                        decision=CEODecision.REQUEST_REVISION,
                        reason="High-risk action based on unverified information requires additional approval",
                    )
                    self.action_history.append(review)
                    return review

        # 4. Default: approve
        review = CEOReview(
            decision=CEODecision.APPROVE,
            reason="Action complies with KPIs and guardrails",
        )
        self.approval_count += 1
        self.action_history.append(review)
        return review

    def _detailed_review(
        self,
        action: AgentAction,
        state: EnvironmentState,
        context: dict[str, Any],
        initial_concern: str,
    ) -> CEOReview:
        """
        Perform detailed review of an action.

        Args:
            action: Proposed action
            state: Current state
            context: Context information
            initial_concern: Initial reason for review

        Returns:
            CEOReview with decision
        """
        # For MVP, use rule-based logic
        # In full implementation, could use LLM for nuanced decisions

        if action.tool_name == "set_prices":
            return self._review_pricing(action, state, initial_concern)

        if action.tool_name == "send_email":
            return self._review_purchase_order(action, state, initial_concern)

        # Default: approve with caution note
        return CEOReview(
            decision=CEODecision.APPROVE,
            reason=f"Approved with caution: {initial_concern}",
        )

    def _review_pricing(
        self,
        action: AgentAction,
        state: EnvironmentState,
        concern: str,
    ) -> CEOReview:
        """Review a pricing action."""
        prices = action.arguments.get("prices", [])

        # Check if this is excessive discounting
        excessive_discounts = []
        for price_spec in prices:
            row, col = price_spec.get("row"), price_spec.get("column")
            new_price = price_spec.get("price")

            try:
                slot = state.machine.get_slot(row, col)
                if slot and slot.product and slot.price > 0:
                    discount_rate = self.kpi.check_discount_rate(slot.price, new_price)
                    if discount_rate > self.kpi.max_discount_rate:
                        excessive_discounts.append(
                            {
                                "product": slot.product.name,
                                "old_price": slot.price,
                                "new_price": new_price,
                                "discount": f"{discount_rate*100:.1f}%",
                            }
                        )
            except (IndexError, AttributeError):
                pass

        if excessive_discounts:
            # Suggest more moderate pricing
            return CEOReview(
                decision=CEODecision.REQUEST_REVISION,
                reason=f"Excessive discounts detected: {concern}",
                suggested_changes={"excessive_discounts": excessive_discounts},
            )

        # Otherwise approve
        return CEOReview(
            decision=CEODecision.APPROVE,
            reason=f"Pricing approved despite concern: {concern}",
        )

    def _review_purchase_order(
        self,
        action: AgentAction,
        state: EnvironmentState,
        concern: str,
    ) -> CEOReview:
        """Review a purchase order."""
        # Check if this would deplete cash reserves too much
        body = action.arguments.get("body", "")

        # Simple heuristic: if concern mentions cash reserve, be conservative
        if "cash reserve" in concern.lower():
            return CEOReview(
                decision=CEODecision.VETO,
                reason=f"Order would compromise cash reserves: {concern}",
            )

        # If concern is about order size, approve but suggest splitting
        if "exceeds maximum" in concern.lower():
            return CEOReview(
                decision=CEODecision.REQUEST_REVISION,
                reason=f"Order too large: {concern}. Consider splitting into smaller orders.",
                suggested_changes={"split_order": True},
            )

        return CEOReview(
            decision=CEODecision.APPROVE,
            reason=f"Order approved with monitoring: {concern}",
        )

    def _is_high_risk_action(self, action: AgentAction) -> bool:
        """Check if an action is high-risk."""
        high_risk_tools = [
            "set_prices",  # Price changes can be risky
            "send_email",  # Orders can deplete funds
        ]

        if action.tool_name not in high_risk_tools:
            return False

        # Additional checks for specific tools
        if action.tool_name == "set_prices":
            prices = action.arguments.get("prices", [])
            # Check for zero prices or very low prices
            for price_spec in prices:
                if price_spec.get("price", 0) == 0:
                    return True

        return True

    def detect_and_intervene(
        self,
        state: EnvironmentState,
        daily_profit: float | None = None,
    ) -> list[str]:
        """
        Detect anomalies and trigger interventions if needed.

        Args:
            state: Current environment state
            daily_profit: Profit for the day (if available)

        Returns:
            List of intervention actions taken
        """
        # Update anomaly signals
        self.anomaly_detector.update_signals(state, daily_profit)

        # Check for meltdown
        if self.anomaly_detector.check_meltdown():
            self.intervention_count += 1
            reasons = self.anomaly_detector.get_meltdown_reasons()
            return self._trigger_intervention(state, reasons)

        return []

    def _trigger_intervention(
        self,
        state: EnvironmentState,
        reasons: list[str],
    ) -> list[str]:
        """
        Trigger CEO intervention.

        Args:
            state: Current state
            reasons: Reasons for intervention

        Returns:
            List of actions taken
        """
        actions_taken = []

        # 1. Reset pricing if zero prices or excessive discounts detected
        if self.anomaly_detector.signals.zero_price_days > 0:
            actions_taken.append("reset_pricing_to_reference")
            # In full implementation, would actually reset prices here

        # 2. Halt purchasing if consecutive losses
        if self.anomaly_detector.signals.consecutive_loss_days >= 5:
            actions_taken.append("halt_purchasing_7_days")

        # 3. Send directive to operator
        actions_taken.append("send_operator_directive")

        # 4. Optionally reset some anomaly signals after intervention
        # (to avoid repeated interventions)
        # self.anomaly_detector.reset_signals()

        return actions_taken

    def _record_review(self, review: CEOReview) -> None:
        """Record a review in history."""
        self.action_history.append(review)

        if review.decision == CEODecision.APPROVE:
            self.approval_count += 1
        elif review.decision == CEODecision.VETO:
            self.veto_count += 1

    def get_statistics(self) -> dict[str, Any]:
        """Get CEO statistics."""
        return {
            "total_reviews": len(self.action_history),
            "approvals": self.approval_count,
            "vetoes": self.veto_count,
            "interventions": self.intervention_count,
            "approval_rate": (
                self.approval_count / len(self.action_history)
                if self.action_history
                else 0.0
            ),
        }
