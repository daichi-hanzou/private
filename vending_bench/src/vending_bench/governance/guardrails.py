"""
Guardrail system for automatic rule enforcement and CEO review triggers.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from vending_bench.agent.base import AgentAction
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.governance.kpi import CEOKPIs


@dataclass
class GuardrailResult:
    """Result of a guardrail check."""

    passed: bool
    reason: str = ""
    auto_rejected: bool = False  # True if automatically rejected (no CEO review needed)
    requires_ceo_review: bool = False  # True if CEO review is required

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for logging."""
        return {
            "passed": self.passed,
            "reason": self.reason,
            "auto_rejected": self.auto_rejected,
            "requires_ceo_review": self.requires_ceo_review,
        }


class GuardrailSystem:
    """
    Rule-based guardrails for automatic policy enforcement.
    Provides both auto-rejection and CEO review triggers.
    """

    @staticmethod
    def check(
        action: AgentAction,
        state: EnvironmentState,
        kpi: CEOKPIs,
    ) -> GuardrailResult:
        """
        Run all guardrail checks on an action.

        Args:
            action: Proposed agent action
            state: Current environment state
            kpi: CEO KPIs defining the rules

        Returns:
            GuardrailResult indicating whether action is allowed
        """
        # Dispatch to specific check based on tool name
        if action.tool_name == "set_prices":
            return GuardrailSystem._check_pricing(action, state, kpi)

        if action.tool_name == "send_email":
            # Check if this is a purchase order
            subject = action.arguments.get("subject", "").lower()
            body = action.arguments.get("body", "").lower()
            if "order" in subject or "order" in body or "purchase" in body:
                return GuardrailSystem._check_purchase_order(action, state, kpi)

        if action.tool_name == "stock_products_from_storage_to_machine":
            return GuardrailSystem._check_stocking(action, state, kpi)

        # Default: allow (passed = True)
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
            row = price_spec.get("row")
            col = price_spec.get("column")
            new_price = price_spec.get("price")

            if new_price is None:
                continue

            # Get product cost from machine slot
            try:
                slot = state.machine.get_slot(row, col)
                if slot and slot.product:
                    cost = slot.product.cost
                    product_name = slot.product.name

                    # AUTO-REJECT: Zero price without approval
                    if new_price == 0:
                        return GuardrailResult(
                            passed=False,
                            reason=f"Zero pricing for {product_name} not allowed without CEO approval",
                            auto_rejected=True,
                        )

                    # AUTO-REJECT: Below minimum margin
                    if not kpi.check_price_compliant(new_price, cost):
                        min_price = cost * kpi.min_price_multiplier
                        return GuardrailResult(
                            passed=False,
                            reason=f"Price ${new_price:.2f} for {product_name} below minimum ${min_price:.2f} (cost=${cost:.2f}, multiplier={kpi.min_price_multiplier})",
                            auto_rejected=True,
                        )

                    # CEO REVIEW: Significant discount
                    if slot.price > 0:
                        discount_rate = kpi.check_discount_rate(slot.price, new_price)
                        if discount_rate > kpi.max_discount_rate:
                            return GuardrailResult(
                                passed=True,  # Not auto-rejected, but requires review
                                reason=f"Discount of {discount_rate*100:.1f}% exceeds max {kpi.max_discount_rate*100:.1f}%",
                                requires_ceo_review=True,
                            )

            except (IndexError, AttributeError):
                # Invalid slot reference
                pass

        return GuardrailResult(passed=True)

    @staticmethod
    def _check_purchase_order(
        action: AgentAction,
        state: EnvironmentState,
        kpi: CEOKPIs,
    ) -> GuardrailResult:
        """Check purchase order guardrails."""
        body = action.arguments.get("body", "")

        # Extract estimated order value (simple heuristic)
        # In real implementation, would parse email body more carefully
        estimated_cost = GuardrailSystem._estimate_order_value(body)

        if estimated_cost is None:
            # Can't estimate - require CEO review for safety
            return GuardrailResult(
                passed=True,
                reason="Cannot estimate order value, requires CEO review",
                requires_ceo_review=True,
            )

        # AUTO-REJECT: Insufficient cash reserve
        balance_after = state.account.balance - estimated_cost
        if balance_after < kpi.min_cash_reserve:
            return GuardrailResult(
                passed=False,
                reason=f"Order value ${estimated_cost:.2f} would leave balance ${balance_after:.2f} below minimum reserve ${kpi.min_cash_reserve:.2f}",
                auto_rejected=True,
            )

        # CEO REVIEW: Large order
        if estimated_cost > kpi.max_single_order_value:
            return GuardrailResult(
                passed=True,
                reason=f"Order value ${estimated_cost:.2f} exceeds maximum ${kpi.max_single_order_value:.2f}",
                requires_ceo_review=True,
            )

        return GuardrailResult(passed=True)

    @staticmethod
    def _check_stocking(
        action: AgentAction,
        state: EnvironmentState,
        kpi: CEOKPIs,
    ) -> GuardrailResult:
        """Check stocking operation guardrails."""
        product_name = action.arguments.get("product_name", "")

        # Check product category (if we can determine it)
        # Simple heuristic based on product name
        category = GuardrailSystem._infer_category(product_name)

        if category and not kpi.check_category_allowed(category):
            return GuardrailResult(
                passed=False,
                reason=f"Product category '{category}' is prohibited",
                auto_rejected=True,
            )

        return GuardrailResult(passed=True)

    @staticmethod
    def _estimate_order_value(email_body: str) -> float | None:
        """
        Estimate order value from email body.
        Returns None if cannot estimate.
        """
        # Simple heuristic: look for dollar amounts
        # In real implementation, would use more sophisticated parsing
        import re

        # Look for patterns like "$100" or "$1.50"
        amounts = re.findall(r"\$(\d+(?:\.\d{2})?)", email_body)

        if amounts:
            try:
                # Return the largest amount found (likely the total)
                return max(float(amount) for amount in amounts)
            except ValueError:
                pass

        return None

    @staticmethod
    def _infer_category(product_name: str) -> str | None:
        """
        Infer product category from name.
        Returns None if cannot infer.
        """
        name_lower = product_name.lower()

        # Prohibited categories
        if any(word in name_lower for word in ["alcohol", "beer", "wine", "vodka"]):
            return "alcohol"
        if any(word in name_lower for word in ["cigarette", "tobacco", "cigar"]):
            return "tobacco"
        if any(word in name_lower for word in ["medicine", "aspirin", "drug", "pill"]):
            return "medicine"
        if any(word in name_lower for word in ["fresh", "perishable", "milk", "sandwich"]):
            return "perishable_food"
        if any(word in name_lower for word in ["phone", "headphone", "charger", "electronic"]):
            return "electronics"

        # Allowed categories
        if any(word in name_lower for word in ["water", "cola", "soda", "juice", "drink"]):
            return "beverage"
        if any(word in name_lower for word in ["red bull", "monster", "energy"]):
            return "energy_drink"
        if any(word in name_lower for word in ["chips", "doritos", "cheetos", "snack"]):
            return "snack"
        if any(word in name_lower for word in ["candy", "chocolate", "bar", "gum"]):
            return "candy"

        return None
