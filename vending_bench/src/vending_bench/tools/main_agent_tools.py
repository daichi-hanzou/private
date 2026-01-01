"""
Main agent tools for Vending-Bench.
Tools available directly to the main agent for remote operations.
"""

from __future__ import annotations

from typing import Any, TYPE_CHECKING

from vending_bench.tools.base import BaseTool, ToolResult

if TYPE_CHECKING:
    from vending_bench.environment.state import EnvironmentState
    from vending_bench.config import Config


class ReadEmailsTool(BaseTool):
    """Read emails from inbox (alias for read_email_inbox)."""

    @property
    def name(self) -> str:
        return "read_emails"

    @property
    def description(self) -> str:
        return "Read all emails from your inbox. Returns a list of emails with sender, subject, and date."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "unread_only": {
                    "type": "boolean",
                    "description": "If true, only show unread emails",
                    "default": False,
                },
            },
            "required": [],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        unread_only: bool = False,
        **kwargs: Any,
    ) -> ToolResult:
        if state.email_system is None:
            return ToolResult.error_result("Email system not initialized")

        output = state.email_system.format_inbox(limit=30)
        summary = state.email_system.get_inbox_summary(unread_only=unread_only)

        return ToolResult.success_result(output, {"emails": summary})


class ReadEmailInboxTool(BaseTool):
    """Read inbox summary."""

    @property
    def name(self) -> str:
        return "read_email_inbox"

    @property
    def description(self) -> str:
        return "Get a summary of your email inbox showing all messages."

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        if state.email_system is None:
            return ToolResult.error_result("Email system not initialized")

        output = state.email_system.format_inbox(limit=30)
        summary = state.email_system.get_inbox_summary()

        return ToolResult.success_result(output, {"emails": summary})


class ReadEmailTool(BaseTool):
    """Read a specific email."""

    @property
    def name(self) -> str:
        return "read_email"

    @property
    def description(self) -> str:
        return "Read a specific email by its ID. Marks the email as read."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "email_id": {
                    "type": "string",
                    "description": "The ID of the email to read",
                },
            },
            "required": ["email_id"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        email_id: str = "",
        **kwargs: Any,
    ) -> ToolResult:
        if state.email_system is None:
            return ToolResult.error_result("Email system not initialized")

        email = state.email_system.read_email(email_id)
        if email is None:
            return ToolResult.error_result(f"Email not found: {email_id}")

        output = email.format_for_display(include_body=True)
        return ToolResult.success_result(output, {"email": email.to_dict()})


class SendEmailTool(BaseTool):
    """Send an email."""

    @property
    def name(self) -> str:
        return "send_email"

    @property
    def description(self) -> str:
        return "Send an email to a recipient. Use this to contact suppliers."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "recipient": {
                    "type": "string",
                    "description": "Email address of the recipient",
                },
                "subject": {
                    "type": "string",
                    "description": "Subject line of the email",
                },
                "body": {
                    "type": "string",
                    "description": "Body content of the email",
                },
            },
            "required": ["recipient", "subject", "body"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        recipient: str = "",
        subject: str = "",
        body: str = "",
        **kwargs: Any,
    ) -> ToolResult:
        if state.email_system is None:
            return ToolResult.error_result("Email system not initialized")

        email = state.email_system.send_email(
            recipient=recipient,
            subject=subject,
            body=body,
            timestamp=state.clock.get_datetime(),
            day_number=state.clock.day_number,
        )

        output = f"Email sent successfully to {recipient}\nSubject: {subject}"
        return ToolResult.success_result(output, {"email_id": email.id})


class AIWebSearchTool(BaseTool):
    """Search the web for information."""

    @property
    def name(self) -> str:
        return "ai_web_search"

    @property
    def description(self) -> str:
        return "Search the web for information about products, suppliers, or vending machine business topics."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "Search query",
                },
            },
            "required": ["query"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        query: str = "",
        **kwargs: Any,
    ) -> ToolResult:
        # MVP: Return fixed search results based on query
        query_lower = query.lower()

        # Import here to avoid circular imports
        from vending_bench.simulation.supplier import DEFAULT_SUPPLIERS

        results = []

        if any(word in query_lower for word in ["supplier", "wholesale", "distributor", "vendor"]):
            for supplier in DEFAULT_SUPPLIERS:
                results.append(
                    f"**{supplier.name}**\n"
                    f"  Location: {supplier.location}\n"
                    f"  Contact: {supplier.email}\n"
                    f"  Products: {', '.join(p.name for p in supplier.products[:3])}..."
                )

        elif any(word in query_lower for word in ["popular", "best", "selling", "vending", "product"]):
            results = [
                "**Popular Vending Machine Products:**",
                "1. Coca-Cola - Classic soft drink, always in demand",
                "2. Red Bull - Energy drinks are high-margin",
                "3. Bottled Water - Essential, especially in warm weather",
                "4. Lay's Chips - Popular snack option",
                "5. Snickers - Best-selling candy bar",
                "",
                "**Tips:**",
                "- Energy drinks have higher margins but lower volume",
                "- Water sells more in summer months",
                "- Variety is important but too many options can confuse customers",
            ]

        elif any(word in query_lower for word in ["price", "pricing", "margin"]):
            results = [
                "**Vending Machine Pricing Guide:**",
                "- Sodas: Typical retail $1.50-2.00 (wholesale $0.50-0.80)",
                "- Energy Drinks: Retail $3.00-4.00 (wholesale $1.50-2.00)",
                "- Water: Retail $1.00-1.50 (wholesale $0.30-0.50)",
                "- Snacks: Retail $1.50-2.50 (wholesale $0.70-1.00)",
                "",
                "**Pricing Tips:**",
                "- Aim for 40-60% markup",
                "- Consider location and competition",
                "- Premium locations can support higher prices",
            ]

        else:
            results = [
                f"Search results for: {query}",
                "",
                "No specific results found. Try searching for:",
                "- 'wholesale suppliers near me'",
                "- 'popular vending machine products'",
                "- 'vending machine pricing guide'",
            ]

        output = "\n".join(results)
        return ToolResult.success_result(output, {"query": query})


class GetStorageInventoryTool(BaseTool):
    """Get storage inventory."""

    @property
    def name(self) -> str:
        return "get_storage_inventory"

    @property
    def description(self) -> str:
        return "View all products currently in your storage warehouse."

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        inventory = state.storage.get_inventory()

        if not inventory:
            output = "Storage is empty. Order products from suppliers to stock your warehouse."
        else:
            lines = ["Storage Inventory:", "-" * 40]
            for item in inventory:
                lines.append(
                    f"  {item['name']}: {item['quantity']} units "
                    f"(${item['unit_price']:.2f} each, total: ${item['total_value']:.2f})"
                )
            lines.append("-" * 40)
            lines.append(f"Total Value: ${state.storage.get_total_value():.2f}")
            output = "\n".join(lines)

        return ToolResult.success_result(output, {"inventory": inventory})


class CheckStorageQuantitiesTool(BaseTool):
    """Check specific product quantities in storage."""

    @property
    def name(self) -> str:
        return "check_storage_quantities"

    @property
    def description(self) -> str:
        return "Check quantities of specific products in storage."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "product_names": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "List of product names to check",
                },
            },
            "required": [],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        product_names: list[str] | None = None,
        **kwargs: Any,
    ) -> ToolResult:
        if product_names:
            quantities = {
                name: state.storage.get_quantity(name) for name in product_names
            }
            lines = ["Product Quantities:"]
            for name, qty in quantities.items():
                lines.append(f"  {name}: {qty} units")
            output = "\n".join(lines)
        else:
            quantities = {
                name: item.quantity for name, item in state.storage.items.items()
            }
            inventory = state.storage.get_inventory()
            output = f"All products: {len(inventory)} types, {state.storage.get_total_items()} total units"

        return ToolResult.success_result(output, {"quantities": quantities})


class ListStorageProductsTool(BaseTool):
    """List products in storage."""

    @property
    def name(self) -> str:
        return "list_storage_products"

    @property
    def description(self) -> str:
        return "List all product types currently in storage."

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        products = state.storage.get_product_names()

        if not products:
            output = "No products in storage."
        else:
            output = f"Products in storage: {', '.join(products)}"

        return ToolResult.success_result(output, {"products": products})


class GetMoneyBalanceTool(BaseTool):
    """Get current money balance."""

    @property
    def name(self) -> str:
        return "get_money_balance"

    @property
    def description(self) -> str:
        return "Check your current money balance (cash on hand)."

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        balance = state.account.balance
        machine_cash = state.machine.cash_box

        lines = [
            f"Money Balance: ${balance:.2f}",
            f"Cash in Vending Machine: ${machine_cash:.2f}",
            f"Total Available: ${balance + machine_cash:.2f}",
        ]

        if state.account.consecutive_fee_failures > 0:
            lines.append(
                f"WARNING: Failed to pay daily fee for {state.account.consecutive_fee_failures} consecutive days"
            )

        output = "\n".join(lines)
        return ToolResult.success_result(
            output,
            {
                "balance": balance,
                "machine_cash": machine_cash,
                "consecutive_fee_failures": state.account.consecutive_fee_failures,
            },
        )


class WaitForNextDayTool(BaseTool):
    """Wait for the next day."""

    @property
    def name(self) -> str:
        return "wait_for_next_day"

    @property
    def description(self) -> str:
        return "Skip to the next day. You will receive a morning report with sales and new emails."

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        # This is a special tool - the actual day advancement is handled by the runner
        # Here we just signal the intent
        return ToolResult.success_result(
            "Waiting for next day...",
            {"action": "wait_for_next_day"},
        )


class SubAgentSpecsTool(BaseTool):
    """Get sub-agent specifications."""

    @property
    def name(self) -> str:
        return "sub_agent_specs"

    @property
    def description(self) -> str:
        return "Get information about the sub-agent, including what tools it has available for physical tasks."

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        **kwargs: Any,
    ) -> ToolResult:
        specs = """Sub-Agent Capabilities:

The sub-agent can perform physical tasks at the vending machine location.

Available Tools:
1. stock_products_from_storage_to_machine
   - Move products from your storage warehouse to the vending machine
   - Parameters: product_name, quantity, (optional) row, column

2. collect_cash_from_machine
   - Collect cash from the vending machine's cash box
   - Transfers money to your account balance

3. set_prices
   - Set retail prices for products in the vending machine
   - Parameters: product_name, price

4. get_machine_inventory
   - View current inventory and prices in the vending machine
   - Shows all slots, products, quantities, and prices

To use the sub-agent:
- Use run_sub_agent with a clear instruction describing what you want done
- Use chat_with_sub_agent to ask questions about the results"""

        return ToolResult.success_result(specs)


class RunSubAgentTool(BaseTool):
    """Run the sub-agent with instructions."""

    @property
    def name(self) -> str:
        return "run_sub_agent"

    @property
    def description(self) -> str:
        return "Give instructions to the sub-agent to perform physical tasks at the vending machine."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "instruction": {
                    "type": "string",
                    "description": "Clear instructions for what the sub-agent should do",
                },
            },
            "required": ["instruction"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        instruction: str = "",
        **kwargs: Any,
    ) -> ToolResult:
        # Sub-agent execution is handled by the runner
        # Here we just validate and pass through
        return ToolResult.success_result(
            f"Sub-agent instruction received: {instruction[:100]}...",
            {"action": "run_sub_agent", "instruction": instruction},
        )


class ChatWithSubAgentTool(BaseTool):
    """Chat with the sub-agent."""

    @property
    def name(self) -> str:
        return "chat_with_sub_agent"

    @property
    def description(self) -> str:
        return "Ask the sub-agent questions about its activities or the vending machine status."

    @property
    def parameters(self) -> dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "question": {
                    "type": "string",
                    "description": "Question to ask the sub-agent",
                },
            },
            "required": ["question"],
        }

    def execute(
        self,
        state: EnvironmentState,
        config: Config,
        question: str = "",
        **kwargs: Any,
    ) -> ToolResult:
        # Sub-agent chat is handled by the runner
        return ToolResult.success_result(
            f"Question for sub-agent: {question}",
            {"action": "chat_with_sub_agent", "question": question},
        )


def create_main_agent_tools() -> list[BaseTool]:
    """Create all main agent tools."""
    return [
        ReadEmailsTool(),
        ReadEmailTool(),
        ReadEmailInboxTool(),
        SendEmailTool(),
        AIWebSearchTool(),
        GetStorageInventoryTool(),
        CheckStorageQuantitiesTool(),
        ListStorageProductsTool(),
        GetMoneyBalanceTool(),
        WaitForNextDayTool(),
        SubAgentSpecsTool(),
        RunSubAgentTool(),
        ChatWithSubAgentTool(),
    ]
