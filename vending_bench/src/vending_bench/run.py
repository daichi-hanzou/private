"""
Main runner for Vending-Bench.
CLI entry point and simulation orchestration.
"""

from __future__ import annotations

import json
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any, TYPE_CHECKING

import typer
from rich.console import Console
from rich.progress import Progress, SpinnerColumn, TextColumn
from rich.table import Table

from vending_bench.config import Config
from vending_bench.environment.state import EnvironmentState
from vending_bench.environment.vending_machine import SlotSize
from vending_bench.simulation.supplier import SupplierSimulator
from vending_bench.simulation.demand_model import DemandModel
from vending_bench.tools.base import ToolRegistry
from vending_bench.tools.main_agent_tools import create_main_agent_tools
from vending_bench.tools.memory_tools import create_memory_tools
from vending_bench.agent.base import AgentAction
from vending_bench.agent.rule_based_agent import RuleBasedAgent
from vending_bench.agent.sub_agent import SubAgent
from vending_bench.agent.context_manager import ContextManager
from vending_bench.scoring.scorer import Scorer, RunResult
from vending_bench.logging.trace_logger import TraceLogger
from vending_bench.logging.metrics_logger import MetricsLogger

if TYPE_CHECKING:
    from vending_bench.agent.base import BaseAgent

app = typer.Typer(help="Vending-Bench: A benchmark for long-term coherence of autonomous agents")
console = Console()


class VendingBenchRunner:
    """
    Main runner for Vending-Bench simulations.

    Orchestrates:
    - Environment state management
    - Agent interactions
    - Daily simulation (sales, deliveries, fees)
    - Logging and scoring
    """

    def __init__(
        self,
        config: Config,
        agent: BaseAgent | None = None,
        verbose: bool = True,
    ) -> None:
        self.config = config
        self.verbose = verbose

        # Initialize environment
        self.state = EnvironmentState.create(config)

        # Initialize simulation components
        self.supplier_sim = SupplierSimulator.create(
            config.supplier,
            seed=config.demand.seed,
        )
        self.demand_model = DemandModel.create(config.demand)

        # Initialize tools
        self.tool_registry = ToolRegistry()
        self.tool_registry.register_many(create_main_agent_tools())
        self.tool_registry.register_many(create_memory_tools())

        # Initialize sub-agent
        self.sub_agent = SubAgent()

        # Initialize agent (default to rule-based)
        self.agent = agent or RuleBasedAgent()

        # Initialize context manager
        self.context_manager = ContextManager(max_tokens=config.agent.context_tokens)

        # Initialize logging
        output_dir = Path(config.logging.output_dir)
        self.trace_logger = TraceLogger(
            output_dir / config.logging.trace_file if config.logging.trace_file else None
        )
        self.metrics_logger = MetricsLogger(
            output_dir / config.logging.metrics_file if config.logging.metrics_file else None
        )

        # Initialize scorer
        self.scorer = Scorer()

        # Tracking
        self.current_observation = ""
        self._last_day_processed = 0

    def run(self) -> RunResult:
        """
        Run the full simulation.

        Returns:
            Final run result with scores and metrics
        """
        # Start logging
        self.trace_logger.start()
        self.metrics_logger.start()

        try:
            # Generate initial observation
            self.current_observation = self._generate_initial_observation()

            # Main loop
            while True:
                # Check termination conditions
                terminated, reason = self.state.is_terminated(
                    max_messages=self.config.agent.max_messages,
                    bankruptcy_threshold=self.config.agent.bankruptcy_threshold,
                )

                if terminated:
                    if self.verbose:
                        console.print(f"\n[bold red]Simulation ended: {reason}[/bold red]")
                    break

                # Agent decides action
                action = self.agent.think(
                    state=self.state,
                    config=self.config,
                    observation=self.current_observation,
                )

                # Log agent action
                self.trace_logger.log_agent_action(
                    action=action,
                    day_number=self.state.clock.day_number,
                    sim_timestamp=self.state.clock.get_datetime(),
                )

                # Execute action
                result = self._execute_action(action)

                # Update observation
                self.current_observation = result.output

                # Pass result to agent
                self.agent.receive_tool_result(action, result)

                # Increment message count
                self.state.message_count += 1

                # Print progress
                if self.verbose and self.state.message_count % 10 == 0:
                    self._print_progress()

            # Calculate final result
            result = self.scorer.calculate_final_result(
                state=self.state,
                termination_reason=reason,
            )

            if self.verbose:
                console.print("\n" + result.format_summary())

            return result

        finally:
            # Stop logging
            self.trace_logger.stop()
            self.metrics_logger.stop()

    def _execute_action(self, action: AgentAction) -> Any:
        """Execute an agent action."""
        from vending_bench.tools.base import ToolResult

        # Handle special actions
        if action.tool_name == "wait_for_next_day":
            return self._process_day_transition()

        elif action.tool_name == "run_sub_agent":
            instruction = action.arguments.get("instruction", "")
            result = self.sub_agent.execute_instruction(
                instruction=instruction,
                state=self.state,
                config=self.config,
            )
            return result

        elif action.tool_name == "chat_with_sub_agent":
            question = action.arguments.get("question", "")
            answer = self.sub_agent.chat(question, self.state)
            return ToolResult.success_result(answer)

        else:
            # Execute through tool registry
            result = self.tool_registry.execute(
                name=action.tool_name,
                state=self.state,
                config=self.config,
                **action.arguments,
            )

            # Log tool call
            tool_calls = self.tool_registry.get_tool_calls()
            if tool_calls:
                self.trace_logger.log_tool_call(
                    tool_calls[-1],
                    agent_reasoning=action.reasoning,
                )

            # Handle supplier emails
            if action.tool_name == "send_email":
                self._process_outgoing_email(action.arguments)

            return result

    def _process_day_transition(self) -> Any:
        """Process the transition to the next day."""
        from vending_bench.tools.base import ToolResult

        # Finalize current day metrics
        self.state.finalize_day_metrics()
        if self.state.current_day_metrics:
            self.metrics_logger.log_daily_metrics(self.state.current_day_metrics)

        # Advance to next day
        self.state.clock.advance_to_next_day()
        self.state.start_new_day()

        # Process morning events
        morning_report = self._process_morning_events()

        # Log morning report
        self.trace_logger.log_morning_report(
            day_number=self.state.clock.day_number,
            sales_report=morning_report.get("sales", {}),
            new_emails=morning_report.get("new_emails", 0),
            deliveries=morning_report.get("deliveries", []),
            sim_timestamp=self.state.clock.get_datetime(),
        )

        return ToolResult.success_result(morning_report["message"])

    def _process_morning_events(self) -> dict[str, Any]:
        """Process all morning events for a new day."""
        lines = [
            f"\n{'='*50}",
            f"MORNING REPORT - Day {self.state.clock.day_number}",
            f"{self.state.clock.current_date.isoformat()}",
            f"{'='*50}",
        ]

        # 1. Process previous day's sales
        if self._last_day_processed < self.state.clock.day_number - 1:
            sales_result = self.demand_model.simulate_daily_sales(
                machine=self.state.machine,
                current_date=self.state.clock.current_date - timedelta(days=1),
                day_number=self.state.clock.day_number - 1,
            )
            self.state.record_sale(sales_result.total_units_sold, sales_result.total_revenue)
            lines.append("")
            lines.append("YESTERDAY'S SALES:")
            lines.append(self.demand_model.format_sales_report(sales_result))

        # 2. Pay daily fee
        fee_paid = self.state.account.pay_daily_fee(
            fee=self.config.environment.daily_fee,
            current_date=self.state.clock.current_date,
            day_number=self.state.clock.day_number,
        )
        self.state.record_daily_fee_result(fee_paid)

        if not fee_paid:
            lines.append("")
            lines.append(f"[WARNING] Could not pay daily fee (${self.config.environment.daily_fee})")
            lines.append(
                f"Consecutive failures: {self.state.account.consecutive_fee_failures}/"
                f"{self.config.agent.bankruptcy_threshold}"
            )

        # 3. Process deliveries
        deliveries = self.supplier_sim.process_deliveries(
            email_system=self.state.email_system,
            current_date=self.state.clock.current_date,
            day_number=self.state.clock.day_number,
        )

        delivery_names = []
        for order in deliveries:
            # Add items to storage
            for item in order.items:
                # Determine size based on product
                product_info = order.supplier.get_product(item.product_name)
                size = product_info.size if product_info else SlotSize.SMALL

                self.state.storage.add_product(
                    name=item.product_name,
                    quantity=item.quantity,
                    purchase_price=item.unit_price,
                    size=size,
                )

            # Charge account
            self.state.account.make_purchase(
                amount=order.total_amount,
                supplier=order.supplier.name,
                items=", ".join(f"{i.product_name} x{i.quantity}" for i in order.items),
                current_date=self.state.clock.current_date,
                day_number=self.state.clock.day_number,
            )

            delivery_names.append(order.id)

        if deliveries:
            lines.append("")
            lines.append("DELIVERIES RECEIVED:")
            for order in deliveries:
                lines.append(f"  Order {order.id} from {order.supplier.name}")
                for item in order.items:
                    lines.append(f"    - {item.product_name}: {item.quantity} units")

        # 4. Generate supplier email responses
        responses = self.supplier_sim.generate_daily_responses(
            email_system=self.state.email_system,
            current_date=self.state.clock.current_date,
            day_number=self.state.clock.day_number,
        )

        # 5. Count new emails
        new_email_count = self.state.email_system.get_unread_count() if self.state.email_system else 0

        if new_email_count > 0:
            lines.append("")
            lines.append(f"NEW EMAILS: {new_email_count} unread message(s)")

        # 6. Summary
        lines.append("")
        lines.append("CURRENT STATUS:")
        lines.append(f"  Balance: ${self.state.account.balance:.2f}")
        lines.append(f"  Cash in Machine: ${self.state.machine.cash_box:.2f}")
        lines.append(f"  Net Worth: ${self.scorer.calculate_net_worth(self.state):.2f}")
        lines.append("=" * 50)

        self._last_day_processed = self.state.clock.day_number

        return {
            "message": "\n".join(lines),
            "sales": {},
            "new_emails": new_email_count,
            "deliveries": delivery_names,
        }

    def _process_outgoing_email(self, args: dict[str, Any]) -> None:
        """Process an outgoing email for supplier simulation."""
        if not self.state.email_system:
            return

        self.supplier_sim.process_outgoing_email(
            email_system=self.state.email_system,
            sender_email=self.state.email_system.agent_email,
            recipient_email=args.get("recipient", ""),
            subject=args.get("subject", ""),
            body=args.get("body", ""),
            current_date=self.state.clock.current_date,
            day_number=self.state.clock.day_number,
        )

    def _generate_initial_observation(self) -> str:
        """Generate the initial observation for the agent."""
        lines = [
            "Welcome to Vending-Bench!",
            "",
            "You are now operating a vending machine business.",
            f"Starting Balance: ${self.config.environment.initial_balance}",
            f"Daily Operating Fee: ${self.config.environment.daily_fee}",
            "",
            "Your vending machine has 12 slots (4 rows x 3 columns):",
            "  - Rows 0-1: Small items",
            "  - Rows 2-3: Large items",
            "",
            "To get started:",
            "1. Search for wholesale suppliers (ai_web_search)",
            "2. Send emails to order products (send_email)",
            "3. Wait for delivery (wait_for_next_day)",
            "4. Use sub-agent to stock and price products (run_sub_agent)",
            "5. Collect cash from sales (run_sub_agent)",
            "",
            "Use sub_agent_specs to see what physical tasks the sub-agent can do.",
            "Use get_money_balance to check your finances.",
            "",
            "Good luck!",
        ]
        return "\n".join(lines)

    def _print_progress(self) -> None:
        """Print progress update."""
        net_worth = self.scorer.calculate_net_worth(self.state)
        console.print(
            f"Day {self.state.clock.day_number} | "
            f"Messages: {self.state.message_count} | "
            f"Balance: ${self.state.account.balance:.2f} | "
            f"Net Worth: ${net_worth:.2f}"
        )


@app.command()
def run(
    config_path: str = typer.Option(
        "configs/default.yaml",
        "--config", "-c",
        help="Path to configuration file",
    ),
    output_dir: str = typer.Option(
        "./output",
        "--output", "-o",
        help="Output directory for logs and results",
    ),
    max_messages: int = typer.Option(
        None,
        "--max-messages", "-m",
        help="Maximum messages (overrides config)",
    ),
    seed: int = typer.Option(
        None,
        "--seed", "-s",
        help="Random seed (overrides config)",
    ),
    verbose: bool = typer.Option(
        True,
        "--verbose/--quiet", "-v/-q",
        help="Verbose output",
    ),
) -> None:
    """Run a Vending-Bench simulation."""
    # Load config
    config_file = Path(config_path)
    if config_file.exists():
        config = Config.from_yaml(config_file)
    else:
        console.print(f"[yellow]Config file not found, using defaults[/yellow]")
        config = Config.default()

    # Apply overrides
    if max_messages:
        config.agent.max_messages = max_messages
    if seed:
        config.demand.seed = seed
    config.logging.output_dir = output_dir

    # Print header
    console.print("\n[bold blue]Vending-Bench Simulation[/bold blue]")
    console.print(f"Config: {config_path}")
    console.print(f"Max Messages: {config.agent.max_messages}")
    console.print(f"Seed: {config.demand.seed}")
    console.print()

    # Run simulation
    runner = VendingBenchRunner(config=config, verbose=verbose)

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        console=console,
        transient=True,
    ) as progress:
        task = progress.add_task("Running simulation...", total=None)
        result = runner.run()

    # Save result
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    result_file = output_path / "result.json"
    with open(result_file, "w") as f:
        json.dump(result.to_dict(), f, indent=2)

    console.print(f"\n[green]Results saved to {result_file}[/green]")


@app.command()
def info() -> None:
    """Show information about Vending-Bench."""
    console.print("\n[bold]Vending-Bench[/bold]")
    console.print("A benchmark for long-term coherence of autonomous agents")
    console.print()

    table = Table(title="Environment Configuration")
    table.add_column("Parameter", style="cyan")
    table.add_column("Default Value", style="green")

    table.add_row("Initial Balance", "$500")
    table.add_row("Daily Fee", "$2")
    table.add_row("Machine Slots", "12 (4 rows x 3 columns)")
    table.add_row("Max Messages", "2000")
    table.add_row("Context Tokens", "30,000")
    table.add_row("Bankruptcy Threshold", "10 consecutive days")

    console.print(table)


if __name__ == "__main__":
    app()
