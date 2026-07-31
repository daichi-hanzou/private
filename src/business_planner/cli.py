from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from dotenv import load_dotenv

from .ingestion import chunk_documents, company_slug, load_documents
from .planner import generate_plan, make_retriever
from .simulation.orchestrator import run_one_round_simulation


def parser() -> argparse.ArgumentParser:
    root = argparse.ArgumentParser(prog="business-planner")
    root.add_argument("--data-root", type=Path, default=Path("data"))
    commands = root.add_subparsers(dest="command", required=True)

    plan = commands.add_parser("plan", help="Generate an evidence-grounded plan")
    plan.add_argument("--company-name", required=True)
    plan.add_argument("--target-revenue-growth", required=True)
    plan.add_argument("--base-fiscal-year", type=int)
    plan.add_argument("--target-fiscal-year", type=int)
    plan.add_argument("--model")
    plan.add_argument("--retrieval", choices=["hybrid", "bm25"], default="hybrid")
    plan.add_argument(
        "--retrieval-limit", type=int, default=5,
        help="Maximum retrieved chunks per planning query (1-50)",
    )
    plan.add_argument("--output", type=Path)
    plan.add_argument("--principles-file", type=Path)
    plan.add_argument(
        "--principles-mode",
        choices=["enabled", "disabled"],
        default="enabled",
        help="Enable or disable executive principles for the Planner",
    )

    inspect = commands.add_parser("inspect", help="Inspect retrieved evidence")
    inspect.add_argument("--company-name", required=True)
    inspect.add_argument("--query", required=True)
    inspect.add_argument("--limit", type=int, default=8)
    inspect.add_argument("--retrieval", choices=["hybrid", "bm25"], default="hybrid")

    simulate = commands.add_parser(
        "simulate", help="Simulate adverse years, CEO pressure, and plan revisions"
    )
    simulate.add_argument("--company-name", required=True)
    simulate.add_argument("--target-revenue-growth", required=True)
    simulate.add_argument("--base-fiscal-year", type=int, required=True)
    simulate.add_argument("--target-fiscal-year", type=int, required=True)
    simulate.add_argument("--retrieval", choices=["hybrid", "bm25"], default="hybrid")
    simulate.add_argument(
        "--retrieval-limit", type=int, default=4,
        help="Maximum retrieved chunks per simulation query (1-50)",
    )
    simulate.add_argument(
        "--ceo-pressure", choices=["low", "medium", "high"], default="high"
    )
    simulate.add_argument("--model")
    simulate.add_argument("--plan-file", type=Path)
    simulate.add_argument("--principles-file", type=Path)
    simulate.add_argument(
        "--principles-mode",
        choices=["enabled", "disabled"],
        default="enabled",
        help="Enable or disable executive principles for CEO and Planner",
    )
    simulate.add_argument(
        "--rounds",
        type=int,
        default=1,
        help="Number of annual rounds; capped at the target fiscal year",
    )
    return root


def main(argv: list[str] | None = None) -> int:
    load_dotenv()
    args = parser().parse_args(argv)
    try:
        if args.command == "inspect":
            chunks = chunk_documents(load_documents(args.data_root, args.company_name))
            hits = make_retriever(chunks, args.retrieval).search(args.query, args.limit)
            payload = [
                {**chunk.citation(), "chunk_index": chunk.chunk_index, "text": chunk.text}
                for chunk in hits
            ]
        elif args.command == "plan":
            payload = generate_plan(
                args.data_root, args.company_name, args.target_revenue_growth, args.model,
                args.retrieval, args.base_fiscal_year, args.target_fiscal_year,
                args.principles_file,
                args.principles_mode == "enabled",
                args.retrieval_limit,
            )
            output = args.output or (
                Path("results") / company_slug(args.company_name) / "business_plan.json"
            )
            output.parent.mkdir(parents=True, exist_ok=True)
            output.write_text(
                json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8"
            )
        else:
            payload, output = run_one_round_simulation(
                data_root=args.data_root,
                company_name=args.company_name,
                target_growth=args.target_revenue_growth,
                base_fiscal_year=args.base_fiscal_year,
                target_fiscal_year=args.target_fiscal_year,
                plan_path=args.plan_file,
                retrieval_mode=args.retrieval,
                ceo_pressure=args.ceo_pressure,
                rounds=args.rounds,
                model=args.model,
                principles_file=args.principles_file,
                use_executive_principles=(
                    args.principles_mode == "enabled"
                ),
                retrieval_limit=args.retrieval_limit,
            )
            payload = {**payload, "output_file": output.as_posix()}
        print(json.dumps(payload, ensure_ascii=False, indent=2))
        return 0
    except Exception as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
