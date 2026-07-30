from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from .ingestion import chunk_documents, company_slug, load_documents
from .planner import generate_plan
from .retrieval import BM25Retriever


def parser() -> argparse.ArgumentParser:
    root = argparse.ArgumentParser(prog="business-planner")
    root.add_argument("--data-root", type=Path, default=Path("data"))
    commands = root.add_subparsers(dest="command", required=True)

    plan = commands.add_parser("plan", help="Generate an evidence-grounded plan")
    plan.add_argument("--company-name", required=True)
    plan.add_argument("--target-revenue-growth", required=True)
    plan.add_argument("--model")
    plan.add_argument("--output", type=Path)

    inspect = commands.add_parser("inspect", help="Inspect local retrieval without an API call")
    inspect.add_argument("--company-name", required=True)
    inspect.add_argument("--query", required=True)
    inspect.add_argument("--limit", type=int, default=8)
    return root


def main(argv: list[str] | None = None) -> int:
    args = parser().parse_args(argv)
    try:
        if args.command == "inspect":
            chunks = chunk_documents(load_documents(args.data_root, args.company_name))
            hits = BM25Retriever(chunks).search(args.query, args.limit)
            payload = [
                {**chunk.citation(), "chunk_index": chunk.chunk_index, "text": chunk.text}
                for chunk in hits
            ]
        else:
            payload = generate_plan(
                args.data_root, args.company_name, args.target_revenue_growth, args.model
            )
            output = args.output or (
                Path("results") / company_slug(args.company_name) / "business_plan.json"
            )
            output.parent.mkdir(parents=True, exist_ok=True)
            output.write_text(
                json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8"
            )
        print(json.dumps(payload, ensure_ascii=False, indent=2))
        return 0
    except Exception as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
