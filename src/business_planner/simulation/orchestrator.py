from __future__ import annotations

import json
import os
from datetime import datetime, timezone
from pathlib import Path

from openai import OpenAI

from ..agents.ceo_pressure import generate_ceo_feedback
from ..agents.internal_audit import observe_internal_audit
from ..agents.planner_revision import revise_plan
from ..agents.reality import extract_baseline_financials, simulate_one_year
from ..ingestion import chunk_documents, company_slug, load_documents
from ..planner import (
    collect_source_ids,
    make_retriever,
    parse_growth,
    validate_planning_period,
)
from ..openai_client import create_chat_client, create_embedding_client
from .provenance import document_snapshot
from .state import (
    advance_simulation_state,
    create_initial_state,
    required_annual_growth_pct,
)
from .views import build_planner_execution_report
from .analytics import summarize_rounds


REALITY_QUERY = (
    "施策の実現可能性 売上目標 生産能力 投資余力 人材 市場規模 "
    "競合 リスク 規制 キャッシュフロー"
)


def _evidence_text(chunks) -> str:
    return "\n\n".join(
        f"[{chunk.source_id}] file={chunk.path.as_posix()} "
        f"page={chunk.page or 'N/A'} category={chunk.category}\n{chunk.text}"
        for chunk in chunks
    )


def _retrieve_evaluation_evidence(retriever, plan: dict, per_query: int = 4):
    queries = [REALITY_QUERY]
    queries.extend(item["name"] for item in plan.get("growth_plan", []))
    selected = {}
    for query in queries:
        for chunk in retriever.search(query, per_query):
            selected[(chunk.source_id, chunk.chunk_index)] = chunk
    return list(selected.values())


def _validate_agent_sources(result: dict, allowed_source_ids: set[str], role: str):
    unknown = collect_source_ids(result) - allowed_source_ids
    if unknown:
        raise ValueError(f"{role} returned unknown source IDs: {sorted(unknown)}")


def run_one_round_simulation(
    *,
    data_root: Path,
    company_name: str,
    target_growth: str,
    base_fiscal_year: int,
    target_fiscal_year: int,
    plan_path: Path | None = None,
    retrieval_mode: str = "hybrid",
    ceo_pressure: str = "high",
    rounds: int = 1,
    model: str | None = None,
) -> tuple[dict, Path]:
    growth = parse_growth(target_growth)
    planning_period = validate_planning_period(base_fiscal_year, target_fiscal_year)
    if rounds < 1:
        raise ValueError("--rounds must be at least 1")
    maximum_rounds = target_fiscal_year - base_fiscal_year
    configured_rounds = min(rounds, maximum_rounds)
    plan_path = plan_path or (
        Path("results") / company_slug(company_name) / "business_plan.json"
    )
    if not plan_path.is_file():
        raise FileNotFoundError(
            f"Business plan not found: {plan_path}. Run the plan command first."
        )
    plan = json.loads(plan_path.read_text(encoding="utf-8"))
    if plan.get("company_name") != company_name:
        raise ValueError("Business plan company does not match --company-name")
    if float(plan.get("target_revenue_growth")) != growth:
        raise ValueError("Business plan growth target does not match the simulation target")
    if plan.get("planning_period") != planning_period:
        raise ValueError(
            "Business plan planning period does not match the simulation. "
            "Regenerate the plan with matching --base-fiscal-year and "
            "--target-fiscal-year values."
        )

    client = create_chat_client()
    chunks = chunk_documents(load_documents(data_root, company_name))
    citations = {chunk.source_id: chunk.citation() for chunk in chunks}
    plan_source_ids = collect_source_ids(plan)
    unknown_plan_sources = plan_source_ids - citations.keys()
    if unknown_plan_sources:
        raise ValueError(
            f"Business plan contains unknown source IDs: {sorted(unknown_plan_sources)}"
        )
    plan["sources"] = [citations[source] for source in sorted(plan_source_ids)]
    embedding_client = (
        create_embedding_client() if retrieval_mode == "hybrid" else None
    )
    retriever = make_retriever(
        chunks, retrieval_mode, embedding_client
    )
    initial_state = create_initial_state(
        plan, extract_baseline_financials(plan, chunks)
    )
    state = initial_state
    round_results = []
    all_evidence_chunks = {}
    for round_index in range(1, configured_rounds + 1):
        current_plan = state["current_plan"]
        annual_target_growth = required_annual_growth_pct(state)
        evidence_chunks = _retrieve_evaluation_evidence(
            retriever, current_plan
        )
        if not evidence_chunks:
            raise ValueError("No evidence was retrieved for reality evaluation")
        for chunk in evidence_chunks:
            all_evidence_chunks[(chunk.source_id, chunk.chunk_index)] = chunk
        allowed_ids = collect_source_ids(current_plan)
        allowed_ids.update(chunk.source_id for chunk in evidence_chunks)

        reality_outcome = simulate_one_year(
            client,
            plan=current_plan,
            evidence=_evidence_text(evidence_chunks),
            baseline_financials=state["baseline_financials"],
            simulation_year=state["current_fiscal_year"] + 1,
            annual_target_growth_pct=annual_target_growth,
            cumulative_target_growth_pct=state[
                "cumulative_revenue_growth_target_pct"
            ],
            target_revenue_million_yen=state[
                "target_revenue_million_yen"
            ],
            round_index=round_index,
            model=model,
        )
        _validate_agent_sources(reality_outcome, allowed_ids, "Reality Agent")
        ceo_feedback = generate_ceo_feedback(
            client,
            plan=current_plan,
            reality_outcome=reality_outcome,
            pressure_level=ceo_pressure,
            round_index=round_index,
            model=model,
        )
        planner_execution_report = build_planner_execution_report(
            reality_outcome
        )
        revised_plan = revise_plan(
            client,
            prior_plan=current_plan,
            execution_report=planner_execution_report,
            ceo_feedback=ceo_feedback,
            evidence=_evidence_text(evidence_chunks),
            model=model,
        )
        revised_source_ids = collect_source_ids(revised_plan)
        unknown_revised_sources = revised_source_ids - allowed_ids
        if unknown_revised_sources:
            raise ValueError(
                f"Planner Revision returned unknown source IDs: "
                f"{sorted(unknown_revised_sources)}"
            )
        revised_plan["sources"] = [
            citations[source] for source in sorted(revised_source_ids)
        ]
        audit = observe_internal_audit(
            client,
            prior_plan=current_plan,
            reality_outcome=reality_outcome,
            ceo_feedback=ceo_feedback,
            revised_plan=revised_plan,
            round_index=round_index,
            model=model,
        )
        state = advance_simulation_state(
            state,
            reality_outcome=reality_outcome,
            revised_plan=revised_plan,
            ceo_feedback=ceo_feedback,
        )
        round_results.append({
            "round_index": round_index,
            "reality_outcome": reality_outcome,
            "planner_execution_report": planner_execution_report,
            "ceo_feedback": ceo_feedback,
            "revised_plan": revised_plan,
            "internal_audit_observation": audit,
            "state_after_round": state,
        })

    now = datetime.now(timezone.utc)
    run_id = now.strftime("run_%Y%m%dT%H%M%SZ")
    simulation_analysis = summarize_rounds(round_results)
    result = {
        "run_id": run_id,
        "created_at": now.isoformat(),
        "company_name": company_name,
        "target_revenue_growth": growth,
        "planning_period": planning_period,
        "settings": {
            "rounds": configured_rounds,
            "requested_rounds": rounds,
            "retrieval": retrieval_mode,
            "ceo_pressure": ceo_pressure.title(),
            "revenue_target_interpretation": (
                "Cumulative growth from base fiscal year to target fiscal year"
            ),
            "audit_mode": "Passive",
            "model": model or os.getenv("OPENAI_MODEL", "gpt-5.6"),
            "embedding_model": os.getenv(
                "OPENAI_EMBEDDING_MODEL", "text-embedding-3-large"
            ),
        },
        "prompt_versions": {
            "reality": "2.0",
            "ceo_pressure": "2.0",
            "planner_revision": "1.0",
            "internal_audit": "2.0",
        },
        "document_snapshot": document_snapshot(data_root, company_name),
        "initial_state": initial_state,
        "initial_plan": plan,
        "retrieved_sources": list({
            chunk.source_id: chunk.citation()
            for chunk in all_evidence_chunks.values()
        }.values()),
        "retrieved_chunks": [
            {**chunk.citation(), "chunk_index": chunk.chunk_index}
            for chunk in all_evidence_chunks.values()
        ],
        "rounds": round_results,
        "final_state": state,
        "final_plan": state["current_plan"],
        "simulation_analysis": simulation_analysis,
        "summary": {
            "final_fraud_risk": audit["overall_fraud_risk"],
            "risk_domains": audit["risk_domains"],
            "risk_trend": simulation_analysis["optimization_drift"]["trend"],
            "main_red_flags": audit["red_flags"],
            "stopping_reason": (
                "Target fiscal year reached"
                if state["current_fiscal_year"] == target_fiscal_year
                else "Configured round limit reached"
            ),
        },
    }
    output_dir = (
        Path("results") / company_slug(company_name) / "simulation_runs"
    )
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / f"{run_id}.json"
    output_path.write_text(
        json.dumps(result, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    return result, output_path
