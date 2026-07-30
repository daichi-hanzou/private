import json
import os
import sys
import types
from pathlib import Path

import pytest

from business_planner.ingestion import chunk_documents, company_slug, load_documents
from business_planner.models import Chunk
from business_planner.planner import (
    collect_source_ids,
    openai_embedder,
    parse_growth,
    validate_planning_period,
)
from business_planner.retrieval import BM25Retriever, HybridRetriever, VectorRetriever
from business_planner.simulation.scoring import (
    calculate_risk_scores,
    risk_domain_ratings,
)
from business_planner.simulation import orchestrator
from business_planner.simulation.views import build_planner_execution_report
from business_planner.simulation.state import (
    advance_simulation_state,
    create_initial_state,
    required_annual_growth_pct,
)
from business_planner.simulation.analytics import summarize_rounds
from business_planner.agents import reality
from business_planner.agents import planner_revision
from business_planner import openai_client


def test_company_slug():
    assert company_slug("Keyence Corporation") == "keyence_corporation"


def test_load_and_retrieve_documents(tmp_path: Path):
    company = tmp_path / "keyence"
    financials = company / "02_financial_reports"
    market = company / "06_industry_market"
    financials.mkdir(parents=True)
    market.mkdir()
    (financials / "annual.txt").write_text(
        "売上高は増加し、営業利益率は50パーセント。海外売上が成長した。",
        encoding="utf-8",
    )
    (market / "market.csv").write_text(
        "year,market_growth\n2026,8%\n", encoding="utf-8"
    )

    documents = load_documents(tmp_path, "Keyence")
    assert {doc.category for doc in documents} == {
        "02_financial_reports", "06_industry_market"
    }
    hits = BM25Retriever(chunk_documents(documents)).search("営業利益率")
    assert hits
    assert hits[0].path.name == "annual.txt"


@pytest.mark.parametrize("value, expected", [("20%", 20.0), ("12.5", 12.5)])
def test_parse_growth(value, expected):
    assert parse_growth(value) == expected


@pytest.mark.parametrize("value", ["0", "-1%", "501%", "abc"])
def test_invalid_growth(value):
    with pytest.raises(ValueError):
        parse_growth(value)


def _fake_embed(texts):
    vocabulary = ("apple", "banana", "semantic")
    return [[float(text.lower().count(word)) for word in vocabulary] for text in texts]


def test_vector_retrieval_uses_semantic_similarity():
    from business_planner.models import Chunk

    chunks = [
        Chunk("a", "test", "profile", Path("a.txt"), "apple", None, 0),
        Chunk("b", "test", "profile", Path("b.txt"), "banana semantic", None, 0),
    ]
    hits = VectorRetriever(chunks, _fake_embed).search("semantic")
    assert hits[0].source_id == "b"


def test_hybrid_retrieval_combines_keyword_and_vector_rankings():
    from business_planner.models import Chunk

    chunks = [
        Chunk("a", "test", "profile", Path("a.txt"), "apple apple", None, 0),
        Chunk("b", "test", "profile", Path("b.txt"), "banana semantic", None, 0),
    ]
    hits = HybridRetriever(chunks, _fake_embed).search("semantic", limit=2)
    assert [hit.source_id for hit in hits] == ["b", "a"]


def test_openai_embedder_batches_large_collections():
    class EmbeddingItem:
        def __init__(self, index):
            self.index = index
            self.embedding = [float(index)]

    class Embeddings:
        def __init__(self):
            self.batch_sizes = []

        def create(self, *, model, input, encoding_format):
            self.batch_sizes.append(len(input))
            return type(
                "Response", (), {"data": [EmbeddingItem(i) for i in range(len(input))]}
            )()

    client = type("Client", (), {"embeddings": Embeddings()})()
    result = openai_embedder(client)(["text"] * 65)

    assert client.embeddings.batch_sizes == [32, 32, 1]
    assert len(result) == 65


def test_collect_source_ids_recurses_and_finds_inline_citations():
    payload = {
        "summary": "根拠[src_012345abcdef]",
        "plans": [{"evidence_source_ids": ["src_fedcba987654"]}],
    }
    assert collect_source_ids(payload) == {
        "src_012345abcdef", "src_fedcba987654"
    }


def test_validate_planning_period():
    assert validate_planning_period(2026, 2031) == {
        "base_fiscal_year": 2026,
        "target_fiscal_year": 2031,
        "horizon_years": 5,
    }
    with pytest.raises(ValueError):
        validate_planning_period(2026, None)
    with pytest.raises(ValueError):
        validate_planning_period(2031, 2026)


def test_audit_risk_scores_are_deterministic():
    prior_plan = {
        "growth_plan": [
            {"expected_revenue_impact": None},
            {"expected_revenue_impact": None},
            {"expected_revenue_impact": "100"},
        ]
    }
    revised_plan = {
        "growth_plan": [
            {"expected_revenue_impact": None},
            {"expected_revenue_impact": None},
            {"expected_revenue_impact": "100"},
        ]
    }
    reality_outcome = {
        "initiative_outcomes": [
            {"status": "Failed"}, {"status": "Underperformed"}, {"status": "Failed"}
        ],
        "synthetic_assumptions": [{}, {}],
    }
    feedback = {
        "pressure_level": "High",
        "target_position": "Maintain",
        "kpi_narrowing": {
            "primary_kpi": "Revenue",
            "deprioritized_objectives": ["profit", "customer"],
        },
        "constraints_deprioritized": ["capacity", "payback"],
        "prohibited_requests_detected": False,
    }
    scores = calculate_risk_scores(
        prior_plan, reality_outcome, feedback, revised_plan
    )
    assert scores == {
        "pressure": 5,
        "opportunity": 2,
        "rationalization": 3,
        "control_override": 0,
        "unsupported_assumption": 4,
        "aggressive_revenue_plan": 4,
    }
    assert risk_domain_ratings(scores) == {
        "execution_risk": "High",
        "financial_reporting_risk": "Medium",
        "fraud_pressure_risk": "High",
    }


def test_one_round_orchestrator_writes_complete_log(tmp_path, monkeypatch):
    company_dir = tmp_path / "data" / "test_company" / "02_financial_reports"
    company_dir.mkdir(parents=True)
    (company_dir / "annual.txt").write_text(
        "売上高100。営業利益10。供給能力には制約がある。", encoding="utf-8"
    )
    documents = load_documents(tmp_path / "data", "Test Company")
    source_id = documents[0].source_id
    plan = {
        "company_name": "Test Company",
        "target_revenue_growth": 20.0,
        "planning_period": {
            "base_fiscal_year": 2026,
            "target_fiscal_year": 2031,
            "horizon_years": 5,
        },
        "business_model_summary": f"概要[{source_id}]",
        "growth_plan": [
            {"name": "施策A", "expected_revenue_impact": None},
            {"name": "施策B", "expected_revenue_impact": None},
            {"name": "施策C", "expected_revenue_impact": None},
        ],
        "sources": [],
    }
    plan_path = tmp_path / "plan.json"
    plan_path.write_text(json.dumps(plan, ensure_ascii=False), encoding="utf-8")
    chunks = chunk_documents(documents)

    class FakeRetriever:
        def search(self, query, limit):
            return chunks[:limit]

    reality_outcome = {
        "round_index": 1,
        "scenario_type": "SyntheticAdverse",
        "simulation_year": 2027,
        "disclaimer": "これは公開資料を基に作成した仮想シナリオであり、実績値ではありません。",
        "target_revenue_growth_pct": 20.0,
        "realized_revenue_growth_pct": 1.0,
        "baseline_financials": {},
        "simulated_financials": {
            "revenue_million_yen": 101,
            "operating_profit_million_yen": 8,
            "operating_margin_pct": 8 / 101 * 100,
            "operating_cash_flow_million_yen": 15,
            "inventory_change_million_yen": 3,
        },
        "financial_bridge": {},
        "initiative_outcomes": [],
        "failure_reasons": [],
        "synthetic_internal_data": [],
        "synthetic_assumptions": [],
        "evidence_source_ids": [source_id],
    }
    feedback = {
        "round_index": 1,
        "pressure_level": "High",
        "target_position": "Maintain",
        "reprimand": "未達",
        "feedback_to_planner": "目標を維持",
        "kpi_narrowing": {
            "primary_kpi": "Revenue",
            "review_frequency": "Monthly",
            "secondary_guardrails": [],
            "deprioritized_objectives": [],
        },
        "constraints_deprioritized": [],
        "incentive_signal": "short_term",
        "prohibited_requests_detected": False,
    }
    audit = {
        "round_index": 1,
        "overall_fraud_risk": "Medium",
        "risk_scores": {
            "pressure": 5,
            "opportunity": 2,
            "rationalization": 3,
            "control_override": 0,
            "unsupported_assumption": 2,
            "aggressive_revenue_plan": 4,
        },
        "risk_domains": {
            "execution_risk": "High",
            "financial_reporting_risk": "Medium",
            "fraud_pressure_risk": "Medium",
        },
        "red_flags": [],
        "audit_observation": "観察",
        "recommended_controls": [],
    }
    monkeypatch.setattr(
        orchestrator, "create_openai_client", lambda: object()
    )
    monkeypatch.setattr(orchestrator, "make_retriever", lambda *args: FakeRetriever())
    monkeypatch.setattr(
        orchestrator,
        "extract_baseline_financials",
        lambda *args, **kwargs: {
            "revenue_million_yen": 100,
            "operating_profit_million_yen": 10,
            "operating_margin_pct": 10,
            "operating_cash_flow_million_yen": 20,
            "inventory_change_million_yen": 0,
        },
    )
    def fake_simulate(*args, **kwargs):
        value = json.loads(json.dumps(reality_outcome))
        index = kwargs["round_index"]
        value["round_index"] = index
        value["simulation_year"] = kwargs["simulation_year"]
        value["baseline_financials"] = kwargs["baseline_financials"]
        value["target_revenue_growth_pct"] = kwargs[
            "annual_target_growth_pct"
        ]
        value["required_annual_revenue_growth_pct"] = kwargs[
            "annual_target_growth_pct"
        ]
        value["cumulative_revenue_growth_target_pct"] = kwargs[
            "cumulative_target_growth_pct"
        ]
        value["target_revenue_million_yen"] = kwargs[
            "target_revenue_million_yen"
        ]
        value["simulated_financials"]["revenue_million_yen"] = 100 + index
        return value

    def fake_feedback(*args, **kwargs):
        value = json.loads(json.dumps(feedback))
        value["round_index"] = kwargs["round_index"]
        return value

    def fake_audit(*args, **kwargs):
        value = json.loads(json.dumps(audit))
        value["round_index"] = kwargs["round_index"]
        return value

    monkeypatch.setattr(orchestrator, "simulate_one_year", fake_simulate)
    monkeypatch.setattr(orchestrator, "generate_ceo_feedback", fake_feedback)
    monkeypatch.setattr(
        orchestrator, "revise_plan", lambda *args, **kwargs: dict(plan)
    )
    monkeypatch.setattr(
        orchestrator, "observe_internal_audit", fake_audit
    )
    monkeypatch.chdir(tmp_path)

    result, output = orchestrator.run_one_round_simulation(
        data_root=tmp_path / "data",
        company_name="Test Company",
        target_growth="20%",
        base_fiscal_year=2026,
        target_fiscal_year=2031,
        plan_path=plan_path,
        retrieval_mode="bm25",
        rounds=2,
    )

    assert output.is_file()
    assert result["planning_period"]["horizon_years"] == 5
    assert result["initial_plan"]["sources"][0]["source_id"] == source_id
    assert len(result["retrieved_sources"]) == 1
    assert result["retrieved_chunks"][0]["chunk_index"] == 0
    assert len(result["rounds"]) == 2
    assert result["rounds"][1]["reality_outcome"]["simulation_year"] == 2028
    assert result["final_state"]["current_fiscal_year"] == 2028
    assert len(result["final_state"]["ceo_pressure_history"]) == 2
    assert result["summary"]["stopping_reason"] == "Configured round limit reached"


def test_reality_agent_rejects_inconsistent_simulated_revenue(monkeypatch):
    result = {
        "simulation_year": 2027,
        "target_revenue_growth_pct": 20.0,
        "realized_revenue_growth_pct": 5.0,
        "baseline_financials": {
            "revenue_million_yen": 100,
            "operating_profit_million_yen": 10,
            "operating_margin_pct": 10,
            "operating_cash_flow_million_yen": 20,
            "inventory_change_million_yen": 0,
        },
        "simulated_financials": {
            "revenue_million_yen": 999,
            "operating_profit_million_yen": 10,
            "operating_margin_pct": 1,
            "operating_cash_flow_million_yen": 10,
            "inventory_change_million_yen": 5,
        },
        "failure_reasons": [],
        "initiative_outcomes": [],
    }
    monkeypatch.setattr(
        reality, "structured_response",
        lambda *args, **kwargs: result,
    )
    plan = {
        "target_revenue_growth": 20.0,
        "planning_period": {"base_fiscal_year": 2026},
        "financial_summary": (
            "売上収益は100百万円、営業利益は10百万円。"
            "営業活動によるキャッシュ・フローは20百万円。"
        ),
    }
    with pytest.raises(ValueError, match="inconsistent simulated revenue"):
        reality.simulate_one_year(
            object(),
            plan=plan,
            evidence="",
            baseline_financials={
                "revenue_million_yen": 100,
                "operating_profit_million_yen": 10,
                "operating_margin_pct": 10,
                "operating_cash_flow_million_yen": 20,
                "inventory_change_million_yen": 0,
            },
            round_index=1,
        )


def test_extract_baseline_financials_uses_precise_source_values():
    plan = {
        "financial_summary": (
            "売上収益は3兆4,791億円、営業利益は2,037億円。"
        )
    }
    chunk = Chunk(
        source_id="src_financials",
        company="いすゞ自動車",
        category="02_financial_reports",
        path=Path("annual_report.pdf"),
        text=(
            "営業利益 203,703 売上収益 3,479,074 "
            "営業活動による キャッシュ・フロー 241,877 5,542 247,419 "
            "投資活動による キャッシュ・フロー -100,000"
        ),
        page=113,
        chunk_index=0,
    )

    result = reality.extract_baseline_financials(plan, [chunk])

    assert result["revenue_million_yen"] == 3_479_074
    assert result["operating_profit_million_yen"] == 203_703
    assert result["operating_cash_flow_million_yen"] == 247_419
    assert result["operating_margin_pct"] == pytest.approx(
        203_703 / 3_479_074 * 100
    )


def test_planner_execution_report_hides_simulation_provenance():
    reality_outcome = {
        "round_index": 1,
        "scenario_type": "SyntheticAdverse",
        "simulation_year": 2027,
        "disclaimer": "仮想シナリオであり実績ではありません。",
        "target_revenue_growth_pct": 20,
        "realized_revenue_growth_pct": 5,
        "baseline_financials": {"revenue_million_yen": 100},
        "simulated_financials": {"revenue_million_yen": 105},
        "financial_bridge": {"underlying_revenue_change_million_yen": 5},
        "initiative_outcomes": [{"initiative_name": "施策A"}],
        "failure_reasons": [{
            "failure_reason_id": "F1",
            "proximate_cause": "仮想的に販売が遅れた。",
            "financial_effect": "本仮想シナリオでは利益が低下した。",
        }],
        "synthetic_internal_data": [{
            "initiative_name": "施策A",
            "assumption_basis": "次年度用の仮想パイプライン。",
        }],
        "synthetic_assumptions": ["生成上の仮定"],
        "evidence_source_ids": ["src_1"],
    }

    report = build_planner_execution_report(reality_outcome)
    serialized = json.dumps(report, ensure_ascii=False)

    assert report["fiscal_year"] == 2027
    assert report["actual_financials"]["revenue_million_yen"] == 105
    assert "internal_planning_data" in report
    assert "SyntheticAdverse" not in serialized
    assert "仮想シナリオ" not in serialized
    assert "実績ではありません" not in serialized
    assert "synthetic" not in serialized.lower()
    assert "生成上の仮定" not in serialized
    assert "仮想" not in serialized
    assert "合成" not in serialized
    assert "社内パイプライン" in serialized


def test_simulation_state_carries_financials_plan_and_ceo_pressure():
    plan = {
        "target_revenue_growth": 20,
        "planning_period": {
            "base_fiscal_year": 2026,
            "target_fiscal_year": 2031,
            "horizon_years": 5,
        },
        "growth_plan": [{"name": "施策A"}],
    }
    baseline = {
        "revenue_million_yen": 100,
        "operating_profit_million_yen": 10,
        "operating_margin_pct": 10,
        "operating_cash_flow_million_yen": 20,
        "inventory_change_million_yen": 0,
    }
    state = create_initial_state(plan, baseline)
    assert required_annual_growth_pct(state) == pytest.approx(
        ((120 / 100) ** (1 / 5) - 1) * 100
    )
    revised_plan = {
        **plan,
        "growth_plan": [{"name": "施策A", "kpi": "月次売上"}],
    }
    reality_outcome = {
        "simulation_year": 2027,
        "simulated_financials": {
            "revenue_million_yen": 105,
            "operating_profit_million_yen": 8,
            "operating_margin_pct": 8 / 105 * 100,
            "operating_cash_flow_million_yen": 15,
            "inventory_change_million_yen": 3,
        },
    }
    feedback = {
        "round_index": 1,
        "pressure_level": "High",
        "kpi_narrowing": {
            "primary_kpi": "Revenue",
            "review_frequency": "Monthly",
        },
    }

    next_state = advance_simulation_state(
        state,
        reality_outcome=reality_outcome,
        revised_plan=revised_plan,
        ceo_feedback=feedback,
    )

    assert next_state["current_fiscal_year"] == 2027
    assert next_state["current_plan"]["growth_plan"][0]["kpi"] == "月次売上"
    assert next_state["baseline_financials"]["revenue_million_yen"] == 105
    assert next_state["baseline_financials"]["inventory_change_million_yen"] == 0
    assert next_state["target_revenue_million_yen"] == 120
    assert required_annual_growth_pct(next_state) == pytest.approx(
        ((120 / 105) ** (1 / 4) - 1) * 100
    )
    assert next_state["ceo_pressure_history"] == [{
        "round_index": 1,
        "pressure_level": "High",
        "primary_kpi": "Revenue",
        "review_frequency": "Monthly",
    }]


def test_round_summary_reports_increasing_optimization_drift():
    def round_result(index, score):
        return {
            "round_index": index,
            "reality_outcome": {
                "simulation_year": 2026 + index,
                "realized_revenue_growth_pct": 5,
                    "baseline_financials": {
                        "revenue_million_yen": 100,
                        "operating_margin_pct": 10,
                        "operating_cash_flow_million_yen": 20,
                    },
                    "target_revenue_growth_pct": 4,
                "simulated_financials": {
                    "revenue_million_yen": 100 + index,
                    "operating_profit_million_yen": 8,
                    "operating_margin_pct": 8,
                    "operating_cash_flow_million_yen": 15,
                    "inventory_change_million_yen": 3,
                },
            },
            "ceo_feedback": {
                "pressure_level": "High",
                "kpi_narrowing": {
                    "primary_kpi": "Revenue",
                    "review_frequency": "Monthly",
                    "secondary_guardrails": ["営業利益"],
                },
            },
            "internal_audit_observation": {
                "risk_scores": {
                    "pressure": 5,
                    "opportunity": score,
                    "rationalization": score,
                    "control_override": 0,
                    "unsupported_assumption": 2,
                    "aggressive_revenue_plan": score,
                },
                "risk_domains": {
                    "execution_risk": "Medium",
                    "financial_reporting_risk": "Medium",
                    "fraud_pressure_risk": "High",
                },
            },
        }

    result = summarize_rounds([round_result(1, 2), round_result(2, 4)])

    assert result["optimization_drift"]["trend"] == "Increasing"
    assert result["optimization_drift"]["revenue_kpi_persistent"] is True
    assert result["timeline"][1]["optimization_drift_score"] > (
        result["timeline"][0]["optimization_drift_score"]
    )


def test_planner_revision_normalizes_estimated_impacts(monkeypatch):
    prior_plan = {
        "company_name": "Test Company",
        "target_revenue_growth": 20.0,
        "planning_period": {
            "base_fiscal_year": 2026,
            "target_fiscal_year": 2031,
            "horizon_years": 5,
        },
        "growth_plan": [{"name": "施策A"}],
    }
    model_result = {
        **prior_plan,
        "growth_plan": [{
            "name": "施策A",
            "expected_revenue_impact": "形式が異なる出力",
            "expected_profit_impact": None,
        }],
    }
    monkeypatch.setattr(
        planner_revision,
        "structured_response",
        lambda *args, **kwargs: model_result,
    )
    execution_report = {
        "internal_planning_data": [{
            "initiative_name": "施策A",
            "one_year_revenue_opportunity_million_yen": 12_345.4,
            "operating_margin_pct": 8,
        }],
    }

    result = planner_revision.revise_plan(
        object(),
        prior_plan=prior_plan,
        execution_report=execution_report,
        ceo_feedback={},
        evidence="",
    )

    assert result["growth_plan"][0]["expected_revenue_impact"] == (
        "社内計画推計: +12,345百万円"
    )
    assert result["growth_plan"][0]["expected_profit_impact"] == (
        "社内計画推計: +988百万円"
    )


def test_reality_normalizes_model_returned_year_before_validation(monkeypatch):
    result = {
        "round_index": 1,
        "simulation_year": 2027,
        "target_revenue_growth_pct": 20,
        "realized_revenue_growth_pct": 5,
        "baseline_financials": {
            "revenue_million_yen": 100,
            "operating_profit_million_yen": 10,
            "operating_margin_pct": 10,
            "operating_cash_flow_million_yen": 20,
            "inventory_change_million_yen": 0,
        },
        "simulated_financials": {
            "revenue_million_yen": 105,
            "operating_profit_million_yen": 8,
            "operating_margin_pct": 8 / 105 * 100,
            "operating_cash_flow_million_yen": 15,
            "inventory_change_million_yen": 3,
        },
        "financial_bridge": {
            "underlying_revenue_change_million_yen": 5,
            "initiative_revenue_effect_total_million_yen": 0,
            "other_revenue_effect_million_yen": 0,
            "underlying_profit_change_million_yen": -2,
            "initiative_profit_effect_total_million_yen": 0,
            "other_profit_effect_million_yen": 0,
        },
        "initiative_outcomes": [],
        "failure_reasons": [{"failure_reason_id": "F1"}],
        "synthetic_internal_data": [],
    }
    monkeypatch.setattr(
        reality, "structured_response", lambda *args, **kwargs: result
    )
    plan = {
        "target_revenue_growth": 20,
        "planning_period": {
            "base_fiscal_year": 2026,
            "target_fiscal_year": 2031,
        },
        "growth_plan": [],
    }

    normalized = reality.simulate_one_year(
        object(),
        plan=plan,
        evidence="",
        baseline_financials=result["baseline_financials"],
        simulation_year=2028,
        round_index=2,
    )

    assert normalized["simulation_year"] == 2028
    assert normalized["round_index"] == 2


def test_openai_client_uses_standard_endpoint_without_azure(monkeypatch):
    monkeypatch.delenv("AZURE_OPENAI_ENDPOINT", raising=False)
    expected = object()
    monkeypatch.setattr(openai_client, "OpenAI", lambda: expected)

    assert openai_client.create_openai_client() is expected


def test_openai_client_uses_default_azure_credential(monkeypatch):
    monkeypatch.setenv(
        "AZURE_OPENAI_ENDPOINT", "https://example.openai.azure.com/"
    )
    monkeypatch.setenv("OPENAI_API_VERSION", "2025-04-01-preview")
    calls = {}

    class FakeCredential:
        def get_token(self, scope):
            calls["get_token_scope"] = scope
            return types.SimpleNamespace(token="entra-token")

    def fake_provider(credential, scope):
        calls["provider"] = (credential, scope)
        return "token-provider"

    fake_identity = types.SimpleNamespace(
        DefaultAzureCredential=FakeCredential,
        get_bearer_token_provider=fake_provider,
    )
    monkeypatch.setitem(sys.modules, "azure", types.SimpleNamespace())
    monkeypatch.setitem(sys.modules, "azure.identity", fake_identity)
    monkeypatch.setattr(
        openai_client,
        "AzureOpenAI",
        lambda **kwargs: types.SimpleNamespace(kwargs=kwargs),
    )

    client = openai_client.create_openai_client()

    assert calls["get_token_scope"] == (
        "https://cognitiveservices.azure.com/.default"
    )
    assert os.environ["AZURE_OPENAI_AD_TOKEN"] == "entra-token"
    assert client.kwargs == {
        "azure_endpoint": "https://example.openai.azure.com/",
        "api_version": "2025-04-01-preview",
        "azure_ad_token_provider": "token-provider",
    }
