from pathlib import Path

import pytest

from business_planner.ingestion import chunk_documents, company_slug, load_documents
from business_planner.planner import parse_growth
from business_planner.retrieval import BM25Retriever


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
