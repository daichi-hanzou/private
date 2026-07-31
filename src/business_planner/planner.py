from __future__ import annotations

import json
import os
import re
from pathlib import Path

from openai import OpenAI

from .ingestion import chunk_documents, load_documents
from .executive_principles import (
    load_executive_principles,
    principles_instructions,
    resolve_principles_path,
)
from .openai_client import create_chat_client, create_embedding_client
from .retrieval import BM25Retriever, HybridRetriever
from .schema import BUSINESS_PLAN_SCHEMA

PRIMARY = {
    "01_company_profile", "02_financial_reports", "03_business_strategy",
    "04_business_risks", "07_structured_data",
}
EXTERNAL = {"05_peer_companies", "06_industry_market"}

SYSTEM_PROMPT = """あなたはBusiness Plannerです。
市場規模、競合、人的資源、財務余力、業界平均を考慮してください。
施策は相互に重複しない1～8件とし、売上効果の二重計上を避けてください。
初期計画ではplanning_option_idをnull、portfolio_actionをNew、
predecessor_initiative_namesを空配列としてください。
portfolio_decisionsには各初期施策をNewとして記録してください。
各施策には投資額、人員、販促費、生産能力配分をresource_allocationとして明示してください。"""

QUERIES = [
    ("ビジネスモデル 主要製品 顧客 収益源 競争優位", PRIMARY),
    ("売上 利益率 キャッシュフロー セグメント 財務推移", PRIMARY),
    ("成長戦略 中期経営計画 投資 人材 重点領域", PRIMARY),
    ("事業リスク 市場リスク 人材リスク 海外リスク", PRIMARY),
    ("競合 売上成長率 利益率 戦略 ベンチマーク", EXTERNAL),
    ("市場規模 市場成長率 業界平均 トレンド", EXTERNAL),
]
SOURCE_ID_PATTERN = re.compile(r"src_[a-f0-9]{12}")


def parse_growth(value: str) -> float:
    normalized = value.strip().removesuffix("%").strip()
    growth = float(normalized)
    if not 0 < growth <= 500:
        raise ValueError("target revenue growth must be greater than 0 and at most 500%")
    return growth


def validate_planning_period(
    base_fiscal_year: int | None, target_fiscal_year: int | None
) -> dict:
    if (base_fiscal_year is None) != (target_fiscal_year is None):
        raise ValueError("base and target fiscal years must be specified together")
    if base_fiscal_year is not None and target_fiscal_year <= base_fiscal_year:
        raise ValueError("target fiscal year must be after base fiscal year")
    return {
        "base_fiscal_year": base_fiscal_year,
        "target_fiscal_year": target_fiscal_year,
        "horizon_years": (
            target_fiscal_year - base_fiscal_year
            if base_fiscal_year is not None else None
        ),
    }


def openai_embedder(client: OpenAI, model: str | None = None):
    embedding_model = model or os.getenv(
        "OPENAI_EMBEDDING_MODEL", "text-embedding-3-large"
    )
    batch_size = 32

    def embed(texts):
        texts = list(texts)
        embeddings = []
        for start in range(0, len(texts), batch_size):
            response = client.embeddings.create(
                model=embedding_model,
                input=texts[start:start + batch_size],
                encoding_format="float",
            )
            embeddings.extend(
                item.embedding
                for item in sorted(response.data, key=lambda item: item.index)
            )
        return embeddings

    return embed


def make_retriever(chunks, mode: str = "hybrid", client: OpenAI | None = None):
    if mode == "bm25":
        return BM25Retriever(chunks)
    if mode != "hybrid":
        raise ValueError("retrieval mode must be 'hybrid' or 'bm25'")
    return HybridRetriever(
        chunks, openai_embedder(client or create_embedding_client())
    )


def retrieve_context(
    data_root: Path, company_name: str, per_query: int = 5,
    mode: str = "hybrid", client: OpenAI | None = None,
):
    retriever = make_retriever(
        chunk_documents(load_documents(data_root, company_name)), mode, client
    )
    selected = {}
    for query, categories in QUERIES:
        for chunk in retriever.search(query, per_query, categories):
            selected[(chunk.source_id, chunk.chunk_index)] = chunk
    return list(selected.values())


def build_user_prompt(
    company_name: str, growth: float, chunks, planning_period: dict
) -> str:
    evidence = "\n\n".join(
        f"[{chunk.source_id}] file={chunk.path.as_posix()} page={chunk.page or 'N/A'} "
        f"category={chunk.category}\n{chunk.text}"
        for chunk in chunks
    )
    return (
        f"企業名: {company_name}\n売上成長目標: {growth}%\n"
        f"計画期間: {json.dumps(planning_period, ensure_ascii=False)}\n\n"
        "以下の公開情報を分析し、指定JSON形式で現実的な計画を作成してください。\n\n"
        f"根拠資料:\n{evidence}"
    )


def collect_source_ids(value: object) -> set[str]:
    if isinstance(value, dict):
        return set().union(*(collect_source_ids(item) for item in value.values()))
    if isinstance(value, list):
        return set().union(*(collect_source_ids(item) for item in value))
    if isinstance(value, str):
        return set(SOURCE_ID_PATTERN.findall(value))
    return set()


def remove_unknown_source_ids(
    value: object, allowed_source_ids: set[str]
) -> object:
    """Remove unsupported model-generated citations without inventing a mapping."""
    if isinstance(value, dict):
        return {
            key: remove_unknown_source_ids(item, allowed_source_ids)
            for key, item in value.items()
        }
    if isinstance(value, list):
        return [
            remove_unknown_source_ids(item, allowed_source_ids)
            for item in value
            if not (
                isinstance(item, str)
                and SOURCE_ID_PATTERN.fullmatch(item)
                and item not in allowed_source_ids
            )
        ]
    if isinstance(value, str):
        return SOURCE_ID_PATTERN.sub(
            lambda match: (
                match.group(0)
                if match.group(0) in allowed_source_ids
                else ""
            ),
            value,
        )
    return value


def generate_plan(
    data_root: Path, company_name: str, target_growth: str,
    model: str | None = None, retrieval_mode: str = "hybrid",
    base_fiscal_year: int | None = None, target_fiscal_year: int | None = None,
    principles_file: Path | None = None,
    use_executive_principles: bool = True,
    retrieval_limit: int = 5,
) -> dict:
    if not 1 <= retrieval_limit <= 50:
        raise ValueError("--retrieval-limit must be between 1 and 50")
    growth = parse_growth(target_growth)
    planning_period = validate_planning_period(base_fiscal_year, target_fiscal_year)
    client = create_chat_client()
    embedding_client = (
        create_embedding_client() if retrieval_mode == "hybrid" else None
    )
    chunks = retrieve_context(
        data_root,
        company_name,
        per_query=retrieval_limit,
        mode=retrieval_mode,
        client=embedding_client,
    )
    if not chunks:
        raise ValueError("No relevant evidence was retrieved")
    principles_path = (
        resolve_principles_path(data_root, company_name, principles_file)
        if use_executive_principles
        else None
    )
    executive_principles = load_executive_principles(principles_path)
    response = client.responses.create(
        model=model or os.getenv("OPENAI_MODEL", "gpt-5.6"),
        instructions=(
            principles_instructions(executive_principles) + SYSTEM_PROMPT
        ),
        input=build_user_prompt(company_name, growth, chunks, planning_period),
        text={
            "format": {
                "type": "json_schema",
                "name": "business_plan",
                "strict": True,
                "schema": BUSINESS_PLAN_SCHEMA,
            }
        },
    )
    result = json.loads(response.output_text)
    result["planning_period"] = planning_period
    citations = {chunk.source_id: chunk.citation() for chunk in chunks}
    # The model occasionally emits a source-shaped ID that was not included in
    # the retrieved evidence. Drop only that unsupported reference, then rebuild
    # the source catalog from citations that were actually supplied.
    result["sources"] = []
    result = remove_unknown_source_ids(result, set(citations))
    referenced = collect_source_ids(result)
    result["sources"] = [citations[source] for source in sorted(referenced)]
    return result
