from __future__ import annotations

import json
import os
from pathlib import Path

from openai import OpenAI

from .ingestion import chunk_documents, load_documents
from .retrieval import BM25Retriever
from .schema import BUSINESS_PLAN_SCHEMA

PRIMARY = {
    "01_company_profile", "02_financial_reports", "03_business_strategy",
    "04_business_risks", "07_structured_data",
}
EXTERNAL = {"05_peer_companies", "06_industry_market"}

SYSTEM_PROMPT = """あなたは公開情報だけを使う慎重なBusiness Plannerです。
与えられた根拠以外を事実として述べてはいけません。全ての主要判断にsource_idを付けます。
市場規模、競合、人的資源、財務余力、業界平均、会計ルール、内部統制を考慮してください。
内部統制の回避、不正、会計操作を提案してはいけません。
目標が根拠上困難なら率直にLowと評価してください。
数値を特定できない場合は捏造せずnullを返し、descriptionまたはrationaleで不足を説明します。
施策は相互に重複しない3件とし、売上効果の二重計上を避けてください。"""

QUERIES = [
    ("ビジネスモデル 主要製品 顧客 収益源 競争優位", PRIMARY),
    ("売上 利益率 キャッシュフロー セグメント 財務推移", PRIMARY),
    ("成長戦略 中期経営計画 投資 人材 重点領域", PRIMARY),
    ("事業リスク 市場リスク 人材リスク 海外リスク", PRIMARY),
    ("競合 売上成長率 利益率 戦略 ベンチマーク", EXTERNAL),
    ("市場規模 市場成長率 業界平均 トレンド", EXTERNAL),
]


def parse_growth(value: str) -> float:
    normalized = value.strip().removesuffix("%").strip()
    growth = float(normalized)
    if not 0 < growth <= 500:
        raise ValueError("target revenue growth must be greater than 0 and at most 500%")
    return growth


def retrieve_context(data_root: Path, company_name: str, per_query: int = 5):
    retriever = BM25Retriever(chunk_documents(load_documents(data_root, company_name)))
    selected = {}
    for query, categories in QUERIES:
        for chunk in retriever.search(query, per_query, categories):
            selected[(chunk.source_id, chunk.chunk_index)] = chunk
    return list(selected.values())


def build_user_prompt(company_name: str, growth: float, chunks) -> str:
    evidence = "\n\n".join(
        f"[{chunk.source_id}] file={chunk.path.as_posix()} page={chunk.page or 'N/A'} "
        f"category={chunk.category}\n{chunk.text}"
        for chunk in chunks
    )
    return (
        f"企業名: {company_name}\n売上成長目標: {growth}%\n\n"
        "以下の公開情報を分析し、指定JSON形式で現実的な計画を作成してください。\n\n"
        f"根拠資料:\n{evidence}"
    )


def generate_plan(
    data_root: Path, company_name: str, target_growth: str,
    model: str | None = None,
) -> dict:
    growth = parse_growth(target_growth)
    chunks = retrieve_context(data_root, company_name)
    if not chunks:
        raise ValueError("No relevant evidence was retrieved")
    client = OpenAI()
    response = client.responses.create(
        model=model or os.getenv("OPENAI_MODEL", "gpt-5.6"),
        instructions=SYSTEM_PROMPT,
        input=build_user_prompt(company_name, growth, chunks),
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
    citations = {chunk.source_id: chunk.citation() for chunk in chunks}
    referenced = set()
    for plan in result["growth_plan"]:
        referenced.update(plan["evidence_source_ids"])
    referenced.update(result["feasibility_assessment"]["evidence_source_ids"])
    result["sources"] = [citations[source] for source in referenced if source in citations]
    return result
