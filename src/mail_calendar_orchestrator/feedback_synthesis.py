from __future__ import annotations

import json
import unicodedata
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Any, Protocol
from uuid import uuid4

from .review_models import ReviewCaseRecord
from .review_service import ClassificationReviewService
from .state import MailStateStore


EVIDENCE_PROMPT = """Extract only the minimal email excerpts needed to understand the
human comment. Snippets must be short verbatim substrings of analysis_text. Do not
summarize the whole email, infer missing text, expose chain-of-thought, or return prose
outside the schema."""
POLICY_PROMPT = """Generalize reviewed human feedback into conservative policy candidates.
Merge similar feedback, retain supporting review IDs and evidence, identify conflicts and
time-ordered preference changes, and respect the existing taxonomy. Do not overgeneralize
a single case or propose unsupported policy. Return only the schema; do not expose
chain-of-thought or modify code, prompts, validators, or deployments."""

EVIDENCE_SCHEMA = {
    "type": "object",
    "properties": {
        "evidence_snippets": {"type": "array", "items": {"type": "string"}},
        "evidence_summary": {"type": "string"},
    },
    "required": ["evidence_snippets", "evidence_summary"],
    "additionalProperties": False,
}
POLICY_SCHEMA = {
    "type": "object",
    "properties": {
        "policy_candidates": {"type": "array", "items": {
            "type": "object", "properties": {
                "policy_id": {"type": "string"}, "summary": {"type": "string"},
                "supporting_cases": {"type": "array", "items": {"type": "string"}},
                "supporting_evidence": {"type": "array", "items": {"type": "string"}},
                "confidence": {"type": "string", "enum": ["low", "medium", "high"]},
                "recommended_targets": {"type": "array", "items": {"type": "string"}},
            }, "required": ["policy_id", "summary", "supporting_cases",
                "supporting_evidence", "confidence", "recommended_targets"],
            "additionalProperties": False,
        }},
        "conflicts": {"type": "array", "items": {"type": "object", "properties": {
            "summary": {"type": "string"},
            "supporting_cases": {"type": "array", "items": {"type": "string"}},
        }, "required": ["summary", "supporting_cases"], "additionalProperties": False}},
        "preference_changes": {"type": "array", "items": {"type": "object", "properties": {
            "older_policy": {"type": "string"}, "newer_policy": {"type": "string"},
            "supporting_cases": {"type": "array", "items": {"type": "string"}},
        }, "required": ["older_policy", "newer_policy", "supporting_cases"],
            "additionalProperties": False}},
    },
    "required": ["policy_candidates", "conflicts", "preference_changes"],
    "additionalProperties": False,
}


class FeedbackClient(Protocol):
    model_name: str
    def extract_evidence(self, payload: dict[str, Any]) -> dict[str, Any]: ...
    def synthesize(self, payload: list[dict[str, Any]]) -> dict[str, Any]: ...


class OpenAIFeedbackClient:
    def __init__(self, openai_client: Any, model_name: str) -> None:
        self.client = openai_client
        self.model_name = model_name

    def _call(self, prompt: str, payload: object, schema: dict[str, Any], name: str) -> dict[str, Any]:
        response = self.client.chat.completions.create(
            model=self.model_name,
            response_format={"type": "json_schema", "json_schema": {
                "name": name, "strict": True, "schema": schema,
            }},
            messages=[{"role": "system", "content": prompt}, {
                "role": "user", "content": json.dumps(payload, ensure_ascii=False),
            }],
        )
        return json.loads(response.choices[0].message.content or "{}")

    def extract_evidence(self, payload: dict[str, Any]) -> dict[str, Any]:
        return self._call(EVIDENCE_PROMPT, payload, EVIDENCE_SCHEMA, "feedback_evidence")

    def synthesize(self, payload: list[dict[str, Any]]) -> dict[str, Any]:
        return self._call(POLICY_PROMPT, payload, POLICY_SCHEMA, "feedback_policy")


@dataclass(frozen=True)
class SynthesisResult:
    synthesis_id: str
    selected: int
    evidence_created: int
    evidence_reused: int
    policy_candidates: int


class FeedbackSynthesisService:
    def __init__(
        self, state: MailStateStore, reviews: ClassificationReviewService,
        client: FeedbackClient, *, evidence_version: str = "v1",
        synthesis_version: str = "v1",
    ) -> None:
        self.state = state
        self.reviews = reviews
        self.client = client
        self.evidence_version = evidence_version
        self.synthesis_version = synthesis_version

    def synthesize(self, *, since: str | None = None, limit: int = 50) -> SynthesisResult:
        cases = self._select_cases(since=since, limit=limit)
        evidence_created = evidence_reused = 0
        inputs: list[dict[str, Any]] = []
        for case in cases:
            evidence = self._existing_evidence(case.review_case_id)
            if evidence is None:
                evidence = self._extract(case)
                evidence_created += 1
            else:
                evidence_reused += 1
            if evidence["grounding_status"] == "grounded":
                inputs.append(evidence)
        result = self.client.synthesize(inputs) if inputs else {
            "policy_candidates": [], "conflicts": [], "preference_changes": [],
        }
        result = _sanitize_policy_result(result, inputs)
        synthesis_id = "SYN-" + uuid4().hex[:12].upper()
        now = datetime.now(timezone.utc).isoformat()
        with self.state.connection:
            self.state.connection.execute(
                "INSERT INTO review_policy_syntheses(synthesis_id,synthesis_version,"
                "synthesizer_model,time_window,review_version,conflicts_json,"
                "preference_changes_json,created_at) VALUES(?,?,?,?,?,?,?,?)",
                (synthesis_id, self.synthesis_version, self.client.model_name,
                 since or "all", self.reviews.review_version,
                 json.dumps(result.get("conflicts", []), ensure_ascii=False),
                 json.dumps(result.get("preference_changes", []), ensure_ascii=False), now),
            )
            for candidate in result.get("policy_candidates", []):
                self.state.connection.execute(
                    "INSERT INTO review_policy_candidates(synthesis_id,policy_id,summary,"
                    "confidence,recommended_targets_json,supporting_review_ids_json,"
                    "supporting_evidence_json,status) VALUES(?,?,?,?,?,?,?,?)",
                    (synthesis_id, str(candidate["policy_id"])[:120],
                     str(candidate["summary"])[:2000], candidate["confidence"],
                     json.dumps(candidate["recommended_targets"], ensure_ascii=False),
                     json.dumps(candidate["supporting_cases"], ensure_ascii=False),
                     json.dumps(candidate["supporting_evidence"], ensure_ascii=False),
                     "proposed"),
                )
        return SynthesisResult(synthesis_id, len(cases), evidence_created,
                               evidence_reused, len(result.get("policy_candidates", [])))

    def _select_cases(self, *, since: str | None, limit: int) -> list[ReviewCaseRecord]:
        clauses = ["review_version=?", "human_review_status='resolved'",
                   "human_comment IS NOT NULL", "trim(human_comment)<>''"]
        params: list[Any] = [self.reviews.review_version]
        if since:
            clauses.append("human_reviewed_at>=?")
            params.append(since)
        params.append(limit)
        rows = self.state.connection.execute(
            "SELECT * FROM review_cases WHERE " + " AND ".join(clauses)
            + " ORDER BY human_reviewed_at ASC LIMIT ?", tuple(params)
        )
        return [self.reviews._record(row) for row in rows]

    def _existing_evidence(self, review_case_id: str) -> dict[str, Any] | None:
        row = self.state.connection.execute(
            "SELECT * FROM review_feedback_evidence WHERE review_case_id=? "
            "AND evidence_version=?", (review_case_id, self.evidence_version),
        ).fetchone()
        if row is None:
            return None
        case = self.reviews.get_case(review_case_id)
        return self._policy_input(case, json.loads(row["evidence_snippets_json"]),
                                  str(row["evidence_summary"]), str(row["grounding_status"]))

    def _extract(self, case: ReviewCaseRecord) -> dict[str, Any]:
        detail = self.reviews.get_case_detail(case.review_case_id)
        context = detail["context"]
        payload = {
            "review_id": case.review_case_id, "subject": context["subject"],
            "analysis_text": context["analysis_text"],
            "original_classification": context["original_classification"],
            "system_final_classification": context["final_classification"],
            "reviewer_suggested_classification": case.suggested_classification,
            "human_verdict": case.human_verdict,
            "human_final_classification": case.human_final_classification,
            "human_comment": case.human_comment,
            "recommended_change_target": case.recommended_change_target,
            "reviewed_at": case.human_reviewed_at.isoformat() if case.human_reviewed_at else None,
        }
        extracted = self.client.extract_evidence(payload)
        snippets = _ground_snippets(context["analysis_text"], extracted.get("evidence_snippets", []))
        status = "grounded" if snippets else "evidence_uncertain"
        summary = str(extracted.get("evidence_summary") or "")[:1000]
        with self.state.connection:
            self.state.connection.execute(
                "INSERT OR IGNORE INTO review_feedback_evidence(review_case_id,"
                "evidence_version,extractor_model,evidence_snippets_json,evidence_summary,"
                "grounding_status,created_at) VALUES(?,?,?,?,?,?,?)",
                (case.review_case_id, self.evidence_version, self.client.model_name,
                 json.dumps(snippets, ensure_ascii=False), summary, status,
                 datetime.now(timezone.utc).isoformat()),
            )
        return self._policy_input(case, snippets, summary, status)

    @staticmethod
    def _policy_input(case: ReviewCaseRecord, snippets: list[str], summary: str,
                      grounding_status: str) -> dict[str, Any]:
        return {"review_id": case.review_case_id, "human_verdict": case.human_verdict,
                "final_classification": case.human_final_classification,
                "human_comment": case.human_comment, "reviewed_at": (
                    case.human_reviewed_at.isoformat() if case.human_reviewed_at else None),
                "recommended_change_target": case.recommended_change_target,
                "evidence_snippets": snippets, "evidence_summary": summary,
                "grounding_status": grounding_status}

    def export(self, *, synthesis_id: str | None = None, format: str = "markdown") -> str:
        if synthesis_id is None:
            row = self.state.connection.execute(
                "SELECT synthesis_id FROM review_policy_syntheses ORDER BY created_at DESC LIMIT 1"
            ).fetchone()
            if row is None:
                raise ValueError("no policy synthesis found")
            synthesis_id = str(row["synthesis_id"])
        synthesis = self.state.connection.execute(
            "SELECT * FROM review_policy_syntheses WHERE synthesis_id=?", (synthesis_id,)
        ).fetchone()
        if synthesis is None:
            raise ValueError(f"policy synthesis not found: {synthesis_id}")
        candidates = [dict(row) for row in self.state.connection.execute(
            "SELECT * FROM review_policy_candidates WHERE synthesis_id=? ORDER BY policy_id",
            (synthesis_id,),
        )]
        safe_candidates = []
        for row in candidates:
            supporting_cases = json.loads(row["supporting_review_ids_json"])
            human_comments = []
            for review_id in supporting_cases:
                comment_row = self.state.connection.execute(
                    "SELECT human_comment FROM review_cases WHERE review_case_id=?",
                    (review_id,),
                ).fetchone()
                if comment_row is not None and comment_row["human_comment"]:
                    human_comments.append(str(comment_row["human_comment"]))
            safe_candidates.append({"policy_id": row["policy_id"], "summary": row["summary"],
                "confidence": row["confidence"], "status": row["status"],
                "recommended_targets": json.loads(row["recommended_targets_json"]),
                "supporting_cases": supporting_cases, "human_comments": human_comments,
                "supporting_evidence": json.loads(row["supporting_evidence_json"])})
        safe = {"synthesis_id": synthesis_id, "synthesis_version": synthesis["synthesis_version"],
                "review_version": synthesis["review_version"], "created_at": synthesis["created_at"],
                "policy_candidates": safe_candidates,
                "conflicts": json.loads(synthesis["conflicts_json"]),
                "preference_changes": json.loads(synthesis["preference_changes_json"])}
        if format == "json":
            return json.dumps(safe, ensure_ascii=False, indent=2)
        lines = ["# AgentLedger Human Feedback Policy Proposal", ""]
        for index, item in enumerate(safe["policy_candidates"], 1):
            lines += [f"## Policy {index}: {item['policy_id']}", "", "Summary:",
                      str(item["summary"]), "", "Supporting cases:"]
            lines += [f"- {value}" for value in item["supporting_cases"]]
            lines += ["", "Human comments:"] + [f"- {value}" for value in item["human_comments"]]
            lines += ["", "Evidence:"] + [f"- {value}" for value in item["supporting_evidence"]]
            lines += ["", "Suggested targets:"] + [f"- {value}" for value in item["recommended_targets"]]
            lines.append("")
        return "\n".join(lines)


def _ground_snippets(text: str, snippets: list[Any]) -> list[str]:
    normalized_text = unicodedata.normalize("NFKC", text)
    grounded: list[str] = []
    for value in snippets:
        snippet = str(value).strip()
        if snippet and len(snippet) <= 500 and unicodedata.normalize("NFKC", snippet) in normalized_text:
            if snippet not in grounded:
                grounded.append(snippet)
    return grounded


def _sanitize_policy_result(
    result: dict[str, Any], inputs: list[dict[str, Any]],
) -> dict[str, Any]:
    allowed_cases = {str(item["review_id"]) for item in inputs}
    allowed_evidence = {
        str(value) for item in inputs for value in item["evidence_snippets"]
    }
    candidates = []
    seen = set()
    for item in result.get("policy_candidates", []):
        policy_id = str(item.get("policy_id") or "")[:120]
        cases = list(dict.fromkeys(
            value for value in item.get("supporting_cases", [])
            if value in allowed_cases
        ))
        evidence = list(dict.fromkeys(
            value for value in item.get("supporting_evidence", [])
            if value in allowed_evidence
        ))
        if not policy_id or policy_id in seen or len(cases) < 2 or not evidence:
            continue
        seen.add(policy_id)
        candidates.append({**item, "policy_id": policy_id,
                           "supporting_cases": cases,
                           "supporting_evidence": evidence})
    def grounded_relations(name: str) -> list[dict[str, Any]]:
        values = []
        for item in result.get(name, []):
            cases = list(dict.fromkeys(
                value for value in item.get("supporting_cases", [])
                if value in allowed_cases
            ))
            if len(cases) >= 2:
                values.append({**item, "supporting_cases": cases})
        return values
    return {"policy_candidates": candidates,
            "conflicts": grounded_relations("conflicts"),
            "preference_changes": grounded_relations("preference_changes")}
