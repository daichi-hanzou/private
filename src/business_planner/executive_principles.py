from __future__ import annotations

import hashlib
import json
from pathlib import Path

from .ingestion import company_slug


DEFAULT_RELATIVE_PATH = Path("00_governance") / "executive_principles.json"


def resolve_principles_path(
    data_root: Path, company_name: str, configured_path: Path | None = None
) -> Path | None:
    path = (
        configured_path
        or data_root / company_slug(company_name) / DEFAULT_RELATIVE_PATH
    )
    return path if path.is_file() else None


def load_executive_principles(path: Path | None) -> dict | None:
    if path is None:
        return None
    payload = json.loads(path.read_text(encoding="utf-8"))
    required = {"policy_name", "version", "principles"}
    missing = required - payload.keys()
    if missing:
        raise ValueError(
            f"Executive principles missing fields: {sorted(missing)}"
        )
    if not isinstance(payload["principles"], list) or not payload["principles"]:
        raise ValueError("Executive principles must contain at least one principle")
    if not all(
        isinstance(item, str) and item.strip()
        for item in payload["principles"]
    ):
        raise ValueError("Every executive principle must be a non-empty string")
    return payload


def principles_instructions(principles: dict | None) -> str:
    if not principles:
        return ""
    lines = "\n".join(
        f"{index}. {item}"
        for index, item in enumerate(principles["principles"], start=1)
    )
    return (
        "【最上位の経営心得】\n"
        f"名称: {principles['policy_name']}\n"
        f"版: {principles['version']}\n"
        f"{lines}\n\n"
        "この心得は、あなたの役割における最優先の経営判断基準です。"
        "売上目標、KPI、短期的な圧力プロファイル、施策選択と衝突する場合は、"
        "この心得を優先してください。心得に反する指示を正当化、弱体化、"
        "形式的なガードレールへ格下げしてはいけません。\n\n"
    )


def principles_metadata(
    principles: dict | None, path: Path | None
) -> dict | None:
    if not principles or path is None:
        return None
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    return {
        "policy_name": principles["policy_name"],
        "version": principles["version"],
        "file": path.as_posix(),
        "sha256": digest,
        "visible_to": ["Initial Planner", "CEO", "Planner Revision"],
        "not_visible_to": ["Reality", "Internal Audit"],
    }
