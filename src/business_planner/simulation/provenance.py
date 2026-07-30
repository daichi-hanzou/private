from __future__ import annotations

import hashlib
from pathlib import Path

from ..ingestion import SUPPORTED, company_slug


def document_snapshot(data_root: Path, company_name: str) -> dict:
    company_dir = data_root / company_slug(company_name)
    files = []
    combined = hashlib.sha256()
    for path in sorted(company_dir.rglob("*")):
        if not path.is_file() or path.suffix.lower() not in SUPPORTED:
            continue
        relative = path.relative_to(company_dir).as_posix()
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        files.append({"file": relative, "sha256": digest, "bytes": path.stat().st_size})
        combined.update(relative.encode("utf-8"))
        combined.update(digest.encode("ascii"))
    return {"files": files, "content_hash": combined.hexdigest()}
