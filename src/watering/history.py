from __future__ import annotations

import json
import sqlite3
from pathlib import Path
from typing import Any


class WateringHistory:
    def __init__(self, database: Path, log_path: Path) -> None:
        self.database = database
        self.log_path = log_path
        database.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
        log_path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
        self.connection = sqlite3.connect(database)
        self.connection.execute("""
            CREATE TABLE IF NOT EXISTS scheduled_watering (
                local_date TEXT PRIMARY KEY,
                request_id TEXT NOT NULL,
                status TEXT NOT NULL,
                created_at TEXT NOT NULL
            )
        """)

    def claim_day(self, local_date: str, request_id: str, created_at: str) -> bool:
        with self.connection:
            cursor = self.connection.execute(
                "INSERT OR IGNORE INTO scheduled_watering VALUES(?,?,?,?)",
                (local_date, request_id, "claimed", created_at),
            )
        return cursor.rowcount == 1

    def set_status(self, local_date: str, status: str) -> None:
        with self.connection:
            self.connection.execute(
                "UPDATE scheduled_watering SET status=? WHERE local_date=?",
                (status, local_date),
            )

    def append(self, event: dict[str, Any]) -> None:
        with self.log_path.open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(event, ensure_ascii=False, separators=(",", ":")) + "\n")

    def close(self) -> None:
        self.connection.close()

    def __enter__(self) -> "WateringHistory":
        return self

    def __exit__(self, *_args: object) -> None:
        self.close()
