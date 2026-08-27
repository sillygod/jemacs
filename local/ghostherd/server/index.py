"""Per-source watermarks so a second import only re-embeds what changed."""

from __future__ import annotations

import sqlite3
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path


@dataclass
class SourceMeta:
    path: str
    mtime: float
    size: int
    agent: str
    session_id: str
    project: str
    kind: str
    title: str | None = None


class SourceIndex:
    def __init__(self, path: Path):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._conn = sqlite3.connect(self.path)
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS sources (
                source_path TEXT PRIMARY KEY,
                mtime REAL NOT NULL,
                size INTEGER NOT NULL,
                agent TEXT,
                session_id TEXT,
                project TEXT,
                kind TEXT,
                chunks INTEGER,
                imported_at TEXT
            )
            """
        )
        self._conn.commit()

    def unchanged(self, meta: SourceMeta) -> bool:
        row = self._conn.execute(
            "SELECT mtime, size FROM sources WHERE source_path = ?",
            (meta.path,),
        ).fetchone()
        if row is None:
            return False
        return abs(row[0] - meta.mtime) < 1e-6 and row[1] == meta.size

    def record(self, meta: SourceMeta, chunks: int) -> None:
        now = datetime.now(timezone.utc).isoformat()
        self._conn.execute(
            """
            INSERT INTO sources (
                source_path, mtime, size, agent, session_id, project,
                kind, chunks, imported_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(source_path) DO UPDATE SET
                mtime=excluded.mtime,
                size=excluded.size,
                agent=excluded.agent,
                session_id=excluded.session_id,
                project=excluded.project,
                kind=excluded.kind,
                chunks=excluded.chunks,
                imported_at=excluded.imported_at
            """,
            (
                meta.path,
                meta.mtime,
                meta.size,
                meta.agent,
                meta.session_id,
                meta.project,
                meta.kind,
                chunks,
                now,
            ),
        )
        self._conn.commit()

    def counts_by_agent(self) -> dict[str, int]:
        rows = self._conn.execute(
            "SELECT agent, COUNT(*) FROM sources GROUP BY agent"
        ).fetchall()
        return {agent or "unknown": n for agent, n in rows}

    def close(self) -> None:
        self._conn.close()
