"""Per-source watermarks so a second import only re-embeds what changed."""

from __future__ import annotations

import sqlite3
import threading
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
    # Which reading of the file this is: the importer's VERSION.  A file
    # read by an older one is read again, unchanged or not.
    parser: int = 1


class SourceIndex:
    """One connection shared by every thread, behind a lock.

    JSON-RPC runs each method via asyncio.to_thread, so the index is
    opened on one worker thread and asked on another -- which sqlite3
    refuses unless told otherwise.  Same arrangement as herd.py.
    """

    def __init__(self, path: Path):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._mu = threading.Lock()
        self._conn = sqlite3.connect(self.path, check_same_thread=False)
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
        columns = {r[1] for r in self._conn.execute("PRAGMA table_info(sources)")}
        if "parser" not in columns:
            self._conn.execute("ALTER TABLE sources ADD COLUMN parser INTEGER NOT NULL DEFAULT 1")
        # The session's title, as its importer read it: "" for one that has
        # none, NULL for a row recorded before titles were kept (see
        # `set_titles').
        if "title" not in columns:
            self._conn.execute("ALTER TABLE sources ADD COLUMN title TEXT")
        self._conn.commit()

    def unchanged(self, meta: SourceMeta) -> bool:
        with self._mu:
            row = self._conn.execute(
                "SELECT mtime, size, parser FROM sources WHERE source_path = ?",
                (meta.path,),
            ).fetchone()
        if row is None:
            return False
        return abs(row[0] - meta.mtime) < 1e-6 and row[1] == meta.size and row[2] == meta.parser

    def record(self, meta: SourceMeta, chunks: int) -> None:
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            self._record(meta, chunks, now)

    def _record(self, meta: SourceMeta, chunks: int, now: str) -> None:
        "Caller holds `_mu`."
        self._conn.execute(
            """
            INSERT INTO sources (
                source_path, mtime, size, agent, session_id, project,
                kind, chunks, imported_at, parser, title
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(source_path) DO UPDATE SET
                mtime=excluded.mtime,
                size=excluded.size,
                agent=excluded.agent,
                session_id=excluded.session_id,
                project=excluded.project,
                kind=excluded.kind,
                chunks=excluded.chunks,
                imported_at=excluded.imported_at,
                parser=excluded.parser,
                title=excluded.title
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
                meta.parser,
                meta.title or "",
            ),
        )
        self._conn.commit()

    def counts_by_agent(self) -> dict[str, int]:
        with self._mu:
            rows = self._conn.execute(
                "SELECT agent, COUNT(*) FROM sources GROUP BY agent"
            ).fetchall()
        return {agent or "unknown": n for agent, n in rows}

    def list_sources(
        self,
        agent: str | None = None,
        project: str | None = None,
        limit: int = 200,
        offset: int = 0,
    ) -> tuple[list[dict], int]:
        where = ["COALESCE(chunks, 0) > 0"]
        args: list = []
        if agent:
            where.append("agent = ?")
            args.append(agent)
        if project:
            clause, more = _under(project)
            where.append(clause)
            args.extend(more)
        clause = " AND ".join(where)
        with self._mu:
            total, rows = self._list(clause, args, limit, offset)
        sources = [
            {
                "source_path": r[0],
                "mtime": r[1],
                "size": r[2],
                "agent": r[3] or "",
                "session_id": r[4] or "",
                "project": r[5] or "",
                "kind": r[6] or "",
                "chunks": r[7] or 0,
                "imported_at": r[8] or "",
                # None: not known yet -- recorded before titles were kept.
                "title": r[9],
            }
            for r in rows
        ]
        return sources, int(total)

    def rows(self) -> list[SourceMeta]:
        "Every source recorded, as its meta (sparse_migrate reads them all)."
        with self._mu:
            found = self._conn.execute(
                "SELECT source_path, mtime, size, agent, session_id, project,"
                " kind, parser, title FROM sources ORDER BY source_path"
            ).fetchall()
        return [SourceMeta(path=r[0], mtime=r[1], size=r[2], agent=r[3] or "",
                           session_id=r[4] or "", project=r[5] or "", kind=r[6] or "",
                           parser=r[7], title=r[8])
                for r in found]

    def forget(self, paths: list[str]) -> None:
        if not paths:
            return
        with self._mu:
            self._conn.executemany("DELETE FROM sources WHERE source_path = ?",
                                   [(p,) for p in paths])
            self._conn.commit()

    def backup(self, dest: Path) -> None:
        "A consistent copy of the index at DEST (SQLite's backup API)."
        with self._mu:
            out = sqlite3.connect(dest)
            try:
                self._conn.backup(out)
            finally:
                out.close()

    def projects_under(self, project: str) -> list[str]:
        """The project values recorded at or under PROJECT -- what a search
        in PROJECT filters chunks by, so that it holds the sessions the
        Sessions list shows for it (`list_sources')."""
        clause, args = _under(project)
        with self._mu:
            rows = self._conn.execute(
                f"SELECT DISTINCT project FROM sources WHERE {clause}", args
            ).fetchall()
        return sorted(r[0] for r in rows if r[0])

    def set_titles(self, titles: dict[str, str]) -> None:
        """Keep TITLES (source path -> title, "" for none) for rows that
        have none yet.  An import writes its own, so this never overwrites."""
        if not titles:
            return
        with self._mu:
            self._conn.executemany(
                "UPDATE sources SET title = ? WHERE source_path = ? AND title IS NULL",
                [(t or "", p) for p, t in titles.items()],
            )
            self._conn.commit()

    def _list(self, clause: str, args: list, limit: int, offset: int) -> tuple[int, list]:
        "Caller holds `_mu`."
        total = self._conn.execute(
            f"SELECT COUNT(*) FROM sources WHERE {clause}", args
        ).fetchone()[0]
        rows = self._conn.execute(
            f"""
            SELECT source_path, mtime, size, agent, session_id, project,
                   kind, chunks, imported_at, title
            FROM sources
            WHERE {clause}
            ORDER BY imported_at DESC
            LIMIT ? OFFSET ?
            """,
            [*args, int(limit), int(offset)],
        ).fetchall()
        return total, rows

    def close(self) -> None:
        with self._mu:
            self._conn.close()


def _under(project: str) -> tuple[str, list[str]]:
    """A WHERE clause for PROJECT and the directories under it: a session
    started in a subdirectory is the project's, and so is one recorded with
    a trailing slash (grok's git root).  LIKE's wildcards in a path are
    taken literally."""
    root = project.rstrip("/") or "/"
    escaped = root.replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_")
    prefix = escaped if escaped.endswith("/") else escaped + "/"
    return "(project = ? OR project LIKE ? ESCAPE '\\')", [root, prefix + "%"]
