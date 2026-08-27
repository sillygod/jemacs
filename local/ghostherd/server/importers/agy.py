"""Agy / Antigravity CLI conversations.

The store is sqlite with protobuf blobs.  There is no public schema, so
this salvages UTF-8 strings rather than pretending to decode protobuf.
conversation_summaries.db holds titles and workspace URIs.
"""

from __future__ import annotations

import sqlite3
from collections.abc import Iterator
from pathlib import Path

from chunk import Turn
from importers.textutil import (
    file_meta,
    project_from_file_uris,
    project_from_workspace_uris,
    salvage_blob,
)
from index import SourceMeta


def iter_agy(
    root: Path, tool_truncate: int = 1500, max_line: int = 262144
) -> Iterator[tuple[SourceMeta, list[Turn]]]:
    del tool_truncate, max_line  # salvage already drops binary; truncation is by prose length
    root = Path(root)
    conv_dir = root / "conversations"
    summaries = _load_summaries(root / "conversation_summaries.db")
    if conv_dir.is_dir():
        for db in sorted(conv_dir.glob("*.db")):
            if db.name.endswith(("-shm", "-wal")):
                continue
            yield _from_db(db, summaries.get(db.stem, {}))
    summary_db = root / "conversation_summaries.db"
    if summary_db.is_file():
        yield _from_summaries_db(summary_db, summaries)


def _load_summaries(path: Path) -> dict[str, dict]:
    if not path.is_file():
        return {}
    out: dict[str, dict] = {}
    try:
        conn = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
    except sqlite3.Error:
        try:
            conn = sqlite3.connect(path)
        except sqlite3.Error:
            return {}
    try:
        rows = conn.execute(
            """
            SELECT conversation_id, title, preview, workspace_uris,
                   last_modified_time
            FROM conversation_summaries
            """
        ).fetchall()
    except sqlite3.Error:
        conn.close()
        return {}
    for conv_id, title, preview, uris, ts in rows:
        out[str(conv_id)] = {
            "title": title or "",
            "preview": preview or "",
            "project": project_from_workspace_uris(uris),
            "ts": ts or "",
        }
    conn.close()
    return out


def _from_db(path: Path, summary: dict) -> tuple[SourceMeta, list[Turn]]:
    session_id = path.stem
    title = summary.get("title") or None
    project = summary.get("project") or ""
    turns: list[Turn] = []
    conn = _open(path)
    if conn is None:
        meta = file_meta(path, "agy", session_id, project, title=title)
        return meta, turns
    try:
        try:
            rows = conn.execute(
                "SELECT idx, step_payload FROM steps ORDER BY idx"
            ).fetchall()
        except sqlite3.Error:
            rows = []
        blobs = [row[1] for row in rows]
        if not blobs:
            try:
                blobs = [
                    row[0]
                    for row in conn.execute("SELECT data FROM trajectory_metadata_blob")
                ]
            except sqlite3.Error:
                blobs = []
        for blob in blobs:
            text = salvage_blob(blob if isinstance(blob, (bytes, bytearray)) else None)
            if not text:
                continue
            if not project:
                project = project_from_file_uris(text)
            turns.append(
                Turn(
                    agent="agy",
                    role="assistant",
                    text=text,
                    project=project,
                    session_id=session_id,
                    source_path=str(path),
                    source_kind="transcript",
                    ts=summary.get("ts") or None,
                    title=title,
                )
            )
    finally:
        conn.close()
    meta = file_meta(path, "agy", session_id, project, title=title)
    return meta, turns


def _from_summaries_db(
    path: Path, summaries: dict[str, dict]
) -> tuple[SourceMeta, list[Turn]]:
    turns: list[Turn] = []
    for conv_id, info in summaries.items():
        parts = [p for p in (info.get("title"), info.get("preview")) if p]
        text = "\n".join(parts).strip()
        if not text:
            continue
        turns.append(
            Turn(
                agent="agy",
                role="memory",
                text=text,
                project=info.get("project") or "",
                session_id=conv_id,
                source_path=str(path),
                source_kind="memory",
                ts=info.get("ts") or None,
                title=info.get("title") or None,
            )
        )
    return file_meta(path, "agy", "summaries", "", kind="memory"), turns


def _open(path: Path) -> sqlite3.Connection | None:
    try:
        return sqlite3.connect(f"file:{path}?mode=ro", uri=True)
    except sqlite3.Error:
        try:
            return sqlite3.connect(path)
        except sqlite3.Error:
            return None
