"""Agy / Antigravity CLI conversations.

Each conversation writes a structured JSONL log under
``brain/<id>/.system_generated/logs/transcript.jsonl`` and a protobuf
sqlite blob under ``conversations/<id>.db``.  Prefer ``transcript.jsonl``
so user turns, assistant replies, tool calls, and step timestamps are
preserved; fall back to salvaging UTF-8 strings from ``conversations/*.db``
only when a conversation has no JSONL transcript.
``conversation_summaries.db`` holds titles and workspace URIs.
"""

from __future__ import annotations

import json
import re
import sqlite3
from collections.abc import Iterator
from pathlib import Path
from typing import Any

from chunk import Turn
from importers.textutil import (
    file_meta,
    project_from_file_uris,
    project_from_workspace_uris,
    salvage_blob,
    truncate_tool,
)
from index import SourceMeta

# Bumped when a transcript is read differently; files read before are
# read again.  2: read brain/<id>/.system_generated/logs/transcript.jsonl
# before falling back to conversations/<id>.db, and retire any .db whose
# session now comes from transcript.jsonl.
VERSION = 2

USER_REQUEST = re.compile(r"<USER_REQUEST>\s*(.*?)\s*</USER_REQUEST>", re.S)
WORKSPACE_CWD = re.compile(
    r"(?:Command Working Directory:\s*|Active Workspaces:\s*\n-\s*)(/[^\s\n<]+)"
)
HINT_KEYS = (
    "toolSummary",
    "description",
    "command",
    "CommandLine",
    "file_path",
    "TargetFile",
    "AbsolutePath",
    "DirectoryPath",
    "SearchPath",
    "path",
    "pattern",
    "Query",
    "query",
    "Url",
    "url",
    "Prompt",
    "prompt",
)


def iter_agy(
    root: Path, tool_truncate: int = 1500, max_line: int = 262144
) -> Iterator[tuple[SourceMeta, list[Turn]]]:
    root = Path(root)
    conv_dir = root / "conversations"
    brain_dir = root / "brain"
    summaries = _load_summaries(root / "conversation_summaries.db")

    transcripts: dict[str, Path] = {}
    if brain_dir.is_dir():
        for tx in sorted(brain_dir.glob("*/.system_generated/logs/transcript.jsonl")):
            if tx.is_file():
                transcripts[tx.parent.parent.parent.name] = tx

    dbs: dict[str, Path] = {}
    if conv_dir.is_dir():
        for db in sorted(conv_dir.glob("*.db")):
            if db.name.endswith(("-shm", "-wal")):
                continue
            dbs[db.stem] = db

    for session_id in sorted(transcripts.keys() | dbs.keys()):
        summary = summaries.get(session_id, {})
        tx = transcripts.get(session_id)
        db = dbs.get(session_id)
        if tx is not None:
            meta, turns = _from_transcript(tx, session_id, summary, tool_truncate, max_line)
            if turns or db is None:
                yield meta, turns
                if db is not None:
                    # Retire any chunks previously indexed from the .db for
                    # this session: mtime=0, size=0 with empty turns deletes
                    # stored chunks once and is skipped as unchanged thereafter.
                    yield (
                        SourceMeta(
                            path=str(db),
                            mtime=0.0,
                            size=0,
                            agent="agy",
                            session_id=session_id,
                            project=meta.project,
                            kind="transcript",
                            title=meta.title,
                            parser=VERSION,
                        ),
                        [],
                    )
                continue
        if db is not None:
            yield _from_db(db, summary)

    summary_db = root / "conversation_summaries.db"
    if summary_db.is_file():
        yield _from_summaries_db(summary_db, summaries)


def _unquote_args(raw: Any) -> dict[str, Any]:
    if isinstance(raw, str):
        try:
            raw = json.loads(raw)
        except ValueError:
            return {}
    if not isinstance(raw, dict):
        return {}
    out: dict[str, Any] = {}
    for k, v in raw.items():
        if isinstance(v, str) and len(v) >= 2 and v[0] in '"[{':
            try:
                out[k] = json.loads(v)
                continue
            except ValueError:
                pass
        out[k] = v
    return out


def _tool_hint(args: dict[str, Any]) -> str:
    for key in HINT_KEYS:
        v = args.get(key)
        if isinstance(v, str) and v.strip():
            line = next((l.strip() for l in v.splitlines() if l.strip()), "")
            return line if len(line) <= 160 else line[:159].rstrip() + "…"
    return ""


def _from_transcript(
    path: Path,
    session_id: str,
    summary: dict,
    tool_truncate: int = 1500,
    max_line: int = 262144,
) -> tuple[SourceMeta, list[Turn]]:
    title = summary.get("title") or None
    project = summary.get("project") or ""
    fallback_ts = summary.get("ts") or None
    turns: list[Turn] = []
    try:
        fh = path.open(encoding="utf-8", errors="replace")
    except OSError:
        return file_meta(path, "agy", session_id, project, title=title, parser=VERSION), turns
    with fh:
        for line in fh:
            if max_line and len(line) > max_line:
                continue
            line = line.strip()
            if not line:
                continue
            try:
                obj = json.loads(line)
            except json.JSONDecodeError:
                continue
            if not isinstance(obj, dict):
                continue
            kind = obj.get("type")
            source = obj.get("source")
            ts = obj.get("created_at") or fallback_ts
            if kind == "USER_INPUT":
                raw = str(obj.get("content") or "")
                if not project:
                    m_cwd = WORKSPACE_CWD.search(raw)
                    project = m_cwd.group(1) if m_cwd else project_from_file_uris(raw)
                m_req = USER_REQUEST.search(raw)
                text = (m_req.group(1) if m_req else raw).strip()
                if text:
                    turns.append(
                        Turn(
                            agent="agy",
                            role="user",
                            text=text,
                            project=project,
                            session_id=session_id,
                            source_path=str(path),
                            source_kind="transcript",
                            ts=ts,
                            title=title,
                        )
                    )
            elif kind == "PLANNER_RESPONSE" and source == "MODEL":
                parts: list[str] = []
                content = str(obj.get("content") or "").strip()
                if content:
                    parts.append(content)
                for call in obj.get("tool_calls") or []:
                    if not isinstance(call, dict):
                        continue
                    name = str(call.get("name") or "tool")
                    args = _unquote_args(call.get("args"))
                    if not project:
                        cwd = args.get("Cwd")
                        if isinstance(cwd, str) and cwd.startswith("/"):
                            project = str(Path(cwd))
                    hint = _tool_hint(args)
                    parts.append(f"[tool {name}] {hint}".strip())
                text = "\n".join(p for p in parts if p).strip()
                if text:
                    turns.append(
                        Turn(
                            agent="agy",
                            role="assistant",
                            text=text,
                            project=project,
                            session_id=session_id,
                            source_path=str(path),
                            source_kind="transcript",
                            ts=ts,
                            title=title,
                        )
                    )
            elif source == "MODEL":
                raw = str(obj.get("content") or "")
                text = truncate_tool(raw, tool_truncate)
                if text:
                    turns.append(
                        Turn(
                            agent="agy",
                            role="tool",
                            text=text,
                            project=project,
                            session_id=session_id,
                            source_path=str(path),
                            source_kind="transcript",
                            ts=ts,
                            title=title,
                        )
                    )
    if project:
        for turn in turns:
            if not turn.project:
                turn.project = project
    meta = file_meta(path, "agy", session_id, project, title=title, parser=VERSION)
    return meta, turns


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
        meta = file_meta(path, "agy", session_id, project, title=title, parser=VERSION)
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
    meta = file_meta(path, "agy", session_id, project, title=title, parser=VERSION)
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
    return file_meta(path, "agy", "summaries", "", kind="memory", parser=VERSION), turns


def _open(path: Path) -> sqlite3.Connection | None:
    try:
        return sqlite3.connect(f"file:{path}?mode=ro", uri=True)
    except sqlite3.Error:
        try:
            return sqlite3.connect(path)
        except sqlite3.Error:
            return None
