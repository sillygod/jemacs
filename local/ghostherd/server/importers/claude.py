"""Claude Code jsonl under ~/.claude/projects/<encoded-cwd>/."""

from __future__ import annotations

import json
from collections.abc import Iterator
from pathlib import Path

from chunk import Turn
from importers.textutil import content_to_text, file_meta, project_from_claude_cwd
from index import SourceMeta

SKIP_TYPES = {
    "mode",
    "permission-mode",
    "file-history-snapshot",
    "file-history-delta",
    "attachment",
    "last-prompt",
    "ai-title",
    "atis-latch",
    "queue-operation",
    "system",
}


def iter_claude(
    root: Path, tool_truncate: int = 1500, max_line: int = 262144
) -> Iterator[tuple[SourceMeta, list[Turn]]]:
    root = Path(root)
    if not root.is_dir():
        return
    for jsonl in sorted(root.glob("**/*.jsonl")):
        yield _from_jsonl(jsonl, tool_truncate, max_line)
    for md in sorted(root.glob("**/memory/*.md")):
        yield _from_memory(md)


def _from_jsonl(
    path: Path, tool_truncate: int, max_line: int = 262144
) -> tuple[SourceMeta, list[Turn]]:
    session_id = path.stem
    project = ""
    title = None
    turns: list[Turn] = []
    with path.open(encoding="utf-8") as fh:
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
            kind = obj.get("type")
            if kind == "ai-title" and not title:
                title = obj.get("title") or obj.get("content")
                if isinstance(title, dict):
                    title = title.get("title")
            if kind in SKIP_TYPES:
                continue
            cwd = obj.get("cwd")
            if cwd and not project:
                project = project_from_claude_cwd(cwd)
            session_id = obj.get("sessionId") or obj.get("session_id") or session_id
            role, text = _turn_from_obj(obj, tool_truncate)
            if not text:
                continue
            turns.append(
                Turn(
                    agent="claude",
                    role=role,
                    text=text,
                    project=project,
                    session_id=session_id,
                    source_path=str(path),
                    source_kind="transcript",
                    ts=obj.get("timestamp"),
                    title=title if isinstance(title, str) else None,
                )
            )
    if not project:
        project = project_from_claude_cwd(None, path.parent)
        for turn in turns:
            turn.project = project
    meta = file_meta(path, "claude", session_id, project, title=title if isinstance(title, str) else None)
    return meta, turns


def _from_memory(path: Path) -> tuple[SourceMeta, list[Turn]]:
    text = path.read_text(encoding="utf-8", errors="ignore").strip()
    project = project_from_claude_cwd(None, path.parent.parent)
    session_id = path.stem
    turns = []
    if text:
        turns.append(
            Turn(
                agent="claude",
                role="memory",
                text=text,
                project=project,
                session_id=session_id,
                source_path=str(path),
                source_kind="memory",
                title=path.stem,
            )
        )
    return file_meta(path, "claude", session_id, project, kind="memory", title=path.stem), turns


def _turn_from_obj(obj: dict, tool_truncate: int) -> tuple[str, str]:
    kind = obj.get("type")
    message = obj.get("message") if isinstance(obj.get("message"), dict) else None
    if kind in ("user", "assistant") and message:
        role = message.get("role") or kind
        text = content_to_text(message.get("content"), tool_truncate)
        return str(role), text
    if kind == "summary":
        summary = obj.get("summary")
        if isinstance(summary, str):
            return "memory", summary
    return "", ""
