"""Grok sessions: only chat_history.jsonl + summary.json.

recap_requests, updates.jsonl, images and terminal logs are the bulk of
~/.grok and are not conversation — importing them would embed the same
history dozens of times and blow the index up.
"""

from __future__ import annotations

import json
from collections.abc import Iterator
from pathlib import Path

from chunk import Turn
from importers.textutil import (
    content_to_text,
    file_meta,
    grok_user_text,
    project_from_grok_dirname,
    truncate_tool,
)
from index import SourceMeta

SKIP_TYPES = {"system", "reasoning"}


def iter_grok(
    root: Path, tool_truncate: int = 1500, max_line: int = 262144
) -> Iterator[tuple[SourceMeta, list[Turn]]]:
    root = Path(root)
    if not root.is_dir():
        return
    for chat in sorted(root.glob("**/chat_history.jsonl")):
        summary_path = chat.parent / "summary.json"
        title, extra = _summary(summary_path)
        project = extra.get("project") or project_from_grok_dirname(chat.parent.parent.name)
        session_id = extra.get("session_id") or chat.parent.name
        yield _from_chat(chat, project, session_id, title, tool_truncate, max_line)
        if summary_path.is_file():
            yield _from_summary(summary_path, project, session_id, title, extra)


def _summary(path: Path) -> tuple[str | None, dict]:
    extra: dict = {}
    if not path.is_file():
        return None, extra
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None, extra
    title = data.get("generated_title") or data.get("session_summary")
    extra["project"] = data.get("git_root_dir") or ""
    extra["session_id"] = path.parent.name
    extra["summary"] = data.get("session_summary") or ""
    extra["last_turn"] = data.get("last_turn_summary") or ""
    extra["created_at"] = data.get("created_at")
    extra["updated_at"] = data.get("updated_at")
    extra["agent_name"] = data.get("agent_name")
    return (title if isinstance(title, str) else None), extra


def _from_chat(
    path: Path,
    project: str,
    session_id: str,
    title: str | None,
    tool_truncate: int,
    max_line: int = 262144,
) -> tuple[SourceMeta, list[Turn]]:
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
            role, text = _turn_from_obj(obj, tool_truncate)
            if not text:
                continue
            turns.append(
                Turn(
                    agent="grok",
                    role=role,
                    text=text,
                    project=project,
                    session_id=session_id,
                    source_path=str(path),
                    source_kind="transcript",
                    title=title,
                )
            )
    return file_meta(path, "grok", session_id, project, title=title), turns


def _from_summary(
    path: Path,
    project: str,
    session_id: str,
    title: str | None,
    extra: dict,
) -> tuple[SourceMeta, list[Turn]]:
    parts = [p for p in (title, extra.get("summary"), extra.get("last_turn")) if p]
    text = "\n".join(dict.fromkeys(parts))  # preserve order, drop dups
    turns = []
    if text:
        turns.append(
            Turn(
                agent="grok",
                role="memory",
                text=text,
                project=project,
                session_id=session_id,
                source_path=str(path),
                source_kind="memory",
                ts=extra.get("updated_at") or extra.get("created_at"),
                title=title,
            )
        )
    return file_meta(path, "grok", session_id, project, kind="memory", title=title), turns


def _turn_from_obj(obj: dict, tool_truncate: int) -> tuple[str, str]:
    kind = obj.get("type")
    if kind in SKIP_TYPES:
        return "", ""
    if obj.get("synthetic_reason") == "compaction_meta":
        return "", ""
    raw = content_to_text(obj.get("content"), tool_truncate)
    if kind == "user":
        return "user", grok_user_text(raw)
    if kind == "assistant":
        names = []
        for call in obj.get("tool_calls") or []:
            if isinstance(call, dict):
                name = call.get("name") or call.get("function", {}).get("name")
                if name:
                    names.append(f"[tool {name}]")
        text = raw
        if names:
            text = (text + "\n" + " ".join(names)).strip()
        return "assistant", text
    if kind == "tool_result":
        return "tool", truncate_tool(raw, tool_truncate)
    return "", ""
