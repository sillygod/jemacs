"""Claude Code jsonl under ~/.claude/projects/<encoded-cwd>/."""

from __future__ import annotations

import json
import os
from collections.abc import Iterator
from pathlib import Path

from chunk import Turn
from importers.textutil import content_to_text, file_meta, project_from_claude_cwd
from index import SourceMeta

# Bumped when a transcript is read differently; files read before are
# read again.  2: what Claude Code writes as a user turn but you did not
# type -- a tool's output, a skill's or command's text, a compaction's
# summary -- is no longer yours.
VERSION = 2

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
    # A claude project dir's name -> the directory its sessions ran in: a
    # memory file's project, which the name alone gives only lossily.
    projects: dict[str, str] = {}
    for jsonl in sorted(root.glob("**/*.jsonl")):
        meta, turns = _from_jsonl(jsonl, tool_truncate, max_line)
        if meta.project:
            projects.setdefault(jsonl.relative_to(root).parts[0], meta.project)
        yield meta, turns
    seen: set[str] = set()
    for md in sorted(root.glob("**/memory/*.md")):
        real, project = memory_source(str(md), projects)
        if real in seen or not Path(real).is_file():
            continue
        seen.add(real)
        yield _from_memory(Path(real), project)


def memory_source(path: str, projects: dict[str, str]) -> tuple[str, str]:
    """Where the memory file at PATH really is, and its project.

    ghostherd links a project's claude memory dir to its shared-memory
    store -- a worktree's to its main checkout's -- and the store links the
    user's facts into every project's: one file under many paths.  It is
    indexed once, under its real path.  A user fact is every project's
    (project "").  PROJECTS: as in `iter_claude'."""
    real = Path(os.path.realpath(path))
    owner = real.parent
    if owner.name == "memory":
        # claude's own memory dir, <project dir>/memory/
        owner = owner.parent
    elif owner.name == "user":
        # the shared store's user facts
        return str(real), ""
    # A project store is named as claude names the project's dir.
    return str(real), projects.get(owner.name) or project_from_claude_cwd(None, owner)


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
    meta = file_meta(path, "claude", session_id, project, title=title if isinstance(title, str) else None,
                     parser=VERSION)
    return meta, turns


def _from_memory(path: Path, project: str) -> tuple[SourceMeta, list[Turn]]:
    text = path.read_text(encoding="utf-8", errors="ignore").strip()
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
    return file_meta(path, "claude", session_id, project, kind="memory", title=path.stem, parser=VERSION), turns


def _turn_from_obj(obj: dict, tool_truncate: int) -> tuple[str, str]:
    kind = obj.get("type")
    message = obj.get("message") if isinstance(obj.get("message"), dict) else None
    if kind in ("user", "assistant") and message:
        # A user turn you did not type: text Claude Code loaded for a skill
        # or a command (isMeta), and the summary a compaction carries on
        # from -- what it sums up is in the turns before.
        if obj.get("isMeta") or obj.get("isCompactSummary"):
            return "", ""
        content = message.get("content")
        text = content_to_text(content, tool_truncate)
        # ...and a tool's output, which comes back as one.
        if kind == "user" and isinstance(content, list) and any(
            isinstance(b, dict) and b.get("type") == "tool_result" for b in content
        ):
            return "tool", text
        role = message.get("role") or kind
        return str(role), text
    if kind == "summary":
        summary = obj.get("summary")
        if isinstance(summary, str):
            return "memory", summary
    return "", ""
