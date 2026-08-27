"""Shared turn extraction: JSON content blocks, salvage, project paths."""

from __future__ import annotations

import json
import re
from pathlib import Path
from urllib.parse import unquote, urlparse

from index import SourceMeta

USER_QUERY = re.compile(r"<user_query>\s*(.*?)\s*</user_query>", re.S)
FILE_URI = re.compile(r"file://(/[^\s\"'<>]+)")
CJK = re.compile(r"[\u4e00-\u9fff]")


def file_meta(
    path: Path,
    agent: str,
    session_id: str,
    project: str,
    kind: str = "transcript",
    title: str | None = None,
) -> SourceMeta:
    st = path.stat()
    return SourceMeta(
        path=str(path),
        mtime=st.st_mtime,
        size=st.st_size,
        agent=agent,
        session_id=session_id,
        project=project,
        kind=kind,
        title=title,
    )


def content_to_text(content, tool_truncate: int = 1500) -> str:
    if content is None:
        return ""
    if isinstance(content, str):
        return content.strip()
    if isinstance(content, dict):
        return content_to_text(content.get("text") or content.get("content"), tool_truncate)
    if not isinstance(content, list):
        return str(content).strip()
    parts: list[str] = []
    for item in content:
        if isinstance(item, str):
            parts.append(item)
            continue
        if not isinstance(item, dict):
            continue
        kind = item.get("type")
        if kind in ("image", "thinking") or kind and "image" in str(kind):
            continue
        if kind == "text" or "text" in item:
            text = item.get("text") or ""
            if isinstance(text, str) and text.strip():
                parts.append(text)
        elif kind == "tool_use":
            name = item.get("name") or "tool"
            inp = item.get("input") or {}
            hint = ""
            if isinstance(inp, dict):
                hint = (
                    inp.get("description")
                    or inp.get("command")
                    or inp.get("file_path")
                    or inp.get("path")
                    or ""
                )
            snippet = f"[tool {name}] {hint}".strip()
            parts.append(snippet)
        elif kind == "tool_result":
            inner = content_to_text(
                item.get("content") or item.get("text") or item.get("output"),
                tool_truncate,
            )
            if len(inner) > tool_truncate:
                inner = inner[:tool_truncate] + "\n…"
            if inner:
                parts.append(inner)
    return "\n".join(p.strip() for p in parts if p and str(p).strip()).strip()


def grok_user_text(text: str) -> str:
    """Prefer the inner <user_query>; the wrapper is system furniture."""
    if not text:
        return ""
    match = USER_QUERY.search(text)
    if match:
        return match.group(1).strip()
    return text.strip()


def project_from_claude_cwd(cwd: str | None, fallback_dir: Path | None = None) -> str:
    if cwd:
        return str(Path(cwd))
    if fallback_dir is None:
        return ""
    # Claude encodes / and . as '-'.  Lossy; only used when cwd is missing.
    name = fallback_dir.name
    if name.startswith("-"):
        return "/" + name[1:].replace("-", "/")
    return str(fallback_dir)


def project_from_grok_dirname(name: str) -> str:
    return unquote(name)


def project_from_file_uris(text: str) -> str:
    match = FILE_URI.search(text or "")
    if not match:
        return ""
    return match.group(1)


def project_from_workspace_uris(raw: str | None) -> str:
    if not raw:
        return ""
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return project_from_file_uris(raw)
    if isinstance(data, list) and data:
        uri = data[0]
        if isinstance(uri, str) and uri.startswith("file://"):
            return urlparse(uri).path or uri[7:]
        if isinstance(uri, str):
            return uri
    if isinstance(data, str):
        return project_from_workspace_uris(json.dumps([data]))
    return ""


def salvage_blob(blob: bytes | None) -> str:
    """UTF-8 salvage from agy's protobuf blobs.  Not a protobuf parser."""
    if not blob:
        return ""
    text = blob.decode("utf-8", "ignore")
    cleaned = []
    for ch in text:
        if ch in "\n\t" or ch.isprintable():
            cleaned.append(ch)
        else:
            cleaned.append(" ")
    s = "".join(cleaned)
    s = re.sub(r"[ \t]+", " ", s)
    s = re.sub(r"\n{3,}", "\n\n", s).strip()
    if not looks_like_prose(s):
        return ""
    return s


def looks_like_prose(text: str, min_chars: int = 24) -> bool:
    if len(text) < min_chars:
        return False
    if CJK.search(text):
        return True
    words = re.findall(r"[A-Za-z]{2,}", text)
    return len(words) >= 4


def truncate_tool(text: str, limit: int) -> str:
    text = (text or "").strip()
    if len(text) > limit:
        return text[:limit] + "\n…"
    return text
