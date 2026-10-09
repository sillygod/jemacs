"""What an agent's conversation is about: the pull requests and ClickUp
tasks it names.

The herd page shows a project's branch and that branch's PR; an agent
may have opened, reviewed and merged a handful more along the way, and
the tasks reach it as links you pasted.  So its transcript is read for
them -- but only what it was given or did counts: a reference in your
prompt, in its own command, or in what it said.  One that only went by
in a tool's output is what it happened to see -- a search, a docs page,
a list -- and is left out.

A transcript can run to tens of megabytes and grows a turn at a time,
so each is read on from where it was left, its references kept.
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any

import conversation

# A pull request, as its page or the forge's API names it.
PR = re.compile(
    r"https?://(?:www\.)?github\.com/([\w.-]+)/([\w.-]+)/pull/(\d+)"
    r"|https?://api\.github\.com/repos/([\w.-]+)/([\w.-]+)/pulls/(\d+)"
    r"|https?://bitbucket\.org/([\w.-]+)/([\w.-]+)/pull-requests/(\d+)"
    r"|https?://api\.bitbucket\.org/2\.0/repositories/([\w.-]+)/([\w.-]+)/pullrequests/(\d+)"
)
# A ClickUp task, as clickup-view reads one (`clickup-view--ref-re'): a
# link, or CU-<id>; an id is lowercase letters and digits with a digit.
TASK = re.compile(
    r"app\.clickup\.com/t/(?:\d+/)?([0-9a-z]*[0-9][0-9a-z]*)(?![/0-9a-zA-Z-])"
    r"|(?<![0-9A-Za-z_])CU-([0-9a-z]*[0-9][0-9a-z]*)(?![0-9A-Za-z_])"
)

LIMIT = 40  # of each, the most recently named

_state: dict[str, dict[str, Any]] = {}


def _scan_text(text: str, ts: Any, order: int, refs: dict) -> None:
    for m in PR.finditer(text or ""):
        g = m.groups()
        for i, host in ((0, "github"), (3, "github"), (6, "bitbucket"), (9, "bitbucket")):
            if g[i]:
                key = ("pr", host, g[i].lower(), g[i + 1].lower(), int(g[i + 2]))
                break
        _note(refs, key, ts, order)
    for m in TASK.finditer(text or ""):
        _note(refs, ("task", m.group(1) or m.group(2)), ts, order)


def _note(refs: dict, key: tuple, ts: Any, order: int) -> None:
    """KEY named again, at ORDER in the transcript -- which says which was
    named last, times or not -- and at time TS when it has one."""
    seen = refs.setdefault(key, {"count": 0, "last": None, "order": 0})
    seen["count"] += 1
    seen["order"] = order
    if ts:
        seen["last"] = ts


def _said(o: dict, cli: str) -> list[tuple[str, Any]]:
    """The texts of record O that count: given to the agent, or by it."""
    out: list[tuple[str, Any]] = []
    if cli == "claude":
        # A compaction's summary names again what the turns before it did.
        if o.get("isSidechain") or o.get("isMeta") or o.get("isCompactSummary"):
            return out
        ts = o.get("timestamp")
        content = (o.get("message") or {}).get("content")
        if isinstance(content, str):
            return [(content, ts)] if o.get("type") == "user" else out
        for b in content or []:
            if not isinstance(b, dict):
                continue
            if b.get("type") == "text":
                out.append((b.get("text") or "", ts))
            elif b.get("type") == "tool_use":
                out.append((json.dumps(b.get("input"), ensure_ascii=False), ts))
    elif cli == "agy":
        ts = o.get("created_at")
        if o.get("type") == "USER_INPUT":
            out.append((o.get("content") or "", ts))
        elif o.get("type") == "PLANNER_RESPONSE":
            out.append((o.get("content") or "", ts))
            for call in o.get("tool_calls") or []:
                out.append((json.dumps(call, ensure_ascii=False), ts))
    elif cli == "grok":
        if o.get("type") == "user" and not o.get("synthetic_reason"):
            out.append((conversation.content_to_text(o.get("content")), None))
        elif o.get("type") == "assistant":
            content = o.get("content")
            out.append((content if isinstance(content, str) else conversation.content_to_text(content), None))
            for call in o.get("tool_calls") or []:
                out.append((json.dumps(call, ensure_ascii=False), None))
    return out


def scan(path: str) -> dict[str, Any]:
    """The pull requests and tasks transcript PATH names, most recent first.
    Read on from where the last scan of it stopped."""
    real, cli = conversation.locate(path)
    key = str(real)
    size = real.stat().st_size
    st = _state.get(key)
    if st is None or size < st["offset"]:
        st = _state[key] = {"offset": 0, "refs": {}, "order": 0}
    if size > st["offset"]:
        with real.open("rb") as f:
            f.seek(st["offset"])
            data = f.read()
        end = data.rfind(b"\n")
        if end >= 0:
            for line in data[: end + 1].splitlines():
                try:
                    o = json.loads(line)
                except ValueError:
                    continue
                if isinstance(o, dict):
                    for text, ts in _said(o, cli):
                        st["order"] += 1
                        _scan_text(text, ts, st["order"], st["refs"])
            st["offset"] += end + 1
    return _result(st["refs"])


def _result(refs: dict) -> dict[str, Any]:
    prs, tasks = [], []
    for k, v in sorted(refs.items(), key=lambda item: item[1]["order"], reverse=True):
        if k[0] == "pr" and len(prs) < LIMIT:
            prs.append({"host": k[1], "owner": k[2], "repo": k[3], "id": k[4],
                        "count": v["count"], "last": v["last"]})
        elif k[0] == "task" and len(tasks) < LIMIT:
            tasks.append({"id": k[1], "count": v["count"], "last": v["last"]})
    return {"prs": prs, "tasks": tasks}
