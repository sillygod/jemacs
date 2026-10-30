"""What an agent's conversation is about: the pull requests and ClickUp
tasks it names.

The herd page shows a project's branch and that branch's PR; an agent
may have opened, reviewed and merged a handful more along the way, and
the tasks reach it as links you pasted.  So its transcript is read for
them -- but only what it was given or did counts: a reference in your
prompt, in its own command, or in what it said.  One that only went by
in a tool's output is what it happened to see -- a search, a docs page,
a list -- and is left out; so is what the herd told a lead its project
works on, at the head of your prompt, which is the others' named again.
And what it wrote into a file is the file's -- a test's made-up fixture,
a doc's example -- not a reference it was given or made.

A transcript can run to tens of megabytes and grows a turn at a time,
so each is read on from where it was left, its references kept -- in
this process alone: a restarted sidecar reads each from its start, so
a change to what counts reaches every reference, not only new ones.
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

# The arguments that carry what a tool writes into a file, by CLI and tool:
# left out of its call, whose path, command and description stay.  As this
# machine's transcripts have them (claude's Edit both old_string and
# old_str); claude's MultiEdit and NotebookEdit as its tools' schema names
# them, and grok's other file toolsets (apply_patch, hashline_edit,
# StrReplace, EditNotebook) as its binary does.
FILE_CONTENT = {
    "claude": {
        "Write": {"content"},
        "Edit": {"old_string", "new_string", "old_str", "new_str"},
        "MultiEdit": {"edits"},
        "NotebookEdit": {"new_source"},
    },
    "agy": {
        "write_to_file": {"CodeContent"},
        "write_file": {"Content"},
        "replace_file_content": {"TargetContent", "ReplacementContent"},
        "multi_replace_file_content": {"ReplacementChunks"},
        "edit_file": {"Target", "Replacement"},
    },
    "grok": {
        "write": {"content"},
        "search_replace": {"old_string", "new_string"},
        "apply_patch": {"patch"},
        "hashline_edit": {"edits"},
        "StrReplace": {"old_string", "new_string"},
        "EditNotebook": {"old_string", "new_string"},
    },
}

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


def _call(cli: str, name: Any, args: Any) -> str:
    """ARGS of CLI's call to tool NAME, as text: without what it writes
    into a file."""
    drop = FILE_CONTENT[cli].get(name) if isinstance(name, str) else None
    if drop and isinstance(args, dict):
        args = {k: v for k, v in args.items() if k not in drop}
    return json.dumps(args, ensure_ascii=False)


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
            return [(conversation.without_herd_notes(content), ts)] if o.get("type") == "user" else out
        for b in content or []:
            if not isinstance(b, dict):
                continue
            if b.get("type") == "text":
                out.append((conversation.without_herd_notes(b.get("text") or ""), ts))
            elif b.get("type") == "tool_use":
                out.append((_call(cli, b.get("name"), b.get("input")), ts))
    elif cli == "agy":
        ts = o.get("created_at")
        if o.get("type") == "USER_INPUT":
            out.append((conversation.without_herd_notes(o.get("content") or ""), ts))
        elif o.get("type") == "PLANNER_RESPONSE":
            out.append((o.get("content") or "", ts))
            for call in o.get("tool_calls") or []:
                if isinstance(call, dict):
                    out.append((_call(cli, call.get("name"), call.get("args")), ts))
    elif cli == "grok":
        if o.get("type") == "user" and not o.get("synthetic_reason"):
            out.append((conversation.without_herd_notes(conversation.content_to_text(o.get("content"))), None))
        elif o.get("type") == "assistant":
            content = o.get("content")
            out.append((content if isinstance(content, str) else conversation.content_to_text(content), None))
            for call in o.get("tool_calls") or []:
                if not isinstance(call, dict):
                    continue
                # Its arguments come as JSON text; kept whole when not.
                args = call.get("arguments")
                try:
                    args = json.loads(args) if isinstance(args, str) else args
                except ValueError:
                    pass
                out.append((_call(cli, call.get("name"), args), None))
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
