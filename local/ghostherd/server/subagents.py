"""What an agent's sub-agents are doing now, from their own transcripts.

Claude Code runs a sub-agent -- its Agent tool -- in a conversation of
its own beside its parent's: <transcript>/subagents/agent-<id>.jsonl,
what it was started as in agent-<id>.meta.json.  The herd page shows
each one still running under its agent, as the CLI's screen does at its
foot: what it is, how long it has run, its tool calls and its context so
far, and what it is doing now.

One has stopped once it handed its report back (its last call), or once
its parent was told it stopped -- failed, killed: a task notification.
Not when told it stopped with background work of its own still running:
it goes on when that ends.  One sent another message runs again.  One
silent for STALE is counted out, its parent likely gone with it.

The shells and monitors an agent started in the background are read off
its own conversation: begun where the call's result says so, ended where
it was told -- a notice with a status, a monitor's expiry.  A monitor's
events are not its end.  None is counted STALE after it began: a server
left running, or a wait stuck, is not something it will be woken by.

Each file is read as it grows, from where it was last read; the parent's
from TAIL_MAX before its end.  Emacs asks again when the parent, the
folder or one of the files this named as running has changed.  The herd
asks too, when an agent's turn ends: one with any of these running is
waiting for them, not done.
"""

from __future__ import annotations

import json
import re
import threading
import time
from pathlib import Path
from typing import Any

import conversation as conv

STALE = 3600
SLACK = 2.0
CACHE_MAX = 256
HANDBACK_TOOL = "SubagentHandback"
TASK_ID = re.compile(r"<task-id>([A-Za-z0-9_-]{1,64})</task-id>")
# What the Bash and Monitor tools answer, as Claude Code 2.1 says it.
BACKGROUND = re.compile(r"\A(Command running in background with ID: |Monitor started \(task )([A-Za-z0-9_-]{1,64})\b")
EXPIRED = re.compile(r"\[Monitor expired after\b")


class Follow:
    """A file read as it grows: each look hands over the lines written
    since the last, a line still being written left for the next.  One
    that shrank or was replaced is read again, from BACK before its end
    when BACK is set."""

    def __init__(self, path: Path, back: int | None = None) -> None:
        self.path, self.back = path, back
        self.key: tuple[int, int, int] | None = None
        self.at = 0

    def look(self) -> tuple[list[str], bool]:
        "The new lines, and whether they are the file read from its start."
        st = self.path.stat()
        key = (st.st_ino, st.st_size, st.st_mtime_ns)
        if key == self.key:
            return [], False
        fresh = self.key is None or st.st_ino != self.key[0] or st.st_size < self.at
        if fresh:
            self.at = 0 if self.back is None else max(0, st.st_size - self.back)
        with self.path.open("rb") as f:
            f.seek(self.at)
            data = f.read(max(0, st.st_size - self.at))
        if fresh and self.at > 0:
            nl = data.find(b"\n")  # begun mid-line
            skip = nl + 1 if nl >= 0 else len(data)
            data, self.at = data[skip:], self.at + skip
        end = data.rfind(b"\n") + 1
        self.at += end
        self.key = key
        return [l for l in data[:end].decode("utf-8", "replace").splitlines() if l.strip()], fresh


class Sub:
    "A sub-agent as its own transcript has it so far."

    def __init__(self, path: Path) -> None:
        self.follow = Follow(path)
        self.clear()

    def clear(self) -> None:
        self.started: str | None = None
        self.said_at: str | None = None  # its last turn, either way
        self.tools = 0
        self.tokens: int | None = None
        self.doing = ""
        self.ended = False
        self._handing: set[str] = set()

    def look(self) -> None:
        lines, fresh = self.follow.look()
        if fresh:
            self.clear()
        for o in conv._records(lines):
            self._take(o)

    def _take(self, o: dict[str, Any]) -> None:
        kind, ts = o.get("type"), o.get("timestamp")
        if kind not in ("user", "assistant"):
            return
        self.started = self.started or ts
        self.said_at = ts or self.said_at
        msg = o.get("message") if isinstance(o.get("message"), dict) else {}
        content = msg.get("content")
        blocks = [b for b in content if isinstance(b, dict)] if isinstance(content, list) else []
        if kind == "assistant":
            self.ended = False
            usage = msg.get("usage")
            if isinstance(usage, dict) and not str(msg.get("model") or "").startswith("<"):
                self.tokens = sum(int(usage.get(k) or 0) for k in
                                  ("input_tokens", "cache_creation_input_tokens", "cache_read_input_tokens"))
            for b in blocks:
                if b.get("type") == "tool_use":
                    self.tools += 1
                    what = conv.hint(b.get("input"))
                    self.doing = str(b.get("name") or "tool") + (" · " + what if what else "")
                    if b.get("name") == HANDBACK_TOOL and b.get("id"):
                        self._handing.add(str(b["id"]))
                elif b.get("type") == "text" and (b.get("text") or "").strip():
                    self.doing = conv.first_line(b["text"])
            return
        results = [b for b in blocks if b.get("type") == "tool_result"]
        if any(str(b.get("tool_use_id")) in self._handing and not b.get("is_error") for b in results):
            self.ended = True
        elif not results and not o.get("isMeta"):
            self.ended = False  # sent another message: it runs again


class Parent:
    """When the parent was last told each of its tasks stopped, and the
    shells and monitors it started that it has not been told ended."""

    def __init__(self, path: Path) -> None:
        self.follow = Follow(path, conv.TAIL_MAX)
        self.stopped: dict[str, float] = {}
        self.tasks: dict[str, dict[str, Any]] = {}

    def look(self) -> None:
        lines, fresh = self.follow.look()
        if fresh:
            self.stopped.clear()
            self.tasks.clear()
        for line in lines:
            told = "<task-notification>" in line
            if not (told or "Command running in background" in line or "Monitor started" in line):
                continue
            try:
                o = json.loads(line)
            except ValueError:
                continue
            if told:
                self._told(o, line)
            self._started(o)

    def _told(self, o: dict[str, Any], line: str) -> None:
        at = conv._epoch(o.get("timestamp"))
        said = _said(o)
        for m in conv.NOTICE.finditer(said):
            n = conv.notice(m.group(0))
            if n["id"] in self.tasks and (n["status"] or EXPIRED.search(m.group(1))):
                del self.tasks[n["id"]]
        n = conv.notice(said)
        if n and n["interim"]:
            return  # it goes on when its own background work ends
        for sid in TASK_ID.findall(line):
            if at is not None and at > self.stopped.get(sid, 0):
                self.stopped[sid] = at

    def _started(self, o: dict[str, Any]) -> None:
        "A shell or monitor begun, as its call's result says: nothing else."
        msg = o.get("message") if o.get("type") == "user" else None
        content = msg.get("content") if isinstance(msg, dict) else None
        at = conv._epoch(o.get("timestamp"))
        if not isinstance(content, list) or at is None:
            return
        for b in content:
            if not (isinstance(b, dict) and b.get("type") == "tool_result"):
                continue
            text = b.get("content")
            if isinstance(text, list):
                text = "\n".join(t.get("text", "") for t in text if isinstance(t, dict))
            m = BACKGROUND.match(text.strip()) if isinstance(text, str) else None
            if m:
                self.tasks[m.group(2)] = {"kind": "monitor" if m.group(1).startswith("Monitor") else "shell",
                                          "started": at}


def _said(o: dict[str, Any]) -> str:
    "What a record of the parent's says: queued, delivered or attached."
    if isinstance(o.get("content"), str):
        return o["content"]
    att = o.get("attachment")
    if isinstance(att, dict) and isinstance(att.get("prompt"), str):
        return att["prompt"]
    msg = o.get("message")
    content = msg.get("content") if isinstance(msg, dict) else None
    if isinstance(content, list):
        return "\n".join(b.get("text", "") for b in content if isinstance(b, dict))
    return content if isinstance(content, str) else ""


_subs: dict[str, Sub] = {}
_parents: dict[str, Parent] = {}
# The page's asking and the herd's run in threads of their own, and a
# file read by two at once would hand the same lines to each.
_mu = threading.Lock()


def _kept(cache: dict[str, Any], key: str, make) -> Any:
    hit = cache.get(key)
    if hit is None:
        if len(cache) >= CACHE_MAX:
            cache.pop(next(iter(cache)))
        hit = cache[key] = make()
    return hit


def live(path: str) -> dict[str, Any]:
    """The sub-agents of the claude transcript PATH still running, oldest
    first, and what to watch for a change in them: the folder, where a
    new one appears, and each running one's file."""
    with _mu:
        return _live(path)


def _live(path: str) -> dict[str, Any]:
    real, cli = conv.locate(path)
    if cli != "claude":
        return {"subs": [], "watch": []}
    folder = real.with_suffix("") / "subagents"
    out: dict[str, Any] = {"subs": [], "watch": [str(folder)]}
    now = time.time()
    fresh = []
    try:
        for p in folder.iterdir():
            if p.name.startswith("agent-") and p.suffix == ".jsonl":
                try:
                    if now - p.stat().st_mtime < STALE:
                        fresh.append(p)
                except OSError:
                    continue
    except OSError:
        return out
    if not fresh:
        return out
    parent = _kept(_parents, str(real), lambda: Parent(real))
    parent.look()
    for p in fresh:
        sid = p.name[len("agent-"):-len(".jsonl")]
        try:
            file, _ = conv.locate(str(p))  # under the CLI's own directory, as its parent
            s = _kept(_subs, str(file), lambda: Sub(file))
            s.look()
        except (ValueError, OSError):
            continue
        if not conv.SUB_ID.match(sid) or not s.started or s.ended:
            continue
        told = parent.stopped.get(sid)
        said = conv._epoch(s.said_at)
        if told is not None and said is not None and told >= said - SLACK:
            continue
        meta = conv.sub_meta(real, sid)
        out["subs"].append({
            "id": sid,
            "who": str(meta.get("name") or meta.get("agentType") or ""),
            "title": str(meta.get("description") or ""),
            "started": s.started,
            "tools": s.tools,
            "tokens": s.tokens,
            "doing": s.doing,
        })
        out["watch"].append(str(p))
    out["subs"].sort(key=lambda x: x["started"] or "")
    return out


def tasks(path: str) -> list[dict[str, Any]]:
    """The shells and monitors the claude conversation at PATH started in
    the background and has not been told ended, oldest first: each as
    {id, kind, started}.  None begun STALE ago or more."""
    with _mu:
        real, cli = conv.locate(path)
        if cli != "claude":
            return []
        parent = _kept(_parents, str(real), lambda: Parent(real))
        parent.look()
        now = time.time()
        return sorted(({"id": k, **v} for k, v in parent.tasks.items() if now - v["started"] < STALE),
                      key=lambda t: t["started"])
