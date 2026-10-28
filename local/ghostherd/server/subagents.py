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
it was told -- a notice with a status, a monitor's expiry -- or where it
stopped one itself (TaskStop), of which it is told nothing more.  A
monitor's events are not its end.  None is counted STALE after it began: a server
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

import os
from datetime import datetime, timezone

import conversation as conv

STALE = 3600
SLACK = 2.0
CACHE_MAX = 256
HANDBACK_TOOL = "SubagentHandback"
TASK_ID = re.compile(r"<task-id>([A-Za-z0-9_-]{1,64})</task-id>")
# What the Bash and Monitor tools answer, as Claude Code 2.1 says it -- and
# Bash for a call that ran past its timeout and was moved to the background
# (46 here, 2026-10-28): a notification ends it, as any other.
BACKGROUND = re.compile(r"\A(Command running in background with ID: |Monitor started \(task |"
                        r"Command did not complete within its \d+s timeout and was moved to the background \(ID: )"
                        r"([A-Za-z0-9_-]{1,64})\b")
EXPIRED = re.compile(r"\[Monitor expired after\b")
# A task its parent stopped: TaskStop -- KillShell, KillBash before it --
# after which Claude Code writes no task notification, only the call's
# result: JSON whose message is "Successfully stopped task: ID (COMMAND)"
# (52 read here, 2026-10-28), "Successfully killed shell: ID (...)" for
# the older ones.  Each tool, and the input that names the task.
STOP_TOOLS = {"TaskStop": "task_id", "KillShell": "shell_id", "KillBash": "shell_id"}
STOPPED = re.compile(r"\ASuccessfully (?:stopped task|killed shell): ([A-Za-z0-9_-]{1,64})\b")

# Agy sub-agent and background task markers in transcript.jsonl.
AGY_SUB_ID = re.compile(r'"conversationId":\s*"([A-Za-z0-9_-]{1,64})"')
AGY_BG_START = re.compile(r"Tool is running as a background task with task id:\s*(\S+)")
AGY_TASK_STATUS = re.compile(r"^Task:\s*(\S+)\s*\nStatus:\s*(\S+)", re.M)
AGY_SENDER = re.compile(r"\bsender=(\S+)")
AGY_TASK_DONE = re.compile(r'Task id "([^"]+)"')

# Grok background task and sub-agent markers in chat_history.jsonl.
GROK_TASK_TYPE = re.compile(r"<task-type>\s*([A-Za-z0-9_-]+)\s*</task-type>")
GROK_STATUS = re.compile(r"<status>\s*([A-Za-z0-9_-]+)\s*</status>")
GROK_SUMMARY = re.compile(r"<summary>\s*(.*?)\s*</summary>", re.S)
GROK_OUTPUT = re.compile(r"<output-file>\s*(\S+?)\s*</output-file>")
GROK_TASK_BLOCK = re.compile(r"=== Task ([A-Za-z0-9_-]{1,64}) ===\s*\n(?:.*?\n)*?Status:\s*(\S+)")
GROK_WAIT_BLOCK = re.compile(r"--- Task ([A-Za-z0-9_-]{1,64}) \[([^\]]+)\] ---")
GROK_DONE_NOTE = re.compile(
    r'(?:Background task|Subagent|Task|Agent)\s+"?([A-Za-z0-9_-]{1,64})"?\s+'
    r"(?:has\s+)?(completed|failed|killed|cancelled|canceled|stopped|terminated|timed\s*out|finished)",
    re.I,
)
GROK_SPAWN_ID = re.compile(r'"?subagent_id"?\s*:\s*"?([A-Za-z0-9_-]{1,64})"?')


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
        # Its stop calls (STOP_TOOLS) not yet answered: call id -> task id.
        self.stopping: dict[str, str] = {}

    def look(self) -> None:
        lines, fresh = self.follow.look()
        if fresh:
            self.stopped.clear()
            self.tasks.clear()
            self.stopping.clear()
        for line in lines:
            told = "<task-notification>" in line
            stop = (any(t in line for t in STOP_TOOLS) or "Successfully stopped task" in line
                    or "Successfully killed shell" in line or any(c in line for c in self.stopping))
            if not (told or stop or "Command running in background" in line or "Monitor started" in line
                    or "moved to the background" in line):
                continue
            try:
                o = json.loads(line)
            except ValueError:
                continue
            if told:
                self._told(o, line)
            self._started(o)
            if stop:
                self._ended(o)

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


    def _ended(self, o: dict[str, Any]) -> None:
        """A task it stopped itself (STOP_TOOLS): ended, and its parent told
        so, once the call's result came back fine -- named by the call, or,
        the call out of what was read, by the result's own words."""
        msg = o.get("message")
        content = msg.get("content") if isinstance(msg, dict) else None
        if not isinstance(content, list):
            return
        if o.get("type") == "assistant":
            for b in content:
                if isinstance(b, dict) and b.get("type") == "tool_use" and b.get("name") in STOP_TOOLS:
                    inp = b.get("input") if isinstance(b.get("input"), dict) else {}
                    tid = inp.get(STOP_TOOLS[b["name"]]) or inp.get("task_id") or inp.get("shell_id")
                    if isinstance(tid, str) and tid and b.get("id"):
                        self.stopping[str(b["id"])] = tid
            return
        if o.get("type") != "user":
            return
        at = conv._epoch(o.get("timestamp"))
        for b in content:
            if not (isinstance(b, dict) and b.get("type") == "tool_result"):
                continue
            tid = self.stopping.pop(str(b.get("tool_use_id")), None)
            if b.get("is_error"):
                continue
            m = STOPPED.match(_stop_said(b.get("content")))
            for t in {tid, m and m.group(1)} - {None}:
                self.tasks.pop(t, None)
                if at is not None and at > self.stopped.get(t, 0):
                    self.stopped[t] = at


def _stop_said(content: Any) -> str:
    "What a stop call's result says: its JSON's message, or its text."
    if isinstance(content, list):
        content = "\n".join(t.get("text", "") for t in content if isinstance(t, dict))
    if not isinstance(content, str):
        return ""
    text = content.strip()
    if text.startswith("{"):
        try:
            said = json.loads(text)
        except ValueError:
            return text
        if isinstance(said, dict) and isinstance(said.get("message"), str):
            return said["message"]
    return text


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


def _agy_args(raw: Any) -> dict[str, Any]:
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


class AgySub:
    """An Agy sub-agent read from its own brain/<sid>/.system_generated/logs/transcript.jsonl."""

    def __init__(self, path: Path) -> None:
        self.follow = Follow(path)
        self.clear()

    def clear(self) -> None:
        self.started: str | None = None
        self.said_at: str | None = None
        self.tools = 0
        self.tokens: int | None = None
        self.doing = ""
        self.ended = False

    def look(self) -> None:
        lines, fresh = self.follow.look()
        if fresh:
            self.clear()
        for o in conv._records(lines):
            self._take(o)

    def _take(self, o: dict[str, Any]) -> None:
        kind, source, ts = o.get("type"), o.get("source"), o.get("created_at")
        if o.get("status") in ("ERROR", "CANCELED", "CANCELLED") or kind == "ERROR_MESSAGE":
            self.ended = True
            return
        if kind == "SYSTEM_MESSAGE" and "All your subagents and background tasks have been terminated" in str(
            o.get("content") or ""
        ):
            self.ended = True
            return
        if kind == "USER_INPUT":
            self.started = self.started or ts
            self.said_at = ts or self.said_at
            self.ended = False
            return
        if kind == "PLANNER_RESPONSE" and source == "MODEL":
            self.started = self.started or ts
            self.said_at = ts or self.said_at
            self.ended = False
            if o.get("input_tokens") is not None or o.get("cache_read_tokens") is not None:
                self.tokens = int(o.get("input_tokens") or 0) + int(o.get("cache_read_tokens") or 0)
            if (o.get("content") or "").strip():
                self.doing = conv.first_line(str(o["content"]))
            calls = [c for c in (o.get("tool_calls") or []) if isinstance(c, dict)]
            for c in calls:
                self.tools += 1
                name = str(c.get("name") or "tool")
                args = _agy_args(c.get("args"))
                what = conv.hint(args)
                self.doing = name + (" · " + what if what else "")
                if name == "send_message":
                    self.ended = True
            if not calls:
                self.ended = True
            return
        if source == "MODEL":
            self.said_at = ts or self.said_at


class AgyParent:
    """Sub-agents and background tasks started in an Agy parent transcript."""

    def __init__(self, path: Path) -> None:
        self.path = path
        self.follow = Follow(path, conv.TAIL_MAX)
        self.spawned: dict[str, dict[str, Any]] = {}
        self.stopped: dict[str, float] = {}
        self.tasks: dict[str, dict[str, Any]] = {}
        self._pending_subs: list[dict[str, str]] = []

    def look(self) -> None:
        lines, fresh = self.follow.look()
        if fresh:
            self.spawned.clear()
            self.stopped.clear()
            self.tasks.clear()
            self._pending_subs.clear()
        for o in conv._records(lines):
            self._take(o)

    def _take(self, o: dict[str, Any]) -> None:
        kind, source = o.get("type"), o.get("source")
        ts = str(o.get("created_at") or "")
        at = conv._epoch(ts)
        ep = at if at is not None else self.path.stat().st_mtime
        if o.get("status") in ("CANCELED", "CANCELLED", "ERROR") and kind == "ERROR_MESSAGE":
            self.tasks.clear()
            for sid in self.spawned:
                self.stopped[sid] = ep
            return
        if kind == "PLANNER_RESPONSE" and source == "MODEL":
            for call in o.get("tool_calls") or []:
                if not isinstance(call, dict):
                    continue
                name = str(call.get("name") or "")
                args = _agy_args(call.get("args"))
                if name == "invoke_subagent":
                    subs_arg = args.get("Subagents")
                    if isinstance(subs_arg, list):
                        self._pending_subs = [
                            {
                                "who": str(s.get("TypeName") or "subagent"),
                                "title": str(
                                    s.get("Role") or conv.first_line(str(s.get("Prompt") or ""))
                                ),
                            }
                            for s in subs_arg
                            if isinstance(s, dict)
                        ]
                elif name == "manage_subagents":
                    action = str(args.get("Action") or "")
                    if action == "kill_all":
                        for sid in self.spawned:
                            self.stopped[sid] = ep
                    elif action == "kill":
                        cids = args.get("ConversationIds")
                        for sid in cids if isinstance(cids, list) else []:
                            self.stopped[str(sid)] = ep
                elif name == "manage_task":
                    if str(args.get("Action") or "") == "kill":
                        tid = str(args.get("TaskId") or "")
                        self.tasks.pop(tid, None)
                elif name == "send_message":
                    rec = str(args.get("Recipient") or "")
                    if rec in self.spawned:
                        # Sending a follow-up message wakes an idle sub-agent.
                        self.stopped.pop(rec, None)
        elif kind == "INVOKE_SUBAGENT":
            content = str(o.get("content") or "")
            if o.get("status") in ("ERROR", "CANCELED", "CANCELLED"):
                self._pending_subs.clear()
                return
            for idx, sid in enumerate(AGY_SUB_ID.findall(content)):
                info = (
                    self._pending_subs[idx]
                    if idx < len(self._pending_subs)
                    else {"who": "subagent", "title": ""}
                )
                self.spawned[sid] = {
                    "who": info["who"],
                    "title": info["title"],
                    "started": ts,
                    "started_ep": ep,
                }
                self.stopped.pop(sid, None)
            self._pending_subs.clear()
        elif kind in ("RUN_COMMAND", "GENERIC"):
            content = str(o.get("content") or "")
            m = AGY_BG_START.search(content)
            if m and o.get("status") not in ("ERROR", "CANCELED", "CANCELLED"):
                tid = m.group(1).strip()
                is_mon = kind == "GENERIC" and (
                    "Timer" in content or "Cron" in content or "schedule" in content.lower()
                )
                self.tasks[tid] = {"kind": "monitor" if is_mon else "shell", "started": ep}
            for tid, st_name in AGY_TASK_STATUS.findall(content):
                if st_name.upper() not in ("RUNNING", "PENDING"):
                    self.tasks.pop(tid, None)
        elif kind == "SYSTEM_MESSAGE":
            content = str(o.get("content") or "")
            if "All your subagents and background tasks have been terminated" in content:
                self.tasks.clear()
                for sid in self.spawned:
                    self.stopped[sid] = ep
                return
            for sender in AGY_SENDER.findall(content):
                self.tasks.pop(sender, None)
                if sender in self.spawned:
                    self.stopped[sender] = ep
            for tid in AGY_TASK_DONE.findall(content):
                self.tasks.pop(tid, None)


class GrokParent:
    """Sub-agents and background tasks started in a Grok chat_history.jsonl."""

    def __init__(self, path: Path) -> None:
        self.path = path
        self.follow = Follow(path, conv.TAIL_MAX)
        self.calls: dict[str, dict[str, Any]] = {}
        self.tasks: dict[str, dict[str, Any]] = {}
        self.subs: dict[str, dict[str, Any]] = {}

    def look(self) -> None:
        lines, fresh = self.follow.look()
        if fresh:
            self.calls.clear()
            self.tasks.clear()
            self.subs.clear()
        for o in conv._records(lines):
            self._take(o)

    def _started_ep(self, output_file: str | None) -> float:
        if output_file:
            try:
                return Path(output_file).stat().st_mtime
            except OSError:
                pass
        try:
            return self.path.stat().st_mtime
        except OSError:
            return time.time()

    def _take(self, o: dict[str, Any]) -> None:
        kind = o.get("type")
        if kind == "assistant":
            for call in o.get("tool_calls") or []:
                if not isinstance(call, dict):
                    continue
                cid = str(call.get("id") or "")
                name = str(call.get("name") or "")
                raw_args = call.get("arguments")
                try:
                    args = json.loads(raw_args) if isinstance(raw_args, str) else (raw_args or {})
                except ValueError:
                    args = {}
                if not isinstance(args, dict):
                    args = {}
                if cid:
                    self.calls[cid] = {"name": name, "args": args}
                if name == "kill_command_or_subagent":
                    tid = str(args.get("task_id") or args.get("subagent_id") or "")
                    self.tasks.pop(tid, None)
                    self.subs.pop(tid, None)
        elif kind == "tool_result":
            cid = str(o.get("tool_call_id") or "")
            call = self.calls.get(cid, {})
            name = call.get("name") or ""
            args = call.get("args") or {}
            content = str(o.get("content") or "")

            m_id = TASK_ID.search(content)
            m_st = GROK_STATUS.search(content)
            if m_id and m_st:
                tid = m_id.group(1)
                status = m_st.group(1).lower()
                if status == "running":
                    m_type = GROK_TASK_TYPE.search(content)
                    ttype = m_type.group(1).lower() if m_type else ""
                    m_sum = GROK_SUMMARY.search(content)
                    summary = m_sum.group(1).strip() if m_sum else ""
                    m_out = GROK_OUTPUT.search(content)
                    out_file = m_out.group(1).strip() if m_out else None
                    ep = self._started_ep(out_file)
                    if ttype == "subagent" or name == "spawn_subagent":
                        who = str(args.get("subagent_type") or args.get("agent_type") or "subagent")
                        title = str(
                            args.get("description")
                            or summary
                            or conv.first_line(str(args.get("prompt") or ""))
                        )
                        iso = (
                            datetime.fromtimestamp(ep, tz=timezone.utc)
                            .isoformat()
                            .replace("+00:00", "Z")
                        )
                        self.subs[tid] = {
                            "id": tid,
                            "who": who,
                            "title": title,
                            "started": iso,
                            "started_ep": ep,
                            "tools": 0,
                            "tokens": None,
                            "doing": summary or title,
                            "output": out_file,
                        }
                    else:
                        is_mon = ttype == "monitor" or name == "monitor_background"
                        self.tasks[tid] = {
                            "kind": "monitor" if is_mon else "shell",
                            "started": ep,
                            "output": out_file,
                        }
                else:
                    self.tasks.pop(tid, None)
                    self.subs.pop(tid, None)
            elif name == "spawn_subagent":
                m_sid = GROK_SPAWN_ID.search(content)
                if m_sid and "error" not in content.lower() and "failed" not in content.lower():
                    tid = m_sid.group(1)
                    if "completed" in content.lower() or "stopped" in content.lower():
                        self.subs.pop(tid, None)
                    elif "running" in content.lower() or "spawned" in content.lower() or "started" in content.lower():
                        ep = self._started_ep(None)
                        iso = (
                            datetime.fromtimestamp(ep, tz=timezone.utc)
                            .isoformat()
                            .replace("+00:00", "Z")
                        )
                        who = str(args.get("subagent_type") or args.get("agent_type") or "subagent")
                        title = str(
                            args.get("description") or conv.first_line(str(args.get("prompt") or ""))
                        )
                        self.subs[tid] = {
                            "id": tid,
                            "who": who,
                            "title": title,
                            "started": iso,
                            "started_ep": ep,
                            "tools": 0,
                            "tokens": None,
                            "doing": title,
                            "output": None,
                        }

            for tid, st_name in GROK_TASK_BLOCK.findall(content) + GROK_WAIT_BLOCK.findall(content):
                if st_name.strip().lower() not in ("running", "pending"):
                    self.tasks.pop(tid, None)
                    self.subs.pop(tid, None)
            for tid, _st in GROK_DONE_NOTE.findall(content):
                self.tasks.pop(tid, None)
                self.subs.pop(tid, None)
        elif kind == "user":
            raw = conv.content_to_text(o.get("content"))
            for tid, _st in GROK_DONE_NOTE.findall(raw):
                self.tasks.pop(tid, None)
                self.subs.pop(tid, None)
            m_id = TASK_ID.search(raw)
            m_st = GROK_STATUS.search(raw)
            if m_id and m_st and m_st.group(1).lower() not in ("running", "pending"):
                self.tasks.pop(m_id.group(1), None)
                self.subs.pop(m_id.group(1), None)


_subs: dict[str, Sub] = {}
_parents: dict[str, Parent] = {}
_agy_subs: dict[str, AgySub] = {}
_agy_parents: dict[str, AgyParent] = {}
_grok_parents: dict[str, GrokParent] = {}
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
    """The sub-agents of the transcript PATH still running, oldest first,
    and what to watch for a change in them."""
    with _mu:
        return _live(path)


def _live(path: str) -> dict[str, Any]:
    real, cli = conv.locate(path)
    if cli == "agy":
        return _live_agy(real)
    if cli == "grok":
        return _live_grok(real)
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


def _live_agy(real: Path) -> dict[str, Any]:
    out: dict[str, Any] = {"subs": [], "watch": []}
    now = time.time()
    try:
        if now - real.stat().st_mtime >= STALE:
            return out
    except OSError:
        return out
    parent = _kept(_agy_parents, str(real), lambda: AgyParent(real))
    parent.look()
    if not parent.spawned:
        return out
    brain_dir = real.parent.parent.parent.parent
    for sid, info in parent.spawned.items():
        if not conv.SUB_ID.match(sid) or now - info["started_ep"] >= STALE:
            continue
        sub_path = brain_dir / sid / ".system_generated" / "logs" / "transcript.jsonl"
        s: AgySub | None = None
        if sub_path.is_file():
            try:
                if now - sub_path.stat().st_mtime >= STALE:
                    continue
                file, _ = conv.locate(str(sub_path))
                s = _kept(_agy_subs, str(file), lambda: AgySub(file))
                s.look()
            except (ValueError, OSError):
                continue
            if s.ended:
                continue
        told = parent.stopped.get(sid)
        if told is not None:
            said = conv._epoch(s.said_at) if s and s.said_at else info["started_ep"]
            if said is None or told >= said - SLACK:
                continue
        out["subs"].append({
            "id": sid,
            "who": info["who"],
            "title": info["title"],
            "started": (s.started if s and s.started else None) or info["started"],
            "tools": s.tools if s else 0,
            "tokens": s.tokens if s else None,
            "doing": (s.doing if s and s.doing else info["title"]),
        })
        if sub_path.is_file():
            out["watch"].append(str(sub_path))
    out["subs"].sort(key=lambda x: x["started"] or "")
    return out


def _live_grok(real: Path) -> dict[str, Any]:
    out: dict[str, Any] = {"subs": [], "watch": []}
    now = time.time()
    try:
        if now - real.stat().st_mtime >= STALE:
            return out
    except OSError:
        return out
    parent = _kept(_grok_parents, str(real), lambda: GrokParent(real))
    parent.look()
    for sid, info in parent.subs.items():
        if not conv.SUB_ID.match(sid) or now - info["started_ep"] >= STALE:
            continue
        out_file = info.get("output")
        if out_file:
            try:
                if now - Path(out_file).stat().st_mtime >= STALE:
                    continue
            except OSError:
                pass
        out["subs"].append({
            "id": sid,
            "who": info["who"],
            "title": info["title"],
            "started": info["started"],
            "tools": info["tools"],
            "tokens": info["tokens"],
            "doing": info["doing"],
        })
    out["subs"].sort(key=lambda x: x["started"] or "")
    return out


def tasks(path: str) -> list[dict[str, Any]]:
    """The shells and monitors the conversation at PATH started in the
    background and has not been told ended, oldest first: each as
    {id, kind, started}.  None begun STALE ago or more."""
    with _mu:
        real, cli = conv.locate(path)
        now = time.time()
        if cli == "claude":
            parent = _kept(_parents, str(real), lambda: Parent(real))
            parent.look()
            return sorted(
                ({"id": k, **v} for k, v in parent.tasks.items() if now - v["started"] < STALE),
                key=lambda t: t["started"],
            )
        if cli == "agy":
            try:
                if now - real.stat().st_mtime >= STALE:
                    return []
            except OSError:
                return []
            parent = _kept(_agy_parents, str(real), lambda: AgyParent(real))
            parent.look()
            return sorted(
                ({"id": k, **v} for k, v in parent.tasks.items() if now - v["started"] < STALE),
                key=lambda t: t["started"],
            )
        if cli == "grok":
            try:
                if now - real.stat().st_mtime >= STALE:
                    return []
            except OSError:
                return []
            parent = _kept(_grok_parents, str(real), lambda: GrokParent(real))
            parent.look()
            out = []
            for k, v in parent.tasks.items():
                if now - v["started"] >= STALE:
                    continue
                out_file = v.get("output")
                if out_file:
                    try:
                        if now - Path(out_file).stat().st_mtime >= STALE:
                            continue
                    except OSError:
                        pass
                out.append({"id": k, "kind": v["kind"], "started": v["started"]})
            return sorted(out, key=lambda t: t["started"])
        return []

