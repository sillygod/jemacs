"""How an answered ask's work went, in one line (ask 4093758e).

The asker sees the taker's conclusion, not how it got there.  This adds one
line to the answer, made of facts only: how long the work took, how many
tool calls the taker made and how many failed -- and of what kind -- the
guard's and the checks' denials, the asks it sent others while it worked,
the relays, its sub-agents.  Nothing about whether the answer is right.

The work is read from the taker's own transcript: from the message that
delivered the ask (the herd's `[ask ID -- ...]' frame) to its `herd reply
ID' -- or, for an ask the herd closed when the taker stopped, to the end.
The rest is the herd's own records.  When the transcript cannot be read,
its CLI's format is not known, or the ask is not in it, the line says what
it could not see, and counts nothing from it -- never a guess.  Failures
are told only where the transcript says so: claude marks a failed tool
result; grok's and agy's transcripts, as the herd reads them, do not.

Counts and tool kinds only: never a command, a path or what a tool said.

It is made after the reply, off the reply's path (herd.py `_trace_soon'),
within READ_SECONDS; past that it says the work took too long to read.
"""

from __future__ import annotations

import collections
import json
import re
import time
from pathlib import Path
from typing import Any

import conversation

READ_SECONDS = 2.0
FIRST_WINDOW = 1024 * 1024
READ_MAX = 32 * 1024 * 1024
SUB_MAX = 8 * 1024 * 1024

# When the work was rough enough to say so, and to suggest remembering what
# it found: four failed tool calls -- not three, since grep finding nothing
# exits 1 and claude counts that a failure -- two edits that did not apply,
# two denials (a permission, the guard, a check), three asks to one agent
# (two rounds of rework), or a relay.
ROUGH_FAILED = 4
ROUGH_EDITS = 2
ROUGH_DENIED = 2
ROUGH_ROUNDS = 3
NUDGE = "rough going: if it found a non-obvious gotcha, `herd remember` it"

SHELL_TOOLS = {"Bash", "run_terminal_command", "run_command"}
EDIT_TOOLS = conversation.EDIT_TOOLS | {"MultiEdit", "NotebookEdit"}
SUB_TOOLS = conversation.SUB_TOOLS

# claude's failed tool results, by kind -- their shapes as this machine's
# transcripts have them (2,400-odd failures read, 2026-10-26).
EXIT = re.compile(r"\AExit code \d+")
CHECK = re.compile(r"Denied by the herd's check")
DENIED = re.compile(r"\A(?:Permission for this \w+ was denied|This (?:Bash )?command (?:requires approval|contains multiple"
                    r" operations)|Claude requested permissions|The user doesn't want to proceed|<tool_use_error>Blocked)")
TIMEOUT = re.compile(r"\bCommand timed out\b|\btimed out\b", re.I)

LABELS = {  # kind: (one, many)
    "shell": ("shell exit", "shell exits"),
    "edit": ("edit not applied", "edits not applied"),
    "denied": ("permission denial", "permission denials"),
    "timeout": ("timeout", "timeouts"),
    "other": ("other error", "other errors"),
}


def classify(tool: str, text: str) -> str | None:
    """What kind of failure TOOL's failed result TEXT is; None for a check's
    denial, which the herd counts from its own record."""
    t = (text or "").strip()
    if CHECK.search(t):
        return None
    if DENIED.match(t):
        return "denied"
    if tool in SHELL_TOOLS and EXIT.match(t):
        return "shell"
    if tool in EDIT_TOOLS:
        return "edit"
    if TIMEOUT.search(t[:300]):
        return "timeout"
    return "other"


class Work:
    "What the taker's transcript shows of the work."

    def __init__(self) -> None:
        self.seen = False
        self.why = ""
        self.cli = ""
        self.failures_known = False
        self.tools: collections.Counter = collections.Counter()
        self.failed: collections.Counter = collections.Counter()
        self.subs = 0
        self.sub_tools = 0
        self.sub_failed: collections.Counter = collections.Counter()
        self.subs_read = 0
        self.start_ts: str | None = None
        self.end_ts: str | None = None


class _Late(Exception):
    pass


def _check(deadline: float) -> None:
    if time.monotonic() > deadline:
        raise _Late()


def _user_input(o: dict, cli: str) -> bool:
    "Whether record O is something said to the agent (not a tool's result)."
    if cli == "claude":
        if o.get("isSidechain"):
            return False
        # A message that came while the agent was busy is taken mid-turn: an
        # `attachment' of the queued command, not a user record (seen on
        # this machine: "absorbed_mid_turn").
        att = o.get("attachment")
        if o.get("type") == "attachment" and isinstance(att, dict):
            return att.get("type") == "queued_command"
        if o.get("type") != "user":
            return False
        content = (o.get("message") or {}).get("content") if isinstance(o.get("message"), dict) else None
        if isinstance(content, str):
            return True
        return isinstance(content, list) and not any(
            isinstance(b, dict) and b.get("type") == "tool_result" for b in content)
    if cli == "grok":
        return o.get("type") == "user"
    return o.get("type") == "USER_INPUT"


def _timed(lines: list[str], deadline: float):
    "LINES' records, read while DEADLINE allows."
    for i, line in enumerate(lines):
        if i % 500 == 0:
            _check(deadline)
        try:
            o = json.loads(line)
        except ValueError:
            continue
        if isinstance(o, dict):
            yield o


def _from_delivery(real: Path, cli: str, ask_id: str, deadline: float) -> list[str] | None:
    """The lines of REAL from the record that delivered ASK_ID on, or None
    when it is not there (within READ_MAX)."""
    mark = f"[ask {ask_id} --".encode()
    size = real.stat().st_size
    window = FIRST_WINDOW
    while True:
        _check(deadline)
        start = max(0, size - window)
        with real.open("rb") as f:
            f.seek(start)
            data = f.read(size - start)
        at = 0
        while True:
            i = data.find(mark, at)
            if i < 0:
                break
            line_start = data.rfind(b"\n", 0, i) + 1
            if start > 0 and line_start == 0:
                break   # the window began mid-line: read further back
            line_end = data.find(b"\n", i)
            line = data[line_start: line_end if line_end >= 0 else len(data)]
            try:
                o = json.loads(line)
            except ValueError:
                o = None
            if isinstance(o, dict) and _user_input(o, cli):
                return [l for l in data[line_start:].decode("utf-8", "replace").splitlines() if l.strip()]
            at = i + len(mark)
        if start == 0 or window >= READ_MAX:
            return None
        window *= 4


def _reply_to(command: Any, ask_id: str) -> bool:
    call = conversation.herd_call(command) if isinstance(command, str) else None
    return bool(call and call["kind"] == "reply" and call.get("ask") == ask_id)


def _claude(records, ask_id: str, w: Work, sub: bool = False) -> None:
    names: dict[str, str] = {}
    first = True
    for o in records:
        if (o.get("isSidechain") and not sub) or o.get("isMeta"):
            continue
        ts = o.get("timestamp")
        if first and not sub:
            w.start_ts, first = ts, False
        msg = o.get("message") if isinstance(o.get("message"), dict) else {}
        content = msg.get("content")
        if o.get("type") == "assistant":
            for b in content if isinstance(content, list) else []:
                if not (isinstance(b, dict) and b.get("type") == "tool_use"):
                    continue
                name = str(b.get("name") or "tool")
                inp = b.get("input") if isinstance(b.get("input"), dict) else {}
                if not sub and name == "Bash" and _reply_to(inp.get("command"), ask_id):
                    w.end_ts = ts
                    return
                names[str(b.get("id"))] = name
                if sub:
                    w.sub_tools += 1
                else:
                    w.tools[name] += 1
                    if name in SUB_TOOLS:
                        w.subs += 1
        elif o.get("type") == "user" and isinstance(content, list):
            for b in content:
                if isinstance(b, dict) and b.get("type") == "tool_result" and b.get("is_error"):
                    kind = classify(names.get(str(b.get("tool_use_id")), ""),
                                    conversation.content_to_text(b.get("content"), 2000) or "")
                    if kind:
                        (w.sub_failed if sub else w.failed)[kind] += 1
        if not sub:
            w.end_ts = ts


def _grok(records, ask_id: str, w: Work) -> None:
    for o in records:
        if o.get("type") != "assistant":
            continue
        for call in o.get("tool_calls") or []:
            if not isinstance(call, dict):
                continue
            name = str(call.get("name") or "tool")
            args = call.get("arguments")
            try:
                parsed = json.loads(args) if isinstance(args, str) else args
            except ValueError:
                parsed = {}
            if name == "run_terminal_command" and isinstance(parsed, dict) and _reply_to(parsed.get("command"), ask_id):
                return
            w.tools[name] += 1


def _agy(records, ask_id: str, w: Work) -> None:
    for o in records:
        if not (o.get("type") == "PLANNER_RESPONSE" and o.get("source") == "MODEL"):
            continue
        for call in o.get("tool_calls") or []:
            if not isinstance(call, dict):
                continue
            name = str(call.get("name") or "tool")
            args = call.get("args") if isinstance(call.get("args"), dict) else {}
            if name == "run_command" and _reply_to(conversation._unquote(str(args.get("CommandLine") or "")), ask_id):
                return
            w.tools[name] += 1


def _subs(real: Path, w: Work, deadline: float) -> None:
    """claude's sub-agents begun while the taker worked: each keeps its own
    conversation beside its parent's; their tool calls and failures."""
    if not (w.start_ts and w.subs):
        return
    folder = real.with_suffix("") / "subagents"
    try:
        files = sorted(folder.glob("agent-*.jsonl"))
    except OSError:
        return
    for f in files:
        _check(deadline)
        try:
            if f.stat().st_size > SUB_MAX:
                continue
            with f.open(encoding="utf-8", errors="replace") as fh:
                lines = [l for l in fh if l.strip()]
        except OSError:
            continue
        recs = list(_timed(lines, deadline))
        began = next((r.get("timestamp") for r in recs if r.get("timestamp")), None)
        if not began or began < w.start_ts or (w.end_ts and began > w.end_ts):
            continue
        w.subs_read += 1
        _claude(recs, "", w, sub=True)


def read_work(transcript: str | None, ask_id: str, seconds: float = READ_SECONDS) -> Work:
    "What TRANSCRIPT shows of the work on ASK_ID, within SECONDS."
    w = Work()
    if not transcript:
        w.why = "no conversation known for it"
        return w
    try:
        real, cli = conversation.locate(transcript)
    except ValueError:
        w.why = "its conversation could not be read"
        return w
    w.cli = cli
    deadline = time.monotonic() + seconds
    try:
        lines = _from_delivery(real, cli, ask_id, deadline)
        if lines is None:
            w.why = "its conversation does not show this ask"
            return w
        {"claude": _claude, "grok": _grok, "agy": _agy}[cli](_timed(lines, deadline), ask_id, w)
        w.failures_known = cli == "claude"
        w.seen = True
        if cli == "claude":
            _subs(real, w, deadline)
    except _Late:
        w.seen = False
        w.why = "it took too long to read"
    except (OSError, KeyError):
        w.seen = False
        w.why = "its conversation could not be read"
    return w


# ---- The line ------------------------------------------------------------------


def _n(count: int, one: str, many: str | None = None) -> str:
    return f"{count} {one if count == 1 else (many or one + 's')}"


def duration(seconds: float | None) -> str:
    if seconds is None or seconds < 0:
        return ""
    s = int(round(seconds))
    if s < 60:
        return f"{s} s"
    if s < 3600:
        return f"{s // 60} min"
    return f"{s // 3600} h {s % 3600 // 60} min"


def _kinds(failed: collections.Counter) -> str:
    return ", ".join(_n(n, *LABELS[k]) for k, n in sorted(failed.items(), key=lambda kv: (-kv[1], kv[0])))


def compose(work: Work, seconds: float | None, guard: int = 0, lint: int = 0,
            asks: dict[str, int] | None = None, relays: int = 0) -> dict[str, Any]:
    """The line, and what it is made of: {line, rough, detail}.  LINE is
    None when the work was not seen and the herd's records say nothing of
    it either: then there is no line, rather than one that only says so."""
    asks = asks or {}
    parts = [duration(seconds)] if seconds is not None else []
    failed = sum(work.failed.values())
    if work.seen:
        calls = _n(sum(work.tools.values()), "tool call")
        if not work.failures_known:
            calls += f" (failures not readable for {work.cli})"
        elif failed:
            calls += f", {failed} failed ({_kinds(work.failed)})"
        elif work.tools:
            calls += ", none failed"
        parts.append(calls)
        if work.subs:
            sub = _n(work.subs, "sub-agent")
            if work.subs_read:
                sf = sum(work.sub_failed.values())
                sub += f" ({_n(work.sub_tools, 'tool call')}" + (f", {sf} failed" if sf else "") + ")"
            parts.append(sub)
    else:
        parts.append(f"tool calls not seen: {work.why}")
    if guard:
        parts.append(_n(guard, "guard denial"))
    if lint:
        parts.append(_n(lint, "check denial"))
    if asks:
        total = sum(asks.values())
        top = sorted(asks.items(), key=lambda kv: (-kv[1], kv[0]))
        whom = ", ".join(f"{a} ×{n}" if n > 1 else a for a, n in top[:3]) + (", …" if len(top) > 3 else "")
        parts.append(f"asked others {total}× ({whom})")
    if relays:
        parts.append(f"relayed {relays}×")
    rough = (failed >= ROUGH_FAILED or work.failed.get("edit", 0) >= ROUGH_EDITS
             or work.failed.get("denied", 0) + guard + lint >= ROUGH_DENIED
             or max(asks.values(), default=0) >= ROUGH_ROUNDS or relays > 0)
    line = "How it went: " + " · ".join(p for p in parts if p)
    if rough:
        line += " · " + NUDGE
    if not (work.seen or guard or lint or asks or relays):
        line, rough = None, False
    return {
        "line": line, "rough": rough,
        "detail": {
            "seconds": seconds, "seen": work.seen, "why": work.why, "cli": work.cli,
            "tools": dict(work.tools), "failed": dict(work.failed),
            "failures_known": work.failures_known, "subs": work.subs, "subs_read": work.subs_read,
            "sub_tools": work.sub_tools, "sub_failed": dict(work.sub_failed),
            "guard": guard, "lint": lint, "asks": asks, "relays": relays,
        },
    }
