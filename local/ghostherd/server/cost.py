"""What one task cost, end to end, and what it produced (ask 2b3fab98).

A task is what the user asked for in one message, and the work it set off:
- said to a lead, or primary -- lead mode: the lead's replies from it on,
  the asks it made for it and those made answering them (handoffs too),
  their takers' replies, and the sub-agents of each;
- said to any other agent -- direct mode: that agent's replies until it is
  set other work, and its sub-agents'.
The herd's own words are never one: an ask, an answer, mail, a relay's
brief, a task notification, a command.

Every reply an agent writes counts to the work it was on as it wrote it:
what its conversation last switched to.  Something said to it switches
it (`said'):
- the user's own words: a task of its own (`task:ID');
- an ask the herd delivered: that ask's taker's work (`ask:ID');
- the answer to one of its own asks: back to the work the ask was made
  for (`back:ID') -- a lead's turn woken by an answer is that ask's
  task's, not whatever the user said last;
and nothing else: mail, the herd's notes and nudges, a sub-agent or a
shell that ended, a sub-agent's report handed back, a command -- the
work goes on as it was.  An ask is
the work its asker was on when the herd client said so ("herd: asked qa
(ask ID)", "herd: handed off to ..."), read off the asker's own tool
results; a sub-agent's replies, the work of the call that started it
(its meta's toolUseId).  The user's words are told from the herd's by
what the herd frames them in (conversation.user_entry): the herd writes
an ask, an answer or mail inside `[ghostherd message from ...]', a relay
inside `[Relay: ...]'; its notes before a lead's message are taken off,
and what is left is the user's.

Each file is read from where the last pass stopped (herd.py
`_cost_pass'), its current work kept with its offset, so a restart picks
up where it was; what the work cost is kept by item, agent and whether a
sub-agent wrote it (`cost_usage'), in tally.py's unit.

What a task produced: the commits made for it, merged or not -- a work
project merges on its forge, often squashed, which no local branch shows.
Each checkout the herd works in has its HEAD's reflog read as it changes
(herd.py `_cost_capture'): every commit made there, on any branch -- a
feature branch, a pull request opened, back to main -- and only those: a
fetch or a pull brings commits in, it makes none.  They are kept, so a
worktree removed with its reflog when its agent stops leaves them.  A
commit is the work its checkout's agent was on as it was made (`attribute'):
lead mode, each ask's taker's; direct mode, the agent's own.  A rebase, an
amend or a cherry-pick makes a copy of a commit already made, counted once,
where it was first made.  Whether each is in its project's main branch is
said beside it, not counted.  An ask whose taker's checkout was never read
-- gone before this was -- has the commits in the main branch that name
it.  Lines changed are insertions plus deletions, merges left out.  A task
with none is "no code"; one whose checkout could not be read, unknown.
"""

from __future__ import annotations

import hashlib
import json
import re
import statistics
import subprocess
import time
import unicodedata
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Any, Iterable

import conversation
import tally

# What the herd client says as it makes an ask (bin/herd's `say'): the ask
# is its asker's then-current work's.  Inside a tool result, read raw, so
# the line's JSON escapes are no matter.
ASKED = re.compile(r'herd: (?:asked [^\s"\\]+|handed off to [^\n"\\]*?|[^\n"\\]*? has no lead: [^\n"\\]*?)'
                   r' \((?:ask|handoff) ([0-9A-Za-z]{4,32})\)')
# The herd's own prompt to an agent taking over another's work (relay.py).
RELAY = "[Relay: "
# The role a lead or primary is owed on a CLI that takes none as it starts
# opens its first message (`ghostherd--lead-role', `ghostherd--primary-role'):
# what follows its rule is the user's.  Alone -- a cold respawn says it
# again -- it is the herd's.
LEAD_ROLE = "You are the lead of this project's herd of agents"
PRIMARY_ROLE = "You are primary: the herd-wide lead."
ROLES = (LEAD_ROLE, PRIMARY_ROLE)
ROLE_RULE = "\n---\n"
# What the herd tells an agy agent as it starts, with no word of the
# user's in it: its guard, when it has one (`ghostherd--agy-guard-note'),
# then the shared memory's opening (`ghostherd--memory-rules').  grok
# reads the same from --rules, claude its role from a flag and its memory
# from a linked directory: neither is told anything.  As Emacs writes them,
# spaces aside (test_cost checks it).
OPENINGS = ("A herd guard checks each of your tool calls.",
            "The herd keeps shared memory for this project --")
# How many times the rules that tell the herd's words from the user's, or
# what of them a task keeps as its title (`title'), changed: tasks kept by
# an earlier reading are read again (herd.py `_cost_recheck').  3: the
# title is the words whole, no longer their first line; a sub-agent's
# report handed back is no task.
SAID_VERSION = 3
PROMPT_MAX = 200
# A prompt this wide at most, its whole text -- a go-ahead, "好，派人做吧",
# "yes merge please" -- says little of what its task is: it is shown with
# the last one before it to the same agent that is not (`context'), cut to
# CONTEXT_MAX.  Wide by the columns it takes: a CJK character is two.
SHORT = 30
CONTEXT_MAX = 60
# ... when it comes this soon after the last reply of the work before it:
# a go-ahead answers what the agent has just proposed, so the clock starts
# at the proposal, not at the prompt that asked for it -- that work may
# have taken an hour.  A short question a day later is a question of its
# own.  Three hours: a break, a meeting, not a night.  The live store
# (2026-10-29) had go-aheads up to 2h14m after the proposal, then none
# until the next day; the Threads tab's two hours for what set a thread
# going (`ghostherd-threads--opener-window') would cut that one -- it
# times the agent, this the user.
CONTEXT_WINDOW = 3 * 3600
LEADS = ("lead", "primary")
# Commits made up to this long after the work's last reply are its own: an
# agent commits, then says so.
COMMIT_SLACK = 120
# Work quiet this long is done: its commits are read then.
SETTLED = 1800
GIT_TIMEOUT = 10
# claude's records worth parsing: replies, things said, and -- of the tool
# results, most of a transcript -- only those the herd client printed in.
TOOL_RESULT = re.compile(rb'"type":\s?"tool_result"')
SAID_RECORD = re.compile(rb'"type":\s?"(?:user|attachment)"')


# ---- What was said to an agent --------------------------------------------------


def said(text: str | None) -> tuple[str, str] | None:
    """What TEXT, said to an agent, switches its work to: ("user", the
    user's words), ("ask", ID) or ("answer", ID) -- or None for the
    herd's own words, which switch nothing."""
    text = text or ""
    # A sub-agent's report, handed back to the claude agent that started it
    # in the background, comes queued as if typed: what it says goes on
    # with the work the sub-agent was started for.
    if text.lstrip().startswith(RELAY) or conversation.AGENT_MESSAGE.match(text.lstrip()):
        return None
    e = conversation.user_entry(text, None)
    if not e:
        return None
    if e["kind"] == "ask":
        return "ask", e["ask"]
    if e["kind"] == "answer":
        return "answer", e["ask"]
    if e["kind"] != "prompt":
        return None
    body = e.get("text") or ""
    if body.startswith(ROLES):
        if ROLE_RULE not in body:
            return None
        # The notes a lead hears first come after its role.
        body = conversation.without_herd_notes(body.split(ROLE_RULE, 1)[1].strip())
    body = body.strip()
    if not body or body.startswith(RELAY) or " ".join(body[:200].split()).startswith(OPENINGS):
        return None
    return "user", body


def width(text: str) -> int:
    "The columns TEXT takes, its spacing collapsed: a wide character, two."
    return sum(2 if unicodedata.east_asian_width(c) in "WF" else 1 for c in " ".join(text.split()))


def title(text: str) -> str:
    """A task's title: the user's words whole, in one line, cut to
    PROMPT_MAX.  Not their first line: that may be only an image's
    stand-in ("[Image #5]") or the head of a paste, with what is asked
    after it."""
    return conversation.one_line(text, PROMPT_MAX)


def _agy_text(o: dict[str, Any]) -> str:
    content = str(o.get("content") or "")
    m = conversation.USER_REQUEST.search(content)
    return m.group(1) if m else content


def _claude_input(o: dict[str, Any]) -> str | None:
    "What record O said to the agent, when it is something said: else None."
    if o.get("isMeta") or o.get("isCompactSummary") or o.get("isVisibleInTranscriptOnly") or o.get("isSidechain"):
        return None
    att = o.get("attachment")
    if o.get("type") == "attachment" and isinstance(att, dict):
        return att.get("prompt") if att.get("type") == "queued_command" and isinstance(att.get("prompt"), str) else None
    msg = o.get("message")
    content = msg.get("content") if isinstance(msg, dict) else None
    if o.get("type") != "user":
        return None
    if isinstance(content, str):
        return content
    if isinstance(content, list) and not any(isinstance(b, dict) and b.get("type") == "tool_result" for b in content):
        return "\n".join(b.get("text", "") for b in content if isinstance(b, dict) and b.get("type") == "text")
    return None


# ---- Reading a file ---------------------------------------------------------------


def task_id(path: str, ref: Any) -> str:
    "A task's id: where its prompt is, as short as an ask's."
    return hashlib.sha1(f"{path}\0{ref}".encode()).hexdigest()[:10]


class Tracker:
    """One file read on, from where it was: the work its agent is on, and
    what each reply cost it.  SUB: a sub-agent's file, whose work is fixed
    (ITEM, its parent's when it was started)."""

    def __init__(self, path: str, cli: str, item: str | None, keys: Iterable[str] = (), sub: bool = False,
                 tasks: bool = True):
        self.path, self.cli, self.item, self.sub = path, cli, item, sub
        # TASKS: the user's words here start tasks -- not in the file of an
        # agent no longer in the herd, read only for the asks it took: its
        # role and project are not known.
        self.tasks_ok = tasks
        self.counted = tally.Counted(keys)
        self.usage: dict[tuple[str, int], list[Any]] = {}   # (item, sub) -> [Usage, first, last]
        self.tasks: list[dict[str, Any]] = []
        self.asked: dict[str, str] = {}
        self.spawned: dict[str, str] = {}
        # Each switch, as (when, item): an ask the herd keeps is the work
        # its asker was on when it was made.
        self.switches: list[tuple[float, str | None]] = []
        # Where the line being read begins: a task's prompt is read again
        # from there (conversation.turn_from).
        self.at = 0

    def _cost(self, item: str | None, when: float | None, u: tally.Usage, sub: bool) -> None:
        if not item:
            return
        got = self.usage.setdefault((item, int(sub)), [tally.Usage(), when, when])
        got[0].add(u)
        if when is not None:
            got[1] = when if got[1] is None else min(got[1], when)
            got[2] = when if got[2] is None else max(got[2], when)

    def _said(self, text: str | None, ref: Any, when: float | None) -> None:
        if self.sub or text is None:
            return
        got = said(text)
        if not got:
            return
        kind, what = got
        if kind == "user" and self.tasks_ok:
            tid = task_id(self.path, ref)
            self.item = "task:" + tid
            self.tasks.append({"id": tid, "at": when, "prompt": title(what),
                               "offset": self.at, "size": width(what)})
        elif kind == "user":
            self.item = None
        elif kind == "ask":
            self.item = "ask:" + what
        else:
            self.item = "back:" + what
        if when is not None:
            self.switches.append((when, self.item))

    def _asked(self, raw: str) -> None:
        for m in ASKED.finditer(raw):
            if self.item:
                self.asked.setdefault(m.group(1), self.item)

    def feed(self, line: bytes, at: int) -> None:
        "One whole LINE of the file, which begins at byte AT."
        self.at = at
        if self.cli == "claude":
            if TOOL_RESULT.search(line):
                if b"herd: " not in line:
                    return
            elif b'"usage"' not in line and not SAID_RECORD.search(line):
                return
        try:
            o = json.loads(line)
        except ValueError:
            return
        if not isinstance(o, dict):
            return
        if self.cli == "claude":
            self._claude(o, line)
        elif self.cli == "agy":
            self._agy(o, line, at)
        elif self.cli == "grok":
            self._grok(o, at)

    def _reply(self, o: dict[str, Any], sub: bool = False) -> None:
        found = tally.record(o, self.cli)
        if found and self.counted.first(found[0]):
            self._cost(self.item, found[1], found[2], sub)

    def _claude(self, o: dict[str, Any], line: bytes) -> None:
        kind = o.get("type")
        if kind == "assistant":
            self._reply(o, self.sub or bool(o.get("isSidechain")))
            msg = o.get("message") if isinstance(o.get("message"), dict) else {}
            for b in msg.get("content") or [] if isinstance(msg.get("content"), list) else []:
                if isinstance(b, dict) and b.get("type") == "tool_use" and b.get("name") in conversation.SUB_TOOLS:
                    if b.get("id") and self.item:
                        self.spawned[str(b["id"])] = self.item
            return
        if kind == "user" and TOOL_RESULT.search(line):
            self._asked(line.decode("utf-8", "replace"))
            return
        text = _claude_input(o)
        if text is not None:
            self._said(text, o.get("uuid") or o.get("timestamp"), tally.epoch(o.get("timestamp")))

    def _agy(self, o: dict[str, Any], line: bytes, at: int) -> None:
        kind = o.get("type")
        if kind == "USER_INPUT":
            self._said(_agy_text(o), o.get("step_index", at), tally.epoch(o.get("created_at")))
        elif kind == "PLANNER_RESPONSE":
            self._reply(o, self.sub)
        elif b"herd: " in line:
            self._asked(line.decode("utf-8", "replace"))

    def _grok(self, o: dict[str, Any], at: int) -> None:
        params = o.get("params") if isinstance(o.get("params"), dict) else {}
        update = params.get("update") if isinstance(params.get("update"), dict) else {}
        kind = update.get("sessionUpdate")
        if kind == "user_message_chunk":
            content = update.get("content") if isinstance(update.get("content"), dict) else {}
            meta = update.get("_meta") if isinstance(update.get("_meta"), dict) else {}
            self._said(content.get("text"), meta.get("promptIndex", at), tally.epoch(o.get("timestamp")))
        elif kind == "turn_completed":
            self._reply(o, self.sub)
        elif kind == "tool_call_update":
            self._asked(json.dumps(update.get("rawOutput") or update.get("content") or "", ensure_ascii=False))


def read_from(path: Path, tracker: Tracker, offset: int, budget: int) -> tuple[int, bool]:
    """Feed TRACKER PATH's whole lines from OFFSET, about BUDGET bytes of
    them: the offset after the last, and whether there is more."""
    with path.open("rb") as f:
        f.seek(offset)
        carry, read, start = b"", 0, offset
        while True:
            block = f.read(min(tally.BLOCK, budget - read) if read < budget else tally.BLOCK)
            if not block:
                return offset, False
            read += len(block)
            data = carry + block
            end = data.rfind(b"\n")
            if end < 0:
                carry = data
                continue
            carry = data[end + 1:]
            at = start
            for line in data[:end].split(b"\n"):
                tracker.feed(line, at)
                at += len(line) + 1
            offset = start = offset + end + 1
            if read >= budget:
                return offset, True


class Recheck(Tracker):
    """A transcript read again, as `Tracker' reads it now, for the tasks
    kept from it (IDS): for each found, where its words are (`offset'),
    what its agent was on before them (`before') and what they are now --
    the user's, its `prompt' and how wide they are whole (`size'); the
    herd's, None (`said').  A task of the file's not found: its words are
    no longer there to read."""

    def __init__(self, path: str, cli: str, ids: Iterable[str]):
        super().__init__(path, cli, None)
        self.ids = set(ids)
        self.found: dict[str, dict[str, Any]] = {}

    def _said(self, text: str | None, ref: Any, when: float | None) -> None:
        tid = task_id(self.path, ref) if text is not None else None
        before, n = self.item, len(self.tasks)
        super()._said(text, ref, when)
        if tid in self.ids and tid not in self.found:
            mine = [t for t in self.tasks[n:] if t["id"] == tid]
            self.found[tid] = {"offset": self.at, "before": before, "prompt": mine[0]["prompt"] if mine else None,
                               "size": mine[0]["size"] if mine else None}


def recheck(path: str, cli: str, ids: Iterable[str], chunk: int = 4 << 20) -> dict[str, dict[str, Any]] | None:
    "`Recheck' PATH whole for tasks IDS; None when it cannot be read."
    t = Recheck(path, cli, ids)
    offset, more = 0, True
    while more:
        try:
            offset, more = read_from(Path(path), t, offset, chunk)
        except OSError:
            return None
        t.tasks.clear()
    return t.found


# ---- A task's cost --------------------------------------------------------------


def made_for(switches: list[tuple[float, str | None]], at: float | None) -> str | None:
    "The work an agent was on AT, from its SWITCHES (when, item), oldest first."
    found = None
    for when, item in switches:
        if at is None or when > at:
            break
        found = item
    return found


def task_of(item: str | None, asked: dict[str, str], depth: int = 0) -> str | None:
    "The task ITEM is work for, through the asks it came by; None when not known."
    if not item or depth > 32:
        return None
    kind, _, ref = item.partition(":")
    if kind == "task":
        return ref
    return task_of(asked.get(ref), asked, depth + 1)


def asks_in(task: str, asked: dict[str, str]) -> list[str]:
    "The asks made for TASK, nested ones too."
    return [a for a in asked if task_of("ask:" + a, asked) == task]


def _usage(row: dict[str, Any]) -> tally.Usage:
    return tally.Usage(**{f: row.get(f) for f in tally.Usage.__slots__})


def _commit(c: list[Any] | dict[str, Any]) -> dict[str, Any]:
    "A commit found otherwise (`costs'' OUTPUT), [sha, lines, merged], as one read from a reflog."
    if isinstance(c, dict):
        return c
    return {"sha": c[0], "lines": c[1], "merged": bool(c[2]), "subject": c[3] if len(c) > 3 else "",
            "of": None, "at": None}


def costs(tasks: list[dict[str, Any]], usage: list[dict[str, Any]], asked: dict[str, str],
          output: dict[str, dict[str, Any]], now: float | None = None,
          made: dict[tuple[str, str], dict[str, Any]] | None = None) -> dict[str, dict[str, Any]]:
    """Each of TASKS' cost and output, by id: from USAGE (rows of item,
    agent, sub and the kinds), ASKED (ask -> the work it was made for),
    MADE (`attribute'': each agent's piece of work's commits, and whether
    its checkout was read for it) and OUTPUT (`task:ID' or `ask:ID' ->
    {commits, named}: the commits found otherwise -- those in the main
    branch that name an ask (NAMED: none found is not none made), or an
    earlier read).  The task's own agent's replies are its `own' -- the
    lead's, in lead mode -- the rest of the herd's its `workers'',
    sub-agents' apart.  Work not over by NOW -- SETTLED quiet -- that has
    made nothing yet is pending."""
    now = time.time() if now is None else now
    made = made or {}
    out = {t["id"]: {"own": tally.Usage(), "workers": tally.Usage(), "subs": tally.Usage(), "asks": set(),
                     "end": t.get("at")} for t in tasks}
    owner = {t["id"]: t["agent"] for t in tasks}
    for r in usage:
        tid = task_of(r["item"], asked)
        got = out.get(tid)
        if got is None:
            continue
        u = _usage(r)
        side = "subs" if r["sub"] else "own" if r["agent"] == owner[tid] else "workers"
        got[side].add(u)
        if r["item"].startswith("ask:"):
            got["asks"].add(r["item"][4:])
        if r.get("last_at") and (got["end"] is None or r["last_at"] > got["end"]):
            got["end"] = r["last_at"]
    context = contexts(tasks, {tid: got["end"] for tid, got in out.items()})
    units: dict[str, list[tuple[str, str]]] = {}
    for key in made:
        tid = task_of(key[0], asked)
        if tid in out:
            units.setdefault(tid, []).append(key)
    result = {}
    for t in tasks:
        got = out[t["id"]]
        lead = t["mode"] != "direct"
        found: dict[str, dict[str, Any]] = {}
        unread = []
        # A lead's own work makes no code here: its asks' takers' does.
        mine = [k for k in units.get(t["id"], []) if not (lead and k[1] == t["agent"])]
        works = [(k[0], made[k]) for k in mine]
        chain = got["asks"] | set(asks_in(t["id"], asked)) if lead else set()
        works += [("ask:" + a, None) for a in sorted(chain) if not any(k[0] == "ask:" + a for k in mine)]
        if not lead and not mine:
            works.append(("task:" + t["id"], None))
        for item, m in works:
            listed = list(m["commits"]) if m else []
            if not (m and m["read"]):
                o = output.get(item)
                if o and (o["commits"] or not o.get("named")):
                    listed += [_commit(c) for c in o["commits"]]
                elif not listed:
                    unread.append(item)
            for c in listed:
                found.setdefault(c["sha"], c)
        shown = sorted(found.values(), key=lambda c: (c.get("at") or 0, c["sha"]))
        new = [c for c in shown if not c.get("of")]
        total = tally.Usage().add(got["own"]).add(got["workers"]).add(got["subs"])
        recent = now - (got["end"] or t.get("at") or 0) < SETTLED
        result[t["id"]] = {
            "id": t["id"], "agent": t["agent"], "project": t["project"], "mode": t["mode"],
            "prompt": t.get("prompt") or "", "context": context.get(t["id"], ""), "at": t.get("at"), "end": got["end"],
            "total": total.weighted(), "own": got["own"].weighted(), "workers": got["workers"].weighted(),
            "subs": got["subs"].weighted(), "kinds": total.shown(), "asks": len(got["asks"]),
            "commits": len(new), "lines": sum(c["lines"] for c in new),
            "merged": sum(1 for c in new if c["merged"]), "mergedLines": sum(c["lines"] for c in new if c["merged"]),
            "code": ("yes" if new else "pending" if recent else "unknown" if unread else "no"),
            # The work whose checkout was never read: an ask's taker's gone
            # before it was -- what names it is looked for (herd.py).
            "unread": unread,
            # Each commit, its copies too (`of'), for the thread it is shown in.
            "made": [{k: c.get(k) for k in ("sha", "subject", "lines", "merged", "of", "at")} for c in shown],
        }
    return result


def contexts(tasks: list[dict[str, Any]], ends: dict[str, float | None] | None = None) -> dict[str, str]:
    """For each of TASKS whose prompt is short (`SHORT', by its `size',
    else its title's), the prompt of the last task before it to the same
    agent in the same project that is not -- else of the last one -- cut
    to CONTEXT_MAX: by id.  Only for one within CONTEXT_WINDOW of the last
    task's work's last reply (ENDS, by id; its prompt, when it has none):
    one later goes on from nothing, and the next goes on from it."""
    ends = ends or {}
    out: dict[str, str] = {}
    # (agent, project) -> (last prompt, last not short, when its work was last heard of)
    last: dict[tuple[str, str], tuple[str, str, float]] = {}
    for t in sorted(tasks, key=lambda t: (t.get("at") or 0, t["id"])):
        prompt = t.get("prompt") or ""
        at = t.get("at") or 0
        short = (t["size"] if t.get("size") is not None else width(prompt)) <= SHORT
        key = (t["agent"], t.get("project") or "")
        before = last.get(key)
        if before and at - before[2] > CONTEXT_WINDOW:
            before = None
        if short and before:
            said = before[1] or before[0]
            out[t["id"]] = conversation.first_line(said, CONTEXT_MAX)
        last[key] = (prompt, prompt if not short else (before[1] if before else ""), max(ends.get(t["id"]) or at, at))
    return out


def titled(row: dict[str, Any]) -> str:
    "A task's prompt, and what a short one goes on from (`contexts')."
    return (row.get("prompt") or "") + (" ← " + row["context"] if row.get("context") else "")


def compare(rows: Iterable[dict[str, Any]], days: int, today: date | None = None) -> dict[str, Any]:
    """Lead mode against direct mode over the last DAYS local days (today
    and the ones before): ROWS are `costs'' tasks.  Per mode: the tasks,
    the median and total per task, the median of those that changed code
    -- a go-ahead is a task of its own, and many pull the median down --
    the tokens per 100 changed lines -- all the mode's tokens over all its
    lines, merged or not: a commit counts in the task it was made in, and
    "commit and push" is a task of its own, so per task it would flatter
    the one that only committed -- how many of those lines are in a main
    branch yet, and, for lead mode, the lead's own share."""
    today = today or date.today()
    since = datetime.combine(today - timedelta(days=days - 1), datetime.min.time()).timestamp()
    out: dict[str, Any] = {"days": days, "since": (today - timedelta(days=days - 1)).isoformat()}
    for mode in ("lead", "direct"):
        mine = [r for r in rows if r["mode"] == mode and (r["at"] or 0) >= since]
        coded = [r for r in mine if r["lines"]]
        total = sum(r["total"] for r in mine)
        lines = sum(r["lines"] for r in coded)
        merged = sum(r.get("mergedLines") or 0 for r in coded)
        out[mode] = {
            "tasks": len(mine),
            "median": round(statistics.median([r["total"] for r in mine])) if mine else 0,
            "medianCoded": round(statistics.median([r["total"] for r in coded])) if coded else None,
            "total": total,
            "per100": round(total * 100 / lines) if lines else None,
            "lines": lines, "coded": len(coded), "merged": merged,
            "noCode": sum(1 for r in mine if r["code"] == "no"),
            "unknown": sum(1 for r in mine if r["code"] == "unknown"),
            "pending": sum(1 for r in mine if r["code"] == "pending"),
            "own": round(sum(r["own"] for r in mine) * 100 / total) if total else None,
        }
    return out


# ---- What a task produced ---------------------------------------------------------


def _git(repo: str, *args: str) -> str | None:
    try:
        r = subprocess.run(["git", "-C", repo, *args], capture_output=True, text=True, timeout=GIT_TIMEOUT)
    except (OSError, subprocess.SubprocessError):
        return None
    return r.stdout if r.returncode == 0 else None


def checkout(directory: str | None) -> str | None:
    "DIRECTORY, expanded, when it is a git checkout that is there."
    if not directory:
        return None
    d = str(Path(directory).expanduser())
    return d if Path(d).is_dir() and _git(d, "rev-parse", "--git-dir") is not None else None


def place(directory: str | None) -> str:
    "DIRECTORY as the commits made in it are kept by: expanded, absolute, no trailing slash."
    return str(Path(directory).expanduser().absolute()).rstrip("/") if directory else ""


def reflog_file(repo: str) -> Path | None:
    "Where REPO's checkout keeps its HEAD's reflog: a worktree's is its own."
    out = (_git(repo, "rev-parse", "--git-path", "logs/HEAD") or "").strip()
    return (Path(repo) / out) if out else None


def main_checkout(repo: str) -> str | None:
    """The checkout REPO's worktree is of, which keeps its commits once the
    worktree is gone: where its common git directory is."""
    out = (_git(repo, "rev-parse", "--path-format=absolute", "--git-common-dir") or "").strip()
    if not out:
        return None
    common = Path(out)
    return place(str(common.parent if common.name == ".git" else common))


# What a reflog entry did (its subject up to the first ": "), when it made a
# commit: new work; an amend, a copy of the commit HEAD was on; a copy of a
# commit made before -- a rebase's or a pull's replaying, a cherry-pick --
# which keeps its author and author date.  The rest -- a checkout, a reset,
# a merge, a pull or a fetch's fast-forward -- made none here.
NEW = ("commit", "commit (initial)", "revert")
AMEND = "commit (amend)"
COPY = re.compile(r"\A(?:(?:rebase|pull)\b[^:(]*\((?:pick|continue|reword|edit|squash|fixup)\)"
                  r"|cherry-pick|commit \(cherry-pick\))\Z")
REFLOG_MAX = 1000
# How many times the rule that tells which commit a copy is of changed:
# copies kept by an earlier one are put right (`recopied', herd.py
# `_cost_recopy').  2: by subject too, not author and author date alone.
COPY_VERSION = 2


def kind(action: str) -> str | None:
    "What a reflog entry's ACTION made: `new' work, an `amend', a `copy' -- or None, no commit."
    if action in NEW:
        return "new"
    if action == AMEND:
        return "amend"
    return "copy" if COPY.match(action) else None


def reflog(repo: str, limit: int = REFLOG_MAX) -> list[dict[str, Any]] | None:
    """REPO's HEAD reflog, newest first, at most LIMIT entries: each one's
    commit, when HEAD moved to it (`at'), what moved it (`action'), the
    commit's author and author date, parents and subject, and the commit
    HEAD was on before.  None when there is none to read."""
    out = _git(repo, "reflog", "show", f"-n{limit}", "--date=unix",
               "--format=%H%x1f%gd%x1f%ae %at%x1f%P%x1f%s%x1f%gs", "HEAD")
    if out is None:
        return None
    rows = []
    for line in out.splitlines():
        parts = line.split("\x1f")
        when = re.search(r"@\{(\d+)\}\Z", parts[1]) if len(parts) == 6 else None
        if when:
            rows.append({"sha": parts[0], "at": float(when.group(1)), "author": parts[2],
                         "parents": parts[3].split(), "subject": parts[4], "action": parts[5].split(": ", 1)[0]})
    for i, r in enumerate(rows):
        r["before"] = rows[i + 1]["sha"] if i + 1 < len(rows) else None
    return rows


def new_commits(entries: list[dict[str, Any]], known: dict[str, dict[str, Any]],
                since: float) -> list[dict[str, Any]]:
    """The commits reflog ENTRIES (newest first) made from SINCE that KNOWN
    (sha -> {author, subject, of}: those kept, from any checkout, the first
    made first) does not have, oldest first: sha, at, author, subject and
    `of' -- for a copy, the commit first made, whose work it is; None for
    new work.  An amend is a copy of the commit it replaced, a rebase's or
    a cherry-pick's of the one with its author, author date and subject --
    two made in one second are told apart by it -- or, reworded, of the
    first with its author and author date; a copy of none known -- made
    before SINCE, or by someone else -- is no work of this checkout's."""
    seen = dict(known)
    first: dict[tuple[str, str], str] = {}
    by_author: dict[str, str] = {}
    for sha, c in seen.items():
        if not c.get("of"):
            first.setdefault((c["author"], c.get("subject") or ""), sha)
            by_author.setdefault(c["author"], sha)
    out = []
    for e in reversed(entries):
        what = kind(e["action"])
        if what is None or e["sha"] in seen or len(e["parents"]) > 1 or e["at"] < since:
            continue
        of = None
        if what == "amend" and e["before"] in seen:
            of = seen[e["before"]].get("of") or e["before"]
        elif what == "copy":
            of = first.get((e["author"], e["subject"])) or by_author.get(e["author"])
            if of is None:
                continue
        c = {"sha": e["sha"], "at": e["at"], "author": e["author"], "subject": e["subject"], "of": of}
        seen[e["sha"]] = c
        if of is None:
            first.setdefault((e["author"], e["subject"]), e["sha"])
            by_author.setdefault(e["author"], e["sha"])
        out.append(c)
    return out


def recopied(commits: Iterable[tuple[str, str, str | None, str | None, float]]) -> dict[str, str]:
    """Of COMMITS kept (sha, author, subject, of, at), the copies `new_commits'
    gave to another commit than it gives them now: one of its author and
    author date but another subject, while one with theirs was made too --
    two commits made in one second, rebased.  sha -> the commit it is a
    copy of.  A copy whose subject is not its commit's for another reason
    (a reword, an amend) is left as it is."""
    commits = list(commits)
    first: dict[tuple[str, str], str] = {}
    subject = {}
    for sha, author, subj, of, _ in sorted(commits, key=lambda c: (c[4], c[0])):
        subject[sha] = subj or ""
        if not of:
            first.setdefault((author, subj or ""), sha)
    out = {}
    for sha, author, subj, of, _ in commits:
        right = first.get((author, subj or "")) if of else None
        if right and right != of and subject.get(of) != (subj or ""):
            out[sha] = right
    return out


def numstat(repo: str, shas: list[str]) -> dict[str, int]:
    "Insertions plus deletions of each of SHAS (binary files not counted), in one `git show'."
    out = _git(repo, "show", "--numstat", "--no-renames", "--format=%x1e%H", *shas) if shas else None
    lines: dict[str, int] = {}
    sha = None
    for row in (out or "").split("\n"):   # not splitlines: \x1e is a line break to it
        if row.startswith("\x1e"):
            sha = row[1:].strip()
            lines[sha] = 0
            continue
        parts = row.split("\t")
        if sha and len(parts) >= 3 and parts[0].isdigit() and parts[1].isdigit():
            lines[sha] += int(parts[0]) + int(parts[1])
    return lines


def lines_of(repo: str, sha: str) -> int:
    "Insertions plus deletions of commit SHA (binary files not counted)."
    return numstat(repo, [sha]).get(sha, 0)


# ---- Which work a commit was made for ----------------------------------------------


def cover(spans: list[list[float]], start: float, end: float) -> list[list[float]]:
    "SPANS (times a checkout's reflog has been read for, [from, to]) with START to END added."
    out = []
    for s, e in sorted(spans + [[start, end]]):
        if out and s <= out[-1][1] + 1:
            out[-1][1] = max(out[-1][1], e)
        else:
            out.append([s, e])
    return out


def covers(spans: Iterable[list[float]], start: float, end: float) -> bool:
    "Whether SPANS (`cover') hold all of START to END."
    return any(s <= start and end <= e for s, e in spans)


def attribute(commits: dict[str, list[dict[str, Any]]], spans: Iterable[tuple[str, str, float, float]],
              homes: dict[str, list[str]], covered: dict[str, list[list[float]]],
              now: float | None = None) -> dict[tuple[str, str], dict[str, Any]]:
    """Each agent's piece of work's commits: COMMITS (checkout -> those made
    there, `new_commits'' with lines and merged), SPANS (item, agent, first
    and last reply), HOMES (agent -> its checkouts), COVERED (checkout ->
    when its reflog was read, `cover').  A commit is the work of the agent
    whose checkout it was made in, on what it was then: of the work there
    whose replies it falls among -- up to COMMIT_SLACK after the last: an
    agent commits, then says so -- the latest begun.  A piece of work is
    `read' when its checkout's reflog was, from its start to its last
    reply: none found then is none made.
    A commit's lines are its last copy's -- an amend, a rebase -- and it is
    merged when any copy is; a copy is shown where it was made, as one."""
    now = time.time() if now is None else now
    last: dict[str, dict[str, Any]] = {}
    for c in sorted((c for cs in commits.values() for c in cs), key=lambda c: c["at"]):
        root = c.get("of") or c["sha"]
        got = last.setdefault(root, {"lines": c["lines"], "merged": False})
        got["lines"] = c["lines"]
        got["merged"] = got["merged"] or bool(c.get("merged"))
    out: dict[tuple[str, str], dict[str, Any]] = {}
    works: dict[str, list[tuple[float, float, tuple[str, str]]]] = {}
    for item, agent, first, end in spans:
        key = (item, agent)
        out[key] = {"commits": [], "read": False}
        for d in homes.get(agent, ()):
            works.setdefault(d, []).append((first, end, key))
            if covers(covered.get(d, ()), first - 5, min(end, now)):
                out[key]["read"] = True
    for d, cs in commits.items():
        here = works.get(d, [])
        for c in cs:
            on = [w for w in here if w[0] - 5 <= c["at"] <= w[1] + COMMIT_SLACK]
            if not on:
                continue
            begun = [w for w in on if w[0] <= c["at"]]
            key = max(begun)[2] if begun else min(on)[2]
            root = last.get(c["sha"], {}) if not c.get("of") else {}
            out[key]["commits"].append(dict(c, lines=root.get("lines", c["lines"]),
                                            merged=root.get("merged", bool(c.get("merged")))))
    return out


# ---- In a main branch ------------------------------------------------------------------


def naming(repo: str, ask: str) -> list[list[Any]]:
    """The commits in REPO's main branch whose message names ASK -- this
    herd's habit, "(ask ID)" -- for an ask whose taker's checkout was
    never read: [sha, lines, merged, subject]."""
    ref = main_branch(repo)
    out = _git(repo, "log", ref or "HEAD", "--no-merges", "--reverse", "--format=%H%x1f%s", "-F",
               f"--grep=ask {ask}") or ""
    found = [line.split("\x1f", 1) for line in out.splitlines() if "\x1f" in line]
    lines = numstat(repo, [sha for sha, _ in found])
    return [[sha, lines.get(sha, 0), 1 if ref else 0, subject] for sha, subject in found]


def main_branch(repo: str) -> str | None:
    "REPO's main branch: origin's default, else main or master, as a local branch."
    head = (_git(repo, "symbolic-ref", "--short", "refs/remotes/origin/HEAD") or "").strip()
    for name in ([head.split("/", 1)[1]] if "/" in head else []) + ["main", "master"]:
        if _git(repo, "rev-parse", "--verify", "--quiet", "refs/heads/" + name) is not None:
            return name
    return None


def in_main(repo: str, shas: Iterable[str], since: float) -> set[str] | None:
    """Those of SHAS in REPO's main branch -- the local one, or origin's as
    last fetched: a pull request merged on the forge is there before it is
    pulled -- looked for among its commits since SINCE.  A squash or a
    rebase on merging makes other commits: these are not in it.  None when
    REPO has no main branch to look in."""
    name = main_branch(repo)
    refs = ["refs/heads/" + name] if name else []
    for other in [name] if name else ["main", "master"]:
        if _git(repo, "rev-parse", "--verify", "--quiet", "refs/remotes/origin/" + other) is not None:
            refs.append("refs/remotes/origin/" + other)
    if not refs:
        return None
    out = _git(repo, "rev-list", f"--since=@{int(since)}", *refs)
    return None if out is None else set(out.split()) & set(shas)


# ---- Where a task's prompt is ------------------------------------------------------


def offset_of(path: str, cli: str, tid: str, chunk: int = 4 << 20) -> int | None:
    """Where in transcript PATH task TID's prompt begins, read again in
    `Tracker''s own way: for a task kept before tasks kept it."""
    t = Tracker(path, cli, None)
    offset, more = 0, True
    while more:
        try:
            offset, more = read_from(Path(path), t, offset, chunk)
        except OSError:
            return None
        hit = next((x for x in t.tasks if x["id"] == tid), None)
        if hit:
            return hit["offset"]
        t.tasks.clear()
    return None


def text(n: int | None) -> str:
    return "—" if n is None else tally.text(n)
