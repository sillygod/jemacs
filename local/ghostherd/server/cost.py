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
shell that ended, a command -- the work goes on as it was.  An ask is
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

What a task produced: the commits it brought.  Lead mode: each ask's
taker's commits while it worked on it, in its own checkout, once they are
in its project's main branch (merged, not rebased: a squash or a rebase
makes new commits, which are not these).  Direct mode: the commits made
in the agent's checkout while it worked on the task.  Lines changed are
insertions plus deletions, merges left out.  A task with none is "no
code"; one whose checkout could not be read, unknown.
"""

from __future__ import annotations

import hashlib
import json
import re
import statistics
import subprocess
import time
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
# The role a lead is owed opens its first message (`ghostherd--lead-role'):
# what follows its rule is the user's.
LEAD_ROLE = "You are the lead of this project's herd of agents"
ROLE_RULE = "\n---\n"
PROMPT_MAX = 200
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
    if text.lstrip().startswith(RELAY):
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
    if body.startswith(LEAD_ROLE) and ROLE_RULE in body:
        body = body.split(ROLE_RULE, 1)[1]
    body = body.strip()
    return ("user", body) if body else None


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
            self.tasks.append({"id": tid, "at": when, "prompt": conversation.first_line(what, PROMPT_MAX)})
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


def costs(tasks: list[dict[str, Any]], usage: list[dict[str, Any]], asked: dict[str, str],
          commits: dict[str, list[list[Any]]], now: float | None = None) -> dict[str, dict[str, Any]]:
    """Each of TASKS' cost and output, by id: from USAGE (rows of item,
    agent, sub and the kinds), ASKED (ask -> the work it was made for) and
    COMMITS (`task:ID' or `ask:ID' -> [sha, lines, merged]).  The task's
    own agent's replies are its `own' -- the lead's, in lead mode -- the
    rest of the herd's its `workers'', sub-agents' apart.  Work not over by
    NOW -- SETTLED quiet -- has had its commits not looked for yet: pending."""
    now = time.time() if now is None else now
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
    result = {}
    for t in tasks:
        got = out[t["id"]]
        shas: dict[str, int] = {}
        known, waiting = False, False
        if t["mode"] == "direct":
            listed = commits.get("task:" + t["id"])
            if listed is not None:
                known = True
                shas.update({c[0]: c[1] for c in listed})
        else:
            # A lead's own work makes no code here: its asks' takers' does.
            chain = got["asks"] | set(asks_in(t["id"], asked))
            known = not chain
            for a in sorted(chain):
                listed = commits.get("ask:" + a)
                if listed is not None:
                    known = True
                    shas.update({c[0]: c[1] for c in listed if c[2]})
                    waiting = waiting or any(not c[2] for c in listed)
        total = tally.Usage().add(got["own"]).add(got["workers"]).add(got["subs"])
        recent = now - (got["end"] or t.get("at") or 0) < SETTLED
        result[t["id"]] = {
            "id": t["id"], "agent": t["agent"], "project": t["project"], "mode": t["mode"],
            "prompt": t.get("prompt") or "", "at": t.get("at"), "end": got["end"],
            "total": total.weighted(), "own": got["own"].weighted(), "workers": got["workers"].weighted(),
            "subs": got["subs"].weighted(), "kinds": total.shown(), "asks": len(got["asks"]),
            "commits": len(shas), "lines": sum(shas.values()),
            # Made, but not in the main branch yet: no code so far, but not none.
            "code": ("yes" if shas else "unmerged" if waiting else "no" if known
                     else "pending" if recent else "unknown"),
        }
    return result


def compare(rows: Iterable[dict[str, Any]], days: int, today: date | None = None) -> dict[str, Any]:
    """Lead mode against direct mode over the last DAYS local days (today
    and the ones before): ROWS are `costs'' tasks.  Per mode: the tasks,
    the median and total per task, the tokens per 100 changed lines -- all
    the mode's tokens over all its lines: a commit counts in the task it was
    made in, and "commit and push" is a task of its own, so per task it
    would flatter the one that only committed -- and, for lead mode, the
    lead's own share."""
    today = today or date.today()
    since = datetime.combine(today - timedelta(days=days - 1), datetime.min.time()).timestamp()
    out: dict[str, Any] = {"days": days, "since": (today - timedelta(days=days - 1)).isoformat()}
    for mode in ("lead", "direct"):
        mine = [r for r in rows if r["mode"] == mode and (r["at"] or 0) >= since]
        coded = [r for r in mine if r["lines"]]
        total = sum(r["total"] for r in mine)
        lines = sum(r["lines"] for r in coded)
        out[mode] = {
            "tasks": len(mine),
            "median": round(statistics.median([r["total"] for r in mine])) if mine else 0,
            "total": total,
            "per100": round(total * 100 / lines) if lines else None,
            "lines": lines, "coded": len(coded),
            "noCode": sum(1 for r in mine if r["code"] == "no"),
            "unmerged": sum(1 for r in mine if r["code"] == "unmerged"),
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


def lines_of(repo: str, sha: str) -> int:
    "Insertions plus deletions of commit SHA (binary files not counted)."
    out = _git(repo, "show", "--numstat", "--format=", "--no-renames", sha) or ""
    n = 0
    for row in out.splitlines():
        parts = row.split("\t")
        if len(parts) >= 3 and parts[0].isdigit() and parts[1].isdigit():
            n += int(parts[0]) + int(parts[1])
    return n


def made(repo: str, since: float, until: float) -> list[list[Any]]:
    """The commits made in REPO's checkout -- on what it has checked out,
    its own line of them: what other branches brought in by a merge is
    another's work -- from SINCE to UNTIL (seconds), merges left out,
    oldest first: [sha, lines]."""
    out = _git(repo, "log", "HEAD", "--first-parent", "--no-merges", "--reverse", "--format=%H",
               f"--since=@{int(since)}", f"--until=@{int(until) + COMMIT_SLACK}") or ""
    return [[sha, lines_of(repo, sha)] for sha in out.split()]


def naming(repo: str, ask: str) -> list[list[Any]]:
    """The commits in REPO's main branch whose message names ASK -- this
    herd's habit, "(ask ID)" -- for an ask whose taker's checkout is gone."""
    ref = main_branch(repo)
    out = _git(repo, "log", ref or "HEAD", "--no-merges", "--reverse", "--format=%H", "-F", f"--grep=ask {ask}") or ""
    return [[sha, lines_of(repo, sha)] for sha in out.split()]


def main_branch(repo: str) -> str | None:
    "REPO's main branch: origin's default, else main or master, as a local branch."
    head = (_git(repo, "symbolic-ref", "--short", "refs/remotes/origin/HEAD") or "").strip()
    for name in ([head.split("/", 1)[1]] if "/" in head else []) + ["main", "master"]:
        if _git(repo, "rev-parse", "--verify", "--quiet", "refs/heads/" + name) is not None:
            return name
    return None


def merged(repo: str, sha: str, ref: str | None) -> bool:
    "Whether SHA is in REPO's main branch REF."
    if not ref:
        return False
    try:
        r = subprocess.run(["git", "-C", repo, "merge-base", "--is-ancestor", sha, ref],
                           capture_output=True, timeout=GIT_TIMEOUT)
    except (OSError, subprocess.SubprocessError):
        return False
    return r.returncode == 0


def text(n: int | None) -> str:
    return "—" if n is None else tally.text(n)
