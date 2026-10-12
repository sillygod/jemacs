"""What an agent hands its heir: where its work stood, as the heir's
first message.

An agent stops -- its CLI out of usage, wedged, or you would rather
another CLI went on -- and a new agent, on the same CLI or another,
takes its name and its work.  The old one may not be able to say
anything any more, so nothing here asks it: its transcript says what
it was asked and what it did, the herd what it owes and waits on, and
git where the checkout is.  The heir is a model too: given this, it
works out the rest itself, the transcript there if it wants more.
"""

from __future__ import annotations

import re
import shlex
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Any

import conversation

BRIEF_MAX = 16000
WORK_MAX = 1500  # one thing it was asked
LAST_MAX = 8000  # its last answer, when the brief has the room
LAST_MIN = 2000  # and when it has not
STEP_MAX = 400  # any other step
WORK_LATER = 5  # instructions after the first
STEPS = 14
HEAD_BYTES = 512 * 1024
GIT_LINES = 30
GIT_TIMEOUT = 5

# An agent's name, and a CLI's, as a relay takes them from outside.
NAME = re.compile(r"\A[A-Za-z0-9][A-Za-z0-9_.-]{0,63}\Z")
KIND = re.compile(r"\A[a-z][a-z0-9-]{0,31}\Z")

# How a brief ends: the heir goes on with work cut short, and does
# nothing yet with work that had come to a stop of its own.
GO_ON = ("Its turn was cut short.  Go on from where it stopped: first say in two lines what the "
         "work is and what you will do next, then do it.  Do not redo what is done; if its last "
         "answer above shows the work finished after all, say so and wait to be told what is next.")
WAIT = ("Its turn had ended: it was waiting to be told what is next.  So do nothing yet: say in "
        "two lines what the work is and where it stands, then wait to be told.")
CUT = "\n[… the rest is left out: the brief is too long.]\n\n"

# A brief, as the heir was given it: its own heir carries the work on.
RELAY = re.compile(r"\A\[Relay: ")
WORK = re.compile(r"^## The work\n(.*?)(?=^## |\Z)", re.S | re.M)
INSTRUCTIONS = ("prompt", "ask", "message")
ORDINALS = ("first", "second", "third", "fourth", "fifth", "sixth", "seventh",
            "eighth", "ninth", "tenth")


def ordinal(n: int) -> str:
    return f"the {ORDINALS[n - 1]}" if 1 <= n <= len(ORDINALS) else f"number {n}"


def _when(ts: Any) -> str:
    t = conversation._epoch(ts)
    return datetime.fromtimestamp(t).strftime("%m-%d %H:%M") if t else ""


def _indent(text: str) -> str:
    return text.replace("\n", "\n  ")


def _head_entries(path: Path, cli: str) -> list[dict[str, Any]]:
    "The first entries of transcript PATH: where its work began."
    with path.open("rb") as f:
        data = f.read(HEAD_BYTES)
        more = bool(f.read(1))
    lines = data.decode("utf-8", "replace").splitlines()
    if more and lines:
        lines = lines[:-1]  # cut mid-line
    thread = conversation.Thread()
    conversation.READERS[cli](conversation._records(l for l in lines if l.strip()), thread)
    return thread.entries


def _instruction(e: dict[str, Any], limit: int = WORK_MAX) -> str:
    when = _when(e.get("ts"))
    who = e.get("who") or ""
    if e.get("kind") == "ask":
        said = f"ask {e.get('ask')} from {who or 'another agent'}"
    elif who:
        said = f"{who} wrote"
    else:
        said = "you were told"
    text = conversation.clip(e.get("text") or "", limit)
    return f"- {when + ' ' if when else ''}{said}:\n  {_indent(text)}"


def _work(head: list[dict[str, Any]], tail: list[dict[str, Any]]) -> str:
    """What it was asked: the first instruction, or the work a brief of
    its own carried, then the latest ones."""
    asked = [e for e in head if e.get("role") == "user" and e.get("kind") in INSTRUCTIONS]
    first = asked[0] if asked else None
    out = []
    carried = WORK.search(first.get("text") or "") if first and RELAY.match(first.get("text") or "") else None
    if carried:
        out.append(carried.group(1).strip())
    elif first:
        out.append(_instruction(first))
    later = [e for e in tail if e.get("role") == "user" and e.get("kind") in INSTRUCTIONS
             and not RELAY.match(e.get("text") or "")
             and not (first and e.get("text") == first.get("text") and e.get("ts") == first.get("ts"))]
    if len(later) > WORK_LATER:
        out.append(f"- … {len(later) - WORK_LATER} more, then:")
    out.extend(_instruction(e) for e in later[-WORK_LATER:])
    return "\n".join(out)


def _said(text: str, limit: int) -> str:
    """TEXT, its last answer, whole -- or cut at LIMIT, saying it is the
    brief that cut it: a bare … reads as an answer it never finished."""
    text = text.strip()
    if len(text) <= limit:
        return text
    kept = text[:limit].rstrip()
    return (f"{kept}\n[… {len(text) - len(kept)} more characters of this answer are left out "
            "of the brief: it goes on in its conversation.]")


def _step(e: dict[str, Any]) -> str | None:
    role, kind = e.get("role"), e.get("kind")
    clip = lambda t: _indent(conversation.clip(t or "", STEP_MAX))  # noqa: E731
    if role == "assistant":
        return f"- It said: {clip(e.get('text'))}"
    if role == "tools":
        hints = [t["name"] + (" " + t["hint"] if t.get("hint") else "") for t in e.get("tools") or []]
        shown = "; ".join(conversation.first_line(h, 100) for h in hints[-8:])
        return f"- It ran: {'… ' if len(hints) > 8 else ''}{shown}"
    if role == "herd" and kind == "ask":
        out = f"- It asked {e.get('to')}{' (ask ' + e['ask'] + ')' if e.get('ask') else ''}: {clip(e.get('text'))}"
        if e.get("answer") is not None:
            out += f"\n  Answered{' (failed)' if e.get('failed') else ''}: {clip(e.get('answer'))}"
        return out
    if role == "herd" and kind == "reply":
        return f"- It answered ask {e.get('ask')}: {clip(e.get('text'))}"
    if role == "user" and kind == "answer":
        how = " (failed)" if e.get("failed") else " (its last words, no answer sent)" if e.get("auto") else ""
        return f"- {e.get('who') or 'An agent'} answered ask {e.get('ask')}{how}: {clip(e.get('text'))}"
    if role == "user" and kind == "note":
        return f"- ({e.get('text')})"
    if role == "user" and kind == "command":
        return f"- You ran {e.get('text')}"
    if role == "user" and kind in INSTRUCTIONS:
        return f"- New instruction{' from ' + e['who'] if e.get('who') else ''} (under The work): " \
               f"{conversation.first_line(e.get('text') or '', 120)}"
    return None


def _steps(tail: list[dict[str, Any]], count: int = STEPS, limit: int = LAST_MAX) -> str:
    "Its last steps, latest last; its last answer as it said it, up to LIMIT."
    steps = [e for e in tail if not (e.get("role") == "user" and RELAY.match(e.get("text") or ""))]
    last = next((i for i in range(len(steps) - 1, -1, -1) if steps[i].get("role") == "assistant"), None)
    lines = []
    for i, e in enumerate(steps[-count:], start=max(0, len(steps) - count)):
        s = f"- It said: {_indent(_said(e.get('text') or '', limit))}" if i == last else _step(e)
        if s:
            lines.append(s)
    if len(steps) > count:
        lines.insert(0, f"- … {len(steps) - count} earlier steps")
    return "\n".join(lines)


def _git(cwd: Path, *args: str) -> str | None:
    try:
        r = subprocess.run(["git", "-C", str(cwd), *args], capture_output=True, text=True,
                           timeout=GIT_TIMEOUT, check=False)
    except (OSError, subprocess.SubprocessError):
        return None
    return r.stdout.rstrip("\n") if r.returncode == 0 else None


def _some(lines: list[str], limit: int = GIT_LINES) -> list[str]:
    return lines[:limit] + ([f"… and {len(lines) - limit} more"] if len(lines) > limit else [])


def checkout(project: str | None) -> str | None:
    "Where the checkout at PROJECT stands, or None when it is not one."
    if not project:
        return None
    root = Path(project).expanduser()
    if not root.is_dir() or _git(root, "rev-parse", "--is-inside-work-tree") != "true":
        return None
    branch = _git(root, "symbolic-ref", "--short", "-q", "HEAD")
    if not branch:
        sha = _git(root, "rev-parse", "--short", "HEAD")
        branch = f"no branch, at {sha}" if sha else "no commits yet"
    out = [f"On {branch}."]
    status = [l for l in (_git(root, "status", "--short") or "").splitlines() if l.strip()]
    if status:
        out.append("Not committed:")
        out.extend("  " + l for l in _some(status))
        stat = [l for l in (_git(root, "diff", "HEAD", "--stat=100") or "").splitlines() if l.strip()]
        if stat:
            out.append("Changed since the last commit:")
            out.extend("  " + l.strip() for l in _some(stat[:-1]) + stat[-1:])
    else:
        out.append("Nothing uncommitted.")
    log = (_git(root, "log", "--oneline", "-5") or "").splitlines()
    if log:
        out.append("Latest commits:")
        out.extend("  " + l for l in log)
    return "\n".join(out)


def _asks(asks: dict[str, list[dict[str, Any]]], client: str) -> str:
    out = []
    for a in asks.get("taking") or []:
        out.append(f"- ask {a['id']} from {a['from']} waits for your answer:\n  "
                   f"{_indent(conversation.clip(a.get('body') or '', WORK_MAX))}\n"
                   f"  When you are done, answer it with:\n"
                   f"  {shlex.quote(client)} reply {a['id']} <<'HERD'\n  <your answer>\n  HERD")
    for a in asks.get("waiting") or []:
        state = "answered, its answer on its way to you" if a.get("status") == "answered" \
            else "failed" if a.get("status") == "failed" else "still open"
        out.append(f"- your ask {a['id']} of {a['to']}, {state}: "
                   f"{conversation.first_line(a.get('body') or '', 200)}")
    return "\n".join(out)


def brief(name: str, path: str | None, project: str | None, old: str, new: str,
          generation: int, reason: str = "", notes: str = "",
          asks: dict[str, list[dict[str, Any]]] | None = None,
          client: str = "herd", cut: bool = False) -> dict[str, Any]:
    """NAME's work as its heir is told it: NAME an OLD agent until now, a
    NEW one from now, the GENERATION-th of the name; REASON why the old
    one stopped, CUT whether it stopped mid-turn.  PATH is its
    transcript, PROJECT its checkout, CLIENT the herd client an ask is
    answered with.  The heir goes on with the work when it was cut
    short or it owes an answer to an ask; otherwise it waits."""
    asks = asks or {}
    real = None
    why = ""
    if path:
        try:
            real, cli = conversation.locate(path)
        except ValueError as e:
            why = str(e)
    else:
        why = "the herd does not know where its transcript is"
    head: list[dict[str, Any]] = []
    tail: list[dict[str, Any]] = []
    if real:
        tail = conversation.read(str(real), conversation.LIMIT)["entries"]
        head = _head_entries(real, cli)
    because = f" -- {reason.strip().rstrip('.')}" if reason.strip() else ""
    where = ""
    if real:
        shown, home = str(real), str(Path.home().resolve())
        if shown.startswith(home + "/"):
            shown = "~" + shown[len(home):]
        where = f", which is at {shown} if you need more"
    top = (f"[Relay: you take over from {name}, {'an' if old[:1] in 'aeiou' else 'a'} {old} agent "
           f"that stopped{because}.  You are {name} now, {ordinal(generation)} of the name: the herd "
           f"and its other agents know you by it, and what is sent to {name} comes to you.  What "
           f"follows is where its work stood, read from its conversation{where}.]")
    work = _work(head, tail) if real else ""
    if not work:
        work = (f"(Its conversation could not be read: {why}.)" if not real else "(Nothing it was asked is in its conversation.)")
        if notes.strip():
            work += f"\nIts notes in the herd: {notes.strip()}"
    sections = [top, "## The work\n" + work]
    asks_text = _asks(asks, client)
    git = checkout(project)
    end = GO_ON if cut or asks.get("taking") else WAIT

    def assemble(count: int, limit: int) -> str:
        parts = list(sections)
        steps = _steps(tail, count, limit) if real else ""
        if steps:
            parts.append("## Where it got to\n" + steps)
        if asks_text:
            parts.append("## Asks\n" + asks_text)
        if git:
            parts.append("## The checkout\n" + git)
        parts.append(end)
        return "\n\n".join(parts)

    # Too long: its older steps go first, then its last answer is cut,
    # then whatever is left over.
    said = next((e.get("text") or "" for e in reversed(tail) if e.get("role") == "assistant"), "")
    count, limit = STEPS, min(LAST_MAX, len(said.strip()))
    text = assemble(count, limit)
    while len(text) > BRIEF_MAX and count > 2:
        count -= 2
        text = assemble(count, limit)
    if len(text) > BRIEF_MAX and limit > LAST_MIN:
        limit = max(LAST_MIN, limit - (len(text) - BRIEF_MAX) - 120)
        text = assemble(count, limit)
    if len(text) > BRIEF_MAX:
        text = text[: BRIEF_MAX - len(end) - len(CUT)].rstrip() + CUT + end
    return {
        "text": text,
        "transcript": str(real) if real else None,
        "taking": [a["id"] for a in asks.get("taking") or []],
        "waiting": [a["id"] for a in asks.get("waiting") or []],
    }
