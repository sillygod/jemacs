"""Threads: who is talking to whom across the herd, and about what.

A thread is a chain of work: a root ask -- one its asker made while
answering none -- and every ask made while answering one in it (an ask's
`parent', which the herd keeps for rounds), handoffs across projects
included, with what came back: the answer, the taker's last words when it
stopped without one, the failure.  Built from the asks table and the herd's
snapshot (each agent's CLI, project, role and state); read-only.  The
user's own prompt that set a root going is Emacs's to add: its log keeps
what the user typed to whom.

`summaries' is the list the page shows (newest activity first); `detail'
is one thread, its lanes and its messages in time order; `talking' is every
open ask in the herd, for the strip across the top.
"""

from __future__ import annotations

from datetime import datetime
from typing import Any, Iterable

import projects

ASK_OPEN = ("queued", "delivered")
# A message's text as one thread carries it: enough for the longest ask
# the herd delivers, not a transcript.
TEXT_MAX = 20000
# Its first lines, for the diagram's card and the list's title.
HEAD_LINES = 3
HEAD_MAX = 240
TITLE_MAX = 90
# The asks the list is built from, most recent activity first.  The page
# asks for a few dozen threads; a thread rarely has more than a handful of
# asks, so this many covers them and their roots.
WINDOW = 400
CLIS = ("claude", "agy", "grok")


def ms(iso: str | None) -> float | None:
    return datetime.fromisoformat(iso).timestamp() * 1000 if iso else None


def head(text: str | None, lines: int = HEAD_LINES, width: int = HEAD_MAX) -> str:
    "TEXT's first non-blank LINES, cut to WIDTH."
    kept = [ln.rstrip() for ln in (text or "").splitlines() if ln.strip()][:lines]
    out = "\n".join(kept)
    return out if len(out) <= width else out[: width - 1].rstrip() + "…"


def title(text: str | None) -> str:
    return head(text, 1, TITLE_MAX)


def taker(row: dict[str, Any]) -> str:
    "Who an ask went to: a handoff's lead is '' while the herd starts it."
    return row["to_name"] or ("lead of " + projects.leaf(row["project"]) if row.get("project") else "?")


def root_of(ask_id: str, by_id: dict[str, dict[str, Any]]) -> str:
    "The root of ASK_ID's chain among BY_ID: its parents followed while known."
    seen = set()
    while True:
        parent = (by_id.get(ask_id) or {}).get("parent")
        if not parent or parent not in by_id or parent in seen:
            return ask_id
        seen.add(ask_id)
        ask_id = parent


def depth(ask_id: str, by_id: dict[str, dict[str, Any]]) -> int:
    n, seen = 0, set()
    while (p := (by_id.get(ask_id) or {}).get("parent")) and p in by_id and p not in seen:
        seen.add(p)
        n, ask_id = n + 1, p
    return n


def group(rows: Iterable[dict[str, Any]]) -> dict[str, list[dict[str, Any]]]:
    "ROWS by the root of their chain, each list in the order asked."
    by_id = {r["id"]: r for r in rows}
    out: dict[str, list[dict[str, Any]]] = {}
    for r in sorted(by_id.values(), key=lambda r: r["created_at"]):
        out.setdefault(root_of(r["id"], by_id), []).append(r)
    return out


def last_at(rows: list[dict[str, Any]]) -> str:
    return max(r["answered_at"] or r["delivered_at"] or r["created_at"] for r in rows)


def status(rows: list[dict[str, Any]], snap: dict[str, dict[str, Any]]) -> str:
    """live: an ask in it is open and its taker is working; waiting: open,
    and nobody on it is working (queued, or a taker idle, blocked, gone);
    failed: its root failed or was cancelled; answered: the rest."""
    open_ = [r for r in rows if r["status"] in ASK_OPEN]
    if open_:
        working = any((snap.get(r["to_name"]) or {}).get("state") == "working"
                      for r in open_ if r["status"] == "delivered")
        return "live" if working else "waiting"
    roots = [r for r in rows if not r.get("parent") or r["parent"] not in {x["id"] for x in rows}]
    if any(r["status"] in ("failed", "cancelled") for r in roots):
        return "failed"
    return "answered"


def kind_of(name: str, snap: dict[str, dict[str, Any]]) -> str:
    "NAME's CLI, from the snapshot or its name; `you' for the user."
    if name == "user":
        return "you"
    k = (snap.get(name) or {}).get("kind") or ""
    if k:
        return k
    return next((c for c in CLIS if name.startswith(c)), "")


def lane(name: str, rows: list[dict[str, Any]], snap: dict[str, dict[str, Any]]) -> dict[str, Any]:
    "One participant: its name, CLI, project (a leaf) and role."
    s = snap.get(name) or {}
    project = s.get("project") or ""
    if not project:
        for r in rows:  # a handoff names the projects at both ends
            if r.get("project") and taker(r) == name:
                project = r["project"]
            elif r.get("from_project") and r["from_name"] == name:
                project = r["from_project"]
    role = s.get("role") or ("primary" if name == "primary" else "lead" if name.startswith("lead-") else "")
    return {"name": name, "kind": kind_of(name, snap), "project": projects.leaf(project) if project else "",
            "role": role, "worktree": bool(s.get("branch"))}


def messages(rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Each ask and what came back, in time order: an ask (a handoff when it
    goes to another project), then its answer -- or the taker's last words
    when it stopped without one, its failure, its cancelling."""
    out = []
    for r in rows:
        to = taker(r)
        out.append({
            "kind": "handoff" if r.get("project") else "ask", "from": r["from_name"], "to": to,
            "at": ms(r["created_at"]), "ask": r["id"], "round": r.get("round"), "of": r.get("round_of"),
            "head": head(r["body"]), "text": (r["body"] or "")[:TEXT_MAX], "status": r["status"],
        })
        if r["status"] == "answered":
            out.append({"kind": "answer", "from": to, "to": r["from_name"], "at": ms(r["answered_at"]),
                        "ask": r["id"], "auto": bool(r.get("auto")), "head": head(r.get("reply")),
                        "text": (r.get("reply") or "")[:TEXT_MAX]})
        elif r["status"] in ("failed", "cancelled"):
            why = r.get("error") or ("cancelled" if r["status"] == "cancelled" else "failed")
            out.append({"kind": r["status"], "from": to, "to": r["from_name"],
                        "at": ms(r["answered_at"] or r["created_at"]), "ask": r["id"],
                        "head": head(why), "text": why[:TEXT_MAX]})
    out.sort(key=lambda m: m["at"] or 0)
    return out


def participants(msgs: list[dict[str, Any]]) -> list[str]:
    seen: list[str] = []
    for m in msgs:
        for n in (m["from"], m["to"]):
            if n not in seen:
                seen.append(n)
    return seen


def _projects(lanes: list[dict[str, Any]]) -> list[str]:
    out: list[str] = []
    for ln in lanes:
        p = ln["project"]
        if p and ln["role"] != "primary" and p not in out:
            out.append(p)
    return out


def waiting(rows: list[dict[str, Any]], snap: dict[str, dict[str, Any]],
            by_id: dict[str, dict[str, Any]]) -> list[dict[str, Any]]:
    "The open asks in ROWS, deepest first: who waits on whom, and how it goes."
    out = []
    for r in rows:
        if r["status"] not in ASK_OPEN:
            continue
        to = taker(r)
        out.append({"ask": r["id"], "from": r["from_name"], "to": to, "status": r["status"],
                    "since": ms(r["delivered_at"] or r["created_at"]),
                    "working": r["status"] == "delivered" and (snap.get(to) or {}).get("state") == "working",
                    "depth": depth(r["id"], by_id)})
    out.sort(key=lambda w: -w["depth"])
    return out


def summary(root: str, rows: list[dict[str, Any]], snap: dict[str, dict[str, Any]]) -> dict[str, Any]:
    "A thread as the list shows it."
    msgs = messages(rows)
    lanes = [lane(n, rows, snap) for n in participants(msgs)]
    first = rows[0]
    projs = _projects(lanes)
    return {
        "id": root, "title": title(first["body"]), "status": status(rows, snap),
        "participants": [ln["name"] for ln in lanes], "projects": projs,
        "cross": len(projs) > 1 or any(r.get("project") for r in rows),
        "count": len(msgs), "open": sum(r["status"] in ASK_OPEN for r in rows),
        "started": ms(first["created_at"]), "at": ms(last_at(rows)),
        "rootFrom": first["from_name"], "rootTo": taker(first),
    }


def summaries(rows: list[dict[str, Any]], snap: dict[str, dict[str, Any]],
              limit: int) -> tuple[list[dict[str, Any]], bool]:
    "The LIMIT threads with the latest activity in ROWS, and whether there are more."
    groups = group(rows)
    order = sorted(groups, key=lambda k: last_at(groups[k]), reverse=True)
    return [summary(k, groups[k], snap) for k in order[:limit]], len(order) > limit


def detail(root: str, rows: list[dict[str, Any]], snap: dict[str, dict[str, Any]]) -> dict[str, Any]:
    "One thread: its lanes, its messages in time order, who waits on whom."
    rows = sorted(rows, key=lambda r: r["created_at"])
    by_id = {r["id"]: r for r in rows}
    msgs = messages(rows)
    lanes = [lane(n, rows, snap) for n in participants(msgs)]
    out = summary(root, rows, snap)
    out.update(lanes=lanes, msgs=msgs, waiting=waiting(rows, snap, by_id))
    return out


def talking(open_rows: list[dict[str, Any]], snap: dict[str, dict[str, Any]],
            by_id: dict[str, dict[str, Any]]) -> list[dict[str, Any]]:
    "Every open ask in the herd: from, to, kind, how long, queued or working; its thread."
    out = []
    for r in sorted(open_rows, key=lambda r: r["created_at"]):
        to = taker(r)
        out.append({"ask": r["id"], "thread": root_of(r["id"], by_id), "from": r["from_name"], "to": to,
                    "kind": "handoff" if r.get("project") else "ask", "status": r["status"],
                    "since": ms(r["delivered_at"] or r["created_at"]),
                    "working": r["status"] == "delivered" and (snap.get(to) or {}).get("state") == "working"})
    return out
