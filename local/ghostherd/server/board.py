"""The Board: the herd's work as items, what is open, and what each went through.

A work item is one piece of work, however many asks it took: a lead's ask
to a worker and the asks the worker made answering it, a lead's relay of
another lead's answer.  Most of the user's work has no ticket, and one
ticket spans several pieces of work, so a ticket is a label on an item,
never what makes one.

Each ask is put in an item once, as the herd first sees it, and stays
there (`place'):

1. the item a lead said it is for (`herd ask --item'), or of the ask it
   relays (`herd ask --relay-of');
2. the item of the ask it was made while answering (its `parent');
3. a relay of another's words (`relay', by how it begins) goes with the
   ask its relayer has just had answered (`RELAY_WINDOW');
4. by the asks and pull requests its body names, when the rules say so
   (`JOIN_BY'): measured on a day's real asks against a hand-made
   grouping, a named ask was another item's about half the time (a
   background "see 1a2b3c4d") and a pull request most of the time (a reply
   lists every PR's state), so by default neither moves an ask -- each is
   shown as a link instead;
5. else an item of its own.

A lead renames, merges, splits, labels and closes items by hand (`herd
item'); what a lead put somewhere stays there.  A pull request is its
item's whose ask named it first; the others it is named in link to it.

An item is never taken for done by its quiet: a piece of work nobody
picked up looks just as quiet as one finished.  It is `live' while an ask
in it is open, `quiet' when none is, and `done' only once a lead closed
it (and nothing was asked in it since).  What the forge says of its pull
requests, and what waits on the user, are Emacs's to add (the Board tab,
ghostherd-memory-page.el): `tail', its asks answered and a PR still open;
`jing', the user's to act on.

Nothing here touches the store: herd.py keeps the items and calls these on
rows it read.
"""

from __future__ import annotations

import re
from datetime import datetime
from typing import Any, Iterable

import links

ASK_OPEN = ("queued", "delivered")

# What moves an ask into the item of what its body names, besides its
# parent and a relay: "refs" (an ask's id), "prs" (a pull request).  Off by
# default; see the module's doc.
JOIN_BY: frozenset[str] = frozenset()

# An ask's id as a body or a reply names it: eight hex digits on their own.
# Only an id the herd keeps counts -- a commit's short hash is rarely eight
# digits long and never one of them.
ASK_ID = re.compile(r"(?<![0-9A-Za-z])([0-9a-f]{8})(?![0-9A-Za-z])")

# A pull request by its repository's name and number, as people write one:
# `shop #412', `shop PR #412', `billing #88、#89', `PR #7'.
# The repository must be one the herd has seen a link to, which says whose
# it is; a bare `PR #N' is the asker's or taker's project's repository.
SHORT_PR = re.compile(r"(?<![\w/.#-])([A-Za-z][\w.-]{0,60}?)[ \t]*(?:PR[ \t]*|pull request[ \t]*)?#(\d{1,6})(?!\d)",
                      re.IGNORECASE)
MORE_PR = re.compile(r"[ \t]*(?:、|,|，|/|&|\+|和|及|與|跟|and)[ \t]*#(\d{1,6})(?!\d)")
BARE_PR = re.compile(r"(?<![\w-])(?:PR|pull request)[ \t]*#(\d{1,6})(?!\d)", re.IGNORECASE)

# An ask that wants only to be read: "reply 收到", "no need to answer".
FYI = re.compile(
    r"回一句收到|回覆?「?收到|收到即可|收到就好|只需要?回|不需要回覆?|不用回覆?|看完回一句|僅供參考|"
    r"\bFYI\b|no (?:need|reply needed) to (?:reply|answer)|just (?:acknowledge|ack|reply (?:ok|got it))",
    re.IGNORECASE)
# A reply that only says it was read.
ACK = re.compile(
    r"\A[\s\W]*(?:收到|了解|知道了|好的?|OK|okay|ack(?:nowledged)?|noted|got it|roger|👍|✅)"
    r"(?:[\s\W]*(?:收到|了解|謝謝|感謝|thanks?|thank you|will do|會照做|照辦))*[\s\W]*\Z",
    re.IGNORECASE)
ACK_MAX = 40
# A lead passing on another's words as an ask of its own, by how it
# begins: "lead-x 轉來 ...", "lead-x 轉 lead-y 的回覆", "Relaying ...".
# Anywhere else "轉" is mostly something else ("轉給 Jing 跑").
RELAY = re.compile(
    r"\A\s*(?:\[[^\]\n]{0,80}\]\s*)?(?:[A-Za-z0-9][A-Za-z0-9._-]*[ \t]*)?(?:轉來|轉述|轉達|轉給你|轉交|轉[ \t:：])"
    r"|\A\s*(?:relay(?:ed|ing)?|forward(?:ed|ing)?)\b",
    re.IGNORECASE)
# What a relay passes on is the answer its relayer has just had: to an ask
# of its own, answered this long before at most, by another than the one
# it now tells.  An id a relay quotes is often one its words only mention.
RELAY_WINDOW = 300

TITLE_MAX = 48
HEAD_MAX = 140
STATUSES = ("live", "quiet", "done")


def ms(iso: str | None) -> float | None:
    return datetime.fromisoformat(iso).timestamp() * 1000 if iso else None


def secs(iso: str | None) -> float:
    return datetime.fromisoformat(iso).timestamp() if iso else 0.0


# ---- What a text names --------------------------------------------------------


def ask_ids(text: str | None, known: Iterable[str], own: str | None = None) -> list[str]:
    "The asks TEXT names that are in KNOWN, in the order named, not OWN."
    known = known if isinstance(known, (set, frozenset, dict)) else set(known)
    out: list[str] = []
    for m in ASK_ID.finditer(text or ""):
        a = m.group(1)
        if a in known and a != own and a not in out:
            out.append(a)
    return out


def pr_links(text: str | None) -> list[tuple[str, str, str, int]]:
    "The pull requests TEXT links to, as (host, owner, repo, number), in order."
    out: list[tuple[str, str, str, int]] = []
    for m in links.PR.finditer(text or ""):
        g = m.groups()
        for i, host in ((0, "github"), (3, "github"), (6, "bitbucket"), (9, "bitbucket")):
            if g[i]:
                key = (host, g[i].lower(), g[i + 1].lower(), int(g[i + 2]))
                if key not in out:
                    out.append(key)
                break
    return out


def pr_refs(text: str | None, repos: dict[str, tuple[str, str]],
            home: Iterable[str] = ()) -> list[tuple[str, str, str, int]]:
    """The pull requests TEXT names: its links, then `REPO #N' for a REPO in
    REPOS (repo -> (host, owner), learned from links), with `、#M' after it,
    and a bare `PR #N' as of the first of HOME, the projects of whoever asked
    and took it, that REPOS has."""
    out = pr_links(text)
    text = text or ""

    def add(repo: str, num: str) -> None:
        host, owner = repos[repo]
        key = (host, owner, repo, int(num))
        if key not in out:
            out.append(key)

    covered: list[tuple[int, int]] = []
    for m in SHORT_PR.finditer(text):
        repo = m.group(1).lower()
        if repo not in repos:
            continue
        add(repo, m.group(2))
        end = m.end()
        while (more := MORE_PR.match(text, end)):
            add(repo, more.group(1))
            end = more.end()
        covered.append((m.start(), end))
    mine = next((r.lower() for r in home if r and r.lower() in repos), None)
    if mine:
        for m in BARE_PR.finditer(text):
            if not any(a <= m.start() < b for a, b in covered):
                add(mine, m.group(1))
    return out


def tickets(text: str | None) -> list[str]:
    "The ClickUp tasks TEXT names, by id, in order."
    out: list[str] = []
    for m in links.TASK.finditer(text or ""):
        t = m.group(1) or m.group(2)
        if t not in out:
            out.append(t)
    return out


def fyi(body: str | None) -> bool:
    "Whether an ask wants only to be read."
    return bool(FYI.search((body or "")[:800]))


def ack(reply: str | None) -> bool:
    "Whether a reply only says it was read: 收到, OK, noted."
    r = (reply or "").strip()
    return bool(r) and len(r) <= ACK_MAX and bool(ACK.match(r))


def relay(body: str | None) -> bool:
    "Whether an ask passes on another's words, by how it begins."
    return bool(RELAY.search((body or "")[:200]))


# ---- What an item is called, and a line of a message ---------------------------

_NOISE = re.compile(r"^\s*(?:\[[^\]\n]*\]\s*)+")
_MARKS = re.compile(r"^\s*(?:#+\s+|[-*>]\s+|\d+[.)]\s+)")
# What a lead writes before what the work is, as the asks that began a
# day's items had it: where it comes from, whose, how urgent, in what
# order, the ticket -- and a line that is only background or rules.
_OPENERS = [re.compile(p, re.IGNORECASE) for p in (
    r"^[（(][^）)\n]{0,60}[）)][，,。.:：\s]*",
    r"^來自\s*[\w.-]+[^。\n]{0,80}?[。.]\s*",
    r"^[\w.-]+\s*(?:轉來|轉述|轉達|轉交|轉\s+[\w.-]+\s*的回覆)[^:：。\n]{0,60}[:：。]?\s*",
    r"^(?:緊急|急|urgent)[，,:：!！\s]+",
    r"^(?:新工作|新題目|新任務|新的?工作|new task)[^：:。\n]{0,40}[：:。]\s*",
    r"^(?:資安事件|incident)\s*(?:ClickUp\s*)?[0-9a-z]+[^。\n]{0,40}[。.]\s*",
    r"^(?:Jing\s*(?:已經|同意|說|要)[^：:\n]{0,30}[：:])\s*",
)]
_SKIP = re.compile(r"^(?:背景|context|background|規則|rules?|全程唯讀|唯讀)\s*[:：]?", re.IGNORECASE)
_SENTENCE = re.compile(r"(?<=[。！？!?])\s*|(?<=[.;；])\s+")


def head(text: str | None, width: int = HEAD_MAX) -> str:
    "TEXT's first line that says something, cut to WIDTH."
    for ln in (text or "").splitlines():
        s = _MARKS.sub("", _NOISE.sub("", ln)).replace("**", "").replace("`", "").strip()
        if s:
            return s if len(s) <= width else s[: width - 1].rstrip() + "…"
    return ""


def title(body: str | None) -> str:
    """An item's name from the ask that began it: its first sentence that
    says what the work is -- the lead's openers off, background and rules
    skipped -- cut to TITLE_MAX."""
    lines = [_MARKS.sub("", _NOISE.sub("", ln)).replace("**", "").replace("`", "").strip()
             for ln in (body or "").splitlines()]
    for ln in lines:
        for _ in range(4):
            before = ln
            for o in _OPENERS:
                ln = o.sub("", ln, count=1).strip()
            if ln == before:
                break
        if not ln or _SKIP.match(ln):
            continue
        s = _SENTENCE.split(ln, maxsplit=1)[0].strip() or ln
        return s if len(s) <= TITLE_MAX else s[: TITLE_MAX - 1].rstrip() + "…"
    return head(body, TITLE_MAX)


# ---- Where an ask goes ----------------------------------------------------------


def place(ask: dict[str, Any], item_of, pr_item, relayed,
          join: Iterable[str] | None = None) -> tuple[str | None, str, str | None]:
    """Where ASK goes: (ITEM, HOW, REF) -- ITEM None for an item of its own.
    ASK has `parent', `relay', `refs' (ask ids its body names, in order) and
    `prs' (pull requests it names).  ITEM_OF gives an ask's item, PR_ITEM a
    pull request's, RELAYED the ask a relay by its asker passes on (or
    None); JOIN what else moves it (`JOIN_BY')."""
    join = JOIN_BY if join is None else join
    parent = ask.get("parent")
    if parent and (it := item_of(parent)):
        return it, "parent", parent
    if ask.get("relay") and (src := relayed(ask)) and (it := item_of(src)):
        return it, "relay", src
    if "refs" in join:
        for ref in ask.get("refs") or []:
            if it := item_of(ref):
                return it, "ref", ref
    if "prs" in join:
        for pr in ask.get("prs") or []:
            if it := pr_item(tuple(pr)):
                return it, "pr", pr_key(pr)
    return None, "new", None


def relay_source(ask: dict[str, Any], answered: Iterable[dict[str, Any]]) -> str | None:
    """The ask relay ASK passes on, of ANSWERED: its asker's own, answered
    within RELAY_WINDOW before it, by another than ASK's taker -- the latest."""
    at = secs(ask["created_at"])
    best = None
    for a in answered:
        if a["from_name"] != ask["from_name"] or a["id"] == ask["id"] or not a.get("answered_at"):
            continue
        if a["to_name"] and a["to_name"] == ask["to_name"]:
            continue
        t = secs(a["answered_at"])
        if at - RELAY_WINDOW <= t <= at and (best is None or t > best[0]):
            best = (t, a["id"])
    return best[1] if best else None


def pr_key(pr: Iterable[Any]) -> str:
    host, owner, repo, num = pr
    return f"{host}:{owner}/{repo}#{num}"


# ---- An item as the board shows it ------------------------------------------------


def status(item: dict[str, Any], asks: list[dict[str, Any]]) -> str:
    """live: an ask in it is open; done: a lead closed it and nothing was
    asked in it since; quiet: the rest -- finished, or dropped: only a
    lead can say which."""
    if any(a["status"] in ASK_OPEN for a in asks):
        return "live"
    closed = item.get("closed_at")
    if closed and all(a["created_at"] <= closed for a in asks):
        return "done"
    return "quiet"


def taker(a: dict[str, Any]) -> str:
    "Who an ask went to: a handoff's lead is '' while the herd starts it."
    return a["to_name"] or ("lead of " + (a.get("to_proj") or "?"))


def last_at(asks: list[dict[str, Any]]) -> str:
    return max(a["answered_at"] or a["delivered_at"] or a["created_at"] for a in asks)


def is_lead(name: str, snap: dict[str, dict[str, Any]]) -> bool:
    return name == "primary" or name.startswith("lead-") or (snap.get(name) or {}).get("role") in ("lead", "primary")


def card(item: dict[str, Any], asks: list[dict[str, Any]], snap: dict[str, dict[str, Any]],
         owner: dict[str, str] | None = None, known: Iterable[str] = ()) -> dict[str, Any]:
    """ITEM, a work_items row, with ASKS, its asks (each with its ask_item
    columns), as a card: what it is, how it stands, who is on it, the pull
    requests and tickets it names -- OWNER says each PR's item -- the other
    items' asks it names (KNOWN: asks to items), and what it waits on."""
    asks = sorted(asks, key=lambda a: a["created_at"])
    first = asks[0]
    owner = owner or {}
    names: list[str] = []
    for a in asks:
        for n in (a["from_name"], taker(a)):
            if n and n not in names:
                names.append(n)
    projs: list[str] = []
    for a in sorted(asks, key=lambda a: not is_lead(a["from_name"], snap)):
        for p in (a.get("from_proj"), a.get("to_proj")):
            if p and p not in projs:
                projs.append(p)
    prs: dict[str, dict[str, Any]] = {}
    labels = list(item.get("labels") or [])
    linked: list[str] = []
    for a in asks:
        at = ms(a["answered_at"] or a["created_at"])
        for pr in a.get("prs") or []:
            k = pr_key(pr)
            if k not in prs:
                prs[k] = {"host": pr[0], "owner": pr[1], "repo": pr[2], "id": pr[3], "first": at, "last": at,
                          "own": owner.get(k, item["id"]) == item["id"], "of": owner.get(k) or item["id"]}
            else:
                prs[k]["last"] = max(prs[k]["last"] or 0, at or 0)
        for t in a.get("tickets") or []:
            if "CU-" + t not in labels:
                labels.append("CU-" + t)
        for r in a.get("refs") or []:
            it = known.get(r) if isinstance(known, dict) else None
            if it and it != item["id"] and it not in linked:
                linked.append(it)
    open_ = [a for a in asks if a["status"] in ASK_OPEN]
    if open_:
        a = max(open_, key=lambda a: a["created_at"])
        block = {"kind": "open", "ask": a["id"], "from": a["from_name"], "to": taker(a),
                 "since": ms(a["delivered_at"] or a["created_at"]), "status": a["status"],
                 "working": a["status"] == "delivered" and (snap.get(a["to_name"]) or {}).get("state") == "working",
                 "head": head(a["body"])}
    else:
        said = [a for a in asks if a["status"] == "answered" and not ack(a.get("reply"))] or asks
        a = max(said, key=lambda a: a["answered_at"] or a["created_at"])
        block = {"kind": a["status"], "ask": a["id"], "from": taker(a), "to": a["from_name"],
                 "since": ms(a["answered_at"] or a["created_at"]),
                 "head": head(a.get("reply") if a["status"] == "answered" else (a.get("error") or a["status"]))}
    return {
        "id": item["id"], "title": item.get("title") or title(first["body"]) or "(untitled)",
        "named": bool(item.get("title")), "status": status(item, asks), "labels": labels,
        "asks": len(asks), "open": len(open_),
        "fyi": sum(1 for a in asks if a.get("fyi")),
        "acks": sum(1 for a in asks if a["status"] == "answered" and ack(a.get("reply"))),
        "relays": sum(1 for a in asks if a.get("how") == "relay"),
        "failed": sum(1 for a in asks if a["status"] in ("failed", "cancelled")),
        "first": ms(first["created_at"]), "last": ms(last_at(asks)),
        "agents": names, "projects": projs, "cross": len(projs) > 1,
        "prs": sorted(prs.values(), key=lambda p: (not p["own"], p["first"] or 0)), "links": linked,
        "block": block, "closed": ms(item.get("closed_at")) if item.get("closed_at") else None,
    }


def waits(open_rows: list[dict[str, Any]], snap: dict[str, dict[str, Any]]) -> list[dict[str, Any]]:
    """Every open ask, who waits on whom: oldest first, each with its item,
    the projects at both ends, whether its taker is at work, and the open ask
    it was made answering (`under') -- the page nests it there."""
    ids = {a["id"] for a in open_rows}
    out = []
    for a in sorted(open_rows, key=lambda a: a["created_at"]):
        to = taker(a)
        out.append({
            "ask": a["id"], "item": a.get("item"), "from": a["from_name"], "to": to,
            "fromProj": a.get("from_proj") or "", "toProj": a.get("to_proj") or "",
            "kind": "handoff" if a.get("project") else "ask", "status": a["status"],
            "since": ms(a["delivered_at"] or a["created_at"]),
            "working": a["status"] == "delivered" and (snap.get(a["to_name"]) or {}).get("state") == "working",
            "under": a["parent"] if a.get("parent") in ids else None, "head": head(a["body"]),
        })
    return out


def timeline(asks: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """What an item went through, in time order: each ask and what came back.
    An ask that wants only to be read, and a reply that only says it was,
    are `fold'ed: the page keeps them out of sight."""
    out = []
    for a in asks:
        to = taker(a)
        out.append({"kind": "handoff" if a.get("project") else "ask", "at": ms(a["created_at"]),
                    "ask": a["id"], "from": a["from_name"], "to": to, "head": head(a["body"]),
                    "fold": "fyi" if a.get("fyi") else None, "relay": a.get("how") == "relay",
                    "status": a["status"]})
        if a["status"] == "answered":
            out.append({"kind": "answer", "at": ms(a["answered_at"]), "ask": a["id"], "from": to,
                        "to": a["from_name"], "head": head(a.get("reply")), "auto": bool(a.get("auto")),
                        "fold": "ack" if ack(a.get("reply")) else None})
        elif a["status"] in ("failed", "cancelled"):
            out.append({"kind": a["status"], "at": ms(a["answered_at"] or a["created_at"]), "ask": a["id"],
                        "from": to, "to": a["from_name"], "head": head(a.get("error") or a["status"])})
    out.sort(key=lambda e: e["at"] or 0)
    return out


def edges(asks: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """The asks between projects, counted by which way they went: for the
    board's map.  A relay is drawn from the project whose words it passes
    on (its `src_proj') to the one it tells, and kept apart."""
    by: dict[tuple[str, str, bool], int] = {}
    for a in asks:
        r = a.get("how") == "relay"
        f, t = (a.get("src_proj") if r else None) or a.get("from_proj") or "", a.get("to_proj") or ""
        if f and t and f != t:
            by[(f, t, r)] = by.get((f, t, r), 0) + 1
    return [{"from": f, "to": t, "relay": r, "n": n} for (f, t, r), n in sorted(by.items())]
