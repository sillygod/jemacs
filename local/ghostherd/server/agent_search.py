"""`herd search': an agent searches what its own project's sessions said.

The session index holds every project's transcripts -- claude's, grok's,
agy's -- and a transcript can hold a secret.  So an agent reaches its own
project's only, worked out here from the session the herd knows it as,
never from what it asks for: the project's directory and those under it
(index._under, the Sessions list's rule), and its worktrees.  Not even
primary, the herd-wide lead, reaches another project's: it routes work,
and a project's history is its lead's to read (`herd handoff').  The
user's own search -- the page, Emacs -- is not this; it reaches them all.

What comes back is short, plain text an agent reads in its turn: the best
HITS chunks, each an excerpt around the words asked for, ITEM characters
at most and BLOCK in all; and how to read on from one (`--session ID --at
N'), by the same budget.  Strings shaped like secrets -- the herd's own
tokens, API keys, KEY=value with a secret's name -- are blanked first: an
agent behind the herd's guard reads here what its guard keeps it from
reading in the files."""

from __future__ import annotations

import os
import re
from pathlib import Path
from typing import Any

HITS = 8
ITEM = 640
BLOCK = 4000
RUN = '"$GHOSTHERD_HERD" search'

# -- the scope ------------------------------------------------------------------


def _norm(path: str) -> str:
    return os.path.normpath(os.path.expanduser(path or "")).rstrip("/") or "/"


def _worktree_owner(directory: str) -> str | None:
    """The main checkout whose worktree DIRECTORY is, by its .git file
    (gitdir: <main>/.git/worktrees/<name>); None when it is not one, or gone."""
    try:
        text = Path(directory, ".git").read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return None
    m = re.match(r"gitdir:\s*(.+?)/\.git/worktrees/", text.strip())
    return m.group(1) if m else None


def scope(engine, project: str, worktree_root: Path, known: list[str]) -> list[str]:
    """The project values, as the index records them, an agent of PROJECT
    may search: PROJECT and the directories under it, and its worktrees --
    WORKTREE_ROOT/<PROJECT's leaf>/<agent>, as the herd makes them.  A
    worktree there is PROJECT's when its .git says so; one since removed
    is PROJECT's when no other project the herd KNOWS has that leaf -- two
    projects of one name keep their removed worktrees' sessions to
    themselves.  A PROJECT that is the home directory, or above it, takes
    itself alone: under it is everyone's."""
    root = _norm(project)
    home = _norm(str(Path.home()))
    if root == home or home.startswith(root.rstrip("/") + "/"):
        return [p for p in engine.index.projects_under(root) if _norm(p) == root]
    values = list(engine.index.projects_under(root))
    leaf = os.path.basename(root)
    base = os.path.join(_norm(str(worktree_root)), leaf)
    others = any(os.path.basename(_norm(k)) == leaf and _norm(k) != root
                 and not _norm(k).startswith(_norm(str(worktree_root)) + "/") for k in known)
    owners: dict[str, bool] = {}
    for value in engine.index.projects_under(base):
        rel = os.path.relpath(_norm(value), base)
        if rel in (".", "") or rel.startswith(".."):
            continue
        directory = os.path.join(base, rel.split(os.sep, 1)[0])
        if directory not in owners:
            owner = _worktree_owner(directory)
            owners[directory] = (_norm(owner) == root) if owner else (
                not Path(directory).exists() and not others)
        if owners[directory]:
            values.append(value)
    return sorted(set(values))


# -- secrets ----------------------------------------------------------------------

_SECRETS = [
    (re.compile(r"-----BEGIN [A-Z ]*PRIVATE KEY-----.*?-----END [A-Z ]*PRIVATE KEY-----", re.S),
     "[private key]"),
    (re.compile(r"/jsonrpc/[0-9a-f]{16,}"), "/jsonrpc/[token]"),
    (re.compile(r"\b(?:sk|pk|rk)-(?:ant-)?[A-Za-z0-9_-]{16,}"), "[key]"),
    (re.compile(r"\b(?:gh[pousr]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,})"), "[key]"),
    (re.compile(r"\bxox[abprs]-[A-Za-z0-9-]{10,}"), "[key]"),
    (re.compile(r"\b(?:AKIA|ASIA)[0-9A-Z]{16}\b"), "[key]"),
    (re.compile(r"\bAIza[0-9A-Za-z_-]{35}\b"), "[key]"),
    (re.compile(r"\bya29\.[0-9A-Za-z_-]{20,}"), "[key]"),
    (re.compile(r"\beyJ[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}"), "[jwt]"),
    (re.compile(r"(?i)\bbearer\s+[A-Za-z0-9._~+/=-]{16,}"), "Bearer [token]"),
    (re.compile(r"(://)[^/\s:@]+:[^/\s@]+@"), r"\1[credentials]@"),
    (re.compile(r"(?i)\b([A-Z0-9_]*(?:TOKEN|SECRET|PASSWORD|PASSWD|API_?KEY|ACCESS_KEY|"
                r"PRIVATE_KEY|CREDENTIALS?)[A-Z0-9_]*)(\s*[=:]\s*)(\"[^\"\n]*\"|'[^'\n]*'|[^\s,;]+)"),
     r"\1\2[redacted]"),
]


def redact(text: str, tokens: tuple[str, ...] = ()) -> str:
    "TEXT with the herd's TOKENS and strings shaped like secrets blanked."
    for t in tokens:
        if t:
            text = text.replace(t, "[token]")
    for rx, repl in _SECRETS:
        text = rx.sub(repl, text)
    return text


def herd_tokens(data_dir: Path) -> tuple[str, ...]:
    out = []
    for name in ("rpc.token", "rpc.agent-token"):
        try:
            out.append((data_dir / name).read_text(encoding="utf-8").strip())
        except OSError:
            pass
    return tuple(t for t in out if t)


# -- excerpts ---------------------------------------------------------------------

_WORD = re.compile(r"[A-Za-z0-9_][A-Za-z0-9_.\-/]*[A-Za-z0-9_]|[A-Za-z0-9_]")
_CJK = re.compile(r"[㐀-䶿一-鿿]+")


def _needles(query: str) -> list[str]:
    "What to look for in a chunk: the query's words (2+ letters) and CJK pairs."
    q = query.lower()
    out = [w for w in _WORD.findall(q) if len(w) >= 2]
    for run in _CJK.findall(q):
        out += [run[i:i + 2] for i in range(len(run) - 1)] or [run]
    return out


def excerpt(text: str, query: str, width: int) -> str:
    """TEXT, its whitespace folded, cut to WIDTH characters around where the
    QUERY's words fall thickest; `…' marks a cut."""
    flat = " ".join((text or "").split())
    if len(flat) <= width:
        return flat
    low = flat.lower()
    spots = sorted(m.start() for n in set(_needles(query)) for m in re.finditer(re.escape(n), low))
    start = 0
    if spots:
        best, j = (0, 0), 0
        for i, s in enumerate(spots):
            while spots[j] < s - width // 2:
                j += 1
            best = max(best, (i - j + 1, -s))
        centre = -best[1]
        start = max(0, min(centre - width // 3, len(flat) - width))
        if start:
            space = flat.find(" ", start)
            start = space + 1 if 0 <= space < start + 30 else start
    room = width - (1 if start else 0)
    end = start + room
    if end < len(flat):
        room -= 1
        end = start + room
        space = flat.rfind(" ", start, end)
        end = space if space > start + room - 30 else end
    return ("…" if start else "") + flat[start:end].rstrip() + ("…" if end < len(flat) else "")


def _date(ts: str) -> str:
    return ts[:10] if re.match(r"(19|20)\d\d-\d\d-\d\d", ts or "") else ""


def _short(session: str) -> str:
    return session[:8] if re.match(r"[0-9a-f]{8}-", session or "") else session


# -- search and read --------------------------------------------------------------


def search(engine, projects: list[str], query: str, tokens: tuple[str, ...] = ()) -> dict[str, Any]:
    """The best chunks for QUERY among PROJECTS' sessions, as text: each
    with its date, CLI, session, turn and title, and an excerpt."""
    if not projects:
        return {"text": "No sessions of this project are indexed yet.", "hits": []}
    hits = engine.search(query, limit=HITS, projects=projects)["hits"]
    if not hits:
        return {"text": f"Nothing in this project's sessions matches {query!r}.", "hits": []}
    foot = f"\nRead on from one: {RUN} --session ID --at TURN"
    out, used, rows = [], len(foot), []
    sources: dict[str, list[dict]] = {}
    for n, h in enumerate(hits, 1):
        sid = h["session_id"]
        if sid not in sources:
            sources[sid] = engine.index.session_sources(sid, projects)
        turn, title = _turn_of(sources[sid], h)
        head = (f"{n}. {_date(h['ts']) or '----------'}  {h['agent']}  {_short(sid)} turn {turn}"
                + (f"  \"{title[:60]}\"" if title else "") + f"  ({h['role']})")
        room = min(ITEM, (BLOCK - used) // max(1, len(hits) - n + 1)) - len(head) - 4
        if room < 80:
            break
        body = excerpt(redact(h["text"], tokens), query, room)
        block = f"{head}\n   {body}"
        out.append(block)
        used += len(block) + 1
        rows.append({"session": sid, "turn": turn, "agent": h["agent"], "date": _date(h["ts"]),
                     "title": title, "role": h["role"], "score": h["score"]})
    return {"text": "\n".join(out) + "\n" + foot, "hits": rows}


def _turn_of(sources: list[dict], hit: dict) -> tuple[int, str]:
    "HIT's place in its session's reading order (`read'), and its title."
    turn = 0
    for s in sources:
        if s["source_path"] == hit["source_path"]:
            return turn + int(hit["chunk_index"] or 0), s["title"] or hit["title"]
        turn += s["chunks"]
    return int(hit["chunk_index"] or 0), hit["title"]


def read(engine, projects: list[str], session: str, at: int = 0,
         tokens: tuple[str, ...] = ()) -> dict[str, Any]:
    """SESSION (an id, or its first characters) of PROJECTS, from turn AT
    on, as text within the budget, with where to read on."""
    found = engine.index.sessions_like((session or "").strip(), projects)
    if not found:
        raise ValueError(f"no session {session!r} among this project's")
    if len(found) > 1 and session not in found:
        raise ValueError(f"{session!r} is more than one session: {', '.join(found)}")
    sid = session if session in found else found[0]
    sources = engine.index.session_sources(sid, projects)
    total = sum(s["chunks"] for s in sources)
    at = max(0, min(int(at or 0), max(0, total - 1)))
    first = sources[0] if sources else {}
    head = (f"Session {sid} · {first.get('agent', '')}"
            + (f" · \"{first['title']}\"" if first.get("title") else "") + f" · {total} turns")
    out, used, turn = [head], len(head), at
    while turn < total and used < BLOCK - 200:
        source, index = _source_at(sources, turn)
        want = list(range(index, min(index + 16, source["chunks"])))
        chunks = engine.store.chunks_at(source["source_path"], want)
        stop = False
        for i in want:
            c = chunks.get(i)
            if c is None:
                turn += 1
                continue
            label = f"-- turn {turn} · {c['role']}" + (f" · {_date(c['ts'])}" if _date(c["ts"]) else "")
            text = " ".join(redact(c["text"], tokens).split())
            room = min(ITEM, BLOCK - used - len(label) - 120)
            if room < 80:
                stop = True
                break
            if len(text) > room:
                text = text[:room - 1].rstrip() + "…"
            out.append(f"{label}\n{text}")
            used += len(label) + len(text) + 2
            turn += 1
        if stop:
            break
    out.append(f"\nRead on: {RUN} --session {_short(sid)} --at {turn}" if turn < total
               else "\n(the end of the session)")
    return {"text": "\n".join(out), "session": sid, "at": at, "next": turn if turn < total else None,
            "turns": total}


def _source_at(sources: list[dict], turn: int) -> tuple[dict, int]:
    for s in sources:
        if turn < s["chunks"]:
            return s, turn
        turn -= s["chunks"]
    return sources[-1], max(0, sources[-1]["chunks"] - 1)
