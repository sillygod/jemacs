"""Shared memory for every agent of a project, whatever its CLI.

One curated fact per Markdown file, in claude's format (frontmatter name,
description, metadata.type) with provenance added (author, created,
updated).  A project's facts live in ~/.ghostherd/memory/<slug>/ -- the
slug of its main checkout, so a worktree shares the main project's store --
and the user-level facts, shared by every project, in
~/.ghostherd/memory/user/.

The index (MEMORY.md) is generated from the files' frontmatter, so
concurrent writers never fight over it: the files are the truth, and a
regenerate reproduces the index whole.  claude's own harness also appends
pointer lines to MEMORY.md; a regenerate replaces them, losing no fact,
because every file becomes a line.

This module is the store, with no knowledge of the sidecar or Emacs: paths
in, files out.  ~/.ghostherd stays apart from ~/.emacs.d/ghostherd-memory
(the Qdrant transcripts and herd.sqlite), which is raw, not curated.
"""

from __future__ import annotations

import datetime
import functools
import hashlib
import os
import re
import threading
from dataclasses import dataclass
from pathlib import Path

import yaml

# The sidecar runs RPCs on a pool of worker threads; this serialises the
# store's writes and index regenerations so two never race on MEMORY.md.
# Cross-process races (claude's harness writing a file while the herd
# regenerates) are safe anyway: writes are atomic and the index is rebuilt
# from the files, so the worst case is a stale line, fixed on the next pass.
_LOCK = threading.RLock()


def _locked(fn):
    @functools.wraps(fn)
    def wrap(*args, **kwargs):
        with _LOCK:
            return fn(*args, **kwargs)
    return wrap

USER_KEY = "user"
INDEX = "MEMORY.md"
TYPES = ("user", "feedback", "project", "reference")
DEFAULT_TYPE = "project"
NAME_RE = re.compile(r"^[a-z0-9][a-z0-9-]{0,78}$")

# Facts an agent must follow, not just know: given in full in an opening.
RULE_TYPES = ("user", "feedback")
# The rest of an opening, one line each: project facts before reference.
LINE_ORDER = {"project": 0, "reference": 1}

def root() -> Path:
    "The store's home.  GHOSTHERD_SHARED_MEMORY_DIR overrides it, for tests."
    return Path(
        os.path.expanduser(os.getenv("GHOSTHERD_SHARED_MEMORY_DIR", "~/.ghostherd/memory"))
    )


def slug_for_path(path: str) -> str:
    """A checkout's path as a store key, claude's own scheme: every `/' and
    `.' becomes `-'.  So ~/.claude/projects/<slug>/memory and the store line
    up, and the migration is a 1:1 mapping."""
    full = os.path.abspath(os.path.expanduser(path))
    return re.sub(r"[/.]", "-", full)


def project_store(key: str) -> Path:
    "The directory for project KEY (a slug), or the user store for `user'."
    return root() / key


def user_store() -> Path:
    return root() / USER_KEY


def iso_now() -> str:
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def _auto_name(text: str) -> str:
    """A kebab slug from the first line's words; a short hash when it has no
    ASCII words (e.g. all CJK), so a fact always gets a stable filename."""
    words = re.findall(r"[a-z0-9]+", _first_line(text).lower())
    slug = "-".join(words[:6])[:60].strip("-")
    if slug and NAME_RE.match(slug):
        return slug
    digest = hashlib.sha1(text.strip().encode("utf-8")).hexdigest()[:8]
    return f"note-{digest}"


def _first_line(text: str, limit: int = 160) -> str:
    "A one-line description from TEXT: its first non-empty line, clipped."
    for line in text.splitlines():
        line = line.strip().lstrip("#").strip()
        if line:
            return line if len(line) <= limit else line[: limit - 1] + "…"
    return ""


def _title(name: str) -> str:
    "A human title for the index, from a kebab NAME."
    words = name.replace("-", " ").strip()
    return words[:1].upper() + words[1:] if words else name


@dataclass
class Fact:
    name: str
    description: str
    type: str
    author: str
    created: str
    updated: str
    body: str
    path: Path
    shared: bool = False  # a user fact, linked into a project store

    @property
    def title(self) -> str:
        return _title(self.name)


def _quote(value: str) -> str:
    "A double-quoted frontmatter scalar, backslash-escaped, as claude writes."
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def _dump_scalar(value: str) -> str:
    "A metadata scalar: quoted when it could be misread by YAML, else bare."
    if value == "" or re.search(r'[:#"\n]|^[\s>|*&!%@`\[\]{},?-]|\s$', value):
        return _quote(value)
    return value


def render(
    name: str,
    description: str,
    body: str,
    *,
    type: str = DEFAULT_TYPE,
    author: str = "user",
    created: str,
    updated: str,
    extra: dict | None = None,
) -> str:
    "A fact file's text: claude's frontmatter with provenance, then BODY."
    meta = {"node_type": "memory", "type": type}
    # Keep claude's own provenance (originSessionId, modified) when a fact
    # it wrote is updated here.
    for k, v in (extra or {}).items():
        if k not in ("node_type", "type", "author", "created", "updated") and v:
            meta[k] = v
    meta["author"] = author
    meta["created"] = created
    meta["updated"] = updated
    lines = ["---", f"name: {name}", f"description: {_quote(description)}", "metadata:"]
    for k, v in meta.items():
        lines.append(f"  {k}: {_dump_scalar(str(v))}")
    lines.append("---")
    text = "\n".join(lines) + "\n\n" + body.strip() + "\n"
    return text


def _split_front(text: str) -> tuple[dict, str]:
    "Parse a fact file into (frontmatter dict, body)."
    if text.startswith("---"):
        m = re.match(r"^---\s*\n(.*?)\n---\s*\n?(.*)$", text, re.S)
        if m:
            try:
                # BaseLoader keeps every scalar a string: a `modified:' ISO
                # stamp must not become a datetime, nor a name a number.
                front = yaml.load(m.group(1), Loader=yaml.BaseLoader) or {}
            except yaml.YAMLError:
                front = {}
            if isinstance(front, dict):
                return front, m.group(2)
    return {}, text


def parse(path: Path) -> Fact | None:
    "Read PATH as a fact, or None when it is not one."
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return None
    front, body = _split_front(text)
    meta = front.get("metadata") if isinstance(front.get("metadata"), dict) else {}
    name = str(front.get("name") or path.stem)
    typ = str(meta.get("type") or "").strip() or DEFAULT_TYPE
    return Fact(
        name=name,
        description=str(front.get("description") or "").strip(),
        type=typ if typ in TYPES else DEFAULT_TYPE,
        author=str(meta.get("author") or "").strip(),
        created=str(meta.get("created") or meta.get("modified") or "").strip(),
        updated=str(meta.get("updated") or meta.get("modified") or "").strip(),
        body=body.strip(),
        path=path,
        shared=path.is_symlink(),
    )


def _atomic_write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + f".tmp-{os.getpid()}")
    tmp.write_text(text, encoding="utf-8")
    os.replace(tmp, path)


def own_facts(store: Path) -> list[Fact]:
    "A store's own facts (not the user facts linked into it), by name."
    facts = []
    for p in sorted(store.glob("*.md")):
        if p.name == INDEX or p.is_symlink():
            continue
        f = parse(p)
        if f:
            facts.append(f)
    return facts


def user_facts() -> list[Fact]:
    store = user_store()
    if not store.is_dir():
        return []
    facts = []
    for p in sorted(store.glob("*.md")):
        if p.name == INDEX:
            continue
        f = parse(p)
        if f:
            f.shared = True
            facts.append(f)
    return facts


def _index_text(project: list[Fact], shared: list[Fact], key: str) -> str:
    "MEMORY.md for a store: the shared user facts, then the project's own."
    def line(f: Fact) -> str:
        gloss = f" — {f.description}" if f.description else ""
        return f"- [{f.title}]({f.name}.md){gloss}"

    out = ["# Memory", ""]
    if key == USER_KEY:
        out.append("Shared by every project in the herd.")
        out.append("")
        out += [line(f) for f in project] or ["(none yet)"]
        return "\n".join(out).rstrip() + "\n"
    out.append("The herd's shared memory for this project.  One fact per file;")
    out.append("this index is generated from them, so edit the files, not this.")
    out.append("")
    if shared:
        out.append("## Shared (every project)")
        out.append("")
        out += [line(f) for f in shared]
        out.append("")
    out.append("## This project")
    out.append("")
    out += [line(f) for f in project] or ["(none yet)"]
    return "\n".join(out).rstrip() + "\n"


def _sync_shared_links(store: Path, shared: list[Fact]) -> None:
    """Make each user fact a relative symlink in STORE, so claude (reading
    the store through its memory symlink) sees it as a file; drop links to
    user facts that are gone.  A real project fact of the same name wins."""
    store.mkdir(parents=True, exist_ok=True)
    want = {f.name for f in shared}
    for p in store.glob("*.md"):
        if p.name == INDEX:
            continue
        if p.is_symlink():
            target = os.readlink(p)
            if "/user/" in target.replace(os.sep, "/") and p.stem not in want:
                p.unlink()  # a user fact that no longer exists
    own = {f.name for f in own_facts(store)}
    for f in shared:
        if f.name in own:
            continue  # a real project fact of the same name takes the name
        link = store / f"{f.name}.md"
        rel = os.path.join("..", USER_KEY, f"{f.name}.md")
        if link.is_symlink():
            if os.readlink(link) == rel:
                continue
            link.unlink()
        elif link.exists():
            continue
        os.symlink(rel, link)


@_locked
def regenerate(store: Path, key: str) -> None:
    """Rebuild STORE's MEMORY.md from its files (the truth).  For a project
    store, sync the shared user facts in first.  Atomic, so a concurrent
    append is at worst replaced, never a fact lost."""
    shared = [] if key == USER_KEY else user_facts()
    if key != USER_KEY:
        _sync_shared_links(store, shared)
    project = own_facts(store)
    _atomic_write(store / INDEX, _index_text(project, shared, key))


@_locked
def regenerate_all_projects() -> None:
    "Refresh every project store's shared section after a user fact changed."
    base = root()
    if not base.is_dir():
        return
    for d in base.iterdir():
        if d.is_dir() and d.name != USER_KEY:
            regenerate(d, d.name)


@_locked
def write_fact(
    store: Path,
    key: str,
    text: str,
    *,
    type: str = DEFAULT_TYPE,
    name: str | None = None,
    author: str = "user",
    now: str | None = None,
) -> dict:
    """Write TEXT as a fact in STORE and regenerate the index.  Returns the
    fact's name and path.  A second write to the same name keeps its
    created, updates updated."""
    now = now or iso_now()
    typ = type if type in TYPES else DEFAULT_TYPE
    name = (name or _auto_name(text)).strip().lower()
    if not NAME_RE.match(name):
        raise ValueError("name is lowercase letters, digits and dashes (1–79)")
    store.mkdir(parents=True, exist_ok=True)
    path = store / f"{name}.md"
    created, extra = now, {}
    if path.exists() and not path.is_symlink():
        old = parse(path)
        if old:
            created = old.created or now
            existing_front, _ = _split_front(path.read_text(encoding="utf-8"))
            em = existing_front.get("metadata") if isinstance(existing_front.get("metadata"), dict) else {}
            extra = {"originSessionId": em.get("originSessionId")}
    description = _first_line(text)
    _atomic_write(path, render(name, description, text, type=typ, author=author,
                               created=created, updated=now, extra=extra))
    regenerate(store, key)
    if key == USER_KEY:
        regenerate_all_projects()
    return {"name": name, "path": str(path), "type": typ, "store": key}


@_locked
def stamp_author(path: Path, author: str, now: str | None = None) -> bool:
    """Record AUTHOR on the fact at PATH when it has none -- for claude's own
    writes, which carry claude's originSessionId but no agent name.  Keeps the
    description, body and claude's provenance; returns whether it changed."""
    f = parse(path)
    if f is None or f.author or path.is_symlink() or not author:
        return False
    now = now or iso_now()
    front, _ = _split_front(path.read_text(encoding="utf-8"))
    meta = front.get("metadata") if isinstance(front.get("metadata"), dict) else {}
    extra = {"originSessionId": meta.get("originSessionId"), "modified": meta.get("modified")}
    _atomic_write(path, render(f.name, f.description, f.body, type=f.type, author=author,
                               created=(f.created or now), updated=(f.updated or now), extra=extra))
    return True


@_locked
def forget(store: Path, key: str, name: str) -> dict:
    "Remove fact NAME from STORE.  Returns whether it was there."
    name = (name or "").strip().lower()
    path = store / f"{name}.md"
    removed = False
    # A real file only -- never follow a shared symlink and delete a user fact
    # from a project store.
    if path.is_symlink():
        raise ValueError(f"{name} is a shared user fact; forget it with --user")
    if path.exists():
        path.unlink()
        removed = True
    regenerate(store, key)
    if key == USER_KEY:
        regenerate_all_projects()
    return {"name": name, "removed": removed, "store": key}


@_locked
def index_text(store: Path, key: str) -> str:
    "The store's MEMORY.md, regenerated fresh so it reflects every file."
    regenerate(store, key)
    try:
        return (store / INDEX).read_text(encoding="utf-8")
    except OSError:
        return ""


def pool(key: str) -> list[Fact]:
    """Every fact an agent of store KEY sees: the shared user facts, then
    the project's own -- a project fact of a user fact's name takes it, as
    in the store's links.  Read from the files, writing nothing."""
    own = own_facts(project_store(key)) if project_store(key).is_dir() else []
    if key == USER_KEY:
        return own
    mine = {f.name for f in own}
    return [f for f in user_facts() if f.name not in mine] + own


def is_rule(f: Fact) -> bool:
    "A fact to follow, given in full: a user fact, or a feedback one."
    return f.shared or f.type in RULE_TYPES


def _newest_first(facts: list[Fact]) -> list[Fact]:
    "FACTS by their last change, newest first; undated ones last, by name."
    dated = sorted((f for f in facts if f.updated or f.created),
                   key=lambda f: f.updated or f.created, reverse=True)
    return dated + sorted((f for f in facts if not (f.updated or f.created)),
                          key=lambda f: f.name)


def _line(f: Fact) -> str:
    "A fact's index line, as MEMORY.md has it."
    gloss = f" — {f.description}" if f.description else ""
    return f"- [{f.title}]({f.name}.md){gloss}"


RECALL = '"$GHOSTHERD_HERD" recall'


def _more_note(rest: list[Fact], room: int) -> str:
    """The line naming the facts an opening does not list, so none is
    dropped silently: how many, how to read them, and their names -- every
    one when they fit in ROOM characters, else as many as do, cut at a name
    with how many are left.  When not one name fits, just the count."""
    if not rest:
        return ""
    head = f"{len(rest)} more, not listed — {RECALL} <topic> for any: "
    names = [f.name for f in rest]
    if len(head) + len(", ".join(names)) <= room:
        return head + ", ".join(names)
    shown: list[str] = []
    for i, name in enumerate(names):
        left = len(names) - i - 1
        if len(head) + len(", ".join(shown + [name])) + len(f" … and {left} more") > room:
            break
        shown.append(name)
    if not shown:
        return head.rstrip(": ") + "."
    return head + ", ".join(shown) + f" … and {len(names) - len(shown)} more"


def opening(key: str, cap: int = 4000) -> dict:
    """What a grok or agy agent of store KEY is told of the memory as it starts.

    The rules first, in full: the user facts and the feedback ones, which
    say how to behave, not merely what is so -- never cut, so they sit
    outside CAP.  Then the other facts one line each, project facts before
    reference ones and each newest first, while the lines and a closing
    note naming every fact left out fit in CAP characters -- whole lines
    only.  So no fact is dropped silently and nothing is cut mid-line; only
    a store too big to name in CAP has the names cut, at a name, with how
    many are left.  Returns the text and which facts it gave in full,
    listed, and left to the note.  Reads the files and writes nothing."""
    facts = pool(key)
    rules = [f for f in facts if is_rule(f)]
    rules = [f for f in rules if f.shared] + _newest_first([f for f in rules if not f.shared])
    rest = [f for f in facts if not is_rule(f)]
    rest = sorted(_newest_first(rest), key=lambda f: LINE_ORDER.get(f.type, 0))
    header = "The other facts, one line each, newest first:"
    listed: list[str] = []
    used = len(header) + 1
    for i, f in enumerate(rest):
        line = _line(f)
        note = _more_note(rest[i + 1:], 1 << 30)  # every name: what the rest needs
        if used + len(line) + 1 + len(note) > cap:
            break
        listed.append(line)
        used += len(line) + 1
    more = rest[len(listed):]
    out = []
    if rules:
        out.append("Its rules, in full -- follow them:")
        for f in rules:
            where = "every project" if f.shared else f.type
            out.append(f"\n### {f.title}  ({where})\n{f.body.strip()}")
    if rest:
        out.append(("\n" if rules else "") + header + "\n")
        out += listed
        if more:
            out.append(_more_note(more, max(0, cap - used)))
    return {
        "text": "\n".join(out).strip() + "\n" if out else "",
        "full": [f.name for f in rules],
        "listed": [f.name for f in rest[: len(listed)]],
        "more": [f.name for f in more],
    }


def _matches(f: Fact, needles: list[str]) -> bool:
    hay = f"{f.name}\n{f.description}\n{f.body}".lower()
    return all(n in hay for n in needles)


@_locked
def ensure_project_symlink(claude_root, cwd: str, project: str) -> dict:
    """Make ~/.claude/projects/<slug(cwd)>/memory a symlink to the project's
    store, so claude writes the herd's shared memory.  CWD is the agent's
    working directory (a worktree or the main checkout); PROJECT is the
    canonical main checkout, whose store a worktree shares.  A real memory
    dir with facts is left for the migration, not clobbered."""
    key = slug_for_path(project)
    store = project_store(key)
    cmem = Path(os.path.expanduser(str(claude_root))) / slug_for_path(cwd) / "memory"
    store.mkdir(parents=True, exist_ok=True)
    regenerate(store, key)
    target = str(store.resolve())
    if cmem.is_symlink():
        if Path(os.path.realpath(cmem)) == Path(os.path.realpath(store)):
            return {"status": "ok", "link": str(cmem)}
        cmem.unlink()
        os.symlink(target, str(cmem))
        return {"status": "relinked", "link": str(cmem)}
    if cmem.exists():
        if any(p.name != INDEX for p in cmem.glob("*.md")):
            return {"status": "has-content", "link": str(cmem)}
        try:
            (cmem / INDEX).unlink(missing_ok=True)
            cmem.rmdir()
        except OSError:
            return {"status": "has-content", "link": str(cmem)}
    cmem.parent.mkdir(parents=True, exist_ok=True)
    os.symlink(target, str(cmem))
    return {"status": "linked", "link": str(cmem)}


@_locked
def recall(store: Path, key: str, query: str | None = None, include_shared: bool = True) -> dict:
    """The index when QUERY is empty, else the matching facts in full.
    Searches the project's own facts and the shared user facts."""
    regenerate(store, key)
    pool = own_facts(store)
    if include_shared and key != USER_KEY:
        pool = user_facts() + pool
    if not (query or "").strip():
        return {"index": index_text(store, key), "count": len(pool)}
    needles = [w for w in query.lower().split() if w]
    hits = [f for f in pool if _matches(f, needles)]
    blocks = []
    for f in hits:
        where = "user" if f.shared else key
        blocks.append(
            f"# {f.title}  ({f.type}, by {f.author or 'unknown'}, {where})\n{f.body}"
        )
    return {"query": query, "count": len(hits),
            "text": "\n\n---\n\n".join(blocks) or f"no fact matches {query!r}",
            "names": [f.name for f in hits]}
