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

A fact's earlier texts are kept in the store's .history/: what a
remember or an edit wrote over, what a forget removed, and what claude's
own writes (or a hand edit, an rm) changed -- noticed by comparing the
files with the text last seen (`notice').  A history file is never a fact:
every reader of facts takes a store's top-level *.md only.

This module is the store, with no knowledge of the sidecar or Emacs: paths
in, files out.  ~/.ghostherd stays apart from ~/.emacs.d/ghostherd-memory
(the Qdrant transcripts and herd.sqlite), which is raw, not curated.
"""

from __future__ import annotations

import datetime
import functools
import hashlib
import json
import os
import re
import threading
import time
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

# A fact's earlier texts, in the store: .history/<name>.<when>.md.  At most
# HISTORY_KEEP of a fact, none older than HISTORY_DAYS -- but a fact's
# newest earlier text stays whatever its age, so a forgotten fact can
# always be got back.  The text last seen of each fact, for `notice', is
# .history/.seen.json.
HISTORY = ".history"
HISTORY_KEEP = 10
HISTORY_DAYS = 365
SEEN = ".seen.json"
# A file changed this recently may be half written: `notice' takes it next time.
SETTLE = 2.0

# Facts an agent must follow, not just know: given in full in an opening.
RULE_TYPES = ("user", "feedback")
# The rest of an opening, one line each: project facts before reference.
LINE_ORDER = {"project": 0, "reference": 1}

# Who to tell when a store's facts change -- the sidecar's fact index
# registers here, so a fact written, edited or forgotten through this
# module reaches it at once.  Writes that bypass it (claude's own, a hand
# edit) are found when the index next reads the files.
_LISTENERS: list = []


def on_change(fn):
    "Call FN(key, names) after facts NAMES of store KEY change."
    if fn not in _LISTENERS:
        _LISTENERS.append(fn)
    return fn


def _changed(key: str, names: list[str]) -> None:
    for fn in list(_LISTENERS):
        try:
            fn(key, names)
        except Exception:  # noqa: BLE001 -- a listener never fails a write
            pass


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


def title_of(name: str) -> str:
    "A human title for a fact NAME, as `Fact.title' gives it."
    return _title(name)


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
    promoted: str = ""     # "<file> <date>" once it lives in an instruction file

    @property
    def title(self) -> str:
        return _title(self.name)

    def as_dict(self) -> dict:
        "The fact as the page sees it (its body included, for read and filter)."
        return {"name": self.name, "title": self.title, "description": self.description,
                "type": self.type, "author": self.author, "created": self.created,
                "updated": self.updated, "body": self.body, "shared": self.shared,
                "promoted": self.promoted}


def valid_name(name: str) -> str:
    """NAME lower-cased and checked -- lowercase letters, digits and dashes,
    1 to 79 long.  Rejects a path trick (no `/', `.', `..') by construction;
    raises ValueError otherwise."""
    name = (name or "").strip().lower()
    if not NAME_RE.match(name):
        raise ValueError("name is lowercase letters, digits and dashes (1–79)")
    return name


def valid_key(key: str) -> str:
    """A store KEY (`user' or a project slug) checked against path tricks: a
    single path segment, no separators.  Raises ValueError otherwise."""
    key = (key or "").strip()
    if key == USER_KEY:
        return key
    if not key or "/" in key or "\\" in key or key in (".", "..") or key.startswith("."):
        raise ValueError("bad project key")
    return key


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
        promoted=str(meta.get("promoted") or "").strip(),
    )


def _atomic_write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + f".tmp-{os.getpid()}")
    tmp.write_text(text, encoding="utf-8")
    os.replace(tmp, path)


# Metadata a rewrite keeps as it is: claude's own provenance, and the
# `promoted' mark -- so editing a promoted fact does not quietly un-promote
# it (only `unpromote' does).  render() passes these through `extra'.
_KEEP_META = ("originSessionId", "modified", "promoted")


def _kept_meta(path: Path) -> dict:
    "The metadata of the file at PATH that a rewrite must carry over."
    try:
        front, _ = _split_front(path.read_text(encoding="utf-8"))
    except OSError:
        return {}
    meta = front.get("metadata") if isinstance(front.get("metadata"), dict) else {}
    return {k: meta.get(k) for k in _KEEP_META if meta.get(k)}


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
    append is at worst replaced, never a fact lost.  A fact changed or
    removed without the herd since it last looked has its earlier text
    kept first (`notice')."""
    if store.is_dir():
        notice(store, key)
    shared = [] if key == USER_KEY else user_facts()
    if key != USER_KEY:
        _sync_shared_links(store, shared)
    # A promoted fact lives in the instruction file now: keep it out of the
    # index (and so out of the .el fallback opening, which reads MEMORY.md),
    # but leave its file -- the page lists it under "Promoted".
    project = [f for f in own_facts(store) if not f.promoted]
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
    new: bool = False,
) -> dict:
    """Write TEXT as a fact in STORE and regenerate the index.

    With NAME: that fact, made or updated -- an update keeps its created and
    keeps the text it replaces in its history.  NEW (keep both) with the
    NAME of a fact that exists is an error: the new one needs another name.
    Without NAME the name comes from the text's first words, and a
    different fact already there under it keeps it: this one takes the
    next free name (NAME-2, ...), never its file.  Text a fact of the store
    already holds word for word is not written again.

    Returns the fact's name, path, type, store and STATUS: new, updated,
    unchanged, or renamed (an auto-name taken by another fact)."""
    now = now or iso_now()
    typ = type if type in TYPES else DEFAULT_TYPE
    store.mkdir(parents=True, exist_ok=True)
    notice(store, key)
    status = "new"
    if name:
        name = valid_name(name)
    else:
        same = holding(store, key, text, typ)
        if same:
            return {"name": same.name, "path": str(same.path), "type": typ,
                    "store": USER_KEY if same.shared else key, "status": "unchanged"}
        name = _auto_name(text)
        if _taken(store, key, name):
            name, status = _free_name(store, key, name), "renamed"
    path = store / f"{name}.md"
    real = path.exists() and not path.is_symlink()
    if real and new:
        raise ValueError(
            f"`{name}' exists, and --new keeps both: give the new fact another "
            f"--name (or none); without --new, --name {name} updates `{name}'")
    description = _first_line(text)
    created, extra = now, {}
    if real:
        old_text = path.read_text(encoding="utf-8")
        old = parse(path)
        if old:
            created = old.created or now
            extra = _kept_meta(path)
        if _content(old_text) == _digest(description, typ, text):
            return {"name": name, "path": str(path), "type": typ, "store": key,
                    "status": "unchanged"}
        keep_history(store, name, old_text, how="remember", by=author, now=now)
        status = "updated"
    _put(store, path, render(name, description, text, type=typ, author=author,
                             created=created, updated=now, extra=extra))
    regenerate(store, key)
    if key == USER_KEY:
        regenerate_all_projects()
    _changed(key, [name])
    return {"name": name, "path": str(path), "type": typ, "store": key, "status": status}


def holding(store: Path, key: str, text: str, typ: str) -> Fact | None:
    "The fact KEY's agents see whose body and type are TEXT's and TYP, if any."
    body = text.strip()
    facts = own_facts(store) + ([] if key == USER_KEY else user_facts())
    return next((f for f in facts if f.body == body and f.type == typ), None)


def _taken(store: Path, key: str, name: str) -> bool:
    """Whether NAME is a fact's in STORE -- or, for a project, a user fact's,
    whose link a project fact of its name would replace."""
    if (store / f"{name}.md").exists() or (store / f"{name}.md").is_symlink():
        return True
    return key != USER_KEY and (user_store() / f"{name}.md").exists()


def _free_name(store: Path, key: str, name: str) -> str:
    "NAME-2, NAME-3, ...: the first that no fact has."
    for i in range(2, 1000):
        suffix = f"-{i}"
        cand = name[: 79 - len(suffix)].rstrip("-") + suffix
        if not _taken(store, key, cand):
            return cand
    raise ValueError(f"no free name like {name}")


@_locked
def stamp_author(path: Path, author: str, now: str | None = None) -> bool:
    """Record AUTHOR on the fact at PATH when it has none -- for claude's own
    writes, which carry claude's originSessionId but no agent name.  Keeps the
    description, body and claude's provenance; returns whether it changed."""
    f = parse(path)
    if f is None or f.author or path.is_symlink() or not author:
        return False
    now = now or iso_now()
    # claude's write itself: kept as an edit, its earlier text in the history.
    notice(path.parent, path.parent.name)
    extra = _kept_meta(path)
    _put(path.parent, path, render(f.name, f.description, f.body, type=f.type, author=author,
                                   created=(f.created or now), updated=(f.updated or now),
                                   extra=extra))
    _changed(path.parent.name, [f.name])
    return True


@_locked
def edit_fact(store: Path, key: str, name: str, *, description: str, body: str,
              type: str = DEFAULT_TYPE, author: str = "user", now: str | None = None) -> dict:
    """Rewrite an existing fact NAME in STORE with a new description, type and
    body, re-authored (the page's edit is the user's), updated now; keeps its
    created and claude's originSessionId, and regenerates the index."""
    name = valid_name(name)
    typ = type if type in TYPES else DEFAULT_TYPE
    now = now or iso_now()
    path = store / f"{name}.md"
    if path.is_symlink():
        raise ValueError(f"{name} is a shared user fact; edit it in the user store")
    if not path.exists():
        raise ValueError(f"no fact named {name}")
    notice(store, key)
    old_text = path.read_text(encoding="utf-8")
    old = parse(path)
    created = (old.created if old else "") or now
    extra = _kept_meta(path)
    description = (description or "").strip()
    if _content(old_text) != _digest(description, typ, body or ""):
        keep_history(store, name, old_text, how="edit", by=author, now=now)
    _put(store, path, render(name, description, body or "", type=typ,
                             author=author, created=created, updated=now, extra=extra))
    regenerate(store, key)
    if key == USER_KEY:
        regenerate_all_projects()
    _changed(key, [name])  # tell the fact index at once, not only on the next read
    return {"name": name, "type": typ, "store": key}


def promotable(f: Fact) -> bool:
    """Whether the herd may propose promoting F into an instruction file.
    A user fact (shared across projects) or any `user' fact is tied to this
    person, not the project -- paths, credentials, personal preferences --
    so it is never proposable; those stay memories."""
    return not f.shared and f.type != "user"


def check_promotable(store: Path, key: str, name: str) -> Fact:
    """Return project store KEY's fact NAME when the herd may promote it, else
    raise ValueError saying why -- the guard a proposal passes through."""
    name = valid_name(name)
    if key == USER_KEY:
        raise ValueError("user-store facts are tied to you; they cannot be promoted")
    path = store / f"{name}.md"
    if path.is_symlink():
        raise ValueError(f"{name} is a shared user fact; it cannot be promoted")
    f = parse(path)
    if f is None or not path.exists():
        raise ValueError(f"no fact named {name} in this project")
    if not promotable(f):
        raise ValueError(f"{name} is a {f.type} fact, tied to you; it cannot be promoted")
    return f


@_locked
def set_promoted(store: Path, key: str, name: str, where: str, now: str | None = None) -> dict:
    """Mark project store KEY's fact NAME promoted into file WHERE: it keeps
    its file (so it is reversible) but gains `promoted: <file> <date>' and
    leaves the index, the openings and retrieval.  Raises if it is not a
    fact the herd may promote."""
    f = check_promotable(store, key, name)
    now = now or iso_now()
    mark = f"{where} {now[:10]}"
    extra = _kept_meta(f.path)
    extra["promoted"] = mark
    _put(store, f.path, render(f.name, f.description, f.body, type=f.type,
                               author=f.author or "user", created=f.created or now,
                               updated=f.updated or now, extra=extra))
    regenerate(store, key)
    _changed(key, [f.name])
    return {"name": f.name, "promoted": mark, "store": key}


@_locked
def unpromote(store: Path, key: str, name: str, now: str | None = None) -> dict:
    """Undo a promotion: drop the `promoted' mark so the fact returns to the
    index, the openings and retrieval.  A no-op when it was not promoted."""
    name = valid_name(name)
    path = store / f"{name}.md"
    if path.is_symlink():
        raise ValueError(f"{name} is a shared user fact")
    f = parse(path)
    if f is None or not path.exists():
        raise ValueError(f"no fact named {name}")
    if not f.promoted:
        return {"name": name, "promoted": "", "store": key, "changed": False}
    now = now or iso_now()
    extra = _kept_meta(path)
    extra.pop("promoted", None)
    _put(store, path, render(f.name, f.description, f.body, type=f.type,
                             author=f.author or "user", created=f.created or now,
                             updated=f.updated or now, extra=extra))
    regenerate(store, key)
    _changed(key, [name])
    return {"name": name, "promoted": "", "store": key, "changed": True}


def facts_for(key: str) -> list[dict]:
    """The facts to show for store KEY: the user facts, and a project's own
    too -- each with its last few earlier versions, one line each, for the
    page's detail (strings: they cross Emacs as they are)."""
    rows = [dict(f.as_dict(), history=_history_lines(user_store(), f.name)) for f in user_facts()]
    if key != USER_KEY:
        store = project_store(key)
        rows += [dict(f.as_dict(), history=_history_lines(store, f.name)) for f in own_facts(store)]
    return rows


def _history_lines(store: Path, name: str, most: int = 5) -> list[str]:
    out = []
    for h in history(store, name)[:most]:
        who = f" by {h['by']}" if h["by"] and h["by"] != "unknown" else ""
        out.append(f"{h['when'][:10]} · {h['how']}{who} · {h['description'] or _first_line(h['body'])}")
    return out


def project_keys() -> list[str]:
    "Every project store's key (slug), those that have a directory, sorted."
    base = root()
    if not base.is_dir():
        return []
    return sorted(d.name for d in base.iterdir() if d.is_dir() and d.name != USER_KEY)


@_locked
def forget(store: Path, key: str, name: str, by: str = "user") -> dict:
    """Remove fact NAME from STORE, its text kept in its history.  Returns
    whether it was there."""
    name = valid_name(name)
    path = store / f"{name}.md"
    removed = False
    # A real file only -- never follow a shared symlink and delete a user fact
    # from a project store.
    if path.is_symlink():
        raise ValueError(f"{name} is a shared user fact; forget it with --user")
    if path.exists():
        notice(store, key)
        keep_history(store, name, path.read_text(encoding="utf-8"), how="forget", by=by)
        path.unlink()
        _seen_drop(store, name)
        removed = True
    regenerate(store, key)
    if key == USER_KEY:
        regenerate_all_projects()
    if removed:
        _changed(key, [name])
    return {"name": name, "removed": removed, "store": key}


# -- History ---------------------------------------------------------------


def _digest(description: str, typ: str, body: str) -> str:
    "What a fact says -- not its stamps, author or marks."
    return hashlib.sha1(f"{description.strip()}\n{typ}\n{body.strip()}".encode("utf-8")).hexdigest()


def _content(text: str) -> str:
    "`_digest' of a fact file's TEXT."
    front, body = _split_front(text)
    meta = front.get("metadata") if isinstance(front.get("metadata"), dict) else {}
    typ = str(meta.get("type") or "").strip() or DEFAULT_TYPE
    return _digest(str(front.get("description") or ""), typ if typ in TYPES else DEFAULT_TYPE, body)


def _stamp(when: str) -> str:
    "An ISO time as a file name's sortable part: 20261025T103000Z."
    return re.sub(r"[-:]", "", when)


def _iso(epoch: float) -> str:
    return datetime.datetime.fromtimestamp(epoch, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def keep_history(store: Path, name: str, old_text: str, *, how: str, by: str = "",
                 now: str | None = None) -> Path:
    """Keep OLD_TEXT, fact NAME's text before a change, in STORE's history:
    what it said, and when, how (remember, edit, forget, edited directly,
    removed directly) and by whom it was superseded."""
    now = now or iso_now()
    front, body = _split_front(old_text)
    meta = front.get("metadata") if isinstance(front.get("metadata"), dict) else {}
    extra = {k: str(v) for k, v in meta.items() if isinstance(v, str)}
    extra.update(superseded=now, superseded_how=how, superseded_by=by or "unknown")
    typ = str(meta.get("type") or "").strip()
    text = render(name, str(front.get("description") or _first_line(body)), body,
                  type=typ if typ in TYPES else DEFAULT_TYPE, author=str(meta.get("author") or ""),
                  created=str(meta.get("created") or meta.get("modified") or ""),
                  updated=str(meta.get("updated") or meta.get("modified") or ""), extra=extra)
    where = store / HISTORY
    where.mkdir(parents=True, exist_ok=True)
    path, i = where / f"{name}.{_stamp(now)}.md", 2
    while path.exists():
        path, i = where / f"{name}.{_stamp(now)}-{i}.md", i + 1
    _atomic_write(path, text)
    _prune(store)
    return path


def history(store: Path, name: str) -> list[dict]:
    """Fact NAME's earlier texts in STORE, newest first: when it was
    superseded, how, by whom, and its description and body then."""
    out = []
    for p in _newest_first_paths(list((store / HISTORY).glob(f"{name}.*.md"))):
        front, body = _split_front(p.read_text(encoding="utf-8"))
        meta = front.get("metadata") if isinstance(front.get("metadata"), dict) else {}
        out.append({"when": str(meta.get("superseded") or ""), "how": str(meta.get("superseded_how") or ""),
                    "by": str(meta.get("superseded_by") or ""),
                    "description": str(front.get("description") or ""), "body": body.strip(),
                    "path": str(p)})
    return out


def _newest_first_paths(paths: list[Path]) -> list[Path]:
    """History files newest first: by the time in their name, then -- two
    in one second -- by when they were written."""
    def key(p: Path):
        try:
            ns = p.stat().st_mtime_ns
        except OSError:
            ns = 0
        return (p.name.split(".", 1)[1][:16], ns)
    return sorted(paths, key=key, reverse=True)


def was_line(f: Fact, store: Path) -> str:
    """One line on what fact F said before, for `recall': its earlier
    description -- or, when that is the same, the first earlier line now
    gone -- and when.  "" for a fact with no history."""
    past = history(store, f.name)
    if not past:
        return ""
    last = past[0]
    said = last["description"]
    if said.strip() == f.description.strip():
        now_lines = set(f.body.splitlines())
        said = next((ln.strip() for ln in last["body"].splitlines()
                     if ln.strip() and ln not in now_lines), said)
    said = said if len(said) <= 120 else said[:119] + "…"
    more = f"; {len(past) - 1} older" if len(past) > 1 else ""
    who = f" by {last['by']}" if last["by"] and last["by"] != "unknown" else ""
    return f"was: {said} (until {last['when'][:10]}, {last['how']}{who}{more})"


def _prune(store: Path) -> None:
    """At most HISTORY_KEEP earlier texts a fact, none older than
    HISTORY_DAYS but its newest."""
    cut = _stamp((datetime.datetime.now(datetime.timezone.utc)
                  - datetime.timedelta(days=HISTORY_DAYS)).strftime("%Y-%m-%dT%H:%M:%SZ"))
    by_name: dict[str, list[Path]] = {}
    for p in (store / HISTORY).glob("*.md"):
        by_name.setdefault(p.name.split(".", 1)[0], []).append(p)
    for paths in by_name.values():
        for i, p in enumerate(_newest_first_paths(paths)):
            if i >= HISTORY_KEEP or (i > 0 and p.name.split(".", 1)[1] < cut):
                p.unlink(missing_ok=True)


# The text last seen of each fact, by store: {name: {stamp, digest, text}}.
_SEEN_CACHE: dict[str, tuple[int, dict]] = {}


def _seen_load(store: Path) -> dict | None:
    path = store / HISTORY / SEEN
    try:
        stamp = path.stat().st_mtime_ns
    except OSError:
        return None
    cached = _SEEN_CACHE.get(str(store))
    if cached and cached[0] == stamp:
        return cached[1]
    try:
        seen = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None
    _SEEN_CACHE[str(store)] = (stamp, seen)
    return seen


def _seen_save(store: Path, seen: dict) -> None:
    path = store / HISTORY / SEEN
    _atomic_write(path, json.dumps(seen, ensure_ascii=False))
    _SEEN_CACHE[str(store)] = (path.stat().st_mtime_ns, seen)


def _seen_entry(path: Path, text: str) -> dict:
    st = path.stat()
    return {"stamp": [st.st_mtime_ns, st.st_size], "digest": _content(text), "text": text}


def _put(store: Path, path: Path, text: str) -> None:
    "Write a fact file the herd's way, and see it as written."
    _atomic_write(path, text)
    seen = _seen_load(store)
    if seen is not None:
        seen[path.stem] = _seen_entry(path, text)
        _seen_save(store, seen)


def _seen_drop(store: Path, name: str) -> None:
    seen = _seen_load(store)
    if seen is not None and seen.pop(name, None) is not None:
        _seen_save(store, seen)


@_locked
def notice(store: Path, key: str) -> list[str]:
    """Keep the earlier text of the facts in STORE changed or removed
    without the herd -- claude writing its own memory file (its memory dir
    is a link to the store), a hand edit, an rm -- by comparing the files
    with the text last seen of each.  The first time it only takes note of
    what is there.  Returns the names it kept history for.

    Cheap when nothing moved: a stat of each file.  A file written in the
    last SETTLE seconds is left for next time, so a half-written one is not
    taken for a version."""
    if not store.is_dir():
        return []
    seen = _seen_load(store)
    first = seen is None
    seen = dict(seen or {})
    files = {p.stem: p for p in store.glob("*.md") if p.name != INDEX and not p.is_symlink()}
    kept: list[str] = []
    changed = first
    now = time.time()
    for name, p in files.items():
        try:
            st = p.stat()
        except OSError:
            continue
        old = seen.get(name)
        if old and old.get("stamp") == [st.st_mtime_ns, st.st_size]:
            continue
        if not first and now - st.st_mtime < SETTLE:
            continue
        try:
            text = p.read_text(encoding="utf-8")
        except OSError:
            continue
        entry = _seen_entry(p, text)
        if old and old.get("digest") != entry["digest"]:
            f = parse(p)
            keep_history(store, name, old["text"], how="edited directly",
                         by=(f.author if f else ""), now=_iso(st.st_mtime))
            kept.append(name)
        seen[name] = entry
        changed = True
    for name in [n for n in seen if n not in files]:
        keep_history(store, name, seen.pop(name)["text"], how="removed directly")
        kept.append(name)
        changed = True
    if changed:
        _seen_save(store, seen)
    return kept


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
    own = [f for f in own_facts(project_store(key)) if not f.promoted] if project_store(key).is_dir() else []
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
    pool = [f for f in own_facts(store) if not f.promoted]
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
