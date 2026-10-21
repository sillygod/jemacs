"""Move the existing claude memories into the shared store, reversibly.

Each ~/.claude/projects/<slug>/memory that belongs to a project the herd
knows becomes that project's store: its facts are copied in verbatim (same
contents, so a diff is clean), facts that belong to the user go to the user
store (deduped by name), the original dir is backed up (renamed, never
deleted), and claude's memory dir is replaced by a symlink to the store, so
claude keeps writing the way it already does.

Idempotent -- a dir already symlinked is left alone -- and reversible:
`unmigrate' puts the backups back and drops the symlinks.  The store it
filled stays, so nothing written since is lost.
"""

from __future__ import annotations

import datetime
import os
import shutil
from pathlib import Path

import shared_memory as sm
from config import get_config

BAK_PREFIX = "memory.bak-"


def _claude_root(claude_root: str | os.PathLike | None) -> Path:
    if claude_root is not None:
        return Path(os.path.expanduser(str(claude_root)))
    return get_config().claude_root


def _facts_in(d: Path) -> list[Path]:
    "The real fact files in D (not MEMORY.md, not symlinks, not subdirs)."
    return [p for p in sorted(d.glob("*.md"))
            if p.name != sm.INDEX and p.is_file() and not p.is_symlink()]


def _unique_backup(base: Path, stamp: str) -> Path:
    bak = base.parent / f"{BAK_PREFIX}{stamp}"
    n = 2
    while bak.exists():
        bak = base.parent / f"{BAK_PREFIX}{stamp}-{n}"
        n += 1
    return bak


def _to_user(fact: sm.Fact, user_names: set[str]) -> bool:
    return fact.type == "user" or fact.name in user_names


def plan(projects, *, claude_root=None, user_names=None) -> dict:
    "What `migrate' would do, touching nothing."
    return _run(projects, claude_root=claude_root, user_names=user_names, apply=False)


def migrate(projects, *, claude_root=None, user_names=None, stamp=None) -> dict:
    "Do the migration; return a per-project report."
    return _run(projects, claude_root=claude_root, user_names=user_names,
                apply=True, stamp=stamp)


def _run(projects, *, claude_root, user_names, apply, stamp=None) -> dict:
    root = _claude_root(claude_root)
    names = set(user_names or [])
    stamp = stamp or datetime.date.today().isoformat()
    report = {"projects": [], "user_store": str(sm.user_store()), "applied": apply}
    for project in projects:
        slug = sm.slug_for_path(project)
        cmem = root / slug / "memory"
        store = sm.project_store(slug)
        entry = {"project": project, "slug": slug, "claude_memory": str(cmem),
                 "store": str(store), "moved": 0, "to_user": [], "deduped": [],
                 "status": ""}
        if cmem.is_symlink():
            entry["status"] = "already"
            report["projects"].append(entry)
            continue
        if not cmem.is_dir():
            entry["status"] = "no-memory"
            report["projects"].append(entry)
            continue
        facts = _facts_in(cmem)
        for p in facts:
            f = sm.parse(p)
            if f is None:
                continue
            if _to_user(f, names):
                dst = sm.user_store() / f"{f.name}.md"
                if dst.exists():
                    entry["deduped"].append(f.name)
                elif apply:
                    dst.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copyfile(p, dst)
                entry["to_user"].append(f.name)
            elif apply:
                store.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(p, store / f"{f.name}.md")
            entry["moved"] += 1
        if apply:
            bak = _unique_backup(cmem, stamp)
            os.rename(cmem, bak)
            store.mkdir(parents=True, exist_ok=True)
            os.symlink(str(store.resolve()), str(cmem))
            entry["backup"] = str(bak)
            entry["status"] = "migrated"
        else:
            entry["status"] = "would-migrate"
        report["projects"].append(entry)
    if apply:
        sm.regenerate(sm.user_store(), sm.USER_KEY)
        sm.regenerate_all_projects()
    return report


def verify(projects, *, claude_root=None, user_names=None) -> dict:
    """After a migration: every original fact is present through the symlink
    with the same contents, and the symlink points at the store."""
    root = _claude_root(claude_root)
    names = set(user_names or [])
    report = {"projects": [], "ok": True}
    for project in projects:
        slug = sm.slug_for_path(project)
        cmem = root / slug / "memory"
        store = sm.project_store(slug)
        entry = {"project": project, "slug": slug, "checked": 0, "problems": []}
        baks = sorted((root / slug).glob(BAK_PREFIX + "*"))
        if not cmem.is_symlink():
            entry["problems"].append("claude memory is not a symlink")
        elif Path(os.path.realpath(cmem)) != Path(os.path.realpath(store)):
            entry["problems"].append(f"symlink points to {os.path.realpath(cmem)}, not the store")
        if baks:
            for p in _facts_in(baks[-1]):
                f = sm.parse(p)
                seen = cmem / p.name  # through the symlink
                want = sm.user_store() / p.name if _to_user(f, names) else store / p.name
                if not seen.exists():
                    entry["problems"].append(f"{p.name} missing through the symlink")
                elif seen.read_text(encoding="utf-8") != p.read_text(encoding="utf-8"):
                    entry["problems"].append(f"{p.name} differs from the backup")
                elif not want.exists():
                    entry["problems"].append(f"{p.name} not in its store")
                else:
                    entry["checked"] += 1
        else:
            entry["problems"].append("no backup found")
        if entry["problems"]:
            report["ok"] = False
        report["projects"].append(entry)
    return report


def unmigrate(projects, *, claude_root=None) -> dict:
    """Put the backups back: drop each memory symlink and rename the newest
    backup to `memory'.  The store is left as it is."""
    root = _claude_root(claude_root)
    report = {"projects": [], "applied": True}
    for project in projects:
        slug = sm.slug_for_path(project)
        cmem = root / slug / "memory"
        entry = {"project": project, "slug": slug, "status": ""}
        baks = sorted((root / slug).glob(BAK_PREFIX + "*"))
        if not cmem.is_symlink():
            entry["status"] = "not-migrated"
            report["projects"].append(entry)
            continue
        if not baks:
            entry["status"] = "no-backup"
            report["projects"].append(entry)
            continue
        cmem.unlink()
        os.rename(baks[-1], cmem)
        entry["status"] = "restored"
        entry["from"] = str(baks[-1])
        report["projects"].append(entry)
    return report
