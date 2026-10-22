"""Which directory a project named on the command line is.

`herd handoff PROJECT' names the project to hand work to as a path, its
leaf name (unnoiac) or its store slug (-Users-jing-src-unnoiac), as the
memory commands' --project does.  Those want a store key; a handoff wants
the directory, to find the project's lead in it or start one there.  So
this resolves to a path:

- a path is taken as given (the client made a relative one absolute
  against its own cwd), then put the way the herd writes a project: the
  main checkout of the git repository it is in, when it is in one -- a
  worktree or a subdirectory names its project, as an agent's directory
  does;
- a name is looked for among the projects the herd knows: those its
  agents are in, by leaf name, and those with a shared-memory store, by
  slug or by the leaf the slug ends with.  A slug is the path with `/' and
  `.' turned into `-', which loses where the dashes were; the directory
  is found again by walking the disk from the root, one entry at a time.

Nothing here is the herd's state: callers pass what the herd knows.
"""

from __future__ import annotations

import os
import re
import subprocess
from typing import Iterable


def norm(path: str) -> str:
    "PATH absolute, without ~ and without a trailing slash: for comparing."
    p = os.path.expanduser((path or "").strip())
    return os.path.normpath(p) if p else ""


def same(a: str | None, b: str | None) -> bool:
    "Whether A and B name the same project, however each is written."
    return bool(a and b) and norm(a) == norm(b)


def leaf(path: str | None) -> str:
    "A project's last directory: what its room is called."
    return os.path.basename(norm(path or "")) or (path or "")


def display(path: str) -> str:
    "PATH as the herd writes a project: under ~ when it is, a trailing slash."
    p = norm(path)
    home = norm("~")
    if p == home or p.startswith(home + os.sep):
        p = "~" + p[len(home):]
    return p.rstrip("/") + "/"


def _slug(text: str) -> str:
    return re.sub(r"[/.]", "-", text)


def checkout(path: str) -> str:
    """The project PATH is in: its git repository's main checkout -- a
    linked worktree's too -- or PATH itself outside git.  Mirrors the
    herd's own `ghostherd--project-root', so a project named by a worktree
    or a subdirectory finds the lead its agents share."""
    p = norm(path)
    try:
        out = subprocess.run(
            ["git", "-C", p, "rev-parse", "--path-format=absolute",
             "--show-toplevel", "--git-common-dir", "--git-dir"],
            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, timeout=5)
    except (OSError, subprocess.SubprocessError):
        return p
    lines = out.stdout.decode("utf-8", "replace").split("\n")
    if out.returncode != 0 or len(lines) < 3 or not lines[0]:
        return p
    top, common, gitdir = (norm(x) for x in lines[:3])
    if common != gitdir and os.path.basename(common) == ".git":
        # A linked worktree of an ordinary repository: its main checkout.
        return os.path.dirname(common)
    return top


def is_git_checkout(path: str) -> bool:
    "Whether PATH is in a git working tree."
    try:
        out = subprocess.run(
            ["git", "-C", norm(path), "rev-parse", "--is-inside-work-tree"],
            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, timeout=5)
    except (OSError, subprocess.SubprocessError):
        return False
    return out.returncode == 0 and out.stdout.strip() == b"true"


def path_for_slug(slug: str, root: str = "/") -> str | None:
    """The existing directory whose slug is SLUG, or None.  Walks down from
    ROOT taking, at each level, the entries whose own slug the rest of SLUG
    starts with -- a dash in a name and a slash read the same, so more than
    one may; the first full match wins."""
    if not slug.startswith("-"):
        return None

    def walk(at: str, rest: str) -> str | None:
        if not rest:
            return at
        try:
            names = sorted(os.listdir(at))
        except OSError:
            return None
        for name in names:
            s = "-" + _slug(name)
            if rest == s or rest.startswith(s + "-"):
                full = os.path.join(at, name)
                if os.path.isdir(full):
                    found = walk(full, rest[len(s):])
                    if found:
                        return found
        return None

    return walk(root, slug)


def resolve(arg: str, known: Iterable[str] = (), store_keys: Iterable[str] = ()) -> str:
    """The project directory ARG names: a path, a leaf name or a store slug.

    KNOWN are the projects the herd's agents are in (paths), STORE_KEYS the
    shared-memory stores' slugs.  Raises ValueError, saying what it knows,
    when ARG names none of them or more than one."""
    arg = (arg or "").strip()
    if not arg:
        raise ValueError("name the project: a path, its leaf name or its slug")
    if "/" in arg or arg.startswith("~"):
        p = norm(arg)
        if not os.path.isabs(p):
            raise ValueError(f"{arg}: give the path absolute (the client makes it so)")
        return p
    known = list(dict.fromkeys(norm(k) for k in known if k))
    store_keys = list(store_keys)
    # Its leaf name, among the projects the herd's agents are in.
    hits = [k for k in known if os.path.basename(k) == arg]
    if len(hits) == 1:
        return hits[0]
    if len(hits) > 1:
        raise ValueError(f"{arg} names more than one project: {', '.join(display(h) for h in hits)}; "
                         "give its path")
    # A slug, or the leaf a slug ends with, among the stores.
    by_slug = {_slug(k): k for k in known}
    if arg in by_slug:
        return by_slug[arg]
    keys = [k for k in store_keys if k == arg] or \
           [k for k in store_keys if k.endswith("-" + _slug(arg))]
    if len(keys) > 1:
        raise ValueError(f"{arg} matches more than one project: {', '.join(keys)}; give its path")
    if keys:
        found = by_slug.get(keys[0]) or path_for_slug(keys[0])
        if found:
            return norm(found)
        raise ValueError(f"{arg}: the project of store {keys[0]} is not on this machine; give its path")
    names = sorted({os.path.basename(k) for k in known})
    raise ValueError(
        f"no project {arg} that the herd knows (its agents are in: {', '.join(names) or 'none'}); "
        "give its path")
