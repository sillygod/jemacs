"""A project fact made every project's, in one approval (ask d197da21).

A fact a project's agents found turns out to hold everywhere -- how to grep
a CLI binary, say.  Its project's lead proposes moving it (`herd promote
propose --fact NAME --every-project'); the user approves once, in the Inbox
or on the Memory tab; and the herd moves it, as a check is turned on: no
one is asked to apply anything.  The fact goes to the user store, its
project copy kept in the project's history; its check, if it has one,
from the project's rules file to user.json, on or off as it was; and its
uses and its check's denials are counted on under the user store.

Refused for a fact the user store already has by name, or already says
(the fact index's said-again test, as `herd remember' refuses), and for
one promoted into an instruction file.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import fact_index
import shared_memory
from herd import _lint_lib, get_herd
from lint import _WRITE


def _check(store: Path, key: str, name: str) -> shared_memory.Fact:
    "Store KEY's fact NAME when it may move, else ValueError saying why."
    f = shared_memory.check_shareable(store, key, name)
    found = fact_index.get_index().near_duplicate(shared_memory.USER_KEY, f.body, name=f.name)
    if found["hit"]:
        hit = found["hit"]
        raise ValueError(f"the user store already says this, as `{hit['name']}': "
                         f"\"{hit.get('description') or ''}\" -- forget this project's copy instead")
    return f


def propose(store: Path, key: str, name: str, author: str) -> dict[str, Any]:
    """Store KEY's fact NAME, pending the user's word to make it every
    project's.  The proposal keeps the fact's text as it was proposed:
    what the user reads before approving."""
    f = _check(store, key, name)
    return get_herd().propose(key, [f.name], "user", f.body, author, share=True)


def check_status(key: str, name: str) -> str | None:
    "Whether store KEY's fact NAME has a check, and is it on: `on', `off', or None."
    try:
        rule = next((r for r in _lint_lib().read_rules(key) if r.get("fact") == name), None)
    except (OSError, ValueError):
        return None
    return (rule.get("status") or "off") if rule else None


def approve(prop: dict[str, Any]) -> dict[str, Any]:
    """The user approved share proposal PROP: the fact moves to the user
    store, its check with it, its counts too.  Checked again first -- the
    user store may have gained it since."""
    lint = _lint_lib()
    key, name = prop["project"], prop["facts"][0]
    store = shared_memory.project_store(key)
    _check(store, key, name)
    with _WRITE:
        # Read both rules files before anything moves: one that cannot be
        # read stops the move here, not halfway.
        mine = lint.read_rules(key)
        rule = next((r for r in mine if r.get("fact") == name), None)
        users = lint.read_rules(shared_memory.USER_KEY) if rule else []
        shared_memory.move_to_user(store, key, name, by="user")
        if rule:
            # The user's file first: the hook never sees the check in neither.
            moved = dict(rule, moved_from=key, changed_at=shared_memory.iso_now())
            lint.write_rules(shared_memory.USER_KEY,
                             [r for r in users if r.get("fact") != name] + [moved])
            lint.write_rules(key, [r for r in mine if r.get("fact") != name])
        get_herd().move_fact_counts(key, name, shared_memory.USER_KEY)
        get_herd().apply_lint_proposal(prop["id"], kind="share")
    return {"id": prop["id"], "status": "applied", "kind": "share", "fact": name, "key": key,
            "check": (rule.get("status") or "off") if rule else None}
