"""The herd's checks, the sidecar's side: a fact made into a rule run before
each shell call an agent makes (bin/herd_lint.py; ask bd7efaa8).

The hook reads the rules from files and decides alone; this is what writes
them -- on the user's word only.  An agent proposes a check (`herd promote
propose --lint'), held to its own examples here and pending in the
proposals the Memory tab and the Inbox show; the user's Approve writes it
to the store's rules file, on.  The page turns it off, on again, or
removes it.  The denials the hook tells of are counted per rule
(herd.py's lint_denials) so a false positive shows.
"""

from __future__ import annotations

import json
import subprocess
import sys
import threading
from pathlib import Path
from typing import Any

import shared_memory
from herd import LINT_PATH, _lint_lib, get_herd

# A rule is checked against its examples in a process of its own: a pattern
# that backtracks without end costs that process, not a sidecar worker.
VALIDATE_SECONDS = 10

# One writer at a time: approve, turn off and remove read the file, change
# one rule and write it whole.
_WRITE = threading.Lock()


def validate(rule: dict[str, Any]) -> dict[str, Any]:
    "RULE as it may be stored (herd_lint.validate), or ValueError saying why not."
    try:
        out = subprocess.run([sys.executable, "-I", str(LINT_PATH), "validate"],
                             input=json.dumps(rule), capture_output=True, text=True,
                             timeout=VALIDATE_SECONDS)
    except subprocess.TimeoutExpired:
        raise ValueError(f"checking the rule against its examples took over {VALIDATE_SECONDS}s: "
                         "a pattern that backtracks without end?") from None
    try:
        got = json.loads(out.stdout)
    except ValueError:
        raise ValueError("the rule could not be checked: "
                         + (out.stderr.strip()[-300:] or "no answer")) from None
    if got.get("error"):
        raise ValueError(got["error"])
    return got["rule"]


def propose(store: Path, key: str, fact: str, rule: dict[str, Any], author: str) -> dict[str, Any]:
    """A check made of store KEY's FACT, pending the user's approval.  The
    fact must be there; the rule must do what its examples say."""
    fact = shared_memory.valid_name(fact)
    if shared_memory.parse(store / f"{fact}.md") is None:
        raise ValueError(f"no fact named {fact} in this store")
    clean = validate(dict(rule, fact=fact))
    return get_herd().propose(key, [fact], "", "", author, rule=clean)


def approve(prop: dict[str, Any]) -> dict[str, Any]:
    """The user approved check proposal PROP: its rule goes on, in its store's
    file -- in place of any earlier rule of the same fact."""
    lint = _lint_lib()
    key, rule = prop["project"], prop["rule"]
    now = shared_memory.iso_now()
    with _WRITE:
        rules = [r for r in lint.read_rules(key) if r.get("fact") != rule["fact"]]
        get_herd().apply_lint_proposal(prop["id"])
        rules.append(dict(rule, status="on", proposal=prop["id"], author=prop["author"],
                          approved_at=now, changed_at=now))
        lint.write_rules(key, rules)
    return {"id": prop["id"], "status": "applied", "kind": "lint", "fact": rule["fact"], "key": key}


def _change(key: str, fact: str, fn) -> dict[str, Any]:
    lint = _lint_lib()
    with _WRITE:
        rules = lint.read_rules(key)
        if not any(r.get("fact") == fact for r in rules):
            raise ValueError(f"no check for {fact} here")
        rules = [r2 for r2 in (fn(r) if r.get("fact") == fact else r for r in rules) if r2 is not None]
        lint.write_rules(key, rules)
    return {"key": key, "fact": fact}


def set_status(key: str, fact: str, status: str) -> dict[str, Any]:
    "Turn store KEY's check for FACT on or off."
    if status not in ("on", "off"):
        raise ValueError("a check is on or off")
    now = shared_memory.iso_now()
    out = _change(key, fact, lambda r: dict(r, status=status, changed_at=now))
    return dict(out, status=status)


def remove(key: str, fact: str) -> dict[str, Any]:
    "Remove store KEY's check for FACT.  Its denials stay counted in the log."
    return dict(_change(key, fact, lambda r: None), removed=True)


def rules(store: Path, key: str) -> list[dict[str, Any]]:
    """Store KEY's checks for the page: each rule, whether its fact is still
    there, and how many calls it denied -- the last of them too."""
    lint = _lint_lib()
    counts = get_herd().lint_counts(key)
    out = []
    for r in lint.read_rules(key):
        fact = str(r.get("fact") or "")
        f = shared_memory.parse(store / f"{fact}.md") if fact else None
        n = counts.get(fact) or {}
        out.append({
            "fact": fact, "title": f.title if f else shared_memory.title_of(fact),
            "gone": f is None, "status": r.get("status") or "off", "hint": r.get("hint") or "",
            "check": r.get("check") or {}, "catches": r.get("catches") or [],
            "passes": r.get("passes") or [], "author": r.get("author") or "",
            "approved_at": r.get("approved_at") or "", "changed_at": r.get("changed_at") or "",
            "count": n.get("count", 0), "last_ms": n.get("last_ms"),
            "last_session": n.get("last_session") or "", "last_command": n.get("last_command") or "",
        })
    return out
