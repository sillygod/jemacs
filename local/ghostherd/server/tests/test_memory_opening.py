"""The opening a grok or agy agent is told as it starts never drops a fact
silently: the rules in full, the rest one line each within the cap, then
the names of what did not fit -- and nothing cut mid-line."""

import pytest

import shared_memory as sm
from herd import get_herd, reset_herd
from jsonrpc_handler import JsonRpcHandler
from memory_store import (KEY, OTHER_NAMES, RULE_BODIES, RULE_NAMES, RULES, SNAP,  # noqa: F401
                          USER_RULE, note_is_whole, others, rules, store, whole_line)

# ---- The opening ---------------------------------------------------------


def test_the_store_is_shaped_like_deploy_platforms(store):
    assert len(sm.pool(KEY)) == 33
    assert {f.name for f in rules()} == set(RULES) | {USER_RULE[0]}


@pytest.mark.parametrize("cap", [4000, 2000, 1000, 500])
def test_the_opening_keeps_every_rule_and_never_cuts_a_line(store, cap):
    out = sm.opening(KEY, cap)
    text = out["text"]
    # Every rule, whole, whatever the cap.
    for body in RULE_BODIES:
        assert body.strip() in text
    assert set(out["full"]) == RULE_NAMES
    # Every index line in it is a whole one.  (A small cap may list none:
    # naming every fact left out comes before listing one more.)
    whole = {whole_line(f) for f in others()}
    shown = [l for l in text.splitlines() if l.startswith("- [")]
    assert all(l in whole for l in shown)
    assert len(shown) == len(out["listed"])
    if cap >= 2000:
        assert shown
    # The index part -- the lines and the note -- is within the cap.
    index_part = text[text.index("The other facts"):].rstrip("\n")
    assert len(index_part) <= cap
    # No fact is dropped silently: each is listed, or left to the note.
    assert set(out["listed"]) | set(out["more"]) == OTHER_NAMES
    assert not set(out["listed"]) & set(out["more"])
    assert len(out["listed"]) + len(out["more"]) == len(OTHER_NAMES)
    if out["more"]:
        note = text.rstrip("\n").splitlines()[-1]
        assert "recall <topic>" in note
        note_is_whole(note, out["more"])


def test_the_opening_names_every_fact_it_does_not_list(store):
    out = sm.opening(KEY, 4000)
    note = out["text"].rstrip("\n").splitlines()[-1]
    assert out["more"]
    assert note_is_whole(note, out["more"]) == out["more"]


def test_the_opening_orders_project_before_reference_newest_first(store):
    out = sm.opening(KEY, 100000)
    assert not out["more"]
    types = {f.name: f for f in others()}
    listed = [types[n] for n in out["listed"]]
    kinds = [f.type for f in listed]
    assert kinds == sorted(kinds, key=lambda t: sm.LINE_ORDER[t])
    for t in ("project", "reference"):
        dates = [f.updated for f in listed if f.type == t]
        assert dates == sorted(dates, reverse=True)


@pytest.mark.parametrize("cap", [120, 200, 300])
def test_a_tiny_cap_cuts_the_names_at_a_name(store, cap):
    out = sm.opening(KEY, cap)
    note = out["text"].rstrip("\n").splitlines()[-1]
    assert not out["listed"] and len(out["more"]) == len(OTHER_NAMES)
    named = note_is_whole(note, out["more"])
    assert len(named) < len(out["more"])



@pytest.fixture
def rpc(store, tmp_path):
    reset_herd()
    from herd import get_herd
    get_herd().tick(sessions=SNAP)
    yield JsonRpcHandler()
    reset_herd()


def test_the_opening_rpc_resolves_the_callers_project(rpc):
    out = rpc._memory_opening({"from": "grok-dp", "max": 4000})
    assert set(out["full"]) == set(RULES) | {USER_RULE[0]}
    assert "### Branch discipline  (feedback)" in out["text"]
