"""Promote memory into instructions: a stable fact proposed, approved by the
user, handed to the lead to apply, and only then marked promoted -- out of
the index, the openings and retrieval, but kept on disk and reversible.  The
herd never edits the repo itself.  A machine- or user-specific fact is
refused.  Plus `herd recall --project P' reaching another project's store."""

import pytest

import fact_index
import shared_memory as sm
from herd import get_herd, reset_herd
from jsonrpc_handler import JsonRpcHandler
from memory_store import KEY, PROJECT, store  # noqa: F401  (store fixture)


# ======================================================================
# shared_memory: the promoted mark and where it hides a fact
# ======================================================================

def test_promoting_takes_a_fact_out_of_index_opening_and_recall(store):
    name = "deploy-trigger-dispatch"   # a project fact in the fixture
    assert name in sm.index_text(store, KEY)
    assert name in [f.name for f in sm.pool(KEY)]

    out = sm.set_promoted(store, KEY, name, "CLAUDE.md", now="2026-10-22T00:00:00Z")
    assert out["promoted"] == "CLAUDE.md 2026-10-22"

    # Gone from the index, the opening (listed and the overflow note) and recall.
    assert name not in sm.index_text(store, KEY)
    assert name not in [f.name for f in sm.pool(KEY)]
    op = sm.opening(KEY)
    assert name not in op["listed"] and name not in op["more"] and name not in op["full"]
    assert name not in sm.recall(store, KEY, query="")["index"]
    assert name not in sm.recall(store, KEY, query="deploy trigger dispatch")["names"]

    # But its file is still there, marked promoted, and the page still sees it.
    assert (store / f"{name}.md").exists()
    assert sm.parse(store / f"{name}.md").promoted == "CLAUDE.md 2026-10-22"
    page = {f["name"]: f for f in sm.facts_for(KEY)}
    assert page[name]["promoted"] == "CLAUDE.md 2026-10-22"


def test_promoting_a_rule_leaves_the_opening_rules(store):
    name = "branch-discipline"          # a feedback rule, given in full
    assert name in sm.opening(KEY)["full"]
    sm.set_promoted(store, KEY, name, "CLAUDE.md")
    assert name not in sm.opening(KEY)["full"]
    assert name not in [f.name for f in sm.pool(KEY)]


def test_the_herd_refuses_a_machine_or_user_fact(store):
    # A user-store fact, or any `user' fact, is tied to the person: never promoted.
    with pytest.raises(ValueError):
        sm.check_promotable(sm.user_store(), sm.USER_KEY, "prefers-reasoned-design")
    # A shared user fact seen through the project store (a symlink) is refused.
    sm.regenerate(store, KEY)
    link = store / "prefers-reasoned-design.md"
    assert link.is_symlink()
    with pytest.raises(ValueError):
        sm.check_promotable(store, KEY, "prefers-reasoned-design")
    # A project fact re-typed `user' is refused too.
    sm.write_fact(store, KEY, "A personal note.", name="personal", type="user")
    with pytest.raises(ValueError):
        sm.check_promotable(store, KEY, "personal")


def test_unpromote_puts_a_fact_back(store):
    name = "deploy-trigger-dispatch"
    sm.set_promoted(store, KEY, name, "CLAUDE.md")
    assert name not in sm.index_text(store, KEY)
    out = sm.unpromote(store, KEY, name)
    assert out["changed"] is True
    assert name in sm.index_text(store, KEY)
    assert name in [f.name for f in sm.pool(KEY)]
    assert sm.parse(store / f"{name}.md").promoted == ""
    # A second unpromote is a no-op.
    assert sm.unpromote(store, KEY, name)["changed"] is False


def test_the_promoted_mark_survives_an_edit(store):
    name = "deploy-trigger-dispatch"
    sm.set_promoted(store, KEY, name, "CLAUDE.md", now="2026-10-22T00:00:00Z")
    # Editing the body (the page's edit) must not quietly un-promote it.
    sm.edit_fact(store, KEY, name, description="d2", body="new body", type="project")
    f = sm.parse(store / f"{name}.md")
    assert f.promoted == "CLAUDE.md 2026-10-22"
    assert f.body == "new body"
    assert name not in sm.index_text(store, KEY)   # still hidden


def test_promotion_removes_the_fact_from_retrieval(store):
    name = "deploy-trigger-dispatch"
    idx = fact_index.get_index()
    idx.sync(KEY)
    assert name in idx._stored(KEY)
    sm.set_promoted(store, KEY, name, "CLAUDE.md")   # fires the on_change listener
    idx.sync(KEY)
    assert name not in idx._stored(KEY)              # its point was dropped
    hits = idx.rank(KEY, "deploy trigger dispatch executor", strict=False)["hits"]
    assert name not in [h["name"] for h in hits]


# ======================================================================
# The RPC flow: propose -> the page -> approve -> the lead's ask -> promoted
# ======================================================================

@pytest.fixture
def rpc(tmp_path, monkeypatch):
    monkeypatch.setenv("GHOSTHERD_SHARED_MEMORY_DIR", str(tmp_path / "mem"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "sidecar"))
    reset_herd()
    key = sm.slug_for_path("/work/proj")
    store = sm.project_store(key)
    sm.write_fact(store, key, "Never commit to main; use a branch and a PR.",
                  name="never-commit-to-main", type="feedback")
    sm.write_fact(store, key, "The retry scheduler lives in worker.py.",
                  name="retry-scheduler", type="project")
    get_herd().tick(sessions=[
        {"name": "lead-proj", "kind": "claude", "state": "idle",
         "project": "/work/proj", "role": "lead"},
        {"name": "claude-proj", "kind": "claude", "state": "idle",
         "project": "/work/proj", "role": ""},
    ])
    yield JsonRpcHandler(), key, store
    reset_herd()


def test_propose_approve_reply_promotes(rpc):
    handler, key, store = rpc
    out = handler._herd_promote_propose({
        "from": "claude-proj", "project": "/work/proj",
        "facts": ["never-commit-to-main"], "file": "CLAUDE.md",
        "diff": "--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@\n+Never commit to main.\n"})
    pid = out["id"]

    # The page's Proposals section sees it, pending, with the fact's title.
    props = handler._herd_memory_proposals({"key": key})["proposals"]
    assert len(props) == 1 and props[0]["status"] == "pending"
    assert props[0]["file"] == "CLAUDE.md"
    assert props[0]["facts"][0]["title"] == "Never commit to main"

    # Approve: the herd asks the project's lead to apply the diff; it is not
    # promoted yet (the lead must apply it first).
    appr = handler._herd_memory_promote_approve({"id": pid})
    assert appr["to"] == "lead-proj" and appr["ask"]
    ask_body = get_herd()._ask_row(appr["ask"])["body"]
    assert "CLAUDE.md" in ask_body and "+Never commit to main." in ask_body
    assert get_herd().proposal(pid)["status"] == "approved"
    assert "never-commit-to-main" in sm.index_text(store, key)    # still a memory

    # The lead replies that it is done -> the fact is promoted.
    handler._herd_reply({"id": appr["ask"], "body": "applied in PR #42", "from": "lead-proj"})
    assert get_herd().proposal(pid)["status"] == "applied"
    assert sm.parse(store / "never-commit-to-main.md").promoted.startswith("CLAUDE.md ")
    assert "never-commit-to-main" not in sm.index_text(store, key)
    assert "never-commit-to-main" not in [f.name for f in sm.pool(key)]


def test_approve_with_no_lead_asks_the_proposer(rpc):
    handler, key, store = rpc
    # No lead in the snapshot: the proposer applies its own.
    get_herd().tick(sessions=[
        {"name": "claude-proj", "kind": "claude", "state": "idle",
         "project": "/work/proj", "role": ""}])
    pid = handler._herd_promote_propose({
        "from": "claude-proj", "project": "/work/proj",
        "facts": ["retry-scheduler"], "file": "AGENTS.md", "diff": "+x\n"})["id"]
    appr = handler._herd_memory_promote_approve({"id": pid})
    assert appr["to"] == "claude-proj"


def test_a_machine_or_user_fact_is_refused_at_propose(rpc):
    handler, key, store = rpc
    sm.write_fact(sm.user_store(), sm.USER_KEY, "A preference.", name="pref", type="user")
    with pytest.raises(ValueError):
        handler._herd_promote_propose({"from": "claude-proj", "project": "/work/proj",
                                       "facts": ["pref"], "user": True,
                                       "file": "CLAUDE.md", "diff": "+x\n"})
    # A project fact typed user is refused too, and nothing is stored.
    sm.write_fact(store, key, "mine", name="mine", type="user")
    with pytest.raises(ValueError):
        handler._herd_promote_propose({"from": "claude-proj", "project": "/work/proj",
                                       "facts": ["mine"], "file": "CLAUDE.md", "diff": "+x\n"})
    assert handler._herd_memory_proposals({"key": key})["proposals"] == []


def test_reject_keeps_the_fact(rpc):
    handler, key, store = rpc
    pid = handler._herd_promote_propose({
        "from": "claude-proj", "project": "/work/proj",
        "facts": ["retry-scheduler"], "file": "CLAUDE.md", "diff": "+x\n"})["id"]
    out = handler._herd_memory_promote_reject({"id": pid, "reason": "too specific"})
    assert out["status"] == "rejected"
    assert get_herd().proposal(pid)["reason"] == "too specific"
    assert "retry-scheduler" in sm.index_text(store, key)
    # A rejected proposal cannot then be approved.
    with pytest.raises(ValueError):
        handler._herd_memory_promote_approve({"id": pid})


def test_unpromote_through_the_rpc(rpc):
    handler, key, store = rpc
    sm.set_promoted(store, key, "retry-scheduler", "CLAUDE.md")
    assert "retry-scheduler" not in sm.index_text(store, key)
    out = handler._herd_memory_unpromote({"key": key, "name": "retry-scheduler"})
    assert out["changed"] is True
    assert "retry-scheduler" in sm.index_text(store, key)


def test_suggest_asks_the_lead_how_to_propose(rpc):
    handler, key, store = rpc
    out = handler._herd_memory_suggest({"key": key})
    assert out["to"] == "lead-proj"
    body = get_herd()._ask_row(out["ask"])["body"]
    assert "promote propose" in body and "--file" in body


def test_recall_project_resolves_path_leaf_and_slug(rpc):
    handler, key, store = rpc
    # From any shell, --project reaches the store by path, leaf name or slug.
    for p in ("/work/proj", "proj", key):
        _store, got = handler._memory_store(
            {"from": "someone-else", "project": p, "project_given": True})
        assert got == key
    with pytest.raises(ValueError):
        handler._memory_store({"project": "no-such-project", "project_given": True})


def test_an_identical_pending_proposal_is_not_stacked(rpc):
    handler, key, _store = rpc
    args = {"from": "claude-proj", "project": "/work/proj",
            "facts": ["never-commit-to-main"], "file": "CLAUDE.md",
            "diff": "--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@\n+Never commit to main.\n"}
    # The user presses "Suggest" twice and the lead proposes the same thing
    # twice: the second is the same row, not a duplicate.
    first = handler._herd_promote_propose(dict(args))
    second = handler._herd_promote_propose(dict(args))
    assert second["id"] == first["id"]
    assert second.get("duplicate") is True
    props = handler._herd_memory_proposals({"key": key})["proposals"]
    assert len(props) == 1

    # A genuinely different diff (or different facts, or file) is its own
    # proposal, never folded into the first.
    other = handler._herd_promote_propose(
        dict(args, diff="--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@\n+Branch, then PR.\n"))
    assert other["id"] != first["id"]
    assert len(handler._herd_memory_proposals({"key": key})["proposals"]) == 2
