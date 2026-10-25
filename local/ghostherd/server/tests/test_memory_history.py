"""Shared memory keeps what it replaces: `herd remember' refuses text that
says again a fact the store has (--name updates it, --new keeps both), an
auto-name never takes another fact's file, an overwrite, an edit, a
forget and a write that bypasses the herd keep the earlier text in the
store's .history/ -- which is never a fact anywhere."""

import json
import os
import time
from pathlib import Path

import pytest

import fact_index
import shared_memory as sm
from herd import get_herd, reset_herd
from importers.claude import iter_claude
from jsonrpc_handler import JsonRpcHandler
from memory_store import KEY, PROJECT, store  # noqa: F401

BRANCH_AGAIN = ("Always work on a feature branch in deploy-platform; never commit or push to main.\n\n"
                "**Why:** a background git sync moves HEAD back to main between turns, and a "
                "commit landed on main because of it.\n\n"
                "**How to apply:** run `git branch --show-current` right before every commit and push.")
NEW_FACT = "The nightly cost report reads the BigQuery billing export; it lags by about a day."


@pytest.fixture
def rpc(store):
    reset_herd()
    get_herd().tick(sessions=[{"name": "claude-dp", "kind": "claude", "state": "idle",
                               "project": PROJECT}])
    yield JsonRpcHandler()
    reset_herd()


def remember(rpc, text, **kw):
    return rpc._herd_remember({"from": "claude-dp", "text": text, **kw})


def test_a_fact_said_again_is_refused_with_what_to_run(rpc, store):
    with pytest.raises(ValueError) as refused:
        remember(rpc, BRANCH_AGAIN)
    msg = str(refused.value)
    assert "`branch-discipline'" in msg and "this project" in msg
    assert "--name branch-discipline" in msg and "--new" in msg
    assert "Run the same command again" in msg
    # Nothing was written.
    assert not list(store.glob("always-work-on-a-feature*.md"))
    # --new: both kept, unchecked.
    out = remember(rpc, BRANCH_AGAIN, new=True)
    assert out["status"] == "new" and out["checked"] == "skipped"
    assert out["name"] != "branch-discipline" and (store / f"{out['name']}.md").exists()
    # --name: the fact is updated, its earlier text kept.
    out = remember(rpc, BRANCH_AGAIN, name="branch-discipline")
    assert out["status"] == "updated" and out["checked"] == "skipped"
    (h,) = sm.history(store, "branch-discipline")
    assert h["how"] == "remember" and h["by"] == "claude-dp"
    assert "twice a commit landed" in h["body"]
    assert "twice" not in sm.parse(store / "branch-discipline.md").body
    # A new fact on its own topic goes straight in, checked.
    out = remember(rpc, NEW_FACT)
    assert out["status"] == "new" and out["checked"] == "meaning"


def test_a_user_fact_said_again_in_a_project_points_at_the_user_store(rpc):
    with pytest.raises(ValueError) as refused:
        remember(rpc, "Jing wants a researched design proposal with its reasoning, not a menu "
                      "of options, when asking how something should be designed and built.")
    assert "--user --name prefers-reasoned-design" in str(refused.value)
    assert "user store" in str(refused.value)


def test_the_same_words_again_change_nothing(rpc, store):
    text = sm.parse(store / "kctx-multi-cluster.md").body
    out = remember(rpc, text, type="reference")
    assert out["status"] == "unchanged" and out["name"] == "kctx-multi-cluster"
    assert sm.history(store, "kctx-multi-cluster") == []
    # Through --name too: no new version.
    out = remember(rpc, text, type="reference", name="kctx-multi-cluster")
    assert out["status"] == "unchanged" and sm.history(store, "kctx-multi-cluster") == []


def test_new_with_the_name_of_a_fact_there_is_an_error(rpc):
    with pytest.raises(ValueError, match="another --name"):
        remember(rpc, NEW_FACT, name="kctx-multi-cluster", new=True)


def test_an_unready_embedder_lets_the_write_through(rpc, store, monkeypatch):
    monkeypatch.setattr(fact_index.FactIndex, "wait_ready", lambda self, s: False)
    out = remember(rpc, BRANCH_AGAIN)
    assert out["checked"] == "unchecked" and out["status"] == "new"


def test_an_auto_name_another_fact_has_is_never_taken(store):
    first = sm.write_fact(store, KEY, "Use uv sync before running the server tests, always.")
    other = sm.write_fact(store, KEY, "Use uv sync before running the e2e suites? No: npm install.")
    assert first["name"] == "use-uv-sync-before-running-the"
    assert other["status"] == "renamed" and other["name"] == "use-uv-sync-before-running-the-2"
    assert "server tests" in sm.parse(store / "use-uv-sync-before-running-the.md").body
    assert sm.history(store, first["name"]) == []
    # Nor a user fact's name, whose link a project fact would replace.
    sm.write_fact(sm.user_store(), sm.USER_KEY, "Prefer short answers.", name="short-answers")
    clash = sm.write_fact(store, KEY, "Short answers\n\nin PR descriptions too.")
    assert clash["status"] == "renamed" and clash["name"] == "short-answers-2"
    assert sm.parse(sm.user_store() / "short-answers.md").body == "Prefer short answers."


def test_edits_and_forgets_keep_the_earlier_text(rpc, store):
    sm.edit_fact(store, KEY, "kctx-multi-cluster", description="kctx picks the account",
                 body="Use kctx for every GKE cluster.", type="reference", author="user")
    (h,) = sm.history(store, "kctx-multi-cluster")
    assert h["how"] == "edit" and "gcloud service account" in h["body"]
    out = rpc._herd_forget({"from": "claude-dp", "name": "kctx-multi-cluster"})
    assert out["removed"]
    hist = sm.history(store, "kctx-multi-cluster")
    assert [x["how"] for x in hist] == ["forget", "edit"] and hist[0]["by"] == "claude-dp"
    assert "every GKE cluster" in hist[0]["body"]


def test_recall_says_what_a_fact_was(rpc, store):
    remember(rpc, BRANCH_AGAIN.replace("Always work", "Always work only"), name="branch-discipline")
    got = rpc._herd_recall({"from": "claude-dp", "query": "branch-discipline"})
    # Briefly: one line on the earlier text, when and by whom -- not that text.
    (was,) = [ln for ln in got["text"].splitlines() if ln.startswith("(was: ")]
    assert was.startswith("(was: Always work on a feature branch in deploy-platform")
    assert "remember by claude-dp" in was and "twice" not in got["text"]
    # A fact with no history has no such line.
    plain = rpc._herd_recall({"from": "claude-dp", "query": "kctx-multi-cluster"})
    assert "(was: " not in plain["text"]


def _store_with_history(rpc, store):
    remember(rpc, BRANCH_AGAIN, name="branch-discipline")
    sm.forget(store, KEY, "kctx-multi-cluster", by="claude-dp")
    hist = list((store / sm.HISTORY).glob("*.md"))
    assert len(hist) == 2
    return hist


def test_history_is_never_a_fact(rpc, store, tmp_path):
    _store_with_history(rpc, store)
    gone = "kctx-multi-cluster"
    # Not a fact of the store, of the pool, of the index MEMORY.md.
    assert gone not in {f.name for f in sm.own_facts(store)}
    assert all("." not in f.name for f in sm.pool(KEY))
    index = (store / sm.INDEX).read_text()
    assert gone not in index and ".history" not in index
    # Not in an opening.
    opening = sm.opening(KEY)
    assert gone not in opening["text"] and ".history" not in opening["text"]
    assert gone not in opening["full"] + opening["listed"] + opening["more"]
    # Not in recall, by name or by words, nor in its index.
    assert gone not in rpc._herd_recall({"from": "claude-dp", "query": gone})["names"]
    hits = rpc._herd_recall({"from": "claude-dp", "query": "kctx GKE cluster gcloud service account"})
    assert gone not in hits["names"]
    assert gone not in rpc._herd_recall({"from": "claude-dp"})["index"]
    # Not in the memory an ask carries.
    block = fact_index.get_index().ask_block({"project": PROJECT, "kind": "claude"},
                                             "Run kubectl on the GKE cluster with kctx and gcloud")
    assert gone not in (block or "")
    # Not in the fact index.
    fact_index.get_index().sync(KEY)
    assert gone not in fact_index.get_index()._stored(KEY)
    assert not any("." in n for n in fact_index.get_index()._stored(KEY))
    # Not in the Memory tab's list -- a fact's detail has its history.
    rows = {r["name"]: r for r in sm.facts_for(KEY)}
    assert gone not in rows
    assert rows["branch-discipline"]["history"] and "remember by claude-dp" in rows["branch-discipline"]["history"][0]
    assert rows["bitbucket-pr-auth"]["history"] == []
    # Not promotable.
    with pytest.raises(ValueError):
        sm.check_promotable(store, KEY, gone)
    # Not in the session index's import of claude's memory dir (a link to the store).
    claude = tmp_path / "claude" / "-work-deploy-platform"
    claude.mkdir(parents=True)
    (claude / "memory").symlink_to(store)
    paths = [meta.path for meta, _ in iter_claude(tmp_path / "claude")]
    assert paths and not any(sm.HISTORY in p for p in paths)


def test_a_write_that_bypasses_the_herd_keeps_the_earlier_text(store, monkeypatch):
    sm.notice(store, KEY)  # the herd has seen the store
    path = store / "bitbucket-pr-auth.md"
    old = path.read_text()
    # claude rewrites the file with its own Write tool.
    path.write_text(old.replace("not a username", "never a username or app password"))
    # Just written: maybe half-way -- left for the next look.
    assert sm.notice(store, KEY) == []
    past = time.time() - 10
    os.utime(path, (past, past))
    # The fact index notices it on its next read of the store.
    fact_index.get_index().sync(KEY)
    (h,) = sm.history(store, "bitbucket-pr-auth")
    assert h["how"] == "edited directly" and "not a username" in h["body"]
    # Once only.
    assert sm.notice(store, KEY) == []
    # rm by hand: its text kept too.
    (store / "kctx-multi-cluster.md").unlink()
    assert sm.notice(store, KEY) == ["kctx-multi-cluster"]
    (h,) = sm.history(store, "kctx-multi-cluster")
    assert h["how"] == "removed directly" and "gcloud service account" in h["body"]
    # A change to stamps alone -- the herd stamping claude's author -- is no version.
    sm.stamp_author(path, "claude-dp")
    os.utime(path, (past + 1, past + 1))
    assert sm.notice(store, KEY) == [] and len(sm.history(store, "bitbucket-pr-auth")) == 1


def test_the_first_look_only_takes_note(store):
    seen = store / sm.HISTORY / sm.SEEN
    seen.unlink(missing_ok=True)
    sm._SEEN_CACHE.clear()
    assert sm.notice(store, KEY) == []
    data = json.loads(seen.read_text())
    assert "branch-discipline" in data and data["branch-discipline"]["text"].startswith("---")


def test_old_history_is_pruned(store, monkeypatch):
    for i in range(sm.HISTORY_KEEP + 3):
        sm.write_fact(store, KEY, f"The retry budget is {i} attempts per job.", name="retry-budget",
                      now=f"2026-10-{1 + i:02d}T00:00:00Z")
    assert len(sm.history(store, "retry-budget")) == sm.HISTORY_KEEP
    # A year past: the earlier texts go but the newest, so it can be got back.
    sm.keep_history(store, "retry-budget", "old text", how="remember",
                    now="2020-01-01T00:00:00Z")
    old = [h for h in sm.history(store, "retry-budget") if h["when"].startswith("2020")]
    assert old == []
    sm.forget(store, KEY, "retry-budget")
    monkeypatch.setattr(sm, "HISTORY_DAYS", 0)
    sm._prune(store)
    left = sm.history(store, "retry-budget")
    assert len(left) == 1 and left[0]["how"] == "forget"
