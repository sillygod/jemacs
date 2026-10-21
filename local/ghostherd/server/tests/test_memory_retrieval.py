"""The facts' own index stays in sync with the files however they change;
`herd recall' and the page's ranking read it, by meaning and words, and
fall back to words alone when the embedder is not up."""

import time

import pytest

import fact_index
import shared_memory as sm
from herd import reset_herd
from jsonrpc_handler import JsonRpcHandler
from memory_store import (DEPLOY_ASK, KEY, PROJECT, SNAP, UNRELATED_ASKS, store)  # noqa: F401

# ---- The index follows the files ---------------------------------------------


def _wait(pred, seconds=10.0):
    end = time.monotonic() + seconds
    while time.monotonic() < end:
        if pred():
            return True
        time.sleep(0.05)
    return pred()


def test_the_index_follows_writes_edits_and_forgets(store):
    idx = fact_index.get_index()
    idx.sync(KEY)
    names = {f.name for f in sm.own_facts(store)}
    assert set(idx._stored(KEY)) == names
    # Written through the store (herd remember, the page): indexed at once.
    sm.write_fact(store, KEY, "Canary deploys go to one pod first, then the rest.",
                  name="canary-deploys")
    assert _wait(lambda: "canary-deploys" in idx._stored(KEY))
    before = idx._stored(KEY)["canary-deploys"]
    # Edited: its text changed, so it is embedded again.
    sm.write_fact(store, KEY, "Canary deploys now take two pods, watched for an hour.",
                  name="canary-deploys")
    assert _wait(lambda: idx._stored(KEY)["canary-deploys"][0] != before[0])
    assert idx._stored(KEY)["canary-deploys"][1] != before[1]
    # Forgotten: its point goes.
    sm.forget(store, KEY, "canary-deploys")
    assert _wait(lambda: "canary-deploys" not in idx._stored(KEY))


def test_claudes_own_writes_are_found_on_the_next_read(store):
    idx = fact_index.get_index()
    idx.sync(KEY)
    # claude writes its memory file itself: no listener hears it.
    (store / "rollback-runbook.md").write_text(
        "---\nname: rollback-runbook\ndescription: How to roll back a bad deploy\n"
        "metadata: \n  type: project\n  originSessionId: s1\n---\n\n"
        "Roll back with helm rollback on the deploy-platform release, then re-run the smoke checks.\n")
    found = idx.rank(KEY, "how do I roll back a bad deploy with helm rollback", strict=False)
    assert found["mode"] == "meaning"
    assert found["hits"] and found["hits"][0]["name"] == "rollback-runbook"
    assert "rollback-runbook" in idx._stored(KEY)
    # And a file removed by hand leaves the index at the next read.
    (store / "rollback-runbook.md").unlink()
    idx.rank(KEY, "anything", strict=False)
    assert "rollback-runbook" not in idx._stored(KEY)


def test_a_restart_embeds_nothing_it_already_has(store):
    idx = fact_index.get_index()
    assert idx.sync(KEY)["embedded"] == len(sm.own_facts(store))
    idx._seen.clear()
    idx._vecs.clear()                       # a new process, the same Qdrant
    assert idx.sync(KEY)["embedded"] == 0


# ---- Recall, the ranking function, and the fallback ------------------------------


@pytest.fixture
def rpc(store, tmp_path):
    reset_herd()
    from herd import get_herd
    get_herd().tick(sessions=SNAP)
    yield JsonRpcHandler()
    reset_herd()


def test_recall_ranks_the_relevant_fact_first(rpc):
    out = rpc._herd_recall({"from": "claude-dp", "query": "query prometheus metrics on cn-stg"})
    assert out["mode"] == "meaning"
    assert out["names"][0] == "cn-stg-metrics-querying"
    assert "two facts that make naive queries return nothing" in out["text"]


def test_memory_rank_is_one_call_with_scores(rpc):
    out = rpc._memory_rank({"from": "claude-dp", "query": "feature branch push main", "limit": 2})
    assert out["mode"] == "meaning" and len(out["hits"]) <= 2
    top = out["hits"][0]
    assert top["name"] == "branch-discipline"
    assert {"score", "z", "lex", "why", "store", "type"} <= set(top)
    strict = rpc._memory_rank({"from": "claude-dp", "query": UNRELATED_ASKS[0], "strict": True})
    assert strict["hits"] == []


def test_words_alone_when_the_embedder_is_not_up(rpc, monkeypatch, tmp_path):
    idx = fact_index.get_index()
    monkeypatch.setattr(idx.embedder, "loaded", False)

    def refuse():
        raise RuntimeError("no model")

    monkeypatch.setattr(idx.embedder, "load", refuse)
    out = rpc._herd_recall({"from": "claude-dp", "query": "cn-stg prometheus metrics"})
    assert out["mode"] == "words"
    assert "ranked by words alone" in out["text"]
    assert out["names"][0] == "cn-stg-metrics-querying"
    # Chinese words count in a recall's fallback.
    zh = rpc._herd_recall({"from": "claude-dp", "query": "憑證續簽"})
    assert zh["names"] and zh["names"][0] == "vm-playbook-nginx-certs"
