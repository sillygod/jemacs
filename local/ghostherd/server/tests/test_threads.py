"""Threads: who is talking to whom across the herd (threads.py, the page's
Threads tab).  A thread is a root ask and every ask made while answering one
in it, with what came back; built from the asks the herd already keeps."""

import sqlite3
from pathlib import Path

import pytest

import threads as th
from herd import HerdStore, reset_herd
from tests.test_ask import SNAP, _deliver


def _store(tmp_path: Path) -> HerdStore:
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=SNAP)
    return h


def _working(h, *names):
    "The snapshot with NAMES working, the rest as SNAP has them."
    snap = [dict(r, state="working") if r["name"] in names else r for r in SNAP]
    h.tick(sessions=snap)
    return snap


# ---- A chain: an ask, the asks made while answering it, the answers ----------


def test_a_chain_is_one_thread_with_its_rounds(tmp_path):
    """claude-main asks agy-a2; agy-a2, answering it, asks agy-a twice;
    both answer.  One thread, its messages in time order, each sub-ask
    with its round."""
    h = _store(tmp_path)
    root = h.ask("agy-a2", "Ship the login fix", from_name="claude-main")
    _deliver(h)
    sub1 = h.ask("agy-a", "Run the e2e suite", from_name="agy-a2")
    _deliver(h)
    h.reply(sub1["id"], "e2e: 12/12", from_name="agy-a")
    sub2 = h.ask("agy-a", "And the phone suite", from_name="agy-a2")
    _deliver(h)
    h.reply(sub2["id"], "phone: 8/8", from_name="agy-a")
    h.reply(root["id"], "Shipped in abc1234.", from_name="agy-a2")

    out = h.threads()
    assert [t["id"] for t in out["threads"]] == [root["id"]]
    t = out["threads"][0]
    assert (t["status"], t["count"], t["participants"]) == ("answered", 6, ["claude-main", "agy-a2", "agy-a"])
    assert t["title"] == "Ship the login fix" and t["rootFrom"] == "claude-main" and not t["cross"]

    d = h.thread(id=root["id"])
    assert [(m["kind"], m["from"], m["to"]) for m in d["msgs"]] == [
        ("ask", "claude-main", "agy-a2"), ("ask", "agy-a2", "agy-a"), ("answer", "agy-a", "agy-a2"),
        ("ask", "agy-a2", "agy-a"), ("answer", "agy-a", "agy-a2"), ("answer", "agy-a2", "claude-main")]
    rounds = [(m["round"], m["of"]) for m in d["msgs"] if m["kind"] == "ask"]
    assert rounds[1:] == [(1, 5), (2, 5)]
    assert [ln["name"] for ln in d["lanes"]] == ["claude-main", "agy-a2", "agy-a"]
    assert [ln["kind"] for ln in d["lanes"]] == ["claude", "agy", "agy"]
    # Any ask in it, or an agent in it, opens the same thread.
    assert h.thread(ask=sub2["id"])["id"] == root["id"]
    assert h.thread(agent="agy-a")["id"] == root["id"]


def test_two_root_asks_are_two_threads(tmp_path):
    "Asks made while answering none are chains of their own, even in one run."
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Fix the login", from_name="claude-main")
    b = h.ask("agy-a", "Fix the footer", from_name="claude-main")
    ids = [t["id"] for t in h.threads()["threads"]]
    assert sorted(ids) == sorted([a["id"], b["id"]])


def test_the_whole_text_and_the_first_lines(tmp_path):
    h = _store(tmp_path)
    body = "Line one\n\nLine two\nLine three\nLine four\n" + "x" * 50
    a = h.ask("agy-a2", body, from_name="claude-main")
    m = h.thread(ask=a["id"])["msgs"][0]
    assert m["text"] == body and m["head"] == "Line one\nLine two\nLine three"


# ---- Where it stands -----------------------------------------------------------


def test_live_waiting_answered_failed(tmp_path):
    h = _store(tmp_path)
    a = h.ask("agy-a2", "the work", from_name="claude-main")
    assert h.thread(ask=a["id"])["status"] == "waiting"         # queued: nobody on it
    _deliver(h)
    _working(h, "agy-a2")
    d = h.thread(ask=a["id"])
    assert d["status"] == "live"
    assert d["waiting"] == [dict(d["waiting"][0], **{"from": "claude-main", "to": "agy-a2", "working": True,
                                                     "status": "delivered", "depth": 0})]
    h.tick(sessions=SNAP)                                         # it stopped: idle
    assert h.thread(ask=a["id"])["status"] == "waiting"
    h.reply(a["id"], "done", from_name="agy-a2")
    assert h.thread(ask=a["id"])["status"] == "answered"
    b = h.ask("agy-a", "other work", from_name="claude-main")
    h.cancel(b["id"])
    d = h.thread(ask=b["id"])
    assert d["status"] == "failed" and d["msgs"][-1]["kind"] == "cancelled"


def test_talking_now_is_every_open_ask(tmp_path):
    h = _store(tmp_path)
    a = h.ask("agy-a2", "one", from_name="claude-main")
    _deliver(h)
    _working(h, "agy-a2")
    sub = h.ask("agy-a", "two", from_name="agy-a2")
    done = h.ask("agy-a", "three", from_name="claude-main")
    _deliver(h)
    h.reply(done["id"], "ok", from_name="agy-a")
    snap = _working(h, "agy-a2")
    out = h.threads()
    talk = {t["ask"]: t for t in out["talking"]}
    assert set(talk) == {a["id"], sub["id"]}
    assert (talk[a["id"]]["working"], talk[a["id"]]["status"]) == (True, "delivered")
    assert talk[sub["id"]]["thread"] == a["id"]                  # the strip opens its thread
    assert out["live"] == 1                                       # threads with an ask open
    assert snap
    # In the thread, who waits on whom: the deepest first.
    w = h.thread(ask=a["id"])["waiting"]
    assert [(x["from"], x["to"], x["depth"]) for x in w] == [("agy-a2", "agy-a", 1), ("claude-main", "agy-a2", 0)]


# ---- Handoffs, orphans, old asks -------------------------------------------------


def test_an_orphan_is_its_own_thread():
    "A parent the herd no longer has: the ask roots its own thread."
    rows = [{"id": "c1", "parent": "gone", "from_name": "a", "to_name": "b", "body": "x", "reply": None,
             "status": "queued", "auto": 0, "error": None, "created_at": "2026-10-22T01:00:00+00:00",
             "delivered_at": None, "answered_at": None, "round": 1, "round_of": 5, "project": None,
             "from_project": None}]
    listed, more = th.summaries(rows, {}, 10)
    assert [t["id"] for t in listed] == ["c1"] and not more
    loop = {"a1": {"id": "a1", "parent": "a2"}, "a2": {"id": "a2", "parent": "a1"}}
    assert th.root_of("a1", loop) in ("a1", "a2")                # a cycle does not hang it


def test_old_asks_without_links_are_single_threads(tmp_path):
    """A herd DB from before rounds: asks with no parent or run.  The store's
    migration adds the columns; each old ask is a thread of its own, and an
    ask made after chains as any does."""
    path = tmp_path / "herd.sqlite"
    db = sqlite3.connect(path)
    db.execute("""CREATE TABLE asks (id TEXT PRIMARY KEY, from_name TEXT NOT NULL, to_name TEXT NOT NULL,
                  body TEXT NOT NULL, wait INTEGER NOT NULL, status TEXT NOT NULL, reply TEXT,
                  auto INTEGER NOT NULL DEFAULT 0, replied_by TEXT, error TEXT,
                  collected INTEGER NOT NULL DEFAULT 0, created_at TEXT NOT NULL, delivered_at TEXT,
                  answered_at TEXT)""")
    from datetime import datetime, timedelta, timezone
    then = datetime.now(timezone.utc) - timedelta(hours=5)     # within the herd's history: kept
    for i, (frm, to) in enumerate([("claude-main", "agy-a2"), ("agy-a2", "agy-a"), ("claude-main", "agy-a")]):
        db.execute("INSERT INTO asks (id, from_name, to_name, body, wait, status, reply, created_at, answered_at)"
                   " VALUES (?, ?, ?, ?, 0, 'answered', 'ok', ?, ?)",
                   (f"old{i}", frm, to, f"old ask {i}", (then + timedelta(minutes=i)).isoformat(),
                    (then + timedelta(minutes=i, seconds=30)).isoformat()))
    db.commit()
    db.close()
    reset_herd()
    h = HerdStore(path)
    h.tick(sessions=SNAP)
    out = h.threads()
    assert sorted(t["id"] for t in out["threads"]) == ["old0", "old1", "old2"]
    assert all(t["count"] == 2 for t in out["threads"])
    new = h.ask("agy-a2", "new work", from_name="claude-main")
    _deliver(h)
    sub = h.ask("agy-a", "part of it", from_name="agy-a2")
    assert h.thread(ask=sub["id"])["id"] == new["id"]


def test_the_list_is_newest_first_and_says_when_there_is_more(tmp_path):
    h = _store(tmp_path)
    ids = [h.ask("agy-a", f"ask {i}", from_name="claude-main")["id"] for i in range(5)]
    out = h.threads(limit=3)
    assert [t["id"] for t in out["threads"]] == ids[::-1][:3] and out["more"]
    assert not h.threads(limit=5)["more"]


def test_asking_for_a_thread_that_is_not_there(tmp_path):
    h = _store(tmp_path)
    with pytest.raises(ValueError, match="no thread with nobody"):
        h.thread(agent="nobody")
    with pytest.raises(ValueError, match="no ask"):
        h.thread(id="feedbeef")


def test_the_rpcs_check_what_they_are_given(tmp_path, monkeypatch):
    from jsonrpc_handler import JsonRpcHandler
    rpc = JsonRpcHandler()
    for bad in ({}, {"id": 5}, {"agent": ["x"]}):
        with pytest.raises(ValueError):
            rpc._herd_thread(bad)
    with pytest.raises(ValueError, match="limit"):
        rpc._herd_threads({"limit": "many"})
