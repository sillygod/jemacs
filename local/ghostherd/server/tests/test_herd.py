import json
from pathlib import Path

from fastapi.testclient import TestClient

from config import Config
from engine import reset_engine
from herd import HerdStore, reset_herd
from main import app


def _store(tmp_path: Path, limit: int = 4000) -> HerdStore:
    reset_herd()
    return HerdStore(tmp_path / "herd.sqlite", inline_limit=limit)


def test_tick_delivers_idle_and_acks(tmp_path: Path):
    h = _store(tmp_path)
    queued = h.enqueue(to="grok-dev", body="hi", from_name="claude-research")
    snap = [{"name": "grok-dev", "kind": "grok", "state": "idle"}]
    first = h.tick(sessions=snap, ack_ids=[])
    assert len(first["pending"]) == 1
    assert first["pending"][0]["id"] == queued["id"]
    assert first["pending"][0]["from"] == "claude-research"
    assert first["pending"][0]["body"] == "hi"
    again = h.tick(sessions=snap, ack_ids=[])
    assert again["pending"][0]["id"] == queued["id"]
    gone = h.tick(sessions=snap, ack_ids=[queued["id"]])
    assert gone["pending"] == []
    inbox = h.inbox("grok-dev")
    assert inbox["messages"][0]["id"] == queued["id"]
    reset_herd()


def test_working_holds_then_idle_releases(tmp_path: Path):
    h = _store(tmp_path)
    h.enqueue(to="grok-dev", body="later", from_name="claude-research")
    held = h.tick(
        sessions=[{"name": "grok-dev", "state": "working"}],
        ack_ids=[],
    )
    assert held["pending"] == []
    ready = h.tick(
        sessions=[{"name": "grok-dev", "state": "idle"}],
        ack_ids=[],
    )
    assert len(ready["pending"]) == 1
    assert ready["pending"][0]["body"] == "later"
    reset_herd()


def test_unknown_target_fails(tmp_path: Path):
    h = _store(tmp_path)
    h.enqueue(to="nope", body="x", from_name="claude-research")
    out = h.tick(
        sessions=[{"name": "grok-dev", "state": "idle"}],
        ack_ids=[],
    )
    assert out["pending"] == []
    reset_herd()


def test_list_returns_snapshot(tmp_path: Path):
    h = _store(tmp_path)
    h.tick(
        sessions=[
            {
                "name": "claude-research",
                "kind": "claude",
                "state": "idle",
                "notes": "research",
                "project": "/src",
            }
        ],
        ack_ids=[],
    )
    listed = h.list_sessions()
    assert listed["sessions"][0]["name"] == "claude-research"
    assert listed["sessions"][0]["notes"] == "research"
    assert h.list_sessions(project="/other")["sessions"] == []
    reset_herd()


def test_from_defaults_to_unknown(tmp_path: Path):
    h = _store(tmp_path)
    queued = h.enqueue(to="grok-dev", body="x")
    out = h.tick(
        sessions=[{"name": "grok-dev", "state": "idle"}],
        ack_ids=[],
    )
    assert out["pending"][0]["from"] == "unknown"
    assert queued["queued"] is True
    reset_herd()


def test_long_body_is_truncated(tmp_path: Path):
    h = _store(tmp_path, limit=8)
    h.enqueue(to="grok-dev", body="abcdefghij", from_name="a")
    out = h.tick(
        sessions=[{"name": "grok-dev", "state": "idle"}],
        ack_ids=[],
    )
    assert out["pending"][0]["truncated"] is True
    assert out["pending"][0]["body"].startswith("abcdefgh")
    assert "herd_inbox" in out["pending"][0]["body"]
    reset_herd()


def _rpc(client, method, params=None, id=1):
    body = {"jsonrpc": "2.0", "id": id, "method": method}
    if params is not None:
        body["params"] = params
    resp = client.post("/jsonrpc/" + client.app.state.token, json=body)
    assert resp.status_code == 200
    data = resp.json()
    assert data.get("error") is None, data
    return data["result"]


def test_herd_rpc_round_trip(tmp_path: Path, monkeypatch):
    reset_engine()
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_FAKE_EMBED", "1")
    with TestClient(app, base_url="http://127.0.0.1") as client:
        queued = _rpc(
            client,
            "herd_message",
            {
                "to": "grok-dev",
                "from": "claude-research",
                "body": "findings",
                "handoff": True,
            },
        )
        assert queued["queued"] is True
        _rpc(
            client,
            "herd_tick",
            {"sessions": [{"name": "grok-dev", "state": "working"}], "ack_ids": []},
        )
        listed = _rpc(client, "herd_list")
        assert listed["sessions"][0]["name"] == "grok-dev"
        pending = _rpc(
            client,
            "herd_tick",
            {"sessions": [{"name": "grok-dev", "state": "idle"}], "ack_ids": []},
        )
        assert pending["pending"][0]["handoff"] is True
        _rpc(
            client,
            "herd_tick",
            {
                "sessions": [{"name": "grok-dev", "state": "idle"}],
                "ack_ids": [queued["id"]],
            },
        )
        inbox = _rpc(client, "herd_inbox", {"session": "grok-dev"})
        assert inbox["messages"][0]["body"] == "findings"
    reset_engine()


def test_command_round_trip(tmp_path: Path):
    h = _store(tmp_path)
    h.tick(sessions=[{"name": "agy", "state": "blocked", "reason": "proceed?"}], ack_ids=[])
    queued = h.enqueue_command("answer", "agy", args={"n": 2}, chat_id=42)
    first = h.tick(
        sessions=[{"name": "agy", "state": "blocked"}],
        ack_ids=[],
    )
    assert first["commands"][0]["id"] == queued["id"]
    assert first["commands"][0]["op"] == "answer"
    assert first["commands"][0]["args"]["n"] == 2
    again = h.tick(sessions=[{"name": "agy", "state": "blocked"}], ack_ids=[])
    assert again["commands"] == []
    h.tick(
        sessions=[{"name": "agy", "state": "idle"}],
        ack_ids=[],
        replies=[{"id": queued["id"], "text": "answered 2"}],
    )
    pending = h.unsent_replies()
    assert pending[0]["text"] == "answered 2"
    assert pending[0]["chat_id"] == 42
    h.mark_replies_sent([queued["id"]])
    assert h.unsent_replies() == []
    reset_herd()


def test_command_reply_reads_status_and_text(tmp_path: Path):
    "`herd relay' queues a command and polls command_reply for Emacs's word."
    h = _store(tmp_path)
    h.tick(sessions=[{"name": "rd", "state": "idle"}], ack_ids=[])
    queued = h.enqueue_command("relay", "rd", args={"kind": "grok", "by": "lead-p"})
    cid = queued["id"]
    assert h.command_reply(cid) == {"status": "queued", "text": None}
    h.tick(sessions=[{"name": "rd", "state": "idle"}], ack_ids=[])  # taken
    assert h.command_reply(cid)["status"] == "taken"
    h.tick(sessions=[{"name": "rd", "state": "idle"}], ack_ids=[],
           replies=[{"id": cid, "text": "relaying rd to grok"}])
    assert h.command_reply(cid) == {"status": "done", "text": "relaying rd to grok"}
    assert h.command_reply("nope")["status"] == "gone"
    reset_herd()


def test_screens_query_round_trips_through_emacs(tmp_path: Path, monkeypatch):
    "`herd screens' queues a read-only command Emacs answers from its log."
    import asyncio
    from herd import get_herd
    from jsonrpc_handler import JsonRpcRequest, handler
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    reset_herd()
    h = get_herd()
    h.tick(sessions=[{"name": "grok-x", "state": "idle"}], ack_ids=[])
    r = asyncio.run(handler.handle(JsonRpcRequest(
        jsonrpc="2.0", id=1, method="herd_screens",
        params={"agent": "grok-x", "since": 123.0, "why": "limit", "by": "lead-p"})))
    cid = r.result["id"]
    out = h.tick(sessions=[{"name": "grok-x", "state": "idle"}], ack_ids=[])
    cmd = out["commands"][0]
    assert cmd["id"] == cid and cmd["op"] == "screens"
    assert cmd["args"] == {"agent": "grok-x", "since": 123.0, "why": "limit"}
    # Emacs reads its log and answers; the CLI polls that reply.
    h.tick(sessions=[{"name": "grok-x", "state": "idle"}], ack_ids=[],
           replies=[{"id": cid, "text": "1 screen:\n2026-10-20 …"}])
    assert h.command_reply(cid)["text"].startswith("1 screen")
    reset_herd()


def test_start_queues_a_start_command(tmp_path: Path, monkeypatch):
    "`herd start' queues a `start' command Emacs acts on; the args ride along."
    import asyncio
    from herd import get_herd
    from jsonrpc_handler import JsonRpcRequest, handler
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    reset_herd()
    h = get_herd()
    r = asyncio.run(handler.handle(JsonRpcRequest(
        jsonrpc="2.0", id=1, method="herd_start",
        params={"by": "lead-p", "kind": "claude", "worktree": "yes",
                "branch": "feat/x", "name": "", "notes": "impl", "new": True})))
    cid = r.result["id"]
    cmd = h.tick(sessions=[], ack_ids=[])["commands"][0]
    assert cmd["id"] == cid and cmd["op"] == "start" and cmd["session"] == "lead-p"
    assert cmd["args"] == {"by": "lead-p", "kind": "claude", "worktree": "yes",
                           "branch": "feat/x", "name": "", "notes": "impl", "new": True}
    h.tick(sessions=[], ack_ids=[], replies=[{"id": cid, "text": "started claude-p (…)"}])
    assert h.command_reply(cid)["text"].startswith("started ")
    reset_herd()


def test_start_rejects_a_bad_kind_or_worktree(tmp_path: Path, monkeypatch):
    "A mis-shapen kind or unknown worktree mode is refused before queuing."
    import asyncio
    from jsonrpc_handler import JsonRpcRequest, handler
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    reset_herd()
    for params in ({"by": "lead-p", "kind": "Not A CLI"},
                   {"by": "lead-p", "kind": "claude", "worktree": "maybe"}):
        r = asyncio.run(handler.handle(JsonRpcRequest(
            jsonrpc="2.0", id=1, method="herd_start", params=params)))
        assert r.error is not None and r.result is None
    reset_herd()


def test_list_enriches_agents_for_the_queue_decision(tmp_path: Path, monkeypatch):
    "`herd list' shows started-by, ahead/merged, the open ask and how many queue."
    from herd import get_herd
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    reset_herd()
    h = get_herd()
    h.tick(sessions=[{"name": "imp", "kind": "grok", "state": "working",
                      "project": "/p/", "startedBy": "lead-p", "branch": "feat/x",
                      "ahead": 3, "merged": False}], ack_ids=[])
    # One ask delivered to imp (it owes it), two more queued behind.
    now = "2026-10-21T00:00:00+00:00"
    with h._mu:
        h._conn.execute(
            "INSERT INTO asks (id, from_name, to_name, body, wait, status, created_at, delivered_at)"
            " VALUES ('a1', 'lead-p', 'imp', 'do', 0, 'delivered', ?, ?)", (now, now))
        for i in (2, 3):
            h._conn.execute(
                "INSERT INTO asks (id, from_name, to_name, body, wait, status, created_at)"
                f" VALUES ('a{i}', 'qa', 'imp', 'later', 0, 'queued', ?)", (now,))
        h._conn.commit()
    s = next(x for x in h.list_sessions()["sessions"] if x["name"] == "imp")
    assert s["started_by"] == "lead-p" and s["ahead"] == 3 and s["merged"] is False
    assert s["branch"] == "feat/x"
    assert s["ask"] == {"id": "a1", "from": "lead-p"}
    assert s["queued"] == 2
    assert isinstance(s["editing"], list)
    reset_herd()


def test_stop_queues_a_stop_command(tmp_path: Path, monkeypatch):
    "`herd stop NAME' queues a `stop' command against that agent."
    import asyncio
    from herd import get_herd
    from jsonrpc_handler import JsonRpcRequest, handler
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    reset_herd()
    h = get_herd()
    r = asyncio.run(handler.handle(JsonRpcRequest(
        jsonrpc="2.0", id=1, method="herd_stop",
        params={"by": "lead-p", "name": "claude-p-2"})))
    cid = r.result["id"]
    cmd = h.tick(sessions=[], ack_ids=[])["commands"][0]
    assert cmd["op"] == "stop" and cmd["session"] == "claude-p-2"
    assert cmd["args"] == {"by": "lead-p"}
    reset_herd()


def test_snapshot_keeps_screen(tmp_path: Path):
    h = _store(tmp_path)
    h.tick(
        sessions=[
            {
                "name": "agy",
                "state": "working",
                "screen": "esc to interrupt\n",
            }
        ],
        ack_ids=[],
    )
    row = h.session("agy")
    assert row["screen"] == "esc to interrupt\n"
    listed = h.list_sessions()["sessions"][0]
    assert "screen" not in listed
    reset_herd()


def test_list_returns_directory_and_branch(tmp_path: Path):
    h = _store(tmp_path)
    h.tick(
        sessions=[
            {
                "name": "wk",
                "kind": "claude",
                "state": "idle",
                "project": "/src",
                "directory": "~/.ghostherd/worktrees/src/wk/",
                "branch": "wk",
            },
            {
                "name": "pl",
                "kind": "claude",
                "state": "idle",
                "project": "/src",
                "directory": "/src/",
            },
        ],
        ack_ids=[],
    )
    by_name = {s["name"]: s for s in h.list_sessions()["sessions"]}
    assert by_name["wk"]["directory"] == "~/.ghostherd/worktrees/src/wk/"
    assert by_name["wk"]["branch"] == "wk"
    assert by_name["pl"]["directory"] == "/src/"
    assert by_name["pl"]["branch"] == ""
    reset_herd()


def test_alias_is_stable(tmp_path: Path):
    h = _store(tmp_path)
    h.tick(sessions=[{"name": "agy-ghost-commit", "state": "idle"}], ack_ids=[])
    listed = h.list_sessions()["sessions"][0]
    short = listed["short"]
    assert len(short) == 8
    assert h.name_for(short) == "agy-ghost-commit"
    assert h.short_for("agy-ghost-commit") == short
    reset_herd()


def test_herd_tick_takes_the_page_address_over_rpc(tmp_path: Path, monkeypatch):
    "The herd page's address, and each room's, come in Emacs's herd_tick; not a string, none."
    reset_engine()
    reset_herd()
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_FAKE_EMBED", "1")
    monkeypatch.delenv("GHOSTHERD_TELEGRAM_TOKEN", raising=False)
    monkeypatch.delenv("GHOSTHERD_TELEGRAM_CHAT_IDS", raising=False)
    page = "http://box.tail0.ts.net/open/ghostherd#agents"
    with TestClient(app, base_url="http://127.0.0.1") as client:
        from herd import get_herd
        _rpc(client, "herd_tick", {"sessions": [{"name": "rd", "state": "idle", "room": page + "x"}],
                                   "page_url": page})
        assert get_herd().page_url() == page
        assert get_herd().session("rd")["room"] == page + "x"
        _rpc(client, "herd_tick", {"sessions": [], "page_url": ["x"]})
        assert get_herd().page_url() == ""
        _rpc(client, "herd_tick", {"sessions": [], "page_url": page})
        _rpc(client, "herd_tick", {"sessions": []})
        assert get_herd().page_url() == ""
    reset_engine()
    reset_herd()


def test_herd_tick_takes_pushes_over_rpc(tmp_path: Path, monkeypatch):
    "What Emacs pushes comes in its herd_tick, a list or refused."
    reset_engine()
    reset_herd()
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_FAKE_EMBED", "1")
    # No bot polling Telegram from a test.
    monkeypatch.delenv("GHOSTHERD_TELEGRAM_TOKEN", raising=False)
    monkeypatch.delenv("GHOSTHERD_TELEGRAM_CHAT_IDS", raising=False)
    with TestClient(app, base_url="http://127.0.0.1") as client:
        _rpc(client, "herd_tick", {"sessions": [], "pushes": [
            {"kind": "blocked", "session": "rd", "project": "/p/", "url": "", "reason": "proceed?"}]})
        from herd import get_herd
        got = get_herd().unsent_pushes()
        assert [(p["kind"], p["session"], p["reason"]) for p in got] == [("blocked", "rd", "proceed?")]
        resp = client.post("/jsonrpc/" + client.app.state.token, json={
            "jsonrpc": "2.0", "id": 2, "method": "herd_tick", "params": {"sessions": [], "pushes": "rd"}})
        assert "pushes must be a list" in resp.json()["error"]["message"]
    reset_engine()
    reset_herd()


def test_list_sessions_orders_by_state_rank_then_recency(tmp_path: Path):
    """list_sessions matches the web page's order: state rank (blocked,
    working, starting, waiting, done, idle, dead), then most recently
    updated (`since' newest first), breaking ties by name."""
    h = _store(tmp_path)
    h.tick(
        sessions=[
            {"name": "z-idle-old", "state": "idle", "since": 1000, "project": "/p"},
            {"name": "z-idle-new", "state": "idle", "since": 5000, "project": "/p"},
            {"name": "a-dead", "state": "dead", "since": 9000, "project": "/p"},
            {"name": "m-working", "state": "working", "since": 2000, "project": "/p"},
            {"name": "b-blocked", "state": "blocked", "since": 500, "project": "/p"},
            {"name": "c-waiting", "state": "waiting", "since": 3000, "project": "/p"},
            {"name": "d-done", "state": "done", "since": 4000, "project": "/p"},
        ],
        ack_ids=[],
    )
    names = [s["name"] for s in h.list_sessions()["sessions"]]
    assert names == [
        "b-blocked",
        "m-working",
        "c-waiting",
        "d-done",
        "z-idle-new",
        "z-idle-old",
        "a-dead",
    ]
    assert [s["name"] for s in h.list_sessions(project="/p")["sessions"]] == names
    reset_herd()


def test_prune_removes_only_old_settled_rows(tmp_path: Path):
    """tick prunes settled mail, asks, commands, replies, and pushes older
    than HISTORY_SECONDS while keeping recent settled rows, open/queued
    rows, uncollected waiting asks, and asks in an active run."""
    from datetime import datetime, timedelta, timezone
    import herd as herd_mod

    h = _store(tmp_path)
    now = datetime.now(timezone.utc)
    old_iso = (now - timedelta(seconds=herd_mod.HISTORY_SECONDS + 3600)).isoformat()
    recent_iso = (now - timedelta(seconds=60)).isoformat()

    snap = [
        {"name": "lead", "kind": "claude", "state": "working", "project": "/p", "role": "lead"},
        {"name": "rd", "kind": "claude", "state": "idle", "project": "/p"},
    ]
    h.tick(sessions=snap)

    # 1. Mail: old delivered, old failed, recent delivered, and old queued (must stay!).
    h._conn.execute(
        "INSERT INTO mail (id, from_name, to_name, body, submit, handoff, status, created_at, delivered_at) "
        "VALUES ('m-old-del', 'lead', 'rd', 'old', 1, 0, 'delivered', ?, ?)",
        (old_iso, old_iso),
    )
    h._conn.execute(
        "INSERT INTO mail (id, from_name, to_name, body, submit, handoff, status, created_at) "
        "VALUES ('m-old-fail', 'lead', 'rd', 'fail', 1, 0, 'failed', ?)",
        (old_iso,),
    )
    h._conn.execute(
        "INSERT INTO mail (id, from_name, to_name, body, submit, handoff, status, created_at, delivered_at) "
        "VALUES ('m-rec-del', 'lead', 'rd', 'rec', 1, 0, 'delivered', ?, ?)",
        (recent_iso, recent_iso),
    )
    h._conn.execute(
        "INSERT INTO mail (id, from_name, to_name, body, submit, handoff, status, created_at) "
        "VALUES ('m-old-queued', 'rd', 'lead', 'still queued', 1, 0, 'queued', ?)",
        (old_iso,),
    )

    # 2. Asks: old settled, recent settled, old open ('delivered'), and old in active run.
    h._conn.execute(
        "INSERT INTO asks (id, from_name, to_name, body, wait, status, collected, created_at, answered_at) "
        "VALUES ('a-old-done', 'lead', 'rd', 'q1', 0, 'answered', 1, ?, ?)",
        (old_iso, old_iso),
    )
    h._conn.execute(
        "INSERT INTO asks (id, from_name, to_name, body, wait, status, collected, created_at, answered_at) "
        "VALUES ('a-rec-done', 'lead', 'rd', 'q2', 0, 'answered', 1, ?, ?)",
        (recent_iso, recent_iso),
    )
    h._conn.execute(
        "INSERT INTO asks (id, from_name, to_name, body, wait, status, collected, created_at, delivered_at) "
        "VALUES ('a-open', 'lead', 'rd', 'q3', 0, 'delivered', 0, ?, ?)",
        (old_iso, old_iso),
    )
    h._conn.execute("INSERT INTO runs (session, run) VALUES ('lead', 'run-active')")
    h._conn.execute(
        "INSERT INTO asks (id, from_name, to_name, body, wait, status, collected, created_at, answered_at, run) "
        "VALUES ('a-in-run', 'lead', 'rd', 'q4', 0, 'answered', 1, ?, ?, 'run-active')",
        (old_iso, old_iso),
    )

    # 3. Commands & Replies: old done+sent, recent done+sent, old queued+unsent,
    #    and an old 'taken' (in-flight but a week gone -> fire-and-forget, pruned).
    h._conn.execute(
        "INSERT INTO commands (id, op, session, status, created_at) VALUES ('c-old-done', 'answer', 'rd', 'done', ?)",
        (old_iso,),
    )
    h._conn.execute(
        "INSERT INTO commands (id, op, session, status, created_at) VALUES ('c-old-taken', 'answer', 'rd', 'taken', ?)",
        (old_iso,),
    )
    h._conn.execute("INSERT INTO replies (id, chat_id, text, sent) VALUES ('c-old-done', 1, 'ok', 1)")
    h._conn.execute("INSERT INTO replies (id, chat_id, text, sent) VALUES ('c-old-unsent', 1, 'wait', 0)")
    h._conn.execute(
        "INSERT INTO commands (id, op, session, status, created_at) VALUES ('c-rec-done', 'answer', 'rd', 'done', ?)",
        (recent_iso,),
    )
    h._conn.execute("INSERT INTO replies (id, chat_id, text, sent) VALUES ('c-rec-done', 1, 'rec', 1)")
    h._conn.execute(
        "INSERT INTO commands (id, op, session, status, created_at) VALUES ('c-old-queued', 'answer', 'rd', 'queued', ?)",
        (old_iso,),
    )

    # 4. Pushes: old sent, recent sent, old unsent.
    h._conn.execute("INSERT INTO pushes (id, push, created_at, sent) VALUES (1, '{}', ?, 1)", (old_iso,))
    h._conn.execute("INSERT INTO pushes (id, push, created_at, sent) VALUES (2, '{}', ?, 1)", (recent_iso,))
    h._conn.execute("INSERT INTO pushes (id, push, created_at, sent) VALUES (3, '{}', ?, 0)", (old_iso,))
    # The first tick above already pruned (nothing old then); clear its mark so
    # this one prunes the rows just inserted, rather than being throttled.
    h._conn.execute("DELETE FROM meta WHERE k = 'pruned'")
    h._conn.commit()

    h.tick(sessions=snap)

    mail_ids = {r[0] for r in h._conn.execute("SELECT id FROM mail").fetchall()}
    assert mail_ids == {"m-rec-del", "m-old-queued"}

    ask_ids = {r[0] for r in h._conn.execute("SELECT id FROM asks").fetchall()}
    assert ask_ids == {"a-rec-done", "a-open", "a-in-run"}
    assert [a["id"] for a in h.recent_asks()["asks"]] == ["a-rec-done", "a-open"]

    cmd_ids = {r[0] for r in h._conn.execute("SELECT id FROM commands").fetchall()}
    assert cmd_ids == {"c-rec-done", "c-old-queued"}

    reply_ids = {r[0] for r in h._conn.execute("SELECT id FROM replies").fetchall()}
    assert reply_ids == {"c-old-unsent", "c-rec-done"}

    push_ids = {r[0] for r in h._conn.execute("SELECT id FROM pushes").fetchall()}
    assert push_ids == {2, 3}
    reset_herd()


def test_prune_is_throttled_to_once_per_interval(tmp_path: Path):
    """tick prunes at most once per PRUNE_EVERY, not on every sub-second tick:
    a row that comes due between ticks waits until the interval has passed."""
    from datetime import datetime, timedelta, timezone
    import herd as herd_mod

    h = _store(tmp_path)
    now = datetime.now(timezone.utc)
    old_iso = (now - timedelta(seconds=herd_mod.HISTORY_SECONDS + 3600)).isoformat()
    snap = [{"name": "rd", "kind": "claude", "state": "idle", "project": "/p"}]
    h.tick(sessions=snap)  # first prune, marks 'pruned'

    h._conn.execute("INSERT INTO pushes (id, push, created_at, sent) VALUES (9, '{}', ?, 1)", (old_iso,))
    h._conn.commit()
    h.tick(sessions=snap)  # within PRUNE_EVERY -> throttled
    assert h._conn.execute("SELECT 1 FROM pushes WHERE id = 9").fetchone() is not None

    # Pretend the last prune was more than an interval ago: the next tick prunes.
    h._conn.execute("UPDATE meta SET v = ? WHERE k = 'pruned'",
                    (str(now.timestamp() - herd_mod.PRUNE_EVERY - 1),))
    h._conn.commit()
    h.tick(sessions=snap)
    assert h._conn.execute("SELECT 1 FROM pushes WHERE id = 9").fetchone() is None
    reset_herd()


