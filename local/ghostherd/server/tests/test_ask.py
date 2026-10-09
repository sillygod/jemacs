"""Asks: mail that wants an answer, and the `herd' client that makes them."""

import json
import os
import socket
import subprocess
import sys
import threading
import time
from pathlib import Path

import pytest
import uvicorn
from fastapi.testclient import TestClient

import herd as herd_mod
from engine import reset_engine
from herd import HerdStore, reset_herd
from main import app

HERD = Path(__file__).resolve().parents[2] / "bin" / "herd"

SNAP = [
    {"name": "claude-main", "kind": "claude", "state": "idle", "project": "/src/a/"},
    {"name": "agy-a", "kind": "agy", "state": "working", "project": "/src/a/"},
    {"name": "agy-a2", "kind": "agy", "state": "idle", "project": "/src/a/"},
    {"name": "agy-b", "kind": "agy", "state": "idle", "project": "/src/b/"},
    {"name": "grok-old", "kind": "grok", "state": "dead", "project": "/src/a/"},
]


def _store(tmp_path: Path) -> HerdStore:
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=SNAP)
    return h


def _snap(**states):
    return [dict(r, state=states.get(r["name"], r["state"])) for r in SNAP]


def _deliver(h, snap=None):
    "One tick that pastes, and the next that acks: what Emacs does."
    out = h.tick(sessions=snap or SNAP)
    h.tick(sessions=snap or SNAP, ack_ids=[p["id"] for p in out["pending"]])
    return out["pending"]


# ---- Who takes it ------------------------------------------------------


def test_kind_picks_a_free_agent_in_the_askers_project(tmp_path):
    h = _store(tmp_path)
    out = h.ask("agy", "fetch the docs", from_name="claude-main")
    assert out["to"] == "agy-a2"  # idle beats working; agy-b is elsewhere


def test_kind_takes_a_busy_one_rather_than_another_project(tmp_path):
    h = _store(tmp_path)
    h.tick(sessions=_snap(**{"agy-a2": "dead"}))
    assert h.ask("agy", "x", from_name="claude-main")["to"] == "agy-a"


def test_kind_with_none_here_names_the_others(tmp_path):
    h = _store(tmp_path)
    h.tick(sessions=[r for r in SNAP if r["name"] not in ("agy-a", "agy-a2")])
    with pytest.raises(ValueError, match="no agy agent in /src/a/.*agy-b"):
        h.ask("agy", "x", from_name="claude-main")


def test_kind_from_outside_the_herd_must_be_unambiguous(tmp_path):
    h = _store(tmp_path)
    with pytest.raises(ValueError, match="several projects"):
        h.ask("agy", "x", from_name="user")
    assert h.ask("claude", "x", from_name="user")["to"] == "claude-main"


def test_refusals(tmp_path):
    h = _store(tmp_path)
    with pytest.raises(ValueError, match="cannot ask itself"):
        h.ask("claude-main", "x", from_name="claude-main")
    with pytest.raises(ValueError, match="no agent or kind named nope"):
        h.ask("nope", "x", from_name="claude-main")
    with pytest.raises(ValueError, match="grok-old is dead"):
        h.ask("grok-old", "x", from_name="claude-main")
    with pytest.raises(ValueError, match="body is required"):
        h.ask("agy", "  ", from_name="claude-main")


def test_no_ask_before_emacs_has_synced(tmp_path):
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    with pytest.raises(ValueError, match="not shared the herd"):
        h.ask("agy", "x", from_name="claude-main")


# ---- Delivery ----------------------------------------------------------


def test_an_ask_is_delivered_as_mail_with_its_id(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main")
    pending = h.tick(sessions=SNAP)["pending"]
    assert [(p["to"], p["ask"], p["body"]) for p in pending] == [("agy-a2", ask["id"], "fetch")]
    assert h.get_ask(ask["id"])["status"] == "queued"
    h.tick(sessions=SNAP, ack_ids=[pending[0]["id"]])
    assert h.get_ask(ask["id"])["status"] == "delivered"


def test_plain_mail_has_no_ask(tmp_path):
    h = _store(tmp_path)
    h.enqueue(to="agy-a2", body="hi", from_name="claude-main")
    assert h.tick(sessions=SNAP)["pending"][0]["ask"] is None


def test_one_open_ask_per_agent(tmp_path):
    h = _store(tmp_path)
    first = h.ask("agy-a2", "one", from_name="claude-main")
    second = h.ask("agy-a2", "two", from_name="user")
    # Both queued: only the first goes out in a tick...
    assert [p["ask"] for p in _deliver(h)] == [first["id"]]
    # ...and the second waits while the first is open, idle or not.
    assert h.tick(sessions=SNAP)["pending"] == []
    h.reply(first["id"], "done")
    pending = h.tick(sessions=SNAP)["pending"]
    assert [p["ask"] for p in pending if p["to"] == "agy-a2"] == [second["id"]]


def test_an_agent_answering_an_ask_may_not_ask(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "one", from_name="claude-main")
    _deliver(h)
    with pytest.raises(ValueError, match=f"agy-a2 is answering ask {ask['id']}"):
        h.ask("claude-main", "back at you", from_name="agy-a2")
    h.reply(ask["id"], "done")
    assert h.ask("claude-main", "now fine", from_name="agy-a2")["to"] == "claude-main"


def test_a_taker_that_leaves_fails_the_ask(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "one", from_name="claude-main")
    h.tick(sessions=[r for r in SNAP if r["name"] != "agy-a2"])
    got = h.get_ask(ask["id"])
    assert (got["status"], got["error"]) == ("failed", "agy-a2 is gone")


# ---- Answers -----------------------------------------------------------


def test_reply_closes_the_ask(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main", wait=True)
    _deliver(h)
    h.reply(ask["id"], "the docs say X", from_name="agy-a2")
    got = h.get_ask(ask["id"])
    assert (got["status"], got["reply"], got["auto"], got["replied_by"]) == (
        "answered", "the docs say X", False, "agy-a2")
    with pytest.raises(ValueError, match="already answered"):
        h.reply(ask["id"], "again")


def test_reply_before_delivery_withdraws_the_paste(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main", wait=True)
    h.reply(ask["id"], "already knew")
    assert h.tick(sessions=SNAP)["pending"] == []


def test_settle_answers_with_the_screen_unless_replied(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main", wait=True)
    _deliver(h)
    assert h.settle(ask["id"], screen="  the page says Y\n")["status"] == "answered"
    got = h.get_ask(ask["id"])
    assert (got["reply"], got["auto"]) == ("the page says Y", True)
    # A real answer after the fact replaces the screen.
    h.reply(ask["id"], "properly: Y")
    got = h.get_ask(ask["id"])
    assert (got["reply"], got["auto"]) == ("properly: Y", False)
    # And settling a replied ask changes nothing.
    h.settle(ask["id"], screen="noise")
    assert h.get_ask(ask["id"])["reply"] == "properly: Y"


def test_settle_dead_fails(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main", wait=True)
    _deliver(h)
    h.settle(ask["id"], dead=True)
    got = h.get_ask(ask["id"])
    assert (got["status"], got["error"]) == ("failed", "agy-a2 exited before answering")
    with pytest.raises(ValueError, match="is failed"):
        h.reply(ask["id"], "late")


def test_settle_before_delivery_is_a_no_op(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main")
    assert h.settle(ask["id"], screen="x")["status"] == "queued"


def test_cancel_withdraws_a_queued_ask(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main")
    assert h.cancel(ask["id"])["status"] == "cancelled"
    assert h.tick(sessions=SNAP)["pending"] == []
    with pytest.raises(ValueError, match="is cancelled: cancelled by the asker"):
        h.reply(ask["id"], "too late")


# ---- The answer finds its way back --------------------------------------


def _mail_to(h, name):
    return [p for p in h.tick(sessions=SNAP)["pending"] if p["to"] == name]


def test_an_asker_not_waiting_gets_the_answer_as_mail(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch the docs\nand more", from_name="claude-main")
    _deliver(h)
    h.reply(ask["id"], "X")
    mail = _mail_to(h, "claude-main")
    assert len(mail) == 1
    assert mail[0]["from"] == "agy-a2" and mail[0]["ask"] is None
    assert mail[0]["body"] == f"Answer to ask {ask['id']} (fetch the docs):\nX"


def test_an_auto_answer_says_so_and_a_failure_is_mailed_too(tmp_path):
    h = _store(tmp_path)
    one = h.ask("agy-a2", "fetch", from_name="claude-main")
    _deliver(h)
    h.settle(one["id"], screen="screen text")
    body = _mail_to(h, "claude-main")[0]["body"]
    assert "no answer was sent; this is its screen" in body and body.endswith("screen text")
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in _mail_to(h, "claude-main")])
    two = h.ask("agy-a2", "again", from_name="claude-main")
    _deliver(h)
    h.settle(two["id"], dead=True)
    assert _mail_to(h, "claude-main")[0]["body"] == (
        f"Ask {two['id']} failed: agy-a2 exited before answering")


def test_a_waiting_asker_gets_no_mail(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main", wait=True)
    _deliver(h)
    h.reply(ask["id"], "X")
    h.collect(ask["id"])
    assert _mail_to(h, "claude-main") == []


def test_an_answer_nobody_collected_goes_out_as_mail(tmp_path, monkeypatch):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main", wait=True)
    _deliver(h)
    h.reply(ask["id"], "X")
    assert _mail_to(h, "claude-main") == []
    monkeypatch.setattr(herd_mod, "UNCOLLECTED_SECONDS", -1)
    assert len(_mail_to(h, "claude-main")) == 1


def test_detach_mails_an_answer_already_there(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch", from_name="claude-main", wait=True)
    _deliver(h)
    h.reply(ask["id"], "X")
    h.detach(ask["id"])
    assert len(_mail_to(h, "claude-main")) == 1
    h.detach(ask["id"])  # once only
    assert len(_mail_to(h, "claude-main")) == 1


def test_tick_reports_recent_asks(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "fetch the docs", from_name="claude-main")
    asks = h.tick(sessions=SNAP)["asks"]
    assert [(a["id"], a["from"], a["to"], a["status"], a["head"]) for a in asks] == [
        (ask["id"], "claude-main", "agy-a2", "queued", "fetch the docs")]
    assert abs(asks[0]["created_ms"] - time.time() * 1000) < 60_000
    assert asks[0]["answered_ms"] is None


# ---- Over RPC ----------------------------------------------------------


def _rpc(client, method, params=None):
    resp = client.post(
        "/jsonrpc/" + client.app.state.token,
        json={"jsonrpc": "2.0", "id": 1, "method": method, "params": params or {}},
    )
    assert resp.status_code == 200
    return resp.json()


@pytest.fixture
def client(tmp_path, monkeypatch):
    reset_engine()
    reset_herd()
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    with TestClient(app, base_url="http://127.0.0.1") as c:
        _rpc(c, "herd_tick", {"sessions": SNAP})
        yield c
    reset_herd()
    reset_engine()


def test_await_returns_open_then_answered(client):
    ask = _rpc(client, "herd_ask", {"to": "agy", "body": "x", "from": "claude-main", "wait": True})["result"]
    t0 = time.monotonic()
    open_ = _rpc(client, "herd_await", {"id": ask["id"], "timeout": 0.6})["result"]
    assert open_["status"] == "queued" and open_["taker_state"] == "idle"
    assert time.monotonic() - t0 >= 0.5
    _rpc(client, "herd_reply", {"id": ask["id"], "body": "Y"})
    done = _rpc(client, "herd_await", {"id": ask["id"], "timeout": 30})["result"]
    assert (done["status"], done["reply"]) == ("answered", "Y")


def test_rpc_errors_are_invalid_params(client):
    err = _rpc(client, "herd_ask", {"to": "nope", "body": "x", "from": "claude-main"})["error"]
    assert err["code"] == -32602 and "no agent or kind named nope" in err["message"]
    err = _rpc(client, "herd_await", {"id": "deadbeef"})["error"]
    assert err["code"] == -32602 and "no ask deadbeef" in err["message"]


# ---- The client, against a live sidecar ---------------------------------


@pytest.fixture
def live(tmp_path, monkeypatch):
    reset_engine()
    reset_herd()
    data = tmp_path / "mem"
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(data))
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        port = s.getsockname()[1]
    server = uvicorn.Server(
        uvicorn.Config(app, host="127.0.0.1", port=port, log_level="warning")
    )
    thread = threading.Thread(target=server.run, daemon=True)
    thread.start()
    for _ in range(100):
        if server.started:
            break
        time.sleep(0.05)
    assert server.started
    token = (data / "rpc.token").read_text().strip()
    url = f"http://127.0.0.1:{port}/jsonrpc/{token}"
    env = {
        "PATH": os.environ.get("PATH", ""),
        "HOME": os.environ.get("HOME", ""),
        "GHOSTHERD_RPC": url,
        "GHOSTHERD_SESSION": "claude-main",
    }
    herd_mod.get_herd().tick(sessions=SNAP)
    yield {"url": url, "env": env, "data": data, "port": port}
    server.should_exit = True
    thread.join(5)
    reset_herd()
    reset_engine()


def run(live, *args, stdin=None, session=None, env=None, python=None, timeout=20):
    e = dict(live["env"])
    if session:
        e["GHOSTHERD_SESSION"] = session
    e.update(env or {})
    cmd = ([python] if python else []) + [str(HERD), *args]
    return subprocess.run(
        cmd, input=stdin, capture_output=True, text=True, env=e, timeout=timeout
    )


def test_client_ask_and_reply_round_trip(live):
    out = run(live, "ask", "agy", "fetch the docs")
    assert out.returncode == 0, out.stderr
    ask_id = out.stdout.strip()
    assert "asked agy-a2 (ask " + ask_id + ")" in out.stderr
    # Delivered, then answered from the other side, text on stdin.
    _deliver(herd_mod.get_herd())
    out = run(live, "reply", ask_id, stdin="line 1\n'quoted' \"too\"\n", session="agy-a2")
    assert out.returncode == 0, out.stderr
    got = json.loads(run(live, "status", ask_id).stdout)
    assert (got["status"], got["reply"]) == ("answered", "line 1\n'quoted' \"too\"\n")


def test_client_wait_prints_the_answer(live):
    h = herd_mod.get_herd()
    proc = subprocess.Popen(
        [str(HERD), "ask", "agy", "-", "--wait"],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        text=True, env=live["env"],
    )
    proc.stdin.write("fetch\n")
    proc.stdin.close()
    proc.stdin = None  # closed: communicate must not flush it
    ask = None
    for _ in range(100):
        asks = h.recent_asks()["asks"]
        if asks:
            ask = asks[0]
            break
        time.sleep(0.05)
    assert ask
    _deliver(h)
    h.settle(ask["id"], screen="what the page said")
    out, err = proc.communicate(timeout=20)
    assert proc.returncode == 0, err
    assert out == "what the page said\n"
    assert "agy-a2 stopped without answering" in err
    # Collected: nothing mailed back on top.
    assert _mail_to(h, "claude-main") == []


def test_client_takes_the_text_after_the_options(live):
    # `ask qa --wait -' is the order an agent reaches for first; argparse
    # used to leave the text over as unrecognized.
    out = run(live, "ask", "agy", "--wait", "--timeout", "1", "-", stdin="from stdin\n")
    assert out.returncode == 3, out.stderr
    out = run(live, "ask", "agy", "--timeout", "1", "--wait", "as an argument")
    assert out.returncode == 3, out.stderr
    heads = [a["head"] for a in herd_mod.get_herd().recent_asks()["asks"]]
    assert {"from stdin", "as an argument"} <= set(heads)
    # Still refused: a second text, an unknown option.
    for args in (("ask", "agy", "one", "two"), ("ask", "agy", "--nope", "x"), ("reply", "1a2b3c4d", "x", "y")):
        out = run(live, *args)
        assert out.returncode == 2 and "unrecognized arguments" in out.stderr, args


def test_client_wait_timeout_detaches(live):
    out = run(live, "ask", "agy", "slow one", "--wait", "--timeout", "1")
    assert out.returncode == 3, out.stderr
    assert "will come to you as a message" in out.stderr
    h = herd_mod.get_herd()
    ask_id = h.recent_asks()["asks"][0]["id"]
    _deliver(h)
    h.reply(ask_id, "late but here")
    assert _mail_to(h, "claude-main")[0]["body"].endswith("late but here")


def test_client_failure_exit_codes(live):
    out = run(live, "ask", "nope", "x")
    assert out.returncode == 1 and "no agent or kind named nope" in out.stderr
    ask_id = run(live, "ask", "agy", "x").stdout.strip()
    run(live, "cancel", ask_id)
    out = run(live, "reply", ask_id, "x", session="agy-a2")
    assert out.returncode == 1 and "cancelled" in out.stderr


def test_client_falls_back_to_the_url_file(live, tmp_path):
    stale = live["url"].rsplit("/", 1)[0] + "/wrong-token"
    url_file = tmp_path / "rpc.url"
    url_file.write_text(live["url"])
    out = run(live, "list", env={"GHOSTHERD_RPC": stale, "GHOSTHERD_RPC_FILE": str(url_file)})
    assert out.returncode == 0, out.stderr
    assert json.loads(out.stdout)["sessions"]
    # Without the file: an error that names host:port, never the token.
    out = run(live, "list", env={"GHOSTHERD_RPC": stale})
    assert out.returncode == 1
    assert f"127.0.0.1:{live['port']}" in out.stderr
    token = live["url"].rsplit("/", 1)[1]
    assert token not in out.stderr and "wrong-token" not in out.stderr


def test_client_without_any_url(live):
    out = run(live, "list", env={"GHOSTHERD_RPC": ""})
    assert out.returncode == 1 and "GHOSTHERD_RPC is unset" in out.stderr


@pytest.mark.skipif(not Path("/usr/bin/python3").exists(), reason="no system python3")
def test_client_runs_on_the_system_python(live):
    out = run(live, "list", python="/usr/bin/python3")
    assert out.returncode == 0, out.stderr


def test_waiting_askers_do_not_hold_worker_threads(live):
    """Forty askers waiting must not stall a quick call behind them.

    Sync methods run on a small thread pool; an await held there for
    its whole wait would use it up.
    """
    import urllib.request

    h = herd_mod.get_herd()
    ask_id = h.ask("agy-a2", "x", from_name="claude-main", wait=True)["id"]

    def call(method, params, timeout):
        req = urllib.request.Request(
            live["url"],
            data=json.dumps({"jsonrpc": "2.0", "id": 1, "method": method, "params": params}).encode(),
            headers={"Content-Type": "application/json"},
        )
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.loads(resp.read())

    # Warm: the first urlopen in a process is slow on its own (proxy
    # lookup), and that is this test's cost, not the sidecar's.
    call("herd_list", {}, 10)
    waiters = [
        threading.Thread(target=call, args=("herd_await", {"id": ask_id, "timeout": 4}, 10))
        for _ in range(40)
    ]
    for t in waiters:
        t.start()
    time.sleep(1.0)
    t0 = time.monotonic()
    assert call("herd_list", {}, 10)["result"]["sessions"]
    took = time.monotonic() - t0
    for t in waiters:
        t.join(10)
    assert took < 1.0, f"herd_list waited {took:.1f}s behind the askers"
