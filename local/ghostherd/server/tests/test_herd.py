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
    resp = client.post("/jsonrpc", json=body)
    assert resp.status_code == 200
    data = resp.json()
    assert data.get("error") is None, data
    return data["result"]


def test_herd_rpc_round_trip(tmp_path: Path, monkeypatch):
    reset_engine()
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_FAKE_EMBED", "1")
    with TestClient(app) as client:
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


def test_alias_is_stable(tmp_path: Path):
    h = _store(tmp_path)
    h.tick(sessions=[{"name": "agy-ghost-commit", "state": "idle"}], ack_ids=[])
    listed = h.list_sessions()["sessions"][0]
    short = listed["short"]
    assert len(short) == 8
    assert h.name_for(short) == "agy-ghost-commit"
    assert h.short_for("agy-ghost-commit") == short
    reset_herd()
