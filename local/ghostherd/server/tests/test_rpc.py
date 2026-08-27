import json
from pathlib import Path

from fastapi.testclient import TestClient

from config import Config
from engine import MemoryEngine, reset_engine
from main import app


def _rpc(client, method, params=None, id=1):
    body = {"jsonrpc": "2.0", "id": id, "method": method}
    if params is not None:
        body["params"] = params
    resp = client.post("/jsonrpc", json=body)
    assert resp.status_code == 200
    data = resp.json()
    assert data.get("error") is None, data
    return data["result"]


def test_health_and_ping(tmp_path: Path, monkeypatch):
    reset_engine()
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_FAKE_EMBED", "1")
    with TestClient(app) as client:
        health = client.get("/health").json()
        assert health["status"] == "ok"
        assert _rpc(client, "ping")["ok"] is True
        status = _rpc(client, "memory_status")
        assert status["fake_embed"] is True
    reset_engine()


def test_import_search_via_rpc(tmp_path: Path, monkeypatch):
    reset_engine()
    claude = tmp_path / "claude" / "proj"
    claude.mkdir(parents=True)
    (claude / "s.jsonl").write_text(
        json.dumps(
            {
                "type": "user",
                "sessionId": "s",
                "cwd": "/tmp/ghostherd",
                "message": {
                    "role": "user",
                    "content": "why did agy-ghost-commit look blocked",
                },
            }
        )
        + "\n",
        encoding="utf-8",
    )
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_FAKE_EMBED", "1")
    monkeypatch.setenv("GHOSTHERD_MEMORY_CLAUDE_ROOT", str(tmp_path / "claude"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_GROK_ROOT", str(tmp_path / "empty-g"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_AGY_ROOT", str(tmp_path / "empty-a"))
    (tmp_path / "empty-g").mkdir()
    (tmp_path / "empty-a").mkdir()
    reset_engine()
    with TestClient(app) as client:
        imported = _rpc(client, "memory_import", {"agents": ["claude"]})
        assert imported["imported"] >= 1
        hits = _rpc(
            client,
            "memory_search",
            {"query": "agy-ghost-commit blocked", "limit": 5},
        )["hits"]
        assert hits
        assert any("ghost-commit" in h["text"] for h in hits)
        listed = _rpc(client, "memory_list", {"agents": ["claude"]})
        assert listed["total"] >= 1
        assert any("s.jsonl" in (s.get("source_path") or "") for s in listed["sources"])
        path = listed["sources"][0]["source_path"]
        body = _rpc(client, "memory_chunks", {"source_path": path, "limit": 20})
        assert body["total"] >= 1
        assert any("ghost-commit" in (c.get("text") or "") for c in body["chunks"])
        assert body["chunks"][0]["chunk_index"] <= body["chunks"][-1]["chunk_index"]
        missing = client.post(
            "/jsonrpc",
            json={"jsonrpc": "2.0", "id": 9, "method": "nope"},
        ).json()
        assert missing["error"]["code"] == -32601
    reset_engine()


def test_engine_uses_injected_roots(tmp_path: Path):
    """Direct engine (no env) still imports from Config roots."""
    reset_engine()
    claude = tmp_path / "claude"
    claude.mkdir()
    (claude / "x.jsonl").write_text(
        json.dumps(
            {
                "type": "user",
                "cwd": "/tmp/p",
                "message": {"role": "user", "content": "shared memory sidecar for ghostherd"},
            }
        )
        + "\n",
        encoding="utf-8",
    )
    cfg = Config(
        data_dir=tmp_path / "mem",
        fake_embed=True,
        claude_root=claude,
        grok_root=tmp_path / "g",
        agy_root=tmp_path / "a",
    )
    (cfg.grok_root).mkdir()
    (cfg.agy_root).mkdir()
    eng = MemoryEngine(cfg)
    try:
        result = eng.import_transcripts(agents=["claude"])
        assert result["imported"] >= 1
        hits = eng.search("memory sidecar")["hits"]
        assert hits
    finally:
        eng.close()
        reset_engine()
