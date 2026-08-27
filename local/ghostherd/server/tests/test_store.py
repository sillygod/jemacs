from pathlib import Path

from chunk import Turn
from config import Config
from engine import MemoryEngine, reset_engine
from store import rrf_merge


class _Hit:
    def __init__(self, pid, score=0.0):
        self.id = pid
        self.score = score
        self.payload = {"text": pid}


def test_rrf_prefers_items_high_in_both_lists():
    dense = [_Hit("a"), _Hit("b"), _Hit("c")]
    sparse = [_Hit("c"), _Hit("a"), _Hit("d")]
    merged = rrf_merge(dense, sparse, limit=3)
    ids = [str(h.id) for h in merged]
    assert ids[0] == "a"
    assert "d" in ids or "c" in ids


def test_import_busy_does_not_stack(tmp_path: Path):
    reset_engine()
    cfg = Config(
        data_dir=tmp_path / "mem",
        fake_embed=True,
        claude_root=tmp_path / "claude",
        grok_root=tmp_path / "grok",
        agy_root=tmp_path / "agy",
    )
    (cfg.claude_root).mkdir()
    (cfg.grok_root).mkdir()
    (cfg.agy_root).mkdir()
    engine = MemoryEngine(cfg)
    try:
        engine._import_lock.acquire()
        result = engine.import_transcripts(agents=["claude"])
        assert result.get("busy") is True
    finally:
        if engine._import_lock.locked():
            engine._import_lock.release()
        engine.close()
        reset_engine()


def test_import_skips_tool_role_by_default(tmp_path: Path):
    reset_engine()
    cfg = Config(
        data_dir=tmp_path / "mem",
        fake_embed=True,
        claude_root=tmp_path / "c",
        grok_root=tmp_path / "g",
        agy_root=tmp_path / "a",
        index_tools=False,
    )
    engine = MemoryEngine(cfg)
    (tmp_path / "x.jsonl").write_text("x", encoding="utf-8")
    from index import SourceMeta

    st = (tmp_path / "x.jsonl").stat()
    meta = SourceMeta(
        path=str(tmp_path / "x.jsonl"),
        mtime=st.st_mtime,
        size=st.st_size,
        agent="grok",
        session_id="s",
        project="/tmp/p",
        kind="transcript",
    )
    turns = [
        Turn(
            agent="grok",
            role="user",
            text="how does the posframe overlay size work in ghostherd",
            source_path=meta.path,
        ),
        Turn(
            agent="grok",
            role="tool",
            text="this tool dump should not be indexed " + ("x" * 80),
            source_path=meta.path,
        ),
    ]
    try:
        engine._ensure_ready()
        n, skipped = engine._import_source(meta, turns, force=True)
        assert not skipped
        assert n >= 1
        hits = engine.search("tool dump should not")["hits"]
        assert not any("tool dump" in h["text"] for h in hits)
        hits = engine.search("posframe overlay")["hits"]
        assert any("posframe" in h["text"] for h in hits)
    finally:
        engine.close()
        reset_engine()


def test_import_and_search_roundtrip(tmp_path: Path):
    reset_engine()
    cfg = Config(
        data_dir=tmp_path / "mem",
        fake_embed=True,
        claude_root=tmp_path / "claude",
        grok_root=tmp_path / "grok",
        agy_root=tmp_path / "agy",
    )
    (cfg.claude_root).mkdir()
    engine = MemoryEngine(cfg)
    try:
        turns = [
            Turn(
                agent="claude",
                role="user",
                text="the sidebar should become a posframe overlay",
                project="/tmp/ghostherd",
                session_id="s1",
                source_path=str(tmp_path / "a.jsonl"),
            ),
            Turn(
                agent="grok",
                role="assistant",
                text="qdrant local path with fastembed multilingual",
                project="/tmp/other",
                session_id="s2",
                source_path=str(tmp_path / "b.jsonl"),
            ),
        ]
        from index import SourceMeta

        (tmp_path / "a.jsonl").write_text("x", encoding="utf-8")
        meta = SourceMeta(
            path=str(tmp_path / "a.jsonl"),
            mtime=(tmp_path / "a.jsonl").stat().st_mtime,
            size=(tmp_path / "a.jsonl").stat().st_size,
            agent="claude",
            session_id="s1",
            project="/tmp/ghostherd",
            kind="transcript",
        )
        engine._ensure_ready()
        n, skipped = engine._import_source(meta, [turns[0]], force=False)
        assert n >= 1 and not skipped
        n2, skipped2 = engine._import_source(meta, [turns[0]], force=False)
        assert skipped2 and n2 == 0

        hits = engine.search("posframe overlay", limit=5)["hits"]
        assert hits
        assert any("posframe" in h["text"] for h in hits)
        assert all(h["agent"] == "claude" for h in engine.search(
            "posframe overlay", limit=5, agent="claude"
        )["hits"])
    finally:
        engine.close()
        reset_engine()
