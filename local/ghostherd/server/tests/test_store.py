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


def test_source_index_answers_from_any_thread(tmp_path):
    """JSON-RPC runs each method via asyncio.to_thread, so the index is
    opened on one worker thread and asked on another.  sqlite3 refuses
    that by default; memory_list (SPC a h v) failed whenever the pool
    handed it a different thread."""
    import threading

    from index import SourceIndex, SourceMeta

    idx = SourceIndex(tmp_path / "sources.sqlite")
    idx.record(SourceMeta("a.jsonl", 1.0, 10, "claude", "s", "/p", "chat"), 3)
    out, errors = [], []

    def ask():
        try:
            out.append(idx.list_sources())
            out.append(idx.counts_by_agent())
            idx.record(SourceMeta("b.jsonl", 2.0, 20, "grok", "t", "/p", "chat"), 1)
        except Exception as exc:  # noqa: BLE001 -- the failure is the point
            errors.append(exc)

    threads = [threading.Thread(target=ask) for _ in range(4)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    assert not errors, errors
    assert out[0][1] >= 1
    assert idx.list_sources()[1] == 2
    idx.close()


def test_reimport_embeds_only_what_changed(tmp_path, monkeypatch):
    """A session being worked in grows by a few turns between imports.
    Only the new chunks may be embedded again, or a five-minute import
    re-embeds every active session whole."""
    import json

    from config import Config
    from engine import MemoryEngine, reset_engine

    root = tmp_path / "claude" / "p"
    root.mkdir(parents=True)
    path = root / "s.jsonl"

    def write(n, edit_first=False):
        with path.open("w") as f:
            for i in range(n):
                text = f"turn {i}: a sentence about the retry loop number {i}"
                if edit_first and i == 0:
                    text = "turn 0 rewritten: the first turn says something else now"
                f.write(json.dumps({"type": "user", "sessionId": "s", "cwd": "/p",
                                    "message": {"role": "user", "content": text}}) + "\n")

    reset_engine()
    cfg = Config(data_dir=tmp_path / "mem", fake_embed=True, claude_root=tmp_path / "claude",
                 grok_root=tmp_path / "g", agy_root=tmp_path / "a")
    cfg.grok_root.mkdir()
    cfg.agy_root.mkdir()
    eng = MemoryEngine(cfg)
    embedded = []
    real_dense = eng.embedder.dense

    def counting(texts):
        embedded.append(len(texts))
        return real_dense(texts)

    monkeypatch.setattr(eng.embedder, "dense", counting)
    try:
        write(10)
        assert eng.import_transcripts(agents=["claude"])["imported"] == 10
        write(13)  # three turns appended
        os_mtime_bump(path)
        assert eng.import_transcripts(agents=["claude"])["imported"] == 3
        assert embedded[-1] == 3
        write(13, edit_first=True)  # an earlier turn changed
        os_mtime_bump(path)
        assert eng.import_transcripts(agents=["claude"])["imported"] == 1
        write(5)  # cut short: the tail goes
        os_mtime_bump(path)
        assert eng.import_transcripts(agents=["claude"])["imported"] == 1  # turn 0 is back to the original
        assert eng.store.point_count() == 5
        assert eng.import_transcripts(agents=["claude"], force=True)["imported"] == 5
    finally:
        eng.close()
        reset_engine()


def os_mtime_bump(path):
    """Give PATH a new mtime even within the same clock tick."""
    import os

    st = os.stat(path)
    os.utime(path, (st.st_atime, st.st_mtime + 5))


def test_a_file_read_by_an_older_importer_is_read_again(tmp_path):
    """An importer that reads a transcript differently says so by its
    VERSION; a file it read before, though unchanged, is read again.  An
    index from before versions has them all as the first."""
    import sqlite3
    from dataclasses import replace

    from index import SourceIndex, SourceMeta

    db = tmp_path / "sources.sqlite"
    conn = sqlite3.connect(db)
    conn.execute(
        "CREATE TABLE sources (source_path TEXT PRIMARY KEY, mtime REAL NOT NULL, size INTEGER NOT NULL,"
        " agent TEXT, session_id TEXT, project TEXT, kind TEXT, chunks INTEGER, imported_at TEXT)"
    )
    conn.execute("INSERT INTO sources VALUES ('a.jsonl', 1.0, 10, 'claude', 's', '/p', 'transcript', 3, 'x')")
    conn.commit()
    conn.close()

    idx = SourceIndex(db)
    first = SourceMeta("a.jsonl", 1.0, 10, "claude", "s", "/p", "transcript")
    second = replace(first, parser=2)
    assert idx.unchanged(first)
    assert not idx.unchanged(second)
    idx.record(second, 2)
    assert idx.unchanged(second) and not idx.unchanged(first)
    assert idx.list_sources()[0][0]["chunks"] == 2
    assert SourceIndex(db).unchanged(second)
