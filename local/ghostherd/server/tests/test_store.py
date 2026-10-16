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


def test_reimport_embeds_only_new_words(tmp_path, monkeypatch):
    """agy gives every step of a conversation the conversation's time,
    which moves with each new step: a chunk whose words are as stored
    keeps its vectors, and only what it says of itself -- a time, a
    title -- is written again.  Its 1,577 chunks were embedded whole
    every five minutes, ten threads at once."""
    import json
    import sqlite3

    from config import Config
    from engine import MemoryEngine, reset_engine

    agy = tmp_path / "agy"
    (agy / "conversations").mkdir(parents=True)
    db = agy / "conversations" / "conv-1.db"
    summaries = agy / "conversation_summaries.db"
    conn = sqlite3.connect(db)
    conn.execute("CREATE TABLE steps (idx INTEGER, step_payload BLOB)")
    conn.commit()
    conn.close()
    conn = sqlite3.connect(summaries)
    conn.execute("CREATE TABLE conversation_summaries (conversation_id TEXT, title TEXT, preview TEXT,"
                 " workspace_uris TEXT, last_modified_time TEXT)")
    conn.execute("INSERT INTO conversation_summaries VALUES (?,?,?,?,?)",
                 ("conv-1", "commit code", "commit it once the tests of the shop pass", json.dumps(["file:///tmp/shop"]), "2026-10-11T10:00"))
    conn.commit()
    conn.close()

    def step(n, at, title="commit code"):
        with sqlite3.connect(db) as c:
            c.executemany("INSERT INTO steps VALUES (?, ?)",
                          [(i, f"step {i}: ran the tests of the shop and read what failed".encode()) for i in n])
        with sqlite3.connect(summaries) as c:
            c.execute("UPDATE conversation_summaries SET last_modified_time = ?, title = ?", (at, title))
        os_mtime_bump(db)
        os_mtime_bump(summaries)

    reset_engine()
    cfg = Config(data_dir=tmp_path / "mem", fake_embed=True, claude_root=tmp_path / "c",
                 grok_root=tmp_path / "g", agy_root=agy)
    eng = MemoryEngine(cfg)
    embedded = []
    for name in ("dense", "sparse"):
        real = getattr(eng.embedder, name)
        monkeypatch.setattr(eng.embedder, name, lambda texts, real=real, name=name:
                            embedded.append((name, len(list(texts)))) or real(texts))
    try:
        step(range(10), "2026-10-11T10:05")
        assert eng.import_transcripts(agents=["agy"])["imported"] == 11  # ten steps, and the summary
        embedded.clear()
        step([10], "2026-10-11T10:10")
        assert eng.import_transcripts(agents=["agy"])["imported"] == 1
        assert embedded == [("dense", 1), ("sparse", 1)]
        stored = eng.store.payloads_of(str(db))
        assert len(stored) == 11 and {p["ts"] for p in stored.values()} == {"2026-10-11T10:10"}
        assert eng.store.payloads_of(str(summaries))[0]["ts"] == "2026-10-11T10:10"
        # Renamed: the summary's words change; the steps only say it.
        embedded.clear()
        step([], "2026-10-11T10:15", title="commit the shop")
        assert eng.import_transcripts(agents=["agy"])["imported"] == 1
        assert embedded == [("dense", 1), ("sparse", 1)]
        assert {p["title"] for p in eng.store.payloads_of(str(db)).values()} == {"commit the shop"}
        hits = eng.search("ran the tests of the shop", limit=3)["hits"]
        assert hits and all(h.get("ts") == "2026-10-11T10:15" for h in hits)
    finally:
        eng.close()
        reset_engine()


def test_embedding_takes_a_few_cores(monkeypatch):
    """Four threads at most unless configured, on the CPU unless CoreML
    is asked for: ten threads and CoreML were slower and dearer."""
    import os

    from embedder import _onnx_providers, _thread_count

    for cores, want in ((11, 4), (4, 3), (1, 1)):
        monkeypatch.setattr(os, "cpu_count", lambda cores=cores: cores)
        assert _thread_count(0) == want
    assert _thread_count(8) == 8
    assert _onnx_providers("auto") is None and _onnx_providers("cpu") is None
    assert _onnx_providers("coreml") == (["CoreMLExecutionProvider", "CPUExecutionProvider"]
                                         if __import__("sys").platform == "darwin" else None)


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


def test_agy_switching_to_transcript_prunes_old_db_without_reembed_loop(tmp_path, monkeypatch):
    """When an agy conversation previously imported from conversations/<id>.db
    now has brain/<id>/.system_generated/logs/transcript.jsonl, the old .db
    chunks must be deleted from Qdrant and retired from sources.sqlite, and
    later imports must not re-process the .db or re-embed unchanged turns."""
    import json
    import sqlite3

    from config import Config
    from engine import MemoryEngine, reset_engine

    agy = tmp_path / "agy"
    (agy / "conversations").mkdir(parents=True)
    db = agy / "conversations" / "conv-1.db"
    conn = sqlite3.connect(db)
    conn.execute("CREATE TABLE steps (idx INTEGER, step_payload BLOB)")
    conn.executemany(
        "INSERT INTO steps VALUES (?, ?)",
        [(i, f"legacy db step {i}: old salvaged protobuf words".encode()) for i in range(3)],
    )
    conn.commit()
    conn.close()

    reset_engine()
    cfg = Config(
        data_dir=tmp_path / "mem",
        fake_embed=True,
        claude_root=tmp_path / "c",
        grok_root=tmp_path / "g",
        agy_root=agy,
    )
    eng = MemoryEngine(cfg)
    embedded = []
    real_dense = eng.embedder.dense
    monkeypatch.setattr(
        eng.embedder,
        "dense",
        lambda texts: embedded.append(len(list(texts))) or real_dense(texts),
    )
    try:
        # 1. Initial import when only conversations/conv-1.db exists.
        assert eng.import_transcripts(agents=["agy"])["imported"] == 3
        assert len(eng.store.payloads_of(str(db))) == 3

        # 2. Now brain/conv-1/.system_generated/logs/transcript.jsonl appears.
        tx = agy / "brain" / "conv-1" / ".system_generated" / "logs" / "transcript.jsonl"
        tx.parent.mkdir(parents=True)

        def write_tx(n):
            lines = []
            for i in range(n):
                lines.append(
                    json.dumps(
                        {
                            "step_index": i,
                            "type": "USER_INPUT" if i % 2 == 0 else "PLANNER_RESPONSE",
                            "source": "USER_EXPLICIT" if i % 2 == 0 else "MODEL",
                            "created_at": f"2026-10-16T12:00:0{i}Z",
                            "content": f"structured transcript turn {i} with enough words to index",
                        }
                    )
                )
            tx.write_text("\n".join(lines) + "\n", encoding="utf-8")

        write_tx(2)
        embedded.clear()
        res = eng.import_transcripts(agents=["agy"])
        assert res["imported"] == 2
        assert embedded == [2]
        # Old .db chunks are completely gone from Qdrant and hidden in sources.sqlite.
        assert eng.store.payloads_of(str(db)) == {}
        assert len(eng.store.payloads_of(str(tx))) == 2
        listed_paths = [s["source_path"] for s in eng.list_sources(agent="agy")["sources"]]
        assert listed_paths == [str(tx)]

        # 3. Even if the .db file's mtime changes on disk, subsequent imports skip it.
        os_mtime_bump(db)
        embedded.clear()
        res2 = eng.import_transcripts(agents=["agy"])
        assert res2["imported"] == 0
        assert embedded == []

        # 4. Appending one turn to transcript.jsonl embeds only the new turn.
        write_tx(3)
        os_mtime_bump(tx)
        embedded.clear()
        res3 = eng.import_transcripts(agents=["agy"])
        assert res3["imported"] == 1
        assert embedded == [1]
        assert eng.store.point_count() == 3
    finally:
        eng.close()
        reset_engine()

