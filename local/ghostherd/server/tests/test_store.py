import json
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


def test_agy_rows_indexed_with_an_encoded_path_are_fixed_once(tmp_path, monkeypatch):
    """An index made before agy's workspace URIs were decoded says
    ".../Open%20Design" for those sessions, in its rows and in its chunks.
    The next import reads each agy source again (its parser version is
    newer) and writes the real path, embedding nothing -- the words are
    the same; the one after that skips them all."""
    import json
    import sqlite3
    from urllib.parse import urlparse

    import importers.agy as agy_mod
    from config import Config
    from engine import MemoryEngine, reset_engine

    enc = "file:///Users/u/Library/Application%20Support/Open%20Design/p1"
    real = "/Users/u/Library/Application Support/Open Design/p1"
    agy = tmp_path / "agy"
    logs = agy / "brain" / "conv-od" / ".system_generated" / "logs"
    logs.mkdir(parents=True)
    tx = logs / "transcript.jsonl"
    tx.write_text("".join(json.dumps({
        "type": "USER_INPUT", "source": "USER_EXPLICIT", "created_at": f"2026-10-20T10:0{i}:00Z",
        "content": f"<USER_REQUEST>draw the {i}th screen of the design</USER_REQUEST>"}) + "\n"
        for i in range(3)))
    conn = sqlite3.connect(agy / "conversation_summaries.db")
    conn.execute("CREATE TABLE conversation_summaries (conversation_id TEXT, title TEXT,"
                 " preview TEXT, workspace_uris TEXT, last_modified_time TEXT)")
    conn.execute("INSERT INTO conversation_summaries VALUES (?,?,?,?,?)",
                 ("conv-od", "a design", "draw the first screens of the open design, its colours and layout",
                  json.dumps([enc]), "2026-10-20"))
    conn.commit()
    conn.close()

    reset_engine()
    cfg = Config(data_dir=tmp_path / "mem", fake_embed=True, claude_root=tmp_path / "c",
                 grok_root=tmp_path / "g", agy_root=agy)
    eng = MemoryEngine(cfg)
    try:
        # As the importer before: version 2, the URI's path not decoded.
        with monkeypatch.context() as m:
            m.setattr(agy_mod, "VERSION", 2)
            m.setattr(agy_mod, "project_from_workspace_uris",
                      lambda raw: urlparse(json.loads(raw)[0]).path)
            eng.import_transcripts(agents=["agy"])
        old = {s["source_path"]: s["project"] for s in eng.list_sources(agent="agy")["sources"]}
        assert old[str(tx)] == urlparse(enc).path
        assert {p["project"] for p in eng.store.payloads_of(str(tx)).values()} == {urlparse(enc).path}

        embedded = []
        for name in ("dense", "sparse"):
            real_fn = getattr(eng.embedder, name)
            monkeypatch.setattr(eng.embedder, name, lambda texts, real_fn=real_fn, name=name:
                                embedded.append(name) or real_fn(texts))
        assert eng.import_transcripts(agents=["agy"])["imported"] == 0
        assert embedded == []
        rows = {s["source_path"]: s for s in eng.list_sources(agent="agy")["sources"]}
        assert rows[str(tx)]["project"] == real
        assert {p["project"] for p in eng.store.payloads_of(str(tx)).values()} == {real}
        assert {p["project"] for p in eng.store.payloads_of(str(agy / "conversation_summaries.db")).values()} == {real}
        assert eng.search("draw the screen of the design", project=real)["hits"]
        embedded.clear()  # the search's own query
        # Once is enough.
        again = eng.import_transcripts(agents=["agy"])
        assert (again["imported"], again["skipped"]) == (0, again["files"])
        assert embedded == []
    finally:
        eng.close()
        reset_engine()


def test_embedding_takes_a_few_cores(monkeypatch):
    """Two threads at most unless configured, on the CPU unless CoreML is
    asked for: past two bge-small-zh barely speeds up for twice the burst
    (ask 2b76d37d), and ten threads and CoreML were slower and dearer."""
    import os

    from embedder import _onnx_providers, _thread_count

    for cores, want in ((11, 2), (4, 2), (2, 1), (1, 1)):
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



def test_a_session_title_is_kept_and_an_older_row_learns_it(tmp_path):
    """The index keeps the title an importer read -- claude's ai-title, which
    comes some turns in -- so a picker of past conversations can name them.
    A row recorded before titles were kept has none (NULL): listing with
    `titles' finds it in the store by point id and keeps it, once."""
    import json

    from config import Config
    from engine import MemoryEngine, reset_engine

    claude = tmp_path / "claude" / "proj"
    claude.mkdir(parents=True)
    lines = [
        {"type": "user", "sessionId": "s1", "cwd": "/tmp/p",
         "message": {"role": "user", "content": f"turn {i} with enough words to make a chunk"}}
        for i in range(20)
    ]
    # The title, after twenty turns: none of the first sixteen chunks has it.
    lines.append({"type": "ai-title", "title": "Fix the login bug"})
    lines.append({"type": "user", "sessionId": "s1", "cwd": "/tmp/p",
                  "message": {"role": "user", "content": "the turn after the title, long enough"}})
    (claude / "s1.jsonl").write_text("\n".join(json.dumps(x) for x in lines) + "\n", encoding="utf-8")

    reset_engine()
    cfg = Config(data_dir=tmp_path / "mem", fake_embed=True, claude_root=tmp_path / "claude",
                 grok_root=tmp_path / "g", agy_root=tmp_path / "a")
    eng = MemoryEngine(cfg)
    try:
        eng.import_transcripts(agents=["claude"])
        (src,) = eng.list_sources()["sources"]
        assert src["title"] == "Fix the login bug"

        # As an index from before titles has it.
        with eng.index._mu:
            eng.index._conn.execute("UPDATE sources SET title = NULL")
            eng.index._conn.commit()
        assert eng.list_sources()["sources"][0]["title"] == ""
        looked = []
        real = eng.store.titles_of
        eng.store.titles_of = lambda paths, *a: looked.append(list(paths)) or real(paths, *a)
        assert eng.list_sources(titles=True)["sources"][0]["title"] == "Fix the login bug"
        # Kept: not looked up again, and there without `titles'.
        assert eng.list_sources(titles=True)["sources"][0]["title"] == "Fix the login bug"
        assert eng.list_sources()["sources"][0]["title"] == "Fix the login bug"
        assert looked == [[src["source_path"]]]
        # A path with no chunks reads "", and is kept as having none.
        assert real(["/nowhere.jsonl"]) == {"/nowhere.jsonl": ""}
    finally:
        eng.close()
        reset_engine()


def test_search_by_project_takes_its_subdirectories_like_the_list(tmp_path):
    """A session started in a project's subdirectory -- or recorded with a
    trailing slash, as grok writes its git root -- is that project's in
    the Sessions list (a path prefix) and must be in its search too."""
    claude = tmp_path / "claude" / "proj"
    claude.mkdir(parents=True)
    for sid, cwd in (("top", "/w/proj"), ("sub", "/w/proj/server"),
                     ("slash", "/w/proj/"), ("other", "/w/project-two")):
        line = {"type": "user", "sessionId": sid, "cwd": cwd,
                "message": {"role": "user",
                            "content": f"the posframe overlay sizing bug, session {sid}"}}
        (claude / f"{sid}.jsonl").write_text(json.dumps(line) + "\n", encoding="utf-8")
    reset_engine()
    cfg = Config(data_dir=tmp_path / "mem", fake_embed=True, claude_root=tmp_path / "claude",
                 grok_root=tmp_path / "g", agy_root=tmp_path / "a")
    eng = MemoryEngine(cfg)
    try:
        eng.import_transcripts(agents=["claude"])
        listed = {s["session_id"] for s in eng.list_sources(project="/w/proj")["sources"]}
        assert listed == {"top", "sub", "slash"}
        found = {h["session_id"] for h in
                 eng.search("posframe overlay sizing", limit=10, project="/w/proj")["hits"]}
        assert found == listed
        # A trailing slash on the asking side means the same project.
        assert {h["session_id"] for h in eng.search(
            "posframe overlay sizing", limit=10, project="/w/proj/")["hits"]} == listed
        # Nothing for a project with no sessions -- not every project's.
        assert eng.search("posframe overlay sizing", project="/w/none")["hits"] == []
    finally:
        eng.close()
        reset_engine()


def test_a_project_search_asks_wide_first_and_finds_the_same(tmp_path, monkeypatch):
    """A search in a project asks for the best of every project first and
    keeps the project's (store.WIDE) -- the filter is slow in embedded
    Qdrant -- and runs the filtered query when too few were its own: the
    same hits either way."""
    import store as store_mod
    from index import SourceMeta

    reset_engine()
    eng = MemoryEngine(Config(data_dir=tmp_path / "mem", fake_embed=True, claude_root=tmp_path / "c",
                              grok_root=tmp_path / "g", agy_root=tmp_path / "a"))
    try:
        eng._ensure_ready()
        words = ["overlay", "sizing", "posframe", "deploy", "branch", "commit", "sidecar", "embed"]
        for p, project, n in (("big", "/w/big", 60), ("small", "/w/small", 3)):
            turns = [Turn(agent="claude", role="user", project=project, session_id=f"{p}{i}",
                          source_path=f"/s/{p}{i}.jsonl",
                          text=f"{words[i % 8]} {words[(i * 3) % 8]} note {i} of {p}")
                     for i in range(n)]
            for t in turns:
                eng._import_source(SourceMeta(t.source_path, 1.0, 1, "claude", t.session_id, project,
                                              "transcript"), [t], force=True)
        calls = []
        real = eng.store._query
        monkeypatch.setattr(eng.store, "_query", lambda *a: calls.append(a[3] is None) or real(*a))

        def both(query, project, wide):
            monkeypatch.setattr(store_mod, "WIDE", wide)
            calls.clear()
            got = [h["id"] for h in eng.search(query, limit=4, project=project)["hits"]]
            return got, list(calls)

        for query in ("overlay sizing", "deploy branch commit", "sidecar"):
            for project in ("/w/big", "/w/small"):
                exact, _ = both(query, project, 1)          # wide never enough: filtered
                fast, asked = both(query, project, 1000)    # wide: the whole store
                assert fast == exact, (query, project)
                assert asked == [True, True]                # two unfiltered asks, no filter
        # The small project, with a wide ask that cannot hold enough of it:
        # filtered after all, and the sparse half goes straight to it.
        got, asked = both("overlay sizing", "/w/small", 20)
        assert got == both("overlay sizing", "/w/small", 1)[0]
        assert asked == [True, False, False]
    finally:
        eng.close()
        reset_engine()
