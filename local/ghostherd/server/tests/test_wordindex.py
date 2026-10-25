"""The BM25 half's own index (wordindex): it scores as Qdrant's sparse
search did, follows every write of the store, is checked and repaired
against the collection, rebuilt from the stored text, and asked only once
it holds this build -- Qdrant until then."""

from pathlib import Path

import pytest

import sparse_migrate
import wordindex
from chunk import Turn
from config import Config
from engine import MemoryEngine, reset_engine
from index import SourceMeta

TEXTS = [
    "the posframe overlay sizing bug in the sidebar",
    "overlay width is measured in columns, not pixels",
    "sidecar cpu burst during the transcript import",
    "全域的總管跟所有專案溝通，記憶升級成指示",
    "記憶升級：herd recall 找得到中文問句",
    "process-send-string blocks the caller until the socket takes the last byte",
    "a turn about nothing in particular, long enough to be a chunk",
    "the overlay again: posframe, sidebar, overlay",
]
QUERIES = ["overlay sidebar", "posframe", "cpu burst import", "記憶升級", "總管溝通",
           "process-send-string socket", "nothing matches xyzzy"]


@pytest.fixture
def eng(tmp_path):
    reset_engine()
    e = MemoryEngine(Config(data_dir=tmp_path / "mem", fake_embed=True, claude_root=tmp_path / "c",
                            grok_root=tmp_path / "g", agy_root=tmp_path / "a"))
    e._ensure_ready()
    for i, text in enumerate(TEXTS):
        _put(e, f"s{i}", "/w/proj" if i % 2 else "/w/other", [text, text + " again, a second turn"])
    yield e
    e.close()
    reset_engine()


def _put(e, sid, project, texts, agent="claude"):
    path = f"/src/{sid}.jsonl"
    turns = [Turn(agent=agent, role="user", text=t, project=project, session_id=sid,
                  source_path=path) for t in texts]
    e._import_source(SourceMeta(path, 1.0, 1, agent, sid, project, "transcript"), turns, force=True)


def _qdrant(e, query, **kw):
    "The sparse half as Qdrant answers it (the word index set aside)."
    built, e.store.words_build = e.store.words_build, ""
    try:
        return e.store.channel("sparse", e.embedder.sparse_query(query), 30, **kw)
    finally:
        e.store.words_build = built


def _words(e, query, **kw):
    assert e.store.words_live()
    return e.store.channel("sparse", e.embedder.sparse_query(query), 30, **kw)


def _same(a, b):
    "The same hits, scored the same (order among equal scores aside)."
    assert sorted((h["id"], round(h["score"], 4)) for h in a) == \
        sorted((h["id"], round(h["score"], 4)) for h in b)
    assert [round(h["score"], 4) for h in a] == [round(h["score"], 4) for h in b]


def test_it_scores_as_qdrant_did(eng):
    assert eng.store.words.count() == eng.store.point_count() > len(TEXTS)
    for q in QUERIES:
        _same(_words(eng, q), _qdrant(eng, q))
        # In a project too; the IDF stays the whole index's.
        _same(_words(eng, q, projects=["/w/proj"]), _qdrant(eng, q, projects=["/w/proj"]))
        _same(_words(eng, q, agent="claude"), _qdrant(eng, q, agent="claude"))
    assert _words(eng, "xyzzy plugh") == []
    assert _words(eng, "overlay", projects=[]) == []


def test_a_scope_narrows_the_chunks_not_the_scores(eng):
    every = {h["id"]: h["score"] for h in _words(eng, "overlay sidebar")}
    mine = _words(eng, "overlay sidebar", projects=["/w/proj"])
    assert mine and all(h["project"] == "/w/proj" for h in mine)
    assert all(abs(every[h["id"]] - h["score"]) < 1e-9 for h in mine)
    hits = eng.search("overlay sidebar", limit=5, project="/w/proj")["hits"]
    assert hits and {h["project"] for h in hits} == {"/w/proj"}


def test_it_follows_the_stores_writes(eng):
    words = eng.store.words
    # A source written again, a chunk changed and one gone.
    _put(eng, "s0", "/w/other", ["the posframe overlay sizing bug, fixed now"])
    # A new source.
    _put(eng, "s9", "/w/proj", ["a brand new session about kubectl contexts"])
    assert words.count() == eng.store.point_count()
    assert wordindex.check(words, eng.store, eng.embedder) == {
        "checked": eng.store.point_count(), "repaired": 0, "removed": 0}
    assert [h["session_id"] for h in _words(eng, "kubectl contexts")] == ["s9"]
    assert not any(h["text"].endswith("second turn") and h["session_id"] == "s0"
                   for h in _words(eng, "posframe"))
    # A chunk that only says something new of itself (restate): its project.
    path = "/src/s9.jsonl"
    t = Turn(agent="claude", role="user", text="a brand new session about kubectl contexts",
             project="/w/moved", session_id="s9", source_path=path)
    eng.store.restate([t])
    assert [h["session_id"] for h in _words(eng, "kubectl", projects=["/w/moved"])] == ["s9"]
    assert wordindex.check(words, eng.store, eng.embedder)["repaired"] == 0
    # A source dropped.
    eng.store.delete_source(path)
    assert _words(eng, "kubectl contexts") == [] and words.count() == eng.store.point_count()


def test_check_repairs_what_differs(eng):
    words = eng.store.words
    pids = sorted(words.digests())
    words.remove(pids[:2])                                      # missing
    with words._mu, words._conn:                                # changed
        words._conn.execute("UPDATE docs SET digest = 'x' WHERE pid = ?", (pids[2],))
    words.add([("ghost-point", "/src/ghost.jsonl", "/w/proj", "claude", "ghost overlay",
                eng.embedder.sparse(["ghost overlay"])[0])])    # one the store has not
    report = wordindex.check(words, eng.store, eng.embedder)
    assert report == {"checked": eng.store.point_count(), "repaired": 3, "removed": 1}
    for q in QUERIES:
        _same(_words(eng, q), _qdrant(eng, q))


def test_a_rebuild_from_the_stored_text(eng):
    eng.store.words.clear()
    assert not eng.store.words_live()
    report = eng.index_words()
    assert report["built"] == eng.store.point_count() and eng.store.words_live()
    for q in QUERIES:
        _same(_words(eng, q), _qdrant(eng, q))
    # Built already: the next start checks it instead.
    report = eng.index_words()
    assert {k: report[k] for k in ("checked", "repaired", "removed")} == {
        "checked": eng.store.point_count(), "repaired": 0, "removed": 0}
    assert eng.status()["words"]["live"] is True


def test_until_it_is_built_search_asks_qdrant(eng, monkeypatch):
    eng.store.words.mark("")
    monkeypatch.setattr(eng.store.words, "search", lambda *a, **k: pytest.fail("asked the word index"))
    assert eng.search("overlay sidebar", limit=3)["hits"]
    # Nor is it kept in step meanwhile: the build takes it all from the store.
    _put(eng, "s20", "/w/proj", ["written while the index was being built"])
    assert eng.store.words.count() == eng.store.point_count() - 1
    monkeypatch.undo()
    eng.index_words()
    assert [h["session_id"] for h in _words(eng, "while the index was being built")][0] == "s20"


def test_a_migration_leaves_it_to_be_built_again(eng, monkeypatch):
    """The sparse vectors rewritten (sparse_migrate): the index holds the
    words of before, so it is set aside, and built after."""
    monkeypatch.setattr(sparse_migrate, "REST", 0)
    eng.store.mark("")
    assert sparse_migrate.needed(eng) and eng.store.words_live()
    eng.migrate()
    assert not eng.store.words_live()
    eng.index_words()
    assert eng.store.words_live()
    for q in QUERIES:
        _same(_words(eng, q), _qdrant(eng, q))


def test_a_new_collection_starts_with_a_whole_empty_index(tmp_path):
    reset_engine()
    e = MemoryEngine(Config(data_dir=tmp_path / "mem", fake_embed=True, claude_root=tmp_path / "c",
                            grok_root=tmp_path / "g", agy_root=tmp_path / "a"))
    try:
        e._ensure_ready()
        assert e.store.words_live() and e.store.words.count() == 0
        _put(e, "s1", "/w/p", ["the first chunk ever written"])
        assert e.store.words.count() == 1 and _words(e, "first chunk")
    finally:
        e.close()
        reset_engine()


def test_off_it_is_not_made(tmp_path):
    reset_engine()
    e = MemoryEngine(Config(data_dir=tmp_path / "mem", fake_embed=True, claude_root=tmp_path / "c",
                            grok_root=tmp_path / "g", agy_root=tmp_path / "a", words_index=False))
    try:
        e._ensure_ready()
        assert e.store.words is None and e.index_words() is None
        assert not Path(tmp_path / "mem" / "words.sqlite").exists()
        assert e.status()["words"] is None
    finally:
        e.close()
        reset_engine()


def test_a_project_search_through_it_finds_what_the_filter_does(eng, monkeypatch):
    """The dense half's wide ask (store.WIDE) reads the scope from the word
    index, not from payloads; the hits are the filtered query's."""
    import store as store_mod

    for i in range(40):
        _put(eng, f"bulk{i}", "/w/proj" if i % 3 else "/w/other",
             [f"overlay sidebar posframe note {i}", f"cpu burst import note {i}"])
    for q in QUERIES[:5]:
        for project in ("/w/proj", "/w/other"):
            monkeypatch.setattr(store_mod, "WIDE", 1000)
            wide = [h["id"] for h in eng.search(q, limit=6, project=project)["hits"]]
            monkeypatch.setattr(store_mod, "WIDE", 1)      # never enough: the filter
            filtered = [h["id"] for h in eng.search(q, limit=6, project=project)["hits"]]
            assert wide == filtered, (q, project)
    assert eng.store.words.in_scope([], None, ["/w/proj"]) == set()


def test_the_loaded_heap_is_out_of_the_collectors_way(eng):
    import gc

    assert gc.get_freeze_count() > 0
