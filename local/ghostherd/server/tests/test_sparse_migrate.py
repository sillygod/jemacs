"""The sparse half of the session index: CJK words, IDF, the claude memory
files indexed once, and the migration of an index built before."""

import json
import os
from pathlib import Path

import pytest
from qdrant_client.models import Modifier, SparseVectorParams

import importers.claude as claude_importer
import sparse_migrate
from config import Config
from engine import MemoryEngine, reset_engine
from importers.claude import iter_claude
from importers.textutil import project_from_claude_cwd
from lexical import segment
from store import COLLECTION


def test_cjk_runs_are_cut_into_pairs_apart_from_the_letters():
    assert segment("ghostherd的記憶升級", "bigram").split() == ["ghostherd", "的記", "記憶", "憶升", "升級"]
    assert segment("改 a 字", "bigram").split() == ["改", "a", "字"]
    assert segment("plain english_words", "bigram") == "plain english_words"
    assert segment("記憶升級", "none") == "記憶升級"


def _write(path: Path, lines: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(json.dumps(x) for x in lines) + "\n", encoding="utf-8")


def _turn(sid: str, cwd: str, text: str) -> dict:
    return {"type": "user", "sessionId": sid, "cwd": cwd,
            "message": {"role": "user", "content": text}}


def _linked_memory(tmp: Path) -> Path:
    """A claude projects dir as ghostherd leaves it: a project's memory dir
    and its worktree's linked to one shared store, which links the user's
    facts into every project; one project keeps claude's own memory dir."""
    claude, store = tmp / "claude", tmp / "store"
    for slug, cwd in (("-w-proj", "/w/proj"), ("-wt-proj", "/wt/proj"), ("-other", "/other"),
                      ("-own", "/own")):
        _write(claude / slug / f"s{slug}.jsonl",
               [_turn(f"s{slug}", cwd, f"a session in {cwd} about the overlay")])
    (store / "user").mkdir(parents=True)
    (store / "user" / "u.md").write_text("A user fact: prefer reasoned designs.\n", encoding="utf-8")
    for slug in ("-w-proj", "-other"):
        (store / slug).mkdir()
        (store / slug / "u.md").symlink_to(Path("..") / "user" / "u.md")
    (store / "-w-proj" / "fact.md").write_text("The sidecar must not block Emacs.\n", encoding="utf-8")
    (store / "-w-proj" / "MEMORY.md").write_text("# Memory\n- [Fact](fact.md) — the sidecar must not block\n", encoding="utf-8")
    (claude / "-w-proj" / "memory").symlink_to(store / "-w-proj")
    (claude / "-wt-proj" / "memory").symlink_to(store / "-w-proj")
    (claude / "-other" / "memory").symlink_to(store / "-other")
    (claude / "-own" / "memory").mkdir()
    (claude / "-own" / "memory" / "own.md").write_text("claude's own memory note\n", encoding="utf-8")
    return claude


def test_a_memory_file_linked_into_many_projects_is_indexed_once(tmp_path):
    claude = _linked_memory(tmp_path)
    store = os.path.realpath(tmp_path / "store")
    memory = {meta.path: turns[0].project
              for meta, turns in iter_claude(claude) if meta.kind == "memory"}
    assert memory == {
        f"{store}/-w-proj/MEMORY.md": "/w/proj",
        f"{store}/-w-proj/fact.md": "/w/proj",
        f"{store}/user/u.md": "",
        os.path.realpath(claude / "-own" / "memory" / "own.md"): "/own",
    }


def _engine(tmp: Path, claude: Path) -> MemoryEngine:
    reset_engine()
    return MemoryEngine(Config(data_dir=tmp / "mem", fake_embed=True, claude_root=claude,
                               grok_root=tmp / "g", agy_root=tmp / "a"))


def _old_index(tmp: Path, monkeypatch) -> MemoryEngine:
    """An index as the code before built it: Chinese unsegmented, no IDF,
    no mark, and every link to a memory file indexed as a file of its own."""
    claude = _linked_memory(tmp)
    _write(claude / "-w-proj" / "zh.jsonl", [_turn("zh", "/w/proj", f"第{i}段：我覺得是不是可以有個全域的總管，跟所有專案溝通")
                                              for i in range(5)])
    with monkeypatch.context() as m:
        m.setenv("GHOSTHERD_MEMORY_CJK", "none")
        m.setattr(claude_importer, "memory_source",
                  lambda path, projects: (path, project_from_claude_cwd(None, Path(path).parent.parent)))
        eng = _engine(tmp, claude)
        eng.import_transcripts(agents=["claude"])
        eng.store.client.update_collection(COLLECTION, sparse_vectors_config={"sparse": SparseVectorParams()})
        eng.store.mark("")
    return eng


def _points(eng: MemoryEngine) -> dict[str, dict]:
    out, offset = {}, None
    while True:
        recs, offset = eng.store.client.scroll(COLLECTION, limit=256, offset=offset,
                                               with_payload=True, with_vectors=True)
        for r in recs:
            out[str(r.id)] = {"payload": r.payload, "dense": r.vector["dense"],
                              "sparse": (list(r.vector["sparse"].indices), list(r.vector["sparse"].values))}
        if offset is None:
            return out


def test_the_migration_rebuilds_the_sparse_half_and_keeps_the_rest(tmp_path, monkeypatch):
    monkeypatch.setattr(sparse_migrate, "REST", 0)
    eng = _old_index(tmp_path, monkeypatch)
    try:
        assert sparse_migrate.needed(eng)
        # Before: the sentence is one token, which "全域的總管" is not.
        assert eng.store.channel("sparse", eng.embedder.sparse_query("全域的總管"), 5) == []
        before = _points(eng)
        aliases = [p for p in {v["payload"]["source_path"] for v in before.values()}
                   if "/claude/" in p and "/memory/" in p and not p.endswith("own.md")]
        assert len(aliases) == 7  # fact, MEMORY.md and u.md twice; u.md once more

        state = eng.migrate()
        after = _points(eng)

        assert state["step"] == "done" and not sparse_migrate.needed(eng)
        assert eng.store.sparse_state() == (Modifier.IDF, sparse_migrate.build())
        # Every transcript point: same id, payload and dense vector; the
        # sparse vector is the new one, from its own text.
        for pid, old in before.items():
            if old["payload"]["source_path"] in aliases:
                assert pid not in after
                continue
            new = after[pid]
            assert new["payload"] == old["payload"]
            assert new["dense"] == pytest.approx(old["dense"], abs=1e-6)
            assert new["sparse"] == eng.embedder.sparse([new["payload"]["text"]])[0]
        # The linked memory files: once each, under the real path, with
        # their text, dense vector and session.
        real = os.path.realpath(tmp_path / "store")
        moved = {v["payload"]["source_path"]: v for v in after.values()
                 if v["payload"]["source_path"].startswith(real)}
        assert set(moved) == {f"{real}/-w-proj/fact.md", f"{real}/-w-proj/MEMORY.md", f"{real}/user/u.md"}
        assert moved[f"{real}/user/u.md"]["payload"]["project"] == ""
        assert moved[f"{real}/-w-proj/fact.md"]["payload"]["project"] == "/w/proj"
        old_fact = next(v for v in before.values() if v["payload"]["source_path"].endswith("fact.md"))
        assert moved[f"{real}/-w-proj/fact.md"]["dense"] == pytest.approx(old_fact["dense"], abs=1e-6)
        assert len(after) == len(before) - 7 + 3
        assert state["removed_points"] == 4
        assert {v["payload"]["session_id"] for v in before.values()} == \
            {v["payload"]["session_id"] for v in after.values()}
        # The index lists each file once.
        paths = [s["source_path"] for s in eng.list_sources()["sources"]]
        assert not set(aliases) & set(paths) and len(paths) == len(set(paths))
        # A backup was taken first, of the index as it was.
        backup = Path(state["backup"])
        assert {p.name for p in backup.iterdir()} == {
            f"{COLLECTION}.storage.sqlite", "meta.json", "sources.sqlite"}
        # Chinese is found by its words now.
        hits = eng.store.channel("sparse", eng.embedder.sparse_query("全域的總管"), 5)
        assert hits and all(h["session_id"] == "zh" for h in hits)
        # Done once: a restart has nothing to do, and an import afterwards
        # neither re-embeds the moved files nor brings the links back.
        assert eng.migrate() is None
        result = eng.import_transcripts(agents=["claude"])
        assert result["imported"] == 0
        assert _points(eng).keys() == after.keys()
    finally:
        eng.close()
        reset_engine()


class Killed(Exception):
    "The sidecar dying mid-way."


def test_a_migration_killed_half_way_takes_up_where_it_was(tmp_path, monkeypatch):
    monkeypatch.setattr(sparse_migrate, "REST", 0)
    monkeypatch.setattr(sparse_migrate, "BATCH", 2)
    eng = _old_index(tmp_path, monkeypatch)
    calls = []
    real = eng.store.set_sparse

    def dies_on_the_third(vectors):
        calls.append(len(vectors))
        if len(calls) == 3:
            raise Killed()
        real(vectors)

    try:
        n = eng.store.point_count()
        monkeypatch.setattr(eng.store, "set_sparse", dies_on_the_third)
        with pytest.raises(Killed):
            eng.migrate()
        state = json.loads((eng.cfg.data_dir / sparse_migrate.STATE).read_text())
        assert state["step"] == "sparse" and state["done"] == 4 and state["cursor"]
        assert sparse_migrate.needed(eng)
    finally:
        eng.close()
        reset_engine()
    # The sidecar starts again.
    eng = _engine(tmp_path, tmp_path / "claude")
    try:
        state = eng.migrate()
        assert state["step"] == "done"
        assert state["done"] == n - 4  # the points after the fold, each once
        for v in _points(eng).values():
            assert v["sparse"] == eng.embedder.sparse([v["payload"]["text"]])[0]
        # One backup: the restart did not take another.
        assert len(list((eng.cfg.data_dir / "backups").iterdir())) == 1
    finally:
        eng.close()
        reset_engine()


def test_a_migration_without_room_for_its_backup_changes_nothing(tmp_path, monkeypatch):
    eng = _old_index(tmp_path, monkeypatch)
    try:
        before = _points(eng)
        monkeypatch.setattr(sparse_migrate.shutil, "disk_usage",
                            lambda p: type("U", (), {"free": 1024})())
        with pytest.raises(sparse_migrate.MigrationError):
            eng.migrate()
        assert _points(eng) == before
        # Failed: not tried again at every start, and said in the status.
        assert not sparse_migrate.needed(eng)
        assert eng.status()["migration"]["step"] == "failed"
        assert eng.search("overlay", limit=3)["hits"]
    finally:
        eng.close()
        reset_engine()


def test_a_new_index_is_of_this_build(tmp_path):
    claude = _linked_memory(tmp_path)
    eng = _engine(tmp_path, claude)
    try:
        eng.import_transcripts(agents=["claude"])
        assert eng.store.sparse_state() == (Modifier.IDF, sparse_migrate.build())
        assert not sparse_migrate.needed(eng) and eng.migrate() is None
    finally:
        eng.close()
        reset_engine()
