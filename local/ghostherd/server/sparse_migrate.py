"""Rebuild the session index's sparse (BM25) half in place, once.

The sparse half used to count a Chinese sentence as one word and weigh a
word in every chunk like a name in one (no IDF).  What it is now: the CJK
cut into words (lexical), IDF on (store.SPARSE).  The index also held each
claude memory file once per link to it (importers.claude.memory_source).

The index outlives the CLIs' own deletes -- claude drops a transcript after
30 days -- so for old sessions it holds the only copy, and this never
re-imports.  Every point keeps its text, payload and dense vector; only its
sparse vector is computed again, from its stored text.  The memory files
indexed under several paths keep one copy, moved to the file's real path
(text and dense vector kept), and lose the rest.

Safe to stop at any point -- a sidecar killed half-way takes up where it was:
- First, a backup: the collection's points and meta.json and sources.sqlite,
  copied with SQLite's backup API under the store's lock, to
  data_dir/backups/<build>-<time>/.  Not started without the disk for it.
- Progress lives in data_dir/sparse-migration.json: the step, the counts
  taken before, the plan for the duplicates, and while re-weighing the next
  point (points come in id order).  Every step does the same again if
  repeated: the plan is fixed before it runs, a vector computed twice is the
  same vector.
- At the end the counts are checked: points after = points before - the
  duplicates' points + their copies under real paths, every other source
  with its points, every session still there.  Only then is the build
  marked in the collection itself (Store.mark): a store restored from the
  backup has the old mark, so it is rebuilt again.
The import waits meanwhile (it says busy), and search answers throughout.

Rollback: stop the sidecar, copy the backup's files back --
  ghostherd_turns.storage.sqlite -> qdrant/collection/ghostherd_turns/storage.sqlite
  meta.json                      -> qdrant/meta.json
  sources.sqlite                 -> sources.sqlite
-- and start the sidecar of the code before this change.  (This code would
rebuild it again.)"""

from __future__ import annotations

import json
import os
import shutil
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable

import onnx_telemetry  # noqa: F401 -- before qdrant_client loads onnxruntime
from qdrant_client.models import PointStruct

from importers.claude import memory_source
from index import SourceMeta
from lexical import segmenter
from store import point_id, sparse_vector

STATE = "sparse-migration.json"
# Points re-weighed per hold of the store's lock: a search waits at most
# one batch (~0.3 s on the 84k-chunk store).
BATCH = 128
# Rest after each batch for this share of the time it took: one core at
# ~2/3 rather than flat out, and the sidecar's other threads get the GIL.
REST = 0.5


def build() -> str:
    "The sparse half this code makes; the mark a migrated collection carries."
    return f"bm25-idf-{segmenter()}-1"


class MigrationError(RuntimeError):
    pass


def needed(engine) -> bool:
    """Whether ENGINE's index is not of this build -- and not one that
    failed to migrate: that waits for a person (see `progress')."""
    store = engine.store
    if not store.collection_exists() or store.sparse_state()[1] == build():
        return False
    return progress(engine).get("step") != "failed"


def progress(engine) -> dict[str, Any]:
    "The migration's state file for this build, {} if none."
    try:
        state = json.loads((engine.cfg.data_dir / STATE).read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    return state if state.get("build") == build() else {}


def run(engine, progress: Callable[[dict], None] | None = None,
        rest: float | None = None, batch: int | None = None) -> dict[str, Any]:
    """Migrate ENGINE's index; the final state.  The caller holds the
    engine's import lock (MemoryEngine.migrate)."""
    m = _Migration(engine, progress or (lambda _: None),
                   REST if rest is None else rest, BATCH if batch is None else batch)
    return m.run()


class _Migration:
    def __init__(self, engine, progress, rest: float, batch: int):
        self.engine = engine
        self.store = engine.store
        self.index = engine.index
        self.embedder = engine.embedder
        self.cfg = engine.cfg
        self.path = self.cfg.data_dir / STATE
        self.progress = progress
        self.rest = rest
        self.batch = batch
        self.state = self._load()

    # -- state ---------------------------------------------------------------

    def _load(self) -> dict[str, Any]:
        try:
            state = json.loads(self.path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            state = {}
        # Run only when `needed': a "done" here is a store restored from its
        # backup, to do again.
        if state.get("build") != build() or state.get("step") in ("done", "failed"):
            state = {"build": build(), "step": "backup", "started": _now()}
        return state

    def _save(self, **changes: Any) -> None:
        self.state.update(changes)
        tmp = self.path.with_suffix(".tmp")
        tmp.write_text(json.dumps(self.state, indent=1), encoding="utf-8")
        os.replace(tmp, self.path)

    def _say(self, msg: str) -> None:
        print(f"[ghostherd-memory] index migration: {msg}", flush=True)

    # -- the steps -----------------------------------------------------------

    def run(self) -> dict[str, Any]:
        t0 = time.monotonic()
        steps = {"backup": self._backup, "fold": self._fold, "idf": self._idf,
                 "sparse": self._sparse, "verify": self._verify}
        try:
            while self.state["step"] in steps:
                steps[self.state["step"]]()
        except MigrationError as exc:
            self._save(step="failed", reason=str(exc))
            self._say(f"FAILED: {exc} -- nothing marked; search goes on as it was")
            raise
        self._save(seconds=round(self.state.get("seconds", 0) + time.monotonic() - t0, 1))
        return self.state

    def _backup(self) -> None:
        files = [p for p in self.store.storage_files() if p.exists()]
        need = sum(p.stat().st_size for p in files) + self.cfg.index_path.stat().st_size
        root = self.cfg.data_dir / "backups"
        root.mkdir(exist_ok=True)
        free = shutil.disk_usage(root).free
        if free < need * 1.5:
            raise MigrationError(f"{need >> 20} MB to back up, {free >> 20} MB free")
        dest = root / f"{build()}-{datetime.now().strftime('%Y%m%d-%H%M%S')}"
        self._say(f"backing up {need >> 20} MB to {dest}")
        self.progress({"current": "backing up the index", "migrating": True})
        self.store.backup(dest)
        self.index.backup(dest / "sources.sqlite")
        # Counted after the copy and before any change: nothing else writes
        # the collection meanwhile (the import waits on our lock).
        self._save(step="fold", backup=str(dest), before=self._census())

    def _census(self) -> dict[str, Any]:
        """Points per source and the sessions, read off every point."""
        sources: dict[str, int] = {}
        sessions: set[str] = set()
        offset, total = None, 0
        while True:
            recs, offset = self.store.page(offset, 2048, ["source_path", "session_id"])
            for r in recs:
                p = r.payload or {}
                sp = p.get("source_path") or ""
                sources[sp] = sources.get(sp, 0) + 1
                sessions.add(p.get("session_id") or "")
            total += len(recs)
            if offset is None:
                return {"points": total, "sources": sources, "sessions": sorted(sessions)}

    def _plan(self) -> list[dict[str, Any]]:
        """The claude memory files indexed under more than one path: each
        group's real path, its project, the paths to drop and the one whose
        points move to the real path (none if the real path is indexed)."""
        rows = self.index.rows()
        root = str(self.cfg.claude_root).rstrip("/") + "/"
        projects: dict[str, str] = {}
        for r in rows:
            if r.agent == "claude" and r.kind == "transcript" and r.project \
                    and r.path.startswith(root):
                projects.setdefault(r.path[len(root):].split("/", 1)[0], r.project)
        indexed = set(self.state["before"]["sources"])
        groups: dict[str, dict[str, Any]] = {}
        for r in rows:
            if r.agent != "claude" or r.kind != "memory":
                continue
            real, project = memory_source(r.path, projects)
            if real == r.path:
                continue
            g = groups.setdefault(real, {"real": real, "project": project, "drop": []})
            g["drop"].append(r.path)
        plan = []
        for real, g in sorted(groups.items()):
            g["drop"].sort()
            g["keep"] = None if real in indexed else g["drop"][0]
            plan.append(g)
        return plan

    def _fold(self) -> None:
        if "plan" not in self.state:
            self._save(plan=self._plan())
        plan = self.state["plan"]
        self.progress({"current": f"folding {len(plan)} duplicated memory files",
                       "migrating": True})
        rows = {r.path: r for r in self.index.rows()}
        for g in plan:
            keep = g["keep"]
            # The real path's row is recorded once its points are all in:
            # with it there, this group was moved before a restart.
            if keep and keep in rows and g["real"] not in rows:
                self._move(rows[keep], g["real"], g["project"])
            for path in g["drop"]:
                self.store.delete_source(path)
            self.index.forget(g["drop"])
        self._say(f"folded {sum(len(g['drop']) for g in plan)} paths into {len(plan)} files")
        self._save(step="idf")

    def _move(self, row: SourceMeta, real: str, project: str) -> None:
        "ROW's points, written again under REAL: same text, same dense vector."
        points = self.store.points_of(row.path)
        texts = [(p.payload or {}).get("text") or "" for p in points]
        sparse = self.embedder.sparse(texts)
        moved = []
        for p, svec in zip(points, sparse):
            payload = dict(p.payload or {}, source_path=real, project=project)
            moved.append(PointStruct(
                id=point_id(real, int(payload.get("chunk_index") or 0)),
                vector={"dense": p.vector["dense"], "sparse": sparse_vector(svec)},
                payload=payload,
            ))
        self.store.put(moved)
        self.index.record(SourceMeta(path=real, mtime=row.mtime, size=row.size,
                                     agent=row.agent, session_id=row.session_id,
                                     project=project, kind=row.kind, title=row.title,
                                     parser=row.parser), len(moved))

    def _idf(self) -> None:
        self.store.weigh_by_idf()
        self._save(step="sparse", cursor=None, done=0, total=self.store.point_count())

    def _sparse(self) -> None:
        self.embedder.load_sparse()
        total = self.state["total"]
        cursor, done = self.state.get("cursor"), int(self.state.get("done") or 0)
        self._say(f"re-weighing {total} chunks' words from {done}")
        while True:
            t = time.monotonic()
            with self.store.client_lock:
                recs, nxt = self.store.page(cursor, self.batch, ["text"])
                vecs = self.embedder.sparse([(r.payload or {}).get("text") or "" for r in recs])
                self.store.set_sparse({r.id: v for r, v in zip(recs, vecs)})
            done += len(recs)
            cursor = nxt
            self._save(cursor=cursor, done=done)
            self.progress({"current": f"re-indexing words: {done} of {total}", "migrating": True,
                           "done": done, "total": total})
            if cursor is None:
                break
            if self.rest:
                time.sleep((time.monotonic() - t) * self.rest)
        self._save(step="verify")

    def _verify(self) -> None:
        before = self.state["before"]
        after = self._census()
        plan = self.state["plan"]
        dropped = {p for g in plan for p in g["drop"]}
        moved = {g["real"]: before["sources"].get(g["keep"], 0) for g in plan if g["keep"]}
        want = dict(before["sources"])
        for p in dropped:
            want.pop(p, None)
        want.update(moved)
        expect = before["points"] - sum(before["sources"].get(p, 0) for p in dropped) \
            + sum(moved.values())
        wrong = {p: (want.get(p), after["sources"].get(p))
                 for p in set(want) | set(after["sources"])
                 if want.get(p) != after["sources"].get(p)}
        lost = sorted(set(before["sessions"]) - set(after["sessions"]))
        if after["points"] != expect or wrong or lost:
            raise MigrationError(
                f"counts off: {after['points']} points, {expect} expected; "
                f"{len(wrong)} sources differ (e.g. {sorted(wrong.items())[:3]}); "
                f"{len(lost)} sessions lost (e.g. {lost[:3]})")
        self.store.mark(build())
        self._say(f"done: {before['points']} points before, {after['points']} after "
                  f"({len(dropped)} duplicate paths, {sum(moved.values())} points moved)")
        self._save(step="done", finished=_now(), after={"points": after["points"],
                                                        "sessions": len(after["sessions"])},
                   removed_points=before["points"] - after["points"])


def _now() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="seconds")
