"""Import + search.  Embedder is loaded on first use, not at process start.

Uvicorn can answer /health before FastEmbed has downloaded a model;
Emacs waits on that health check the way ecloud does.
"""

from __future__ import annotations

import threading
from pathlib import Path
from typing import Any

from chunk import chunk_turns
from config import Config, get_config
from embedder import Embedder
from importers import IMPORTERS, ROOT_ATTR
from index import SourceIndex, SourceMeta
from store import Store

_lock = threading.Lock()
_engine: "MemoryEngine | None" = None


def _log(msg: str) -> None:
    print(f"[ghostherd-memory] {msg}", flush=True)


class MemoryEngine:
    def __init__(self, cfg: Config):
        self.cfg = cfg
        self.cfg.data_dir.mkdir(parents=True, exist_ok=True)
        self.embedder = Embedder(cfg)
        self.store = Store(cfg.qdrant_path)
        self.index = SourceIndex(cfg.index_path)
        self._ready = False
        self._import_lock = threading.Lock()
        self._import_progress: dict[str, Any] | None = None

    def close(self) -> None:
        self.index.close()
        self.store.close()

    def _ensure_ready(self, recreate: bool = False) -> None:
        self.embedder.load()
        self.store.ensure(self.embedder.dim, recreate=recreate)
        self._ready = True

    def status(self) -> dict[str, Any]:
        points = 0
        try:
            points = self.store.point_count()
        except Exception:
            points = 0
        return {
            "ready": True,
            "embed_loaded": self.embedder.loaded,
            "embed_model": self.cfg.embed_model,
            "fake_embed": self.cfg.fake_embed,
            "data_dir": str(self.cfg.data_dir),
            "collection": "ghostherd_turns",
            "points": points,
            "sources": self.index.counts_by_agent(),
            "import": dict(self._import_progress) if self._import_progress else None,
        }

    def search(
        self,
        query: str,
        limit: int = 8,
        agent: str | None = None,
        project: str | None = None,
    ) -> dict[str, Any]:
        query = (query or "").strip()
        if not query:
            return {"hits": [], "query": query}
        self._ensure_ready()
        dense = self.embedder.dense([query])[0]
        sparse = self.embedder.sparse([query])[0]
        hits = self.store.search(
            dense,
            sparse,
            limit=max(1, int(limit)),
            agent=agent or None,
            project=project or None,
        )
        return {"hits": hits, "query": query}

    def list_sources(
        self,
        agent: str | None = None,
        project: str | None = None,
        limit: int = 200,
        offset: int = 0,
    ) -> dict[str, Any]:
        sources, total = self.index.list_sources(
            agent=agent or None,
            project=project or None,
            limit=max(1, int(limit)),
            offset=max(0, int(offset)),
        )
        return {"sources": sources, "total": total}

    def list_chunks(
        self,
        source_path: str | None = None,
        session_id: str | None = None,
        limit: int = 400,
        offset: int = 0,
    ) -> dict[str, Any]:
        chunks, total = self.store.list_chunks(
            source_path=source_path or None,
            session_id=session_id or None,
            limit=max(1, int(limit)),
            offset=max(0, int(offset)),
        )
        return {
            "chunks": chunks,
            "total": total,
            "offset": max(0, int(offset)),
            "limit": max(1, int(limit)),
        }

    def import_transcripts(
        self,
        agents: list[str] | None = None,
        force: bool = False,
        project: str | None = None,
    ) -> dict[str, Any]:
        wanted = list(agents or IMPORTERS.keys())
        unknown = [a for a in wanted if a not in IMPORTERS]
        if unknown:
            raise ValueError(f"unknown agents: {unknown}")
        if not self._import_lock.acquire(blocking=False):
            prog = dict(self._import_progress or {})
            prog["busy"] = True
            return prog
        try:
            return self._import_transcripts(wanted, force, project)
        finally:
            self._import_progress = None
            self._import_lock.release()

    def _import_transcripts(
        self, wanted: list[str], force: bool, project: str | None
    ) -> dict[str, Any]:
        self._import_progress = {
            "running": True,
            "files": 0,
            "imported": 0,
            "skipped": 0,
            "current": "loading embedder",
        }
        _log("import start (first run loads FastEmbed; watch this buffer)")
        self.embedder.load()
        rebuilt = False
        try:
            self.store.ensure(self.embedder.dim, recreate=False)
        except ValueError:
            _log("embedding size changed; rebuilding the collection")
            self.store.ensure(self.embedder.dim, recreate=True)
            rebuilt = True
        force = force or rebuilt
        self._ready = True
        imported = 0
        skipped = 0
        sessions: set[str] = set()
        errors: list[str] = []
        files = 0
        for agent in wanted:
            root: Path = getattr(self.cfg, ROOT_ATTR[agent])
            _log(f"scanning {agent} under {root}")
            try:
                iterator = IMPORTERS[agent](
                    root,
                    tool_truncate=self.cfg.tool_truncate,
                    max_line=self.cfg.max_line_bytes,
                )
            except TypeError:
                iterator = IMPORTERS[agent](root)
            try:
                for meta, turns in iterator:
                    files += 1
                    self._import_progress = {
                        "running": True,
                        "files": files,
                        "imported": imported,
                        "skipped": skipped,
                        "current": meta.path,
                        "agent": agent,
                    }
                    if project and not _project_matches(meta.project, project):
                        if not any(_project_matches(t.project, project) for t in turns):
                            skipped += 1
                            continue
                        turns = [t for t in turns if _project_matches(t.project, project)]
                    try:
                        n, was_skip = self._import_source(meta, turns, force=force)
                    except Exception as exc:  # noqa: BLE001 — keep going across files
                        errors.append(f"{meta.path}: {exc}")
                        _log(f"error {meta.path}: {exc}")
                        continue
                    if was_skip:
                        skipped += 1
                    else:
                        imported += n
                        if meta.session_id:
                            sessions.add(meta.session_id)
                        _log(f"{agent} +{n} chunks  {Path(meta.path).name}")
            except Exception as exc:  # noqa: BLE001
                errors.append(f"{agent}: {exc}")
                _log(f"error {agent}: {exc}")
        _log(f"import done: {imported} chunks, {skipped} skipped, {len(errors)} errors")
        return {
            "imported": imported,
            "skipped": skipped,
            "sessions": len(sessions),
            "files": files,
            "errors": errors,
            "points": self.store.point_count(),
            "busy": False,
        }

    def _import_source(
        self, meta: SourceMeta, turns: list, force: bool
    ) -> tuple[int, bool]:
        if not force and self.index.unchanged(meta):
            return 0, True
        if not self.cfg.index_tools:
            turns = [t for t in turns if t.role != "tool"]
        chunks = chunk_turns(
            turns,
            max_chars=self.cfg.max_chars,
            min_chars=self.cfg.min_chars,
            overlap=self.cfg.overlap,
        )
        self.store.delete_source(meta.path)
        if not chunks:
            self.index.record(meta, 0)
            return 0, False
        texts = [c.text for c in chunks]
        dense = self.embedder.dense(texts)
        sparse = self.embedder.sparse(texts)
        n = self.store.upsert(chunks, dense, sparse)
        self.index.record(meta, n)
        return n, False


def _project_matches(have: str, wanted: str) -> bool:
    if not wanted:
        return True
    if not have:
        return False
    have_n = str(Path(have))
    want_n = str(Path(wanted))
    return have_n == want_n or have_n.startswith(want_n.rstrip("/") + "/")


def get_engine() -> MemoryEngine:
    global _engine
    with _lock:
        if _engine is None:
            _engine = MemoryEngine(get_config())
        return _engine


def reset_engine() -> None:
    global _engine
    with _lock:
        if _engine is not None:
            try:
                _engine.close()
            except Exception:
                pass
            _engine = None
