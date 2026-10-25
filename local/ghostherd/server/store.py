"""Embedded Qdrant store.  No Docker: QdrantClient(path=...)."""

from __future__ import annotations

import functools
import threading
import uuid
from pathlib import Path
from typing import Any

from qdrant_client import QdrantClient
from qdrant_client.models import (
    Distance,
    FieldCondition,
    Filter,
    FilterSelector,
    MatchAny,
    MatchValue,
    Modifier,
    OverwritePayloadOperation,
    PointIdsList,
    PointStruct,
    PointVectors,
    SetPayload,
    SparseVector,
    SparseVectorParams,
    VectorParams,
)

from chunk import Turn

COLLECTION = "ghostherd_turns"
NAMESPACE = uuid.UUID("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
# The sparse half is BM25: the chunks carry their words' term weights
# (embedder.sparse) and Qdrant multiplies in each word's rarity at query
# time.  Without IDF a word in every chunk counted as much as a name in one.
SPARSE = SparseVectorParams(modifier=Modifier.IDF)


def point_id(source_path: str, chunk_index: int) -> str:
    return str(uuid.uuid5(NAMESPACE, f"{source_path}:{chunk_index}"))


def payload_for(turn: Turn) -> dict[str, Any]:
    """What a chunk stores besides its vectors.  Import compares this to
    tell a chunk that changed from one already embedded."""
    return {
        "text": turn.text,
        "agent": turn.agent,
        "role": turn.role,
        "project": turn.project or "",
        "session_id": turn.session_id or "",
        "source_path": turn.source_path,
        "source_kind": turn.source_kind,
        "ts": turn.ts or "",
        "title": turn.title or "",
        "chunk_index": turn.chunk_index,
    }


class StoreClosed(RuntimeError):
    "The store's client was closed (an engine reset); nothing more goes to it."


def _locked(fn):
    """Run a Store method holding `client_lock'.  QdrantLocal is not
    thread-safe, and its one client -- one per path: it takes an exclusive
    lock on the directory -- is shared by the transcripts' requests and the
    fact index's background worker."""
    @functools.wraps(fn)
    def wrap(self, *args, **kwargs):
        with self.client_lock:
            if self.closed:
                raise StoreClosed("the memory store is closed")
            return fn(self, *args, **kwargs)
    return wrap


def rrf_merge(
    dense_hits: list[Any],
    sparse_hits: list[Any],
    limit: int,
    k: int = 60,
) -> list[Any]:
    """Reciprocal rank fusion.  Dense and sparse each contribute 1/(k+rank)."""
    scores: dict[str, float] = {}
    payloads: dict[str, Any] = {}
    for rank, hit in enumerate(dense_hits, start=1):
        pid = str(hit.id)
        scores[pid] = scores.get(pid, 0.0) + 1.0 / (k + rank)
        payloads[pid] = hit
    for rank, hit in enumerate(sparse_hits, start=1):
        pid = str(hit.id)
        scores[pid] = scores.get(pid, 0.0) + 1.0 / (k + rank)
        payloads.setdefault(pid, hit)
    ordered = sorted(scores, key=lambda pid: scores[pid], reverse=True)
    out = []
    for pid in ordered[:limit]:
        hit = payloads[pid]
        hit.score = scores[pid]
        out.append(hit)
    return out


class Store:
    def __init__(self, path: Path):
        self.path = Path(path)
        self.path.mkdir(parents=True, exist_ok=True)
        self.client = QdrantClient(path=str(self.path))
        self._ready_dim: int | None = None
        # Every call into `client' holds this -- here, and in fact_index,
        # whose worker shares the client.  Re-entrant, so a method holding
        # it can call another that takes it.
        self.client_lock = threading.RLock()
        self.closed = False

    def close(self) -> None:
        "Close the client once no call is in it; later calls get StoreClosed."
        with self.client_lock:
            if not self.closed:
                self.closed = True
                self.client.close()

    @_locked
    def collection_exists(self) -> bool:
        return bool(self.client.collection_exists(COLLECTION))

    @_locked
    def point_count(self) -> int:
        if not self.collection_exists():
            return 0
        return int(self.client.count(COLLECTION, exact=True).count)

    @_locked
    def ensure(self, dim: int, recreate: bool = False, build: str = "") -> None:
        """The collection, made if missing (or of another DIM, with RECREATE)
        and then marked as BUILD: a new collection is of this code's build."""
        exists = self.collection_exists()
        if exists:
            info = self.client.get_collection(COLLECTION)
            vectors = info.config.params.vectors
            if isinstance(vectors, dict):
                current = int(vectors["dense"].size)
            else:
                current = int(vectors.size)
            if current != dim:
                if not recreate:
                    raise ValueError(
                        f"qdrant dim {current} != embed dim {dim}; "
                        "reimport with force=true"
                    )
                self.client.delete_collection(COLLECTION)
                exists = False
        if not exists:
            self.client.create_collection(
                collection_name=COLLECTION,
                vectors_config={
                    "dense": VectorParams(size=dim, distance=Distance.COSINE),
                },
                sparse_vectors_config={"sparse": SPARSE},
            )
            if build:
                self.mark(build)
            # Payload indexes are a no-op on embedded Qdrant (path=); they
            # warn and do not make Filter faster.  Server Qdrant would.
        self._ready_dim = dim

    @_locked
    def delete_source(self, source_path: str) -> None:
        if not self.collection_exists():
            return
        self.client.delete(
            collection_name=COLLECTION,
            points_selector=FilterSelector(
                filter=Filter(
                    must=[
                        FieldCondition(
                            key="source_path",
                            match=MatchValue(value=source_path),
                        )
                    ]
                )
            ),
        )

    @_locked
    def payloads_of(self, source_path: str) -> dict[int, dict[str, Any]]:
        """chunk_index -> stored payload for every chunk of SOURCE_PATH."""
        if not self.collection_exists():
            return {}
        out: dict[int, dict[str, Any]] = {}
        offset = None
        while True:
            batch, offset = self.client.scroll(
                collection_name=COLLECTION,
                scroll_filter=Filter(
                    must=[FieldCondition(key="source_path", match=MatchValue(value=source_path))]
                ),
                limit=512,
                offset=offset,
                with_payload=True,
                with_vectors=False,
            )
            for rec in batch:
                payload = rec.payload or {}
                out[int(payload.get("chunk_index") or 0)] = payload
            if offset is None:
                return out

    @_locked
    def titles_of(self, paths: list[str], depth: int = 64) -> dict[str, str]:
        """The title each of PATHS says of itself: the first its chunks
        carry among the first DEPTH, "" for one with none.  Read by point
        id -- a (path, index) names its chunk -- not by a filter, which
        would scan the whole collection once a path.  A claude transcript
        names its title some turns in, so its first chunks have none: the
        first sixteen are asked of every path, the rest only of those
        still without."""
        out = {p: "" for p in paths}
        if not paths or not self.collection_exists():
            return out
        todo, start = list(paths), 0
        for stop in (min(16, depth), depth):
            if not todo or start >= stop:
                break
            ids = {point_id(p, i): (p, i) for p in todo for i in range(start, stop)}
            found: dict[str, tuple[int, str]] = {}
            for rec in self.client.retrieve(
                collection_name=COLLECTION,
                ids=list(ids),
                with_payload=["title"],
                with_vectors=False,
            ):
                p, i = ids[str(rec.id)]
                title = (rec.payload or {}).get("title") or ""
                if title and (p not in found or i < found[p][0]):
                    found[p] = (i, title)
            for p, (_, title) in found.items():
                out[p] = title
            todo = [p for p in todo if not out[p]]
            start = stop
        return out

    @_locked
    def delete_chunks(self, source_path: str, indexes: list[int]) -> None:
        if indexes and self.collection_exists():
            self.client.delete(
                collection_name=COLLECTION,
                points_selector=PointIdsList(points=[point_id(source_path, i) for i in indexes]),
            )

    @_locked
    def upsert(
        self,
        turns: list[Turn],
        dense: list[list[float]],
        sparse: list[tuple[list[int], list[float]]],
    ) -> int:
        if not turns:
            return 0
        points = []
        for turn, dvec, svec in zip(turns, dense, sparse, strict=True):
            points.append(
                PointStruct(
                    id=point_id(turn.source_path, turn.chunk_index),
                    vector={"dense": dvec, "sparse": sparse_vector(svec)},
                    payload=payload_for(turn),
                )
            )
        self.client.upsert(collection_name=COLLECTION, points=points)
        return len(points)

    @_locked
    def restate(self, turns: list[Turn]) -> int:
        """What TURNS say of themselves -- a time, a title -- written over
        their stored chunks', whose words and so vectors are the same."""
        if not turns:
            return 0
        self.client.batch_update_points(
            collection_name=COLLECTION,
            update_operations=[
                OverwritePayloadOperation(
                    overwrite_payload=SetPayload(
                        payload=payload_for(turn),
                        points=[point_id(turn.source_path, turn.chunk_index)],
                    )
                )
                for turn in turns
            ],
        )
        return len(turns)

    @_locked
    def search(
        self,
        dense: list[float],
        sparse: tuple[list[int], list[float]],
        limit: int = 8,
        agent: str | None = None,
        projects: list[str] | None = None,
    ) -> list[dict[str, Any]]:
        """Dense and sparse hits fused (RRF).  PROJECTS: the project values
        a chunk may have -- None for any, [] for none."""
        if projects is not None and not projects:
            return []
        if not self.collection_exists() or self.point_count() == 0:
            return []
        qfilter = _filter(agent, projects)
        fetch = max(limit * 3, 16)
        dense_hits = self._query("dense", dense, fetch, qfilter)
        indices, values = sparse
        if not indices:
            merged = dense_hits[:limit]
        else:
            sparse_hits = self._query(
                "sparse", SparseVector(indices=indices, values=values), fetch, qfilter)
            merged = rrf_merge(dense_hits, sparse_hits, limit)
        return [_hit_to_dict(h) for h in merged]

    @_locked
    def channel(
        self,
        using: str,
        vector: Any,
        limit: int,
        agent: str | None = None,
        projects: list[str] | None = None,
    ) -> list[dict[str, Any]]:
        "One half alone (USING: dense or sparse) -- for eval/, which weighs each."
        if using == "sparse":
            indices, values = vector
            if not indices:
                return []
            vector = SparseVector(indices=indices, values=values)
        return [_hit_to_dict(h) for h in self._query(using, vector, limit, _filter(agent, projects))]

    def _query(self, using: str, vector: Any, limit: int, qfilter: Filter | None) -> list[Any]:
        "Caller holds `client_lock'."
        return self.client.query_points(
            collection_name=COLLECTION,
            query=vector,
            using=using,
            query_filter=qfilter,
            limit=limit,
            with_payload=True,
        ).points

    # -- what sparse_migrate works with ------------------------------------

    def storage_files(self) -> list[Path]:
        """The files that hold the collection in embedded Qdrant: its
        points (sqlite) and the store's collection list (meta.json)."""
        return [self.path / "collection" / COLLECTION / "storage.sqlite",
                self.path / "meta.json"]

    @_locked
    def backup(self, dest: Path) -> list[Path]:
        """Copy the collection's files into DEST, consistently: holding the
        lock no write comes in between, and the points go by SQLite's
        backup API, not a file copy."""
        import shutil
        import sqlite3

        dest.mkdir(parents=True, exist_ok=True)
        points, meta = self.storage_files()
        out = [dest / f"{COLLECTION}.storage.sqlite", dest / "meta.json"]
        src = sqlite3.connect(f"file:{points}?mode=ro", uri=True)
        dst = sqlite3.connect(out[0])
        try:
            src.backup(dst)
        finally:
            dst.close()
            src.close()
        shutil.copy2(meta, out[1])
        return out

    @_locked
    def sparse_state(self) -> tuple[Any, str]:
        "(the sparse vectors' modifier, the build the index records) -- see `mark'."
        info = self.client.get_collection(COLLECTION)
        params = (info.config.params.sparse_vectors or {}).get("sparse")
        meta = info.config.metadata or {}
        return (params.modifier if params else None), str(meta.get("ghostherd_sparse") or "")

    @_locked
    def weigh_by_idf(self) -> None:
        self.client.update_collection(COLLECTION, sparse_vectors_config={"sparse": SPARSE})

    @_locked
    def mark(self, build: str) -> None:
        """Record in the collection itself which build of the sparse half it
        holds: restoring a backup restores the mark with the points."""
        self.client.update_collection(COLLECTION, metadata={"ghostherd_sparse": build})

    @_locked
    def page(self, offset: Any, limit: int, fields: list[str]) -> tuple[list[Any], Any]:
        "LIMIT points from OFFSET in id order, payload FIELDS only; and the next offset."
        return self.client.scroll(
            collection_name=COLLECTION,
            limit=limit,
            offset=offset,
            with_payload=fields,
            with_vectors=False,
        )

    @_locked
    def set_sparse(self, vectors: dict[str, tuple[list[int], list[float]]]) -> None:
        "Replace the sparse vector of each point id in VECTORS; nothing else."
        self.client.update_vectors(
            collection_name=COLLECTION,
            points=[
                PointVectors(id=pid, vector={"sparse": sparse_vector(vec)})
                for pid, vec in vectors.items()
            ],
        )

    @_locked
    def points_of(self, source_path: str) -> list[Any]:
        "Every point of SOURCE_PATH with its payload and vectors."
        out: list[Any] = []
        offset = None
        while True:
            batch, offset = self.client.scroll(
                collection_name=COLLECTION,
                scroll_filter=Filter(must=[
                    FieldCondition(key="source_path", match=MatchValue(value=source_path))]),
                limit=256,
                offset=offset,
                with_payload=True,
                with_vectors=True,
            )
            out.extend(batch)
            if offset is None:
                return out

    @_locked
    def put(self, points: list[PointStruct]) -> None:
        if points:
            self.client.upsert(collection_name=COLLECTION, points=points)

    @_locked
    def list_chunks(
        self,
        source_path: str | None = None,
        session_id: str | None = None,
        limit: int = 400,
        offset: int = 0,
    ) -> tuple[list[dict[str, Any]], int]:
        """Payload scroll.  No embedding — this is the viewer, not search."""
        if not source_path and not session_id:
            raise ValueError("source_path or session_id is required")
        if not self.collection_exists():
            return [], 0
        must = []
        if source_path:
            must.append(
                FieldCondition(
                    key="source_path", match=MatchValue(value=source_path)
                )
            )
        if session_id:
            must.append(
                FieldCondition(
                    key="session_id", match=MatchValue(value=session_id)
                )
            )
        records: list[Any] = []
        next_offset = None
        while True:
            batch, next_offset = self.client.scroll(
                collection_name=COLLECTION,
                scroll_filter=Filter(must=must),
                limit=256,
                offset=next_offset,
                with_payload=True,
                with_vectors=False,
            )
            records.extend(batch)
            if next_offset is None:
                break
        records.sort(
            key=lambda rec: int((rec.payload or {}).get("chunk_index") or 0)
        )
        total = len(records)
        sliced = records[max(0, offset) : max(0, offset) + max(1, limit)]
        chunks = []
        for rec in sliced:
            item = _hit_to_dict(rec)
            item["score"] = 0.0
            chunks.append(item)
        return chunks, total


def _filter(agent: str | None, projects: list[str] | None) -> Filter | None:
    must = []
    if agent:
        must.append(FieldCondition(key="agent", match=MatchValue(value=agent)))
    if projects:
        must.append(FieldCondition(key="project", match=MatchAny(any=list(projects))))
    if not must:
        return None
    return Filter(must=must)


def sparse_vector(vec: tuple[list[int], list[float]]) -> SparseVector:
    """A chunk with no word BM25 counts (all punctuation) still needs a
    sparse vector: one placeholder entry, as before."""
    indices, values = vec
    if not indices:
        indices, values = [0], [1.0]
    return SparseVector(indices=indices, values=values)


def _hit_to_dict(hit: Any) -> dict[str, Any]:
    payload = hit.payload or {}
    return {
        "id": str(hit.id),
        "score": float(getattr(hit, "score", None) or 0.0),
        "text": payload.get("text") or "",
        "agent": payload.get("agent") or "",
        "role": payload.get("role") or "",
        "project": payload.get("project") or "",
        "session_id": payload.get("session_id") or "",
        "source_path": payload.get("source_path") or "",
        "source_kind": payload.get("source_kind") or "",
        "ts": payload.get("ts") or "",
        "title": payload.get("title") or "",
        "chunk_index": payload.get("chunk_index") or 0,
    }
