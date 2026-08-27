"""Embedded Qdrant store.  No Docker: QdrantClient(path=...)."""

from __future__ import annotations

import uuid
from pathlib import Path
from typing import Any

from qdrant_client import QdrantClient
from qdrant_client.models import (
    Distance,
    FieldCondition,
    Filter,
    FilterSelector,
    MatchValue,
    PointStruct,
    SparseVector,
    SparseVectorParams,
    VectorParams,
)

from chunk import Turn

COLLECTION = "ghostherd_turns"
NAMESPACE = uuid.UUID("6ba7b810-9dad-11d1-80b4-00c04fd430c8")


def point_id(source_path: str, chunk_index: int) -> str:
    return str(uuid.uuid5(NAMESPACE, f"{source_path}:{chunk_index}"))


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

    def close(self) -> None:
        self.client.close()

    def collection_exists(self) -> bool:
        return bool(self.client.collection_exists(COLLECTION))

    def point_count(self) -> int:
        if not self.collection_exists():
            return 0
        return int(self.client.count(COLLECTION, exact=True).count)

    def ensure(self, dim: int, recreate: bool = False) -> None:
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
                sparse_vectors_config={"sparse": SparseVectorParams()},
            )
            # Payload indexes are a no-op on embedded Qdrant (path=); they
            # warn and do not make Filter faster.  Server Qdrant would.
        self._ready_dim = dim

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
            indices, values = svec
            if not indices:
                indices, values = [0], [1.0]
            points.append(
                PointStruct(
                    id=point_id(turn.source_path, turn.chunk_index),
                    vector={
                        "dense": dvec,
                        "sparse": SparseVector(indices=indices, values=values),
                    },
                    payload={
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
                    },
                )
            )
        self.client.upsert(collection_name=COLLECTION, points=points)
        return len(points)

    def search(
        self,
        dense: list[float],
        sparse: tuple[list[int], list[float]],
        limit: int = 8,
        agent: str | None = None,
        project: str | None = None,
    ) -> list[dict[str, Any]]:
        if not self.collection_exists() or self.point_count() == 0:
            return []
        qfilter = _filter(agent, project)
        fetch = max(limit * 3, 16)
        dense_hits = self.client.query_points(
            collection_name=COLLECTION,
            query=dense,
            using="dense",
            query_filter=qfilter,
            limit=fetch,
            with_payload=True,
        ).points
        indices, values = sparse
        if not indices:
            merged = dense_hits[:limit]
        else:
            sparse_hits = self.client.query_points(
                collection_name=COLLECTION,
                query=SparseVector(indices=indices, values=values),
                using="sparse",
                query_filter=qfilter,
                limit=fetch,
                with_payload=True,
            ).points
            merged = rrf_merge(dense_hits, sparse_hits, limit)
        return [_hit_to_dict(h) for h in merged]


def _filter(agent: str | None, project: str | None) -> Filter | None:
    must = []
    if agent:
        must.append(FieldCondition(key="agent", match=MatchValue(value=agent)))
    if project:
        must.append(FieldCondition(key="project", match=MatchValue(value=project)))
    if not must:
        return None
    return Filter(must=must)


def _hit_to_dict(hit: Any) -> dict[str, Any]:
    payload = hit.payload or {}
    return {
        "id": str(hit.id),
        "score": float(hit.score or 0.0),
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
