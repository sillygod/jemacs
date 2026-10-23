"""The shared-memory facts in a Qdrant collection of their own, and the
ranking that reads it -- for `herd recall', for the memory an ask carries,
and for the page.

The facts sit in `ghostherd_facts', beside the transcripts' collection in
the same embedded Qdrant (one client per path: the engine's), never mixed
with them.  A point is a fact -- its store and name -- with the dense
vector of its title, description and body, and a digest of its text.

Kept in sync from the files, which are the truth.  A write through
shared_memory (herd remember, forget, the page's edits) tells the index at
once, and the background worker embeds it.  A write that bypasses it --
claude writing its own memory file, a hand edit -- is found the next time
a store is read: each read compares the files' stamps with the last seen,
and re-embeds only a fact whose text changed.  So a restart re-embeds
nothing it already has.

The embedder loads slowly the first time (FastEmbed, ~2s from cache).  An
ask never waits for it: until it is up, the ask's memory is found by words
alone, and the load starts in the background.  A recall waits a little
for it, then falls back to words the same way.
"""

from __future__ import annotations

import atexit
import hashlib
import queue
import threading
import time
import uuid
from contextlib import contextmanager
from typing import Any

from qdrant_client.models import (
    Distance,
    FieldCondition,
    Filter,
    FilterSelector,
    MatchAny,
    MatchValue,
    PointIdsList,
    PointStruct,
    VectorParams,
)

import memory_rank as mr
import shared_memory as sm
from store import StoreClosed

COLLECTION = "ghostherd_facts"
NAMESPACE = uuid.UUID("2f1d6c1e-9b2a-4d7e-8f51-6a0c3e9b7d24")
# How long a failed embedder load is believed before trying again.
RETRY_LOAD = 60.0
# How long the worker idles before it ends; the next change starts another.
WORKER_IDLE = 2.0
# An ask's memory: at most this many characters, whole facts only; a fact
# longer than BODY_MAX shows its start and how to read the rest.
ASK_BLOCK_MAX = 1800
BODY_MAX = 300
ASK_HEADING = "Relevant memory"


def point_id(key: str, name: str) -> str:
    return str(uuid.uuid5(NAMESPACE, f"{key}:{name}"))


def fact_text(f: sm.Fact) -> str:
    "What a fact is embedded as: its title, description and body."
    return f"{f.title}\n{f.description}\n{f.body}"[:4000]


def digest(f: sm.Fact) -> str:
    return hashlib.sha1(
        f"{f.name}\n{f.type}\n{f.description}\n{f.body}".encode("utf-8")
    ).hexdigest()


def _stamps(key: str) -> dict[str, tuple[int, int]]:
    "Store KEY's own fact files, name -> (mtime_ns, size): what a change moves."
    store = sm.project_store(key)
    out: dict[str, tuple[int, int]] = {}
    if not store.is_dir():
        return out
    for p in store.glob("*.md"):
        if p.name == sm.INDEX or p.is_symlink():
            continue
        try:
            st = p.stat()
        except OSError:
            continue
        out[p.stem] = (st.st_mtime_ns, st.st_size)
    return out


def _doc(key: str, f: sm.Fact) -> mr.Doc:
    return mr.Doc(key=key, name=f.name, title=f.title, description=f.description,
                  body=f.body, type=f.type, shared=f.shared,
                  extra={"author": f.author})


class FactIndex:
    def __init__(self, engine):
        self.engine = engine
        self._embedder = None
        # Built once, though the startup warm thread and a first query race
        # for it; a plain Lock (not self._mu, which `sync' holds while it
        # calls `.embedder').
        self._embedder_lock = threading.Lock()
        self._mu = threading.RLock()
        self._seen: dict[str, dict[str, tuple[int, int]]] = {}
        self._vecs: dict[str, dict[str, list[float]]] = {}
        self._bg: list[list[float]] | None = None
        self._dim: int | None = None
        self._failed_at = 0.0
        self.load_error: str | None = None
        self._queue: "queue.Queue[tuple[str, str | None]]" = queue.Queue()
        self._worker: threading.Thread | None = None

    # -- the embedder ----------------------------------------------------

    @property
    def embedder(self):
        """The facts' own embedder -- a multilingual model, so a Chinese ask
        finds an English fact -- not the engine's (the transcripts').  Shares
        the engine's config, so a test's fake embedder stays fake.  Built once
        under a lock: the warm thread and a first query would otherwise both
        build it and load the model twice (double-checked, so the common path
        takes no lock)."""
        if self._embedder is None:
            with self._embedder_lock:
                if self._embedder is None:
                    from embedder import Embedder
                    self._embedder = Embedder(
                        self.engine.cfg, model=self.engine.cfg.fact_embed_model)
        return self._embedder

    def ready(self) -> bool:
        return bool(self.embedder.loaded)

    def _load(self) -> bool:
        "Load the embedder now; False when it cannot be (and not for a while)."
        if self.ready():
            return True
        if time.monotonic() - self._failed_at < RETRY_LOAD and self.load_error:
            return False
        try:
            self.embedder.load()
            self.load_error = None
            return True
        except Exception as exc:  # noqa: BLE001 -- fastembed missing, no model, no disk
            self.load_error = str(exc)
            self._failed_at = time.monotonic()
            return False

    def wait_ready(self, seconds: float) -> bool:
        """The embedder, loaded within SECONDS -- here, or by the worker
        already loading it -- or False."""
        if self.ready():
            return True
        if seconds <= 0:
            self.warm()
            return False
        done = threading.Event()
        result: list[bool] = []

        def go():
            result.append(self._load())
            done.set()

        threading.Thread(target=go, daemon=True).start()
        done.wait(seconds)
        return bool(result and result[0])

    # -- the worker ------------------------------------------------------

    def _enqueue(self, task: tuple[str, str | None]) -> None:
        self._queue.put(task)
        with self._mu:
            if self._worker is None or not self._worker.is_alive():
                self._worker = threading.Thread(target=self._work, daemon=True,
                                                name="ghostherd-facts")
                self._worker.start()

    def _work(self) -> None:
        while True:
            try:
                op, key = self._queue.get(timeout=WORKER_IDLE)
            except queue.Empty:
                return
            if op == "stop":
                return
            try:
                if not self._load():
                    continue
                if op == "warm":
                    for k in self.keys():
                        self.sync(k)
                elif op == "sync" and key:
                    self.sync(key)
            except Exception:  # noqa: BLE001 -- the next read retries it
                pass

    def close(self) -> None:
        """Stop the worker and wait for it, so nothing is mid-write when the
        engine (and its Qdrant client) closes, or the process ends."""
        worker = self._worker
        if worker is not None and worker.is_alive():
            self._queue.put(("stop", None))
            worker.join(timeout=10)

    def warm(self) -> None:
        "Load the embedder and index every store, in the background."
        self._enqueue(("warm", None))

    def notify(self, key: str, _names: list[str]) -> None:
        "A store changed (shared_memory.on_change): index it in the background."
        self._enqueue(("sync", key))

    def keys(self) -> list[str]:
        base = sm.root()
        if not base.is_dir():
            return []
        return sorted(d.name for d in base.iterdir() if d.is_dir())

    # -- the collection --------------------------------------------------

    @contextmanager
    def _client(self):
        """The engine's Qdrant client, held: QdrantLocal is not thread-safe,
        and the transcripts' requests use the same client under the same
        lock (store.client_lock), so this worker and a request are never
        both in it.  Only the client calls hold it -- not the embedding
        between them.  A closed store (an engine reset) raises StoreClosed
        rather than reach a closed client."""
        store = self.engine.store
        with store.client_lock:
            if store.closed:
                raise StoreClosed("the memory store is closed")
            yield store.client

    def _ensure(self) -> None:
        "The collection, sized for the embedder's vectors."
        dim = int(self.embedder.dim)
        if self._dim == dim:
            return
        with self._client() as client:
            self._ensure_in(client, dim)

    def _ensure_in(self, client, dim: int) -> None:
        if client.collection_exists(COLLECTION):
            info = client.get_collection(COLLECTION)
            vectors = info.config.params.vectors
            size = int(vectors["dense"].size if isinstance(vectors, dict) else vectors.size)
            if size != dim:
                client.delete_collection(COLLECTION)
                self._seen.clear()
                self._vecs.clear()
        if not client.collection_exists(COLLECTION):
            client.create_collection(
                collection_name=COLLECTION,
                vectors_config={"dense": VectorParams(size=dim, distance=Distance.COSINE)},
            )
        self._dim = dim

    def _stored(self, key: str) -> dict[str, tuple[str, list[float]]]:
        "Store KEY's points: name -> (digest, vector)."
        out: dict[str, tuple[str, list[float]]] = {}
        offset = None
        flt = Filter(must=[FieldCondition(key="store", match=MatchValue(value=key))])
        with self._client() as client:
            while True:
                batch, offset = client.scroll(
                    collection_name=COLLECTION, scroll_filter=flt, limit=256,
                    offset=offset, with_payload=True, with_vectors=True)
                for rec in batch:
                    p = rec.payload or {}
                    vec = rec.vector["dense"] if isinstance(rec.vector, dict) else rec.vector
                    out[str(p.get("name"))] = (str(p.get("digest") or ""), list(vec or []))
                if offset is None:
                    return out

    def sync(self, key: str) -> dict[str, Any]:
        """Bring store KEY's points in line with its files: embed a fact that
        is new or whose text changed, drop one whose file is gone.  Nothing
        is read past the stamps when no file moved since the last sync."""
        with self._mu:
            self._ensure()
            stamps = _stamps(key)
            if self._seen.get(key) == stamps and key in self._vecs:
                return {"store": key, "embedded": 0, "removed": 0, "unchanged": True}
            # A promoted fact lives in the instruction file now: leave it out
            # of retrieval.  Dropped from `facts', its point falls into `gone'
            # below and is deleted, so promotion both stops re-indexing and
            # removes any point already there.
            facts = [f for f in sm.own_facts(sm.project_store(key)) if not f.promoted] if stamps else []
            stored = self._stored(key)
            fresh = [f for f in facts if stored.get(f.name, ("", []))[0] != digest(f)]
            names = {f.name for f in facts}
            gone = [n for n in stored if n not in names]
            if gone:
                with self._client() as client:
                    client.delete(collection_name=COLLECTION,
                                  points_selector=PointIdsList(points=[point_id(key, n) for n in gone]))
            vecs = {n: v for n, (_, v) in stored.items() if n in names}
            if fresh:
                # Embedded outside the client's lock: a search need not wait.
                dense = self.embedder.dense([fact_text(f) for f in fresh])
                with self._client() as client:
                    client.upsert(collection_name=COLLECTION, points=[
                        PointStruct(id=point_id(key, f.name), vector={"dense": v},
                                    payload={"store": key, "name": f.name, "type": f.type,
                                             "digest": digest(f), "updated": f.updated})
                        for f, v in zip(fresh, dense, strict=True)])
                vecs.update({f.name: v for f, v in zip(fresh, dense)})
            self._vecs[key] = vecs
            self._seen[key] = stamps
            return {"store": key, "embedded": len(fresh), "removed": len(gone)}

    def drop(self, key: str) -> None:
        "Forget store KEY's points (a test's, or a store removed by hand)."
        with self._mu, self._client() as client:
            if client.collection_exists(COLLECTION):
                client.delete(collection_name=COLLECTION, points_selector=FilterSelector(
                    filter=Filter(must=[FieldCondition(key="store", match=MatchAny(any=[key]))])))
            self._seen.pop(key, None)
            self._vecs.pop(key, None)

    def _background(self) -> list[list[float]]:
        if self._bg is None:
            self._bg = self.embedder.dense(mr.BACKGROUND)
        return self._bg

    # -- ranking ---------------------------------------------------------

    def rank(self, key: str, query: str, *, strict: bool = True, k: int | None = None,
             exclude: tuple[str, ...] | list[str] = (), wait: float = 0.0) -> dict[str, Any]:
        """The facts of store KEY's pool (its own and the user's) relevant to
        QUERY, best first: {mode, hits}.  STRICT for an ask, else a recall's
        looser bar (see memory_rank).  EXCLUDE: names the caller already has
        in full.  MODE is `meaning' when the embedder ranked them, `words'
        when it was not up within WAIT seconds and words alone did."""
        facts = sm.pool(key)
        if not facts or not (query or "").strip():
            return {"mode": "none", "hits": []}
        keyed = [(sm.USER_KEY if f.shared else key, f) for f in facts]
        docs = [_doc(k_, f) for k_, f in keyed]
        want = (mr.ASK_K if strict else mr.RECALL_K) if k is None else k
        drop = set(exclude)
        if not self.wait_ready(wait):
            hits = mr.rank_words(docs, query, strict=strict, k=len(docs))
            hits = [h for h in hits if h.doc.name not in drop][:want]
            return {"mode": "words", "hits": [_hit_json(h) for h in hits]}
        with self._mu:
            for store in {k_ for k_, _ in keyed}:
                self.sync(store)
            vecs = [self._vecs.get(k_, {}).get(f.name) for k_, f in keyed]
            # A fact whose file appeared after the sync started: embed it now.
            missing = [i for i, v in enumerate(vecs) if not v]
            if missing:
                fill = self.embedder.dense([fact_text(keyed[i][1]) for i in missing])
                for i, v in zip(missing, fill):
                    vecs[i] = v
            parts = mr.parts(query)
            pvecs = self.embedder.dense(parts)
            bg = self._background()
        hits = mr.rank(docs, vecs, parts, pvecs, bg, strict=strict, k=len(docs))
        hits = [h for h in hits if h.doc.name not in drop][:want]
        return {"mode": "meaning", "hits": [_hit_json(h) for h in hits]}

    # -- what an ask carries ------------------------------------------------

    def ask_block(self, taker: dict[str, Any] | None, body: str) -> str | None:
        """The memory an ask to TAKER carries, under ASK_HEADING: the facts
        most relevant to its BODY, from the taker's project, or None when
        none passes the bar.  A grok or agy taker was given the rules in
        full as it started, so they are not given again."""
        project = (taker or {}).get("project") or ""
        if not project or not (body or "").strip():
            return None
        key = sm.slug_for_path(project)
        exclude: list[str] = []
        if (taker or {}).get("kind") in ("grok", "agy"):
            exclude = sm.opening(key)["full"]
        found = self.rank(key, body, strict=True, exclude=exclude, wait=0.0)
        return compose_block(found["hits"])


def _hit_json(h: mr.Hit) -> dict[str, Any]:
    d = h.doc
    return {"name": d.name, "store": d.key, "type": d.type, "shared": d.shared,
            "title": d.title, "description": d.description, "body": d.body,
            "author": d.extra.get("author") or "",
            "score": round(h.score, 3), "z": round(h.z, 3), "lex": round(h.lex, 3),
            "why": h.why}


def _excerpt(body: str, limit: int) -> tuple[str, bool]:
    "BODY whole when it fits in LIMIT, else its start cut at a line or word."
    body = body.strip()
    if len(body) <= limit:
        return body, False
    cut = body[:limit]
    at = max(cut.rfind("\n"), cut.rfind(" "))
    return (cut[:at] if at > limit // 2 else cut).rstrip(), True


def compose_block(hits: list[dict[str, Any]]) -> str | None:
    """HITS as the section an ask ends with, or None for no hits.  Whole
    facts only, within ASK_BLOCK_MAX; a long one gives its start and how to
    read the rest."""
    if not hits:
        return None
    head = (f"{ASK_HEADING} (from the herd's shared memory, by this ask's "
            f"subject; {sm.RECALL} NAME for the whole of one):")
    out = [head]
    used = len(head)
    for h in hits:
        where = "every project" if h.get("shared") else h.get("type") or "project"
        body, cut = _excerpt(h.get("body") or "", BODY_MAX)
        entry = f"\n- {h['name']} ({where}): {h.get('description') or h.get('title')}"
        if body:
            entry += "\n  " + body.replace("\n", "\n  ")
        if cut:
            entry += f" … ({sm.RECALL} {h['name']} for the rest)"
        if used + len(entry) > ASK_BLOCK_MAX and len(out) > 1:
            break
        out.append(entry)
        used += len(entry)
    return "\n".join(out) if len(out) > 1 else None


_lock = threading.Lock()
_index: FactIndex | None = None


def get_index() -> FactIndex:
    "The index over the engine's Qdrant; a new engine gets a new one."
    from engine import get_engine

    global _index
    engine = get_engine()
    with _lock:
        if _index is None or _index.engine is not engine:
            _index = FactIndex(engine)
        return _index


def reset_index() -> None:
    "Close the index (engine.reset_engine calls it before closing Qdrant)."
    global _index
    with _lock:
        idx, _index = _index, None
    if idx is not None:
        idx.close()


atexit.register(reset_index)


def _on_change(key: str, names: list[str]) -> None:
    "shared_memory's listener: tell the live index, if one is up."
    idx = _index
    if idx is not None:
        idx.notify(key, names)


sm.on_change(_on_change)
