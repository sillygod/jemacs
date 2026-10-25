"""The session index's word (BM25) half, out of Qdrant: postings in SQLite.

Embedded Qdrant scores a sparse query against every chunk in a Python
loop: 0.49 s of a 0.6 s search on 85k chunks, 0.88 s in one project.  The
same BM25 here reads the query's words' postings: 5.5 ms at the median
(11 at p90), 8 ms in a project (20).  Measured on a snapshot of the real
store (ask 880bc57f) against two others: an index of our own in memory was
faster still (0.3 ms) but held 40-80 MB more in the sidecar and was
rebuilt at every start; SQLite FTS5's own bm25() ranked worse on the eval
(fused MRR 0.574 against 0.628) and cuts words its own way.

This scores as Qdrant did: each chunk's BM25 term weights
(embedder.sparse -- the CJK pairs, the same words) times each query
word's IDF, Qdrant's ln((N - df + 0.5) / (df + 0.5) + 1), over the whole
index whatever the scope -- the same scores to 1e-6 (SQLite adds in
double, Qdrant in single precision); the eval's BM25 numbers to the last
digit.  A scope (projects, an agent) is a join in SQL, not a filter
checked point by point, and it scopes the dense half's wide ask too
(store._scoped).  132 MB on disk for 85k chunks, a 2 MB page cache in
memory; built in 47 s (32 s of one core), checked at each start in 5 s.

Derived data: built from the chunks' stored text -- never a re-import --
in the background the first time the sidecar starts (`build'), then
kept in step by the store's own writes.  `check' compares it with the
collection, every point's id and a digest of its text, project and
agent, and repairs what differs.  Until it is built, and whenever it is
not of this build (sparse_migrate.build: the words' segmenter), search
asks Qdrant as it did."""

from __future__ import annotations

import array
import hashlib
import math
import sqlite3
import threading
from pathlib import Path
from typing import Any, Iterable

SCHEMA = """
CREATE TABLE IF NOT EXISTS docs (
    doc INTEGER PRIMARY KEY,
    pid TEXT UNIQUE NOT NULL,
    source TEXT,
    project TEXT,
    agent TEXT,
    digest TEXT,
    toks BLOB
);
CREATE INDEX IF NOT EXISTS docs_source ON docs(source);
CREATE TABLE IF NOT EXISTS post (
    tok INTEGER NOT NULL,
    doc INTEGER NOT NULL,
    w REAL NOT NULL,
    PRIMARY KEY (tok, doc)
) WITHOUT ROWID;
CREATE TABLE IF NOT EXISTS meta (k TEXT PRIMARY KEY, v TEXT);
"""


def digest(text: str, project: str, agent: str) -> str:
    "What `check' compares: a chunk's words, and what a scope asks of it."
    return hashlib.sha1(f"{project}\0{agent}\0{text}".encode("utf-8")).hexdigest()[:20]


def idf(n: int, df: int) -> float:
    "Qdrant's: never below zero, so a word in every chunk still counts a little."
    return math.log((n - df + 0.5) / (df + 0.5) + 1)


class WordIndex:
    """One SQLite file, one connection shared by the sidecar's threads
    behind a lock (as SourceIndex)."""

    def __init__(self, path: Path):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._mu = threading.RLock()
        self._conn = sqlite3.connect(self.path, check_same_thread=False)
        self._conn.execute("PRAGMA journal_mode=WAL")
        self._conn.execute("PRAGMA synchronous=NORMAL")
        self._conn.executescript(SCHEMA)
        self._conn.commit()

    def close(self) -> None:
        with self._mu:
            self._conn.close()

    # -- what it is --------------------------------------------------------------

    def built(self) -> str:
        "The build it holds (sparse_migrate.build), \"\" while none is complete."
        with self._mu:
            row = self._conn.execute("SELECT v FROM meta WHERE k = 'build'").fetchone()
        return row[0] if row else ""

    def mark(self, build: str) -> None:
        with self._mu, self._conn:
            if build:
                self._conn.execute("INSERT OR REPLACE INTO meta VALUES ('build', ?)", (build,))
            else:
                self._conn.execute("DELETE FROM meta WHERE k = 'build'")

    def count(self) -> int:
        with self._mu:
            return int(self._conn.execute("SELECT COUNT(*) FROM docs").fetchone()[0])

    def digests(self) -> dict[str, str]:
        "Every chunk's point id -> its digest."
        with self._mu:
            return dict(self._conn.execute("SELECT pid, digest FROM docs"))

    def clear(self) -> None:
        "Empty, and of no build: a build starts here."
        with self._mu, self._conn:
            self._conn.execute("DELETE FROM meta WHERE k = 'build'")
            self._conn.execute("DELETE FROM post")
            self._conn.execute("DELETE FROM docs")

    # -- kept in step ------------------------------------------------------------

    def add(self, chunks: Iterable[tuple[str, str, str, str, str, tuple[list[int], list[float]]]]) -> int:
        """CHUNKS -- (point id, source, project, agent, text, its sparse
        vector) -- in the index, each replacing what it held under its id."""
        rows = list(chunks)
        if not rows:
            return 0
        with self._mu, self._conn:
            self._drop(self._docs_of([r[0] for r in rows]))
            next_doc = int(self._conn.execute("SELECT COALESCE(MAX(doc), 0) FROM docs").fetchone()[0]) + 1
            docs, posts = [], []
            for i, (pid, source, project, agent, text, (ix, vs)) in enumerate(rows):
                if not ix:
                    ix, vs = [0], [1.0]  # as the store gives a chunk with no words (sparse_vector)
                doc = next_doc + i
                docs.append((doc, pid, source, project, agent, digest(text, project, agent),
                             array.array("q", ix).tobytes()))
                posts.extend((int(t), doc, float(w)) for t, w in zip(ix, vs))
            self._conn.executemany("INSERT INTO docs VALUES (?,?,?,?,?,?,?)", docs)
            self._conn.executemany("INSERT INTO post VALUES (?,?,?)", posts)
        return len(rows)

    def remove(self, pids: list[str]) -> None:
        if pids:
            with self._mu, self._conn:
                self._drop(self._docs_of(pids))

    def remove_source(self, source: str) -> None:
        with self._mu, self._conn:
            self._drop([r[0] for r in self._conn.execute(
                "SELECT doc FROM docs WHERE source = ?", (source,))])

    def retag(self, chunks: Iterable[tuple[str, str, str, str]]) -> None:
        """What CHUNKS -- (point id, project, agent, text) -- now say of
        themselves; their words are the same (store.restate)."""
        with self._mu, self._conn:
            self._conn.executemany(
                "UPDATE docs SET project = ?, agent = ?, digest = ? WHERE pid = ?",
                [(project, agent, digest(text, project, agent), pid)
                 for pid, project, agent, text in chunks])

    def _docs_of(self, pids: list[str]) -> list[int]:
        "Caller holds `_mu'."
        out: list[int] = []
        for i in range(0, len(pids), 500):
            part = pids[i:i + 500]
            out += [r[0] for r in self._conn.execute(
                f"SELECT doc FROM docs WHERE pid IN ({','.join('?' * len(part))})", part)]
        return out

    def _drop(self, docs: list[int]) -> None:
        "Caller holds `_mu' in a transaction."
        for doc in docs:
            row = self._conn.execute("SELECT toks FROM docs WHERE doc = ?", (doc,)).fetchone()
            if row and row[0]:
                toks = array.array("q")
                toks.frombytes(row[0])
                self._conn.executemany("DELETE FROM post WHERE tok = ? AND doc = ?",
                                       [(t, doc) for t in toks])
            self._conn.execute("DELETE FROM docs WHERE doc = ?", (doc,))

    # -- search ------------------------------------------------------------------

    def in_scope(self, pids: list[str], agent: str | None = None,
                 projects: list[str] | None = None) -> set[str]:
        "Which of PIDS are chunks of PROJECTS (and AGENT's): a scope, read here."
        out: set[str] = set()
        cond, extra = "", []
        if projects is not None:
            cond += f" AND project IN ({','.join('?' * len(projects))})"
            extra += list(projects)
        if agent:
            cond += " AND agent = ?"
            extra.append(agent)
        with self._mu:
            for i in range(0, len(pids), 500):
                part = pids[i:i + 500]
                out.update(r[0] for r in self._conn.execute(
                    f"SELECT pid FROM docs WHERE pid IN ({','.join('?' * len(part))}){cond}",
                    [*part, *extra]))
        return out

    def search(self, indices: list[int], limit: int, agent: str | None = None,
               projects: list[str] | None = None) -> list[tuple[str, float]]:
        """The chunks best matching a query's words INDICES (each once,
        weight 1: embedder.sparse_query), best first: (point id, score).
        PROJECTS and AGENT narrow which chunks, not the words' IDF."""
        toks = sorted({int(t) for t in indices})
        if not toks or projects == []:
            return []
        with self._mu:
            n = int(self._conn.execute("SELECT COUNT(*) FROM docs").fetchone()[0])
            dfs = dict(self._conn.execute(
                f"SELECT tok, COUNT(*) FROM post WHERE tok IN ({','.join('?' * len(toks))}) GROUP BY tok",
                toks))
            q = [(t, idf(n, dfs[t])) for t in toks if t in dfs]
            if not q:
                return []
            args: list[Any] = [x for pair in q for x in pair]
            where, join = [], ""
            if projects is not None or agent:
                join = " JOIN docs d ON d.doc = p.doc"
                if projects is not None:
                    where.append(f"d.project IN ({','.join('?' * len(projects))})")
                    args += list(projects)
                if agent:
                    where.append("d.agent = ?")
                    args.append(agent)
            sql = (f"WITH q(tok, idf) AS (VALUES {','.join('(?,?)' for _ in q)}) "
                   f"SELECT p.doc, SUM(p.w * q.idf) AS s FROM post p JOIN q ON p.tok = q.tok{join}"
                   + (f" WHERE {' AND '.join(where)}" if where else "")
                   + f" GROUP BY p.doc ORDER BY s DESC LIMIT {int(limit)}")
            hits = self._conn.execute(sql, args).fetchall()
            if not hits:
                return []
            pids = dict(self._conn.execute(
                f"SELECT doc, pid FROM docs WHERE doc IN ({','.join('?' * len(hits))})",
                [d for d, _ in hits]))
        return [(pids[d], float(s)) for d, s in hits if d in pids]


# -- built, and checked, against the store ----------------------------------------

PAGE = 1024


def _pages(store, fields: list[str]):
    "Every point of the store, in id order, a page at a time: (records)."
    offset = None
    while True:
        recs, offset = store.page(offset, PAGE, fields)
        yield recs
        if offset is None:
            return


def build(words: WordIndex, store, embedder, build_id: str, progress=None, rest: float = 0.0) -> int:
    """WORDS made anew from STORE's stored text: each chunk's sparse vector
    computed again (embedder.sparse), as the store's are.  Marked BUILD_ID
    only once whole.  The caller keeps other writers out (the engine's
    import lock).  Returns the chunks indexed."""
    import time

    words.clear()
    embedder.load_sparse()
    total, done = store.point_count(), 0
    for recs in _pages(store, ["text", "source_path", "project", "agent"]):
        t = time.monotonic()
        words.add(_chunks(recs, embedder))
        done += len(recs)
        if progress:
            progress(done, total)
        if rest:
            time.sleep((time.monotonic() - t) * rest)
    words.mark(build_id)
    return done


def check(words: WordIndex, store, embedder) -> dict[str, int]:
    """Compare WORDS with STORE -- every point's id, and the digest of its
    text, project and agent -- and repair what differs: a chunk missing or
    changed is indexed again from its stored text, one gone is dropped."""
    have = words.digests()
    seen: set[str] = set()
    stale: list[Any] = []
    for recs in _pages(store, ["text", "source_path", "project", "agent"]):
        for r in recs:
            pid = str(r.id)
            seen.add(pid)
            p = r.payload or {}
            if have.get(pid) != digest(p.get("text") or "", p.get("project") or "", p.get("agent") or ""):
                stale.append(r)
    extra = [pid for pid in have if pid not in seen]
    words.remove(extra)
    for i in range(0, len(stale), PAGE):
        words.add(_chunks(stale[i:i + PAGE], embedder))
    return {"checked": len(seen), "repaired": len(stale), "removed": len(extra)}


def _chunks(recs, embedder):
    payloads = [r.payload or {} for r in recs]
    vecs = embedder.sparse([p.get("text") or "" for p in payloads])
    return [(str(r.id), p.get("source_path") or "", p.get("project") or "", p.get("agent") or "",
             p.get("text") or "", v) for r, p, v in zip(recs, payloads, vecs)]
