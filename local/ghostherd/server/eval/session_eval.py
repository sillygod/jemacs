"""A labelled eval of the session index's search, on a snapshot of the store.

The queries (session_queries.json) are what the user would type: exact
identifiers (function and file names, error strings), Chinese words and
phrases, Chinese mixed with English, plain English.  Each names the chunks
it should find, by session and text (so a chunk moved by sparse_migrate
still counts), resolved once from a regex of what the wanted chunks say.

Never on the live store.  Embedded Qdrant reads every point -- and writes
-- before it checks the store's lock, so the commands below take only a
directory `snapshot' made, and check the lock first anyway:

  cd local/ghostherd/server
  uv run python eval/session_eval.py snapshot ~/.emacs.d/emacs-home/ghostherd-memory /tmp/snap
  uv run python eval/session_eval.py run /tmp/snap --legacy   # the old query side
  uv run python eval/session_eval.py migrate /tmp/snap        # sparse_migrate, timed
  uv run python eval/session_eval.py run /tmp/snap

`snapshot' copies with SQLite's backup API, in one step: a consistent copy
while the sidecar runs (it waits on the copy's read lock, ~1 s for 800 MB).
`label' resolves the regexes again, on a snapshot of a store that changed.
GHOSTHERD_MEMORY_CJK (lexical) picks the CJK segmenter for `migrate' and
`run'; `run --legacy' asks as the code before it did (unsegmented, a
document's weights) -- on a snapshot not migrated, that is the old search.

Metrics, over the top 30 chunks of each search: MRR (1/rank of the first
wanted chunk), hit@1/5/10 (one wanted chunk in the top k), recall@10 (the
wanted chunks in the top 10, of at most 10), and ses@5 (a wanted chunk's
session among the top 5 chunks')."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import resource
import shutil
import sqlite3
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))

QUERIES = HERE / "session_queries.json"
MARK = "SNAPSHOT"
TOP = 30
MODES = ("dense", "sparse", "fused")


def key(payload: dict) -> tuple[str, str]:
    text = payload.get("text") or ""
    return payload.get("session_id") or "", hashlib.sha1(text.encode("utf-8")).hexdigest()[:16]


# -- the snapshot ------------------------------------------------------------

def snapshot(src: Path, dst: Path) -> None:
    if dst.exists():
        sys.exit(f"{dst} exists")
    files = [p.relative_to(src) for p in sorted((src / "qdrant").glob("collection/*/storage.sqlite"))]
    files.append(Path("sources.sqlite"))
    for rel in files:
        out = dst / rel
        out.parent.mkdir(parents=True, exist_ok=True)
        t = time.monotonic()
        con = sqlite3.connect(f"file:{src / rel}?mode=ro", uri=True, timeout=2)
        dest = sqlite3.connect(out)
        try:
            con.backup(dest)  # one step: consistent, holding a read lock
        finally:
            dest.close()
            con.close()
        print(f"{rel}: {out.stat().st_size / 1e6:.1f} MB in {time.monotonic() - t:.2f} s")
    shutil.copy2(src / "qdrant" / "meta.json", dst / "qdrant" / "meta.json")
    (dst / MARK).write_text(f"snapshot of {src} at {time.strftime('%Y-%m-%d %H:%M:%S')}\n")


def open_engine(data: Path):
    """An engine on the snapshot DATA -- refused for anything else, or a
    store another process has open."""
    if not (data / MARK).is_file():
        sys.exit(f"{data} is not a snapshot (no {MARK}); make one with `snapshot'")
    lock = data / "qdrant" / ".lock"
    if lock.exists():
        import portalocker

        with open(lock, "r+") as fh:
            try:
                portalocker.lock(fh, portalocker.LockFlags.EXCLUSIVE | portalocker.LockFlags.NON_BLOCKING)
                portalocker.unlock(fh)
            except portalocker.exceptions.LockException:
                sys.exit(f"{data} is open in another process")
    os.environ["GHOSTHERD_MEMORY_DIR"] = str(data)
    import warnings

    warnings.filterwarnings("ignore", message="Local mode is not recommended")
    from config import Config
    from engine import MemoryEngine

    cfg = Config.from_env()
    return MemoryEngine(cfg)


# -- labels ------------------------------------------------------------------

def label(data: Path, path: Path) -> None:
    spec = json.loads(path.read_text(encoding="utf-8"))
    eng = open_engine(data)
    try:
        rows, offset = [], None
        while True:
            recs, offset = eng.store.page(offset, 2048, ["text", "session_id", "project"])
            rows.extend(r.payload or {} for r in recs)
            if offset is None:
                break
    finally:
        eng.close()
    for q in spec["queries"]:
        rx = re.compile(q["label"]["regex"], re.I | re.S)
        proj = q["label"].get("project")
        found = sorted({key(p) for p in rows
                        if (not proj or proj in (p.get("project") or "")) and rx.search(p.get("text") or "")})
        if not found:
            sys.exit(f"nothing to find for {q['query']!r}")
        q["relevant"] = [list(k) for k in found]
    spec["labelled"] = {"points": len(rows), "at": time.strftime("%Y-%m-%d")}
    path.write_text(json.dumps(spec, ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    print(f"labelled {len(spec['queries'])} queries on {len(rows)} points")


# -- the eval ----------------------------------------------------------------

def score(hits: list[dict], relevant: set[tuple[str, str]]) -> dict[str, float]:
    keys = [key(h) for h in hits[:TOP]]
    sessions = {s for s, _ in relevant}
    rank = next((i for i, k in enumerate(keys, 1) if k in relevant), 0)
    return {
        "mrr": 1.0 / rank if rank else 0.0,
        "hit@1": float(0 < rank <= 1),
        "hit@5": float(0 < rank <= 5),
        "hit@10": float(0 < rank <= 10),
        "recall@10": len(set(keys[:10]) & relevant) / min(len(relevant), 10),
        "ses@5": float(any(h["session_id"] in sessions for h in hits[:5])),
    }


def run(data: Path, path: Path, legacy: bool, show: bool) -> dict:
    if legacy:
        os.environ["GHOSTHERD_MEMORY_CJK"] = "none"
    spec = json.loads(path.read_text(encoding="utf-8"))
    eng = open_engine(data)
    out: dict[str, dict[str, list[dict]]] = {m: {} for m in MODES}
    try:
        eng._ensure_ready()
        for q in spec["queries"]:
            text, kind = q["query"], q["kind"]
            relevant = {tuple(k) for k in q["relevant"]}
            dense = eng.embedder.dense([text])[0]
            # The code before asked with a document's weights, unsegmented.
            sparse = eng.embedder.sparse([text])[0] if legacy else eng.embedder.sparse_query(text)
            hits = {
                "dense": eng.store.channel("dense", dense, TOP),
                "sparse": eng.store.channel("sparse", sparse, TOP),
                "fused": eng.store.search(dense, sparse, limit=TOP),
            }
            for m in MODES:
                s = score(hits[m], relevant)
                out[m].setdefault(kind, []).append(s)
                out[m].setdefault("all", []).append(s)
                if show and m == "fused":
                    print(f"{s['mrr']:.2f} {s['hit@10']:.0f} {kind:5} {text}")
    finally:
        eng.close()
    return {m: {kind: _mean(rows) for kind, rows in kinds.items()} for m, kinds in out.items()}


def _mean(rows: list[dict]) -> dict[str, float]:
    return {k: round(sum(r[k] for r in rows) / len(rows), 3) for k in rows[0]} | {"n": len(rows)}


def table(result: dict) -> str:
    cols = ("mrr", "hit@1", "hit@5", "hit@10", "recall@10", "ses@5")
    lines = [f"{'mode':7}{'kind':7}{'n':>4} " + " ".join(f"{c:>9}" for c in cols)]
    for m, kinds in result.items():
        for kind in ("ident", "zh", "mixed", "en", "all"):
            if kind in kinds:
                r = kinds[kind]
                lines.append(f"{m:7}{kind:7}{r['n']:>4} " + " ".join(f"{r[c]:>9.3f}" for c in cols))
    return "\n".join(lines)


# -- the migration, timed ----------------------------------------------------

def migrate(data: Path) -> None:
    eng = open_engine(data)
    try:
        before = eng.store.point_count()
        t, cpu = time.monotonic(), _cpu()
        state = eng.migrate()
        wall, used = time.monotonic() - t, _cpu() - cpu
        print(json.dumps({
            "points_before": before,
            "points_after": eng.store.point_count(),
            "wall_s": round(wall, 1),
            "cpu_s": round(used, 1),
            "cores": round(used / wall, 2) if wall else 0,
            "max_rss_mb": round(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 2**20),
            "state": {k: v for k, v in (state or {}).items() if k not in ("before", "plan")},
            "folded_paths": sum(len(g["drop"]) for g in (state or {}).get("plan", [])),
        }, indent=1))
    finally:
        eng.close()


def _cpu() -> float:
    r = resource.getrusage(resource.RUSAGE_SELF)
    return r.ru_utime + r.ru_stime


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("snapshot")
    p.add_argument("src", type=Path)
    p.add_argument("dst", type=Path)
    p = sub.add_parser("label")
    p.add_argument("data", type=Path)
    p.add_argument("--queries", type=Path, default=QUERIES)
    p = sub.add_parser("run")
    p.add_argument("data", type=Path)
    p.add_argument("--queries", type=Path, default=QUERIES)
    p.add_argument("--legacy", action="store_true", help="ask as the code before did")
    p.add_argument("--show", action="store_true", help="each query's fused MRR and hit@10")
    p.add_argument("--json", type=Path, help="also write the numbers here")
    p = sub.add_parser("migrate")
    p.add_argument("data", type=Path)
    a = ap.parse_args()
    if a.cmd == "snapshot":
        snapshot(a.src.expanduser(), a.dst.expanduser())
    elif a.cmd == "label":
        label(a.data.expanduser(), a.queries)
    elif a.cmd == "run":
        result = run(a.data.expanduser(), a.queries, a.legacy, a.show)
        print(table(result))
        if a.json:
            a.json.write_text(json.dumps(result, indent=1) + "\n", encoding="utf-8")
    else:
        migrate(a.data.expanduser())


if __name__ == "__main__":
    main()
