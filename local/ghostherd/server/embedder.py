"""Dense + sparse embeddings.

FastEmbed loads slowly, which is why this lives in a persistent process
rather than a one-shot script.  Tests set GHOSTHERD_MEMORY_FAKE_EMBED so
they never download a model: fake vectors are hashed bags of tokens,
good enough for overlap-based retrieval.
"""

from __future__ import annotations

import hashlib
import math
import os
import re
import sys
import threading
from typing import Iterable

import onnx_telemetry  # noqa: F401 -- before fastembed loads onnxruntime
from config import Config
from lexical import segment

TOKEN = re.compile(r"[\w\u4e00-\u9fff]+", re.UNICODE)

FAKE_DIM = 384
FAKE_SPARSE_MOD = 4096


def tokenize(text: str) -> list[str]:
    return [t.lower() for t in TOKEN.findall(text or "")]


class Embedder:
    def __init__(self, cfg: Config, model: str | None = None):
        "MODEL overrides cfg.embed_model -- the facts use their own."
        self.cfg = cfg
        self.fake = cfg.fake_embed
        self.model = model or cfg.embed_model
        self.sparse_model = cfg.sparse_model
        self.dim = FAKE_DIM if self.fake else 0
        self._dense = None
        self._sparse = None
        self.loaded = False
        # The import, a recall and the fact index's worker may all ask at
        # once; the model is loaded once.
        self._load_lock = threading.Lock()

    def load(self) -> None:
        if self.loaded:
            return
        self.load_sparse()
        with self._load_lock:
            if not self.loaded:
                self._load()

    def load_sparse(self) -> None:
        """The sparse model alone: BM25 is a word count, no ONNX -- what
        sparse_migrate needs, without the dense model's load and memory."""
        if self.fake or self._sparse is not None:
            return
        with self._load_lock:
            if self._sparse is None:
                from fastembed import SparseTextEmbedding

                print(f"[ghostherd-memory] loading sparse model {self.sparse_model}…", flush=True)
                self._sparse = SparseTextEmbedding(
                    model_name=self.sparse_model,
                    threads=_thread_count(self.cfg.embed_threads),
                )

    def _load(self) -> None:
        if self.fake:
            self.dim = FAKE_DIM
            self.loaded = True
            return
        threads = _thread_count(self.cfg.embed_threads)
        providers = _onnx_providers(self.cfg.onnx_provider)
        print(
            f"[ghostherd-memory] loading dense model {self.model} "
            f"(threads={threads}, providers={providers or 'default'})…",
            flush=True,
        )
        self._dense = _make_text_embedding(self.model, threads, providers)
        probe = next(self._dense.embed(["ping"]))
        self.dim = int(len(probe))
        self.loaded = True
        print(f"[ghostherd-memory] embedder ready (dim={self.dim})", flush=True)

    def dense(self, texts: Iterable[str]) -> list[list[float]]:
        self.load()
        items = list(texts)
        if not items:
            return []
        if self.fake:
            return [_fake_dense(t) for t in items]
        out = []
        for vec in self._dense.embed(items, batch_size=max(1, self.cfg.embed_batch)):
            out.append(vec.tolist() if hasattr(vec, "tolist") else list(map(float, vec)))
        return out

    def sparse(self, texts: Iterable[str]) -> list[tuple[list[int], list[float]]]:
        """BM25 term weights of indexed TEXTS, their CJK cut into words
        (lexical).  The words' rarity is Qdrant's to add: the collection
        weighs them by IDF (store.SPARSE)."""
        self.load_sparse()
        items = [segment(t) for t in texts]
        if not items:
            return []
        if self.fake:
            return [_fake_sparse(t) for t in items]
        out = []
        for vec in self._sparse.embed(items, batch_size=max(1, self.cfg.embed_batch)):
            indices = [int(i) for i in vec.indices]
            values = [float(v) for v in vec.values]
            out.append((indices, values))
        return out

    def sparse_query(self, text: str) -> tuple[list[int], list[float]]:
        """A query's words, each once at weight 1 -- BM25's query side, not
        a document's term frequencies, which `sparse' gives."""
        self.load_sparse()
        text = segment(text)
        if self.fake:
            if not tokenize(text):
                return [], []
            indices, _ = _fake_sparse(text)
            return indices, [1.0] * len(indices)
        vec = next(iter(self._sparse.query_embed(text)))
        return [int(i) for i in vec.indices], [float(v) for v in vec.values]


def _thread_count(configured: int) -> int:
    """Two at most, unless configured (GHOSTHERD_MEMORY_THREADS).  FastEmbed
    sets ONNX Runtime's intra_op_num_threads to this, so an import's batch
    embedding burns this many cores -- a visible burst while the rest of the
    machine idles.  Past two, bge-small-zh barely speeds up for twice the
    burst: measured on this M-series Mac, a 128-chunk batch ran 13 chunk/s
    at four threads (3.3 cores, ~335%) and 11 chunk/s at two (1.9 cores,
    ~190%) -- 15% slower for half the peak -- while a recall query stayed
    ~2-6 ms either way (one short query, thread count hardly matters).  So
    two keeps the import off most of the machine without slowing search."""
    if configured and configured > 0:
        return configured
    n = os.cpu_count() or 4
    return max(1, min(2, n - 1))


def _onnx_providers(choice: str) -> list[str] | None:
    "CPU unless CoreML is asked for: it took 11 s where the CPU took 9."
    choice = (choice or "auto").lower()
    if choice == "coreml" and sys.platform == "darwin":
        return ["CoreMLExecutionProvider", "CPUExecutionProvider"]
    return None


# ONNX Runtime's CPU memory arena keeps the most any run asked for, and
# asks for more as batches change shape: one import's batch of 256 long
# chunks left the sidecar 3.1 GB bigger, a second 5.2 GB -- the 10 GB it
# reached in a day.  Without it a run's memory is given back when it ends:
# 7-23 MB kept, at the same speed (measured on bge-small-zh, ask 18e8b5dd).
ARENA = False


def _make_text_embedding(model: str, threads: int, providers: list[str] | None):
    from fastembed import TextEmbedding

    if not providers:
        return TextEmbedding(model_name=model, threads=threads, enable_cpu_mem_arena=ARENA)
    try:
        return TextEmbedding(
            model_name=model, threads=threads, providers=providers,
            enable_cpu_mem_arena=ARENA,
        )
    except Exception as exc:  # noqa: BLE001 — CoreML rejects some graphs
        print(
            f"[ghostherd-memory] {providers[0]} failed ({exc}); CPU only",
            flush=True,
        )
        return TextEmbedding(model_name=model, threads=threads, enable_cpu_mem_arena=ARENA)


def _fake_dense(text: str) -> list[float]:
    vec = [0.0] * FAKE_DIM
    toks = tokenize(text)
    if not toks:
        vec[0] = 1.0
        return vec
    for tok in toks:
        digest = hashlib.sha256(tok.encode("utf-8")).digest()
        idx = int.from_bytes(digest[:4], "little") % FAKE_DIM
        sign = 1.0 if digest[4] % 2 == 0 else -1.0
        vec[idx] += sign
    norm = math.sqrt(sum(x * x for x in vec)) or 1.0
    return [x / norm for x in vec]


def _fake_sparse(text: str) -> tuple[list[int], list[float]]:
    counts: dict[int, float] = {}
    for tok in tokenize(text):
        digest = hashlib.sha256(tok.encode("utf-8")).digest()
        idx = int.from_bytes(digest[:4], "little") % FAKE_SPARSE_MOD
        counts[idx] = counts.get(idx, 0.0) + 1.0
    if not counts:
        return [0], [1.0]
    indices = sorted(counts)
    values = [counts[i] for i in indices]
    return indices, values
