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
from typing import Iterable

from config import Config

TOKEN = re.compile(r"[\w\u4e00-\u9fff]+", re.UNICODE)

FAKE_DIM = 384
FAKE_SPARSE_MOD = 4096


def tokenize(text: str) -> list[str]:
    return [t.lower() for t in TOKEN.findall(text or "")]


class Embedder:
    def __init__(self, cfg: Config):
        self.cfg = cfg
        self.fake = cfg.fake_embed
        self.model = cfg.embed_model
        self.sparse_model = cfg.sparse_model
        self.dim = FAKE_DIM if self.fake else 0
        self._dense = None
        self._sparse = None
        self.loaded = False

    def load(self) -> None:
        if self.loaded:
            return
        if self.fake:
            self.dim = FAKE_DIM
            self.loaded = True
            return
        from fastembed import SparseTextEmbedding, TextEmbedding

        threads = _thread_count(self.cfg.embed_threads)
        providers = _onnx_providers(self.cfg.onnx_provider)
        print(
            f"[ghostherd-memory] loading dense model {self.model} "
            f"(threads={threads}, providers={providers or 'default'})…",
            flush=True,
        )
        self._dense = _make_text_embedding(self.model, threads, providers)
        print(f"[ghostherd-memory] loading sparse model {self.sparse_model}…", flush=True)
        self._sparse = SparseTextEmbedding(
            model_name=self.sparse_model, threads=threads
        )
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
        self.load()
        items = list(texts)
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


def _thread_count(configured: int) -> int:
    if configured and configured > 0:
        return configured
    n = os.cpu_count() or 4
    return max(1, n - 1)


def _onnx_providers(choice: str) -> list[str] | None:
    choice = (choice or "auto").lower()
    if choice in ("cpu", "none", ""):
        return None
    if choice == "coreml" or (choice == "auto" and sys.platform == "darwin"):
        return ["CoreMLExecutionProvider", "CPUExecutionProvider"]
    return None


def _make_text_embedding(model: str, threads: int, providers: list[str] | None):
    from fastembed import TextEmbedding

    if not providers:
        return TextEmbedding(model_name=model, threads=threads)
    try:
        return TextEmbedding(
            model_name=model, threads=threads, providers=providers
        )
    except Exception as exc:  # noqa: BLE001 — CoreML rejects some graphs
        print(
            f"[ghostherd-memory] {providers[0]} failed ({exc}); CPU only",
            flush=True,
        )
        return TextEmbedding(model_name=model, threads=threads)


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
