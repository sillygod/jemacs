"""The embedder's thread cap keeps an import's ONNX batch off most of the
machine (ask 2b76d37d): FastEmbed sets ONNX intra_op_num_threads to it, so a
batch embed burns that many cores.  Two at most, unless configured."""

import os
import subprocess
import sys
from pathlib import Path

import pytest

import embedder as emb
from config import Config

SERVER = Path(__file__).resolve().parent.parent

# The cap values themselves are pinned in test_store.py
# (test_embedding_takes_a_few_cores); here we prove the cap reaches the model.


@pytest.mark.parametrize("module", ["main", "store", "fact_index", "sparse_migrate", "embedder"])
def test_onnx_runtimes_telemetry_never_starts(module, tmp_path):
    """ONNX Runtime posts telemetry ~10 s after it loads, and a process that
    exits while the post is out aborts (onnx_telemetry.py, ask 72535795).
    The sidecar, and each module that loads qdrant_client or fastembed,
    turns it off first: onnxruntime makes no telemetry store under HOME
    (with it on, `Library/Application Support/Microsoft/DeveloperTools/
    .onnxruntime' -- ~/.cache/... on Linux -- holds a device ID and the
    events to post).  A fresh process each: onnxruntime reads the switch
    once, when it loads, and this one is loaded already."""
    env = {k: v for k, v in os.environ.items() if k != "ORT_DISABLE_TELEMETRY"}
    env.update(HOME=str(tmp_path), XDG_CACHE_HOME=str(tmp_path / ".cache"))
    subprocess.run([sys.executable, "-c", f"import {module}, fastembed"],
                   cwd=SERVER, env=env, check=True, timeout=120)
    assert not list(tmp_path.rglob(".onnxruntime"))


def test_load_passes_the_capped_count_to_both_models(monkeypatch):
    """The dense and sparse models are built with the capped thread count --
    which becomes ONNX Runtime's intra_op_num_threads."""
    import fastembed

    captured = {}

    def fake_make(model, threads, providers):
        captured["dense"] = threads

        class _M:
            def embed(self, items, **k):
                return iter([[0.1, 0.2, 0.3]])

        return _M()

    class _FakeSparse:
        def __init__(self, model_name, threads):
            captured["sparse"] = threads

    monkeypatch.setattr(emb, "_make_text_embedding", fake_make)
    monkeypatch.setattr(fastembed, "SparseTextEmbedding", _FakeSparse)

    e = emb.Embedder(Config(fake_embed=False, embed_threads=0))
    e.load()
    assert captured["dense"] == emb._thread_count(0) <= 2
    assert captured["sparse"] == captured["dense"]
    assert e.dim == 3                                 # from the probe vector


def test_the_models_run_without_onnx_runtimes_arena(monkeypatch):
    """ONNX Runtime's arena kept what the biggest batch asked for, and grew:
    the sidecar's 10 GB (ask 18e8b5dd).  Every dense model is made without
    it, and a batch is 16 chunks."""
    import fastembed

    made = []

    class _Model:
        def __init__(self, model_name, threads, **kw):
            made.append(kw)

    monkeypatch.setattr(fastembed, "TextEmbedding", _Model)
    emb._make_text_embedding("m", 2, None)
    emb._make_text_embedding("m", 2, ["CPUExecutionProvider"])
    assert made and all(kw.get("enable_cpu_mem_arena") is False for kw in made)
    assert Config().embed_batch == 16
