import os
import tempfile

# ONNX Runtime's telemetry off before a test file's own `import
# qdrant_client' loads onnxruntime: a run that ended while its post was out
# aborted at exit, every test passed (onnx_telemetry.py).
import onnx_telemetry  # noqa: F401

os.environ.setdefault("GHOSTHERD_MEMORY_FAKE_EMBED", "1")
# A test that reaches `get_herd()' without setting GHOSTHERD_MEMORY_DIR would
# otherwise fall back to config.py's default -- the real
# ~/.emacs.d/ghostherd-memory -- and write a DB there (that is how a stray
# herd.sqlite once appeared at the repo root).  Point the default at a
# throwaway dir instead, so no test writes a DB or log under the user's home;
# a test that wants its own still sets the env itself.
os.environ.setdefault("GHOSTHERD_MEMORY_DIR",
                      tempfile.mkdtemp(prefix="ghostherd-test-"))
# The same for the shared-memory store: its default is the real
# ~/.ghostherd/memory, and an ask now reads its taker's project store for
# the memory it carries.  A test that wants a store of its own sets it.
os.environ.setdefault("GHOSTHERD_SHARED_MEMORY_DIR",
                      tempfile.mkdtemp(prefix="ghostherd-test-memory-"))
# The images replies name are copied under GHOSTHERD_IMAGES_DIR: never the
# user's ~/.emacs.d/ghostherd-images.
os.environ.setdefault("GHOSTHERD_IMAGES_DIR",
                      tempfile.mkdtemp(prefix="ghostherd-test-images-"))
# The checks' rules (bin/herd_lint.py): never the user's ~/.ghostherd/lint.
os.environ.setdefault("GHOSTHERD_LINT_DIR",
                      tempfile.mkdtemp(prefix="ghostherd-test-lint-"))
# Never the user's Telegram bot.  A herd agent's environment has its token
# and chats, from the Emacs that started it, and every `TestClient(app)'
# runs the sidecar's lifespan, which starts the bot when both are set: a
# full run long-polled the real bot ~60 times (ask 72535795), taking what
# the live sidecar should have had.  A test that wants a bot makes its own.
os.environ.pop("GHOSTHERD_TELEGRAM_TOKEN", None)
os.environ.pop("GHOSTHERD_TELEGRAM_CHAT_IDS", None)


import pytest  # noqa: E402


@pytest.fixture(autouse=True)
def _no_tally_passes(monkeypatch):
    """No test's tick starts a tally pass of its own (herd.py `_tally_soon'):
    it would read whatever transcript a test's links name.  A test that
    wants one runs it itself (`_tally_pass'), its HOME a temp dir."""
    import tally
    monkeypatch.setattr(tally, "EVERY", float("inf"))
