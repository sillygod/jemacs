import os
import tempfile

os.environ.setdefault("GHOSTHERD_MEMORY_FAKE_EMBED", "1")
# A test that reaches `get_herd()' without setting GHOSTHERD_MEMORY_DIR would
# otherwise fall back to config.py's default -- the real
# ~/.emacs.d/ghostherd-memory -- and write a DB there (that is how a stray
# herd.sqlite once appeared at the repo root).  Point the default at a
# throwaway dir instead, so no test writes a DB or log under the user's home;
# a test that wants its own still sets the env itself.
os.environ.setdefault("GHOSTHERD_MEMORY_DIR",
                      tempfile.mkdtemp(prefix="ghostherd-test-"))
