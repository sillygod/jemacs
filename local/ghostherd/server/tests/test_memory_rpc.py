"""The memory RPCs resolve the caller's project and drive the store."""

import pytest

import shared_memory as sm
from herd import get_herd, reset_herd
from jsonrpc_handler import JsonRpcHandler


@pytest.fixture
def mem(tmp_path, monkeypatch):
    monkeypatch.setenv("GHOSTHERD_SHARED_MEMORY_DIR", str(tmp_path / "mem"))
    reset_herd()
    herd = get_herd()
    herd.tick(sessions=[
        {"name": "claude-proj", "kind": "claude", "state": "idle", "project": "~/work/proj"},
        {"name": "agy-proj", "kind": "agy", "state": "idle", "project": "~/work/proj"},
    ])
    yield JsonRpcHandler()
    reset_herd()


def test_a_session_writes_to_its_project_store(mem):
    out = mem._herd_remember({"from": "claude-proj", "text": "Never push to main.",
                              "type": "feedback"})
    assert out["store"] == sm.slug_for_path("~/work/proj")
    assert out["name"] == "never-push-to-main" and out["type"] == "feedback"
    # Another CLI in the same project recalls it.
    got = mem._herd_recall({"from": "agy-proj", "query": "push main"})
    assert got["count"] == 1 and "Never push to main." in got["text"]


def test_recall_without_query_is_the_index(mem):
    mem._herd_remember({"from": "claude-proj", "text": "A fact", "name": "a"})
    out = mem._herd_recall({"from": "claude-proj"})
    assert "# Memory" in out["index"] and "- [A](a.md)" in out["index"]


def test_user_store_is_shared_and_separate(mem):
    mem._herd_remember({"from": "claude-proj", "text": "Prefers reasoned design.",
                        "name": "prefers", "type": "user", "user": True})
    # It lands in the user store, and shows up in the project's recall.
    assert (sm.user_store() / "prefers.md").exists()
    got = mem._herd_recall({"from": "agy-proj"})
    assert "Shared (every project)" in got["index"] and "prefers.md" in got["index"]


def test_forget_removes(mem):
    mem._herd_remember({"from": "claude-proj", "text": "x", "name": "doomed"})
    out = mem._herd_forget({"from": "claude-proj", "name": "doomed"})
    assert out["removed"] is True
    assert mem._herd_forget({"from": "claude-proj", "name": "doomed"})["removed"] is False


def test_a_plain_shell_uses_its_cwd_project(mem):
    # No session: the project hint from the shell's cwd is used.
    out = mem._herd_remember({"from": "user", "project": "~/work/other", "text": "y", "name": "y"})
    assert out["store"] == sm.slug_for_path("~/work/other")
    assert (sm.project_store(sm.slug_for_path("~/work/other")) / "y.md").exists()


def test_no_project_and_not_user_is_an_error(mem):
    with pytest.raises(ValueError):
        mem._herd_remember({"from": "user", "project": "", "text": "z"})


def test_a_bad_type_is_refused(mem):
    with pytest.raises(ValueError):
        mem._herd_remember({"from": "claude-proj", "text": "z", "type": "nonsense"})
