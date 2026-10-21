"""The shared-memory store: facts as files, an index the herd generates."""

import os

import pytest

import shared_memory as sm


@pytest.fixture
def home(tmp_path, monkeypatch):
    monkeypatch.setenv("GHOSTHERD_SHARED_MEMORY_DIR", str(tmp_path / "memory"))
    return tmp_path / "memory"


def test_slug_matches_claudes_scheme():
    assert sm.slug_for_path("/Users/jing/.emacs.d") == "-Users-jing--emacs-d"
    assert (sm.slug_for_path("~/Downloads/works/unno/deploy-platform")
            == os.path.expanduser("~").replace("/", "-").replace(".", "-")
            + "-Downloads-works-unno-deploy-platform")
    # The herd sends an abbreviated path with a trailing slash; same slug.
    assert sm.slug_for_path("~/.emacs.d/") == sm.slug_for_path(
        os.path.expanduser("~/.emacs.d"))


def test_write_and_parse_round_trips(home):
    store = sm.project_store("proj")
    out = sm.write_fact(store, "proj", "Never commit to main.\nUse a branch.",
                        type="feedback", author="claude-x", now="2026-10-21T00:00:00Z")
    assert out["name"] == "never-commit-to-main"
    f = sm.parse(store / "never-commit-to-main.md")
    assert f.type == "feedback" and f.author == "claude-x"
    assert f.description == "Never commit to main."
    assert "Use a branch." in f.body
    assert f.created == "2026-10-21T00:00:00Z" == f.updated


def test_a_second_write_keeps_created_updates_updated(home):
    store = sm.project_store("proj")
    sm.write_fact(store, "proj", "first", name="n", now="2026-10-01T00:00:00Z")
    sm.write_fact(store, "proj", "second body", name="n", now="2026-10-21T00:00:00Z")
    f = sm.parse(store / "n.md")
    assert f.created == "2026-10-01T00:00:00Z"
    assert f.updated == "2026-10-21T00:00:00Z"
    assert "second body" in f.body


def test_auto_name_handles_cjk(home):
    store = sm.project_store("proj")
    out = sm.write_fact(store, "proj", "永不直接推送到主分支")  # no ASCII words
    assert out["name"].startswith("note-")
    assert (store / (out["name"] + ".md")).exists()


def test_project_index_lists_own_facts(home):
    store = sm.project_store("proj")
    sm.write_fact(store, "proj", "Alpha fact body", name="alpha", now="t")
    sm.write_fact(store, "proj", "Beta fact body", name="beta", now="t")
    idx = (store / "MEMORY.md").read_text()
    assert "## This project" in idx
    assert "- [Alpha](alpha.md)" in idx and "- [Beta](beta.md)" in idx


def test_user_facts_are_shared_into_every_project(home):
    proj = sm.project_store("proj")
    sm.write_fact(proj, "proj", "project-local thing", name="local", now="t")
    # A user fact reaches the project store as a symlink and in its index.
    sm.write_fact(sm.user_store(), "user", "Prefers reasoned design.",
                  name="prefers-design", type="user", author="user", now="t")
    link = proj / "prefers-design.md"
    assert link.is_symlink()
    assert os.readlink(link) == os.path.join("..", "user", "prefers-design.md")
    assert sm.parse(link).description == "Prefers reasoned design."
    idx = (proj / "MEMORY.md").read_text()
    assert "## Shared (every project)" in idx
    assert "- [Prefers design](prefers-design.md)" in idx
    assert "## This project" in idx and "- [Local](local.md)" in idx


def test_forget_removes_and_refuses_a_shared_link(home):
    proj = sm.project_store("proj")
    sm.write_fact(proj, "proj", "x", name="own", now="t")
    sm.write_fact(sm.user_store(), "user", "u", name="shared", type="user", now="t")
    assert sm.forget(proj, "proj", "own")["removed"] is True
    assert not (proj / "own.md").exists()
    # The shared symlink in the project store cannot be forgotten there.
    with pytest.raises(ValueError):
        sm.forget(proj, "proj", "shared")
    # Forgetting the user fact removes it and its links everywhere.
    assert sm.forget(sm.user_store(), "user", "shared")["removed"] is True
    assert not (proj / "shared.md").exists()


def test_recall_index_then_full_facts(home):
    store = sm.project_store("proj")
    sm.write_fact(store, "proj", "The retry scheduler lives in worker.py", name="retry", now="t")
    sm.write_fact(store, "proj", "Unrelated note about colours", name="colour", now="t")
    index = sm.recall(store, "proj")
    assert "retry" in index["index"] and index["count"] == 2
    hit = sm.recall(store, "proj", "retry scheduler")
    assert hit["count"] == 1 and hit["names"] == ["retry"]
    assert "worker.py" in hit["text"]


def test_regenerate_loses_nothing_after_a_stray_append(home):
    store = sm.project_store("proj")
    sm.write_fact(store, "proj", "kept fact", name="kept", now="t")
    # claude's harness appends its own pointer line; a regenerate replaces the
    # index but the fact file -- the truth -- is still listed.
    with open(store / "MEMORY.md", "a") as fh:
        fh.write("- [Stray](stray.md) — a line with no file\n")
    sm.regenerate(store, "proj")
    idx = (store / "MEMORY.md").read_text()
    assert "- [Kept](kept.md)" in idx       # the real fact survived
    assert "stray.md" not in idx            # the fileless line is gone


def test_a_claude_written_fact_parses_without_author(home):
    store = sm.project_store("proj")
    store.mkdir(parents=True)
    (store / "c.md").write_text(
        '---\nname: c\ndescription: "a claude fact"\nmetadata:\n'
        '  node_type: memory\n  type: project\n'
        '  originSessionId: abc-123\n  modified: 2026-08-12T05:16:17.457Z\n---\n\nBody.\n')
    f = sm.parse(store / "c.md")
    assert f.name == "c" and f.type == "project" and f.author == ""
    assert f.updated == "2026-08-12T05:16:17.457Z"  # falls back to modified
    # Rewriting it keeps claude's originSessionId.
    sm.write_fact(store, "proj", "new body", name="c", author="claude-x", now="t")
    text = (store / "c.md").read_text()
    assert "originSessionId: abc-123" in text and "author: claude-x" in text


def test_claude_symlink_points_a_worktree_at_its_main_store(home, tmp_path):
    claude = tmp_path / "claude"
    main = "/work/proj"
    wt = "/work/worktrees/proj/feat"
    out = sm.ensure_project_symlink(claude, wt, main)
    assert out["status"] == "linked"
    link = claude / sm.slug_for_path(wt) / "memory"
    assert link.is_symlink()
    # the worktree's claude dir points at the MAIN project's store
    assert os.path.realpath(link) == os.path.realpath(sm.project_store(sm.slug_for_path(main)))
    # idempotent
    assert sm.ensure_project_symlink(claude, wt, main)["status"] == "ok"


def test_claude_symlink_leaves_a_real_dir_with_facts(home, tmp_path):
    claude = tmp_path / "claude"
    main = "/work/proj"
    cmem = claude / sm.slug_for_path(main) / "memory"
    cmem.mkdir(parents=True)
    (cmem / "x.md").write_text("---\nname: x\ndescription: \"d\"\nmetadata:\n  type: project\n---\n\nbody\n")
    out = sm.ensure_project_symlink(claude, main, main)
    assert out["status"] == "has-content" and not cmem.is_symlink()


def test_claude_symlink_replaces_an_empty_real_dir(home, tmp_path):
    claude = tmp_path / "claude"
    main = "/work/proj"
    cmem = claude / sm.slug_for_path(main) / "memory"
    cmem.mkdir(parents=True)
    out = sm.ensure_project_symlink(claude, main, main)
    assert out["status"] == "linked" and cmem.is_symlink()


def test_stamp_author_fills_a_missing_author(home):
    store = sm.project_store("proj")
    store.mkdir(parents=True)
    (store / "c.md").write_text(
        '---\nname: c\ndescription: "d"\nmetadata:\n  node_type: memory\n'
        '  type: project\n  originSessionId: abc\n  modified: 2026-01-01T00:00:00Z\n---\n\nBody.\n')
    assert sm.stamp_author(store / "c.md", "claude-x") is True
    f = sm.parse(store / "c.md")
    assert f.author == "claude-x" and "Body." in f.body
    assert "originSessionId: abc" in (store / "c.md").read_text()  # claude's provenance kept
    # idempotent: a fact that already has an author is left alone
    assert sm.stamp_author(store / "c.md", "claude-y") is False
    assert sm.parse(store / "c.md").author == "claude-x"
    # never follows a shared symlink
    (store / "u.md").symlink_to("../user/u.md")
    assert sm.stamp_author(store / "u.md", "claude-x") is False
