"""Migrating claude memories into the shared store: backups, symlink, revert."""

import os

import pytest

import memory_migrate as mig
import shared_memory as sm

FACT = ('---\nname: {n}\ndescription: "{d}"\nmetadata:\n  node_type: memory\n'
        '  type: {t}\n  originSessionId: s-{n}\n  modified: 2026-01-01T00:00:00.0Z\n'
        '---\n\n{b}\n')


@pytest.fixture
def world(tmp_path, monkeypatch):
    monkeypatch.setenv("GHOSTHERD_SHARED_MEMORY_DIR", str(tmp_path / "ghostherd"))
    claude = tmp_path / "claude"
    proj = tmp_path / "work" / "proj"
    proj.mkdir(parents=True)
    slug = sm.slug_for_path(str(proj))
    cmem = claude / slug / "memory"
    cmem.mkdir(parents=True)
    (cmem / "branch.md").write_text(FACT.format(n="branch", d="never commit to main",
                                                t="feedback", b="Use a branch."))
    (cmem / "retry.md").write_text(FACT.format(n="retry", d="retry lives in worker",
                                               t="project", b="worker.py."))
    (cmem / "prefers.md").write_text(FACT.format(n="prefers", d="likes reasoned design",
                                                 t="feedback", b="Design, do not menu."))
    (cmem / "MEMORY.md").write_text("- [old index](branch.md)\n")
    return {"claude": claude, "proj": str(proj), "slug": slug, "cmem": cmem}


def test_plan_counts_without_touching(world):
    rep = mig.plan([world["proj"]], claude_root=world["claude"], user_names={"prefers"})
    e = rep["projects"][0]
    assert e["status"] == "would-migrate" and e["moved"] == 3
    assert e["to_user"] == ["prefers"]
    # nothing moved
    assert world["cmem"].is_dir() and not world["cmem"].is_symlink()
    assert not (sm.user_store() / "prefers.md").exists()


def test_migrate_backs_up_symlinks_and_routes(world):
    rep = mig.migrate([world["proj"]], claude_root=world["claude"],
                      user_names={"prefers"}, stamp="2026-10-21")
    e = rep["projects"][0]
    assert e["status"] == "migrated" and e["moved"] == 3 and e["to_user"] == ["prefers"]
    # backup kept, original gone, symlink in its place
    bak = world["claude"] / world["slug"] / "memory.bak-2026-10-21"
    assert bak.is_dir() and (bak / "branch.md").exists()
    assert world["cmem"].is_symlink()
    store = sm.project_store(world["slug"])
    assert os.path.realpath(world["cmem"]) == os.path.realpath(store)
    # project facts are in the project store, verbatim; prefers went to user
    assert (store / "branch.md").read_text() == (bak / "branch.md").read_text()
    assert (store / "retry.md").is_file() and not (store / "retry.md").is_symlink()
    assert (sm.user_store() / "prefers.md").read_text() == (bak / "prefers.md").read_text()
    # claude sees prefers through the store's shared symlink, not a real file
    assert (store / "prefers.md").is_symlink()
    # the index is generated
    assert "# Memory" in (store / "MEMORY.md").read_text()


def test_verify_passes_after_migrate(world):
    mig.migrate([world["proj"]], claude_root=world["claude"], user_names={"prefers"})
    v = mig.verify([world["proj"]], claude_root=world["claude"], user_names={"prefers"})
    assert v["ok"] is True
    assert v["projects"][0]["checked"] == 3 and v["projects"][0]["problems"] == []


def test_migrate_is_idempotent(world):
    mig.migrate([world["proj"]], claude_root=world["claude"], user_names={"prefers"})
    again = mig.migrate([world["proj"]], claude_root=world["claude"], user_names={"prefers"})
    assert again["projects"][0]["status"] == "already"
    # only one backup
    baks = list((world["claude"] / world["slug"]).glob("memory.bak-*"))
    assert len(baks) == 1


def test_unmigrate_restores(world):
    mig.migrate([world["proj"]], claude_root=world["claude"], user_names={"prefers"})
    rep = mig.unmigrate([world["proj"]], claude_root=world["claude"])
    assert rep["projects"][0]["status"] == "restored"
    assert world["cmem"].is_dir() and not world["cmem"].is_symlink()
    assert (world["cmem"] / "branch.md").exists()
    assert not list((world["claude"] / world["slug"]).glob("memory.bak-*"))


def test_dedupe_keeps_the_existing_user_fact(world):
    # a user fact already there is not overwritten
    sm.user_store().mkdir(parents=True)
    (sm.user_store() / "prefers.md").write_text("KEEP ME")
    rep = mig.migrate([world["proj"]], claude_root=world["claude"], user_names={"prefers"})
    assert rep["projects"][0]["deduped"] == ["prefers"]
    assert (sm.user_store() / "prefers.md").read_text() == "KEEP ME"
