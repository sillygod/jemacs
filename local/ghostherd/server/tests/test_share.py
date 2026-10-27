"""A project fact made every project's in one approval (ask d197da21): the
lead proposes it, the user approves once, and the herd moves it to the user
store -- its project copy kept in the project's history, its check from the
project's rules file to user.json on or off as it was, its counts with it.
Refused for a fact the user store already has, or already says."""

import json
import os
import subprocess

import pytest

import herd as herd_mod
import shared_memory as sm
from herd import _lint_lib, get_herd, reset_herd
from jsonrpc_handler import JsonRpcHandler
from memory_store import KEY, PROJECT, SNAP, USER_RULE, store  # noqa: F401  (store fixture)
from tests.test_ask import HERD, live  # noqa: F401  (the live sidecar fixture)

FACT = "kctx-multi-cluster"            # a reference fact: a tool every project uses
RULED = "pod-env-grep-leaks-secrets"   # a feedback fact with a check
RULE = {"fact": RULED, "hint": "Ask for named variables.", "check": {"command": {"all": ["^env\\b"]}},
        "catches": ["env | grep TOKEN"], "passes": ["envsubst < a"], "status": "off",
        "author": "claude-dp", "approved_at": "2026-10-20T00:00:00Z", "changed_at": "2026-10-21T00:00:00Z"}
OTHER_RULE = {"fact": "branch-discipline", "hint": "h", "check": {"line": {"all": ["x"]}},
              "catches": ["x"], "passes": ["y"], "status": "on"}


@pytest.fixture
def rpc(store, tmp_path, monkeypatch):
    monkeypatch.setenv("GHOSTHERD_LINT_DIR", str(tmp_path / "lint"))
    reset_herd()
    get_herd().tick(sessions=[dict(s, role="lead") if s["name"] == "lead" else s for s in SNAP])
    yield JsonRpcHandler()
    reset_herd()


def _propose(rpc, name, **more):
    return rpc._herd_promote_propose(dict({"from": "lead", "facts": [name], "share": True}, **more))


def _rules(key):
    return _lint_lib().read_rules(key)


def test_a_fact_moves_to_every_project_in_one_approval(rpc, store, tmp_path):
    h = get_herd()
    lint = _lint_lib()
    lint.write_rules(KEY, [RULE, OTHER_RULE])
    # Its history: an earlier text.
    before = sm.parse(store / f"{RULED}.md")
    sm.write_fact(store, KEY, before.body + "\n\nAlso: never `printenv`.", type="feedback", name=RULED,
                  author="claude-dp")
    said = sm.parse(store / f"{RULED}.md")
    # Its counts: uses of each kind, and its check's denials.
    h.use(KEY, [RULED], "ask", agent="claude-dp", ask="a1")
    h.use(KEY, [RULED], "recall", agent="claude-dp", how="name")
    h.lint_denied(KEY, RULED, "claude-dp", "claude", "env | grep TOKEN")
    with h._mu:
        h._flush_uses()
        h._conn.commit()
    # Another project, which gets it too.
    other = sm.project_store("-work-other")
    sm.write_fact(other, "-work-other", "The other project's own fact.", name="other-fact")

    out = _propose(rpc, RULED)
    assert out["kind"] == "share" and out["facts"] == [RULED] and out["file"] == "user"
    # What the user sees: the fact's text, and its check, which moves with it.
    (p,) = rpc._herd_pending_proposals({})["proposals"]
    assert (p["kind"], p["diff"], p["author"]) == ("share", said.body, "lead")
    assert p["facts"][0] == {"name": RULED, "title": said.title, "gone": False, "promoted": False,
                             "description": said.description, "check": "off"}
    assert rpc._herd_memory_proposals({"key": KEY})["proposals"][0]["id"] == out["id"]

    got = rpc._herd_memory_promote_approve({"id": out["id"]})
    assert got == {"id": out["id"], "status": "applied", "kind": "share", "fact": RULED, "key": KEY,
                   "check": "off"}

    # The fact is the user store's, whole; here, its link -- and the other project's.
    moved = sm.parse(sm.user_store() / f"{RULED}.md")
    assert (moved.body, moved.type, moved.author, moved.created, moved.description) == (
        said.body, "feedback", "claude-dp", said.created, said.description)
    for s in (store, other):
        link = s / f"{RULED}.md"
        assert link.is_symlink() and os.readlink(link) == os.path.join("..", "user", f"{RULED}.md")
    assert RULED in (sm.user_store() / "MEMORY.md").read_text()
    # Its project copy kept as a forget keeps it; its earlier texts went with it.
    kept = sm.history(store, RULED)
    assert kept[0]["how"] == "moved to the user store" and kept[0]["body"] == said.body
    assert [x["body"] for x in sm.history(sm.user_store(), RULED)] == [before.body]
    assert sm.was_line(moved, sm.user_store()).startswith("was: ")
    # Its check: in user.json, off as it was; out of the project's file, the rest left.
    (r,) = [x for x in _rules(sm.USER_KEY) if x["fact"] == RULED]
    assert r["status"] == "off" and r["moved_from"] == KEY and r["approved_at"] == RULE["approved_at"]
    assert [x["fact"] for x in _rules(KEY)] == ["branch-discipline"]
    # Its counts, under the user store.
    with h._mu:
        keys = {k for (k,) in h._conn.execute("SELECT key FROM fact_uses WHERE fact = ?", (RULED,))}
        totals = h._conn.execute("SELECT key, kind, n FROM fact_use_totals WHERE fact = ? ORDER BY kind",
                                 (RULED,)).fetchall()
    assert keys == {sm.USER_KEY}
    assert totals == [(sm.USER_KEY, "ask", 1), (sm.USER_KEY, "recall", 1)]
    assert h.lint_counts(sm.USER_KEY)[RULED]["count"] == 1 and RULED not in h.lint_counts(KEY)
    listed = {f["name"]: f for f in rpc._herd_memory_list({"key": KEY})["facts"]}
    assert listed[RULED]["shared"] and listed[RULED]["uses"]["ask"] == 1
    # Once.
    with pytest.raises(ValueError):
        rpc._herd_memory_promote_approve({"id": out["id"]})
    assert h.proposal(out["id"])["status"] == "applied"


def test_a_fact_with_no_check_leaves_the_rules_files_alone(rpc, store, tmp_path):
    out = _propose(rpc, FACT)
    assert rpc._herd_pending_proposals({})["proposals"][0]["facts"][0]["check"] is None
    assert rpc._herd_memory_promote_approve({"id": out["id"]})["check"] is None
    assert (sm.user_store() / f"{FACT}.md").is_file() and (store / f"{FACT}.md").is_symlink()
    assert not (tmp_path / "lint").exists()


def test_moving_is_refused_for_a_fact_the_user_store_has_or_says(rpc, store):
    # Its name is a user fact's.
    sm.write_fact(sm.user_store(), sm.USER_KEY, "Something else entirely.", name=FACT)
    with pytest.raises(ValueError, match=f"already has a fact named {FACT}"):
        _propose(rpc, FACT)
    # It says what a user fact says.
    sm.write_fact(store, KEY, USER_RULE[1], type="feedback", name="design-proposals", new=True)
    with pytest.raises(ValueError, match=f"already says this, as `{USER_RULE[0]}'"):
        _propose(rpc, "design-proposals")
    # A user fact itself, seen through the project's link; and from the user store.
    with pytest.raises(ValueError, match="every project has it already"):
        _propose(rpc, USER_RULE[0])
    with pytest.raises(ValueError, match="every project has it already"):
        _propose(rpc, USER_RULE[0], user=True)
    # Promoted into an instruction file; not there; two at once.
    sm.set_promoted(store, KEY, "clickup-ticket-upkeep", "CLAUDE.md")
    with pytest.raises(ValueError, match="promoted into CLAUDE.md"):
        _propose(rpc, "clickup-ticket-upkeep")
    with pytest.raises(ValueError, match="no fact named nope"):
        _propose(rpc, "nope")
    with pytest.raises(ValueError, match="one fact at a time"):
        rpc._herd_promote_propose({"from": "lead", "facts": [FACT, RULED], "share": True})
    assert get_herd().proposals() == []


def test_approving_checks_again_and_moves_nothing_when_refused(rpc, store, tmp_path):
    out = _propose(rpc, FACT)
    # Meanwhile the user store came to say it, under a name of its own.
    sm.write_fact(sm.user_store(), sm.USER_KEY, sm.parse(store / f"{FACT}.md").body,
                  type="reference", name="kctx-everywhere")
    with pytest.raises(ValueError, match="already says this, as `kctx-everywhere'"):
        rpc._herd_memory_promote_approve({"id": out["id"]})
    assert not (store / f"{FACT}.md").is_symlink() and not (sm.user_store() / f"{FACT}.md").exists()
    assert get_herd().proposal(out["id"])["status"] == "pending"
    # Or by its very name: the move itself refuses.
    (sm.user_store() / "kctx-everywhere.md").rename(sm.user_store() / f"{FACT}.md")
    with pytest.raises(ValueError, match=f"already has a fact named {FACT}"):
        rpc._herd_memory_promote_approve({"id": out["id"]})


def test_a_rules_file_that_cannot_be_read_stops_the_move_before_it_starts(rpc, store, tmp_path):
    _lint_lib().write_rules(KEY, [RULE])
    out = _propose(rpc, RULED)
    (tmp_path / "lint" / "user.json").write_text(json.dumps({"rules": "not a list"}))
    with pytest.raises(ValueError, match="no rules list"):
        rpc._herd_memory_promote_approve({"id": out["id"]})
    assert not (store / f"{RULED}.md").is_symlink() and not (sm.user_store() / f"{RULED}.md").exists()
    assert [r["fact"] for r in _rules(KEY)] == [RULED]


def test_an_identical_pending_one_is_not_stacked(rpc):
    a, b = _propose(rpc, FACT), _propose(rpc, FACT)
    assert b == dict(a, duplicate=True) and len(get_herd().proposals()) == 1


def test_suggest_asks_for_every_project_and_checks_too(rpc):
    rpc._herd_memory_suggest({"key": KEY})
    body = get_herd().recent_asks()["asks"][0]
    with get_herd()._mu:
        text = get_herd()._ask_row(body["id"])["body"]
    assert "--file CLAUDE.md" in text
    assert "promote propose --fact NAME --every-project" in text
    assert "promote propose --lint --fact NAME" in text


def test_herd_promote_propose_every_project(live, tmp_path, monkeypatch):
    monkeypatch.setenv("GHOSTHERD_SHARED_MEMORY_DIR", str(tmp_path / "shared"))
    key = sm.slug_for_path("/src/a")
    sm.write_fact(sm.project_store(key), key, "Use kctx for any GKE cluster.", name="kctx", type="reference")
    env = dict(live["env"], HOME=str(tmp_path))

    def herd(*args):
        return subprocess.run([str(HERD), "promote", "propose", *args, "--project", "/src/a"],
                              capture_output=True, text=True, env=env, timeout=30)
    done = herd("--fact", "kctx", "--every-project")
    assert done.returncode == 0, done.stderr
    assert done.stderr == "herd: proposed making kctx every project's (pending the user's approval)\n"
    (p,) = herd_mod.get_herd().proposals()
    assert (p["kind"], p["project"], p["facts"]) == ("share", key, ["kctx"])
    # Its own flags only.
    for wrong in (["--file", "CLAUDE.md"], ["--lint"], ["--fact", "other"]):
        done = herd("--fact", "kctx", "--every-project", *wrong)
        assert done.returncode != 0 and ("--every-project moves" in done.stderr
                                         or "one fact at a time" in done.stderr), done.stderr
