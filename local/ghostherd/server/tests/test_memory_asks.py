"""An ask carries the shared-memory facts most relevant to it, after its
body, under a clear heading -- and only when something passes the bar."""

import fact_index
from herd import HerdStore, reset_herd
from memory_store import DEPLOY_ASK, SNAP, UNRELATED_ASKS, store  # noqa: F401

# ---- An ask carries the facts relevant to it ----------------------------------


def _herd(tmp_path):
    reset_herd()
    tmp_path.mkdir(parents=True, exist_ok=True)
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=SNAP)
    return h




def _delivered(h, to, body):
    h._asked = h.ask(to, body, from_name="lead")["id"]
    out = h.tick(sessions=SNAP)
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in out["pending"]])
    return next(p["body"] for p in out["pending"] if p["to"] == to)


def test_a_deploy_ask_carries_branch_discipline(store, tmp_path):
    h = _herd(tmp_path)
    assert h.ask_memory is not None          # the sidecar's handler set it
    text = _delivered(h, "claude-dp", DEPLOY_ASK)
    assert text.startswith(DEPLOY_ASK)       # the ask first, as asked
    memory = text[len(DEPLOY_ASK):]
    assert fact_index.ASK_HEADING in memory
    assert "branch-discipline" in memory
    assert "git branch --show-current" in memory  # the rule itself, not a pointer
    assert len(memory) <= fact_index.ASK_BLOCK_MAX + 4
    # The ask as stored is what was asked.
    with h._mu:
        stored = h._conn.execute("SELECT body FROM asks WHERE id = ?", (h._asked,)).fetchone()[0]
    assert stored == DEPLOY_ASK


def test_an_unrelated_ask_carries_nothing(store, tmp_path):
    for i, body in enumerate(UNRELATED_ASKS):
        h = _herd(tmp_path / str(i))
        assert _delivered(h, "claude-dp", body) == body


def test_a_grok_taker_is_not_given_its_rules_twice(store, tmp_path):
    h = _herd(tmp_path)
    text = _delivered(h, "grok-dp", DEPLOY_ASK)
    # Its opening gave it the rules in full; the ask does not repeat them.
    assert "branch-discipline" not in text[len(DEPLOY_ASK):]


def test_an_ask_in_a_project_with_no_memory_is_left_alone(store, tmp_path):
    reset_herd()
    h = HerdStore(tmp_path / "herd2.sqlite")
    snap = [{"name": "x", "kind": "claude", "state": "idle", "project": "/elsewhere"}]
    h.tick(sessions=snap)
    h.ask("x", DEPLOY_ASK, from_name="user")
    out = h.tick(sessions=snap)
    assert out["pending"][0]["body"] == DEPLOY_ASK


def test_a_failing_lookup_never_holds_an_ask(store, tmp_path):
    h = _herd(tmp_path)
    h.ask_memory = lambda taker, body: 1 / 0
    assert _delivered(h, "claude-dp", DEPLOY_ASK) == DEPLOY_ASK


def test_an_ask_waits_for_its_memory_only_a_moment(store, tmp_path):
    h = _herd(tmp_path)
    h.ask_memory = None                      # as if the lookup never came back
    h.ask("claude-dp", DEPLOY_ASK, from_name="lead")
    with h._mu:
        h._conn.execute("UPDATE mail SET memory_until = '2999-01-01T00:00:00+00:00'")
        h._conn.commit()
    assert not h.tick(sessions=SNAP)["pending"]          # held while it is found
    with h._mu:
        h._conn.execute("UPDATE mail SET memory_until = '2000-01-01T00:00:00+00:00'")
        h._conn.commit()
    assert h.tick(sessions=SNAP)["pending"][0]["body"] == DEPLOY_ASK  # then goes without
