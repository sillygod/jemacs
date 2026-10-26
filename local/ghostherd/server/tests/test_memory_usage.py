"""How often each shared-memory fact reaches an agent (ask 17f83e5d): in an
ask's memory block as it is delivered, as a rule in a grok's or agy's
opening, returned by `herd recall' by name or by query -- each kind apart,
in the herd's sqlite, never in the fact files; old rows rolled up; and a
count that fails never fails what it counts."""

from datetime import datetime, timedelta, timezone

import pytest

import fact_index
import herd as herd_mod
import shared_memory as sm
from herd import HerdStore, get_herd, reset_herd
from jsonrpc_handler import JsonRpcHandler
from memory_store import DEPLOY_ASK, KEY, PROJECT, SNAP, store  # noqa: F401  (store fixture)


@pytest.fixture
def h(tmp_path, monkeypatch):
    "The herd the sidecar's handler reaches (get_herd), in a dir of its own."
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "herd-data"))
    reset_herd()
    herd = get_herd()
    herd.tick(sessions=SNAP)
    yield herd
    reset_herd()


def _rows(h):
    with h._mu:
        h._flush_uses()
        return h._conn.execute(
            "SELECT key, fact, kind, how, agent, ask FROM fact_uses ORDER BY rowid").fetchall()


def _ago(days):
    return (datetime.now(timezone.utc) - timedelta(days=days)).isoformat()


# ---- An ask: counted as it is delivered, with the facts its block holds ----------


def test_an_asks_facts_count_when_it_is_delivered(store, tmp_path, h):
    ask = h.ask("claude-dp", DEPLOY_ASK, from_name="lead")
    out = h.tick(sessions=SNAP)
    body = out["pending"][0]["body"]
    assert fact_index.ASK_HEADING in body
    assert _rows(h) == []                       # in its block, not yet delivered
    ack = [p["id"] for p in out["pending"]]
    h.tick(sessions=SNAP, ack_ids=ack)
    rows = _rows(h)
    assert rows and {r[2] for r in rows} == {"ask"}
    assert {(r[4], r[5]) for r in rows} == {("claude-dp", ask["id"])}
    # Exactly the facts the block shows -- each named in it, none left out.
    names = [r[1] for r in rows]
    assert all(f"- {n} (" in body for n in names)
    assert body.count("\n- ") == len(names)
    # The user fact counts in the user store, the rest in the project's.
    assert {r[0] for r in rows} <= {KEY, sm.USER_KEY}
    # Acked again: once only.
    h.tick(sessions=SNAP, ack_ids=ack)
    assert len(_rows(h)) == len(rows)


def test_an_ask_cancelled_before_delivery_counts_nothing(store, tmp_path, h):
    ask = h.ask("claude-dp", DEPLOY_ASK, from_name="lead")
    h.cancel(ask["id"])
    out = h.tick(sessions=SNAP)
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in out["pending"]])
    assert _rows(h) == []


def test_a_block_cut_short_counts_only_what_it_gave(monkeypatch):
    hits = [{"name": f"f{i}", "store": KEY, "shared": False, "type": "project",
             "description": "d" * 30, "body": "x" * 600} for i in range(20)]
    text, given = fact_index.compose(hits)
    assert 0 < len(given) < 20
    assert [h["name"] for h in given] == [f"f{i}" for i in range(len(given))]
    assert f"- f{len(given)} (" not in text
    assert fact_index.compose([]) == (None, [])


def test_an_old_hook_that_gives_only_text_still_works(tmp_path, h):
    h.ask_memory = lambda taker, body: "Relevant memory:\n- x (project): y"
    h.ask("claude-dp", "do it", from_name="lead")
    out = h.tick(sessions=SNAP)
    assert out["pending"][0]["body"].endswith("- x (project): y")
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in out["pending"]])
    assert _rows(h) == []


# ---- An opening: the rules it gives in full -------------------------------------


def test_an_opening_counts_its_rules_not_its_listing(store, tmp_path, h):
    out = JsonRpcHandler()._memory_opening({"project": PROJECT, "agent": "grok-dp"})
    rows = _rows(h)
    assert {r[2] for r in rows} == {"opening"}
    assert sorted(r[1] for r in rows) == sorted(out["full"])
    assert not {r[1] for r in rows} & set(out["listed"] + out["more"])
    assert {r[4] for r in rows} == {"grok-dp"}
    user = [r for r in rows if r[0] == sm.USER_KEY]
    assert [r[1] for r in user] == out["full_shared"] == ["prefers-reasoned-design"]


# ---- A recall: by name, by query; never the index, never the page ---------------


def test_recall_by_name_and_by_query_count_apart(store, tmp_path, h):
    rpc = JsonRpcHandler()
    rpc._herd_recall({"from": "claude-dp", "query": "branch-discipline"})
    assert _rows(h) == [(KEY, "branch-discipline", "recall", "name", "claude-dp", None)]
    got = rpc._herd_recall({"from": "claude-dp", "query": "deploy to staging with dagger"})
    rows = _rows(h)[1:]
    assert got["names"] and sorted(r[1] for r in rows) == sorted(got["names"])
    assert {(r[2], r[3], r[4]) for r in rows} == {("recall", "query", "claude-dp")}
    n = len(_rows(h))
    rpc._herd_recall({"from": "claude-dp", "query": ""})          # the index
    rpc._herd_memory_list({"key": KEY})                            # the page reads
    rpc._memory_rank({"from": "claude-dp", "query": "deploy"})
    assert len(_rows(h)) == n


def test_recall_of_a_user_fact_counts_in_the_user_store(store, tmp_path, h):
    JsonRpcHandler()._herd_recall({"from": "claude-dp", "query": "prefers-reasoned-design"})
    assert _rows(h) == [(sm.USER_KEY, "prefers-reasoned-design", "recall", "name", "claude-dp", None)]


# ---- What the page reads ----------------------------------------------------------


def test_the_page_gets_30_days_the_last_use_and_since_when(store, tmp_path, h):
    h.use(KEY, ["branch-discipline"], "ask", agent="a", ask="k1")
    h.use(KEY, ["branch-discipline"], "recall", agent="a", how="query")
    h.use(KEY, ["branch-discipline"] * 3, "opening", agent="g")
    # A use 40 days ago: in the total and the last, not the 30 days.
    h.use(KEY, ["uv-lock-must-follow-pyproject"], "ask", at=_ago(40))
    out = JsonRpcHandler()._herd_memory_list({"key": KEY})
    facts = {f["name"]: f for f in out["facts"]}
    bd = facts["branch-discipline"]["uses"]
    assert (bd["ask"], bd["recall"], bd["opening"], bd["used"], bd["total"]) == (1, 1, 3, 2, 2)
    assert bd["last"] and bd["last_opening"]
    uv = facts["uv-lock-must-follow-pyproject"]["uses"]
    assert (uv["used"], uv["total"]) == (0, 1) and uv["last"].startswith(_ago(40)[:10])
    never = facts["kctx-multi-cluster"]["uses"]
    assert (never["used"], never["total"], never["last"]) == (0, 0, None)
    assert out["uses_days"] == 30 and out["uses_since"]
    # The facts themselves are untouched: no file, no index line, no history changed.
    assert "uses" not in (store / "branch-discipline.md").read_text()
    assert not (store / ".history").exists() or not list((store / ".history").glob("branch-discipline*"))


def test_since_when_is_kept_across_restarts(tmp_path, h):
    since = h.fact_usage(KEY)["since"]
    h2 = HerdStore(h.path)
    assert h2.fact_usage(KEY)["since"] == since


def test_one_facts_uses_say_who_and_which_ask(tmp_path, h):
    h.use(KEY, ["a"], "ask", agent="claude-dp", ask="k1")
    h.use(KEY, ["a"], "recall", agent="grok-dp", how="name")
    got = h.fact_uses(KEY, "a")
    assert [(u["kind"], u["how"], u["agent"], u["ask"]) for u in got] == [
        ("recall", "name", "grok-dp", None), ("ask", None, "claude-dp", "k1")]


# ---- Bounded ----------------------------------------------------------------------


def test_old_uses_are_dropped_and_their_totals_kept(h):
    for d in (100, 95, 91, 5):
        h.use(KEY, ["a"], "ask", at=_ago(d))
    h.use(KEY, ["a"], "recall", at=_ago(120))
    with h._mu:
        h._flush_uses()
        h._conn.execute("DELETE FROM meta WHERE k = 'pruned'")
        h._conn.commit()
    h.tick(sessions=SNAP)                    # the hourly prune
    with h._mu:
        left = h._conn.execute("SELECT kind, at FROM fact_uses").fetchall()
        totals = h._conn.execute("SELECT kind, n, first_at, last_at FROM fact_use_totals ORDER BY kind").fetchall()
    assert len(left) == 1 and left[0][1].startswith(_ago(5)[:10])
    assert [(t[0], t[1]) for t in totals] == [("ask", 4), ("recall", 1)]
    assert totals[0][2].startswith(_ago(100)[:10]) and totals[0][3].startswith(_ago(5)[:10])
    u = h.fact_usage(KEY)["facts"][f"{KEY}/a"]
    assert (u["ask"], u["used"], u["total"]) == (1, 1, 5)
    assert u["last"].startswith(_ago(5)[:10])
    # More later: the totals add, the first use moves back.
    h.use(KEY, ["a"], "ask", at=_ago(200))
    h.tick(sessions=SNAP)
    with h._mu:
        n, first = h._conn.execute("SELECT n, first_at FROM fact_use_totals WHERE kind = 'ask'").fetchone()
    assert n == 5 and first.startswith(_ago(200)[:10])


def test_past_the_row_cap_the_oldest_are_dropped(monkeypatch, h):
    monkeypatch.setattr(herd_mod, "USE_ROWS_MAX", 5)
    for i in range(8):
        h.use(KEY, ["a"], "ask", at=_ago(1 + i / 100))
    with h._mu:
        h._flush_uses()
        h._conn.execute("DELETE FROM meta WHERE k = 'pruned'")
        h._conn.commit()
    h.tick(sessions=SNAP)
    with h._mu:
        assert h._conn.execute("SELECT COUNT(*) FROM fact_uses").fetchone()[0] == 5
        assert h._conn.execute("SELECT n FROM fact_use_totals").fetchone()[0] == 8
    assert h.fact_usage(KEY)["facts"][f"{KEY}/a"]["total"] == 8


def test_a_tick_writes_a_moments_worth(monkeypatch, h):
    monkeypatch.setattr(herd_mod, "USE_FLUSH_MAX", 100)
    h.use(KEY, ["a"] * 250, "recall")
    h.tick(sessions=SNAP)
    with h._mu:
        assert h._conn.execute("SELECT COUNT(*) FROM fact_uses").fetchone()[0] == 100
    assert len(h._uses) == 150
    h.tick(sessions=SNAP)
    h.tick(sessions=SNAP)
    assert len(h._uses) == 0 and h.fact_usage(KEY)["facts"][f"{KEY}/a"]["recall"] == 250


def test_the_queue_is_bounded(tmp_path, monkeypatch, h):
    for _ in range(herd_mod.USE_QUEUE_MAX + 50):
        h.use(KEY, ["a"], "recall")
    assert len(h._uses) == herd_mod.USE_QUEUE_MAX


# ---- A count that fails never fails what it counts --------------------------------


def test_a_broken_uses_table_fails_no_ask_recall_or_opening(store, tmp_path, h):
    with h._mu:
        h._conn.execute("DROP TABLE fact_uses")
        h._conn.commit()
    ask = h.ask("claude-dp", DEPLOY_ASK, from_name="lead")
    out = h.tick(sessions=SNAP)
    assert fact_index.ASK_HEADING in out["pending"][0]["body"]
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in out["pending"]])
    assert h.get_ask(ask["id"])["status"] == "delivered"
    rpc = JsonRpcHandler()
    assert rpc._herd_recall({"from": "claude-dp", "query": "branch-discipline"})["count"] == 1
    assert rpc._memory_opening({"project": PROJECT, "agent": "grok-dp"})["full"]
    listing = rpc._herd_memory_list({"key": KEY})
    assert listing["facts"] and "uses_since" not in listing   # the list shows, without counts


def test_a_raising_recorder_fails_nothing(store, tmp_path, monkeypatch, h):

    def boom(*a, **k):
        raise RuntimeError("no")
    monkeypatch.setattr(HerdStore, "use", boom)
    monkeypatch.setattr(HerdStore, "fact_usage", boom)
    out = h.tick(sessions=SNAP)
    h.ask("claude-dp", DEPLOY_ASK, from_name="lead")
    out = h.tick(sessions=SNAP)
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in out["pending"]])
    rpc = JsonRpcHandler()
    assert rpc._herd_recall({"from": "claude-dp", "query": "branch-discipline"})["count"] == 1
    assert rpc._herd_recall({"from": "claude-dp", "query": "deploy dagger"})["count"] >= 1
    assert rpc._memory_opening({"project": PROJECT})["full"]
    assert rpc._herd_memory_list({"key": KEY})["facts"]


def test_a_use_never_waits_on_the_database(tmp_path, h):
    "Counting holds no lock: the herd's own lock taken, a use still returns at once."
    import threading
    import time
    done = threading.Event()
    with h._mu:
        t = threading.Thread(target=lambda: (h.use(KEY, ["a"] * 100, "recall"), done.set()))
        start = time.monotonic()
        t.start()
        assert done.wait(1.0)
        assert time.monotonic() - start < 0.5
    t.join()
    assert len(_rows(h)) == 100
