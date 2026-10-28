"""What the agents' replies cost, in tokens (tally.py): each agent's by local
day, read from its transcripts as they grow and kept in herd.sqlite; each
project's, its lead's share apart; each ask's, on its line."""

import json
import os
import time
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

import pytest

import ask_trace as T
import herd as herd_mod
import tally
from herd import HerdStore, reset_herd
from tests.test_ask import _rpc, client, live  # noqa: F401  (fixtures)

PROJECT = "~/src/dp/"
SNAP = [
    {"name": "lead-dp", "kind": "claude", "state": "idle", "project": PROJECT, "role": "lead"},
    {"name": "claude-dp", "kind": "claude", "state": "working", "project": PROJECT},
    {"name": "agy-dp", "kind": "agy", "state": "idle", "project": PROJECT},
    {"name": "grok-dp", "kind": "grok", "state": "idle", "project": PROJECT},
]


@pytest.fixture
def home(tmp_path, monkeypatch):
    h = tmp_path / "home"
    h.mkdir()
    monkeypatch.setenv("HOME", str(h))
    return h


@pytest.fixture
def taipei():
    """Local time eight hours ahead of UTC, so a day is not UTC's.  Put back
    by hand: monkeypatch's own undo comes after this fixture's end, too late
    for the tzset that makes it take."""
    old = os.environ.get("TZ")
    os.environ["TZ"] = "Asia/Taipei"
    time.tzset()
    yield
    if old is None:
        os.environ.pop("TZ", None)
    else:
        os.environ["TZ"] = old
    time.tzset()


def _store(tmp_path, snap=SNAP) -> HerdStore:
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=snap)
    return h


def _write(path: Path, records, mode="w") -> str:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open(mode) as f:
        f.write("".join(json.dumps(r) + "\n" for r in records))
    return str(path)


def _iso(when: datetime) -> str:
    return when.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")


NOW = datetime.now(timezone.utc).replace(microsecond=0)


def usage(inp=0, w5=0, w1=0, read=0, out=0, total=None):
    return {"input_tokens": inp, "cache_creation_input_tokens": w5 + w1 if total is None else total,
            "cache_read_input_tokens": read, "output_tokens": out,
            "cache_creation": {"ephemeral_5m_input_tokens": w5, "ephemeral_1h_input_tokens": w1}}


def reply(mid, u, when=NOW, model="claude-opus-5-5", blocks=1, **kw):
    """One claude reply, as claude writes it: a record per content block, each
    with the same usage."""
    return [dict({"type": "assistant", "timestamp": _iso(when), "requestId": "req" + mid,
                  "message": {"id": "msg" + mid, "model": model, "role": "assistant",
                              "content": [{"type": "text", "text": f"block {i}"}], "usage": u}}, **kw)
            for i in range(blocks)]


def claude_file(home, name, records, mode="w"):
    return _write(home / ".claude" / "projects" / "-src-dp" / f"{name}.jsonl", records, mode)


def day_rows(h):
    with h._mu:
        return {(r[0], r[1]): r[2:] for r in h._conn.execute(
            "SELECT agent, day, input, cache_write_5m, cache_write_1h, cache_read, output, replies"
            " FROM tally_days ORDER BY agent, day")}


# ---- The unit ------------------------------------------------------------------------


def test_each_kind_weighted_as_it_is_priced():
    u = tally.claude_usage(usage(inp=1000, w5=1000, w1=1000, read=10000, out=100))
    assert (u.input, u.cache_write_5m, u.cache_write_1h, u.cache_read, u.output) == (1000, 1000, 1000, 10000, 100)
    assert u.weighted() == 1000 + 1250 + 2000 + 1000 + 500
    assert u.shown() == {"weighted": 5750, "input": 1000, "cache_write": 2000, "cache_read": 10000, "output": 100}


def test_a_cache_write_that_does_not_say_its_lifetime_is_a_five_minute_one():
    u = tally.claude_usage({"input_tokens": 3, "cache_creation_input_tokens": 800, "output_tokens": 2})
    assert (u.cache_write_5m, u.cache_write_1h, u.weighted()) == (800, 0, 3 + 1000 + 10)
    # Its parts win where they say more than its total (seen: 0 in all,
    # 735 for the hour); what they leave of it is a five-minute one.
    assert tally.claude_usage(usage(w1=735, total=0)).cache_write_1h == 735
    v = tally.claude_usage(usage(w1=100, total=400))
    assert (v.cache_write_5m, v.cache_write_1h) == (300, 100)


def test_agy_and_grok_say_it_their_own_way():
    a = tally.agy_usage({"input_tokens": 8000, "cache_read_tokens": 2000, "output_tokens": 300})
    assert (a.input, a.cache_read, a.output, a.weighted()) == (8000, 2000, 300, 8000 + 200 + 1500)
    # grok's input includes what it read from cache.
    g = tally.grok_usage({"inputTokens": 766976, "cachedReadTokens": 675200, "outputTokens": 26818,
                          "cacheCreationTokens": 0, "reasoningTokens": 24661})
    assert (g.input, g.cache_read, g.output) == (91776, 675200, 26818)


def test_the_number_as_the_page_writes_it():
    assert [tally.text(n) for n in (0, 999, 1000, 340_400, 1_000_000, 1_250_000, 12_000_000)] == \
        ["0", "999", "1k", "340k", "1M", "1.3M", "12M"]
    # As toFixed rounds a float: 1.65 is 1.6499... as one; 12.4 stays.
    assert [tally.text(n) for n in (1_650_000, 12_400_000, 1_000_100)] == ["1.6M", "12.4M", "1M"]


# ---- Read as it grows ----------------------------------------------------------------------


def test_a_reply_counts_once_and_a_synthetic_one_not_at_all(tmp_path):
    path = tmp_path / "t.jsonl"
    _write(path, reply("1", usage(inp=10, out=1), blocks=3)
           + [{"type": "user", "timestamp": _iso(NOW), "message": {"content": "next"}}]
           + reply("2", usage(inp=20, out=2), blocks=2)
           + reply("3", usage(inp=99), model="<synthetic>"))
    found, end, more = tally.read_from(path, "claude", 0, 1 << 20)
    seen = tally.Counted()
    counted = [u for k, _, u in found if seen.first(k)]
    assert [u.input for u in counted] == [10, 20] and end == path.stat().st_size and not more


def test_a_line_still_being_written_waits_for_its_end(tmp_path):
    path = tmp_path / "t.jsonl"
    whole = json.dumps(reply("1", usage(inp=10))[0]) + "\n"
    half = json.dumps(reply("2", usage(inp=20))[0])
    path.write_text(whole + half[:40])
    found, end, _ = tally.read_from(path, "claude", 0, 1 << 20)
    assert [u.input for _, _, u in found] == [10] and end == len(whole)
    with path.open("a") as f:
        f.write(half[40:] + "\n")
    found, end, _ = tally.read_from(path, "claude", end, 1 << 20)
    assert [u.input for _, _, u in found] == [20] and end == path.stat().st_size


def test_a_pass_reads_from_where_the_last_stopped(home, tmp_path):
    """Each agent's transcript and its sub-agents', from the offset kept: a
    reply written as records split across passes counts once."""
    h = _store(tmp_path)
    path = claude_file(home, "s1", reply("1", usage(inp=100, w5=1000, read=5000, out=10), blocks=2))
    sub = _write(Path(path).with_suffix("") / "subagents" / "agent-a1.jsonl",
                 reply("s", usage(inp=7, out=1), isSidechain=True))
    h.report("claude-dp", cli="claude", conversation="s1", transcript=path)
    h._tally_pass()
    today = date.today().isoformat()
    assert day_rows(h) == {("claude-dp", today): (107, 1000, 0, 5000, 11, 2)}
    h._tally_pass()                                      # nothing new: nothing added
    assert day_rows(h)[("claude-dp", today)][0] == 107
    # The next reply's first block, then the rest of it after the pass.
    rest = reply("2", usage(inp=50, out=5), blocks=3)
    claude_file(home, "s1", rest[:1], mode="a")
    h._tally_pass()
    claude_file(home, "s1", rest[1:], mode="a")
    _write(Path(sub), reply("s2", usage(inp=3), isSidechain=True), mode="a")
    h._tally_pass()
    assert day_rows(h)[("claude-dp", today)] == (160, 1000, 0, 5000, 16, 4)


def test_a_long_history_is_read_a_budget_at_a_time(home, tmp_path):
    h = _store(tmp_path)
    recs = [r for i in range(200) for r in reply(str(i), usage(inp=1, out=1), blocks=2)]
    path = claude_file(home, "s1", recs)
    h.report("claude-dp", cli="claude", conversation="s1", transcript=path)
    passes = 1
    while h._tally_pass(budget=4096):
        passes += 1
    assert passes > 5
    assert day_rows(h)[("claude-dp", date.today().isoformat())][-1] == 200


def test_a_restart_counts_nothing_twice(home, tmp_path):
    h = _store(tmp_path)
    path = claude_file(home, "s1", reply("1", usage(inp=100)))
    h.report("claude-dp", cli="claude", conversation="s1", transcript=path)
    h._tally_pass()
    h.close()
    h = _store(tmp_path)
    claude_file(home, "s1", reply("2", usage(inp=1)), mode="a")
    h._tally_pass()
    assert day_rows(h)[("claude-dp", date.today().isoformat())][0] == 101


def test_a_stopped_agent_still_counts_in_its_projects_week(home, tmp_path):
    """What it wrote as it stopped is read after it left the herd; its days
    stay in its project's."""
    h = _store(tmp_path)
    path = claude_file(home, "s1", reply("1", usage(inp=100)))
    h.report("claude-dp", cli="claude", conversation="s1", transcript=path)
    h._tally_pass()
    claude_file(home, "s1", reply("2", usage(inp=20)), mode="a")
    h.tick(sessions=[r for r in SNAP if r["name"] != "claude-dp"])
    h._tally_pass()
    out = h.tally()
    (p,) = out["projects"]
    assert p["project"] == PROJECT and p["week"]["input"] == 120 and p["workers"]["week"]["input"] == 120
    assert [a["name"] for a in out["agents"]] == ["claude-dp"]


def test_a_file_written_anew_is_read_past_what_was_counted(home, tmp_path):
    h = _store(tmp_path)
    old = NOW - timedelta(minutes=5)
    path = claude_file(home, "s1", reply("1", usage(inp=100), when=old) + reply("2", usage(inp=50), when=old))
    h.report("claude-dp", cli="claude", conversation="s1", transcript=path)
    h._tally_pass()
    claude_file(home, "s1", reply("1", usage(inp=100), when=old) + reply("3", usage(inp=7)))   # shorter
    h._tally_pass()
    assert day_rows(h)[("claude-dp", date.today().isoformat())][0] == 157


def test_days_are_local(home, tmp_path, taipei):
    h = _store(tmp_path)
    late = datetime(2026, 10, 27, 17, 0, tzinfo=timezone.utc)       # 01:00 on the 28th in Taipei
    path = claude_file(home, "s1", reply("1", usage(inp=5), when=late - timedelta(hours=2))
                       + reply("2", usage(inp=7), when=late))
    h.report("claude-dp", cli="claude", conversation="s1", transcript=path)
    h._tally_pass()
    assert set(day_rows(h)) == {("claude-dp", "2026-10-27"), ("claude-dp", "2026-10-28")}


def test_agy_and_grok_agents_are_counted_too(home, tmp_path):
    h = _store(tmp_path)
    agy = _write(home / ".gemini" / "antigravity-cli" / "brain" / "b1" / ".system_generated" / "logs" / "transcript.jsonl", [
        {"source": "MODEL", "type": "PLANNER_RESPONSE", "status": "DONE", "created_at": _iso(NOW), "step_index": 1,
         "input_tokens": 8000, "cache_read_tokens": 2000, "output_tokens": 300},
        {"source": "MODEL", "type": "VIEW_FILE", "status": "DONE", "created_at": _iso(NOW), "step_index": 2},
    ])
    chat = _write(home / ".grok" / "sessions" / "%2Fsrc%2Fdp" / "g1" / "chat_history.jsonl",
                  [{"type": "user", "content": [{"type": "text", "text": "hi"}]}])
    _write(Path(chat).with_name("updates.jsonl"), [grok_turn("p1", NOW, inp=1000, read=600, out=10)])
    h.report("agy-dp", cli="agy", conversation="b1", transcript=agy)
    h.report("grok-dp", cli="grok", conversation="g1", transcript=str(Path(chat).with_name("updates.jsonl")))
    h._tally_pass()
    today = date.today().isoformat()
    rows = day_rows(h)
    assert rows[("agy-dp", today)] == (8000, 0, 0, 2000, 300, 1)
    assert rows[("grok-dp", today)] == (400, 0, 0, 600, 10, 1)


# ---- What the page is sent ----------------------------------------------------------------


def test_today_and_the_week_and_the_leads_share(home, tmp_path):
    h = _store(tmp_path)
    lead = claude_file(home, "l1", reply("1", usage(inp=100)) + reply("2", usage(inp=900), when=NOW - timedelta(days=3)))
    work = claude_file(home, "w1", reply("3", usage(inp=300)) + reply("4", usage(inp=5000), when=NOW - timedelta(days=9)))
    h.report("lead-dp", cli="claude", conversation="l1", transcript=lead)
    h.report("claude-dp", cli="claude", conversation="w1", transcript=work)
    h._tally_pass()
    out = h.tally()
    assert out["today"] == date.today().isoformat() and out["days"] == 7
    by = {a["name"]: a for a in out["agents"]}
    assert (by["lead-dp"]["today"]["weighted"], by["lead-dp"]["week"]["weighted"]) == (100, 1000)
    # Nine days back is out of the week.
    assert (by["claude-dp"]["today"]["input"], by["claude-dp"]["week"]["input"]) == (300, 300)
    (p,) = out["projects"]
    assert (p["today"]["weighted"], p["week"]["weighted"]) == (400, 1300)
    assert (p["lead"]["week"]["weighted"], p["workers"]["week"]["weighted"]) == (1000, 300)
    assert set(p["week"]) == {"weighted", "input", "cache_write", "cache_read", "output"}


def test_the_page_asks_the_sidecar(home, client, monkeypatch):  # noqa: F811
    h = herd_mod.get_herd()
    h.tick(sessions=SNAP)
    path = claude_file(home, "s1", reply("1", usage(inp=10, read=1000, out=2)))
    h.report("lead-dp", cli="claude", conversation="s1", transcript=path)
    h._tally_pass()
    out = _rpc(client, "herd_tally", {})["result"]
    assert out["agents"][0]["today"] == {"weighted": 120, "input": 10, "cache_write": 0, "cache_read": 1000, "output": 2}


def test_a_tick_starts_a_pass_on_its_worker(home, tmp_path, monkeypatch):
    monkeypatch.setattr(tally, "EVERY", 0.0)
    h = _store(tmp_path)
    # `_store''s own tick started a pass: a tick while one is on its way starts none.
    deadline = time.monotonic() + 5
    while h._tallying and time.monotonic() < deadline:
        time.sleep(0.01)
    path = claude_file(home, "s1", reply("1", usage(inp=10)))
    h.report("claude-dp", cli="claude", conversation="s1", transcript=path)
    h.tick(sessions=SNAP)
    deadline = time.monotonic() + 5
    while not day_rows(h) and time.monotonic() < deadline:
        time.sleep(0.02)
    assert day_rows(h)


# ---- An ask's ------------------------------------------------------------------------------

ASK = "4a3ecc0f"
FRAME = f"[ghostherd message from lead-dp → claude-dp]\n[ask {ASK} -- whoever asked is waiting for your answer]\nFix it."


def c_user(text, when):
    return {"type": "user", "timestamp": _iso(when), "message": {"role": "user", "content": text}}


def c_reply_call(mid, when, u):
    return [{"type": "assistant", "timestamp": _iso(when), "requestId": "req" + mid,
             "message": {"id": "msg" + mid, "model": "claude-opus-5-5", "usage": u,
                         "content": [{"type": "tool_use", "id": "t" + mid, "name": "Bash",
                                      "input": {"command": f"herd reply {ASK} <<'HERD'\nDone.\nHERD"}}]}}]


def test_an_asks_work_costs_what_its_replies_did(home):
    """Each reply from the ask's delivery to its answer -- the answer's own
    too -- once, and its sub-agents' begun meanwhile; nothing before or after."""
    t0 = NOW - timedelta(minutes=10)
    agent_call = [{"type": "assistant", "timestamp": _iso(t0 + timedelta(minutes=2)), "requestId": "reqA",
                   "message": {"id": "msgA", "model": "claude-opus-5-5", "usage": usage(inp=1),
                               "content": [{"type": "tool_use", "id": "tA", "name": "Agent", "input": {"prompt": "look"}}]}}]
    path = claude_file(home, "s1", reply("0", usage(inp=999_999), when=t0)            # before: not its work
                       + [c_user(FRAME, t0 + timedelta(minutes=1))]
                       + reply("1", usage(inp=100, w1=1000, read=20_000, out=50), when=t0 + timedelta(minutes=2), blocks=3)
                       + agent_call
                       + reply("2", usage(inp=10, out=5), when=t0 + timedelta(minutes=3))
                       + c_reply_call("2", t0 + timedelta(minutes=3), usage(inp=10, out=5))    # the answer's reply, its last record
                       + reply("3", usage(inp=888_888), when=t0 + timedelta(minutes=4)))  # after: not its work
    _write(Path(path).with_suffix("") / "subagents" / "agent-x.jsonl",
           [c_user("look", t0 + timedelta(minutes=2))]
           + reply("s", usage(inp=40), when=t0 + timedelta(minutes=2), isSidechain=True))
    out = T.compose(T.read_work(path, ASK), 120)
    tokens = out["tokens"]
    assert (tokens["input"], tokens["cache_write"], tokens["cache_read"], tokens["output"]) == (151, 1000, 20_000, 55)
    assert tokens["weighted"] == 151 + 2000 + 2000 + 275 and tokens["subagents"] == 40 and tokens["pending"] is False
    assert out["line"] == "How it went: 2 min · 1 tool call, none failed · 1 sub-agent (0 tool calls) · 4k weighted tokens"


def test_an_agy_asks_work_too(home):
    frame = FRAME.replace("claude-dp", "agy-dp")
    path = _write(home / ".gemini" / "antigravity-cli" / "brain" / "b1" / ".system_generated" / "logs" / "transcript.jsonl", [
        {"source": "MODEL", "type": "PLANNER_RESPONSE", "status": "DONE", "created_at": _iso(NOW), "step_index": 0,
         "input_tokens": 5_000_000, "cache_read_tokens": 0, "output_tokens": 0},
        {"source": "USER_EXPLICIT", "type": "USER_INPUT", "status": "DONE", "created_at": _iso(NOW), "step_index": 1,
         "content": "<USER_REQUEST>" + frame + "</USER_REQUEST>"},
        {"source": "MODEL", "type": "PLANNER_RESPONSE", "status": "DONE", "created_at": _iso(NOW), "step_index": 2,
         "input_tokens": 1000, "cache_read_tokens": 3000, "output_tokens": 100,
         "tool_calls": [{"name": "run_command", "args": {"CommandLine": f'herd reply {ASK} "done"'}}]},
    ])
    out = T.compose(T.read_work(path, ASK), 60)
    assert out["tokens"]["weighted"] == 1000 + 300 + 500 and out["line"].endswith("· 2k weighted tokens")


def grok_turn(pid, when, inp=0, read=0, out=0):
    return {"timestamp": int(when.timestamp()), "method": "_x.ai/session/update",
            "params": {"sessionId": "g1", "update": {"sessionUpdate": "turn_completed", "prompt_id": pid,
                                                     "usage": {"inputTokens": inp, "cachedReadTokens": read,
                                                               "outputTokens": out, "cacheCreationTokens": 0}}},
            "_meta": {"agentTimestampMs": int(when.timestamp() * 1000)}}


GSNAP = [{"name": "lead-dp", "kind": "claude", "state": "idle", "project": PROJECT, "role": "lead"},
         {"name": "grok-dp", "kind": "grok", "state": "idle", "project": PROJECT}]


def test_a_grok_asks_tokens_come_when_its_turn_ends(home, tmp_path):
    """grok writes a turn's usage as it ends, after the answer it gave in it:
    the line says so, and the ask has them once the turn's end is read."""
    h = _store(tmp_path, GSNAP)
    ask = h.ask("grok-dp", "fix it", from_name="lead-dp")
    out = h.tick(sessions=GSNAP)
    h.tick(sessions=GSNAP, ack_ids=[p["id"] for p in out["pending"]])
    frame = f"[ghostherd message from lead-dp → grok-dp]\n[ask {ask['id']} -- whoever asked]\nfix it"
    chat = _write(home / ".grok" / "sessions" / "%2Fsrc%2Fdp" / "g1" / "chat_history.jsonl", [
        {"type": "user", "content": [{"type": "text", "text": frame}]},
        {"type": "assistant", "content": "", "tool_calls": [
            {"id": "c1", "name": "run_terminal_command",
             "arguments": json.dumps({"command": f"herd reply {ask['id']} done"})}]},
    ])
    updates = _write(Path(chat).with_name("updates.jsonl"),
                     [grok_turn("p0", NOW - timedelta(hours=1), inp=5_000_000)])   # an earlier turn: not its work
    h.report("grok-dp", cli="grok", conversation="g1", transcript=updates)
    h._tally_pass()
    h.reply(ask["id"], "done", from_name="grok-dp")
    h.wait_traces()
    got = h.get_ask(ask["id"])
    assert "— weighted tokens (grok writes them as its turn ends" in got["trace"]
    assert got["tokens"]["pending"] is True and got["tokens"]["weighted"] == 0
    # Its turn ends a moment after the answer.
    _write(Path(updates), [grok_turn("p1", datetime.now(timezone.utc) + timedelta(seconds=2),
                                     inp=10_000, read=8000, out=100)], mode="a")
    h._tally_pass()
    got = h.get_ask(ask["id"])
    assert got["tokens"]["pending"] is False
    assert (got["tokens"]["input"], got["tokens"]["cache_read"], got["tokens"]["weighted"]) == (2000, 8000, 2000 + 800 + 500)
    # A later turn is another's.
    _write(Path(updates), [grok_turn("p2", datetime.now(timezone.utc) + timedelta(seconds=60), inp=7)], mode="a")
    h._tally_pass()
    assert h.get_ask(ask["id"])["tokens"]["weighted"] == 3300


def test_herd_status_shows_an_asks_tokens(home, tmp_path):
    "get_ask is what `herd status' prints: the tokens beside the line."
    h = _store(tmp_path)
    ask = h.ask("claude-dp", "fix it", from_name="lead-dp")
    out = h.tick(sessions=SNAP)
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in out["pending"]])
    frame = f"[ghostherd message from lead-dp → claude-dp]\n[ask {ask['id']} -- whoever asked]\nfix it"
    path = claude_file(home, "s9", [c_user(frame, NOW)] + reply("1", usage(inp=1000, out=10), blocks=2)
                       + [dict(r, message=dict(r["message"], content=[{"type": "tool_use", "id": "x", "name": "Bash",
                                                                         "input": {"command": f"herd reply {ask['id']} ok"}}]))
                          for r in reply("2", usage(inp=10))])
    h.report("claude-dp", cli="claude", conversation="s9", transcript=path)
    h.reply(ask["id"], "ok", from_name="claude-dp")
    h.wait_traces()
    got = h.get_ask(ask["id"])
    assert got["tokens"] == {"weighted": 1060, "input": 1010, "cache_write": 0, "cache_read": 0, "output": 10,
                             "subagents": 0, "pending": False}
    assert got["trace"].endswith("· 1k weighted tokens")
    # The answer's mail ends with the same line.
    mail = [p for p in h.tick(sessions=SNAP)["pending"] if p["to"] == "lead-dp"]
    assert mail[0]["body"].endswith("[" + got["trace"] + "]")


def test_no_transcript_no_tokens(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("claude-dp", "fix it", from_name="lead-dp")
    out = h.tick(sessions=SNAP)
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in out["pending"]])
    h.reply(ask["id"], "ok", from_name="claude-dp")
    h.wait_traces()
    assert h.get_ask(ask["id"])["tokens"] is None
