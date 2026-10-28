"""What one task cost, end to end, and what it produced (cost.py): a prompt
of the user's to a lead and all it set off, or to an agent directly; read
from fixture transcripts under a temp HOME, the commits from temp repos."""

import json
import os
import subprocess
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

import pytest

import cost
import herd as herd_mod
import tally
from herd import HerdStore, reset_herd
from tests.test_ask import live, run  # noqa: F401  (fixtures)
from tests.test_tally import _iso, _write, reply, usage

NOW = datetime.now(timezone.utc).replace(microsecond=0)


@pytest.fixture
def home(tmp_path, monkeypatch):
    h = tmp_path / "home"
    h.mkdir()
    monkeypatch.setenv("HOME", str(h))
    return h


def at(minutes):
    "A moment MINUTES before now."
    return NOW - timedelta(minutes=minutes)


def git(repo, *args, when=None):
    env = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@x", GIT_COMMITTER_NAME="t",
               GIT_COMMITTER_EMAIL="t@x")
    if when:
        env["GIT_AUTHOR_DATE"] = env["GIT_COMMITTER_DATE"] = when.isoformat()
    return subprocess.run(["git", "-C", str(repo), *args], check=True, capture_output=True, text=True,
                          env=env).stdout


def repo(path, when):
    "A checkout with one commit on master, made WHEN."
    path.mkdir(parents=True)
    git(path, "init", "-q", "-b", "master")
    (path / "README").write_text("x\n")
    git(path, "add", "README")
    git(path, "commit", "-q", "-m", "start", when=when)
    return path


def commit(path, name, lines, when, message="work"):
    (path / name).write_text("".join(f"line {i}\n" for i in range(lines)))
    git(path, "add", name)
    git(path, "commit", "-q", "-m", message, when=when)
    return git(path, "rev-parse", "HEAD").strip()


def you(text, when, **kw):
    return dict({"type": "user", "timestamp": _iso(when), "uuid": "u-" + _iso(when),
                 "message": {"role": "user", "content": text}}, **kw)


def frame(sender, taker, body):
    return f"[ghostherd message from {sender} → {taker}]\n{body}"


def ran(i, command, said, when):
    "A Bash call and what it printed."
    return [{"type": "assistant", "timestamp": _iso(when), "requestId": "r" + i,
             "message": {"id": "m" + i, "model": "claude-opus-5-5", "usage": usage(inp=1),
                         "content": [{"type": "tool_use", "id": "t" + i, "name": "Bash", "input": {"command": command}}]}},
            {"type": "user", "timestamp": _iso(when),
             "message": {"role": "user", "content": [{"type": "tool_result", "tool_use_id": "t" + i, "content": said}]}}]


def transcript(home, name, records, project="-src-dp"):
    return _write(home / ".claude" / "projects" / project / f"{name}.jsonl", records)


# ---- What switches an agent's work ----------------------------------------------


@pytest.mark.parametrize("text,want", [
    ("Fix the login flow.", ("user", "Fix the login flow.")),
    (frame("lead", "rd", "[ask 1a2b3c4d -- whoever asked is waiting]\nDo it."), ("ask", "1a2b3c4d")),
    (frame("rd", "lead", "Answer to ask 1a2b3c4d (Do it.):\nDone."), ("answer", "1a2b3c4d")),
    (frame("rd", "lead", "Ask 1a2b3c4d failed: it stopped"), ("answer", "1a2b3c4d")),
    (frame("qa", "lead", "FYI the tests pass."), None),                       # mail
    ("[Relay: you take over from rd, a claude agent that stopped.]\n## The work", None),
    ("<task-notification>\n<task-id>b1</task-id>\n<status>completed</status>\n</task-notification>", None),
    ("<command-name>/model</command-name>\n<command-args>opus</command-args>", None),
    ("<local-command-stdout>Set model</local-command-stdout>", None),
    ("[Request interrupted by user for tool use]", None),
    ("[Since you last heard from me, I said to rd:\n- rd: x\n]\n\nShip it.", ("user", "Ship it.")),
    ("You are the lead of this project's herd of agents: you plan.\n---\n\nShip it.", ("user", "Ship it.")),
    ('<pasted_content id="a1">\n' + frame("lead", "rd", "[ask 1a2b3c4d -- x]\nDo it.") + "\n</pasted_content>",
     ("ask", "1a2b3c4d")),
    ("   ", None),
])
def test_what_is_the_users_and_what_the_herds(text, want):
    assert cost.said(text) == want


def test_a_lead_turn_woken_by_an_answer_is_that_asks_tasks(tmp_path):
    """The user's second message takes the lead to a new task; the answer to
    the first task's ask takes it back.  A notification, mail, a command
    switch nothing; the client's word on an ask ties it to the work it was
    asked for."""
    recs = ([you("Fix the build.", at(50))] + reply("1", usage(inp=100), when=at(49))
            + ran("2", "herd ask rd 'fix it'", "herd: asked rd (ask aaaa1111), round 1 of 5", at(48))
            + [you("Also, what is 2+2?", at(40))] + reply("3", usage(inp=20), when=at(39))
            + [you(frame("rd", "lead", "Answer to ask aaaa1111 (fix it):\nFixed."), at(30))]
            + reply("4", usage(inp=7), when=at(29))
            + [you("<task-notification>\n<status>completed</status>\n</task-notification>", at(20))]
            + reply("5", usage(inp=3), when=at(19)))
    path = tmp_path / "lead.jsonl"
    _write(path, recs)
    t = cost.Tracker(str(path), "claude", None)
    cost.read_from(path, t, 0, 1 << 20)
    first, second = [x["id"] for x in t.tasks]
    got = {it: u.input for (it, _), (u, _, _) in t.usage.items()}
    assert got == {"task:" + first: 101, "task:" + second: 20, "back:aaaa1111": 10}
    assert t.asked == {"aaaa1111": "task:" + first}
    assert [x["prompt"] for x in t.tasks] == ["Fix the build.", "Also, what is 2+2?"]
    assert cost.task_of("back:aaaa1111", {"aaaa1111": "task:" + first}) == first


def test_grok_and_agy_say_it_their_own_way(tmp_path):
    g = tmp_path / "updates.jsonl"
    _write(g, [
        {"timestamp": int(at(10).timestamp()), "params": {"update": {
            "sessionUpdate": "user_message_chunk", "_meta": {"promptIndex": 0},
            "content": {"type": "text", "text": "Make it faster."}}}},
        {"timestamp": int(at(9).timestamp()), "params": {"update": {
            "sessionUpdate": "turn_completed", "prompt_id": "p0", "usage": {"inputTokens": 900, "cachedReadTokens": 800}}}},
        {"timestamp": int(at(8).timestamp()), "params": {"update": {
            "sessionUpdate": "user_message_chunk", "_meta": {"promptIndex": 1},
            "content": {"type": "text", "text": frame("lead", "grok", "[ask bbbb2222 -- x]\nLook.")}}}},
        {"timestamp": int(at(7).timestamp()), "params": {"update": {
            "sessionUpdate": "turn_completed", "prompt_id": "p1", "usage": {"inputTokens": 50}}}},
    ])
    t = cost.Tracker(str(g), "grok", None)
    cost.read_from(g, t, 0, 1 << 20)
    assert {it: u.input for (it, _), (u, _, _) in t.usage.items()} == {"task:" + t.tasks[0]["id"]: 100,
                                                                     "ask:bbbb2222": 50}
    a = tmp_path / "transcript.jsonl"
    _write(a, [
        {"type": "USER_INPUT", "source": "USER_EXPLICIT", "step_index": 0, "created_at": _iso(at(5)),
         "content": "<USER_REQUEST>Tidy the docs.</USER_REQUEST>"},
        {"type": "PLANNER_RESPONSE", "source": "MODEL", "step_index": 1, "created_at": _iso(at(4)),
         "input_tokens": 300, "cache_read_tokens": 0, "output_tokens": 10},
    ])
    t = cost.Tracker(str(a), "agy", None)
    cost.read_from(a, t, 0, 1 << 20)
    assert [x["prompt"] for x in t.tasks] == ["Tidy the docs."]
    assert sum(u.input for u, _, _ in t.usage.values()) == 300


# ---- A herd at work -----------------------------------------------------------------

LEAD, RD, SOLO = "lead-dp", "rd-dp", "claude-solo"


def herd_at_work(home, tmp_path, merged=True):
    """A lead that asks a worker in its own worktree, which uses a
    sub-agent and commits; an agent in a project with no lead, told
    directly, which commits.  Returns the store and the paths."""
    dp = repo(tmp_path / "dp", at(600))
    wt = tmp_path / "wt-rd"
    git(dp, "worktree", "add", "-q", "-b", "rd-work", str(wt))
    solo = repo(tmp_path / "solo", at(600))
    snap = [{"name": LEAD, "kind": "claude", "state": "idle", "project": str(dp) + "/", "role": "lead",
             "directory": str(dp) + "/"},
            {"name": RD, "kind": "claude", "state": "idle", "project": str(dp) + "/", "directory": str(wt) + "/"},
            {"name": SOLO, "kind": "claude", "state": "idle", "project": str(solo) + "/", "directory": str(solo) + "/"}]
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=snap)
    ask = h.ask(RD, "fix the build", from_name=LEAD)["id"]
    out = h.tick(sessions=snap)
    h.tick(sessions=snap, ack_ids=[p["id"] for p in out["pending"]])
    with h._mu:   # as if asked forty minutes ago
        h._conn.execute("UPDATE asks SET created_at = ?, delivered_at = ? WHERE id = ?",
                        (at(40).isoformat(), at(39).isoformat(), ask))
        h._conn.commit()
    lead = transcript(home, "lead", [you("Fix the build, please.", at(45))] + reply("1", usage(inp=1000), when=at(44))
                      + ran("2", f"herd ask {RD} 'fix the build'", f"herd: asked {RD} (ask {ask}), round 1 of 5", at(40))
                      + [you("Unrelated: how is the weather?", at(35))] + reply("3", usage(inp=50), when=at(34))
                      + [you(frame(RD, LEAD, f"Answer to ask {ask} (fix the build):\nFixed."), at(20))]
                      + reply("4", usage(inp=300), when=at(19)))
    rd = transcript(home, "rd", [you(frame(LEAD, RD, f"[ask {ask} -- whoever asked is waiting]\nfix the build"), at(39))]
                    + reply("5", usage(inp=4000, out=100), when=at(38))
                    + [{"type": "assistant", "timestamp": _iso(at(37)), "requestId": "r6",
                        "message": {"id": "m6", "model": "claude-opus-5-5", "usage": usage(inp=10),
                                    "content": [{"type": "tool_use", "id": "call-1", "name": "Agent", "input": {}}]}}]
                    + reply("7", usage(inp=2000), when=at(25)))
    sub = Path(rd).with_suffix("") / "subagents"
    _write(sub / "agent-s1.jsonl", [you("look", at(36))] + reply("s", usage(inp=500), when=at(36), isSidechain=True))
    (sub / "agent-s1.meta.json").write_text(json.dumps({"toolUseId": "call-1"}))
    commit(wt, "fix.py", 30, at(30), message=f"fix (ask {ask})")
    if merged:
        git(dp, "merge", "-q", "--no-ff", "rd-work", "-m", "Merge rd-work", when=at(15))
    so = transcript(home, "solo", [you("Add a health check.", at(100))] + reply("8", usage(inp=700), when=at(95))
                    + [you("Now push it.", at(80))] + reply("9", usage(inp=60), when=at(79)), project="-src-solo")
    commit(solo, "health.py", 12, at(96))   # in its turn, before its last reply
    h.report(LEAD, cli="claude", conversation="lead", transcript=lead)
    h.report(RD, cli="claude", conversation="rd", transcript=rd)
    h.report(SOLO, cli="claude", conversation="solo", transcript=so)
    h.reply(ask, "Fixed.", from_name=RD)
    h.wait_traces()
    return h, ask, dp


def counted(h):
    while h._tally_pass():
        pass
    with h._tally_mu:
        h._cost_output(force=True)
    h._cost_seen = None
    return {t["prompt"]: t for t in h.costs(7, tasks=True)["tasks"]}


def test_a_task_is_its_prompt_and_all_it_set_off(home, tmp_path):
    h, ask, _ = herd_at_work(home, tmp_path)
    tasks = counted(h)
    fix = tasks["Fix the build, please."]
    # The lead's own: its first turn and the one the answer woke (each
    # reply's 1 for the call too); the worker's; its sub-agent's.
    assert fix["mode"] == "lead" and fix["agent"] == LEAD
    assert (fix["own"], fix["workers"], fix["subs"]) == (1000 + 1 + 300, 4000 + 500 + 10 + 2000, 500)
    assert fix["total"] == fix["own"] + fix["workers"] + fix["subs"] and fix["asks"] == 1
    # The commit its ask's taker made, merged into the main branch.
    assert (fix["code"], fix["commits"], fix["lines"]) == ("yes", 1, 30)
    # The user's aside while the ask was out is a task of its own.
    aside = tasks["Unrelated: how is the weather?"]
    assert (aside["own"], aside["workers"], aside["code"]) == (50, 0, "no")
    # Direct mode: each prompt its own task, the commit in its window.
    health, push = tasks["Add a health check."], tasks["Now push it."]
    assert (health["mode"], health["own"], health["code"], health["lines"]) == ("direct", 700, "yes", 12)
    assert (push["own"], push["code"]) == (60, "no")


def test_not_merged_yet_is_not_no_code(home, tmp_path):
    h, ask, dp = herd_at_work(home, tmp_path, merged=False)
    fix = counted(h)["Fix the build, please."]
    assert (fix["code"], fix["lines"]) == ("unmerged", 0)
    git(dp, "merge", "-q", "--no-ff", "rd-work", "-m", "Merge rd-work")
    with h._mu:   # looked at a minute ago
        h._conn.execute("UPDATE cost_output SET checked_at = checked_at - 120")
        h._conn.commit()
    fix = counted(h)["Fix the build, please."]
    assert (fix["code"], fix["lines"]) == ("yes", 30)


def test_lead_mode_against_direct_mode(home, tmp_path):
    h, _, _ = herd_at_work(home, tmp_path)
    counted(h)
    c = h.costs(7)["compare"]
    lead, direct = c["lead"], c["direct"]
    assert (lead["tasks"], direct["tasks"]) == (2, 2)
    fix_total = 1301 + 6510 + 500
    assert lead["total"] == fix_total + 50 and lead["median"] == round((fix_total + 50) / 2)
    assert lead["per100"] == round((fix_total + 50) * 100 / 30) and lead["own"] == round(1351 * 100 / (fix_total + 50))
    assert (direct["total"], direct["per100"], direct["noCode"], direct["coded"]) == (760, round(760 * 100 / 12), 1, 1)
    assert c["since"] == (date.today() - timedelta(days=6)).isoformat()
    # One project's.
    assert h.costs(7, project="solo")["compare"]["lead"]["tasks"] == 0


def test_each_thread_carries_its_tasks_cost(home, tmp_path):
    h, ask, _ = herd_at_work(home, tmp_path)
    counted(h)
    (t,) = h.threads()["threads"]
    assert t["cost"]["prompt"] == "Fix the build, please." and t["cost"]["lines"] == 30
    assert h.thread(ask=ask)["cost"]["workers"] == 6510
    assert [c["days"] for c in h.threads()["costs"]] == [7, 30]


def test_nothing_twice_across_a_restart(home, tmp_path):
    h, _, _ = herd_at_work(home, tmp_path)
    before = counted(h)["Fix the build, please."]["total"]
    h.close()
    h = HerdStore(tmp_path / "herd.sqlite")
    assert counted(h)["Fix the build, please."]["total"] == before
    with h._mu:
        assert h._conn.execute("SELECT COUNT(*) FROM cost_tasks").fetchone()[0] == 4


def test_an_ask_its_client_never_named_is_the_work_its_asker_was_on(home, tmp_path):
    """`herd ask --wait' run in the background: its "(ask ID)" is in an
    output file not read yet.  The herd's record of the ask says when it
    was made; the asker was on its task then."""
    h, ask, _ = herd_at_work(home, tmp_path)
    lead = Path(h._transcript(LEAD))
    lead.write_text(lead.read_text().replace(f"(ask {ask})", "(see the output file)"))
    assert counted(h)["Fix the build, please."]["workers"] == 6510


def test_an_agent_gone_from_the_herd_still_counts_for_the_asks_it_took(home, tmp_path):
    h, ask, _ = herd_at_work(home, tmp_path)
    snap = [{"name": LEAD, "kind": "claude", "state": "idle", "project": "/x/", "role": "lead"}]
    with h._mu:
        h._conn.execute("DELETE FROM tally_files WHERE agent = ?", (RD,))
        h._conn.commit()
    h.tick(sessions=snap)
    fix = counted(h)["Fix the build, please."]
    assert fix["workers"] == 6510


def test_herd_cost_from_an_agent(home, live):  # noqa: F811
    "`herd cost' is an agent's to run: the numbers, no secrets."
    out = run(live, "cost", "--days", "7")
    assert out.returncode == 0, out.stderr
    assert "lead mode" in out.stdout and "per 100 lines" in out.stdout and "lead's own share" in out.stdout
    out = run(live, "cost", "--json", "--tasks")
    assert json.loads(out.stdout)["compare"]["days"] == 7


def test_a_taker_counts_its_own_commits_not_those_a_merge_brought_in(home, tmp_path):
    """The taker merged the main branch into its own while it worked: the
    other branch's commit that came with it is that branch's work."""
    h, ask, dp = herd_at_work(home, tmp_path, merged=False)
    git(dp, "switch", "-q", "-c", "other")
    commit(dp, "other.py", 500, at(34))
    git(dp, "switch", "-q", "master")
    git(dp, "merge", "-q", "--no-ff", "other", "-m", "Merge other", when=at(33))
    wt = tmp_path / "wt-rd"
    git(wt, "merge", "-q", "--no-ff", "master", "-m", "Merge master", when=at(32))
    git(dp, "merge", "-q", "--no-ff", "rd-work", "-m", "Merge rd-work", when=at(15))
    assert counted(h)["Fix the build, please."]["lines"] == 30


def test_a_sub_agent_with_no_meta_is_its_parents_work(home, tmp_path):
    "An older claude wrote no meta beside a sub-agent: it is what its parent was on."
    h, ask, _ = herd_at_work(home, tmp_path)
    (Path(h._transcript(RD)).with_suffix("") / "subagents" / "agent-s1.meta.json").unlink()
    fix = counted(h)["Fix the build, please."]
    assert fix["subs"] == 500


def test_work_not_over_has_its_commits_read_later(home, tmp_path):
    "A task its agent is still on, five minutes in: its code is not read yet -- not none, not unreadable."
    h, _, _ = herd_at_work(home, tmp_path)
    so = Path(h._transcript(SOLO))
    _write(so, [you("And a readiness check.", at(6))] + reply("10", usage(inp=5), when=at(5)), mode="a")
    tasks = counted(h)
    assert tasks["And a readiness check."]["code"] == "pending"
    assert h.costs(7)["compare"]["direct"]["pending"] == 1
