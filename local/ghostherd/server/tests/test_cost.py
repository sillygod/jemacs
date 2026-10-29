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
    # The note after images pasted into claude's input, which keeps its
    # blank line as a bare line break.
    ("[Image #1][What this project works on, as the herd sees it.  Read them:\n- branch x in ~/p/\n- PR #3 [y]]\n"
     "Ship it.", ("user", "[Image #1]Ship it.")),
    ("[Image #1] [Image #2][Since you last heard from me, I wrote:\n- to rd: a]\n\nShip it.",
     ("user", "[Image #1] [Image #2]Ship it.")),
    ("[Image #5]", ("user", "[Image #5]")),
    # What the herd tells an agy agent as it starts: no task, guarded or not.
    ("A herd guard checks each of your tool calls.  Your workspace is /w and your\ncommands run there.\n\n"
     "The herd keeps shared memory for this project -- curated facts every\nagent sees.", None),
    ("The herd keeps shared memory for this project -- curated facts every\nagent sees.", None),
    # A role alone -- a cold respawn's -- is the herd's; primary's opens its
    # first message as a lead's does, the notes after it.
    ("You are the lead of this project's herd of agents: you plan.\n", None),
    ("You are primary: the herd-wide lead.  You route.\n---\n\nWhat is everyone doing?",
     ("user", "What is everyone doing?")),
    ("You are the lead of this project's herd of agents: you plan.\n---\n\n"
     "[What this project works on, as the herd sees it.  Read them:\n- branch x]\n\nShip it.", ("user", "Ship it.")),
    ("You are the lead of this project's herd of agents: you plan.\n---\n\n[Relay: you take over from lead.]\n## Work",
     None),
])
def test_what_is_the_users_and_what_the_herds(text, want):
    assert cost.said(text) == want


def test_what_the_herd_writes_an_agent_as_it_starts_is_no_task(tmp_path):
    """The agy start message, a lead's role alone and primary's first
    message, as Emacs writes them -- not as copied here, so a change to
    them fails here, not silently in the numbers."""
    import shutil

    from tests.test_conversation import GHOSTHERD_EL

    emacs = shutil.which("emacs")
    if not emacs:
        pytest.skip("no emacs")
    root = GHOSTHERD_EL.parent
    lisp = r"""
(let ((ghostherd-memory-dir "%s"))
  (princ (concat (format ghostherd--agy-guard-note "/w/" "/g/") (ghostherd--memory-opening "/no/project/")))
  (princ "\0")
  (princ (ghostherd--memory-opening "/no/project/"))
  (princ "\0")
  (princ ghostherd--lead-role)
  (princ "\0")
  (princ (concat ghostherd--primary-role "\n---\n\n" "What is everyone doing?")))
""" % (tmp_path / "memory")
    out = subprocess.run([emacs, "-Q", "--batch", "-L", str(root), "-L", str(root.parent / "xwapp"),
                          "-l", "ghostherd", "--eval", lisp], capture_output=True, text=True, timeout=60)
    assert out.returncode == 0, out.stderr[-2000:]
    guarded, unguarded, role, primary = out.stdout.split("\0")
    assert guarded.startswith(cost.OPENINGS[0]) and len(guarded) > 400
    assert cost.said(guarded) is None and cost.said(unguarded) is None and cost.said(role) is None
    assert cost.said(primary) == ("user", "What is everyone doing?")
    # agy's own record of it, read for tasks: none.
    path = tmp_path / "transcript.jsonl"
    _write(path, [{"type": "USER_INPUT", "step_index": 0, "created_at": _iso(at(5)),
                   "content": "<USER_REQUEST>" + guarded + "</USER_REQUEST>"}])
    t = cost.Tracker(str(path), "agy", None)
    cost.read_from(path, t, 0, 1 << 20)
    assert t.tasks == [] and t.item is None


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
    git(dp, "worktree", "add", "-q", "-b", "rd-work", str(wt), when=at(500))
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


def test_a_commit_counts_merged_or_not(home, tmp_path):
    """A work project merges on its forge, often squashed: a commit made
    counts as made; whether it is in the main branch is said beside it."""
    h, ask, dp = herd_at_work(home, tmp_path, merged=False)
    fix = counted(h)["Fix the build, please."]
    assert (fix["code"], fix["lines"], fix["merged"], fix["mergedLines"]) == ("yes", 30, 0, 0)
    assert [(c["lines"], c["merged"], c["subject"]) for c in fix["made"]] == [(30, False, f"fix (ask {ask})")]
    git(dp, "merge", "-q", "--no-ff", "rd-work", "-m", "Merge rd-work")
    with h._mu:   # looked at a minute ago
        h._conn.execute("UPDATE cost_commits SET checked_at = checked_at - 120")
        h._conn.commit()
    fix = counted(h)["Fix the build, please."]
    assert (fix["code"], fix["lines"], fix["merged"], fix["mergedLines"]) == ("yes", 30, 1, 30)
    assert h.costs(7)["compare"]["lead"]["merged"] == 30


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
    (t,) = [t for t in h.threads()["threads"] if not t.get("direct")]
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


# ---- The commits a task made ------------------------------------------------------


def rewrite(repo, *args, when):
    "A git command that copies commits: its own time, theirs kept (their author and author date)."
    env = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@x", GIT_COMMITTER_NAME="t",
               GIT_COMMITTER_EMAIL="t@x", GIT_COMMITTER_DATE=when.isoformat(), GIT_EDITOR="true")
    subprocess.run(["git", "-C", str(repo), *args], check=True, capture_output=True, text=True, env=env)
    return git(repo, "rev-parse", "HEAD").strip()


def told(h, name, prompt, start, replies):
    "The user's PROMPT to NAME at START, and its replies at REPLIES (minutes ago)."
    path = Path(h._transcript(name))
    records = [you(prompt, at(start))]
    for i, m in enumerate(replies):
        records += reply(f"{prompt[:6]}{i}", usage(inp=40), when=at(m))
    _write(path, records, mode="a")


def test_a_direct_agents_commits_on_a_feature_branch_are_its_tasks(home, tmp_path):
    """A work project commits on a feature branch, opens a pull request and
    goes back to main: the commit is still the task's.  What a pull brings
    in -- a colleague's commit -- was made elsewhere, and is no one's here."""
    h, _, _ = herd_at_work(home, tmp_path)
    solo, theirs = tmp_path / "solo", tmp_path / "colleague"
    git(tmp_path, "clone", "-q", str(solo), str(theirs))
    told(h, SOLO, "Fix the login page.", 70, [69, 60])
    git(solo, "switch", "-q", "-c", "login", when=at(68))
    sha = commit(solo, "login.py", 25, at(66))
    git(solo, "switch", "-q", "master", when=at(65))
    commit(theirs, "theirs.py", 400, at(64))
    git(solo, "pull", "-q", "--ff-only", str(theirs), "master", when=at(63))
    login = counted(h)["Fix the login page."]
    assert (login["code"], login["commits"], login["lines"], login["merged"]) == ("yes", 1, 25, 0)
    assert [c["sha"] for c in login["made"]] == [sha]
    # And the branch merged, a merge commit -- no work of its own: merged.
    git(solo, "merge", "-q", "--no-ff", "login", "-m", "Merge login", when=at(10))
    with h._mu:   # looked at long enough ago for an hour-old commit: asked less often as it ages
        h._conn.execute("UPDATE cost_commits SET checked_at = checked_at - 600")
        h._conn.commit()
    login = counted(h)["Fix the login page."]
    assert (login["commits"], login["merged"]) == (1, 1)


def test_a_stopped_takers_commits_outlive_its_worktree(home, tmp_path):
    """Its lead stops a worker right after merging, and its worktree goes,
    reflog and all: the commits were kept first -- each by the pass after
    it was made, the last by the stop itself."""
    h, ask, dp = herd_at_work(home, tmp_path, merged=False)
    counted(h)
    wt = tmp_path / "wt-rd"
    late = commit(wt, "docs.md", 4, at(26), message="docs")
    h.enqueue_command("stop", RD, {"by": LEAD})
    git(dp, "worktree", "remove", "--force", str(wt))
    git(dp, "branch", "-q", "-D", "rd-work")
    h._reflogs.clear()
    fix = counted(h)["Fix the build, please."]
    assert (fix["code"], fix["commits"], fix["lines"]) == ("yes", 2, 34)
    assert late in [c["sha"] for c in fix["made"]]


def test_an_amend_a_rebase_and_a_cherry_pick_count_once(home, tmp_path):
    """Each is a copy of a commit made before: its work counts once, where it
    was first made, at its last copy's lines; it is merged when any copy
    is; and the task that made a copy shows it, counting nothing."""
    h, _, _ = herd_at_work(home, tmp_path)
    solo = tmp_path / "solo"
    told(h, SOLO, "Add a cache.", 70, [69, 55])
    told(h, SOLO, "Put it on master too.", 50, [49, 45])
    git(solo, "switch", "-q", "-c", "cache", when=at(68))
    first = commit(solo, "cache.py", 10, at(66))
    (solo / "cache.py").write_text("".join(f"line {i}\n" for i in range(15)))
    amended = rewrite(solo, "commit", "-q", "-a", "--amend", "--no-edit", when=at(64))
    rebased = rewrite(solo, "rebase", "-q", "--force-rebase", "master", "cache", when=at(58))
    git(solo, "switch", "-q", "master", when=at(48))
    picked = rewrite(solo, "cherry-pick", rebased, when=at(47))
    tasks = counted(h)
    cache, pick = tasks["Add a cache."], tasks["Put it on master too."]
    assert len({first, amended, rebased, picked}) == 4
    assert (cache["commits"], cache["lines"], cache["merged"]) == (1, 15, 1)
    assert [(c["sha"], c["of"]) for c in cache["made"]] == [(first, None), (amended, first), (rebased, first)]
    assert (pick["code"], pick["commits"], pick["lines"]) == ("no", 0, 0)
    assert [(c["sha"], c["of"]) for c in pick["made"]] == [(picked, first)]


def test_an_ask_whose_takers_checkout_was_never_read(home, tmp_path, monkeypatch):
    """Its taker's worktree gone before the herd read it (asks before this
    was): the commits in the main branch that name the ask are its."""
    monkeypatch.setattr(HerdStore, "_capture_soon", lambda self, agent: None)
    h, ask, dp = herd_at_work(home, tmp_path)
    git(dp, "worktree", "remove", "--force", str(tmp_path / "wt-rd"))
    fix = counted(h)["Fix the build, please."]
    assert (fix["code"], fix["commits"], fix["lines"], fix["merged"]) == ("yes", 1, 30, 1)
    assert fix["made"][0]["subject"] == f"fix (ask {ask})"


def test_a_commit_is_the_work_its_agent_was_on():
    """The latest begun of the work it falls in, up to COMMIT_SLACK after
    its last reply; read when its checkout's reflog was, all that time."""
    c = lambda sha, t: {"sha": sha, "at": t, "lines": 1, "of": None, "merged": False}
    spans = [("task:a", "x", 100, 200), ("task:b", "x", 250, 300), ("ask:q", "y", 100, 400)]
    made = cost.attribute({"/x": [c("1", 150), c("2", 260), c("3", 310), c("4", 900)], "/y": [c("5", 200)]},
                          spans, {"x": ["/x"], "y": ["/y"]}, {"/x": [[0, 1000]]}, now=1000)
    assert [m["sha"] for m in made[("task:a", "x")]["commits"]] == ["1"]
    assert [m["sha"] for m in made[("task:b", "x")]["commits"]] == ["2", "3"]
    assert [m["sha"] for m in made[("ask:q", "y")]["commits"]] == ["5"]
    assert made[("task:a", "x")]["read"] and not made[("ask:q", "y")]["read"]
    assert cost.cover([[0, 10]], 5, 20) == [[0, 20]] and cost.cover([[0, 10]], 30, 40) == [[0, 10], [30, 40]]


def test_none_found_by_name_is_not_none_made():
    "An ask whose checkout was never read, and nothing names: its task's code is not known."
    tasks = [{"id": "t", "agent": "lead", "project": "/p", "mode": "lead", "prompt": "x", "at": 1.0}]
    usage = [{"item": "task:t", "agent": "lead", "sub": 0, "input": 5, "first_at": 1.0, "last_at": 2.0}]
    named = {"ask:q": {"commits": [], "named": True}}
    row = cost.costs(tasks, usage, {"q": "task:t"}, named, now=10 ** 9)["t"]
    assert (row["code"], row["unread"]) == ("unknown", ["ask:q"])
    old = {"ask:q": {"commits": [], "named": False}}
    assert cost.costs(tasks, usage, {"q": "task:t"}, old, now=10 ** 9)["t"]["code"] == "no"


# ---- Direct tasks on the Threads tab -----------------------------------------------------


def test_a_task_told_directly_is_a_thread_of_its_own(home, tmp_path):
    h, _, _ = herd_at_work(home, tmp_path)
    counted(h)
    direct = [t for t in h.threads()["threads"] if t.get("direct")]
    assert [t["title"] for t in direct] == ["Now push it.", "Add a health check."]
    health = direct[1]
    assert (health["participants"], health["projects"], health["rootFrom"]) == (["user", SOLO], ["solo"], "user")
    assert health["id"].startswith("task:") and health["cost"]["lines"] == 12
    t = h.thread(id=health["id"])
    assert [(m["kind"], m["from"], m["to"], m["text"]) for m in t["msgs"]] == [
        ("message", "user", SOLO, "Add a health check."), ("answer", SOLO, "user", "block 0")]
    assert [ln["name"] for ln in t["lanes"]] == ["user", SOLO] and t["lanes"][1]["project"] == "solo"
    (c,) = t["cost"]["made"]
    assert (c["lines"], c["merged"], c["subject"], c["of"]) == (12, True, "work", None)
    assert t["cost"]["threadCount"] == 1
    # A task kept before tasks kept where its prompt is: found again.
    with h._mu:
        h._conn.execute("UPDATE cost_tasks SET offset = NULL")
        h._conn.commit()
    assert [m["text"] for m in h.thread(id=health["id"])["msgs"]] == ["Add a health check.", "block 0"]
    with pytest.raises(ValueError):
        h.thread(id="task:nosuchtask")


def test_two_threads_of_one_task_show_its_cost_once(home, tmp_path):
    """A lead's two asks for one task are two threads: the first shows the
    task's cost, the other says it is part of that one's task."""
    h, ask, _ = herd_at_work(home, tmp_path)
    second = h.ask(RD, "and the docs", from_name=LEAD)["id"]
    with h._mu:   # asked as the answer to the first woke the lead
        h._conn.execute("UPDATE asks SET created_at = ?, delivered_at = ? WHERE id = ?",
                        (at(19.5).isoformat(), at(19.4).isoformat(), second))
        h._conn.commit()
    h.reply(second, "Done.", from_name=RD)
    counted(h)
    asks = {t["id"]: t for t in h.threads()["threads"] if not t.get("direct")}
    assert set(asks) == {ask, second}
    assert asks[ask]["cost"]["lines"] == 30 and "same" not in asks[ask]["cost"]
    assert asks[second]["cost"] == {"id": asks[ask]["cost"]["id"], "mode": "lead", "same": ask,
                                    "sameTitle": "fix the build"}
    assert h.thread(id=ask)["cost"]["threadCount"] == 2


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


# ---- Who may read the tasks -----------------------------------------------------------


def _costs(client, token, **params):
    resp = client.post("/jsonrpc/" + token, json={"jsonrpc": "2.0", "id": 1, "method": "herd_costs",
                                                  "params": dict({"days": 7, "tasks": True}, **params)})
    assert resp.status_code == 200
    return resp.json()["result"]


def test_an_agent_reads_its_own_projects_tasks_and_every_projects_numbers(home, tmp_path, monkeypatch):
    """`herd cost' from an agent: the numbers of every project -- a lead
    compares with direct mode elsewhere -- but the tasks, and the user's
    words that began them, of its own project only, the one the herd knows
    it in; a project it names is no way round that.  A caller the herd does
    not know reads no task; the herd's own token (Emacs, the page) all."""
    from fastapi.testclient import TestClient
    from engine import reset_engine
    from main import app
    reset_engine()
    # The herd of herd_at_work, in the store the sidecar serves.
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path))
    built, _, dp = herd_at_work(home, tmp_path)
    built.close()
    reset_herd()
    solo = str(tmp_path / "solo") + "/"
    with TestClient(app, base_url="http://127.0.0.1") as client:
        counted(herd_mod.get_herd())
        admin, agent = client.app.state.token, client.app.state.agent_token
        prompts = lambda out: sorted(t["prompt"] for t in out["tasks"])
        everything = _costs(client, admin)
        assert prompts(everything) == ["Add a health check.", "Fix the build, please.", "Now push it.",
                                       "Unrelated: how is the weather?"] and "withheld" not in everything
        # The lead: its project's tasks; every project's numbers.
        lead = _costs(client, agent, **{"from": LEAD})
        assert prompts(lead) == ["Fix the build, please.", "Unrelated: how is the weather?"]
        assert lead["withheld"] == 2 and lead["compare"] == everything["compare"]
        assert lead["compare"]["direct"]["tasks"] == 2
        # Another project named: its numbers, none of its words.
        other = _costs(client, agent, project=solo, **{"from": LEAD})
        assert other["tasks"] == [] and other["withheld"] == 2 and other["compare"]["direct"]["total"] == 760
        # The direct agent, likewise, the other way round.
        assert prompts(_costs(client, agent, **{"from": SOLO})) == ["Add a health check.", "Now push it."]
        # A caller the herd does not know -- a plain shell, or a name it
        # made up: numbers only.
        for who in ({}, {"from": "user"}, {"from": "nobody-here"}):
            out = _costs(client, agent, **who)
            assert out["tasks"] == [] and out["withheld"] == 4 and out["compare"]["lead"]["tasks"] == 2
        # Saying it is the herd in the call changes nothing: the token does.
        assert _costs(client, agent, role="admin", **{"from": "nobody-here"})["tasks"] == []
    reset_herd()
    reset_engine()


def test_herd_cost_says_what_it_left_out(home, live):  # noqa: F811
    "The CLI names its caller, and says the rest of the tasks are another project's."
    h = herd_mod.get_herd()
    h._cost_seen = (float("inf"), {"asked": {}, "tasks": {
        "k1": {"id": "k1", "agent": "lead-x", "project": "/elsewhere/", "mode": "lead", "prompt": "secret plans",
               "at": NOW.timestamp(), "end": NOW.timestamp(), "total": 10, "own": 10, "workers": 0, "subs": 0,
               "kinds": {}, "asks": 0, "commits": 0, "lines": 0, "code": "no"}}})
    out = run(live, "cost", "--tasks")
    assert out.returncode == 0, out.stderr
    assert "secret plans" not in out.stdout and "1 more task" in out.stdout


# ---- Tasks kept by older rules ------------------------------------------------------

AGY = "agy-dp"
# What the rules before SAID_VERSION 2 read as a lead's note: it at the
# very start, closed by a blank line.
OLD_NOTE = r"(?:^|(?<=>))\[((?:Since you last heard from me|What this project works on)[^\n]*)\n.*?\]\n\n"


def agy_said(text, when, step):
    return {"type": "USER_INPUT", "source": "USER_EXPLICIT", "step_index": step, "created_at": _iso(when),
            "content": "<USER_REQUEST>" + text + "</USER_REQUEST>"}


def agy_reply(inp, when, step):
    return {"type": "PLANNER_RESPONSE", "source": "MODEL", "step_index": step, "created_at": _iso(when),
            "input_tokens": inp, "cache_read_tokens": 0, "output_tokens": 0}


def old_rules_herd(home, tmp_path, monkeypatch):
    """A lead told, after two pasted images and its note, to ship a fix,
    which it asks an agy agent for; the agy agent, respawned on its
    conversation while on that ask, told the herd's opening again, then
    told directly to tidy the docs, which it commits -- all read by the
    older rules: the opening a task, the note the lead's task's title."""
    import re
    import conversation

    dp = repo(tmp_path / "dp", at(600))
    snap = [{"name": LEAD, "kind": "claude", "state": "idle", "project": str(dp) + "/", "role": "lead",
             "directory": str(dp) + "/"},
            {"name": AGY, "kind": "agy", "state": "idle", "project": str(dp) + "/", "directory": str(dp) + "/"}]
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=snap)
    ask = h.ask(AGY, "ship the fix", from_name=LEAD)["id"]
    out = h.tick(sessions=snap)
    h.tick(sessions=snap, ack_ids=[p["id"] for p in out["pending"]])
    note = "[What this project works on, as the herd sees it.  Read them:\n- branch x in ~/p/\n- its PR #3 [open]]\n"
    lead = transcript(home, "lead", [you([{"type": "text", "text": "[Image #1] [Image #2]" + note + "Ship the fix."},
                                          {"type": "image", "source": {"type": "base64", "data": ""}}], at(50))]
                      + reply("1", usage(inp=100), when=at(49))
                      + ran("2", f"herd ask {AGY} 'ship the fix'", f"herd: asked {AGY} (ask {ask}), round 1 of 5", at(48)))
    opening = ("A herd guard checks each of your tool calls.  Your workspace is /w and your\ncommands run there.\n\n"
               "The herd keeps shared memory for this project -- curated facts every\nagent sees.")
    agy = _write(home / ".gemini" / "antigravity-cli" / "brain" / "b1" / ".system_generated" / "logs" / "transcript.jsonl",
                 [agy_said(frame(LEAD, AGY, f"[ask {ask} -- whoever asked is waiting]\nship the fix"), at(47), 0),
                  agy_reply(20, at(46), 1),
                  agy_said(opening, at(40), 2), agy_reply(300, at(39), 3),
                  agy_said("Tidy the docs.", at(30), 4), agy_reply(50, at(28), 5)])
    commit(dp, "docs.md", 9, at(29), message="docs")
    h.report(LEAD, cli="claude", conversation="lead", transcript=lead)
    h.report(AGY, cli="agy", conversation="b1", transcript=agy)
    monkeypatch.setattr(cost, "OPENINGS", ())
    monkeypatch.setattr(cost, "ROLES", ())
    monkeypatch.setattr(conversation, "LEAD_NOTE", re.compile(OLD_NOTE, re.S | re.M))
    old = counted(h)
    monkeypatch.setattr(cost, "OPENINGS", OPENINGS)
    monkeypatch.setattr(cost, "ROLES", ROLES)
    monkeypatch.setattr(conversation, "LEAD_NOTE", LEAD_NOTE)
    with h._mu:   # a store from before the rules were versioned
        h._conn.execute("DELETE FROM meta WHERE k LIKE 'cost_said%'")
        h._conn.commit()
    h._said_ok = False
    return h, ask, old


import conversation as _conversation  # noqa: E402

OPENINGS, ROLES, LEAD_NOTE = cost.OPENINGS, cost.ROLES, _conversation.LEAD_NOTE


def test_tasks_kept_by_older_rules_are_read_again(home, tmp_path, monkeypatch):
    """The herd's opening, read as a task by the older rules, is none: what
    its replies cost goes to the work its agent was on as it came -- the
    lead's ask -- and the lead's task, titled by the note, takes the user's
    words.  The commits kept are left as they were; the store is backed up
    first; the check passes and is marked."""
    h, ask, old = old_rules_herd(home, tmp_path, monkeypatch)
    titled = "[Image #1] [Image #2][What this project works on, as the herd sees it.  Read them:"
    assert set(old) == {titled, cost.OPENINGS[0] + "  Your workspace is /w and your", "Tidy the docs."}
    assert old[titled]["workers"] == 20 and old["Tidy the docs."]["lines"] == 9
    with h._mu:
        commits = h._cost_census()
    new = counted(h)
    assert set(new) == {"[Image #1] [Image #2]Ship the fix.", "Tidy the docs."}
    ship = new["[Image #1] [Image #2]Ship the fix."]
    assert (ship["own"], ship["workers"]) == (old[titled]["own"], 20 + 300)
    assert (new["Tidy the docs."]["own"], new["Tidy the docs."]["lines"]) == (50, 9)
    with h._mu:
        assert h._cost_census() == commits
        assert h._conn.execute("SELECT v FROM meta WHERE k = 'cost_said'").fetchone() == (str(cost.SAID_VERSION),)
        report = json.loads(h._conn.execute("SELECT v FROM meta WHERE k = 'cost_said_report'").fetchone()[0])
        # The agy agent is on what it was told last, as a reading now leaves it.
        assert h._conn.execute("SELECT item FROM cost_files WHERE agent = ?", (AGY,)).fetchone()[0].startswith("task:")
        assert not h._conn.execute("SELECT 1 FROM cost_usage WHERE item NOT IN"
                                   " (SELECT 'task:' || id FROM cost_tasks) AND item NOT LIKE 'ask:%'").fetchall()
    assert (report["ok"], report["unmade"], report["retitled"], report["moved"]) == (True, 1, 1, 300)
    backups = list((tmp_path / "backups").iterdir())
    assert len(backups) == 1 and report["backup"] == str(backups[0])
    import sqlite3
    with sqlite3.connect(backups[0]) as b:   # the store as it was before
        assert b.execute("SELECT COUNT(*) FROM cost_tasks").fetchone()[0] == 3
    # Done: not again -- and a run stopped half-way, done again, finds the
    # rest as it should be, with no second backup.
    h._said_ok = False
    assert counted(h) == new
    with h._mu:
        h._conn.execute("DELETE FROM meta WHERE k = 'cost_said'")
        h._conn.commit()
    h._said_ok = False
    assert counted(h) == new and len(list((tmp_path / "backups").iterdir())) == 1
    with h._mu:
        again = json.loads(h._conn.execute("SELECT v FROM meta WHERE k = 'cost_said_report'").fetchone()[0])
    assert (again["ok"], again["unmade"], again["retitled"], again["placed"]) == (True, 0, 0, 0)


def test_a_task_whose_words_are_gone_is_judged_by_its_first_line(home, tmp_path, monkeypatch):
    """Its transcript gone, a task is judged by what was kept of its words:
    the herd's opening is none, the user's words stay."""
    h, ask, old = old_rules_herd(home, tmp_path, monkeypatch)
    Path(h._transcript(AGY)).unlink()
    with h._tally_mu:
        h._cost_recheck()
    with h._mu:
        left = {p for (p,) in h._conn.execute("SELECT prompt FROM cost_tasks")}
        report = json.loads(h._conn.execute("SELECT v FROM meta WHERE k = 'cost_said_report'").fetchone()[0])
    assert left == {"[Image #1] [Image #2]Ship the fix.", "Tidy the docs."}
    assert (report["ok"], report["unread"], report["unmade"], report["dropped"]) == (True, 2, 1, 300)


def test_a_fresh_store_is_marked_with_no_backup(home, tmp_path):
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h._tally_pass()
    with h._mu:
        assert h._conn.execute("SELECT v FROM meta WHERE k = 'cost_said'").fetchone() == (str(cost.SAID_VERSION),)
    assert not (tmp_path / "backups").exists()
