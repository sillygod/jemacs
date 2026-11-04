"""Handoffs: an ask from one project to another project's lead.

Projects are temp git repos; the herd's agents are a snapshot, as Emacs
would send it; Emacs starting a lead is a reply to the `handoff' command,
as its tick would carry it."""

import json
import subprocess
import time
from datetime import datetime, timezone
from pathlib import Path

import pytest

import herd as herd_mod
import jsonrpc_handler
import projects
import shared_memory
from herd import HerdStore, reset_herd
from jsonrpc_handler import JsonRpcHandler
from test_ask import HERD, live  # noqa: F401 -- the live sidecar fixture


def _git(*args, cwd):
    subprocess.run(["git", "-c", "user.email=t@t", "-c", "user.name=t", *args],
                   cwd=cwd, check=True, capture_output=True)


def _repo(path: Path) -> Path:
    path.mkdir(parents=True)
    _git("init", "-q", "-b", "main", cwd=path)
    _git("commit", "-q", "--allow-empty", "-m", "start", cwd=path)
    return path


@pytest.fixture
def world(tmp_path):
    """Four projects: ghost and unnoiac with a lead each, solo with an agent
    and no lead, new with nobody -- and a plain directory, not a checkout."""
    base = tmp_path.resolve()
    w = {k: _repo(base / k) for k in ("ghost", "unnoiac", "solo", "new")}
    (base / "plain").mkdir()
    w["plain"] = base / "plain"
    w["p"] = {k: str(v) + "/" for k, v in w.items() if isinstance(v, Path)}
    w["snap"] = [
        {"name": "lead-ghost", "kind": "claude", "state": "idle", "project": w["p"]["ghost"], "role": "lead"},
        {"name": "rd-ghost", "kind": "claude", "state": "working", "project": w["p"]["ghost"]},
        {"name": "lead-unnoiac", "kind": "claude", "state": "idle", "project": w["p"]["unnoiac"], "role": "lead"},
        {"name": "qa-unnoiac", "kind": "agy", "state": "idle", "project": w["p"]["unnoiac"]},
        {"name": "agy-solo", "kind": "agy", "state": "idle", "project": w["p"]["solo"]},
    ]
    return w


@pytest.fixture
def h(world, tmp_path):
    reset_herd()
    store = HerdStore(tmp_path / "herd.sqlite")
    store.tick(sessions=world["snap"])
    yield store
    store.close()


def _pending(h, snap):
    "One tick's deliveries, acked by the next: what Emacs does."
    out = h.tick(sessions=snap)
    h.tick(sessions=snap, ack_ids=[p["id"] for p in out["pending"]])
    return out["pending"]


def _mail_to(h, name):
    with h._mu:
        return [r[0] for r in h._conn.execute(
            "SELECT body FROM mail WHERE to_name = ? AND status = 'queued' ORDER BY created_at",
            (name,))]


def _commands(h, op="handoff"):
    with h._mu:
        return h._conn.execute(
            "SELECT id, session, args FROM commands WHERE op = ? ORDER BY created_at", (op,)
        ).fetchall()


# ---- Which directory a project named on the command line is -------------------


def test_a_project_is_named_by_path_leaf_or_slug(world, tmp_path, monkeypatch):
    known = [world["p"]["ghost"], world["p"]["unnoiac"]]
    un = str(world["unnoiac"])
    assert projects.resolve(un + "/", known) == un
    assert projects.resolve("unnoiac", known) == un
    assert projects.resolve(projects._slug(un), known) == un
    # Known only by its shared-memory store: the slug's directory is found
    # again on disk, dashes in names and all.
    odd = _repo(tmp_path.resolve() / "my-proj.v2")
    key = shared_memory.slug_for_path(str(odd))
    assert projects.resolve("my-proj.v2", [], [key]) == str(odd)
    assert projects.resolve(key, [], [key]) == str(odd)
    assert projects.path_for_slug(key) == str(odd)
    with pytest.raises(ValueError, match="no project nowhere that the herd knows.*ghost, unnoiac"):
        projects.resolve("nowhere", known)
    twin = _repo(tmp_path.resolve() / "elsewhere" / "unnoiac")
    with pytest.raises(ValueError, match="more than one project"):
        projects.resolve("unnoiac", known + [str(twin)])
    with pytest.raises(ValueError, match="not on this machine"):
        projects.resolve("gone", [], ["-no-such-dir-gone"])


def test_a_worktree_or_subdirectory_names_its_main_checkout(world, tmp_path):
    un = world["unnoiac"]
    (un / "src").mkdir()
    assert projects.checkout(str(un / "src")) == str(un)
    wt = tmp_path.resolve() / "wt-un"
    _git("worktree", "add", "-q", "-b", "feat", str(wt), cwd=un)
    assert projects.checkout(str(wt)) == str(un)
    assert projects.checkout(str(world["plain"])) == str(world["plain"])


# ---- Who may hand off -------------------------------------------------------------


def test_a_lead_hands_off_to_another_projects_lead(world, h):
    out = h.handoff(str(world["unnoiac"]), "The login API renamed `token' to `access_token'.",
                    from_name="lead-ghost")
    assert (out["to"], out["project"], out["starting"]) == ("lead-unnoiac", world["p"]["unnoiac"], False)
    [mail] = _pending(h, world["snap"])
    assert (mail["to"], mail["ask"]) == ("lead-unnoiac", out["id"])
    # The herd frames it: from which project and agent, that it waits, what to do.
    head = mail["body"].split("\n\n", 1)[0]
    assert head.startswith(f"[Handoff to unnoiac from lead-ghost, in project ghost ({world['p']['ghost']})")
    assert "waits on your answer" in head and "cannot be asked while it waits" in head
    assert mail["body"].endswith("The login API renamed `token' to `access_token'.")
    # The ask itself is the brief as written, and names both projects.
    ask = next(a for a in h.recent_asks()["asks"] if a["id"] == out["id"])
    assert ask["head"].startswith("The login API")
    assert (ask["project"], ask["from_project"]) == (world["p"]["unnoiac"], world["p"]["ghost"])
    got = h.get_ask(out["id"])
    assert (got["project"], got["from_project"]) == (world["p"]["unnoiac"], world["p"]["ghost"])


def test_a_worker_under_a_lead_is_told_to_tell_its_lead(world, h):
    with pytest.raises(ValueError, match="rd-ghost works under lead-ghost, ghost's lead: tell your lead"):
        h.handoff(str(world["unnoiac"]), "x", from_name="rd-ghost")
    assert h.recent_asks()["asks"] == []


def test_an_agent_with_no_lead_over_it_and_the_user_may(world, h):
    assert h.handoff(str(world["unnoiac"]), "x", from_name="agy-solo")["to"] == "lead-unnoiac"
    out = h.handoff(str(world["ghost"]), "from a shell", from_name="user")
    assert out["to"] == "lead-ghost"
    body = next(m for m in _pending(h, world["snap"]) if m["ask"] == out["id"])["body"]
    assert body.startswith("[Handoff to ghost from the user.  The user found work")
    assert "cannot be asked" not in body


def test_ones_own_project_is_not_a_handoff(world, h):
    with pytest.raises(ValueError, match="ghost is your own project: ask its agents"):
        h.handoff(str(world["ghost"]), "x", from_name="lead-ghost")


def test_a_dead_lead_is_said_plainly(world, h):
    snap = [dict(r, state="dead") if r["name"] == "lead-unnoiac" else r for r in world["snap"]]
    h.tick(sessions=snap)
    with pytest.raises(ValueError, match="lead-unnoiac, unnoiac's lead, is dead"):
        h.handoff(str(world["unnoiac"]), "x", from_name="lead-ghost")


# ---- A project with no lead gets one ------------------------------------------------


def test_no_lead_asks_emacs_to_start_one_once_and_binds_it(world, h):
    new = world["p"]["new"]
    a = h.handoff(str(world["new"]), "first", from_name="lead-ghost")
    b = h.handoff(str(world["new"]), "second", from_name="agy-solo")
    assert (a["to"], a["starting"], a["project"]) == ("", True, new)
    # One start for the project, however many handoffs wait on it.
    [(cid, session, args)] = _commands(h)
    assert session == "lead-ghost"
    assert '"project": "%s"' % new in args and '"from_project": "%s"' % world["p"]["ghost"] in args
    # Nothing to deliver while there is no lead.
    out = h.tick(sessions=world["snap"])
    assert [c["op"] for c in out["commands"]] == ["handoff"]
    assert out["pending"] == []
    assert {k["id"]: k["to"] for k in out["asks"]}[a["id"]] == ""
    # Emacs started lead-new: its reply rides the tick whose snapshot has it.
    snap = world["snap"] + [{"name": "lead-new", "kind": "claude", "state": "starting",
                             "project": new, "role": "lead"}]
    h.tick(sessions=snap, replies=[{"id": cid, "text": "started lead-new", "lead": "lead-new"}])
    assert h.get_ask(a["id"])["to"] == "lead-new" and h.get_ask(b["id"])["to"] == "lead-new"
    assert h.tick(sessions=snap)["pending"] == []          # still starting
    snap[-1] = dict(snap[-1], state="idle")
    delivered = _pending(h, snap)
    assert [(m["to"], m["ask"]) for m in delivered] == [("lead-new", a["id"])]  # one at a time
    assert delivered[0]["body"].startswith("[Handoff to new from lead-ghost, in project ghost")
    assert h.get_ask(a["id"])["status"] == "delivered"


def test_a_refused_start_fails_the_handoffs_and_says_why(world, h):
    a = h.handoff(str(world["new"]), "x", from_name="lead-ghost")             # not waiting
    b = h.handoff(str(world["new"]), "y", from_name="agy-solo", wait=True)
    [(cid, _, _)] = _commands(h)
    why = "the herd has started 3 leads for handoffs already (max 3); ask the user"
    h.tick(sessions=world["snap"], replies=[{"id": cid, "text": why}])
    for k in (a, b):
        got = h.get_ask(k["id"])
        assert (got["status"], got["error"]) == ("failed", why)
    # The one not waiting hears it as mail; the waiting one through its wait.
    assert _mail_to(h, "lead-ghost") == [f"Ask {a['id']} failed: {why}"]
    assert _mail_to(h, "agy-solo") == []


def test_a_start_emacs_never_answers_fails_in_time(world, h, monkeypatch):
    a = h.handoff(str(world["new"]), "x", from_name="lead-ghost")
    h.tick(sessions=world["snap"])
    assert h.get_ask(a["id"])["status"] == "queued"
    monkeypatch.setattr(herd_mod, "HANDOFF_START_SECONDS", 0)
    time.sleep(0.01)
    h.tick(sessions=world["snap"])
    got = h.get_ask(a["id"])
    assert got["status"] == "failed" and "did not start a lead in " + world["p"]["new"] in got["error"]


def test_a_missing_directory_or_one_not_a_checkout_is_refused(world, h, tmp_path):
    with pytest.raises(ValueError, match="no directory .*/nope/"):
        h.handoff(str(tmp_path.resolve() / "nope"), "x", from_name="lead-ghost")
    with pytest.raises(ValueError, match="plain/ is not a git checkout, and has no lead"):
        h.handoff(str(world["plain"]), "x", from_name="lead-ghost")
    assert _commands(h) == []
    # A directory that is not a checkout but has a lead takes the handoff.
    snap = world["snap"] + [{"name": "lead-plain", "kind": "claude", "state": "idle",
                             "project": world["p"]["plain"], "role": "lead"}]
    h.tick(sessions=snap)
    assert h.handoff(str(world["plain"]), "x", from_name="lead-ghost")["to"] == "lead-plain"


# ---- The answer, rounds and the loop rules are an ask's ------------------------------


def test_the_answer_comes_back_to_the_sender(world, h):
    a = h.handoff(str(world["unnoiac"]), "rename it", from_name="lead-ghost")
    b = h.handoff(str(world["unnoiac"]), "and this", from_name="agy-solo", wait=True)
    _pending(h, world["snap"])
    h.reply(a["id"], "Done on branch fix/token, PR 12.", from_name="lead-unnoiac")
    [mail] = _mail_to(h, "lead-ghost")
    assert mail.startswith(f"Answer to ask {a['id']} (rename it):") and "PR 12" in mail
    _pending(h, world["snap"])
    h.reply(b["id"], "Done too.", from_name="lead-unnoiac")
    got = h.get_ask(b["id"])
    assert (got["status"], got["reply"]) == ("answered", "Done too.")
    assert _mail_to(h, "agy-solo") == []      # it waits: it collects the answer itself


def test_nobody_hands_back_to_whoever_waits_across_projects(world, h):
    snap = world["snap"] + [{"name": "lead-new", "kind": "claude", "state": "idle",
                             "project": world["p"]["new"], "role": "lead"}]
    h.tick(sessions=snap)
    a = h.handoff(str(world["unnoiac"]), "x", from_name="lead-ghost")
    _pending(h, snap)
    # lead-unnoiac, answering ghost's handoff, may not hand back to it ...
    with pytest.raises(ValueError, match="lead-ghost is waiting on lead-unnoiac"):
        h.handoff(str(world["ghost"]), "back", from_name="lead-unnoiac")
    # ... nor may a third project it hands on to.
    b = h.handoff(str(world["new"]), "on", from_name="lead-unnoiac")
    assert b["to"] == "lead-new"
    _pending(h, snap)
    for back in ("ghost", "unnoiac"):
        with pytest.raises(ValueError, match="is waiting on lead-new"):
            h.handoff(str(world[back]), "back", from_name="lead-new")
    assert h.get_ask(a["id"])["status"] == "delivered"


def test_handoffs_to_a_project_count_as_rounds(world, h):
    # The user asks lead-ghost, one round with any one agent.
    top = h.ask("lead-ghost", "fix it", from_name="user", rounds=1)
    _pending(h, world["snap"])
    assert h.get_ask(top["id"])["status"] == "delivered"
    h.handoff(str(world["unnoiac"]), "one", from_name="lead-ghost")
    with pytest.raises(ValueError, match="lead-ghost has handed unnoiac work 1 times for ask .*, 1 at most"):
        h.handoff(str(world["unnoiac"]), "two", from_name="lead-ghost")
    # A lead still being started has no name yet: counted by its project.
    h.handoff(str(world["new"]), "one", from_name="lead-ghost")
    with pytest.raises(ValueError, match="handed new work 1 times"):
        h.handoff(str(world["new"]), "two", from_name="lead-ghost")


def test_a_run_of_handoffs_counts_by_project_too(world, h, monkeypatch):
    # Asked by nobody: rounds last its run.  A lead still being started has
    # no name, so the second handoff to its project counts by the project.
    monkeypatch.setattr(herd_mod, "ASK_ROUNDS", 1)
    h.handoff(str(world["new"]), "one", from_name="agy-solo")
    with pytest.raises(ValueError, match="agy-solo has handed new work 1 times in this run"):
        h.handoff(str(world["new"]), "two", from_name="agy-solo")


def test_a_lead_emacs_finds_waiting_on_the_sender_is_not_handed_it(world, h):
    # lead-unnoiac answers ghost's handoff, and hands work on to a
    # directory inside ghost: the snapshot names no lead there, but Emacs
    # finds the project's -- lead-ghost, which waits on it.  A loop: failed.
    first = h.handoff(str(world["unnoiac"]), "x", from_name="lead-ghost")
    _pending(h, world["snap"])
    sub = world["ghost"] / "sub"
    sub.mkdir()
    back = h.handoff(str(sub), "back", from_name="lead-unnoiac")
    assert back["starting"]
    [(cid, _, _)] = _commands(h)
    h.tick(sessions=world["snap"], replies=[{"id": cid, "text": "handed to lead-ghost", "lead": "lead-ghost"}])
    got = h.get_ask(back["id"])
    assert (got["status"], got["to"]) == ("failed", "")
    assert got["error"] == "lead-ghost is waiting on lead-unnoiac; answer its ask rather than ask it"
    assert h.get_ask(first["id"])["status"] == "delivered"


def test_a_handoff_reads_as_an_ask_in_the_rooms_chat(world, h):
    # As Emacs pastes it (`ghostherd--ask-deliver'): the frame and the brief
    # in the ask, and -- for a lead owed its role -- the role after it.
    import conversation
    out = h.handoff(str(world["unnoiac"]), "rename it", from_name="lead-ghost")
    [mail] = _pending(h, world["snap"])
    pasted = (
        "[ghostherd message from lead-ghost → lead-unnoiac]\n"
        f"[ask {out['id']} -- whoever asked is waiting for your answer]\n{mail['body']}\n\n"
        f"When you are done, send your answer with:\n/p/bin/herd reply {out['id']} <<'HERD'\n"
        "<your answer>\nHERD\nAnswer even if you could not do it, and say why.\n\n---\n\n"
        "Your role in this herd, given with your first message as your CLI takes none as it "
        "starts:\n\nYou are the lead of this project's herd of agents\n")
    e = conversation.user_entry(pasted, 0)
    assert (e["kind"], e["ask"], e["who"]) == ("ask", out["id"], "lead-ghost")
    assert e["text"].startswith("[Handoff to unnoiac from lead-ghost") and e["text"].endswith("rename it")
    assert "You are the lead" not in e["text"]


# ---- Over the wire --------------------------------------------------------------------


def test_rpc_resolves_a_leaf_name_to_the_main_checkout(world, h, tmp_path, monkeypatch):
    monkeypatch.setattr(jsonrpc_handler, "get_herd", lambda: h)
    rpc = JsonRpcHandler()
    out = rpc._herd_handoff({"project": "unnoiac", "body": "x", "from": "lead-ghost"})
    assert out["to"] == "lead-unnoiac"
    wt = tmp_path.resolve() / "wt-un2"
    _git("worktree", "add", "-q", "-b", "feat2", str(wt), cwd=world["unnoiac"])
    out = rpc._herd_handoff({"project": str(wt), "body": "y", "from": "agy-solo"})
    assert out["to"] == "lead-unnoiac"
    with pytest.raises(ValueError, match="tell your lead"):
        rpc._herd_handoff({"project": "unnoiac", "body": "x", "from": "rd-ghost"})


def test_rpc_knows_a_project_claude_ran_in_by_its_name(world, h, tmp_path, monkeypatch):
    # Nobody works in `new' now and it has no store; claude once ran there.
    # Primary routes to such projects by name.
    monkeypatch.setattr(jsonrpc_handler, "get_herd", lambda: h)
    claude_root = tmp_path / "claude-projects"
    (claude_root / projects._slug(str(world["new"]))).mkdir(parents=True)
    monkeypatch.setenv("GHOSTHERD_MEMORY_CLAUDE_ROOT", str(claude_root))
    out = JsonRpcHandler()._herd_handoff({"project": "new", "body": "x", "from": "lead-ghost"})
    assert (out["project"], out["starting"]) == (world["p"]["new"], True)
    monkeypatch.setenv("GHOSTHERD_MEMORY_CLAUDE_ROOT", str(tmp_path / "none"))
    with pytest.raises(ValueError, match="no project new that the herd knows"):
        JsonRpcHandler()._herd_handoff({"project": "new", "body": "y", "from": "lead-ghost"})


def test_client_hands_off_waits_for_a_lead_to_start_and_is_refused(live, world):  # noqa: F811
    h = herd_mod.get_herd()
    h.tick(sessions=world["snap"])
    # To a project with a lead: an ask's id, as `ask' prints it.
    out = run(live, "handoff", str(world["unnoiac"]) + "/", "the brief", session="lead-ghost")
    assert out.returncode == 0, out.stderr
    assert "handed off to lead-unnoiac, unnoiac's lead (ask %s)" % out.stdout.strip() in out.stderr
    # A worker under a lead: refused, saying whom to tell.
    out = run(live, "handoff", "unnoiac", "x", session="rd-ghost")
    assert out.returncode == 1 and "tell your lead" in out.stderr
    # A path relative to the shell's cwd, not the sidecar's.
    out = _start(live, "handoff", "../new", "-", stdin="from stdin\n", session="lead-ghost",
                 cwd=str(world["ghost"]))
    cid = _wait_command(h)
    snap = world["snap"] + [{"name": "lead-new", "kind": "claude", "state": "starting",
                             "project": world["p"]["new"], "role": "lead"}]
    h.tick(sessions=snap, replies=[{"id": cid, "text": "started lead-new", "lead": "lead-new"}])
    stdout, stderr = out.communicate(timeout=20)
    assert out.returncode == 0, stderr
    assert "new has no lead: the herd starts one there" in stderr
    assert "started lead-new, new's lead" in stderr
    assert h.get_ask(stdout.strip())["body"] == "from stdin\n"
    # Refused by Emacs: the reason, and a failure's exit status.
    out = _start(live, "handoff", "solo", "x", session="lead-ghost")
    cid = _wait_command(h)
    h.tick(sessions=world["snap"],
           replies=[{"id": cid, "text": "the herd is set not to start a lead for a handoff"}])
    _, stderr = out.communicate(timeout=20)
    assert out.returncode == 4 and "set not to start a lead" in stderr


def run(live, *args, session, stdin=None, cwd=None):
    e = dict(live["env"], GHOSTHERD_SESSION=session)
    return subprocess.run([str(HERD), *args], input=stdin, capture_output=True, text=True,
                          env=e, cwd=cwd, timeout=30)


def _start(live, *args, session, stdin=None, cwd=None):
    "The client running in the background, its stdin given and closed."
    e = dict(live["env"], GHOSTHERD_SESSION=session)
    proc = subprocess.Popen([str(HERD), *args], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, text=True, env=e, cwd=cwd)
    proc.stdin.write(stdin or "")
    proc.stdin.close()
    proc.stdin = None
    return proc


def _wait_command(h, seconds=10):
    "The id of the next `handoff' command, once the client has queued it."
    end = time.monotonic() + seconds
    while time.monotonic() < end:
        with h._mu:
            row = h._conn.execute(
                "SELECT id FROM commands WHERE op = 'handoff' AND status = 'queued'").fetchone()
        if row:
            with h._mu:
                h._conn.execute("UPDATE commands SET status = 'taken' WHERE id = ?", (row[0],))
                h._conn.commit()
            return row[0]
        time.sleep(0.05)
    raise AssertionError("no handoff command queued")


# ---- Cross-project `herd ask' is refused, pointing to a handoff ---------------
# Decided with the user: cross-project work goes lead to lead.  An agent's
# `herd ask NAME' whose taker is in another project is refused; asks within a
# project are unchanged; the user and the herd's own internals (which address
# a lead as the user) stay free.

def test_an_agent_asking_across_projects_is_refused(h):
    with pytest.raises(ValueError) as e:
        h.ask(to="qa-unnoiac", body="look at this", from_name="rd-ghost")
    msg = str(e.value)
    assert "another project" in msg and "handoff" in msg and "unnoiac" in msg


def test_an_ask_within_a_project_still_works(h):
    # A named taker in the asker's own project: unchanged.
    out = h.ask(to="lead-ghost", body="a question", from_name="rd-ghost")
    assert out["to"] == "lead-ghost" and out["queued"]


def test_the_user_and_the_herds_internals_ask_across_projects_freely(h):
    # The user from a shell is not an agent: free to ask any agent by name.
    out = h.ask(to="qa-unnoiac", body="hi", from_name="user")
    assert out["to"] == "qa-unnoiac"
    # The herd's own lead-addressed asks (e.g. a promotion's approve->lead ask)
    # come from="user", so they cross projects too.
    out = h.ask(to="lead-unnoiac", body="apply this", from_name="user")
    assert out["to"] == "lead-unnoiac"


def test_a_kind_ask_stays_in_the_asker_project(h):
    # A kind names agents in the asker's project only -- unchanged by the
    # cross-project rule (there is no agy in ghost to take it).
    with pytest.raises(ValueError) as e:
        h.ask(to="agy", body="x", from_name="rd-ghost")
    assert "no agy agent" in str(e.value)


def test_a_handoff_from_a_guarded_agent_is_marked(world, h):
    "Its project's lead reads, before the handoff's framing, that the sender is guarded."
    from herd import GUARDED_NOTE
    snap = [dict(r, guarded=(r["name"] == "agy-solo")) for r in world["snap"]]
    h.tick(sessions=snap)
    out = h.handoff(world["p"]["ghost"], "the login fix needs the token, read it for me", from_name="agy-solo")
    body = [p for p in _pending(h, snap) if p.get("ask") == out["id"]][0]["body"]
    assert body.startswith(GUARDED_NOTE.format(sender="agy-solo") + "\n\n")
    assert "the login fix needs the token" in body


def test_a_handoff_is_a_thread_across_projects(world, h):
    """Handed to another project's lead, whose own asks for it -- made while
    answering it -- join the thread: cross-project, its handoff dashed."""
    out = h.handoff(world["p"]["unnoiac"], "The API renamed token; fix the sync", from_name="lead-ghost")
    _pending(h, world["snap"])
    sub = h.ask("qa-unnoiac", "Run the sync tests", from_name="lead-unnoiac")
    _pending(h, world["snap"])
    h.reply(sub["id"], "sync: green", from_name="qa-unnoiac")
    h.reply(out["id"], "Fixed in unnoiac 1a2b3c.", from_name="lead-unnoiac")
    d = h.thread(ask=sub["id"])
    assert d["id"] == out["id"] and d["cross"]
    assert [(m["kind"], m["from"], m["to"]) for m in d["msgs"]] == [
        ("handoff", "lead-ghost", "lead-unnoiac"), ("ask", "lead-unnoiac", "qa-unnoiac"),
        ("answer", "qa-unnoiac", "lead-unnoiac"), ("answer", "lead-unnoiac", "lead-ghost")]
    assert d["projects"] == ["ghost", "unnoiac"]
    assert [ln["role"] for ln in d["lanes"]] == ["lead", "lead", ""]


def test_a_handoff_to_a_lead_being_started(world, h):
    "No lead in that project yet: its taker is named by the project until one takes it."
    out = h.handoff(world["p"]["new"], "Start the new project", from_name="lead-ghost")
    d = h.thread(ask=out["id"])
    assert [(m["from"], m["to"]) for m in d["msgs"]] == [("lead-ghost", "lead of new")]
    talk = h.threads()["talking"]
    assert [(t["to"], t["kind"], t["status"]) for t in talk if t["ask"] == out["id"]] == [("lead of new", "handoff", "queued")]


# ---- The leads the herd starts for handoffs: two caps, a queue past them ---------
# Asked 0ade8eeb: the cap counted leads alive, so one that answered and sat
# idle held its slot -- "3 until the user kills one" -- and a handoff past
# it was refused, which a lead retried in a loop.  Now the cap counts the
# leads AT WORK (owing a handoff its answer), a handoff past it is queued
# first come first served, and a second, higher cap on the ones UP stops
# the one idle longest that owes nothing to make room.


def _herd_lead(name, project, state="idle", since=0.0, by="lead-ghost", **kw):
    "A lead the herd started for a handoff, as Emacs's snapshot has it."
    return dict({"name": name, "kind": "claude", "state": state, "project": project,
                 "role": "lead", "handoffBy": by, "since": since}, **kw)


@pytest.fixture
def more(world, tmp_path):
    "Six more checkouts with no lead: the projects handed work."
    base = tmp_path.resolve()
    p = {k: str(_repo(base / k)) + "/" for k in ("fox", "parker", "leopard", "genesis", "venom", "argo")}
    return p


def _caps(h, snap, **caps):
    return h.tick(sessions=snap, handoff_caps=caps)


def _bind(h, snap, cid, lead):
    "Emacs started LEAD for the `handoff' command CID: its reply, on the tick whose snapshot has it."
    return h.tick(sessions=snap, replies=[{"id": cid, "text": "started " + lead, "lead": lead}])


def _answer(h, ask_id, by):
    h.reply(ask_id, "done", from_name=by)


def _slots(h):
    return h.handoff_slots()


def test_the_cap_counts_leads_at_work_not_leads_up(world, more, h):
    # Three leads the herd started for handoffs, all answered and idle: none
    # at work, so a fourth project's lead starts -- the 13:34 parker case.
    snap = world["snap"] + [_herd_lead("lead-" + k, more[k]) for k in ("fox", "leopard", "genesis")]
    _caps(h, snap, working=3, alive=8)
    s = _slots(h)
    assert (s["working"], s["alive"]) == (0, 3)
    out = h.handoff(more["parker"], "x", from_name="lead-ghost")
    assert (out["starting"], out.get("waiting")) == (True, False)
    assert [json.loads(a)["project"] for _, _, a in _commands(h)] == [more["parker"]]
    # A lead that owes a handoff its answer counts; a user's own lead never does.
    snap2 = snap + [{"name": "lead-venom", "kind": "claude", "state": "working", "project": more["venom"], "role": "lead"}]
    h.tick(sessions=snap2)
    for k in ("fox", "leopard"):
        h.handoff(more[k], "more for " + k, from_name="lead-ghost")
    h.handoff(more["venom"], "for the user's lead", from_name="lead-ghost")
    s = _slots(h)
    assert s["working"] == 3          # fox, leopard, and parker's start under way
    assert s["alive"] == 4            # fox, leopard, genesis, and parker's start
    assert sorted(l["name"] for l in s["leads"] if l["working"]) == ["lead-fox", "lead-leopard"]


def test_past_the_cap_a_handoff_waits_its_turn_first_come_first_served(world, more, h):
    snap = world["snap"] + [_herd_lead("lead-fox", more["fox"])]
    _caps(h, snap, working=1, alive=8)
    busy = h.handoff(more["fox"], "fox work", from_name="lead-ghost")     # fox now at work
    a = h.handoff(more["parker"], "parker work", from_name="lead-ghost")
    b = h.handoff(more["leopard"], "leopard work", from_name="agy-solo")
    c = h.handoff(more["parker"], "more parker work", from_name="agy-solo")
    assert (a["starting"], a["waiting"], a["ahead"]) == (False, True, 0)
    assert (b["waiting"], b["ahead"]) == (True, 1)
    assert (c["waiting"], c["ahead"]) == (True, 0)      # joins parker's place in line
    assert "1 lead at work on handoffs, 1 at most (lead-fox)" in a["why"]
    assert _commands(h) == []                            # nothing started past the cap
    # The page's cards: waiting for a slot, not for a start.
    assert {k["id"]: k["waiting"] for k in h.recent_asks()["asks"]}[a["id"]] is True
    assert {k["id"]: k["waiting"] for k in h.recent_asks()["asks"]}[busy["id"]] is False
    # Its sender is told the same, by `herd status' / --wait.
    got = h.get_ask(a["id"])
    assert (got["status"], got["to"], got["waiting"], got["ahead"]) == ("queued", "", True, 0)
    q = _slots(h)["queue"]
    assert [(x["project"], x["from"], x["asks"], x["ahead"], x["starting"]) for x in q] == [
        (more["parker"], "lead-ghost", 2, 0, False), (more["leopard"], "agy-solo", 1, 1, False)]
    # Waiting is not failing: long past the start timeout, still queued.
    with h._mu:
        h._conn.execute("UPDATE asks SET created_at = '2020-01-01T00:00:00+00:00' WHERE id = ?", (a["id"],))
        h._conn.commit()
    h.tick(sessions=snap)
    assert h.get_ask(a["id"])["status"] == "queued"
    # fox answers: its slot goes to parker, first in line -- leopard waits on.
    _pending(h, snap)
    _answer(h, busy["id"], "lead-fox")
    out = h.tick(sessions=snap)
    [(cid, session, args)] = _commands(h)
    assert json.loads(args)["project"] == more["parker"] and session == "lead-ghost"
    assert {k["id"]: k["waiting"] for k in h.recent_asks()["asks"]}[a["id"]] is False   # starting now
    assert [c["op"] for c in out["commands"]] == ["handoff"]
    assert h.get_ask(b["id"])["waiting"] is True
    # parker's lead starts and owes both its handoffs: leopard still waits.
    snap2 = snap + [_herd_lead("lead-parker", more["parker"], state="starting")]
    _bind(h, snap2, cid, "lead-parker")
    assert {h.get_ask(k["id"])["to"] for k in (a, c)} == {"lead-parker"}
    assert h.get_ask(b["id"])["waiting"] is True
    assert len(_commands(h)) == 1
    # Both answered, parker idles: leopard's turn.
    for k in (a, c):
        _answer(h, k["id"], "lead-parker")
    h.tick(sessions=snap2)
    assert [json.loads(a)["project"] for _, _, a in _commands(h)] == [more["parker"], more["leopard"]]


def test_a_queued_handoff_can_be_cancelled(world, more, h):
    snap = world["snap"] + [_herd_lead("lead-fox", more["fox"])]
    _caps(h, snap, working=1, alive=8)
    busy = h.handoff(more["fox"], "fox work", from_name="lead-ghost")
    a = h.handoff(more["parker"], "x", from_name="lead-ghost")
    b = h.handoff(more["leopard"], "y", from_name="lead-ghost")
    assert h.cancel(a["id"])["status"] == "cancelled"
    assert h.get_ask(b["id"])["ahead"] == 0             # moved up
    assert [x["project"] for x in _slots(h)["queue"]] == [more["leopard"]]
    # A start asked for and not yet taken by Emacs is withdrawn with its last handoff.
    _answer(h, busy["id"], "lead-fox")
    with h._mu:
        h._admit_handoffs(datetime.now(timezone.utc).isoformat())
        h._conn.commit()
    [(cid, _, _)] = _commands(h)
    h.cancel(b["id"])
    with h._mu:
        status = h._conn.execute("SELECT status FROM commands WHERE id = ?", (cid,)).fetchone()[0]
        starts = h._conn.execute("SELECT COUNT(*) FROM handoff_starts").fetchone()[0]
    assert (status, starts) == ("done", 0)
    assert h.tick(sessions=snap)["commands"] == []


def test_a_project_that_got_a_lead_meanwhile_takes_its_handoffs(world, more, h):
    snap = world["snap"] + [_herd_lead("lead-fox", more["fox"])]
    _caps(h, snap, working=1, alive=8)
    h.handoff(more["fox"], "fox work", from_name="lead-ghost")
    a = h.handoff(more["parker"], "for parker", from_name="lead-ghost")
    assert h.get_ask(a["id"])["waiting"]
    # The user starts parker's lead themselves: no slot needed, it takes it.
    snap2 = snap + [{"name": "lead-parker", "kind": "claude", "state": "idle",
                     "project": more["parker"], "role": "lead"}]
    h.tick(sessions=snap2)
    assert h.get_ask(a["id"])["to"] == "lead-parker"
    assert _commands(h) == []
    delivered = _pending(h, snap2)
    assert ("lead-parker", a["id"]) in [(m["to"], m["ask"]) for m in delivered]
    # One whose lead is dead meanwhile: failed, as a handoff to it is refused.
    b = h.handoff(more["leopard"], "for leopard", from_name="lead-ghost")
    snap3 = snap2 + [{"name": "lead-leopard", "kind": "claude", "state": "dead",
                      "project": more["leopard"], "role": "lead"}]
    h.tick(sessions=snap3)
    got = h.get_ask(b["id"])
    assert got["status"] == "failed" and "lead-leopard, leopard's lead, is dead" in got["error"]


def test_raising_the_cap_starts_what_waits_at_once(world, more, h):
    snap = world["snap"] + [_herd_lead("lead-fox", more["fox"])]
    _caps(h, snap, working=1, alive=8)
    h.handoff(more["fox"], "fox work", from_name="lead-ghost")
    a = h.handoff(more["parker"], "x", from_name="lead-ghost")
    b = h.handoff(more["leopard"], "y", from_name="lead-ghost")
    out = _caps(h, snap, working=3, alive=8)
    assert sorted(c["args"]["project"] for c in out["commands"]) == sorted([more["parker"], more["leopard"]])
    assert all(h.get_ask(k["id"])["starting"] for k in (a, b))
    # Lowered, it starts nothing more -- and stops nothing at work.
    out = _caps(h, snap, working=1, alive=8)
    assert out["commands"] == [] and out["handoff_slots"]["working_cap"] == 1


def test_the_caps_come_only_from_emacs_and_only_sane(world, h):
    for bad in (0, -1, 51, "5", 2.5, True, None):
        _caps(h, world["snap"], working=bad, alive=bad)
        assert h.handoff_slots()["working_cap"] == herd_mod.HANDOFF_WORKING
        assert h.handoff_slots()["alive_cap"] == herd_mod.HANDOFF_ALIVE
    _caps(h, world["snap"], working=5, alive=12)
    assert (h.handoff_slots()["working_cap"], h.handoff_slots()["alive_cap"]) == (5, 12)
    # The tick is the herd's own: no agent's token reaches it.
    assert "herd_tick" not in jsonrpc_handler.AGENT_METHODS
    assert not any("cap" in m for m in jsonrpc_handler.AGENT_METHODS)


def test_an_agent_cannot_set_the_caps_over_the_wire(live, world):  # noqa: F811
    import urllib.request
    h = herd_mod.get_herd()
    h.tick(sessions=world["snap"])
    body = json.dumps({"jsonrpc": "2.0", "id": 1, "method": "herd_tick",
                       "params": {"sessions": world["snap"], "handoff_caps": {"working": 9}}}).encode()
    req = urllib.request.Request(live["url"], data=body, headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=10) as r:
        out = json.loads(r.read())
    assert "error" in out
    assert h.handoff_slots()["working_cap"] == herd_mod.HANDOFF_WORKING


def test_set_not_to_start_a_lead_is_refused_at_once(world, h):
    _caps(h, world["snap"], may_start=False)
    with pytest.raises(ValueError, match="ghostherd-handoff-may-start-lead is off"):
        h.handoff(str(world["new"]), "x", from_name="lead-ghost")
    assert _commands(h) == []


def test_a_restart_neither_loses_nor_repeats_a_queued_handoff(world, more, h, tmp_path):
    snap = world["snap"] + [_herd_lead("lead-fox", more["fox"])]
    _caps(h, snap, working=1, alive=8)
    busy = h.handoff(more["fox"], "fox work", from_name="lead-ghost")
    a = h.handoff(more["parker"], "x", from_name="lead-ghost")
    # The sidecar restarts: the queue is in its file.
    h.close()
    h2 = HerdStore(tmp_path / "herd.sqlite")
    try:
        assert h2.get_ask(a["id"])["waiting"] is True
        # Emacs restarts: before it has its herd again, nothing starts and nothing fails.
        out = h2.tick(sessions=[], restored=False)
        assert out["commands"] == [] and h2.get_ask(a["id"])["status"] == "queued"
        h2.tick(sessions=snap, restored=True)
        _answer(h2, busy["id"], "lead-fox")
        out = h2.tick(sessions=snap, restored=True)
        [cmd] = out["commands"]
        assert cmd["args"]["project"] == more["parker"]
        # Emacs took the start and went down before answering it.  The next
        # one, fresh, is not handed it before it has its herd ...
        out = h2.tick(sessions=[], restored=False)
        assert out["commands"] == []
        with h2._mu:
            assert h2._conn.execute("SELECT COUNT(*) FROM handoff_starts").fetchone()[0] == 0
        # ... and finds the lead the last one did start: it takes the
        # handoff, and no second lead is started.
        snap2 = snap + [_herd_lead("lead-parker", more["parker"], state="idle")]
        out = h2.tick(sessions=snap2, restored=True)
        assert out["commands"] == []
        assert h2.get_ask(a["id"])["to"] == "lead-parker"
        # Had it not started one, the start is asked for again -- once.
        b = h2.handoff(more["leopard"], "y", from_name="lead-ghost")
        assert h2.get_ask(b["id"])["waiting"]                      # parker owes its answer
        _pending(h2, snap2)
        _answer(h2, a["id"], "lead-parker")
        [cmd] = h2.tick(sessions=snap2, restored=True)["commands"]
        h2.tick(sessions=[], restored=False)
        out = h2.tick(sessions=snap2, restored=True)
        assert [c["args"]["project"] for c in out["commands"]] == [more["leopard"]]
        assert h2.tick(sessions=snap2, restored=True)["commands"] == []
    finally:
        h2.close()


# -- The cap on the leads UP: the one idle longest that owes nothing is stopped --


def _stop_of(h):
    [(_, _, args)] = _commands(h)
    return json.loads(args)["stop"]


@pytest.mark.parametrize("why, change", [
    ("an open ask delivered or queued to it", "ask_to"),
    ("an ask of its own still open", "ask_from"),
    ("a live agent it started", "started"),
    ("blocked", "blocked"),
    ("at work", "working"),
    ("a draft in its box (Emacs says so)", "draft"),
    ("you looking at it (Emacs says so)", "watched"),
])
def test_past_the_cap_up_the_idle_longest_that_owes_nothing_is_stopped(world, more, h, why, change):
    # fox idle longest, leopard after it; genesis is the user's own lead,
    # idle longest of all, and never the herd's to stop.
    fox = _herd_lead("lead-fox", more["fox"], since=100.0)
    leo = _herd_lead("lead-leopard", more["leopard"], since=200.0)
    user = {"name": "lead-genesis", "kind": "claude", "state": "idle", "project": more["genesis"],
            "role": "lead", "since": 10.0}
    snap = world["snap"] + [fox, leo, user]
    _caps(h, snap, working=3, alive=2)
    # As it stands: fox is the one to stop.
    with h._mu:
        assert h._handoff_slots()["stoppable"] == ["lead-fox", "lead-leopard"]
    # With fox held by WHY, leopard is the one.
    extra = []
    if change == "ask_to":
        h.handoff(more["fox"], "fox work", from_name="lead-ghost")
    elif change == "ask_from":
        h.tick(sessions=snap)
        h.handoff(str(world["ghost"]), "a question for ghost", from_name="lead-fox")
    elif change == "started":
        extra = [{"name": "rd-fox", "kind": "claude", "state": "idle", "project": more["fox"],
                  "startedBy": "lead-fox"}]
    elif change in ("blocked", "working"):
        fox = dict(fox, state=change)
    elif change == "draft":
        fox = dict(fox, held="a draft in its box")
    elif change == "watched":
        fox = dict(fox, held="you are looking at it")
    snap = world["snap"] + [fox, leo, user] + extra
    h.tick(sessions=snap)
    out = h.handoff(more["parker"], "x", from_name="lead-ghost")
    assert out["starting"], (why, out)
    assert _stop_of(h) == "lead-leopard", why


def test_past_the_cap_up_with_none_to_stop_the_handoff_waits(world, more, h):
    snap = world["snap"] + [_herd_lead("lead-fox", more["fox"], held="a draft in its box"),
                            _herd_lead("lead-leopard", more["leopard"], state="working")]
    _caps(h, snap, working=3, alive=2)
    a = h.handoff(more["parker"], "x", from_name="lead-ghost")
    assert (a["waiting"], a["ahead"]) == (True, 0)
    assert "2 leads up that it started for handoffs, 2 at most, and none it may stop" in a["why"]
    assert _commands(h) == []
    # The draft sent: fox owes nothing now, and is the one stopped.
    snap[-2] = dict(snap[-2], held="")
    h.tick(sessions=snap)
    assert _stop_of(h) == "lead-fox"


def test_a_stop_emacs_will_not_make_leaves_the_handoff_waiting(world, more, h):
    snap = world["snap"] + [_herd_lead("lead-fox", more["fox"], since=1.0),
                            _herd_lead("lead-leopard", more["leopard"], since=2.0)]
    _caps(h, snap, working=3, alive=2)
    a = h.handoff(more["parker"], "x", from_name="lead-ghost")
    [(cid, _, args)] = _commands(h)
    assert json.loads(args)["stop"] == "lead-fox"
    # Taken into use meanwhile: Emacs says wait, not fail.
    h.tick(sessions=snap, replies=[{"id": cid, "text": "lead-fox is in use now", "wait": True}])
    got = h.get_ask(a["id"])
    assert got["status"] == "queued" and got["to"] == ""
    # The next start stops leopard; fox is let be a while.
    cmds = _commands(h)
    assert len(cmds) == 2 and json.loads(cmds[-1][2])["stop"] == "lead-leopard"


def test_the_client_says_queued_not_refused(live, world, more):  # noqa: F811
    h = herd_mod.get_herd()
    snap = world["snap"] + [_herd_lead("lead-fox", more["fox"])]
    h.tick(sessions=snap, handoff_caps={"working": 1, "alive": 8})
    h.handoff(more["fox"], "fox work", from_name="lead-ghost")
    out = run(live, "handoff", more["parker"], "the brief", session="lead-ghost")
    assert out.returncode == 0, out.stderr
    assert "parker has no lead, and the herd has 1 lead at work on handoffs, 1 at most (lead-fox): " \
           "this handoff is queued for a slot, 0 ahead of it" in out.stderr
    assert "It is not refused -- do not hand it off again" in out.stderr
    assert h.get_ask(out.stdout.strip())["waiting"] is True
    # --wait: still open when its time is up -- the answer comes as a message.  Not a failure.
    out = run(live, "handoff", more["leopard"], "more", "--wait", "--timeout", "2", session="lead-ghost")
    assert out.returncode == 3, out.stderr
    assert "queued for a slot, 1 ahead of it" in out.stderr and "still open after 2s" in out.stderr
    assert "failed" not in out.stderr and "refused" not in out.stderr.replace("not refused", "")
    # A status line as each change comes, not once a step: next in line,
    # its lead starting, its lead -- then the answer.
    proc = _start(live, "handoff", more["genesis"], "and more", "--wait", "--timeout", "120", session="lead-ghost")
    time.sleep(1.5)
    queued = [a for a in h.recent_asks()["asks"] if a["project"] == more["parker"]]
    h.cancel(queued[0]["id"])                                  # genesis moves up
    h.cancel(next(a for a in h.recent_asks()["asks"] if a["project"] == more["leopard"])["id"])
    time.sleep(1.5)
    fox = next(a for a in h.recent_asks()["asks"] if a["project"] == more["fox"])
    h.reply(fox["id"], "done", from_name="lead-fox")           # a slot frees: genesis starts
    [cmd] = h.tick(sessions=snap)["commands"]
    cid = cmd["id"]
    time.sleep(1.5)
    snap2 = snap + [_herd_lead("lead-genesis", more["genesis"], state="idle")]
    h.tick(sessions=snap2, replies=[{"id": cid, "text": "started lead-genesis", "lead": "lead-genesis"}])
    time.sleep(1.5)
    mine = next(a for a in h.recent_asks()["asks"] if a["project"] == more["genesis"])
    h.reply(mine["id"], "genesis done", from_name="lead-genesis")
    stdout, stderr = proc.communicate(timeout=60)
    assert proc.returncode == 0, stderr
    lines = [ln for ln in stderr.splitlines() if ln.startswith("herd: ")]
    assert "2 ahead of it" in lines[0]
    assert any("queued for a lead slot, 0 ahead of it" in ln for ln in lines), lines
    assert any("genesis's lead is starting" in ln for ln in lines), lines
    # Bound to its lead, the ask's own lines, as for any ask.
    assert any(ln.startswith("herd: lead-genesis is idle; the ask waits until it is free") for ln in lines), lines
    assert stdout.strip() == "genesis done"
