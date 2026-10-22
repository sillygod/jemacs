"""Handoffs: an ask from one project to another project's lead.

Projects are temp git repos; the herd's agents are a snapshot, as Emacs
would send it; Emacs starting a lead is a reply to the `handoff' command,
as its tick would carry it."""

import subprocess
import time
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
