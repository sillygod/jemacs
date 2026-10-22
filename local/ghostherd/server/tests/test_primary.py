"""Primary, the herd-wide lead (ask 9622f1e9): the user's one entry point
for what crosses projects.  It routes work to projects' leads by handoff
and keeps track of it -- from Telegram, free-form -- and asks no project's
agents, writes no memory but its own, and none of the agents ask it."""

import subprocess
from pathlib import Path

import pytest

import herd as herd_mod
import jsonrpc_handler
import shared_memory as sm
from herd import HerdStore, reset_herd
from jsonrpc_handler import JsonRpcHandler
from telegram import HELP, TelegramBot


def _git(*args, cwd):
    subprocess.run(["git", "-c", "user.email=t@t", "-c", "user.name=t", *args],
                   cwd=cwd, check=True, capture_output=True)


def _repo(path: Path) -> Path:
    path.mkdir(parents=True)
    _git("init", "-q", "-b", "main", cwd=path)
    _git("commit", "-q", "--allow-empty", "-m", "start", cwd=path)
    return path


@pytest.fixture
def world(tmp_path, monkeypatch):
    """Primary in the herd's own directory, two projects: ghost with a lead
    and a worker, fresh with nobody."""
    monkeypatch.setenv("GHOSTHERD_SHARED_MEMORY_DIR", str(tmp_path / "mem"))
    base = tmp_path.resolve()
    w = {"primary": base / "herd" / "primary", "ghost": _repo(base / "ghost"), "fresh": _repo(base / "fresh")}
    w["primary"].mkdir(parents=True)
    w["p"] = {k: str(v) + "/" for k, v in w.items()}
    w["snap"] = [
        {"name": "primary", "kind": "claude", "state": "idle", "project": w["p"]["primary"], "role": "primary"},
        {"name": "lead-ghost", "kind": "claude", "state": "idle", "project": w["p"]["ghost"], "role": "lead"},
        {"name": "rd-ghost", "kind": "claude", "state": "idle", "project": w["p"]["ghost"]},
    ]
    return w


@pytest.fixture
def h(world, tmp_path, monkeypatch):
    reset_herd()
    store = HerdStore(tmp_path / "herd.sqlite")
    store.tick(sessions=world["snap"])
    monkeypatch.setattr(herd_mod, "_herd", store)
    monkeypatch.setattr(jsonrpc_handler, "get_herd", lambda: store)
    yield store
    store.close()


def _deliver(h, snap):
    out = h.tick(sessions=snap)
    h.tick(sessions=snap, ack_ids=[p["id"] for p in out["pending"]])
    return out["pending"]


# ---- It routes: a handoff, with no project of its own to name ----------------


def test_primary_hands_off_and_hears_back(world, h):
    out = h.handoff(str(world["ghost"]), "The user wants the login fixed.", from_name="primary")
    assert (out["to"], out["starting"]) == ("lead-ghost", False)
    [mail] = _deliver(h, world["snap"])
    head = mail["body"].split("\n\n", 1)[0]
    assert head.startswith("[Handoff to ghost from primary, the herd-wide lead, for the user.")
    assert world["p"]["primary"] not in head  # no project of its own to name
    # The lead answers: back to primary, which tells the user.
    h.reply(out["id"], "Fixed on fix/login, PR 7.", from_name="lead-ghost")
    with h._mu:
        [(body,)] = h._conn.execute(
            "SELECT body FROM mail WHERE to_name = 'primary' AND status = 'queued'").fetchall()
    assert body.startswith(f"Answer to ask {out['id']}") and "PR 7" in body


def test_primary_starts_a_lead_where_there_is_none(world, h):
    out = h.handoff(str(world["fresh"]), "x", from_name="primary")
    assert out["starting"]
    with h._mu:
        [(args,)] = h._conn.execute("SELECT args FROM commands WHERE op = 'handoff'").fetchall()
    assert '"by": "primary"' in args


def test_primary_asks_no_projects_agents(world, h):
    with pytest.raises(ValueError, match="rd-ghost is in ghost, another project; cross-project work goes lead to lead"):
        h.ask("rd-ghost", "fix it", from_name="primary")
    with pytest.raises(ValueError, match="hand it to ghost's lead with `herd handoff ghost'"):
        h.ask("lead-ghost", "fix it", from_name="primary")


def test_no_agent_asks_primary_but_the_user_may(world, h):
    for asker in ("lead-ghost", "rd-ghost"):
        with pytest.raises(ValueError, match="primary is the herd-wide lead: it takes the user's requests"):
            h.ask("primary", "tell the user", from_name=asker)
    assert h.ask("primary", "what is everyone doing?", from_name="user")["to"] == "primary"


def test_primarys_place_is_no_project_to_hand_off_to(world, h):
    with pytest.raises(ValueError, match="is where primary, the herd-wide lead, works"):
        h.handoff(str(world["primary"]), "x", from_name="lead-ghost")


# ---- It reads every project; it writes only its own memory --------------------


def test_primary_lists_the_whole_herd(world, h):
    names = {s["name"] for s in JsonRpcHandler()._herd_list({"from": "primary"})["sessions"]}
    assert names == {"primary", "lead-ghost", "rd-ghost"}


def test_primary_writes_its_own_memory_and_reads_the_rest(world, h):
    rpc = JsonRpcHandler()
    out = rpc._herd_remember({"from": "primary", "text": "Login bugs go to ghost.", "name": "login-to-ghost"})
    assert out["store"] == sm.slug_for_path(world["p"]["primary"])
    rpc._herd_remember({"from": "lead-ghost", "text": "Deploy with make ship.", "name": "deploy"})
    got = rpc._herd_recall({"from": "primary", "project": world["p"]["ghost"], "project_given": True,
                            "query": "deploy"})
    assert "make ship" in got["text"]
    for params in ({"user": True}, {"project": world["p"]["ghost"], "project_given": True}):
        with pytest.raises(ValueError, match="primary writes only its own memory"):
            rpc._herd_remember(dict(params, **{"from": "primary", "text": "x", "name": "x"}))
        with pytest.raises(ValueError, match="primary writes only its own memory"):
            rpc._herd_forget(dict(params, **{"from": "primary", "name": "deploy"}))
    with pytest.raises(ValueError, match="primary writes only its own memory"):
        rpc._herd_promote_propose({"from": "primary", "project": world["p"]["ghost"], "project_given": True,
                                   "facts": ["deploy"], "file": "CLAUDE.md", "diff": "+x"})
    # A lead still writes the user store, as ever.
    assert rpc._herd_remember({"from": "lead-ghost", "user": True, "text": "y", "name": "y"})["store"] == "user"


# ---- Telegram: free-form words go to primary ------------------------------------


@pytest.fixture
def bot(world, h):
    return TelegramBot("tok", (99,), h)


def _commands(h):
    return [(c["op"], c["session"], c["args"], c["chat_id"]) for c in h.tick(sessions=h_snap(h))["commands"]]


def h_snap(h):
    with h._mu:
        rows = h._conn.execute("SELECT name, kind, state, project, role FROM snapshot").fetchall()
    return [dict(name=r[0], kind=r[1], state=r[2], project=r[3], role=r[4]) for r in rows]


def test_free_form_words_go_to_primary(world, h, bot):
    assert bot.handle_message(99, "what is everyone doing?")["text"] == "→ primary"
    assert _commands(h) == [("primary-say", "primary", {"body": "what is everyone doing?"}, 99)]
    # Not running: Emacs starts or resumes it -- and says so.
    h.tick(sessions=[r for r in world["snap"] if r["name"] != "primary"])
    assert bot.handle_message(99, "fix the login across projects")["text"].startswith("→ primary (starting it")
    # Commands are still commands.
    assert bot.handle_message(99, "/herd")["text"] != "→ primary"
    assert bot.handle_message(99, "/nope")["text"] == "unknown. /herd or /help"
    assert "goes to primary" in HELP


def test_a_reply_goes_to_whoever_pushed(world, h, bot):
    h.remember_pushed(99, 501, world["p"]["ghost"])
    h.remember_pushed(99, 502, world["p"]["primary"])
    # A reply to a lead's push: that project's lead, as before.
    assert bot.handle_message(99, "go on", reply_to=501)["text"] == "→ the lead of ghost"
    # A reply to primary's own report: primary.
    assert bot.handle_message(99, "and the other one?", reply_to=502)["text"] == "→ primary"
    assert _commands(h) == [
        ("lead-say", world["p"]["ghost"], {"project": world["p"]["ghost"], "body": "go on"}, 99),
        ("primary-say", "primary", {"body": "and the other one?"}, 99)]


def test_primarys_report_says_reply_to_primary(world, h, bot):
    h.report("primary", state="done", last="Handed the login fix to ghost; lead-ghost is on it.")
    h.tick(sessions=world["snap"], pushes=[{"kind": "report", "session": "primary",
                                            "project": world["p"]["primary"], "since": 0}])
    push = h.unsent_pushes()[-1]
    text, _ = bot.prepare_push(push, 0)
    assert text.startswith("✓ primary") and "lead-ghost is on it." in text
    assert text.endswith("↩ Reply to tell primary.")
