"""Hooks: what claude's, grok's and agy's lifecycle hooks tell the herd."""

import importlib.machinery
import importlib.util
import json
import os
import subprocess
import sys
import time
from pathlib import Path

import pytest

import herd as herd_mod
from herd import HerdStore, reset_herd
from tests.test_ask import HERD, SNAP, _deliver, live  # noqa: F401  (fixture)


def _client():
    "bin/herd as a module -- without leaving a __pycache__ in bin/."
    loader = importlib.machinery.SourceFileLoader("herd_client", str(HERD))
    spec = importlib.util.spec_from_loader("herd_client", loader)
    mod = importlib.util.module_from_spec(spec)
    was, sys.dont_write_bytecode = sys.dont_write_bytecode, True
    try:
        loader.exec_module(mod)
    finally:
        sys.dont_write_bytecode = was
    return mod


hc = _client()


def _jsonl(path: Path, records) -> str:
    path.write_text("".join(json.dumps(r) + "\n" for r in records))
    return str(path)


def _news(p, event=None):
    return {k: v for k, v in hc.hook_report(p, event).items() if v}


# ---- claude ------------------------------------------------------------

CLAUDE = {"session_id": "c0ffee00-1111-2222-3333-444455556666",
          "transcript_path": "/nowhere.jsonl", "cwd": "/src/a"}


def test_claude_events():
    assert _news(dict(CLAUDE, hook_event_name="UserPromptSubmit", prompt="hi")) == {
        "cli": "claude", "conversation": CLAUDE["session_id"],
        "transcript": "/nowhere.jsonl", "state": "working"}
    assert _news(dict(CLAUDE, hook_event_name="SessionStart"))["conversation"] == CLAUDE["session_id"]
    assert "state" not in _news(dict(CLAUDE, hook_event_name="SessionStart"))
    blocked = _news(dict(CLAUDE, hook_event_name="Notification", notification_type="permission_prompt",
                         message="Claude needs your permission to use Bash"))
    assert (blocked["state"], blocked["reason"]) == ("blocked", "Claude needs your permission to use Bash")
    # Older payloads carry no type: the message says it.
    assert _news(dict(CLAUDE, hook_event_name="Notification",
                      message="Claude needs your permission to use Bash"))["state"] == "blocked"
    assert "state" not in _news(dict(CLAUDE, hook_event_name="Notification", notification_type="idle_prompt",
                                     message="Claude is waiting for your input"))
    assert "state" not in _news(dict(CLAUDE, hook_event_name="PreToolUse"))


def _user(text):
    return {"type": "user", "message": {"role": "user", "content": text}}


def _tool_result():
    return {"type": "user", "message": {"role": "user", "content": [
        {"type": "tool_result", "tool_use_id": "t1", "content": "ok"}]}}


def _assistant(mid, *blocks, sidechain=False):
    rec = {"type": "assistant", "message": {"id": mid, "role": "assistant", "content": list(blocks)}}
    if sidechain:
        rec["isSidechain"] = True
    return rec


def _text(t):
    return {"type": "text", "text": t}


def test_claude_stop_reads_the_answer_that_ended_the_turn(tmp_path):
    path = _jsonl(tmp_path / "c.jsonl", [
        _user("an older question"),
        _assistant("m0", _text("an older answer")),
        _user("fetch the docs"),
        _assistant("m1", _text("Looking."), {"type": "tool_use", "id": "t1", "name": "WebFetch", "input": {}}),
        _tool_result(),
        _assistant("m2", _text("The docs say X.")),
        _assistant("m2", _text("And Y.")),
        _assistant("s1", _text("a subagent's words"), sidechain=True),
    ])
    news = _news(dict(CLAUDE, hook_event_name="Stop", transcript_path=path))
    assert (news["state"], news["last"]) == ("done", "The docs say X.\n\nAnd Y.")


def test_claude_stop_before_its_answer_is_written_says_nothing(tmp_path):
    "Past the prompt lies the previous turn's answer: the wrong one."
    path = _jsonl(tmp_path / "c.jsonl", [
        _user("q1"), _assistant("m0", _text("answer to q1")), _user("q2")])
    news = _news(dict(CLAUDE, hook_event_name="Stop", transcript_path=path))
    assert news["state"] == "done" and "last" not in news


def test_claude_stop_takes_the_payloads_answer_first():
    news = _news(dict(CLAUDE, hook_event_name="Stop", last_assistant_message="from the payload"))
    assert news["last"] == "from the payload"


# ---- grok --------------------------------------------------------------

GROK = {"sessionId": "g-123", "cwd": "/src/a", "workspaceRoot": "/src/a"}


def test_grok_names_its_conversation_not_its_updates(tmp_path):
    """grok's hooks name updates.jsonl; the conversation the herd reads is
    chat_history.jsonl beside it."""
    updates = tmp_path / "updates.jsonl"
    updates.write_text("{}\n")
    p = dict(GROK, hook_event_name="UserPromptSubmit", transcriptPath=str(updates))
    assert _news(p)["transcript"] == str(updates)  # nothing beside it: as named
    (tmp_path / "chat_history.jsonl").write_text("{}\n")
    assert _news(p)["transcript"] == str(tmp_path / "chat_history.jsonl")
    # Another CLI's file is its own.
    assert _news(dict(CLAUDE, hook_event_name="UserPromptSubmit", transcript_path=str(updates)))["transcript"] == str(updates)


def test_grok_events():
    assert _news(dict(GROK, hook_event_name="UserPromptSubmit", hookEventName="user_prompt_submit")) == {
        "cli": "grok", "conversation": "g-123", "state": "working"}
    news = _news(dict(GROK, hook_event_name="Stop", reason="end_turn", lastAssistantMessage="grok's answer"))
    assert (news["state"], news["last"]) == ("done", "grok's answer")
    # The extra Stop at session end is not a turn.
    assert "state" not in _news(dict(GROK, hook_event_name="Stop", reason="shutdown"))
    assert _news(dict(GROK, hook_event_name="StopCancelled", reason="user_interrupt"))["state"] == "auto"
    assert _news(dict(GROK, hook_event_name="StopFailure", error="rate_limit"))["reason"] == "stopped: rate_limit"
    assert _news(dict(GROK, hook_event_name="Notification", notificationType="permission_prompt",
                      message="Approve run_terminal_command?"))["state"] == "blocked"


# ---- agy ---------------------------------------------------------------


def _agy(path):
    return {"conversationId": "a-456", "workspacePaths": ["/src/a"], "transcriptPath": path}


def _step(source, kind, content=None, tools=False):
    rec = {"step_index": 0, "source": source, "type": kind, "status": "DONE"}
    if content is not None:
        rec["content"] = content
    if tools:
        rec["tool_calls"] = [{"name": "run_command", "args": {}}]
    return rec


def test_agy_events(tmp_path):
    path = _jsonl(tmp_path / "transcript.jsonl", [
        _step("USER_EXPLICIT", "USER_INPUT", "q1"),
        _step("MODEL", "PLANNER_RESPONSE", "answer to q1"),
        _step("USER_EXPLICIT", "USER_INPUT", "commit it"),
        _step("MODEL", "PLANNER_RESPONSE", tools=True),
        _step("MODEL", "GENERIC", "tool output"),
        _step("MODEL", "PLANNER_RESPONSE", "Committed ee5fee6."),
    ])
    # agy's payload names no event: the hook's command line does.
    assert _news(_agy(path), "PreInvocation")["state"] == "working"
    news = _news(dict(_agy(path), terminationReason="model_stop"), "Stop")
    assert (news["cli"], news["conversation"], news["state"], news["last"]) == (
        "agy", "a-456", "done", "Committed ee5fee6.")
    assert _news(dict(_agy(path), terminationReason="error", error="quota"), "Stop")["reason"] == "stopped: quota"


def test_agy_stop_before_its_answer_is_written_says_nothing(tmp_path):
    path = _jsonl(tmp_path / "transcript.jsonl", [
        _step("MODEL", "PLANNER_RESPONSE", "old answer"),
        _step("USER_EXPLICIT", "USER_INPUT", "new question"),
        _step("MODEL", "PLANNER_RESPONSE", tools=True)])
    assert hc.agy_last(path) is None


def test_a_huge_transcript_is_read_from_its_tail(tmp_path):
    big = [_step("MODEL", "GENERIC", "x" * 4000) for _ in range(300)]
    path = _jsonl(tmp_path / "transcript.jsonl",
                  [_step("USER_EXPLICIT", "USER_INPUT", "q")] + big + [_step("MODEL", "PLANNER_RESPONSE", "end")])
    assert os.path.getsize(path) > hc.TAIL_BYTES
    assert hc.agy_last(path) == "end"


# ---- The sidecar's side --------------------------------------------------


def _store(tmp_path):
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=SNAP)
    return h


def test_reports_go_to_emacs_once_newest_first(tmp_path):
    h = _store(tmp_path)
    h.report("agy-a2", state="working")
    h.report("agy-a2", state="done", reason="finished its turn")
    h.report("claude-main", state="blocked", reason="Bash?")
    out = h.tick(sessions=SNAP)
    assert sorted((r["session"], r["state"]) for r in out["reports"]) == [
        ("agy-a2", "done"), ("claude-main", "blocked")]
    assert h.tick(sessions=SNAP)["reports"] == []
    with pytest.raises(ValueError, match="state must be"):
        h.report("agy-a2", state="sleeping")


def test_links_ride_each_tick_for_the_herd(tmp_path):
    h = _store(tmp_path)
    h.report("agy-a2", cli="agy", conversation="a-1", transcript="/t.jsonl")
    h.report("agy-a2", state="done", last="All done.\nDetails.")
    h.report("someone-gone", cli="claude", conversation="x")
    links = h.tick(sessions=SNAP)["links"]
    assert [(l["session"], l["cli"], l["conversation"], l["last_head"]) for l in links] == [
        ("agy-a2", "agy", "a-1", "All done.")]
    assert links[0]["last_ms"] > 0
    # A new conversation drops the old one's transcript and answer.
    h.report("agy-a2", conversation="a-2")
    link = h.link("agy-a2")
    assert (link["conversation"], link["transcript"], link["last"]) == ("a-2", "", "")


def test_done_with_an_answer_closes_the_takers_ask(tmp_path):
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "commit it", from_name="claude-main")
    _deliver(h)
    h.report("agy-a2", state="working")  # woke to the ask: a turn after delivery
    out = h.report("agy-a2", state="done", last="Committed ee5fee6.")
    assert out["closed_ask"] == ask["id"]
    got = h.get_ask(ask["id"])
    assert (got["status"], got["reply"], got["auto"], got["auto_source"]) == (
        "answered", "Committed ee5fee6.", True, "message")
    mail = [p for p in h.tick(sessions=SNAP)["pending"] if p["to"] == "claude-main"]
    assert "this is its last message:\nCommitted ee5fee6." in mail[0]["body"]


def test_settle_prefers_an_answer_given_after_the_ask(tmp_path):
    h = _store(tmp_path)
    h.report("agy-a2", state="done", last="an answer from before the ask")
    ask = h.ask("agy-a2", "x", from_name="claude-main", wait=True)
    _deliver(h)
    h.report("agy-a2", state="working")  # woke to the ask: a turn after delivery
    h.settle(ask["id"], screen="the screen")
    got = h.get_ask(ask["id"])
    assert (got["reply"], got["auto_source"]) == ("the screen", "screen")
    two = h.ask("agy-a2", "y", from_name="claude-main", wait=True)
    _deliver(h)
    h.report("agy-a2", state="working")  # woke to it, too
    h.report("agy-a2", last="the answer to y")  # no state: no close here
    assert h.get_ask(two["id"])["status"] == "delivered"
    h.settle(two["id"], screen="the screen")
    got = h.get_ask(two["id"])
    assert (got["reply"], got["auto_source"]) == ("the answer to y", "message")


def test_a_turn_already_running_when_the_ask_arrived_does_not_answer_it(tmp_path):
    """An ask delivered mid-turn is not closed by that turn's recap: only a
    turn begun after the ask may answer it with its own last words."""
    h = _store(tmp_path)
    h.report("agy-a2", state="working")  # a turn is already running
    time.sleep(0.002)
    ask = h.ask("agy-a2", "item 3", from_name="claude-main")
    _deliver(h)
    # The running turn ends with the recap of its own, earlier work.
    out = h.report("agy-a2", state="done", last="done with item 2")
    assert out["closed_ask"] is None
    assert h.get_ask(ask["id"])["status"] == "delivered"
    # The next turn, begun after the ask, answers it.
    time.sleep(0.002)
    h.report("agy-a2", state="working")
    out = h.report("agy-a2", state="done", last="done with item 3")
    assert out["closed_ask"] == ask["id"]
    got = h.get_ask(ask["id"])
    assert (got["reply"], got["auto_source"]) == ("done with item 3", "message")


def test_settle_does_not_answer_with_a_turn_that_predated_the_ask(tmp_path):
    """settle, too, refuses a recap: a turn already running when the ask
    arrived ends after it but began before it, so the screen answers."""
    h = _store(tmp_path)
    h.report("agy-a2", state="working")
    time.sleep(0.002)
    ask = h.ask("agy-a2", "x", from_name="claude-main", wait=True)
    _deliver(h)
    h.report("agy-a2", state="done", last="recap of the earlier turn")
    assert h.get_ask(ask["id"])["status"] == "delivered"  # report did not close it
    h.report("agy-a2", state="working")  # a turn after delivery: now it has read the ask
    h.settle(ask["id"], screen="the screen")
    got = h.get_ask(ask["id"])
    assert (got["reply"], got["auto_source"]) == ("the screen", "screen")


def test_grace_does_not_close_an_ask_delivered_as_a_turn_ended(tmp_path, monkeypatch):
    """The bug that mis-closed ask f2bd1c46: delivered as a turn was ending,
    then the deliver-grace closed it with that turn's recap.  The grace is
    only for an unknown turn-start (hookless/restart); when turn_at IS known
    and sits before delivery, the taker is on the old turn -- wait, do not
    let the grace close it.  A turn begun after delivery answers it."""
    monkeypatch.setattr(herd_mod, "DELIVER_GRACE", 0)  # the grace has "passed"
    h = _store(tmp_path)
    h.report("agy-a2", state="working")          # the old turn's last `working'
    time.sleep(0.002)
    ask = h.ask("agy-a2", "review it", from_name="claude-main")
    _deliver(h)                                  # delivered after that working
    # No turn has begun after delivery.  Even with the grace elapsed, the ask
    # is not closed: turn_at is known and before delivery.
    r = h.settle(ask["id"], screen="the old turn's recap")
    assert r.get("too_soon") and h.get_ask(ask["id"])["status"] == "delivered"
    # The next turn, begun after delivery, answers it with its own words.
    time.sleep(0.002)
    h.report("agy-a2", state="working")
    out = h.report("agy-a2", state="done", last="the real review")
    assert out["closed_ask"] == ask["id"]
    assert h.get_ask(ask["id"])["reply"] == "the real review"


def test_the_next_ask_waits_until_the_answered_turn_ends(tmp_path):
    """Answering an ask with `herd reply' does not free the agent for the next
    queued ask until that turn has ended by the hooks -- so the next does not
    land in the turn-ending moment and get answered by this turn's recap."""
    h = _store(tmp_path)
    first = h.ask("agy-a2", "one", from_name="claude-main")
    second = h.ask("agy-a2", "two", from_name="user")
    assert [p["ask"] for p in _deliver(h)] == [first["id"]]
    h.reply(first["id"], "done one")             # its own reply, a tool call mid-turn
    # Held while the turn ends...
    assert [p["ask"] for p in h.tick(sessions=SNAP)["pending"] if p["to"] == "agy-a2"] == []
    h.report("agy-a2", state="done")             # ...and delivered once it has.
    assert [p["ask"] for p in h.tick(sessions=SNAP)["pending"] if p["to"] == "agy-a2"] == [second["id"]]


def test_a_real_reply_reaches_an_asker_that_already_read_the_auto_answer(tmp_path):
    """A taker's real answer supersedes an auto one the asker already read,
    and reaches the asker as a message its own collect will not cancel."""
    h = _store(tmp_path)
    ask = h.ask("agy-a2", "commit it", from_name="claude-main", wait=True)
    _deliver(h)
    h.report("agy-a2", state="working")  # woke to the ask: a turn after delivery
    h.settle(ask["id"], screen="half-done screen")  # the herd auto-closes it
    h.collect(ask["id"], asker=True)                # the waiting asker reads it, moves on
    assert h._ask_row(ask["id"])["collected"] == 1
    h.reply(ask["id"], "Committed abc1234.", from_name="agy-a2")  # the real answer
    got = h.get_ask(ask["id"])
    assert (got["status"], got["auto"], got["reply"]) == ("answered", False, "Committed abc1234.")
    # A stale collect by the asker must not cancel the correction.
    h.collect(ask["id"], asker=True)
    mail = [p for p in h.tick(sessions=SNAP)["pending"] if p["to"] == "claude-main"]
    assert any("Committed abc1234." in m["body"] for m in mail), mail
    assert any(m["body"].startswith("A later answer to ask") for m in mail)


# ---- `herd hook', as a CLI runs it ----------------------------------------


def hook(live, payload, *event, session="claude-main", env=None, tmp=None):
    e = dict(live["env"], GHOSTHERD_SESSION=session, TMPDIR=str(tmp))
    if session is None:
        del e["GHOSTHERD_SESSION"]
    e.update(env or {})
    return subprocess.run([str(HERD), "hook", *event], input=json.dumps(payload),
                          capture_output=True, text=True, env=e, timeout=20)


def test_hook_reports_and_stays_quiet(live, tmp_path):
    h = herd_mod.get_herd()
    out = hook(live, dict(CLAUDE, hook_event_name="UserPromptSubmit"), tmp=tmp_path)
    assert (out.returncode, out.stdout) == (0, ""), out.stderr
    tick = h.tick(sessions=SNAP)
    assert tick["reports"] == [{"session": "claude-main", "state": "working", "reason": ""}]
    assert tick["links"][0]["conversation"] == CLAUDE["session_id"]
    # The same `working' moments later is not sent again...
    hook(live, dict(CLAUDE, hook_event_name="UserPromptSubmit"), tmp=tmp_path)
    assert h.tick(sessions=SNAP)["reports"] == []
    # ...anything else is.
    hook(live, dict(CLAUDE, hook_event_name="Stop", last_assistant_message="ok"), tmp=tmp_path)
    assert h.tick(sessions=SNAP)["reports"][0]["state"] == "done"


def test_hook_closes_an_ask_from_agys_transcript(live, tmp_path):
    h = herd_mod.get_herd()
    ask = h.ask("agy-a2", "commit it", from_name="claude-main", wait=True)
    _deliver(h)
    h.report("agy-a2", state="working")  # woke to the ask: a turn after delivery
    path = _jsonl(tmp_path / "transcript.jsonl", [
        _step("USER_EXPLICIT", "USER_INPUT", "commit it"),
        _step("MODEL", "PLANNER_RESPONSE", "Committed abc1234.")])
    out = hook(live, dict(_agy(path), terminationReason="model_stop"), "Stop", session="agy-a2", tmp=tmp_path)
    assert (out.returncode, out.stdout.strip()) == (0, "{}"), out.stderr
    got = h.get_ask(ask["id"])
    assert (got["status"], got["reply"], got["auto_source"]) == ("answered", "Committed abc1234.", "message")


def test_hook_does_not_let_a_running_turn_answer_an_ask(live, tmp_path):
    """Through the real `herd hook' binary (what a CLI runs), the incident
    7cca60ba (2026-10-17): an ask delivered while a turn is already running
    is not closed by that turn's recap -- only a turn begun after the ask
    answers it."""
    h = herd_mod.get_herd()
    # A turn is already running when the ask arrives.
    hook(live, dict(CLAUDE, hook_event_name="UserPromptSubmit"), tmp=tmp_path)
    ask = h.ask("claude-main", "item 3", from_name="lead")
    _deliver(h)
    # The running turn ends with the recap of its own, earlier work.
    hook(live, dict(CLAUDE, hook_event_name="Stop",
                    last_assistant_message="done with item 2"), tmp=tmp_path)
    assert h.get_ask(ask["id"])["status"] == "delivered"
    # The next turn, begun after the ask, answers it.
    hook(live, dict(CLAUDE, hook_event_name="UserPromptSubmit"), tmp=tmp_path)
    hook(live, dict(CLAUDE, hook_event_name="Stop",
                    last_assistant_message="done with item 3"), tmp=tmp_path)
    got = h.get_ask(ask["id"])
    assert (got["status"], got["reply"], got["auto_source"]) == (
        "answered", "done with item 3", "message")


def test_hook_outside_the_herd_does_nothing(live, tmp_path):
    h = herd_mod.get_herd()
    out = hook(live, {"conversationId": "x"}, "PreInvocation", session=None, tmp=tmp_path)
    assert (out.returncode, out.stdout.strip(), out.stderr) == (0, "{}", "")
    out = hook(live, dict(CLAUDE, hook_event_name="Stop"), session=None, tmp=tmp_path)
    assert (out.returncode, out.stdout) == (0, "")
    assert h.tick(sessions=SNAP)["reports"] == []


def test_hook_fails_open(live, tmp_path):
    out = hook(live, {"conversationId": "x"}, "Stop", session="agy-a2", tmp=tmp_path,
               env={"GHOSTHERD_RPC": "http://127.0.0.1:9/jsonrpc/x"})
    assert (out.returncode, out.stdout.strip()) == (0, "{}")
    assert "hook:" in out.stderr
    out = subprocess.run([str(HERD), "hook"], input="not json", capture_output=True, text=True,
                         env=dict(live["env"], TMPDIR=str(tmp_path)), timeout=20)
    assert (out.returncode, out.stdout) == (0, "")


# ---- A CLI the agent runs is not the agent (claude-jnotes, 2026-10-22) -------
#
# claude-jnotes ran `claude -p' and `agy -p' from a scratch dir to probe how
# each CLI writes files.  They inherited its environment, its name in the herd
# with it, and their hooks reported as claude-jnotes: their turns' `working'
# and `done', and agy's conversation as its own.  Its card lost its
# conversation (the link an agy one, which a claude card ignores: a screen
# dump, no model) and read `done' while it worked.

PROBE = {"session_id": "probe-c1", "transcript_path": "/scratch/probe.jsonl", "cwd": "/scratch/ws"}


def _own(h, **kw):
    "A report from claude-main's own CLI, process 100 under tmux (10)."
    return h.report("claude-main", pid=100, ancestors=[10], **kw)


def test_a_cli_of_another_kind_is_not_the_agent(tmp_path):
    h = _store(tmp_path)
    _own(h, state="working", cli="claude", conversation="own-1", transcript="/own.jsonl")
    h.tick(sessions=SNAP)
    ask = h.ask("claude-main", "check it", from_name="agy-a2")
    _deliver(h)
    # An agy the agent runs: no process named, its kind alone gives it away.
    for state, last in (("working", None), ("done", "Created out.txt.")):
        out = h.report("claude-main", state=state, cli="agy", conversation="agy-9",
                       transcript="/brain/agy-9/transcript.jsonl", last=last)
        assert "ignored" in out and "agy" in out["ignored"]
    link = h.link("claude-main")
    assert (link["cli"], link["conversation"], link["transcript"]) == ("claude", "own-1", "/own.jsonl")
    assert h.tick(sessions=SNAP)["reports"] == []           # no `done' while it works
    assert h.get_ask(ask["id"])["status"] == "delivered"     # not answered with agy's words


def test_a_cli_started_under_the_agents_own_is_not_the_agent(tmp_path):
    h = _store(tmp_path)
    _own(h, state="working", cli="claude", conversation="own-1", transcript="/own.jsonl")
    # `claude -p' run from the agent's Bash tool: the same kind, its own
    # conversation, a process under the agent's (100).
    out = h.report("claude-main", state="done", cli="claude", conversation="probe-c1",
                   transcript="/scratch/probe.jsonl", last="Found it.",
                   pid=200, ancestors=[150, 100, 10])
    assert "started by claude-main's own CLI (100)" in out["ignored"]
    assert h.link("claude-main")["conversation"] == "own-1"
    # The agent's own CLI on a new conversation -- a /clear -- is followed.
    _own(h, cli="claude", conversation="own-2", transcript="/own-2.jsonl")
    assert h.link("claude-main")["conversation"] == "own-2"
    # A new process not under the old one -- a restart -- is followed, and
    # pinned: what it starts is told from it in turn.
    h.report("claude-main", state="working", cli="claude", conversation="own-3",
             transcript="/own-3.jsonl", pid=300, ancestors=[10])
    assert h.link("claude-main")["conversation"] == "own-3"
    assert "ignored" in h.report("claude-main", state="done", cli="claude", conversation="p2",
                                 pid=400, ancestors=[390, 300, 10])
    assert h.link("claude-main")["conversation"] == "own-3"


def test_a_shell_takes_the_cli_run_in_it(tmp_path):
    h = _store(tmp_path)
    snap = SNAP + [{"name": "sh-1", "kind": "shell", "state": "idle", "project": "/src/a/"}]
    h.tick(sessions=snap)
    h.report("sh-1", state="working", cli="claude", conversation="c-1", pid=500, ancestors=[490, 10])
    assert h.link("sh-1")["cli"] == "claude"
    assert h.tick(sessions=snap)["reports"][0]["state"] == "working"


def test_the_jnotes_morning_replayed(tmp_path):
    """The herd log of 2026-10-22, 08:31-08:36: the agent's turn, and two
    probes of each CLI inside it.  The agent stays working on its own
    conversation; its turn's start and end are its own."""
    h = _store(tmp_path)
    _own(h, state="working", cli="claude", conversation="b5667d46", transcript="/jnotes/b5667d46.jsonl")
    h.tick(sessions=SNAP)
    turn_at = h._turn_at("claude-main")
    probes = [("claude", "cb74ba04", 201), ("agy", "0af783be", 202),
              ("claude", "10860dbe", 203), ("agy", "992aa41f", 204)]
    for cli, conv, pid in probes:
        for state, last in (("working", None), ("done", "probe answer")):
            out = h.report("claude-main", state=state, cli=cli, conversation=conv,
                           transcript=f"/probe/{conv}.jsonl", last=last,
                           pid=pid, ancestors=[pid - 50, 100, 10])
            assert "ignored" in out
    assert h.tick(sessions=SNAP)["reports"] == []
    link = h.link("claude-main")
    assert (link["cli"], link["conversation"]) == ("claude", "b5667d46")
    assert h._turn_at("claude-main") == turn_at
    with h._mu:
        assert h._conn.execute("SELECT turn_end_at FROM links WHERE session = 'claude-main'").fetchone()[0] is None
    # Its own turn ends: that is taken.
    _own(h, state="done", cli="claude", conversation="b5667d46", last="Committed.")
    assert h.tick(sessions=SNAP)["reports"][0]["state"] == "done"


def test_the_rpc_checks_the_process_ids(tmp_path):
    from jsonrpc_handler import JsonRpcHandler
    rpc = JsonRpcHandler()
    for bad in ({"pid": "100"}, {"pid": True}, {"ancestors": "10"}, {"ancestors": [10, "x"]}):
        with pytest.raises(ValueError):
            rpc._herd_report(dict(session="claude-main", state="working", **bad))


# The process table `ps -A -o pid=,ppid=,comm=' gives: a probe's hook, run
# through sh by a claude under the agent's tool shell, under the agent's
# claude, under tmux.
PS = """\
    1     0 /sbin/launchd
   10     1 /opt/homebrew/bin/tmux
  100    10 claude
  150   100 /bin/bash
  200   150 claude
  210   200 /bin/sh
  220   210 -zsh
"""


def _ps(monkeypatch, table, me_parent):
    monkeypatch.setattr(hc.subprocess, "run",
                        lambda *a, **k: subprocess.CompletedProcess(a, 0, stdout=table, stderr=""))
    monkeypatch.setattr(hc.os, "getppid", lambda: me_parent)


def test_lineage_finds_the_cli_past_its_shells(monkeypatch):
    _ps(monkeypatch, PS, 220)                 # the hook's parent: a login zsh
    assert hc._lineage() == (200, [150, 100, 10])
    _ps(monkeypatch, PS, 100)                 # run by the CLI itself, no shell
    assert hc._lineage() == (100, [10])


def test_lineage_says_nothing_when_ps_cannot(monkeypatch):
    def fail(*a, **k):
        raise OSError("no ps")
    monkeypatch.setattr(hc.subprocess, "run", fail)
    assert hc._lineage() == (None, [])
    _ps(monkeypatch, "garbage\n", 5)
    assert hc._lineage() == (None, [])


FAKE_CLI = """\
import json, os, subprocess, sys
# A CLI, as far as the herd can see: it runs a hook's command through sh --
# the command `herd hooks install' writes, so sh stays between the CLI and
# the hook -- and the CLIs it starts (its probes) run theirs.  argv: steps.
for step in json.loads(sys.argv[1]):
    if step[0] == "hook":
        subprocess.run(["sh", "-c", step[2]], input=json.dumps(step[1]),
                       text=True, env=os.environ, check=True, stdout=subprocess.DEVNULL)
    else:
        subprocess.run([sys.executable, __file__, json.dumps(step[1])], env=os.environ, check=True)
"""


def test_a_probe_through_the_real_hook_reports_nothing(live, tmp_path):
    """End to end, with real processes: the agent's own CLI reports through
    `herd hook'; a claude and an agy it starts report too, under its name;
    the herd follows the agent alone."""
    h = herd_mod.get_herd()
    cli = tmp_path / "fakecli.py"
    cli.write_text(FAKE_CLI)
    own = dict(CLAUDE, hook_event_name="UserPromptSubmit")
    run = hc._claude_command()
    claude_probe = [["hook", dict(PROBE, hook_event_name="UserPromptSubmit"), run],
                    ["hook", dict(PROBE, hook_event_name="Stop", last_assistant_message="probe"), run]]
    agy_probe = [["hook", {"conversationId": "agy-9", "transcriptPath": "/brain/agy-9.jsonl"},
                  hc._agy_command("PreInvocation")],
                 ["hook", {"conversationId": "agy-9", "transcriptPath": "/brain/agy-9.jsonl",
                           "terminationReason": "model_stop"}, hc._agy_command("Stop")]]
    steps = [["hook", own, run], ["cli", claude_probe], ["cli", agy_probe],
             ["hook", dict(CLAUDE, hook_event_name="SessionStart", session_id="own-2"), run]]
    env = dict(live["env"], TMPDIR=str(tmp_path), GHOSTHERD_HERD=str(HERD))
    subprocess.run([sys.executable, str(cli), json.dumps(steps)], env=env, check=True, timeout=60)
    tick = h.tick(sessions=SNAP)
    assert [(r["session"], r["state"]) for r in tick["reports"]] == [("claude-main", "working")]
    link = h.link("claude-main")
    # Its own /clear (a new conversation from the same CLI) is followed;
    # the probes' conversations never were.
    assert (link["cli"], link["conversation"]) == ("claude", "own-2")


# ---- Installing ----------------------------------------------------------


def hooks(home, *args):
    env = {"PATH": os.environ["PATH"], "HOME": str(home)}
    return subprocess.run([str(HERD), "hooks", *args], capture_output=True, text=True, env=env, timeout=20)


@pytest.fixture
def home(tmp_path):
    (tmp_path / ".claude").mkdir()
    (tmp_path / ".gemini" / "config").mkdir(parents=True)
    (tmp_path / ".claude" / "settings.json").write_text(json.dumps({
        "env": {"SOME_KEY": "secret-value"},
        "hooks": {"Stop": [{"hooks": [{"type": "command", "command": "say done"}]}],
                  "PreToolUse": [{"matcher": "Bash", "hooks": [{"type": "command", "command": "rtk hook"}]}]},
    }))
    return tmp_path


def test_install_adds_ours_and_keeps_theirs(home):
    out = hooks(home, "install")
    assert out.returncode == 0, out.stderr
    settings = json.loads((home / ".claude" / "settings.json").read_text())
    assert settings["env"] == {"SOME_KEY": "secret-value"}
    assert settings["hooks"]["PreToolUse"][0]["hooks"][0]["command"] == "rtk hook"
    assert settings["hooks"]["Stop"][0]["hooks"][0]["command"] == "say done"
    for ev in ("SessionStart", "UserPromptSubmit", "Notification", "Stop"):
        assert any(hc.MARK in h["command"] for g in settings["hooks"][ev] for h in g["hooks"]), ev
    agy = json.loads((home / ".gemini" / "config" / "hooks.json").read_text())
    assert set(agy["ghostherd"]) == {"PreInvocation", "Stop"}
    assert list(home.glob(".claude/settings.json.ghostherd-*.bak"))
    # Twice is once.
    assert "nothing to do" in hooks(home, "install").stdout
    assert "PreInvocation, Stop" in hooks(home, "status").stdout


def test_uninstall_takes_out_only_ours(home):
    hooks(home, "install")
    out = hooks(home, "uninstall")
    assert out.returncode == 0, out.stderr
    settings = json.loads((home / ".claude" / "settings.json").read_text())
    assert settings["hooks"] == {
        "Stop": [{"hooks": [{"type": "command", "command": "say done"}]}],
        "PreToolUse": [{"matcher": "Bash", "hooks": [{"type": "command", "command": "rtk hook"}]}]}
    assert json.loads((home / ".gemini" / "config" / "hooks.json").read_text()) == {}


def test_dry_run_writes_nothing_and_shows_only_ours(home):
    before = (home / ".claude" / "settings.json").read_text()
    out = hooks(home, "install", "--dry-run")
    assert out.returncode == 0, out.stderr
    assert (home / ".claude" / "settings.json").read_text() == before
    assert not (home / ".gemini" / "config" / "hooks.json").exists()
    assert "secret-value" not in out.stdout and "rtk hook" not in out.stdout
    assert hc.MARK.replace('"', '\\"') in out.stdout


def test_install_refuses_a_broken_settings_file(home):
    (home / ".claude" / "settings.json").write_text("{not json")
    out = hooks(home, "install")
    assert out.returncode == 1 and "not valid JSON" in out.stderr
    assert (home / ".claude" / "settings.json").read_text() == "{not json"


# ---- Listing every hook --------------------------------------------------


def _write(path: Path, text: str) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)
    return path


@pytest.fixture
def world(tmp_path):
    "A home and a project with hooks in every place the three CLIs look."
    home, proj = tmp_path / "home", tmp_path / "proj"
    _write(home / ".claude" / "settings.json", json.dumps({
        "env": {"SOME_KEY": "secret-value"},
        "enabledPlugins": {"fin@market": True, "off@market": False},
        "hooks": {
            "PreToolUse": [{"matcher": "Bash", "hooks": [
                {"type": "command", "command": 'node "/x/gitnexus-hook.cjs"'}]}],
            "SessionStart": [{"matcher": "*", "hooks": [{"type": "command", "command": "herdr session"}]}],
        }}, indent=2))
    _write(home / ".claude" / "plugins" / "cache" / "market" / "fin" / "0.1" / "hooks" / "hooks.json",
           json.dumps({"hooks": {"Stop": [{"hooks": [{"type": "command", "command": "fin-stop"}]}]}}))
    _write(home / ".claude" / "plugins" / "cache" / "market" / "off" / "0.1" / "hooks" / "hooks.json",
           json.dumps({"hooks": {"Stop": [{"hooks": [{"type": "command", "command": "never"}]}]}}))
    _write(home / ".grok" / "hooks" / "mine.json",
           json.dumps({"hooks": {"Stop": [{"hooks": [{"type": "command", "command": "grok-stop"}]}]}}))
    _write(home / ".grok" / "hooks-disabled" / "eye.json",
           json.dumps({"hooks": {"Stop": [{"hooks": [{"type": "command", "command": "eye"}]}]}}))
    _write(home / ".cursor" / "hooks.json",
           json.dumps({"version": 1, "hooks": {"afterFileEdit": [{"command": "fmt"}]}}))
    _write(home / ".gemini" / "config" / "hooks.json", json.dumps({
        "lint": {"PostToolUse": [{"matcher": "run_command", "hooks": [{"command": "./lint.sh"}]}]},
        "nag": {"enabled": False, "Stop": [{"type": "command", "command": "./nag.sh"}]},
    }, indent=2))
    _write(proj / ".claude" / "settings.local.json",
           json.dumps({"hooks": {"Stop": [{"hooks": [{"type": "command", "command": "proj-stop"}]}]}}))
    _write(proj / ".agents" / "hooks.json",
           json.dumps({"p": {"PreInvocation": [{"type": "command", "command": "proj-agy"}]}}))
    return home, proj


def listing(home, *args):
    env = {"PATH": os.environ["PATH"], "HOME": str(home)}
    out = subprocess.run([str(HERD), "hooks", "list", "--json", *args],
                         capture_output=True, text=True, env=env, timeout=20)
    assert out.returncode == 0, out.stderr
    return json.loads(out.stdout)


def _by_command(inv):
    return {h["command"]: h for h in inv["hooks"]}


def test_list_finds_every_place(world):
    home, proj = world
    inv = listing(home, "--project", str(proj))
    got = {c: (h["readers"], h["event"], h["enabled"], h["scope"]) for c, h in _by_command(inv).items()}
    assert got == {
        'node "/x/gitnexus-hook.cjs"': (["claude", "grok"], "PreToolUse", True, "global"),
        "herdr session": (["claude", "grok"], "SessionStart", True, "global"),
        "fin-stop": (["claude"], "Stop", True, "global"),
        "grok-stop": (["grok"], "Stop", True, "global"),
        "eye": (["grok"], "Stop", False, "global"),
        "fmt": (["grok"], "afterFileEdit", True, "global"),
        "./lint.sh": (["agy"], "PostToolUse", True, "global"),
        "./nag.sh": (["agy"], "Stop", False, "global"),
        "proj-stop": (["claude", "grok"], "Stop", True, str(proj)),
        "proj-agy": (["agy"], "PreInvocation", True, str(proj)),
    }
    # The disabled plugin, and the env's secret, are nowhere.
    assert "never" not in got
    assert "secret-value" not in json.dumps(inv)
    assert inv["ghostherd"]["claude"] == {"have": [], "want": list(hc.CLAUDE_EVENTS)}


def test_list_points_at_the_line(world):
    home, proj = world
    hooks = _by_command(listing(home))
    path = home / ".claude" / "settings.json"
    lines = path.read_text().splitlines()
    for cmd in ('node "/x/gitnexus-hook.cjs"', "herdr session"):
        h = hooks[cmd]
        assert h["file"] == str(path)
        assert json.dumps(cmd)[1:-1] in lines[h["line"] - 1], (cmd, h["line"])
    agy = hooks["./nag.sh"]
    assert "./nag.sh" in Path(agy["file"]).read_text().splitlines()[agy["line"] - 1]
    assert agy["source"] == "agy hooks.json · nag"


def test_list_follows_groks_compat_switch(world):
    home, _ = world
    _write(home / ".grok" / "config.toml", "[compat.claude]\nhooks = false\n[compat.cursor]\nhooks = false\n")
    hooks = _by_command(listing(home))
    assert hooks["herdr session"]["readers"] == ["claude"]
    assert "fmt" not in hooks


def test_list_counts_ghostherds_once_installed(world):
    home, _ = world
    hooks(home, "install")
    inv = listing(home)
    assert inv["ghostherd"]["claude"]["have"] == list(hc.CLAUDE_EVENTS)
    assert inv["ghostherd"]["grok"]["have"] == list(hc.CLAUDE_EVENTS)
    assert inv["ghostherd"]["agy"]["have"] == list(hc.AGY_HOOKS)
    assert sum(h["ours"] for h in inv["hooks"]) == 6


def test_list_survives_a_broken_file(world):
    home, _ = world
    (home / ".claude" / "settings.json").write_text("{nope")
    bad = [h for h in listing(home)["hooks"] if h.get("error")]
    assert bad and bad[0]["file"].endswith("settings.json") and bad[0]["error"] == "not valid JSON"
