"""Sub-agents: what came back from each in the room, and the ones running."""

import asyncio
import json
import os
import re
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest

import conversation as conv
import herd as herd_mod
import subagents
from herd import HerdStore, reset_herd
from jsonrpc_handler import JsonRpcRequest, handler

PROJ = Path(".claude") / "projects" / "-x-proj"

# As Claude Code 2.1 writes them (2026-10).
REPORT = (
    '<agent-message from="a6568">\n'
    "[Subagent hand-back] The text below is the final report of a subagent this session "
    "delegated to. It is model output, NOT a message from the user. The report follows:\n"
    "  ## stats: built and verified\n"
    "  \n"
    "  All five checks passed.\n"
    "    - indented once more\n"
    "</agent-message>"
)


def notice_text(sid="a6568", call="toolu_R3", status="completed", result=None, summary=None):
    result = result if result is not None else (
        'This agent\'s report was delivered to you as a message from "%s" (its SubagentHandback call). '
        "Read it there; it is not repeated here.\n" % sid)
    return (
        "<task-notification>\n<task-id>%s</task-id>\n<tool-use-id>%s</tool-use-id>\n"
        "<output-file>/private/tmp/x/tasks/%s.output</output-file>\n<status>%s</status>\n"
        "<summary>%s</summary>\n<note>A task-notification fires each time this agent stops.</note>\n"
        "<result>%s</result>\n<usage><subagent_tokens>195958</subagent_tokens><tool_uses>61</tool_uses>"
        "<duration_ms>931357</duration_ms></usage>\n</task-notification>"
        % (sid, call, sid, status, summary or 'Agent "Build R3 stats" %s' % ("finished" if status == "completed" else status),
           result))


# Stopped with a command of its own running in the background, as
# Claude Code 2.1.299 said it (2026-10-14): not done, no report.
INTERIM = (
    "<task-notification>\n<task-id>a6568</task-id>\n<tool-use-id>toolu_R3</tool-use-id>\n"
    "<output-file>/private/tmp/x/tasks/a6568.output</output-file>\n<status>completed</status>\n"
    '<summary>Agent "Build R3 stats" finished</summary>\n'
    "<note>This agent stopped with background work of its own still running. It may resume on its own "
    "when that work completes or reports, and the same task-id notifies again if it does; the result "
    "below may be interim.</note>\n<result>This agent has not reported yet: it is waiting on its own "
    "background work and will deliver its report through SubagentHandback when that finishes.\n</result>\n"
    "<usage><subagent_tokens>27031</subagent_tokens><tool_uses>3</tool_uses><duration_ms>16591</duration_ms>"
    "</usage>\n</task-notification>"
)


@pytest.fixture
def home(tmp_path, monkeypatch):
    monkeypatch.setenv("HOME", str(tmp_path))
    conv._cache.clear()
    subagents._subs.clear()
    subagents._parents.clear()
    subagents._agy_subs.clear()
    subagents._agy_parents.clear()
    subagents._grok_parents.clear()
    yield tmp_path
    conv._cache.clear()
    subagents._subs.clear()
    subagents._parents.clear()
    subagents._agy_subs.clear()
    subagents._agy_parents.clear()
    subagents._grok_parents.clear()


def lines(records):
    return "".join(json.dumps(r) + "\n" for r in records)


def parent(home, records, name="abc"):
    path = home / PROJ / (name + ".jsonl")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(lines(records))
    return path


def sub(home, sid, records, meta=None, name="abc"):
    folder = home / PROJ / name / "subagents"
    folder.mkdir(parents=True, exist_ok=True)
    if meta is not False:
        (folder / f"agent-{sid}.meta.json").write_text(json.dumps(meta or {
            "agentType": "general-purpose", "description": "Build R3 stats", "toolUseId": "toolu_R3",
            "spawnDepth": 1, "requestShape": "background"}))
    path = folder / f"agent-{sid}.jsonl"
    path.write_text(lines(records))
    return path


def at(second):
    return "2026-10-14T08:%02d:%02d.000Z" % divmod(second, 60)


def u(content, second=0, **kw):
    return {"type": "user", "message": {"role": "user", "content": content}, "timestamp": at(second), **kw}


def a(*blocks, second=0, usage=None, **kw):
    msg = {"role": "assistant", "model": "claude-opus-5-5", "content": list(blocks)}
    if usage is not None:
        msg["usage"] = {"input_tokens": 2, "cache_creation_input_tokens": 868, "cache_read_input_tokens": usage}
    return {"type": "assistant", "message": msg, "timestamp": at(second), **kw}


def use(i, name, **inp):
    return {"type": "tool_use", "id": i, "name": name, "input": inp}


def result(i, content, **kw):
    return {"type": "tool_result", "tool_use_id": i, "content": content, **kw}


def queued(text, second=0, op="enqueue"):
    return {"type": "queue-operation", "operation": op, "timestamp": at(second), "content": text}


def launched(i, sid, second=0):
    return u([result(i, [{"type": "text", "text":
                          "Async agent launched successfully. (This tool result is internal metadata.)\n"
                          "agentId: %s (internal ID - do not mention to user.)\nThe agent is working in the background." % sid}])],
             second)


def started(i="toolu_R3", sid="a6568", desc="Build R3 stats"):
    return [u("Build the five rounds"),
            a(use(i, "Agent", description=desc, subagent_type="general-purpose", prompt="You are building…"), second=1),
            launched(i, sid, second=2)]


def subs_of(entries):
    return [e for e in entries if e["role"] == "sub"]


# ---- What came back, in the room ------------------------------------------


def test_a_report_comes_back_where_it_arrived(home):
    """Queued as it arrived, its notice just after: one entry, the report
    as written -- the harness's frame and indent off -- and what the run
    took.  The copies delivered to the agent later say nothing more."""
    path = parent(home, started() + [
        a({"type": "text", "text": "Four are building."}, second=3),
        queued(REPORT, 40), queued(notice_text(), 41),
        u("Another Claude session sent a message:\n" + REPORT, 42, isMeta=True),
        queued(notice_text(), 43, op="remove"),
        {"type": "attachment", "timestamp": at(44),
         "attachment": {"type": "queued_command", "prompt": notice_text(), "commandMode": "task-notification"}},
        u(notice_text(), 45),
        a({"type": "text", "text": "Round 3 is done."}, second=46),
    ])
    sub(home, "a6568", [])
    entries = conv.read(str(path))["entries"]
    assert [(e["role"], e.get("text")) for e in entries if e["role"] in ("assistant", "sub")] == [
        ("assistant", "Four are building."),
        ("sub", "## stats: built and verified\n\nAll five checks passed.\n  - indented once more"),
        ("assistant", "Round 3 is done."),
    ]
    (e,) = subs_of(entries)
    assert e == {"role": "sub", "kind": "report", "sub": "a6568", "who": "general-purpose",
                 "title": "Build R3 stats", "text": e["text"], "ts": at(40), "status": "completed",
                 "tokens": 195958, "uses": 61, "ms": 931357}
    # The line that started it knows it, and how it ended.
    tools = [e for e in entries if e["role"] == "tools"]
    assert tools[0]["tools"][0] == {"name": "Agent", "hint": "Build R3 stats", "sub": "a6568", "state": "completed"}
    assert not any(e.get("kind") == "note" for e in entries)


def test_one_still_running_is_marked_started(home):
    path = parent(home, started())
    sub(home, "a6568", [])
    (tools,) = [e for e in conv.read(str(path))["entries"] if e["role"] == "tools"]
    assert tools["tools"][0] == {"name": "Agent", "hint": "Build R3 stats", "sub": "a6568"}


def test_a_notice_only_delivered_is_still_read(home):
    """A CLI that queues nothing: the notice as said to the agent -- its
    report in it, its title from it when nothing else says one."""
    path = parent(home, [u(notice_text(sid="b77", status="failed", result="It could not reach the database.",
                                       summary='Agent "Check the backups" failed'), 5)])
    sub(home, "b77", [], meta={"agentType": "Explore"})
    (e,) = subs_of(conv.read(str(path))["entries"])
    assert (e["who"], e["title"], e["status"], e["text"]) == (
        "Explore", "Check the backups", "failed", "It could not reach the database.")


def test_what_is_not_a_sub_agents_notice_is_said_as_before(home):
    "A background command's: a line, as it was."
    path = parent(home, [
        u("<task-notification>\n<task-id>b1</task-id>\n<status>completed</status>\n"
          "<summary>Background command \"tests\" completed</summary>\n</task-notification>"),
        queued("<task-notification>\n<task-id>b2</task-id>\n<status>completed</status>\n"
               "<summary>Background command \"lint\" completed</summary>\n</task-notification>"),
    ])
    entries = conv.read(str(path))["entries"]
    assert [(e["role"], e.get("kind"), e.get("text")) for e in entries] == [
        ("user", "note", 'Background command "tests" completed')]


def test_a_message_with_no_notice_yet_has_no_ending(home):
    path = parent(home, started() + [queued(REPORT, 9)])
    sub(home, "a6568", [])
    (e,) = subs_of(conv.read(str(path))["entries"])
    assert "status" not in e and e["text"].startswith("## stats")


def test_its_name_and_what_it_was_asked_without_its_meta(home):
    "No file beside it: the line that started it names what it was asked."
    path = parent(home, started(desc="Find the flaky test") + [queued(REPORT, 9), queued(notice_text(), 10)])
    (e,) = subs_of(conv.read(str(path))["entries"])
    assert (e["who"], e["title"]) == ("", "Find the flaky test")


def test_a_notice_whose_report_went_before_it_out_of_sight(home):
    "Its report said to be in a message the tail does not reach: none shown, not the pointer."
    path = parent(home, [queued(notice_text(), 10)])
    sub(home, "a6568", [])
    (e,) = subs_of(conv.read(str(path))["entries"])
    assert (e["title"], e["text"], e["status"], e["uses"]) == ("Build R3 stats", "", "completed", 61)


def test_one_waiting_on_its_own_work_has_not_come_back(home):
    "Told it stopped with work of its own running: no report, not ended -- until it is."
    path = parent(home, started() + [queued(INTERIM, 20), u(INTERIM, 21)])
    sub(home, "a6568", [])
    entries = conv.read(str(path))["entries"]
    assert subs_of(entries) == [] and not any(e.get("kind") == "note" for e in entries)
    (tools,) = [e for e in entries if e["role"] == "tools"]
    assert "state" not in tools["tools"][0]
    with open(path, "a") as out:
        out.write(lines([queued(REPORT, 60), queued(notice_text(), 61)]))
    (e,) = subs_of(conv.read(str(path))["entries"])
    assert (e["status"], e["uses"], e["text"][:8]) == ("completed", 61, "## stats")


def test_a_sub_agent_run_while_its_agent_waited(home):
    "No launch: what the call answered is its report."
    path = parent(home, [
        a(use("t9", "Task", description="Survey the API", subagent_type="Explore"), second=1),
        u([result("t9", [{"type": "text", "text": "Three endpoints lack auth."}])], 30),
        a(use("t10", "Agent", description="Break it"), second=31),
        u([result("t10", "ran out of turns", is_error=True)], 40),
    ])
    entries = conv.read(str(path))["entries"]
    assert [(e["title"], e["text"], e["status"]) for e in subs_of(entries)] == [
        ("Survey the API", "Three endpoints lack auth.", "completed"), ("Break it", "ran out of turns", "failed")]
    tools = [t for e in entries if e["role"] == "tools" for t in e["tools"]]
    assert [t.get("state") for t in tools] == ["completed", "failed"]


def test_the_chat_leaves_reports_in_the_room(home):
    path = parent(home, started() + [queued(REPORT, 9), queued(notice_text(), 10),
                                     a({"type": "text", "text": "Round 3 is done."}, second=11)])
    sub(home, "a6568", [])
    c = conv.chat([("rd", str(path))])
    assert [(x["kind"], x["text"]) for x in c["entries"]] == [
        ("prompt", "Build the five rounds"), ("turn", "Round 3 is done.")]


def test_a_meta_is_read_only_beside_its_transcript(home):
    p = parent(home, [])
    sub(home, "a1", [])
    assert conv.sub_meta(p, "a1")["agentType"] == "general-purpose"
    assert conv.sub_meta(p, "../../x") == {}
    assert conv.sub_meta(p, "nope") == {}


# ---- The ones running ------------------------------------------------------


def working(second=10, tokens=192359):
    "A sub-agent part way: asked, two calls made, the last a command."
    return [
        u("You are building one pair-debug project…", 0),
        a(use("s1", "Read", file_path="/x/guide.md"), second=2, usage=1000),
        u([result("s1", "…")], 3),
        a({"type": "text", "text": "Now the build.\nMore."}, second=4, usage=5000),
        a(use("s2", "Bash", command="python3 build.py", description="Build the stats project"), second=second,
          usage=tokens),
    ]


def handed_back(second=60):
    return [
        a(use("h1", "SubagentHandback", message="## stats: built"), second=second),
        u([result("h1", [{"type": "text", "text": '{"success":true,"message":"Report delivered to your caller."}'}])],
          second + 1),
    ]


def test_one_running_says_what_it_is_doing(home):
    p = parent(home, started())
    f = sub(home, "a6568", working())
    r = subagents.live(str(p))
    assert r == {
        "subs": [{"id": "a6568", "who": "general-purpose", "title": "Build R3 stats", "started": at(0),
                  "tools": 2, "tokens": 193229, "doing": "Bash · Build the stats project"}],
        "watch": [str(f.parent), str(f)],
    }


def test_what_it_said_last_is_what_it_is_doing(home):
    p = parent(home, started())
    sub(home, "a6568", working()[:4], meta={"agentType": "general-purpose", "name": "code-review"})
    (s,) = subagents.live(str(p))["subs"]
    assert (s["who"], s["title"], s["doing"], s["tools"]) == ("code-review", "", "Now the build.", 1)


def test_one_that_handed_back_has_stopped_until_sent_more(home):
    p = parent(home, started())
    f = sub(home, "a6568", working() + handed_back())
    assert subagents.live(str(p)) == {"subs": [], "watch": [str(f.parent)]}
    # Sent another message: it runs again, before it says a word.
    with f.open("a") as out:
        out.write(lines([u("Also round 6.", 90)]))
    assert [s["id"] for s in subagents.live(str(p))["subs"]] == ["a6568"]
    # Read on from where it was.
    with f.open("a") as out:
        out.write(lines([a(use("s3", "Bash", command="ls"), second=91)]))
    (s,) = subagents.live(str(p))["subs"]
    assert (s["tools"], s["doing"], s["started"]) == (4, "Bash · ls", at(0))


def test_one_its_parent_was_told_had_stopped(home):
    "Failed, killed: told so after its last turn.  Before it, it ran again."
    p = parent(home, started() + [queued(notice_text(status="killed"), 30)])
    sub(home, "a6568", working(second=29))
    assert subagents.live(str(p))["subs"] == []
    subagents._subs.clear()
    subagents._parents.clear()
    sub(home, "a6568", working(second=50))
    assert [s["id"] for s in subagents.live(str(p))["subs"]] == ["a6568"]


@pytest.mark.parametrize("said", [INTERIM, re.sub(r"<note>.*?</note>\n", "", INTERIM, flags=re.S)],
                         ids=["noted", "by its result alone"])
def test_one_waiting_on_its_own_work_runs_on(home, said):
    "Its parent told it stopped with work of its own still running: it goes on."
    p = parent(home, started() + [queued(said, 30),
                                  {"type": "attachment", "timestamp": at(31),
                                   "attachment": {"type": "queued_command", "prompt": said}}])
    sub(home, "a6568", working(second=29))
    assert [s["id"] for s in subagents.live(str(p))["subs"]] == ["a6568"]


def test_one_silent_too_long_is_counted_out(home):
    p = parent(home, started())
    f = sub(home, "a6568", working())
    old = time.time() - subagents.STALE - 5
    os.utime(f, (old, old))
    assert subagents.live(str(p))["subs"] == []


def test_oldest_first_and_only_its_own(home):
    p = parent(home, started())
    sub(home, "a2", [dict(r, timestamp=at(30)) for r in working()])
    sub(home, "a1", working())
    sub(home, "z9", working(), name="other")
    assert [s["id"] for s in subagents.live(str(p))["subs"]] == ["a1", "a2"]


def test_a_file_that_leaves_the_transcripts_is_not_read(home, tmp_path):
    p = parent(home, started())
    outside = tmp_path / "elsewhere.jsonl"
    outside.write_text(lines(working()))
    sub(home, "a1", [])
    link = home / PROJ / "abc" / "subagents" / "agent-a1.jsonl"
    link.unlink()
    link.symlink_to(outside)
    assert subagents.live(str(p))["subs"] == []


def test_none_for_another_cli_or_before_any(home):
    p = parent(home, started())
    assert subagents.live(str(p)) == {"subs": [], "watch": [str(home / PROJ / "abc" / "subagents")]}
    g = home / ".grok" / "sessions" / "s1" / "chat_history.jsonl"
    g.parent.mkdir(parents=True)
    g.write_text("{}\n")
    assert subagents.live(str(g)) == {"subs": [], "watch": []}


def test_a_file_is_read_as_it_grows(home, monkeypatch):
    "Only what was added; a line still being written waits for its end."
    f = home / "x.jsonl"
    f.write_text('{"a": 1}\n{"b": 2}\n')
    follow = subagents.Follow(f)
    assert follow.look() == (['{"a": 1}', '{"b": 2}'], True)
    assert follow.look() == ([], False)
    with f.open("a") as out:
        out.write('{"c": 3}\n{"d"')
    assert follow.look() == (['{"c": 3}'], False)
    with f.open("a") as out:
        out.write(': 4}\n')
    assert follow.look() == (['{"d": 4}'], False)
    f.write_text('{"e": 5}\n')  # shorter: read again
    assert follow.look() == (['{"e": 5}'], True)


def test_a_parent_is_read_from_its_tail(home):
    f = home / "p.jsonl"
    f.write_text('{"first": "x"}\n' * 50 + '{"last": 1}\n')
    lines_, fresh = subagents.Follow(f, back=30).look()
    assert fresh and lines_ == ['{"first": "x"}', '{"last": 1}']


def test_rpc(home):
    p = parent(home, started())
    sub(home, "a6568", working())

    def call(params):
        return asyncio.run(handler.handle(JsonRpcRequest(jsonrpc="2.0", id=1, method="herd_subagents", params=params)))

    r = call({"path": str(p)})
    assert r.error is None and [s["id"] for s in r.result["subs"]] == ["a6568"]
    assert call({"path": str(home / "x.jsonl")}).error.message == "no such transcript"


# ---- Shells and monitors in the background --------------------------------


def ago_at(seconds):
    "A record's time, SECONDS before now."
    return (datetime.now(timezone.utc) - timedelta(seconds=seconds)).isoformat().replace("+00:00", "Z")


def shell(i, bid, seconds, desc="Run the full backend test suite"):
    "A Bash call run in the background SECONDS ago, as Claude Code 2.1.299 answers it."
    return [dict(a(use(i, "Bash", command="pytest -q", description=desc, run_in_background=True)),
                 timestamp=ago_at(seconds)),
            dict(u([result(i, "Command running in background with ID: %s. Output is being written to: "
                              "/private/tmp/x/tasks/%s.output" % (bid, bid))]), timestamp=ago_at(seconds))]


def monitor(i, mid, seconds):
    return [dict(a(use(i, "Monitor", command="until [ -s f ]; do sleep 5; done", description="Wait for it")),
                 timestamp=ago_at(seconds)),
            dict(u([result(i, [{"type": "text", "text":
                                "Monitor started (task %s, expires in 15m unless the source ends first; "
                                "you get one notice at expiry — re-arm if you still need the watch). "
                                "You will be notified on each event." % mid}])]), timestamp=ago_at(seconds))]


def shell_ended(bid, status="completed"):
    return queued("<task-notification>\n<task-id>%s</task-id>\n<tool-use-id>toolu_x</tool-use-id>\n"
                  "<output-file>/private/tmp/x/tasks/%s.output</output-file>\n<status>%s</status>\n"
                  '<summary>Background command "Run the full backend test suite" %s (exit code 0)</summary>\n'
                  "</task-notification>" % (bid, bid, status, status))


def monitor_event(mid, event="Pytest: 3705 passed"):
    return queued('<task-notification>\n<task-id>%s</task-id>\n<summary>Monitor event: "Wait for it"</summary>\n'
                  "<event>%s</event>\n</task-notification>" % (mid, event))


def test_shells_and_monitors_run_until_told_they_ended(home):
    p = parent(home, started() + shell("t1", "bx1", 90) + shell("t2", "bx2", 120) + monitor("t3", "bm1", 60)
               + [monitor_event("bm1")])
    assert [(t["id"], t["kind"]) for t in subagents.tasks(str(p))] == [
        ("bx2", "shell"), ("bx1", "shell"), ("bm1", "monitor")]  # an event is not its end
    with p.open("a") as w:
        w.write(lines([shell_ended("bx1"), shell_ended("bx2", "killed"),
                       {"type": "attachment", "timestamp": at(9),
                        "attachment": {"type": "queued_command", "prompt": shell_ended("bx1")}}]))
    assert [t["id"] for t in subagents.tasks(str(p))] == ["bm1"]
    with p.open("a") as w:
        w.write(lines([monitor_event("bm1", "[Monitor expired after 15m with no events delivered. "
                                            "Re-arm it if you still need the watch.]")]))
    assert subagents.tasks(str(p)) == []
    # A conversation begun again: what it had running is read anew.
    p.write_text(lines(shell("t1", "bx3", 5)))
    assert [t["id"] for t in subagents.tasks(str(p))] == ["bx3"]
    p.write_text(lines(shell("t2", "bx4", 4, desc="Build")))
    assert [t["id"] for t in subagents.tasks(str(p))] == ["bx4"]


def stop(i, tid, seconds, tool="TaskStop", key="task_id", said="Successfully stopped task: %s (pytest -q)",
         error=False):
    """A stop call SECONDS ago and its result, as Claude Code writes them: no
    task notification follows.  TaskStop's result is JSON (its message, the
    task); KillShell's, before it, said "Successfully killed shell"."""
    text = json.dumps({"message": said % tid, key: tid, "task_type": "local_bash", "command": "pytest -q"})
    return [dict(a(use(i, tool, **{key: tid})), timestamp=ago_at(seconds)),
            dict(u([result(i, text, **({"is_error": True} if error else {}))]),
                 timestamp=ago_at(seconds), toolUseResult={"message": said % tid, key: tid})]


def test_a_task_it_stopped_itself_has_ended(home):
    """TaskStop -- KillShell before it -- ends the task it names once its
    result comes back fine: Claude Code tells nothing more of it.  Not a
    stop that failed, nor one only quoted (an api-request-blob)."""
    p = parent(home, shell("t1", "bx1", 90) + shell("t2", "bx2", 80) + shell("t3", "bx3", 70)
               + shell("t4", "bash_1", 60) + shell("t5", "bx5", 50)
               + stop("s1", "bx1", 40)
               + [{"type": "api-request-blob", "hash": "h", "sessionId": "x",
                   "message": {"content": [use("s9", "TaskStop", task_id="bx2"),
                                           result("s9", json.dumps({"message": "Successfully stopped task: bx2 (x)"}))]}}]
               + stop("s3", "bx3", 30, said="No task found with ID: %s", error=True)
               + stop("s4", "bash_1", 20, tool="KillShell", key="shell_id", said="Successfully killed shell: %s (sleep 100)"))
    assert [t["id"] for t in subagents.tasks(str(p))] == ["bx2", "bx3", "bx5"]
    # Its call read in an earlier look, its result -- in words not known --
    # in this one: the call names it.
    call, answer = stop("s5", "bx5", 10, said="Task %s is no more")
    with p.open("a") as w:
        w.write(lines([call]))
    assert [t["id"] for t in subagents.tasks(str(p))] == ["bx2", "bx3", "bx5"]
    with p.open("a") as w:
        w.write(lines([answer]))
    assert [t["id"] for t in subagents.tasks(str(p))] == ["bx2", "bx3"]
    # Its call out of what was read: the result's own words name it.
    with p.open("a") as w:
        w.write(lines(stop("s6", "bx3", 5)[1:]))
    assert [t["id"] for t in subagents.tasks(str(p))] == ["bx2"]


def test_one_begun_an_hour_ago_is_not_waited_on(home):
    "A server left running, or a wait stuck: one ran 23 hours here (2026-10)."
    p = parent(home, shell("t1", "bx1", subagents.STALE + 5) + shell("t2", "bx2", 10))
    assert [t["id"] for t in subagents.tasks(str(p))] == ["bx2"]


def test_only_its_calls_results_begin_one(home):
    said = "Command running in background with ID: bfake. Not really."
    p = parent(home, [dict(u(said), timestamp=ago_at(5)),
                      dict(a({"type": "text", "text": said}), timestamp=ago_at(4)),
                      # A log it read that says so, further down.
                      dict(a(use("r1", "Read", file_path="/tmp/log")), timestamp=ago_at(3)),
                      dict(u([result("r1", "1\tstarting\n2\t" + said)]), timestamp=ago_at(3))])
    assert subagents.tasks(str(p)) == []


def test_none_for_another_cli(home):
    path = home / ".grok" / "sessions" / "x" / "chat_history.jsonl"
    path.parent.mkdir(parents=True)
    path.write_text(lines(shell("t1", "bx1", 10)))
    assert subagents.tasks(str(path)) == []


# ---- A turn that ends with them running -----------------------------------

TEAM = [
    {"name": "lead", "kind": "claude", "state": "idle", "project": "/src/a/", "role": "lead"},
    {"name": "rd", "kind": "claude", "state": "idle", "project": "/src/a/"},
    {"name": "qa", "kind": "claude", "state": "idle", "project": "/src/a/"},
]


def team(home, p):
    "The herd, with the lead's ask pasted into rd, whose conversation is P."
    reset_herd()
    h = HerdStore(home / "herd.sqlite")
    h.tick(sessions=TEAM)
    ask = h.ask("rd", "build the five rounds", from_name="lead")
    out = h.tick(sessions=TEAM)
    h.tick(sessions=TEAM, ack_ids=[m["id"] for m in out["pending"]])
    h.report("rd", cli="claude", conversation="abc", transcript=str(p))
    return h, ask


def reports(h):
    return [(r["session"], r["state"], r["reason"]) for r in h.tick(sessions=TEAM)["reports"]]


def test_a_turn_ended_with_sub_agents_running_is_waiting(home):
    "Seen live (2026-10-15): four handed out, the turn ended, and the herd said idle."
    p = parent(home, started())
    f = sub(home, "a6568", working())
    h, ask = team(home, p)
    out = h.report("rd", state="done", reason="finished its turn", last="Handed the rounds to a sub-agent.")
    assert (out["state"], out["closed_ask"]) == ("waiting", None)
    assert reports(h) == [("rd", "waiting", "waiting on 1 sub-agent")]
    assert h.get_ask(ask["id"])["status"] == "delivered"
    # It hands back; rd wakes to it and ends with nothing out.
    with f.open("a") as w:
        w.write(lines(handed_back()))
    h.report("rd", state="working")
    out = h.report("rd", state="done", reason="finished its turn", last="All five rounds built.")
    assert (out["state"], out["closed_ask"]) == ("done", ask["id"])
    assert h.get_ask(ask["id"])["reply"] == "All five rounds built."


def test_waiting_says_on_what(home):
    p = parent(home, started() + shell("t1", "bx1", 30) + monitor("t2", "bm1", 20))
    sub(home, "a6568", working())
    sub(home, "a7777", working())
    h, _ = team(home, p)
    h.ask("qa", "test round 1", from_name="rd")
    h.report("rd", state="done", last="Asked qa; two sub-agents building, the suite running.")
    assert reports(h) == [("rd", "waiting", "waiting on qa, 2 sub-agents, 1 shell and 1 monitor")]


def test_a_turn_ended_with_a_shell_running_is_waiting(home):
    "Seen live (2026-10-15): a wait for the staging triage in the background, and the herd said idle."
    p = parent(home, shell("t1", "bx1", 30, "Wait until the requeued staging issues finish triage"))
    h, ask = team(home, p)
    out = h.report("rd", state="done", last="A check in the background waits for the triage.")
    assert (out["state"], out["closed_ask"]) == ("waiting", None)
    assert reports(h) == [("rd", "waiting", "waiting on 1 shell")]
    assert h.settle(ask["id"], screen="")["waiting_on"] == ["1 shell"]
    # It ends; rd wakes to it, and ends its turn with nothing out.
    with p.open("a") as w:
        w.write(lines([shell_ended("bx1")]))
    h.report("rd", state="working")
    out = h.report("rd", state="done", last="Gemini got 13 of 13 right on staging.")
    assert (out["state"], out["closed_ask"]) == ("done", ask["id"])


def test_a_waiting_agent_takes_an_ask_after_an_idle_one(home):
    p = parent(home, started())
    sub(home, "a6568", working())
    h, _ = team(home, p)
    snap = [dict(r, state="waiting") if r["name"] == "rd" else r for r in TEAM]
    h.tick(sessions=snap)
    assert h.ask("claude", "x", from_name="lead")["to"] == "qa"
    assert h.ask("rd", "y", from_name="qa")["to"] == "rd"  # its box is free
    h.tick(sessions=[dict(r, state="dead") if r["name"] == "qa" else r for r in snap])
    assert h.ask("claude", "z", from_name="lead")["to"] == "rd"


def test_settle_leaves_the_ask_open_while_sub_agents_run(home):
    "Emacs's own, for a taker with no hooks or a waiting it did not hear."
    p = parent(home, started())
    sub(home, "a6568", working())
    h, ask = team(home, p)
    r = h.settle(ask["id"], screen="handed it on")
    assert (r["status"], r["waiting_on"]) == ("delivered", ["1 sub-agent"])


def ago(seconds):
    return (datetime.now(timezone.utc) - timedelta(seconds=seconds)).isoformat()


def test_waiting_with_nothing_out_and_no_word_is_done(home):
    "Its sub-agent killed, say, and rd never woke to it."
    p = parent(home, started())
    f = sub(home, "a6568", working())
    h, ask = team(home, p)
    h.report("rd", state="working")  # rd woke to the ask: a turn after delivery
    h.report("rd", state="done", last="Handed it on.")
    assert reports(h)[0][1] == "waiting"
    old = time.time() - subagents.STALE - 5
    os.utime(f, (old, old))  # counted out
    os.utime(p, (old, old))
    assert reports(h) == []  # but it said so a moment ago
    h._conn.execute("UPDATE reports SET at = ?", (ago(herd_mod.WAIT_GRACE + 1),))
    p.touch()
    assert reports(h) == []  # and its conversation just changed: a sub-agent's word, waking it
    os.utime(p, (old, old))
    assert h.tick(sessions=[r for r in TEAM if r["name"] != "rd"])["reports"] == []  # not in the herd now
    assert reports(h) == [("rd", "done", "nothing left to wait on")]
    assert reports(h) == []
    # Its ask is closed as for any taker that stopped: Emacs settles it.
    assert h.get_ask(ask["id"])["status"] == "delivered"
    h.settle(ask["id"], screen="")
    assert h.get_ask(ask["id"])["reply"] == "Handed it on."


def test_still_out_it_waits_on(home):
    p = parent(home, started())
    sub(home, "a6568", working())
    h, _ = team(home, p)
    h.report("rd", state="done", last="Handed it on.")
    reports(h)
    old = time.time() - subagents.STALE - 5
    os.utime(p, (old, old))
    h._conn.execute("UPDATE reports SET at = ?", (ago(herd_mod.WAIT_GRACE + 1),))
    assert reports(h) == []


def test_an_heir_is_not_waiting_on_the_old_ones_sub_agents(home, monkeypatch):
    p = parent(home, started())
    f = sub(home, "a6568", working())
    h, _ = team(home, p)
    h.report("rd", state="done", last="Handed it on.")
    h.unlink("rd")  # relayed: a new CLI has its name
    monkeypatch.setattr(herd_mod, "WAIT_GRACE", 0)
    old = time.time() - subagents.STALE - 5
    os.utime(f, (old, old))
    assert reports(h) == []


# ---- Agy and Grok sub-agents and background tasks --------------------------


def agy_tx(home, cid, records):
    p = home / ".gemini" / "antigravity-cli" / "brain" / cid / ".system_generated" / "logs" / "transcript.jsonl"
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(lines(records))
    return p


def test_agy_subagents_live_handoff_kill_and_stale(home):
    t0 = ago_at(30)
    t1 = ago_at(25)
    t2 = ago_at(20)
    p = agy_tx(
        home,
        "parent-1",
        [
            {
                "step_index": 1,
                "type": "PLANNER_RESPONSE",
                "source": "MODEL",
                "created_at": t0,
                "content": "Spawning two researchers.",
                "tool_calls": [
                    {
                        "id": "c1",
                        "name": "invoke_subagent",
                        "args": {
                            "Subagents": json.dumps(
                                [
                                    {"TypeName": "research", "Role": "Explore importers", "Prompt": "check agy.py"},
                                    {"TypeName": "self", "Role": "Audit tests", "Prompt": "check tests"},
                                ]
                            )
                        },
                    }
                ],
            },
            {
                "step_index": 2,
                "type": "INVOKE_SUBAGENT",
                "source": "MODEL",
                "created_at": t0,
                "content": '[{"conversationId": "sub-1"}, {"conversationId": "sub-2"}]',
            },
        ],
    )
    s1 = agy_tx(
        home,
        "sub-1",
        [
            {"step_index": 0, "type": "USER_INPUT", "source": "USER_EXPLICIT", "created_at": t0, "content": "check agy.py"},
            {
                "step_index": 1,
                "type": "PLANNER_RESPONSE",
                "source": "MODEL",
                "created_at": t1,
                "input_tokens": 1200,
                "cache_read_tokens": 300,
                "content": "Reading agy.py.",
                "tool_calls": [
                    {
                        "id": "x1",
                        "name": "view_file",
                        "args": {"AbsolutePath": json.dumps("/x/agy.py"), "toolSummary": json.dumps("View agy.py")},
                    }
                ],
            },
        ],
    )
    s2 = agy_tx(
        home,
        "sub-2",
        [
            {"step_index": 0, "type": "USER_INPUT", "source": "USER_EXPLICIT", "created_at": t0, "content": "check tests"},
            {
                "step_index": 1,
                "type": "PLANNER_RESPONSE",
                "source": "MODEL",
                "created_at": t2,
                "content": "Running pytest.",
                "tool_calls": [
                    {
                        "id": "x2",
                        "name": "run_command",
                        "args": {"CommandLine": json.dumps("pytest -q")},
                    }
                ],
            },
        ],
    )
    got = subagents.live(str(p))
    assert got["subs"] == [
        {
            "id": "sub-1",
            "who": "research",
            "title": "Explore importers",
            "started": t0,
            "tools": 1,
            "tokens": 1500,
            "doing": "view_file · View agy.py",
        },
        {
            "id": "sub-2",
            "who": "self",
            "title": "Audit tests",
            "started": t0,
            "tools": 1,
            "tokens": None,
            "doing": "run_command · pytest -q",
        },
    ]
    assert set(got["watch"]) == {str(s1), str(s2)}

    # sub-1 hands its report back via send_message -> no longer running.
    with s1.open("a") as f:
        f.write(
            lines(
                [
                    {
                        "step_index": 2,
                        "type": "PLANNER_RESPONSE",
                        "source": "MODEL",
                        "created_at": ago_at(15),
                        "content": "Done.",
                        "tool_calls": [{"id": "x3", "name": "send_message", "args": {"Recipient": '"parent-1"', "Message": '"ok"'}}],
                    }
                ]
            )
        )
    assert [s["id"] for s in subagents.live(str(p))["subs"]] == ["sub-2"]

    # Parent kills sub-2 via manage_subagents -> no longer running.
    with p.open("a") as f:
        f.write(
            lines(
                [
                    {
                        "step_index": 3,
                        "type": "PLANNER_RESPONSE",
                        "source": "MODEL",
                        "created_at": ago_at(10),
                        "content": "Cancel sub-2.",
                        "tool_calls": [
                            {
                                "id": "c2",
                                "name": "manage_subagents",
                                "args": {"Action": '"kill"', "ConversationIds": '["sub-2"]'},
                            }
                        ],
                    }
                ]
            )
        )
    assert subagents.live(str(p))["subs"] == []


def test_agy_background_tasks_termination_and_staleness(home):
    t0 = ago_at(40)
    t1 = ago_at(30)
    p = agy_tx(
        home,
        "parent-bg",
        [
            {
                "step_index": 1,
                "type": "RUN_COMMAND",
                "source": "MODEL",
                "created_at": t0,
                "content": "Tool is running as a background task with task id: parent-bg/task-1\n",
            },
            {
                "step_index": 2,
                "type": "GENERIC",
                "source": "MODEL",
                "created_at": t1,
                "content": (
                    "Tool is running as a background task with task id: parent-bg/timer-2\n"
                    "Task Description: Timer: 60s (condition: any)\n"
                ),
            },
        ],
    )
    assert [(t["id"], t["kind"]) for t in subagents.tasks(str(p))] == [
        ("parent-bg/task-1", "shell"),
        ("parent-bg/timer-2", "monitor"),
    ]

    # Task-1 finishes via SYSTEM_MESSAGE notification.
    with p.open("a") as f:
        f.write(
            lines(
                [
                    {
                        "step_index": 3,
                        "type": "SYSTEM_MESSAGE",
                        "source": "SYSTEM",
                        "created_at": ago_at(20),
                        "content": '[Message] sender=parent-bg/task-1 content=Task id "parent-bg/task-1" finished',
                    }
                ]
            )
        )
    assert [t["id"] for t in subagents.tasks(str(p))] == ["parent-bg/timer-2"]

    # User interrupts turn: "All your subagents and background tasks have been terminated."
    with p.open("a") as f:
        f.write(
            lines(
                [
                    {
                        "step_index": 4,
                        "type": "SYSTEM_MESSAGE",
                        "source": "SYSTEM",
                        "created_at": ago_at(10),
                        "content": "All your subagents and background tasks have been terminated.",
                    }
                ]
            )
        )
    assert subagents.tasks(str(p)) == []

    # A task started more than STALE seconds ago is never waited on.
    p_stale = agy_tx(
        home,
        "parent-stale",
        [
            {
                "step_index": 1,
                "type": "RUN_COMMAND",
                "source": "MODEL",
                "created_at": ago_at(subagents.STALE + 10),
                "content": "Tool is running as a background task with task id: parent-stale/task-old\n",
            }
        ],
    )
    assert subagents.tasks(str(p_stale)) == []


def test_grok_tasks_and_subagents_lifecycle_and_staleness(home):
    g = home / ".grok" / "sessions" / "proj" / "s1" / "chat_history.jsonl"
    g.parent.mkdir(parents=True, exist_ok=True)
    records = [
        {
            "type": "assistant",
            "content": "Starting background work.",
            "tool_calls": [
                {
                    "id": "call_sh",
                    "name": "run_terminal_command",
                    "arguments": json.dumps({"command": "pytest -q", "run_in_background": True}),
                },
                {
                    "id": "call_mon",
                    "name": "monitor_background",
                    "arguments": json.dumps({"command": "watch_build", "description": "Watch build"}),
                },
                {
                    "id": "call_sub",
                    "name": "spawn_subagent",
                    "arguments": json.dumps(
                        {"subagent_type": "research", "description": "Audit Grok tools", "prompt": "inspect tools"}
                    ),
                },
            ],
        },
        {
            "type": "tool_result",
            "tool_call_id": "call_sh",
            "content": (
                "<task-id>grok_sh1</task-id>\n<task-type>bash</task-type>\n"
                "<status>running</status>\n<summary>pytest -q</summary>"
            ),
        },
        {
            "type": "tool_result",
            "tool_call_id": "call_mon",
            "content": (
                "<task-id>grok_mon1</task-id>\n<task-type>monitor</task-type>\n"
                "<status>running</status>\n<summary>Watch build</summary>"
            ),
        },
        {
            "type": "tool_result",
            "tool_call_id": "call_sub",
            "content": (
                "<task-id>grok_sub1</task-id>\n<task-type>subagent</task-type>\n"
                "<status>running</status>\n<summary>Audit Grok tools</summary>"
            ),
        },
    ]
    g.write_text(lines(records))

    assert [(t["id"], t["kind"]) for t in subagents.tasks(str(g))] == [
        ("grok_sh1", "shell"),
        ("grok_mon1", "monitor"),
    ]
    subs = subagents.live(str(g))["subs"]
    assert len(subs) == 1
    assert (subs[0]["id"], subs[0]["who"], subs[0]["title"]) == ("grok_sub1", "research", "Audit Grok tools")

    # check_command_or_subagent reports grok_sh1 completed, and kill_command_or_subagent kills grok_mon1.
    with g.open("a") as f:
        f.write(
            lines(
                [
                    {
                        "type": "tool_result",
                        "tool_call_id": "call_chk",
                        "content": "=== Task grok_sh1 ===\nType: bash\nStatus: completed\nExit code: 0\n",
                    },
                    {
                        "type": "assistant",
                        "content": "Killing monitor.",
                        "tool_calls": [
                            {
                                "id": "call_kill",
                                "name": "kill_command_or_subagent",
                                "arguments": json.dumps({"task_id": "grok_mon1"}),
                            }
                        ],
                    },
                    {
                        "type": "user",
                        "content": [
                            {
                                "type": "text",
                                "text": '<system-reminder>Subagent "grok_sub1" completed.</system-reminder>',
                            }
                        ],
                    },
                ]
            )
        )
    assert subagents.tasks(str(g)) == []
    assert subagents.live(str(g))["subs"] == []

    # A Grok transcript untouched for STALE seconds never keeps tasks or sub-agents waiting.
    g_stale = home / ".grok" / "sessions" / "proj" / "s2" / "chat_history.jsonl"
    g_stale.parent.mkdir(parents=True, exist_ok=True)
    g_stale.write_text(lines(records))
    old = time.time() - subagents.STALE - 10
    os.utime(g_stale, (old, old))
    assert subagents.tasks(str(g_stale)) == []
    assert subagents.live(str(g_stale))["subs"] == []

