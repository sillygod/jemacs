"""How an answered ask's work went, in one line (ask 4093758e): read from
the taker's own transcript between the ask's delivery and its reply, and
the herd's records -- counts only, never a guess, never in a reply's way."""

import json
import os
import subprocess
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest

import ask_trace as T
import conversation
import herd as herd_mod
import threads as threads_mod
from herd import HerdStore, get_herd, reset_herd
from tests.test_ask import HERD, SNAP, _deliver, live  # noqa: F401  (fixture)

ASK = "4093758e"
SECRET = "/Users/me/src/proj/.env"     # a path no line may carry
CMD = "pytest -q tests/test_secret_thing.py"


@pytest.fixture
def home(tmp_path, monkeypatch):
    h = tmp_path / "home"
    h.mkdir()
    monkeypatch.setenv("HOME", str(h))
    return h


def _write(path: Path, records) -> str:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(json.dumps(r) + "\n" for r in records))
    return str(path)


def _ts(minutes: float) -> str:
    return (datetime(2026, 10, 26, 9, 0, tzinfo=timezone.utc) + timedelta(minutes=minutes)).isoformat().replace("+00:00", "Z")


FRAME = f"[ghostherd message from lead-dp → claude-dp]\n[ask {ASK} -- whoever asked is waiting for your answer]\nFix the build."


# ---- claude ----------------------------------------------------------------------

def c_user(text, t, **kw):
    return dict({"type": "user", "timestamp": _ts(t), "message": {"role": "user", "content": text}}, **kw)


def c_use(i, name, t, **inp):
    return {"type": "assistant", "timestamp": _ts(t),
            "message": {"id": "m" + i, "role": "assistant", "content": [{"type": "tool_use", "id": i, "name": name, "input": inp}]}}


def c_result(i, text, t, error=False):
    return {"type": "user", "timestamp": _ts(t),
            "message": {"role": "user", "content": [dict({"type": "tool_result", "tool_use_id": i, "content": text},
                                                         **({"is_error": True} if error else {}))]}}


def claude_transcript(home, extra_before=(), subagents=None):
    recs = list(extra_before) + [
        c_use("old", "Bash", 0, command="ls"),                     # before the ask: not its work
        c_result("old", "Exit code 2\nboom", 0.1, error=True),
        c_user(FRAME, 1),
        c_use("t1", "Read", 2, file_path=SECRET),
        c_result("t1", "x", 2.1),
        c_use("t2", "Bash", 3, command=CMD),
        c_result("t2", "Exit code 1\nFAILED test_secret_thing", 3.1, error=True),
        c_use("t3", "Edit", 4, file_path=SECRET, old_string="a", new_string="b"),
        c_result("t3", "<tool_use_error>File has not been read yet. Read it first before writing to it.</tool_use_error>", 4.1, error=True),
        c_use("t4", "Bash", 5, command="cat " + SECRET),
        c_result("t4", "Permission for this action was denied by the Claude Code auto mode classifier.", 5.1, error=True),
        c_use("t5", "Bash", 6, command="grep -ao '.{0,9}x' ~/.local/bin/claude"),
        c_result("t5", "Denied by the herd's check for the fact `grep-cli-binaries-cheaply': ...", 6.1, error=True),
        c_use("t6", "Agent", 7, description="look around", prompt="find it"),
        c_result("t6", "found it", 9),
        c_use("t7", "Bash", 10, command=CMD),
        c_result("t7", "1 passed", 10.5),
        c_use("t8", "Bash", 11, command=f'"$GHOSTHERD_HERD" reply {ASK} <<\'HERD\'\nFixed.\nHERD'),
        c_result("t8", "herd: answered", 11.1),
        c_use("t9", "Bash", 12, command="rm -rf build"),          # after the reply: not its work
        c_result("t9", "Exit code 1", 12.1, error=True),
    ]
    path = home / ".claude" / "projects" / "-src-dp" / "s1.jsonl"
    out = _write(path, recs)
    for sid, began, rs in subagents or []:
        _write(path.with_suffix("") / "subagents" / f"agent-{sid}.jsonl",
               [dict(r, isSidechain=True) for r in [c_user("find it", began)] + rs])
    return out


def test_claude_counts_the_work_between_delivery_and_reply(home):
    sub_recs = [c_use("s1", "Grep", 7.5, pattern="x"), c_use("s2", "Bash", 7.6, command="false"),
                c_result("s2", "Exit code 1", 7.7, error=True)]
    path = claude_transcript(home, subagents=[("aa11", 7.2, sub_recs), ("old1", 0.5, sub_recs)])
    w = T.read_work(path, ASK)
    assert w.seen and w.cli == "claude" and w.failures_known
    assert dict(w.tools) == {"Read": 1, "Bash": 4, "Edit": 1, "Agent": 1}
    # Each kind; the check's denial is the herd's to count (lint_denials).
    assert dict(w.failed) == {"shell": 1, "edit": 1, "denied": 1}
    assert (w.subs, w.subs_read, w.sub_tools, dict(w.sub_failed)) == (1, 1, 2, {"shell": 1})
    out = T.compose(w, 660, lint=1)
    assert out["line"] == ("How it went: 11 min · 7 tool calls, 3 failed (1 permission denial, 1 edit not applied, "
                           "1 shell exit) · 1 sub-agent (2 tool calls, 1 failed) · 1 check denial · " + T.NUDGE)
    assert out["rough"]      # a permission denial and a check's: two denials


def test_an_ask_taken_mid_turn_is_found(home):
    """A message that comes while claude works is absorbed mid-turn: an
    attachment of the queued command, after a queue-operation -- never a
    user record.  As this machine's transcripts have it."""
    recs = [
        {"type": "queue-operation", "operation": "enqueue", "timestamp": _ts(0.9), "content": FRAME},
        c_use("x", "Read", 0.95, file_path="/y"),
        {"type": "queue-operation", "operation": "remove", "timestamp": _ts(1.1), "content": FRAME,
         "reason": "absorbed_mid_turn"},
        {"type": "attachment", "isSidechain": False, "timestamp": _ts(1),
         "attachment": {"type": "queued_command", "prompt": FRAME, "commandMode": "prompt"}},
        c_use("a", "Bash", 2, command="make"),
        c_use("r", "Bash", 3, command=f"herd reply {ASK} done"),
    ]
    path = _write(home / ".claude" / "projects" / "-x" / "m.jsonl", recs)
    w = T.read_work(path, ASK)
    assert w.seen and dict(w.tools) == {"Bash": 1}


def test_no_command_text_or_path_in_the_line(home):
    path = claude_transcript(home)
    out = T.compose(T.read_work(path, ASK), 60)
    blob = json.dumps(out)
    for secret in (SECRET, CMD, "test_secret_thing", ".env", "rm -rf", "FAILED"):
        assert secret not in blob, secret


# ---- grok and agy ------------------------------------------------------------------

def test_grok_counts_tool_calls_and_says_failures_are_not_readable(home):
    def call(i, name, **args):
        return {"id": i, "name": name, "arguments": json.dumps(args)}
    recs = [
        {"type": "user", "content": "earlier"},
        {"type": "assistant", "content": "", "tool_calls": [call("c0", "run_terminal_command", command="ls")]},
        {"type": "user", "content": FRAME},
        {"type": "assistant", "content": "", "tool_calls": [call("c1", "read_file", target_file=SECRET),
                                                             call("c2", "run_terminal_command", command=CMD)]},
        {"type": "tool_result", "tool_call_id": "c2", "content": "error: exit 1"},
        {"type": "assistant", "content": "", "tool_calls": [call("c3", "search_replace", file_path=SECRET)]},
        {"type": "assistant", "content": "", "tool_calls": [
            call("c4", "run_terminal_command", command=f"$GHOSTHERD_HERD reply {ASK} 'done'")]},
        {"type": "assistant", "content": "", "tool_calls": [call("c5", "run_terminal_command", command="ls")]},
    ]
    path = _write(home / ".grok" / "sessions" / "-src-dp" / "g1" / "chat_history.jsonl", recs)
    w = T.read_work(path, ASK)
    assert w.seen and w.cli == "grok" and not w.failures_known
    assert dict(w.tools) == {"read_file": 1, "run_terminal_command": 1, "search_replace": 1}
    assert T.compose(w, 300)["line"] == "How it went: 5 min · 3 tool calls (failures not readable for grok)"


def test_agy_counts_tool_calls_and_says_failures_are_not_readable(home):
    def plan(i, *calls):
        return {"source": "MODEL", "type": "PLANNER_RESPONSE", "created_at": f"t{i}", "step_index": i,
                "tool_calls": [{"name": n, "args": a} for n, a in calls]}
    recs = [
        {"source": "USER_EXPLICIT", "type": "USER_INPUT", "created_at": "t0", "step_index": 0,
         "content": "<USER_REQUEST>" + FRAME + "</USER_REQUEST>"},
        plan(1, ("view_file", {"AbsolutePath": json.dumps(SECRET)}), ("run_command", {"CommandLine": json.dumps(CMD)})),
        {"source": "MODEL", "type": "GENERIC", "created_at": "t2", "step_index": 2, "content": "tool output"},
        plan(3, ("replace_file_content", {"TargetFile": json.dumps(SECRET)})),
        plan(4, ("run_command", {"CommandLine": json.dumps(f'"$GHOSTHERD_HERD" reply {ASK} "done"')})),
        plan(5, ("run_command", {"CommandLine": json.dumps("ls")})),
    ]
    path = _write(home / ".gemini" / "antigravity-cli" / "brain" / "b1" / ".system_generated" / "logs" / "transcript.jsonl", recs)
    w = T.read_work(path, ASK)
    assert w.seen and w.cli == "agy" and not w.failures_known
    assert dict(w.tools) == {"view_file": 1, "run_command": 1, "replace_file_content": 1}
    assert "failures not readable for agy" in T.compose(w, 90, guard=1)["line"]


# ---- Left out, or saying what it could not see -------------------------------------


def test_what_it_cannot_see_it_says_and_counts_nothing(home, tmp_path):
    cases = {
        "no conversation known for it": None,
        "its conversation could not be read": str(tmp_path / "elsewhere.jsonl"),
    }
    (tmp_path / "elsewhere.jsonl").write_text(json.dumps(c_user(FRAME, 1)) + "\n")   # outside the CLIs' roots
    for why, path in cases.items():
        w = T.read_work(path, ASK)
        assert (w.seen, w.why, dict(w.tools)) == (False, why, {})
    other = _write(home / ".claude" / "projects" / "-x" / "s.jsonl", [c_user("hello", 1), c_use("a", "Bash", 2, command="ls")])
    w = T.read_work(other, ASK)
    assert (w.seen, w.why) == (False, "its conversation does not show this ask")
    # Not seen, and nothing else to say: no line at all, rather than one saying so.
    assert T.compose(w, 120)["line"] is None
    # Not seen, but the herd's records have something: the line says what it could not see.
    assert T.compose(w, 120, guard=1)["line"] == (
        "How it went: 2 min · tool calls not seen: its conversation does not show this ask · 1 guard denial")


def test_an_echo_of_the_frame_in_a_tool_call_is_not_the_delivery(home):
    recs = [c_use("e", "Bash", 0, command=f"echo '[ask {ASK} -- x]'"), c_use("f", "Bash", 0.5, command="ls")]
    path = _write(home / ".claude" / "projects" / "-x" / "s.jsonl", recs)
    assert T.read_work(path, ASK).why == "its conversation does not show this ask"


def test_a_delivery_far_back_is_found(home, monkeypatch):
    monkeypatch.setattr(T, "FIRST_WINDOW", 4096)
    filler = [c_use(f"f{i}", "Read", 2 + i / 1000, file_path="/x" * 50) for i in range(300)]
    path = _write(home / ".claude" / "projects" / "-x" / "s.jsonl",
                  [c_user("x" * 5000, 0), c_user(FRAME, 1)] + filler +
                  [c_use("r", "Bash", 9, command=f"herd reply {ASK} done")])
    w = T.read_work(path, ASK)
    assert w.seen and w.tools["Read"] == 300


def test_past_its_budget_it_says_so(home):
    path = claude_transcript(home)
    w = T.read_work(path, ASK, seconds=0)
    assert (w.seen, w.why) == (False, "it took too long to read")


# ---- Smooth and rough ------------------------------------------------------------------


def _work(calls=10, **failed):
    w = T.Work()
    w.seen, w.cli, w.failures_known = True, "claude", True
    w.tools["Bash"] = calls
    w.failed.update(failed)
    return w


def test_a_smooth_ask_gets_no_nudge():
    out = T.compose(_work(12, shell=1), 240, asks={"qa-dp": 1})
    assert out["line"] == "How it went: 4 min · 12 tool calls, 1 failed (1 shell exit) · asked others 1× (qa-dp)"
    assert not out["rough"] and T.NUDGE not in out["line"]
    assert T.compose(_work(5), 40)["line"] == "How it went: 40 s · 5 tool calls, none failed"


@pytest.mark.parametrize("kw,rough", [
    ({"failed": {"shell": 3}}, False), ({"failed": {"shell": 4}}, True),
    ({"failed": {"edit": 1}}, False), ({"failed": {"edit": 2}}, True),
    ({"failed": {"denied": 1}}, False), ({"failed": {"denied": 1}, "guard": 1}, True), ({"lint": 2}, True),
    ({"asks": {"qa-dp": 2}}, False), ({"asks": {"qa-dp": 3}}, True), ({"asks": {"a": 2, "b": 2}}, False),
    ({"relays": 1}, True),
])
def test_the_nudge_threshold(kw, rough):
    w = _work(20, **kw.pop("failed", {}))
    out = T.compose(w, 600, **kw)
    assert out["rough"] is rough
    assert (T.NUDGE in out["line"]) is rough


def test_durations():
    assert [T.duration(s) for s in (5, 59, 60, 3599, 3600, 7500)] == ["5 s", "59 s", "1 min", "59 min", "1 h 0 min", "2 h 5 min"]


# ---- On the herd: made after the reply, never in its way -------------------------------

# A claude taker, so the transcript it reports is its own.
TSNAP = SNAP + [{"name": "claude-t", "kind": "claude", "state": "idle", "project": "/src/a/"}]


def _store(tmp_path):
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=TSNAP)
    return h


def _deliver_t(h):
    out = h.tick(sessions=TSNAP)
    h.tick(sessions=TSNAP, ack_ids=[p["id"] for p in out["pending"]])
    return out["pending"]


def _answer_mail(h, to="claude-main"):
    return [p for p in h.tick(sessions=TSNAP)["pending"] if p["to"] == to]


def _taken(h, home, ask_id, *work, name="claude-t"):
    "The taker's transcript: the ask's delivery, WORK, then its reply."
    frame = f"[ghostherd message from claude-main → {name}]\n[ask {ask_id} -- whoever asked]\nfix the build"
    path = _write(home / ".claude" / "projects" / "-src-a" / f"{ask_id}.jsonl",
                  [c_user(frame, 1), *work, c_use("r", "Bash", 9, command=f"herd reply {ask_id} done")])
    h.report(name, cli="claude", conversation=ask_id, transcript=path)
    return path


def test_a_reply_gets_its_line_on_the_asks_answer(home, tmp_path):
    h = _store(tmp_path)
    ask = h.ask("claude-t", "fix the build", from_name="claude-main")
    _deliver_t(h)
    _taken(h, home, ask["id"], c_use("a", "Bash", 2, command="make"), c_result("a", "Exit code 2", 2.1, error=True))
    # A check denied it once while it worked.
    with h._mu:
        h._conn.execute("INSERT INTO lint_denials (id, key, fact, session, cli, command, created_at)"
                        " VALUES ('d1', 'user', 'f', 'claude-t', 'claude', 'x', ?)",
                        (datetime.now(timezone.utc).isoformat(),))
        h._conn.commit()
    assert h.reply(ask["id"], "fixed", from_name="claude-t") == {"id": ask["id"], "status": "answered"}
    h.wait_traces()
    got = h.get_ask(ask["id"])
    assert got["trace"].startswith("How it went: ") and "1 tool call, 1 failed (1 shell exit)" in got["trace"]
    assert "1 check denial" in got["trace"]
    mail = _answer_mail(h)
    assert mail[0]["body"].endswith("fixed\n\n[" + got["trace"] + "]")
    # The room reads it apart from the answer.
    entry = conversation.user_entry(f"[ghostherd message from claude-t → claude-main]\n{mail[0]['body']}", None)
    assert (entry["text"], entry["trace"]) == ("fixed", got["trace"])
    # Threads shows it, and the counts behind it.
    with h._mu:
        rows = h._thread_rows("WHERE id = ?", (ask["id"],))
    answer = next(m for m in threads_mod.messages(rows) if m["kind"] == "answer")
    assert answer["trace"] == got["trace"] and answer["traceDetail"]["failed"] == {"shell": 1}


def test_the_answer_mail_waits_for_its_line_only_a_moment(home, tmp_path, monkeypatch):
    h = _store(tmp_path)
    ask = h.ask("claude-t", "x", from_name="claude-main")
    _deliver_t(h)
    _taken(h, home, ask["id"])
    gate = __import__("threading").Event()
    real = T.read_work
    monkeypatch.setattr(T, "read_work", lambda *a, **k: (gate.wait(5), real(*a, **k))[1])
    t = time.monotonic()
    h.reply(ask["id"], "answer", from_name="claude-t")
    assert time.monotonic() - t < 0.5                    # the reply never waits on it
    assert _answer_mail(h) == []                         # the mail is held for the line
    with h._mu:
        h._conn.execute("UPDATE mail SET memory_until = '2000-01-01T00:00:00+00:00' WHERE trace_of = ?", (ask["id"],))
        h._conn.commit()
    assert _answer_mail(h)[0]["body"].endswith("answer")  # past the hold: goes without
    gate.set()
    h.wait_traces()


def test_a_broken_trace_never_fails_or_holds_a_reply(home, tmp_path, monkeypatch):
    h = _store(tmp_path)
    ask = h.ask("claude-t", "x", from_name="claude-main")
    _deliver_t(h)
    _taken(h, home, ask["id"])
    monkeypatch.setattr(T, "read_work", lambda *a, **k: 1 / 0)
    assert h.reply(ask["id"], "answer", from_name="claude-t")["status"] == "answered"
    h.wait_traces()
    mail = _answer_mail(h)
    assert mail and mail[0]["body"].endswith("answer")    # released at once, without a line
    assert h.get_ask(ask["id"])["trace"] is None


def test_a_reply_stands_when_the_herds_records_cannot_be_read(tmp_path, monkeypatch):
    h = _store(tmp_path)
    ask = h.ask("claude-t", "x", from_name="claude-main")
    _deliver_t(h)
    monkeypatch.setattr(HerdStore, "_trace_facts", lambda *a: 1 / 0)
    assert h.reply(ask["id"], "answer", from_name="claude-t")["status"] == "answered"
    assert _answer_mail(h)[0]["body"].endswith("answer")  # not held: nothing is coming
    assert h.get_ask(ask["id"])["trace"] is None


def test_with_no_transcript_the_herds_records_alone_and_no_wait(tmp_path):
    h = _store(tmp_path)
    parent = h.ask("claude-t", "x", from_name="claude-main")
    _deliver_t(h)
    for _ in range(3):                                   # three asks to one agent while it works
        k = h.ask("agy-a2", "check it", from_name="claude-t")
        _deliver_t(h)
        h.reply(k["id"], "ok", from_name="agy-a2")
    h.reply(parent["id"], "done", from_name="claude-t")
    got = h.get_ask(parent["id"])
    assert got["trace"] == ("How it went: 0 s · tool calls not seen: no conversation known for it · "
                            "asked others 3× (agy-a2 ×3) · " + T.NUDGE)
    assert [m["body"] for m in _answer_mail(h)][-1].endswith("[" + got["trace"] + "]")


def test_an_ask_the_herd_closed_on_its_takers_last_words_gets_one_too(home, tmp_path):
    h = _store(tmp_path)
    ask = h.ask("claude-t", "x", from_name="claude-main")
    _deliver_t(h)
    _taken(h, home, ask["id"], c_use("a", "Read", 2, file_path="/x"))
    h.report("claude-t", state="working")
    assert h.report("claude-t", state="done", last="Done, I think.")["closed_ask"] == ask["id"]
    h.wait_traces()
    assert "1 tool call, none failed" in h.get_ask(ask["id"])["trace"]


def test_herd_ask_wait_prints_the_line_apart_from_the_answer(live, home, tmp_path):
    h = herd_mod.get_herd()
    h.tick(sessions=TSNAP)
    out = subprocess.Popen([str(HERD), "ask", "claude-t", "fix it", "--wait"], stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE, text=True, env=dict(live["env"], HOME=str(home)))
    asks = []
    for _ in range(200):
        asks = [a for a in h.recent_asks()["asks"] if a["to"] == "claude-t"]
        if asks:
            break
        time.sleep(0.05)
    _deliver_t(h)
    k = asks[0]["id"]
    _taken(h, home, k, c_use("a", "Bash", 2, command="make"))
    h.reply(k, "it is fixed", from_name="claude-t")
    stdout, stderr = out.communicate(timeout=30)
    assert out.returncode == 0, stderr
    assert stdout == "it is fixed\n"                     # the answer alone
    assert "herd: How it went: " in stderr and "1 tool call, none failed" in stderr
    said = conversation.ask_outcome(stderr + stdout)
    assert said["answer"] == "it is fixed" and said["trace"].startswith("How it went: ")
