"""The room: an agent's conversation, read from the tail of its transcript."""

import asyncio
import json
import os
from pathlib import Path

import pytest

import conversation as conv
from jsonrpc_handler import JsonRpcRequest, handler

# As ghostherd.el frames them: `ghostherd-message-template' around
# `ghostherd--ask-text', and herd.py's `_mail_back'.
ASK_TEXT = (
    "[ghostherd message from rd → qa]\n"
    "[ask 1a2b3c4d -- whoever asked is waiting for your answer]\n"
    "Test the login flow.\nBoth browsers.\n\n"
    "When you are done, send your answer with:\n"
    "'/x/bin/herd' reply 1a2b3c4d <<'HERD'\n<your answer>\nHERD\n"
    "Answer even if you could not do it, and say why.  Do not ask other\n"
    "agents while you work on this; the herd refuses that.\n"
)
ANSWER_TEXT = "[ghostherd message from qa → rd]\nAnswer to ask 1a2b3c4d (Test the login flow.):\nTwo failures: (a) and (b).\n"
AUTO_TEXT = (
    "[ghostherd message from qa → rd]\nAnswer to ask 1a2b3c4d (Test it) -- no answer was sent; "
    "this is its last message:\nI ran out of time.\n"
)
FAILED_TEXT = "[ghostherd message from qa → rd]\nAsk 1a2b3c4d failed: the taker died\n"


@pytest.fixture
def home(tmp_path, monkeypatch):
    monkeypatch.setenv("HOME", str(tmp_path))
    conv._cache.clear()
    yield tmp_path
    conv._cache.clear()


def _write(path: Path, records) -> str:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(json.dumps(r) + "\n" for r in records))
    return str(path)


def _claude_file(home, records):
    return _write(home / ".claude" / "projects" / "-x-proj" / "abc.jsonl", records)


def u(content, **kw):
    return {"type": "user", "message": {"role": "user", "content": content},
            "timestamp": "2026-10-09T01:00:00Z", **kw}


def a(mid, *blocks, **kw):
    return {"type": "assistant", "message": {"id": mid, "role": "assistant", "content": list(blocks)},
            "timestamp": "2026-10-09T01:00:05Z", **kw}


def text(t):
    return {"type": "text", "text": t}


def use(i, name, **inp):
    return {"type": "tool_use", "id": i, "name": name, "input": inp}


def result(i, content):
    return {"type": "tool_result", "tool_use_id": i, "content": content}


def test_claude_turns(home):
    path = _claude_file(home, [
        {"type": "permission-mode", "permissionMode": "default"},
        u("Fix the login bug<system-reminder>ignore me</system-reminder>"),
        u("hidden", isMeta=True),
        a("m1", text("Looking.")),
        a("m1", use("t1", "Bash", command="git status", description="Show status")),
        u([result("t1", "clean")]),
        a("m2", use("t2", "Read", file_path="/x/a.py")),
        a("m2", use("t3", "Edit", file_path="/x/a.py", old_string="a", new_string="b")),
        a("s1", text("from a subagent"), isSidechain=True),
        a("m3", text("Fixed it.")),
        a("m3", text("Tests pass.")),
        u("<command-name>/usage</command-name>\n<command-args></command-args>"),
        u("<local-command-stdout>42%</local-command-stdout>"),
        u("[Request interrupted by user]"),
        u("<task-notification>\n<task-id>b1</task-id>\n<status>completed</status>\n"
          "<summary>Background command \"tests\" completed</summary>\n</task-notification>"),
    ])
    r = conv.read(path)
    assert r["cli"] == "claude" and r["earlier"] is False
    got = [(e["role"], e.get("kind"), e.get("text"), e.get("who")) for e in r["entries"] if e["role"] != "tools"]
    assert got == [
        ("user", "prompt", "Fix the login bug", ""),
        ("assistant", None, "Looking.", None),
        ("assistant", None, "Fixed it.\n\nTests pass.", None),
        ("user", "command", "/usage", ""),
        ("user", "note", "Request interrupted by user", ""),
        ("user", "note", 'Background command "tests" completed', ""),
    ]
    tools = [e for e in r["entries"] if e["role"] == "tools"]
    # One line for the tools between two things said, not one per call.
    assert [[t["name"] for t in e["tools"]] for e in tools] == [["Bash", "Read", "Edit"]]
    assert [t["hint"] for t in tools[0]["tools"]] == ["Show status", "/x/a.py", "/x/a.py"]
    assert all("key" not in e for e in r["entries"])


def test_what_the_herd_said_is_attributed(home):
    path = _claude_file(home, [u(ASK_TEXT), u(ANSWER_TEXT), u(AUTO_TEXT), u(FAILED_TEXT),
                               u("[ghostherd message from user → qa]\nplease hurry\n")])
    e = conv.read(path)["entries"]
    assert e[0] == {"role": "user", "who": "rd", "kind": "ask", "ask": "1a2b3c4d",
                    "text": "Test the login flow.\nBoth browsers.", "ts": "2026-10-09T01:00:00Z"}
    assert (e[1]["who"], e[1]["kind"], e[1]["ask"], e[1]["text"], e[1]["auto"]) == \
        ("qa", "answer", "1a2b3c4d", "Two failures: (a) and (b).", False)
    assert (e[2]["kind"], e[2]["text"], e[2]["auto"]) == ("answer", "I ran out of time.", True)
    assert (e[3]["kind"], e[3]["text"], e[3]["failed"]) == ("answer", "the taker died", True)
    assert (e[4]["who"], e[4]["kind"], e[4]["text"]) == ("user", "message", "please hurry")


@pytest.mark.parametrize("command,want", [
    ("\"$GHOSTHERD_HERD\" ask qa --wait <<'HERD'\nTest the login flow.\nBoth browsers.\nHERD",
     {"kind": "ask", "to": "qa", "text": "Test the login flow.\nBoth browsers."}),
    ("/Users/x/.emacs.d/local/ghostherd/bin/herd ask --wait agy \"commit it\"",
     {"kind": "ask", "to": "agy", "text": "commit it"}),
    ("${GHOSTHERD_HERD} ask --timeout 30 claude 'hi there' 2>&1 | tail -5",
     {"kind": "ask", "to": "claude", "text": "hi there"}),
    ("cd /x && herd reply 1a2b3c4d <<EOF\nAll green.\nEOF",
     {"kind": "reply", "ask": "1a2b3c4d", "text": "All green."}),
    ("\"$GHOSTHERD_HERD\" ask qa --wait - <<'EOF'\nRound 2.\nEOF",
     {"kind": "ask", "to": "qa", "text": "Round 2."}),
    ("got=$(herd ask qa --wait \"hi\")", {"kind": "ask", "to": "qa", "text": "hi"}),
    ("herd ask qa \"is a > b?\" >out.txt", {"kind": "ask", "to": "qa", "text": "is a > b?"}),
    # Piped in: the text is not on the line, and what follows `;' is not it.
    ("cat q.txt | herd ask qa; echo asked", {"kind": "ask", "to": "qa", "text": ""}),
])
def test_herd_calls_are_read_out_of_commands(command, want):
    got = conv.herd_call(command)
    assert {k: got[k] for k in want} == want and got["role"] == "herd"


@pytest.mark.parametrize("command", [
    "git log --grep herd", "shepherd ask x", "herd list", "herd ask --wait", "",
    # Asking the client how to ask is not an ask.
    "\"$GHOSTHERD_HERD\" ask --help 2>&1 | head -30", "herd ask -h", "herd reply --help",
    "herd ask 2>&1", "herd ask | cat", "herd ask qa --help",
    "herd ask 2>&1 'unclosed",
])
def test_other_commands_are_not_herd_calls(command):
    assert conv.herd_call(command) is None


def test_claude_ask_gets_its_answer(home):
    cmd = "\"$GHOSTHERD_HERD\" ask qa --wait <<'HERD'\nTest it.\nHERD"
    path = _claude_file(home, [
        u("ship it"),
        a("m1", text("Asking QA."), use("t1", "Bash", command=cmd, description="Ask QA")),
        u([result("t1", "Two failures.\n")]),
        a("m2", use("t2", "Bash", command="herd reply 99887766 'ok'")),
        u([result("t2", "sent")]),
    ])
    e = conv.read(path)["entries"]
    assert [x["role"] for x in e] == ["user", "assistant", "herd", "herd"]
    assert (e[2]["kind"], e[2]["to"], e[2]["text"], e[2]["answer"]) == ("ask", "qa", "Test it.", "Two failures.")
    # A reply's output is the client's receipt, not anyone's answer.
    assert (e[3]["kind"], e[3]["ask"], "answer" in e[3]) == ("reply", "99887766", False)


def test_a_pasted_ask_is_still_the_herds(home):
    # Claude Code keeps a paste wrapped in its transcript, and the herd pastes.
    pasted = '\n\n<pasted_content id="df18">\n' + ASK_TEXT + '</pasted_content id="df18">\n'
    path = _claude_file(home, [u(pasted), u('<pasted_content id="a1">\nmy notes\n</pasted_content>')])
    e = conv.read(path)["entries"]
    assert (e[0]["who"], e[0]["kind"], e[0]["ask"], e[0]["text"]) == \
        ("rd", "ask", "1a2b3c4d", "Test the login flow.\nBoth browsers.")
    assert (e[1]["kind"], e[1]["text"]) == ("prompt", "my notes")


OUT = "/private/tmp/claude-501/p/tasks/b0x.output"
BG = ("Command running in background with ID: b0x. Output is being written to: " + OUT +
      ". You will be notified when it completes. To check interim output, use Read.")


def test_an_ask_in_the_background_is_answered_from_its_output(home):
    ask = "\"$GHOSTHERD_HERD\" ask qa --wait <<'EOF'\nTest round 2.\nEOF"
    records = [
        a("m1", use("t1", "Bash", command=ask, run_in_background=True)),
        u([result("t1", BG)]),
    ]
    entry = lambda: next(x for x in conv.read(_claude_file(home, records))["entries"] if x["role"] == "herd")
    # Claude Code's word that it went to the background is no answer.
    assert entry()["waiting"] is True and "answer" not in entry()
    records += [a("m2", use("t2", "Bash", command="tail -3 " + OUT)),
                u([result("t2", "herd: asked qa (ask 26910956)\nherd: qa has it: working")])]
    assert (entry()["waiting"], entry()["ask"], "answer" in entry()) == (True, "26910956", False)
    records += [a("m3", use("t3", "Read", file_path=OUT)),
                u([result("t3", "     1→herd: asked qa (ask 26910956)\n     2→All green.\n     3→\n"
                                "     4→[exited with code 0]")])]
    e = entry()
    assert (e["answer"], e["ask"], "waiting" in e) == ("All green.", "26910956", False)
    tools = [x for x in conv.read(_claude_file(home, records))["entries"] if x["role"] == "tools"]
    assert [t["name"] for x in tools for t in x["tools"]] == ["Bash", "Read"]
    # Read by a command that goes on printing after the file: the answer
    # is what came before Claude Code's end of it.
    records[-2:] = [a("m3", use("t3", "Bash", command="f=%s; cat \"$f\"; echo ---; herd status 26910956" % OUT)),
                    u([result("t3", "herd: asked qa (ask 26910956)\nherd: qa has it: working  input sent\nAll green.\n\n"
                                    "[exited with code 0]\n---\n26910956 answered auto= False")])]
    e = entry()
    assert (e["answer"], "waiting" in e) == ("All green.", False)


def test_what_became_of_an_ask(home):
    def ran(i, cmd, out, error=False, bg=False):
        return [a("m" + i, use(i, "Bash", command=cmd, run_in_background=bg)),
                u([dict(result(i, out), is_error=error)])]
    records = (
        ran("t1", "\"$GHOSTHERD_HERD\" ask qa --wait - <<'EOF'\nTest it.\nEOF", BG, bg=True)
        + ran("t2", "cat " + OUT, "usage: herd [-h] {ask,reply} ...\nherd: error: unrecognized arguments: -\n\n"
                                  "[exited with code 2]")
        + ran("t3", "herd ask nope 'x'", "Exit code 1\nherd: no agent or kind named nope", error=True)
        + ran("t4", "herd ask qa --wait --timeout 5 'slow'",
              "Exit code 3\nherd: asked qa (ask 0badcafe)\nherd: ask 0badcafe still open after 5s; the answer "
              "will come to you as a message (or: herd status 0badcafe)", error=True)
        + ran("t5", "herd ask qa --wait 'quiet'",
              "herd: asked qa (ask 1234abcd)\nherd: qa stopped without answering; this is its screen\n> idle")
        + ran("t6", "herd ask qa 'later'", "The user doesn't want to proceed with this tool use.", error=True)
    )
    asks = [x for x in conv.read(_claude_file(home, records))["entries"] if x["role"] == "herd"]
    assert [(x.get("failed"), x.get("waiting"), x.get("answer"), x.get("auto")) for x in asks] == [
        (True, None, "error: unrecognized arguments: -", None),
        (True, None, "no agent or kind named nope", None),
        (None, True, None, None),  # the answer comes as a message
        (None, None, "> idle", True),
        (True, None, "The user doesn't want to proceed with this tool use.", None),
    ]
    assert asks[2]["ask"] == "0badcafe"


def test_agy(home):
    path = _write(home / ".gemini" / "antigravity-cli" / "brain" / "c1" / ".system_generated" / "logs" / "transcript.jsonl", [
        {"source": "USER_EXPLICIT", "type": "USER_INPUT", "created_at": "t0", "step_index": 0,
         "content": "<USER_REQUEST>\ncommit the fix\n</USER_REQUEST>\n<ADDITIONAL_METADATA>cursor at x</ADDITIONAL_METADATA>"},
        {"source": "MODEL", "type": "PLANNER_RESPONSE", "created_at": "t1", "step_index": 1, "thinking": "hmm",
         "tool_calls": [{"name": "run_command", "args": {"CommandLine": "git status", "toolSummary": "Checking the tree"}},
                        {"name": "view_file", "args": {"AbsolutePath": "/x/a.py"}}]},
        {"source": "MODEL", "type": "GENERIC", "created_at": "t2", "step_index": 2, "content": "tool output"},
        {"source": "MODEL", "type": "PLANNER_RESPONSE", "created_at": "t3", "step_index": 3,
         "tool_calls": [{"name": "run_command", "args": {"CommandLine": "herd reply 1a2b3c4d 'committed abc'"}}]},
        {"source": "MODEL", "type": "PLANNER_RESPONSE", "created_at": "t4", "step_index": 4, "content": "Committed."},
        {"source": "SYSTEM", "type": "SYSTEM_MESSAGE", "created_at": "t5", "step_index": 5, "content": "sys"},
    ])
    r = conv.read(path)
    assert r["cli"] == "agy"
    e = r["entries"]
    assert [(x["role"], x.get("text")) for x in e] == [
        ("user", "commit the fix"), ("tools", None), ("herd", "committed abc"), ("assistant", "Committed.")]
    assert [t["hint"] for t in e[1]["tools"]] == ["Checking the tree", "/x/a.py"]
    assert e[0]["ts"] == "t0"


def test_grok(home):
    path = _write(home / ".grok" / "sessions" / "%2Fx%2Fproj" / "s1" / "chat_history.jsonl", [
        {"type": "system", "content": "you are grok"},
        {"type": "user", "content": [{"type": "text", "text": "<user_query>run the tests</user_query>"}], "prompt_index": 0},
        {"type": "user", "content": [{"type": "text", "text": "summary of before"}], "synthetic_reason": "compaction_meta"},
        {"type": "reasoning", "summary": [], "encrypted_content": "x"},
        {"type": "assistant", "content": "On it.", "tool_calls": [
            {"id": "c1", "name": "run_terminal_command", "arguments": json.dumps({"command": "herd ask qa 'check'", "description": "ask"})},
            {"id": "c2", "name": "read_file", "arguments": json.dumps({"target_file": "/x/a.py"})}]},
        {"type": "tool_result", "tool_call_id": "c1", "content": "Looks good."},
        {"type": "tool_result", "tool_call_id": "c2", "content": "file text"},
        {"type": "assistant", "content": "Done."},
    ])
    e = conv.read(path)["entries"]
    assert [(x["role"], x.get("text")) for x in e] == [
        ("user", "run the tests"), ("assistant", "On it."), ("herd", "check"), ("tools", None), ("assistant", "Done.")]
    assert e[2]["answer"] == "Looks good." and e[3]["tools"] == [{"name": "read_file", "hint": "/x/a.py"}]


def test_only_transcripts_are_read(home, tmp_path):
    secret = home / ".ssh" / "id.jsonl"
    secret.parent.mkdir()
    secret.write_text('{"type":"user"}\n')
    inside = home / ".claude" / "projects" / "p"
    inside.mkdir(parents=True)
    (inside / "link.jsonl").symlink_to(secret)
    (inside / "notes.txt").write_text("x")
    for path in (str(secret), str(inside / "link.jsonl"), str(inside / "notes.txt"),
                 str(inside / "missing.jsonl"), str(inside / ".." / ".." / ".." / ".ssh" / "id.jsonl"), ""):
        with pytest.raises(ValueError):
            conv.read(path)


def test_long_transcripts_are_read_from_the_tail(home, monkeypatch):
    monkeypatch.setattr(conv, "TAIL_BYTES", 2000)
    monkeypatch.setattr(conv, "TAIL_MAX", 2000)
    path = _claude_file(home, [u("prompt %d %s" % (i, "x" * 50)) for i in range(100)])
    r = conv.read(path, limit=5)
    assert r["earlier"] is True
    assert [e["text"].split()[1] for e in r["entries"]] == ["95", "96", "97", "98", "99"]
    # The tail begins mid-line: that line is dropped, not misread.
    assert all(e["text"].startswith("prompt ") for e in conv.read(path, limit=200)["entries"])
    conv._cache.clear()
    monkeypatch.setattr(conv, "TAIL_MAX", 10 ** 6)
    assert len(conv.read(path, limit=200)["entries"]) == 100


def test_tool_output_does_not_crowd_the_turns_out(home, monkeypatch):
    "A turn's tool output can fill the window: read further back for the turns."
    monkeypatch.setattr(conv, "TAIL_BYTES", 4000)
    records = []
    for i in range(6):
        records += [u("turn %d" % i), a("m%d" % i, use("t%d" % i, "Bash", command="cat big")),
                    u([result("t%d" % i, "y" * 3000)]), a("n%d" % i, text("done %d" % i))]
    path = _claude_file(home, records)
    said = [e["text"] for e in conv.read(path, limit=8)["entries"] if e["role"] != "tools"]
    assert said == ["done 3", "turn 4", "done 4", "turn 5", "done 5"]
    conv._cache.clear()
    monkeypatch.setattr(conv, "TAIL_MAX", 4000)
    assert len(conv.read(path, limit=8)["entries"]) < 8


def test_unchanged_transcript_is_not_read_again(home, monkeypatch):
    path = _claude_file(home, [u("one")])
    first = conv.read(path)
    reads = []
    monkeypatch.setattr(conv, "_tail", lambda p, n: reads.append(p) or ([], False))
    assert conv.read(path) is first and reads == []
    with open(path, "a") as f:
        f.write(json.dumps(u("two")) + "\n")
    os.utime(path, ns=(1, 2))
    conv.read(path)
    assert len(reads) == 1


def test_rpc(home):
    path = _claude_file(home, [u("hello"), a("m1", text("hi"))])

    def call(params):
        return asyncio.run(handler.handle(JsonRpcRequest(jsonrpc="2.0", id=1, method="herd_conversation", params=params)))

    r = call({"path": path, "limit": 1})
    assert r.error is None and [e["text"] for e in r.result["entries"]] == ["hi"]
    assert r.result["earlier"] is True
    assert call({"path": str(home / "x.jsonl")}).error.message == "no such transcript"
    assert call({"path": path, "limit": "lots"}).error.message == "limit must be a number"


# ---- A project's chat ------------------------------------------------------

def at(minute):
    return "2026-10-09T01:%02d:00Z" % minute


def _agent(home, name, records):
    return _write(home / ".claude" / "projects" / "-x-proj" / (name + ".jsonl"), records)


def _view(r):
    return [(e.get("who"), e.get("to"), e["kind"], e.get("agent")) for e in r["entries"]]


def test_chat_is_the_projects_conversations_as_one(home):
    ask = "\"$GHOSTHERD_HERD\" ask qa --wait <<'HERD'\nTest the login flow.\nBoth browsers.\nHERD"
    rd = _agent(home, "rd", [
        u("build it", timestamp=at(0)),
        a("m1", text("Built."), use("t1", "Bash", command=ask), timestamp=at(1)),
        u([result("t1", "herd: asked qa (ask 1a2b3c4d)\nTwo failures.")], timestamp=at(4)),
        a("m2", text("Fixing both."), timestamp=at(5)),
        # The answer mailed as well: the taker's reply already says it.
        u(ANSWER_TEXT, timestamp=at(6)),
    ])
    qa = _agent(home, "qa", [
        u(ASK_TEXT, timestamp=at(2)),
        a("q1", use("q1t", "Bash", command="herd reply 1a2b3c4d <<'H'\nTwo failures.\nH"), timestamp=at(3)),
        a("q2", text("Replied."), timestamp=at(3)),
    ])
    r = conv.chat([("rd", rd), ("qa", qa)])
    assert _view(r) == [
        ("user", "rd", "prompt", "rd"),
        ("rd", "qa", "ask", "qa"),      # as its taker received it
        ("qa", "rd", "reply", "qa"),
        ("qa", None, "turn", "qa"),
        ("rd", None, "turn", "rd"),     # how its turn ended, not what it said on the way
    ]
    assert [e.get("text") for e in r["entries"]] == [
        "build it", "Test the login flow.\nBoth browsers.", "Two failures.", "Replied.", "Fixing both."]
    assert r["entries"][1]["ask"] == "1a2b3c4d" and r["missing"] == []


def test_chat_shows_what_the_taker_never_saw(home):
    rd = _agent(home, "rd", [
        u("go", timestamp=at(0)),
        a("m1", use("t1", "Bash", command="herd ask qa --wait - <<'EOF'\nfirst try\nEOF"), timestamp=at(1)),
        u([dict(result("t1", "Exit code 2\nusage: herd ...\nherd: error: unrecognized arguments: -"), is_error=True)],
          timestamp=at(1)),
        a("m2", use("t2", "Bash", command="herd ask qa --wait 'second try'"), timestamp=at(2)),
        u([result("t2", "herd: asked qa (ask 99887766)\nherd: qa stopped without answering; this is its screen\n> idle")],
          timestamp=at(9)),
    ])
    qa = _agent(home, "qa", [
        u(ASK_TEXT.replace("1a2b3c4d", "99887766").replace("Test the login flow.\nBoth browsers.", "second try"),
          timestamp=at(3)),
        a("q1", text("Looking."), timestamp=at(4)),
    ])
    ops = _write(home / ".grok" / "sessions" / "%2Fx%2Fproj" / "s1" / "chat_history.jsonl", [
        {"type": "user", "content": [{"type": "text", "text": "<user_query>watch the deploy</user_query>"}]},
        {"type": "assistant", "content": "Watching."},
    ])
    # grok writes no times: its stream ends when its file was last written.
    os.utime(ops, (conv._epoch(at(5)), conv._epoch(at(5))))
    r = conv.chat([("rd", rd), ("qa", qa), ("ops", ops), ("gone", str(home / "nowhere.jsonl"))])
    view = [(e.get("who"), e.get("to"), e["kind"], e.get("text")) for e in r["entries"]]
    assert view == [
        ("user", "rd", "prompt", "go"),
        ("rd", "qa", "ask", "first try"),       # failed on the way: only rd saw it
        ("rd", "qa", "ask", "second try"),
        ("qa", None, "turn", "Looking."),
        ("user", "ops", "prompt", "watch the deploy"),
        ("ops", None, "turn", "Watching."),
        ("qa", "rd", "answer", "> idle"),       # the herd's answer for qa, when rd got it
    ]
    assert r["entries"][1]["failed"] is True and "unrecognized arguments" in r["entries"][1]["answer"]
    assert r["entries"][-1]["auto"] is True and r["entries"][-1]["ask"] == "99887766"
    assert r["missing"] == ["gone"]


def test_chat_keeps_the_last_of_a_long_day(home):
    rd = _agent(home, "rd", [u("p%d" % n, timestamp=at(n)) for n in range(30)])
    r = conv.chat([("rd", rd)], limit=5)
    assert [e["text"] for e in r["entries"]] == ["p25", "p26", "p27", "p28", "p29"]
    assert r["earlier"] is True


def test_chat_rpc(home):
    rd = _agent(home, "rd", [u("hello", timestamp=at(0))])

    def call(params):
        return asyncio.run(handler.handle(JsonRpcRequest(jsonrpc="2.0", id=1, method="herd_chat", params=params)))

    r = call({"agents": [{"name": "rd", "path": rd}, {"name": "x", "path": "/etc/passwd"}]})
    assert r.error is None and r.result["entries"][0]["text"] == "hello" and r.result["missing"] == ["x"]
    assert call({"agents": "rd"}).error.message == "agents must be a list"
    assert call({"agents": [{"name": "rd"}]}).error.message == "each agent is {name, path}"
    assert call({"agents": [], "limit": "lots"}).error.message == "limit must be a number"


# ---- What ghostherd writes, read back -------------------------------------
#
# The room tells the herd's traffic from yours by its framing, which
# ghostherd.el writes and herd.py mails.  Read the framing from those
# sources, so a change to it fails here, not silently in the room.

GHOSTHERD_EL = Path(__file__).resolve().parents[2] / "ghostherd.el"


def _elisp_string(source: str, *after: str) -> str:
    "The first string literal in SOURCE after each of AFTER in turn, as Emacs reads it."
    at = 0
    for a in after:
        at = source.index(a, at)
    start = source.index('"', at) + 1
    out, i = [], start
    while source[i] != '"':
        c = source[i]
        if c == "\\":
            nxt = source[i + 1]
            i += 2
            if nxt != "\n":  # a backslash-newline is nothing
                out.append({"n": "\n", "t": "\t"}.get(nxt, nxt))
            continue
        out.append(c)
        i += 1
    return "".join(out)


def test_the_room_reads_what_ghostherd_writes(tmp_path):
    from tests.test_ask import SNAP, _deliver, _mail_to, _store

    el = GHOSTHERD_EL.read_text()
    framed = _elisp_string(el, "(defcustom ghostherd-message-template")
    ask_text = _elisp_string(el, "(defun ghostherd--ask-text", "(format ")
    message = lambda frm, to, body: framed % (frm, to, body)  # noqa: E731

    h = _store(tmp_path)
    body = "fetch the docs\nand more"
    ask = h.ask("agy-a2", body, from_name="claude-main")
    _deliver(h)
    pasted = message("claude-main", "agy-a2", ask_text % (ask["id"], body, "'/x/bin/herd'", ask["id"]))
    e = conv.user_entry(pasted, None)
    assert (e["kind"], e["who"], e["ask"], e["text"]) == ("ask", "claude-main", ask["id"], body)

    h.reply(ask["id"], "X\n(with parens): yes")
    mail = _mail_to(h, "claude-main")[0]
    e = conv.user_entry(message(mail["from"], "claude-main", mail["body"]), None)
    assert (e["kind"], e["who"], e["ask"], e["text"], e["auto"]) == ("answer", "agy-a2", ask["id"], "X\n(with parens): yes", False)
    h.tick(sessions=SNAP, ack_ids=[mail["id"]])

    two = h.ask("agy-a2", "again (twice)", from_name="claude-main")
    _deliver(h)
    h.settle(two["id"], screen="the screen")
    mail = _mail_to(h, "claude-main")[0]
    e = conv.user_entry(message(mail["from"], "claude-main", mail["body"]), None)
    assert (e["kind"], e["text"], e["auto"]) == ("answer", "the screen", True)
    h.tick(sessions=SNAP, ack_ids=[mail["id"]])

    three = h.ask("agy-a2", "once more", from_name="claude-main")
    _deliver(h)
    h.settle(three["id"], dead=True)
    mail = _mail_to(h, "claude-main")[0]
    e = conv.user_entry(message(mail["from"], "claude-main", mail["body"]), None)
    assert (e["kind"], e["failed"]) == ("answer", True) and "exited before answering" in e["text"]
