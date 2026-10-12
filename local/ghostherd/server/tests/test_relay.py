"""A relay: what an agent hands its heir, read from its transcript, the
herd's asks and the checkout."""

import asyncio
import json
import subprocess
from pathlib import Path

import pytest

import conversation as conv
import relay
from herd import HerdStore, reset_herd
from jsonrpc_handler import JsonRpcRequest, handler


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


def _claude(home, records, name="abc"):
    return _write(home / ".claude" / "projects" / "-x-proj" / f"{name}.jsonl", records)


def u(content, minute=0, **kw):
    return {"type": "user", "message": {"role": "user", "content": content},
            "timestamp": f"2026-10-09T01:{minute:02d}:00Z", **kw}


def a(mid, *blocks, minute=0):
    return {"type": "assistant", "message": {"id": mid, "role": "assistant", "content": list(blocks)},
            "timestamp": f"2026-10-09T01:{minute:02d}:30Z"}


def text(t):
    return {"type": "text", "text": t}


def use(i, name, **inp):
    return {"type": "tool_use", "id": i, "name": name, "input": inp}


def result(i, content):
    return {"type": "tool_result", "tool_use_id": i, "content": content}


def section(t, name):
    "Section NAME of brief T, without its heading."
    body = t.split(f"## {name}\n", 1)[1]
    return body.split("\n## ", 1)[0].split("\n\nGo on from where", 1)[0]


ASK = (
    "[ghostherd message from lead → rd]\n"
    "[ask 1a2b3c4d -- whoever asked is waiting for your answer]\n"
    "Add --separator to slugify, through qa.\n\n"
    "When you are done, send your answer with:\n"
    "'/x/bin/herd' reply 1a2b3c4d <<'HERD'\n<your answer>\nHERD\n"
)


def _work_of(home):
    return _claude(home, [
        {"type": "permission-mode", "permissionMode": "default"},
        u("Fix the login bug: a session cookie outlives its logout.", 0),
        a("m1", text("Looking at the cookie handling."), minute=1),
        a("m1", use("t1", "Read", file_path="/x/auth.py"), minute=1),
        u([result("t1", "...")], 1),
        a("m2", use("t2", "Edit", file_path="/x/auth.py"), minute=2),
        u([result("t2", "ok")], 2),
        u("Also keep the remember-me cookie.", 3),
        a("m3", use("t3", "Bash", command="pytest -q tests/test_auth.py", description="Run the auth tests"), minute=4),
        u([result("t3", "1 failed")], 4),
        a("m4", text("One test fails: remember-me is cleared with the session.  Next: keep it apart."), minute=5),
    ])


def test_a_brief_says_what_it_was_asked_and_where_it_got_to(home):
    path = _work_of(home)
    b = relay.brief("rd", path, None, "claude", "grok", 2, reason="claude's usage is used up")
    t = b["text"]
    assert t.startswith("[Relay: you take over from rd, a claude agent that stopped -- claude's usage is used up.")
    assert "You are rd now, the second of the name" in t
    assert "which is at ~/.claude/projects/-x-proj/abc.jsonl if you need more" in t
    work = section(t, "The work")
    # The first instruction, then the later one, each once.
    assert work.index("session cookie outlives") < work.index("remember-me cookie")
    assert work.count("Fix the login bug") == 1 and work.count("Also keep") == 1
    steps = section(t, "Where it got to")
    assert "It ran: Read /x/auth.py" in steps and "It ran: Bash Run the auth tests" in steps
    assert steps.rstrip().endswith("Next: keep it apart.")
    assert t.rstrip().endswith("Do not redo what is done.")
    assert b["transcript"].endswith("abc.jsonl") and b["taking"] == [] and b["waiting"] == []


def test_its_last_answer_is_kept_whole_its_other_steps_short(home):
    long = "word " * 500
    path = _claude(home, [u("Do it.", 0), a("m1", text("early " + long), minute=1), a("m2", text("last " + long), minute=2)])
    steps = section(relay.brief("rd", path, None, "claude", "agy", 2)["text"], "Where it got to")
    early, last = steps.split("- It said: ")[1:3]
    assert len(early) < relay.STEP_MAX + 20
    assert len(last) > 2000


def test_an_ask_it_was_given_is_part_of_the_work(home):
    path = _claude(home, [u(ASK, 0), a("m1", text("On it."), minute=1)])
    work = section(relay.brief("rd", path, None, "claude", "grok", 2)["text"], "The work")
    assert "ask 1a2b3c4d from lead:" in work and "Add --separator to slugify" in work
    assert "When you are done" not in work


def test_the_heir_of_an_heir_carries_the_work_not_the_brief(home):
    first = relay.brief("rd", _work_of(home), None, "claude", "grok", 2)["text"]
    # The second's conversation: the brief, then work of its own.
    path = _claude(home, [u(first, 10), a("m9", text("Keeping remember-me apart now."), minute=11),
                          u("And log the logout.", 12), a("m10", text("Logged."), minute=13)], name="def")
    t = relay.brief("rd", path, None, "grok", "agy", 3)["text"]
    assert "the third of the name" in t
    work = section(t, "The work")
    assert "Fix the login bug" in work and "Also keep the remember-me" in work and "And log the logout" in work
    assert "[Relay:" not in work and t.count("[Relay:") == 1
    steps = section(t, "Where it got to")
    assert "Relay" not in steps and "Logged." in steps


def test_agy_and_grok_transcripts(home):
    agy = _write(home / ".gemini" / "antigravity-cli" / "brain" / "c1" / ".system_generated" / "logs" / "transcript.jsonl", [
        {"source": "USER_EXPLICIT", "type": "USER_INPUT", "created_at": "2026-10-09T01:00:00Z", "step_index": 0,
         "content": "<USER_REQUEST>\ncommit the fix\n</USER_REQUEST>"},
        {"source": "MODEL", "type": "PLANNER_RESPONSE", "created_at": "2026-10-09T01:01:00Z", "step_index": 1,
         "tool_calls": [{"name": "run_command", "args": {"CommandLine": "git status", "toolSummary": "Checking the tree"}}]},
        {"source": "MODEL", "type": "PLANNER_RESPONSE", "created_at": "2026-10-09T01:02:00Z", "step_index": 2, "content": "Committed."},
    ])
    t = relay.brief("qa", agy, None, "agy", "claude", 2)["text"]
    assert "an agy agent" in t and "commit the fix" in t and "It ran: run_command Checking the tree" in t
    grok = _write(home / ".grok" / "sessions" / "%2Fx" / "s1" / "chat_history.jsonl", [
        {"type": "user", "content": [{"type": "text", "text": "<user_query>run the tests</user_query>"}]},
        {"type": "assistant", "content": "All green."},
    ])
    t = relay.brief("qa", grok, None, "grok", "claude", 2)["text"]
    assert "run the tests" in t and "It said: All green." in t


def test_what_it_owes_and_waits_on(home, tmp_path):
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    snap = [{"name": n, "kind": "claude", "state": "idle", "project": "/p/"} for n in ("lead", "rd", "qa")]
    h.tick(sessions=snap)
    given = h.ask("rd", "Add --separator, through qa.", from_name="lead")
    out = h.tick(sessions=snap)
    h.tick(sessions=snap, ack_ids=[p["id"] for p in out["pending"]])
    made = h.ask("qa", "Test --separator.", from_name="rd")
    asks = h.relay_asks("rd")
    assert [x["id"] for x in asks["taking"]] == [given["id"]] and asks["taking"][0]["from"] == "lead"
    assert [(x["id"], x["to"]) for x in asks["waiting"]] == [(made["id"], "qa")]
    b = relay.brief("rd", None, None, "claude", "grok", 2, notes="implementer", asks=asks,
                    client="/x/my bin/herd")
    t = b["text"]
    assert f"ask {given['id']} from lead waits for your answer" in t and "Add --separator, through qa." in t
    assert f"'/x/my bin/herd' reply {given['id']} <<'HERD'" in t
    assert f"your ask {made['id']} of qa, still open: Test --separator." in t
    assert b["taking"] == [given["id"]] and b["waiting"] == [made["id"]]
    # No transcript: it says so, and what the herd knows.
    assert "Its conversation could not be read: the herd does not know where its transcript is" in t
    assert "Its notes in the herd: implementer" in t
    assert h.relay_asks("qa")["taking"] == []


def _git(cwd, *args):
    subprocess.run(["git", "-C", str(cwd), *args], check=True, capture_output=True,
                   env={"GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@x", "GIT_COMMITTER_NAME": "t",
                        "GIT_COMMITTER_EMAIL": "t@x", "HOME": str(cwd), "PATH": "/usr/bin:/bin:/opt/homebrew/bin"})


def test_the_checkout(home, tmp_path):
    repo = tmp_path / "proj"
    repo.mkdir()
    _git(repo, "init", "-q", "-b", "fix/login")
    (repo / "auth.py").write_text("a = 1\n")
    _git(repo, "add", "auth.py")
    _git(repo, "commit", "-q", "-m", "Keep the session apart")
    (repo / "auth.py").write_text("a = 2\nb = 3\n")
    (repo / "notes.md").write_text("x\n")
    c = relay.checkout(str(repo))
    assert c.startswith("On fix/login.")
    assert " M auth.py" in c and "?? notes.md" in c
    assert "auth.py | 3 ++-" in c and "1 file changed" in c
    assert "Keep the session apart" in c.split("Latest commits:", 1)[1]
    t = relay.brief("rd", None, str(repo), "claude", "grok", 2)["text"]
    assert "## The checkout\nOn fix/login." in t
    assert relay.checkout(str(tmp_path / "nowhere")) is None
    plain = tmp_path / "plain"
    plain.mkdir()
    assert relay.checkout(str(plain)) is None
    assert "## The checkout" not in relay.brief("rd", None, str(plain), "claude", "grok", 2)["text"]


def test_a_long_conversation_fits(home):
    records = [u("Start.", 0)]
    for i in range(200):
        records.append(a(f"m{i}", text(f"step {i} " + "x" * 3000), minute=1))
    t = relay.brief("rd", _claude(home, records), None, "claude", "grok", 2)["text"]
    assert len(t) <= relay.BRIEF_MAX
    assert t.rstrip().endswith("Do not redo what is done.") and "Start." in t


def test_a_brief_too_long_drops_its_oldest_steps_first(home):
    records = []
    for i in range(7):
        records.append(u(f"instruction {i} " + "y" * 2000, i))
        records.append(a(f"m{i}", text(f"answer {i} " + "z" * 2000), minute=i))
    records.append(a("last", text("THE LAST ANSWER " + "w" * 2900), minute=30))
    t = relay.brief("rd", _claude(home, records), None, "claude", "grok", 2)["text"]
    assert len(t) <= relay.BRIEF_MAX
    steps = section(t, "Where it got to")
    assert "earlier steps" in steps.splitlines()[0] and "THE LAST ANSWER" in steps
    # Steps dropped, not the brief cut off: its last answer whole.
    assert "w" * 2900 in steps and "\n…\n\nGo on" not in t
    assert t.rstrip().endswith("Do not redo what is done.")


def test_a_brief_too_long_however_few_steps_is_cut(home, tmp_path):
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    asks = {"taking": [{"id": f"a{i:07d}", "from": "lead", "body": f"part {i} " + "q" * 3000} for i in range(12)]}
    t = relay.brief("rd", None, None, "claude", "grok", 2, asks=asks)["text"]
    assert len(t) <= relay.BRIEF_MAX
    assert t.rstrip().endswith("Do not redo what is done.") and "\n…\n" in t
    h.close()


def test_an_answer_on_its_way_is_waited_on(tmp_path):
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    snap = [{"name": n, "kind": "claude", "state": "idle", "project": "/p/"} for n in ("rd", "qa")]
    h.tick(sessions=snap)
    made = h.ask("qa", "Test it.", from_name="rd", wait=True)
    out = h.tick(sessions=snap)
    h.tick(sessions=snap, ack_ids=[p["id"] for p in out["pending"]])
    h.reply(made["id"], "Both pass.", from_name="qa")
    waiting = h.relay_asks("rd")["waiting"]
    assert [(x["id"], x["status"]) for x in waiting] == [(made["id"], "answered")]
    t = relay.brief("rd", None, None, "claude", "grok", 2, asks=h.relay_asks("rd"))["text"]
    assert f"your ask {made['id']} of qa, answered, its answer on its way to you: Test it." in t
    h.collect(made["id"], asker=True)
    assert h.relay_asks("rd")["waiting"] == []
    h.close()
    reset_herd()


def test_only_transcripts_are_read(home, tmp_path):
    secret = tmp_path / "secret.jsonl"
    secret.write_text(json.dumps(u("my password is hunter2")) + "\n")
    t = relay.brief("rd", str(secret), None, "claude", "grok", 2)["text"]
    assert "hunter2" not in t and "Its conversation could not be read: not a transcript" in t


def test_rpc(home, tmp_path, monkeypatch):
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    reset_herd()
    path = _work_of(home)

    def call(**params):
        return asyncio.run(handler.handle(JsonRpcRequest(jsonrpc="2.0", id=1, method="herd_relay_brief", params=params)))

    r = call(name="rd", path=path, project=str(tmp_path), **{"from": "claude", "to": "grok"}, generation=2,
             reason="used up")
    assert r.error is None and "You are rd now, the second of the name" in r.result["text"]
    assert call(name="-rf", **{"from": "claude", "to": "grok"}).error.message == "name is an agent's name"
    assert call(name="rd", **{"from": "Claude Code", "to": "grok"}).error.message == "from is a CLI's name"
    assert call(name="rd", **{"from": "claude", "to": "grok"}, generation=1).error.message == "generation is 2 to 999"
    assert call(name="rd", **{"from": "claude", "to": "grok"}, generation="II").error.message == "generation must be a number"
    reset_herd()


def test_the_heir_does_not_inherit_what_the_old_ones_hooks_said(tmp_path):
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    snap = [{"name": n, "kind": "claude", "state": "idle", "project": "/p/"} for n in ("lead", "rd")]
    h.tick(sessions=snap)
    given = h.ask("rd", "Fix it.", from_name="lead")
    out = h.tick(sessions=snap)
    h.tick(sessions=snap, ack_ids=[p["id"] for p in out["pending"]])
    h.report("rd", cli="claude", conversation="c1", transcript="/t/c1.jsonl", last="I was the first.")
    assert h.link("rd")["conversation"] == "c1"
    assert h.unlink("rd") == {"session": "rd", "forgot": True}
    assert h.link("rd") is None and h.unlink("rd")["forgot"] is False
    # Its inherited ask, settled before the heir's hooks say anything:
    # the screen, not the first one's last answer.
    h.settle(given["id"], screen="the heir's screen")
    assert h.get_ask(given["id"])["reply"] == "the heir's screen"
    with pytest.raises(ValueError):
        h.unlink(" ")
