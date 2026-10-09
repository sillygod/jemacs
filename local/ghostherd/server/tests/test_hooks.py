"""Hooks: what claude's, grok's and agy's lifecycle hooks tell the herd."""

import importlib.machinery
import importlib.util
import json
import os
import subprocess
import sys
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
    h.settle(ask["id"], screen="the screen")
    got = h.get_ask(ask["id"])
    assert (got["reply"], got["auto_source"]) == ("the screen", "screen")
    two = h.ask("agy-a2", "y", from_name="claude-main", wait=True)
    _deliver(h)
    h.report("agy-a2", last="the answer to y")  # no state: no close here
    assert h.get_ask(two["id"])["status"] == "delivered"
    h.settle(two["id"], screen="the screen")
    got = h.get_ask(two["id"])
    assert (got["reply"], got["auto_source"]) == ("the answer to y", "message")


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
    path = _jsonl(tmp_path / "transcript.jsonl", [
        _step("USER_EXPLICIT", "USER_INPUT", "commit it"),
        _step("MODEL", "PLANNER_RESPONSE", "Committed abc1234.")])
    out = hook(live, dict(_agy(path), terminationReason="model_stop"), "Stop", session="agy-a2", tmp=tmp_path)
    assert (out.returncode, out.stdout.strip()) == (0, "{}"), out.stderr
    got = h.get_ask(ask["id"])
    assert (got["status"], got["reply"], got["auto_source"]) == ("answered", "Committed abc1234.", "message")


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
