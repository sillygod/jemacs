"""The herd's checks (bin/herd_lint.py; ask bd7efaa8): a shared-memory fact
made into a rule run before every shell call -- deny only, fail open,
active only once the user approves it, easy to turn off, each denial
counted.  The rules here are the three first proposals' own patterns."""

import json
import os
import shlex
import subprocess
import sys
import time
from pathlib import Path

import pytest

import herd as herd_mod
import lint
import shared_memory as sm
from herd import get_herd, reset_herd
from jsonrpc_handler import FORBIDDEN, JsonRpcHandler, JsonRpcRequest

BIN = Path(__file__).resolve().parents[2] / "bin"
HERD = BIN / "herd"
sys.path.insert(0, str(BIN))
import herd_lint as L  # noqa: E402
from tests.test_ask import live  # noqa: E402,F401  (the live sidecar fixture)

# ---- The first proposals' rules ------------------------------------------------
#
# As proposed (docs/checks/): what each catches, and lets through, is pinned
# here -- the examples the user reads on the page, and more.

CHECKS = BIN.parent / "docs" / "checks"


def _rule(fact):
    return json.loads((CHECKS / f"{fact}.json").read_text())


QDRANT = _rule("embedded-qdrant-open-is-a-write")
GREP = _rule("grep-cli-binaries-cheaply")
EMACS = _rule("never-probe-the-users-live-emacs")
RULES = {r["fact"]: r for r in (QDRANT, GREP, EMACS)}


def _cases():
    for fact, r in RULES.items():
        for c in r["catches"]:
            yield pytest.param(fact, c, True, id=f"{fact[:12]}-catch-{c[:30]}")
        for c in r["passes"]:
            yield pytest.param(fact, c, False, id=f"{fact[:12]}-pass-{c[:30]}")


@pytest.mark.parametrize("fact,command,denied", list(_cases()))
def test_each_rule_catches_what_it_says_and_passes_the_rest(fact, command, denied):
    assert L.Rule(RULES[fact]).matches(L.Views(command)) is denied


def test_the_first_rules_validate():
    for r in RULES.values():
        assert L.validate(dict(r))["fact"] == r["fact"]


@pytest.mark.parametrize("fact,command,denied", [
    # The live index opened by code piped to an interpreter, or by a QdrantLocal.
    ("embedded-qdrant-open-is-a-write",
     "python3 <<EOF\nfrom qdrant_client.local.qdrant_local import QdrantLocal\n"
     "QdrantLocal('/Users/me/.emacs.d/emacs-home/ghostherd-memory/qdrant')\nEOF", True),
    ("embedded-qdrant-open-is-a-write",
     "bash -c \"python3 -c 'from qdrant_client import QdrantClient; "
     "QdrantClient(path=\\\"/Users/me/.emacs.d/emacs-home/ghostherd-memory\\\")'\"", True),
    # An answer that quotes the bad call, as an argument: text, not code.
    ("embedded-qdrant-open-is-a-write",
     "herd reply 1 \"never QdrantClient(path='~/.emacs.d/emacs-home/ghostherd-memory')\"", False),
    ("grep-cli-binaries-cheaply", "grep -e '.{0,9}x' /opt/homebrew/bin/claude", True),
    ("grep-cli-binaries-cheaply", "egrep -ao '.{,40}x' ~/.local/bin/agy", True),
    ("grep-cli-binaries-cheaply", "rtk proxy grep -ao '.{0,40}x' $(command -v claude)", True),
    ("grep-cli-binaries-cheaply", "strings $(which claude) > $S/c.txt && grep -o '.{0,40}x' $S/c.txt", False),
    ("grep-cli-binaries-cheaply", "ls -la ~/.local/bin/claude && grep -o 'x.{0,40}' notes.txt", False),
    ("never-probe-the-users-live-emacs", "emacsclient --socket-name=/tmp/s/server -e '(profiler-report)'", False),
    ("never-probe-the-users-live-emacs", "cd ~/.emacs.d\nemacsclient -e '(profiler-memory-log)'", True),
    ("never-probe-the-users-live-emacs", "echo \"emacsclient -e '(profiler-report)'\"", False),
])
def test_more_of_what_each_rule_sees(fact, command, denied):
    assert L.Rule(RULES[fact]).matches(L.Views(command)) is denied


@pytest.mark.parametrize("command", [
    # What a check cannot see -- a script run by name, a path built in a variable.
    "python3 /tmp/peek_live_index.py",
    "D=$HOME/.emacs.d/emacs-home; python3 -c \"import os; from qdrant_client import QdrantClient; "
    "QdrantClient(path=os.environ['D'] + '/ghostherd-memory')\"",
])
def test_known_misses(command):
    assert not L.Rule(QDRANT).matches(L.Views(command))


# ---- Reading a shell line ---------------------------------------------------


def test_views_split_and_strip_wrappers():
    v = L.Views("cd /x && LC_ALL=C timeout 30 grep -a foo bar | head -3; rtk proxy git status\n"
                "for f in a b; do nice -n 5 grep x $f; done")
    assert v.command == ["cd /x", "grep -a foo bar", "| head -3", "git status",
                         "for f in a b", "grep x $f", "done"]
    assert "LC_ALL=C" in v.line and "for f in a b" in v.line


def test_views_code_from_interpreters_and_not_from_text():
    v = L.Views("python3 -c 'print(1)' && node -e \"x()\"\n"
                "cat > notes.md <<'EOF'\nprint(2)\nEOF\n"
                "herd reply 1 <<'HERD'\nprint(3)\nHERD\n"
                ".venv/bin/python - <<'PY'\nprint(4)\nPY\n"
                "uv run --with numpy python <<EOF\nprint(5)\nEOF")
    assert v.code == ["print(1)", "x()", "print(4)", "print(5)"]
    assert "print(2)" not in v.line and "print(3)" not in v.line


def test_views_a_script_written_then_run_is_code():
    v = L.Views("cat > \"$S/p.py\" <<'EOF'\nprint('run')\nEOF\npython3 $S/p.py\n"
                "cat > $S/q.py <<'EOF'\nprint('kept')\nEOF")
    assert v.code == ["print('run')"]


def test_views_nested_shells_and_substitutions():
    v = L.Views("bash -c 'grep -a x $(which claude)' && echo `ls /tmp`")
    assert "grep -a x $(which claude)" in v.command
    assert "which claude" in v.command and "ls /tmp" in v.command


def test_views_never_raise_on_odd_input():
    for text in ["'unclosed", "a <<", "$(", "`", "echo \\", "x <<EOF", "))((", "", "\n\n", "a |& b"]:
        L.Views(text)


# ---- What a rule may be ---------------------------------------------------------


@pytest.mark.parametrize("change,why", [
    ({"hint": ""}, "hint"),
    ({"hint": "x" * 700}, "under 600"),
    ({"check": {}}, "a check"),
    ({"check": {"argv": {"all": ["x"]}}}, "no view"),
    ({"check": {"line": {"all": ["("]}}}, "does not compile"),
    ({"check": {"line": {"none": ["x"]}}}, "at least one pattern"),
    ({"catches": []}, "at least one command it denies"),
    ({"passes": []}, "lets through"),
    ({"catches": ["ls"]}, "does not catch its own example"),
    ({"passes": ["grep -a -o '.{0,60}x' ~/.local/bin/claude"]}, "denies its own pass example"),
])
def test_a_rule_is_held_to_its_shape_and_examples(change, why):
    with pytest.raises(ValueError, match=why):
        L.validate(dict(GREP, **change))


def test_a_backtracking_rule_is_refused_not_waited_on(monkeypatch):
    monkeypatch.setattr(lint, "VALIDATE_SECONDS", 2)
    slow = {"fact": "x", "hint": "h", "check": {"line": {"all": [r"(a+)+$"]}},
            "catches": ["a" * 40 + "b"], "passes": ["ls"]}
    t = time.monotonic()
    with pytest.raises(ValueError, match="backtracks"):
        lint.validate(slow)
    assert time.monotonic() - t < 6


# ---- The hook, as each CLI runs it ----------------------------------------------


def _write(root, key, *rules, status="on"):
    L.write_rules(key, [dict(r, status=status) for r in rules], root=str(root))


def _env(root, **more):
    e = {"PATH": os.environ["PATH"], "HOME": os.environ.get("HOME", ""),
         "GHOSTHERD_LINT_DIR": str(root), "GHOSTHERD_HERD": str(HERD), "GHOSTHERD_SESSION": "claude-main"}
    e.update(more)
    return e


def claude_payload(command, cwd="/"):
    return {"session_id": "s1", "transcript_path": "/t.jsonl", "cwd": cwd, "permission_mode": "default",
            "hook_event_name": "PreToolUse", "tool_name": "Bash",
            "tool_input": {"command": command, "description": "x"}, "tool_use_id": "t1"}


def grok_payload(command, cwd="/"):
    "As grok 1.0.70's own docs show a PreToolUse event."
    return {"hookEventName": "pre_tool_use", "hook_event_name": "PreToolUse", "sessionId": "abc-123",
            "cwd": cwd, "workspaceRoot": cwd, "permissionMode": "default",
            "toolName": "run_terminal_command", "toolInput": {"command": command},
            "toolUseId": "u1", "toolInputTruncated": False, "timestamp": "2026-10-26T00:00:00Z"}


def run_installed(payload, env):
    "The command `herd hooks install' writes for claude and grok, through sh as they run it."
    out = subprocess.run(["sh", "-c", f'[ -z "$GHOSTHERD_HERD" ] || {HERD} lint'],
                         input=json.dumps(payload), capture_output=True, text=True, env=env, timeout=20)
    assert out.returncode == 0, out.stderr
    return json.loads(out.stdout) if out.stdout.strip() else None


BAD = "grep -a -o '.{0,60}PreToolUse' ~/.local/bin/claude"


@pytest.mark.parametrize("make", [claude_payload, grok_payload], ids=["claude", "grok"])
def test_the_hook_denies_with_the_facts_name_and_hint(tmp_path, make):
    _write(tmp_path, "user", GREP)
    got = run_installed(make(BAD), _env(tmp_path))
    out = got["hookSpecificOutput"]
    assert (out["hookEventName"], out["permissionDecision"]) == ("PreToolUse", "deny")
    assert "grep-cli-binaries-cheaply" in out["permissionDecisionReason"]
    assert "strings FILE | grep -F" in out["permissionDecisionReason"]
    assert "herd recall grep-cli-binaries-cheaply" in out["permissionDecisionReason"]
    assert run_installed(make("strings ~/.local/bin/claude | grep -F x"), _env(tmp_path)) is None


def test_the_hook_does_nothing_outside_the_herd(tmp_path):
    _write(tmp_path, "user", GREP)
    env = _env(tmp_path)
    del env["GHOSTHERD_HERD"]
    assert run_installed(claude_payload(BAD), env) is None


def test_the_hook_only_looks_at_shell_calls(tmp_path):
    _write(tmp_path, "user", GREP)
    for p in [dict(claude_payload(BAD), tool_name="Write"), dict(grok_payload(BAD), toolName="read_file"),
              {"hook_event_name": "PreToolUse", "tool_name": "Bash", "tool_input": {"cmd": BAD}}]:
        assert run_installed(p, _env(tmp_path)) is None


@pytest.mark.parametrize("breakage", ["not json", '{"rules": 3}', None])
def test_the_hook_fails_open(tmp_path, breakage):
    if breakage is None:   # one rule does not compile: the others still run
        L.write_rules("user", [{"fact": "broken", "status": "on", "hint": "h",
                                "check": {"line": {"all": ["("]}}}, dict(EMACS, status="on")],
                      root=str(tmp_path))
        assert run_installed(claude_payload(BAD), _env(tmp_path)) is None
        assert run_installed(claude_payload("emacsclient -e '(profiler-report)'"), _env(tmp_path))
        return
    (tmp_path / "user.json").write_text(breakage)
    assert run_installed(claude_payload(BAD), _env(tmp_path)) is None


def test_the_hook_fails_open_on_bad_stdin_and_no_rules(tmp_path):
    for stdin in ["", "not json", "[]", '{"tool_name": "Bash", "tool_input": "x"}']:
        out = subprocess.run([str(HERD), "lint"], input=stdin, capture_output=True, text=True,
                             env=_env(tmp_path), timeout=20)
        assert (out.returncode, out.stdout) == (0, "")
    assert run_installed(claude_payload(BAD), _env(tmp_path / "none")) is None


def test_a_match_past_its_budget_makes_no_decision(tmp_path, monkeypatch):
    L.write_rules("user", [{"fact": "slow", "status": "on", "hint": "h",
                            "check": {"line": {"all": [r"(a+)+$"]}}}], root=str(tmp_path))
    monkeypatch.setattr(L, "BUDGET_SECONDS", 0.2)
    t = time.monotonic()
    assert L.decide(claude_payload("a" * 40 + "b"), root=str(tmp_path)) is None
    assert time.monotonic() - t < 2


def test_a_rule_turned_off_denies_nothing(tmp_path):
    _write(tmp_path, "user", GREP, status="off")
    assert run_installed(claude_payload(BAD), _env(tmp_path)) is None


def _git_checkout(path):
    path.mkdir(parents=True)
    subprocess.run(["git", "init", "-q", str(path)], check=True)
    return path


def test_a_projects_rules_apply_in_it_and_its_worktrees_only(tmp_path):
    main = _git_checkout(tmp_path / "src" / "proj")
    (main / "sub").mkdir()
    wt = tmp_path / "worktrees" / "proj-2"
    wt.mkdir(parents=True)
    (wt / ".git").write_text(f"gitdir: {main}/.git/worktrees/proj-2\n")
    other = _git_checkout(tmp_path / "src" / "other")
    rules = tmp_path / "lint"
    _write(rules, sm.slug_for_path(str(main)), GREP)
    assert L.slug(str(main)) == sm.slug_for_path(str(main))
    for cwd in (main, main / "sub", wt):
        assert run_installed(claude_payload(BAD, str(cwd)), _env(rules)), cwd
    assert run_installed(claude_payload(BAD, str(other)), _env(rules)) is None
    assert run_installed(claude_payload(BAD, str(tmp_path)), _env(rules)) is None


def test_the_fast_path_imports_little():
    "`herd lint' answers before bin/herd imports argparse and urllib."
    out = subprocess.run([sys.executable, "-X", "importtime", str(HERD), "lint"], input="{}",
                         capture_output=True, text=True, timeout=20)
    imported = out.stderr
    assert "herd_lint" in imported
    assert "argparse" not in imported and "urllib" not in imported


# ---- agy: through its guard ---------------------------------------------------


@pytest.fixture
def guard(tmp_path):
    ws = _git_checkout(tmp_path / "ws")
    (ws / "notes.md").write_text("hi\n")
    gd = tmp_path / "guard" / "agy-ws"
    gd.mkdir(parents=True)
    (gd / "policy.json").write_text(json.dumps({
        "agent": "agy-ws", "mode": "auto", "workspace": str(ws), "guard_root": str(gd.parent),
        "herd": [str(HERD)]}))
    return gd, ws


def run_guard(gd, command, env):
    "The command the herd writes into a guarded agy's hooks.json."
    cmd = (f"{shlex.quote(str(HERD))} guard {shlex.quote(str(gd))}"
           " || echo '{\"decision\":\"deny\",\"reason\":\"The herd guard did not answer; tell the user.\"}'")
    payload = {"toolCall": {"name": "run_command", "args": {"CommandLine": command}},
               "workspacePaths": [str(gd), json.loads((gd / "policy.json").read_text())["workspace"]]}
    out = subprocess.run(["sh", "-c", cmd], input=json.dumps(payload), capture_output=True, text=True,
                         env=env, timeout=20)
    return json.loads(out.stdout)


LS_RULE = {"fact": "no-ls", "hint": "List with find instead.", "check": {"command": {"all": [r"^ls\b"]}},
           "catches": ["ls"], "passes": ["find ."]}


def test_agy_through_its_guard(guard, tmp_path):
    gd, ws = guard
    rules = tmp_path / "lint"
    env = _env(rules, GHOSTHERD_SESSION="agy-ws")
    assert run_guard(gd, "ls", env)["decision"] == "allow"        # no rules: the guard's word
    _write(rules, sm.slug_for_path(str(ws)), LS_RULE)
    no = run_guard(gd, "ls", env)
    assert no["decision"] == "deny" and "no-ls" in no["reason"] and "find instead" in no["reason"]
    assert run_guard(gd, "cat notes.md", env)["decision"] == "allow"
    # The guard's own denials stay its own.
    assert run_guard(gd, "env", env)["decision"] == "deny"
    # A broken rules file: no decision -- the guard decides, and allows.
    (rules / (sm.slug_for_path(str(ws)) + ".json")).write_text("{")
    assert run_guard(gd, "ls", env)["decision"] == "allow"


def test_a_check_never_spends_the_users_one_time_approval(guard, tmp_path):
    import herd_guard as g
    gd, ws = guard
    rules = tmp_path / "lint"
    call = {"name": "run_command", "args": {"CommandLine": "ls /etc"}}
    g.add_approval(str(gd), g.fingerprint(call), "req1")
    _write(rules, "user", LS_RULE)
    assert run_guard(gd, "ls /etc", _env(rules))["decision"] == "deny"
    assert g.take_approval(str(gd), g.fingerprint(call)) == "req1"


# ---- Proposed, approved, turned off, removed; each denial counted ----------------


@pytest.fixture
def rpc(tmp_path, monkeypatch):
    monkeypatch.setenv("GHOSTHERD_SHARED_MEMORY_DIR", str(tmp_path / "mem"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "sidecar"))
    monkeypatch.setenv("GHOSTHERD_LINT_DIR", str(tmp_path / "lint"))
    reset_herd()
    project = _git_checkout(tmp_path / "work" / "proj")
    key = sm.slug_for_path(str(project))
    store = sm.project_store(key)
    sm.write_fact(store, key, "Never grep a CLI binary with a leading .{0,N}.",
                  name="grep-cli-binaries-cheaply", type="feedback")
    sm.write_fact(sm.user_store(), sm.USER_KEY, "Never ls; find is the way.", name="no-ls", type="feedback")
    get_herd().tick(sessions=[{"name": "claude-proj", "kind": "claude", "state": "idle",
                               "project": str(project), "role": ""}])
    yield JsonRpcHandler(), key, store, project
    reset_herd()


def _propose(handler, rule, **more):
    params = {"from": "claude-proj", "facts": [rule["fact"]],
              "lint": {k: v for k, v in rule.items() if k != "fact"}}
    params.update(more)
    return handler._herd_promote_propose(params)


def test_propose_approve_turn_off_remove(rpc, tmp_path):
    handler, key, store, project = rpc
    out = _propose(handler, GREP)
    assert out["kind"] == "lint"
    deny = lambda: L.decide(claude_payload(BAD, str(project)))  # noqa: E731
    # Pending: on the Memory tab and in the Inbox, and not on.
    props = handler._herd_memory_proposals({"key": key})["proposals"]
    assert [(p["kind"], p["status"], p["facts"][0]["name"]) for p in props] == [
        ("lint", "pending", "grep-cli-binaries-cheaply")]
    assert props[0]["rule"]["catches"] == GREP["catches"]
    assert [p["id"] for p in handler._herd_pending_proposals({})["proposals"]] == [out["id"]]
    assert deny() is None and handler._herd_lint_rules({"key": key})["rules"] == []
    # The same check proposed again is the same proposal.
    assert _propose(handler, GREP)["duplicate"] is True
    # Approved: on at once -- no lead asked to apply anything.
    appr = handler._herd_memory_promote_approve({"id": out["id"]})
    assert (appr["status"], appr["fact"]) == ("applied", "grep-cli-binaries-cheaply")
    assert get_herd().proposal(out["id"])["status"] == "applied"
    assert get_herd().tick(sessions=[])["pending"] == []
    assert deny()[2].fact == "grep-cli-binaries-cheaply"
    with pytest.raises(ValueError, match="already applied"):
        handler._herd_memory_promote_approve({"id": out["id"]})
    # Off, on again, removed.
    handler._herd_lint_set({"key": key, "fact": "grep-cli-binaries-cheaply", "status": "off"})
    assert deny() is None
    assert handler._herd_lint_rules({"key": key})["rules"][0]["status"] == "off"
    handler._herd_lint_set({"key": key, "fact": "grep-cli-binaries-cheaply", "status": "on"})
    assert deny() is not None
    handler._herd_lint_remove({"key": key, "fact": "grep-cli-binaries-cheaply"})
    assert deny() is None and handler._herd_lint_rules({"key": key})["rules"] == []
    with pytest.raises(ValueError, match="no check"):
        handler._herd_lint_set({"key": key, "fact": "grep-cli-binaries-cheaply", "status": "on"})


def test_a_user_facts_check_is_every_agents(rpc):
    handler, _key, _store, project = rpc
    out = _propose(handler, LS_RULE, user=True)
    handler._herd_memory_promote_approve({"id": out["id"]})
    assert L.decide(claude_payload("ls", "/"))[1] == "user"
    assert L.decide(claude_payload("ls", str(project)))[1] == "user"


def test_a_rejected_check_never_goes_on(rpc):
    handler, key, _store, project = rpc
    out = _propose(handler, GREP)
    handler._herd_memory_promote_reject({"id": out["id"], "reason": "too narrow"})
    assert get_herd().proposal(out["id"])["status"] == "rejected"
    assert L.decide(claude_payload(BAD, str(project))) is None


@pytest.mark.parametrize("change,why", [
    ({"facts": ["no-such-fact"]}, "no fact named"),
    ({"facts": ["grep-cli-binaries-cheaply", "no-ls"]}, "one fact"),
    ({"lint": "grep"}, "JSON object"),
    ({"lint": dict({k: v for k, v in GREP.items() if k != "fact"}, catches=["ls"])}, "own example"),
])
def test_a_proposal_is_refused(rpc, change, why):
    handler, key, _store, _p = rpc
    with pytest.raises(ValueError, match=why):
        _propose(handler, GREP, **change)
    assert handler._herd_memory_proposals({"key": key})["proposals"] == []


def test_a_corrupt_rules_file_refuses_the_approval_and_keeps_it_pending(rpc, tmp_path):
    handler, key, _store, _p = rpc
    out = _propose(handler, GREP)
    (tmp_path / "lint").mkdir(exist_ok=True)
    (tmp_path / "lint" / f"{key}.json").write_text("{")
    with pytest.raises(ValueError):
        handler._herd_memory_promote_approve({"id": out["id"]})
    assert get_herd().proposal(out["id"])["status"] == "pending"
    assert (tmp_path / "lint" / f"{key}.json").read_text() == "{"


def test_the_pages_methods_are_not_an_agents(rpc):
    import asyncio
    handler = rpc[0]
    for method in ("herd_lint_set", "herd_lint_remove", "herd_lint_rules", "herd_memory_promote_approve"):
        resp = asyncio.run(handler.handle(JsonRpcRequest(id=1, method=method, params={}), role="agent"))
        assert resp.error and resp.error.code == FORBIDDEN, method
    resp = asyncio.run(handler.handle(JsonRpcRequest(id=1, method="herd_lint_denied", params={}), role="agent"))
    assert resp.error is None or resp.error.code != FORBIDDEN


def test_a_denial_is_counted_and_logged(live, tmp_path, monkeypatch):
    rules = tmp_path / "lint"
    _write(rules, "user", GREP)
    h = herd_mod.get_herd()
    env = _env(rules, GHOSTHERD_RPC=live["url"], GHOSTHERD_SESSION="claude-main")
    for _ in range(2):
        assert run_installed(claude_payload(BAD), env)
    # The grok and the guarded agy paths tell it too.
    assert run_installed(grok_payload(BAD), dict(env, GHOSTHERD_SESSION="grok-old"))
    counts = h.lint_counts("user")["grep-cli-binaries-cheaply"]
    assert counts["count"] == 3 and counts["last_session"] == "grok-old" and counts["last_cli"] == "grok"
    assert counts["last_command"] == BAD
    tick = h.tick(sessions=[])
    assert [(d["session"], d["fact"], d["cli"]) for d in tick["lint"]] == [
        ("grok-old", "grep-cli-binaries-cheaply", "grok"),
        ("claude-main", "grep-cli-binaries-cheaply", "claude"),
        ("claude-main", "grep-cli-binaries-cheaply", "claude")]
    monkeypatch.setenv("GHOSTHERD_LINT_DIR", str(rules))
    page = JsonRpcHandler()._herd_lint_rules({"key": "user"})["rules"][0]
    assert (page["fact"], page["count"], page["status"]) == ("grep-cli-binaries-cheaply", 3, "on")


def test_a_denial_with_no_sidecar_still_denies(tmp_path):
    _write(tmp_path, "user", GREP)
    t = time.monotonic()
    env = _env(tmp_path, GHOSTHERD_RPC="http://127.0.0.1:9/jsonrpc/x")
    assert run_installed(claude_payload(BAD), env)["hookSpecificOutput"]["permissionDecision"] == "deny"
    assert time.monotonic() - t < 5


def test_the_client_proposes_and_checks(live, tmp_path, monkeypatch):
    """`herd promote propose --lint' and `herd lint check', as an agent runs them."""
    monkeypatch.setenv("GHOSTHERD_LINT_DIR", str(tmp_path / "lint"))
    h = herd_mod.get_herd()
    project = _git_checkout(tmp_path / "proj")
    h.tick(sessions=[{"name": "claude-main", "kind": "claude", "state": "idle", "project": str(project)}])
    key = sm.slug_for_path(str(project))
    sm.write_fact(sm.project_store(key), key, "Never grep CLI binaries with .{0,N}.",
                  name="grep-cli-binaries-cheaply", type="feedback")
    env = dict(live["env"], GHOSTHERD_LINT_DIR=str(tmp_path / "lint"))
    rule = {k: v for k, v in GREP.items() if k != "fact"}
    out = subprocess.run([str(HERD), "promote", "propose", "--fact", "grep-cli-binaries-cheaply", "--lint"],
                         input=json.dumps(rule), capture_output=True, text=True, env=env, timeout=30)
    assert out.returncode == 0, out.stderr
    assert "proposed a check for grep-cli-binaries-cheaply" in out.stderr
    rf = tmp_path / "rule.json"
    rf.write_text(json.dumps(GREP))
    check = lambda *a: subprocess.run([str(HERD), "lint", "check", *a], capture_output=True,  # noqa: E731
                                      text=True, env=env, timeout=20)
    got = check("--rule", str(rf), BAD)
    assert got.returncode == 3 and "grep-cli-binaries-cheaply" in got.stdout
    assert check("--rule", str(rf), "grep -F x ~/.local/bin/claude").returncode == 0
    # Not on until approved.
    assert check("--cwd", str(project), BAD).returncode == 0
    pid = h.proposals(status="pending")[0]["id"]
    JsonRpcHandler()._herd_memory_promote_approve({"id": pid})
    assert check("--cwd", str(project), BAD).returncode == 3
