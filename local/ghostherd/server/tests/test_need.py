"""A lead's own 需要 Jing lines (ask fd5ad907): `herd need list' and `need
done' -- its own alone, who and why kept and shown on the Board -- the note
a lead is told of them as its turn begins, only when one is new, and how
each CLI's hook hands that note on.  Made-up work and names throughout."""

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

import herd as herd_mod
from herd import needs_note
from tests.test_ask import HERD, live, run  # noqa: F401  (fixture)
from tests.test_board import LEADS, _item_of, _items, _lead_answer, _store
from tests.test_hooks import CLAUDE, GROK, hook

MORE = LEADS + [
    {"name": "lead-g", "kind": "agy", "state": "idle", "project": "/src/g/", "role": "lead"},
    {"name": "lead-k", "kind": "grok", "state": "idle", "project": "/src/k/", "role": "lead"},
]


def test_a_lead_marks_its_own_lines_handled_saying_why(tmp_path):
    h = _store(tmp_path)
    out = _lead_answer(h, "做好了。\n需要 Jing：決定 sandbox 要不要關掉訪客登入\n需要 Jing：核准 shop #12 上線")
    it = _item_of(h, out["id"])
    lines = h.need_list("lead-b")["lines"]
    assert [(e["text"], e["item"], e["agent"]) for e in lines] == [
        ("決定 sandbox 要不要關掉訪客登入", it, "lead-b"), ("核准 shop #12 上線", it, "lead-b")]
    one, two = (e["id"] for e in lines)
    # Another lead has none; the user sees every lead's.
    assert h.need_list("lead-a")["lines"] == []
    assert len(h.need_list("user")["lines"]) == 2 == len(h.need_list("")["lines"])
    # Another's line is refused, said why; nothing closes.
    out = h.need_done([one], by="lead-a", why="x")
    assert out["closed"] == [] and out["skipped"] == [{"id": one, "why": "lead-b's line, not yours"}]
    # The user marks them on the Board, not as an agent.
    for who in ("user", "", "  "):
        with pytest.raises(ValueError, match="the user marks them on the Board"):
            h.need_done([one], by=who)
    # Its own: closed once, said again or unknown skipped, why kept tidy.
    out = h.need_done([one, one, 99999], by="lead-b", why="  Jing 在對話裡回了：\n先關掉  ")
    assert out == {"closed": [one], "skipped": [{"id": 99999, "why": "no such 需要 Jing line"}], "open": 1}
    assert h.need_done([one], by="lead-b")["skipped"] == [{"id": one, "why": "already marked handled by lead-b"}]
    # On the Board: off the card and the list, on its timeline and among
    # the lines leads marked handled -- by whom, why, when.
    assert [n["id"] for n in _items(h)[it]["needs"]] == [two]
    assert [n["id"] for n in h.open_needs()["needs"]] == [two]
    ev = next(e for e in h.board_item(it)["events"] if e.get("event") == one)
    assert (ev["state"], ev["by"], ev["why"]) == ("done", "lead-b", "Jing 在對話裡回了： 先關掉") and ev["closedAt"]
    [e] = h.board()["ended"]
    assert (e["id"], e["item"], e["agent"], e["by"], e["why"], e["text"]) == (
        one, it, "lead-b", "lead-b", "Jing 在對話裡回了： 先關掉", "決定 sandbox 要不要關掉訪客登入")
    # The user's own mark is the Board's as ever: not among the leads'.
    h.need_close(two)
    assert [e["id"] for e in h.board()["ended"]] == [one]
    ev = next(e for e in h.board_item(it)["events"] if e.get("event") == two)
    assert (ev["state"], ev["by"], ev["why"]) == ("done", "user", "")
    assert h.need_done([two], by="lead-b")["skipped"] == [{"id": two, "why": "already marked handled by user"}]
    # A line said again in its item is no longer open: not closed.
    out = _lead_answer(h, "需要 Jing：換掉兩個環境的管理密碼", item=it)
    [three] = [e["id"] for e in h.need_list("lead-b")["lines"]]
    _lead_answer(h, "需要 Jing：換掉管理密碼（改寫）", item=it)
    assert h.need_done([three], by="lead-b")["skipped"][0]["why"].startswith("no longer open")


def test_a_lead_is_told_its_open_lines_as_a_turn_begins_when_one_is_new(tmp_path):
    h = _store(tmp_path)

    def turn(conv="c1", who="lead-b"):
        h.report(who, state="done", cli="claude", conversation=conv)
        return h.report(who, state="working", cli="claude", conversation=conv).get("note")

    assert turn() is None                                    # nothing open
    _lead_answer(h, "需要 Jing：換掉 dashboard 的管理密碼")
    [first] = h.need_list("lead-b")["lines"]
    note = turn()
    assert "your 需要 Jing lines still open on the user's Board (1, oldest first):" in note
    assert f"  {first['id']}  item {first['item']}  換掉 dashboard 的管理密碼" in note
    assert '"$GHOSTHERD_HERD" need done ID... --why "one line"' in note
    assert turn() is None                                    # the same list: not every turn
    assert turn("c2") is not None                            # a new conversation has it again
    _lead_answer(h, "需要 Jing：確認 builder3 可不可以跑夜間建置")
    note = turn("c2")                                        # a new line: the list again
    assert "(2, oldest first)" in note and note.index("dashboard") < note.index("builder3")
    h.need_done([first["id"]], by="lead-b", why="moved to another item")
    assert turn("c2") is None                                # only shorter: not again
    # Only as a turn begins; never to a worker, nor a lead of no lines.
    assert "note" not in h.report("lead-b", state="done", cli="claude", conversation="c3")
    assert turn(who="agy-a2") is None
    assert turn(who="lead-a") is None


def test_the_note_is_short():
    lines = [{"id": i, "item": "a1%04x" % i, "text": "決定第 %d 件" % i + "很長的說明" * 40} for i in range(1, 14)]
    note = needs_note(lines)
    rows = [r for r in note.splitlines() if r[:2] == "  " and r.strip()[:1].isdigit()]
    assert len(rows) == 10 and all(len(r) < 110 for r in rows) and rows[0].startswith("  1  item a10001  決定第 1 件")
    assert '  ... and 3 more: "$GHOSTHERD_HERD" need list' in note
    assert needs_note([{"id": 3, "item": "", "text": "x\nmore"}]).splitlines()[1] == "  3  (no item)  x"
    assert len(note) < 1600


def test_herd_need_over_the_agents_token(live, tmp_path):
    h = herd_mod.get_herd()
    h.tick(sessions=LEADS)
    _lead_answer(h, "需要 Jing：用自己的帳號從公司的機器掃一次 port")
    env = {"HOME": str(tmp_path), "TMPDIR": str(tmp_path)}
    out = run(live, "need", "list", session="lead-b", env=env)
    assert out.returncode == 0 and "用自己的帳號從公司的機器掃一次 port" in out.stdout, out.stderr
    [line] = h.need_list("lead-b")["lines"]
    out = run(live, "need", "done", str(line["id"]), "--why", "Jing 回了", session="lead-a", env=env)
    assert out.returncode == 1 and "lead-b's line, not yours" in out.stderr
    out = run(live, "need", "done", "abc", session="lead-b", env=env)
    assert out.returncode == 1 and "abc is not a line's id" in out.stderr
    out = run(live, "need", "list", "--why", "x", session="lead-b", env=env)
    assert out.returncode == 1 and "takes no --why" in out.stderr
    out = run(live, "need", "done", "#%d" % line["id"], "--why", "Jing 在對話裡回了", session="lead-b", env=env)
    assert out.returncode == 0, out.stderr
    assert "marked handled: %d -- Jing 在對話裡回了" % line["id"] in out.stderr and "0 of yours still open" in out.stderr
    out = run(live, "need", "list", session="lead-b", env=env)
    assert out.returncode == 0 and "no 需要 Jing line of yours is open" in out.stderr


def _kept(tmp):
    return sorted(p.name for p in Path(tmp).glob("ghostherd-note-*"))


def test_each_cli_is_handed_the_note_its_own_way(live, tmp_path):
    """claude: its UserPromptSubmit's context; agy: a system message among
    its PreInvocation's steps; grok, whose UserPromptSubmit cannot: kept
    for its next shell call, said by the check's PreToolUse, once."""
    h = herd_mod.get_herd()
    h.tick(sessions=MORE)
    _lead_answer(h, "需要 Jing：決定 plugin 目錄要不要拿掉")
    for who in ("lead-g", "lead-k"):
        assert h.note_report_lines(who, "好了。\n需要 Jing：核准 replica 改成 3（%s）" % who) == 1
    env = {"HOME": str(tmp_path)}
    # claude
    out = hook(live, dict(CLAUDE, hook_event_name="UserPromptSubmit"), session="lead-b", tmp=tmp_path, env=env)
    said = json.loads(out.stdout)["hookSpecificOutput"]
    assert said["hookEventName"] == "UserPromptSubmit" and "決定 plugin 目錄要不要拿掉" in said["additionalContext"]
    hook(live, dict(CLAUDE, hook_event_name="Stop", last_assistant_message="ok"), session="lead-b", tmp=tmp_path, env=env)
    out = hook(live, dict(CLAUDE, hook_event_name="UserPromptSubmit"), session="lead-b", tmp=tmp_path, env=env)
    assert (out.returncode, out.stdout) == (0, ""), out.stderr      # the same list: nothing
    # agy
    agy = {"conversationId": "g-1", "invocationNum": 0}
    out = hook(live, agy, "PreInvocation", session="lead-g", tmp=tmp_path, env=env)
    [step] = json.loads(out.stdout)["injectSteps"]
    assert "核准 replica 改成 3（lead-g）" in step["systemMessage"]["systemMessage"]
    hook(live, dict(agy, terminationReason="end"), "Stop", session="lead-g", tmp=tmp_path, env=env)
    out = hook(live, agy, "PreInvocation", session="lead-g", tmp=tmp_path, env=env)
    assert out.stdout.strip() == "{}"
    # grok: nothing printed at the prompt; the note kept for its next shell call.
    assert _kept(tmp_path) == []
    out = hook(live, dict(GROK, hook_event_name="UserPromptSubmit", hookEventName="user_prompt_submit"),
               session="lead-k", tmp=tmp_path, env=env)
    assert (out.returncode, out.stdout) == (0, ""), out.stderr
    assert len(_kept(tmp_path)) == 1
    call = {"hookEventName": "pre_tool_use", "hook_event_name": "PreToolUse", "sessionId": "g-123",
            "toolName": "run_terminal_command", "toolInput": {"command": "ls"}, "cwd": str(tmp_path)}
    lint_env = dict(os.environ, HOME=str(tmp_path), TMPDIR=str(tmp_path), GHOSTHERD_SESSION="lead-k",
                    GHOSTHERD_LINT_DIR=str(tmp_path / "lint"))
    lint_env.pop("GHOSTHERD_RPC", None)
    lint = lambda: subprocess.run([sys.executable, str(HERD), "lint"], input=json.dumps(call), capture_output=True,
                                  text=True, env=lint_env, timeout=20)
    out = lint()
    said = json.loads(out.stdout)["hookSpecificOutput"]
    assert said == {"hookEventName": "PreToolUse", "additionalContext": said["additionalContext"]}
    assert "核准 replica 改成 3（lead-k）" in said["additionalContext"] and "permissionDecision" not in said
    assert _kept(tmp_path) == []
    out = lint()                                                    # once
    assert (out.returncode, out.stdout) == (0, "")
    # Another agent's shell call never takes it.
    out = hook(live, dict(GROK, hook_event_name="UserPromptSubmit", hookEventName="user_prompt_submit",
                          sessionId="g-124"), session="lead-k", tmp=tmp_path, env=env)
    assert len(_kept(tmp_path)) == 1                                # a new conversation: kept again
    out = subprocess.run([sys.executable, str(HERD), "lint"], input=json.dumps(call), capture_output=True,
                         text=True, env=dict(lint_env, GHOSTHERD_SESSION="lead-b"), timeout=20)
    assert out.stdout == "" and len(_kept(tmp_path)) == 1


def test_a_kept_note_is_the_agents_alone(tmp_path, monkeypatch):
    """grok's note, kept for its next shell call: one file a name -- names
    alike but for a character apart -- the user's alone, never written or
    read through a link someone left there."""
    sys.path.insert(0, str(Path(HERD).parent))
    import herd_lint
    monkeypatch.setenv("TMPDIR", str(tmp_path))
    names = ["lead-專案甲", "lead-專案乙", "lead-" + "x" * 90 + "a", "lead-" + "x" * 90 + "b"]
    assert len({herd_lint.note_file(n) for n in names}) == 4
    herd_lint.keep_note("lead-k", "first")
    herd_lint.keep_note("lead-k", "second")
    path = Path(herd_lint.note_file("lead-k"))
    assert oct(path.stat().st_mode & 0o777) == "0o600"
    assert herd_lint.take_note("lead-k") == "second" and herd_lint.take_note("lead-k") is None
    # A link left where the note goes: kept beside it, not through it.
    target = tmp_path / "elsewhere"
    target.write_text("untouched")
    Path(str(path) + ".tmp").symlink_to(target)
    herd_lint.keep_note("lead-k", "third")
    assert target.read_text() == "untouched" and herd_lint.take_note("lead-k") == "third"
    # One planted as a link is not read.
    path.symlink_to(target)
    assert herd_lint.take_note("lead-k") is None and target.read_text() == "untouched"
