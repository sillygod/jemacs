"""What an agent's conversation names: its pull requests and its tasks."""

import asyncio
import json
from pathlib import Path

import pytest

import links
from jsonrpc_handler import JsonRpcRequest, handler


@pytest.fixture
def home(tmp_path, monkeypatch):
    monkeypatch.setenv("HOME", str(tmp_path))
    links._state.clear()
    yield tmp_path
    links._state.clear()


def _line(r) -> str:
    return json.dumps(r, ensure_ascii=False) + "\n"


def _claude(home, records) -> Path:
    path = home / ".claude" / "projects" / "-x-proj" / "abc.jsonl"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(map(_line, records)))
    return path


def u(content, ts="2026-10-09T01:00:00Z", **kw):
    return {"type": "user", "message": {"role": "user", "content": content}, "timestamp": ts, **kw}


def a(*blocks, ts="2026-10-09T01:00:05Z", **kw):
    return {"type": "assistant", "message": {"role": "assistant", "content": list(blocks)}, "timestamp": ts, **kw}


def text(t):
    return {"type": "text", "text": t}


def tool(name, **input):
    return {"type": "tool_use", "id": "t1", "name": name, "input": input}


def result(t):
    return {"type": "tool_result", "tool_use_id": "t1", "content": t}


def prs(r):
    return [(p["host"], p["owner"], p["repo"], p["id"]) for p in r["prs"]]


def tasks(r):
    return [t["id"] for t in r["tasks"]]


def test_what_was_given_or_said_counts_not_what_went_by(home):
    path = _claude(home, [
        u("review https://bitbucket.org/Team/Deploy-Platform/pull-requests/371 for https://app.clickup.com/t/86abc1def"),
        a(text("Looking at it."),
          tool("Bash", command="curl https://api.bitbucket.org/2.0/repositories/team/deploy-platform/pullrequests/372")),
        # A search's output names others: seen, not worked on.
        u([result("PRs: https://bitbucket.org/team/deploy-platform/pull-requests/1 CU-86other00 "
                  "https://app.clickup.com/t/86other11")]),
        a(text("Opened https://github.com/me/notes/pull/9, for CU-86xyz9876.")),
    ])
    r = links.scan(str(path))
    assert prs(r) == [("github", "me", "notes", 9), ("bitbucket", "team", "deploy-platform", 372),
                      ("bitbucket", "team", "deploy-platform", 371)]
    assert tasks(r) == ["86xyz9876", "86abc1def"]


def test_most_recently_named_first_with_how_often_and_when(home):
    path = _claude(home, [
        u("see CU-86aaa1 and CU-86bbb2", ts="2026-10-09T01:00:00Z"),
        a(text("CU-86aaa1 is done."), ts="2026-10-09T02:00:00Z"),
        u("now https://github.com/me/x/pull/2 then https://github.com/me/x/pull/3", ts="2026-10-09T03:00:00Z"),
        a(tool("Bash", command="gh pr view https://github.com/me/x/pull/2"), ts="2026-10-09T04:00:00Z"),
    ])
    r = links.scan(str(path))
    assert tasks(r) == ["86aaa1", "86bbb2"]
    assert [(t["count"], t["last"]) for t in r["tasks"]] == [(2, "2026-10-09T02:00:00Z"), (1, "2026-10-09T01:00:00Z")]
    assert [(p["id"], p["count"], p["last"]) for p in r["prs"]] == [
        (2, 2, "2026-10-09T04:00:00Z"), (3, 1, "2026-10-09T03:00:00Z")]


def test_a_task_id_is_clickup_s(home):
    path = _claude(home, [u(
        "CU-86abc1 CU-abcdef CU-86ABC1 xCU-86zzz9 CU-86x_y "
        "https://app.clickup.com/t/9012/86team7 https://app.clickup.com/t/86ok5/sub "
        "https://app.clickup.com/t/86also3.")])
    # Named in one message, they are as it names them.
    assert tasks(links.scan(str(path))) == ["86abc1", "86team7", "86also3"]


def test_owner_and_repo_are_one_spelling(home):
    path = _claude(home, [u("https://github.com/Me/Notes/pull/9 and https://www.github.com/me/notes/pull/9")])
    r = links.scan(str(path))
    assert prs(r) == [("github", "me", "notes", 9)] and r["prs"][0]["count"] == 2


def test_side_conversations_meta_and_summaries_do_not_count(home):
    path = _claude(home, [
        u("CU-86side1", isSidechain=True),
        a(text("https://github.com/me/x/pull/5"), isSidechain=True),
        u("CU-86meta1", isMeta=True),
        u("CU-86summ1", isCompactSummary=True),
        u("CU-86real1"),
    ])
    r = links.scan(str(path))
    assert tasks(r) == ["86real1"] and r["prs"] == []


def test_read_on_from_where_it_stopped(home, monkeypatch):
    path = _claude(home, [u("CU-86one11")])
    assert tasks(links.scan(str(path))) == ["86one11"]
    # A turn half written: its line is read once it ends.
    with path.open("a") as f:
        f.write(_line(a(text("CU-86two22"))))
        f.write(_line(u("CU-86three3"))[:20])
    assert tasks(links.scan(str(path))) == ["86two22", "86one11"]
    read = []
    real_said = links._said
    monkeypatch.setattr(links, "_said", lambda o, cli: read.append(o) or real_said(o, cli))
    with path.open("a") as f:
        f.write(_line(u("CU-86three3"))[20:])
    r = links.scan(str(path))
    assert tasks(r) == ["86three3", "86two22", "86one11"]
    assert len(read) == 1, "only the new line is read"
    assert links.scan(str(path)) == r and len(read) == 1


def test_a_rewritten_transcript_is_read_again(home):
    path = _claude(home, [u("CU-86one11"), u("CU-86two22")])
    links.scan(str(path))
    path.write_text(_line(u("CU-86new333")))
    assert tasks(links.scan(str(path))) == ["86new333"]


def test_agy(home):
    path = home / ".gemini" / "antigravity-cli" / "brain" / "c1" / ".system_generated" / "logs" / "transcript.jsonl"
    path.parent.mkdir(parents=True)
    path.write_text("".join(map(_line, [
        {"type": "USER_INPUT", "created_at": "t0", "content": "<USER_REQUEST>fix CU-86agy001</USER_REQUEST>"},
        {"type": "PLANNER_RESPONSE", "created_at": "t1",
         "tool_calls": [{"name": "run_command", "args": {"CommandLine": "gh pr view https://github.com/me/x/pull/4"}}]},
        {"type": "GENERIC", "created_at": "t2", "content": "https://github.com/me/x/pull/99 CU-86out999"},
        {"type": "PLANNER_RESPONSE", "created_at": "t3", "content": "Merged https://github.com/me/x/pull/4."},
    ])))
    r = links.scan(str(path))
    assert prs(r) == [("github", "me", "x", 4)] and r["prs"][0]["last"] == "t3"
    assert tasks(r) == ["86agy001"]


def test_grok(home):
    path = home / ".grok" / "sessions" / "%2Fx%2Fproj" / "s1" / "chat_history.jsonl"
    path.parent.mkdir(parents=True)
    path.write_text("".join(map(_line, [
        {"type": "user", "content": [{"type": "text", "text": "<user_query>CU-86grok01</user_query>"}]},
        {"type": "user", "content": [{"type": "text", "text": "CU-86summ01"}], "synthetic_reason": "compaction_meta"},
        {"type": "assistant", "content": "On it.", "tool_calls": [
            {"id": "c1", "name": "run_terminal_command",
             "arguments": json.dumps({"command": "gh pr view https://github.com/me/x/pull/7"})}]},
        {"type": "tool_result", "tool_call_id": "c1", "content": "https://github.com/me/x/pull/70"},
    ])))
    r = links.scan(str(path))
    assert prs(r) == [("github", "me", "x", 7)] and r["prs"][0]["last"] is None
    assert tasks(r) == ["86grok01"]


def test_only_transcripts_are_read(home):
    secret = home / ".ssh" / "notes.jsonl"
    secret.parent.mkdir()
    secret.write_text(_line(u("CU-86secret1")))
    for path in (str(secret), str(home / ".claude" / "projects" / "missing.jsonl"), ""):
        with pytest.raises(ValueError):
            links.scan(path)


def test_the_rpc(home):
    path = _claude(home, [u("CU-86rpc001")])
    r = asyncio.run(handler.handle(JsonRpcRequest(method="herd_links", params={"path": str(path)})))
    assert r.error is None and tasks(r.result) == ["86rpc001"]
    r = asyncio.run(handler.handle(JsonRpcRequest(method="herd_links", params={"path": str(home / "x.jsonl")})))
    assert r.error is not None and r.error.code == -32602
