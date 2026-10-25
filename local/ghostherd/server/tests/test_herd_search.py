"""`herd search': an agent searches its own project's past sessions --
the project's directory, those under it and its worktrees -- never
another project's, in short excerpts within a budget, and reads one on
by session and turn."""

import os
from pathlib import Path

import pytest

import agent_search
from chunk import Turn
from engine import get_engine, reset_engine
from herd import get_herd, reset_herd
from index import SourceMeta
from jsonrpc_handler import JsonRpcHandler

SECRET = "sk-ant-api03-" + "x" * 40


@pytest.fixture
def herd(tmp_path, monkeypatch):
    """Sessions of /w/proj (the main checkout, a subdirectory, a worktree, a
    removed worktree), of /w/other, and of primary's own directory."""
    wt = tmp_path / "wt"
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    monkeypatch.setenv("GHOSTHERD_WORKTREE_ROOT", str(wt))
    monkeypatch.setenv("GHOSTHERD_MEMORY_CLAUDE_ROOT", str(tmp_path / "claude"))
    reset_engine()
    reset_herd()
    # A live worktree of /w/proj: its .git points into the main checkout's.
    live = wt / "proj" / "claude-proj-2"
    live.mkdir(parents=True)
    (live / ".git").write_text("gitdir: /w/proj/.git/worktrees/claude-proj-2\n")
    eng = get_engine()
    eng._ensure_ready()
    sessions = {
        "s-main": ("/w/proj", "claude", "the overlay sizing bug: posframe width in the main checkout"),
        "s-sub": ("/w/proj/server", "grok", "the overlay sizing bug, seen from the server dir"),
        "s-wt": (str(live), "claude", "the overlay sizing bug fixed in a worktree branch"),
        "s-gone": (str(wt / "proj" / "old-agent"), "agy", "the overlay sizing bug, an agent since removed"),
        "s-other": ("/w/other", "claude", "the overlay sizing bug in another project, OPENAI_API_KEY="
                    + SECRET),
        "s-primary": ("/p/primary", "claude", "primary routed the overlay sizing bug to proj"),
    }
    for sid, (project, agent, text) in sessions.items():
        _index(eng, sid, project, agent, [text])
    get_herd().tick(sessions=[
        {"name": "claude-proj", "kind": "claude", "state": "idle", "project": "/w/proj"},
        {"name": "claude-other", "kind": "claude", "state": "idle", "project": "/w/other"},
        {"name": "primary", "kind": "claude", "state": "idle", "project": "/p/primary",
         "role": "primary"},
    ])
    yield eng
    reset_herd()
    reset_engine()


def _index(eng, sid, project, agent, texts, path=None, kind="transcript", role="user"):
    path = path or f"/src/{sid}.jsonl"
    meta = SourceMeta(path=path, mtime=1.0, size=1, agent=agent, session_id=sid, project=project,
                      kind=kind, title=f"title of {sid}")
    turns = [Turn(agent=agent, role=role, text=t, project=project, session_id=sid, source_path=path,
                  source_kind=kind, ts="2026-10-24T10:00:00Z", title=f"title of {sid}")
             for t in texts]
    eng._import_source(meta, turns, force=True)


def search(who, **params):
    return JsonRpcHandler()._herd_search({"from": who, **params})


def sessions_of(out):
    return {h["session"] for h in out["hits"]}


def test_an_agent_finds_its_projects_sessions_and_its_worktrees_only(herd):
    found = sessions_of(search("claude-proj", query="overlay sizing bug"))
    # Its checkout, a directory under it, its worktree -- live or removed.
    assert found == {"s-main", "s-sub", "s-wt", "s-gone"}
    assert sessions_of(search("claude-other", query="overlay sizing bug")) == {"s-other"}


def test_another_projects_session_cannot_be_read(herd):
    with pytest.raises(ValueError, match="no session"):
        search("claude-proj", session="s-other")
    assert "the overlay" in search("claude-other", session="s-other")["text"]


def test_primary_reaches_its_own_sessions_only(herd):
    assert sessions_of(search("primary", query="overlay sizing bug")) == {"s-primary"}
    with pytest.raises(ValueError, match="no session"):
        search("primary", session="s-main")


def test_a_caller_the_herd_does_not_know_is_refused(herd):
    for who in ("user", "", "nobody-by-that-name"):
        with pytest.raises(ValueError, match="for the herd's agents"):
            search(who, query="overlay")
    # A project named in the call is not taken.
    out = search("claude-proj", query="overlay sizing bug", project="/w/other")
    assert "s-other" not in sessions_of(out)


def test_a_worktree_of_another_project_of_the_same_name_is_not_its(herd, tmp_path):
    wt = tmp_path / "wt"
    theirs = wt / "proj" / "claude-x"
    theirs.mkdir(parents=True)
    (theirs / ".git").write_text("gitdir: /elsewhere/proj/.git/worktrees/claude-x\n")
    _index(herd, "s-theirs", str(theirs), "claude", ["the overlay sizing bug, someone else's proj"])
    assert "s-theirs" not in sessions_of(search("claude-proj", query="overlay sizing bug"))
    # And once another /…/proj is known, a removed worktree is no longer
    # taken for this one's: whose it was cannot be told.
    known = ["/w/proj", "/elsewhere/proj"]
    scope = agent_search.scope(herd, "/w/proj", wt, known)
    assert str(wt / "proj" / "old-agent") not in scope and "/w/proj/server" in scope


def test_a_project_at_home_takes_itself_alone(herd):
    home = str(Path.home())
    _index(herd, "s-home", home, "claude", ["at home"])
    _index(herd, "s-home-sub", home + "/somewhere", "claude", ["under home"])
    scope = agent_search.scope(herd, home, Path("/nowhere"), [])
    assert scope == [home]


def test_the_budget(herd):
    # Under a chunk's 1800 characters, the needle in the middle of each.
    long = ("word " * 150) + "the needle sits here, in the middle of a long turn " + ("word " * 150)
    for i in range(12):
        _index(herd, f"s-long-{i}", "/w/proj", "claude", [f"turn number {i} " + long])
    out = search("claude-proj", query="needle")
    text = out["text"]
    assert len(text) <= agent_search.BLOCK
    hits = [b for b in text.split("\n") if b and b[0].isdigit()]
    assert 1 <= len(hits) <= agent_search.HITS
    blocks = text.split("\n\n")[0].split("\n")
    for head, body in zip(blocks[::2], blocks[1::2]):
        assert len(head) + len(body) + 1 <= agent_search.ITEM
    # An excerpt is cut around what was asked for.
    assert "needle sits here" in text and "…" in text
    assert 'search --session ID --at TURN' in text


def test_reading_on_by_session_and_turn(herd):
    texts = [f"turn {i}: " + ("detail " * 60) for i in range(30)]
    _index(herd, "6630344c-aaaa-bbbb-cccc-dddddddddddd", "/w/proj", "claude", texts,
           path="/src/long.jsonl")
    # What its CLI kept of it comes after the transcript.
    _index(herd, "6630344c-aaaa-bbbb-cccc-dddddddddddd", "/w/proj", "claude",
           ["the summary of the long session"], path="/src/long.summary", kind="memory",
           role="memory")
    out = search("claude-proj", session="6630344c")
    assert out["session"] == "6630344c-aaaa-bbbb-cccc-dddddddddddd" and out["turns"] == 31
    assert len(out["text"]) <= agent_search.BLOCK + 100
    assert out["text"].startswith("Session 6630344c-aaaa") and "-- turn 0 · user" in out["text"]
    nxt = out["next"]
    assert nxt and f"--session 6630344c --at {nxt}" in out["text"]
    seen = out["at"]
    while out["next"] is not None:
        out = search("claude-proj", session="6630344c", at=out["next"])
        assert out["at"] > seen and len(out["text"]) <= agent_search.BLOCK + 100
        seen = out["at"]
    assert "the summary of the long session" in out["text"] and "(the end of the session)" in out["text"]
    # A search hit says where to read on: its turn in this order.
    hit = next(h for h in search("claude-proj", query="summary of the long session")["hits"]
               if h["session"].startswith("6630344c"))
    assert hit["turn"] == 30


def test_secrets_are_blanked(herd, tmp_path):
    token = "f" * 64
    (tmp_path / "mem" / "rpc.agent-token").write_text(token)
    _index(herd, "s-leak", "/w/proj", "claude",
           [f"the deploy failed; env showed ANTHROPIC_API_KEY={SECRET} and "
            f"http://127.0.0.1:49152/jsonrpc/{token} and https://bob:hunter2@git.example.com/x.git"])
    for out in (search("claude-proj", query="deploy failed env"),
                search("claude-proj", session="s-leak")):
        assert SECRET not in out["text"] and token not in out["text"] and "hunter2" not in out["text"]
        assert "ANTHROPIC_API_KEY=[redacted]" in out["text"]


def test_redact_shapes():
    text = agent_search.redact(
        "token=abc123 password: 'p w' ghp_" + "a" * 36 + " AKIAABCDEFGHIJKLMNOP "
        "Authorization: Bearer " + "z" * 30 + " -----BEGIN RSA PRIVATE KEY-----\nxyz\n-----END RSA PRIVATE KEY-----")
    for gone in ("abc123", "'p w'", "ghp_", "AKIAABCD", "z" * 30, "xyz"):
        assert gone not in text, gone
    assert agent_search.redact("plain words stay") == "plain words stay"


def test_excerpt():
    text = "a " * 500 + "the needle is here" + " b" * 500
    ex = agent_search.excerpt(text, "needle", 120)
    assert "needle" in ex and len(ex) <= 120 and ex.startswith("…") and ex.endswith("…")
    assert agent_search.excerpt("short text", "x", 120) == "short text"
    zh = "前言" * 200 + "全域的總管跟所有專案溝通" + "後記" * 200
    assert "總管" in agent_search.excerpt(zh, "總管", 60)
