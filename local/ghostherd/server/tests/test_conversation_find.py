"""Finding words in an agent's whole conversation (the room's find bar).

The find reads the whole transcript, not the tail the room shows -- parsed
once, then only what was added -- and looks in what the room shows as said:
your words, the agent's, the herd's messages and answers, a sub-agent's
report; never a tool call or its output.  A hit is its place in the whole
(`i'), which `read(start=i)' pages to.
"""

import asyncio
import json
import os
from pathlib import Path

import pytest

import conversation as conv
from jsonrpc_handler import FORBIDDEN, JsonRpcRequest, handler


@pytest.fixture
def home(tmp_path, monkeypatch):
    monkeypatch.setenv("HOME", str(tmp_path))
    conv._cache.clear()
    conv._wholes.clear()
    yield tmp_path
    conv._cache.clear()
    conv._wholes.clear()


def at(n):
    return "2026-10-09T%02d:%02d:00Z" % (n // 60, n % 60)


def u(content, n=0, **kw):
    return {"type": "user", "message": {"role": "user", "content": content}, "timestamp": at(n), **kw}


def a(mid, *blocks, n=0):
    return {"type": "assistant", "message": {"id": mid, "role": "assistant", "content": list(blocks)},
            "timestamp": at(n)}


def text(t):
    return {"type": "text", "text": t}


def use(i, name, **inp):
    return {"type": "tool_use", "id": i, "name": name, "input": inp}


def result(i, content):
    return {"type": "tool_result", "tool_use_id": i, "content": content}


def _file(home, records, name="abc.jsonl") -> str:
    path = home / ".claude" / "projects" / "-x-proj" / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(json.dumps(r) + "\n" for r in records))
    return str(path)


def _append(path, records):
    with open(path, "a") as f:
        f.write("".join(json.dumps(r) + "\n" for r in records))


def _said(path, hits):
    "The text of each hit's entry, read back at its place."
    return [conv.read(path, 1, start=h["i"])["entries"][0].get("text") for h in hits]


def test_cjk_is_found_as_written_without_a_tokenizer(home):
    path = _file(home, [
        u("幫我看看 ghostherd 的房間能不能搜尋關鍵字", 1),
        a("m1", text("可以：在房間的跳轉列加一個搜尋按鈕。"), n=2),
        u("另外，手機上也要能用", 3),
    ])
    r = conv.find(path, "搜尋")
    assert r["total"] == 2 and not r["capped"]
    assert [h["role"] for h in r["hits"]] == ["user", "assistant"]
    assert "搜尋" in r["hits"][0]["snippet"] and "搜尋" in r["hits"][1]["snippet"]
    # One CJK character is a word; one ASCII letter is not (as the page's
    # termRe highlights them).
    assert conv.find(path, "機")["total"] == 1
    assert conv.find(path, "g")["total"] == 0 and conv.find(path, "g")["terms"] == []
    assert conv.find(path, "   ")["hits"] == []


def test_case_does_not_matter(home):
    path = _file(home, [u("The GhostHerd page", 1), a("m1", text("ghostherd it is"), n=2),
                        u("nothing here", 3)])
    assert conv.find(path, "GHOSTHERD")["total"] == 2
    hit = conv.find(path, "ghostHERD")["hits"][0]
    assert hit["snippet"] == "The GhostHerd page"   # as written, not lowered


def test_every_word_must_be_in_the_entry(home):
    path = _file(home, [
        u("alpha only", 1),
        u("beta only", 2),
        u("beta, and then\n\nmuch later, alpha", 3),
        a("m1", text("ALPHA and Beta"), n=4),
    ])
    r = conv.find(path, "alpha beta")
    assert r["terms"] == ["alpha", "beta"]
    assert r["total"] == 2 and _said(path, r["hits"]) == ["beta, and then\n\nmuch later, alpha", "ALPHA and Beta"]
    # A word said twice is one word.
    assert conv.find(path, "alpha ALPHA")["terms"] == ["alpha"]


def test_the_newest_hits_up_to_the_cap_oldest_first(home):
    path = _file(home, [u("needle %d" % i, i) for i in range(10)])
    r = conv.find(path, "needle", limit=3)
    assert r["total"] == 10 and r["capped"] is True and r["entries"] == 10
    assert [h["snippet"] for h in r["hits"]] == ["needle 7", "needle 8", "needle 9"]
    assert [h["i"] for h in r["hits"]] == [7, 8, 9]
    full = conv.find(path, "needle")
    assert full["capped"] is False and len(full["hits"]) == 10


def test_tool_calls_and_their_output_are_not_searched(home):
    path = _file(home, [
        u("run the tests", 1),
        a("m1", use("t1", "Bash", command="grep -r needle ."), n=2),
        u([result("t1", "needle found in a.py")], 3),
        a("m2", text("Done."), n=4),
    ])
    assert conv.find(path, "needle")["total"] == 0


def test_what_the_herd_said_and_the_answer_an_ask_got(home):
    msg = "[ghostherd message from lead → rd]\nPlease look at the flaky login test.\n"
    path = _file(home, [
        u(msg, 1),
        a("m1", use("t1", "Bash", command="herd ask qa --wait <<'HERD'\nCheck the login test\nHERD"), n=2),
        u([result("t1", "herd: asked qa (ask 1a2b3c4d)\nIt passes on both browsers.")], 3),
    ])
    r = conv.find(path, "login")
    assert [(h["role"], h["kind"], h["who"]) for h in r["hits"]] == [("user", "message", "lead"), ("herd", "ask", "")]
    assert r["hits"][1]["to"] == "qa" and "part" not in r["hits"][1]
    # Found only in the answer the ask got, shown under it in the room.
    ans = conv.find(path, "browsers")["hits"]
    assert len(ans) == 1 and ans[0]["part"] == "answer" and "both browsers" in ans[0]["snippet"]


def test_a_snippet_is_a_window_around_the_match_never_the_whole(home):
    long = "lorem ipsum " * 200 + "the NEEDLE is here\n\n\n   and   on " + "dolor sit " * 200
    path = _file(home, [u(long, 1)])
    s = conv.find(path, "needle")["hits"][0]["snippet"]
    assert s.startswith("…") and s.endswith("…")
    assert "NEEDLE" in s and "\n" not in s and "   " not in s
    assert len(s) <= conv.SNIPPET + 2
    assert s.index("NEEDLE") == conv.SNIPPET_LEAD + 1  # after the "…"
    hit = conv.find(path, "needle")["hits"][0]
    assert hit["head"] == long.strip()[: conv.HIT_HEAD] and hit["ts"] == at(1)


def test_a_hit_is_read_back_at_its_place_and_pages_around_it(home):
    path = _file(home, [u("msg %d%s" % (i, " needle" if i == 3 else ""), i) for i in range(10)])
    hit = conv.find(path, "needle")["hits"][0]
    assert hit["i"] == 3
    p = conv.read(path, 4, start=2)
    assert [e["text"] for e in p["entries"]] == ["msg 2", "msg 3 needle", "msg 4", "msg 5"]
    assert (p["start"], p["total"], p["earlier"], p["later"], p["before"]) == (2, 10, True, True, 4)
    tail = conv.read(path, 4, start=8)
    assert [e["text"] for e in tail["entries"]] == ["msg 8", "msg 9"] and tail["later"] is False
    head = conv.read(path, 4, start=0)
    assert head["earlier"] is False and head["entries"][0]["text"] == "msg 0"
    assert conv.read(path, 4, start=99)["entries"] == []
    assert "key" not in p["entries"][0]


def test_appended_lines_are_read_on_from_where_it_stopped(home, monkeypatch):
    path = _file(home, [u("needle %d" % i, i) for i in range(3)])
    first = conv.find(path, "needle")
    assert [h["i"] for h in first["hits"]] == [0, 1, 2]
    fed = []
    real = conv.READERS["claude"]

    def counted(records, thread):
        fed.append(1)
        real(records, thread)

    monkeypatch.setitem(conv.READERS, "claude", counted)
    _append(path, [a("m9", text("a needle answered"), n=10)])
    # A line still being written is left for the next read.
    with open(path, "a") as f:
        f.write('{"type": "user", "message": {"role": "user", "content": "needle half')
    r = conv.find(path, "needle")
    assert [h["i"] for h in r["hits"]] == [0, 1, 2, 3] and r["entries"] == 4
    assert len(fed) == 1  # only the new bytes went through the reader
    with open(path, "a") as f:
        f.write(' done"}, "timestamp": "%s"}\n' % at(11))
    r = conv.find(path, "needle")
    assert [h["snippet"] for h in r["hits"]][-1] == "needle half done" and r["entries"] == 5


def test_a_transcript_rewritten_is_read_again_from_the_start(home):
    path = _file(home, [u("needle one", 1), u("needle two", 2)])
    assert conv.find(path, "needle")["total"] == 2
    _file(home, [u("needle three", 1)])   # shorter: rewritten, not appended
    r = conv.find(path, "needle")
    assert r["total"] == 1 and r["hits"][0]["snippet"] == "needle three"
    # The same length but other bytes at its start: read again too.
    _file(home, [u("needle four", 1)])
    assert conv.find(path, "needle")["hits"][0]["snippet"] == "needle four"


def test_only_a_few_wholes_are_kept(home, monkeypatch):
    monkeypatch.setattr(conv, "WHOLE_MAX", 2)
    paths = [_file(home, [u("needle", 1)], name="t%d.jsonl" % n) for n in range(3)]
    for p in paths:
        conv.find(p, "needle")
    assert list(conv._wholes) == [str(Path(p).resolve()) for p in paths[1:]]


def test_load_earlier_past_the_tail_reads_the_whole(home, monkeypatch):
    "A page further back than the tail's 8 MB reaches is read from the whole."
    monkeypatch.setattr(conv, "TAIL_BYTES", 2000)
    monkeypatch.setattr(conv, "TAIL_MAX", 2000)
    path = _file(home, [u("prompt %d %s" % (i, "x" * 50), i) for i in range(100)])
    r = conv.read(path, 5, 90)
    assert [e["text"].split()[1] for e in r["entries"]] == ["5", "6", "7", "8", "9"]
    assert r["earlier"] is True
    first = conv.read(path, 5, 95)
    assert first["entries"][0]["text"].startswith("prompt 0 ") and first["earlier"] is False
    # The poll's tail is still the tail alone.
    assert len(conv.read(path, 200)["entries"]) < 100


def test_only_transcripts_are_searched(home):
    secret = home / ".ssh" / "id.jsonl"
    secret.parent.mkdir()
    secret.write_text(json.dumps(u("needle")) + "\n")
    for path in (str(secret), str(home / ".claude" / "projects" / "missing.jsonl"), ""):
        with pytest.raises(ValueError):
            conv.find(path, "needle")


def _rpc(method, params, role="admin"):
    return asyncio.run(handler.handle(JsonRpcRequest(jsonrpc="2.0", id=1, method=method, params=params), role))


def test_rpc_find_and_page(home):
    path = _file(home, [u("msg %d needle" % i, i) for i in range(5)])
    r = _rpc("herd_conversation_find", {"path": path, "query": "NEEDLE", "limit": 2})
    assert r.error is None
    assert r.result["total"] == 5 and r.result["capped"] is True and [h["i"] for h in r.result["hits"]] == [3, 4]
    p = _rpc("herd_conversation", {"path": path, "start": 3, "limit": 2})
    assert p.error is None and [e["text"] for e in p.result["entries"]] == ["msg 3 needle", "msg 4 needle"]
    assert _rpc("herd_conversation", {"path": path, "start": "x"}).error.message == "start must be a number"
    assert _rpc("herd_conversation_find", {"path": path}).error.message == "query is the words to find"
    assert _rpc("herd_conversation_find", {"path": path, "query": "x", "limit": "lots"}).error.message == "limit must be a number"
    # The cap is the sidecar's to keep.
    big = _rpc("herd_conversation_find", {"path": path, "query": "needle", "limit": 10 ** 6})
    assert big.error is None and len(big.result["hits"]) == 5
    assert _rpc("herd_conversation_find", {"path": str(home / "x.jsonl"), "query": "needle"}).error.message == "no such transcript"


def test_an_agent_cannot_search_a_conversation(home):
    "The same guard as reading one: the herd's own method, not an agent's."
    path = _file(home, [u("needle", 1)])
    for method in ("herd_conversation_find", "herd_conversation"):
        r = _rpc(method, {"path": path, "query": "needle", "start": 0}, role="agent")
        assert r.error and r.error.code == FORBIDDEN, method
