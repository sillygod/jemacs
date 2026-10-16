"""The meter: what a claude agent runs on, and how full its context is."""

import asyncio
import json
import os
from pathlib import Path

import pytest

import conversation as conv
import meter
from jsonrpc_handler import JsonRpcRequest, handler


@pytest.fixture
def home(tmp_path, monkeypatch):
    monkeypatch.setenv("HOME", str(tmp_path))
    meter._cache.clear()
    yield tmp_path
    meter._cache.clear()


def _file(home, records, cli=".claude/projects/-x-proj", name="abc.jsonl"):
    path = home / cli / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(json.dumps(r) + "\n" for r in records))
    return str(path)


def reply(model="claude-opus-5-5", fresh=2, made=1102, cached=832178, effort="xhigh", **kw):
    return {"type": "assistant", "effort": effort, "timestamp": "2026-10-14T07:05:32Z",
            "message": {"role": "assistant", "model": model, "content": [{"type": "text", "text": "hi"}],
                        "usage": {"input_tokens": fresh, "cache_creation_input_tokens": made,
                                  "cache_read_input_tokens": cached, "output_tokens": 1330}}, **kw}


def said(text):
    return {"type": "user", "message": {"role": "user",
                                        "content": "<local-command-stdout>%s</local-command-stdout>" % text}}


def prompt(text):
    return {"type": "user", "message": {"role": "user", "content": text}}


def test_the_last_reply_says_what_it_ran_on(home):
    path = _file(home, [reply(model="claude-sonnet-5-5", cached=10, effort="high"), prompt("go on"), reply()])
    m = meter.read(path)
    assert m == {"model": "Opus 5.5", "effort": "xhigh", "tokens": 833282, "window": 1_000_000,
                 "pending": False, "compacted": False, "ts": "2026-10-14T07:05:32Z"}


def test_what_is_not_a_reply_of_its_own_is_passed_over(home):
    "An API error's synthetic message, a subagent's: not what the agent runs on."
    path = _file(home, [reply(cached=500),
                        reply(model="<synthetic>", cached=0),
                        reply(model="claude-haiku-5-5", cached=7, isSidechain=True),
                        {"type": "assistant", "message": {"role": "assistant", "model": "claude-opus-5-5"}}])
    m = meter.read(path)
    assert (m["model"], m["tokens"]) == ("Opus 5.5", 1604)


@pytest.mark.parametrize("model,size", [
    ("claude-opus-5-5", 1_000_000), ("claude-opus-4-7", 1_000_000), ("claude-opus-4-6", 200_000),
    ("claude-opus-4-6[1m]", 1_000_000), ("Opus 4.6 (1M context)", 1_000_000),
    ("claude-sonnet-5", 1_000_000), ("claude-sonnet-4-6", 200_000), ("claude-sonnet-4-5-20250929", 200_000),
    ("claude-haiku-5-5", 1_000_000), ("claude-haiku-4-5-20251001", 200_000),
    ("claude-fable-5-1", 1_000_000), ("Fable 5", 1_000_000), ("Sonnet 5.5", 1_000_000),
    ("gpt-5", None), ("", None)])
def test_the_window_by_the_documentation(model, size):
    assert meter.window(model) == size


@pytest.mark.parametrize("model,shown", [
    ("claude-opus-5-5", "Opus 5.5"), ("claude-haiku-4-5-20251001", "Haiku 4.5"),
    ("claude-sonnet-5", "Sonnet 5"), ("claude-fable-5-1", "Fable 5.1"), ("something-else", "something-else")])
def test_a_model_by_its_pickers_name(model, shown):
    assert meter.name(model) == shown


def test_more_than_the_window_is_a_1m_variant(home):
    "opus-4-6[1m] answers as claude-opus-4-6: what it was given says which."
    m = meter.read(_file(home, [reply(model="claude-opus-4-6", cached=300_000)]))
    assert m["window"] == 1_000_000


def test_a_switch_since_is_the_model_the_next_reply_is_on(home):
    path = _file(home, [reply(), prompt("<command-name>/model</command-name>"),
                        said("Set model to `Sonnet 5.5` for this session only with `high` effort")])
    m = meter.read(path)
    assert (m["model"], m["effort"], m["pending"], m["window"], m["tokens"]) == \
        ("Sonnet 5.5", "high", True, 1_000_000, 833282)


def test_a_switch_without_its_effort_does_not_keep_the_old_one(home):
    "Said without an effort, the new model's own default: not known here."
    m = meter.read(_file(home, [reply(), said("Set model to `Haiku 4.5` for this session only")]))
    assert (m["model"], m["effort"], m["pending"], m["window"]) == ("Haiku 4.5", "", True, 200_000)


def test_the_latest_switch_wins_and_kept_changes_nothing(home):
    path = _file(home, [reply(), said("Set model to `Haiku 5.5` for this session only with `low` effort"),
                        said("Set model to `Fable 5.1` for this session only with `max` effort"),
                        said("Kept model as `Fable 5.1`")])
    m = meter.read(path)
    assert (m["model"], m["effort"]) == ("Fable 5.1", "max")
    assert meter.read(_file(home, [reply(), said("Kept model as `Opus 5.5`")], name="kept.jsonl"))["pending"] is False


def test_an_effort_set_since(home):
    m = meter.read(_file(home, [reply(), said("Set effort level to `medium`")]))
    assert (m["model"], m["effort"], m["pending"]) == ("Opus 5.5", "medium", True)


def test_a_switch_before_the_last_reply_is_history(home):
    m = meter.read(_file(home, [said("Set model to `Sonnet 5.5` for this session only"), reply()]))
    assert (m["model"], m["pending"]) == ("Opus 5.5", False)


def test_compacted_since_the_last_reply(home):
    path = _file(home, [reply(), {"type": "system", "subtype": "compact_boundary",
                                  "compactMetadata": {"trigger": "manual", "preTokens": 833282}}])
    m = meter.read(path)
    assert (m["tokens"], m["compacted"], m["model"]) == (None, True, "Opus 5.5")


def test_no_reply_yet_and_other_clis(home):
    assert meter.read(_file(home, [prompt("hello")])) == {}
    agy = _file(home, [reply()], cli=".gemini/antigravity-cli/brain/x/.system_generated/logs",
                name="transcript.jsonl")
    assert meter.read(agy) == {}


def test_only_transcripts_are_read(home):
    secret = home / ".ssh" / "id.jsonl"
    secret.parent.mkdir()
    secret.write_text(json.dumps(reply()) + "\n")
    for path in (str(secret), str(home / ".claude" / "projects" / "missing.jsonl"), ""):
        with pytest.raises(ValueError):
            meter.read(path)


def test_the_last_reply_is_looked_for_past_a_big_tool_output(home, monkeypatch):
    monkeypatch.setattr(meter, "TAIL_BYTES", 2000)
    path = _file(home, [reply(), {"type": "user", "message": {"role": "user", "content": [
        {"type": "tool_result", "tool_use_id": "t", "content": "y" * 20000}]}}])
    assert meter.read(path)["tokens"] == 833282
    meter._cache.clear()
    monkeypatch.setattr(conv, "TAIL_MAX", 2000)
    assert meter.read(path) == {}


def test_unchanged_transcript_is_not_read_again(home, monkeypatch):
    path = _file(home, [reply()])
    first = meter.read(path)
    reads = []
    monkeypatch.setattr(conv, "_tail", lambda p, n: reads.append(p) or ([], False))
    assert meter.read(path) is first and reads == []
    with open(path, "a") as f:
        f.write(json.dumps(reply(cached=1)) + "\n")
    os.utime(path, ns=(1, 2))
    meter.read(path)
    assert len(reads) == 1


def test_rpc(home):
    path = _file(home, [reply()])

    def call(params):
        return asyncio.run(handler.handle(JsonRpcRequest(jsonrpc="2.0", id=1, method="herd_meter", params=params)))

    r = call({"path": path})
    assert r.error is None and r.result["tokens"] == 833282
    assert call({"path": str(home / "x.jsonl")}).error.message == "no such transcript"


# ---- grok and agy: context from where each CLI keeps it --------------------

def _grok(home, signals, name="chat_history.jsonl"):
    d = home / ".grok/sessions/enc/sid"
    d.mkdir(parents=True, exist_ok=True)
    (d / name).write_text("")  # located, but holds no tokens
    if signals is not None:
        (d / "signals.json").write_text(json.dumps(signals))
    return str(d / name)


def _agy(home, records):
    d = home / ".gemini/antigravity-cli/brain/bid/.system_generated/logs"
    d.mkdir(parents=True, exist_ok=True)
    p = d / "transcript_full.jsonl"
    p.write_text("".join(json.dumps(r) + "\n" for r in records))
    return str(p)


def _planner(inp, cached, step, src="MODEL"):
    return {"type": "PLANNER_RESPONSE", "source": src, "input_tokens": inp,
            "cache_read_tokens": cached, "created_at": "2026-10-16T00:%02d:00Z" % step, "step_index": step}


def test_grok_context_from_signals_with_a_window(home):
    "grok keeps its context in signals.json beside the conversation: a bar."
    path = _grok(home, {"contextTokensUsed": 163889, "contextWindowTokens": 256000, "contextWindowUsage": 64})
    assert meter.read(path) == {"model": "", "effort": "", "tokens": 163889, "window": 256000,
                                "pending": False, "compacted": False, "ts": None}


def test_grok_without_a_window_shows_the_number_only(home):
    path = _grok(home, {"contextTokensUsed": 5000})
    m = meter.read(path)
    assert (m["tokens"], m["window"]) == (5000, None)


def test_grok_with_no_signals_or_no_count_is_empty(home):
    assert meter.read(_grok(home, None)) == {}
    assert meter.read(_grok(home, {"compactionCount": 8})) == {}


def test_grok_caches_on_signals_not_the_conversation(home):
    "The tokens live in signals.json, so its change is what must re-read."
    path = _grok(home, {"contextTokensUsed": 100, "contextWindowTokens": 256000})
    assert meter.read(path)["tokens"] == 100
    sig = Path(path).with_name("signals.json")
    sig.write_text(json.dumps({"contextTokensUsed": 200, "contextWindowTokens": 256000}))
    os.utime(sig, ns=(sig.stat().st_size + 1, sig.stat().st_mtime_ns + 1000))
    assert meter.read(path)["tokens"] == 200


def test_agy_context_from_the_last_planner_reply(home):
    "agy's prompt size -- input + cache-read -- from its last model reply; no window."
    path = _agy(home, [{"type": "USER_INPUT", "content": "go", "created_at": "t0"},
                       _planner(3002, 164600, 56), _planner(2927, 186298, 67)])
    assert meter.read(path) == {"model": "", "effort": "", "tokens": 2927 + 186298, "window": None,
                                "pending": False, "compacted": False, "ts": "2026-10-16T00:67:00Z"}


def test_agy_without_a_model_reply_is_empty(home):
    "A user turn, or a planner turn that is not the model's, carries no tokens."
    path = _agy(home, [{"type": "USER_INPUT", "content": "go", "created_at": "t0"},
                       _planner(10, 20, 1, src="PLANNER")])
    assert meter.read(path) == {}


def test_agy_rpc_reports_the_number(home):
    path = _agy(home, [_planner(1000, 2000, 1)])
    r = asyncio.run(handler.handle(JsonRpcRequest(jsonrpc="2.0", id=1, method="herd_meter", params={"path": path})))
    assert r.error is None and r.result == {"model": "", "effort": "", "tokens": 3000, "window": None,
                                            "pending": False, "compacted": False, "ts": "2026-10-16T00:01:00Z"}
