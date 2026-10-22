"""An ask closes on its taker's turn ending -- for a taker whose hooks
report, never on its screen reading idle (ask b77cf6dd).

Ask 18f57d20 was closed 23 minutes into claude-.emacs.d's turn: the user
had scrolled its view back, the spinner went out of it, the screen read
idle, and `settle' took the screen for an answer -- though the agent's
hook had said `working' and never `done'.  The screen below is the one it
was closed with."""

import json
import time
from datetime import datetime, timedelta, timezone

import pytest

import conversation as conv
from herd import HerdStore, reset_herd

# The screen ask 18f57d20 was closed with (2026-10-22 04:43:28Z): claude
# scrolled back, "Jump to bottom (click) ↓" over the view's last line,
# the footer's "esc to interrupt" cut off.
SCROLLED = """\
❯ [Image #69]
      3291 +        '<button type="button" class="ghost small"
           + data-act="prop-reject" data-id="' + esc(p.id) + '
           +">Reject</button></div>';
      3292 +    return Jump to bottom (click) ↓
                             ✔ Update installed · Restart to update
─────────────────────────────────────────────────────────────────────
❯
─────────────────────────────────────────────────────────────────────
  ⏵⏵ auto mode on (shift+tab to cycle) · ⧉ herd-gate · esc to inte…
"""

SNAP = [
    {"name": "lead", "kind": "claude", "state": "waiting", "project": "/p/", "role": "lead"},
    {"name": "rd", "kind": "claude", "state": "idle", "project": "/p/"},
    {"name": "grok-rd", "kind": "grok", "state": "idle", "project": "/p/"},
]
PROJ = ".claude/projects/-p"


@pytest.fixture
def home(tmp_path, monkeypatch):
    monkeypatch.setenv("HOME", str(tmp_path))
    yield tmp_path


@pytest.fixture
def h(tmp_path):
    reset_herd()
    store = HerdStore(tmp_path / "herd.sqlite")
    store.tick(sessions=SNAP)
    yield store
    store.close()


def _asked(h, taker="rd"):
    "The lead's ask, delivered to TAKER, which woke to it: a turn after delivery."
    ask = h.ask(taker, "build the proposals UI", from_name="lead", wait=True)
    out = h.tick(sessions=SNAP)
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in out["pending"]])
    time.sleep(0.002)
    h.report(taker, state="working")
    return ask


# When the module's records were made, for those made as it loads (the
# parametrized ones): their turn began then.
LOADED = datetime.now(timezone.utc)


def _ts(delta=0.0):
    return (datetime.now(timezone.utc) + timedelta(seconds=delta)).isoformat().replace("+00:00", "Z")


def _transcript(home, records, name="c1"):
    path = home / PROJ / (name + ".jsonl")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(json.dumps(r) + "\n" for r in records))
    return path


def user(content, delta=0.0, **kw):
    return {"type": "user", "message": {"role": "user", "content": content}, "timestamp": _ts(delta), **kw}


def assistant(stop, delta=0.0, content=None):
    return {"type": "assistant", "timestamp": _ts(delta),
            "message": {"role": "assistant", "stop_reason": stop,
                        "content": content or [{"type": "text", "text": "…"}]}}


def system(subtype, delta=0.0):
    return {"type": "system", "subtype": subtype, "timestamp": _ts(delta)}


TOOL_USE = [{"type": "tool_use", "id": "t1", "name": "Bash", "input": {"command": "pytest"}}]
TOOL_RESULT = [{"type": "tool_result", "tool_use_id": "t1", "content": "ok"}]


# ---- The case -------------------------------------------------------------------


def test_a_scrolled_back_screen_mid_turn_does_not_close_the_ask(h):
    ask = _asked(h)
    # Emacs reads the scrolled-back view as idle and settles: the hook said
    # `working' and nothing since, so the turn is on and the ask stays open.
    r = h.settle(ask["id"], screen=SCROLLED)
    assert (r["status"], r.get("turn_on")) == ("delivered", True)
    got = h.get_ask(ask["id"])
    assert (got["status"], got["reply"]) == ("delivered", None)
    # However long it reads idle: settled again, still open.
    assert h.settle(ask["id"], screen=SCROLLED)["status"] == "delivered"
    # Its turn ends by its hook, with its last words: that closes it.
    out = h.report("rd", state="done", last="Done -- the proposals UI is in.")
    assert out["closed_ask"] == ask["id"]
    got = h.get_ask(ask["id"])
    assert (got["reply"], got["auto_source"]) == ("Done -- the proposals UI is in.", "message")


def test_a_turn_end_without_words_lets_the_screen_answer(h):
    ask = _asked(h)
    h.report("rd", state="done")
    assert h.settle(ask["id"], screen="the page says Y")["status"] == "answered"
    assert h.get_ask(ask["id"])["auto_source"] == "screen"


def test_dying_still_fails_it_turn_or_no_turn(h):
    ask = _asked(h)
    assert h.settle(ask["id"], dead=True)["status"] == "failed"


def test_a_taker_with_no_hooks_still_settles_on_its_screen(h, monkeypatch):
    import herd as herd_mod
    monkeypatch.setattr(herd_mod, "DELIVER_GRACE", 0)
    ask = h.ask("rd", "x", from_name="lead")
    out = h.tick(sessions=SNAP)
    h.tick(sessions=SNAP, ack_ids=[p["id"] for p in out["pending"]])
    time.sleep(0.002)
    # No `working' ever reached the herd: the screen is all there is.
    assert h.settle(ask["id"], screen="its screen")["status"] == "answered"


def test_grok_interrupted_is_a_turn_end(h):
    ask = _asked(h, "grok-rd")
    assert h.settle(ask["id"], screen="›")["status"] == "delivered"
    h.report("grok-rd", state="auto")  # StopCancelled: the user interrupted it
    assert h.settle(ask["id"], screen="›")["status"] == "answered"


# ---- claude's transcript says when the hooks cannot ----------------------------


def test_the_transcript_ends_a_turn_its_hooks_never_said_ended(h, home):
    # The user interrupted it: claude fires no Stop hook for that.
    ask = _asked(h)
    p = _transcript(home, [user("build it", -5), assistant("tool_use", -4, TOOL_USE),
                           user(TOOL_RESULT, -3), user("[Request interrupted by user]", 1)])
    h.report("rd", cli="claude", conversation="c1", transcript=str(p))
    assert h.settle(ask["id"], screen=SCROLLED)["status"] == "answered"


def test_the_transcript_says_a_turn_runs_its_hooks_never_announced(h, home):
    # Its turn ended (Stop hook), then a background task's notice woke it --
    # no hook says so -- and it is at work again when the screen reads idle.
    ask = _asked(h)
    h.report("rd", state="done")
    p = _transcript(home, [assistant("end_turn", 0.5), system("turn_duration", 0.6),
                           user("<task-notification><status>completed</status></task-notification>", 2),
                           assistant("tool_use", 3, TOOL_USE)])
    h.report("rd", cli="claude", conversation="c1", transcript=str(p))
    r = h.settle(ask["id"], screen=SCROLLED)
    assert (r["status"], r.get("turn_on")) == ("delivered", True)
    # It finishes: the transcript says so, and the ask closes.
    with p.open("a") as f:
        for rec in (assistant("end_turn", 4), system("stop_hook_summary", 4.1), system("turn_duration", 4.2)):
            f.write(json.dumps(rec) + "\n")
    assert h.settle(ask["id"], screen="its screen")["status"] == "answered"


@pytest.mark.parametrize("records, want", [
    ([user("go", 1), assistant("end_turn", 2), system("turn_duration", 3)], True),
    ([user("go", 1), assistant("end_turn", 2)], True),            # its final message: the end
    ([user("go", 1), assistant("end_turn", 2), system("stop_hook_summary", 3)], True),
    ([user("go", 1), assistant("tool_use", 2, TOOL_USE)], False),
    ([user("go", 1), assistant("tool_use", 2, TOOL_USE), user(TOOL_RESULT, 3)], False),
    ([user("go", 1), assistant(None, 2)], False),                 # a reply being written
    ([user("go", 1), assistant("tool_use", 2, TOOL_USE),
      user([{"type": "text", "text": "[Request interrupted by user for tool use]"}], 3)], True),
    ([system("turn_duration", 1), user("next", 2)], False),       # a prompt: the next turn runs
    ([user("go", 1), assistant("end_turn", 2),
      user("<command-name>/model</command-name>", 3)], True),      # a slash command is no turn
    ([user("go", 1), assistant("end_turn", 2),
      dict(assistant("tool_use", 3, TOOL_USE), isSidechain=True)], True),  # a sub-agent's
    ([user("go", 1), assistant("end_turn", 2), user("meta", 3, isMeta=True)], True),
    ([user("go", -9), assistant("end_turn", -8)], None),          # before this turn began
    ([], None),
])
def test_turn_ended_reads_claudes_marks(home, records, want):
    p = _transcript(home, records)
    assert conv.turn_ended(str(p), LOADED.timestamp()) is want


def test_turn_ended_cannot_say_for_other_clis_or_no_file(home):
    agy = home / ".gemini/antigravity-cli/brain/b1/.system_generated/logs/transcript.jsonl"
    agy.parent.mkdir(parents=True)
    agy.write_text(json.dumps({"type": "PLANNER_RESPONSE"}) + "\n")
    assert conv.turn_ended(str(agy)) is None
    assert conv.turn_ended(str(home / PROJ / "none.jsonl")) is None


def test_turn_ended_reads_back_past_an_entry_bigger_than_its_window(home):
    # A tool's result carrying a screenshot, or a prompt with images pasted:
    # one entry of megabytes, larger than the first window read back.
    image = {"type": "image", "source": {"type": "base64", "media_type": "image/png",
                                         "data": "A" * (conv.TURN_TAIL * 2)}}
    big_result = [{"type": "tool_result", "tool_use_id": "t1", "content": [image]}]
    p = _transcript(home, [user("look", 1), assistant("tool_use", 2, TOOL_USE), user(big_result, 3)])
    assert conv.turn_ended(str(p)) is False
    # And after it, the turn's end -- with its hooks' summary after that.
    p = _transcript(home, [user([{"type": "text", "text": "see"}, image], 1),
                           assistant("end_turn", 2, [{"type": "text", "text": "x" * conv.TURN_TAIL}]),
                           system("stop_hook_summary", 3)])
    assert conv.turn_ended(str(p)) is True
