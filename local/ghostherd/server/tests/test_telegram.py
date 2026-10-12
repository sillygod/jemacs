from herd import HerdStore, reset_herd
from telegram import TelegramBot, format_herd, session_keyboard


def _bot(tmp_path, chats=(99,)):
    reset_herd()
    herd = HerdStore(tmp_path / "herd.sqlite")
    herd.tick(
        sessions=[
            {
                "name": "agy-commit",
                "kind": "agy",
                "state": "blocked",
                "reason": "proceed?",
                "project": "/tmp",
            },
            {"name": "grok-dev", "kind": "grok", "state": "working"},
        ],
        ack_ids=[],
    )
    return TelegramBot("tok", chats, herd), herd


def test_unknown_chat_is_ignored(tmp_path):
    bot, _ = _bot(tmp_path)
    assert bot.handle_message(1, "/herd") is None
    assert bot.handle_callback(1, "h") is None
    reset_herd()


def test_herd_lists_sessions(tmp_path):
    bot, _ = _bot(tmp_path)
    msg = bot.handle_message(99, "/herd")
    assert "Herd  2" in msg["text"]
    assert "⚠1" in msg["text"]
    labels = [
        btn["text"]
        for row in msg["reply_markup"]["inline_keyboard"]
        for btn in row
    ]
    assert any("agy-commit" in t for t in labels)
    reset_herd()


def test_answer_only_when_blocked(tmp_path):
    bot, herd = _bot(tmp_path)
    grok = herd.session("grok-dev")
    agy = herd.session("agy-commit")
    refused = bot.handle_callback(99, f"n:{grok['short']}:1")
    assert "not blocked" in refused["text"]
    ok = bot.handle_callback(99, f"n:{agy['short']}:2")
    assert ok["text"].startswith("answer")
    cmds = herd.tick(
        sessions=[
            {"name": "agy-commit", "state": "blocked"},
            {"name": "grok-dev", "state": "working"},
        ],
        ack_ids=[],
    )["commands"]
    assert cmds[0]["op"] == "answer"
    assert cmds[0]["args"]["n"] == 2
    reset_herd()


def test_prompt_next_message(tmp_path):
    bot, herd = _bot(tmp_path)
    agy = herd.session("agy-commit")
    bot.handle_callback(99, f"p:{agy['short']}")
    queued = bot.handle_message(99, "please commit")
    assert "queued prompt" in queued["text"]
    mail = herd.tick(
        sessions=[{"name": "agy-commit", "state": "idle"}],
        ack_ids=[],
    )["pending"]
    assert mail[0]["from"] == "telegram"
    assert mail[0]["body"] == "please commit"
    reset_herd()


def test_blocked_card_has_answer_keys(tmp_path):
    """A key per choice of a menu Emacs read; none for one it could not,
    for a number picked blind could be any of its choices -- abort stays."""
    bot, herd = _bot(tmp_path)
    data = lambda s: [btn["callback_data"] for row in session_keyboard(s)["inline_keyboard"] for btn in row]
    agy = herd.session("agy-commit")
    assert not any(d.startswith("n:") for d in data(agy)) and f"ab:{agy['short']}" in data(agy)
    herd.tick(sessions=[{"name": "agy-commit", "kind": "agy", "state": "blocked", "prompt": MENU},
                        {"name": "grok-dev", "kind": "grok", "state": "working"}])
    agy = herd.session("agy-commit")
    assert [d for d in data(agy) if d.startswith("n:")] == [f"n:{agy['short']}:{n}" for n in (1, 2, 3, 4)]
    grok = herd.session("grok-dev")
    grok_data = [
        btn["callback_data"]
        for row in session_keyboard(grok)["inline_keyboard"]
        for btn in row
    ]
    assert not any(d.startswith("n:") for d in grok_data)
    reset_herd()


def test_format_herd_empty():
    text = format_herd([])
    assert "no sessions" in text


def test_screen_uses_snapshot_not_queue(tmp_path):
    bot, herd = _bot(tmp_path)
    herd.tick(
        sessions=[
            {
                "name": "agy-commit",
                "kind": "agy",
                "state": "blocked",
                "reason": "proceed?",
                "screen": "Do you want to proceed?\n❯ 1. Yes",
            }
        ],
        ack_ids=[],
    )
    agy = herd.session("agy-commit")
    msg = bot.handle_callback(99, f"sc:{agy['short']}")
    assert "Do you want to proceed?" in msg["text"]
    assert "agy-commit" in msg["text"]
    assert not msg["text"].startswith("screen →")
    leftover = herd.tick(
        sessions=[{"name": "agy-commit", "state": "blocked"}],
        ack_ids=[],
    )["commands"]
    assert leftover == []
    reset_herd()


def test_screen_without_cache_enqueues(tmp_path):
    bot, herd = _bot(tmp_path)
    grok = herd.session("grok-dev")
    msg = bot.handle_callback(99, f"sc:{grok['short']}")
    assert msg["text"].startswith("screen →")
    cmds = herd.tick(
        sessions=[{"name": "grok-dev", "state": "working"}],
        ack_ids=[],
    )["commands"]
    assert cmds[0]["op"] == "screen"
    reset_herd()


def test_explain_uses_snapshot(tmp_path):
    bot, herd = _bot(tmp_path)
    agy = herd.session("agy-commit")
    msg = bot.handle_callback(99, f"ex:{agy['short']}")
    assert "blocked" in msg["text"]
    assert "proceed?" in msg["text"]
    leftover = herd.tick(
        sessions=[{"name": "agy-commit", "state": "blocked"}],
        ack_ids=[],
    )["commands"]
    assert leftover == []
    reset_herd()


# ---- Pushes -----------------------------------------------------------

import asyncio
from datetime import datetime, timedelta, timezone

from telegram import REPORT_WAIT, HELP

ROOM = "http://box.tail0.ts.net/open/ghostherd#room/chat/%2Ftmp"


class FakeTelegram:
    """sendMessage, answered with a message id each; or failing."""

    def __init__(self, fail=False):
        self.sent = []
        self.fail = fail

    async def post(self, url, json=None):
        self.sent.append((url, json))
        fail = self.fail

        class Resp:
            def raise_for_status(self):
                if fail:
                    raise RuntimeError("telegram is down")

            def json(self):
                return {"ok": True, "result": {"message_id": 1000 + len(outer.sent)}}

        outer = self
        return Resp()


def _push(herd, **push):
    herd.tick(
        sessions=[
            {"name": "agy-commit", "kind": "agy", "state": "blocked", "reason": "proceed?", "project": "/tmp"},
            {"name": "lead-tmp", "kind": "claude", "state": "done", "project": "/tmp", "role": "lead"},
        ],
        pushes=[push],
    )
    return herd.unsent_pushes()[-1]


def test_tick_keeps_what_emacs_pushes(tmp_path):
    _, herd = _bot(tmp_path)
    herd.tick(sessions=[], pushes=[{"kind": "blocked", "session": "a"}, {"kind": "report"}, "x", {"session": "b"}])
    got = herd.unsent_pushes()
    assert [(p["kind"], p["session"]) for p in got] == [("blocked", "a")]
    assert got[0]["id"] and got[0]["created_at"]
    herd.mark_pushes_sent([got[0]["id"]])
    assert herd.unsent_pushes() == []
    reset_herd()


def test_a_blocked_agent_pushed_with_its_screen_and_room(tmp_path):
    bot, herd = _bot(tmp_path)
    push = _push(herd, kind="blocked", session="agy-commit", project="/tmp/shop/", url=ROOM, reason="Allow npm test?")
    text, markup = bot.prepare_push(push, 0)
    assert text.startswith("⚠ agy-commit is blocked · shop") and "Allow npm test?" in text
    short = herd.session("agy-commit")["short"]
    rows = markup["inline_keyboard"]
    # Its menu not read: no number to pick blind.
    assert rows[0] == [{"text": "screen", "callback_data": f"sc:{short}"}]
    assert rows[1] == [{"text": "Open the room", "url": ROOM}]
    # Answered at the desk meanwhile, or gone: not pushed.
    herd.tick(sessions=[{"name": "agy-commit", "state": "working"}])
    assert bot.prepare_push(push, 0) == ("", None)
    reset_herd()


def test_a_leads_report_carries_its_words_once_they_come(tmp_path):
    bot, herd = _bot(tmp_path)
    herd.report(session="lead-tmp", last="Old words, from the turn before")
    import time
    time.sleep(0.01)
    began = datetime.now(timezone.utc).timestamp()
    push = _push(herd, kind="report", session="lead-tmp", project="/tmp/shop", url="", since=began)
    # Its own words not in yet: it waits, then goes without them.
    assert bot.prepare_push(push, began + 1) is None
    text, markup = bot.prepare_push(push, began + REPORT_WAIT + 60)
    assert "(its words have not come in)" in text and "Old words" not in text
    assert markup is None
    time.sleep(0.01)
    herd.report(session="lead-tmp", state="done", last="Shipped X; qa passed both rounds.")
    text, _ = bot.prepare_push(push, began + 1)
    assert text.startswith("✓ lead-tmp · shop") and "Shipped X; qa passed both rounds." in text
    assert "Reply to tell the lead" in text
    reset_herd()


def test_a_used_up_agent_is_offered_its_relay(tmp_path):
    bot, herd = _bot(tmp_path)
    push = _push(herd, kind="used-up", session="lead-tmp", project="/tmp/shop/", url=ROOM,
                 to="grok", reason="its claude usage (5h) is used up, until 15:00", **{"from": "claude"})
    text, markup = bot.prepare_push(push, 0)
    assert text.splitlines()[0] == "⌛ lead-tmp stopped · shop"
    assert "Its claude usage (5h) is used up, until 15:00." in text
    assert "Relay it to grok: a grok agent takes its name and its work." in text
    short = herd.session("lead-tmp")["short"]
    rows = markup["inline_keyboard"]
    assert rows[0] == [{"text": "Relay to grok", "callback_data": f"rl:{short}:grok"},
                       {"text": "screen", "callback_data": f"sc:{short}"}]
    assert rows[1] == [{"text": "Open the room", "url": ROOM}]
    # The tap: Emacs relays it.
    assert bot.handle_callback(99, f"rl:{short}:grok")["text"] == "relay → lead-tmp"
    snap = [{"name": "lead-tmp", "kind": "claude", "state": "idle", "project": "/tmp"}]
    cmd = herd.tick(sessions=snap)["commands"][-1]
    assert (cmd["op"], cmd["session"], cmd["args"]) == ("relay", "lead-tmp", {"kind": "grok"})
    assert bot.handle_callback(99, f"rl:{short}:Grok Build")["text"] == "bad relay"
    assert bot.handle_callback(99, "rl:zz:grok")["text"] == "unknown session"
    # Relayed meanwhile -- its name on grok now -- or gone: not pushed.
    herd.tick(sessions=[dict(snap[0], kind="grok", state="working")])
    assert bot.prepare_push(push, 0) == ("", None)
    herd.tick(sessions=[])
    assert bot.prepare_push(push, 0) == ("", None)
    reset_herd()


def test_a_failed_ask_says_what_and_why(tmp_path):
    bot, herd = _bot(tmp_path)
    push = _push(herd, kind="ask-failed", session="lead-tmp", project="/tmp/shop", url=ROOM,
                 ask="d725ee06", to="qa", error="error: unrecognized arguments: -", head="Test the login flow")
    text, markup = bot.prepare_push(push, 0)
    assert text.splitlines()[0] == "✖ lead-tmp's ask to qa failed · shop"
    assert "error: unrecognized arguments: -" in text and "It asked: Test the login flow" in text
    assert markup == {"inline_keyboard": [[{"text": "Open the room", "url": ROOM}]]}
    assert bot.prepare_push(dict(push, kind="nonsense"), 0) == ("", None)
    reset_herd()


def test_pushes_go_to_every_chat_and_replies_find_their_lead(tmp_path):
    bot, herd = _bot(tmp_path, chats=(99, 77))
    herd.report(session="lead-tmp", state="done", last="Done.")
    _push(herd, kind="report", session="lead-tmp", project="/tmp/shop/", url=ROOM, since=0)
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert [(u, p["chat_id"]) for u, p in tg.sent] == [("https://t/sendMessage", 77), ("https://t/sendMessage", 99)]
    assert all(p["reply_markup"]["inline_keyboard"][0][0]["url"] == ROOM for _, p in tg.sent)
    assert herd.unsent_pushes() == []
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert len(tg.sent) == 2
    # A reply to it, from its chat: to the lead of its project.
    said = bot.handle_message(99, "go on with round 3", reply_to=1002)
    assert said["text"] == "→ the lead of shop"
    cmds = herd.tick(sessions=[])["commands"]
    assert [(c["op"], c["args"], c["chat_id"]) for c in cmds] == [
        ("lead-say", {"project": "/tmp/shop/", "body": "go on with round 3"}, 99)]
    # Another chat's message id, or a message no push sent: as before.
    assert bot.handle_message(77, "hello", reply_to=1002)["text"] == "unknown. /herd or /help"
    assert bot.handle_message(99, "hello", reply_to=5)["text"] == "unknown. /herd or /help"
    assert "Reply to one to tell" in HELP
    reset_herd()


def test_a_push_waits_and_a_failed_send_is_tried_again(tmp_path):
    bot, herd = _bot(tmp_path)
    _push(herd, kind="report", session="lead-tmp", project="/tmp", url="", since=datetime.now(timezone.utc).timestamp() + 5)
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert tg.sent == [] and len(herd.unsent_pushes()) == 1
    herd.mark_pushes_sent([herd.unsent_pushes()[0]["id"]])
    _push(herd, kind="blocked", session="agy-commit", project="/tmp", url="", reason="?")
    down = FakeTelegram(fail=True)
    try:
        asyncio.run(bot._flush_pushes(down, "https://t/{method}"))
    except RuntimeError:
        pass
    assert len(herd.unsent_pushes()) == 1
    reset_herd()


def test_a_test_push(tmp_path):
    bot, herd = _bot(tmp_path)
    push = _push(herd, kind="test", session="ghostherd", project="", url="")
    text, markup = bot.prepare_push(push, 0)
    assert text.startswith("The herd reaches you here.") and markup is None
    reset_herd()


def test_the_bot_pushes_while_it_waits_for_you(tmp_path, monkeypatch):
    "Its long poll holds for 25 seconds; pushes go out on their own meanwhile."
    import telegram

    bot, herd = _bot(tmp_path)
    _push(herd, kind="blocked", session="agy-commit", project="/tmp", url="", reason="?")
    posted = []

    class Client:
        def __init__(self, *a, **k):
            pass

        async def __aenter__(self):
            return self

        async def __aexit__(self, *a):
            return False

        async def get(self, url, params=None):
            await asyncio.sleep(25)

        async def post(self, url, json=None):
            posted.append((url.rsplit("/", 1)[-1], json))

            class R:
                def raise_for_status(self):
                    pass

                def json(self):
                    return {"result": {"message_id": 1}}

            return R()

    monkeypatch.setattr(telegram.httpx, "AsyncClient", Client)
    monkeypatch.setattr(telegram, "PUSH_EVERY", 0.05)

    async def go():
        stop = asyncio.Event()
        task = asyncio.create_task(bot.run(stop))
        for _ in range(40):
            if posted:
                break
            await asyncio.sleep(0.05)
        stop.set()
        task.cancel()
        try:
            await task
        except asyncio.CancelledError:
            pass

    asyncio.run(go())
    assert [m for m, _ in posted] == ["sendMessage"] and posted[0][1]["chat_id"] == 99
    assert herd.unsent_pushes() == []
    reset_herd()


MENU = {"question": "Requesting permission for:\ngrep -rn respawn local/\nRun this command?",
        "options": [{"n": 1, "label": "Yes, run command"},
                    {"n": 2, "label": "Yes, and always allow in this conversation for commands that start with"},
                    {"n": 3, "label": "Yes, and always allow for commands that start with 'grep' (Persist to"},
                    {"n": 4, "label": "No, cancel"}]}


def test_a_blocked_agent_pushed_with_its_menu_in_words(tmp_path):
    bot, herd = _bot(tmp_path)
    herd.tick(sessions=[{"name": "agy-commit", "kind": "agy", "state": "blocked", "reason": "Command — Requesting permission for:",
                         "project": "/tmp", "prompt": MENU}],
              pushes=[{"kind": "blocked", "session": "agy-commit", "project": "/tmp/shop", "url": ROOM,
                       "reason": "Command — Requesting permission for:"}])
    push = herd.unsent_pushes()[-1]
    text, markup = bot.prepare_push(push, 0)
    assert text == ("⚠ agy-commit is blocked · shop\n\n" + MENU["question"] + "\n\n"
                    "1. Yes, run command\n"
                    "2. Yes, and always allow in this conversation for commands that start with\n"
                    "3. Yes, and always allow for commands that start with 'grep' (Persist to\n"
                    "4. No, cancel")
    short = herd.session("agy-commit")["short"]
    rows = markup["inline_keyboard"]
    assert [r[0]["callback_data"] for r in rows[:4]] == [f"n:{short}:{n}" for n in (1, 2, 3, 4)]
    assert rows[0][0]["text"] == "1 · Yes, run command" and rows[3][0]["text"] == "4 · No, cancel"
    assert len(rows[1][0]["text"]) == 60 and rows[1][0]["text"].endswith("…")
    assert rows[4] == [{"text": "screen", "callback_data": f"sc:{short}"}]
    assert rows[5] == [{"text": "Open the room", "url": ROOM}]
    # Its card from /herd: the same menu, and abort.
    card = bot.handle_callback(99, f"o:{short}")
    assert MENU["question"] in card["text"] and "4. No, cancel" in card["text"]
    labels = [r[0]["text"] for r in card["reply_markup"]["inline_keyboard"]]
    assert labels[2:7] == ["1 · Yes, run command", rows[1][0]["text"], rows[2][0]["text"], "4 · No, cancel", "abort"]
    # No longer blocked: no menu, whatever the last one was.
    herd.tick(sessions=[{"name": "agy-commit", "kind": "agy", "state": "working", "prompt": MENU}])
    card = bot.handle_callback(99, f"o:{short}")
    assert "No, cancel" not in card["text"] and "· " not in str(card["reply_markup"])
    reset_herd()
