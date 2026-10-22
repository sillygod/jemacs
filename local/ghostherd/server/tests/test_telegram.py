from herd import HerdStore, reset_herd
from telegram import TelegramBot, format_herd, herd_keyboard, push_keyboard, session_keyboard


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


def test_a_waiting_agent_is_neither_at_work_nor_idle():
    "Its turn over, its sub-agents still out."
    herd = [{"name": "rd", "state": "waiting", "short": "r1"}, {"name": "qa", "state": "working", "short": "q1"}]
    assert format_herd(herd) == "Herd  2   ⟳1  ◷1"
    assert herd_keyboard(herd)["inline_keyboard"][0][0]["text"] == "◷ rd"


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


def test_a_lead_started_for_a_handoff_says_for_whom(tmp_path):
    bot, herd = _bot(tmp_path)
    push = _push(herd, kind="lead-started", session="lead-tmp", project="/tmp/shop/", url=ROOM,
                 by="lead-ghost", head="The login API renamed token", **{"from": "~/src/ghost/"})
    text, markup = bot.prepare_push(push, 0)
    assert text.splitlines()[0] == "▶ started lead-tmp · shop"
    assert "For a handoff from lead-ghost in ghost: work found there" in text
    assert "It asks: The login API renamed token" in text and "Reply to tell the lead" in text
    assert markup == {"inline_keyboard": [[{"text": "Open the room", "url": ROOM}]]}
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
    # Another chat's message id, or a message no push sent: free-form
    # words, for primary, the herd-wide lead.
    assert bot.handle_message(77, "hello", reply_to=1002)["text"].startswith("→ primary")
    assert bot.handle_message(99, "hello", reply_to=5)["text"].startswith("→ primary")
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


PAGE = "http://box.tail0.ts.net/open/ghostherd#agents"


def _urls(msg):
    return [b for row in msg["reply_markup"]["inline_keyboard"] for b in row if "url" in b]


def test_herd_links_the_page_and_a_card_its_room(tmp_path):
    """/herd opens the herd page on the phone, and a session's card its
    room, under the addresses Emacs gave on its last tick; given none,
    or one Telegram would refuse with the whole message, no button."""
    bot, herd = _bot(tmp_path)
    herd.tick(sessions=[{"name": "agy-commit", "kind": "agy", "state": "idle", "project": "/tmp/", "room": ROOM},
                        {"name": "grok-dev", "kind": "grok", "state": "working"}],
              page_url=PAGE)
    rows = bot.handle_message(99, "/herd")["reply_markup"]["inline_keyboard"]
    assert rows[0] == [{"text": "Open the herd page", "url": PAGE}]
    assert len(_urls(bot.handle_message(99, "/herd"))) == 1
    assert _urls(bot.handle_callback(99, "h")) == [{"text": "Open the herd page", "url": PAGE}]
    short = herd.session("agy-commit")["short"]
    assert herd.session("agy-commit")["room"] == ROOM
    for tap in (f"o:{short}", f"m:{short}"):
        assert _urls(bot.handle_callback(99, tap)) == [{"text": "Open the room", "url": ROOM}]
    assert _urls(bot.handle_callback(99, "o:" + herd.session("grok-dev")["short"])) == []
    for url in ("", None, "http://box/open/ghostherd", "javascript:alert(1)", "file:///etc/passwd", "http://[::1/x",
                "javascript://box.tail0.ts.net/%0Aalert(1)", "ftp://box.tail0.ts.net/open/ghostherd"):
        herd.tick(sessions=[{"name": "agy-commit", "state": "idle", "room": url}], page_url=url)
        assert herd.page_url() == (url or "")
        assert _urls(bot.handle_message(99, "/herd")) == []
        assert _urls(bot.handle_callback(99, f"o:{short}")) == []
        assert push_keyboard({"kind": "report", "session": "agy-commit", "url": url}) is None
    reset_herd()


# ---- Markdown to Telegram HTML ----------------------------------------

from telegram import md_to_html, to_html, _strip_html, _esc

# The report from the screenshot: **原因：** and friends against CJK, a
# backtick commit, "- " bullets, plain paragraphs.
REPORT = (
    "捲軸顏色改好了（`8f162b2`），網頁已經重新載入。\n\n"
    "**原因：** ghostherd 頁面和 PR 頁面（pr-view）共用同一份基本樣式。\n\n"
    "**修法：** 只改這一份共用樣式，兩邊一致。\n\n"
    "- 標明是深色頁面。\n"
    "- 捲軸用主題現有的線條顏色，滑鼠移上去時變亮。\n\n"
    "**測試：** 新的捲軸測試 13 個全過。"
)
REPORT_HTML = (
    "捲軸顏色改好了（<code>8f162b2</code>），網頁已經重新載入。\n\n"
    "<b>原因：</b> ghostherd 頁面和 PR 頁面（pr-view）共用同一份基本樣式。\n\n"
    "<b>修法：</b> 只改這一份共用樣式，兩邊一致。\n\n"
    "• 標明是深色頁面。\n"
    "• 捲軸用主題現有的線條顏色，滑鼠移上去時變亮。\n\n"
    "<b>測試：</b> 新的捲軸測試 13 個全過。"
)


def test_a_report_markdown_renders_as_telegram_html():
    assert md_to_html(REPORT) == REPORT_HTML
    # The CJK markers that came through raw in the screenshot are gone.
    assert "**" not in md_to_html(REPORT) and "`" not in md_to_html(REPORT)


def test_markdown_covers_the_inline_and_block_cases():
    # & < > escaped.
    assert md_to_html("a & b < c > d") == "a &amp; b &lt; c &gt; d"
    # bold/italic, both markers; __/_ leave snake_case alone, ** hugs CJK.
    assert md_to_html("**b** __b__ *i* _i_") == "<b>b</b> <b>b</b> <i>i</i> <i>i</i>"
    assert md_to_html("foo_bar_baz stays") == "foo_bar_baz stays"
    assert md_to_html("**原因：**對") == "<b>原因：</b>對"
    # spaced-out asterisks are arithmetic, not italics.
    assert md_to_html("2 * 3 * 4") == "2 * 3 * 4"
    # code hides its contents from the converter and escapes them.
    assert md_to_html("`a <b> & *c*`") == "<code>a &lt;b&gt; &amp; *c*</code>"
    # links, with the url's & escaped in the attribute.
    assert md_to_html("[x](https://h.co/a?b=1&c=2)") == '<a href="https://h.co/a?b=1&amp;c=2">x</a>'
    # headings -> bold; "- "/"* " -> bullets; numbered left as typed.
    assert md_to_html("## Title") == "<b>Title</b>"
    assert md_to_html("- one\n* two") == "• one\n• two"
    assert md_to_html("1. one\n2. two") == "1. one\n2. two"
    # blockquote and fenced code.
    assert md_to_html("> quoted") == "<blockquote>quoted</blockquote>"
    assert md_to_html("```py\nx = 1 < 2\n```") == '<pre><code class="language-py">x = 1 &lt; 2</code></pre>'
    assert md_to_html("```\nplain\n```") == "<pre>plain</pre>"
    # a table -> a <pre> block (kept monospace).
    assert md_to_html("| a | b |\n|---|---|\n| 1 | 2 |") == "<pre>| a | b |\n|---|---|\n| 1 | 2 |</pre>"


def test_a_report_push_is_sent_as_html(tmp_path):
    bot, herd = _bot(tmp_path)
    herd.report(session="lead-tmp", state="done", last=REPORT)
    _push(herd, kind="report", session="lead-tmp", project="/tmp/shop", url=ROOM, since=0)
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert tg.sent, "the push was sent"
    _, payload = tg.sent[0]
    assert payload["parse_mode"] == "HTML"
    assert "<b>原因：</b>" in payload["text"] and "<code>8f162b2</code>" in payload["text"]
    assert "**" not in payload["text"]
    # The herd chrome is still there, escaped-safe.
    assert payload["text"].startswith("✓ lead-tmp · shop") and "Reply to tell the lead" in payload["text"]
    reset_herd()


class FakeParseFail:
    """Rejects a message with parse_mode (400 can't parse entities); takes
    the plain retry."""

    def __init__(self):
        self.sent = []

    async def post(self, url, json=None):
        self.sent.append((url, json))
        has_pm = "parse_mode" in (json or {})
        mid = 1000 + len(self.sent)

        class Resp:
            status_code = 400 if has_pm else 200

            def raise_for_status(self):
                pass

            def json(self):
                if has_pm:
                    return {"ok": False, "description": "Bad Request: can't parse entities: unsupported start tag"}
                return {"ok": True, "result": {"message_id": mid}}

        return Resp()


def test_a_message_telegram_cannot_parse_is_resent_as_plain(tmp_path):
    bot, herd = _bot(tmp_path)
    herd.report(session="lead-tmp", state="done", last="**原因：** `x` and <y>")
    _push(herd, kind="report", session="lead-tmp", project="/tmp/shop", url=ROOM, since=0)
    tg = FakeParseFail()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    # Two posts: the HTML try, then the plain retry.
    assert len(tg.sent) == 2
    assert tg.sent[0][1]["parse_mode"] == "HTML"
    plain = tg.sent[1][1]
    assert "parse_mode" not in plain
    assert "<b>" not in plain["text"] and "原因：" in plain["text"] and "<y>" in plain["text"]
    # The push is not lost: it is marked sent.
    assert herd.unsent_pushes() == []
    reset_herd()


def test_the_markdown_is_clipped_before_a_tag_or_entity_is_cut():
    # A clip that lands inside a bold span leaves the marker literal, never a
    # half-written <b>; the result stays within the limit and well-formed.
    html = to_html("**bold** " * 400, limit=300)
    assert len(html) <= 300
    assert html.count("<b>") == html.count("</b>")
    import re as _re
    assert not _re.search(r"<[a-z/][^>]*$", html)  # no tag left open at the cut
    # Entities are whole: escaping happens after the clip.
    ents = to_html("&" * 400, limit=120)
    assert len(ents) <= 120
    assert not _re.search(r"&(?!amp;|lt;|gt;|quot;)", ents)


def test_a_blocked_push_escapes_its_screen_and_keeps_numbers(tmp_path):
    bot, herd = _bot(tmp_path)
    menu = {"question": "Run `rm -rf <dir>` & proceed?",
            "options": [{"n": 1, "label": "Yes"}, {"n": 2, "label": "No"}]}
    herd.tick(sessions=[{"name": "agy-commit", "kind": "agy", "state": "blocked",
                         "reason": "?", "project": "/tmp", "prompt": menu}],
              pushes=[{"kind": "blocked", "session": "agy-commit", "project": "/tmp/shop", "url": ROOM, "reason": "?"}])
    push = herd.unsent_pushes()[-1]
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    text = tg.sent[0][1]["text"]
    # The shell is escaped into a code span, not run as markup; the numbered
    # choices stay as typed.
    assert "<code>rm -rf &lt;dir&gt;</code> &amp; proceed?" in text
    assert "1. Yes\n2. No" in text
    reset_herd()


def test_the_screen_snapshot_is_a_pre_block(tmp_path):
    bot, herd = _bot(tmp_path)
    herd.tick(sessions=[{"name": "agy-commit", "kind": "agy", "state": "blocked",
                         "reason": "?", "screen": "line one\n<tag> & more"}], ack_ids=[])
    agy = herd.session("agy-commit")
    msg = bot.handle_callback(99, f"sc:{agy['short']}")
    assert msg["parse_mode"] == "HTML"
    assert msg["text"].startswith("<pre>") and msg["text"].endswith("</pre>")
    assert "&lt;tag&gt; &amp; more" in msg["text"]
    reset_herd()
