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
    bot, herd = _bot(tmp_path)
    agy = herd.session("agy-commit")
    kb = session_keyboard(agy)
    data = [
        btn["callback_data"]
        for row in kb["inline_keyboard"]
        for btn in row
    ]
    assert f"n:{agy['short']}:1" in data
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
