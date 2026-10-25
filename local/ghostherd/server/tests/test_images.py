"""The images an agent's reply names: copied when the reply is recorded,
under an id, for the page and Telegram -- and nothing else (images.py)."""

import asyncio
import json
import os
import stat
import time
from pathlib import Path

import pytest
from PIL import Image

import conversation as conv
import images
from herd import HerdStore, get_herd, reset_herd
from jsonrpc_handler import JsonRpcRequest, handler
from telegram import PUSH_IMAGES, TelegramBot


@pytest.fixture
def store(tmp_path, monkeypatch):
    monkeypatch.setenv("GHOSTHERD_IMAGES_DIR", str(tmp_path / "kept"))
    images.reset_store()
    yield images.get_store()
    images.reset_store()


def png(path: Path, size=(64, 40), color=(200, 30, 30, 255), fmt="PNG") -> str:
    path.parent.mkdir(parents=True, exist_ok=True)
    im = Image.new("RGBA" if fmt == "PNG" else "RGB", size, color if fmt == "PNG" else color[:3])
    im.save(path, fmt)
    return str(path)


def rpc(method, params, role="admin"):
    r = asyncio.run(handler.handle(JsonRpcRequest(id=1, method=method, params=params), role))
    assert r.error is None, r.error
    return r.result


# ---- What a reply names ----------------------------------------------------

def test_what_a_reply_names():
    text = (
        "The screenshots are in /private/tmp/x/shots/after-light.png and /tmp/b.JPG.\n"
        "With spaces: `/tmp/my shots/c d.webp`, and ![dark](/tmp/e.gif \"Dark\") or "
        "[the log](</tmp/with space/f.png>).\n"
        "Also file:///tmp/g%20h.jpeg and ~/Desktop/i.png; again /tmp/b.JPG.\n"
        "Not these: https://example.com/j.png, /tmp/k.png.bak, /tmp/l.pngx, docs/m.png, "
        "/tmp/n.txt, C:/o.png"
    )
    assert images.named(text) == [
        "/private/tmp/x/shots/after-light.png", "/tmp/b.JPG", "/tmp/my shots/c d.webp",
        "/tmp/e.gif", "/tmp/with space/f.png", "file:///tmp/g%20h.jpeg", "~/Desktop/i.png",
    ]


def test_a_named_image_is_copied_under_an_id(store, tmp_path):
    src = png(tmp_path / "scratch" / "shots" / "after.png", size=(1200, 800))
    recs = store.capture(f"Done.  The screenshot: {src}")
    assert len(recs) == 1
    rec = recs[0]
    assert images.ID.match(rec["id"]) and rec["ext"] == "png" and rec["name"] == "after.png"
    assert (rec["w"], rec["h"], rec["thumb"]) == (1200, 800, True)
    full, thumb = store.path(rec), store.path(rec, thumb=True)
    assert full.read_bytes() == Path(src).read_bytes()
    # Yours alone: the directory and each copy.
    assert stat.S_IMODE(store.root.stat().st_mode) == 0o700
    assert stat.S_IMODE(full.stat().st_mode) == 0o600 and stat.S_IMODE(thumb.stat().st_mode) == 0o600
    with Image.open(thumb) as t:
        assert t.format == "JPEG" and max(t.size) <= images.THUMB_PX
    # The page and the bot get ids, never where the image came from.
    assert images.public(recs) == [{"id": rec["id"], "ext": "png", "name": "after.png",
                                    "w": 1200, "h": 800, "thumb": True}]
    assert str(tmp_path) not in json.dumps(images.public(recs))


def test_it_outlives_its_scratch_dir_and_is_kept_once(store, tmp_path):
    src = png(tmp_path / "scratch" / "a.png")
    first = store.capture(f"See {src}")
    os.unlink(src)
    # The message read again later -- in the Inbox, a thread, a push.
    assert store.lookup(f"See {src}") == first
    assert store.path(first[0]).exists()
    # The same picture under another name and another message: one copy.
    again = png(tmp_path / "other" / "b.png")
    second = store.capture(f"Again: {again}")
    assert second[0]["id"] == first[0]["id"] and second[0]["name"] == "b.png"
    assert len(list(store.root.glob("*.png"))) == 1
    # Another picture: another id, not one to guess from the first.
    third = store.capture(f"And {png(tmp_path / 'c.png', color=(0, 0, 255, 255))}")
    assert third[0]["id"] != first[0]["id"]


def test_what_is_refused(store, tmp_path, monkeypatch):
    real = png(tmp_path / "elsewhere" / "private.png")
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    # Not an image, whatever its name says.
    (scratch / "notes.png").write_text("password=hunter2\n")
    # PNG's first bytes and nothing an image is made of.
    (scratch / "fake.png").write_bytes(b"\x89PNG\r\n\x1a\n" + b"\0" * 200)
    # A symlink to an image elsewhere.
    (scratch / "link.png").symlink_to(real)
    # A way out of the directory it names.
    traversal = f"{scratch}/../elsewhere/private.png"
    # A directory, and a file that is not there.
    (scratch / "dir.png").mkdir()
    big = png(scratch / "big.png", size=(300, 300))
    monkeypatch.setattr(images, "MAX_BYTES", os.path.getsize(big) - 1)
    for p in (f"{scratch}/notes.png", f"{scratch}/fake.png", f"{scratch}/link.png", traversal,
              f"{scratch}/dir.png", f"{scratch}/missing.png", big, "relative/x.png"):
        assert store.capture(f"Here: {p} ") == [], p
    assert not list(store.root.glob("*.png"))
    # Too many pixels for the bytes, too: a decompression bomb is no thumbnail.
    monkeypatch.setattr(images, "MAX_BYTES", 16 * 1024 * 1024)
    monkeypatch.setattr(images, "MAX_PIXELS", 299 * 300)
    assert store.capture(f"Again {big} ") == []
    # The same directory's own image is copied: the refusals are each their own.
    ok = png(scratch / "fine.png", color=(1, 1, 1, 255))
    assert len(store.capture(f"Fine {ok}")) == 1
    # Not this user's: planted in a shared /tmp by another.
    theirs = png(scratch / "theirs.png", color=(2, 2, 2, 255))
    monkeypatch.setattr(images.os, "getuid", lambda: os.stat(theirs).st_uid + 1)
    assert store.capture(f"Theirs {theirs}") == []


def test_a_message_read_later_takes_the_image_it_meant(store, tmp_path):
    src = png(tmp_path / "s" / "shot.png")
    said = time.time() - 3600
    # Read from a transcript, without its time: nothing is copied.
    assert store.lookup(f"Look at {src}") == []
    assert not store.root.exists() or not list(store.root.glob("*.png"))
    # The file changed after the message was written: not the one it meant.
    assert store.lookup(f"Look at {src}", at=said) == []
    # A message seen once is not looked at again: it named nothing to copy.
    os.utime(src, (said - 60, said - 60))
    assert store.lookup(f"Look at  {src}\n", at=said) == []
    # Another, written after the file: copied.
    assert len(store.lookup(f"And at {src}", at=said)) == 1


def test_only_its_own_message_finds_an_image(store, tmp_path):
    a, b = png(tmp_path / "a.png"), png(tmp_path / "b.png", color=(0, 255, 0, 255))
    store.capture(f"Only {a}")
    # A message that names nothing gets nothing, though an image is kept.
    assert store.lookup("Nothing to see") == []
    # One naming another file -- recorded never -- copies nothing on lookup.
    assert store.lookup(f"Only {b}") == []
    assert len(list(store.root.glob("*.png"))) == 1


def test_at_most_a_few_per_message(store, tmp_path):
    paths = [png(tmp_path / f"{n}.png", color=(n * 20, 0, 0, 255)) for n in range(images.MAX_PER_MESSAGE + 3)]
    assert len(store.capture(" ".join(paths))) == images.MAX_PER_MESSAGE


def test_old_copies_go(store, tmp_path):
    old = store.capture(f"old {png(tmp_path / 'old.png')}")[0]
    new = store.capture(f"new {png(tmp_path / 'new.png', color=(1, 2, 3, 255))}")[0]
    now = time.time()
    assert store.prune(now) == 0
    store._db().execute("UPDATE images SET used = ? WHERE id = ?", (now - 31 * 86400, old["id"]))
    store._db().commit()
    assert store.prune(now) == 1
    assert not store.path(old).exists() and not store.path(old, thumb=True).exists()
    # A message whose image went shows none -- not whatever is at its path now.
    assert store.lookup(f"old {tmp_path / 'old.png'}", at=now) == []
    assert store.lookup(f"new {tmp_path / 'new.png'}")[0]["id"] == new["id"]
    # Past the budget, the oldest go first.
    store.budget = 1
    assert store.prune(now) == 1 and not store.path(new).exists()


# ---- Where they are recorded -----------------------------------------------

@pytest.fixture
def herd(tmp_path, monkeypatch, store):
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "mem"))
    reset_herd()
    h = get_herd()
    h.tick(sessions=[{"name": "lead-x", "kind": "claude", "state": "working", "project": "/tmp/x", "role": "lead"},
                     {"name": "rd", "kind": "claude", "state": "idle", "project": "/tmp/x"}])
    yield h
    reset_herd()


def test_a_turns_last_words_bring_their_images_to_the_inbox(herd, tmp_path):
    src = png(tmp_path / "shots" / "after.png")
    rpc("herd_report", {"session": "lead-x", "state": "done", "cli": "claude",
                        "last": f"Shipped.  The screenshot is in {src}"}, role="agent")
    images.drain()
    os.unlink(src)                                  # its scratch dir went
    link = rpc("herd_link", {"session": "lead-x"})["link"]
    assert [i["name"] for i in link["last_images"]] == ["after.png"]
    assert set(link["last_images"][0]) == {"id", "ext", "name", "w", "h", "thumb"}


def test_a_probes_words_are_not_recorded(herd, tmp_path):
    src = png(tmp_path / "probe.png")
    out = rpc("herd_report", {"session": "lead-x", "state": "done", "cli": "grok",
                              "last": f"probe {src}"}, role="agent")
    assert out.get("ignored")
    images.drain()
    assert not images.get_store().root.exists() or not list(images.get_store().root.glob("*.png"))


def test_an_answer_brings_its_images_to_the_thread(herd, tmp_path):
    ask = herd.ask(to="rd", body="Take a screenshot", from_name="lead-x")
    src = png(tmp_path / "shot.png")
    rpc("herd_reply", {"id": ask["id"], "body": f"Here it is: `{src}`", "from": "rd"}, role="agent")
    images.drain()
    th = rpc("herd_thread", {"ask": ask["id"]})
    answer = [m for m in th["msgs"] if m["kind"] == "answer"][0]
    assert [i["name"] for i in answer["images"]] == ["shot.png"]
    # The ask itself is not a reply: what it names is not copied.
    assert "images" not in [m for m in th["msgs"] if m["kind"] == "ask"][0]


def test_an_agent_cannot_ask_for_images():
    "No method takes a path, and what reads them back is the page's alone."
    for method in ("herd_link", "herd_thread", "herd_conversation", "herd_chat"):
        r = asyncio.run(handler.handle(JsonRpcRequest(id=1, method=method, params={}), "agent"))
        assert r.error and r.error.code == -32003, method
    assert not any("image" in m for m in handler._methods)


def test_a_rooms_messages_carry_their_images(tmp_path, monkeypatch, store):
    monkeypatch.setenv("HOME", str(tmp_path))
    conv._cache.clear()
    shot = png(tmp_path / "s" / "room.png")
    mine = png(tmp_path / "s" / "prompt.png", color=(9, 9, 9, 255))
    os.utime(shot, (1_700_000_000, 1_700_000_000))
    path = tmp_path / ".claude" / "projects" / "-x" / "c.jsonl"
    path.parent.mkdir(parents=True)

    def rec(kind, content, ts="2026-10-09T01:00:05Z", mid="m1"):
        msg = {"role": kind, "content": content}
        if kind == "assistant":
            msg["id"] = mid
        return {"type": kind, "message": msg, "timestamp": ts}

    path.write_text("".join(json.dumps(r) + "\n" for r in [
        rec("user", f"Look at {mine}"),
        rec("assistant", [{"type": "tool_use", "id": "t1", "name": "Bash", "input": {"command": f"ls {shot}"}}]),
        rec("user", [{"type": "tool_result", "tool_use_id": "t1", "content": shot}]),
        rec("assistant", [{"type": "text", "text": f"The room: {shot}"}], mid="m2"),
    ]))
    entries = conv.read(str(path))["entries"]
    said = [e for e in entries if e["role"] == "assistant"]
    assert [i["name"] for i in said[0]["images"]] == ["room.png"]
    # Your own words, and what a tool printed, are not an agent's reply.
    assert all("images" not in e for e in entries if e["role"] != "assistant")
    chat = conv.chat([("rd", str(path))])["entries"]
    assert [i["name"] for i in [c for c in chat if c["kind"] == "turn"][0]["images"]] == ["room.png"]
    conv._cache.clear()


# ---- Telegram --------------------------------------------------------------

class FakeTelegram:
    """Telegram's API, answering each call with message ids -- or 400 for
    the methods in REFUSE.  Nothing leaves the test."""

    def __init__(self, refuse=(), fail=()):
        self.sent = []
        self.refuse, self.fail = set(refuse), set(fail)

    async def post(self, url, json=None, data=None, files=None):
        method = url.rsplit("/", 1)[-1]
        self.sent.append((method, json or data, files))
        n = len(self.sent)
        refuse, fail = method in self.refuse, method in self.fail
        many = method == "sendMediaGroup"

        class Resp:
            status_code = 400 if refuse else 200

            def raise_for_status(self):
                if refuse or fail:
                    raise RuntimeError(f"{method} failed")

            def json(self):
                if refuse:
                    return {"ok": False, "description": "Bad Request: PHOTO_INVALID_DIMENSIONS"}
                if many:
                    return {"ok": True, "result": [{"message_id": 2000 + n * 10 + i} for i in range(len(files))]}
                return {"ok": True, "result": {"message_id": 1000 + n}}

        return Resp()


def _report(herd, tmp_path, words, **push):
    herd.report(session="lead-x", state="done", last=words)
    images.drain()
    herd.tick(sessions=[{"name": "lead-x", "kind": "claude", "state": "done", "project": "/tmp/x", "role": "lead"}],
              pushes=[dict({"kind": "report", "session": "lead-x", "project": "/tmp/x", "url": "", "since": 0}, **push)])
    return TelegramBot("tok", (99,), herd)


def test_a_report_brings_its_image_as_a_photo_in_reply(herd, tmp_path):
    src = png(tmp_path / "after.png", size=(800, 600))
    bot = _report(herd, tmp_path, f"Shipped.  {src}")
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage", "sendPhoto"]
    _, data, files = tg.sent[1]
    assert data["chat_id"] == "99" and json.loads(data["reply_parameters"])["message_id"] == 1001
    assert files["photo"][0] == "after.png" and files["photo"][2] == "image/png"
    assert files["photo"][1] == Path(src).read_bytes()
    assert herd.unsent_pushes() == []
    # A reply to the photo goes to the lead, as one to the text does.
    assert bot.handle_message(99, "and the dark one?", reply_to=1002)["text"] == "→ the lead of x"


def test_more_go_as_an_album_and_a_few_at_most(herd, tmp_path):
    paths = [png(tmp_path / f"{n}.png", color=(n * 30, 0, 0, 255)) for n in range(PUSH_IMAGES + 2)]
    bot = _report(herd, tmp_path, "Shots: " + " ".join(paths))
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage", "sendMediaGroup"]
    _, data, files = tg.sent[1]
    media = json.loads(data["media"])
    assert len(media) == len(files) == PUSH_IMAGES
    assert [m["media"] for m in media] == [f"attach://p{n}" for n in range(PUSH_IMAGES)]


def test_too_big_for_a_photo_goes_as_a_file(herd, tmp_path):
    tall = png(tmp_path / "page.png", size=(400, 9800))
    anim = png(tmp_path / "anim.gif", fmt="GIF")
    bot = _report(herd, tmp_path, f"Full page: {tall} and {anim}")
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage", "sendDocument", "sendDocument"]
    assert [f["document"][0] for _, _, f in tg.sent[1:]] == ["page.png", "anim.gif"]


def test_a_photo_telegram_will_not_take_goes_as_a_file(herd, tmp_path):
    bot = _report(herd, tmp_path, f"See {png(tmp_path / 'x.png')}")
    tg = FakeTelegram(refuse={"sendPhoto"})
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage", "sendPhoto", "sendDocument"]


def test_images_failing_never_send_the_push_again(herd, tmp_path):
    bot = _report(herd, tmp_path, f"See {png(tmp_path / 'x.png')}")
    tg = FakeTelegram(fail={"sendPhoto"})
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert herd.unsent_pushes() == []
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage", "sendPhoto"]


def test_images_turned_off_send_none(herd, tmp_path):
    bot = _report(herd, tmp_path, f"See {png(tmp_path / 'x.png')}", images=False)
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage"]


def test_only_a_report_brings_images(herd, tmp_path):
    src = png(tmp_path / "x.png")
    herd.tick(sessions=[{"name": "lead-x", "kind": "claude", "state": "blocked", "project": "/tmp/x",
                         "role": "lead", "reason": f"Allow opening {src}?"}],
              pushes=[{"kind": "blocked", "session": "lead-x", "project": "/tmp/x", "url": "",
                       "reason": f"Allow opening {src}?"}])
    bot = TelegramBot("tok", (99,), herd)
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage"]
