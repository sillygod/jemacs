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
from telegram import PUSH_IMAGES, PUSH_REPORTS, TelegramBot


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
    for method in ("herd_link", "herd_thread", "herd_conversation", "herd_chat", "herd_reports"):
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


# ---- HTML reports ----------------------------------------------------------

PAGE = ("<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">"
        "<title>The viewer &amp; its\n   theme</title></head><body><h1>Report</h1></body></html>\n")


def page(path: Path, text: str = PAGE) -> str:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return str(path)


def test_a_reply_names_its_html_reports():
    text = ("The report: /tmp/x/report.html, also `/tmp/my dir/b.htm` and [c](</tmp/c d.html>), "
            "~/r/e.HTML and file:///tmp/f%20g.html.  Not https://example.com/h.html, /tmp/i.html.bak, "
            "/tmp/j.htmlx, docs/k.html")
    assert images.named(text) == ["/tmp/x/report.html", "/tmp/my dir/b.htm", "/tmp/c d.html",
                                  "~/r/e.HTML", "file:///tmp/f%20g.html"]


def test_an_html_report_is_copied_under_an_id(store, tmp_path):
    src = page(tmp_path / "scratch" / "report.html")
    recs = store.capture(f"Done.  The report is in {src}")
    assert len(recs) == 1
    rec = recs[0]
    assert images.ID.match(rec["id"]) and rec["ext"] == "html" and rec["name"] == "report.html"
    assert rec["title"] == "The viewer & its theme" and not rec["thumb"]
    copy = store.path(rec)
    assert copy.name == rec["id"] + ".html" and copy.read_bytes() == Path(src).read_bytes()
    assert stat.S_IMODE(copy.stat().st_mode) == 0o600
    assert not store.path(rec, thumb=True).exists()
    # The page gets the id and the title, never where it came from.
    assert images.public(recs) == [{"id": rec["id"], "ext": "html", "name": "report.html",
                                    "w": 0, "h": 0, "thumb": False, "title": "The viewer & its theme"}]
    assert str(tmp_path) not in json.dumps(images.public(recs))
    # Its scratch dir gone, the same words still find it; .htm, a BOM and
    # a comment before the doctype are HTML too.
    os.unlink(src)
    assert [r["id"] for r in store.lookup(f"Done.  The report is in {src}")] == [rec["id"]]
    other = page(tmp_path / "s" / "b.htm", "﻿  <!-- made by hand -->\n<html><body>x</body></html>")
    got = store.capture(f"And {other}")
    assert [(r["ext"], r["title"]) for r in got] == [("html", "")]


def test_what_html_is_refused(store, tmp_path, monkeypatch):
    real = page(tmp_path / "elsewhere" / "private.html")
    s = tmp_path / "scratch"
    s.mkdir()
    (s / "notes.html").write_text("password=hunter2\n")                       # not HTML
    (s / "nul.html").write_bytes(b"<!doctype html><title>x</title>\0")        # a NUL: not text
    (s / "latin.html").write_bytes(b"<!doctype html><title>caf\xe9</title>")   # not UTF-8
    (s / "shot.html").write_bytes(Path(png(s / "a.png")).read_bytes())        # an image by another name
    (s / "late.html").write_text("<p>no doctype</p><html></html>")           # not one that opens as a page
    (s / "link.html").symlink_to(real)                                        # a symlink
    (s / "dir.html").mkdir()                                                   # a directory
    (s / "page.png").write_text(PAGE)                                          # a page by an image's name
    big = page(s / "big.html", PAGE + "x" * 100)
    monkeypatch.setattr(images, "MAX_HTML_BYTES", os.path.getsize(big) - 1)
    for p in (f"{s}/notes.html", f"{s}/nul.html", f"{s}/latin.html", f"{s}/shot.html", f"{s}/late.html",
              f"{s}/link.html", f"{s}/dir.html", f"{s}/page.png", big, f"{s}/../elsewhere/private.html",
              f"{s}/missing.html", "relative/x.html"):
        assert store.capture(f"Here: {p} ") == [], p
    assert not list(store.root.glob("*.html"))
    # The same directory's own page is copied: the refusals are each their own.
    assert len(store.capture(f"Fine {page(s / 'fine.html')}")) == 1
    # Not this user's: planted in a shared /tmp by another.
    theirs = page(s / "theirs.html", PAGE.replace("Report", "Theirs"))
    monkeypatch.setattr(images.os, "getuid", lambda: os.stat(theirs).st_uid + 1)
    assert store.capture(f"Theirs {theirs}") == []


def test_a_report_read_later_is_the_one_it_meant(store, tmp_path):
    src = page(tmp_path / "s" / "r.html")
    os.utime(src, (1_700_000_000, 1_700_000_000))
    said = f"See {src}"
    assert store.lookup(said, at=1_699_000_000) == []          # written well after the message
    assert len(store.lookup(f"Again {src}", at=1_700_000_000)) == 1


def test_an_index_from_before_reports_gains_their_titles(tmp_path, monkeypatch):
    import sqlite3
    root = tmp_path / "kept"
    root.mkdir()
    old = sqlite3.connect(root / "index.sqlite")
    old.executescript("""
        CREATE TABLE images (id TEXT PRIMARY KEY, sha TEXT UNIQUE NOT NULL, ext TEXT NOT NULL,
            bytes INTEGER NOT NULL, w INTEGER NOT NULL, h INTEGER NOT NULL,
            thumb INTEGER NOT NULL, created REAL NOT NULL, used REAL NOT NULL);
    """)
    # A copy from before, used just now: kept.
    old.execute("INSERT INTO images VALUES ('0123456789abcdef0123456789abcdef', 's', 'png', 3, 1, 1, 0, ?, ?)",
                (time.time(), time.time()))
    (root / "0123456789abcdef0123456789abcdef.png").write_bytes(b"png")
    old.commit()
    old.close()
    monkeypatch.setenv("GHOSTHERD_IMAGES_DIR", str(root))
    images.reset_store()
    try:
        st = images.get_store()
        src = page(tmp_path / "r.html")
        assert [r["title"] for r in st.capture(f"See {src}")] == ["The viewer & its theme"]
        db = sqlite3.connect(root / "index.sqlite")
        assert db.execute("SELECT title FROM images WHERE ext = 'png'").fetchone() == ("",)
        db.close()
    finally:
        images.reset_store()


def test_old_reports_go(store, tmp_path):
    rec = store.capture(f"See {page(tmp_path / 'r.html')}")[0]
    assert store.prune(now=time.time() + store.keep + 10) == 1
    assert not store.path(rec).exists()
    assert store.lookup(f"See {tmp_path / 'r.html'}") == []


def test_a_turns_report_reaches_the_inbox(herd, tmp_path):
    src = page(tmp_path / "out" / "report.html")
    shot = png(tmp_path / "out" / "after.png")
    rpc("herd_report", {"session": "lead-x", "state": "done", "cli": "claude",
                        "last": f"Shipped.  {shot} and the write-up {src}"}, role="agent")
    images.drain()
    link = rpc("herd_link", {"session": "lead-x"})["link"]
    assert [(i["ext"], i["name"]) for i in link["last_images"]] == [("png", "after.png"), ("html", "report.html")]
    assert link["last_images"][1]["title"] == "The viewer & its theme"
    assert "title" not in link["last_images"][0]


def test_a_report_brings_its_html_as_a_file_and_no_link(herd, tmp_path):
    src = page(tmp_path / "report.html")
    shot = png(tmp_path / "after.png", size=(800, 600))
    bot = _report(herd, tmp_path, f"Shipped.  {shot} and {src}", url="https://box.tail0.ts.net/open/ghostherd")
    tg = FakeTelegram()
    asyncio.run(bot._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage", "sendPhoto", "sendDocument"]
    _, data, files = tg.sent[2]
    assert files["document"] == ("report.html", Path(src).read_bytes(), "text/html")
    assert data["caption"] == "The viewer & its theme (report.html)"
    assert json.loads(data["reply_parameters"])["message_id"] == 1001
    # Nothing sent carries an address but the room's own button, which
    # holds no secret: the report itself goes, not a link to it.
    for _, payload, _ in tg.sent:
        flat = json.dumps(payload)
        assert "/.reports/" not in flat and "/.files/" not in flat and "127.0.0.1" not in flat
    assert herd.unsent_pushes() == []


def test_reports_turned_off_and_a_few_at_most(herd, tmp_path):
    pages = [page(tmp_path / f"r{n}.html", PAGE.replace("Report", f"Report {n}")) for n in range(4)]
    shot = png(tmp_path / "x.png")
    words = "Done: " + " ".join(pages) + " " + shot
    tg = FakeTelegram()
    asyncio.run(_report(herd, tmp_path, words, reports=False)._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage", "sendPhoto"]
    tg = FakeTelegram()
    asyncio.run(_report(herd, tmp_path, words + " ", images=False)._flush_pushes(tg, "https://t/{method}"))
    assert [m for m, _, _ in tg.sent] == ["sendMessage"] + ["sendDocument"] * PUSH_REPORTS


# ---- The Docs tab's Reports --------------------------------------------------

def _at(store, rec, at):
    "Say REC was copied at AT."
    store._db().execute("UPDATE images SET created = ? WHERE id = ?", (at, rec["id"]))
    store._db().commit()


def test_reports_listed_newest_first_with_who_wrote_them(herd, tmp_path):
    first = page(tmp_path / "a" / "first.html", PAGE.replace("The viewer", "First"))
    rpc("herd_report", {"session": "lead-x", "state": "done", "cli": "claude",
                        "last": f"Shipped.  {png(tmp_path / 'a' / 'shot.png')} and {first}"}, role="agent")
    images.drain()
    ask = herd.ask(to="rd", body="Write it up", from_name="lead-x")
    second = page(tmp_path / "b" / "second.html", PAGE.replace("The viewer", "Second"))
    rpc("herd_reply", {"id": ask["id"], "body": f"Here: `{second}`", "from": "rd"}, role="agent")
    images.drain()
    store = images.get_store()
    one, two = store.lookup(f"Here: `{second}`")[0], [r for r in store.lookup(
        f"Shipped.  {tmp_path / 'a' / 'shot.png'} and {first}") if r["ext"] == "html"][0]
    _at(store, two, 1_700_000_000.25)
    _at(store, one, 1_700_000_100.5)
    out = rpc("herd_reports", {})
    # Reports alone, the newest first; the image is not one.
    assert out == {"more": False, "reports": [
        {"id": one["id"], "title": "Second & its theme", "name": "second.html", "bytes": len(PAGE.replace("The viewer", "Second")),
         "at": 1_700_000_100.5, "agent": "rd", "project": "x", "gone": False, "older": []},
        {"id": two["id"], "title": "First & its theme", "name": "first.html", "bytes": len(PAGE.replace("The viewer", "First")),
         "at": 1_700_000_000.25, "agent": "lead-x", "project": "x", "gone": False, "older": []}]}
    # Never where a report came from.
    assert str(tmp_path) not in json.dumps(out)


def test_reports_page_by_page(store, herd, tmp_path):
    recs = []
    for n in range(5):
        recs.append(store.capture(f"r{n} {page(tmp_path / f'r{n}.html', PAGE.replace('Report', f'R{n}'))}",
                                  who=images.writer("rd", "/tmp/x"))[0])
    # Three copied at the same moment, across a page's end: told apart by id.
    for n, rec in enumerate(recs):
        _at(store, rec, 1_700_000_000 + min(n, 2))
    want = [r["id"] for r in sorted(recs, key=lambda r: (1_700_000_000 + min(recs.index(r), 2), r["id"]),
                                    reverse=True)]
    got, before = [], None
    for _ in range(len(recs)):                      # a page that never ends fails, not hangs
        out = rpc("herd_reports", dict({"limit": 2}, **({"before": before} if before else {})))
        got += [r["id"] for r in out["reports"]]
        assert len(out["reports"]) <= 2
        if not out["more"]:
            break
        before = {"at": out["reports"][-1]["at"], "id": out["reports"][-1]["id"]}
    assert got == want
    for bad in ({"at": "x", "id": recs[0]["id"]}, {"at": 1, "id": "../index"}, {"at": True, "id": recs[0]["id"]},
                [1, 2], "x"):
        r = asyncio.run(handler.handle(JsonRpcRequest(id=1, method="herd_reports",
                                                      params={"before": bad}), "admin"))
        assert r.error and "before" in r.error.message, bad
    assert len(rpc("herd_reports", {"limit": 10_000})["reports"]) == 5


def test_a_report_whose_copy_is_gone_says_so(store, tmp_path):
    rec = store.capture(f"See {page(tmp_path / 'r.html')}", who=images.writer("rd", "/tmp/x"))[0]
    store.path(rec).unlink()
    [listed] = store.reports()["reports"]
    assert listed["id"] == rec["id"] and listed["gone"] is True and listed["title"] == "The viewer & its theme"
    # A message shows it no more; and pruned in its time, it leaves the list.
    assert store.lookup(f"See {tmp_path / 'r.html'}") == []
    store.prune(now=time.time() + store.keep + 10)
    assert store.reports() == {"reports": [], "more": False}


def test_who_wrote_a_report_is_its_first_writer(store, tmp_path):
    src = page(tmp_path / "r.html")
    os.utime(src, (1_700_000_000, 1_700_000_000))
    # Met first where its writer is not known -- a push read before the
    # reply was recorded: the reply's own capture says who.
    said = f"Done: {src}"
    rec = store.lookup(said, at=1_700_000_000)[0]
    assert store.reports()["reports"][0]["agent"] == ""
    assert store.capture(said, who=images.writer("rd", "/tmp/x"))[0]["id"] == rec["id"]
    assert [(r["agent"], r["project"]) for r in store.reports()["reports"]] == [("rd", "/tmp/x")]
    # Named again by another -- a lead passing it on -- it stays the writer's.
    store.capture(f"rd's report: {src}", who=images.writer("lead-x", "/tmp/x"))
    store.capture(said, who=images.writer("lead-x", "/tmp/x"))
    assert [r["agent"] for r in store.reports()["reports"]] == ["rd"]
    assert images.writer("") is None and images.writer("  ") is None


def test_a_rooms_report_is_its_agents(tmp_path, monkeypatch, herd):
    monkeypatch.setenv("HOME", str(tmp_path))
    conv._cache.clear()
    src = page(tmp_path / "s" / "room.html")
    os.utime(src, (1_700_000_000, 1_700_000_000))
    path = tmp_path / ".claude" / "projects" / "-x" / "c.jsonl"
    path.parent.mkdir(parents=True)
    path.write_text(json.dumps({"type": "assistant", "timestamp": "2026-10-09T01:00:05Z", "message": {
        "role": "assistant", "id": "m1", "content": [{"type": "text", "text": f"Mid-turn: {src}"}]}}) + "\n")
    herd.report(session="rd", transcript=str(path), cli="claude")
    entries = rpc("herd_conversation", {"path": str(path)})["entries"]
    assert [i["name"] for i in entries[0]["images"]] == ["room.html"]
    assert [(r["agent"], r["project"]) for r in rpc("herd_reports", {})["reports"]] == [("rd", "x")]
    # Read in the chat, by the agent's name.
    conv._cache.clear()
    other = page(tmp_path / "s" / "chat.html", PAGE.replace("Report", "Chat"))
    os.utime(other, (1_700_000_000, 1_700_000_000))
    path.write_text(json.dumps({"type": "assistant", "timestamp": "2026-10-09T01:00:05Z", "message": {
        "role": "assistant", "id": "m1", "content": [{"type": "text", "text": f"And {other}"}]}}) + "\n")
    rpc("herd_chat", {"agents": [{"name": "lead-x", "path": str(path)}]})
    assert sorted((r["name"], r["agent"]) for r in rpc("herd_reports", {})["reports"]) == [
        ("chat.html", "lead-x"), ("room.html", "rd")]
    # A sub-agent's, read in its own view: its parent's agent's.
    sub = page(tmp_path / "s" / "sub.html", PAGE.replace("Report", "Sub"))
    os.utime(sub, (1_700_000_000, 1_700_000_000))
    spath = conv.sub_path(path, "a1")
    spath.parent.mkdir(parents=True)
    spath.write_text(json.dumps({"type": "assistant", "isSidechain": True, "timestamp": "2026-10-09T01:00:05Z",
                                 "message": {"role": "assistant", "id": "m9",
                                             "content": [{"type": "text", "text": f"Sub: {sub}"}]}}) + "\n")
    rpc("herd_sub_conversation", {"path": str(path), "sub": "a1"})
    assert ("sub.html", "rd") in [(r["name"], r["agent"]) for r in rpc("herd_reports", {})["reports"]]
    conv._cache.clear()


def test_an_index_from_before_writers_gains_them(tmp_path, monkeypatch):
    import sqlite3
    root = tmp_path / "kept"
    root.mkdir()
    old = sqlite3.connect(root / "index.sqlite")
    old.executescript("""
        CREATE TABLE images (id TEXT PRIMARY KEY, sha TEXT UNIQUE NOT NULL, ext TEXT NOT NULL,
            bytes INTEGER NOT NULL, w INTEGER NOT NULL, h INTEGER NOT NULL,
            thumb INTEGER NOT NULL, created REAL NOT NULL, used REAL NOT NULL,
            title TEXT NOT NULL DEFAULT '');
    """)
    old.execute("INSERT INTO images VALUES ('0123456789abcdef0123456789abcdef', 's', 'html', 3, 0, 0, 0, ?, ?, 'Old')",
                (time.time(), time.time()))
    (root / "0123456789abcdef0123456789abcdef.html").write_bytes(b"<html>")
    old.commit()
    old.close()
    monkeypatch.setenv("GHOSTHERD_IMAGES_DIR", str(root))
    images.reset_store()
    try:
        st = images.get_store()
        st.capture(f"See {page(tmp_path / 'r.html')}", who=images.writer("rd", "/tmp/x"))
        assert [(r["title"], r["agent"], r["name"]) for r in st.reports()["reports"]] == [
            ("The viewer & its theme", "rd", "r.html"), ("Old", "", "0123456789abcdef0123456789abcdef.html")]
    finally:
        images.reset_store()


# ---- A report rewritten and named again -------------------------------------


def _rewrite(path, text, when):
    "PATH rewritten with TEXT, its time WHEN: an agent's second draft."
    Path(path).write_text(text, encoding="utf-8")
    os.utime(path, (when, when))


def test_a_message_said_again_shows_the_file_as_it_is_now(store, tmp_path):
    """The same words naming a report the agent rewrote: the new copy, not
    the old one.  A file not changed is not read again; one gone keeps its
    copy."""
    src = page(tmp_path / "r.html", PAGE.replace("Report", "Draft 1"))
    shot = png(tmp_path / "s.png")
    said = f"The report: {src}, and {shot}"
    first = store.capture(said)
    assert len(first) == 2
    _rewrite(src, PAGE.replace("Report", "Draft 2"), time.time() + 5)
    reads = []
    real = images._load
    images._load = lambda p, at=None: reads.append(os.path.basename(p)) or real(p, at)
    try:
        second = store.capture(said)
    finally:
        images._load = real
    assert reads == ["r.html"]                              # the image did not change: not read
    assert second[0]["id"] != first[0]["id"] and second[1]["id"] == first[1]["id"]
    assert b"Draft 2" in store.path(second[0]).read_bytes()
    assert store.lookup(said) == second                     # the message shows the new one
    # Said again unchanged: nothing read, the same copies.
    images._load = lambda p, at=None: pytest.fail("read again: " + p)
    try:
        assert store.capture(said) == second
        os.unlink(src)                                       # gone: the copy kept
        assert store.capture(said) == second
    finally:
        images._load = real
    # Never where it came from: a digest of the path, not the path.
    rows = store._db().execute("SELECT src, stamp FROM refs").fetchall()
    assert rows and all(len(src_) == 32 and str(tmp_path) not in src_ + st for src_, st in rows)


def test_a_message_copied_before_sources_were_kept_is_looked_at_once(store, tmp_path):
    src = page(tmp_path / "r.html", PAGE.replace("Report", "Draft 1"))
    said = f"See {src}"
    first = store.capture(said)
    store._db().execute("UPDATE refs SET src = '', stamp = ''")   # as an index from before kept it
    store._db().commit()
    _rewrite(src, PAGE.replace("Report", "Draft 2"), time.time() + 5)
    second = store.capture(said)
    assert second[0]["id"] != first[0]["id"] and second[0]["src"] == images.source(src)
    # Gone before it was looked at: the copy it had, by its name.
    store._db().execute("UPDATE refs SET src = '', stamp = ''")
    store._db().commit()
    os.unlink(src)
    assert [r["id"] for r in store.capture(said)] == [second[0]["id"]]


def test_a_report_rewritten_is_one_row_its_older_copies_under_it(herd, tmp_path):
    """Named again as an agent rewrote it, a report is one row: its newest
    copy, the older under it, newest first -- by project and file name; the
    same name in another project is another report."""
    store = images.get_store()
    src = page(tmp_path / "a" / "status.html", PAGE.replace("Report", "v1"))
    rd = images.writer("rd", "/src/x")
    v1 = store.capture(f"Status: {src}", who=rd)[0]
    for n, when in ((2, 1_700_000_100), (3, 1_700_000_200)):
        _rewrite(src, PAGE.replace("Report", f"v{n}"), time.time() + n)
        store.capture(f"Status: {src}", who=rd)
    other = store.capture(f"Theirs: {page(tmp_path / 'b' / 'status.html', PAGE.replace('Report', 'y'))}",
                          who=images.writer("rd-y", "/src/y"))[0]
    copies = store._db().execute("SELECT id FROM images WHERE ext = 'html' AND project = '/src/x'"
                                 " ORDER BY created").fetchall()
    assert len(copies) == 3
    for (id_,), at in zip(copies, (1_700_000_000, 1_700_000_100, 1_700_000_200)):
        _at(store, {"id": id_}, at)
    _at(store, other, 1_700_000_050)
    out = rpc("herd_reports", {})
    assert [(r["id"], r["project"], [o["id"] for o in r["older"]]) for r in out["reports"]] == [
        (copies[2][0], "x", [copies[1][0], copies[0][0]]), (other["id"], "y", [])]
    assert copies[0][0] == v1["id"]
    older = out["reports"][0]["older"]
    assert [(o["name"], o["agent"], o["project"], o["at"], o["gone"]) for o in older] == [
        ("status.html", "rd", "x", 1_700_000_100, False), ("status.html", "rd", "x", 1_700_000_000, False)]
    # A page at a time: by each report's newest.
    first = rpc("herd_reports", {"limit": 1})
    assert [r["id"] for r in first["reports"]] == [copies[2][0]] and first["more"]
    rest = rpc("herd_reports", {"limit": 1, "before": {"at": first["reports"][0]["at"], "id": copies[2][0]}})
    assert [r["id"] for r in rest["reports"]] == [other["id"]] and not rest["more"]
    assert str(tmp_path) not in json.dumps(out)
