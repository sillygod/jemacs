"""Images -- and HTML reports -- an agent's reply names, kept for the herd
page and Telegram.

An agent says where its screenshots are -- "the screenshots are in
/private/tmp/.../shots/after.png" -- and you want to see them, at the
desk, on your phone and in Telegram, not a path.  So when the herd
records a reply -- an agent's last words at the end of a turn, its answer
to an ask, what it says in its room -- each image the reply names is
copied here under an id no one can guess: the scratch directory it was
in goes away, and an old message still shows it.

What is copied, and nothing else:

- a path in the reply's own text: absolute, ~/ or file:// -- bare, in
  backticks, or the target of a Markdown link or image -- ending in an
  image's extension, with no `..' in it;
- to a regular file, not a symlink (opened O_NOFOLLOW), this user's own,
  of at most MAX_BYTES;
- that is an image by its first bytes -- PNG, JPEG, GIF or WebP -- and
  one Pillow reads, of at most MAX_PIXELS;
- unchanged since the message was written, for a message read later (a
  room's, from its transcript): a file changed after is not the one it
  meant.

A report an agent wrote as an HTML page -- a path ending .html or .htm
-- is copied the same way, as <id>.html: the same checks, at most
MAX_HTML_BYTES, and text that is HTML (UTF-8, no NUL, opening with a
doctype or <html>) instead of an image.  Its <title> names it.  Emacs
serves the copy as a document to read, never to run (a sandbox: no
script, no request but fonts), so only what the page holds itself is
shown -- a stylesheet or image beside it on disk is not.

Nothing takes a path but a reply's text: the page and the bot get ids,
and Emacs serves a copy by its id alone (`ghostherd-reply-images-directory').
The copies are yours alone (0700 / 0600), and go KEEP_DAYS after a
message last named them, or the oldest first past the BUDGET.

Each copy keeps who named it first -- the agent whose reply it was, and
its project, as the herd knew them then (`writer') -- so the
reports can be listed on their own, newest first (`reports'): the Docs
tab's Reports.  A copy first met where the writer is not known (a push
read before its reply was recorded) learns it from the reply's own
capture.

A message is known by its text (`key'): the same words in the Inbox, a
thread, a room or a push find the same images, wherever they are read.
"""

from __future__ import annotations

import hashlib
import html
import io
import os
import re
import secrets
import sqlite3
import stat
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import Any
from urllib.parse import unquote

MAX_BYTES = 16 * 1024 * 1024
MAX_PIXELS = 50_000_000
# An HTML report: its data: images make it big, but it is read whole.
MAX_HTML_BYTES = 8 * 1024 * 1024
# How long a report's title may be, and how far into it it is looked for.
TITLE_CHARS = 200
TITLE_WITHIN = 64 * 1024
# How many images one message brings, at most.
MAX_PER_MESSAGE = 8
# A thumbnail's longest side: sharp at three pixels a point.
THUMB_PX = 480
# A file may be this much newer than the message that names it: written
# as it was being said.
SLACK = 120
# A message's key is its text this long, its spacing aside: a room clips
# a long message, and a hook and a transcript space it differently.
KEY_CHARS = 4000
PRUNE_EVERY = 3600

# What each kind of image starts with, and the extension its copy takes.
KINDS = (
    (b"\x89PNG\r\n\x1a\n", "png", "PNG"),
    (b"\xff\xd8\xff", "jpg", "JPEG"),
    (b"GIF87a", "gif", "GIF"),
    (b"GIF89a", "gif", "GIF"),
)
EXTS = ("png", "jpg", "gif", "webp")
TYPES = {"png": "image/png", "jpg": "image/jpeg", "gif": "image/gif", "webp": "image/webp",
         "html": "text/html"}
ID = re.compile(r"\A[0-9a-f]{32}\Z")
# How long the names of who wrote a copy may be.
WHO_CHARS = 200

# Where a path is named.  An extension it must end in, not glued to more
# of a name (x.png.bak is not one).
_EXT = r"\.(?:png|jpe?g|gif|webp|html?)(?![\w/-]|\.\w)"
_HTML_PATH = re.compile(r"\.html?\Z", re.I)
_DOCTYPE = re.compile(r"(?:<!doctype\s+html|<html)[\s>]", re.I)
_TITLE = re.compile(r"<title\b[^>]*>(.*?)</title\s*>", re.I | re.S)
_START = r"(?:file://|~/|/)"
# The target of a Markdown link or image, which may hold spaces: ](path),
# ](<path>), ](path "title").
_MD = re.compile(r"\]\(\s*<?(" + _START + r"[^\n<>()]*?" + _EXT + r")>?(?:\s+\"[^\"\n]*\")?\s*\)", re.I)
# In backticks, which may hold spaces.
_TICK = re.compile(r"`(" + _START + r"[^`\n]*?" + _EXT + r")`", re.I)
# Bare: no spaces, and not the tail of a URL or a longer word.
_BARE = re.compile(r"(?<![\w.:/~%-])(" + _START + r"[^\s`'\"<>()\[\]{}|,;*]*?" + _EXT + r")", re.I)


def writer(agent: Any, project: Any = None) -> dict[str, str] | None:
    "Who wrote a message, as a copy keeps it: AGENT's name, and its PROJECT."
    agent = str(agent or "").strip()[:WHO_CHARS]
    return {"agent": agent, "project": str(project or "").strip()[:WHO_CHARS * 4]} if agent else None


def key(text: str) -> str:
    "What a message is known by: its text, spacing aside, this far."
    norm = " ".join((text or "").split())[:KEY_CHARS]
    return hashlib.sha256(norm.encode("utf-8")).hexdigest()


def named(text: str) -> list[str]:
    """The image and HTML paths TEXT names, as written, in order, once
    each.  Only a path that reads as one: whether it is one is for `load'."""
    found: list[tuple[int, str]] = []
    for rx in (_MD, _TICK, _BARE):
        for m in rx.finditer(text or ""):
            found.append((m.start(1), m.group(1)))
    out: list[str] = []
    for _, p in sorted(found):
        if p not in out:
            out.append(p)
    return out


def _local(p: str) -> str | None:
    "The file path P names, or None: not absolute, or with `..' in it."
    if p.lower().startswith("file://"):
        p = unquote(p[len("file://"):])
    elif p.startswith("~/"):
        p = os.path.expanduser(p)
    if not p.startswith("/") or "\0" in p:
        return None
    if ".." in p.split("/"):
        return None
    return p


def _kind(head: bytes) -> tuple[str, str] | None:
    "(extension, Pillow's format) of an image starting with HEAD, or None."
    for magic, ext, fmt in KINDS:
        if head.startswith(magic):
            return ext, fmt
    if head[:4] == b"RIFF" and head[8:12] == b"WEBP":
        return "webp", "WEBP"
    return None


def html_text(data: bytes) -> str | None:
    """DATA as text when it is an HTML page -- UTF-8 (a BOM aside), no NUL,
    opening, past blank space and comments, with a doctype or <html> --
    or None."""
    if b"\0" in data:
        return None
    try:
        text = data.decode("utf-8")
    except UnicodeDecodeError:
        return None
    head = text.lstrip("\ufeff").lstrip()
    while head.startswith("<!--"):
        end = head.find("-->")
        if end < 0:
            return None
        head = head[end + 3:].lstrip()
    return text if _DOCTYPE.match(head[:64] + " ") else None


def html_title(text: str) -> str:
    "The title of the HTML page TEXT, on one line, or \"\"."
    m = _TITLE.search(text[:TITLE_WITHIN])
    return " ".join(html.unescape(m.group(1)).split())[:TITLE_CHARS] if m else ""


def load(p: str, at: float | None = None) -> tuple[bytes, str] | None:
    """The bytes of the image or HTML page at P and its extension, or None
    when it is not one to copy (see the module's doc): AT is when the
    message naming it was written, which the file may not be newer than.
    What P ends in says which it must be."""
    path = _local(p)
    if not path:
        return None
    page = bool(_HTML_PATH.search(path))
    cap = MAX_HTML_BYTES if page else MAX_BYTES
    try:
        fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | getattr(os, "O_NONBLOCK", 0))
    except OSError:
        return None  # gone, unreadable, or a symlink (ELOOP)
    try:
        st = os.fstat(fd)
        if (not stat.S_ISREG(st.st_mode) or st.st_uid != os.getuid()
                or not 0 < st.st_size <= cap):
            return None
        if at is not None and st.st_mtime > at + SLACK:
            return None
        with os.fdopen(fd, "rb", closefd=False) as f:
            data = f.read(cap + 1)
    except OSError:
        return None
    finally:
        os.close(fd)
    if len(data) > cap:
        return None
    if page:
        return (data, "html") if html_text(data) is not None else None
    kind = _kind(data[:16])
    return (data, kind[0]) if kind else None


def _pillow():
    try:
        from PIL import Image
    except ImportError:
        return None
    return Image


def _inspect(data: bytes, ext: str) -> tuple[int, int] | None:
    """The size of image DATA when Pillow reads it as what its bytes say
    it is, or None: a fake, or too many pixels.  (0, 0) without Pillow."""
    Image = _pillow()
    if Image is None:
        return 0, 0
    fmt = {"png": "PNG", "jpg": "JPEG", "gif": "GIF", "webp": "WEBP"}[ext]
    try:
        with Image.open(io.BytesIO(data), formats=[fmt]) as im:
            w, h = im.size
    except Exception:  # noqa: BLE001 -- anything Pillow will not read
        return None
    if not (0 < w and 0 < h and w * h <= MAX_PIXELS):
        return None
    return w, h


def _thumbnail(data: bytes) -> bytes | None:
    "A JPEG of image DATA, THUMB_PX at most each way, on white; or None."
    Image = _pillow()
    if Image is None:
        return None
    try:
        with Image.open(io.BytesIO(data)) as im:
            im.seek(0)
            im.draft("RGB", (THUMB_PX, THUMB_PX))
            im.thumbnail((THUMB_PX, THUMB_PX), Image.Resampling.BILINEAR, reducing_gap=2.0)
            if im.mode in ("RGBA", "LA") or (im.mode == "P" and "transparency" in im.info):
                rgba = im.convert("RGBA")
                flat = Image.new("RGB", rgba.size, "white")
                flat.paste(rgba, mask=rgba.getchannel("A"))
            else:
                flat = im.convert("RGB")
            out = io.BytesIO()
            flat.save(out, "JPEG", quality=82, optimize=True)
            return out.getvalue()
    except Exception:  # noqa: BLE001 -- no thumbnail: the page shows the image itself
        return None


def _write(path: Path, data: bytes) -> None:
    "DATA in PATH, whole or not at all, readable by this user alone."
    tmp = path.with_name(f".{path.name}.{secrets.token_hex(4)}")
    fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data)
        os.replace(tmp, path)
    except BaseException:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise


class ImageStore:
    """The copies in ROOT, and which message named which: index.sqlite
    there, so the directory is the whole of it."""

    def __init__(self, root: Path, keep_days: float = 30, budget_mb: float = 512):
        self.root = Path(root)
        self.keep = max(0.0, float(keep_days)) * 86400
        self.budget = max(0, int(float(budget_mb) * 1024 * 1024))
        self._mu = threading.Lock()
        self._conn: sqlite3.Connection | None = None
        self._pruned = 0.0

    def _db(self) -> sqlite3.Connection:
        "The index, made with the directory the first time it is needed."
        if self._conn is None:
            self.root.mkdir(mode=0o700, parents=True, exist_ok=True)
            os.chmod(self.root, 0o700)
            conn = sqlite3.connect(self.root / "index.sqlite", check_same_thread=False)
            conn.execute("PRAGMA journal_mode=WAL")
            conn.executescript(
                """
                CREATE TABLE IF NOT EXISTS images (
                    id TEXT PRIMARY KEY, sha TEXT UNIQUE NOT NULL, ext TEXT NOT NULL,
                    bytes INTEGER NOT NULL, w INTEGER NOT NULL, h INTEGER NOT NULL,
                    thumb INTEGER NOT NULL, created REAL NOT NULL, used REAL NOT NULL);
                -- A message seen: how many images it named (0: none to copy).
                CREATE TABLE IF NOT EXISTS messages (
                    key TEXT PRIMARY KEY, n INTEGER NOT NULL, at REAL NOT NULL);
                CREATE TABLE IF NOT EXISTS refs (
                    key TEXT NOT NULL, ord INTEGER NOT NULL, id TEXT NOT NULL, name TEXT NOT NULL,
                    PRIMARY KEY (key, ord));
                CREATE INDEX IF NOT EXISTS refs_id ON refs (id);
                -- The reports, newest first (`reports').
                CREATE INDEX IF NOT EXISTS images_kind ON images (ext, created, id);
                """
            )
            # A report's title, and who named a copy first: columns an index
            # from before them lacks.
            have = {r[1] for r in conn.execute("PRAGMA table_info(images)")}
            for col in ("title", "agent", "project"):
                if col not in have:
                    conn.execute(f"ALTER TABLE images ADD COLUMN {col} TEXT NOT NULL DEFAULT ''")
            conn.commit()
            self._conn = conn
        return self._conn

    def path(self, rec: dict[str, Any], thumb: bool = False) -> Path:
        "Where the copy REC (or its thumbnail) is."
        return self.root / (f"{rec['id']}.t.jpg" if thumb else f"{rec['id']}.{rec['ext']}")

    def _recs(self, k: str) -> list[dict[str, Any]]:
        "The images message K named, those still kept.  Caller holds `_mu'."
        rows = self._db().execute(
            """SELECT i.id, i.ext, r.name, i.w, i.h, i.thumb, i.bytes, i.title FROM refs r
               JOIN images i ON i.id = r.id WHERE r.key = ? ORDER BY r.ord""", (k,)).fetchall()
        out = []
        for id_, ext, name, w, h, thumb, size, title in rows:
            rec = {"id": id_, "ext": ext, "name": name, "w": w, "h": h, "thumb": bool(thumb), "bytes": size,
                   "title": title}
            if self.path(rec).exists():
                out.append(rec)
        return out

    def capture(self, text: str, at: float | None = None,
                who: dict[str, str] | None = None) -> list[dict[str, Any]]:
        """Copy the images TEXT names, as a reply recorded now: tried again
        when none were found before (a file not written yet).  AT, when
        the message was written, for one read later; see `load'.  WHO
        wrote it (`writer'), for the copies no one is known to have named."""
        k = key(text)
        paths = named(text)
        with self._mu:
            db = self._db()
            seen = db.execute("SELECT n FROM messages WHERE key = ?", (k,)).fetchone()
            if seen and seen[0]:
                if who:
                    db.execute("""UPDATE images SET agent = ?, project = ?
                                  WHERE agent = '' AND id IN (SELECT id FROM refs WHERE key = ?)""",
                               (who["agent"], who["project"], k))
                    db.commit()
                return self._recs(k)
            if not paths:
                return []
            now = time.time()
            recs = []
            for p in paths:
                if len(recs) >= MAX_PER_MESSAGE:
                    break
                rec = self._copy(p, at, now, who)
                if rec and all(r["id"] != rec["id"] for r in recs):
                    recs.append(rec)
            db.execute("INSERT OR REPLACE INTO messages (key, n, at) VALUES (?, ?, ?)", (k, len(recs), now))
            db.execute("DELETE FROM refs WHERE key = ?", (k,))
            db.executemany("INSERT INTO refs (key, ord, id, name) VALUES (?, ?, ?, ?)",
                           [(k, n, r["id"], r["name"]) for n, r in enumerate(recs)])
            db.commit()
        self._maybe_prune()
        return recs

    def lookup(self, text: str, at: float | None = None,
               who: dict[str, str] | None = None) -> list[dict[str, Any]]:
        """The images message TEXT named, as recorded -- or, a message the
        herd has not seen (one in a room, say), copied now when AT says
        when it was written, WHO wrote it.  Without AT, only what was
        recorded."""
        if not text or not named(text):
            return []
        k = key(text)
        with self._mu:
            if self._db().execute("SELECT 1 FROM messages WHERE key = ?", (k,)).fetchone():
                return self._recs(k)
        return self.capture(text, at, who) if at is not None else []

    def _copy(self, p: str, at: float | None, now: float,
              who: dict[str, str] | None = None) -> dict[str, Any] | None:
        """Copy the image or page at P, once by its content, as WHO named it.
        Caller holds `_mu'."""
        got = load(p, at)
        if not got:
            return None
        data, ext = got
        page = ext == "html"
        size = (0, 0) if page else _inspect(data, ext)
        if not size:
            return None
        sha = hashlib.sha256(data).hexdigest()
        db = self._db()
        name = os.path.basename(_local(p) or p)
        row = db.execute("SELECT id, ext, w, h, thumb, bytes, title FROM images WHERE sha = ?", (sha,)).fetchone()
        if row and self.path({"id": row[0], "ext": row[1]}).exists():
            db.execute("UPDATE images SET used = ? WHERE id = ?", (now, row[0]))
            if who:
                db.execute("UPDATE images SET agent = ?, project = ? WHERE id = ? AND agent = ''",
                           (who["agent"], who["project"], row[0]))
            return {"id": row[0], "ext": row[1], "name": name, "w": row[2], "h": row[3],
                    "thumb": bool(row[4]), "bytes": row[5], "title": row[6]}
        if row:  # its file gone from under it: copy afresh
            db.execute("DELETE FROM images WHERE id = ?", (row[0],))
        rec = {"id": secrets.token_hex(16), "ext": ext, "name": name, "w": size[0], "h": size[1],
               "bytes": len(data), "title": html_title(data.decode("utf-8")) if page else ""}
        _write(self.path(rec), data)
        small = None if page else _thumbnail(data)
        if small:
            _write(self.path(rec, thumb=True), small)
        rec["thumb"] = bool(small)
        db.execute("INSERT INTO images (id, sha, ext, bytes, w, h, thumb, created, used, title, agent, project)"
                   " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                   (rec["id"], sha, ext, len(data), rec["w"], rec["h"], int(rec["thumb"]), now, now,
                    rec["title"], (who or {}).get("agent", ""), (who or {}).get("project", "")))
        return rec

    def reports(self, limit: int = 30, before: tuple[float, str] | None = None) -> dict[str, Any]:
        """The HTML reports kept, newest first: LIMIT of them -- older than
        BEFORE, the (`at', id) of the last one shown, when given -- and
        whether there are more.  Each with its title, the name of the file
        it was copied from (never where that was), who named it first, when
        it was copied, its size, and `gone' when its copy is no longer on
        disk (taken from the directory by hand: the index outlives it until
        the copy would have been pruned).  Read from the index and a stat
        of each copy shown, nothing more."""
        sql = ("SELECT i.id, i.title, i.bytes, i.created, i.agent, i.project,"
               " (SELECT r.name FROM refs r WHERE r.id = i.id LIMIT 1)"
               " FROM images i WHERE i.ext = 'html'")
        args: list[Any] = []
        if before:
            sql += " AND (i.created < ? OR (i.created = ? AND i.id < ?))"
            args += [before[0], before[0], before[1]]
        sql += " ORDER BY i.created DESC, i.id DESC LIMIT ?"
        with self._mu:
            rows = self._db().execute(sql, (*args, limit + 1)).fetchall()
        out = [{"id": id_, "title": title, "name": name or f"{id_}.html", "bytes": size, "at": created,
                "agent": agent, "project": project,
                "gone": not self.path({"id": id_, "ext": "html"}).is_file()}
               for id_, title, size, created, agent, project, name in rows[:limit]]
        return {"reports": out, "more": len(rows) > limit}

    def _maybe_prune(self) -> None:
        if time.time() - self._pruned >= PRUNE_EVERY:
            self.prune()

    def prune(self, now: float | None = None) -> int:
        """Drop the copies no message has named for KEEP_DAYS, then the
        oldest past the BUDGET; how many went."""
        now = time.time() if now is None else now
        with self._mu:
            self._pruned = now
            db = self._db()
            gone = [r[0] for r in db.execute(
                "SELECT id FROM images WHERE used < ?", (now - self.keep,))]
            total = 0
            for id_, size in db.execute("SELECT id, bytes FROM images ORDER BY used DESC, created DESC"):
                if id_ in gone:
                    continue
                total += size
                if total > self.budget:
                    gone.append(id_)
            for id_ in gone:
                ext = db.execute("SELECT ext FROM images WHERE id = ?", (id_,)).fetchone()
                for f in (self.path({"id": id_, "ext": ext[0] if ext else "png"}),
                          self.path({"id": id_}, thumb=True)):
                    try:
                        f.unlink()
                    except OSError:
                        pass
                db.execute("DELETE FROM images WHERE id = ?", (id_,))
                # A message whose images went shows none, rather than being
                # copied again from what is at its paths now.
                db.execute("DELETE FROM refs WHERE id = ?", (id_,))
            # Messages seen long ago that hold no image: forgotten, so the
            # table does not grow with every path ever named.
            db.execute("DELETE FROM messages WHERE at < ? AND key NOT IN (SELECT key FROM refs)",
                       (now - self.keep,))
            db.commit()
        return len(gone)

    def close(self) -> None:
        with self._mu:
            if self._conn is not None:
                self._conn.close()
                self._conn = None


def public(recs: list[dict[str, Any]]) -> list[dict[str, Any]]:
    "RECS as the page gets them: ids, never where they came from; a report's title."
    return [dict({"id": r["id"], "ext": r["ext"], "name": r["name"], "w": r["w"], "h": r["h"],
                  "thumb": r["thumb"]}, **({"title": r.get("title") or ""} if r["ext"] == "html" else {}))
            for r in recs]


_store: ImageStore | None = None
_store_mu = threading.Lock()
_worker: ThreadPoolExecutor | None = None


def get_store() -> ImageStore:
    global _store
    with _store_mu:
        if _store is None:
            from config import get_config
            cfg = get_config()
            _store = ImageStore(cfg.images_dir, cfg.images_keep_days, cfg.images_budget_mb)
        return _store


def reset_store() -> None:
    global _store, _worker
    with _store_mu:
        if _worker is not None:
            _worker.shutdown(wait=True)
            _worker = None
        if _store is not None:
            _store.close()
        _store = None


def capture_soon(text: str | None, who: dict[str, str] | None = None) -> None:
    """Copy what TEXT names in the background, one message at a time: the
    hook or `herd reply' that recorded it does not wait on Pillow.  WHO
    wrote it (`writer')."""
    global _worker
    if not text or not named(text):
        return
    with _store_mu:
        if _worker is None:
            _worker = ThreadPoolExecutor(max_workers=1, thread_name_prefix="ghostherd-images")
        worker = _worker
    worker.submit(_capture_quietly, text, who)


def _capture_quietly(text: str, who: dict[str, str] | None = None) -> None:
    try:
        get_store().capture(text, who=who)
    except Exception as exc:  # noqa: BLE001 -- a reply is never failed by its images
        print(f"[ghostherd-memory] images: {exc}", flush=True)


def drain() -> None:
    "Wait for the copies `capture_soon' queued (tests)."
    with _store_mu:
        worker = _worker
    if worker is not None:
        worker.submit(lambda: None).result()


def attach(entries: list[dict[str, Any]], at_of=None, owner=None) -> None:
    """Give each of a conversation's ENTRIES that an agent wrote the
    images it names: `images' for its text, `answer_images' for the
    answer an ask of it got.  AT_OF gives an entry's time (epoch); OWNER,
    called once if at all, whose conversation it is (`writer'): the
    writer of what it says itself -- an answer it got is another's,
    known when that was recorded."""
    store = None
    mine_by: list[dict[str, str] | None] = []
    for e in entries:
        role, kind = e.get("role"), e.get("kind")
        mine = (role in ("assistant", "sub") or (role == "herd" and kind == "reply")
                or (role == "user" and kind == "answer"))
        for field, out in (("text", "images"), ("answer", "answer_images")):
            text = e.get(field)
            if field == "text" and not mine:
                continue
            if field == "answer" and not (role == "herd" and kind == "ask"):
                continue
            if not isinstance(text, str) or not named(text):
                continue
            store = store or get_store()
            at = at_of(e) if at_of else None
            who = None
            if field == "text" and role in ("assistant", "sub", "herd") and owner:
                if not mine_by:
                    try:
                        mine_by.append(owner())
                    except Exception:  # noqa: BLE001 -- a room is never failed by its writer
                        mine_by.append(None)
                who = mine_by[0]
            try:
                recs = store.lookup(text, at, who)
            except Exception:  # noqa: BLE001 -- a room is never failed by its images
                recs = []
            if recs:
                e[out] = public(recs)
