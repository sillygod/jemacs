#!/usr/bin/env python3
"""The herd's trust of the worktrees it creates (ask 2befbbd6).

A CLI asks, the first time it starts in a folder, whether to trust it,
and an agent parked on that prompt takes no work.  When the herd has
just created a git worktree for an agent off a project its CLI already
trusts -- by that CLI's own record -- it records the worktree trusted in
the same store: the same repository, so no new trust.  Only that one
entry is touched, the way its CLI keeps it.

  herd_trust.py trusts CLI STORE PATH   exit 0 when CLI's record trusts PATH
  herd_trust.py add CLI STORE PATH      record PATH; prints added or present
  herd_trust.py remove CLI STORE PATH   drop PATH; prints removed or absent

grok: STORE is ~/.grok/trusted_folders.toml, a [folders."PATH"] table of
`trusted = true' and `decided_at' (epoch seconds) per folder, exact; it
is saved under STORE.lock, which grok locks for its own saves, re-reading
the store first -- so neither save loses the other's.
agy: STORE is ~/.gemini/antigravity-cli/settings.json, PATH an entry of
its "trustedWorkspaces" list.  agy keeps no lock: the file is read again
just before it is replaced, and replaced only if it did not change since.
claude needs none: it counts a linked worktree trusted when its main
checkout is (claude 2.1.310, measured), so ~/.claude.json is never
written.

A store that cannot be read as its CLI writes it is left as it is, and
the answer is no: the CLI then asks, as it would have.  Every write goes
to a temporary file beside the store, then over it in one rename.
"""

from __future__ import annotations

import contextlib
import fcntl
import json
import os
import re
import sys
import tempfile
import time

try:
    import tomllib
except ImportError:  # Python < 3.11: checked by our own reading only
    tomllib = None

LOCK_WAIT = 5.0
TRIES = 3


class Refused(Exception):
    "The store is not as its CLI writes it, or is busy: leave it be."


@contextlib.contextmanager
def _locked(store):
    "STORE.lock held exclusively, as grok holds it while it saves."
    fd = os.open(store + ".lock", os.O_RDWR | os.O_CREAT, 0o644)
    try:
        end = time.monotonic() + LOCK_WAIT
        while True:
            try:
                fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except BlockingIOError:
                if time.monotonic() > end:
                    raise Refused(f"{store}.lock is held")
                time.sleep(0.05)
        yield
    finally:
        os.close(fd)


def _read(store):
    "(TEXT, STAT) of STORE; ('', None) when there is none yet."
    try:
        with open(store, encoding="utf-8") as f:
            st = os.fstat(f.fileno())
            return f.read(), st
    except FileNotFoundError:
        return "", None


def _same(store, st):
    "STORE is as it was when read with stat ST."
    try:
        now = os.stat(store)
    except FileNotFoundError:
        return st is None
    return st is not None and (now.st_mtime_ns, now.st_size, now.st_ino) == (
        st.st_mtime_ns, st.st_size, st.st_ino)


def _replace(store, text, st):
    "STORE becomes TEXT in one rename, its mode kept (0600 when new)."
    os.makedirs(os.path.dirname(store) or ".", exist_ok=True)
    fd, tmp = tempfile.mkstemp(dir=os.path.dirname(store) or ".",
                               prefix="." + os.path.basename(store) + ".herd-")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            f.write(text)
            f.flush()
            os.fchmod(f.fileno(), (st.st_mode & 0o777) if st else 0o600)
            os.fsync(f.fileno())
        os.replace(tmp, store)
    except BaseException:
        with contextlib.suppress(FileNotFoundError):
            os.unlink(tmp)
        raise


# ---- grok ---------------------------------------------------------------

def _toml_key(path):
    "PATH as a TOML basic string, as grok's table names are written."
    out = []
    for ch in path:
        if ch in '"\\':
            out.append("\\" + ch)
        elif ord(ch) < 0x20 or ord(ch) == 0x7F:
            out.append("\\u%04X" % ord(ch))
        else:
            out.append(ch)
    return '"' + "".join(out) + '"'


def _grok_folders(text):
    "PATH -> table, of grok's store TEXT."
    if not text.strip():
        return {}
    if tomllib is None:
        raise Refused("no TOML reader in this Python")
    try:
        data = tomllib.loads(text)
    except tomllib.TOMLDecodeError as exc:
        raise Refused(f"not TOML: {exc}")
    folders = data.get("folders", {})
    if not isinstance(folders, dict):
        raise Refused("folders is not a table")
    return folders


def _grok_header(path):
    return re.compile(r"^\[folders\." + re.escape(_toml_key(path)) + r"\][ \t]*$", re.M)


def grok_has(text, path):
    return path in _grok_folders(text)


def grok_trusts(text, path):
    entry = _grok_folders(text).get(path)
    return isinstance(entry, dict) and entry.get("trusted") is True


def grok_add(text, path):
    "TEXT with PATH's table added, or None when it is there already."
    if path in _grok_folders(text):
        return None
    block = f"[folders.{_toml_key(path)}]\ntrusted = true\ndecided_at = {int(time.time())}\n"
    if text.strip():
        text = text.rstrip("\n") + "\n\n" + block
    else:
        text = block
    return text


def grok_remove(text, path):
    "TEXT without PATH's table, or None when it has none."
    if path not in _grok_folders(text):
        return None
    m = _grok_header(path).search(text)
    if not m:
        raise Refused("its table is not written as grok writes it")
    nxt = re.compile(r"^\[", re.M).search(text, m.end())
    end = nxt.start() if nxt else len(text)
    head, tail = text[:m.start()].rstrip("\n"), text[end:].lstrip("\n")
    if head and tail:
        return head + "\n\n" + tail
    return head + "\n" if head else tail


# ---- agy ----------------------------------------------------------------

def _agy_settings(text):
    if not text.strip():
        return {}
    try:
        data = json.loads(text)
    except ValueError as exc:
        raise Refused(f"not JSON: {exc}")
    if not isinstance(data, dict):
        raise Refused("not a JSON object")
    trusted = data.get("trustedWorkspaces", [])
    if not isinstance(trusted, list):
        raise Refused("trustedWorkspaces is not a list")
    return data


def _agy_text(data):
    "DATA as agy writes its settings: two spaces, a final newline."
    return json.dumps(data, indent=2, ensure_ascii=False) + "\n"


def agy_has(text, path):
    return path in _agy_settings(text).get("trustedWorkspaces", [])


def agy_trusts(text, path):
    trusted = _agy_settings(text).get("trustedWorkspaces", [])
    return path in trusted or path.rstrip("/") + "/" in trusted


def agy_add(text, path):
    data = _agy_settings(text)
    trusted = data.get("trustedWorkspaces", [])
    if path in trusted:
        return None
    data["trustedWorkspaces"] = trusted + [path]
    return _agy_text(data)


def agy_remove(text, path):
    data = _agy_settings(text)
    trusted = data.get("trustedWorkspaces", [])
    if path not in trusted:
        return None
    data["trustedWorkspaces"] = [p for p in trusted if p != path]
    return _agy_text(data)


# CLI -> (trusts, has, add, remove, locks): whether its record trusts a
# path, holds an entry for it, the store with one added or removed, and
# whether it saves under STORE.lock.
CLIS = {
    "grok": (grok_trusts, grok_has, grok_add, grok_remove, True),
    "agy": (agy_trusts, agy_has, agy_add, agy_remove, False),
}


def change(cli, store, path, adding):
    """Add or remove PATH in CLI's STORE (a link is written through):
    'added'/'present' or 'removed'/'absent'."""
    _, has, add, remove, locks = CLIS[cli]
    target = os.path.realpath(store)
    with _locked(store) if locks else contextlib.nullcontext():
        for _ in range(TRIES):
            text, st = _read(target)
            new = (add if adding else remove)(text, path)
            if new is None:
                return "present" if adding else "absent"
            if has(new, path) != adding:
                raise Refused("the change did not read back")
            if not _same(target, st):
                continue  # written meanwhile: read it again
            _replace(target, new, st)
            return "added" if adding else "removed"
    raise Refused(f"{store} kept changing")


def main(argv):
    if len(argv) != 4 or argv[0] not in ("trusts", "add", "remove") or argv[1] not in CLIS:
        print(__doc__.split("\n\n")[1], file=sys.stderr)
        return 2
    op, cli, store, path = argv
    store = os.path.abspath(os.path.expanduser(store))
    if not os.path.isabs(path) or os.path.normpath(path) in ("/", os.path.expanduser("~")):
        print(f"herd_trust: refusing {path!r}: not a folder of its own", file=sys.stderr)
        return 2
    try:
        if op == "trusts":
            text, _ = _read(os.path.realpath(store))
            return 0 if CLIS[cli][0](text, path) else 1
        print(change(cli, store, path, op == "add"))
        return 0
    except (Refused, OSError) as exc:
        print(f"herd_trust: {store}: {exc}", file=sys.stderr)
        return 3


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
