"""What an agent runs on, and how full its context is, from its transcript.

The herd page shows each claude agent's model, its effort and its
context as a bar: what its last reply was made with, and how many tokens
that reply was given.  Claude writes all three on every reply -- the
model and the usage on the message, the effort beside it -- so the tail
of the transcript says it, with nothing read off a screen.  Claude's own
status line knows the same, but turning it on takes `esc to interrupt'
off the screen, which is how the herd knows an agent is working.

A switch since the last reply -- `/model' and its picker, which the
herd drives for you -- is written as the command's output: shown as
the model the next reply will be on.  A compaction since leaves the
size unknown until then.

Emacs asks again only when the file has changed, as it does for the room.
"""

from __future__ import annotations

import re
from typing import Any

import conversation as conv

TAIL_BYTES = 256 * 1024
CACHE_MAX = 64

# "Set model to `Sonnet 5.5` for this session only with `xhigh` effort",
# "Set model to `Opus 5.5`": the picker's `s', and its Enter or a typed
# `/model <name>'.  "Kept model as" changes nothing.
SET_MODEL = re.compile(r"Set model to `?([^`\n]+?)`?(?: for this session only)?(?: with `?(\w+)`? effort)?\s*\Z")
SET_EFFORT = re.compile(r"Set effort(?: level)? to `?(\w+)`?")
STDOUT = re.compile(r"<local-command-stdout>(.*?)</local-command-stdout>", re.S)
FAMILY = re.compile(r"(opus|sonnet|haiku|fable)[- ](\d+)(?:[-.](\d{1,2})(?!\d))?", re.I)

_cache: dict[str, tuple[int, int, dict[str, Any]]] = {}


def name(model: str) -> str:
    "A model id as its picker names it: claude-opus-5-5 is Opus 5.5."
    m = FAMILY.search(model or "")
    if not m:
        return model or ""
    family, major, minor = m.groups()
    return family.capitalize() + " " + major + ("." + minor if minor else "")


def window(model: str) -> int | None:
    """The context window MODEL runs with, by Claude Code's documentation
    (model-config, 2026-10): 1M for a [1m] variant, and natively for
    Fable, Sonnet 5 and later, Haiku 5.5 and Opus 4.7 and later; 200K
    for the rest.  None for a model it does not name."""
    if re.search(r"\[1m\]|1m context", model or "", re.I):
        return 1_000_000
    m = FAMILY.search(model or "")
    if not m:
        return None
    family, major, minor = m.group(1).lower(), int(m.group(2)), int(m.group(3) or 0)
    v = (major, minor)
    if (family == "fable" or (family == "sonnet" and v >= (5, 0))
            or (family == "haiku" and v >= (5, 5)) or (family == "opus" and v >= (4, 7))):
        return 1_000_000
    return 200_000


def _said(o: dict[str, Any]) -> str:
    "What a local command printed, from its record; empty for anything else."
    msg = o.get("message")
    content = msg.get("content") if isinstance(msg, dict) else o.get("content")
    if not isinstance(content, str):
        return ""
    m = STDOUT.search(content)
    return m.group(1).strip() if m else ""


def _scan(records: list[dict[str, Any]]) -> dict[str, Any] | None:
    """The meter from RECORDS, oldest first: the last reply, and what has
    happened to it since.  None when there is no reply among them."""
    # Since the last reply, the latest of each: read newest first.
    model = effort = None
    compacted = False
    for o in reversed(records):
        if o.get("isSidechain"):
            continue
        kind = o.get("type")
        if kind == "system" and o.get("subtype") == "compact_boundary":
            compacted = True
            continue
        said = _said(o)
        if said:
            m = SET_MODEL.search(said)
            if m and model is None:
                model = m.group(1).strip()
                # Said with the model, or not at all: the new model's own
                # default, which this does not know.
                if effort is None:
                    effort = m.group(2) or ""
            e = SET_EFFORT.search(said)
            if e and not m and effort is None:
                effort = e.group(1)
            continue
        msg = o.get("message")
        if kind != "assistant" or not isinstance(msg, dict):
            continue
        usage = msg.get("usage")
        replied = msg.get("model") or ""
        if not isinstance(usage, dict) or not replied or replied.startswith("<"):
            continue
        tokens = sum(int(usage.get(k) or 0) for k in
                     ("input_tokens", "cache_creation_input_tokens", "cache_read_input_tokens"))
        pending = model is not None
        model = model or name(replied)
        size = window(model if pending else replied)
        # A [1m] variant's id does not say so: what it was given does.  Not
        # for a switch since: a context that will not fit the new window
        # is worth showing as one.
        if size and tokens > size and not pending:
            size = 1_000_000
        return {
            "model": model,
            "effort": (effort if effort is not None else o.get("effort")) or "",
            "tokens": None if compacted else tokens,
            "window": size,
            "pending": pending or effort is not None,
            "compacted": compacted,
            "ts": o.get("timestamp"),
        }
    return None


def read(path: str) -> dict[str, Any]:
    """The meter of the claude transcript PATH; empty when it has no reply
    yet, or is another CLI's."""
    real, cli = conv.locate(path)
    if cli != "claude":
        return {}
    st = real.stat()
    key = str(real)
    hit = _cache.get(key)
    if hit and hit[0] == st.st_size and hit[1] == st.st_mtime_ns:
        return hit[2]
    window_bytes = TAIL_BYTES
    while True:
        lines, cut = conv._tail(real, window_bytes)
        found = _scan(list(conv._records(lines)))
        if found or not cut or window_bytes >= conv.TAIL_MAX:
            break
        window_bytes *= 4
    out = found or {}
    if len(_cache) >= CACHE_MAX:
        _cache.pop(next(iter(_cache)))
    _cache[key] = (st.st_size, st.st_mtime_ns, out)
    return out
