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

import json
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


def _grow(real, scan):
    "Read REAL's tail, growing it until SCAN finds something or the file ends."
    window_bytes = TAIL_BYTES
    while True:
        lines, cut = conv._tail(real, window_bytes)
        found = scan(list(conv._records(lines)))
        if found or not cut or window_bytes >= conv.TAIL_MAX:
            return found or {}
        window_bytes *= 4


def _claude_meter(real):
    "claude writes model, effort and usage on every reply; read the tail."
    return _grow(real, _scan)


def _agy_scan(records):
    """agy's context from the last planner reply: the prompt it was given
    (input + what it read from cache).  No window is written anywhere, so
    the herd shows the number without a bar.  Model and effort are only on
    its screen, read there, not here.  A compaction since that reply -- a
    CHECKPOINT record newer than it -- clears the number, the way claude
    does on a compact_boundary: the context was just reset and no reply has
    sized it yet, so the pre-compaction number would mislead."""
    for o in reversed(records):
        kind = o.get("type")
        if kind == "CHECKPOINT":
            return {"model": "", "effort": "", "tokens": None, "window": None,
                    "pending": False, "compacted": True, "ts": o.get("created_at")}
        if kind == "PLANNER_RESPONSE" and o.get("source") == "MODEL":
            inp = o.get("input_tokens")
            if isinstance(inp, int):
                cached = o.get("cache_read_tokens")
                return {"model": "", "effort": "",
                        "tokens": inp + (cached if isinstance(cached, int) else 0),
                        "window": None, "pending": False, "compacted": False,
                        "ts": o.get("created_at")}
    return None


def _agy_meter(real):
    return _grow(real, _agy_scan)


def _grok_meter(real):
    """grok keeps its context in signals.json beside the conversation: the
    tokens in use and the window itself, in lockstep with it.  Its model and
    effort are read off its screen, not here."""
    try:
        data = json.loads(real.with_name("signals.json").read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    used = data.get("contextTokensUsed")
    if not isinstance(used, int):
        return {}
    total = data.get("contextWindowTokens")
    return {"model": "", "effort": "", "tokens": used,
            "window": total if isinstance(total, int) and total > 0 else None,
            "pending": False, "compacted": False, "ts": None}


_READERS = {"claude": _claude_meter, "agy": _agy_meter, "grok": _grok_meter}


def _target(real, cli):
    "The file whose change means the meter changed; grok keeps it in signals.json."
    if cli == "grok":
        sig = real.with_name("signals.json")
        if sig.is_file():
            return sig
    return real


def read(path: str) -> dict[str, Any]:
    """The meter of transcript PATH -- model, effort, context tokens and the
    window they sit in; empty when the CLI is unknown or it has no reply yet.
    claude, grok and agy each keep it differently; grok and agy carry no
    model or effort here (the herd reads those off their screen)."""
    real, cli = conv.locate(path)
    reader = _READERS.get(cli)
    if not reader:
        return {}
    try:
        st = _target(real, cli).stat()
    except OSError:
        return {}
    key = str(real)
    hit = _cache.get(key)
    if hit and hit[0] == st.st_size and hit[1] == st.st_mtime_ns:
        return hit[2]
    out = reader(real)
    if len(_cache) >= CACHE_MAX:
        _cache.pop(next(iter(_cache)))
    _cache[key] = (st.st_size, st.st_mtime_ns, out)
    return out
