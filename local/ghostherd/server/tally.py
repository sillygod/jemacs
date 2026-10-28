"""What the agents' replies cost, in tokens: each agent's by day, each ask's.

Each CLI writes what every reply was given and what it wrote.  claude on
each assistant record (message.usage) -- one reply is written as several
records with the same usage, so a reply counts once, by its message id and
request id; a synthetic one (its model begins with `<') not at all.  agy on
each planner response.  grok once a turn, as the turn ends: a
`turn_completed' update, in updates.jsonl beside its conversation, whose
input includes what it read from cache.

The herd shows one number for each: input-equivalent tokens, each kind
weighted as it is priced (WEIGHTS) -- input 1, cache read 0.1, cache write
1.25 for the 5-minute cache and 2 for the 1-hour one (1.25 when a reply
does not say which), output 5.  Roughly what eats the plan's quota; the
quota left is in no transcript, so it is not shown.  The kinds themselves
go beside it, where there is room: a title, `herd status'.

The sidecar reads each agent's transcript -- and claude's sub-agents',
beside it -- from where it last stopped (herd.py `_tally_pass'), and keeps
the offsets and the totals by day in herd.sqlite: a restart, or an agent
stopped, loses nothing, and its usage still counts in its project's week.
A pass reads PASS_BYTES at most; one that stops short comes again a moment
later, so a long history is caught up in seconds, never in one go.  Days
are local time.
"""

from __future__ import annotations

import json
import math
from collections import deque
from decimal import ROUND_HALF_UP, Decimal
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Any, Iterable

import conversation

# Seconds between passes, and between those catching up on a long history.
EVERY = 10.0
CATCH_UP = 0.2
# Bytes one pass reads, over all the files; what one read takes at once.
PASS_BYTES = 64 * 1024 * 1024
BLOCK = 4 * 1024 * 1024
# The replies last counted in a file, each by its ids: claude writes one
# reply as several records, one after another, and a pass can end between
# them.  Kept with its offset.
KEYS_KEPT = 32
# How many days a week is: today and the six before it.
WEEK = 7
# A file not read for longer than this, and no live agent's, is let be.
IDLE_DAYS = WEEK + 1

FIELDS = ("input", "cache_write_5m", "cache_write_1h", "cache_read", "output")
WEIGHTS = {"input": 1.0, "cache_write_5m": 1.25, "cache_write_1h": 2.0, "cache_read": 0.1, "output": 5.0}

# What a record with usage has in it, to skip the rest unparsed.
NEEDLE = {"claude": b'"usage"', "agy": b"PLANNER_RESPONSE", "grok": b"turn_completed"}
GROK_UPDATES = "updates.jsonl"


class Usage:
    "Tokens by kind, and how many replies (claude, agy) or turns (grok) they came in."

    __slots__ = FIELDS + ("replies",)

    def __init__(self, **kw: int) -> None:
        for f in self.__slots__:
            setattr(self, f, int(kw.get(f) or 0))

    def add(self, other: "Usage") -> "Usage":
        for f in self.__slots__:
            setattr(self, f, getattr(self, f) + getattr(other, f))
        return self

    def __bool__(self) -> bool:
        return any(getattr(self, f) for f in self.__slots__)

    def weighted(self) -> int:
        return round(sum(getattr(self, f) * WEIGHTS[f] for f in FIELDS))

    def shown(self) -> dict[str, int]:
        "One number, and the kinds beside it."
        return {"weighted": self.weighted(), "input": self.input,
                "cache_write": self.cache_write_5m + self.cache_write_1h,
                "cache_read": self.cache_read, "output": self.output}

    def stored(self) -> dict[str, int]:
        return {f: getattr(self, f) for f in self.__slots__}


def _int(v: Any) -> int:
    return v if isinstance(v, int) and not isinstance(v, bool) and v > 0 else 0


def claude_usage(u: Any) -> Usage | None:
    """A claude reply's message.usage.  Its cache writes by their lifetime
    when it says (cache_creation) -- those win: a reply can say 0 in all
    and 735 for the hour -- and what they do not account for of the total,
    at the 5-minute weight."""
    if not isinstance(u, dict):
        return None
    write = _int(u.get("cache_creation_input_tokens"))
    cc = u.get("cache_creation") if isinstance(u.get("cache_creation"), dict) else {}
    hour, five = _int(cc.get("ephemeral_1h_input_tokens")), _int(cc.get("ephemeral_5m_input_tokens"))
    return Usage(input=_int(u.get("input_tokens")), cache_write_5m=five + max(0, write - five - hour),
                 cache_write_1h=hour,
                 cache_read=_int(u.get("cache_read_input_tokens")), output=_int(u.get("output_tokens")),
                 replies=1)


def agy_usage(o: dict[str, Any]) -> Usage | None:
    "An agy planner response's: its input apart from what it read from cache."
    if not isinstance(o.get("input_tokens"), int):
        return None
    return Usage(input=_int(o.get("input_tokens")), cache_read=_int(o.get("cache_read_tokens")),
                 output=_int(o.get("output_tokens")), replies=1)


def grok_usage(u: Any) -> Usage | None:
    "A grok turn's: its input includes what it read from cache."
    if not isinstance(u, dict):
        return None
    read = _int(u.get("cachedReadTokens"))
    return Usage(input=max(0, _int(u.get("inputTokens")) - read), cache_read=read,
                 cache_write_5m=_int(u.get("cacheCreationTokens")), output=_int(u.get("outputTokens")),
                 replies=1)


def epoch(ts: Any) -> float | None:
    "A record's time: an ISO string (claude, agy) or seconds (grok)."
    if isinstance(ts, (int, float)) and not isinstance(ts, bool):
        return float(ts)
    if isinstance(ts, str) and ts:
        try:
            return datetime.fromisoformat(ts.replace("Z", "+00:00")).timestamp()
        except ValueError:
            return None
    return None


def record(o: Any, cli: str) -> tuple[str | None, float | None, Usage] | None:
    """What record O of CLI's cost: (the reply's key, or None when it needs
    none; when, in seconds; its usage) -- None for a record that is no reply."""
    if not isinstance(o, dict):
        return None
    if cli == "claude":
        msg = o.get("message")
        if o.get("type") != "assistant" or not isinstance(msg, dict):
            return None
        if str(msg.get("model") or "").startswith("<"):
            return None
        u = claude_usage(msg.get("usage"))
        if u is None:
            return None
        key = f"{msg.get('id') or ''}/{o.get('requestId') or ''}"
        return (key if key != "/" else None), epoch(o.get("timestamp")), u
    if cli == "agy":
        if o.get("type") != "PLANNER_RESPONSE" or o.get("source") != "MODEL":
            return None
        u = agy_usage(o)
        return (None, epoch(o.get("created_at")), u) if u is not None else None
    if cli == "grok":
        params = o.get("params") if isinstance(o.get("params"), dict) else {}
        update = params.get("update") if isinstance(params.get("update"), dict) else {}
        if update.get("sessionUpdate") != "turn_completed":
            return None
        u = grok_usage(update.get("usage"))
        if u is None:
            return None
        meta = o.get("_meta") if isinstance(o.get("_meta"), dict) else {}
        ms = meta.get("agentTimestampMs")
        when = ms / 1000 if isinstance(ms, (int, float)) and not isinstance(ms, bool) else epoch(o.get("timestamp"))
        return (str(update.get("prompt_id") or "") or None), when, u
    return None


class Counted:
    "Replies counted once each: the last few keys seen, as a file keeps them."

    def __init__(self, keys: Iterable[str] = ()) -> None:
        self.keys: deque[str] = deque((k for k in keys if isinstance(k, str)), maxlen=KEYS_KEPT)
        self._set = set(self.keys)

    def first(self, key: str | None) -> bool:
        "Whether KEY is new (None always is); it is remembered."
        if key is None:
            return True
        if key in self._set:
            return False
        if len(self.keys) == self.keys.maxlen:
            self._set.discard(self.keys[0])
        self.keys.append(key)
        self._set.add(key)
        return True


def files_of(transcript: str | None) -> list[tuple[Path, str]]:
    """The files an agent's usage is written in, with its CLI: claude's
    conversation and its sub-agents' beside it, agy's transcript, grok's
    updates beside its conversation.  None for one that is no transcript."""
    if not transcript:
        return []
    try:
        real, cli = conversation.locate(transcript)
    except ValueError:
        return []
    if cli == "claude":
        try:
            subs = sorted((real.with_suffix("") / "subagents").glob("agent-*.jsonl"))
        except OSError:
            subs = []
        return [(real, cli)] + [(s, cli) for s in subs]
    if cli == "grok":
        updates = real.with_name(GROK_UPDATES)
        return [(updates, cli)] if updates.is_file() else []
    return [(real, cli)]


def read_from(path: Path, cli: str, offset: int, budget: int,
              ) -> tuple[list[tuple[str | None, float | None, Usage]], int, bool]:
    """The replies PATH has from OFFSET: whole lines only, about BUDGET bytes
    of them (more for one line longer than that).  Them, the offset after
    the last whole line read, and whether there is more to read."""
    needle = NEEDLE[cli]
    out = []
    with path.open("rb") as f:
        f.seek(offset)
        carry = b""
        read = 0
        while True:
            # The budget's rest; a block more for a line longer than it.
            block = f.read(min(BLOCK, budget - read) if read < budget else BLOCK)
            if not block:
                return out, offset, False
            read += len(block)
            data = carry + block
            end = data.rfind(b"\n")
            if end < 0:
                carry = data
                continue
            carry = data[end + 1:]
            for line in data[:end].split(b"\n"):
                if needle not in line:
                    continue
                try:
                    found = record(json.loads(line), cli)
                except ValueError:
                    continue
                if found:
                    out.append(found)
            offset += end + 1
            if read >= budget:
                return out, offset, True


def day(when: float | None, today: date | None = None) -> str:
    "The local day of WHEN, in seconds; today for a record that says none."
    if when is None:
        return (today or date.today()).isoformat()
    return datetime.fromtimestamp(when).date().isoformat()


def text(n: int) -> str:
    """N as the page writes it (app.js `tokens'): 12M, 1.3M, 340k, 999.  As
    JavaScript's toFixed rounds: the float's own value, a half up -- 1.25M
    is 1.3M, but 1.65M, which as a float is 1.6499..., is 1.6M."""
    if n >= 1_000_000:
        if not n % 1_000_000:
            return f"{n // 1_000_000}M"
        v = Decimal(n / 1_000_000).quantize(Decimal("0.1"), ROUND_HALF_UP).normalize()
        return f"{v:f}M"
    if n >= 1000:
        return f"{math.floor(n / 1000 + 0.5)}k"
    return str(n)


def _shown_of(row: dict[str, Any]) -> Usage:
    return Usage(**{f: row.get(f) for f in Usage.__slots__})


def summary(rows: Iterable[dict[str, Any]], today: date) -> dict[str, Any]:
    """What the page shows from ROWS, the totals of each agent's days in a
    project (tally_days): each agent's today and its last WEEK days, and each
    project's, with its lead's share apart from its workers'.  A lead is the
    project's lead, or primary."""
    first = (today - timedelta(days=WEEK - 1)).isoformat()
    now = today.isoformat()
    agents: dict[tuple[str, str], dict[str, Any]] = {}
    projects: dict[str, dict[str, Any]] = {}
    for r in rows:
        if r["day"] < first or r["day"] > now:
            continue
        u = _shown_of(r)
        a = agents.setdefault((r["agent"], r["project"]), {
            "name": r["agent"], "project": r["project"], "role": r.get("role") or "",
            "today": Usage(), "week": Usage()})
        p = projects.setdefault(r["project"], {
            "project": r["project"], "today": Usage(), "week": Usage(),
            "lead": {"today": Usage(), "week": Usage()}, "workers": {"today": Usage(), "week": Usage()}})
        side = p["lead"] if (r.get("role") or "") in ("lead", "primary") else p["workers"]
        for t in (a, p, side):
            t["week"].add(u)
            if r["day"] == now:
                t["today"].add(u)

    def shown(t: dict[str, Any]) -> dict[str, Any]:
        return {k: (v.shown() if isinstance(v, Usage) else shown(v) if isinstance(v, dict) else v)
                for k, v in t.items()}
    return {"today": now, "days": WEEK,
            "agents": [shown(a) for a in agents.values()],
            "projects": [shown(p) for p in projects.values()]}


# ---- An ask's: the work between its delivery and its answer ------------------


def grok_turns(conversation_file: Path, since: float | None, until: float | None) -> Usage | None:
    """grok's turns that ended from SINCE to UNTIL, in seconds, from the
    updates beside CONVERSATION_FILE (its tail, back to SINCE); None when
    there are none to read."""
    updates = conversation_file.with_name(GROK_UPDATES)
    try:
        size = updates.stat().st_size
    except OSError:
        return None
    total = Usage()
    window = 1024 * 1024
    while True:
        start = max(0, size - window)
        with updates.open("rb") as f:
            f.seek(start)
            data = f.read(size - start)
        total, oldest = Usage(), None
        for line in data.split(b"\n")[1 if start else 0:]:
            if NEEDLE["grok"] not in line:
                continue
            try:
                found = record(json.loads(line), "grok")
            except ValueError:
                continue
            if not found or found[1] is None:
                continue
            oldest = found[1] if oldest is None else min(oldest, found[1])
            if (since is None or found[1] >= since) and (until is None or found[1] <= until):
                total.add(found[2])
        if start == 0 or (oldest is not None and since is not None and oldest < since) or window >= 32 * 1024 * 1024:
            return total
        window *= 4
