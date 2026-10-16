"""Inter-agent mail and Telegram commands on the sidecar sqlite.

Agents POST herd_message.  Emacs herd_tick pushes a session snapshot
and pulls whatever is deliverable: the target exists and is not
working.  The sidecar never types into a PTY.

Telegram taps write `commands`; Emacs runs them and returns `replies`.

An ask is mail that wants an answer.  herd_ask queues it like mail and
gives it an id; the target answers with herd_reply; the asker either
waits on herd_await or, when it does not, gets the answer back as mail.
Emacs closes an ask the target finished without answering (herd_settle),
so an asker is never left waiting on an agent that forgot.

Agents report on themselves through their CLI's hooks (herd_report): a
state, which conversation they are on, and the answer that ended a
turn.  Emacs takes the states on its tick; the sidecar keeps the rest
(`links'), and closes an unanswered ask with the taker's own last
answer rather than its screen.  A turn that ends with work it started
still out -- asks of its own, sub-agents, shells or monitors running --
ends to wait: `waiting', its ask left open, until a turn ends with
nothing out.
"""

from __future__ import annotations

import hashlib
import json
import sqlite3
import threading
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import subagents
from config import get_config

_lock = threading.Lock()
_herd: "HerdStore | None" = None

ASK_OPEN = ("queued", "delivered")
ASK_CLOSED = ("answered", "failed", "cancelled")

# Who takes an ask addressed to a kind, best first: free now, free soon,
# waiting on the human.  A state not listed (dead) never takes one.
_TAKER_RANK = {"idle": 0, "done": 0, "working": 1, "starting": 1, "waiting": 1, "blocked": 2}

# How list_sessions orders the herd, matching the web page's RANK:
# the state that needs you first, then the most recently updated.
_STATE_RANK = {
    "blocked": 0,
    "working": 1,
    "starting": 2,
    "waiting": 3,
    "done": 4,
    "idle": 5,
    "dead": 6,
}

# How many asks may wait on one another, lead to rd to qa: one may be
# asked while a third of its chain waits on it, and asks no further.
ASK_DEPTH = 3

# Rounds: how many times an agent may ask one other agent while it
# answers one ask -- rd taking the lead's work through qa.  The asker
# of that ask says how many (`rounds'); this many when it does not, and
# for an agent nobody asked, in one run: from its first ask until it
# finishes a turn with none open.  What stops a loop is the herd, then,
# not the agents remembering the number they were given.
ASK_ROUNDS = 5
ASK_ROUNDS_MAX = 20

# An answer a waiting asker has not collected after this long is mailed
# to it instead: the `herd ask --wait' that would have printed it died.
UNCOLLECTED_SECONDS = 120

# An agent waiting on what it started, with nothing out any more and no
# word from it or its conversation this long, did not wake to it -- its
# sub-agents killed, say: done.  A sub-agent coming back writes to its
# parent's conversation at once, and wakes it a moment later.
WAIT_GRACE = 30

# Closed asks the page still shows, for this long.
RECENT_SECONDS = 1800

# Settled mail, asks, commands, replies, and pushes kept in herd.sqlite
# before tick prunes them.
HISTORY_SECONDS = 7 * 86400


def _ms(iso: str | None) -> float | None:
    "Epoch milliseconds of ISO, for pages that should not parse dates."
    return datetime.fromisoformat(iso).timestamp() * 1000 if iso else None


def _head(text: str | None, width: int = 120) -> str:
    "First non-blank line of TEXT, cut to WIDTH."
    for line in (text or "").splitlines():
        line = line.strip()
        if line:
            return line if len(line) <= width else line[: width - 1] + "…"
    return ""


def _and(words: list[str]) -> str:
    "WORDS as a list in a sentence: a, b and c."
    return words[0] if len(words) == 1 else ", ".join(words[:-1]) + " and " + words[-1]


def _background(transcript: str | None) -> list[str]:
    """What the conversation at TRANSCRIPT has running in the background,
    which wakes it as each ends, in words: 2 sub-agents, 1 shell, 1 monitor.
    Per-CLI dispatch is in subagents.py, so this serves claude, agy and grok."""
    if not transcript:
        return []
    try:
        subs = len(subagents.live(transcript)["subs"])
        kinds = [t["kind"] for t in subagents.tasks(transcript)]
    except (ValueError, OSError):
        return []
    counts = ((subs, "sub-agent"), (kinds.count("shell"), "shell"), (kinds.count("monitor"), "monitor"))
    return [f"{n} {what}{'' if n == 1 else 's'}" for n, what in counts if n]


def _mtime(path: str | None) -> float:
    "When PATH last changed, or 0."
    try:
        return Path(path).stat().st_mtime if path else 0.0
    except OSError:
        return 0.0


class HerdStore:
    def __init__(self, path: Path, inline_limit: int = 4000):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.inline_limit = max(1, int(inline_limit))
        self._mu = threading.Lock()
        self._conn = sqlite3.connect(self.path, check_same_thread=False)
        self._conn.execute("PRAGMA journal_mode=WAL")
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS snapshot (
                name TEXT PRIMARY KEY,
                kind TEXT,
                state TEXT,
                notes TEXT,
                project TEXT
            )
            """
        )
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS mail (
                id TEXT PRIMARY KEY,
                from_name TEXT NOT NULL,
                to_name TEXT NOT NULL,
                body TEXT NOT NULL,
                submit INTEGER NOT NULL,
                handoff INTEGER NOT NULL,
                status TEXT NOT NULL,
                error TEXT,
                created_at TEXT NOT NULL,
                delivered_at TEXT
            )
            """
        )
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS meta (
                k TEXT PRIMARY KEY,
                v TEXT
            )
            """
        )
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS aliases (
                short TEXT PRIMARY KEY,
                name TEXT UNIQUE NOT NULL
            )
            """
        )
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS commands (
                id TEXT PRIMARY KEY,
                op TEXT NOT NULL,
                session TEXT NOT NULL,
                args TEXT,
                chat_id INTEGER,
                status TEXT NOT NULL,
                created_at TEXT NOT NULL
            )
            """
        )
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS replies (
                id TEXT PRIMARY KEY,
                chat_id INTEGER,
                text TEXT NOT NULL,
                sent INTEGER NOT NULL DEFAULT 0
            )
            """
        )
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS asks (
                id TEXT PRIMARY KEY,
                from_name TEXT NOT NULL,
                to_name TEXT NOT NULL,
                body TEXT NOT NULL,
                wait INTEGER NOT NULL,
                status TEXT NOT NULL,
                reply TEXT,
                auto INTEGER NOT NULL DEFAULT 0,
                replied_by TEXT,
                error TEXT,
                collected INTEGER NOT NULL DEFAULT 0,
                created_at TEXT NOT NULL,
                delivered_at TEXT,
                answered_at TEXT
            )
            """
        )
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS reports (
                session TEXT PRIMARY KEY,
                state TEXT NOT NULL,
                reason TEXT,
                at TEXT NOT NULL,
                taken INTEGER NOT NULL DEFAULT 0
            )
            """
        )
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS links (
                session TEXT PRIMARY KEY,
                cli TEXT,
                conversation TEXT,
                transcript TEXT,
                last_message TEXT,
                last_at TEXT,
                updated_at TEXT NOT NULL
            )
            """
        )
        # What Emacs pushes to your phone, as it handed it over; sent by the
        # Telegram bot.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS pushes (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                push TEXT NOT NULL,
                created_at TEXT NOT NULL,
                sent INTEGER NOT NULL DEFAULT 0
            )
            """
        )
        # Which project a push to a chat was about: a reply to it goes to
        # that project's lead.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS pushed (
                chat_id INTEGER NOT NULL,
                message_id INTEGER NOT NULL,
                project TEXT NOT NULL,
                at TEXT NOT NULL,
                PRIMARY KEY (chat_id, message_id)
            )
            """
        )
        # The run of asks an agent nobody asked is on: the first ask's id.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS runs (
                session TEXT PRIMARY KEY,
                run TEXT NOT NULL
            )
            """
        )
        cols = {
            r[1] for r in self._conn.execute("PRAGMA table_info(snapshot)")
        }
        if "reason" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN reason TEXT")
        if "screen" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN screen TEXT")
        if "role" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN role TEXT")
        # A blocked agent's menu, as Emacs read it off its screen (JSON).
        if "prompt" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN prompt TEXT")
        # The address of its project's room from your phone, without the
        # page's secret: for the bot to link to.
        if "room" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN room TEXT")
        if "since" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN since REAL")
        mail_cols = {
            r[1] for r in self._conn.execute("PRAGMA table_info(mail)")
        }
        if "ask_id" not in mail_cols:
            self._conn.execute("ALTER TABLE mail ADD COLUMN ask_id TEXT")
        ask_cols = {
            r[1] for r in self._conn.execute("PRAGMA table_info(asks)")
        }
        if "auto_source" not in ask_cols:
            self._conn.execute("ALTER TABLE asks ADD COLUMN auto_source TEXT")
        # The ask its asker was answering, the run it belongs to, its round
        # with its taker and how many it may have; and how many its own
        # taker may have with any one agent (NULL: ASK_ROUNDS).
        for col, typ in (("parent", "TEXT"), ("run", "TEXT"), ("round", "INTEGER"),
                         ("round_of", "INTEGER"), ("rounds", "INTEGER")):
            if col not in ask_cols:
                self._conn.execute(f"ALTER TABLE asks ADD COLUMN {col} {typ}")
        self._conn.commit()

    def close(self) -> None:
        with self._mu:
            self._conn.close()

    def _synced(self) -> bool:
        row = self._conn.execute(
            "SELECT v FROM meta WHERE k = 'synced'"
        ).fetchone()
        return bool(row and row[0] == "1")

    def list_sessions(self, project: str | None = None) -> dict[str, Any]:
        with self._mu:
            if project:
                rows = self._conn.execute(
                    """
                    SELECT name, kind, state, notes, project, reason, role, since
                    FROM snapshot WHERE project = ?
                    """,
                    (project,),
                ).fetchall()
            else:
                rows = self._conn.execute(
                    """
                    SELECT name, kind, state, notes, project, reason, role, since
                    FROM snapshot
                    """
                ).fetchall()
        rows.sort(
            key=lambda r: (
                _STATE_RANK.get(r[2] or "", 9),
                -(float(r[7]) if isinstance(r[7], (int, float)) else 0.0),
                r[0],
            )
        )
        return {
            "sessions": [
                {
                    "name": r[0],
                    "kind": r[1] or "",
                    "state": r[2] or "",
                    "notes": r[3] or "",
                    "project": r[4] or "",
                    "reason": r[5] or "",
                    "role": r[6] or "",
                    "short": self.short_for(r[0]),
                }
                for r in rows
            ]
        }

    def enqueue(
        self,
        to: str,
        body: str,
        from_name: str | None = None,
        submit: bool = True,
        handoff: bool = False,
    ) -> dict[str, Any]:
        to = (to or "").strip()
        if not to:
            raise ValueError("to is required")
        sender = (from_name or "").strip() or "unknown"
        mail_id = str(uuid.uuid4())
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            self._conn.execute(
                """
                INSERT INTO mail (
                    id, from_name, to_name, body, submit, handoff,
                    status, created_at
                ) VALUES (?, ?, ?, ?, ?, ?, 'queued', ?)
                """,
                (mail_id, sender, to, body if body is not None else "",
                 1 if submit else 0, 1 if handoff else 0, now),
            )
            self._conn.commit()
        return {"id": mail_id, "queued": True}

    def inbox(self, session: str, limit: int = 50) -> dict[str, Any]:
        name = (session or "").strip()
        if not name:
            raise ValueError("session is required")
        try:
            limit = max(1, int(limit))
        except (TypeError, ValueError) as exc:
            raise ValueError("limit must be an integer") from exc
        with self._mu:
            rows = self._conn.execute(
                """
                SELECT id, from_name, to_name, body, created_at, delivered_at
                FROM mail
                WHERE to_name = ? AND status = 'delivered'
                ORDER BY delivered_at DESC, created_at DESC
                LIMIT ?
                """,
                (name, limit),
            ).fetchall()
        return {
            "messages": [
                {
                    "id": r[0],
                    "from": r[1],
                    "to": r[2],
                    "body": r[3],
                    "created_at": r[4],
                    "delivered_at": r[5],
                }
                for r in rows
            ]
        }

    # ---- Asks ------------------------------------------------------------

    def ask(
        self,
        to: str,
        body: str,
        from_name: str | None = None,
        wait: bool = False,
        rounds: int | None = None,
    ) -> dict[str, Any]:
        """Queue BODY for TO as an ask.  TO is a session or a kind.

        WAIT says the asker will collect the answer with herd_await;
        otherwise the answer comes back to it as mail.  ROUNDS is how
        many times the taker may ask any one agent while it answers this
        (ASK_ROUNDS when not given).
        """
        if body is None or not str(body).strip():
            raise ValueError("body is required")
        if rounds is not None:
            try:
                ok = int(rounds) == rounds and 1 <= rounds <= ASK_ROUNDS_MAX
            except (TypeError, ValueError):
                ok = False
            if not ok or isinstance(rounds, bool):
                raise ValueError(f"rounds must be a whole number from 1 to {ASK_ROUNDS_MAX}")
            rounds = int(rounds)
        sender = (from_name or "").strip() or "user"
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            if not self._synced():
                raise ValueError(
                    "Emacs has not shared the herd yet; is ghostherd-mode on?"
                )
            # An agent answering an ask may hand part of it on -- the lead
            # asks rd, rd asks qa -- but never to one waiting on it, which
            # is how two agents end up waiting on each other; and only so
            # deep.
            chain = self._chain(sender)
            if len(chain) >= ASK_DEPTH:
                raise ValueError(
                    f"{sender} is answering for {' <- '.join(chain)}, {len(chain)} asks deep; "
                    "answer rather than ask further"
                )
            target = self._resolve_taker(to, sender, exclude=chain)
            if target in chain:
                raise ValueError(
                    f"{target} is waiting on {sender} ({sender} <- {' <- '.join(chain)}); "
                    "answer its ask rather than ask it"
                )
            ask_id = self._new_ask_id()
            parent, run, n, cap = self._round(sender, target, ask_id)
            if cap is not None and n >= cap:
                if parent:
                    raise ValueError(
                        f"{sender} has asked {target} {n} times for ask {parent['id']}, "
                        f"{cap} at most; answer {parent['from_name']} with where it stands "
                        "rather than ask again"
                    )
                raise ValueError(
                    f"{sender} has asked {target} {n} times in this run of asks, {cap} at most "
                    f"(it ends when {sender} finishes a turn with no ask open); "
                    "say where it stands rather than ask again"
                )
            if run == ask_id:
                self._conn.execute(
                    "INSERT OR REPLACE INTO runs (session, run) VALUES (?, ?)", (sender, run)
                )
            self._conn.execute(
                """
                INSERT INTO asks (id, from_name, to_name, body, wait,
                                  status, created_at, parent, run, round,
                                  round_of, rounds)
                VALUES (?, ?, ?, ?, ?, 'queued', ?, ?, ?, ?, ?, ?)
                """,
                (ask_id, sender, target, str(body), 1 if wait else 0, now,
                 parent["id"] if parent else None, run,
                 n + 1 if cap is not None else None, cap, rounds),
            )
            self._conn.execute(
                """
                INSERT INTO mail (
                    id, from_name, to_name, body, submit, handoff,
                    status, created_at, ask_id
                ) VALUES (?, ?, ?, ?, 1, 0, 'queued', ?, ?)
                """,
                (str(uuid.uuid4()), sender, target, str(body), now, ask_id),
            )
            self._conn.commit()
        out: dict[str, Any] = {"id": ask_id, "to": target, "queued": True}
        if cap is not None:
            out.update(round=n + 1, of=cap)
        return out

    def _round(self, sender: str, target: str, ask_id: str):
        """Where SENDER asking TARGET stands: (PARENT, RUN, N, CAP) --
        the ask it is answering (a dict, or None), the run the new ask
        ASK_ID joins, how many times it has asked TARGET in it already,
        and how many it may.  CAP is None for one outside the herd: the
        user is not held to rounds.  Caller holds `_mu`."""
        row = self._conn.execute(
            """
            SELECT id, from_name, run, rounds FROM asks
            WHERE to_name = ? AND status = 'delivered'
            ORDER BY delivered_at DESC LIMIT 1
            """,
            (sender,),
        ).fetchone()
        if row:
            parent = {"id": row[0], "from_name": row[1]}
            n = self._conn.execute(
                "SELECT COUNT(*) FROM asks WHERE from_name = ? AND to_name = ? AND parent = ?",
                (sender, target, row[0]),
            ).fetchone()[0]
            return parent, row[2] or row[0], n, row[3] or ASK_ROUNDS
        if not self._conn.execute(
            "SELECT 1 FROM snapshot WHERE name = ?", (sender,)
        ).fetchone():
            return None, ask_id, 0, None
        run = self._conn.execute(
            "SELECT run FROM runs WHERE session = ?", (sender,)
        ).fetchone()
        if not run:
            return None, ask_id, 0, ASK_ROUNDS
        n = self._conn.execute(
            """
            SELECT COUNT(*) FROM asks
            WHERE from_name = ? AND to_name = ? AND parent IS NULL AND run = ?
            """,
            (sender, target, run[0]),
        ).fetchone()[0]
        return None, run[0], n, ASK_ROUNDS

    def _awaited(self, name: str) -> list[str]:
        """The agents NAME has an ask of its own open with, or an outcome
        on its way from: while any, its run of asks goes on.  Caller
        holds `_mu`."""
        rows = self._conn.execute(
            f"""
            SELECT to_name FROM asks WHERE from_name = ? AND (
              status IN {ASK_OPEN}
              OR (wait = 1 AND collected = 0 AND status IN ('answered', 'failed')))
            UNION ALL
            SELECT from_name FROM mail WHERE to_name = ? AND status = 'queued'
              AND ask_id IS NULL AND (body LIKE 'Answer to ask %' OR body LIKE 'Ask % failed:%')
            """,
            (name, name),
        ).fetchall()
        return list(dict.fromkeys(r[0] for r in rows))

    def _transcript(self, name: str) -> str | None:
        "NAME's conversation, as its hooks last named it.  Caller holds `_mu`."
        row = self._conn.execute(
            "SELECT transcript FROM links WHERE session = ?", (name,)
        ).fetchone()
        return row[0] if row and row[0] else None

    def _out(self, name: str) -> list[str]:
        """What NAME has still out, in words: the agents its asks are with,
        what it has running in the background.  Caller holds `_mu`."""
        return self._awaited(name) + _background(self._transcript(name))

    def _end_runs(self, old: dict[str, str], new: dict[str, str]) -> None:
        """End the run of each agent that just finished a turn -- came to
        idle or done -- with nothing open, and of each gone or dead.
        OLD and NEW are the snapshot's states before and after this tick.
        Caller holds `_mu`."""
        settled = ("idle", "done")
        for (name,) in self._conn.execute("SELECT session FROM runs").fetchall():
            state = new.get(name)
            if state in (None, "dead") or (
                state in settled and old.get(name) not in settled
                and not self._awaited(name)
            ):
                self._conn.execute("DELETE FROM runs WHERE session = ?", (name,))

    def _chain(self, name: str) -> list[str]:
        """Who NAME is answering for, nearest first: the askers of the
        asks it has open, and theirs in turn.  Caller holds `_mu`."""
        chain: list[str] = []
        frontier = [name]
        while frontier and len(chain) <= ASK_DEPTH:
            marks = ",".join("?" * len(frontier))
            rows = self._conn.execute(
                f"SELECT from_name FROM asks WHERE status = 'delivered' AND to_name IN ({marks})",
                frontier,
            ).fetchall()
            frontier = [r[0] for r in rows if r[0] != name and r[0] not in chain]
            chain.extend(dict.fromkeys(frontier))
        return chain

    def _resolve_taker(self, to: str, sender: str, exclude: list[str] = ()) -> str:
        """The session TO names, or the best one of kind TO.

        A kind picks among agents in the asker's project only: an agy in
        another checkout would commit there.  Nor a lead, which plans the
        work rather than takes it, nor one in EXCLUDE.  Caller holds `_mu`.
        """
        want = (to or "").strip()
        if not want:
            raise ValueError("to is required")
        if want == sender:
            raise ValueError("an agent cannot ask itself")
        rows = self._conn.execute(
            "SELECT name, kind, state, project, role FROM snapshot"
        ).fetchall()
        for name, _kind, state, _project, _role in rows:
            if name == want:
                if state not in _TAKER_RANK:
                    raise ValueError(f"{want} is {state or 'not running'}")
                return name
        if not any(r[1] == want for r in rows):
            raise ValueError(f"no agent or kind named {want}")
        mine = next((r[3] for r in rows if r[0] == sender), None)
        alive = [
            r for r in rows
            if r[1] == want and r[0] != sender and r[2] in _TAKER_RANK
            and r[4] != "lead" and r[0] not in exclude
        ]
        here = [r for r in alive if r[3] == mine] if mine else alive
        if not here:
            where = f" in {mine}" if mine else ""
            others = ", ".join(sorted(r[0] for r in alive)) or "none running"
            raise ValueError(
                f"no {want} agent{where} (others: {others}); name one to ask it"
            )
        if not mine and len({r[3] for r in here}) > 1:
            names = ", ".join(sorted(r[0] for r in here))
            raise ValueError(f"{want} agents in several projects ({names}); name one")
        here.sort(key=lambda r: (_TAKER_RANK[r[2]], r[0]))
        return here[0][0]

    def _new_ask_id(self) -> str:
        "Short enough to type in a reply.  Caller holds `_mu`."
        while True:
            ask_id = uuid.uuid4().hex[:8]
            if not self._conn.execute(
                "SELECT 1 FROM asks WHERE id = ?", (ask_id,)
            ).fetchone():
                return ask_id

    _ASK_COLS = (
        "id, from_name, to_name, body, wait, status, reply, auto, replied_by, "
        "error, collected, created_at, delivered_at, answered_at, auto_source"
    )

    def _ask_row(self, ask_id: str) -> dict[str, Any]:
        "Caller holds `_mu`."
        ask_id = (ask_id or "").strip()
        if not ask_id:
            raise ValueError("id is required")
        row = self._conn.execute(
            f"SELECT {self._ASK_COLS} FROM asks WHERE id = ?", (ask_id,)
        ).fetchone()
        if not row:
            raise ValueError(f"no ask {ask_id}")
        keys = [k.strip() for k in self._ASK_COLS.split(",")]
        return dict(zip(keys, row))

    def get_ask(self, ask_id: str) -> dict[str, Any]:
        "The ask, with its taker's state as Emacs last saw it."
        with self._mu:
            row = self._ask_row(ask_id)
            taker = self._conn.execute(
                "SELECT state, reason FROM snapshot WHERE name = ?",
                (row["to_name"],),
            ).fetchone()
        return {
            "id": row["id"],
            "status": row["status"],
            "from": row["from_name"],
            "to": row["to_name"],
            "body": row["body"],
            "reply": row["reply"],
            "auto": bool(row["auto"]),
            "auto_source": row["auto_source"] if row["auto"] else None,
            "replied_by": row["replied_by"],
            "error": row["error"],
            "created_at": row["created_at"],
            "delivered_at": row["delivered_at"],
            "answered_at": row["answered_at"],
            "taker_state": (taker[0] or "") if taker else "",
            "taker_reason": (taker[1] or "") if taker else "",
            # Its outcome went out as mail: nobody was waiting for it, or
            # the waiting asker was gone for longer than the herd holds it.
            "mailed": not row["wait"] and bool(row["collected"]),
        }

    def relay_asks(self, name: str) -> dict[str, list[dict[str, Any]]]:
        """What NAME's heir takes on: the asks NAME was handed and has not
        answered, and those it made that its heir will hear back on --
        open, or answered and on their way to it.  Asks are NAME's, not
        its CLI's: the heir has its name, and they go on as they were."""
        with self._mu:
            taking = self._conn.execute(
                """
                SELECT id, from_name, body FROM asks
                WHERE to_name = ? AND status = 'delivered' ORDER BY created_at
                """,
                (name,),
            ).fetchall()
            waiting = self._conn.execute(
                f"""
                SELECT id, to_name, body, status FROM asks
                WHERE from_name = ? AND (status IN {ASK_OPEN}
                  OR (status IN ('answered', 'failed') AND collected = 0))
                ORDER BY created_at
                """,
                (name,),
            ).fetchall()
        return {
            "taking": [{"id": r[0], "from": r[1], "body": r[2] or ""} for r in taking],
            "waiting": [{"id": r[0], "to": r[1], "body": r[2] or "", "status": r[3]} for r in waiting],
        }

    def reply(
        self, ask_id: str, body: str, from_name: str | None = None
    ) -> dict[str, Any]:
        """Answer an ask.  A real answer replaces what Emacs settled it
        with -- a screen, or a failure: an ask whose paste was never
        submitted can be by hand, and a taker can come back.  Only the
        asker's cancel is final."""
        if body is None or not str(body).strip():
            raise ValueError("body is required")
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            row = self._ask_row(ask_id)
            if row["status"] == "cancelled":
                why = f": {row['error']}" if row["error"] else ""
                raise ValueError(f"ask {row['id']} is cancelled{why}")
            if row["status"] == "answered" and not row["auto"]:
                raise ValueError(f"ask {row['id']} is already answered")
            self._conn.execute(
                """
                UPDATE asks SET status = 'answered', reply = ?, auto = 0,
                  replied_by = ?, collected = 0, answered_at = ?
                WHERE id = ?
                """,
                (str(body), (from_name or "").strip() or None, now, row["id"]),
            )
            # Answered before Emacs pasted it: nothing left to deliver.
            self._conn.execute(
                """
                UPDATE mail SET status = 'cancelled', error = 'answered'
                WHERE ask_id = ? AND status = 'queued'
                """,
                (row["id"],),
            )
            self._mail_back(row["id"])
            self._conn.commit()
        return {"id": row["id"], "status": "answered"}

    def settle(
        self, ask_id: str, screen: str | None = None, dead: bool = False,
        error: str | None = None,
    ) -> dict[str, Any]:
        """Emacs: the taker stopped (or died) without answering.

        Close the ask with the taker's screen as its answer, marked auto;
        as failed when it died, or with ERROR when Emacs could not hand it
        the ask at all.  A no-op once the ask is closed: the taker's own
        reply, made before it went idle, always wins.  And a no-op while
        the taker waits on asks of its own or work in the background: it
        handed part of the work on and ended its turn to wait, which is
        not stopping.
        """
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            row = self._ask_row(ask_id)
            if row["status"] != "delivered":
                return {"id": row["id"], "status": row["status"]}
            if not dead and not error:
                waiting = [r[0] for r in self._conn.execute(
                    f"SELECT id FROM asks WHERE from_name = ? AND status IN {ASK_OPEN}",
                    (row["to_name"],),
                ).fetchall()]
                waiting += _background(self._transcript(row["to_name"]))
                if waiting:
                    return {"id": row["id"], "status": "delivered", "waiting_on": waiting}
            if dead or error:
                self._close_ask(
                    row["id"], "failed", now,
                    error=error or f"{row['to_name']} exited before answering",
                )
                status = "failed"
            else:
                last = self._last_since(row["to_name"], row["delivered_at"])
                self._conn.execute(
                    """
                    UPDATE asks SET status = 'answered', reply = ?, auto = 1,
                      auto_source = ?, answered_at = ?
                    WHERE id = ?
                    """,
                    (
                        last or (screen or "").strip() or "(empty screen)",
                        "message" if last else "screen",
                        now,
                        row["id"],
                    ),
                )
                status = "answered"
            self._mail_back(row["id"])
            self._conn.commit()
        return {"id": row["id"], "status": status}

    def cancel(self, ask_id: str) -> dict[str, Any]:
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            row = self._ask_row(ask_id)
            if row["status"] in ASK_CLOSED:
                return {"id": row["id"], "status": row["status"]}
            self._close_ask(row["id"], "cancelled", now, error="cancelled by the asker")
            self._conn.execute(
                """
                UPDATE mail SET status = 'cancelled', error = 'cancelled'
                WHERE ask_id = ? AND status = 'queued'
                """,
                (row["id"],),
            )
            self._conn.commit()
        return {"id": row["id"], "status": "cancelled"}

    def detach(self, ask_id: str) -> dict[str, Any]:
        """The asker stopped waiting: send the answer as mail instead."""
        with self._mu:
            row = self._ask_row(ask_id)
            self._conn.execute("UPDATE asks SET wait = 0 WHERE id = ?", (row["id"],))
            if row["status"] in ("answered", "failed"):
                self._mail_back(row["id"])
            self._conn.commit()
        return {"id": row["id"], "status": row["status"]}

    def collect(self, ask_id: str, asker: bool = False) -> None:
        """A waiting asker has the answer: do not mail it as well.  When
        ASKER says it is the asker itself that has it -- not someone
        looking on -- an answer already mailed and not yet handed to it is
        taken back: it would be the second time it reads it."""
        ask_id = (ask_id or "").strip()
        with self._mu:
            self._conn.execute(
                f"""
                UPDATE asks SET collected = 1
                WHERE id = ? AND status IN {ASK_CLOSED}
                """,
                (ask_id,),
            )
            if asker:
                row = self._ask_row(ask_id)
                # The mail `_mail_back' writes, by the words it opens with.
                self._conn.execute(
                    """
                    UPDATE mail SET status = 'cancelled', error = 'collected'
                    WHERE to_name = ? AND status = 'queued' AND ask_id IS NULL
                      AND (body LIKE ? OR body LIKE ?)
                    """,
                    (row["from_name"], f"Answer to ask {ask_id} (%", f"Ask {ask_id} failed:%"),
                )
            self._conn.commit()

    def recent_asks(self) -> dict[str, Any]:
        with self._mu:
            return {"asks": self._recent_asks()}

    def _close_ask(self, ask_id: str, status: str, now: str, error: str) -> None:
        "Caller holds `_mu`."
        self._conn.execute(
            f"""
            UPDATE asks SET status = ?, error = ?, answered_at = ?
            WHERE id = ? AND status IN {ASK_OPEN}
            """,
            (status, error, now, ask_id),
        )

    def _mail_back(self, ask_id: str) -> None:
        """Mail a closed ask's outcome to an asker that is not waiting.
        Caller holds `_mu`."""
        row = self._ask_row(ask_id)
        if row["wait"] or row["collected"] or row["status"] not in ("answered", "failed"):
            return
        if row["status"] == "failed":
            text = f"Ask {row['id']} failed: {row['error']}"
        else:
            note = (
                " -- no answer was sent; this is "
                + ("its last message" if row["auto_source"] == "message"
                   else "its screen when it stopped")
                if row["auto"] else ""
            )
            text = (
                f"Answer to ask {row['id']} ({_head(row['body'], 80)}){note}:\n"
                f"{row['reply']}"
            )
        self._conn.execute(
            """
            INSERT INTO mail (
                id, from_name, to_name, body, submit, handoff, status, created_at
            ) VALUES (?, ?, ?, ?, 1, 0, 'queued', ?)
            """,
            (
                str(uuid.uuid4()), row["to_name"], row["from_name"], text,
                datetime.now(timezone.utc).isoformat(),
            ),
        )
        self._conn.execute("UPDATE asks SET collected = 1 WHERE id = ?", (row["id"],))

    def _sweep_uncollected(self, now: str) -> None:
        """Answers a waiting asker never collected go out as mail.
        Caller holds `_mu`."""
        cutoff = (
            datetime.fromisoformat(now).timestamp() - UNCOLLECTED_SECONDS
        )
        for ask_id, answered_at in self._conn.execute(
            """
            SELECT id, answered_at FROM asks
            WHERE wait = 1 AND collected = 0
              AND status IN ('answered', 'failed')
            """
        ).fetchall():
            if answered_at and datetime.fromisoformat(answered_at).timestamp() < cutoff:
                self._conn.execute("UPDATE asks SET wait = 0 WHERE id = ?", (ask_id,))
                self._mail_back(ask_id)

    def _recent_asks(self) -> list[dict[str, Any]]:
        """Open asks, and those closed in the last RECENT_SECONDS.
        Caller holds `_mu`."""
        cutoff = datetime.now(timezone.utc).timestamp() - RECENT_SECONDS
        out = []
        for r in self._conn.execute(
            """
            SELECT id, from_name, to_name, status, auto, error, body, reply,
                   created_at, delivered_at, answered_at, auto_source,
                   parent, run, round, round_of
            FROM asks ORDER BY created_at DESC LIMIT 200
            """
        ).fetchall():
            if r[3] in ASK_CLOSED and r[10] and (
                datetime.fromisoformat(r[10]).timestamp() < cutoff
            ):
                continue
            out.append(
                {
                    "id": r[0],
                    "from": r[1],
                    "to": r[2],
                    "status": r[3],
                    "auto": bool(r[4]),
                    "auto_source": (r[11] or "screen") if r[4] else None,
                    "error": r[5] or "",
                    "head": _head(r[6]),
                    "reply_head": _head(r[7]),
                    "created_at": r[8],
                    "delivered_at": r[9],
                    "answered_at": r[10],
                    "created_ms": _ms(r[8]),
                    "answered_ms": _ms(r[10]),
                    "parent": r[12],
                    "run": r[13],
                    "round": r[14],
                    "of": r[15],
                }
            )
            if len(out) >= 50:
                break
        return out


    # ---- What agents say about themselves ----------------------------------

    REPORT_STATES = ("working", "blocked", "idle", "done", "auto")

    def report(
        self,
        session: str,
        state: str | None = None,
        reason: str | None = None,
        cli: str | None = None,
        conversation: str | None = None,
        transcript: str | None = None,
        last: str | None = None,
    ) -> dict[str, Any]:
        """An agent's hook: what it is doing, and on which conversation.

        STATE waits for Emacs's next tick, newest per session.  The rest
        is kept here.  A turn ending in `done' with LAST closes the
        agent's unanswered ask with that answer: the taker's own words,
        where Emacs's fallback has only its screen.  Not one ending with
        work it started still out: that is `waiting', its ask left open
        for the turn that ends with nothing out.
        """
        session = (session or "").strip()
        if not session:
            raise ValueError("session is required")
        if state is not None and state not in self.REPORT_STATES:
            raise ValueError(f"state must be one of {', '.join(self.REPORT_STATES)}")
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            if cli or conversation or transcript or last:
                self._conn.execute(
                    "INSERT OR IGNORE INTO links (session, updated_at) VALUES (?, ?)",
                    (session, now),
                )
                # A new conversation's transcript and answer replace the
                # old one's; a hook that only names the state keeps them.
                same = self._conn.execute(
                    "SELECT conversation FROM links WHERE session = ?", (session,)
                ).fetchone()[0]
                if conversation and same and conversation != same:
                    self._conn.execute(
                        """
                        UPDATE links SET transcript = NULL, last_message = NULL,
                          last_at = NULL WHERE session = ?
                        """,
                        (session,),
                    )
                self._conn.execute(
                    """
                    UPDATE links SET
                      cli = COALESCE(?, cli),
                      conversation = COALESCE(?, conversation),
                      transcript = COALESCE(?, transcript),
                      last_message = COALESCE(?, last_message),
                      last_at = CASE WHEN ? IS NULL THEN last_at ELSE ? END,
                      updated_at = ?
                    WHERE session = ?
                    """,
                    (cli or None, conversation or None, transcript or None,
                     last or None, last or None, now, now, session),
                )
            if state == "done":
                out = self._out(session)
                if out:
                    state, reason = "waiting", "waiting on " + _and(out)
            if state:
                self._conn.execute(
                    """
                    INSERT OR REPLACE INTO reports (session, state, reason, at, taken)
                    VALUES (?, ?, ?, ?, 0)
                    """,
                    (session, state, reason or None, now),
                )
            closed = None
            if state == "done" and last:
                open_ask = self._conn.execute(
                    "SELECT id FROM asks WHERE to_name = ? AND status = 'delivered'",
                    (session,),
                ).fetchone()
                if open_ask:
                    self._conn.execute(
                        """
                        UPDATE asks SET status = 'answered', reply = ?, auto = 1,
                          auto_source = 'message', answered_at = ?
                        WHERE id = ?
                        """,
                        (last, now, open_ask[0]),
                    )
                    self._mail_back(open_ask[0])
                    closed = open_ask[0]
            self._conn.commit()
        return {"session": session, "state": state, "closed_ask": closed}

    def unlink(self, session: str) -> dict[str, Any]:
        """Forget what SESSION's hooks said: a new agent has its name now,
        on a conversation of its own -- one its hooks will name.  Until
        they do, the last answer of the one before is not its, nor what it
        said it was doing: waiting on sub-agents gone with it."""
        session = (session or "").strip()
        if not session:
            raise ValueError("session is required")
        with self._mu:
            self._conn.execute("DELETE FROM reports WHERE session = ?", (session,))
            n = self._conn.execute("DELETE FROM links WHERE session = ?", (session,)).rowcount
            self._conn.commit()
        return {"session": session, "forgot": bool(n)}

    def _last_since(self, session: str, since: str | None) -> str | None:
        "SESSION's last answer if it came after SINCE.  Caller holds `_mu`."
        row = self._conn.execute(
            "SELECT last_message, last_at FROM links WHERE session = ?", (session,)
        ).fetchone()
        if not row or not row[0] or not row[1]:
            return None
        if since and datetime.fromisoformat(row[1]) <= datetime.fromisoformat(since):
            return None
        return row[0]

    def link(self, session: str) -> dict[str, Any] | None:
        with self._mu:
            row = self._conn.execute(
                """
                SELECT session, cli, conversation, transcript, last_message,
                       last_at, updated_at
                FROM links WHERE session = ?
                """,
                ((session or "").strip(),),
            ).fetchone()
        return self._link_dict(row, full=True) if row else None

    @staticmethod
    def _link_dict(r, full: bool = False) -> dict[str, Any]:
        out = {
            "session": r[0],
            "cli": r[1] or "",
            "conversation": r[2] or "",
            "transcript": r[3] or "",
            "last_head": _head(r[4]),
            "last_ms": _ms(r[5]),
        }
        if full:
            out["last"] = r[4] or ""
        return out

    def tick(
        self,
        sessions: list[dict[str, Any]] | None,
        ack_ids: list[str] | None = None,
        replies: list[dict[str, Any]] | None = None,
        ack_cmd_ids: list[str] | None = None,
        pushes: list[dict[str, Any]] | None = None,
        page_url: str | None = None,
    ) -> dict[str, Any]:
        """PAGE_URL is the herd page's address from your phone, without its
        secret, as Emacs says it each tick; none, it is not linked."""
        sessions = sessions or []
        pushes = [p for p in (pushes or []) if isinstance(p, dict) and p.get("kind") and p.get("session")]
        ack_ids = [i for i in (ack_ids or []) if i]
        ack_cmd_ids = [i for i in (ack_cmd_ids or []) if i]
        replies = replies or []
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            if ack_ids:
                placeholders = ",".join("?" * len(ack_ids))
                self._conn.execute(
                    f"""
                    UPDATE mail SET status = 'delivered', delivered_at = ?
                    WHERE id IN ({placeholders}) AND status = 'queued'
                    """,
                    [now, *ack_ids],
                )
                self._conn.execute(
                    f"""
                    UPDATE asks SET status = 'delivered', delivered_at = ?
                    WHERE status = 'queued' AND id IN (
                      SELECT ask_id FROM mail
                      WHERE id IN ({placeholders}) AND ask_id IS NOT NULL)
                    """,
                    [now, *ack_ids],
                )
            if ack_cmd_ids:
                placeholders = ",".join("?" * len(ack_cmd_ids))
                self._conn.execute(
                    f"""
                    UPDATE commands SET status = 'taken'
                    WHERE id IN ({placeholders}) AND status = 'queued'
                    """,
                    ack_cmd_ids,
                )
            for reply in replies:
                rid = str(reply.get("id") or "").strip()
                if not rid:
                    continue
                chat = self._conn.execute(
                    "SELECT chat_id FROM commands WHERE id = ?",
                    (rid,),
                ).fetchone()
                self._conn.execute(
                    """
                    INSERT OR REPLACE INTO replies (id, chat_id, text, sent)
                    VALUES (?, ?, ?, 0)
                    """,
                    (
                        rid,
                        chat[0] if chat else reply.get("chat_id"),
                        str(reply.get("text") or ""),
                    ),
                )
                self._conn.execute(
                    "UPDATE commands SET status = 'done' WHERE id = ?",
                    (rid,),
                )
            for push in pushes:
                self._conn.execute(
                    "INSERT INTO pushes (push, created_at) VALUES (?, ?)",
                    (json.dumps(push), now),
                )
            was = dict(self._conn.execute("SELECT name, state FROM snapshot").fetchall())
            self._conn.execute("DELETE FROM snapshot")
            for row in sessions:
                name = str(row.get("name") or "").strip()
                if not name:
                    continue
                raw_since = row.get("since")
                since = (
                    float(raw_since)
                    if isinstance(raw_since, (int, float)) and not isinstance(raw_since, bool)
                    else 0.0
                )
                self._conn.execute(
                    """
                    INSERT INTO snapshot
                      (name, kind, state, notes, project, reason, screen, role, prompt, room, since)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    """,
                    (
                        name,
                        str(row.get("kind") or ""),
                        str(row.get("state") or ""),
                        str(row.get("notes") or ""),
                        str(row.get("project") or ""),
                        str(row.get("reason") or ""),
                        str(row.get("screen") or ""),
                        str(row.get("role") or ""),
                        json.dumps(row["prompt"]) if isinstance(row.get("prompt"), dict) else None,
                        str(row.get("room") or ""),
                        since,
                    ),
                )
                self._upsert_alias(name)
            self._conn.execute(
                "INSERT OR REPLACE INTO meta (k, v) VALUES ('synced', '1')"
            )
            self._conn.execute(
                "INSERT OR REPLACE INTO meta (k, v) VALUES ('page_url', ?)",
                (page_url if isinstance(page_url, str) else "",),
            )
            names = {
                str(row.get("name") or "").strip()
                for row in sessions
                if str(row.get("name") or "").strip()
            }
            queued = self._conn.execute(
                "SELECT id, to_name, ask_id FROM mail WHERE status = 'queued'"
            ).fetchall()
            for mail_id, to_name, ask_id in queued:
                if to_name not in names:
                    self._conn.execute(
                        """
                        UPDATE mail
                        SET status = 'failed', error = 'unknown session'
                        WHERE id = ?
                        """,
                        (mail_id,),
                    )
                    if ask_id:
                        self._close_ask(
                            ask_id, "failed", now, error=f"{to_name} is gone"
                        )
            # Before collecting what to deliver, so a swept answer goes
            # out in this tick rather than the next.
            self._sweep_uncollected(now)
            self._end_runs(was, {
                str(row.get("name") or "").strip(): str(row.get("state") or "")
                for row in sessions
            })
            self._prune(now)
            hold = {"working", "starting"}
            # One ask at a time per agent: a second one pasted on top of
            # the first would leave it unsure which it is answering.
            answering = {
                r[0]
                for r in self._conn.execute(
                    "SELECT to_name FROM asks WHERE status = 'delivered'"
                )
            }
            deliverable = []
            for row in self._conn.execute(
                """
                SELECT m.id, m.from_name, m.to_name, m.body,
                       m.submit, m.handoff, m.ask_id, s.state, a.rounds
                FROM mail m
                JOIN snapshot s ON s.name = m.to_name
                LEFT JOIN asks a ON a.id = m.ask_id
                WHERE m.status = 'queued'
                ORDER BY m.created_at
                """
            ).fetchall():
                if (row[7] or "") in hold:
                    continue
                if row[6]:
                    if row[2] in answering:
                        continue
                    answering.add(row[2])
                deliverable.append(row[:7] + row[8:])
            asks = self._recent_asks()
            self._unwait(names, now)
            reports = [
                {"session": r[0], "state": r[1], "reason": r[2] or ""}
                for r in self._conn.execute(
                    "SELECT session, state, reason FROM reports WHERE taken = 0 ORDER BY at"
                ).fetchall()
            ]
            self._conn.execute("UPDATE reports SET taken = 1 WHERE taken = 0")
            links = [
                self._link_dict(r)
                for r in self._conn.execute(
                    """
                    SELECT session, cli, conversation, transcript, last_message,
                           last_at, updated_at
                    FROM links ORDER BY session
                    """
                ).fetchall()
                if r[0] in names
            ]
            cmd_rows = self._conn.execute(
                """
                SELECT id, op, session, args, chat_id
                FROM commands WHERE status = 'queued'
                ORDER BY created_at
                """
            ).fetchall()
            if cmd_rows:
                placeholders = ",".join("?" * len(cmd_rows))
                self._conn.execute(
                    f"""
                    UPDATE commands SET status = 'taken'
                    WHERE id IN ({placeholders})
                    """,
                    [r[0] for r in cmd_rows],
                )
            commands = []
            for cid, op, session, args, chat_id in cmd_rows:
                parsed = {}
                if args:
                    try:
                        parsed = json.loads(args)
                    except json.JSONDecodeError:
                        parsed = {}
                commands.append(
                    {
                        "id": cid,
                        "op": op,
                        "session": session,
                        "args": parsed,
                        "chat_id": chat_id,
                    }
                )
            self._conn.commit()
        pending = []
        for mail_id, from_name, to_name, body, submit, handoff, ask_id, rounds in deliverable:
            text = body or ""
            truncated = len(text) > self.inline_limit
            if truncated:
                text = (
                    text[: self.inline_limit]
                    + "\n\n[truncated — herd_inbox session="
                    + to_name
                    + " id="
                    + mail_id
                    + "]"
                )
            pending.append(
                {
                    "id": mail_id,
                    "from": from_name,
                    "to": to_name,
                    "body": text,
                    "truncated": truncated,
                    "submit": bool(submit),
                    "handoff": bool(handoff),
                    "ask": ask_id,
                }
            )
            if ask_id:
                # How many times its taker may ask any one agent for it.
                pending[-1]["rounds"] = rounds or ASK_ROUNDS
        return {
            "pending": pending,
            "commands": commands,
            "asks": asks,
            "reports": reports,
            "links": links,
        }

    def _unwait(self, names: set[str], now: str) -> None:
        """Each of NAMES waiting with nothing out, and quiet for WAIT_GRACE
        since: `done', as if its turn had ended now.  Emacs then closes its
        ask as for any taker that stopped.  Caller holds `_mu`."""
        t = datetime.fromisoformat(now).timestamp()
        for session, at in self._conn.execute(
            "SELECT session, at FROM reports WHERE state = 'waiting'"
        ).fetchall():
            if session not in names or self._out(session):
                continue
            quiet = max(datetime.fromisoformat(at).timestamp(),
                        _mtime(self._transcript(session)))
            if t - quiet < WAIT_GRACE:
                continue
            self._conn.execute(
                """
                INSERT OR REPLACE INTO reports (session, state, reason, at, taken)
                VALUES (?, 'done', 'nothing left to wait on', ?, 0)
                """,
                (session, now),
            )

    def _prune(self, now: str) -> None:
        """Delete terminal records older than HISTORY_SECONDS.  Never touch
        queued/open mail, open or uncollected waiting asks, asks belonging
        to an active run or an open parent ask, queued commands, or unsent
        replies and pushes.  Caller holds `_mu`."""
        cutoff = datetime.fromtimestamp(
            datetime.fromisoformat(now).timestamp() - HISTORY_SECONDS,
            tz=timezone.utc,
        ).isoformat()
        self._conn.execute(
            """
            DELETE FROM mail
            WHERE status IN ('delivered', 'failed', 'cancelled')
              AND COALESCE(delivered_at, created_at) < ?
            """,
            (cutoff,),
        )
        self._conn.execute(
            f"""
            DELETE FROM asks
            WHERE status IN {ASK_CLOSED}
              AND (wait = 0 OR collected = 1)
              AND COALESCE(answered_at, created_at) < ?
              AND (run IS NULL OR run NOT IN (SELECT run FROM runs))
              AND (parent IS NULL OR parent NOT IN (
                SELECT id FROM asks WHERE status IN {ASK_OPEN}))
            """,
            (cutoff,),
        )
        self._conn.execute(
            """
            DELETE FROM commands
            WHERE status IN ('done', 'taken') AND created_at < ?
            """,
            (cutoff,),
        )
        self._conn.execute(
            """
            DELETE FROM replies
            WHERE sent = 1 AND id NOT IN (SELECT id FROM commands)
            """
        )
        self._conn.execute(
            """
            DELETE FROM pushes
            WHERE sent = 1 AND created_at < ?
            """,
            (cutoff,),
        )

    def _upsert_alias(self, name: str) -> str:
        "Caller holds `_mu`."
        row = self._conn.execute(
            "SELECT short FROM aliases WHERE name = ?", (name,)
        ).fetchone()
        if row:
            return row[0]
        short = hashlib.sha256(name.encode()).hexdigest()[:8]
        # Rare collision: append until unique.
        base = short
        n = 0
        while True:
            taken = self._conn.execute(
                "SELECT name FROM aliases WHERE short = ?", (short,)
            ).fetchone()
            if taken is None:
                self._conn.execute(
                    "INSERT INTO aliases (short, name) VALUES (?, ?)",
                    (short, name),
                )
                return short
            if taken[0] == name:
                return short
            n += 1
            short = (base[:6] + f"{n:02x}")[:8]

    def short_for(self, name: str) -> str:
        name = (name or "").strip()
        if not name:
            return ""
        with self._mu:
            return self._upsert_alias(name)

    def name_for(self, short: str) -> str | None:
        short = (short or "").strip()
        if not len(short) == 8:
            # still look up
            pass
        with self._mu:
            row = self._conn.execute(
                "SELECT name FROM aliases WHERE short = ?", (short,)
            ).fetchone()
        return row[0] if row else None

    def session(self, name: str) -> dict[str, Any] | None:
        name = (name or "").strip()
        with self._mu:
            row = self._conn.execute(
                """
                SELECT name, kind, state, notes, project, reason, screen, prompt, room
                FROM snapshot WHERE name = ?
                """,
                (name,),
            ).fetchone()
        if not row:
            return None
        return {
            "name": row[0],
            "kind": row[1] or "",
            "state": row[2] or "",
            "notes": row[3] or "",
            "project": row[4] or "",
            "reason": row[5] or "",
            "screen": row[6] or "",
            "prompt": json.loads(row[7]) if row[7] else None,
            "room": row[8] or "",
            "short": self.short_for(row[0]),
        }

    def page_url(self) -> str:
        "The herd page's address from your phone, as Emacs last said it; or ''."
        with self._mu:
            row = self._conn.execute(
                "SELECT v FROM meta WHERE k = 'page_url'"
            ).fetchone()
        return (row[0] if row else "") or ""

    def enqueue_command(
        self,
        op: str,
        session: str,
        args: dict[str, Any] | None = None,
        chat_id: int | None = None,
    ) -> dict[str, Any]:
        op = (op or "").strip()
        session = (session or "").strip()
        if not op:
            raise ValueError("op is required")
        if not session:
            raise ValueError("session is required")
        cid = str(uuid.uuid4())
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            self._conn.execute(
                """
                INSERT INTO commands
                  (id, op, session, args, chat_id, status, created_at)
                VALUES (?, ?, ?, ?, ?, 'queued', ?)
                """,
                (
                    cid,
                    op,
                    session,
                    json.dumps(args or {}),
                    chat_id,
                    now,
                ),
            )
            self._conn.commit()
        return {"id": cid, "queued": True}

    def unsent_replies(self) -> list[dict[str, Any]]:
        with self._mu:
            rows = self._conn.execute(
                """
                SELECT id, chat_id, text FROM replies WHERE sent = 0
                ORDER BY id
                """
            ).fetchall()
        return [
            {"id": r[0], "chat_id": r[1], "text": r[2]} for r in rows
        ]

    def unsent_pushes(self) -> list[dict[str, Any]]:
        """Pushes not sent yet, oldest first: each as Emacs gave it, with
        its id and when it came."""
        with self._mu:
            rows = self._conn.execute(
                "SELECT id, push, created_at FROM pushes WHERE sent = 0 ORDER BY id"
            ).fetchall()
        return [dict(json.loads(r[1]), id=r[0], created_at=r[2]) for r in rows]

    def mark_pushes_sent(self, ids: list[int]) -> None:
        ids = [i for i in ids if i]
        if not ids:
            return
        with self._mu:
            placeholders = ",".join("?" * len(ids))
            self._conn.execute(
                f"UPDATE pushes SET sent = 1 WHERE id IN ({placeholders})", ids
            )
            self._conn.commit()

    PUSHED_KEPT = 500

    def remember_pushed(self, chat_id: int, message_id: int, project: str) -> None:
        "Message MESSAGE_ID, pushed to CHAT_ID, was about PROJECT."
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            self._conn.execute(
                "INSERT OR REPLACE INTO pushed (chat_id, message_id, project, at) VALUES (?, ?, ?, ?)",
                (chat_id, message_id, project, now),
            )
            self._conn.execute(
                """
                DELETE FROM pushed WHERE rowid NOT IN (
                  SELECT rowid FROM pushed ORDER BY at DESC LIMIT ?)
                """,
                (self.PUSHED_KEPT,),
            )
            self._conn.commit()

    def pushed_project(self, chat_id: int, message_id: int) -> str | None:
        "The project the push MESSAGE_ID to CHAT_ID was about, or None."
        with self._mu:
            row = self._conn.execute(
                "SELECT project FROM pushed WHERE chat_id = ? AND message_id = ?",
                (chat_id, message_id),
            ).fetchone()
        return row[0] if row else None

    def mark_replies_sent(self, ids: list[str]) -> None:
        ids = [i for i in ids if i]
        if not ids:
            return
        with self._mu:
            placeholders = ",".join("?" * len(ids))
            self._conn.execute(
                f"UPDATE replies SET sent = 1 WHERE id IN ({placeholders})",
                ids,
            )
            self._conn.commit()


def get_herd() -> HerdStore:
    global _herd
    cfg = get_config()
    with _lock:
        if _herd is None or _herd.path != cfg.herd_path:
            if _herd is not None:
                try:
                    _herd.close()
                except Exception:
                    pass
            _herd = HerdStore(cfg.herd_path, cfg.mail_inline_limit)
        return _herd


def reset_herd() -> None:
    global _herd
    with _lock:
        if _herd is not None:
            try:
                _herd.close()
            except Exception:
                pass
            _herd = None
