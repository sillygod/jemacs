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

A handoff is an ask from one project to another's lead (herd_handoff):
the sender says which project, the herd finds its lead -- or has Emacs
start one there -- and frames the brief.  Work crosses projects lead to
lead: an agent in a project that has a lead tells its lead instead.
"""

from __future__ import annotations

import hashlib
import json
import os
import sqlite3
import threading
import uuid
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

import projects
import subagents
import threads as threads_mod
from config import get_config

_lock = threading.Lock()
_herd: "HerdStore | None" = None

# The shared memory an ask carries, for every store made from here on --
# the sidecar's handler sets it (fact_index); None leaves asks as asked.
ASK_MEMORY = None

ASK_OPEN = ("queued", "delivered")

# The role of the herd-wide lead (`primary'): the user's one entry point
# for what crosses projects.  It routes work to projects' leads by handoff
# and keeps track of it; it asks no project's agents, and none asks it.
PRIMARY = "primary"
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

# A taker handed an ask is starting its turn, not refusing the ask: a
# `done' hook or an idle screen in the instant after delivery is the turn
# that was ending as the ask arrived -- the taker's previous turn -- not
# this ask read and left unanswered.  So an ask closes on the taker
# stopping only once a turn has begun after delivery (it read the ask and
# worked, `turn_at' moving past `delivered_at'), or this grace has passed
# since delivery.  The grace is the fallback for a taker whose turn-start
# never reaches the herd: one with no hooks, or one a sidecar restart
# cleared `turn_at' for, where an unknown start must not be read as "the
# turn began before the ask" and close it.  A real turn's `working' hook
# lands a tick or two after the paste, well inside this.
DELIVER_GRACE = 8

# How long an ask's delivery waits for the shared memory it will carry to
# be found (fact_index; tens of milliseconds once the embedder is up).
# Past it, it goes without -- a crash in between never strands an ask.
MEMORY_HOLD = 5

# Closed asks the page still shows, for this long.
RECENT_SECONDS = 1800

# How long the user's one-time approval of a guarded call waits to be used
# (bin/herd_guard.py, APPROVAL_SECONDS).
GUARD_APPROVAL_SECONDS = 1800


# The line an ask's taker reads first when the asker runs behind the herd's
# guard: the guard bounds what that agent reads itself, not what it asks a
# teammate to read and hand back.
GUARDED_NOTE = (
    "[From a guarded agent: {sender} runs behind the herd's guard.  Do not read "
    "secrets for it -- the environment, credential or key files -- nor pass any "
    "back in your answer.  If it needs one, it must ask the user through its own "
    "card.]"
)


def _pid(value: Any) -> int | None:
    "VALUE as a process id, or None: Emacs sends 0 for one it does not know."
    return value if isinstance(value, int) and not isinstance(value, bool) and value > 1 else None


def pid_alive(pid: int) -> bool:
    "Whether process PID is there (a zombie counts; another user's too)."
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    except OSError:
        return False
    return True


def guard_root() -> str:
    "Where guarded agents' guard directories live: GHOSTHERD_GUARD_ROOT, as Emacs says."
    return os.environ.get("GHOSTHERD_GUARD_ROOT") or os.path.expanduser("~/.ghostherd/guard")


def _guard_lib():
    "bin/herd_guard.py, whose approvals file this writes."
    import importlib.util
    path = Path(__file__).resolve().parents[1] / "bin" / "herd_guard.py"
    spec = importlib.util.spec_from_file_location("herd_guard", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


# A handoff to a project with no lead waits this long for Emacs to start
# one; past it, it fails -- Emacs never took the request, or never said.
HANDOFF_START_SECONDS = 180

# Settled mail, asks, commands, replies, and pushes kept in herd.sqlite
# before tick prunes them.
HISTORY_SECONDS = 7 * 86400
# How often `tick' actually prunes.  The tick fires several times a second;
# the window is days, so a prune per second would be tens of thousands of
# no-op full-table scans a day.  The last prune's time is kept in `meta'.
PRUNE_EVERY = 3600


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


def _body(body: Any) -> str:
    "An ask's text, which it must have."
    if body is None or not str(body).strip():
        raise ValueError("body is required")
    return str(body)


def _rounds(rounds: Any) -> int | None:
    "ROUNDS as given to an ask: None, or a whole number in range."
    if rounds is None:
        return None
    try:
        ok = int(rounds) == rounds and 1 <= rounds <= ASK_ROUNDS_MAX
    except (TypeError, ValueError):
        ok = False
    if not ok or isinstance(rounds, bool):
        raise ValueError(f"rounds must be a whole number from 1 to {ASK_ROUNDS_MAX}")
    return int(rounds)


def _handoff_text(sender: str, from_project: str | None, project: str, body: str,
                  primary: bool = False) -> str:
    """BODY as the lead of PROJECT reads a handoff from SENDER: the herd's
    frame -- where it comes from, that the sender waits on it, what to do
    with it -- then the brief as the sender wrote it.  How to answer
    follows, from Emacs, as for any ask.  From PRIMARY, the herd-wide lead,
    it is the user's request, routed: no project of its own to name."""
    user = sender == "user"
    who = "the user" if user else sender
    Who = who[:1].upper() + who[1:]
    there = projects.leaf(from_project) if from_project and not primary else ""
    origin = (f"{who}, the herd-wide lead, for the user" if primary
              else f"{who}, in project {there} ({from_project})" if there else who)
    back = f" in {there}" if there else ""
    tail = "" if user else (
        f"  {Who} does not direct your agents, and cannot be asked while it waits: "
        "put any question in your answer.")
    return (
        f"[Handoff to {projects.leaf(project)} from {origin}.  {Who} found work that "
        "belongs in this project and waits on your answer.  Take it in as your own: plan "
        "it here, by this project's rules and with its agents, and answer with the result "
        f"-- what was done, where (branch, commit, pull request), and what is left for "
        f"{who}{back}.{tail}]\n\n{body}"
    )


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
        # What an ask carries from the shared memory: (taker's session, body)
        # -> the text to deliver after the body, or None.  Set by the
        # sidecar (fact_index); None leaves asks as they are.
        self.ask_memory = ASK_MEMORY
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
        # What a guarded agent's guard refused, and what the user approved of
        # it, once (bin/herd_guard.py): for the card, the log and the
        # one-time allowance.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS guard_requests (
                id TEXT PRIMARY KEY,
                session TEXT NOT NULL,
                guard_dir TEXT,
                tool TEXT,
                summary TEXT,
                reason TEXT,
                rule TEXT,
                kind TEXT,
                approvable INTEGER NOT NULL DEFAULT 0,
                fp TEXT,
                call TEXT,
                status TEXT NOT NULL,
                created_at TEXT NOT NULL,
                approved_at TEXT,
                used_at TEXT
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
        # A proposal to promote facts into a project's instruction file,
        # pending the user's approval on the page.  On approve the herd hands
        # the diff to the project's lead as an ask; when that ask is answered
        # the facts are marked promoted.  The herd never edits the repo itself.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS proposals (
                id TEXT PRIMARY KEY,
                project TEXT NOT NULL,
                facts TEXT NOT NULL,
                file TEXT NOT NULL,
                diff TEXT NOT NULL,
                author TEXT NOT NULL,
                status TEXT NOT NULL,
                reason TEXT,
                ask_id TEXT,
                created_at TEXT NOT NULL,
                decided_at TEXT
            )
            """
        )
        # The user's own state on each Inbox item, keyed by the item's stable
        # id (guard:<id>, proposal:<id>, blocked:<agent>, question:<agent>:<ts>,
        # ...): `read' once looked at, `done' once dismissed, `override' to
        # move it between Needs-you and Updates.  Desk and phone share it, and
        # it survives a reload or restart.  A row whose item is gone (the cause
        # resolved itself) is pruned, so a later item of the same id is fresh.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS inbox_state (
                item_id TEXT PRIMARY KEY,
                read INTEGER NOT NULL DEFAULT 0,
                done INTEGER NOT NULL DEFAULT 0,
                override TEXT,
                updated_at TEXT NOT NULL
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
        # Where each agent works, and the branch it is on: a worktree agent
        # is in its own directory on its own branch (the others share the
        # project's), so the lead can find and read its work.
        if "directory" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN directory TEXT")
        if "branch" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN branch TEXT")
        # Who started the agent through `herd start', and where its worktree
        # branch stands: so `herd list' shows a lead what it owns and whether
        # an agent's work is done (merged) -- the queue-or-start decision.
        if "started_by" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN started_by TEXT")
        if "ahead" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN ahead INTEGER")
        if "merged" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN merged INTEGER")
        # The process the agent lives under, as Emacs knows it: its tmux
        # pane's, or its ghostel terminal's shell.  A hook's report is the
        # agent's only from inside it (`_not_its_own').
        if "host_pid" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN host_pid INTEGER")
        # Whether the agent runs behind the herd's guard (a guarded agy):
        # what it asks of another is marked so (`GUARDED_NOTE').
        if "guarded" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN guarded INTEGER")
        mail_cols = {
            r[1] for r in self._conn.execute("PRAGMA table_info(mail)")
        }
        if "ask_id" not in mail_cols:
            self._conn.execute("ALTER TABLE mail ADD COLUMN ask_id TEXT")
        # The shared memory an ask carries, delivered after its body (the
        # body itself stays as asked), and until when delivery waits for it
        # to be found -- a moment; past it the ask goes without.
        for col in ("memory", "memory_until"):
            if col not in mail_cols:
                self._conn.execute(f"ALTER TABLE mail ADD COLUMN {col} TEXT")
        ask_cols = {
            r[1] for r in self._conn.execute("PRAGMA table_info(asks)")
        }
        if "auto_source" not in ask_cols:
            self._conn.execute("ALTER TABLE asks ADD COLUMN auto_source TEXT")
        # When the agent's current turn began (its last `working'), and the
        # turn that produced `last_message'.  An ask is answered by a turn's
        # own words only when that turn began after the ask -- not by the
        # recap of a turn already running when the ask arrived.
        link_cols = {
            r[1] for r in self._conn.execute("PRAGMA table_info(links)")
        }
        # `turn_end_at': when a turn last ended (a `done'/`idle' hook).  A
        # queued ask is not delivered while a turn is still in flight by the
        # hooks -- a `working' with no end since -- so it does not land in the
        # moment a turn is ending (the screen reads idle a beat before the
        # turn-end hook lands), to be answered by that ending turn's recap.
        for col in ("turn_at", "last_turn_at", "turn_end_at"):
            if col not in link_cols:
                self._conn.execute(f"ALTER TABLE links ADD COLUMN {col} TEXT")
        # `pid': the process of the agent's own CLI, as its hooks name it --
        # so a CLI the agent itself runs, under its name, is told from it
        # (`_not_its_own').
        if "pid" not in link_cols:
            self._conn.execute("ALTER TABLE links ADD COLUMN pid INTEGER")
        # The ask its asker was answering, the run it belongs to, its round
        # with its taker and how many it may have; and how many its own
        # taker may have with any one agent (NULL: ASK_ROUNDS).
        for col, typ in (("parent", "TEXT"), ("run", "TEXT"), ("round", "INTEGER"),
                         ("round_of", "INTEGER"), ("rounds", "INTEGER")):
            if col not in ask_cols:
                self._conn.execute(f"ALTER TABLE asks ADD COLUMN {col} {typ}")
        # A handoff (`herd handoff'): the project it went to, whose lead
        # takes it, and the project of the agent that sent it.  NULL for an
        # ordinary ask.  Its taker is '' while the herd starts that lead.
        for col in ("project", "from_project"):
            if col not in ask_cols:
                self._conn.execute(f"ALTER TABLE asks ADD COLUMN {col} TEXT")
        # Sent by an agent behind the herd's guard: its taker is told not
        # to fetch secrets for it.
        if "guarded" not in ask_cols:
            self._conn.execute("ALTER TABLE asks ADD COLUMN guarded INTEGER")
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
        cols = ("name, kind, state, notes, project, reason, role, since, "
                "directory, branch, started_by, ahead, merged")
        with self._mu:
            if project:
                rows = self._conn.execute(
                    f"SELECT {cols} FROM snapshot WHERE project = ?", (project,)
                ).fetchall()
            else:
                rows = self._conn.execute(f"SELECT {cols} FROM snapshot").fetchall()
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
                    "directory": r[8] or "",
                    "branch": r[9] or "",
                    "started_by": (r[10] if len(r) > 10 else "") or "",
                    "ahead": (r[11] if len(r) > 11 else 0) or 0,
                    "merged": bool(r[12]) if len(r) > 12 and r[12] is not None else False,
                    # What a lead needs to decide queue-or-start: the ask this
                    # agent owes now and how many wait behind it, and the files
                    # its turn is editing (so work on the same files queues).
                    **self._agent_work(r[0], r[2] or "", r[8] or ""),
                    "short": self.short_for(r[0]),
                }
                for r in rows
            ]
        }

    def primary(self) -> dict[str, Any] | None:
        "The herd-wide lead as the snapshot has it -- name, state, project -- or None."
        with self._mu:
            row = self._conn.execute(
                "SELECT name, state, project FROM snapshot WHERE role = ? LIMIT 1", (PRIMARY,)
            ).fetchone()
        return {"name": row[0], "state": row[1] or "", "project": row[2] or ""} if row else None

    def projects(self) -> list[str]:
        "The projects the herd's agents are in, as Emacs writes them."
        with self._mu:
            return [r[0] for r in self._conn.execute(
                "SELECT DISTINCT project FROM snapshot WHERE project != '' ORDER BY project")]

    def _agent_work(self, name: str, state: str, root: str) -> dict[str, Any]:
        """For `herd list': NAME's open ask (id, from), how many asks wait
        queued behind it, and the files its current turn is editing."""
        with self._mu:
            delivered = self._conn.execute(
                "SELECT id, from_name FROM asks "
                "WHERE to_name = ? AND status = 'delivered' ORDER BY created_at LIMIT 1",
                (name,),
            ).fetchone()
            queued = self._conn.execute(
                "SELECT COUNT(*) FROM asks WHERE to_name = ? AND status = 'queued'",
                (name,),
            ).fetchone()[0]
            transcript = self._transcript(name)
        ask = {"id": delivered[0], "from": delivered[1]} if delivered else None
        editing: list[str] = []
        # Only an agent mid-turn is editing; reading every idle agent's
        # transcript on each `herd list' is not worth it.
        if transcript and state in ("working", "blocked", "waiting"):
            try:
                import conversation
                editing = conversation.edited(transcript, root or None).get("files", [])
            except (OSError, ValueError, ImportError):
                editing = []
        return {"ask": ask, "queued": int(queued or 0), "editing": editing}

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
        body = _body(body)
        rounds = _rounds(rounds)
        sender = (from_name or "").strip() or "user"
        with self._mu:
            if not self._synced():
                raise ValueError(
                    "Emacs has not shared the herd yet; is ghostherd-mode on?"
                )
            chain = self._chain_checked(sender)
            target = self._resolve_taker(to, sender, exclude=chain)
            self._not_in_chain(target, sender, chain)
            out, mail_id = self._insert_ask(sender, target, body, wait, rounds)
            self._conn.commit()
        if mail_id:
            self._attach_memory(mail_id, target, body)
        return out

    def _chain_checked(self, sender: str) -> list[str]:
        """Who SENDER is answering for (`_chain'), refusing when it is
        already as deep as asks go.  Caller holds `_mu`."""
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
        return chain

    @staticmethod
    def _not_in_chain(target: str, sender: str, chain: list[str]) -> None:
        if target in chain:
            raise ValueError(
                f"{target} is waiting on {sender} ({sender} <- {' <- '.join(chain)}); "
                "answer its ask rather than ask it"
            )

    def _insert_ask(
        self, sender: str, target: str, body: str, wait: bool, rounds: int | None,
        project: str | None = None, from_project: str | None = None,
    ) -> tuple[dict[str, Any], str | None]:
        """Record an ask of TARGET by SENDER, held to its rounds, and the
        mail that delivers it.  A handoff names the PROJECT it went to (and
        the one it came FROM_PROJECT); its TARGET is '' while the herd
        starts that project's lead, and its mail waits for it.  Returns what
        to tell the asker, and the mail to find memory for (None when there
        is none to find).  Caller holds `_mu` and commits."""
        now = datetime.now(timezone.utc).isoformat()
        ask_id = self._new_ask_id()
        parent, run, n, cap = self._round(sender, target or None, ask_id, project)
        if cap is not None and n >= cap:
            whom = f"handed {projects.leaf(project)} work" if project else f"asked {target}"
            if parent:
                raise ValueError(
                    f"{sender} has {whom} {n} times for ask {parent['id']}, "
                    f"{cap} at most; answer {parent['from_name']} with where it stands "
                    "rather than ask again"
                )
            raise ValueError(
                f"{sender} has {whom} {n} times in this run of asks, {cap} at most "
                f"(it ends when {sender} finishes a turn with no ask open); "
                "say where it stands rather than ask again"
            )
        if run == ask_id:
            self._conn.execute(
                "INSERT OR REPLACE INTO runs (session, run) VALUES (?, ?)", (sender, run)
            )
        guarded = self._conn.execute(
            "SELECT guarded FROM snapshot WHERE name = ?", (sender,)).fetchone()
        self._conn.execute(
            """
            INSERT INTO asks (id, from_name, to_name, body, wait,
                              status, created_at, parent, run, round,
                              round_of, rounds, project, from_project, guarded)
            VALUES (?, ?, ?, ?, ?, 'queued', ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (ask_id, sender, target or "", body, 1 if wait else 0, now,
             parent["id"] if parent else None, run,
             n + 1 if cap is not None else None, cap, rounds,
             project, from_project if project else None,
             1 if guarded and guarded[0] else 0),
        )
        mail_id = self._ask_mail(ask_id, target, now) if target else None
        out: dict[str, Any] = {"id": ask_id, "to": target or "", "queued": True}
        if cap is not None:
            out.update(round=n + 1, of=cap)
        return out, mail_id

    def _ask_mail(self, ask_id: str, target: str, now: str) -> str | None:
        """Queue the mail that delivers ask ASK_ID to TARGET: its body, framed
        as a handoff for one.  Held for the shared memory it will carry when
        there is a memory to ask; returns that mail's id, or None.  Caller
        holds `_mu`."""
        row = self._conn.execute(
            "SELECT from_name, body, project, from_project, guarded FROM asks WHERE id = ?",
            (ask_id,),
        ).fetchone()
        sender, body, project, from_project, guarded = row
        role = self._conn.execute(
            "SELECT role FROM snapshot WHERE name = ?", (sender,)).fetchone()
        text = (_handoff_text(sender, from_project, project, body,
                              primary=bool(role and role[0] == PRIMARY))
                if project else body)
        if guarded:
            text = GUARDED_NOTE.format(sender=sender) + "\n\n" + text
        mail_id = str(uuid.uuid4())
        hold = None
        if self.ask_memory is not None:
            hold = (datetime.now(timezone.utc) + timedelta(seconds=MEMORY_HOLD)).isoformat()
        self._conn.execute(
            """
            INSERT INTO mail (
                id, from_name, to_name, body, submit, handoff,
                status, created_at, ask_id, memory_until
            ) VALUES (?, ?, ?, ?, 1, 0, 'queued', ?, ?, ?)
            """,
            (mail_id, sender, target, text, now, ask_id, hold),
        )
        return mail_id if hold is not None else None

    # ---- Handoffs: work from one project for another's lead -----------------

    def handoff(
        self,
        project: str,
        body: str,
        from_name: str | None = None,
        wait: bool = False,
        rounds: int | None = None,
    ) -> dict[str, Any]:
        """Hand BODY to PROJECT's lead as an ask: the answer, rounds and loop
        rules are an ask's.  PROJECT is the project's directory, resolved.

        A project with no lead gets one: the herd asks Emacs to start it
        (a `handoff' command, one per project however many handoffs wait on
        it), and the ask waits with no taker until Emacs names the lead it
        started -- or fails with why it did not.  `starting' says so.
        """
        body = _body(body)
        rounds = _rounds(rounds)
        sender = (from_name or "").strip() or "user"
        where = projects.norm(project)
        if not where or not os.path.isabs(where):
            raise ValueError("project is the project's directory, absolute")
        shown = projects.display(where)
        # Looked at before the lock: they run git.  Used only when the
        # project has no lead, which only the snapshot says.
        exists = os.path.isdir(where)
        git = exists and projects.is_git_checkout(where)
        with self._mu:
            if not self._synced():
                raise ValueError(
                    "Emacs has not shared the herd yet; is ghostherd-mode on?"
                )
            rows = self._conn.execute(
                "SELECT name, state, project, role FROM snapshot"
            ).fetchall()
            mine = next((r for r in rows if r[0] == sender), None)
            from_project = (mine[2] if mine else "") or ""
            primary = next((r for r in rows if r[3] == PRIMARY and projects.same(r[2], where)), None)
            if primary:
                raise ValueError(
                    f"{shown} is where {primary[0]}, the herd-wide lead, works -- not a "
                    "project to hand work to"
                )
            if projects.same(from_project, where):
                raise ValueError(
                    f"{projects.leaf(where)} is your own project: ask its agents "
                    "(herd ask NAME) rather than hand it off"
                )
            refusal = self._handoff_refusal(sender, rows, where)
            if refusal:
                raise ValueError(refusal)
            chain = self._chain_checked(sender)
            lead = next((r for r in rows if r[3] == "lead" and projects.same(r[2], where)), None)
            if lead:
                if lead[1] not in _TAKER_RANK:
                    raise ValueError(
                        f"{lead[0]}, {projects.leaf(where)}'s lead, is {lead[1] or 'not running'}; "
                        "ask the user to start it again"
                    )
                self._not_in_chain(lead[0], sender, chain)
            elif not exists:
                raise ValueError(f"no directory {shown}: no project to hand off to")
            elif not git:
                raise ValueError(
                    f"{shown} is not a git checkout, and has no lead: the herd starts "
                    "a lead only in a git checkout; ask the user"
                )
            target = lead[0] if lead else ""
            out, mail_id = self._insert_ask(
                sender, target, body, wait, rounds, project=shown, from_project=from_project)
            if not lead:
                # One start per project: a handoff that finds one under way
                # waits on it with the others.
                starting = self._conn.execute(
                    """
                    SELECT 1 FROM asks WHERE status = 'queued' AND to_name = ''
                      AND project = ? AND id != ?
                    """,
                    (shown, out["id"]),
                ).fetchone()
                if not starting:
                    self._command(
                        "handoff", sender,
                        {"project": shown, "by": sender, "from_project": from_project,
                         "head": _head(body, 200)},
                    )
            self._conn.commit()
        if mail_id:
            self._attach_memory(mail_id, target, body)
        out.update(project=shown, starting=not lead)
        return out

    def _handoff_refusal(self, sender: str, rows, where: str) -> str | None:
        """Why SENDER may not hand work to project WHERE, or None.  ROWS are
        the snapshot's (name, state, project, role).  Caller holds `_mu`.

        Work crosses projects lead to lead, so each lead knows what happens
        in its project and nobody from outside directs its agents: an agent
        in a project that has a lead -- other than that lead -- tells its
        lead.  The rule is about the sender's project, not its role: a
        project's lead may, an agent in a project with none may (nobody
        directs it), and so may the user and an agent in no project --
        which is where a herd-wide lead, whose project is the herd itself,
        sends the user's cross-project requests from."""
        mine = next((r for r in rows if r[0] == sender), None)
        if not mine or not mine[2]:
            return None
        lead = next((r for r in rows if r[3] == "lead" and projects.same(r[2], mine[2])), None)
        if not lead or lead[0] == sender:
            return None
        return (
            f"{sender} works under {lead[0]}, {projects.leaf(mine[2])}'s lead: tell your lead "
            f"-- put what {projects.leaf(where)} needs in your answer to it.  Work crosses "
            "projects lead to lead, so each lead knows what happens in its project and "
            "nobody from outside directs its agents."
        )

    def _bind_handoff(self, project: str, lead: str, said: str, now: str) -> list[tuple[str, str, str]]:
        """Emacs answered the start of PROJECT's lead: give the handoffs that
        wait on it their taker LEAD and their mail -- or, with no LEAD, fail
        them with what it SAID.  Returns the mail to find memory for, as
        (mail, taker, body).  Caller holds `_mu`."""
        out = []
        for ask_id, sender, body in self._conn.execute(
            """
            SELECT id, from_name, body FROM asks
            WHERE status = 'queued' AND to_name = '' AND project = ?
            ORDER BY created_at
            """,
            (project,),
        ).fetchall():
            error = None
            if not lead:
                error = said or f"no lead was started in {project}"
            elif lead in self._chain(sender) or lead == sender:
                # Emacs found a lead the snapshot did not: one waiting on
                # the sender, a loop.
                error = f"{lead} is waiting on {sender}; answer its ask rather than ask it"
            if error:
                self._close_ask(ask_id, "failed", now, error=error)
                self._mail_back(ask_id)
                continue
            self._conn.execute("UPDATE asks SET to_name = ? WHERE id = ?", (lead, ask_id))
            mail_id = self._ask_mail(ask_id, lead, now)
            if mail_id:
                out.append((mail_id, lead, body))
        return out

    def _sweep_handoff_starts(self, now: str) -> None:
        """Handoffs that waited HANDOFF_START_SECONDS for a lead Emacs never
        named fail: it never took the request, or never answered it.
        Caller holds `_mu`."""
        cutoff = datetime.fromisoformat(now).timestamp() - HANDOFF_START_SECONDS
        for ask_id, project, created in self._conn.execute(
            "SELECT id, project, created_at FROM asks WHERE status = 'queued' AND to_name = ''"
        ).fetchall():
            if datetime.fromisoformat(created).timestamp() < cutoff:
                self._close_ask(
                    ask_id, "failed", now,
                    error=f"the herd did not start a lead in {project} "
                          f"within {HANDOFF_START_SECONDS}s; is Emacs up?",
                )
                self._mail_back(ask_id)

    def _attach_memory(self, mail_id: str, target: str, body: str) -> None:
        """Find what the shared memory says about BODY for TARGET and put it
        on the ask's mail, then release it for delivery -- outside the lock,
        since ranking embeds.  A failure releases it without memory."""
        memory = None
        try:
            memory = self.ask_memory(self.session(target), body)
        except Exception:  # noqa: BLE001 -- memory never holds up an ask
            memory = None
        with self._mu:
            self._conn.execute(
                "UPDATE mail SET memory = ?, memory_until = NULL WHERE id = ?",
                (memory or None, mail_id),
            )
            self._conn.commit()

    def _round(self, sender: str, target: str | None, ask_id: str,
               project: str | None = None):
        """Where SENDER asking TARGET stands: (PARENT, RUN, N, CAP) --
        the ask it is answering (a dict, or None), the run the new ask
        ASK_ID joins, how many times it has asked TARGET in it already,
        and how many it may.  CAP is None for one outside the herd: the
        user is not held to rounds.  A handoff counts by the PROJECT it
        goes to as well, its lead or not: a lead the herd is still
        starting has no name yet.  Caller holds `_mu`."""
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
                "SELECT COUNT(*) FROM asks WHERE from_name = ? "
                "AND (to_name = ? OR project = ?) AND parent = ?",
                (sender, target, project, row[0]),
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
            WHERE from_name = ? AND (to_name = ? OR project = ?)
              AND parent IS NULL AND run = ?
            """,
            (sender, target, project, run[0]),
        ).fetchone()[0]
        return None, run[0], n, ASK_ROUNDS

    def _awaited(self, name: str) -> list[str]:
        """The agents NAME has an ask of its own open with, or an outcome
        on its way from: while any, its run of asks goes on.  Caller
        holds `_mu`."""
        rows = self._conn.execute(
            f"""
            SELECT COALESCE(NULLIF(to_name, ''), project) FROM asks WHERE from_name = ? AND (
              status IN {ASK_OPEN}
              OR (wait = 1 AND collected = 0 AND status IN ('answered', 'failed')))
            UNION ALL
            SELECT from_name FROM mail WHERE to_name = ? AND status = 'queued'
              AND ask_id IS NULL AND (body LIKE 'Answer to ask %' OR body LIKE 'Ask % failed:%')
            """,
            (name, name),
        ).fetchall()
        return list(dict.fromkeys(r[0] for r in rows))

    def _turn_over(self, name: str, turn_at: str) -> bool:
        """Whether NAME's turn, begun at TURN_AT by its hooks, has ended.
        Its transcript says when it can -- claude's marks a turn's end, and
        a turn its hooks never announced (one a background task woke) --
        and beats the hooks; else a turn end its hook reported since.
        Caller holds `_mu`."""
        transcript = self._transcript(name)
        if transcript:
            import conversation
            said = conversation.turn_ended(transcript, datetime.fromisoformat(turn_at).timestamp())
            if said is not None:
                return said
        row = self._conn.execute(
            "SELECT turn_end_at FROM links WHERE session = ?", (name,)
        ).fetchone()
        return bool(row and row[0]
                    and datetime.fromisoformat(row[0]) >= datetime.fromisoformat(turn_at))

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
        # The asker's own project, when it is an agent: the user (not in the
        # snapshot) and an agent in no project are free, as with a handoff.
        mine = next((r[3] for r in rows if r[0] == sender), None)
        for name, _kind, state, project, role in rows:
            if name == want:
                if state not in _TAKER_RANK:
                    raise ValueError(f"{want} is {state or 'not running'}")
                # The herd-wide lead takes the user's requests, not agents'
                # asks: its answers come back to it, its report goes to the
                # user, and an agent with something for the user tells its own
                # lead.  (The user, not an agent, may ask it.)
                if role == PRIMARY and mine is not None and sender != want:
                    raise ValueError(
                        f"{want} is the herd-wide lead: it takes the user's requests, not "
                        "agents' asks -- answer whoever asked you, or tell your lead"
                    )
                # Cross-project work goes lead to lead: an agent may not reach
                # into another project by naming an agent in it.  Hand it to
                # that project's lead instead, who plans it with its agents.
                # (A kind-ask below already stays in the asker's project; the
                # herd's own internals address a lead as the user, so are free.)
                if mine and project and not projects.same(mine, project):
                    raise ValueError(
                        f"{want} is in {projects.leaf(project)}, another project; "
                        f"cross-project work goes lead to lead -- hand it to "
                        f"{projects.leaf(project)}'s lead with "
                        f"`herd handoff {projects.leaf(project)}', do not ask its agents"
                    )
                return name
        if not any(r[1] == want for r in rows):
            raise ValueError(f"no agent or kind named {want}")
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
        "error, collected, created_at, delivered_at, answered_at, auto_source, "
        "project, from_project"
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
            "project": row["project"] or "",
            "from_project": row["from_project"] or "",
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
            # The asker already read a provisional (auto) answer and moved on:
            # a fresh `collected = 0' would be swept back to it as mail, but
            # its own `collect' cancels that mail by the ask it names.  So
            # send the real answer as its own message, which no collect undoes.
            superseded_seen = bool(row["auto"]) and bool(row["collected"])
            self._conn.execute(
                """
                UPDATE asks SET status = 'answered', reply = ?, auto = 0,
                  replied_by = ?, collected = ?, answered_at = ?
                WHERE id = ?
                """,
                (str(body), (from_name or "").strip() or None,
                 1 if superseded_seen else 0, now, row["id"]),
            )
            # Answered before Emacs pasted it: nothing left to deliver.
            self._conn.execute(
                """
                UPDATE mail SET status = 'cancelled', error = 'answered'
                WHERE ask_id = ? AND status = 'queued'
                """,
                (row["id"],),
            )
            if superseded_seen:
                text = (
                    f"A later answer to ask {row['id']} ({_head(row['body'], 80)}) "
                    f"replaces the one the herd closed it with:\n{str(body)}"
                )
                self._conn.execute(
                    """
                    INSERT INTO mail (
                        id, from_name, to_name, body, submit, handoff, status, created_at
                    ) VALUES (?, ?, ?, ?, 1, 0, 'queued', ?)
                    """,
                    (str(uuid.uuid4()), row["to_name"], row["from_name"], text, now),
                )
            else:
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
                # An ask is closed only by a turn that began after it reached
                # the taker.  The `done' Emacs saw as the ask arrived is the
                # previous turn ending -- its `turn_at' is older than the
                # delivery -- so it must not answer this ask; the next turn,
                # begun after delivery, does.  The grace is the fallback ONLY
                # when no turn-start is known at all (turn_at NULL: a hookless
                # taker, or one a restart cleared) -- there an unknown start
                # cannot be read as "before the ask".  When turn_at IS known
                # and sits at or before delivery, the taker is still on the
                # old turn: wait, do not let the grace close it with that
                # turn's recap.  (This is the bug that closed ask f2bd1c46
                # with the herd-start turn's recap: delivered mid-turn-end,
                # then grace-closed 12s on though no new turn had begun.)
                ref = row["delivered_at"] or row["created_at"]
                turn_at = self._turn_at(row["to_name"])
                began_after = bool(
                    turn_at and ref
                    and datetime.fromisoformat(turn_at) > datetime.fromisoformat(ref))
                if not (
                    began_after
                    or (turn_at is None and self._deliver_grace_passed(ref, now))
                ):
                    return {"id": row["id"], "status": "delivered", "too_soon": True}
                # A taker whose hooks report has stopped only when its turn
                # has ended, by its hook or its transcript -- never on its
                # screen reading idle, which a view scrolled back does with
                # the agent mid-turn (ask 18f57d20, closed 23 minutes into
                # one).  The screen is the fallback for a taker with no hook.
                if turn_at is not None and not self._turn_over(row["to_name"], turn_at):
                    return {"id": row["id"], "status": "delivered", "turn_on": True}
            if dead or error:
                self._close_ask(
                    row["id"], "failed", now,
                    error=error or f"{row['to_name']} exited before answering",
                )
                status = "failed"
            else:
                last = self._last_since(
                    row["to_name"], row["created_at"], row["delivered_at"])
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

    # ---- Threads: who is talking to whom (threads.py) --------------------

    _THREAD_COLS = ("id, from_name, to_name, body, reply, status, auto, error, created_at, "
                    "delivered_at, answered_at, parent, round, round_of, project, from_project")

    def _thread_rows(self, where: str, args: tuple = ()) -> list[dict[str, Any]]:
        "Asks as dicts, for threads.py.  Caller holds `_mu`."
        cur = self._conn.execute(f"SELECT {self._THREAD_COLS} FROM asks {where}", args)
        names = [d[0] for d in cur.description]
        return [dict(zip(names, r)) for r in cur.fetchall()]

    def _with_ancestors(self, rows: list[dict[str, Any]]) -> dict[str, dict[str, Any]]:
        "ROWS by id, with the parents they name fetched too.  Caller holds `_mu`."
        by_id = {r["id"]: r for r in rows}
        for _ in range(ASK_DEPTH + 2):
            missing = {r["parent"] for r in by_id.values() if r.get("parent") and r["parent"] not in by_id}
            if not missing:
                break
            marks = ",".join("?" * len(missing))
            for r in self._thread_rows(f"WHERE id IN ({marks})", tuple(missing)):
                by_id[r["id"]] = r
        return by_id

    def _thread_snap(self) -> dict[str, dict[str, Any]]:
        "Each agent's CLI, state, project, role and branch.  Caller holds `_mu`."
        return {r[0]: {"kind": r[1], "state": r[2], "project": r[3], "role": r[4], "branch": r[5]}
                for r in self._conn.execute(
                    "SELECT name, kind, state, project, role, branch FROM snapshot").fetchall()}

    def threads(self, limit: int = 40) -> dict[str, Any]:
        """The threads with the latest activity (LIMIT of them), every open
        ask in the herd for the strip across the top, and how many threads
        are live -- an ask in them still open."""
        limit = max(1, min(200, int(limit)))
        with self._mu:
            rows = self._thread_rows(
                "ORDER BY COALESCE(answered_at, delivered_at, created_at) DESC LIMIT ?",
                (max(threads_mod.WINDOW, limit * 10),))
            open_rows = self._thread_rows(f"WHERE status IN {ASK_OPEN}")
            by_id = self._with_ancestors(rows + open_rows)
            snap = self._thread_snap()
        listed, more = threads_mod.summaries(list(by_id.values()), snap, limit)
        talking = threads_mod.talking(open_rows, snap, by_id)
        return {"threads": listed, "more": more, "talking": talking,
                "live": len({t["thread"] for t in talking})}

    def thread(self, id: str | None = None, ask: str | None = None,
               agent: str | None = None) -> dict[str, Any]:
        """One thread, whole: by its root ID, by an ASK in it, or AGENT's
        latest -- the one with the newest ask to or from it."""
        with self._mu:
            start = (id or ask or "").strip()
            if not start and agent:
                row = self._conn.execute(
                    "SELECT id FROM asks WHERE from_name = ? OR to_name = ? "
                    "ORDER BY COALESCE(answered_at, delivered_at, created_at) DESC LIMIT 1",
                    (agent, agent)).fetchone()
                if not row:
                    raise ValueError(f"no thread with {agent} in it")
                start = row[0]
            first = self._thread_rows("WHERE id = ?", (start,))
            if not first:
                raise ValueError(f"no ask {start}")
            by_id = self._with_ancestors(first)
            root = threads_mod.root_of(start, by_id)
            rows, frontier = {root: by_id[root]}, [root]
            for _ in range(ASK_DEPTH + 2):
                if not frontier:
                    break
                marks = ",".join("?" * len(frontier))
                kids = self._thread_rows(f"WHERE parent IN ({marks})", tuple(frontier))
                frontier = [k["id"] for k in kids if k["id"] not in rows]
                rows.update({k["id"]: k for k in kids})
            snap = self._thread_snap()
        return threads_mod.detail(root, list(rows.values()), snap)

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
                # A handoff that failed before it had a taker: the herd says so.
                str(uuid.uuid4()), row["to_name"] or "herd", row["from_name"], text,
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
                   parent, run, round, round_of, project, from_project, guarded
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
                    # A handoff: the project it went to and the one it came
                    # from -- the cards name projects, not only agents.
                    "project": r[16] or "",
                    "from_project": r[17] or "",
                    # Sent by an agent behind the herd's guard.
                    "guarded": bool(r[18]),
                }
            )
            if len(out) >= 50:
                break
        return out


    # ---- Promote memory into instructions ---------------------------------

    def _proposal_row(self, row) -> dict[str, Any]:
        return {
            "id": row[0], "project": row[1], "facts": json.loads(row[2] or "[]"),
            "file": row[3], "diff": row[4], "author": row[5], "status": row[6],
            "reason": row[7], "ask_id": row[8], "created_at": row[9],
            "decided_at": row[10],
        }

    _PROPOSAL_COLS = ("id, project, facts, file, diff, author, status, reason, "
                      "ask_id, created_at, decided_at")

    def propose(self, project: str, facts: list[str], file: str, diff: str,
                author: str) -> dict[str, Any]:
        """Store a pending proposal to promote FACTS of PROJECT (a store key)
        into instruction FILE, with the exact DIFF.  The herd holds it until
        the user decides on the page; it writes nothing to the repo."""
        project = (project or "").strip()
        file = (file or "").strip()
        names = [str(f).strip() for f in (facts or []) if str(f).strip()]
        if not project:
            raise ValueError("project is required")
        if not names:
            raise ValueError("name at least one fact to promote")
        if not file:
            raise ValueError("a target instruction file is required")
        if not (diff or "").strip():
            raise ValueError("the exact diff is required")
        pid = str(uuid.uuid4())
        now = datetime.now(timezone.utc).isoformat()
        facts_json = json.dumps(names)
        with self._mu:
            # Don't stack identical pending proposals: the user may press
            # "Suggest promotions" more than once (e.g. when the page showed
            # nothing), and the lead may answer each.  An identical one still
            # pending -- same project, file, facts and diff -- is returned as
            # is, so the page shows one, not a pile of the same.
            dup = self._conn.execute(
                "SELECT id FROM proposals WHERE status = 'pending' AND project = ? "
                "AND file = ? AND facts = ? AND diff = ? ORDER BY created_at DESC LIMIT 1",
                (project, file, facts_json, diff),
            ).fetchone()
            if dup:
                return {"id": dup[0], "project": project, "facts": names,
                        "file": file, "duplicate": True}
            self._conn.execute(
                "INSERT INTO proposals (id, project, facts, file, diff, author, "
                "status, created_at) VALUES (?, ?, ?, ?, ?, ?, 'pending', ?)",
                (pid, project, facts_json, file, diff,
                 (author or "unknown").strip() or "unknown", now),
            )
            self._conn.commit()
        return {"id": pid, "project": project, "facts": names, "file": file}

    def proposals(self, project: str | None = None,
                  status: str | None = None) -> list[dict[str, Any]]:
        "Proposals, newest first; of one PROJECT and/or one STATUS when given."
        where, args = [], []
        if project:
            where.append("project = ?")
            args.append(project)
        if status:
            where.append("status = ?")
            args.append(status)
        sql = f"SELECT {self._PROPOSAL_COLS} FROM proposals"
        if where:
            sql += " WHERE " + " AND ".join(where)
        sql += " ORDER BY created_at DESC"
        with self._mu:
            rows = self._conn.execute(sql, args).fetchall()
        return [self._proposal_row(r) for r in rows]

    def proposal(self, proposal_id: str) -> dict[str, Any]:
        with self._mu:
            row = self._conn.execute(
                f"SELECT {self._PROPOSAL_COLS} FROM proposals WHERE id = ?",
                (proposal_id,),
            ).fetchone()
        if not row:
            raise ValueError(f"no such proposal: {proposal_id}")
        return self._proposal_row(row)

    def proposal_by_ask(self, ask_id: str) -> dict[str, Any] | None:
        "The proposal whose apply-ask is ASK_ID, or None."
        if not ask_id:
            return None
        with self._mu:
            row = self._conn.execute(
                f"SELECT {self._PROPOSAL_COLS} FROM proposals WHERE ask_id = ?",
                (ask_id,),
            ).fetchone()
        return self._proposal_row(row) if row else None

    def approve_proposal(self, proposal_id: str, ask_id: str) -> None:
        "Record that PROPOSAL_ID was approved and handed to its lead as ASK_ID."
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            self._conn.execute(
                "UPDATE proposals SET status = 'approved', ask_id = ?, "
                "decided_at = ? WHERE id = ? AND status = 'pending'",
                (ask_id, now, proposal_id),
            )
            self._conn.commit()

    def reject_proposal(self, proposal_id: str, reason: str | None = None) -> dict[str, Any]:
        "Reject a pending proposal, recording WHY.  The facts stay memories."
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            cur = self._conn.execute(
                "UPDATE proposals SET status = 'rejected', reason = ?, "
                "decided_at = ? WHERE id = ? AND status = 'pending'",
                ((reason or "").strip() or None, now, proposal_id),
            )
            self._conn.commit()
            if cur.rowcount == 0:
                raise ValueError("no pending proposal with that id")
        return {"id": proposal_id, "status": "rejected"}

    def mark_proposal_applied(self, proposal_id: str) -> None:
        "The lead applied an approved proposal: mark it done."
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            self._conn.execute(
                "UPDATE proposals SET status = 'applied', decided_at = ? "
                "WHERE id = ? AND status = 'approved'",
                (now, proposal_id),
            )
            self._conn.commit()

    # ---- A guarded agent's guard -------------------------------------------
    #
    # Its guard decides alone (bin/herd_guard.py); what it refused comes here
    # for the card and the log.  A refusal the user may approve is approved
    # once, for that exact call: the herd writes the allowance into the
    # agent's guard directory -- only there, under the guard root, whatever
    # the request says -- and tells the agent to run it again.

    def guard_denied(self, session: str, guard_dir: str, tool: str, summary: str, reason: str,
                     rule: str, kind: str, approvable: bool, fp: str, call: str) -> dict[str, Any]:
        "Record a refusal; the same call refused again while it waits keeps its request."
        session = (session or "").strip()
        if not session:
            raise ValueError("session is required")
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            if approvable and fp:
                row = self._conn.execute(
                    "SELECT id FROM guard_requests WHERE session = ? AND fp = ? AND status = 'pending'",
                    (session, fp)).fetchone()
                if row:
                    return {"id": row[0], "status": "pending"}
            gid = uuid.uuid4().hex[:8]
            self._conn.execute(
                """
                INSERT INTO guard_requests (id, session, guard_dir, tool, summary, reason, rule, kind,
                                            approvable, fp, call, status, created_at)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (gid, session, guard_dir or "", tool or "", (summary or "")[:400], (reason or "")[:1000],
                 rule or "", kind or "", 1 if approvable else 0, fp or "", (call or "")[:4000],
                 "pending" if approvable else "denied", now),
            )
            self._conn.commit()
        return {"id": gid, "status": "pending" if approvable else "denied"}

    def guard_approve(self, gid: str, root: str | None = None) -> dict[str, Any]:
        """The user approves refused call GID, once: its allowance goes into its
        agent's guard directory under ROOT (GUARD_ROOT), and the agent is told."""
        root = os.path.realpath(os.path.expanduser(root or guard_root()))
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            row = self._conn.execute(
                "SELECT session, guard_dir, summary, approvable, fp, status FROM guard_requests WHERE id = ?",
                ((gid or "").strip(),)).fetchone()
            if not row:
                raise ValueError(f"no guard request {gid}")
            session, guard_dir, summary, approvable, fp, status = row
            if not approvable:
                raise ValueError("that call is never run by an agent; it cannot be approved")
            if status == "approved":  # approved already (a second tap): once is once
                return {"id": gid, "status": "approved"}
            if status != "pending":
                raise ValueError(f"guard request {gid} is {status}")
            mine = os.path.join(root, session)
            if not guard_dir or os.path.realpath(guard_dir) != os.path.realpath(mine) or not os.path.isdir(mine):
                raise ValueError(f"guard request {gid} is not from {session}'s guard")
            _guard_lib().add_approval(mine, fp, gid)
            self._conn.execute(
                "UPDATE guard_requests SET status = 'approved', approved_at = ? WHERE id = ?", (now, gid))
            self._conn.execute(
                """
                INSERT INTO mail (id, from_name, to_name, body, submit, handoff, status, created_at)
                VALUES (?, 'herd', ?, ?, 1, 0, 'queued', ?)
                """,
                (str(uuid.uuid4()), session,
                 f"[guard] The user approved, once: {summary}.  Run exactly that call again -- it is "
                 f"allowed this once (request {gid}).", now),
            )
            self._conn.commit()
        return {"id": gid, "status": "approved"}

    def guard_used(self, gid: str) -> dict[str, Any]:
        "The approved call ran: its approval is spent."
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            self._conn.execute(
                "UPDATE guard_requests SET status = 'used', used_at = ? WHERE id = ? AND status = 'approved'",
                (now, (gid or "").strip()))
            self._conn.commit()
        return {"id": gid, "status": "used"}

    def _guard_recent(self) -> list[dict[str, Any]]:
        """Requests the card shows: waiting or approved, and the rest of the
        last RECENT_SECONDS.  An approval unused past its time has lapsed.
        Caller holds `_mu`."""
        now = datetime.now(timezone.utc).timestamp()
        out = []
        for r in self._conn.execute(
            """
            SELECT id, session, tool, summary, reason, rule, kind, approvable, status,
                   created_at, approved_at, used_at
            FROM guard_requests ORDER BY created_at DESC LIMIT 200
            """
        ).fetchall():
            status = r[8]
            if status == "approved" and r[10] and now - datetime.fromisoformat(r[10]).timestamp() > GUARD_APPROVAL_SECONDS:
                status = "lapsed"
                self._conn.execute("UPDATE guard_requests SET status = 'lapsed' WHERE id = ?", (r[0],))
            if status not in ("pending", "approved") and now - datetime.fromisoformat(r[9]).timestamp() > RECENT_SECONDS:
                continue
            out.append({"id": r[0], "session": r[1], "tool": r[2], "summary": r[3], "reason": r[4],
                        "rule": r[5], "kind": r[6], "approvable": bool(r[7]), "status": status,
                        "created_ms": _ms(r[9]), "approved_ms": _ms(r[10]), "used_ms": _ms(r[11])})
            if len(out) >= 50:
                break
        return out

    def guard_requests(self) -> dict[str, Any]:
        with self._mu:
            out = self._guard_recent()
            self._conn.commit()
        return {"requests": out}

    # ---- Inbox item state (read / done / override), shared desk + phone -----

    def inbox_state(self, live_ids: list[str] | None = None) -> dict[str, Any]:
        """The user's state on each Inbox item: {id: {read, done, override}}.
        When LIVE_IDS is given (the items Emacs sees right now), state for any
        item not in it is pruned -- its cause resolved itself, so a later item
        of the same id starts fresh."""
        with self._mu:
            if live_ids is not None:
                keep = {str(i) for i in live_ids}
                gone = [r[0] for r in
                        self._conn.execute("SELECT item_id FROM inbox_state").fetchall()
                        if r[0] not in keep]
                for item_id in gone:
                    self._conn.execute(
                        "DELETE FROM inbox_state WHERE item_id = ?", (item_id,))
                if gone:
                    self._conn.commit()
            rows = self._conn.execute(
                "SELECT item_id, read, done, override FROM inbox_state").fetchall()
        # A list (not a map): Emacs reads JSON objects as plists, and an item
        # id makes an awkward keyword.
        return {"state": [{"id": r[0], "read": bool(r[1]), "done": bool(r[2]),
                           "override": r[3] or None} for r in rows]}

    def inbox_set(self, item_id: str, read=None, done=None, override=None) -> dict[str, Any]:
        """Set the user's state on one Inbox item.  A field left None is kept;
        READ/DONE are booleans; OVERRIDE is "need"/"update" to force a group,
        or "" to clear it back to the item's own group."""
        item_id = (item_id or "").strip()
        if not item_id:
            raise ValueError("item_id is required")
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            row = self._conn.execute(
                "SELECT read, done, override FROM inbox_state WHERE item_id = ?",
                (item_id,)).fetchone()
            cur_read, cur_done, cur_over = (row[0], row[1], row[2]) if row else (0, 0, None)
            new_read = cur_read if read is None else int(bool(read))
            new_done = cur_done if done is None else int(bool(done))
            new_over = cur_over if override is None else (override or None)
            self._conn.execute(
                """
                INSERT INTO inbox_state (item_id, read, done, override, updated_at)
                VALUES (?, ?, ?, ?, ?)
                ON CONFLICT(item_id) DO UPDATE SET
                    read = excluded.read, done = excluded.done,
                    override = excluded.override, updated_at = excluded.updated_at
                """,
                (item_id, new_read, new_done, new_over, now))
            self._conn.commit()
        return {"item_id": item_id, "read": bool(new_read),
                "done": bool(new_done), "override": new_over}

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
        pid: int | None = None,
        ancestors: list[int] | None = None,
        headless: bool = False,
    ) -> dict[str, Any]:
        """An agent's hook: what it is doing, and on which conversation.

        PID is the process of the CLI whose hook this is, ANCESTORS the
        processes above it, as the hook found them; HEADLESS, that the CLI
        runs one prompt and exits (`claude -p').  A report from a CLI the
        agent itself runs -- a probe, under the agent's name, since it
        inherits the agent's environment, directly or through a server it
        started -- is not the agent's: it is ignored whole, and says why
        (`_not_its_own').

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
            why = self._not_its_own(session, cli, pid, ancestors, headless)
            if why:
                return {"session": session, "state": None, "closed_ask": None, "ignored": why}
            if pid:
                # The agent's own CLI: what tells a CLI it runs from it.
                self._conn.execute(
                    "INSERT OR IGNORE INTO links (session, updated_at) VALUES (?, ?)",
                    (session, now),
                )
                self._conn.execute("UPDATE links SET pid = ? WHERE session = ?", (pid, session))
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
                          last_at = NULL, turn_at = NULL, last_turn_at = NULL
                        WHERE session = ?
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
                      -- the message's turn began when this turn did (`turn_at'),
                      -- so a later `settle' can tell a fresh answer from a recap.
                      last_turn_at = CASE WHEN ? IS NULL THEN last_turn_at ELSE turn_at END,
                      updated_at = ?
                    WHERE session = ?
                    """,
                    (cli or None, conversation or None, transcript or None,
                     last or None, last or None, now, last or None, now, session),
                )
            if state == "working":
                # A turn begins.  Record when, so an ask delivered mid-turn is
                # not answered by the turn already running (its `turn_at' is
                # older than the ask); the next turn, begun after it, answers.
                self._conn.execute(
                    "INSERT OR IGNORE INTO links (session, updated_at) VALUES (?, ?)",
                    (session, now),
                )
                self._conn.execute(
                    "UPDATE links SET turn_at = ?, updated_at = ? WHERE session = ?",
                    (now, now, session),
                )
            if state in ("done", "idle", "auto"):
                # A turn ends -- `auto' is one interrupted (grok's
                # StopCancelled).  A queued ask waits for this -- the turn-end
                # hook -- rather than the screen reading idle a beat sooner,
                # so it is not delivered into the moment a turn is ending.
                # And an open ask closes only on it, for an agent whose hooks
                # report (`settle').
                self._conn.execute(
                    "INSERT OR IGNORE INTO links (session, updated_at) VALUES (?, ?)",
                    (session, now),
                )
                self._conn.execute(
                    "UPDATE links SET turn_end_at = ?, updated_at = ? WHERE session = ?",
                    (now, now, session),
                )
            if state == "done":
                out = self._out(session)
                if out:
                    state, reason = "waiting", "waiting on " + _and(out)
            # A turn that ends with nothing out ends the agent's run of asks.
            # Taken from the agent's own hook, this splits runs right even
            # when a turn ends and the next begins within one Emacs tick --
            # which the tick's before/after view (`_end_runs') cannot see; it
            # stays the fallback for an agent with no hook to report `done'.
            if state == "done":
                self._conn.execute("DELETE FROM runs WHERE session = ?", (session,))
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
                turn_at = self._conn.execute(
                    "SELECT turn_at FROM links WHERE session = ?", (session,)
                ).fetchone()
                turn_at = turn_at[0] if turn_at and turn_at[0] else None
                open_ask = self._conn.execute(
                    "SELECT id, created_at, delivered_at FROM asks"
                    " WHERE to_name = ? AND status = 'delivered'",
                    (session,),
                ).fetchone()
                # Only a turn that began after the ask was delivered may answer
                # it with its own last words.  A known start no later than the
                # delivery is the turn that was ending as the ask arrived -- a
                # recap -- and leaves the ask open for the next turn.  An
                # unknown start (no `working' reached the herd -- a hookless
                # agent, or a restart cleared it) must not be read as a start
                # before the ask and close it in the instant of delivery: wait
                # out the grace, then fall back to the old close for a taker
                # whose hooks never say `working'.
                ref = (open_ask[2] or open_ask[1]) if open_ask else None
                close_now = bool(open_ask) and (
                    (datetime.fromisoformat(turn_at) > datetime.fromisoformat(ref))
                    if turn_at and ref
                    else self._deliver_grace_passed(ref, now)
                )
                if close_now:
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

    def _not_its_own(self, session: str, cli: str | None, pid: int | None,
                     ancestors: list[int] | None, headless: bool = False) -> str | None:
        """Why a hook report under SESSION's name is not SESSION's, or None.

        An agent runs other CLIs as tools -- `claude -p', `agy -p' from a
        scratch directory -- and they inherit its environment, its name in
        the herd with it, so their hooks report as the agent: their turns'
        `working' and `done', their conversation and transcript.  The herd
        then followed the probe's conversation and lost the agent's
        (claude-jnotes, 2026-10-22: its card showed a screen dump and read
        `done' while it worked).  Two things tell them apart:

        - a CLI of another kind than the agent's (a shell session takes
          any: the CLI run in it is its agent);
        - a headless CLI (`claude -p'): a herd agent is an interactive
          one, in its terminal (a shell session takes it: it is the CLI
          run in it);
        - a CLI process outside the agent's host -- its tmux pane's
          process, or its ghostel terminal's shell, as Emacs says each
          tick (HOST_PID) -- where its own CLI always runs.  A server the
          agent starts in the background leaves its tree for launchd,
          and the CLIs it runs with them (claude-jnotes, 2026-10-22
          evening: the jnotes backend's `claude -p' closed three of its
          asks with test answers).  A host whose process is gone is not
          one: a respawn's new host comes with the next tick;
        - a CLI process started under the agent's own CLI process, which
          its hooks name (PID, pinned on the link) -- the same kind of CLI
          as the agent, on a conversation of its own.  The agent's own
          CLI, on a new conversation (a /clear), is followed; so is a new
          process in its host not under the old one (a restart).
        Caller holds `_mu`."""
        row = self._conn.execute(
            "SELECT kind, host_pid FROM snapshot WHERE name = ?", (session,)).fetchone()
        kind = (row[0] or "") if row else ""
        host = row[1] if row and row[1] else None
        if cli and kind and kind != "shell" and cli != kind:
            return f"a {cli} CLI is not {session}, a {kind} agent: one it ran"
        if headless and kind != "shell":
            return f"a headless {cli or 'CLI'} run (-p) is not {session}, an interactive agent: one it started"
        if pid and host and host != pid and host not in set(ancestors or []) and pid_alive(host):
            return (f"process {pid} is not in {session}'s terminal (process {host}): one started "
                    f"outside it -- by a server it ran in the background, say")
        if pid:
            row = self._conn.execute(
                "SELECT pid FROM links WHERE session = ?", (session,)).fetchone()
            own = row[0] if row and row[0] else None
            if own and pid != own and own in set(ancestors or []):
                return f"process {pid} was started by {session}'s own CLI ({own})"
        return None

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

    def _turn_at(self, session: str) -> str | None:
        """When SESSION's turn last reported `working', or None when no
        turn-start is known -- a hookless taker, or one a restart cleared.
        Caller holds `_mu`."""
        row = self._conn.execute(
            "SELECT turn_at FROM links WHERE session = ?", (session,)
        ).fetchone()
        return row[0] if row and row[0] else None

    def _turn_after(self, session: str, ref: str | None) -> bool:
        """SESSION began a turn after REF: a `working' hook reached the herd
        since then, so the taker has read and worked on an ask delivered at
        REF.  False when no turn-start is known -- a hookless taker, or one a
        restart cleared -- which is not proof a turn began, either way.
        Caller holds `_mu`."""
        if not ref:
            return False
        turn_at = self._turn_at(session)
        return bool(
            turn_at
            and datetime.fromisoformat(turn_at) > datetime.fromisoformat(ref)
        )

    def _deliver_grace_passed(self, ref: str | None, now: str) -> bool:
        """REF (an ask's delivery) is far enough back to trust the taker
        stopping without a turn-start having reached the herd."""
        if not ref:
            return True
        return (
            datetime.fromisoformat(now) - datetime.fromisoformat(ref)
        ).total_seconds() >= DELIVER_GRACE

    def _last_since(
        self, session: str, created: str | None, delivered: str | None = None
    ) -> str | None:
        """SESSION's last answer if its turn began after the ask REACHED it.

        The turn began at `last_turn_at'; weigh that against when the ask was
        DELIVERED -- when it reached the taker -- not when it was created,
        which can be long before, queued behind another ask.  A turn already
        running at delivery (its recap ended after, but began before) does
        not answer it.  When the turn's start is unknown (no `working'
        reached the herd), fall back to the old test: the message came after
        delivery.  Caller holds `_mu`."""
        row = self._conn.execute(
            "SELECT last_message, last_at, last_turn_at FROM links WHERE session = ?",
            (session,),
        ).fetchone()
        if not row or not row[0]:
            return None
        last_message, last_at, last_turn_at = row
        ref = delivered or created
        if last_turn_at:
            if ref and datetime.fromisoformat(last_turn_at) <= datetime.fromisoformat(ref):
                return None
            return last_message
        if not last_at:
            return None
        if ref and datetime.fromisoformat(last_at) <= datetime.fromisoformat(ref):
            return None
        return last_message

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
            started = []
            for reply in replies:
                rid = str(reply.get("id") or "").strip()
                if not rid:
                    continue
                chat = self._conn.execute(
                    "SELECT chat_id, op, args FROM commands WHERE id = ?",
                    (rid,),
                ).fetchone()
                if chat and chat[1] == "handoff":
                    # Bound once the snapshot has the lead Emacs names.
                    started.append((json.loads(chat[2] or "{}").get("project") or "",
                                    str(reply.get("lead") or "").strip(),
                                    str(reply.get("text") or "")))
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
                      (name, kind, state, notes, project, reason, screen, role, prompt, room, since,
                       directory, branch, started_by, ahead, merged, host_pid, guarded)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
                        str(row.get("directory") or ""),
                        str(row.get("branch") or ""),
                        str(row.get("startedBy") or ""),
                        int(row.get("ahead") or 0),
                        1 if row.get("merged") is True else 0,
                        _pid(row.get("hostPid")),
                        1 if row.get("guarded") is True else 0,
                    ),
                )
                self._upsert_alias(name)
            self._conn.execute(
                "INSERT OR REPLACE INTO meta (k, v) VALUES ('synced', '1')"
            )
            to_attach = []
            for project, lead, said in started:
                to_attach += self._bind_handoff(project, lead, said, now)
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
            self._sweep_handoff_starts(now)
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
            # An agent that just answered an ask (`herd reply') is not yet free
            # for the next: the reply is a tool call inside a turn that is still
            # ending.  Hold the next ask until that turn has ended by the hooks
            # (a `done'/`idle' after the answer), or the grace passes -- so it
            # does not land in the turn-ending moment and get answered by this
            # turn's recap (the bug that mis-closed ask f2bd1c46).  Only asks
            # wait this way; ordinary mail delivers on state alone.
            finishing = {
                r[0]
                for r in self._conn.execute(
                    """
                    SELECT a.to_name
                    FROM asks a
                    LEFT JOIN links l ON l.session = a.to_name
                    WHERE a.status = 'answered' AND a.answered_at IS NOT NULL
                      AND a.auto = 0  -- its own `herd reply', mid-turn; an
                                      -- auto-close means the turn already ended
                      AND (julianday(?) - julianday(a.answered_at)) * 86400.0 < ?
                      AND (l.turn_end_at IS NULL OR l.turn_end_at < a.answered_at)
                    """,
                    (now, DELIVER_GRACE),
                )
            }
            deliverable = []
            for row in self._conn.execute(
                """
                SELECT m.id, m.from_name, m.to_name, m.body,
                       m.submit, m.handoff, m.ask_id, s.state, a.rounds, m.memory, a.guarded
                FROM mail m
                JOIN snapshot s ON s.name = m.to_name
                LEFT JOIN asks a ON a.id = m.ask_id
                WHERE m.status = 'queued'
                  AND (m.memory_until IS NULL OR m.memory_until < ?)
                ORDER BY m.created_at
                """,
                (now,),
            ).fetchall():
                if (row[7] or "") in hold:
                    continue
                if row[6]:
                    if row[2] in answering or row[2] in finishing:
                        continue
                    answering.add(row[2])
                deliverable.append(row[:7] + row[8:])
            asks = self._recent_asks()
            guard = self._guard_recent()
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
        for mail_id, target, body in to_attach:
            self._attach_memory(mail_id, target, body)
        pending = []
        for mail_id, from_name, to_name, body, submit, handoff, ask_id, rounds, memory, guarded in deliverable:
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
            if memory:
                # After the body, so what was asked comes first; whole, since
                # it is capped where it is found.
                text = text.rstrip() + "\n\n" + memory
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
                # Its asker runs behind the herd's guard: for the log.
                pending[-1]["guarded"] = bool(guarded)
        return {
            "pending": pending,
            "commands": commands,
            "asks": asks,
            "reports": reports,
            "links": links,
            "guard": guard,
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
        replies and pushes.  Runs at most once per PRUNE_EVERY -- the tick is
        sub-second, the window is days -- keeping the last run's time in
        `meta'.  Caller holds `_mu'."""
        now_ts = datetime.fromisoformat(now).timestamp()
        row = self._conn.execute("SELECT v FROM meta WHERE k = 'pruned'").fetchone()
        if row is not None:
            try:
                if now_ts - float(row[0]) < PRUNE_EVERY:
                    return
            except (TypeError, ValueError):
                pass
        cutoff = datetime.fromtimestamp(now_ts - HISTORY_SECONDS, tz=timezone.utc).isoformat()
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
        # `taken' is in flight (handed to Emacs, awaiting its reply), not
        # strictly terminal; after a week with no reply it is a fire-and-
        # forget command that will never come back, so it is collected too.
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
        self._conn.execute(
            "INSERT OR REPLACE INTO meta (k, v) VALUES ('pruned', ?)", (str(now_ts),)
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
                SELECT name, kind, state, notes, project, reason, screen, prompt, room, role
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
            "role": row[9] or "",
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
        with self._mu:
            cid = self._command(op, session, args, chat_id)
            self._conn.commit()
        return {"id": cid, "queued": True}

    def _command(self, op: str, session: str, args: dict[str, Any] | None = None,
                 chat_id: int | None = None) -> str:
        "Queue a command for Emacs; its id.  Caller holds `_mu` and commits."
        cid = str(uuid.uuid4())
        self._conn.execute(
            """
            INSERT INTO commands
              (id, op, session, args, chat_id, status, created_at)
            VALUES (?, ?, ?, ?, ?, 'queued', ?)
            """,
            (cid, op, session, json.dumps(args or {}), chat_id,
             datetime.now(timezone.utc).isoformat()),
        )
        return cid

    def command_reply(self, cmd_id: str) -> dict[str, Any]:
        """A queued command's status and reply text, by its id -- what `herd
        relay' waits on to learn what Emacs did.  `gone' when no such command
        is on record (pruned, or never queued)."""
        cmd_id = (cmd_id or "").strip()
        with self._mu:
            crow = self._conn.execute(
                "SELECT status FROM commands WHERE id = ?", (cmd_id,)
            ).fetchone()
            rrow = self._conn.execute(
                "SELECT text FROM replies WHERE id = ?", (cmd_id,)
            ).fetchone()
        return {
            "status": crow[0] if crow else "gone",
            "text": rrow[0] if rrow else None,
        }

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
