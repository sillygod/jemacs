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

import collections
import concurrent.futures
import hashlib
import json
import os
import sqlite3
import threading
import time
import uuid
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Any

import projects
import conversation
import cost
import subagents
import tally
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
# How long a refused call waits for the user before it expires (Emacs's
# `ghostherd-guard-request-ttl' says each tick): a request hours old is
# about a turn long over, and a respawned agent's are cleared at once.
GUARD_REQUEST_SECONDS = 3600


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


def _hold_until(seconds: float) -> str:
    "When a mail held for SECONDS is let go."
    return (datetime.now(timezone.utc) + timedelta(seconds=seconds)).isoformat()


def _seconds(row: dict[str, Any]) -> float | None:
    "How long ask ROW's work took: delivered (or made) to answered."
    begun, end = row.get("delivered_at") or row.get("created_at"), row.get("answered_at")
    try:
        return (datetime.fromisoformat(end) - datetime.fromisoformat(begun)).total_seconds()
    except (TypeError, ValueError):
        return None


def _trace_line(trace: str | None) -> str | None:
    "The one line of a stored trace (ask_trace), or None."
    try:
        line = json.loads(trace)["line"] if trace else None
    except (ValueError, KeyError, TypeError):
        return None
    return line if isinstance(line, str) and line else None


def _no_trace(why: str) -> str:
    """What `asks.trace' holds once the line cannot be made, WHY in a word: a
    trace with no line, which every reader takes as none -- not smooth, not
    rough -- where NULL is one not made yet, that each waiter waits for."""
    return json.dumps({"line": None, "failed": why})


def trace_hold_left(answered_at: str | None) -> float:
    """How long an answer's waiters still hold for its line: TRACE_HOLD from
    the answer, as its mail -- none for an answer long given."""
    try:
        since = (datetime.now(timezone.utc) - datetime.fromisoformat(answered_at)).total_seconds()
    except (TypeError, ValueError):
        return TRACE_HOLD
    return max(0.0, min(TRACE_HOLD, TRACE_HOLD - since))


def _trace_tokens(trace: str | None) -> dict[str, Any] | None:
    "What a stored trace says the work cost, in tokens (ask_trace), or None."
    try:
        tokens = json.loads(trace).get("tokens") if trace else None
    except (ValueError, AttributeError):
        return None
    return tokens if isinstance(tokens, dict) else None


def _taker_note(trace: str | None, ask_id: str) -> str | None:
    "What a stored trace tells the ask's taker on its `herd reply' (ask_trace), or None."
    try:
        import ask_trace
        return ask_trace.taker_note(json.loads(trace), ask_id) if trace else None
    except Exception:  # noqa: BLE001 -- no note, never a failed look at the ask
        return None


def trace_note(line: str) -> str:
    "LINE as it ends an answer's message: bracketed, alone on its line."
    return f"[{line}]"


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


LINT_PATH = Path(__file__).resolve().parents[1] / "bin" / "herd_lint.py"


def _lint_lib():
    "bin/herd_lint.py: the checks the hook runs, whose rules files this writes."
    import importlib.util
    spec = importlib.util.spec_from_file_location("herd_lint", LINT_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


# How often each shared-memory fact reaches an agent (`use'): in an ask's
# memory block as it is delivered, in a grok's or agy's opening as a rule
# given in full, or returned by `herd recall'.  One row per use, kept whole
# USE_DETAIL_DAYS -- the page's 30 days and room for a later page to show
# who and which ask -- and one lifetime total per fact and kind, added to
# as the rows are written; older rows are then dropped, and past
# USE_ROWS_MAX rows the oldest too, the totals keeping their count.  A use
# waits in a queue of USE_QUEUE_MAX until the tick (or the page) writes it,
# USE_FLUSH_MAX at a time: nothing that hands out memory ever waits on this
# database, and no tick writes more than a moment's worth.
USE_KINDS = ("ask", "opening", "recall")

# How an answered ask's work went, in one line (ask_trace.py): made off the
# reply's path, after it; the answer's mail to an asker not waiting holds
# for it this long at most, then goes without.
TRACE_HOLD = 3.0
# What one task cost (cost.py): the days it is worked out for, how long a
# working-out is kept, and how often -- and how many at a time -- the
# work's commits are looked for in git.
COST_DAYS = 30
# What a piece of work cost, as `cost_usage' keeps it: tally.Usage's kinds.
COST_KINDS = tally.Usage.__slots__
COST_KEEP = 30.0
OUTPUT_EVERY = 60.0
OUTPUT_MAX = 20
# A checkout that is no git checkout, or whose reflog could not be read, is
# looked at again this much later.
REFLOG_MISS = 600.0
# The commits in a main branch that name an ask, when none did: looked for
# again this much later -- the branch may have been merged since.
NAMED_AGAIN = 6 * 3600

# How long after its answer a grok taker's turn may end and still be the
# turn the answer was given in, whose usage the ask's tokens wait for.
GROK_TURN_WAIT = 1800
USE_DETAIL_DAYS = 90
USE_ROWS_MAX = 200_000
USE_QUEUE_MAX = 10_000
USE_FLUSH_MAX = 2_000

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


def _first_at(path: Path) -> float | None:
    "When the conversation at PATH began: its first record's time."
    try:
        with path.open("rb") as f:
            for line in f:
                try:
                    o = json.loads(line)
                except ValueError:
                    continue
                if isinstance(o, dict) and o.get("timestamp"):
                    return tally.epoch(o["timestamp"])
    except OSError:
        pass
    return None


def _size(path: Path) -> int:
    "PATH's size, or 0."
    try:
        return path.stat().st_size
    except OSError:
        return 0


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
        # Uses of shared-memory facts not yet written (see `use'), and the
        # connection the page reads them through -- its own, read-only: a WAL
        # reader that never takes `_mu', so counting a store's uses never
        # holds up an ask.
        self._uses: collections.deque = collections.deque(maxlen=USE_QUEUE_MAX)
        self._reader: sqlite3.Connection | None = None
        self._read_mu = threading.Lock()
        # One worker makes the asks' traces, after their replies.
        self._tracer: concurrent.futures.ThreadPoolExecutor | None = None
        self._traces: collections.deque = collections.deque(maxlen=32)
        # Seconds a guard request waits before it expires (`tick' sets it).
        self.guard_ttl = GUARD_REQUEST_SECONDS
        # One worker reads what the agents' replies cost (`_tally_pass'),
        # after a tick, every tally.EVERY at most.
        self._tallier: concurrent.futures.ThreadPoolExecutor | None = None
        self._worker_mu = threading.Lock()
        self._tally_mu = threading.Lock()
        self._tallying = False
        self._tally_at = 0.0
        self._closed = False
        # The tasks' costs as last worked out (`_cost_view'), and when; when
        # their commits were last looked for (`_cost_output').
        self._cost_seen: tuple[float, dict[str, Any]] | None = None
        self._output_at = 0.0
        # Each checkout's reflog as last looked at (`_cost_capture'): where
        # it is, its size and time, when it was last kept as read; under its
        # own lock, as a stop's look comes from an RPC's thread.
        self._reflogs: dict[str, dict[str, Any]] = {}
        self._capture_mu = threading.Lock()
        # Where in its transcript each direct task's prompt is, for a task
        # kept before tasks kept it (cost.offset_of): found once.
        self._offsets: dict[str, int | None] = {}
        # Whether the tasks kept were read by the rules the reader has now
        # (`_cost_recheck'), and the copies kept told by its rule (`_cost_recopy').
        self._said_ok = False
        self._copies_ok = False
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
        # When a request was closed without running: kept denied by the
        # user, or expired (its time out, its agent respawned).
        if "closed_at" not in {r[1] for r in self._conn.execute("PRAGMA table_info(guard_requests)")}:
            self._conn.execute("ALTER TABLE guard_requests ADD COLUMN closed_at TEXT")
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
        # A proposal is a promotion into an instruction file (`promote'), or
        # a fact made into a check (`lint', bin/herd_lint.py): its rule, as
        # JSON, which the herd writes to the checks' store on approval.
        prop_cols = {r[1] for r in self._conn.execute("PRAGMA table_info(proposals)")}
        if "kind" not in prop_cols:
            self._conn.execute("ALTER TABLE proposals ADD COLUMN kind TEXT")
        if "rule" not in prop_cols:
            self._conn.execute("ALTER TABLE proposals ADD COLUMN rule TEXT")
        # Each call a check denied (`herd lint', `herd guard'): for the log,
        # and each rule's count on the page, so a false positive shows.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS lint_denials (
                id TEXT PRIMARY KEY,
                key TEXT NOT NULL,
                fact TEXT NOT NULL,
                session TEXT,
                cli TEXT,
                command TEXT,
                created_at TEXT NOT NULL
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
        # Its directory is gone -- a worktree removed outside the herd: so
        # `herd list' tells its lead, who can stop it.
        if "directory_missing" not in cols:
            self._conn.execute("ALTER TABLE snapshot ADD COLUMN directory_missing INTEGER")
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
        # Which facts that memory gave, as [[store, name]...]: each counts
        # as a use when the mail is delivered (`use').
        if "memory_facts" not in mail_cols:
            self._conn.execute("ALTER TABLE mail ADD COLUMN memory_facts TEXT")
        # The ask whose answer this mail is, waiting for its trace (`_trace').
        if "trace_of" not in mail_cols:
            self._conn.execute("ALTER TABLE mail ADD COLUMN trace_of TEXT")
        # Each time a shared-memory fact reached an agent, and the totals of
        # the rows rolled up (`use', `_roll_up_uses').  Since when they are
        # counted is in `meta' (uses_since): a fact used before has no
        # history, which the page says rather than show it unused.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS fact_uses (
                key TEXT NOT NULL,
                fact TEXT NOT NULL,
                kind TEXT NOT NULL,
                how TEXT,
                at TEXT NOT NULL,
                agent TEXT,
                ask TEXT
            )
            """
        )
        self._conn.execute("CREATE INDEX IF NOT EXISTS fact_uses_by_fact ON fact_uses (key, fact, at)")
        self._conn.execute("CREATE INDEX IF NOT EXISTS fact_uses_by_at ON fact_uses (at)")
        # The page's window, read from the index alone.
        self._conn.execute("CREATE INDEX IF NOT EXISTS fact_uses_window ON fact_uses (key, at, fact, kind)")
        # The facts an ask came with, for its taker's note (`_delivered_facts').
        self._conn.execute("CREATE INDEX IF NOT EXISTS fact_uses_by_ask ON fact_uses (ask) WHERE ask IS NOT NULL")
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS fact_use_totals (
                key TEXT NOT NULL,
                fact TEXT NOT NULL,
                kind TEXT NOT NULL,
                n INTEGER NOT NULL,
                first_at TEXT,
                last_at TEXT,
                PRIMARY KEY (key, fact, kind)
            )
            """
        )
        # What the agents' replies cost, in tokens (tally.py): each transcript
        # read, whose it is and where the last pass stopped -- with the last
        # replies it counted, which a pass can end between the records of --
        # and each agent's totals by local day in a project, by kind.  Kept
        # for an agent stopped: its usage still counts in its project's week.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS tally_files (
                path TEXT PRIMARY KEY,
                agent TEXT NOT NULL,
                project TEXT NOT NULL,
                role TEXT,
                cli TEXT NOT NULL,
                offset INTEGER NOT NULL DEFAULT 0,
                keys TEXT,
                last_ts REAL,
                read_at TEXT NOT NULL
            )
            """
        )
        self._conn.execute("CREATE INDEX IF NOT EXISTS tally_files_by_read ON tally_files (read_at)")
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS tally_days (
                agent TEXT NOT NULL,
                project TEXT NOT NULL,
                day TEXT NOT NULL,
                role TEXT,
                input INTEGER NOT NULL DEFAULT 0,
                cache_write_5m INTEGER NOT NULL DEFAULT 0,
                cache_write_1h INTEGER NOT NULL DEFAULT 0,
                cache_read INTEGER NOT NULL DEFAULT 0,
                output INTEGER NOT NULL DEFAULT 0,
                replies INTEGER NOT NULL DEFAULT 0,
                PRIMARY KEY (agent, project, day)
            )
            """
        )
        self._conn.execute("CREATE INDEX IF NOT EXISTS tally_days_by_day ON tally_days (day)")
        # What one task cost, end to end (cost.py): each transcript the tally
        # reads, read again on a cursor of its own -- the work its agent was
        # on where it stopped -- and what each piece of work cost, by agent
        # and whether a sub-agent wrote it.  A task is a prompt of the user's;
        # `cost_links' says which work each ask was made for -- by the herd's
        # record of it (`ask:ID'), or, that gone, by what its asker's client
        # said (`said:ID') -- and each sub-agent's call (`call:ID');
        # `cost_commits' every commit made in a checkout the herd works in,
        # read from its HEAD's reflog -- kept, so a worktree gone with its
        # reflog leaves them -- and `cost_reflogs' when each checkout's was
        # read; `cost_output' the commits found otherwise, as [sha, lines,
        # merged, subject]: those in the main branch that name an ask whose
        # taker's checkout was never read (`named'), or an earlier read's.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS cost_files (
                path TEXT PRIMARY KEY,
                agent TEXT NOT NULL,
                project TEXT NOT NULL,
                role TEXT,
                cli TEXT NOT NULL,
                directory TEXT,
                offset INTEGER NOT NULL DEFAULT 0,
                keys TEXT,
                item TEXT,
                read_at TEXT NOT NULL
            )
            """
        )
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS cost_usage (
                item TEXT NOT NULL,
                agent TEXT NOT NULL,
                sub INTEGER NOT NULL,
                input INTEGER NOT NULL DEFAULT 0,
                cache_write_5m INTEGER NOT NULL DEFAULT 0,
                cache_write_1h INTEGER NOT NULL DEFAULT 0,
                cache_read INTEGER NOT NULL DEFAULT 0,
                output INTEGER NOT NULL DEFAULT 0,
                replies INTEGER NOT NULL DEFAULT 0,
                first_at REAL,
                last_at REAL,
                PRIMARY KEY (item, agent, sub)
            )
            """
        )
        self._conn.execute("CREATE INDEX IF NOT EXISTS cost_usage_by_last ON cost_usage (last_at)")
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS cost_tasks (
                id TEXT PRIMARY KEY,
                agent TEXT NOT NULL,
                project TEXT NOT NULL,
                role TEXT,
                mode TEXT NOT NULL,
                cli TEXT,
                prompt TEXT,
                at REAL,
                directory TEXT,
                path TEXT,
                offset INTEGER
            )
            """
        )
        self._conn.execute("CREATE INDEX IF NOT EXISTS cost_tasks_by_at ON cost_tasks (at)")
        # Where in its transcript a task's prompt is: its thread reads its
        # turn from there.  Kept since the Threads tab shows direct tasks.
        if "offset" not in {r[1] for r in self._conn.execute("PRAGMA table_info(cost_tasks)")}:
            self._conn.execute("ALTER TABLE cost_tasks ADD COLUMN offset INTEGER")
        # How wide its prompt is, whole (cost.width): a short one is shown
        # with the one before it (cost.contexts).  Set for the tasks kept
        # before it was as they are read again (`_cost_recheck').
        if "size" not in {r[1] for r in self._conn.execute("PRAGMA table_info(cost_tasks)")}:
            self._conn.execute("ALTER TABLE cost_tasks ADD COLUMN size INTEGER")
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS cost_links (
                key TEXT PRIMARY KEY,
                item TEXT NOT NULL
            )
            """
        )
        # When each agent's work switched, and to what: an ask the herd still
        # keeps is the work its asker was on as it was made.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS cost_switches (
                agent TEXT NOT NULL,
                at REAL NOT NULL,
                item TEXT
            )
            """
        )
        self._conn.execute("CREATE INDEX IF NOT EXISTS cost_switches_by_agent ON cost_switches (agent, at)")
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS cost_output (
                work TEXT PRIMARY KEY,
                repo TEXT,
                commits TEXT NOT NULL,
                checked_at REAL NOT NULL
            )
            """
        )
        if "named" not in {r[1] for r in self._conn.execute("PRAGMA table_info(cost_output)")}:
            self._conn.execute("ALTER TABLE cost_output ADD COLUMN named INTEGER NOT NULL DEFAULT 0")
        # A commit's `of' is the commit first made, of which it is a copy
        # (an amend, a rebase, a cherry-pick): its work counts once.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS cost_commits (
                directory TEXT NOT NULL,
                sha TEXT NOT NULL,
                at REAL NOT NULL,
                author TEXT NOT NULL,
                subject TEXT,
                lines INTEGER NOT NULL DEFAULT 0,
                of TEXT,
                merged INTEGER NOT NULL DEFAULT 0,
                checked_at REAL,
                PRIMARY KEY (directory, sha)
            )
            """
        )
        self._conn.execute("CREATE INDEX IF NOT EXISTS cost_commits_by_at ON cost_commits (at)")
        # `covered' the spans of time each checkout's reflog has been read
        # for, as JSON [[from, to], ...] (cost.cover); `repo' the checkout
        # it is a worktree of, where its commits are once it is gone.
        self._conn.execute(
            """
            CREATE TABLE IF NOT EXISTS cost_reflogs (
                directory TEXT PRIMARY KEY,
                repo TEXT,
                covered TEXT NOT NULL,
                read_at REAL NOT NULL
            )
            """
        )
        self._conn.execute(
            "INSERT OR IGNORE INTO meta (k, v) VALUES ('uses_since', ?)",
            (datetime.now(timezone.utc).isoformat(),),
        )
        ask_cols = {
            r[1] for r in self._conn.execute("PRAGMA table_info(asks)")
        }
        if "auto_source" not in ask_cols:
            self._conn.execute("ALTER TABLE asks ADD COLUMN auto_source TEXT")
        # How its work went, as JSON {line, rough, detail} (ask_trace.py).
        if "trace" not in ask_cols:
            self._conn.execute("ALTER TABLE asks ADD COLUMN trace TEXT")
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
        # A line the last sidecar was to make and never did -- it stopped
        # between an answer and its trace -- will not come now: no waiter
        # holds for it, and an answer held for it goes without.
        self._conn.execute("UPDATE asks SET trace = ? WHERE status = 'answered' AND trace IS NULL",
                           (_no_trace("lost"),))
        self._conn.execute("UPDATE mail SET memory_until = NULL"
                           " WHERE trace_of IS NOT NULL AND status = 'queued' AND memory_until IS NOT NULL")
        self._conn.commit()

    def close(self) -> None:
        self._closed = True
        if self._tracer is not None:
            self._tracer.shutdown(wait=False, cancel_futures=True)
        if self._tallier is not None:
            self._tallier.shutdown(wait=False, cancel_futures=True)
        with self._mu:
            self._flush_uses()
            try:
                self._conn.commit()
            except sqlite3.Error:
                pass
            self._conn.close()
        with self._read_mu:
            if self._reader is not None:
                self._reader.close()
                self._reader = None

    def _synced(self) -> bool:
        row = self._conn.execute(
            "SELECT v FROM meta WHERE k = 'synced'"
        ).fetchone()
        return bool(row and row[0] == "1")

    def list_sessions(self, project: str | None = None) -> dict[str, Any]:
        cols = ("name, kind, state, notes, project, reason, role, since, "
                "directory, branch, started_by, ahead, merged, directory_missing")
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
                    "directory_missing": bool(r[13]) if len(r) > 13 and r[13] is not None else False,
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
        since ranking embeds.  A failure releases it without memory.  The
        hook gives the text, or (text, [[store, name]...]) -- the facts it
        holds, each a use once the mail is delivered."""
        memory, facts = None, None
        try:
            memory = self.ask_memory(self.session(target), body)
            if isinstance(memory, tuple):
                memory, facts = memory
                facts = json.dumps([[str(k), str(n)] for k, n in facts or []]) if memory else None
        except Exception:  # noqa: BLE001 -- memory never holds up an ask
            memory, facts = None, None
        with self._mu:
            self._conn.execute(
                "UPDATE mail SET memory = ?, memory_until = NULL, memory_facts = ? WHERE id = ?",
                (memory or None, facts, mail_id),
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

    def _hooked(self, name: str) -> bool:
        """Whether NAME's own CLI's hooks report to the herd: they named
        that CLI, the kind NAME runs (another's, run under its name, is
        turned away -- `_not_its_own').  Caller holds `_mu`."""
        row = self._conn.execute(
            "SELECT l.cli, s.kind FROM links l JOIN snapshot s ON s.name = l.session"
            " WHERE l.session = ?",
            (name,),
        ).fetchone()
        return bool(row and row[0] and row[0] == row[1])

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
        "project, from_project, trace"
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
            # How the work went, one line (ask_trace), once it is made; and
            # whether it is, or never will be: nothing more is coming.
            "trace": _trace_line(row["trace"]),
            "traced": bool(row["trace"]),
            # And, when it was rough, what its taker is told (bin/herd reply).
            "taker_note": _taker_note(row["trace"], row["id"]),
            # What the work cost, in tokens (tally.py): the one number, the
            # kinds, its sub-agents' share; `pending' while grok's turn has
            # yet to end.  None when its transcript was not read.
            "tokens": _trace_tokens(row["trace"]),
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
                  replied_by = ?, collected = ?, answered_at = ?, trace = NULL
                WHERE id = ?
                """,
                (str(body), (from_name or "").strip() or None,
                 1 if superseded_seen else 0, now, row["id"]),
            )
            later = self._trace_inline(row["id"])
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
                        id, from_name, to_name, body, submit, handoff, status, created_at,
                        memory_until, trace_of
                    ) VALUES (?, ?, ?, ?, 1, 0, 'queued', ?, ?, ?)
                    """,
                    (str(uuid.uuid4()), row["to_name"], row["from_name"], text, now,
                     _hold_until(TRACE_HOLD) if later else None, row["id"] if later else None),
                )
            else:
                self._mail_back(row["id"], hold=later)
            self._conn.commit()
        if later:
            self._trace_soon(row["id"])
        self._capture_soon(row["to_name"])
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
                # No turn at all, of a taker whose hooks report -- they say
                # each turn it begins: it never took the ask.  A dialog let
                # the paste go by (c644aebc: grok's folder-trust prompt, the
                # Return after the paste its "Yes, proceed").  Its screen
                # answers nothing: fail the ask, saying so, for the asker to
                # send again.  The grace's screen is for a taker with none.
                if turn_at is None and self._hooked(row["to_name"]):
                    error = (f"{row['to_name']} began no turn on it: its CLI "
                             "never took the ask -- send it again")
                # A taker whose hooks report has stopped only when its turn
                # has ended, by its hook or its transcript -- never on its
                # screen reading idle, which a view scrolled back does with
                # the agent mid-turn (ask 18f57d20, closed 23 minutes into
                # one).  The screen is the fallback for a taker with no hook.
                if turn_at is not None and not self._turn_over(row["to_name"], turn_at):
                    return {"id": row["id"], "status": "delivered", "turn_on": True}
            if dead or error:
                # Emacs has not restored its herd yet (a fresh Emacs ticking
                # an empty snapshot): its `dead' is not proof the taker died.
                # Hold the ask delivered; Emacs settles it again once it has
                # restored, and a taker that comes back answers it normally.
                restored_row = self._conn.execute(
                    "SELECT v FROM meta WHERE k = 'restored'"
                ).fetchone()
                if restored_row is not None and restored_row[0] == "0":
                    return {"id": row["id"], "status": "delivered", "unrestored": True}
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
            later = status == "answered" and self._trace_inline(row["id"])
            self._mail_back(row["id"], hold=later)
            self._conn.commit()
        if later:
            self._trace_soon(row["id"])
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
                    "delivered_at, answered_at, parent, round, round_of, project, from_project, trace")

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
        # What the task each thread is part of cost (cost.py), and lead mode
        # against direct mode over a week and a month.
        view = self._cost_view()
        for t in listed:
            t["cost"] = self._thread_cost(t["id"], view)
        # And each task told to an agent directly, a thread of its own: no
        # ask begins it, so the asks' threads have none of them.
        for row in view["tasks"].values():
            if row["mode"] == "direct":
                t = threads_mod.direct(row, snap, view["on"].get(row["agent"]) == "task:" + row["id"])
                t["cost"] = self._thread_cost(t["id"], view)
                listed.append(t)
        listed.sort(key=lambda t: -(t["at"] or 0))
        more = more or len(listed) > limit
        listed = listed[:limit]
        threads_mod.one_cost(listed)
        return {"threads": listed, "more": more, "talking": talking,
                "live": len({t["thread"] for t in talking}),
                "costs": [self.costs(days)["compare"] for days in (7, 30)]}

    def thread(self, id: str | None = None, ask: str | None = None,
               agent: str | None = None) -> dict[str, Any]:
        """One thread, whole: by its root ID, by an ASK in it, or AGENT's
        latest -- the one with the newest ask to or from it.  A direct
        task's thread by its ID, `task:ID'."""
        if (id or "").startswith("task:"):
            return self._direct_thread(id[5:])
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
        out = threads_mod.detail(root, list(rows.values()), snap)
        out["cost"] = self._thread_cost(root, self._cost_view(), whole=True)
        return out

    def _direct_thread(self, tid: str) -> dict[str, Any]:
        """Task TID, told to an agent directly, as a thread: the user's words
        and the reply that ended the turn they began, read from its
        transcript in the room's way (conversation.turn) -- from where its
        prompt is, or, kept before that was, found again (cost.offset_of)."""
        view = self._cost_view()
        row = view["tasks"].get(tid)
        if not row or row["mode"] != "direct":
            raise ValueError(f"no task {tid}")
        with self._read_mu:
            path, cli, offset = self._read_conn().execute(
                "SELECT path, cli, offset FROM cost_tasks WHERE id = ?", (tid,)).fetchone()
        if offset is None and cli != "grok" and path:
            if tid not in self._offsets:
                self._offsets[tid] = cost.offset_of(path, cli, tid)
            offset = self._offsets[tid]
        try:
            said = conversation.turn(path, row["agent"], row["prompt"], offset if cli != "grok" else None,
                                     cost.PROMPT_MAX)
        except (ValueError, OSError):
            said = None
        with self._mu:
            snap = self._thread_snap()
        out = threads_mod.direct_detail(row, said, snap, view["on"].get(row["agent"]) == "task:" + tid)
        out["cost"] = self._thread_cost(out["id"], view, whole=True)
        return out

    def _close_ask(self, ask_id: str, status: str, now: str, error: str) -> None:
        "Caller holds `_mu`."
        self._conn.execute(
            f"""
            UPDATE asks SET status = ?, error = ?, answered_at = ?
            WHERE id = ? AND status IN {ASK_OPEN}
            """,
            (status, error, now, ask_id),
        )

    def _mail_back(self, ask_id: str, hold: bool = False) -> None:
        """Mail a closed ask's outcome to an asker that is not waiting.
        HOLD: its trace is being made (`_trace_soon'), and the mail waits a
        moment for it.  Caller holds `_mu`."""
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
        # An answer goes with how its work went: made already (from the
        # herd's records alone, or an answer swept back late), or waited for
        # a moment while its taker's transcript is read (`_trace').
        line = _trace_line(row["trace"]) if row["status"] == "answered" else None
        held = hold and row["status"] == "answered" and not row["trace"]
        self._conn.execute(
            """
            INSERT INTO mail (
                id, from_name, to_name, body, submit, handoff, status, created_at,
                memory, memory_until, trace_of
            ) VALUES (?, ?, ?, ?, 1, 0, 'queued', ?, ?, ?, ?)
            """,
            (
                # A handoff that failed before it had a taker: the herd says so.
                str(uuid.uuid4()), row["to_name"] or "herd", row["from_name"], text,
                datetime.now(timezone.utc).isoformat(),
                trace_note(line) if line else None,
                _hold_until(TRACE_HOLD) if held else None, row["id"] if held else None,
            ),
        )
        self._conn.execute("UPDATE asks SET collected = 1 WHERE id = ?", (row["id"],))

    # ---- How the work went (ask_trace) ----------------------------------------

    def _trace_inline(self, ask_id: str) -> bool:
        """ASK_ID's trace when there is no transcript to read -- the herd's
        own records alone, made here, stored at once -- and True when there
        is one: then `_trace_soon' makes it, off the reply's path.  Caller
        holds `_mu`; never raises (False: no line)."""
        try:
            import ask_trace
            row = self._ask_row(ask_id)
            if self._transcript(row["to_name"]):
                return True
            work = ask_trace.Work()
            work.why = "no conversation known for it"
            out = ask_trace.compose(work, _seconds(row), **self._trace_facts(row))
            if out["rough"]:
                out["memory"] = self._delivered_facts(ask_id)
            self._conn.execute("UPDATE asks SET trace = ? WHERE id = ?",
                               (json.dumps(out, ensure_ascii=False), ask_id))
        except Exception:  # noqa: BLE001
            try:
                self._no_line(ask_id, "error")
            except Exception:  # noqa: BLE001
                pass
        return False

    def _no_line(self, ask_id: str, why: str) -> None:
        """No line is coming for ASK_ID (WHY): say so on the ask, which lets
        each await on it go, and send the answer held for it without.  A
        line already made stays.  Caller holds `_mu`, and commits."""
        self._conn.execute("UPDATE asks SET trace = ? WHERE id = ? AND trace IS NULL",
                           (_no_trace(why), ask_id))
        self._conn.execute(
            "UPDATE mail SET memory_until = NULL WHERE trace_of = ? AND status = 'queued'", (ask_id,))

    def _give_up_trace(self, ask_id: str, why: str) -> None:
        "`_no_line', taking `_mu'; never raises."
        try:
            with self._mu:
                self._no_line(ask_id, why)
                self._conn.commit()
        except Exception:  # noqa: BLE001 -- closed: the next sidecar's sweep says it
            pass

    def _trace_soon(self, ask_id: str) -> None:
        "Make ASK_ID's trace on the worker, after its reply: never in its way."
        try:
            if self._tracer is None:
                self._tracer = concurrent.futures.ThreadPoolExecutor(
                    max_workers=1, thread_name_prefix="ask-trace")
            self._traces.append(self._tracer.submit(self._trace, ask_id))
        except Exception:  # noqa: BLE001 -- the worker would not take it
            self._give_up_trace(ask_id, "not started")

    def wait_traces(self, timeout: float = 10.0) -> None:
        "Until the traces asked for so far are made (tests, a measurement)."
        concurrent.futures.wait(list(self._traces), timeout=timeout)

    def _trace(self, ask_id: str) -> None:
        """ASK_ID's line: what its taker's transcript shows of the work, and
        the herd's own records of it -- stored on the ask, and put on the
        answer's mail that waits for it.  Without one (`_no_line'), every
        waiter goes on at once: the mail, an await, the taker's look."""
        import ask_trace
        try:
            with self._mu:
                row = self._ask_row(ask_id)
                if row["status"] != "answered":
                    self._no_line(ask_id, "not answered")
                    self._conn.commit()
                    return
                transcript = self._transcript(row["to_name"])
                facts = self._trace_facts(row)
                said = [r for (r,) in self._conn.execute(
                    "SELECT reason FROM guard_requests WHERE session = ? AND created_at >= ? AND created_at <= ?"
                    " ORDER BY created_at LIMIT 500", self._trace_window(row))]
            _, begun, end = self._trace_window(row)
            work = ask_trace.read_work(transcript, row["id"], said=said,
                                       since=tally.epoch(begun), until=tally.epoch(end))
            out = ask_trace.compose(work, _seconds(row), **facts)
            with self._mu:
                if out["rough"]:
                    out["memory"] = self._delivered_facts(row["id"])
                self._conn.execute("UPDATE asks SET trace = ? WHERE id = ?",
                                   (json.dumps(out, ensure_ascii=False), row["id"]))
                self._conn.execute(
                    "UPDATE mail SET memory = ?, memory_until = NULL WHERE trace_of = ? AND status = 'queued'",
                    (trace_note(out["line"]) if out["line"] else None, row["id"]))
                self._conn.commit()
        except Exception:  # noqa: BLE001 -- no line, never a held answer
            self._give_up_trace(ask_id, "error")

    def _delivered_facts(self, ask_id: str) -> list[dict[str, Any]]:
        """The shared-memory facts ASK_ID came with (its uses of kind `ask'),
        as {name, user}: what its taker's note offers to update rather than
        say again.  Only for rough work; caller holds `_mu`; never raises."""
        try:
            from shared_memory import USER_KEY
            self._flush_uses()
            return [{"name": fact, "user": key == USER_KEY} for key, fact in self._conn.execute(
                "SELECT key, fact FROM fact_uses WHERE ask = ? AND kind = 'ask'"
                " GROUP BY key, fact ORDER BY MIN(rowid) LIMIT 8", (ask_id,)).fetchall()]
        except Exception:  # noqa: BLE001 -- the note goes without, never the line
            return []

    @staticmethod
    def _trace_window(row: dict[str, Any]) -> tuple[str, str, str]:
        "Whose work on ROW, and from when to when: (taker, begun, end)."
        return (row["to_name"], row["delivered_at"] or row["created_at"],
                row["answered_at"] or datetime.now(timezone.utc).isoformat())

    def _trace_facts(self, row: dict[str, Any]) -> dict[str, Any]:
        """What the herd itself saw of ROW's work: the guard's and the checks'
        denials of its taker while it worked, the asks it sent answering it,
        the relays it was asked for.  Caller holds `_mu`."""
        window = self._trace_window(row)

        def count(sql: str) -> int:
            return self._conn.execute(sql, window).fetchone()[0]
        asks: dict[str, int] = {}
        for to, project in self._conn.execute(
                "SELECT to_name, project FROM asks WHERE parent = ?", (row["id"],)).fetchall():
            who = to or (projects.leaf(project) + "'s lead" if project else "?")
            asks[who] = asks.get(who, 0) + 1
        return {
            "guard": count("SELECT COUNT(*) FROM guard_requests WHERE session = ? AND created_at >= ? AND created_at <= ?"),
            "lint": count("SELECT COUNT(*) FROM lint_denials WHERE session = ? AND created_at >= ? AND created_at <= ?"),
            "relays": count("SELECT COUNT(*) FROM commands WHERE op = 'relay' AND session = ?"
                            " AND created_at >= ? AND created_at <= ?"),
            "asks": asks,
        }

    # ---- What the agents' replies cost (tally.py) ------------------------------

    def _tally_soon(self) -> None:
        """A pass of the tally on its worker, unless one is on its way or
        one ran within tally.EVERY: never in a tick's way."""
        if self._tallying or self._closed or time.monotonic() - self._tally_at < tally.EVERY:
            return
        self._tallying = True
        try:
            self._worker().submit(self._tally_run)
        except Exception:  # noqa: BLE001 -- shutting down: the next sidecar reads it
            self._tallying = False

    def _worker(self) -> concurrent.futures.ThreadPoolExecutor:
        "The tally's one worker."
        with self._worker_mu:
            if self._tallier is None:
                self._tallier = concurrent.futures.ThreadPoolExecutor(max_workers=1, thread_name_prefix="tally")
            return self._tallier

    def _capture_soon(self, agent: str) -> None:
        """AGENT's checkout's commits read now, on the tally's worker
        (`_cost_capture'): it has answered an ask, and its lead may stop
        it -- its worktree and reflog gone -- before the next pass."""
        with self._mu:
            row = self._conn.execute("SELECT directory FROM snapshot WHERE name = ?", (agent,)).fetchone()
        if not (row and row[0]) or self._closed:
            return
        try:
            self._worker().submit(self._cost_capture, [row[0]], True)
        except Exception:  # noqa: BLE001 -- shutting down: the next pass reads it
            pass

    def _tally_run(self) -> None:
        "Passes, a moment apart, until each file is read to its end."
        try:
            while self._tally_pass() and not self._closed:
                time.sleep(tally.CATCH_UP)
        except Exception:  # noqa: BLE001 -- a failed pass wrote nothing; the next reads it
            pass
        finally:
            self._tally_at = time.monotonic()
            self._tallying = False

    def _tally_pass(self, budget: int = tally.PASS_BYTES) -> bool:
        """Read what each agent's transcripts have added since the last pass
        and add it to its days -- a live agent's, and a file read within
        tally.IDLE_DAYS, so what one wrote as it stopped still counts.  True
        when a file has more than BUDGET let this pass read.  Files are read
        without `_mu'; what each gave is written with its new offset in one
        commit, so nothing is counted twice, not even after a crash.  One
        pass at a time."""
        with self._tally_mu:
            self._cost_recheck()
            self._cost_recopy()
            more = self._tally_read(budget)
            more = self._cost_read(budget) or more
            self._cost_capture()
            self._cost_output()
            return more

    def _tally_read(self, budget: int) -> bool:
        "`_tally_pass', its lock held."
        now = datetime.now(timezone.utc)
        with self._mu:
            live = {r[0]: (r[1] or "", r[2] or "") for r in self._conn.execute(
                "SELECT name, project, role FROM snapshot")}
            links = {r[0]: r[1] for r in self._conn.execute("SELECT session, transcript FROM links")
                     if r[0] in live}
        wanted: dict[str, tuple[str, str, str, str]] = {}
        for name, (project, role) in live.items():
            for path, cli in tally.files_of(links.get(name)):
                wanted.setdefault(str(path), (name, project, role, cli))
        cols = ("path", "agent", "project", "role", "cli", "offset", "keys", "last_ts")
        cutoff = (now - timedelta(days=tally.IDLE_DAYS)).isoformat()
        with self._mu:
            known = {r[0]: dict(zip(cols, r)) for r in self._conn.execute(
                f"SELECT {', '.join(cols)} FROM tally_files WHERE read_at >= ?", (cutoff,))}
            paths = [p for p in wanted if p not in known]
            for i in range(0, len(paths), 500):
                chunk = paths[i:i + 500]
                known.update({r[0]: dict(zip(cols, r)) for r in self._conn.execute(
                    f"SELECT {', '.join(cols)} FROM tally_files WHERE path IN ({','.join('?' * len(chunk))})",
                    chunk)})
        for path, (name, project, role, cli) in wanted.items():
            f = known.get(path)
            if f is None:
                known[path] = {"path": path, "agent": name, "project": project, "role": role, "cli": cli,
                               "offset": 0, "keys": None, "last_ts": None, "new": True}
            elif f["agent"] == name:
                # Its agent as it is now: a project's lead may be made one later.
                f["project"], f["role"] = project, role
        days: dict[tuple[str, str, str], tally.Usage] = {}
        roles: dict[tuple[str, str, str], str] = {}
        done = []
        turns = []
        more, left = False, budget
        for path, f in known.items():
            try:
                size = Path(path).stat().st_size
            except OSError:
                continue
            offset, upto = f["offset"] or 0, None
            if size < offset:
                # Written anew, shorter: read again, past what was counted.
                offset, upto, f["keys"] = 0, f["last_ts"], None
            if size == offset and not f.get("new"):
                continue
            if left <= 0:
                more = True
                break
            try:
                found, end, rest = tally.read_from(Path(path), f["cli"], offset, left)
            except (OSError, KeyError):
                continue
            left -= end - offset
            more = more or rest
            try:
                counted = tally.Counted(json.loads(f["keys"]) if f["keys"] else ())
            except ValueError:
                counted = tally.Counted()
            last = f["last_ts"]
            for key, when, u in found:
                if not counted.first(key) or (upto is not None and when is not None and when <= upto):
                    continue
                k = (f["agent"], f["project"], tally.day(when))
                days.setdefault(k, tally.Usage()).add(u)
                roles[k] = f["role"] or ""
                if when is not None:
                    last = max(last or 0.0, when)
                    if f["cli"] == "grok":
                        turns.append((f["agent"], when, u))
            done.append((path, f, end, json.dumps(list(counted.keys)), last))
        if not done:
            return more
        with self._mu:
            for (agent, project, d), u in days.items():
                v = u.stored()
                self._conn.execute(
                    """
                    INSERT INTO tally_days (agent, project, day, role, input, cache_write_5m,
                      cache_write_1h, cache_read, output, replies)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ON CONFLICT (agent, project, day) DO UPDATE SET
                      role = excluded.role, input = input + excluded.input,
                      cache_write_5m = cache_write_5m + excluded.cache_write_5m,
                      cache_write_1h = cache_write_1h + excluded.cache_write_1h,
                      cache_read = cache_read + excluded.cache_read,
                      output = output + excluded.output, replies = replies + excluded.replies
                    """,
                    (agent, project, d, roles[(agent, project, d)], v["input"], v["cache_write_5m"],
                     v["cache_write_1h"], v["cache_read"], v["output"], v["replies"]),
                )
            for path, f, end, keys, last in done:
                self._conn.execute(
                    """
                    INSERT INTO tally_files (path, agent, project, role, cli, offset, keys, last_ts, read_at)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ON CONFLICT (path) DO UPDATE SET project = excluded.project, role = excluded.role,
                      offset = excluded.offset, keys = excluded.keys, last_ts = excluded.last_ts,
                      read_at = excluded.read_at
                    """,
                    (path, f["agent"], f["project"], f["role"] or "", f["cli"], end, keys, last,
                     now.isoformat()),
                )
            if turns:
                self._grok_turns_ended(turns)
            self._conn.commit()
        return more

    def _grok_turns_ended(self, turns: list[tuple[str, float, tally.Usage]]) -> None:
        """grok writes a turn's usage as the turn ends -- after the answer it
        gave in it, so an ask's line went without (ask_trace: `pending').
        Each such ask takes the first of its taker's TURNS to end after its
        answer, within GROK_TURN_WAIT.  Caller holds `_mu`."""
        for agent, when, u in sorted(turns, key=lambda t: t[1]):
            for ask_id, trace, answered_at in self._conn.execute(
                    "SELECT id, trace, answered_at FROM asks WHERE to_name = ? AND status = 'answered'"
                    " AND trace LIKE '%\"pending\": true%'", (agent,)).fetchall():
                answered = tally.epoch(answered_at)
                try:
                    out = json.loads(trace)
                    tokens = out["tokens"]
                except (ValueError, KeyError, TypeError):
                    continue
                if not (isinstance(tokens, dict) and tokens.get("pending") and answered is not None
                        and answered <= when <= answered + GROK_TURN_WAIT):
                    continue
                spent = tally.Usage(input=tokens.get("input"), cache_write_5m=tokens.get("cache_write"),
                                    cache_read=tokens.get("cache_read"), output=tokens.get("output")).add(u)
                out["tokens"] = dict(tokens, **spent.shown(), pending=False)
                self._conn.execute("UPDATE asks SET trace = ? WHERE id = ?",
                                   (json.dumps(out, ensure_ascii=False), ask_id))

    def tally(self) -> dict[str, Any]:
        """Each agent's and each project's tokens, today and over the last
        week, as the passes have counted them (tally.summary).  Read through
        the read-only connection: never in an ask's way."""
        today = date.today()
        first = (today - timedelta(days=tally.WEEK - 1)).isoformat()
        cols = ("agent", "project", "day", "role") + tally.Usage.__slots__
        with self._read_mu:
            rows = [dict(zip(cols, r)) for r in self._read_conn().execute(
                f"SELECT {', '.join(cols)} FROM tally_days WHERE day >= ?", (first,))]
        return tally.summary(rows, today)

    # ---- What one task cost (cost.py) ------------------------------------------

    def _cost_read(self, budget: int) -> bool:
        """`_tally_pass''s second reading: each file the tally reads, on a
        cursor of its own (cost.Tracker) -- the work its agent was on, and
        what each reply cost it.  A file the tally read before this was is
        read from its start.  A sub-agent's after its parent's, whose call
        to it says whose work it is.  True when a file has more than BUDGET
        let this pass read.  Caller holds `_tally_mu'."""
        now = datetime.now(timezone.utc)
        cutoff = (now - timedelta(days=tally.IDLE_DAYS)).isoformat()
        with self._mu:
            live = {r[0]: r[1] or "" for r in self._conn.execute("SELECT name, directory FROM snapshot")}
            files = [dict(zip(("path", "agent", "project", "role", "cli"), r), tasks=True) for r in self._conn.execute(
                "SELECT path, agent, project, role, cli FROM tally_files WHERE read_at >= ?", (cutoff,))]
            gone = [r for r in self._conn.execute("SELECT session, transcript FROM links") if r[0] not in live]
            cur = {r[0]: dict(zip(("offset", "keys", "item", "directory"), r[1:])) for r in self._conn.execute(
                "SELECT path, offset, keys, item, directory FROM cost_files")}
            links = dict(self._conn.execute("SELECT key, item FROM cost_links WHERE key LIKE 'call:%'"))
        # An agent no longer in the herd, read for the asks it took: the work
        # behind a task its lead is still costed for.  Its own words are no
        # task -- its role and project are not known -- and only a
        # conversation of the last COST_DAYS.
        seen = {f["path"] for f in files}
        for name, transcript in gone:
            for path, cli in tally.files_of(transcript):
                if str(path) not in seen and time.time() - _mtime(str(path)) < COST_DAYS * 86400:
                    seen.add(str(path))
                    files.append({"path": str(path), "agent": name, "project": "", "role": "", "cli": cli,
                                  "tasks": False})
        # Parents first: a sub-agent's work is its parent's call's.
        files.sort(key=lambda f: "/subagents/" in f["path"])
        usage: dict[tuple[str, str, int], list[Any]] = {}
        tasks, new_links, done, switches, read = [], {}, [], [], set()
        more, left = False, budget
        for f in files:
            path, c = f["path"], cur.get(f["path"]) or {"offset": 0, "keys": None, "item": None, "directory": None}
            directory = live.get(f["agent"]) or c["directory"] or ""
            try:
                size = Path(path).stat().st_size
            except OSError:
                continue
            offset = c["offset"] or 0
            if size < offset:
                offset, c["keys"] = 0, None
            if size == offset and path in cur:
                continue
            if left <= 0:
                more = True
                break
            sub = "/subagents/agent-" in path
            item = c["item"]
            if sub and item is None:
                parent = Path(path).parent.parent.with_suffix(".jsonl")
                call = conversation.sub_meta(parent, Path(path).stem[len("agent-"):]).get("toolUseId")
                item = new_links.get(f"call:{call}") or links.get(f"call:{call}")
                if item is None:
                    pc = cur.get(str(parent))
                    behind = (pc is None or (pc["offset"] or 0) < _size(parent)) and str(parent) not in read
                    if call and behind:
                        continue   # its parent's call not read yet: next pass
                    # No call to go by (an older claude wrote no meta): what
                    # its parent was on as it began.
                    item = self._cost_on(f["agent"], _first_at(Path(path)), switches)
            try:
                keys = json.loads(c["keys"]) if c["keys"] else ()
            except ValueError:
                keys = ()
            t = cost.Tracker(path, f["cli"], item, keys, sub=sub, tasks=f["tasks"])
            try:
                end, rest = cost.read_from(Path(path), t, offset, left)
            except OSError:
                continue
            left -= end - offset
            more = more or rest
            for (it, s), (u, first, last) in t.usage.items():
                got = usage.setdefault((it, f["agent"], s), [tally.Usage(), None, None])
                got[0].add(u)
                got[1] = first if got[1] is None else got[1] if first is None else min(got[1], first)
                got[2] = last if got[2] is None else got[2] if last is None else max(got[2], last)
            mode = "lead" if (f["role"] or "") in cost.LEADS else "direct"
            for task in t.tasks:
                tasks.append((task["id"], f["agent"], f["project"], f["role"] or "", mode, f["cli"],
                              task["prompt"], task["at"], directory, path, task["offset"], task["size"]))
            new_links.update({"said:" + a: it for a, it in t.asked.items()})
            new_links.update({"call:" + k: it for k, it in t.spawned.items()})
            switches += [(f["agent"], at, it) for at, it in t.switches]
            done.append((path, f, directory, end, json.dumps(list(t.counted.keys)), t.item))
            read.add(path)
        if not done:
            return more
        with self._mu:
            for (it, agent, s), (u, first, last) in usage.items():
                v = u.stored()
                self._cost_add((it, agent, s, v["input"], v["cache_write_5m"], v["cache_write_1h"], v["cache_read"],
                                v["output"], v["replies"], first, last))
            self._conn.executemany(
                "INSERT OR IGNORE INTO cost_tasks (id, agent, project, role, mode, cli, prompt, at, directory, path,"
                " offset, size) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", tasks)
            # The first word on an ask is its asker's: a later one is a quote.
            self._conn.executemany("INSERT OR IGNORE INTO cost_links (key, item) VALUES (?, ?)",
                                   list(new_links.items()))
            self._conn.executemany("INSERT INTO cost_switches (agent, at, item) VALUES (?, ?, ?)", switches)
            self._cost_asks()
            for path, f, directory, end, keys, item in done:
                self._conn.execute(
                    """
                    INSERT INTO cost_files (path, agent, project, role, cli, directory, offset, keys, item, read_at)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ON CONFLICT (path) DO UPDATE SET project = excluded.project, role = excluded.role,
                      directory = excluded.directory, offset = excluded.offset, keys = excluded.keys,
                      item = excluded.item, read_at = excluded.read_at
                    """,
                    (path, f["agent"], f["project"], f["role"] or "", f["cli"], directory, end, keys, item,
                     now.isoformat()),
                )
            self._conn.commit()
        self._cost_seen = None
        return more

    def _cost_add(self, row: tuple[Any, ...]) -> None:
        """Add ROW -- item, agent, sub, the kinds as stored (COST_KINDS),
        first and last reply -- to what that piece of work cost.  Caller
        holds `_mu'."""
        self._conn.execute(
            f"""
            INSERT INTO cost_usage (item, agent, sub, {', '.join(COST_KINDS)}, first_at, last_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT (item, agent, sub) DO UPDATE SET
              input = input + excluded.input,
              cache_write_5m = cache_write_5m + excluded.cache_write_5m,
              cache_write_1h = cache_write_1h + excluded.cache_write_1h,
              cache_read = cache_read + excluded.cache_read,
              output = output + excluded.output, replies = replies + excluded.replies,
              first_at = MIN(COALESCE(first_at, excluded.first_at), COALESCE(excluded.first_at, first_at)),
              last_at = MAX(COALESCE(last_at, excluded.last_at), COALESCE(excluded.last_at, last_at))
            """,
            row,
        )

    def _cost_recheck(self) -> None:
        """The tasks kept, read again by the rules that tell the user's words
        from the herd's, and what of them is a task's title, as they are now
        (cost.SAID_VERSION) -- once each time they change, before the pass
        reads on.  A task whose words are now the herd's is none: what it
        cost goes to the work its agent was on before them, as a reading now
        would count it (`_cost_unmake'), and one whose words read otherwise
        now takes them as its prompt.  A task whose file no longer has its
        words is judged by the title kept of them, and keeps it.  The commits kept are not touched: they cannot be
        read again.

        First a backup of the store, beside it in backups/; then each task
        put right in a commit of its own, so a sidecar stopped half-way
        does the rest the next time; then checked -- every task as read
        now, the commits as they were -- and only then marked done.  One
        that fails the check is marked so, not tried again: the backup is
        there.  Caller holds `_tally_mu'."""
        if self._said_ok:
            return
        version = str(cost.SAID_VERSION)
        with self._mu:
            row = self._conn.execute("SELECT v FROM meta WHERE k = 'cost_said'").fetchone()
            if row and row[0] in (version, "failed " + version):
                self._said_ok = True
                return
            tasks = self._conn.execute("SELECT id, agent, path, cli, prompt, offset, size FROM cost_tasks").fetchall()
            commits = self._cost_census()
            backup = self._conn.execute("SELECT v FROM meta WHERE k = 'cost_said_backup'").fetchone()
        report: dict[str, Any] = {"version": cost.SAID_VERSION, "tasks": len(tasks), "unmade": 0, "retitled": 0,
                                  "placed": 0, "unread": 0, "moved": 0, "dropped": 0}
        if tasks and not (backup and backup[0].startswith(version + " ")):
            with self._mu:
                dest = self._cost_backup(f"cost-said-{version}")
                self._conn.execute("INSERT OR REPLACE INTO meta (k, v) VALUES ('cost_said_backup', ?)",
                                   (f"{version} {dest}",))
                self._conn.commit()
            report["backup"] = dest
        by_path: dict[tuple[str, str], list[tuple[str, str, str, int | None, int | None]]] = {}
        for tid, agent, path, cli, prompt, offset, size in tasks:
            by_path.setdefault((path or "", cli or ""), []).append((tid, agent, prompt or "", offset, size))
        want: dict[str, str | None] = {}
        for (path, cli), mine in by_path.items():
            found = cost.recheck(path, cli, [t[0] for t in mine]) if path else None
            for tid, agent, prompt, offset, size in mine:
                got = (found or {}).get(tid)
                if got is None:
                    report["unread"] += 1
                    if cost.said(prompt) is not None:
                        want[tid] = prompt
                        continue
                    got = {"before": None, "prompt": None}
                if got["prompt"] is None:
                    moved, dropped = self._cost_unmake(tid, agent, got["before"])
                    report["unmade"] += 1
                    report["moved"] += moved
                    report["dropped"] += dropped
                    want[tid] = None
                    continue
                want[tid] = got["prompt"]
                if got["prompt"] != prompt or offset is None or size != got["size"]:
                    with self._mu:
                        self._conn.execute("UPDATE cost_tasks SET prompt = ?, offset = COALESCE(offset, ?), size = ?"
                                           " WHERE id = ?", (got["prompt"], got["offset"], got["size"], tid))
                        self._conn.commit()
                    report["retitled"] += got["prompt"] != prompt
                    report["placed"] += offset is None
        with self._mu:
            kept = dict(self._conn.execute("SELECT id, prompt FROM cost_tasks"))
            wrong = [t for t, p in want.items() if (p is None) != (t not in kept) or (p is not None and kept[t] != p)]
            herds = [t for t, p in kept.items() if t in want and cost.said(p or "") is None]
            same = self._cost_census() == commits
            ok = not wrong and not herds and same
            report.update(ok=ok, wrong=wrong[:20], herds=herds[:20], commits=same)
            self._conn.execute("INSERT OR REPLACE INTO meta (k, v) VALUES ('cost_said', ?)",
                               (version if ok else "failed " + version,))
            self._conn.execute("INSERT OR REPLACE INTO meta (k, v) VALUES ('cost_said_report', ?)",
                               (json.dumps(report),))
            self._conn.commit()
        self._said_ok = True
        self._cost_seen = None
        self._offsets.clear()
        if tasks:
            print(f"[ghostherd-memory] cost: tasks read again (rules {version}): {json.dumps(report)}", flush=True)

    def _cost_backup(self, tag: str) -> str:
        """The store copied whole -- one file, by SQLite's backup -- beside it
        in backups/, named for TAG and the time: where.  Caller holds `_mu'."""
        dest = self.path.parent / "backups" / f"herd-{tag}-{datetime.now():%Y%m%d-%H%M%S}.sqlite"
        dest.parent.mkdir(parents=True, exist_ok=True)
        out = sqlite3.connect(dest)
        try:
            self._conn.backup(out)
            out.execute("PRAGMA journal_mode=DELETE")   # one file, whole
        finally:
            out.close()
        return str(dest)

    def _cost_recopy(self) -> None:
        """The commits kept as copies of another (`of') by the rule before
        cost.COPY_VERSION, put right: a rebase's copy was the first commit's
        with its author and author date, so two made in one second gave
        both copies to the first -- which then took the other's lines, the
        other never merged (cost.recopied).  Once: backed up first, when
        there is one to put right; all in one commit, checked -- none left,
        nothing else changed -- before it is kept and marked (meta
        `cost_copies'); one that fails is undone and marked so.  Caller
        holds `_tally_mu'."""
        if self._copies_ok:
            return
        version = str(cost.COPY_VERSION)
        cols = "sha, author, subject, of, at"
        report: dict[str, Any] = {"version": cost.COPY_VERSION}
        with self._mu:
            row = self._conn.execute("SELECT v FROM meta WHERE k = 'cost_copies'").fetchone()
            if row and row[0] in (version, "failed " + version):
                self._copies_ok = True
                return
            commits = self._conn.execute(f"SELECT {cols} FROM cost_commits").fetchall()
            wrong = cost.recopied(commits)
            report.update(copies=sum(1 for c in commits if c[3]), fixed=len(wrong))
            ok = True
            if wrong:
                before = self._cost_census()
                report["backup"] = self._cost_backup(f"cost-copies-{version}")
                was = {c[0]: c[3] for c in commits}
                self._conn.executemany("UPDATE cost_commits SET of = ? WHERE sha = ?",
                                       [(to, sha) for sha, to in wrong.items()])
                now = self._conn.execute(f"SELECT {cols} FROM cost_commits").fetchall()
                left = cost.recopied(now)
                # Those copies, and nothing else: every other row as it was.
                ok = (not left and self._cost_census() == before
                      and {c[0]: c[3] for c in now} == dict(was, **wrong))
                report.update(ok=ok, left=sorted(left)[:20],
                              moved=[[sha[:12], (was.get(sha) or "")[:12], to[:12]] for sha, to in sorted(wrong.items())][:50])
                if not ok:
                    self._conn.rollback()
            self._conn.execute("INSERT OR REPLACE INTO meta (k, v) VALUES ('cost_copies', ?)",
                               (version if ok else "failed " + version,))
            self._conn.execute("INSERT OR REPLACE INTO meta (k, v) VALUES ('cost_copies_report', ?)",
                               (json.dumps(report),))
            self._conn.commit()
        self._copies_ok = True
        self._cost_seen = None
        if wrong:
            print(f"[ghostherd-memory] cost: copies put right (rule {version}): {json.dumps(report)}", flush=True)

    def _cost_census(self) -> tuple[Any, ...]:
        "The commits kept, to tell they are as they were.  Caller holds `_mu'."
        return self._conn.execute("SELECT COUNT(*), TOTAL(lines), TOTAL(merged), group_concat(directory || sha, ',')"
                                  " FROM (SELECT * FROM cost_commits ORDER BY directory, sha)").fetchone()

    def _cost_unmake(self, tid: str, agent: str, before: str | None) -> tuple[int, int]:
        """Task TID, of AGENT, is none: its words were the herd's.  What it
        cost goes to BEFORE, the work its agent was on as they came -- or,
        on nothing, is counted to none -- and so do the asks and sub-agents
        it was said to have set off, and the agent's switch to it.  In one
        commit.  The weighted tokens moved and dropped."""
        item = "task:" + tid
        with self._mu:
            cols = ("agent", "sub") + COST_KINDS + ("first_at", "last_at")
            rows = self._conn.execute(f"SELECT {', '.join(cols)} FROM cost_usage WHERE item = ?", (item,)).fetchall()
            weighted = sum(tally.Usage(**dict(zip(COST_KINDS, r[2:2 + len(COST_KINDS)]))).weighted() for r in rows)
            if before:
                for r in rows:
                    self._cost_add((before,) + tuple(r))
                self._conn.execute("UPDATE cost_links SET item = ? WHERE item = ?", (before, item))
            else:
                self._conn.execute("DELETE FROM cost_links WHERE item = ?", (item,))
            self._conn.execute("DELETE FROM cost_usage WHERE item = ?", (item,))
            self._conn.execute("UPDATE cost_switches SET item = ? WHERE item = ?", (before, item))
            self._conn.execute("UPDATE cost_files SET item = ? WHERE item = ?", (before, item))
            self._conn.execute("DELETE FROM cost_tasks WHERE id = ?", (tid,))
            self._conn.commit()
        return (weighted, 0) if before else (0, weighted)

    def _cost_on(self, agent: str, at: float | None, fresh: list[tuple[str, float, str | None]]) -> str | None:
        "What AGENT was on AT: from its switches this pass (FRESH) or kept."
        mine = sorted((w, it) for a, w, it in fresh if a == agent)
        with self._mu:
            kept = self._conn.execute("SELECT at, item FROM cost_switches WHERE agent = ? AND at <= ?"
                                      " ORDER BY at DESC LIMIT 1", (agent, at or 0)).fetchall()
        return cost.made_for(sorted(kept + mine), at)

    def _cost_asks(self) -> None:
        """Which work each ask the herd keeps was made for: the ask its asker
        was answering (`parent'), else what its asker was on as it made it
        -- once its asker's conversation has been read past that.  Kept
        (`ask:ID'), so it outlives the ask's row.  Caller holds `_mu'."""
        since = (datetime.now(timezone.utc) - timedelta(days=COST_DAYS)).isoformat()
        for ask_id, asker, created, parent in self._conn.execute(
                "SELECT id, from_name, created_at, parent FROM asks WHERE created_at >= ?"
                " AND 'ask:' || id NOT IN (SELECT key FROM cost_links)", (since,)).fetchall():
            item = "ask:" + parent if parent else None
            at = tally.epoch(created)
            if item is None and at is not None:
                read = self._conn.execute("SELECT MAX(at) FROM cost_switches WHERE agent = ?", (asker,)).fetchone()[0]
                last = self._conn.execute("SELECT MAX(last_at) FROM cost_usage WHERE agent = ?", (asker,)).fetchone()[0]
                if max(read or 0, last or 0) < at:
                    continue   # its asker not read that far yet
                row = self._conn.execute("SELECT item FROM cost_switches WHERE agent = ? AND at <= ?"
                                         " ORDER BY at DESC LIMIT 1", (asker, at)).fetchone()
                item = row[0] if row else None
            if item:
                self._conn.execute("INSERT OR REPLACE INTO cost_links (key, item) VALUES (?, ?)",
                                   ("ask:" + ask_id, item))

    def _cost_capture(self, directories: list[str] | None = None, force: bool = False) -> None:
        """Keep the commits made in each checkout the herd works in, read
        from its HEAD's reflog (cost.reflog): every live agent's, and those
        the work of the last COST_DAYS was done in that are still there --
        or DIRECTORIES -- when its reflog changed since the last look, or
        FORCE.  One whose reflog is as it was has been read up to now all
        the same: kept so (`cost_reflogs') once an OUTPUT_EVERY.  A
        worktree's are kept when it goes, its reflog with it: a commit is
        read within a pass of its making, and a stop looks first
        (`enqueue_command').  Under `_capture_mu'."""
        now, mono = time.time(), time.monotonic()
        if directories is None:
            cutoff = datetime.fromtimestamp(now - COST_DAYS * 86400, timezone.utc).isoformat()
            with self._mu:
                directories = [r[0] for r in self._conn.execute("SELECT directory FROM snapshot")]
                directories += [r[0] for r in self._conn.execute(
                    "SELECT DISTINCT directory FROM cost_files WHERE read_at >= ?", (cutoff,))]
                directories += [r[0] for r in self._conn.execute(
                    "SELECT DISTINCT directory FROM cost_tasks WHERE at >= ?", (now - COST_DAYS * 86400,))]
        with self._capture_mu:
            known: dict[str, dict[str, Any]] | None = None
            for d in sorted({cost.place(d) for d in directories if d}):
                st = self._reflogs.setdefault(d, {})
                if not force and mono - st.get("missed", -REFLOG_MISS) < REFLOG_MISS:
                    continue
                if "file" not in st:
                    st["file"] = cost.reflog_file(d) if os.path.isdir(d) else None
                    st["repo"] = cost.main_checkout(d) if st["file"] else None
                if st["file"] is None:
                    self._reflogs[d] = {"missed": mono}
                    continue
                try:
                    stat = st["file"].stat()
                    stamp = (stat.st_size, stat.st_mtime_ns)
                except OSError:
                    stamp = None   # none yet, or kept otherwise (reftable): read now and then
                same = stamp == st.get("stamp") if stamp else mono - st.get("read", -OUTPUT_EVERY) < OUTPUT_EVERY
                if same and not force:
                    if mono - st.get("kept", 0) >= OUTPUT_EVERY:
                        st["kept"] = mono
                        with self._mu:
                            self._cost_covered(d, None, now)
                            self._conn.commit()
                    continue
                entries = cost.reflog(d)
                if entries is None:
                    self._reflogs[d] = {"missed": mono}
                    continue
                if known is None:
                    with self._mu:
                        known = {r[0]: {"author": r[1], "subject": r[2] or "", "of": r[3]} for r in self._conn.execute(
                            "SELECT sha, author, subject, of FROM cost_commits WHERE at >= ? ORDER BY at, sha",
                            (now - 2 * COST_DAYS * 86400,))}
                new = cost.new_commits(entries, known, now - COST_DAYS * 86400)
                lines: dict[str, int] = {}
                for i in range(0, len(new), 200):
                    lines.update(cost.numstat(d, [c["sha"] for c in new[i:i + 200]]))
                with self._mu:
                    self._conn.executemany(
                        "INSERT OR IGNORE INTO cost_commits (directory, sha, at, author, subject, lines, of)"
                        " VALUES (?, ?, ?, ?, ?, ?, ?)",
                        [(d, c["sha"], c["at"], c["author"], c["subject"], lines.get(c["sha"], 0), c["of"])
                         for c in new])
                    self._cost_covered(d, min((e["at"] for e in entries), default=now), now, st.get("repo"))
                    self._conn.commit()
                known.update({c["sha"]: c for c in new})
                st.update(stamp=stamp, read=mono, kept=mono)
                if new:
                    self._cost_seen = None

    def _cost_covered(self, d: str, oldest: float | None, now: float, repo: str | None = None) -> None:
        """Checkout D's reflog read at NOW, holding what was done there since
        OLDEST -- or, OLDEST None, as it was when last read.  Caller holds
        `_mu' and commits."""
        row = self._conn.execute("SELECT covered, read_at FROM cost_reflogs WHERE directory = ?", (d,)).fetchone()
        if oldest is None and not row:
            return
        spans = cost.cover(json.loads(row[0]) if row else [], row[1] if oldest is None else oldest, now)
        self._conn.execute(
            "INSERT INTO cost_reflogs (directory, repo, covered, read_at) VALUES (?, ?, ?, ?)"
            " ON CONFLICT (directory) DO UPDATE SET repo = COALESCE(excluded.repo, repo),"
            " covered = excluded.covered, read_at = excluded.read_at",
            (d, repo, json.dumps(spans), now))

    def _cost_output(self, force: bool = False) -> None:
        """What the work made, beyond the reflogs (`_cost_capture'): whether
        each commit kept is in its project's main branch yet -- asked again
        until it is, less often as it ages -- and, for an ask whose taker's
        checkout was never read (gone before it was), the commits in its
        project's main branch that name it.  Once a minute, a few at a
        time, on the tally's worker: never in a page's way.  Caller holds
        `_tally_mu'."""
        if not force and time.monotonic() - self._output_at < OUTPUT_EVERY:
            return
        self._output_at = time.monotonic()
        now = time.time()
        since = now - COST_DAYS * 86400
        with self._mu:
            unmerged = self._conn.execute(
                "SELECT c.directory, c.sha, c.at, r.repo FROM cost_commits c"
                " LEFT JOIN cost_reflogs r ON r.directory = c.directory"
                " WHERE c.merged = 0 AND c.at >= ?"
                " AND (c.checked_at IS NULL OR ? - c.checked_at >= MAX(?, (? - c.at) / 24))",
                (since, now, OUTPUT_EVERY, now)).fetchall()
            had = {r[0]: (bool(r[1]), r[2], json.loads(r[3])) for r in self._conn.execute(
                "SELECT work, named, checked_at, commits FROM cost_output")}
        by_repo: dict[str, list[tuple[str, str, float]]] = {}
        for d, sha, at, repo in unmerged:
            where = cost.checkout(repo) or cost.checkout(d)
            if where:
                by_repo.setdefault(where, []).append((d, sha, at))
        merged, checked = [], []
        for where, cs in list(by_repo.items())[:OUTPUT_MAX]:
            found = cost.in_main(where, [c[1] for c in cs], min(c[2] for c in cs) - 86400)
            checked += [(now, d, sha) for d, sha, _ in cs]
            merged += [(d, sha) for d, sha, _ in cs if found and sha in found]
        # An ask's whose taker's checkout was never read: the commits that name it.
        named = []
        budget = OUTPUT_MAX
        for row in self._cost_view()["tasks"].values():
            if row["mode"] == "direct" or budget <= 0:
                continue
            for work in row["unread"]:
                old = had.get(work)
                if budget <= 0 or (old and (not old[0] or old[2] or now - old[1] < NAMED_AGAIN)):
                    continue
                main = cost.checkout(row["project"])
                if main:
                    budget -= 1
                    named.append((work, main, json.dumps(cost.naming(main, work[4:])), now))
        if not (checked or named):
            return
        with self._mu:
            self._conn.executemany("UPDATE cost_commits SET checked_at = ? WHERE directory = ? AND sha = ?", checked)
            self._conn.executemany("UPDATE cost_commits SET merged = 1 WHERE directory = ? AND sha = ?", merged)
            self._conn.executemany(
                "INSERT OR REPLACE INTO cost_output (work, repo, commits, checked_at, named) VALUES (?, ?, ?, ?, 1)",
                named)
            self._conn.commit()
        if merged or named:
            self._cost_seen = None

    def _cost_view(self) -> dict[str, Any]:
        """Every task of the last COST_DAYS, costed (cost.costs) with the
        commits each piece of work made (cost.attribute), which work each
        ask was made for, and what each agent is on: worked out again
        COST_KEEP seconds after the last time, or once a pass has read
        more.  Through the read-only connection: never in an ask's way."""
        seen = self._cost_seen
        if seen and time.monotonic() - seen[0] < COST_KEEP:
            return seen[1]
        now = time.time()
        since = datetime.combine(date.today() - timedelta(days=COST_DAYS - 1), datetime.min.time()).timestamp()
        tcols = ("id", "agent", "project", "role", "mode", "cli", "prompt", "at", "size")
        ucols = ("item", "agent", "sub") + tally.Usage.__slots__ + ("first_at", "last_at")
        ccols = ("directory", "sha", "at", "author", "subject", "lines", "of", "merged")
        with self._read_mu:
            conn = self._read_conn()
            tasks = [dict(zip(tcols, r)) for r in conn.execute(
                f"SELECT {', '.join(tcols)} FROM cost_tasks WHERE at >= ?", (since,))]
            links = conn.execute("SELECT key, item FROM cost_links WHERE key LIKE 'ask:%' OR key LIKE 'said:%'"
                                 " ORDER BY key DESC").fetchall()
            # The herd's own record of an ask wins over what its client said.
            asked = {k.split(":", 1)[1]: v for k, v in links}
            usage = [dict(zip(ucols, r)) for r in conn.execute(
                f"SELECT {', '.join(ucols)} FROM cost_usage WHERE last_at >= ?", (since,))]
            output = {w: {"commits": json.loads(c), "named": bool(n)} for w, c, n in conn.execute(
                "SELECT work, commits, named FROM cost_output")}
            commits: dict[str, list[dict[str, Any]]] = {}
            for r in conn.execute(f"SELECT {', '.join(ccols)} FROM cost_commits WHERE at >= ? ORDER BY at",
                                  (since - 86400,)):
                c = dict(zip(ccols, r))
                c["merged"] = bool(c["merged"])
                commits.setdefault(c["directory"], []).append(c)
            covered = {d: json.loads(c) for d, c in conn.execute("SELECT directory, covered FROM cost_reflogs")}
            homes: dict[str, list[str]] = {}
            for agent, d in conn.execute("SELECT agent, directory FROM cost_files WHERE directory != ''"
                                         " UNION SELECT name, directory FROM snapshot WHERE directory != ''"):
                if cost.place(d) not in homes.setdefault(agent, []):
                    homes[agent].append(cost.place(d))
            # An ask whose taker's replies were never read: its time on it.
            asks = conn.execute("SELECT id, to_name, delivered_at, answered_at FROM asks"
                                " WHERE delivered_at IS NOT NULL AND to_name != ''").fetchall()
            on = dict(conn.execute("SELECT agent, item FROM cost_files WHERE item IS NOT NULL"
                                   " AND path NOT LIKE '%/subagents/%' ORDER BY read_at"))
            # A piece of work begins as it is set: the user's words, an ask delivered.
            began = {(r[1], r[0]): r[2] for r in conn.execute(
                "SELECT agent, item, MIN(at) FROM cost_switches WHERE at >= ? GROUP BY agent, item", (since,))}
        spans: dict[tuple[str, str], list[float]] = {}
        for r in usage:
            if r["first_at"] is None or r["item"].split(":", 1)[0] not in ("task", "ask", "back"):
                continue
            key = (r["item"], r["agent"])
            got = spans.setdefault(key, [min(r["first_at"], began.get(key, r["first_at"])), r["last_at"]])
            got[0], got[1] = min(got[0], r["first_at"]), max(got[1], r["last_at"])
        for ask, taker, delivered, answered in asks:
            if not any(k[0] == "ask:" + ask for k in spans):
                start = tally.epoch(delivered)
                if start is not None:
                    spans[("ask:" + ask, taker)] = [start, tally.epoch(answered) or now]
        made = cost.attribute(commits, [(k[0], k[1], v[0], v[1]) for k, v in spans.items()], homes, covered, now)
        view = {"tasks": cost.costs(tasks, usage, asked, output, now, made), "asked": asked, "on": on}
        self._cost_seen = (time.monotonic(), view)
        return view

    def costs(self, days: int = 7, project: str | None = None, tasks: bool = False,
              own: str | None = None) -> dict[str, Any]:
        """Lead mode against direct mode over the last DAYS local days
        (cost.compare) -- of PROJECT's tasks when given -- and, with TASKS,
        each task, newest first.  OWN, for an agent's call: the project the
        herd knows it in, whose tasks -- and the user's words that began
        them -- are the only ones it is shown; the numbers are every
        project's.  "" for a caller the herd does not know: numbers only.
        None for Emacs and the page: everything."""
        days = max(1, min(COST_DAYS, int(days)))
        rows = list(self._cost_view()["tasks"].values())
        if project:
            rows = [r for r in rows if projects.same(r["project"], project)
                    or projects.leaf(r["project"]) == projects.leaf(project)]
        out: dict[str, Any] = {"compare": cost.compare(rows, days)}
        if tasks:
            since = out["compare"]["since"]
            listed = sorted((r for r in rows if r["at"] and date.fromtimestamp(r["at"]).isoformat() >= since),
                            key=lambda r: -(r["at"] or 0))
            if own is not None:
                mine = [r for r in listed if own and projects.same(r["project"], own)]
                if len(mine) < len(listed):
                    out["withheld"] = len(listed) - len(mine)
                listed = mine
            out["tasks"] = listed
        return out

    def _thread_cost(self, root: str, view: dict[str, Any], whole: bool = False) -> dict[str, Any] | None:
        """What the task thread ROOT (an ask, or `task:ID') is part of cost,
        as the Threads tab shows it; None when not known.  WHOLE, for the
        open thread: its commits too, and how many threads the task is.
        Its agent, prompt and time (`at') too: the page opens a thread with
        them when the herd log has only the note before the user's words
        (`ghostherd-threads--note-opener').  The page's alone: herd_threads
        and herd_thread are not an agent's to call."""
        task = root[5:] if root.startswith("task:") else cost.task_of("ask:" + root, view["asked"])
        row = view["tasks"].get(task) if task else None
        if not row:
            return None
        keys = ("id", "agent", "mode", "prompt", "context", "at", "total", "own", "workers", "subs", "asks",
                "commits", "lines", "merged", "mergedLines", "code")
        out = {k: row[k] for k in keys}
        if whole:
            out["made"] = row["made"]
            out["threadCount"] = self._task_threads(task, row, view)
        return out

    def _task_threads(self, task: str, row: dict[str, Any], view: dict[str, Any]) -> int:
        """How many threads TASK is: its asks that no ask of its was being
        answered by when made -- each a root -- and a direct task's own."""
        asks = set(cost.asks_in(task, view["asked"]))
        with self._read_mu:
            marks = ",".join("?" * len(asks))
            parents = dict(self._read_conn().execute(
                f"SELECT id, parent FROM asks WHERE id IN ({marks})", tuple(asks)).fetchall()) if asks else {}
        return sum(1 for a in asks if parents.get(a) not in asks) + (row["mode"] == "direct")

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
            "decided_at": row[10], "kind": row[11] or "promote",
            "rule": json.loads(row[12]) if row[12] else None,
        }

    _PROPOSAL_COLS = ("id, project, facts, file, diff, author, status, reason, "
                      "ask_id, created_at, decided_at, kind, rule")

    def propose(self, project: str, facts: list[str], file: str, diff: str,
                author: str, rule: dict[str, Any] | None = None,
                share: bool = False) -> dict[str, Any]:
        """Store a pending proposal to promote FACTS of PROJECT (a store key)
        into instruction FILE, with the exact DIFF.  The herd holds it until
        the user decides on the page; it writes nothing to the repo.  With
        RULE, a check made of the one fact instead (`lint': FILE and DIFF
        are the herd's own, `lint' and the rule as JSON).  With SHARE, the
        one fact moved to the user store, so every project has it (`share':
        FILE `user', DIFF the fact's text as it was proposed; share.py)."""
        project = (project or "").strip()
        names = [str(f).strip() for f in (facts or []) if str(f).strip()]
        kind = "lint" if rule is not None else "share" if share else "promote"
        if rule is not None:
            if len(names) != 1:
                raise ValueError("a check is one fact's")
            file, diff = "lint", json.dumps(rule, sort_keys=True, ensure_ascii=False)
        if share:
            if len(names) != 1:
                raise ValueError("move one fact at a time")
            file = "user"
        file = (file or "").strip()
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
                        "file": file, "kind": kind, "duplicate": True}
            self._conn.execute(
                "INSERT INTO proposals (id, project, facts, file, diff, author, "
                "status, created_at, kind, rule) VALUES (?, ?, ?, ?, ?, ?, 'pending', ?, ?, ?)",
                (pid, project, facts_json, file, diff,
                 (author or "unknown").strip() or "unknown", now, kind,
                 diff if rule is not None else None),
            )
            self._conn.commit()
        return {"id": pid, "project": project, "facts": names, "file": file, "kind": kind}

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

    def apply_lint_proposal(self, proposal_id: str, kind: str = "lint") -> None:
        """The user approved a check (or a move to the user store, KIND
        `share'): the herd has done it, nothing left for anyone to do."""
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            cur = self._conn.execute(
                "UPDATE proposals SET status = 'applied', decided_at = ? "
                "WHERE id = ? AND status = 'pending' AND kind = ?",
                (now, proposal_id, kind),
            )
            self._conn.commit()
            if cur.rowcount == 0:
                raise ValueError("no pending check with that id" if kind == "lint"
                                 else "no pending proposal with that id")

    def move_fact_counts(self, key: str, fact: str, to: str) -> None:
        """Store KEY's FACT is store TO's now (share.py): its uses, their
        lifetime totals and its check's denials go with it, so the page
        counts on from where it was."""
        with self._mu:
            self._flush_uses()
            self._conn.execute("UPDATE fact_uses SET key = ? WHERE key = ? AND fact = ?", (to, key, fact))
            self._conn.execute(
                """
                INSERT INTO fact_use_totals (key, fact, kind, n, first_at, last_at)
                SELECT ?, fact, kind, n, first_at, last_at FROM fact_use_totals
                WHERE key = ? AND fact = ?
                ON CONFLICT (key, fact, kind) DO UPDATE SET
                  n = n + excluded.n,
                  first_at = MIN(first_at, excluded.first_at),
                  last_at = MAX(last_at, excluded.last_at)
                """, (to, key, fact))
            self._conn.execute("DELETE FROM fact_use_totals WHERE key = ? AND fact = ?", (key, fact))
            self._conn.execute("UPDATE lint_denials SET key = ? WHERE key = ? AND fact = ?", (to, key, fact))
            self._conn.commit()

    # ---- The checks' denials ------------------------------------------------
    #
    # A check denies alone (bin/herd_lint.py), before the call; what it
    # denied comes here, after, for the log and each rule's count.

    def lint_denied(self, key: str, fact: str, session: str = "", cli: str = "",
                    command: str = "") -> dict[str, Any]:
        "Record that store KEY's check for FACT denied COMMAND (SESSION's, on CLI)."
        key, fact = (key or "").strip(), (fact or "").strip()
        if not key or not fact:
            raise ValueError("key and fact are required")
        did = uuid.uuid4().hex[:12]
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            self._conn.execute(
                "INSERT INTO lint_denials (id, key, fact, session, cli, command, created_at)"
                " VALUES (?, ?, ?, ?, ?, ?, ?)",
                (did, key, fact, (session or "").strip(), (cli or "").strip(),
                 (command or "")[:2000], now))
            self._conn.commit()
        return {"id": did}

    def lint_counts(self, key: str) -> dict[str, dict[str, Any]]:
        "Per fact of store KEY: how many calls its check denied, and the last."
        with self._mu:
            rows = self._conn.execute(
                "SELECT fact, COUNT(*), MAX(created_at) FROM lint_denials WHERE key = ? GROUP BY fact",
                (key,)).fetchall()
            out = {}
            for fact, n, at in rows:
                last = self._conn.execute(
                    "SELECT session, cli, command FROM lint_denials WHERE key = ? AND fact = ?"
                    " ORDER BY created_at DESC LIMIT 1", (key, fact)).fetchone()
                out[fact] = {"count": n, "last_ms": _ms(at), "last_session": last[0],
                             "last_cli": last[1], "last_command": (last[2] or "")[:400]}
        return out

    def _lint_recent(self) -> list[dict[str, Any]]:
        "The denials of the last RECENT_SECONDS, newest first: the log tells each once."
        cutoff = (datetime.now(timezone.utc) - timedelta(seconds=RECENT_SECONDS)).isoformat()
        return [{"id": r[0], "key": r[1], "fact": r[2], "session": r[3], "cli": r[4],
                 "command": (r[5] or "")[:300], "created_ms": _ms(r[6])}
                for r in self._conn.execute(
                    "SELECT id, key, fact, session, cli, command, created_at FROM lint_denials"
                    " WHERE created_at >= ? ORDER BY created_at DESC LIMIT 50", (cutoff,)).fetchall()]

    # ---- How often each fact reaches an agent ---------------------------------
    #
    # Counted where the herd hands a fact to an agent, and nowhere else: an
    # ask's memory block as the ask is delivered, a rule given in full in a
    # grok's or agy's opening, and `herd recall' by name or by query.  The
    # page reading a fact, an index listing, and what claude reads from its
    # own memory dir are not seen, so not counted.  Nothing is said of what
    # a use did: there is no ground truth for that.

    def use(self, key: str, facts, kind: str, agent: str = "", ask: str | None = None,
            how: str | None = None, at: str | None = None) -> None:
        """Note that FACTS (names) of store KEY reached AGENT, by KIND
        (USE_KINDS) -- HOW it was asked for, the ASK it rode in; AT now
        unless given.  An append to a bounded queue the tick writes: it never
        waits, and never fails what called it."""
        try:
            at = at or datetime.now(timezone.utc).isoformat()
            for name in facts or []:
                self._uses.append((str(key), str(name), kind, how, at, agent or "", ask))
        except Exception:  # noqa: BLE001 -- a count never costs a use
            pass

    def _used_by_delivery(self, mail_ids: list[str]) -> None:
        """The facts the memory blocks of MAIL_IDS hold, each a use as that
        mail is delivered -- once: only mail still queued.  Caller holds `_mu`."""
        try:
            marks = ",".join("?" * len(mail_ids))
            for to_name, ask_id, facts in self._conn.execute(
                f"SELECT to_name, ask_id, memory_facts FROM mail WHERE id IN ({marks})"
                " AND status = 'queued' AND memory IS NOT NULL AND memory_facts IS NOT NULL",
                mail_ids,
            ).fetchall():
                for key, name in json.loads(facts):
                    self.use(key, [name], "ask", agent=to_name, ask=ask_id)
        except Exception:  # noqa: BLE001
            pass

    def _flush_uses(self) -> None:
        """Write up to USE_FLUSH_MAX of the uses waiting in the queue, and add
        them to their facts' totals.  Caller holds `_mu`; never raises."""
        rows = []
        try:
            while self._uses and len(rows) < USE_FLUSH_MAX:
                rows.append(self._uses.popleft())
        except IndexError:
            pass
        if not rows:
            return
        sums: dict[tuple[str, str, str], list] = {}
        for key, fact, kind, _how, at, _agent, _ask in rows:
            t = sums.setdefault((key, fact, kind), [0, at, at])
            t[0] += 1
            t[1], t[2] = min(t[1], at), max(t[2], at)
        try:
            self._conn.executemany(
                "INSERT INTO fact_uses (key, fact, kind, how, at, agent, ask) VALUES (?, ?, ?, ?, ?, ?, ?)",
                rows)
            self._conn.executemany(
                """
                INSERT INTO fact_use_totals (key, fact, kind, n, first_at, last_at)
                VALUES (?, ?, ?, ?, ?, ?)
                ON CONFLICT (key, fact, kind) DO UPDATE SET
                  n = n + excluded.n,
                  first_at = MIN(first_at, excluded.first_at),
                  last_at = MAX(last_at, excluded.last_at)
                """, [(*k, *v) for k, v in sums.items()])
        except Exception:  # noqa: BLE001 -- lost counts, never a lost tick
            pass

    def _roll_up_uses(self, now_ts: float) -> None:
        """Bound the uses: drop the rows older than USE_DETAIL_DAYS, and the
        oldest past USE_ROWS_MAX -- their facts' totals keep them counted.
        Caller holds `_mu`; never raises."""
        try:
            cut = datetime.fromtimestamp(now_ts - USE_DETAIL_DAYS * 86400, tz=timezone.utc).isoformat()
            over = self._conn.execute(
                "SELECT at FROM fact_uses ORDER BY at DESC LIMIT 1 OFFSET ?", (USE_ROWS_MAX,)).fetchone()
            if over and over[0] >= cut:
                cut = over[0] + "~"      # every row at or before that one
            self._conn.execute("DELETE FROM fact_uses WHERE at < ?", (cut,))
        except Exception:  # noqa: BLE001
            pass

    def fact_usage(self, key: str, days: int = 30) -> dict[str, Any]:
        """The uses of store KEY's facts and the user store's, for the Memory
        tab: {since, days, facts: {"store/name": {ask, recall, opening,
        used, last, last_opening, total}}} -- ask, recall and opening the
        uses of each kind in the last DAYS days; USED the asks and recalls
        among them (an opening gives every rule at every start, so it shows
        apart); LAST the last ask or recall, rolled-up ones too; TOTAL every
        ask and recall since SINCE."""
        stores = [key, "user"]
        cutoff = (datetime.now(timezone.utc) - timedelta(days=days)).isoformat()
        out: dict[str, dict[str, Any]] = {}

        def slot(k, n):
            return out.setdefault(f"{k}/{n}", {"ask": 0, "recall": 0, "opening": 0, "used": 0,
                                                "last": None, "last_opening": None, "total": 0})
        with self._mu:
            self._flush_uses()
            self._conn.commit()
        with self._read_mu:
            db = self._read_conn()
            since = db.execute("SELECT v FROM meta WHERE k = 'uses_since'").fetchone()
            for k, n, kind, count in db.execute(
                "SELECT key, fact, kind, COUNT(*) FROM fact_uses WHERE key IN (?, ?) AND at >= ?"
                " GROUP BY key, fact, kind", (*stores, cutoff)).fetchall():
                if kind in USE_KINDS:
                    slot(k, n)[kind] = count
            for k, n, kind, count, last in db.execute(
                "SELECT key, fact, kind, n, last_at FROM fact_use_totals WHERE key IN (?, ?)", stores).fetchall():
                self._add_use(slot(k, n), kind, count, last)
        for v in out.values():
            v["used"] = v["ask"] + v["recall"]
        return {"since": since[0] if since else None, "days": days, "facts": out}

    def _read_conn(self) -> sqlite3.Connection:
        "The read-only connection the uses are read through.  Caller holds `_read_mu`."
        if self._reader is None:
            self._reader = sqlite3.connect(f"file:{self.path}?mode=ro", uri=True, check_same_thread=False)
            self._reader.execute("PRAGMA busy_timeout = 2000")
        return self._reader

    @staticmethod
    def _add_use(v: dict[str, Any], kind: str, count: int, last: str | None) -> None:
        if kind == "opening":
            v["last_opening"] = max(filter(None, [v["last_opening"], last]), default=None)
        elif kind in ("ask", "recall"):
            v["total"] += count
            v["last"] = max(filter(None, [v["last"], last]), default=None)

    def fact_uses(self, key: str, name: str, limit: int = 50) -> list[dict[str, Any]]:
        "Store KEY's fact NAME's last LIMIT uses, newest first: when, how, by whom, which ask."
        with self._mu:
            self._flush_uses()
            self._conn.commit()
        with self._read_mu:
            rows = self._read_conn().execute(
                "SELECT kind, how, at, agent, ask FROM fact_uses WHERE key = ? AND fact = ?"
                " ORDER BY at DESC LIMIT ?", (key, name, limit)).fetchall()
        return [{"kind": r[0], "how": r[1], "at": r[2], "agent": r[3], "ask": r[4]} for r in rows]

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
                # The user kept this very call denied: tried again, it stays
                # denied, and no new request asks them again.
                cutoff = (datetime.now(timezone.utc) - timedelta(seconds=self.guard_ttl)).isoformat()
                row = self._conn.execute(
                    "SELECT id FROM guard_requests WHERE session = ? AND fp = ? AND status = 'declined'"
                    " AND closed_at > ? ORDER BY closed_at DESC LIMIT 1",
                    (session, fp, cutoff)).fetchone()
                if row:
                    return {"id": row[0], "status": "declined"}
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

    def guard_reject(self, gid: str, root: str | None = None) -> dict[str, Any]:
        """The user keeps refused call GID denied: the request closes, an
        approval not yet used is taken back, and the agent is told -- once --
        not to try it again.  The same call tried again stays denied
        without asking the user again (`guard_denied')."""
        root = os.path.realpath(os.path.expanduser(root or guard_root()))
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            row = self._conn.execute(
                "SELECT session, guard_dir, summary, approvable, fp, status FROM guard_requests WHERE id = ?",
                ((gid or "").strip(),)).fetchone()
            if not row:
                raise ValueError(f"no guard request {gid}")
            session, guard_dir, summary, approvable, fp, status = row
            if status == "declined":   # a second tap
                return {"id": gid, "status": "declined"}
            if status not in ("pending", "approved"):
                raise ValueError(f"guard request {gid} is {status}")
            if status == "approved":
                mine = os.path.join(root, session)
                if guard_dir and os.path.realpath(guard_dir) == os.path.realpath(mine) and os.path.isdir(mine):
                    _guard_lib().take_approval(mine, fp)
            self._conn.execute(
                "UPDATE guard_requests SET status = 'declined', closed_at = ? WHERE id = ?", (now, gid))
            self._conn.execute(
                """
                INSERT INTO mail (id, from_name, to_name, body, submit, handoff, status, created_at)
                VALUES (?, 'herd', ?, ?, 1, 0, 'queued', ?)
                """,
                (str(uuid.uuid4()), session,
                 f"[guard] The user kept this denied: {summary}.  Don't retry it; say what you "
                 f"needed instead (request {gid}).", now),
            )
            self._conn.commit()
        return {"id": gid, "status": "declined"}

    def guard_clear(self, session: str) -> dict[str, Any]:
        """SESSION was respawned or relaunched: its guard is rebuilt, and an
        old call's fingerprint means nothing to the new process -- every
        request still open (waiting, or approved and unused) expires."""
        session = (session or "").strip()
        if not session:
            raise ValueError("session is required")
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            n = self._conn.execute(
                "UPDATE guard_requests SET status = 'expired', closed_at = ?"
                " WHERE session = ? AND status IN ('pending', 'approved')",
                (now, session)).rowcount
            self._conn.commit()
        return {"session": session, "cleared": n}

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
        """Requests the card and the log are told of: waiting or approved, and
        the rest for RECENT_SECONDS after they closed.  An approval unused
        past its time has lapsed; a request waiting past `guard_ttl' expires.
        Caller holds `_mu`."""
        now = datetime.now(timezone.utc).timestamp()
        out = []
        for r in self._conn.execute(
            """
            SELECT id, session, tool, summary, reason, rule, kind, approvable, status,
                   created_at, approved_at, used_at, closed_at
            FROM guard_requests ORDER BY created_at DESC LIMIT 200
            """
        ).fetchall():
            status = r[8]
            if status == "approved" and r[10] and now - datetime.fromisoformat(r[10]).timestamp() > GUARD_APPROVAL_SECONDS:
                status = "lapsed"
                self._conn.execute("UPDATE guard_requests SET status = 'lapsed' WHERE id = ?", (r[0],))
            closed = r[12]
            if status == "pending" and now - datetime.fromisoformat(r[9]).timestamp() > self.guard_ttl:
                status, closed = "expired", datetime.now(timezone.utc).isoformat()
                self._conn.execute(
                    "UPDATE guard_requests SET status = 'expired', closed_at = ? WHERE id = ?", (closed, r[0]))
            last = max(x for x in (r[9], r[10], r[11], closed) if x)
            if status not in ("pending", "approved") and now - datetime.fromisoformat(last).timestamp() > RECENT_SECONDS:
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
            closed, later = None, False
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
                    later = self._trace_inline(open_ask[0])
                    self._mail_back(open_ask[0], hold=later)
                    closed = open_ask[0]
            self._conn.commit()
        if closed and later:
            self._trace_soon(closed)
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
                       last_at, updated_at, turn_at
                FROM links WHERE session = ?
                """,
                ((session or "").strip(),),
            ).fetchone()
        return self._link_dict(row, full=True) if row else None

    def writer(self, name: str | None = None,
               transcript: str | None = None) -> tuple[str, str] | None:
        """Who wrote a message, as a copy of what it names keeps it
        (images.py): the agent NAME, or the one whose conversation is at
        TRANSCRIPT, and its project as Emacs last said it ("" once it has
        left the herd).  None when neither names one."""
        name = (name or "").strip()
        with self._mu:
            if not name and transcript:
                row = self._conn.execute(
                    "SELECT session FROM links WHERE transcript = ? ORDER BY updated_at DESC LIMIT 1",
                    (transcript,)).fetchone()
                name = row[0] if row else ""
            if not name:
                return None
            row = self._conn.execute(
                "SELECT project FROM snapshot WHERE name = ?", (name,)).fetchone()
        return name, (row[0] if row and row[0] else "")

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
        # When its hooks last said a turn began: a `working' and the `done'
        # after it can both land between two ticks, and a tick carries each
        # agent's newest report only -- Emacs then tells a turn its prompt
        # began from this (ask cf9873be).
        out["turn_ms"] = _ms(r[7])
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
        restored: bool | None = None,
        guard_ttl: int | float | None = None,
    ) -> dict[str, Any]:
        """PAGE_URL is the herd page's address from your phone, without its
        secret, as Emacs says it each tick; none, it is not linked.

        RESTORED is whether this Emacs has restored its herd yet.  A fresh
        Emacs, before it has, ticks an empty or partial snapshot that is a
        herd not yet picked up -- not agents that died -- so an open ask for
        a taker missing from it is held, not failed.  None (an Emacs too old
        to say) is read as restored, keeping the old behaviour.
        GUARD_TTL is how long a guard request waits before it expires."""
        restored_flag = True if restored is None else bool(restored)
        if isinstance(guard_ttl, (int, float)) and not isinstance(guard_ttl, bool) and guard_ttl > 0:
            self.guard_ttl = guard_ttl
        sessions = sessions or []
        pushes = [p for p in (pushes or []) if isinstance(p, dict) and p.get("kind") and p.get("session")]
        ack_ids = [i for i in (ack_ids or []) if i]
        ack_cmd_ids = [i for i in (ack_cmd_ids or []) if i]
        replies = replies or []
        now = datetime.now(timezone.utc).isoformat()
        with self._mu:
            if ack_ids:
                placeholders = ",".join("?" * len(ack_ids))
                self._used_by_delivery(ack_ids)
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
                       directory, branch, started_by, ahead, merged, host_pid, guarded,
                       directory_missing)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
                        1 if row.get("directoryMissing") is True else 0,
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
            # Remember whether Emacs has restored, for `settle' (a separate
            # call) to read: it must not fail a taker's ask while the herd
            # is not yet picked up.
            self._conn.execute(
                "INSERT OR REPLACE INTO meta (k, v) VALUES ('restored', ?)",
                ("1" if restored_flag else "0",),
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
                # A taker missing from the snapshot of an Emacs that has not
                # restored yet is a herd not yet picked up, not a gone agent:
                # hold its queued ask rather than fail it.
                if to_name not in names and restored_flag:
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
            # Not into a taker at work, nor one still starting, nor one on a
            # menu: a trust prompt or a permission prompt takes no paste,
            # and reads the Return pressed after it as its answer -- grok's
            # folder-trust prompt took it as "Yes, proceed" and the ask was
            # lost (c644aebc); claude's, its cursor on "No, exit", quits.
            # It waits for the user to answer the menu.
            hold = {"working", "starting", "blocked"}
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
            lint = self._lint_recent()
            self._flush_uses()
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
                           last_at, updated_at, turn_at
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
        # What the replies since cost: read on its worker, not here.
        self._tally_soon()
        return {
            "pending": pending,
            "commands": commands,
            "asks": asks,
            "reports": reports,
            "links": links,
            "guard": guard,
            "lint": lint,
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
        self._roll_up_uses(now_ts)
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
        if op == "stop":
            # Its checkout's commits kept first: Emacs removes a merged
            # worktree as it stops it, its reflog with it.
            with self._mu:
                row = self._conn.execute("SELECT directory FROM snapshot WHERE name = ?", (session,)).fetchone()
            if row and row[0]:
                try:
                    self._cost_capture([row[0]], force=True)
                except Exception:  # noqa: BLE001 -- the stop goes ahead; the last pass read most of it
                    pass
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
