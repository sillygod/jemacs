"""JSON-RPC 2.0 dispatcher.  Same contract as ecloud: one POST /jsonrpc."""

from __future__ import annotations

import asyncio
import inspect
import traceback
from typing import Any, Callable

from pydantic import BaseModel

import conversation
import links
import meter
import relay
import shared_memory
import subagents
from engine import get_engine
from herd import ASK_CLOSED, get_herd

PARSE_ERROR = -32700
INVALID_REQUEST = -32600
METHOD_NOT_FOUND = -32601
INVALID_PARAMS = -32602
INTERNAL_ERROR = -32603

AWAIT_MAX = 55.0
AWAIT_POLL = 0.5


class JsonRpcRequest(BaseModel):
    jsonrpc: str = "2.0"
    id: int | str | None = None
    method: str
    params: dict[str, Any] = {}


class JsonRpcError(BaseModel):
    code: int
    message: str
    data: Any = None


class JsonRpcResponse(BaseModel):
    jsonrpc: str = "2.0"
    id: int | str | None = None
    result: Any = None
    error: JsonRpcError | None = None


class JsonRpcHandler:
    def __init__(self):
        self._methods: dict[str, Callable] = {
            "ping": self._ping,
            "memory_status": self._status,
            "memory_import": self._import,
            "memory_search": self._search,
            "memory_list": self._list,
            "memory_chunks": self._chunks,
            "herd_list": self._herd_list,
            "herd_message": self._herd_message,
            "herd_inbox": self._herd_inbox,
            "herd_tick": self._herd_tick,
            "herd_ask": self._herd_ask,
            "herd_reply": self._herd_reply,
            "herd_await": self._herd_await,
            "herd_cancel": self._herd_cancel,
            "herd_detach": self._herd_detach,
            "herd_settle": self._herd_settle,
            "herd_asks": self._herd_asks,
            "herd_report": self._herd_report,
            "herd_link": self._herd_link,
            "herd_conversation": self._herd_conversation,
            "herd_sub_conversation": self._herd_sub_conversation,
            "herd_chat": self._herd_chat,
            "herd_meter": self._herd_meter,
            "herd_subagents": self._herd_subagents,
            "herd_edits": self._herd_edits,
            "herd_links": self._herd_links,
            "herd_relay_brief": self._herd_relay_brief,
            "herd_relay": self._herd_relay,
            "herd_screens": self._herd_screens,
            "herd_start": self._herd_start,
            "herd_stop": self._herd_stop,
            "herd_command_reply": self._herd_command_reply,
            "herd_unlink": self._herd_unlink,
            "herd_remember": self._herd_remember,
            "herd_recall": self._herd_recall,
            "herd_forget": self._herd_forget,
        }

    async def handle(self, request: JsonRpcRequest) -> JsonRpcResponse:
        method = self._methods.get(request.method)
        if method is None:
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(
                    code=METHOD_NOT_FOUND,
                    message=f"Method not found: {request.method}",
                ),
            )
        try:
            if inspect.iscoroutinefunction(method):
                result = await method(request.params or {})
            else:
                result = await asyncio.to_thread(method, request.params or {})
            return JsonRpcResponse(id=request.id, result=result)
        except ValueError as exc:
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(code=INVALID_PARAMS, message=str(exc)),
            )
        except Exception as exc:  # noqa: BLE001 — JSON-RPC must always answer
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(
                    code=INTERNAL_ERROR,
                    message=str(exc),
                    data={"trace": traceback.format_exc()[-2000:]},
                ),
            )

    def _ping(self, _params: dict) -> dict:
        return {"ok": True, "service": "ghostherd-memory"}

    def _status(self, _params: dict) -> dict:
        return get_engine().status()

    def _import(self, params: dict) -> dict:
        agents = params.get("agents")
        if isinstance(agents, str):
            agents = [agents]
        return get_engine().import_transcripts(
            agents=agents,
            force=bool(params.get("force")),
            project=params.get("project") or None,
        )

    def _search(self, params: dict) -> dict:
        query = params.get("query") or params.get("q") or ""
        if not str(query).strip():
            raise ValueError("query is required")
        limit = params.get("limit", 8)
        try:
            limit = int(limit)
        except (TypeError, ValueError) as exc:
            raise ValueError("limit must be an integer") from exc
        return get_engine().search(
            query=str(query),
            limit=limit,
            agent=params.get("agent") or None,
            project=params.get("project") or None,
        )

    def _list(self, params: dict) -> dict:
        return get_engine().list_sources(
            agent=params.get("agent") or None,
            project=params.get("project") or None,
            limit=int(params.get("limit") or 200),
            offset=int(params.get("offset") or 0),
        )

    def _chunks(self, params: dict) -> dict:
        source = params.get("source_path") or params.get("source") or None
        session = params.get("session_id") or params.get("session") or None
        if not source and not session:
            raise ValueError("source_path or session_id is required")
        return get_engine().list_chunks(
            source_path=source,
            session_id=session,
            limit=int(params.get("limit") or 400),
            offset=int(params.get("offset") or 0),
        )

    def _herd_list(self, params: dict) -> dict:
        project = params.get("project") or None
        return get_herd().list_sessions(project=project)

    def _herd_message(self, params: dict) -> dict:
        to = params.get("to")
        if params.get("body") is None:
            raise ValueError("body is required")
        submit = params.get("submit", True)
        return get_herd().enqueue(
            to=str(to or ""),
            body=str(params.get("body")),
            from_name=params.get("from"),
            submit=bool(submit) if submit is not None else True,
            handoff=bool(params.get("handoff")),
        )

    def _herd_inbox(self, params: dict) -> dict:
        session = params.get("session") or params.get("name") or ""
        return get_herd().inbox(
            session=str(session),
            limit=params.get("limit", 50),
        )

    def _herd_tick(self, params: dict) -> dict:
        sessions = params.get("sessions") or []
        if not isinstance(sessions, list):
            raise ValueError("sessions must be a list")
        ack_ids = params.get("ack_ids") or []
        if not isinstance(ack_ids, list):
            raise ValueError("ack_ids must be a list")
        replies = params.get("replies") or []
        if not isinstance(replies, list):
            raise ValueError("replies must be a list")
        pushes = params.get("pushes") or []
        if not isinstance(pushes, list):
            raise ValueError("pushes must be a list")
        return get_herd().tick(
            sessions=sessions,
            ack_ids=[str(i) for i in ack_ids],
            replies=replies,
            pushes=pushes,
            page_url=params.get("page_url"),
        )

    def _herd_ask(self, params: dict) -> dict:
        return get_herd().ask(
            to=str(params.get("to") or ""),
            body=params.get("body"),
            from_name=params.get("from"),
            wait=bool(params.get("wait")),
            rounds=params.get("rounds"),
        )

    def _herd_reply(self, params: dict) -> dict:
        return get_herd().reply(
            str(params.get("id") or ""),
            params.get("body"),
            from_name=params.get("from"),
        )

    async def _herd_await(self, params: dict) -> dict:
        """The ask once it closes, or as it stands after TIMEOUT seconds.

        Async on purpose: a sync method holds one of the few worker
        threads for the whole wait, and a few waiting askers would
        stall memory search behind them.  Capped, so a client loops
        rather than hold one request open for an hour.
        """
        ask_id = str(params.get("id") or "")
        who = str(params.get("from") or "").strip()
        try:
            timeout = float(params.get("timeout", 30))
        except (TypeError, ValueError) as exc:
            raise ValueError("timeout must be a number") from exc
        timeout = max(0.0, min(timeout, AWAIT_MAX))
        loop = asyncio.get_running_loop()
        deadline = loop.time() + timeout
        herd = get_herd()
        while True:
            ask = await asyncio.to_thread(herd.get_ask, ask_id)
            if ask["status"] in ASK_CLOSED:
                # Collected by the asker, or by a client that does not say
                # who it is; one looking on at another's ask is not it.
                if not who or who == ask["from"]:
                    await asyncio.to_thread(herd.collect, ask_id, who == ask["from"])
                return ask
            if loop.time() >= deadline:
                return ask
            await asyncio.sleep(AWAIT_POLL)

    def _herd_cancel(self, params: dict) -> dict:
        return get_herd().cancel(str(params.get("id") or ""))

    def _herd_detach(self, params: dict) -> dict:
        return get_herd().detach(str(params.get("id") or ""))

    def _herd_settle(self, params: dict) -> dict:
        return get_herd().settle(
            str(params.get("id") or ""),
            screen=params.get("screen"),
            dead=bool(params.get("dead")),
            error=str(params.get("error") or "").strip() or None,
        )

    def _herd_asks(self, _params: dict) -> dict:
        return get_herd().recent_asks()

    def _herd_report(self, params: dict) -> dict:
        def text(key):
            v = params.get(key)
            return str(v) if v not in (None, "") else None
        return get_herd().report(
            session=str(params.get("session") or ""),
            state=text("state"),
            reason=text("reason"),
            cli=text("cli"),
            conversation=text("conversation"),
            transcript=text("transcript"),
            last=text("last"),
        )

    def _herd_link(self, params: dict) -> dict:
        return {"link": get_herd().link(str(params.get("session") or ""))}

    def _herd_conversation(self, params: dict) -> dict:
        try:
            limit = max(1, min(200, int(params.get("limit") or conversation.LIMIT)))
        except (TypeError, ValueError):
            raise ValueError("limit must be a number") from None
        try:
            before = max(0, int(params.get("before") or 0))
        except (TypeError, ValueError):
            raise ValueError("before must be a number") from None
        return conversation.read(str(params.get("path") or ""), limit, before)

    def _herd_sub_conversation(self, params: dict) -> dict:
        """Sub-agent SUB's own conversation, under parent transcript PATH.
        SUB is held to a filename's shape and the path resolved under the
        CLI root, so a crafted id cannot read outside it."""
        try:
            limit = max(1, min(200, int(params.get("limit") or conversation.LIMIT)))
        except (TypeError, ValueError):
            raise ValueError("limit must be a number") from None
        return conversation.read_sub(str(params.get("path") or ""),
                                     str(params.get("sub") or ""), limit)

    def _herd_meter(self, params: dict) -> dict:
        """What a claude agent runs on and how full its context is, from
        the transcript at PATH: empty for another CLI's, or before a reply."""
        return meter.read(str(params.get("path") or ""))

    def _herd_subagents(self, params: dict) -> dict:
        """The sub-agents a claude agent has running, from the transcript
        at PATH and theirs beside it; and the files whose change is a
        change in them."""
        return subagents.live(str(params.get("path") or ""))

    def _herd_edits(self, params: dict) -> dict:
        """The files the conversation at PATH edited recently, relative to
        ROOT (the checkout the agent works in) when given; empty for no
        PATH."""
        path = str(params.get("path") or "")
        if not path:
            return {"files": []}
        root = params.get("root")
        try:
            return conversation.edited(path, str(root) if root else None)
        except (OSError, ValueError):
            return {"files": []}

    def _herd_chat(self, params: dict) -> dict:
        """A project's agents' conversations as one timeline.  AGENTS is a
        list of {name, path}; a path that is not a transcript leaves that
        agent out, named in `missing'."""
        agents = params.get("agents")
        if not isinstance(agents, list):
            raise ValueError("agents must be a list")
        pairs = []
        for a in agents:
            if not (isinstance(a, dict) and isinstance(a.get("name"), str)
                    and isinstance(a.get("path"), str)):
                raise ValueError("each agent is {name, path}")
            pairs.append((a["name"], a["path"]))
        try:
            limit = max(1, min(500, int(params.get("limit") or conversation.CHAT_LIMIT)))
        except (TypeError, ValueError):
            raise ValueError("limit must be a number") from None
        try:
            before = max(0, int(params.get("before") or 0))
        except (TypeError, ValueError):
            raise ValueError("before must be a number") from None
        return conversation.chat(pairs, limit, before)

    def _herd_links(self, params: dict) -> dict:
        """The pull requests and ClickUp tasks a transcript names, as given
        to its agent or by it; most recent first."""
        return links.scan(str(params.get("path") or ""))

    def _herd_unlink(self, params: dict) -> dict:
        return get_herd().unlink(str(params.get("session") or ""))

    def _herd_relay(self, params: dict) -> dict:
        """Queue a relay of agent TARGET to KIND, asked by agent BY with
        REASON, onto the same commands queue a Telegram tap uses; Emacs runs
        it (guarded by who asks) and answers.  BY and the guards are Emacs's
        to judge -- this only shapes and queues the request."""
        target = str(params.get("target") or "").strip()
        if not relay.NAME.match(target):
            raise ValueError("target is an agent's name")
        kind = str(params.get("kind") or "").strip()
        if kind and not relay.KIND.match(kind):
            raise ValueError("kind is a CLI's name")
        by = str(params.get("by") or "").strip()
        reason = str(params.get("reason") or "")[:300]
        return get_herd().enqueue_command(
            "relay", target, {"kind": kind, "reason": reason, "by": by}
        )

    def _herd_screens(self, params: dict) -> dict:
        """Queue a read-only `screens' query onto the commands queue; Emacs
        reads the matching saved screens from its log and answers.  The log
        lives in Emacs, so this round-trips rather than reading sqlite."""
        agent = str(params.get("agent") or "").strip()
        why = str(params.get("why") or "").strip()
        since = params.get("since")
        since = float(since) if isinstance(since, (int, float)) else None
        return get_herd().enqueue_command(
            "screens", agent or "herd", {"agent": agent, "since": since, "why": why}
        )

    def _herd_start(self, params: dict) -> dict:
        """Queue a `herd start': a lead starts (or reuses) an agent of KIND in
        its own project.  Emacs judges BY (a lead, its project), the reuse, the
        limits and the usage; this only shapes and queues the request."""
        by = str(params.get("by") or "").strip()
        kind = str(params.get("kind") or "").strip()
        if not relay.KIND.match(kind):
            raise ValueError("kind is a CLI's name: claude, grok or agy")
        worktree = str(params.get("worktree") or "auto").strip()
        if worktree not in ("yes", "no", "auto"):
            raise ValueError("worktree is yes, no or auto")
        branch = str(params.get("branch") or "").strip()
        name = str(params.get("name") or "").strip()
        if name and not relay.NAME.match(name):
            raise ValueError("name is letters, digits, . _ -")
        notes = str(params.get("notes") or "")[:2000]
        return get_herd().enqueue_command(
            "start", by,
            {"by": by, "kind": kind, "worktree": worktree, "branch": branch,
             "name": name, "notes": notes, "new": bool(params.get("new"))},
        )

    def _herd_stop(self, params: dict) -> dict:
        """Queue a `herd stop': a lead stops an agent it started.  Emacs checks
        BY started NAME and that NAME owes no open or queued ask, then kills it
        with the worktree cleanup."""
        by = str(params.get("by") or "").strip()
        name = str(params.get("name") or "").strip()
        if not relay.NAME.match(name):
            raise ValueError("name is an agent's name")
        return get_herd().enqueue_command("stop", name, {"by": by})

    def _herd_command_reply(self, params: dict) -> dict:
        """A queued command's status and reply, by its id -- `herd relay'
        polls it for what Emacs did."""
        return get_herd().command_reply(str(params.get("id") or ""))

    def _memory_store(self, params: dict):
        """Resolve (store path, key) for a memory call: the user store with
        --user; otherwise the caller's project -- from its session (the herd
        has it resolved to the main checkout, so a worktree shares the main
        store), or the PROJECT hint a plain shell passes from its cwd."""
        if params.get("user"):
            return shared_memory.user_store(), shared_memory.USER_KEY
        who = str(params.get("from") or "").strip()
        project = ""
        if who and who != "user":
            session = get_herd().session(who) or {}
            project = session.get("project") or ""
        if not project:
            project = str(params.get("project") or "").strip()
        if not project:
            raise ValueError(
                "no project for this memory: run inside one, or use --user")
        key = shared_memory.slug_for_path(project)
        return shared_memory.project_store(key), key

    def _herd_remember(self, params: dict) -> dict:
        """Write a fact to the caller's project store (or --user), authored by
        the caller (GHOSTHERD_SESSION, or `user')."""
        text = str(params.get("text") or "")
        if not text.strip():
            raise ValueError("nothing to remember")
        typ = str(params.get("type") or shared_memory.DEFAULT_TYPE).strip()
        if typ not in shared_memory.TYPES:
            raise ValueError("type is one of: " + ", ".join(shared_memory.TYPES))
        name = str(params.get("name") or "").strip() or None
        store, key = self._memory_store(params)
        author = str(params.get("from") or "user").strip() or "user"
        return shared_memory.write_fact(store, key, text, type=typ, name=name, author=author)

    def _herd_recall(self, params: dict) -> dict:
        "The project's index, or the facts matching QUERY in full."
        store, key = self._memory_store(params)
        return shared_memory.recall(store, key, query=str(params.get("query") or ""))

    def _herd_forget(self, params: dict) -> dict:
        "Remove a fact by NAME from the caller's project store (or --user)."
        name = str(params.get("name") or "").strip()
        if not name:
            raise ValueError("which fact? give its name")
        store, key = self._memory_store(params)
        return shared_memory.forget(store, key, name)

    def _herd_relay_brief(self, params: dict) -> dict:
        """What agent NAME hands its heir, as the heir's first message: its
        work from its transcript PATH, its asks, the checkout at PROJECT.
        FROM and TO are the old and new CLIs, GENERATION the heir's place
        in the line, REASON why the old one stopped, CUT true when it
        stopped mid-turn, NOTES its notes, CLIENT the herd client an ask is
        answered with."""
        name = str(params.get("name") or "")
        if not relay.NAME.match(name):
            raise ValueError("name is an agent's name")
        kinds = []
        for key in ("from", "to"):
            kind = str(params.get(key) or "")
            if not relay.KIND.match(kind):
                raise ValueError(f"{key} is a CLI's name")
            kinds.append(kind)
        try:
            generation = int(params.get("generation") or 2)
        except (TypeError, ValueError):
            raise ValueError("generation must be a number") from None
        if not 2 <= generation <= 999:
            raise ValueError("generation is 2 to 999")
        path = params.get("path")
        project = params.get("project")
        return relay.brief(
            name,
            str(path) if path else None,
            str(project) if project else None,
            kinds[0],
            kinds[1],
            generation,
            reason=str(params.get("reason") or "")[:300],
            notes=str(params.get("notes") or "")[:500],
            asks=get_herd().relay_asks(name),
            client=str(params.get("client") or "herd")[:1000],
            cut=params.get("cut") is True,
        )


handler = JsonRpcHandler()
