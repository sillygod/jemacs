"""JSON-RPC 2.0 dispatcher.  Same contract as ecloud: one POST /jsonrpc."""

from __future__ import annotations

import asyncio
import inspect
import traceback
from typing import Any, Callable

from pydantic import BaseModel

import conversation
import fact_index
import herd as herd_module
import images
import links
import memory_migrate
import meter
import projects
import relay
import shared_memory
import subagents
from engine import get_engine
from herd import ASK_CLOSED, get_herd

# How long a recall waits for the embedder to come up before it ranks by
# words alone.  The CLI's own timeout is 15s.
RECALL_WAIT = 8.0


def _ask_memory(taker, body):
    "What an ask carries from the shared memory (herd.py's hook)."
    return fact_index.get_index().ask_block(taker, body)


herd_module.ASK_MEMORY = _ask_memory

PARSE_ERROR = -32700
INVALID_REQUEST = -32600
METHOD_NOT_FOUND = -32601
INVALID_PARAMS = -32602
INTERNAL_ERROR = -32603
FORBIDDEN = -32003

# What an agent does, through `bin/herd' (its asks, its memory, a lead's
# start / stop / relay / screens and their replies), its hooks (report,
# the guard's refusals and uses) and PROTOCOL.md (message, inbox): the
# methods the agents' token reaches.  The rest is the herd's own -- Emacs
# and the page: the tick, approving a guard's refusal, the Memory tab, the
# transcript index, migrating claude's memory dirs.
AGENT_METHODS = frozenset({
    "ping",
    "herd_list", "herd_message", "herd_inbox",
    "herd_ask", "herd_handoff", "herd_reply", "herd_await", "herd_cancel", "herd_detach", "herd_asks",
    "herd_report", "herd_guard_denied", "herd_guard_used",
    "herd_remember", "herd_recall", "herd_forget", "herd_promote_propose", "herd_search",
    "herd_start", "herd_stop", "herd_relay", "herd_screens", "herd_command_reply",
})


def _recall_block(fact: dict, key: str) -> str:
    """A fact in full, as `herd recall' prints it -- and, for one that was
    changed, a line on what it said before (shared_memory.was_line)."""
    store = shared_memory.user_store() if fact["shared"] else shared_memory.project_store(key)
    f = shared_memory.parse(store / f"{fact['name']}.md")
    was = shared_memory.was_line(f, store) if f else ""
    return (f"# {fact['title']}  ({fact['type']}, by {fact['author'] or 'unknown'}, "
            f"{'user' if fact['shared'] else key})\n{fact['body']}"
            + (f"\n\n({was})" if was else ""))


def _restated(hit: dict, key: str) -> str:
    """Why `herd remember' refused, and the two ways on -- for an agent,
    which runs it again without asking anyone."""
    name = hit["name"]
    if hit["shared"] and key != shared_memory.USER_KEY:
        where, update = "the user store (every project)", f"--user --name {name}"
    else:
        where = "the user store" if key == shared_memory.USER_KEY else "this project"
        update = f"--name {name}"
    when = (hit.get("updated") or "")[:10]
    said = hit.get("description") or ""
    width = max(len(update), len("--new"))
    return (f"not remembered: this says again the fact `{name}' in {where}"
            f"{' (' + when + ')' if when else ''}: \"{said}\"\n"
            f"Run the same command again, adding one of:\n"
            f"  {update.ljust(width)}   to update `{name}' with this text "
            f"(its current text is kept in its history)\n"
            f"  {'--new'.ljust(width)}   to keep both, as separate facts")


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
            "memory_words": self._words,
            "memory_import": self._import,
            "memory_search": self._search,
            "memory_list": self._list,
            "memory_chunks": self._chunks,
            "herd_list": self._herd_list,
            "herd_message": self._herd_message,
            "herd_inbox": self._herd_inbox,
            "herd_tick": self._herd_tick,
            "herd_ask": self._herd_ask,
            "herd_handoff": self._herd_handoff,
            "herd_guard_denied": self._herd_guard_denied,
            "herd_guard_approve": self._herd_guard_approve,
            "herd_guard_reject": self._herd_guard_reject,
            "herd_guard_clear": self._herd_guard_clear,
            "herd_guard_used": self._herd_guard_used,
            "herd_guard_requests": self._herd_guard_requests,
            "herd_inbox_state": self._herd_inbox_state,
            "herd_inbox_set": self._herd_inbox_set,
            "herd_pending_proposals": self._herd_pending_proposals,
            "herd_reply": self._herd_reply,
            "herd_await": self._herd_await,
            "herd_cancel": self._herd_cancel,
            "herd_detach": self._herd_detach,
            "herd_settle": self._herd_settle,
            "herd_asks": self._herd_asks,
            "herd_threads": self._herd_threads,
            "herd_thread": self._herd_thread,
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
            "herd_search": self._herd_search,
            "herd_forget": self._herd_forget,
            "memory_opening": self._memory_opening,
            "memory_rank": self._memory_rank,
            "memory_link_claude": self._memory_link_claude,
            "memory_migrate": self._memory_migrate,
            "memory_unmigrate": self._memory_unmigrate,
            "herd_memory_projects": self._herd_memory_projects,
            "herd_memory_list": self._herd_memory_list,
            "herd_memory_edit": self._herd_memory_edit,
            "herd_memory_delete": self._herd_memory_delete,
            "herd_promote_propose": self._herd_promote_propose,
            "herd_memory_proposals": self._herd_memory_proposals,
            "herd_memory_promote_approve": self._herd_memory_promote_approve,
            "herd_memory_promote_reject": self._herd_memory_promote_reject,
            "herd_memory_unpromote": self._herd_memory_unpromote,
            "herd_memory_suggest": self._herd_memory_suggest,
        }

    async def handle(self, request: JsonRpcRequest, role: str = "admin") -> JsonRpcResponse:
        method = self._methods.get(request.method)
        if method is None:
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(
                    code=METHOD_NOT_FOUND,
                    message=f"Method not found: {request.method}",
                ),
            )
        if role != "admin" and request.method not in AGENT_METHODS:
            # An answer, not a refusal of the URL: `bin/herd' must not take
            # it for a sidecar away and retry.
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(
                    code=FORBIDDEN,
                    message=f"{request.method} is the herd's own, not an agent's: Emacs and the "
                            "page call it, with the user.  Ask the user.",
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

    def _words(self, params: dict) -> dict:
        """The BM25 half's own index (wordindex): checked against the
        collection and repaired, or with REBUILD made anew from the chunks'
        stored text -- in the background; memory_status's `words' says how
        it went.  The herd's own: it holds the import for a while."""
        import threading

        engine = get_engine()
        rebuild = bool(params.get("rebuild"))

        def go():
            try:
                engine.index_words(rebuild=rebuild)
            except Exception as exc:  # noqa: BLE001 -- the status says so
                engine._words_report = {"error": str(exc)}

        threading.Thread(target=go, daemon=True, name="ghostherd-words").start()
        return {"started": "rebuild" if rebuild else "check"}

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
            titles=params.get("titles") is True,
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
            restored=params.get("restored"),
            guard_ttl=params.get("guard_ttl"),
        )

    def _herd_ask(self, params: dict) -> dict:
        return get_herd().ask(
            to=str(params.get("to") or ""),
            body=params.get("body"),
            from_name=params.get("from"),
            wait=bool(params.get("wait")),
            rounds=params.get("rounds"),
        )

    def _herd_handoff(self, params: dict) -> dict:
        """`herd handoff PROJECT': an ask of another project's lead.  PROJECT
        is a path (made absolute by the client), a leaf name or a slug,
        resolved among the projects the herd knows -- its agents', its
        shared-memory stores', and those claude has run in (its projects
        dir names them by slug): primary routes to projects nobody works in
        now -- to the project's main checkout."""
        slugs = list(dict.fromkeys(shared_memory.project_keys() + self._claude_slugs()))
        where = projects.resolve(str(params.get("project") or ""), get_herd().projects(), slugs)
        return get_herd().handoff(
            projects.checkout(where),
            params.get("body"),
            from_name=params.get("from"),
            wait=bool(params.get("wait")),
            rounds=params.get("rounds"),
        )

    def _herd_guard_denied(self, params: dict) -> dict:
        "A guarded agent's guard refused a call (`herd guard')."
        return get_herd().guard_denied(
            str(params.get("session") or ""), str(params.get("guard_dir") or ""),
            str(params.get("tool") or ""), str(params.get("summary") or ""),
            str(params.get("reason") or ""), str(params.get("rule") or ""),
            str(params.get("kind") or ""), bool(params.get("approvable")),
            str(params.get("fp") or ""), str(params.get("call") or ""))

    def _herd_guard_approve(self, params: dict) -> dict:
        "The user approves a refused call once (the card, the phone)."
        return get_herd().guard_approve(str(params.get("id") or ""))

    def _herd_guard_reject(self, params: dict) -> dict:
        "The user keeps a refused call denied (the card, the Inbox, the phone)."
        return get_herd().guard_reject(str(params.get("id") or ""))

    def _herd_guard_clear(self, params: dict) -> dict:
        "An agent respawned: its open guard requests expire."
        return get_herd().guard_clear(str(params.get("session") or ""))

    def _herd_guard_used(self, params: dict) -> dict:
        return get_herd().guard_used(str(params.get("id") or ""))

    def _herd_guard_requests(self, _params: dict) -> dict:
        return get_herd().guard_requests()

    def _herd_inbox_state(self, params: dict) -> dict:
        "The user's read/done/override on the Inbox items; prunes the gone ones."
        ids = params.get("ids")
        live = [str(i) for i in ids] if isinstance(ids, list) else None
        return get_herd().inbox_state(live)

    def _herd_inbox_set(self, params: dict) -> dict:
        "Mark an Inbox item read / done, or override its group."
        return get_herd().inbox_set(
            str(params.get("id") or ""),
            read=params.get("read"), done=params.get("done"),
            override=params.get("override"))

    @staticmethod
    def _claude_slugs() -> list[str]:
        "The projects claude has run in, by the slugs its projects dir names."
        from config import get_config
        try:
            return sorted(d.name for d in get_config().claude_root.iterdir()
                          if d.is_dir() and d.name.startswith("-"))
        except OSError:
            return []

    def _herd_reply(self, params: dict) -> dict:
        ask_id = str(params.get("id") or "")
        out = get_herd().reply(ask_id, params.get("body"), from_name=params.get("from"))
        # The images the answer names, copied before their scratch dir goes.
        images.capture_soon(str(params.get("body")))
        # A promotion the lead was asked to apply is done once that ask is
        # answered: mark its facts promoted so they leave the index, the
        # openings and retrieval.  Best effort -- never fails the reply.
        try:
            self._apply_promotion(ask_id)
        except Exception:  # noqa: BLE001
            pass
        return out

    def _apply_promotion(self, ask_id: str) -> None:
        prop = get_herd().proposal_by_ask(ask_id)
        if not prop or prop["status"] != "approved":
            return
        key = prop["project"]
        store = shared_memory.project_store(key)
        for name in prop["facts"]:
            try:
                shared_memory.set_promoted(store, key, name, prop["file"])
            except ValueError:
                pass  # a fact gone or already not promotable: skip it
        get_herd().mark_proposal_applied(prop["id"])

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

    def _herd_threads(self, params: dict) -> dict:
        "The page's Threads tab: the recent threads, and what is open now."
        try:
            limit = int(params.get("limit") or 40)
        except (TypeError, ValueError):
            raise ValueError("limit must be a number") from None
        return get_herd().threads(limit)

    def _herd_thread(self, params: dict) -> dict:
        "One thread, whole: by its id, an ask in it, or an agent's latest."
        def opt(key):
            v = params.get(key)
            if v is not None and not isinstance(v, str):
                raise ValueError(f"{key} must be a string")
            return (v or "").strip() or None
        id_, ask, agent = opt("id"), opt("ask"), opt("agent")
        if not (id_ or ask or agent):
            raise ValueError("give the thread's id, an ask in it, or an agent")
        out = get_herd().thread(id=id_, ask=ask, agent=agent)
        # An answer's images: what it named, as recorded when it came.
        store = None
        for m in out.get("msgs") or []:
            if m.get("kind") == "answer" and images.named(m.get("text") or ""):
                store = store or images.get_store()
                at = m["at"] / 1000 if m.get("at") else None
                recs = images.public(store.lookup(m["text"], at))
                if recs:
                    m["images"] = recs
        return out

    def _herd_report(self, params: dict) -> dict:
        def text(key):
            v = params.get(key)
            return str(v) if v not in (None, "") else None
        pid = params.get("pid")
        ancestors = params.get("ancestors") or []
        if pid is not None and (isinstance(pid, bool) or not isinstance(pid, int)):
            raise ValueError("pid must be a process id")
        if not isinstance(ancestors, list) or not all(
                isinstance(a, int) and not isinstance(a, bool) for a in ancestors):
            raise ValueError("ancestors must be a list of process ids")
        out = get_herd().report(
            session=str(params.get("session") or ""),
            state=text("state"),
            reason=text("reason"),
            cli=text("cli"),
            conversation=text("conversation"),
            transcript=text("transcript"),
            last=text("last"),
            pid=pid or None,
            ancestors=ancestors,
            headless=params.get("headless") is True,
        )
        # The turn's last words: the images they name, copied now -- not a
        # probe's words under the agent's name, which were not recorded.
        if not out.get("ignored"):
            images.capture_soon(text("last"))
        return out

    def _herd_link(self, params: dict) -> dict:
        link = get_herd().link(str(params.get("session") or ""))
        if link and link.get("last"):
            at = link["last_ms"] / 1000 if link.get("last_ms") else None
            link["last_images"] = images.public(images.get_store().lookup(link["last"], at))
        return {"link": link}

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
        # An explicit --project (a path, a leaf name, or a slug) wins over the
        # caller's own session, so the user can reach any project's memory from
        # any shell.
        if params.get("project_given"):
            key = self._project_key_arg(str(params.get("project") or ""))
            return shared_memory.project_store(key), key
        who = str(params.get("from") or "").strip()
        project = ""
        if who and who != "user":
            session = get_herd().session(who) or {}
            project = session.get("project") or ""
        if not project:
            project = str(params.get("project") or "").strip()
        if not project:
            raise ValueError(
                "no project for this memory: run inside one, or use --project/--user")
        key = shared_memory.slug_for_path(project)
        return shared_memory.project_store(key), key

    def _write_store(self, params: dict):
        """`_memory_store' for a write -- remember, forget, promote.  The
        herd-wide lead writes its own store only: a project's memory is its
        lead's, the user's preferences the user's, and the herd's promotion
        of a fact is a project's -- it reads them all (recall --project), and
        recommends."""
        store, key = self._memory_store(params)
        who = str(params.get("from") or "").strip()
        me = get_herd().primary()
        if me and who == me["name"] and key != shared_memory.slug_for_path(me["project"]):
            whose = "the user's" if key == shared_memory.USER_KEY else "its project's lead's"
            raise ValueError(
                f"{who} writes only its own memory: that store is {whose} to change "
                "-- recommend it instead")
        return store, key

    def _project_key_arg(self, p: str) -> str:
        """A store key from a --project value: a path (has a `/' or `~'), an
        existing store slug, or a project's leaf name (e.g. deploy-platform)."""
        import os
        import re
        p = (p or "").strip()
        if not p:
            raise ValueError("--project needs a path or a project name")
        if "/" in p or p.startswith("~") or os.path.isabs(os.path.expanduser(p)):
            return shared_memory.slug_for_path(p)
        keys = shared_memory.project_keys()
        if p in keys:
            return p
        # A leaf name: match a known project path's basename, else a slug that
        # ends with it (the slug is the path with `/' and `.' -> `-').
        for k, path in self._project_paths().items():
            if os.path.basename(os.path.normpath(os.path.expanduser(path))) == p:
                return k
        leaf = re.sub(r"[/.]", "-", p)
        cand = [k for k in keys if k.endswith("-" + leaf)]
        if len(cand) == 1:
            return cand[0]
        if len(cand) > 1:
            raise ValueError(f"{p!r} matches more than one project: {', '.join(cand)}")
        raise ValueError(
            f"no project memory for {p!r}; known: {', '.join(keys) or 'none'}")

    def _herd_remember(self, params: dict) -> dict:
        """Write a fact to the caller's project store (or --user), authored by
        the caller (GHOSTHERD_SESSION, or `user').

        Text that says again a fact the store has is refused, naming that
        fact and what to run next (`_restated'): --name updates it (its
        text kept in its history), --new keeps both.  A NAME that exists is
        an update, unchecked."""
        text = str(params.get("text") or "")
        if not text.strip():
            raise ValueError("nothing to remember")
        typ = str(params.get("type") or shared_memory.DEFAULT_TYPE).strip()
        if typ not in shared_memory.TYPES:
            raise ValueError("type is one of: " + ", ".join(shared_memory.TYPES))
        name = str(params.get("name") or "").strip() or None
        if name:
            name = shared_memory.valid_name(name)
        new = bool(params.get("new"))
        store, key = self._write_store(params)
        author = str(params.get("from") or "user").strip() or "user"
        checked = "skipped"
        if not name and shared_memory.holding(store, key, text, typ):
            pass  # said word for word already: write_fact answers `unchanged'
        elif not new and not (name and (store / f"{name}.md").exists()):
            found = fact_index.get_index().near_duplicate(key, text, name=name)
            checked = found["mode"]
            if found["hit"]:
                raise ValueError(_restated(found["hit"], key))
        out = shared_memory.write_fact(store, key, text, type=typ, name=name,
                                       author=author, new=new)
        out["checked"] = checked
        return out

    def _attribute_project(self, key: str) -> list:
        """Stamp the agent's name onto store files a claude agent of this
        project wrote natively (its memory dir is a symlink to the store), from
        the edit marks in its transcript.  Only files it edited recently and
        that have no author yet -- so old migrated facts are left as they are,
        and it is cheap to run on a read."""
        from config import get_config
        claude_root = get_config().claude_root
        store = shared_memory.project_store(key)
        if not store.is_dir():
            return []
        changed = []
        for s in get_herd().list_sessions().get("sessions", []):
            if s.get("kind") != "claude":
                continue
            if shared_memory.slug_for_path(s.get("project") or "") != key:
                continue
            link = get_herd().link(s.get("name") or "") or {}
            transcript = link.get("transcript")
            if not transcript:
                continue
            cmem = claude_root / shared_memory.slug_for_path(
                s.get("directory") or s.get("project") or "") / "memory"
            try:
                edited = conversation.edited(transcript, str(cmem)).get("files", [])
            except (OSError, ValueError):
                edited = []
            for fname in edited:
                if not fname.endswith(".md") or fname == shared_memory.INDEX:
                    continue
                path = store / fname
                if path.exists() and not path.is_symlink():
                    if shared_memory.stamp_author(path, s["name"]):
                        changed.append({"name": fname[:-3], "author": s["name"]})
        if changed:
            shared_memory.regenerate(store, key)
        return changed

    def _herd_recall(self, params: dict) -> dict:
        """The project's index, or the facts relevant to QUERY in full, best
        first -- ranked by meaning and words (fact_index), or by words alone
        when the embedder is not up within RECALL_WAIT."""
        store, key = self._memory_store(params)
        if key != shared_memory.USER_KEY:
            try:
                self._attribute_project(key)
            except Exception:  # noqa: BLE001 — attribution must never fail a read
                pass
        query = str(params.get("query") or "").strip()
        if not query:
            return shared_memory.recall(store, key, query="")
        # A fact's name reads that fact, whole -- what an ask's memory says
        # to do with a fact it shows only the start of.
        if shared_memory.NAME_RE.match(query.lower()):
            named = [f.as_dict() for f in shared_memory.pool(key) if f.name == query.lower()]
            if named:
                return {"query": query, "count": 1, "mode": "name",
                        "text": _recall_block(named[0], key), "names": [query.lower()]}
        found = fact_index.get_index().rank(key, query, strict=False, wait=RECALL_WAIT)
        hits = found["hits"]
        blocks = [_recall_block(h, key) for h in hits]
        note = "" if found["mode"] != "words" else (
            "(ranked by words alone: the embedder is not up yet)\n\n")
        return {"query": query, "count": len(hits), "mode": found["mode"],
                "text": (note + "\n\n---\n\n".join(blocks)) if blocks
                else f"no fact matches {query!r}",
                "names": [h["name"] for h in hits]}

    def _herd_search(self, params: dict) -> dict:
        """What the calling agent's own project's past sessions said
        (agent_search): QUERY's best chunks, or SESSION read from turn AT.
        The project is the one the herd knows the caller in -- a project
        named in the call is not taken -- and a caller the herd does not
        know (a plain shell) is refused: the user has the page's Search."""
        import agent_search
        from config import get_config
        from engine import get_engine

        who = str(params.get("from") or "").strip()
        me = get_herd().session(who) if who and who != "user" else None
        if not me:
            raise ValueError(
                "herd search is for the herd's agents, of their own project; "
                "search every project from the herd page's Search (or Emacs)")
        project = me.get("project") or ""
        if not project:
            raise ValueError(f"{who} has no project to search")
        cfg = get_config()
        engine = get_engine()
        known = list(get_herd().projects()) + [
            p for p in engine.index.projects_under("/") if p]
        projects = agent_search.scope(engine, project, cfg.worktree_root, known)
        tokens = agent_search.herd_tokens(cfg.data_dir)
        if str(params.get("session") or "").strip():
            return agent_search.read(engine, projects, str(params["session"]),
                                     int(params.get("at") or 0), tokens)
        query = str(params.get("query") or "").strip()
        if not query:
            raise ValueError("search for what?  herd search WORDS, or --session ID")
        return agent_search.search(engine, projects, query, tokens)

    def _memory_opening(self, params: dict) -> dict:
        """What a grok or agy agent of the caller's project is told of the
        memory as it starts (shared_memory.opening): the rules in full, the
        rest one line each within MAX characters, then the names of what did
        not fit.  Emacs asks for it at spawn."""
        _store, key = self._memory_store(params)
        cap = int(params.get("max") or 4000)
        return shared_memory.opening(key, max(0, cap))

    def _memory_rank(self, params: dict) -> dict:
        """The facts of the caller's project (or --user) relevant to QUERY,
        best first, each with its score and why -- the one ranking `herd
        recall' and asks use, for the page to call.  STRICT (default false)
        takes an ask's bar; LIMIT caps the hits; WAIT is how long to wait for
        the embedder before words alone rank them."""
        _store, key = self._memory_store(params)
        query = str(params.get("query") or "")
        limit = params.get("limit")
        wait = params.get("wait")
        return fact_index.get_index().rank(
            key, query, strict=bool(params.get("strict")),
            k=int(limit) if limit not in (None, "") else None,
            wait=float(wait) if wait not in (None, "") else RECALL_WAIT)

    def _herd_forget(self, params: dict) -> dict:
        "Remove a fact by NAME from the caller's project store (or --user)."
        name = str(params.get("name") or "").strip()
        if not name:
            raise ValueError("which fact? give its name")
        store, key = self._write_store(params)
        return shared_memory.forget(store, key, name,
                                    by=str(params.get("from") or "user").strip() or "user")

    def _memory_link_claude(self, params: dict) -> dict:
        """Point a claude agent's memory dir at its project's store.  Emacs
        calls this at spawn / worktree creation / reuse.  CWD is the agent's
        working dir (worktree or main), PROJECT the canonical main checkout."""
        cwd = str(params.get("cwd") or params.get("project") or "").strip()
        project = str(params.get("project") or cwd).strip()
        if not project:
            raise ValueError("project is required")
        from config import get_config
        return shared_memory.ensure_project_symlink(get_config().claude_root, cwd, project)

    def _known_projects(self) -> list[str]:
        "The distinct projects the herd has in its snapshot, main checkouts."
        seen, out = set(), []
        for s in get_herd().list_sessions().get("sessions", []):
            p = (s.get("project") or "").strip()
            if p and p not in seen:
                seen.add(p)
                out.append(p)
        return out

    def _memory_migrate(self, params: dict) -> dict:
        """Move existing claude memories into the store.  PROJECTS defaults to
        the projects the herd knows; USER_NAMES are facts to route to the user
        store on top of every type:user one; DRY_RUN plans without touching."""
        projects = params.get("projects") or self._known_projects()
        if not isinstance(projects, list):
            raise ValueError("projects must be a list")
        user_names = set(params.get("user_names") or [])
        if params.get("dry_run"):
            return memory_migrate.plan(projects, user_names=user_names)
        report = memory_migrate.migrate(projects, user_names=user_names)
        report["verify"] = memory_migrate.verify(projects, user_names=user_names)
        return report

    def _memory_unmigrate(self, params: dict) -> dict:
        "Put the claude memory backups back, dropping the store symlinks."
        projects = params.get("projects") or self._known_projects()
        if not isinstance(projects, list):
            raise ValueError("projects must be a list")
        return memory_migrate.unmigrate(projects)

    # ---- The Memory tab on the page -----------------------------------

    def _project_paths(self) -> dict:
        "Store key (slug) -> the project path the herd knows for it, if any."
        out = {}
        for s in get_herd().list_sessions().get("sessions", []):
            p = (s.get("project") or "").strip()
            if p:
                out[shared_memory.slug_for_path(p)] = p
        return out

    def _memory_store_by_key(self, params: dict):
        """Resolve (store path, key) from a page request: a `key' (slug) or a
        `project' (path), guarded against path tricks."""
        key = str(params.get("key") or "").strip()
        if not key:
            proj = str(params.get("project") or "").strip()
            key = shared_memory.slug_for_path(proj) if proj else ""
        key = shared_memory.valid_key(key)
        store = (shared_memory.user_store() if key == shared_memory.USER_KEY
                 else shared_memory.project_store(key))
        return store, key

    def _herd_memory_projects(self, _params: dict) -> dict:
        """The stores for the tab's picker: the user store and each project's,
        each with its slug key, a label and the project path (so the page can
        match an open room's project to its store)."""
        import os
        paths = self._project_paths()
        projects = []
        for k in shared_memory.project_keys():
            path = paths.get(k, "")
            label = (os.path.basename(os.path.normpath(os.path.expanduser(path)))
                     if path else k)
            projects.append({"key": k, "label": label or k, "project": path})
        return {"user": {"key": shared_memory.USER_KEY, "label": "User (shared)"},
                "projects": projects}

    def _herd_memory_list(self, params: dict) -> dict:
        "The facts for store KEY -- the user facts, and a project's own too."
        store, key = self._memory_store_by_key(params)
        if key != shared_memory.USER_KEY:
            try:
                self._attribute_project(key)
            except Exception:  # noqa: BLE001 — attribution must never fail a read
                pass
        return {"key": key, "facts": shared_memory.facts_for(key)}

    def _herd_memory_edit(self, params: dict) -> dict:
        "Rewrite a fact (description, type, body) from the page, authored `user'."
        typ = str(params.get("type") or shared_memory.DEFAULT_TYPE).strip()
        if typ not in shared_memory.TYPES:
            raise ValueError("type is one of: " + ", ".join(shared_memory.TYPES))
        store, key = self._memory_store_by_key(params)
        return shared_memory.edit_fact(
            store, key, str(params.get("name") or ""),
            description=str(params.get("description") or ""),
            body=str(params.get("body") or ""), type=typ, author="user")

    def _herd_memory_delete(self, params: dict) -> dict:
        "Delete a fact by NAME from store KEY (the page's confirmed delete)."
        store, key = self._memory_store_by_key(params)
        return shared_memory.forget(store, key, str(params.get("name") or ""))

    # ---- Promote memory into instructions ---------------------------------

    def _lead_for(self, key: str) -> str | None:
        "The name of project store KEY's lead agent, or None."
        for s in get_herd().list_sessions().get("sessions", []):
            if s.get("role") == "lead" and \
               shared_memory.slug_for_path(s.get("project") or "") == key:
                return s.get("name")
        return None

    def _herd_promote_propose(self, params: dict) -> dict:
        """An agent proposes promoting stable facts into the project's
        instruction file, with the exact diff.  The herd checks each fact is
        the project's (never a machine/user one) and stores it pending the
        user's approval on the page; it writes nothing to the repo."""
        store, key = self._write_store(params)
        if key == shared_memory.USER_KEY:
            raise ValueError("user-store facts cannot be promoted")
        facts = params.get("facts") or []
        if not isinstance(facts, list):
            raise ValueError("facts must be a list of names")
        names = [str(f).strip() for f in facts if str(f).strip()]
        if not names:
            raise ValueError("name at least one fact to promote")
        # Refuse here, before storing: a fact tied to the machine or the user
        # (a user fact, the user store) is never proposable.
        for name in names:
            shared_memory.check_promotable(store, key, name)
        return get_herd().propose(
            key, names, str(params.get("file") or "").strip(),
            str(params.get("diff") or ""), str(params.get("from") or "unknown"))

    def _herd_memory_proposals(self, params: dict) -> dict:
        """The proposals for store KEY, newest first -- what the page's
        Proposals section shows.  Pending ones by default; STATUS filters."""
        _store, key = self._memory_store_by_key(params)
        status = str(params.get("status") or "").strip() or None
        props = get_herd().proposals(project=key, status=status)
        # The page shows the facts' titles and whether each is still a memory.
        for p in props:
            titled = []
            for name in p["facts"]:
                f = shared_memory.parse(shared_memory.project_store(key) / f"{name}.md")
                titled.append({"name": name, "title": f.title if f else shared_memory.title_of(name),
                               "gone": f is None, "promoted": bool(f and f.promoted)})
            p["facts"] = titled
        return {"key": key, "proposals": props}

    def _herd_pending_proposals(self, _params: dict) -> dict:
        "Every pending memory proposal across projects, for the Inbox."
        props = get_herd().proposals(status="pending")
        for p in props:
            titled = []
            store = shared_memory.project_store(p["project"])
            for name in p["facts"]:
                f = shared_memory.parse(store / f"{name}.md")
                titled.append({"name": name, "title": f.title if f else shared_memory.title_of(name),
                               "gone": f is None, "promoted": bool(f and f.promoted)})
            p["facts"] = titled
        return {"proposals": props}

    def _herd_memory_promote_approve(self, params: dict) -> dict:
        """The user approved a proposal: hand its diff to the project's lead
        (or, with no lead, its proposer) as an ask to apply it following the
        project's branch and PR rules.  The facts are promoted when that ask
        is answered.  The herd never edits the repo."""
        pid = str(params.get("id") or "").strip()
        prop = get_herd().proposal(pid)
        if prop["status"] != "pending":
            raise ValueError(f"proposal is already {prop['status']}")
        key = prop["project"]
        to = self._lead_for(key) or prop["author"]
        if not to or to == "unknown":
            raise ValueError("no lead or proposer to apply this; start the project's lead")
        shown = ", ".join(f["name"] if isinstance(f, dict) else str(f) for f in prop["facts"])
        body = (
            f"The user approved promoting {shown} into {prop['file']}.\n\n"
            "Apply this approved change, following this project's branch and PR "
            "rules (e.g. a branch and a PR where that is the rule, never a direct "
            f"commit to a protected branch), then reply when done.\n\n"
            f"The approved diff for {prop['file']}:\n\n{prop['diff']}"
        )
        ask = get_herd().ask(to=to, body=body, from_name="user")
        get_herd().approve_proposal(pid, ask["id"])
        return {"id": pid, "status": "approved", "ask": ask["id"], "to": to}

    def _herd_memory_promote_reject(self, params: dict) -> dict:
        "Reject a proposal, recording why; the facts stay memories."
        return get_herd().reject_proposal(
            str(params.get("id") or "").strip(), str(params.get("reason") or ""))

    def _herd_memory_unpromote(self, params: dict) -> dict:
        "Put a promoted fact back: it returns to the index, openings, retrieval."
        store, key = self._memory_store_by_key(params)
        return shared_memory.unpromote(store, key, str(params.get("name") or ""))

    def _herd_memory_suggest(self, params: dict) -> dict:
        """The page's "Suggest promotions": ask the project's lead (or the agent
        the user picked) to review the memory and propose promotions."""
        _store, key = self._memory_store_by_key(params)
        if key == shared_memory.USER_KEY:
            raise ValueError("the user store has nothing to promote")
        to = str(params.get("agent") or "").strip() or self._lead_for(key)
        if not to:
            raise ValueError("no lead for this project; start one or pick an agent")
        body = (
            "Please review this project's shared memory and propose any facts "
            "that are stable, general rules belonging in the project's "
            "instruction file (CLAUDE.md, or AGENTS.md where it links to it) -- "
            "e.g. a branch/PR rule the whole team follows.\n\n"
            "Read the memory (`$GHOSTHERD_HERD recall`) and the instruction file, "
            "then for the ones worth promoting run:\n"
            "  $GHOSTHERD_HERD promote propose --fact NAME [--fact NAME ...] "
            "--file CLAUDE.md   (the exact diff on stdin)\n\n"
            "Propose only stable, general rules. Never machine- or user-specific "
            "facts (paths, credentials setup, personal preferences) -- those stay "
            "memories; the herd refuses them. The user approves each on the page."
        )
        ask = get_herd().ask(to=to, body=body, from_name="user")
        return {"key": key, "ask": ask["id"], "to": to}

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
