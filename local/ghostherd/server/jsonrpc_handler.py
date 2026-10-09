"""JSON-RPC 2.0 dispatcher.  Same contract as ecloud: one POST /jsonrpc."""

from __future__ import annotations

import asyncio
import inspect
import traceback
from typing import Any, Callable

from pydantic import BaseModel

import conversation
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
        return get_herd().tick(
            sessions=sessions,
            ack_ids=[str(i) for i in ack_ids],
            replies=replies,
        )

    def _herd_ask(self, params: dict) -> dict:
        return get_herd().ask(
            to=str(params.get("to") or ""),
            body=params.get("body"),
            from_name=params.get("from"),
            wait=bool(params.get("wait")),
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
                await asyncio.to_thread(herd.collect, ask_id)
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
        return conversation.read(str(params.get("path") or ""), limit)


handler = JsonRpcHandler()
