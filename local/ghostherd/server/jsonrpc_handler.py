"""JSON-RPC 2.0 dispatcher.  Same contract as ecloud: one POST /jsonrpc."""

from __future__ import annotations

import asyncio
import traceback
from typing import Any, Callable

from pydantic import BaseModel

from engine import get_engine

PARSE_ERROR = -32700
INVALID_REQUEST = -32600
METHOD_NOT_FOUND = -32601
INVALID_PARAMS = -32602
INTERNAL_ERROR = -32603


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


handler = JsonRpcHandler()
