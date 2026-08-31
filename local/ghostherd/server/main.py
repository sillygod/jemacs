"""FastAPI entry point.  JSON-RPC 2.0 over HTTP POST /jsonrpc, like ecloud."""

from __future__ import annotations

import asyncio
import json

from contextlib import asynccontextmanager

from fastapi import FastAPI, Request, Response
from fastapi.middleware.cors import CORSMiddleware
from pydantic import ValidationError

from config import get_config
from engine import reset_engine
from jsonrpc_handler import (
    INVALID_REQUEST,
    PARSE_ERROR,
    JsonRpcError,
    JsonRpcRequest,
    JsonRpcResponse,
    handler,
)

@asynccontextmanager
async def lifespan(_app: FastAPI):
    cfg = get_config()
    stop = asyncio.Event()
    task = None
    if cfg.telegram_token and cfg.telegram_chat_ids:
        from telegram import run_bot

        task = asyncio.create_task(run_bot(stop, cfg), name="ghostherd-telegram")
    try:
        yield
    finally:
        stop.set()
        if task is not None:
            task.cancel()
            try:
                await task
            except (asyncio.CancelledError, Exception):
                pass
        reset_engine()


app = FastAPI(
    title="ghostherd-memory",
    description="Local transcript memory for ghostherd (Qdrant + FastEmbed)",
    version="0.1.0",
    lifespan=lifespan,
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/health")
async def health_check() -> dict:
    return {"status": "ok", "version": "0.1.0", "service": "ghostherd-memory"}


@app.post("/jsonrpc")
async def jsonrpc_endpoint(request: Request) -> Response:
    try:
        data = json.loads(await request.body())
    except json.JSONDecodeError as exc:
        error_response = JsonRpcResponse(
            error=JsonRpcError(code=PARSE_ERROR, message=f"Parse error: {exc}")
        )
        return Response(
            content=error_response.model_dump_json(exclude_none=True),
            media_type="application/json; charset=utf-8",
        )

    if isinstance(data, list):
        responses = []
        for item in data:
            resp = await _handle_single_request(item)
            if resp is not None:
                responses.append(resp)
        return Response(
            content=json.dumps(
                [r.model_dump(exclude_none=True) for r in responses],
                ensure_ascii=False,
            ),
            media_type="application/json; charset=utf-8",
        )

    response = await _handle_single_request(data)
    if response is None:
        return Response(status_code=204)
    return Response(
        content=response.model_dump_json(exclude_none=True),
        media_type="application/json; charset=utf-8",
    )


async def _handle_single_request(data: dict) -> JsonRpcResponse | None:
    try:
        rpc_request = JsonRpcRequest(**data)
    except (ValidationError, TypeError) as exc:
        return JsonRpcResponse(
            id=data.get("id") if isinstance(data, dict) else None,
            error=JsonRpcError(
                code=INVALID_REQUEST,
                message=f"Invalid request: {exc}",
            ),
        )
    is_notification = rpc_request.id is None
    response = await handler.handle(rpc_request)
    if is_notification:
        return None
    return response


if __name__ == "__main__":
    import uvicorn

    cfg = get_config()
    uvicorn.run("main:app", host=cfg.host, port=cfg.port, reload=False)
