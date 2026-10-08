"""FastAPI entry point.  JSON-RPC 2.0 over HTTP POST /jsonrpc/<token>.

The ecloud shape (one POST /jsonrpc, loopback) with the token ecloud
does without: see auth.py for why loopback alone is not enough.
"""

from __future__ import annotations

import asyncio
import json
import logging

from contextlib import asynccontextmanager

from fastapi import FastAPI, Request, Response
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from pydantic import ValidationError

from auth import RedactToken, fingerprint, load_or_create_token, token_ok
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

VERSION = "0.2.0"
UNAUTHORIZED = -32001
UNSUPPORTED_MEDIA = -32002

logging.getLogger("uvicorn.access").addFilter(RedactToken())


@asynccontextmanager
async def lifespan(app: FastAPI):
    cfg = get_config()
    app.state.token = load_or_create_token(cfg.data_dir)
    app.state.token_file = str(cfg.data_dir / "rpc.token")
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
    version=VERSION,
    lifespan=lifespan,
)

# No CORS middleware: Emacs and curl never ask, and a browser page is
# exactly the caller to refuse.  The Host check stops DNS rebinding,
# where a page's own hostname resolves to 127.0.0.1 and same-origin
# rules no longer apply.
app.add_middleware(TrustedHostMiddleware, allowed_hosts=["127.0.0.1", "localhost"])


@app.get("/health")
async def health_check(request: Request) -> dict:
    return {
        "status": "ok",
        "version": VERSION,
        "service": "ghostherd-memory",
        "token_id": fingerprint(request.app.state.token),
    }


def _refuse(status: int, code: int, message: str) -> Response:
    body = JsonRpcResponse(error=JsonRpcError(code=code, message=message))
    return Response(
        content=body.model_dump_json(exclude_none=True),
        status_code=status,
        media_type="application/json; charset=utf-8",
    )


def _no_token(request: Request) -> Response:
    # Says where the URL is, never what it is: an agent spawned before
    # the token, holding a bare GHOSTHERD_RPC, can recover from this.
    return _refuse(
        401,
        UNAUTHORIZED,
        "This sidecar needs its token in the URL.  Use $GHOSTHERD_RPC, or "
        "the URL in ghostherd-mail/rpc.url under Emacs's user-emacs-directory "
        "(the token itself is " + request.app.state.token_file + ").",
    )


@app.post("/jsonrpc")
async def jsonrpc_without_token(request: Request) -> Response:
    return _no_token(request)


@app.post("/jsonrpc/{token}")
async def jsonrpc_endpoint(token: str, request: Request) -> Response:
    if not token_ok(token, request.app.state.token):
        return _no_token(request)
    # A cross-origin page can send text/plain without asking first;
    # application/json needs a CORS preflight, which nothing here grants.
    ctype = request.headers.get("content-type", "").split(";")[0].strip().lower()
    if ctype != "application/json":
        return _refuse(415, UNSUPPORTED_MEDIA, "Content-Type must be application/json")
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
