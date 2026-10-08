"""Who may call the sidecar: whoever can read rpc.token.

Loopback keeps other machines out, not other origins.  Any web page
open in a browser on this machine can POST to 127.0.0.1, and before
the token one could read the whole transcript index (CORS allowed
every origin) or queue a prompt that Emacs would paste into an agent
and submit.  The token lives in the data directory, mode 0600; the
URL handed to agents carries it, so they call "$GHOSTHERD_RPC" as
before.
"""

from __future__ import annotations

import hashlib
import hmac
import logging
import os
import re
import secrets
from pathlib import Path

TOKEN_FILE = "rpc.token"


def load_or_create_token(data_dir: Path) -> str:
    """The token in DATA_DIR/rpc.token, created on first start.

    Created here rather than by Emacs because Python has a CSPRNG at
    hand; Emacs reads the file once /health answers.  O_EXCL so two
    starts racing cannot each write a different token.
    """
    data_dir.mkdir(parents=True, exist_ok=True)
    path = data_dir / TOKEN_FILE
    try:
        fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    except FileExistsError:
        os.chmod(path, 0o600)
        token = path.read_text(encoding="utf-8").strip()
        if not token:
            raise RuntimeError(f"{path} is empty; delete it and restart the sidecar")
        return token
    token = secrets.token_hex(32)
    with os.fdopen(fd, "w", encoding="utf-8") as f:
        f.write(token)
    return token


def fingerprint(token: str) -> str:
    """A public name for TOKEN, for /health.

    Lets Emacs tell its own sidecar from a stale one (another token, or
    one from before tokens) in the same request it already makes.
    """
    return hashlib.sha256(token.encode("utf-8")).hexdigest()[:12]


def token_ok(given: str, token: str) -> bool:
    return hmac.compare_digest(given.encode("utf-8"), token.encode("utf-8"))


_TOKEN_PATH = re.compile(r"^/jsonrpc/[^/?#]+")


class RedactToken(logging.Filter):
    """Keep the token out of uvicorn's access log.

    That log lands in *ghostherd-memory*, which is the buffer one pastes
    to an agent when something is wrong.
    """

    def filter(self, record: logging.LogRecord) -> bool:
        args = record.args
        if isinstance(args, tuple) and len(args) >= 3 and isinstance(args[2], str):
            redacted = _TOKEN_PATH.sub("/jsonrpc/…", args[2])
            if redacted != args[2]:
                record.args = args[:2] + (redacted,) + args[3:]
        return True
