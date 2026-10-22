"""Who may call the sidecar, and what: two tokens.

Loopback keeps other machines out, not other origins.  Any web page
open in a browser on this machine can POST to 127.0.0.1, and before
the token one could read the whole transcript index (CORS allowed
every origin) or queue a prompt that Emacs would paste into an agent
and submit.

rpc.token is the herd's own -- Emacs's, and through it the page's -- and
reaches every method.  rpc.agent-token is the agents': the URL in their
GHOSTHERD_RPC and in the rpc.url they fall back to carries it, and it
reaches only what an agent does (`AGENT_METHODS' in jsonrpc_handler.py)
-- not approving a guard's refusal, not the Memory tab, not the tick.
Before it, the one token was in every agent's environment, so any code
an agent ran could approve its own guard requests.  Both live in the
data directory, mode 0600.
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
AGENT_TOKEN_FILE = "rpc.agent-token"


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


def _write_new(path: Path) -> str:
    "A fresh token in PATH, replacing whatever was there, mode 0600 throughout."
    token = secrets.token_hex(32)
    tmp = path.with_name(path.name + ".new")
    fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w", encoding="utf-8") as f:
        f.write(token)
    os.replace(tmp, path)
    return token


def load_tokens(data_dir: Path) -> tuple[str, str]:
    """(ADMIN, AGENT): the herd's token and the agents'.

    The first start with an agent token -- no rpc.agent-token yet -- also
    replaces the admin token.  Agents spawned before it hold the admin
    URL in their environment; once it is refused, `bin/herd' falls back
    to rpc.url, which Emacs rewrites with the agent URL, and goes on.
    The old admin token is then in no agent's hands.
    """
    admin = load_or_create_token(data_dir)
    path = data_dir / AGENT_TOKEN_FILE
    if path.exists():
        os.chmod(path, 0o600)
        agent = path.read_text(encoding="utf-8").strip()
        if not agent:
            raise RuntimeError(f"{path} is empty; delete it and restart the sidecar")
        return admin, agent
    admin = _write_new(data_dir / TOKEN_FILE)
    return admin, _write_new(path)


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
