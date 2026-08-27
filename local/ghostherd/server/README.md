# ghostherd-memory sidecar

Local FastAPI process that imports claude / grok / agy transcripts into
Qdrant and answers JSON-RPC search. Same shape as ecloud: `POST /jsonrpc`,
`GET /health`, `uv run uvicorn`, bind loopback.

```bash
uv sync
uv run uvicorn main:app --host 127.0.0.1 --port 49152
```

Emacs starts that itself (`M-x ghostherd-memory-start`) and picks a free
port from 49152 upward if 49152 is taken. Tests:

```bash
GHOSTHERD_MEMORY_FAKE_EMBED=1 uv sync --extra dev
uv run pytest
```

Do not import grok `recap_requests/` or `updates.jsonl`.
