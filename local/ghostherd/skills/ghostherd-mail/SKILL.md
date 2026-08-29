---
name: ghostherd-mail
description: Send work to another ghostherd agent via the sidecar JSON-RPC URL in GHOSTHERD_RPC. Use when handing research, reviews, or follow-up to a sibling session (claude-research → grok-dev, implementer → reviewer) without the ghostherd shell binary.
---

# ghostherd-mail

You are one named session in an Emacs ghostherd. Siblings are other
agents on the same machine. Talk to them over HTTP JSON-RPC — not
`bin/ghostherd`, not `emacsclient`.

## Environment

- `GHOSTHERD_SESSION` — your name (use this as `from`)
- `GHOSTHERD_RPC` — `http://127.0.0.1:<port>/jsonrpc`
- If that env is missing or stale, read `ghostherd-mail/rpc.url`
  under Emacs's `user-emacs-directory`.

## List siblings

```bash
curl -sS "$GHOSTHERD_RPC" -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"herd_list","params":{}}'
```

## Send

```bash
curl -sS "$GHOSTHERD_RPC" -H 'Content-Type: application/json' \
  -d "$(python3 - <<'PY'
import json, os
print(json.dumps({
  "jsonrpc": "2.0",
  "id": 1,
  "method": "herd_message",
  "params": {
    "from": os.environ["GHOSTHERD_SESSION"],
    "to": "grok-dev",
    "body": "Findings:\n- ...",
    "handoff": True
  }
}))
PY
)"
```

`handoff: true` asks Emacs to watch the target until it settles and
notify the human. Omit it (or false) for fire-and-forget.

Emacs pastes into the other agent's prompt. If that agent is
`working`, the sidecar queues until it is idle. Do not retry in a
busy loop.

## Long bodies

The TUI only gets a truncated paste. The full text is on the sidecar:

```bash
curl -sS "$GHOSTHERD_RPC" -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"herd_inbox","params":{"session":"grok-dev"}}'
```
