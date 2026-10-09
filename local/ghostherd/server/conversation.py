"""An agent's conversation as it goes, read from the tail of its transcript.

The herd page's room shows a project's agents side by side: what was
said to each -- by you, or by another agent through the herd -- and
what it answered, its tool calls folded to a line.  Emacs names the
transcript (its hooks did, or the conversation it was started on) and
asks again only when the file has changed; this reads the last part of
it, so a long conversation costs what a short one does.

Only files under the CLIs' own transcript directories are read: the
path comes over the RPC, and this is not a way to read anything else.
"""

from __future__ import annotations

import json
import os
import re
import shlex
from pathlib import Path
from typing import Any

from importers.textutil import content_to_text, grok_user_text

TAIL_BYTES = 768 * 1024
# A turn of tool calls can write megabytes of output: read further back,
# up to this, until there are enough entries to show.
TAIL_MAX = 8 * 1024 * 1024
LIMIT = 60
TEXT_MAX = 12000
HINT_MAX = 160
CACHE_MAX = 64

# Where each CLI keeps its transcripts, under the home directory.
ROOTS = (
    (".claude/projects", "claude"),
    (".gemini/antigravity-cli", "agy"),
    (".grok/sessions", "grok"),
)

# What the herd writes into an agent, as ghostherd.el frames it:
# `ghostherd-message-template', `ghostherd--ask-text' and the sidecar's
# own mail of an answer (herd.py, `_mail_back').  Change them together.
MESSAGE = re.compile(r"\A\[ghostherd message from (.+?) → (.+?)\]\n?(.*)\Z", re.S)
ASK = re.compile(r"\A\[ask ([0-9A-Za-z]{4,32}) -- [^\]\n]*\]\n?(.*)\Z", re.S)
ASK_TAIL = "\n\nWhen you are done, send your answer with:"
ANSWER = re.compile(
    r"\AAnswer to ask ([0-9A-Za-z]{4,32}) \((.*?)\)( -- no answer was sent[^:\n]*)?:\n(.*)\Z", re.S
)
FAILED = re.compile(r"\AAsk ([0-9A-Za-z]{4,32}) failed: (.*)\Z", re.S)

# The herd client in a shell command: `"$GHOSTHERD_HERD" ask qa --wait
# <<'HERD'', `/path/bin/herd reply 1a2b3c4d "done"'.
HERD_CALL = re.compile(r"""(?:^|[\s;&|("'/_{])herd["'}]*\s+(ask|reply)\s""", re.I)
HEREDOC = re.compile(r"""<<-?\s*(['"]?)([A-Za-z_][A-Za-z0-9_]*)\1""")
CONTROL = {"|", "||", "&&", ";", "&", "|&", ";;", "(", ")"}
TARGET = re.compile(r"\A[A-Za-z0-9][\w.@-]*\Z")  # an agent, a kind or an ask id

# What the client says around an answer (bin/herd's `say', on stderr),
# and how Claude Code reports a command that failed or went to the
# background -- where the answer is in the output file, read later.
ASKED = re.compile(r"^herd: asked \S+ \(ask ([0-9A-Za-z]{4,32})\)$", re.M)
FAILED_RUN = re.compile(r"\AExit code (\d+)\n")
BACKGROUND = re.compile(
    r"\ACommand running in background with ID: \S+? Output is being written to: (\S+?)\.?(?:\s|\Z)"
)
# Ends the output file -- though the reading command may print on after it.
EXITED = re.compile(r"\n*^\[exited with code (\d+)\]$", re.M)
LINE_NO = re.compile(r"^ *\d+(?:→|\t)", re.M)  # the Read tool's

SYSTEM_REMINDER = re.compile(r"<system-reminder>.*?</system-reminder>", re.S)
# Claude Code keeps a paste in its transcript wrapped, the herd's too.
PASTED = re.compile(r'<pasted_content((?:\s+[\w-]+="[^"]*")*)>\n?(.*?)\n?</pasted_content(?:\1)?>', re.S)
TAG = re.compile(r"<([a-z][a-z-]*)>(.*?)</\1>", re.S)
USER_REQUEST = re.compile(r"<USER_REQUEST>(.*?)</USER_REQUEST>", re.S)

_cache: dict[str, tuple[int, int, dict[str, Any]]] = {}


def clip(text: str, limit: int = TEXT_MAX) -> str:
    text = text.strip()
    return text if len(text) <= limit else text[:limit].rstrip() + "\n…"


def first_line(text: str, limit: int = HINT_MAX) -> str:
    line = next((l.strip() for l in text.splitlines() if l.strip()), "")
    return line if len(line) <= limit else line[: limit - 1].rstrip() + "…"


def locate(path: str) -> tuple[Path, str]:
    """PATH, resolved, and the CLI that wrote it; ValueError otherwise."""
    try:
        real = Path(os.path.expanduser(path)).resolve(strict=True)
    except (OSError, RuntimeError):
        raise ValueError("no such transcript") from None
    if real.suffix == ".jsonl" and real.is_file():
        home = Path(os.path.expanduser("~")).resolve()
        for root, cli in ROOTS:
            if real.is_relative_to(home / root):
                return real, cli
    raise ValueError("not a transcript")


def _tail(path: Path, window: int) -> tuple[list[str], bool]:
    "PATH's last WINDOW bytes as lines, and whether earlier ones were left out."
    with path.open("rb") as f:
        f.seek(0, 2)
        size = f.tell()
        f.seek(max(0, size - window))
        data = f.read()
    lines = data.decode("utf-8", "replace").splitlines()
    cut = size > window
    if cut:
        lines = lines[1:]  # begun mid-line
    return [l for l in lines if l.strip()], cut


def _records(lines):
    for line in lines:
        try:
            o = json.loads(line)
        except ValueError:
            continue
        if isinstance(o, dict):
            yield o


def hint(args: Any) -> str:
    "One line saying what a tool call does, from its arguments."
    if isinstance(args, str):
        try:
            args = json.loads(args)
        except ValueError:
            return first_line(args)
    if not isinstance(args, dict):
        return ""
    for key in ("toolSummary", "description", "command", "CommandLine", "file_path",
                "target_file", "AbsolutePath", "path", "pattern", "Query", "query",
                "url", "skill", "prompt"):
        v = args.get(key)
        if isinstance(v, str) and v.strip():
            return first_line(v)
    return ""


def _words(line: str) -> list[str]:
    "LINE's words up to its first control operator, its redirections left out."
    lex = shlex.shlex(line, posix=True, punctuation_chars=True)
    lex.whitespace_split = True
    try:
        tokens = list(lex)
    except ValueError:
        tokens = line.split()
    words: list[str] = []
    skip = False
    for t in tokens:
        if skip:
            skip = False
        elif t in CONTROL:
            break
        elif set(t) <= set("<>&") and ("<" in t or ">" in t):
            if words and words[-1].isdigit():
                words.pop()  # the 2 of 2>&1
            skip = True
        else:
            words.append(t)
    return words


def herd_call(command: str) -> dict[str, Any] | None:
    """An ask or a reply through the herd client, from a shell COMMAND.

    The words on the command's first line, then the body: a here-document
    when there is one, else the text argument.  Asking the client for its
    usage is not an ask.
    """
    m = HERD_CALL.search(command or "")
    if not m:
        return None
    verb = m.group(1).lower()
    rest = command[m.end():]
    line, _, after = rest.partition("\n")
    body = None
    doc = HEREDOC.search(line)
    if doc:
        line = line[: doc.start()]
        end = re.search(r"^\s*" + re.escape(doc.group(2)) + r"\s*$", after, re.M)
        body = after[: end.start()] if end else after
    words = _words(line)
    if "-h" in words or "--help" in words:
        return None
    positional = []
    skip = False
    for w in words:
        if skip:
            skip = False
        elif w == "--timeout":
            skip = True
        elif not w.startswith("-"):
            positional.append(w)
    if not positional or not TARGET.match(positional[0]):
        return None
    if body is None:
        # One text argument, or `-' for stdin -- which is not shown.
        body = positional[1] if len(positional) > 1 else ""
    out: dict[str, Any] = {"role": "herd", "kind": verb, "text": clip(body)}
    out["to" if verb == "ask" else "ask"] = positional[0]
    return out


def user_entry(text: str, ts: Any) -> dict[str, Any] | None:
    """What was said to the agent: by you, or by the herd for another agent."""
    text = PASTED.sub(lambda m: m.group(2), SYSTEM_REMINDER.sub("", text or "")).strip()
    if not text:
        return None
    entry: dict[str, Any] = {"role": "user", "who": "", "kind": "prompt", "ts": ts}
    m = MESSAGE.match(text)
    if m:
        entry["who"], body = m.group(1), m.group(3).strip()
        if (a := ASK.match(body)):
            entry.update(kind="ask", ask=a.group(1), text=clip(a.group(2).split(ASK_TAIL, 1)[0]))
        elif (a := ANSWER.match(body)):
            entry.update(kind="answer", ask=a.group(1), text=clip(a.group(4)), auto=bool(a.group(3)))
        elif (a := FAILED.match(body)):
            entry.update(kind="answer", ask=a.group(1), text=clip(a.group(2)), failed=True)
        else:
            entry.update(kind="message", text=clip(body))
        return entry
    tags = dict((k, v.strip()) for k, v in TAG.findall(text))
    if "command-name" in tags:
        args = tags.get("command-args", "")
        entry.update(kind="command", text=(tags["command-name"] + " " + args).strip())
    elif "bash-input" in tags:
        entry.update(kind="command", text="! " + tags["bash-input"])
    elif tags.keys() & {"local-command-stdout", "local-command-stderr", "bash-stdout",
                        "bash-stderr", "local-command-caveat"}:
        return None
    elif "task-notification" in tags:
        inner = dict((k, v.strip()) for k, v in TAG.findall(tags["task-notification"]))
        entry.update(kind="note", text=first_line(inner.get("summary") or inner.get("status")
                                                  or "a background task finished"))
    elif text.startswith("[Request interrupted"):
        entry.update(kind="note", text=first_line(text.strip("[]")))
    else:
        entry["text"] = clip(text)
    return entry


def ask_outcome(text: str, error: bool = False, final: bool = True) -> dict[str, Any]:
    """What became of an ask, from the herd client's output: its id, and
    the answer or why there is none.  FINAL is false for the output of a
    background command that has not ended: no answer there is not yet one.
    """
    out: dict[str, Any] = {}
    code = 0
    if (m := FAILED_RUN.match(text)):
        code, text = int(m.group(1)), text[m.end():]
    if (m := EXITED.search(text)):
        code, text, final = int(m.group(1)), text[: m.start()], True
    if (m := ASKED.search(text)):
        out["ask"] = m.group(1)
    lines = text.splitlines()
    said = [l[len("herd: "):] for l in lines if l.startswith("herd: ")]
    body = "\n".join(l for l in lines if not l.startswith("herd: ")).strip()
    if code == 3 or not final:
        out["waiting"] = True  # still open: the answer comes as a message
    elif code or error:
        out["failed"] = True
        why = [l for l in said if not l.startswith("asked ")]
        out["answer"] = clip(why[-1] if why else body or "the herd client failed")
    else:
        out["answer"] = clip(body)
        if any("stopped without answering" in l for l in said):
            out["auto"] = True
    return out


class Thread:
    "Entries in the order they happened, tool calls folded together."

    def __init__(self) -> None:
        self.entries: list[dict[str, Any]] = []
        self._calls: dict[str, dict[str, Any]] = {}
        self._outputs: dict[str, dict[str, Any]] = {}  # background ask's output file

    def user(self, text: str, ts: Any = None) -> None:
        e = user_entry(text, ts)
        if e:
            self.entries.append(e)

    def say(self, text: str, ts: Any = None, key: Any = None) -> None:
        text = (text or "").strip()
        if not text:
            return
        last = self.entries[-1] if self.entries else None
        if key is not None and last and last["role"] == "assistant" and last.get("key") == key:
            last["text"] = clip(last["text"] + "\n\n" + text)
        else:
            self.entries.append({"role": "assistant", "text": clip(text), "ts": ts, "key": key})

    def tool(self, name: str, args: Any, ts: Any = None, call_id: Any = None,
             command: str | None = None) -> None:
        herd = herd_call(command) if command else None
        if herd:
            herd["ts"] = ts
            self.entries.append(herd)
            if call_id:
                self._calls[str(call_id)] = herd
            return
        if call_id and self._outputs:
            said = args if isinstance(args, str) else json.dumps(args, ensure_ascii=False)
            ask = next((a for f, a in self._outputs.items() if f in said), None)
            if ask:
                self._calls[str(call_id)] = ask
        last = self.entries[-1] if self.entries else None
        if not (last and last["role"] == "tools"):
            last = {"role": "tools", "tools": [], "ts": ts}
            self.entries.append(last)
        last["tools"].append({"name": str(name or "tool"), "hint": hint(args)})

    def result(self, call_id: Any, content: Any, error: bool = False) -> None:
        herd = self._calls.pop(str(call_id), None) if call_id else None
        if not (herd and herd["kind"] == "ask"):
            return
        text = content_to_text(content, TEXT_MAX) or ""
        bg = BACKGROUND.match(text)
        if bg:
            herd["waiting"] = True
            self._outputs[bg.group(1)] = herd
            return
        if LINE_NO.match(text):
            text = LINE_NO.sub("", text)
        reading = herd.pop("waiting", False)
        out = ask_outcome(text, error, final=not reading)
        herd.update(out)
        if not (reading and out.get("waiting") and not EXITED.search(text)):
            for f in [f for f, a in self._outputs.items() if a is herd]:
                del self._outputs[f]

    def done(self, limit: int) -> tuple[list[dict[str, Any]], bool]:
        for e in self.entries:
            e.pop("key", None)
        return self.entries[-limit:], len(self.entries) > limit


def _claude(records, thread: Thread) -> None:
    for o in records:
        if o.get("isSidechain") or o.get("isMeta"):
            continue
        msg = o.get("message") if isinstance(o.get("message"), dict) else {}
        content = msg.get("content")
        ts = o.get("timestamp")
        if o.get("type") == "user":
            if isinstance(content, str):
                thread.user(content, ts)
                continue
            blocks = [b for b in content or [] if isinstance(b, dict)]
            results = [b for b in blocks if b.get("type") == "tool_result"]
            for b in results:
                thread.result(b.get("tool_use_id"), b.get("content"), bool(b.get("is_error")))
            if not results:
                thread.user("\n".join(b.get("text", "") for b in blocks if b.get("type") == "text"), ts)
        elif o.get("type") == "assistant":
            for b in content if isinstance(content, list) else []:
                if not isinstance(b, dict):
                    continue
                if b.get("type") == "text":
                    thread.say(b.get("text", ""), ts, key=msg.get("id") or o.get("uuid"))
                elif b.get("type") == "tool_use":
                    inp = b.get("input") if isinstance(b.get("input"), dict) else {}
                    thread.tool(b.get("name"), inp, ts, b.get("id"),
                                inp.get("command") if b.get("name") == "Bash" else None)


def _agy(records, thread: Thread) -> None:
    for o in records:
        kind, ts = o.get("type"), o.get("created_at")
        if kind == "USER_INPUT":
            text = o.get("content") or ""
            m = USER_REQUEST.search(text)
            thread.user(m.group(1) if m else text, ts)
        elif kind == "PLANNER_RESPONSE" and o.get("source") == "MODEL":
            thread.say(o.get("content") or "", ts, key=("step", o.get("step_index")))
            for call in o.get("tool_calls") or []:
                if not isinstance(call, dict):
                    continue
                args = call.get("args") if isinstance(call.get("args"), dict) else {}
                thread.tool(call.get("name"), args, ts, None,
                            args.get("CommandLine") if call.get("name") == "run_command" else None)


def _grok(records, thread: Thread) -> None:
    for o in records:
        kind = o.get("type")
        if kind == "user":
            if o.get("synthetic_reason"):
                continue
            thread.user(grok_user_text(content_to_text(o.get("content"))))
        elif kind == "assistant":
            content = o.get("content")
            thread.say(content if isinstance(content, str) else content_to_text(content))
            for call in o.get("tool_calls") or []:
                if not isinstance(call, dict):
                    continue
                args = call.get("arguments")
                try:
                    parsed = json.loads(args) if isinstance(args, str) else args
                except ValueError:
                    parsed = {}
                command = parsed.get("command") if isinstance(parsed, dict) else None
                thread.tool(call.get("name"), parsed, None, call.get("id"),
                            command if call.get("name") == "run_terminal_command" else None)
        elif kind == "tool_result":
            thread.result(o.get("tool_call_id"), o.get("content"))


READERS = {"claude": _claude, "agy": _agy, "grok": _grok}


def read(path: str, limit: int = LIMIT) -> dict[str, Any]:
    """The last LIMIT entries of the conversation in transcript PATH."""
    real, cli = locate(path)
    st = real.stat()
    key = str(real)
    hit = _cache.get(key)
    if hit and hit[0] == st.st_size and hit[1] == st.st_mtime_ns and hit[2]["limit"] == limit:
        return hit[2]
    window = TAIL_BYTES
    while True:
        lines, cut = _tail(real, window)
        thread = Thread()
        READERS[cli](_records(lines), thread)
        if not cut or len(thread.entries) >= limit or window >= TAIL_MAX:
            break
        window *= 4
    entries, more = thread.done(limit)
    out = {
        "path": key,
        "cli": cli,
        "entries": entries,
        "earlier": cut or more,
        "mtime": st.st_mtime,
        "limit": limit,
    }
    if len(_cache) >= CACHE_MAX:
        _cache.pop(next(iter(_cache)))
    _cache[key] = (st.st_size, st.st_mtime_ns, out)
    return out
