"""An agent's conversation as it goes, read from the tail of its transcript.

The herd page's room shows a project's agents side by side: what was
said to each -- by you, or by another agent through the herd -- and
what it answered, its tool calls folded to a line, and what came back
from the sub-agents it ran.  Emacs names the
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
from datetime import datetime
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
# What opens the user's message to a lead: the herd's words, not theirs
# (`ghostherd--lead-unheard-note', `ghostherd--lead-work-note').  After
# the role, on a CLI that takes it with the first message; just inside
# the tag a CLI wraps a prompt in.
LEAD_NOTE = re.compile(r"(?:^|(?<=>))\[((?:Since you last heard from me|What this project works on)[^\n]*)\n.*?\]\n\n",
                       re.S | re.M)

# The herd client in a shell command: `"$GHOSTHERD_HERD" ask qa --wait
# <<'HERD'', `/path/bin/herd reply 1a2b3c4d "done"'.
HERD_CALL = re.compile(r"""(?:^|[\s;&|("'/_{])herd["'}]*\s+(ask|reply)\s""", re.I)
HEREDOC = re.compile(r"""<<-?\s*(['"]?)([A-Za-z_][A-Za-z0-9_]*)\1""")
CONTROL = {"|", "||", "&&", ";", "&", "|&", ";;", "(", ")"}
TARGET = re.compile(r"\A[A-Za-z0-9][\w.@-]*\Z")  # an agent, a kind or an ask id

# What the client says around an answer (bin/herd's `say', on stderr),
# and how Claude Code reports a command that failed or went to the
# background -- where the answer is in the output file, read later.
ASKED = re.compile(r"^herd: asked \S+ \(ask ([0-9A-Za-z]{4,32})\)(?:, round \d+ of \d+)?$", re.M)
FAILED_RUN = re.compile(r"\AExit code (\d+)\n")
BACKGROUND = re.compile(
    r"\ACommand running in background with ID: \S+? Output is being written to: (\S+?)\.?(?:\s|\Z)"
)
# Ends the output file -- though the reading command may print on after it.
EXITED = re.compile(r"\n*^\[exited with code (\d+)\]$", re.M)
LINE_NO = re.compile(r"^ *\d+(?:→|\t)", re.M)  # the Read tool's

# Claude Code's sub-agents: its Agent tool (Task before it).  One run in
# the background answers its call at once, with its id; what it found
# comes back later as a message from it, then a notice that it stopped --
# queued for the agent as they arrive, read by it when it can.  Each has
# a conversation of its own beside its parent's (subagents.py).
SUB_TOOLS = {"Agent", "Task"}
# Tools that write a file, and the argument that names it -- confirmed from
# real transcripts on this machine: claude Edit/Write (file_path), grok
# search_replace/write (file_path), agy write_to_file / replace_file_content
# / multi_replace_file_content (TargetFile).  claude's MultiEdit and
# NotebookEdit are in no transcript here, so they are left out (a one-line
# addition when a real sample shows their shape).
EDIT_TOOLS = {
    "Edit", "Write",                                        # claude
    "search_replace", "write",                             # grok
    "write_to_file", "replace_file_content",              # agy
    "multi_replace_file_content",
}
EDIT_PATH_KEYS = ("file_path", "TargetFile")
SUB_ID = re.compile(r"\A[A-Za-z0-9_-]{1,64}\Z")
LAUNCHED = re.compile(r"\AAsync agent launched\b.*?\bagentId: ([A-Za-z0-9_-]{1,64})", re.S)
AGENT_MESSAGE = re.compile(r'<agent-message from="([A-Za-z0-9_-]{1,64})">\n?(.*?)(?:</agent-message>|\Z)', re.S)
# The harness's line over a report, whose every line it indents.
HANDBACK = re.compile(r"\A\[Subagent hand-back\][^\n]*\n")
NOTICE = re.compile(r"<task-notification>(.*?)</task-notification>", re.S)
# A notice whose report came as the message before it.
DELIVERED = re.compile(r"\AThis agent's report was delivered to you as a message\b")
# A notice that it stopped with background work of its own still
# running: it goes on when that ends, and no report has come.
INTERIM = re.compile(r"\bthe result below may be interim\b|\AThis agent has not reported yet\b")
STOPPED = re.compile(r'\AAgent "(.*)" \w+\Z', re.S)

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
                # grok's hooks name updates.jsonl, its screen's stream of
                # updates; the conversation is beside it.
                history = real.with_name("chat_history.jsonl")
                if cli == "grok" and real.name != history.name and history.is_file():
                    history = history.resolve()
                    if history.is_relative_to(home / root):
                        real = history
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


def _unquote(v: str) -> str:
    """agy double-encodes its tool arguments: each value is itself a JSON
    string, so a path arrives as '"/x/a.py"', quotes and all.  Peel that one
    layer; claude's and grok's plain values pass through untouched."""
    s = v.strip()
    if len(s) >= 2 and s[0] == '"' and s[-1] == '"':
        try:
            d = json.loads(s)
        except ValueError:
            return s
        if isinstance(d, str):
            return d.strip()
    return s


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
        elif w in ("--timeout", "--rounds"):
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


def notice(text: str) -> dict[str, Any] | None:
    """A task notification in TEXT: which task, how it ended, and for a
    sub-agent what it cost.  None when there is none."""
    m = NOTICE.search(text or "")
    if not m:
        return None
    body = m.group(1)

    def tag(*names: str) -> str:
        for name in names:
            t = re.search(r"<%s>(.*?)</%s>" % (name, name), body, re.S)
            if t:
                return t.group(1).strip()
        return ""

    def num(*names: str) -> int | None:
        v = tag(*names)
        return int(v) if v.isdigit() else None

    return {"id": tag("task-id"), "call": tag("tool-use-id"), "status": tag("status"),
            "summary": tag("summary"), "result": tag("result"),
            "interim": bool(INTERIM.search(tag("note")) or INTERIM.search(tag("result"))),
            "tokens": num("subagent_tokens", "total_tokens"), "uses": num("tool_uses"),
            "ms": num("duration_ms")}


def sub_meta(transcript: Path, sid: str) -> dict[str, Any]:
    """What sub-agent SID of TRANSCRIPT was started as -- its type, its
    description, a name it was given -- from beside its conversation."""
    if not SUB_ID.match(sid or ""):
        return {}
    try:
        m = json.loads((transcript.with_suffix("") / "subagents" / f"agent-{sid}.meta.json").read_text())
    except (OSError, ValueError):
        return {}
    return m if isinstance(m, dict) else {}


def without_herd_notes(text: str) -> str:
    """TEXT, said to a lead, less what the herd put in it."""
    return LEAD_NOTE.sub("", text or "")


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
        notes = []
        while (n := LEAD_NOTE.match(text)):
            notes.append(re.split(r"[.:]", n.group(1), maxsplit=1)[0])
            text = text[n.end():]
        entry["text"] = clip(text)
        if notes:
            entry["notes"] = notes
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

    def __init__(self, transcript: Path | None = None) -> None:
        self.entries: list[dict[str, Any]] = []
        self._calls: dict[str, dict[str, Any]] = {}
        self._outputs: dict[str, dict[str, Any]] = {}  # background ask's output file
        # Sub-agents: each call's line among the tools, by call and then by
        # the id it was started with; a report waiting for its notice; the
        # notices read off the queue, not to be read again when delivered.
        self.transcript = transcript
        self._subcalls: dict[str, dict[str, Any]] = {}
        self._subs: dict[str, dict[str, Any]] = {}
        self._reports: dict[str, dict[str, Any]] = {}
        self._queued: set[str] = set()
        self._metas: dict[str, dict[str, Any]] = {}
        # Absolute paths this conversation edited, in order, deduped: what an
        # edit tool (EDIT_TOOLS) named in its arguments.
        self.edited: list[str] = []

    def user(self, text: str, ts: Any = None) -> None:
        e = user_entry(text, ts)
        if e:
            self.entries.append(e)

    def note(self, text: str, ts: Any = None) -> None:
        "Something that happened to the conversation, said by no one."
        self.entries.append({"role": "user", "who": "", "kind": "note", "text": text, "ts": ts})

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
        line = {"name": str(name or "tool"), "hint": hint(args)}
        last["tools"].append(line)
        if name in EDIT_TOOLS and isinstance(args, dict):
            for k in EDIT_PATH_KEYS:
                p = args.get(k)
                if isinstance(p, str) and p.strip():
                    p = _unquote(p)
                    if p and p not in self.edited:
                        self.edited.append(p)
                    break
        if name in SUB_TOOLS and call_id:
            self._subcalls[str(call_id)] = line

    def result(self, call_id: Any, content: Any, error: bool = False, ts: Any = None) -> None:
        line = self._subcalls.pop(str(call_id), None) if call_id else None
        if line is not None:
            text = content_to_text(content, TEXT_MAX) or ""
            m = LAUNCHED.match(text)
            if m:
                # In the background: the rest comes later, by this id.
                line["sub"] = m.group(1)
                self._subs[m.group(1)] = line
            else:
                # Run while the agent waited: what it answered is its report.
                line["state"] = "failed" if error else "completed"
                self.entries.append({"role": "sub", "kind": "report", "sub": "", "who": "",
                                     "title": line["hint"], "text": clip(text),
                                     "status": line["state"], "ts": ts})
            return
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
        if not out.get("waiting"):
            herd["answered_ts"] = ts
        if not (reading and out.get("waiting") and not EXITED.search(text)):
            for f in [f for f, a in self._outputs.items() if a is herd]:
                del self._outputs[f]

    def _meta(self, sid: str) -> dict[str, Any]:
        if sid not in self._metas:
            self._metas[sid] = sub_meta(self.transcript, sid) if self.transcript else {}
        return self._metas[sid]

    def _sub_entry(self, sid: str, ts: Any, text: str = "") -> dict[str, Any]:
        "Sub-agent SID's report, said where it came back."
        meta = self._meta(sid)
        line = self._subs.get(sid) or {}
        e = {"role": "sub", "kind": "report", "sub": sid,
             "who": str(meta.get("name") or meta.get("agentType") or ""),
             "title": str(meta.get("description") or line.get("hint") or ""),
             "text": clip(text), "ts": ts}
        self.entries.append(e)
        return e

    def queued(self, text: str, ts: Any = None) -> None:
        """What was queued for the agent as it arrived: a sub-agent's report,
        its notice that it stopped.  Anything else is read when delivered."""
        m = AGENT_MESSAGE.search(text)
        if m:
            report = re.sub(r"^  ", "", HANDBACK.sub("", m.group(2).strip("\n"), count=1), flags=re.M)
            self._reports[m.group(1)] = self._sub_entry(m.group(1), ts, report)
            return
        n = notice(text)
        if n and self._sub(n):
            self._queued.add(text.strip())
            self._stopped(n, ts)

    def delivered(self, text: str, ts: Any = None) -> bool:
        """True when TEXT, said to the agent, was a sub-agent's notice: read
        off the queue already, or now.  Any other is said as it is."""
        n = notice(text) if "<task-notification>" in text else None
        if not (n and self._sub(n)):
            return False
        if text.strip() not in self._queued:
            self._stopped(n, ts)
        return True

    def _sub(self, n: dict[str, Any]) -> bool:
        sid = n["id"]
        return bool(sid) and (sid in self._subs or sid in self._reports or bool(self._meta(sid)))

    def _stopped(self, n: dict[str, Any], ts: Any) -> None:
        sid = n["id"]
        if n["interim"]:
            return  # still at work, in the background: no report yet
        if sid in self._subs:
            self._subs[sid]["state"] = n["status"] or "completed"
        e = self._reports.pop(sid, None)
        if e is None:
            result = "" if DELIVERED.match(n["result"]) else n["result"]
            e = self._sub_entry(sid, ts, result)
            if not e["title"] and (m := STOPPED.match(n["summary"])):
                e["title"] = m.group(1)
        e.update(status=n["status"] or "completed", tokens=n["tokens"], uses=n["uses"], ms=n["ms"])

    def done(self, limit: int) -> tuple[list[dict[str, Any]], bool]:
        for e in self.entries:
            e.pop("key", None)
        return self.entries[-limit:], len(self.entries) > limit


def _claude(records, thread: Thread, sub: bool = False) -> None:
    # A sub-agent's own transcript is written entirely as sidechain; read
    # SUB such a one, where that mark is the conversation, not an aside.
    for o in records:
        if (o.get("isSidechain") and not sub) or o.get("isMeta"):
            continue
        msg = o.get("message") if isinstance(o.get("message"), dict) else {}
        content = msg.get("content")
        ts = o.get("timestamp")
        if o.get("type") == "queue-operation":
            if o.get("operation") == "enqueue" and isinstance(o.get("content"), str):
                thread.queued(o["content"], ts)
        elif o.get("type") == "user":
            if o.get("isCompactSummary"):
                thread.note("the conversation was compacted", ts)
                continue
            if isinstance(content, str):
                if not thread.delivered(content, ts):
                    thread.user(content, ts)
                continue
            blocks = [b for b in content or [] if isinstance(b, dict)]
            results = [b for b in blocks if b.get("type") == "tool_result"]
            for b in results:
                thread.result(b.get("tool_use_id"), b.get("content"), bool(b.get("is_error")), ts)
            if not results:
                said = "\n".join(b.get("text", "") for b in blocks if b.get("type") == "text")
                if not thread.delivered(said, ts):
                    thread.user(said, ts)
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
            if o.get("synthetic_reason") == "compaction_meta":
                thread.note("the conversation was compacted")
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


def _read(real: Path, cli: str, limit: int, reader, key: str) -> dict[str, Any]:
    "The last LIMIT entries REAL holds, parsed by READER, cached under KEY."
    st = real.stat()
    hit = _cache.get(key)
    if hit and hit[0] == st.st_size and hit[1] == st.st_mtime_ns and hit[2]["limit"] == limit:
        return hit[2]
    window = TAIL_BYTES
    while True:
        lines, cut = _tail(real, window)
        thread = Thread(real)
        reader(_records(lines), thread)
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
        "edited": list(thread.edited),
    }
    if len(_cache) >= CACHE_MAX:
        _cache.pop(next(iter(_cache)))
    _cache[key] = (st.st_size, st.st_mtime_ns, out)
    return out


def read(path: str, limit: int = LIMIT) -> dict[str, Any]:
    """The last LIMIT entries of the conversation in transcript PATH."""
    real, cli = locate(path)
    return _read(real, cli, limit, READERS[cli], str(real))


def edited(path: str, root: str | None = None, limit: int = LIMIT) -> dict[str, Any]:
    """The files the conversation in transcript PATH edited recently (its
    tail), as a confirmed edit tool named them.  With ROOT, only those under
    it, relative to it, in order -- a worktree's edits never reach another
    checkout's tree -- otherwise the absolute paths as seen."""
    files = (read(path, limit) or {}).get("edited") or []
    if root:
        base = os.path.abspath(os.path.expanduser(root))
        prefix = base if base.endswith(os.sep) else base + os.sep
        rel = []
        for p in files:
            ap = os.path.abspath(os.path.expanduser(p))
            if ap == base or ap.startswith(prefix):
                r = os.path.relpath(ap, base)
                if not r.startswith(".."):
                    rel.append(r)
        files = rel
    seen: set[str] = set()
    return {"files": [f for f in files if not (f in seen or seen.add(f))]}


def sub_path(transcript: Path, sid: str) -> Path:
    "Where sub-agent SID keeps its own conversation, beside its parent's."
    return transcript.with_suffix("") / "subagents" / f"agent-{sid}.jsonl"


def read_sub(path: str, sid: str, limit: int = LIMIT) -> dict[str, Any]:
    """The last LIMIT entries of sub-agent SID's own transcript, under parent
    transcript PATH.  Only claude keeps sub-agent transcripts; SID is held to
    the same shape as a filename and the path resolved under the CLI root, so
    it cannot climb out.  Its records are all sidechain, read as the thread."""
    if not SUB_ID.match(sid or ""):
        raise ValueError("no such sub-agent")
    parent, cli = locate(path)
    if cli != "claude":
        raise ValueError("no sub-agents here")
    real, _ = locate(str(sub_path(parent, sid)))
    return _read(real, "claude", limit,
                 lambda recs, thread: _claude(recs, thread, sub=True),
                 "sub:" + str(real))


# ---- A project's chat ------------------------------------------------------
#
# The room shows each agent's conversation in a column; the chat is the
# same conversations as one timeline, at the level of who said what to
# whom: your prompts, the asks and answers between agents, and how each
# turn of each agent ended.  Its working -- tool calls, what an agent
# says on the way -- stays in the room.

CHAT_LIMIT = 200
CHAT_PER_AGENT = 150


def _epoch(ts: Any) -> float | None:
    if not ts:
        return None
    try:
        return datetime.fromisoformat(str(ts).replace("Z", "+00:00")).timestamp()
    except ValueError:
        return None


def _chat_items(name: str, entries: list[dict[str, Any]], taken: dict[str, str],
                replied: set[str]) -> list[dict[str, Any]]:
    """NAME's ENTRIES as chat items.  An ask is shown as its taker received
    it, and answered by the taker's reply: the asker's side of the same
    ask appears only for what its taker never saw -- an ask that failed
    on the way, or an outcome the herd gave instead of a reply."""
    out: list[dict[str, Any]] = []
    said: dict[str, Any] | None = None

    def add(**item: Any) -> None:
        out.append({k: v for k, v in item.items() if v not in (None, "", False)})

    def turn() -> None:
        nonlocal said
        if said:
            add(who=name, kind="turn", text=said["text"], ts=said.get("ts"))
        said = None

    for e in entries:
        role, kind, ask = e.get("role"), e.get("kind"), e.get("ask")
        if role == "assistant":
            said = e
        elif role == "user":
            turn()
            if kind == "prompt":
                add(who="user", to=name, kind="prompt", text=e.get("text"), ts=e.get("ts"),
                    notes=e.get("notes"))
            elif kind in ("ask", "message"):
                add(who=e.get("who"), to=name, kind=kind, ask=ask, text=e.get("text"), ts=e.get("ts"))
            elif kind == "answer" and ask not in replied:
                add(who=e.get("who"), to=name, kind="answer", ask=ask, text=e.get("text"),
                    auto=e.get("auto"), failed=e.get("failed"), ts=e.get("ts"))
        elif role == "herd" and kind == "reply":
            add(who=name, to=taken.get(ask, ""), kind="reply", ask=ask, text=e.get("text"), ts=e.get("ts"))
        elif role == "herd" and kind == "ask":
            if ask not in taken:
                add(who=name, to=e.get("to"), kind="ask", ask=ask, text=e.get("text"), ts=e.get("ts"),
                    answer=e.get("answer"), auto=e.get("auto"), failed=e.get("failed"),
                    waiting=e.get("waiting"))
            elif ask not in replied and (e.get("answer") or e.get("failed")):
                add(who=e.get("to"), to=name, kind="answer", ask=ask, text=e.get("answer"),
                    auto=e.get("auto"), failed=e.get("failed"), ts=e.get("answered_ts") or e.get("ts"))
    turn()
    return out


def _keys(items: list[dict[str, Any]], mtime: float) -> list[float]:
    """When each of a stream's ITEMS happened.  One without a time (grok
    writes none) has its predecessor's; a stream with none at all ends at
    its file's MTIME, a second apart, which orders it against the others
    only roughly."""
    known = [_epoch(i.get("ts")) for i in items]
    if not any(k is not None for k in known):
        return [mtime - (len(items) - 1 - n) for n in range(len(items))]
    last = next(k for k in known if k is not None)
    keys = []
    for k in known:
        last = k if k is not None else last
        keys.append(last)
    return keys


def chat(agents: list[tuple[str, str]], limit: int = CHAT_LIMIT) -> dict[str, Any]:
    """The conversations of AGENTS -- (name, transcript) pairs -- as one
    timeline, its last LIMIT items, each saying which agent's it is."""
    # Read enough of each agent to fill the merged limit.  Folding turns can
    # only shrink a stream, so for the timeline to reach LIMIT items a column
    # must offer at least that many of its own.  `load earlier' is a bigger
    # LIMIT, so the per-agent read grows with it rather than staying pinned
    # at CHAT_PER_AGENT, where the button loaded nothing past the first look.
    per = max(CHAT_PER_AGENT, limit)
    streams, missing, earlier = [], [], False
    for name, path in agents:
        try:
            r = read(path, per)
        except ValueError:
            missing.append(name)
            continue
        streams.append((name, r))
        earlier = earlier or r["earlier"]
    taken: dict[str, str] = {}
    replied: set[str] = set()
    for name, r in streams:
        for e in r["entries"]:
            if e.get("role") == "user" and e.get("kind") == "ask" and e.get("ask"):
                taken[e["ask"]] = e.get("who") or ""
            elif e.get("role") == "herd" and e.get("kind") == "reply" and e.get("ask"):
                replied.add(e["ask"])
    merged = []
    for order, (name, r) in enumerate(streams):
        items = _chat_items(name, r["entries"], taken, replied)
        for n, (item, key) in enumerate(zip(items, _keys(items, r["mtime"]))):
            item["agent"] = name
            merged.append(((key, order, n), item))
    merged.sort(key=lambda m: m[0])
    entries = [item for _, item in merged]
    return {"entries": entries[-limit:], "earlier": earlier or len(entries) > limit,
            "missing": missing, "limit": limit}
