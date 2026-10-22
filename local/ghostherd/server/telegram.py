"""Telegram long-poll for the herd.  Optional: no token, no task.

Taps enqueue sqlite commands; Emacs herd_tick runs them.  This module
never talks to a PTY.  Unknown chats are ignored.

Screen (and explain) read the last Emacs snapshot immediately — the
poll already captured the pane.  Typing into a PTY still goes through
Emacs.

Pushes: Emacs decides when something wants you while you are away -- an
agent blocked, a lead's report, an ask that failed -- and hands it over
in herd_tick; this words it and sends it to every allowed chat.  A
reply to a push goes to its project's lead.

Links: the herd page, and each project's room, under the address Emacs
gives in herd_tick -- without the page's secret, so Telegram keeping it
gives nothing away -- for /herd and a session's card to open on the
phone, nothing bookmarked.
"""

from __future__ import annotations

import asyncio
import logging
import os
import re
from datetime import datetime
from typing import Any
from urllib.parse import urlparse

import httpx

from config import Config, get_config
from herd import HerdStore, get_herd
from relay import KIND as RELAY_KIND

log = logging.getLogger("ghostherd.telegram")

API = "https://api.telegram.org/bot{token}/{method}"

GLYPH = {
    "blocked": "⚠",
    "working": "⟳",
    "waiting": "◷",
    "done": "✓",
    "idle": "○",
    "starting": "…",
    "dead": "✖",
}

HELP = (
    "ghostherd bot.\n"
    "/herd — list sessions\n"
    "Tap a session for screen / explain / prompt / interrupt.\n"
    "Blocked sessions also get 1 / 2 / 3 to answer a prompt.\n"
    "/cancel — drop a pending prompt.\n"
    "/herd also opens the herd page, and a session its room, in this "
    "phone's browser: over your tailnet, nothing to bookmark.\n"
    "While you are away from the desk, the herd tells you here: an agent "
    "blocked, a lead's report, an ask that failed.  Reply to one to tell "
    "that project's lead.\n"
    "Anything else you write goes to primary, the herd-wide lead -- started "
    "or resumed if need be: what crosses projects, or where you do not know "
    "it belongs, and \"what is everyone doing?\"."
)

# Seconds between looks for pushes to send.
PUSH_EVERY = 2.0
# Seconds a lead's report waits for the lead's own words: its hook may
# bring them a moment after its state.
REPORT_WAIT = 30.0


def _btn(text: str, data: str) -> dict:
    return {"text": text, "callback_data": data}


def _link(text: str, url: str | None) -> list[dict] | None:
    """A row with a button that opens URL, or None: not http(s) under a
    full name.  Telegram refuses a message whose button it will not
    take, and the whole message goes."""
    try:
        parts = urlparse(url or "")
    except ValueError:
        return None
    if parts.scheme in ("http", "https") and "." in (parts.hostname or ""):
        return [{"text": text, "url": url}]
    return None


def herd_keyboard(sessions: list[dict[str, Any]], page: str = "") -> dict:
    "The herd: the page first, when it has an address; a button a session."
    rows = [row for row in [_link("Open the herd page", page)] if row]
    for s in sessions:
        glyph = GLYPH.get(s.get("state") or "", "?")
        label = f"{glyph} {s['name']}"
        if len(label) > 40:
            label = label[:37] + "…"
        rows.append([_btn(label, f"o:{s['short']}")])
    rows.append([_btn("refresh", "h")])
    return {"inline_keyboard": rows}


def _options(session: dict[str, Any] | None) -> list[dict[str, Any]]:
    "A blocked SESSION's menu choices, as Emacs read them off its screen."
    prompt = (session or {}).get("prompt") or {}
    if (session or {}).get("state") != "blocked" or not isinstance(prompt, dict):
        return []
    return [o for o in prompt.get("options") or [] if isinstance(o, dict) and o.get("n")]


def answer_rows(session: dict[str, Any]) -> list[list[dict]]:
    """A button for each of a blocked SESSION's choices, in its words, a
    row each; none when its menu could not be read, for a number picked
    blind could be any of them."""
    short = session["short"]
    return [[_btn(_cut(f"{o['n']} · {o.get('label') or ''}", 60), f"n:{short}:{o['n']}")]
            for o in _options(session)]


def menu_text(session: dict[str, Any] | None, reason: str = "") -> str:
    """What a blocked SESSION's menu asks, and its choices numbered; its
    REASON when the menu could not be read."""
    options = _options(session)
    question = ((session or {}).get("prompt") or {}).get("question") if options else ""
    lines = [question or reason or ""]
    if options:
        lines.append("\n".join(f"{o['n']}. {o.get('label') or ''}" for o in options))
    return "\n\n".join(line for line in lines if line)


def _cut(text: str, n: int) -> str:
    return text if len(text) <= n else text[: n - 1] + "…"


def session_keyboard(session: dict[str, Any], more: bool = False) -> dict:
    short = session["short"]
    state = session.get("state") or ""
    rows = [
        [_btn("screen", f"sc:{short}"), _btn("explain", f"ex:{short}")],
        [_btn("prompt", f"p:{short}"), _btn("interrupt", f"i:{short}")],
    ]
    if state == "blocked":
        rows.extend(answer_rows(session))
        rows.append([_btn("abort", f"ab:{short}")])
    room = _link("Open the room", session.get("room"))
    if room:
        rows.append(room)
    if more:
        rows.append(
            [
                _btn("respawn?", f"rs:{short}"),
                _btn("kill?", f"k:{short}"),
            ]
        )
        rows.append([_btn("herd", "h")])
    else:
        rows.append([_btn("more", f"m:{short}"), _btn("herd", "h")])
    return {"inline_keyboard": rows}


def confirm_keyboard(short: str, op: str) -> dict:
    yes = "k2" if op == "k" else "rs2"
    return {
        "inline_keyboard": [
            [_btn("confirm", f"{yes}:{short}"), _btn("cancel", f"o:{short}")]
        ]
    }


def format_herd(sessions: list[dict[str, Any]]) -> str:
    counts = {}
    for s in sessions:
        st = s.get("state") or ""
        counts[st] = counts.get(st, 0) + 1
    bits = []
    if counts.get("blocked"):
        bits.append(f"⚠{counts['blocked']}")
    if counts.get("working"):
        bits.append(f"⟳{counts['working']}")
    if counts.get("waiting"):
        bits.append(f"◷{counts['waiting']}")
    extra = "  ".join(bits)
    line = f"Herd  {len(sessions)}"
    if extra:
        line += f"   {extra}"
    if not sessions:
        line += "\n(no sessions — Emacs poll has not synced yet)"
    return line


def format_session(session: dict[str, Any]) -> str:
    glyph = GLYPH.get(session.get("state") or "", "?")
    lines = [
        f"{session['name']} · {session.get('kind') or '?'} · "
        f"{glyph} {session.get('state') or '?'}"
    ]
    if session.get("project"):
        lines.append(session["project"])
    if session.get("notes"):
        lines.append(session["notes"])
    said = menu_text(session, session.get("reason") or "")
    if said:
        lines.append(said)
    return "\n".join(lines)


def format_screen(session: dict[str, Any]) -> str:
    """Last polled pane, plus a one-line state header."""
    body = (session.get("screen") or "").rstrip()
    if not body:
        return ""
    glyph = GLYPH.get(session.get("state") or "", "?")
    head = (
        f"{session['name']} · {glyph} {session.get('state') or '?'}"
    )
    return f"{head}\n\n{body}"


def _clip(text: str, n: int = 3500) -> str:
    if len(text) > n:
        return text[:n] + "\n…"
    return text


# ---- Markdown to Telegram HTML ---------------------------------------
#
# Telegram pushes are sent with parse_mode=HTML (less escaping than
# MarkdownV2).  A lead's report -- and any other agent prose -- is written
# in Markdown; this turns it into the small tag set Telegram renders, so
# **原因：**, `code' and "- " lists read as formatted text instead of their
# markers.  Plain text is escaped (& < >).  What is not markup stays as
# typed.  Markdown is clipped before converting, so a cut never lands in
# the middle of a tag or an entity, and the converter only ever emits
# balanced tags.

TG_LIMIT = 4096  # Telegram's message-length ceiling.


def _esc(s: str) -> str:
    "Escape the three characters Telegram's HTML parser treats as markup."
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def _esc_attr(s: str) -> str:
    return _esc(s).replace('"', "&quot;")


_CODE = re.compile(r"`([^`\n]+)`")
_LINK = re.compile(r"\[([^\]\n]*)\]\(([^)\s]+)\)")
# **bold** / __bold__: the span starts and ends on a non-space, so a bare
# marker or spaced-out asterisks (2 * 3 * 4) are left alone; __ wants a
# non-word boundary too, so snake_case is not italicised, while **原因：**
# right against CJK still matches.
_BOLD1 = re.compile(r"\*\*(?=\S)(.+?)(?<=\S)\*\*", re.S)
_BOLD2 = re.compile(r"(?<![0-9A-Za-z])__(?=\S)(.+?)(?<=\S)__(?![0-9A-Za-z])", re.S)
_IT1 = re.compile(r"\*(?=\S)(.+?)(?<=\S)\*", re.S)
_IT2 = re.compile(r"(?<![0-9A-Za-z])_(?=\S)(.+?)(?<=\S)_(?![0-9A-Za-z])", re.S)
_HEADING = re.compile(r"^\s{0,3}#{1,6}\s+(.*?)\s*#*\s*$")
_LIST = re.compile(r"^\s*[-*]\s+(.*)$")
_FENCE = re.compile(r"^\s*(?:```|~~~)(\w*)\s*$")
_FENCE_END = re.compile(r"^\s*(?:```|~~~)\s*$")
_TABLE_SEP = re.compile(r"^\s*\|?[\s:|-]*-[\s:|-]*\|?\s*$")


def _inline(s: str) -> str:
    "Convert the inline Markdown in one span; escape everything else."
    out: list[str] = []
    i, n = 0, len(s)
    while i < n:
        m = _CODE.match(s, i)
        if m:
            out.append("<code>" + _esc(m.group(1)) + "</code>")
            i = m.end()
            continue
        m = _LINK.match(s, i)
        if m:
            out.append('<a href="' + _esc_attr(m.group(2)) + '">' + _inline(m.group(1)) + "</a>")
            i = m.end()
            continue
        m = _BOLD1.match(s, i) or _BOLD2.match(s, i)
        if m:
            out.append("<b>" + _inline(m.group(1)) + "</b>")
            i = m.end()
            continue
        m = _IT1.match(s, i) or _IT2.match(s, i)
        if m:
            out.append("<i>" + _inline(m.group(1)) + "</i>")
            i = m.end()
            continue
        out.append(_esc(s[i]))
        i += 1
    return "".join(out)


def md_to_html(md: str) -> str:
    "Markdown to the HTML subset Telegram renders."
    lines = md.split("\n")
    out: list[str] = []
    i, n = 0, len(lines)
    while i < n:
        line = lines[i]
        fence = _FENCE.match(line)
        if fence:  # ``` ... ``` -> <pre>, code taken literally
            body: list[str] = []
            i += 1
            while i < n and not _FENCE_END.match(lines[i]):
                body.append(lines[i])
                i += 1
            i += 1  # the closing fence, if any
            code = _esc("\n".join(body))
            lang = fence.group(1)
            if lang:
                out.append(f'<pre><code class="language-{_esc(lang)}">{code}</code></pre>')
            else:
                out.append(f"<pre>{code}</pre>")
            continue
        if "|" in line and i + 1 < n and _TABLE_SEP.match(lines[i + 1]):
            block: list[str] = []  # a table -> a <pre> block, kept monospace
            while i < n and "|" in lines[i]:
                block.append(lines[i])
                i += 1
            out.append("<pre>" + _esc("\n".join(block)) + "</pre>")
            continue
        if line.lstrip().startswith(">"):
            quote: list[str] = []  # > quote -> <blockquote>
            while i < n and lines[i].lstrip().startswith(">"):
                quote.append(_inline(re.sub(r"^\s*>\s?", "", lines[i])))
                i += 1
            out.append("<blockquote>" + "\n".join(quote) + "</blockquote>")
            continue
        h = _HEADING.match(line)
        if h:  # # heading -> a bold line
            out.append("<b>" + _inline(h.group(1)) + "</b>")
            i += 1
            continue
        li = _LIST.match(line)
        if li:  # "- "/"* " -> "• "; numbered items are left as typed
            out.append("• " + _inline(li.group(1)))
            i += 1
            continue
        out.append(_inline(line))
        i += 1
    return "\n".join(out)


def to_html(text: str, limit: int = TG_LIMIT) -> str:
    """TEXT (Markdown) as Telegram HTML, within LIMIT.  The Markdown is
    clipped before it is converted, so a cut never splits a tag or an
    entity.  Escaping and tags can lengthen the text, so the cut is chosen
    by the converted length (a search over how much source fits), not the
    source length."""
    html = md_to_html(text)
    if len(html) <= limit:
        return html
    lo, hi, best = 0, len(text), md_to_html(_clip(text, 0))
    while lo <= hi:
        mid = (lo + hi) // 2
        cand = md_to_html(_clip(text, mid))
        if len(cand) <= limit:
            best, lo = cand, mid + 1
        else:
            hi = mid - 1
    return best


def _strip_html(html: str) -> str:
    "HTML back to plain text, for the message that Telegram would not parse."
    text = re.sub(r"<[^>]+>", "", html)
    return (text.replace("&lt;", "<").replace("&gt;", ">")
            .replace("&quot;", '"').replace("&amp;", "&"))


def _parse_error(resp: Any) -> bool:
    "A 400 from Telegram that means it could not parse our entities."
    if getattr(resp, "status_code", 200) != 400:
        return False
    try:
        desc = ((resp.json() or {}).get("description") or "").lower()
    except Exception:
        desc = ""
    return "parse" in desc or "entit" in desc or "tag" in desc


def _leaf(project: str | None) -> str:
    "A project's last directory: what its room is called."
    return os.path.basename((project or "").rstrip("/")) or (project or "")


def format_push(push: dict[str, Any], words: str = "", session: dict[str, Any] | None = None) -> str:
    """PUSH as a message.  WORDS, for a lead's report, are the lead's own
    last message; SESSION, for a blocked agent, is it as last seen, its
    menu read off its screen."""
    kind = push.get("kind")
    name = push.get("session") or "?"
    where = _leaf(push.get("project"))
    head = f" · {where}" if where else ""
    if kind == "blocked":
        return f"⚠ {name} is blocked{head}\n\n{menu_text(session, push.get('reason') or '')}".rstrip()
    if kind == "report":
        whom = "primary" if session and session.get("role") == "primary" else "the lead"
        return (f"✓ {name}{head}\n\n{words or '(its words have not come in)'}"
                f"\n\n↩ Reply to tell {whom}.")
    if kind == "test":
        return ("The herd reaches you here.\n\nWhile you are away from the desk it "
                "tells you here when an agent is blocked, a lead reports or an ask "
                "fails.  Reply to one of those to tell that project's lead.")
    if kind == "used-up":
        to = push.get("to") or "another CLI"
        return (f"⌛ {name} stopped{head}\n\n{_sentence(push.get('reason') or 'Its usage is used up')}"
                f"\n\nRelay it to {to}: a {to} agent takes its name and its work.")
    if kind == "lead-started":
        # A lead the herd started in a project for a handoff from another.
        src = _leaf(push.get("from"))
        lines = [f"▶ started {name}{head}",
                 f"For a handoff from {push.get('by') or '?'}"
                 + (f" in {src}: work found there that belongs in this project."
                    if src else ": work that belongs in this project.")]
        if push.get("head"):
            lines.append("It asks: " + push["head"])
        lines.append("↩ Reply to tell the lead.")
        return "\n\n".join(lines)
    if kind == "ask-failed":
        lines = [f"✖ {name}'s ask to {push.get('to') or '?'} failed{head}"]
        for part in (push.get("error"), push.get("head") and "It asked: " + push["head"]):
            if part:
                lines.append(part)
        return "\n\n".join(lines)
    return ""


def _sentence(text: str) -> str:
    text = text.strip()
    return text[:1].upper() + text[1:] + ("" if text.endswith((".", "!", "?")) else ".")


def push_keyboard(push: dict[str, Any], session: dict[str, Any] | None = None) -> dict | None:
    """A blocked agent's answers and screen, when it is still in the
    herd; a used-up one's relay; the room, when the push has its address."""
    rows = []
    if push.get("kind") == "blocked" and session:
        rows.extend(answer_rows(session))
        rows.append([_btn("screen", f"sc:{session['short']}")])
    if push.get("kind") == "used-up" and session and RELAY_KIND.match(push.get("to") or ""):
        rows.append([_btn(f"Relay to {push['to']}", f"rl:{session['short']}:{push['to']}"),
                     _btn("screen", f"sc:{session['short']}")])
    room = _link("Open the room", push.get("url"))
    if room:
        rows.append(room)
    return {"inline_keyboard": rows} if rows else None


def _epoch(iso: str | None) -> float:
    try:
        return datetime.fromisoformat(iso or "").timestamp()
    except ValueError:
        return 0.0


class TelegramBot:
    def __init__(
        self,
        token: str,
        chat_ids: tuple[int, ...],
        herd: HerdStore | None = None,
    ):
        self.token = token
        self.chat_ids = set(chat_ids)
        self.herd = herd if herd is not None else get_herd()
        self.pending_prompt: dict[int, str] = {}
        self.offset = 0

    def allowed(self, chat_id: int | None) -> bool:
        return chat_id is not None and chat_id in self.chat_ids

    def handle_message(
        self, chat_id: int, text: str, reply_to: int | None = None
    ) -> dict[str, Any] | None:
        """Return a sendMessage payload, or None to ignore.  REPLY_TO is
        the id of the message TEXT replies to."""
        if not self.allowed(chat_id):
            log.info("telegram ignored chat %s", chat_id)
            return None
        text = (text or "").strip()
        if text in ("/cancel", "cancel"):
            self.pending_prompt.pop(chat_id, None)
            return {"chat_id": chat_id, "text": "cancelled"}
        project = self.herd.pushed_project(chat_id, reply_to) if reply_to else None
        if project and text:
            # A reply to primary's own report goes back to it; to any other
            # push, to that project's lead.
            primary = self.herd.primary()
            if primary and primary.get("project") == project:
                return self._to_primary(chat_id, text)
            self.herd.enqueue_command(
                "lead-say", project, args={"project": project, "body": text}, chat_id=chat_id
            )
            return {"chat_id": chat_id, "text": f"→ the lead of {_leaf(project)}"}
        if chat_id in self.pending_prompt:
            session = self.pending_prompt.pop(chat_id)
            self.herd.enqueue(
                to=session,
                body=text,
                from_name="telegram",
                submit=True,
            )
            return {
                "chat_id": chat_id,
                "text": f"queued prompt → {session}",
            }
        if text in ("/start", "/help", "Help", "help"):
            return {
                "chat_id": chat_id,
                "text": HELP,
                "reply_markup": {
                    "keyboard": [[{"text": "Herd"}, {"text": "Help"}]],
                    "resize_keyboard": True,
                },
            }
        if text in ("/herd", "Herd", "herd"):
            return self._herd_message(chat_id)
        # Anything else, not a command: for primary, the herd-wide lead.
        if text and not text.startswith("/"):
            return self._to_primary(chat_id, text)
        return {
            "chat_id": chat_id,
            "text": "unknown. /herd or /help",
        }

    def _to_primary(self, chat_id: int, text: str) -> dict[str, Any]:
        """TEXT for primary: Emacs starts or resumes it when it is not up,
        and tells it once it is ready."""
        self.herd.enqueue_command("primary-say", "primary", args={"body": text}, chat_id=chat_id)
        up = self.herd.primary()
        return {"chat_id": chat_id,
                "text": "→ primary" if up and up.get("state") != "dead"
                else "→ primary (starting it; it reads this once it is up)"}

    def handle_callback(self, chat_id: int, data: str) -> dict[str, Any] | None:
        if not self.allowed(chat_id):
            log.info("telegram ignored chat %s", chat_id)
            return None
        data = data or ""
        if data == "h":
            return self._herd_message(chat_id)
        if data.startswith("o:"):
            return self._session_message(chat_id, data[2:])
        if data.startswith("m:"):
            return self._session_message(chat_id, data[2:], more=True)
        if data.startswith("p:"):
            name = self.herd.name_for(data[2:])
            if not name:
                return {"chat_id": chat_id, "text": "unknown session"}
            self.pending_prompt[chat_id] = name
            return {
                "chat_id": chat_id,
                "text": f"next message goes to {name} as a prompt. /cancel to abort.",
            }
        if data.startswith("n:"):
            rest = data[2:]
            short, _, n = rest.rpartition(":")
            try:
                choice = int(n)
            except ValueError:
                return {"chat_id": chat_id, "text": "bad answer"}
            return self._enqueue(chat_id, "answer", short, {"n": choice})
        if data.startswith("sc:"):
            return self._snapshot_view(chat_id, data[3:], "screen")
        if data.startswith("ex:"):
            return self._snapshot_view(chat_id, data[3:], "explain")
        if data.startswith("i:"):
            return self._enqueue(chat_id, "interrupt", data[2:])
        if data.startswith("ab:"):
            return self._enqueue(chat_id, "abort", data[3:])
        if data.startswith("k2:"):
            return self._enqueue(chat_id, "kill", data[3:])
        if data.startswith("rs2:"):
            return self._enqueue(chat_id, "respawn", data[4:])
        if data.startswith("rl:"):
            short, _, kind = data[3:].rpartition(":")
            if not RELAY_KIND.match(kind):
                return {"chat_id": chat_id, "text": "bad relay"}
            return self._enqueue(chat_id, "relay", short, {"kind": kind})
        if data.startswith("k:"):
            short = data[2:]
            name = self.herd.name_for(short) or short
            return {
                "chat_id": chat_id,
                "text": f"kill {name}?",
                "reply_markup": confirm_keyboard(short, "k"),
            }
        if data.startswith("rs:"):
            short = data[3:]
            name = self.herd.name_for(short) or short
            return {
                "chat_id": chat_id,
                "text": f"respawn {name}?",
                "reply_markup": confirm_keyboard(short, "rs"),
            }
        return {"chat_id": chat_id, "text": "unknown tap"}

    def _herd_message(self, chat_id: int) -> dict[str, Any]:
        sessions = self.herd.list_sessions()["sessions"]
        return {
            "chat_id": chat_id,
            "text": format_herd(sessions),
            "reply_markup": herd_keyboard(sessions, self.herd.page_url()),
        }

    def _session_message(
        self, chat_id: int, short: str, more: bool = False
    ) -> dict[str, Any]:
        name = self.herd.name_for(short)
        session = self.herd.session(name) if name else None
        if not session:
            return {"chat_id": chat_id, "text": "session not in last snapshot"}
        return {
            "chat_id": chat_id,
            "text": format_session(session),
            "reply_markup": session_keyboard(session, more=more),
        }

    def _enqueue(
        self,
        chat_id: int,
        op: str,
        short: str,
        args: dict | None = None,
    ) -> dict[str, Any]:
        name = self.herd.name_for(short)
        if not name:
            return {"chat_id": chat_id, "text": "unknown session"}
        session = self.herd.session(name)
        if op == "answer" and (not session or session.get("state") != "blocked"):
            return {
                "chat_id": chat_id,
                "text": f"{name} is not blocked; answer ignored",
            }
        self.herd.enqueue_command(op, name, args=args, chat_id=chat_id)
        return {"chat_id": chat_id, "text": f"{op} → {name}"}

    def _snapshot_view(
        self, chat_id: int, short: str, kind: str
    ) -> dict[str, Any]:
        """Serve screen/explain from the last Emacs poll.

        No PTY round-trip.  If this snapshot has no pane yet, fall
        back to enqueue so Emacs captures once.
        """
        name = self.herd.name_for(short)
        session = self.herd.session(name) if name else None
        if not session:
            return {"chat_id": chat_id, "text": "session not in last snapshot"}
        if kind == "screen":
            body = format_screen(session)
            if not body:
                return self._enqueue(chat_id, "screen", short)
            # The pane is terminal output, not prose: keep it monospace in a
            # <pre>, only escaped, never run through the Markdown converter.
            text = "<pre>" + _esc(_clip(body)) + "</pre>"
        else:
            glyph = GLYPH.get(session.get("state") or "", "?")
            # explain is the agent's own reason -- prose, so Markdown.
            text = to_html(
                f"{glyph} {session.get('state') or '?'}\n"
                f"{session.get('reason') or '—'}"
            )
        return {
            "chat_id": chat_id,
            "text": text,
            "parse_mode": "HTML",
            "reply_markup": session_keyboard(session),
        }

    def prepare_push(
        self, push: dict[str, Any], now: float
    ) -> tuple[str, dict | None] | None:
        """PUSH as (text, keyboard); None while a lead's report waits for
        its words.  An empty text drops it: a blocked agent answered at
        the desk meanwhile, or a kind this does not know."""
        kind = push.get("kind")
        session = self.herd.session(push.get("session") or "")
        if kind == "blocked" and not (session and session.get("state") == "blocked"):
            return "", None
        # Relayed meanwhile, or gone: nothing to offer.
        if kind == "used-up" and not (session and session.get("kind") == push.get("from")):
            return "", None
        words = ""
        if kind == "report":
            link = self.herd.link(push.get("session") or "") or {}
            fresh = link.get("last") and (link.get("last_ms") or 0) / 1000 >= float(push.get("since") or 0)
            if fresh:
                words = link["last"]
            elif now - _epoch(push.get("created_at")) < REPORT_WAIT:
                return None
        text = format_push(push, words, session)
        return text, (push_keyboard(push, session) if text else None)

    async def _send(self, client: httpx.AsyncClient, url: str, payload: dict) -> Any:
        """sendMessage PAYLOAD.  When it carries HTML that Telegram will not
        parse (400 can't parse entities), resend it as plain text, so a push
        is never lost."""
        resp = await client.post(url.format(method="sendMessage"), json=payload)
        if payload.get("parse_mode") and _parse_error(resp):
            plain = {k: v for k, v in payload.items() if k != "parse_mode"}
            plain["text"] = _strip_html(payload["text"])
            resp = await client.post(url.format(method="sendMessage"), json=plain)
        resp.raise_for_status()
        return resp

    async def _flush_pushes(self, client: httpx.AsyncClient, url: str, now: float | None = None) -> None:
        now = now if now is not None else datetime.now().timestamp()
        done = []
        for push in self.herd.unsent_pushes():
            ready = self.prepare_push(push, now)
            if ready is None:
                continue
            text, markup = ready
            html = to_html(text) if text else ""
            for chat_id in sorted(self.chat_ids) if text else []:
                payload = {"chat_id": chat_id, "text": html, "parse_mode": "HTML"}
                if markup:
                    payload["reply_markup"] = markup
                resp = await self._send(client, url, payload)
                mid = ((resp.json() or {}).get("result") or {}).get("message_id")
                if mid and push.get("project"):
                    self.herd.remember_pushed(chat_id, mid, push["project"])
            done.append(push["id"])
        self.herd.mark_pushes_sent(done)

    async def _push_loop(self, client: httpx.AsyncClient, url: str, stop: asyncio.Event) -> None:
        while not stop.is_set():
            try:
                await self._flush_pushes(client, url)
            except asyncio.CancelledError:
                raise
            except Exception:
                log.exception("telegram push")
            try:
                await asyncio.wait_for(stop.wait(), timeout=PUSH_EVERY)
            except TimeoutError:
                pass

    async def run(self, stop: asyncio.Event) -> None:
        url = API.format(token=self.token, method="{method}")
        timeout = httpx.Timeout(40.0, connect=10.0)
        async with httpx.AsyncClient(timeout=timeout) as client:
            # Pushes go out on their own: the long poll below waits up to
            # 25 seconds for you to say something.
            pushing = asyncio.create_task(self._push_loop(client, url, stop))
            try:
                await self._poll(client, url, stop)
            finally:
                pushing.cancel()

    async def _poll(self, client: httpx.AsyncClient, url: str, stop: asyncio.Event) -> None:
        while not stop.is_set():
            try:
                await self._flush_replies(client, url)
                resp = await client.get(
                    url.format(method="getUpdates"),
                    params={
                        "timeout": 25,
                        "offset": self.offset,
                        "allowed_updates": '["message","callback_query"]',
                    },
                )
                resp.raise_for_status()
                for upd in resp.json().get("result") or []:
                    self.offset = int(upd["update_id"]) + 1
                    await self._dispatch(client, url, upd)
            except asyncio.CancelledError:
                raise
            except Exception:
                log.exception("telegram loop")
                try:
                    await asyncio.wait_for(stop.wait(), timeout=2)
                except TimeoutError:
                    pass

    async def _dispatch(self, client: httpx.AsyncClient, url: str, upd: dict) -> None:
        msg = upd.get("message") or upd.get("edited_message")
        cb = upd.get("callback_query")
        payload = None
        if cb:
            chat_id = (cb.get("message") or {}).get("chat", {}).get("id")
            payload = self.handle_callback(chat_id, cb.get("data") or "")
            await client.post(
                url.format(method="answerCallbackQuery"),
                json={"callback_query_id": cb["id"]},
            )
        elif msg:
            chat_id = (msg.get("chat") or {}).get("id")
            payload = self.handle_message(
                chat_id, msg.get("text") or "",
                reply_to=(msg.get("reply_to_message") or {}).get("message_id"),
            )
        if payload:
            await self._send(client, url, payload)

    async def _flush_replies(self, client: httpx.AsyncClient, url: str) -> None:
        pending = self.herd.unsent_replies()
        sent = []
        for row in pending:
            chat_id = row.get("chat_id")
            if not self.allowed(chat_id):
                sent.append(row["id"])
                continue
            text = _clip(row.get("text") or "")
            await client.post(
                url.format(method="sendMessage"),
                json={"chat_id": chat_id, "text": text},
            )
            sent.append(row["id"])
        if sent:
            self.herd.mark_replies_sent(sent)


async def run_bot(stop: asyncio.Event, cfg: Config | None = None) -> None:
    cfg = cfg or get_config()
    if not cfg.telegram_token or not cfg.telegram_chat_ids:
        return
    bot = TelegramBot(cfg.telegram_token, cfg.telegram_chat_ids, get_herd())
    log.info("telegram bot starting for %s chat(s)", len(cfg.telegram_chat_ids))
    await bot.run(stop)
