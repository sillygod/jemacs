"""Telegram long-poll for the herd.  Optional: no token, no task.

Taps enqueue sqlite commands; Emacs herd_tick runs them.  This module
never talks to a PTY.  Unknown chats are ignored.
"""

from __future__ import annotations

import asyncio
import logging
from typing import Any

import httpx

from config import Config, get_config
from herd import HerdStore, get_herd

log = logging.getLogger("ghostherd.telegram")

API = "https://api.telegram.org/bot{token}/{method}"

GLYPH = {
    "blocked": "⚠",
    "working": "⟳",
    "done": "✓",
    "idle": "○",
    "starting": "…",
    "dead": "✖",
}

HELP = (
    "ghostherd bot — pull only, no push.\n"
    "/herd — list sessions\n"
    "Tap a session for screen / explain / prompt / interrupt.\n"
    "Blocked sessions also get 1 / 2 / 3 to answer a prompt.\n"
    "/cancel — drop a pending prompt."
)


def _btn(text: str, data: str) -> dict:
    return {"text": text, "callback_data": data}


def herd_keyboard(sessions: list[dict[str, Any]]) -> dict:
    rows = []
    for s in sessions:
        glyph = GLYPH.get(s.get("state") or "", "?")
        label = f"{glyph} {s['name']}"
        if len(label) > 40:
            label = label[:37] + "…"
        rows.append([_btn(label, f"o:{s['short']}")])
    rows.append([_btn("refresh", "h")])
    return {"inline_keyboard": rows}


def session_keyboard(session: dict[str, Any], more: bool = False) -> dict:
    short = session["short"]
    state = session.get("state") or ""
    rows = [
        [_btn("screen", f"sc:{short}"), _btn("explain", f"ex:{short}")],
        [_btn("prompt", f"p:{short}"), _btn("interrupt", f"i:{short}")],
    ]
    if state == "blocked":
        rows.append(
            [
                _btn("1", f"n:{short}:1"),
                _btn("2", f"n:{short}:2"),
                _btn("3", f"n:{short}:3"),
                _btn("abort", f"ab:{short}"),
            ]
        )
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
    if session.get("reason"):
        lines.append(session["reason"])
    return "\n".join(lines)


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

    def handle_message(self, chat_id: int, text: str) -> dict[str, Any] | None:
        """Return a sendMessage payload, or None to ignore."""
        if not self.allowed(chat_id):
            log.info("telegram ignored chat %s", chat_id)
            return None
        text = (text or "").strip()
        if text in ("/cancel", "cancel"):
            self.pending_prompt.pop(chat_id, None)
            return {"chat_id": chat_id, "text": "cancelled"}
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
        return {
            "chat_id": chat_id,
            "text": "unknown. /herd or /help",
        }

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
            return self._enqueue(chat_id, "screen", data[3:])
        if data.startswith("ex:"):
            return self._enqueue(chat_id, "explain", data[3:])
        if data.startswith("i:"):
            return self._enqueue(chat_id, "interrupt", data[2:])
        if data.startswith("ab:"):
            return self._enqueue(chat_id, "abort", data[3:])
        if data.startswith("k2:"):
            return self._enqueue(chat_id, "kill", data[3:])
        if data.startswith("rs2:"):
            return self._enqueue(chat_id, "respawn", data[4:])
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
            "reply_markup": herd_keyboard(sessions),
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

    async def run(self, stop: asyncio.Event) -> None:
        url = API.format(token=self.token, method="{method}")
        timeout = httpx.Timeout(40.0, connect=10.0)
        async with httpx.AsyncClient(timeout=timeout) as client:
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
            payload = self.handle_message(chat_id, msg.get("text") or "")
        if payload:
            await client.post(url.format(method="sendMessage"), json=payload)

    async def _flush_replies(self, client: httpx.AsyncClient, url: str) -> None:
        pending = self.herd.unsent_replies()
        sent = []
        for row in pending:
            chat_id = row.get("chat_id")
            if not self.allowed(chat_id):
                sent.append(row["id"])
                continue
            text = row.get("text") or ""
            if len(text) > 3500:
                text = text[:3500] + "\n…"
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
