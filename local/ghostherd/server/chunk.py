"""Split a turn into embeddable pieces.

One turn is one chunk when it fits.  Longer ones split on blank lines,
then lines, then a hard wrap, with a short overlap so a sentence that
straddles a cut is still findable from either side.
"""

from __future__ import annotations

from dataclasses import dataclass, replace


@dataclass
class Turn:
    agent: str
    role: str
    text: str
    project: str = ""
    session_id: str = ""
    source_path: str = ""
    source_kind: str = "transcript"
    ts: str | None = None
    title: str | None = None
    chunk_index: int = 0


def split_text(text: str, max_chars: int = 1800, overlap: int = 120) -> list[str]:
    text = (text or "").strip()
    if not text:
        return []
    if len(text) <= max_chars:
        return [text]
    parts: list[str] = []
    rest = text
    while rest:
        if len(rest) <= max_chars:
            parts.append(rest)
            break
        window = rest[:max_chars]
        cut = window.rfind("\n\n")
        if cut < max_chars // 3:
            cut = window.rfind("\n")
        if cut < max_chars // 3:
            cut = window.rfind(" ")
        if cut < max_chars // 3:
            cut = max_chars
        piece = rest[:cut].strip()
        if piece:
            parts.append(piece)
        nxt = rest[max(0, cut - overlap) :].lstrip()
        if nxt == rest:
            nxt = rest[cut:].lstrip()
        rest = nxt
    return parts


def chunk_turns(
    turns: list[Turn],
    max_chars: int = 1800,
    min_chars: int = 24,
    overlap: int = 120,
) -> list[Turn]:
    out: list[Turn] = []
    index = 0
    for turn in turns:
        for piece in split_text(turn.text, max_chars=max_chars, overlap=overlap):
            if len(piece) < min_chars:
                continue
            out.append(replace(turn, text=piece, chunk_index=index))
            index += 1
    return out
