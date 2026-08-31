"""Sidecar configuration from the environment.

Bind loopback only: there is no transport auth, same as ecloud.
"""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path


def _expand(value: str) -> Path:
    return Path(os.path.expanduser(value)).resolve()


@dataclass
class Config:
    host: str = "127.0.0.1"
    port: int = 49152
    data_dir: Path = Path.home() / ".emacs.d" / "ghostherd-memory"
    # Chinese-first, still embeds English identifiers.  MiniLM-L12
    # multilingual is ~4× slower on this machine (~19 vs ~73 chunk/s)
    # and is available as GHOSTHERD_MEMORY_EMBED_MODEL.
    # Do not default to bge-small-en — it will quietly fail on 中文.
    embed_model: str = "BAAI/bge-small-zh-v1.5"
    sparse_model: str = "Qdrant/bm25"
    fake_embed: bool = False
    claude_root: Path = Path.home() / ".claude" / "projects"
    grok_root: Path = Path.home() / ".grok" / "sessions"
    agy_root: Path = Path.home() / ".gemini" / "antigravity-cli"
    max_chars: int = 1800
    min_chars: int = 24
    overlap: int = 120
    tool_truncate: int = 1500
    # json.loads of a 1MB tool-result line is CPU with nothing to search.
    max_line_bytes: int = 262144
    embed_batch: int = 256
    embed_threads: int = 0  # 0 = cpu_count - 1
    # auto: CoreML on macOS if the session accepts it, else CPU.
    onnx_provider: str = "auto"
    # tool dumps are rarely what you search for and dominate grok volume.
    index_tools: bool = False
    mail_inline_limit: int = 4000
    telegram_token: str = ""
    telegram_chat_ids: tuple[int, ...] = ()

    @property
    def qdrant_path(self) -> Path:
        return self.data_dir / "qdrant"

    @property
    def index_path(self) -> Path:
        return self.data_dir / "sources.sqlite"

    @property
    def herd_path(self) -> Path:
        return self.data_dir / "herd.sqlite"

    @classmethod
    def from_env(cls) -> "Config":
        fake = os.getenv("GHOSTHERD_MEMORY_FAKE_EMBED", "").lower() in (
            "1",
            "true",
            "yes",
        )
        data = os.getenv("GHOSTHERD_MEMORY_DIR")
        return cls(
            host=os.getenv("GHOSTHERD_MEMORY_HOST", "127.0.0.1"),
            port=int(os.getenv("GHOSTHERD_MEMORY_PORT", "49152")),
            data_dir=_expand(data) if data else Path.home() / ".emacs.d" / "ghostherd-memory",
            embed_model=os.getenv(
                "GHOSTHERD_MEMORY_EMBED_MODEL",
                "BAAI/bge-small-zh-v1.5",
            ),
            sparse_model=os.getenv("GHOSTHERD_MEMORY_SPARSE_MODEL", "Qdrant/bm25"),
            fake_embed=fake,
            claude_root=_expand(
                os.getenv("GHOSTHERD_MEMORY_CLAUDE_ROOT", "~/.claude/projects")
            ),
            grok_root=_expand(
                os.getenv("GHOSTHERD_MEMORY_GROK_ROOT", "~/.grok/sessions")
            ),
            agy_root=_expand(
                os.getenv("GHOSTHERD_MEMORY_AGY_ROOT", "~/.gemini/antigravity-cli")
            ),
            max_chars=int(os.getenv("GHOSTHERD_MEMORY_MAX_CHARS", "1800")),
            min_chars=int(os.getenv("GHOSTHERD_MEMORY_MIN_CHARS", "24")),
            overlap=int(os.getenv("GHOSTHERD_MEMORY_OVERLAP", "120")),
            tool_truncate=int(os.getenv("GHOSTHERD_MEMORY_TOOL_TRUNCATE", "1500")),
            max_line_bytes=int(os.getenv("GHOSTHERD_MEMORY_MAX_LINE", "262144")),
            embed_batch=int(os.getenv("GHOSTHERD_MEMORY_EMBED_BATCH", "256")),
            embed_threads=int(os.getenv("GHOSTHERD_MEMORY_THREADS", "0")),
            onnx_provider=os.getenv("GHOSTHERD_MEMORY_ONNX", "auto"),
            index_tools=os.getenv("GHOSTHERD_MEMORY_INDEX_TOOLS", "").lower()
            in ("1", "true", "yes"),
            telegram_token=os.getenv("GHOSTHERD_TELEGRAM_TOKEN", "").strip(),
            telegram_chat_ids=_parse_chat_ids(
                os.getenv("GHOSTHERD_TELEGRAM_CHAT_IDS", "")
            ),
        )


def _parse_chat_ids(raw: str) -> tuple[int, ...]:
    ids = []
    for part in (raw or "").split(","):
        part = part.strip()
        if not part:
            continue
        try:
            ids.append(int(part))
        except ValueError:
            continue
    return tuple(ids)


def get_config() -> Config:
    return Config.from_env()
