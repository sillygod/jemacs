from collections.abc import Callable, Iterator

from chunk import Turn
from importers.agy import iter_agy
from importers.claude import iter_claude
from importers.grok import iter_grok
from index import SourceMeta

Importer = Callable[..., Iterator[tuple[SourceMeta, list[Turn]]]]

IMPORTERS: dict[str, Importer] = {
    "claude": iter_claude,
    "grok": iter_grok,
    "agy": iter_agy,
}

ROOT_ATTR = {
    "claude": "claude_root",
    "grok": "grok_root",
    "agy": "agy_root",
}

__all__ = ["IMPORTERS", "ROOT_ATTR", "iter_claude", "iter_grok", "iter_agy"]
