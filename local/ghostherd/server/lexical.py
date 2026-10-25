"""The words the sparse (BM25) half of the session index counts.

FastEmbed's BM25 splits on what is not a word character, and a Chinese
character is one: a sentence written without spaces -- all of a Chinese
turn -- became a single token that only that exact sentence matched.  So
before BM25 sees a text, each run of CJK characters is cut into tokens,
the same way on the indexed chunk and on the query: overlapping
character pairs (記憶升級 -> 記憶 憶升 升級), a lone character as itself.

Pairs, not a word segmenter.  Measured with eval/ on a snapshot of the
real store (84k chunks, 11.6k with CJK), jieba found no more: fused MRR
0.672 against pairs' 0.681, the same hit@10 (0.872), and BM25 alone
0.838 against 0.861.  And it costs: 15x the CPU to cut the CJK chunks
(3.2 s against 0.2), a 38 MB package, 95 MB more memory and a cache
file written to $TMPDIR; its dictionary is Simplified Chinese, while the
user writes Traditional.  Pairs need nothing and read either script.

GHOSTHERD_MEMORY_CJK picks another way (jieba, or none) for the eval
only; the index records which it was built with (sparse_migrate), and a
different one rebuilds the sparse half."""

from __future__ import annotations

import os
import re

# Han ideographs: the unified block, extension A and the compatibility
# block.  Kana and Hangul stay as they were: they are rare here, and a
# word segmenter for them is another matter.
CJK_RUN = re.compile(r"[㐀-䶿一-鿿豈-﫿]+")

SEGMENTERS = ("bigram", "jieba", "none")
DEFAULT = "bigram"


def segmenter() -> str:
    name = (os.getenv("GHOSTHERD_MEMORY_CJK") or DEFAULT).strip().lower()
    if name not in SEGMENTERS:
        raise ValueError(f"GHOSTHERD_MEMORY_CJK={name!r}: one of {', '.join(SEGMENTERS)}")
    return name


def bigrams(run: str) -> list[str]:
    if len(run) < 2:
        return [run]
    return [run[i:i + 2] for i in range(len(run) - 1)]


def _jieba_words(run: str) -> list[str]:
    import jieba  # only for GHOSTHERD_MEMORY_CJK=jieba (the eval)

    return [w for w in jieba.cut_for_search(run) if w.strip()]


def segment(text: str, how: str | None = None) -> str:
    """TEXT with every CJK run replaced by its tokens, spaced apart from
    each other and from the letters around them ("ghostherd的記憶" ->
    "ghostherd 的記 記憶")."""
    how = how or segmenter()
    if how == "none" or not text:
        return text or ""
    cut = bigrams if how == "bigram" else _jieba_words
    return CJK_RUN.sub(lambda m: " " + " ".join(cut(m.group())) + " ", text)
