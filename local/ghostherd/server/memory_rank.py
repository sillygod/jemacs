"""Rank shared-memory facts against a query: by meaning, and by words.

Pure scoring -- vectors in, ranked facts out -- so it is tested without a
model or Qdrant; fact_index.py feeds it.

Why two signals that must agree.  The embedder (bge-small-zh) is good at
meaning and poor at telling relevant from merely similar: tuned on
deploy-platform's real store and asks, an unrelated English ask about a
React form scored 0.74 against the facts, as high as a real hit, and the
facts written in Chinese stood close to every Chinese ask whatever its
topic.  Words alone fail the other way: a long fact matches many words of
any long ask, and in a store mostly in English a Chinese word is so rare
that it looks decisive.  What tells a relevant fact is the two agreeing on
one part of the query: its meaning stands out from the rest of the store
there, and it shares identifiers with it -- pr_review, ClickUp, `env |
grep', cn-stg -- which is how asks name what they are about.

- Meaning: cosine to each part of the query (an ask is split into
  paragraphs, so one topic among several is not blurred away), as a
  z-score against the store's facts plus a fixed background of generic
  notes -- the background keeps the score meaningful in a store of five
  facts, where a z-score over five could never stand out.
- Words: BM25 over ASCII identifiers (not CJK bigrams, which made the
  Chinese facts match everything), the name and description weighing
  three times the body, in units of the store's rarest word so the bar
  is the same in a store of 5 facts or 50.

Tuned on 16 labelled queries from deploy-platform (11 of 12 wanted facts
found, no fact for any unrelated query).  A Chinese ask finds an English
fact -- the common case, most facts being written in English -- by meaning
alone: facts embed with a multilingual model (fact_index), so a Chinese
query lands near the English fact, and the two languages share no word to
confirm with, so a strong-enough meaning (Z_ALONE, above where any
unrelated query reaches on the real store) stands on its own.  A recall is
asked for, and short -- two words make little BM25 -- so its gate is looser
still: standing out by meaning with any word in common, or by meaning alone
when far enough, or by words alone; `recall 部署' finds an English deploy
fact by meaning, `recall commit to main' the branch rule.
"""

from __future__ import annotations

import math
import re
from collections import Counter
from dataclasses import dataclass, field
from typing import Any, Sequence

# Meaning: how far a fact must stand out from the store and the
# background, in standard deviations, on one part of the query.
Z_MIN = 1.25
# A recall may rest on meaning alone when it stands out this far.
Z_ALONE = 2.0
# Words: BM25 in units of the store's largest idf.
LEX_MIN = 2.5
# How much the words add to the meaning when ranking what passed.
LEX_WEIGHT = 0.32
# Asks carry at most this many facts.
ASK_K = 3
RECALL_K = 5

# Generic notes in both languages -- no project's facts -- that every
# query part is also measured against, so a fact's z-score says how it
# stands out from text in general, not only from its own small store.
BACKGROUND = [
    "請幫我看一下這個問題", "把這個功能做完再回報", "這個錯誤要怎麼修",
    "幫我整理一下目前的進度", "確認一下正式環境的狀態", "寫測試並跑一次",
    "這段程式碼可以怎麼改善", "請先讀完再開始動手", "使用者同意了，請照這個方向做",
    "把結果整理成表格給我",
    "Please look into this issue", "Finish the feature and report back",
    "How should I fix this error", "Summarize the current progress",
    "Check the state of production", "Write tests and run them",
    "How can this code be improved", "Read it all before you start",
    "The user agreed; go ahead in this direction", "Put the results in a table for me",
]

_STOP = set("""
the a an and or to of in on for is are be it this that with as at by from not no do
does use used using run we you i our your if then than so was were has have had can
will just also into out up over only when what how why which who new fix please make
all any its one two get got let set see now need needs must should would could after
before about there their them they he she his her him me my via per vs etc
""".split())

_WORD = re.compile(r"[a-z0-9][a-z0-9_.\-/]*[a-z0-9]|[a-z0-9]")
_CJK = re.compile(r"[一-鿿]+")


def words(text: str) -> list[str]:
    """TEXT's ASCII words and identifiers, lowercased: `pr_review' gives
    pr_review, pr and review; stop words, digits and single letters go."""
    out: list[str] = []
    for w in _WORD.findall((text or "").lower()):
        parts = {w} | {p for p in re.split(r"[_.\-/]", w) if p}
        out += [p for p in parts if len(p) >= 2 and p not in _STOP and not p.isdigit()]
    return out


def bigrams(text: str) -> list[str]:
    "TEXT's CJK runs as overlapping character pairs (a lone one as itself)."
    out: list[str] = []
    for run in _CJK.findall(text or ""):
        out += ["zh:" + run[i:i + 2] for i in range(len(run) - 1)] or ["zh:" + run]
    return out


def parts(text: str, size: int = 500, most: int = 12) -> list[str]:
    """TEXT in paragraphs of about SIZE characters -- small ones joined, a
    long one left whole for the model to cut -- MOST at most.  An ask names
    several things; scored whole, one topic among them is blurred away."""
    paras = [p.strip() for p in re.split(r"\n\s*\n", text or "") if p.strip()]
    out: list[str] = []
    cur = ""
    for p in paras:
        if len(cur) + len(p) < size:
            cur = (cur + "\n" + p).strip()
        else:
            if cur:
                out.append(cur)
            cur = p
    if cur:
        out.append(cur)
    return out[:most] or [(text or "").strip()]


@dataclass
class Doc:
    "A fact as the ranking sees it."
    key: str            # its store: a project's slug, or `user'
    name: str
    title: str
    description: str
    body: str
    type: str = "project"
    shared: bool = False
    extra: dict[str, Any] = field(default_factory=dict)

    def counts(self, cjk: bool = False) -> Counter:
        "Its words, the title and description weighing three times the body."
        tok = (lambda t: words(t) + bigrams(t)) if cjk else words
        c = Counter(tok(self.body))
        for w in tok(f"{self.title} {self.description}"):
            c[w] += 3
        return c


class Lexicon:
    "BM25 over a store's facts, its idf from them."

    def __init__(self, docs: Sequence[Doc], cjk: bool = False):
        self.cjk = cjk
        self.tf = [d.counts(cjk) for d in docs]
        n = len(docs)
        df = Counter(w for c in self.tf for w in c)
        self.idf = {w: math.log(1 + (n - k + 0.5) / (k + 0.5)) for w, k in df.items()}
        self.len = [sum(c.values()) for c in self.tf]
        self.avg = (sum(self.len) / n) if n else 1.0
        # The largest idf this store can give: the unit the bar is set in.
        self.unit = math.log(1 + (max(n, 1) - 0.5) / 1.5) or 1.0

    def scores(self, query: str) -> list[float]:
        "Each fact's BM25 for QUERY, in units of the store's largest idf."
        q = set(words(query) + (bigrams(query) if self.cjk else []))
        out = []
        for c, length in zip(self.tf, self.len):
            s = 0.0
            for w in q:
                f = c.get(w)
                if f:
                    s += self.idf[w] * f * 2.2 / (f + 1.2 * (0.25 + 0.75 * length / self.avg))
            out.append(s / self.unit)
        return out


def _dot(a: Sequence[float], b: Sequence[float]) -> float:
    return sum(x * y for x, y in zip(a, b))


def _zscores(fact_vecs, bg_vecs, part_vec) -> list[float]:
    "How far each fact stands out for one query part, from facts and background."
    fc = [_dot(v, part_vec) for v in fact_vecs]
    allc = fc + [_dot(v, part_vec) for v in bg_vecs]
    mean = sum(allc) / len(allc)
    sd = math.sqrt(sum((c - mean) ** 2 for c in allc) / len(allc)) or 1e-6
    return [(c - mean) / sd for c in fc]


@dataclass
class Hit:
    doc: Doc
    score: float
    z: float
    lex: float
    why: str  # "meaning+words", "meaning", "words"


def rank(
    docs: Sequence[Doc],
    doc_vecs: Sequence[Sequence[float]],
    query_parts: Sequence[str],
    part_vecs: Sequence[Sequence[float]],
    bg_vecs: Sequence[Sequence[float]],
    *,
    strict: bool = True,
    k: int | None = None,
) -> list[Hit]:
    """The facts DOCS relevant to the query, best first.

    STRICT (an ask): meaning and words must agree on one part of the query.
    Otherwise (a recall): meaning with any word in common, meaning alone
    when it stands out by Z_ALONE, or words alone.  At most K."""
    if not docs or not part_vecs:
        return []
    lex = Lexicon(docs)
    best: dict[int, Hit] = {}
    for text, pv in zip(query_parts, part_vecs):
        zs = _zscores(doc_vecs, bg_vecs, pv)
        ls = lex.scores(text)
        for i, (z, l) in enumerate(zip(zs, ls)):
            if z >= Z_MIN and l >= LEX_MIN:
                why = "meaning+words"
            elif not strict and z >= Z_MIN and l > 0:
                why = "meaning+words"
            elif z >= Z_ALONE:
                # Meaning alone, when it stands out far.  An ask (strict) rests
                # on it too: a query and a fact in different languages -- a
                # Chinese ask, an English fact, the common case here -- share
                # no word to confirm meaning with, and Z_ALONE is above where
                # any unrelated query reaches (measured on the real store).
                why = "meaning"
            elif not strict and l >= LEX_MIN:
                why = "words"
            else:
                continue
            score = z + LEX_WEIGHT * l
            if i not in best or score > best[i].score:
                best[i] = Hit(docs[i], score, z, l, why)
    hits = sorted(best.values(), key=lambda h: h.score, reverse=True)
    return hits[: (k if k is not None else (ASK_K if strict else RECALL_K))]


def rank_words(docs: Sequence[Doc], query: str, *, strict: bool = True,
               k: int | None = None) -> list[Hit]:
    """The keyword fallback, for when the embedder is not up: words alone.
    ASCII identifiers and CJK character bigrams both count, in BM25's units
    of the store's rarest word -- so a Chinese query is not reduced to one
    whitespace-run token with nothing to match.  An ask (STRICT) needs twice
    the bar, with no meaning to confirm it; a recall takes any match."""
    if not docs or not (query or "").strip():
        return []
    ls = Lexicon(docs, cjk=True).scores(query)
    if strict:
        hits = [Hit(d, l, 0.0, l, "words") for d, l in zip(docs, ls) if l >= 2 * LEX_MIN]
    else:
        hits = [Hit(d, l, 0.0, l, "words") for d, l in zip(docs, ls) if l > 0]
    hits.sort(key=lambda h: h.score, reverse=True)
    return hits[: (k if k is not None else (ASK_K if strict else RECALL_K))]
