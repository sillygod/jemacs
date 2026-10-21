"""memory_rank's gates, with the meaning and the words of each fact fixed
by hand -- what passes for an ask, what for a recall, and the fallback."""

import pytest

import memory_rank as mr


def _docs(n):
    return [mr.Doc("k", f"f{i}", f"F{i}", "", "") for i in range(n)]


def _rank(monkeypatch, zs, ls, strict, k=None):
    "Rank with each fact's z and words given, one query part."
    monkeypatch.setattr(mr, "_zscores", lambda fv, bv, pv: list(zs))
    monkeypatch.setattr(mr.Lexicon, "scores", lambda self, q: list(ls))
    docs = _docs(len(zs))
    hits = mr.rank(docs, [[0.0]] * len(docs), ["q"], [[0.0]], [], strict=strict, k=k)
    return {h.doc.name: h.why for h in hits}


# (z, words, passes an ask, passes a recall)
CASES = [
    (1.9, 1.7, False, True),    # a short recall: meaning, a word in common
    (1.3, 0.0, False, False),   # close in meaning, no word shared, not far
    (2.3, 0.0, False, True),    # far in meaning alone: a recall only
    (0.4, 3.0, False, True),    # words alone: a recall only
    (1.3, 2.6, True, True),     # both agree: an ask too
    (1.0, 2.0, False, False),   # neither enough
]


@pytest.mark.parametrize("z, words, ask, recall", CASES)
def test_the_gates(monkeypatch, z, words, ask, recall):
    # Padded with facts that pass nothing, so k never cuts.
    zs, ls = [z, 0.0, 0.0], [words, 0.0, 0.0]
    assert ("f0" in _rank(monkeypatch, zs, ls, strict=True)) == ask
    assert ("f0" in _rank(monkeypatch, zs, ls, strict=False)) == recall


def test_best_first_and_capped(monkeypatch):
    got = _rank(monkeypatch, [1.3, 3.0, 2.0, 1.5], [3.0, 4.0, 2.6, 2.7], strict=True)
    assert list(got) == ["f1", "f2", "f3"]           # ASK_K, best first


def test_words_are_ascii_identifiers():
    assert set(mr.words("Run `pr_review` on cn-stg, then 部署")) >= {"pr_review", "pr", "review", "cn-stg", "cn", "stg"}
    assert not [w for w in mr.words("部署 the to of") ]
    assert mr.bigrams("憑證續簽") == ["zh:憑證", "zh:證續", "zh:續簽"]


def test_an_ask_is_read_in_parts():
    text = "\n\n".join(["first topic " * 20, "second topic " * 20, "short", "tail"])
    parts = mr.parts(text, size=300)
    assert len(parts) >= 2 and all(p.strip() for p in parts)
    assert mr.parts("") == [""]


def test_the_word_fallback_is_stricter_for_an_ask():
    docs = [mr.Doc("k", "cn-stg-metrics", "Cn stg metrics", "query prometheus on cn-stg", "prometheus cn-stg"),
            mr.Doc("k", "other", "Other", "nothing alike", "unrelated words here")]
    assert [h.doc.name for h in mr.rank_words(docs, "cn-stg prometheus metrics", strict=False)][0] == "cn-stg-metrics"
    assert mr.rank_words(docs, "prometheus", strict=True) == []   # one word: not enough for an ask
