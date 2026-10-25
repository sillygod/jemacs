"""eval/session_eval.py: its metrics, and that it opens only a snapshot."""

import json
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "eval"))
import session_eval  # noqa: E402


def _hit(session: str, text: str) -> dict:
    return {"session_id": session, "text": text}


def test_the_metrics():
    hits = [_hit("a", "x"), _hit("b", "wanted one"), _hit("c", "y"), _hit("b", "wanted two")]
    relevant = {session_eval.key(_hit("b", "wanted one")), session_eval.key(_hit("b", "wanted two")),
                session_eval.key(_hit("d", "never found"))}
    s = session_eval.score(hits, relevant)
    assert s["mrr"] == 0.5 and s["hit@1"] == 0 and s["hit@5"] == 1
    assert s["recall@10"] == pytest.approx(2 / 3) and s["ses@5"] == 1
    assert session_eval.score([], relevant)["mrr"] == 0


def test_it_opens_a_snapshot_only(tmp_path):
    with pytest.raises(SystemExit, match="not a snapshot"):
        session_eval.open_engine(tmp_path)


def test_the_queries_are_labelled():
    spec = json.loads(session_eval.QUERIES.read_text(encoding="utf-8"))
    kinds = {q["kind"] for q in spec["queries"]}
    assert kinds == {"ident", "zh", "mixed", "en"}
    assert all(q["relevant"] and q["label"]["regex"] for q in spec["queries"])
