"""The Inbox item user-state store (read / done / override) and its
auto-resolution: a row whose item is gone from the live set is pruned."""

from pathlib import Path

import pytest

from herd import HerdStore, reset_herd


def _store(tmp_path: Path) -> HerdStore:
    reset_herd()
    return HerdStore(tmp_path / "herd.sqlite")


def _state(h) -> dict:
    "The inbox state as {id: entry}, for easy assertions."
    return {e["id"]: {k: v for k, v in e.items() if k != "id"}
            for e in h.inbox_state()["state"]}


def test_set_and_read_item_state(tmp_path: Path):
    h = _store(tmp_path)
    assert _state(h) == {}
    h.inbox_set("guard:g1", read=True)
    h.inbox_set("question:lead:12", done=True, override="update")
    st = _state(h)
    assert st["guard:g1"] == {"read": True, "done": False, "override": None}
    assert st["question:lead:12"] == {"read": False, "done": True, "override": "update"}


def test_fields_left_none_are_kept(tmp_path: Path):
    h = _store(tmp_path)
    h.inbox_set("p:1", read=True, done=False, override="need")
    # only flip `done'; read and override must stay
    h.inbox_set("p:1", done=True)
    st = _state(h)["p:1"]
    assert st == {"read": True, "done": True, "override": "need"}
    # clear the override with ""
    h.inbox_set("p:1", override="")
    assert _state(h)["p:1"]["override"] is None


def test_auto_resolution_prunes_a_gone_item(tmp_path: Path):
    h = _store(tmp_path)
    h.inbox_set("guard:g1", done=True)       # user dismissed it
    h.inbox_set("guard:g2", read=True)
    # g1's cause is gone (not in the live set); g2 is still live.
    st = {e["id"]: e for e in h.inbox_state(live_ids=["guard:g2", "proposal:p9"])["state"]}
    assert "guard:g1" not in st              # pruned
    assert st["guard:g2"]["read"] is True
    # a later item that reuses the same id starts fresh (no leftover `done').
    st2 = {e["id"]: e for e in h.inbox_state(live_ids=["guard:g1"])["state"]}
    assert "guard:g1" not in st2


def test_an_empty_live_set_prunes_all_but_none_keeps_everything(tmp_path: Path):
    h = _store(tmp_path)
    h.inbox_set("a", done=True)
    h.inbox_set("b", read=True)
    # None (no ids given) must NOT prune -- a plain read keeps the store.
    assert set(_state(h)) == {"a", "b"}
    # an explicit empty live set (nothing pending now) clears it.
    assert h.inbox_state(live_ids=[])["state"] == []


def test_item_id_required(tmp_path: Path):
    h = _store(tmp_path)
    with pytest.raises(ValueError):
        h.inbox_set("")
