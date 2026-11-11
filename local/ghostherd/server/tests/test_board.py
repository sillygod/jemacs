"""The Board (board.py, the page's Board tab): each ask in a work item, put
there once -- with the ask it was made answering, a relay with the answer
it passes on, where a lead said -- and kept; a lead renames, merges, splits,
labels and closes items; an item is never done by its quiet alone."""

import json
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest

import board
from herd import HerdStore, reset_herd
from tests.test_ask import SNAP, _deliver, client  # noqa: F401 -- the RPC client, a fixture

LEADS = SNAP + [
    {"name": "lead-a", "kind": "claude", "state": "idle", "project": "/src/a/", "role": "lead"},
    {"name": "lead-b", "kind": "claude", "state": "idle", "project": "/src/b/", "role": "lead"},
    {"name": "lead-c", "kind": "claude", "state": "idle", "project": "/src/c/", "role": "lead"},
]


def _store(tmp_path: Path) -> HerdStore:
    reset_herd()
    h = HerdStore(tmp_path / "herd.sqlite")
    h.tick(sessions=LEADS)
    return h


def _deliver_all(h):
    return _deliver(h, LEADS)


def _items(h, days=1, hours=4):
    return {c["id"]: c for c in h.board(days, hours)["items"]}


def _item_of(h, ask_id):
    "The item ASK_ID is in, the asks not placed yet placed first."
    with h._mu:
        h._assign_items()
        return h._item_of(ask_id)


def _backdate(h, ask_id, minutes):
    "Ask ASK_ID made, delivered and answered MINUTES earlier than it was."
    with h._mu:
        for col in ("created_at", "delivered_at", "answered_at"):
            h._conn.execute(
                f"UPDATE asks SET {col} = ? WHERE id = ? AND {col} IS NOT NULL",
                ((datetime.now(timezone.utc) - timedelta(minutes=minutes)).isoformat(), ask_id))
        h._conn.commit()


# ---- What a text names ----------------------------------------------------------


def test_ask_ids_only_the_herds_and_not_its_own():
    known = {"1a2b3c4d", "deadbeef"}
    text = "see 1a2b3c4d and commit 2c13ac6f, then deadbeef; again 1a2b3c4d"
    assert board.ask_ids(text, known) == ["1a2b3c4d", "deadbeef"]
    assert board.ask_ids(text, known, own="deadbeef") == ["1a2b3c4d"]


def test_pr_refs_links_short_forms_and_lists():
    repos = {"shop": ("bitbucket", "acme"), "shop-helm": ("bitbucket", "acme")}
    text = ("Opened https://bitbucket.org/acme/api/pull-requests/12 and shop #512; "
            "also shop PR #513、#514 and #515; shop-helm #9.  PR #40 too.")
    got = board.pr_refs(text, repos, home=("shop",))
    assert got == [("bitbucket", "acme", "api", 12), ("bitbucket", "acme", "shop", 512),
                   ("bitbucket", "acme", "shop", 513), ("bitbucket", "acme", "shop", 514),
                   ("bitbucket", "acme", "shop", 515), ("bitbucket", "acme", "shop-helm", 9),
                   ("bitbucket", "acme", "shop", 40)]
    # A repository nobody linked is not one: `issue #3' is no pull request.
    assert board.pr_refs("see issue #3 and mobile #4", repos) == []


def test_fyi_ack_and_relay():
    assert board.fyi("部署好了，看完回一句收到即可")
    assert board.fyi("FYI: the key rotated")
    assert not board.fyi("Please rotate the key and tell me when done")
    assert board.ack("收到。") and board.ack("OK, thanks!") and board.ack("👍")
    assert not board.ack("收到，但 #12 還沒 merge，要等 staging 先過")
    assert board.relay("lead-b 轉來 lead-c 的回覆：已部署")
    assert board.relay("lead-b 轉 lead-c 的回覆（關於 key）：...")
    assert board.relay("Relaying lead-c's answer: done")
    # Anywhere else it is something else.
    assert not board.relay("DB 腳本我已經轉給 Jing 跑")
    assert not board.relay("請轉給 lead-c 或負責的人")


def test_title_takes_the_lead_s_openers_off():
    assert board.title("來自 lead-b（/src/b）。請把 staging 的 key 換掉。之後回報") == "請把 staging 的 key 換掉"
    assert board.title("**緊急**，staging 改成 DEBUG=False") == "staging 改成 DEBUG=False"
    assert board.title("（排在 #12 之後）新任務（唯讀）：查 VM 有沒有被入侵") == "查 VM 有沒有被入侵"
    assert board.title("背景：攻擊者上週進來過。\n請盤點所有 key") == "請盤點所有 key"
    assert len(board.title("x" * 200)) == board.TITLE_MAX


@pytest.mark.parametrize("body, want", [
    # Lines that say nothing are skipped: numbering, links, acknowledgements.
    ("1.\n修 cart 的重複扣款", "修 cart 的重複扣款"),
    ("https://claude.ai/artifact/abc123\n盤點 billing 的 secret", "盤點 billing 的 secret"),
    ("看完回一句收到即可。\n結論：shop 的 webhook 打不到外網", "結論：shop 的 webhook 打不到外網"),
    ("很好，謝謝。\n請把 staging 的憑證換掉", "請把 staging 的憑證換掉"),
    # What frames a message goes; what follows it stays.
    ("這是 1a2b3c4d 第 2 點的回報。\n修 shop #412 的測試", "修 shop #412 的測試"),
    ("這是 cart 壓測的結果，回一句收到就好。", "cart 壓測的結果"),
    ("補充 1a2b3c4d 的範圍：billing 的 webhook 也要改", "billing 的 webhook 也要改"),
    ("接續 1a2b3c4d。\n把 shop #412 拆成兩個 PR", "把 shop #412 拆成兩個 PR"),
    ("急件：分析 cart 的扣款異常，從資料庫這一面看。", "分析 cart 的扣款異常，從資料庫這一面看"),
    ("優先度提高（使用者想 merge #40）：請先處理 #40，其他往後排。", "請先處理 #40，其他往後排"),
    ("現況（11/04 的快照，lead 盤點）：billing 的 key 都還沒換", "billing 的 key 都還沒換"),
    # Markdown off.
    ("- **修**掉 `cart` 的[重複扣款](https://example.com/x)", "修掉 cart 的重複扣款"),
    # An aside, the work's rules, an approval with nothing after it, a line of configuration: not a name.
    ("（排在 #40 之後做。）再開一個小 PR：staging 改成 DEBUG=False", "再開一個小 PR：staging 改成 DEBUG=False"),
    ("唯讀調查，不要改程式。\n找出 billing 對帳差 3 元的原因", "找出 billing 對帳差 3 元的原因"),
    ("Jing 已經說要做了。\n輪換 staging 的 key", "輪換 staging 的 key"),
    ("lion：confidential，grant=password\n修掉 token 的公開端點", "修掉 token 的公開端點"),
    # One with a PR or a ticket just after a topic that says no work, preferred;
    # a topic that says its work kept.
    ("攻擊者還在打 cart 的 API。\n請開 hotfix PR #12 補上驗證", "請開 hotfix PR #12 補上驗證"),
    ("請把 billing 的稅率表同步到 staging。\nshop PR #412 已經部署", "請把 billing 的稅率表同步到 staging"),
    ("使用者要整理一份各 project 的修正報告。\nshop #2529 有沒有部署到 production？", "shop #2529 有沒有部署到 production？"),
    # Kept as written: an approval with what it approves, a time's colon, a comma.
    ("Jing approved two Board UX changes.", "Jing approved two Board UX changes."),
    ("11/07 20:36 UTC，有人用 staff 帳號登入了後台", "11/07 20:36 UTC，有人用 staff 帳號登入了後台"),
    ("A design mockup, not code for the repo.", "A design mockup, not code for the repo."),
])
def test_title_names_the_work(body, want):
    assert board.title(body) == want


def test_title_goes_on_to_the_next_asks_when_the_first_says_nothing():
    assert board.title("看完回一句收到即可。", "修 cart 的庫存鎖") == "修 cart 的庫存鎖"
    assert board.title("修 shop 的結帳頁", "也看一下 billing") == "修 shop 的結帳頁"
    # Nothing says anything at all: the first line as it was.
    assert board.title("收到", "OK") == "收到"


# ---- Where an ask goes ------------------------------------------------------------


def test_an_ask_made_answering_another_goes_with_it(tmp_path):
    h = _store(tmp_path)
    root = h.ask("agy-a2", "Ship the login fix", from_name="lead-a")
    _deliver_all(h)
    sub = h.ask("agy-a", "Run the e2e suite", from_name="agy-a2")
    other = h.ask("agy-b", "Something else entirely", from_name="lead-b")
    items = _items(h)
    assert _item_of(h, sub["id"]) == _item_of(h, root["id"]) != _item_of(h, other["id"])
    c = items[_item_of(h, root["id"])]
    assert (c["title"], c["asks"], c["open"], c["status"]) == ("Ship the login fix", 2, 2, "live")
    assert c["agents"] == ["lead-a", "agy-a2", "agy-a"] and c["projects"] == ["a"]
    assert c["block"]["kind"] == "open" and c["block"]["to"] == "agy-a"


def test_placing_is_kept(tmp_path):
    """An ask's item is decided once: a later ask, or a reply, does not move it."""
    h = _store(tmp_path)
    a = h.ask("agy-a2", "First piece", from_name="lead-a")
    _deliver_all(h)
    h.board()
    before = _item_of(h, a["id"])
    h.reply(a["id"], "Done; see also the other work", from_name="agy-a2")
    h.board()
    assert _item_of(h, a["id"]) == before


def test_a_named_ask_or_pr_links_but_does_not_move(tmp_path):
    """Measured on a day's asks, a named ask was another item's half the
    time: it is a link, not where the ask goes (board.JOIN_BY)."""
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Open https://bitbucket.org/acme/shop/pull-requests/7", from_name="lead-a")
    _deliver_all(h)
    h.board()
    b = h.ask("agy-b", f"Background: see {a['id']}; review shop #7", from_name="lead-b")
    items = _items(h)
    ia, ib = _item_of(h, a["id"]), _item_of(h, b["id"])
    assert ia != ib
    assert items[ib]["links"] == [ia]
    pr_a, pr_b = items[ia]["prs"][0], items[ib]["prs"][0]
    assert (pr_a["repo"], pr_a["id"], pr_a["own"]) == ("shop", 7, True)
    assert (pr_b["own"], pr_b["of"]) == (False, ia)


def test_join_by_refs_when_the_rules_say_so(tmp_path, monkeypatch):
    monkeypatch.setattr(board, "JOIN_BY", frozenset({"refs", "prs"}))
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Open https://bitbucket.org/acme/shop/pull-requests/7", from_name="lead-a")
    h.board()
    b = h.ask("agy-b", f"Follow up on {a['id']}", from_name="lead-b")
    c = h.ask("agy-b", "Review shop #7", from_name="lead-b")
    h.board()
    assert _item_of(h, a["id"]) == _item_of(h, b["id"]) == _item_of(h, c["id"])


def test_a_relay_goes_with_the_answer_it_passes_on(tmp_path):
    """lead-b asks lead-c, has its answer, and passes it on to lead-a as an
    ask of its own a minute later: one item.  The same words long after, or
    to the one that answered, are not a relay of it."""
    h = _store(tmp_path)
    src = h.handoff("/src/c", "Which key does production use?", from_name="lead-b")
    _deliver_all(h)
    h.reply(src["id"], "The GENERAL key, rotated at 17:00", from_name="lead-c")
    h.board()
    relayed = h.handoff("/src/a", "lead-b 轉來 lead-c 的回覆：production 用 GENERAL key", from_name="lead-b")
    plain = h.handoff("/src/a", "A new piece of work", from_name="lead-b")
    h.board()
    assert _item_of(h, relayed["id"]) == _item_of(h, src["id"]) != _item_of(h, plain["id"])
    with h._mu:
        how = h._conn.execute("SELECT how, ref FROM ask_item WHERE ask = ?", (relayed["id"],)).fetchone()
    assert how == ("relay", src["id"])
    c = _items(h)[_item_of(h, src["id"])]
    assert c["relays"] == 1 and c["projects"] == ["b", "c", "a"]
    # Long after the answer: not a relay of it.
    _backdate(h, src["id"], 30)
    late = h.handoff("/src/a", "lead-b 轉來：還有一件事", from_name="lead-b")
    h.board()
    assert _item_of(h, late["id"]) != _item_of(h, src["id"])


def test_the_lead_says_which_item_and_which_relay(tmp_path):
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Rotate the key", from_name="lead-a")
    _deliver_all(h)
    h.reply(a["id"], "rotated", from_name="agy-a2")
    h.board()
    it = _item_of(h, a["id"])
    b = h.ask("agy-a2", "Now deploy it", from_name="lead-a", item=it)
    c = h.handoff("/src/b", "They need the new key", from_name="lead-a", item=a["id"])
    d = h.ask("agy-a2", "Passing on: done", from_name="lead-a", relay_of=a["id"])
    h.board()
    assert _item_of(h, b["id"]) == _item_of(h, c["id"]) == _item_of(h, d["id"]) == it
    with pytest.raises(ValueError, match="no item zzz"):
        h.ask("agy-a2", "x", from_name="lead-a", item="zzz")
    with pytest.raises(ValueError, match="no ask 00000000 to relay"):
        h.ask("agy-a2", "x", from_name="lead-a", relay_of="00000000")


def test_a_handoff_names_both_projects(tmp_path):
    h = _store(tmp_path)
    out = h.handoff("/src/b", "Please rotate b's key", from_name="lead-a")
    _deliver_all(h)
    c = _items(h)[_item_of(h, out["id"])]
    assert c["projects"] == ["a", "b"] and c["cross"]
    w = h.board()["waits"]
    assert [(x["from"], x["to"], x["kind"], x["fromProj"], x["toProj"]) for x in w] == [
        ("lead-a", "lead-b", "handoff", "a", "b")]


def test_a_repository_learned_from_a_reply_names_the_next_asks_prs(tmp_path):
    """Placed in one go: a reply's link teaches the repository before an
    ask made after it, naming that repository's PR by name, is read."""
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Open the PR", from_name="lead-a")
    _deliver_all(h)
    h.reply(a["id"], "Opened https://bitbucket.org/acme/shop/pull-requests/412", from_name="agy-a2")
    b = h.handoff("/src/b", "shop #412 waits on your API", from_name="lead-a")
    items = _items(h)
    pr = items[_item_of(h, b["id"])]["prs"][0]
    assert (pr["repo"], pr["id"], pr["own"], pr["of"]) == ("shop", 412, False, _item_of(h, a["id"]))


def test_reply_names_join_the_item(tmp_path):
    """What a reply names -- a PR, a ticket -- is its item's too."""
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Fix the bug in CU-86abc12", from_name="lead-a")
    _deliver_all(h)
    h.board()
    h.reply(a["id"], "Opened https://bitbucket.org/acme/shop/pull-requests/31", from_name="agy-a2")
    c = _items(h)[_item_of(h, a["id"])]
    assert c["labels"] == ["CU-86abc12"]
    assert [t["id"] for t in c["tickets"]] == ["86abc12"] and c["tickets"][0]["last"]
    assert [(p["repo"], p["id"]) for p in c["prs"]] == [("shop", 31)]
    assert c["status"] == "quiet" and c["block"]["kind"] == "answered"
    # Learned from the link: `shop #32' is that repository's now.
    b = h.ask("agy-a2", "And shop #32 too", from_name="lead-a")
    assert [(p["repo"], p["id"]) for p in _items(h)[_item_of(h, b["id"])]["prs"]] == [("shop", 32)]


# ---- An item as the board shows it ---------------------------------------------------


def test_never_done_by_its_quiet(tmp_path):
    """All answered is quiet; only a lead's close makes it done, and a new
    ask in it opens it again."""
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Do it", from_name="lead-a")
    _deliver_all(h)
    h.reply(a["id"], "done", from_name="agy-a2")
    it = _item_of(h, a["id"]) or h.board() and _item_of(h, a["id"])
    assert _items(h)[it]["status"] == "quiet"
    h.item_edit("close", it, by="lead-a")
    assert _items(h)[it]["status"] == "done"
    h.ask("agy-a2", "One more thing", from_name="lead-a", item=it)
    assert _items(h)[it]["status"] == "live"


def test_fyi_and_acks_are_counted_to_fold(tmp_path):
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Deployed; reply 收到 is enough (回一句收到即可)", from_name="lead-a")
    _deliver_all(h)
    h.reply(a["id"], "收到", from_name="agy-a2")
    b = h.ask("agy-a2", "Now the real work", from_name="lead-a", item=a["id"])
    _deliver_all(h)
    h.reply(b["id"], "Done: 3 files changed, tests pass", from_name="agy-a2")
    out = h.board_item(a["id"])
    assert (out["fyi"], out["acks"]) == (1, 1)
    folds = [(e["kind"], e["fold"]) for e in out["events"]]
    assert folds == [("ask", "fyi"), ("answer", "ack"), ("ask", None), ("answer", None)]
    # What it waits on is the last answer that says something.
    assert out["block"]["head"] == "Done: 3 files changed, tests pass"


def test_window_keeps_live_items_and_recent_ones(tmp_path):
    h = _store(tmp_path)
    old = h.ask("agy-a2", "Old work", from_name="lead-a")
    _deliver_all(h)
    h.reply(old["id"], "done", from_name="agy-a2")
    _backdate(h, old["id"], 3 * 24 * 60)
    live = h.ask("agy-b", "Long-running", from_name="lead-b")
    _backdate(h, live["id"], 3 * 24 * 60)
    out = h.board(1)
    ids = {c["id"] for c in out["items"]}
    assert _item_of(h, live["id"]) in ids and _item_of(h, old["id"]) not in ids and out["more"]
    assert _item_of(h, old["id"]) in {c["id"] for c in h.board(7)["items"]}


def test_edges_count_asks_between_projects(tmp_path):
    h = _store(tmp_path)
    for _ in range(2):
        h.handoff("/src/b", "work for b", from_name="lead-a")
    h.ask("agy-a2", "inside a", from_name="lead-a")
    assert h.board()["edges"] == [{"from": "a", "to": "b", "relay": False, "n": 2}]
    assert h.board()["counts"]["xproj"] == 2


# ---- A lead's changes ------------------------------------------------------------------


def test_rename_merge_split_move_label(tmp_path):
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Part one", from_name="lead-a")
    _deliver_all(h)
    a1 = h.ask("agy-a", "Part one, sub", from_name="agy-a2")
    b = h.ask("agy-b", "Part two", from_name="lead-b")
    h.board()
    ia, ib = _item_of(h, a["id"]), _item_of(h, b["id"])
    h.item_edit("rename", ia, by="lead-a", title="Rotate the keys")
    assert _items(h)[ia]["title"] == "Rotate the keys" and _items(h)[ia]["named"]
    h.item_edit("label", ib, by="lead-b", labels=["CU-86x1", "deploy"])
    out = h.item_edit("merge", ib, by="lead-b", into=a["id"])
    assert out == {"item": ia, "merged": ib}
    items = _items(h)
    assert ib not in items and items[ia]["asks"] == 3 and items[ia]["labels"] == ["CU-86x1", "deploy"]
    # The old id still finds it.
    assert h.board_item(ib)["id"] == ia
    # Split an ask out: the ask made answering it goes with it.
    out = h.item_edit("split", by="lead-a", asks=[a["id"]], title="Part one alone")
    assert sorted(out["moved"]) == sorted([a["id"], a1["id"]])
    new = out["item"]
    items = _items(h)
    assert items[new]["asks"] == 2 and items[new]["title"] == "Part one alone" and items[ia]["asks"] == 1
    # Move it back; what a lead moved is pinned.
    h.item_edit("move", ia, by="lead-a", asks=[a1["id"]])
    assert _item_of(h, a1["id"]) == ia
    with h._mu:
        assert h._conn.execute("SELECT how, pinned FROM ask_item WHERE ask = ?", (a1["id"],)).fetchone() == ("lead", 1)
    h.item_edit("rename", ia, by="lead-a", title="")
    assert not _items(h)[ia]["named"]


def test_only_a_lead_changes_items(tmp_path):
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Part one", from_name="lead-a")
    h.board()
    with pytest.raises(ValueError, match="agy-a2 is not a lead"):
        h.item_edit("rename", a["id"], by="agy-a2", title="mine")
    h.item_edit("rename", a["id"], by="user", title="the user's")
    assert h.items()["items"][0]["title"] == "the user's"


def test_pruned_asks_take_their_items(tmp_path):
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Old", from_name="lead-a")
    _deliver_all(h)
    h.reply(a["id"], "done", from_name="agy-a2")
    h.board()
    it = _item_of(h, a["id"])
    _backdate(h, a["id"], 8 * 24 * 60)
    with h._mu:
        h._conn.execute("DELETE FROM meta WHERE k = 'pruned'")
        h._conn.execute("DELETE FROM runs")  # its asker's run of asks is over
        h._prune(datetime.now(timezone.utc).isoformat())
        assert not h._conn.execute("SELECT 1 FROM ask_item WHERE ask = ?", (a["id"],)).fetchone()
        assert not h._conn.execute("SELECT 1 FROM work_items WHERE id = ?", (it,)).fetchone()


def test_rpc_methods_and_agents(tmp_path, monkeypatch):
    """The page's RPCs; a lead's are its agents' too."""
    from jsonrpc_handler import AGENT_METHODS
    assert {"herd_items", "herd_board_item", "herd_item_edit"} <= AGENT_METHODS
    assert "herd_board" not in AGENT_METHODS


# ---- Phase 2: a lead's fixed lines, who opened a PR, a request that went nowhere ----


def test_fixed_lines_either_colon_markdown_and_none():
    text = ("Done.\n"
            "需要 Jing：跑 rotate.sh\n"
            "- **需要Jing:** merge shop #412\n"
            "部署: shop #412 → production\n"
            "需要 Jing（c1a2b3）：看一下 billing\n"
            "這行提到需要 Jing 但不是固定行\n"
            "需要 Jing：無\n")
    got = [(x["kind"], x["text"], x["ref"], x["none"]) for x in board.fixed_lines(text)]
    assert got == [("need", "跑 rotate.sh", None, False), ("need", "merge shop #412", None, False),
                   ("deploy", "shop #412 → production", None, False), ("need", "看一下 billing", "c1a2b3", False),
                   ("need", "無", None, True)]
    assert board.fixed_lines("nothing here") == []


def test_opened_prs_by_words_beside_the_number():
    repos = {"shop": ("bitbucket", "acme")}
    assert board.opened_prs("#412 開好了：https://bitbucket.org/acme/shop/pull-requests/412", repos) == [
        ("bitbucket", "acme", "shop", 412)]
    assert board.opened_prs("Opened shop #413 for review", repos) == [("bitbucket", "acme", "shop", 413)]
    # Named, not opened: a review, a merge.
    assert board.opened_prs("Reviewed shop #412; merge it after shop #400", repos) == []


def _lead_answer(h, text, item=None):
    "lead-b answers a handoff from lead-a with TEXT; returns the handoff."
    out = h.handoff("/src/b", "Please look at it", from_name="lead-a", item=item)
    _deliver_all(h)
    h.reply(out["id"], text, from_name="lead-b")
    return out


def test_a_leads_lines_in_its_answer_are_its_items_events(tmp_path):
    h = _store(tmp_path)
    out = _lead_answer(h, "Rotated staging.\n需要 Jing：在維護時段跑 rotate.sh\n部署：staging 三個 namespace")
    c = _items(h)[_item_of(h, out["id"])]
    assert [(n["text"], n["agent"], n["ask"]) for n in c["needs"]] == [("在維護時段跑 rotate.sh", "lead-b", out["id"])]
    assert [d["text"] for d in c["deploys"]] == ["staging 三個 namespace"]
    tl = [(e["kind"], e["head"], e.get("state")) for e in h.board_item(out["id"])["events"] if e["key"]]
    assert ("need", "在維護時段跑 rotate.sh", "open") in tl and ("deploy", "staging 三個 namespace", None) in tl
    # A worker's line is not the user's to read.
    a = h.ask("agy-a2", "check it", from_name="lead-a")
    _deliver_all(h)
    h.reply(a["id"], "需要 Jing：給我 token", from_name="agy-a2")
    assert _items(h)[_item_of(h, a["id"])]["needs"] == []


def test_how_a_need_ends(tmp_path):
    """Replaced by the lead's next need lines in the item; ended by
    "需要 Jing：無"; handled by the user; and the item closed."""
    h = _store(tmp_path)
    out = _lead_answer(h, "需要 Jing：A\n需要 Jing：B")
    it = _item_of(h, out["id"])
    assert [n["text"] for n in _items(h)[it]["needs"]] == ["A", "B"]
    # The lead again, in the item: what it says now is what is needed.
    _lead_answer(h, "需要 Jing：B, still", item=it)
    assert [n["text"] for n in _items(h)[it]["needs"]] == ["B, still"]
    _lead_answer(h, "需要 Jing：無", item=it)
    assert _items(h)[it]["needs"] == []
    _lead_answer(h, "需要 Jing：C", item=it)
    need = _items(h)[it]["needs"][0]
    assert h.open_needs()["needs"][0]["text"] == "C"
    assert h.need_close(need["id"]) == {"id": need["id"], "closed": True}
    assert _items(h)[it]["needs"] == [] and h.open_needs()["needs"] == []
    _lead_answer(h, "需要 Jing：D", item=it)
    h.item_edit("close", it, by="lead-a")
    assert _items(h)[it]["needs"] == []


def test_a_leads_report_lines_go_to_the_item_it_was_at(tmp_path):
    """The words that end a lead's turn: their lines go to the item of the
    ask it was answering, or the one a line names; read twice, kept once."""
    h = _store(tmp_path)
    out = h.handoff("/src/b", "Rotate the key", from_name="lead-a")
    _deliver_all(h)
    other = h.ask("agy-a2", "Unrelated work", from_name="lead-a")
    h.board()
    msg = "Working on it.\n需要 Jing：approve the maintenance window"
    h.report("lead-b", state="idle", last=msg)
    h.report("lead-b", state="idle", last=msg)
    c = _items(h)[_item_of(h, out["id"])]
    assert [n["text"] for n in c["needs"]] == ["approve the maintenance window"]
    # A line naming another item goes there.
    oi = _item_of(h, other["id"])
    h.report("lead-b", state="idle", last=f"部署（{oi}）：billing v2 to staging")
    assert [d["text"] for d in _items(h)[oi]["deploys"]] == ["billing v2 to staging"]
    # A worker's report is not read for lines.
    assert h.note_report_lines("agy-a2", "需要 Jing：x") == 0


def test_a_pr_is_the_items_whose_answer_said_it_was_opened(tmp_path):
    """Asked for before it had a number: the answer that says it was opened
    owns it, not the first ask to name it."""
    h = _store(tmp_path)
    review = h.ask("agy-a2", "Look at https://bitbucket.org/acme/shop/pull-requests/40", from_name="lead-a")
    _deliver_all(h)
    h.reply(review["id"], "Looks fine", from_name="agy-a2")
    work = h.ask("agy-a2", "Fix the cart and open a PR", from_name="lead-a")
    _deliver_all(h)
    h.reply(work["id"], "Opened https://bitbucket.org/acme/shop/pull-requests/41", from_name="agy-a2")
    later = h.ask("agy-a2", "Review shop #41 and shop #40 together", from_name="lead-a")
    items = _items(h)
    iw, il = _item_of(h, work["id"]), _item_of(h, later["id"])
    pr41 = {p["id"]: p for p in items[iw]["prs"]}[41]
    assert pr41["own"] and pr41["opened"]["ask"] == work["id"]
    assert {p["id"]: p["own"] for p in items[il]["prs"]} == {41: False, 40: False}
    tl = [(e["kind"], e["head"]) for e in h.board_item(work["id"])["events"] if e["kind"] == "open"]
    assert tl == [("open", "shop #41 開好")]


def test_a_deploy_line_marks_its_prs_deployed(tmp_path):
    h = _store(tmp_path)
    work = h.ask("agy-a2", "Fix it", from_name="lead-a")
    _deliver_all(h)
    h.reply(work["id"], "Opened https://bitbucket.org/acme/shop/pull-requests/41", from_name="agy-a2")
    it = _item_of(h, work["id"]) or (h.board() and _item_of(h, work["id"]))
    _lead_answer(h, "部署：#41 到 production 三站", item=it)
    pr = _items(h)[it]["prs"][0]
    assert pr["deployed"]["text"] == "#41 到 production 三站"


def test_a_request_that_went_nowhere(tmp_path):
    """The last ask cancelled, failed, or its taker stopped without
    answering, nothing open: the card says so (Emacs flags it 沒人接)."""
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Do the thing", from_name="lead-a")
    _deliver_all(h)
    h.cancel(a["id"])
    c = _items(h)[_item_of(h, a["id"])]
    assert c["unanswered"]["ask"] == a["id"] and c["unanswered"]["status"] == "cancelled"
    b = h.ask("agy-a2", "Again", from_name="lead-a", item=a["id"])
    assert _items(h)[_item_of(h, a["id"])]["unanswered"] is None
    _deliver_all(h)
    h.reply(b["id"], "done", from_name="agy-a2")
    assert _items(h)[_item_of(h, a["id"])]["unanswered"] is None


def test_an_old_item_with_a_need_open_stays_on_the_board(tmp_path):
    h = _store(tmp_path)
    out = _lead_answer(h, "需要 Jing：decide on the vendor")
    h.board()  # its lines read as it came
    _backdate(h, out["id"], 3 * 24 * 60)
    with h._mu:
        h._conn.execute("UPDATE item_events SET at = ?", ((datetime.now(timezone.utc) - timedelta(days=3)).isoformat(),))
        h._conn.commit()
    assert _item_of(h, out["id"]) in _items(h, 1)


def test_a_need_under_no_item(tmp_path):
    h = _store(tmp_path)
    h.report("lead-c", state="idle", last="需要 Jing：tell me which region")
    out = h.board()
    assert [n["text"] for n in out["needs"]] == ["tell me which region"]
    assert h.open_needs()["needs"][0]["item"] == ""


def test_the_lines_the_leads_are_taught_are_the_lines_read():
    """The lead role (ghostherd-lead.el) teaches the two fixed lines as they
    are to be written: each, as taught, is read back as its kind."""
    role = (Path(__file__).resolve().parents[2] / "ghostherd-lead.el").read_text(encoding="utf-8")
    role = role[role.index("(defconst ghostherd--lead-role"):]
    taught = [ln for ln in role.splitlines() if ln.strip().startswith(("需要 Jing：", "部署："))]
    assert len(taught) == 2
    kinds = [board.fixed_lines(ln)[0]["kind"] for ln in taught]
    assert kinds == ["need", "deploy"]
    assert board.fixed_lines("需要 Jing：無")[0]["none"]


def test_lines_from_before_the_board_kept_them_are_not_needs_now(tmp_path):
    """A reply answered before the Board kept lines -- read again when it
    grew them -- is not taken for a need open now."""
    h = _store(tmp_path)
    out = _lead_answer(h, "需要 Jing：an old request")
    with h._mu:
        h._conn.execute("UPDATE meta SET v = ? WHERE k = 'board_events_since'",
                        ((datetime.now(timezone.utc) + timedelta(minutes=5)).isoformat(),))
        h._conn.execute("DELETE FROM item_events")
        h._conn.execute("UPDATE ask_item SET replied = 0")
        h._conn.commit()
    assert _items(h)[_item_of(h, out["id"])]["needs"] == []


# ---- `herd item' on the command line ------------------------------------------------


def _cli(monkeypatch):
    """bin/herd as a module, its RPCs recorded -- (method, params) -- and
    answered as the sidecar does; run as lead-a."""
    import importlib.machinery
    import importlib.util
    import sys
    from tests.test_ask import HERD
    loader = importlib.machinery.SourceFileLoader("herd_cli_board", str(HERD))
    spec = importlib.util.spec_from_loader("herd_cli_board", loader)
    mod = importlib.util.module_from_spec(spec)
    was, sys.dont_write_bytecode = sys.dont_write_bytecode, True
    try:
        loader.exec_module(mod)
    finally:
        sys.dont_write_bytecode = was
    sent = []

    def rpc(method, params=None, timeout=15):
        sent.append((method, params))
        if method == "herd_items":
            return {"items": [], "more": False, "days": 1}
        out = {"item": (params or {}).get("id") or "n1"}
        if (params or {}).get("op") in ("split", "move"):
            out["moved"] = params["asks"]
        return out
    monkeypatch.setattr(mod, "rpc", rpc)
    monkeypatch.setenv("GHOSTHERD_SESSION", "lead-a")
    return mod, sent


def test_item_rename_takes_its_title_as_words_or_title(monkeypatch, capsys):
    """`rename ID --title T' renames: the --title was once dropped, the
    title went out empty and gave the item back its first ask's name."""
    hc, sent = _cli(monkeypatch)
    assert hc.main(["item", "rename", "w1", "--title", "Rotate the keys"]) == 0
    assert sent[-1] == ("herd_item_edit", {"op": "rename", "by": "lead-a", "id": "w1", "title": "Rotate the keys"})
    assert "renamed: Rotate the keys" in capsys.readouterr().err
    assert hc.main(["item", "rename", "w1", "Rotate", "the", "keys"]) == 0
    assert sent[-1][1]["title"] == "Rotate the keys"
    # Its own name back: said so, never by leaving the title out.
    assert hc.main(["item", "rename", "w1", ""]) == 0
    assert sent[-1][1]["title"] == ""
    assert hc.main(["item", "rename", "w1", "--title", ""]) == 0
    assert sent[-1][1]["title"] == ""
    assert "back to the name its first ask gives" in capsys.readouterr().err
    n = len(sent)
    assert hc.main(["item", "rename", "w1"]) == 1
    assert "rename w1 to what?" in capsys.readouterr().err
    assert hc.main(["item", "rename", "w1", "A", "--title", "B"]) == 1
    assert "give the title once" in capsys.readouterr().err
    assert len(sent) == n


def test_item_options_an_action_does_not_take_are_refused(monkeypatch, capsys):
    hc, sent = _cli(monkeypatch)
    for argv, opt in ((["item", "merge", "w1", "w2", "--title", "X"], "--title"),
                      (["item", "close", "w1", "--remove"], "--remove"),
                      (["item", "show", "w1", "--days", "3"], "--days"),
                      (["item", "label", "w1", "deploy", "--json"], "--json"),
                      (["item", "move", "w1", "1a2b3c4d", "--title", "X"], "--title")):
        assert hc.main(argv) == 1, argv
        assert ("takes no " + opt) in capsys.readouterr().err
    assert sent == []
    # Those that take them, do.
    assert hc.main(["item", "split", "1a2b3c4d", "--title", "Alone"]) == 0
    assert sent[-1][1] == {"op": "split", "by": "lead-a", "asks": ["1a2b3c4d"], "title": "Alone"}
    assert hc.main(["item", "label", "w1", "deploy", "--remove"]) == 0
    assert sent[-1][1]["remove"] is True
    assert hc.main(["item", "list", "--days", "7"]) == 0
    assert sent[-1] == ("herd_items", {"days": 7.0})
    assert hc.main(["item"]) == 0
    assert sent[-1] == ("herd_items", {"days": 1})


def test_board_counts_for_the_tabs_badge(tmp_path):
    """The badge's counts, without the board: an item waiting on the user (a
    lead's need open, not live -- however long: its lead waits on the user,
    and says so, `late'), one nobody has (quiet past the hours, its last ask
    gone nowhere, no need open) -- the same the page shows."""
    h = _store(tmp_path)
    assert h.board_counts() == {"jing": 0, "orphan": 0, "bare": 0}
    need = _lead_answer(h, "需要 Jing：pick a vendor")
    gone = h.ask("agy-a2", "Do the thing", from_name="lead-a")
    _deliver_all(h)
    h.cancel(gone["id"])
    live = _lead_answer(h, "需要 Jing：while it is still at work")
    h.ask("agy-a2", "and more", from_name="lead-a", item=live["id"])
    assert h.board_counts() == {"jing": 1, "orphan": 0, "bare": 0}
    # Five hours later: quiet past four, the one gone nowhere is nobody's;
    # the need open still waits on the user -- long, not nobody's.
    for a in (need["id"], gone["id"]):
        _backdate(h, a, 5 * 60)
    with h._mu:
        h._conn.execute("UPDATE item_events SET at = ?", ((datetime.now(timezone.utc) - timedelta(hours=5)).isoformat(),))
        h._conn.commit()
    # The one gone nowhere has no need open: 要你處理 lists it apart (bare).
    assert h.board_counts(orphan_hours=4) == {"jing": 1, "orphan": 1, "bare": 1}
    cards = _items(h)
    n, g = cards[_item_of(h, need["id"])], cards[_item_of(h, gone["id"])]
    assert (n["orphan"], n["late"], g["orphan"], g["late"]) == (False, True, True, False)
    assert h.board_counts(orphan_hours=24) == {"jing": 1, "orphan": 0, "bare": 0}
    assert _items(h, hours=24)[_item_of(h, need["id"])]["late"] is False
    # Closed by a lead: neither.
    h.item_edit("close", need["id"], by="lead-a")
    assert h.board_counts(orphan_hours=4) == {"jing": 0, "orphan": 1, "bare": 1}


def _badge_of_board(h, days, hours=4):
    """The badge as the page counts it with the Board open: Emacs takes a
    card's `orphan' as 沒人接, a need open on one not live nor done as 等
    Jing (the page's bdUrgent)."""
    out = {"jing": 0, "orphan": 0, "bare": 0}
    for c in h.board(days, hours)["items"]:
        if c["orphan"]:
            out["orphan"] += 1
            out["bare"] += not c["needs"]
        elif c["status"] not in ("live", "done") and c["needs"]:
            out["jing"] += 1
    return out


def _ended(h, ask_id, how, minutes):
    "Ask ASK_ID ended with no answer -- failed, cancelled, stopped -- MINUTES ago."
    when = (datetime.now(timezone.utc) - timedelta(minutes=minutes)).isoformat()
    with h._mu:
        h._conn.execute("UPDATE asks SET status = ?, auto = ?, created_at = ?, delivered_at = ?, answered_at = ? WHERE id = ?",
                        ("answered" if how == "stopped" else how, 1 if how == "stopped" else 0, when, when, when, ask_id))
        h._conn.commit()


def test_the_badge_counts_what_the_board_shows(tmp_path):
    """An item whose last ask went nowhere three days ago is past the
    Board's window of a day: the badge counts it no more than the Board
    shows it.  A week's window holds it: it counts in both."""
    h = _store(tmp_path)
    gone = h.ask("agy-a2", "Do the thing", from_name="lead-a")
    _deliver_all(h)
    _ended(h, gone["id"], "cancelled", 3 * 24 * 60)
    it = _item_of(h, gone["id"])
    assert it not in _items(h, 1)
    assert h.board_counts(1) == {"jing": 0, "orphan": 0, "bare": 0} == _badge_of_board(h, 1)
    assert _items(h, 7)[it]["orphan"] is True
    assert h.board_counts(7) == {"jing": 0, "orphan": 1, "bare": 1} == _badge_of_board(h, 7)


def test_the_badge_and_the_board_agree_on_every_kind(tmp_path):
    """Whatever the window and the hours, the badge (the tab closed) and
    the Board's own cards (the tab open) count the same: needs open, old or
    new, are always shown; asks gone nowhere -- failed, cancelled, stopped
    -- are counted while the window holds them; live and closed ones never."""
    h = _store(tmp_path)
    ago = lambda minutes: (datetime.now(timezone.utc) - timedelta(minutes=minutes)).isoformat()
    needs = []
    for minutes in (10, 5 * 60, 3 * 24 * 60):
        out = _lead_answer(h, f"需要 Jing：pick one of {minutes}")  # one message is kept once: each its own
        _item_of(h, out["id"])  # its line read before it is made old
        _backdate(h, out["id"], minutes)
        needs.append((out["id"], minutes))
    with h._mu:
        for a, minutes in needs:
            h._conn.execute("UPDATE item_events SET at = ? WHERE ask = ?", (ago(minutes), a))
        h._conn.commit()
    for how, minutes in (("failed", 60), ("cancelled", 5 * 60), ("stopped", 2 * 24 * 60), ("failed", 3 * 24 * 60)):
        a = h.ask("agy-a2", f"Do it ({how}, {minutes})", from_name="lead-a")
        _deliver_all(h)
        _ended(h, a["id"], how, minutes)
    live = _lead_answer(h, "需要 Jing：while still at work")
    h.ask("agy-a2", "and more", from_name="lead-a", item=_item_of(h, live["id"]))
    shut = _lead_answer(h, "需要 Jing：later")
    h.item_edit("close", shut["id"], by="lead-a")
    seen = set()
    for days in (0.5, 1, 7):
        for hours in (0, 4, 24):
            counts = h.board_counts(days, hours)
            assert counts == _badge_of_board(h, days, hours), (days, hours)
            seen.add(tuple(counts.values()))
    # A day: the needs -- of ten minutes, five hours, three days -- wait on
    # the user, however old; the ask cancelled five hours ago is nobody's,
    # the one failed an hour ago not yet.  A week adds the two gone two and
    # three days ago.
    assert h.board_counts(1, 4) == {"jing": 3, "orphan": 1, "bare": 1}
    assert h.board_counts(7, 4) == {"jing": 3, "orphan": 3, "bare": 3}
    assert len(seen) > 3


def test_the_badge_and_the_board_take_the_window_over_rpc(client):
    """The page's window reaches both RPCs: herd_board_counts takes the
    Board's days, herd_board the hours an item may be quiet before it is
    nobody's."""
    from herd import get_herd
    from tests.test_ask import _rpc
    h = get_herd()
    gone = h.ask("agy-a2", "Do the thing", from_name="claude-main")
    _ended(h, gone["id"], "failed", 3 * 24 * 60)
    assert _rpc(client, "herd_board_counts", {"days": 1, "orphan_hours": 4})["result"] == {"jing": 0, "orphan": 0, "bare": 0}
    assert _rpc(client, "herd_board_counts", {"days": 7, "orphan_hours": 4})["result"] == {"jing": 0, "orphan": 1, "bare": 1}
    assert _rpc(client, "herd_board_counts", {})["result"] == {"jing": 0, "orphan": 0, "bare": 0}
    [c] = _rpc(client, "herd_board", {"days": 7, "orphan_hours": 100})["result"]["items"]
    assert c["orphan"] is False
    [c] = _rpc(client, "herd_board", {"days": 7})["result"]["items"]
    assert c["orphan"] is True
    assert "days must be a number" in _rpc(client, "herd_board_counts", {"days": "a week"})["error"]["message"]


def test_a_leads_ticket_label_is_one_of_its_tickets(tmp_path):
    h = _store(tmp_path)
    a = h.ask("agy-a2", "Fix the cart", from_name="lead-a")
    h.item_edit("label", a["id"], by="lead-a", labels=["CU-86zz9", "deploy"])
    c = _items(h)[_item_of(h, a["id"])]
    assert [t["id"] for t in c["tickets"]] == ["86zz9"] and c["labels"] == ["CU-86zz9", "deploy"]
