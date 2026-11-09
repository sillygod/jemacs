// The Board tab: the herd's work, item by item.  Watched only while shown;
// the whole once, then the cards that changed, by version -- a version
// missed asks for the whole again; the map, the open asks nested under the
// one they were made answering; work going on open, work at rest folded; a
// timeline asked for when opened, its FYI and 收到 folded away; the PRs by
// repository; what waits on Jing, to the Inbox.  The phone: one column, no
// sideways scroll at 390 px.  Fixture data (board-fixture.js), Emacs played
// by the test.  BOARD_SHOTS=DIR saves board-desk.png and board-phone.png.
const path = require("path");
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const { FULL, CARDS, TIMELINE } = require("./board-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const SHOTS = process.env.BOARD_SHOTS || "";
const ops = (intents, op) => intents.filter((i) => i.op === op);
// The page scrolls in #app, not as a document: the whole board is shot in
// a window as tall as it.
async function shoot(page, width, file) {
  const h = await page.evaluate(() => document.querySelector(".top").offsetHeight + document.querySelector("#app").scrollHeight + 20);
  await page.setViewportSize({ width, height: Math.min(h, 16000) });
  await sleep(200);
  await page.screenshot({ path: file });
}

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- The desk --------------------------------------------------------
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: DESK });
      await sleep(100);
      check("desk: nothing of the board is asked for before the tab is opened", !ops(intents, "board-watch").length);
      await page.click('.tab[data-tab="board"]');
      await sleep(100);
      check("desk: opening the tab watches the board, for the days kept", ops(intents, "board-watch").length === 1 &&
        ops(intents, "board-watch")[0].days === 1, ops(intents, "board-watch"));
      check("desk: until Emacs answers, it says it is reading", (await page.textContent(".bdbody")).includes("讀取"));
      await page.evaluate((p) => GM.boardPatch(p), FULL);
      await sleep(150);

      const tiles = await page.$$eval(".bdtile", (ts) => ts.map((t) => ({ big: t.querySelector(".big").textContent, sub: t.querySelector(".sub").textContent })));
      check("desk: the tiles count items, open waits, waits on Jing and PRs",
        tiles.map((t) => t.big).join() === "17,4,4,6", tiles.map((t) => t.big));
      check("desk: the items tile says what the asks were", /34 則 ask：跨專案 9、FYI 4、轉述 1、撤回／失敗 2/.test(tiles[0].sub), tiles[0].sub);
      check("desk: the waits tile names the cross-project pairs and the oldest", /跨專案 2：shop → billing、infra → ops-helm/.test(tiles[1].sub) &&
        /專案內 2/.test(tiles[1].sub) && /最久的開了 20 分/.test(tiles[1].sub), tiles[1].sub);
      check("desk: the board tab's red count: items nobody has, and those waiting on Jing by a lead's line", (await page.textContent("#board-n")) === "2");
      check("desk: the Jing tile counts the leads' lines and the Inbox's, and the items nobody has",
        /lead 寫的「需要 Jing」2 件/.test(tiles[2].sub) && /Inbox 等你處理 2 件/.test(tiles[2].sub) && /沒人接 1 項/.test(tiles[2].sub), tiles[2].sub);

      const order = await page.$$eval(".bdarea .wi", (ws) => ws.map((w) => w.id.slice(5) + ":" + w.querySelector(".pill").textContent + (w.classList.contains("folded") ? ":f" : "")));
      check("desk: on top what needs Jing's eye -- live, waiting on Jing, nobody's, a PR open -- then the items at rest of some size, folded",
        order.join(" ") === "c1a2b3:進行中 d4e5f6:進行中 e7f8a9:進行中 b1c2d3:等 Jing c6d7e8:沒人接 f0a1b2:收尾 9a8b7c:安靜:f 0f1e2d:完成:f", order);
      const folds = await page.$$eval(".bdfoldh", (fs) => fs.map((f) => f.querySelector(".fl").textContent));
      check("desk: the rest at rest, a line a project, the latest first",
        folds.join(" | ") === "shop：零星 3 則 · 最後 40 分前 | billing：零星 3 則 · 最後 1 小時前 | infra：零星 2 則（完成 1） · 最後 5 小時前 | mobile：零星 1 則 · 最後 8 小時前", folds);
      await page.click('.bdfoldh[data-p="billing"]');
      await sleep(80);
      const opened = await page.$$eval('.bdfold[data-p="billing"] .wi', (ws) => ws.map((w) => w.id.slice(5)));
      check("desk: a tap opens that project's cards", opened.join() === "51a004,51a005,a3b4c5", opened);
      check("desk: the others stay folded", !(await page.$('.bdfold[data-p="shop"] .wi')));

      const c1 = await page.$eval("#bd-w-c1a2b3", (w) => ({
        prs: [...w.querySelectorAll(".bdpr")].map((p) => p.textContent), blk: w.querySelector(".blk").textContent,
        tags: [...w.querySelectorAll(".tk,.pj")].map((t) => t.textContent), tl: w.querySelector(".bdtlb").textContent }));
      check("desk: a card's PRs with what the forge says, and where they go", c1.prs.join("|") === "shop #412open → production|billing #88已部署 → master", c1.prs);
      check("desk: a live card says what it waits on, whether its taker is at it", /卡在等 taskaudit|卡在等 taxaudit（在做/.test(c1.blk), c1.blk);
      check("desk: its ticket and projects", c1.tags.join() === "CU-86b2k1,shop,billing", c1.tags);
      check("desk: its timeline is folded, saying what it folds away", /▸ 時間線 · 6 則 ask（折疊：1 則 FYI、1 則「收到」）/.test(c1.tl), c1.tl);
      const pr57 = await page.$eval('#bd-w-e7f8a9 .bdpr.notown', (p) => p.getAttribute("title"));
      check("desk: a PR another item owns is dashed and says whose", /屬於 b1c2d3/.test(pr57), pr57);
      const jingBlk = await page.$eval("#bd-w-b1c2d3 .blk", (b) => b.textContent);
      check("desk: an item waiting on Jing says what its lead wrote, with Handled", /等 Jing在今晚的維護時段/.test(jingBlk) && /已處理/.test(jingBlk), jingBlk);
      const orphan = await page.$eval("#bd-w-c6d7e8 .blk", (b) => b.textContent);
      check("desk: an item nobody has says how long it is quiet and what is left", /沒人接安靜 7 小時，還有「需要 Jing」沒處理：三個方案選一個/.test(orphan), orphan);
      const dep = await page.$eval("#bd-w-c1a2b3 .bddep", (b) => b.textContent);
      check("desk: a card shows its last 部署 line", /部署billing #88 → production 三站/.test(dep), dep);

      // The map.
      const nodes = await page.$$eval(".bdg-svg .nd text", (ts) => ts.map((t) => t.textContent));
      check("desk: the map of now: the projects with open asks or waits on Jing, and Jing",
        ["shop", "billing", "infra", "ops-helm", "mobile", "Jing"].every((n) => nodes.includes(n)), nodes);
      const edges = await page.$$eval(".bdg-svg path", (ps) => ps.map((p) => p.getAttribute("class")));
      check("desk: the open cross-project asks blue, the waits on Jing red",
        edges.filter((e) => e === "e-now").length === 2 && edges.filter((e) => e === "e-jing").length === 2, edges);
      check("desk: an asks-only-within-a-project count under its project", (await page.textContent(".bdg-svg")).includes("內部 1"));
      await page.click('[data-act="bd:graph"][data-g="all"]');
      await sleep(80);
      const all = await page.$$eval(".bdg-svg path", (ps) => ps.map((p) => p.getAttribute("class")));
      check("desk: the window's cross-project asks, a relay dashed apart",
        all.filter((e) => e === "e-all").length === 5 && all.filter((e) => e === "e-relay").length === 1, all);
      check("desk: its chip counts them", (await page.textContent('[data-act="bd:graph"][data-g="all"]')).includes("（10）"));
      await page.click('[data-act="bd:graph"][data-g="now"]');

      // The open asks.
      const waits = await page.$$eval(".bdwaits .wait", (ws) => ws.map((w) => w.querySelector(".wrow").textContent.replace(/\s+/g, " ")));
      check("desk: open asks across projects first, a handoff dashed-arrowed", waits[0].startsWith("lead-shop⇢lead-billing") && waits[1].startsWith("lead-infra⇢lead-ops-helm"), waits);
      const chain = await page.$eval(".bdwaits .wait .chain", (c) => c.textContent);
      check("desk: the ask made answering one is nested under it", /└ lead-billing → taxaudit/.test(chain), chain);
      check("desk: the first two waits on Jing under them", (await page.$$(".bdwaits .wj")).length === 2);

      // Fold and unfold; a timeline asked for when opened.
      await page.click('#bd-w-a3b4c5 .wh');
      await sleep(80);
      check("desk: a folded card opens on a tap", !(await page.$eval("#bd-w-a3b4c5", (w) => w.classList.contains("folded"))));
      await page.click('.bdfoldh[data-p="billing"]');
      await sleep(80);
      check("desk: and its project folds again", !(await page.$("#bd-w-a3b4c5")));
      await page.click('#bd-w-c1a2b3 .wh');
      await sleep(80);
      check("desk: and an open one folds", await page.$eval("#bd-w-c1a2b3", (w) => w.classList.contains("folded")));
      await page.click('#bd-w-c1a2b3 .wh');
      await sleep(80);
      await page.click('#bd-w-c1a2b3 .bdtlb');
      await sleep(80);
      check("desk: opening a timeline asks for it", ops(intents, "board-timeline").map((i) => i.id).join() === "c1a2b3");
      await page.evaluate((t) => GM.boardTimeline(t), TIMELINE);
      await sleep(80);
      const lis = await page.$$eval("#bd-w-c1a2b3 .bdtl li", (ls) => ls.map((l) => l.querySelector(".kk").textContent));
      check("desk: the timeline's key events: the first ask, the PR opened, the need, its merge and deploy, what is open",
        lis.join() === "ask,開 PR,Jing,merge,部署,handoff,ask", lis);
      check("desk: a need handled says so", (await page.textContent("#bd-w-c1a2b3 .bdtl li.past")).includes("已處理"));
      check("desk: its summary counts the key events", (await page.textContent("#bd-w-c1a2b3 .bdtlb")).includes("7 件關鍵事件"));
      await page.click('#bd-w-c1a2b3 [data-act="bd:tlall"]');
      await sleep(80);
      check("desk: every ask and answer on asking, the FYI and 收到 too", (await page.$$("#bd-w-c1a2b3 .bdtl li")).length === 10);

      // A patch: the card that changed alone.
      await page.evaluate(() => { document.querySelector("#bd-w-d4e5f6").dataset.mark = "kept"; });
      const changed = Object.assign({}, CARDS[0], { open: 1, last: Date.now(), block: Object.assign({}, CARDS[0].block, { head: "對照表寫好一半" }) });
      await page.evaluate((c) => GM.boardPatch({ v: 2, base: 1, full: false, cards: [c], drop: [] }), changed);
      await sleep(80);
      check("desk: a patch redraws the card that changed, not the others",
        (await page.$eval("#bd-w-d4e5f6", (w) => w.dataset.mark)) === "kept" &&
        (await page.textContent("#bd-w-c1a2b3 .blk")).includes("對照表寫好一半"));
      check("desk: an open timeline whose card moved on is asked for again", ops(intents, "board-timeline").length === 2);
      await page.evaluate(() => GM.boardPatch({ v: 3, base: 2, full: false, cards: [], drop: ["3c4d5e"] }));
      await sleep(80);
      check("desk: an item gone is dropped", !(await page.$("#bd-w-3c4d5e")) && (await page.textContent(".bdtile .big")) === "16");
      await page.evaluate(() => GM.boardPatch({ v: 9, base: 7, full: false, cards: [], drop: [] }));
      await sleep(80);
      check("desk: a version missed asks for the whole again", ops(intents, "board-resend").length === 1);

      // A need handled on the Board.
      await page.click('#bd-w-b1c2d3 [data-act="bd:need-done"]');
      await sleep(60);
      check("desk: Handled tells Emacs which need", ops(intents, "board-need-done").map((i) => i.id).join() === "11",
        ops(intents, "board-need-done"));
      const jl = await page.$$eval(".bd-jing .ji", (ls) => ls.map((l) => (l.classList.contains("line") ? "line:" : "inbox:") + l.querySelector(".bdjline").textContent.slice(0, 6)));
      check("desk: what waits on Jing: the leads' lines first, the Inbox's after", jl.join("|") === "line:在今晚的維護|line:三個方案選一|inbox:rotate|inbox:Permis", jl);

      // Filters.
      await page.click('[data-act="bd:filter"][data-f="busy"]');
      await sleep(80);
      check("desk: 還有事 keeps the live, waiting-on-Jing, nobody's and tail ones", (await page.$$(".bdarea .wi")).length === 6 && !(await page.$(".bdfoldh")));
      await page.click('[data-act="bd:filter"][data-f="quiet"]');
      await sleep(80);
      check("desk: 安靜 lists every quiet item, none folded away", (await page.$$(".bdarea .wi")).length === 9 && !(await page.$(".bdfoldh")));
      await page.click('[data-act="bd:filter"][data-f="all"]');

      // PRs by repository.
      const repos = await page.$$eval(".prt tr.rg th", (ts) => ts.map((t) => t.textContent));
      check("desk: every PR once, by repository", repos.join("|") === "acme/billing 2|acme/ops-helm 2|acme/shop 2", repos);
      const row57 = await page.$$eval(".prt tbody tr:not(.rg)", (rs) => rs.map((r) => r.textContent).find((t) => t.startsWith("#57")));
      check("desk: a PR's item is the one that owns it, with how many more name it", /b1c2d3/.test(row57) && /＋1 項提到/.test(row57), row57);
      check("desk: 7 days on asking, kept", (await page.$('[data-act="bd:days"][data-days="7"]')) !== null);
      await page.click('[data-act="bd:days"]');
      await sleep(60);
      check("desk: asks Emacs for 7 days", ops(intents, "board-days").map((i) => i.days).join() === "7");

      if (SHOTS) {
        await page.evaluate((p) => GM.boardPatch(p), Object.assign({}, FULL, { v: 20 }));
        await sleep(150);
        await shoot(page, DESK.viewport.width, path.join(SHOTS, "board-desk.png"));
      }

      // A lead's need in the Inbox: Handled closes it on the Board too.
      await page.evaluate(() => GM.renderInbox({ count: 1, items: [{ id: "need:11", kind: "need", group: "need", tag: "需要 Jing", tone: "red",
        agent: "lead-infra", cli: "", project: "", title: "在今晚的維護時段跑 rotate.sh", at: Date.now() - 60e3, whyLabel: "A lead's 需要 Jing line",
        body: "在今晚的維護時段跑 rotate.sh", preview: "在今晚的維護時段跑 rotate.sh", state: "", read: false, override: null, event: 11, item: "b1c2d3" }] }));

      // Waits on Jing go to the Inbox.
      await page.click('.ji [data-act="bd:inbox"]');
      await sleep(80);
      check("desk: what waits on Jing is handled in the Inbox", ops(intents, "inbox-watch").length >= 1 &&
        ops(intents, "board-unwatch").length === 1 && (await page.getAttribute("#app", "data-view")) === "inbox");
      await sleep(80);
      if (!(await page.$('[data-act="ib:need-done"]'))) await page.click('[data-act="ib:pick"][data-id="need:11"]');
      await sleep(80);
      await page.click('[data-act="ib:need-done"]');
      await sleep(80);
      check("desk: a need is handled from the Inbox", ops(intents, "board-need-done").length === 2 && ops(intents, "board-need-done")[1].id === 11,
        ops(intents, "board-need-done"));
      check("desk: no page errors", !errors.length, errors);
      await ctx.close();
    }

    // ---- The phone -------------------------------------------------------
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, hash: "#board" });
      await sleep(100);
      check("phone: a page loaded on #board watches it", ops(intents, "board-watch").length === 1);
      await page.evaluate((p) => GM.boardPatch(p), FULL);
      await sleep(150);
      const wide = await page.evaluate(() => ({ doc: document.documentElement.scrollWidth, body: document.body.scrollWidth, w: window.innerWidth }));
      check("phone: nothing scrolls sideways at 390 px", wide.doc <= wide.w && wide.body <= wide.w, wide);
      const cols = await page.$eval(".cards", (c) => getComputedStyle(c).gridTemplateColumns.split(" ").length);
      const tcols = await page.$eval(".bdstrip", (c) => getComputedStyle(c).gridTemplateColumns.split(" ").length);
      check("phone: the cards in one column, the tiles two by two", cols === 1 && tcols === 2, { cols, tcols });
      const vb = await page.$eval(".bdg-svg", (s) => s.getAttribute("viewBox"));
      check("phone: the map drawn for the narrow screen", vb === "0 0 360 320", vb);
      const over = await page.$$eval(".boardpage *", (els) => els.filter((e) => e.getBoundingClientRect().right > window.innerWidth + 1 &&
        !e.closest(".gscroll")).map((e) => e.className || e.tagName).slice(0, 5));
      check("phone: no part of the board past the screen's edge", !over.length, over);
      const tap = await page.$eval("#bd-w-9a8b7c .wh", (b) => b.getBoundingClientRect().height);
      const fh = await page.$eval(".bdfoldh", (b) => b.getBoundingClientRect().height);
      check("phone: a project's fold line is a finger's target", fh >= 40, fh);
      check("phone: a folded card's head is a finger's target", tap >= 28, tap);
      if (SHOTS) {
        await page.click('#bd-w-c1a2b3 .bdtlb');
        await page.evaluate((t) => GM.boardTimeline(t), TIMELINE);
        await sleep(150);
        await shoot(page, 390, path.join(SHOTS, "board-phone.png"));
      }
      check("phone: no page errors", !errors.length, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
