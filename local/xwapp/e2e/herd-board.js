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
        tiles.map((t) => t.big).join() === "10,4,2,6", tiles.map((t) => t.big));
      check("desk: the items tile says what the asks were", /34 則 ask：跨專案 9、FYI 4、轉述 1、撤回／失敗 2/.test(tiles[0].sub), tiles[0].sub);
      check("desk: the waits tile names the cross-project pairs and the oldest", /跨專案 2：shop → billing、infra → ops-helm/.test(tiles[1].sub) &&
        /專案內 2/.test(tiles[1].sub) && /最久的開了 20 分/.test(tiles[1].sub), tiles[1].sub);
      check("desk: the board tab shows how many items have something going on", (await page.textContent("#board-n")) === "5");

      const order = await page.$$eval(".cards .wi", (ws) => ws.map((w) => w.id.slice(5) + ":" + w.querySelector(".pill").textContent + (w.classList.contains("folded") ? ":f" : "")));
      check("desk: live first, then waiting on Jing, the PR still open, quiet, done -- each by its last",
        order.join(" ") === "c1a2b3:進行中 d4e5f6:進行中 e7f8a9:進行中 b1c2d3:等 Jing f0a1b2:收尾 9a8b7c:安靜:f a3b4c5:安靜:f c6d7e8:安靜:f 0f1e2d:完成:f 3c4d5e:完成:f", order);

      const c1 = await page.$eval("#bd-w-c1a2b3", (w) => ({
        prs: [...w.querySelectorAll(".bdpr")].map((p) => p.textContent), blk: w.querySelector(".blk").textContent,
        tags: [...w.querySelectorAll(".tk,.pj")].map((t) => t.textContent), tl: w.querySelector(".bdtlb").textContent }));
      check("desk: a card's PRs with what the forge says, and where they go", c1.prs.join("|") === "shop #412open → production|billing #88已 merge → master", c1.prs);
      check("desk: a live card says what it waits on, whether its taker is at it", /卡在等 taskaudit|卡在等 taxaudit（在做/.test(c1.blk), c1.blk);
      check("desk: its ticket and projects", c1.tags.join() === "CU-86b2k1,shop,billing", c1.tags);
      check("desk: its timeline is folded, saying what it folds away", /▸ 時間線 · 6 則 ask（折疊：1 則 FYI、1 則「收到」）/.test(c1.tl), c1.tl);
      const pr57 = await page.$eval('#bd-w-e7f8a9 .bdpr.notown', (p) => p.getAttribute("title"));
      check("desk: a PR another item owns is dashed and says whose", /屬於 b1c2d3/.test(pr57), pr57);
      const jingBlk = await page.$eval("#bd-w-b1c2d3 .blk", (b) => b.textContent);
      check("desk: an item waiting on Jing says on what, from the Inbox, as a guess", /等 Jinglead-infra：rotate\.sh 要現在跑/.test(jingBlk) && /推得/.test(jingBlk), jingBlk);

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
      check("desk: the timeline without the FYI and its 收到", lis.join() === "ask,回覆,handoff,ask", lis);
      await page.click('#bd-w-c1a2b3 [data-act="bd:tlall"]');
      await sleep(80);
      check("desk: shown on asking", (await page.$$("#bd-w-c1a2b3 .bdtl li")).length === 6);

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
      check("desk: an item gone is dropped", !(await page.$("#bd-w-3c4d5e")) && (await page.textContent(".bdtile .big")) === "9");
      await page.evaluate(() => GM.boardPatch({ v: 9, base: 7, full: false, cards: [], drop: [] }));
      await sleep(80);
      check("desk: a version missed asks for the whole again", ops(intents, "board-resend").length === 1);

      // Filters.
      await page.click('[data-act="bd:filter"][data-f="busy"]');
      await sleep(80);
      check("desk: 還有事 keeps the live, waiting-on-Jing and tail ones", (await page.$$(".cards .wi")).length === 5);
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

      // Waits on Jing go to the Inbox.
      await page.click('.ji [data-act="bd:inbox"]');
      await sleep(80);
      check("desk: what waits on Jing is handled in the Inbox", ops(intents, "inbox-watch").length >= 1 &&
        ops(intents, "board-unwatch").length === 1 && (await page.getAttribute("#app", "data-view")) === "inbox");
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
      const tap = await page.$eval("#bd-w-a3b4c5 .wh", (b) => b.getBoundingClientRect().height);
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
