// The Board is the page's home now the Inbox tab is gone (ask 8d6e1947).
// No Inbox tab; the page opens on the Board; the Inbox's old links -- #inbox,
// #inbox/ID -- land there, on the item; a push's link (#board/at/ID,
// #board/agent/NAME) opens what it is about.  The Inbox's updates are on
// the Board: one of work on its card, a dot while new, read as the card is
// opened, its words asked for then; one of no work item under 最近回報,
// folded, by agent, with 到對話.  One red count, the Board's, as many as
// 要你處理 lists, the tab open or not.  Desk and phone; Emacs played by
// the test (board-fixture.js, and the Inbox's items here).  Synthetic data.
const { webkit } = require("playwright");
const { phonePage, sleep, boardUnfold } = require("./herdlib");
const { FULL, now } = require("./board-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const min = 60e3;
const ops = (intents, op) => intents.filter((i) => i.op === op);
const it = (id, kind, group, tag, tone, agent, ago, extra) => Object.assign({ id, kind, group, tag, tone, agent, cli: "claude",
  project: "~/src/infra/", title: id, preview: "", at: now - ago * min, read: false, override: null, whyLabel: "", body: "", state: "" }, extra || {});
// What needs Jing: a menu, a question, a lead's line; the updates: a report
// of a turn an ask began (taxaudit's work, c1a2b3), one Jing began with
// copybot, and a lead the herd started -- no work item.
const ITEMS = [
  it("blocked:pushfix", "blocked", "need", "Permission", "warn", "pushfix", 6, { project: "~/src/mobile/", title: "Permission: run xcodebuild test",
    options: [{ n: 1, label: "Yes" }, { n: 2, label: "No" }] }),
  it("question:lead-infra:1", "question", "need", "Question", "accent", "lead-infra", 30, { title: "rotate.sh 要現在跑，還是等今晚？", replyVia: "lead" }),
  it("need:11", "need", "need", "需要 Jing", "red", "lead-infra", 35, { cli: "", title: "在今晚的維護時段跑 rotate.sh", event: 11, item: "b1c2d3",
    body: "在今晚的維護時段跑 rotate.sh\n\nWork item b1c2d3: staging 憑證輪換" }),
  it("report:taxaudit:1", "report", "fyi", "Report", "green", "taxaudit", 3, { project: "~/src/billing/", title: "對照表寫好了", preview: "對照表寫好了，三區差異列在下面",
    fromAsk: true }),
  it("report:copybot:2", "report", "fyi", "Report", "green", "copybot", 20, { project: "~/src/shop/", title: "文案改好了", preview: "結帳頁的文案改好了", fromAsk: false }),
  it("lead-started:lead-ops-helm", "lead-started", "fyi", "Lead", "muted", "lead-ops-helm", 50, { project: "~/src/ops-helm/",
    title: "Lead started for ops-helm", preview: "Started to take a handoff from lead-infra", body: "The herd started lead-ops-helm to take a handoff." }),
];
const NEEDS = ["blocked:pushfix", "question:lead-infra:1", "need:11"];

async function open(browser, where, hash, intents) {
  const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {}, hash });
  await sleep(120);
  return { ctx, page, errors };
}
const push = async (page, items) => {
  await page.evaluate((its) => GM.renderInbox({ items: its, count: its.filter((x) => x.group === "need").length, needsOnly: true }), items || ITEMS);
  await page.evaluate((p) => GM.boardPatch(p), FULL);
  await sleep(150);
};

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const W = (n) => where + ": " + n;
      let intents = [];
      let { ctx, page, errors } = await open(browser, where, "", intents);
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));

      // ---- Home: the Board, no Inbox tab ------------------------------------------
      const tabs = await page.$$eval(".tab", (ts) => ts.map((t) => t.getAttribute("data-tab")));
      check(W("no Inbox tab; the Board first"), !tabs.includes("inbox") && tabs[0] === "board" && !(await page.$("#inbox-n")), tabs);
      check(W("loaded with nothing asked for, the page opens on the Board"),
        (await page.getAttribute("#app", "data-view")) === "board" && (await page.evaluate(() => location.hash)) === "#board" &&
        ops(intents, "board-watch").some((i) => i.inbox === true));
      check(W("on the Board it asks Emacs for no badge: it counts its own"), !ops(intents, "board-peek").length,
        intents.map((i) => i.op));
      // What needs you alone first (Emacs's first push), the updates after: none lost.
      await page.evaluate((its) => GM.renderInbox({ items: its.filter((x) => x.group === "need"), count: 3, needsOnly: true, updates: false }), ITEMS);
      await page.evaluate((p) => GM.boardPatch(p), FULL);
      await sleep(120);
      check(W("要你處理 drawn from what needs you alone"), /要你處理/.test(await page.textContent(".bd-need")) && !(await page.$(".bdupdsec")));
      await push(page);
      await page.evaluate((its) => GM.renderInbox({ items: its.filter((x) => x.group === "need"), count: 3, needsOnly: true, updates: false }), ITEMS);
      await sleep(80);
      check(W("a push of what needs you alone keeps the updates the page has"), !!(await page.$(".bdupdsec")));
      await push(page);

      // ---- One red count ------------------------------------------------------------
      const head = await page.textContent(".bd-need > h2.section");
      const listed = +(/(\d+) 件/.exec(head) || [])[1];
      check(W("one red count, the Board's: as many as 要你處理 lists"), (await page.textContent("#board-n")) === String(listed) && listed === 4 &&
        (await page.$$(".tab .count.badge.red")).length === 1, { head, badge: await page.textContent("#board-n") });
      const left = intents.length;
      await tap('.tab[data-tab="agents"]');
      await sleep(100);
      check(W("the tab closed, it keeps that count until Emacs looks -- asked at once"), (await page.textContent("#board-n")) === "4" &&
        ops(intents.slice(left), "board-peek").length === 1, intents.slice(left).map((i) => i.op));
      await page.evaluate(() => GM.boardBadge({ count: 5, needs: 4, bare: 1 }));
      check(W("and shows Emacs's look, counted the same way, when it comes"), (await page.textContent("#board-n")) === "5");
      await tap('.tab[data-tab="board"]');
      await sleep(100);
      await push(page);

      // ---- An update of work: on its card ---------------------------------------------
      const folded = await page.$eval("#bd-w-c1a2b3", (w) => ({ folded: w.classList.contains("folded"), dot: (w.querySelector(".t-new") || {}).textContent || "" }));
      check(W("a report of work is on its card: a dot while new, the card folded"), folded.folded && /新回報/.test(folded.dot), folded);
      const n0 = intents.length;
      await tap("#bd-w-c1a2b3 .wh");
      await sleep(120);
      const asked = intents.slice(n0);
      check(W("the card opened: the report read, its words asked for"),
        asked.some((i) => i.op === "inbox-set" && i.id === "report:taxaudit:1" && i.read === true) &&
        asked.some((i) => i.op === "inbox-detail" && i.id === "report:taxaudit:1" && i.text === true), asked);
      await page.evaluate(() => GM.inboxDetailData({ for: "report:taxaudit:1", report: "## 對照表\n三區差異如下：**北區** 5%。", images: [], text: true }));
      await sleep(100);
      const upd = await page.$eval("#bd-w-c1a2b3 .bdupds", (b) => ({ text: b.textContent, html: b.innerHTML }));
      check(W("and shown whole on it, Markdown and all, with 到對話"), /三區差異如下/.test(upd.text) && /<strong>北區<\/strong>/.test(upd.html) &&
        /data-act="bd:talk"/.test(upd.html), upd.text);
      check(W("no longer new"), !(await page.$("#bd-w-c1a2b3 .t-new")));
      // A new one while the card is open: new on it, 已讀 marks it read.
      const more = ITEMS.concat([it("report:taxaudit:2", "report", "fyi", "Report", "green", "taxaudit", 1, { project: "~/src/billing/",
        title: "又更新了", preview: "又更新了一版", fromAsk: true })]);
      await push(page, more.map((x) => (x.id === "report:taxaudit:1" ? Object.assign({}, x, { read: true }) : x)));
      check(W("one come while the card is open says it is new, a button to read it"),
        !!(await page.$('#bd-w-c1a2b3 .bdupd.new[data-id="report:taxaudit:2"] [data-act="bd:read"]')));
      const n1 = intents.length;
      await tap('#bd-w-c1a2b3 [data-act="bd:read"][data-id="report:taxaudit:2"]');
      await sleep(80);
      check(W("已讀 marks it read"), intents.slice(n1).some((i) => i.op === "inbox-set" && i.id === "report:taxaudit:2" && i.read === true));
      await push(page);

      // ---- 最近回報: the updates of no work item ---------------------------------------
      const sec = await page.$eval(".bdupdsec", (s) => ({ head: s.querySelector(".bdsech").getAttribute("aria-expanded"), text: s.textContent,
        rows: s.querySelectorAll(".bdnrow").length }));
      check(W("最近回報 under 要你處理, folded, its count said"), sec.head === "false" && /最近回報/.test(sec.text) && /2 則/.test(sec.text) && sec.rows === 0, sec);
      await tap('.bdupdsec .bdsech[data-sec="upd"]');
      await sleep(100);
      const groups = await page.$$eval(".bdupdsec .bdng", (gs) => gs.map((g) => ({ a: g.getAttribute("data-agent"),
        rows: [...g.querySelectorAll(".bdnrow")].map((r) => r.getAttribute("data-id")), talk: !!g.querySelector('.bdngh [data-act="bd:talk"]') })));
      check(W("opened: by agent, the newest first, each with 到對話 →"),
        groups.map((g) => g.a).join() === "copybot,lead-ops-helm" && groups.every((g) => g.talk && g.rows.length === 1), groups);
      check(W("none of them on a card, none in 要你處理"), !(await page.$('.bdneeds:not(.bdupdg) [data-id="report:copybot:2"]')) &&
        !(await page.$('#bd-w-c1a2b3 [data-id="report:copybot:2"]')));
      const n2 = intents.length;
      await tap('.bdupdsec .bdnrow[data-id="report:copybot:2"] [data-act="ib:done"]');
      await sleep(100);
      check(W("已讀 there: read for good, gone"), intents.slice(n2).some((i) => i.op === "inbox-set" && i.id === "report:copybot:2" && i.done === true) &&
        !(await page.$('.bdupdsec [data-id="report:copybot:2"]')));
      check(W("its open kept for this device"), (await page.evaluate(() => localStorage.getItem("ghostherd.board.shut") || "")).includes("!upd"));
      await ctx.close();

      // ---- The Inbox's old links, and a push's ------------------------------------------
      const land = async (hash, items) => {
        intents = [];
        ({ ctx, page, errors } = await open(browser, where, hash, intents));
        await push(page, items);
        await sleep(200);
        return page;
      };
      page = await land("#inbox");
      check(W("#inbox, an old link: the Board"), (await page.getAttribute("#app", "data-view")) === "board" &&
        (await page.evaluate(() => location.hash)) === "#board");
      await ctx.close();
      page = await land("#inbox/need%3A11");
      const n11 = await page.evaluate(() => { const r = document.querySelector('.bdnrow.open[data-id="need:11"]'); const b = r && r.getBoundingClientRect();
        return !!r && b.top >= 0 && b.bottom <= innerHeight + 2; });
      check(W("#inbox/ID, an old link to an item: on the Board, that item opened, in view"), n11);
      await ctx.close();
      page = await land("#board/at/blocked%3Apushfix");
      check(W("a push's link to what waits on you: that item opened"), !!(await page.$('.bdnrow.open[data-id="blocked:pushfix"] [data-act="ib:answer"]')));
      await ctx.close();
      page = await land("#board/at/report%3Ataxaudit%3A1");
      const card = await page.evaluate(() => { const w = document.getElementById("bd-w-c1a2b3"); return { open: !!w && !w.classList.contains("folded") }; });
      check(W("a link to an update of work: its card opened, the update read"), card.open &&
        intents.some((i) => i.op === "inbox-set" && i.id === "report:taxaudit:1" && i.read === true), card);
      await ctx.close();
      page = await land("#board/at/report%3Acopybot%3A2");
      check(W("a link to one of no work item: 最近回報 opened, it open"), !!(await page.$('.bdupdsec .bdnrow.open[data-id="report:copybot:2"]')));
      await ctx.close();
      page = await land("#board/agent/lead-infra");
      check(W("a push's link to an agent: its group in 要你處理, opened, in view"),
        await page.evaluate(() => { const g = document.querySelector('.bd-need .bdng.open[data-agent="lead-infra"]'); const b = g && g.getBoundingClientRect();
          return !!g && b.top < innerHeight && b.bottom > 0; }));
      await ctx.close();
      page = await land("#board/at/guard%3Agone1");
      check(W("a link to what is handled already: said so, on the Board"),
        (await page.getAttribute("#app", "data-view")) === "board" && /已處理，或已不在 Board 上/.test(await page.textContent("#status")));
      // The links wait for what they need: before the items come, nothing yet.
      await ctx.close();
      intents = [];
      ({ ctx, page, errors } = await open(browser, where, "#board/at/need%3A11", intents));
      await sleep(100);
      check(W("before the items come, a link waits, says nothing"), !/已處理/.test(await page.textContent("#status")));
      await push(page);
      await sleep(150);
      check(W("and opens its item once they have"), !!(await page.$('.bdnrow.open[data-id="need:11"]')));
      if (where === "phone") {
        await boardUnfold(page);
        const wide = await page.evaluate(() => ({ doc: document.documentElement.scrollWidth, w: innerWidth }));
        check(W("nothing sideways, everything open"), wide.doc <= wide.w, wide);
      }
      check(W("no page errors"), !errors.filter((e) => !/access control checks/.test(e)).length, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
