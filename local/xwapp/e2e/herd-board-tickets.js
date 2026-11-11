// The Board's tickets: one row per ticket the shown items carry -- what
// ClickUp says of it ("?" when it could not be read), its items by status,
// their PRs, its projects, when last named -- opened, its items' cards; a
// ticket narrows the Board, with a project too; a row for the items with
// none; hints where ClickUp and the work disagree, by the status's type.
// Fixture data (board-fixture.js), no ClickUp asked.  BOARD_SHOTS=DIR saves
// tickets-desk.png and tickets-phone.png.
const path = require("path");
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const { FULL } = require("./board-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const SHOTS = process.env.BOARD_SHOTS || "";
const ops = (intents, op) => intents.filter((i) => i.op === op);
const rowsOf = (page) => page.$$eval(".bdtks .bdtk", (rs) => rs.map((r) => ({
  id: r.getAttribute("data-t"), st: r.querySelector(".bdtkst").textContent, name: r.querySelector(".bdtkname").textContent,
  n: r.querySelector(".bdtkn").textContent, prs: [...r.querySelectorAll(".bdpr .prn")].map((p) => p.textContent),
  projs: [...r.querySelectorAll(".bdtkp .pj")].map((p) => p.textContent), mis: (r.querySelector(".bdtkmis") || {}).textContent || "",
  href: r.querySelector("a.bdtkid").getAttribute("href") })));

async function shoot(page, width, file) {
  const h = await page.evaluate(() => document.querySelector(".top").offsetHeight + document.querySelector("#app").scrollHeight + 20);
  await page.setViewportSize({ width, height: Math.min(h, 16000) });
  await sleep(200);
  await page.screenshot({ path: file });
}

(async () => {
  const browser = await webkit.launch();
  try {
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: DESK });
      await page.click('.tab[data-tab="board"]');
      await page.evaluate((p) => GM.boardPatch(p), FULL);
      await sleep(150);
      // Cards start folded: the two whose parts are tapped here opened, as a tap does.
      for (const id of ["c1a2b3", "d4e5f6"]) { await page.click("#bd-w-" + id + " .wh"); await sleep(40); }
      const rows = await rowsOf(page);
      check("desk: one row per ticket the shown items carry, the latest named first",
        rows.map((r) => r.id).join() === "86b2k1,86c001,86b2k9,86c002,86c003", rows.map((r) => r.id));
      const r1 = rows[0];
      check("desk: a row says what ClickUp says: its title and status", r1.name === "結帳頁改用新稅率表" && /in progress/.test(r1.st), r1);
      check("desk: its items by status", r1.n === "2 件：進行中 1、安靜 1", r1.n);
      check("desk: their PRs, its projects", r1.prs.join() === "shop #412,billing #88,billing #90" && r1.projs.join() === "shop,billing", r1);
      check("desk: its id opens the ticket as the work chips do", r1.href === "https://app.clickup.com/t/86b2k1", r1.href);
      check("desk: a ticket ClickUp could not tell of says ?", rows[4].st === "?" && rows[4].name === "", rows[4]);
      check("desk: when last named", (await page.textContent('.bdtk[data-t="86b2k1"] .bdtkat')).startsWith("最後提到 "));

      // Hints, by the status's type.
      check("desk: done in ClickUp with an item still at work: a hint", /ClickUp 已完成，但還有工作項目進行中 1/.test(rows[1].mis), rows[1].mis);
      check("desk: open in ClickUp, its PRs merged, nothing at work: a hint", /ClickUp 還開著，但 PR 都已 merge／部署/.test(rows[3].mis), rows[3].mis);
      check("desk: none where they agree, or ClickUp could not be read", !rows[0].mis && !rows[2].mis && !rows[4].mis);
      check("desk: the section counts them", /5 個單號 · 對不上 2/.test(await page.textContent(".bd-tn")));
      const noneRow = await page.textContent(".bdtknone");
      check("desk: a row for the items with no ticket, and their projects",
        /沒有單號/.test(noneRow) && /11 件/.test(noneRow) && /shop/.test(noneRow) && /infra/.test(noneRow), noneRow);

      // A row opened: its items' cards.
      await page.click('.bdtk[data-t="86c002"] .bdtkh .bdtkname');
      await sleep(80);
      const opened = await page.$$eval('.bdtk[data-t="86c002"] .cards .wi', (ws) => ws.map((w) => w.id.slice(5)));
      check("desk: a row opens to its items' cards", opened.join() === "a3b4c5,0f1e2d", opened);
      await page.click('.bdtk[data-t="86c002"] .bdtkh .bdtkname');
      await sleep(80);
      check("desk: and closes", !(await page.$('.bdtk[data-t="86c002"] .cards')));

      // A ticket narrows the Board.
      await page.click('.bdtk[data-t="86b2k1"] [data-act="bd:tfilter"]');
      await sleep(100);
      const narrowed = await page.evaluate(() => ({
        chip: [...document.querySelectorAll(".bdchips .bdonly")].map((c) => c.textContent),
        cards: [...document.querySelectorAll(".bdarea .wi")].map((w) => w.id.slice(5)),
        folds: [...document.querySelectorAll(".bdfoldh")].map((f) => f.getAttribute("data-p")),
        waits: document.querySelectorAll(".bdwaits .wait").length,
        tickets: [...document.querySelectorAll(".bdtks .bdtk")].map((r) => r.getAttribute("data-t")) }));
      check("desk: a ticket narrows the Board to its items, a chip says so",
        narrowed.chip.join() === "只看 CU-86b2k1 ✕" && narrowed.cards.join() === "c1a2b3" && narrowed.folds.join() === "billing", narrowed);
      check("desk: its waits, and the tickets its items carry", narrowed.waits === 1 && narrowed.tickets.join() === "86b2k1,86c002", narrowed);
      // With a project too: both hold.
      await page.click('#bd-w-c1a2b3 .meta [data-p="shop"]');
      await sleep(100);
      const both = await page.evaluate(() => ({
        chips: [...document.querySelectorAll(".bdchips .bdonly")].map((c) => c.textContent),
        cards: [...document.querySelectorAll(".bdarea .wi")].map((w) => w.id.slice(5)),
        folds: [...document.querySelectorAll(".bdfoldh")].length, bar: document.querySelector(".bdfilter").textContent }));
      check("desk: a project with it: both hold, two chips",
        both.chips.join() === "只看 shop ✕,只看 CU-86b2k1 ✕" && both.cards.join() === "c1a2b3" && !both.folds && /只看 shop 裡 CU-86b2k1/.test(both.bar), both);
      await page.click('.bdchips .bdonlyt');
      await sleep(80);
      check("desk: the ticket's ✕ leaves the project", !(await page.$(".bdonlyt")) && /只看 shop/.test(await page.textContent(".bdchips")));
      await page.click(".bdchips .bdonly");
      await sleep(80);
      // A ticket label on a card narrows too.
      await page.click('#bd-w-d4e5f6 .meta [data-act="bd:tfilter"]');
      await sleep(80);
      check("desk: a ticket on a card narrows too", /只看 CU-86c001/.test(await page.textContent(".bdchips")) &&
        (await page.$$eval(".bdarea .wi", (ws) => ws.map((w) => w.id.slice(5)))).join() === "d4e5f6");
      await page.click('#bd-w-d4e5f6 .meta [data-act="bd:tfilter"]');
      await sleep(80);
      // The items with none.
      await page.click(".bdtknone");
      await sleep(100);
      const bare = await page.evaluate(() => ({
        chip: (document.querySelector(".bdchips .bdonlyt") || {}).textContent,
        top: [...document.querySelectorAll(".bdarea .wi")].map((w) => w.id.slice(5)),
        tickets: document.querySelectorAll(".bdtks .bdtk").length }));
      check("desk: the no-ticket row narrows to the items with none",
        /只看 沒有單號的/.test(bare.chip) && bare.top.join() === "e7f8a9,b1c2d3,9a8b7c" && bare.tickets === 0, bare);
      await page.click(".bdtknone");
      await sleep(80);
      check("desk: tapped again, cleared", !(await page.$(".bdonlyt")));
      if (SHOTS) {
        await page.click('.bdtk[data-t="86c001"] .bdtkh .bdtkname');
        await sleep(80);
        await shoot(page, 1360, path.join(SHOTS, "tickets-desk.png"));
      }
      check("desk: nothing asks ClickUp from the page", !intents.some((i) => /clickup|task/.test(i.op)));
      // From a sidecar before cards named their tickets: their CU- labels.
      await page.evaluate((p) => GM.boardPatch(Object.assign({}, p, { v: 30,
        cards: p.cards.map((c) => { const o = Object.assign({}, c); delete o.tickets; return o; }) })), FULL);
      await sleep(120);
      check("desk: a sidecar that names no tickets: the cards' CU- labels",
        (await page.$$eval(".bdtks .bdtk", (rs) => rs.map((r) => r.getAttribute("data-t")))).length === 5);
      check("desk: no page errors", !errors.length, errors);
      await ctx.close();
    }
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, hash: "#board" });
      await page.evaluate((p) => GM.boardPatch(p), FULL);
      await sleep(150);
      await page.$eval("#bd-tickets", (h) => h.scrollIntoView());
      await page.click('.bdtk[data-t="86c002"] .bdtkh .bdtkname');
      await sleep(100);
      const wide = await page.evaluate(() => ({ doc: document.documentElement.scrollWidth, w: window.innerWidth }));
      check("phone: tickets and an opened row, nothing sideways at 390 px", wide.doc <= wide.w, wide);
      const over = await page.$$eval(".bd-tickets *", (els) => els.filter((e) => e.getBoundingClientRect().right > window.innerWidth + 1)
        .map((e) => e.className || e.tagName).slice(0, 5));
      check("phone: no part of a row past the screen's edge", !over.length, over);
      const h = await page.$eval('.bdtk[data-t="86b2k1"] .bdtkh', (e) => e.getBoundingClientRect().height);
      const nh = await page.$eval(".bdtknone", (e) => e.getBoundingClientRect().height);
      check("phone: a row's head and the no-ticket row are a finger's target", h >= 40 && nh >= 44, { h, nh });
      // The no-ticket row's 只看: one line, as wide as its words, a button
      // as the rows' 只看這張 are.
      const only = await page.evaluate(() => {
        const b = document.querySelector(".bdtknone .bdtkonly"), t = document.querySelector('.bdtk[data-t="86b2k1"] [data-act="bd:tfilter"]');
        const r = b.getBoundingClientRect(), tr = t.getBoundingClientRect();
        const range = document.createRange();
        range.selectNodeContents(b);
        const tops = [...new Set([...range.getClientRects()].map((x) => Math.round(x.top)))];
        return { w: r.width, h: r.height, lines: tops.length, tw: tr.width, th: tr.height, text: b.textContent,
                 fit: b.scrollWidth <= b.clientWidth + 1 };
      });
      check("phone: the no-ticket row's 只看 is on one line, not a character a line", only.text === "只看" && only.lines === 1 && only.fit, only);
      check("phone: and a button as 只看這張 is: as tall, at least as wide as its words",
        Math.abs(only.h - only.th) <= 1 && only.h >= 30 && only.w >= 44, only);
      // The ▸ stays on its row's title line: no band under a long title.
      const heads = await page.$$eval(".bdtks .bdtk", (rs) => rs.map((r) => {
        const id = r.querySelector(".bdtkid").getBoundingClientRect(), chev = r.querySelector(".bdtkh .chev").getBoundingClientRect();
        const name = r.querySelector(".bdtkname"), nr = name.getBoundingClientRect(), m = r.querySelector(".bdtkm").getBoundingClientRect();
        const last = Math.max(id.bottom, name.textContent ? nr.bottom : 0, r.querySelector(".bdtkst").getBoundingClientRect().bottom);
        return { t: r.getAttribute("data-t"), onLine: chev.top < id.bottom && chev.bottom > id.top, band: Math.round(m.top - last) };
      }));
      check("phone: each row's ▸ on its title line", heads.every((x) => x.onLine), heads);
      check("phone: no empty band between a row's title and its counts", heads.every((x) => x.band <= 8), heads);
      if (SHOTS) {
        await page.evaluate(() => { document.querySelector("#app").scrollTop = 0; });
        await shoot(page, 390, path.join(SHOTS, "tickets-phone.png"));
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
