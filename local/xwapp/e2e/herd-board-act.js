// The Board's ways out and in: an agent's name opens its project's room at
// that agent -- its column on the desk, the chat's box on the phone -- and
// 回 Board brings the Board back as it was; a name that left the herd says
// so; an ask's id still goes to Threads.  A project narrows the Board; 交給
// lead opens the lead's room with the message written, never sent; a card
// waiting on Jing is answered from itself; the tab's red count, from Emacs
// while the tab is closed and from the cards while it is open.  Fixture data
// (board-fixture.js), Emacs played by the test.  BOARD_SHOTS=DIR saves
// room-desk.png, room-phone.png, board-filter-phone.png.
const path = require("path");
const { webkit } = require("playwright");
const { phonePage, sleep, HERD, agent } = require("./herdlib");
const { FULL, AGENTS, P } = require("./board-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const SHOTS = process.env.BOARD_SHOTS || "";
const ops = (intents, op) => intents.filter((i) => i.op === op);
// The herd as the Agents tab gets it: the Board's agents, each in its room.
const herdOf = (agents) => Object.assign({}, HERD, { work: [], asks: [],
  agents: agents.map((a) => agent(a.name, { project: a.project, role: a.role, state: a.role ? "idle" : "working", kind: "claude",
                                            reason: a.role ? "" : "at work" })) });

async function boardOpen(page, intents, patch) {
  await page.click('.tab[data-tab="board"]');
  await sleep(80);
  await page.evaluate((p) => GM.boardPatch(p), patch || FULL);
  await sleep(150);
}

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- The desk ------------------------------------------------------
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: DESK });
      await sleep(100);
      check("desk: the page asks for the Board's badge as it starts", ops(intents, "board-peek").length === 1);
      await page.evaluate(() => GM.boardBadge({ jing: 2, orphan: 1 }));
      const badge = await page.$eval("#board-n", (b) => ({ t: b.textContent, red: b.classList.contains("red") && b.classList.contains("badge") }));
      check("desk: with the tab closed, the tab shows Emacs's count in red", badge.t === "3" && badge.red, badge);
      await page.evaluate(() => GM.boardBadge({ jing: 0, orphan: 0 }));
      check("desk: none, no badge", (await page.textContent("#board-n")) === "");

      await boardOpen(page, intents);
      check("desk: with the tab open, the count is its own cards': 沒人接 and 等 Jing by a lead's line",
        (await page.textContent("#board-n")) === "2");

      // Names are ways to rooms; ids to Threads.
      const row = await page.$eval(".bdwaits .wait .wrow", (r) => ({
        names: [...r.querySelectorAll('[data-act="bd:agent"]')].map((b) => b.textContent),
        ref: (r.querySelector('[data-act="bd:thread"]') || {}).textContent }));
      check("desk: a wait row is its two names and its ask's id, apart", row.names.join() === "lead-shop,lead-billing" && row.ref === "1a1a1a1a", row);
      const named = await page.$$eval("#bd-w-c1a2b3 [data-act=\"bd:agent\"]", (bs) => bs.map((b) => b.textContent));
      check("desk: a card's people and what it waits on are names to rooms",
        ["lead-shop", "lead-billing", "taxaudit", "cartfix"].every((n) => named.includes(n)), named);

      // A name that left the herd.
      const n0 = ops(intents, "room-watch").length;
      await page.click('#bd-w-c6d7e8 .ag [data-name="ratefix"]');
      await sleep(80);
      check("desk: a name no longer in the herd says so, and opens nothing",
        /ratefix 已不在 herd 裡/.test(await page.textContent("#status")) && ops(intents, "room-watch").length === n0 &&
        (await page.getAttribute("#app", "data-view")) === "board");

      // To the room, at the agent; back where the Board was.
      // Where the page is as the name is tapped: the name in view.
      await page.$eval('#bd-w-c1a2b3 .blk [data-name="taxaudit"]', (b) => b.scrollIntoView({ block: "center" }));
      await sleep(60);
      const at = await page.evaluate(() => document.querySelector("#app").scrollTop);
      check("desk: (the Board is scrolled down to the card)", at > 100, at);
      await page.click('#bd-w-c1a2b3 .blk [data-name="taxaudit"]');
      await sleep(120);
      const rw = ops(intents, "room-watch").slice(-1)[0] || {};
      check("desk: a name opens its project's room", rw.project === P("billing") && (await page.getAttribute("#app", "data-view")) === "agents", rw);
      check("desk: the room says the way back", (await page.textContent(".room .rhead")).includes("← 回 Board"));
      await page.evaluate((h) => GM.renderHerd(h), herdOf(AGENTS));
      await sleep(250);
      const col = await page.evaluate(() => {
        const c = document.querySelector('.rcol[data-name="taxaudit"]');
        const box = document.querySelector(".rcols").getBoundingClientRect(), r = c.getBoundingClientRect();
        return { landed: c.hasAttribute("data-landed"), inView: r.left >= box.left - 1 && r.right <= box.right + 1 };
      });
      check("desk: and lands on that agent's column, marked a moment", col.landed && col.inView, col);
      if (SHOTS) await page.screenshot({ path: path.join(SHOTS, "room-desk.png") });
      await page.click('[data-act="bd:back"]');
      await sleep(200);
      const back = await page.evaluate(() => ({ view: document.querySelector("#app").getAttribute("data-view"),
                                                at: document.querySelector("#app").scrollTop }));
      check("desk: 回 Board brings the Board back where it was", back.view === "board" && Math.abs(back.at - at) <= 2, { back, at });
      check("desk: and watches it again", ops(intents, "board-watch").length === 2 && ops(intents, "room-close").length >= 1);

      // An ask's id still goes to Threads.
      await page.evaluate((p) => GM.boardPatch(Object.assign({}, p, { v: 2 })), FULL);
      await sleep(120);
      await page.click('.bdwaits .wait .wrow [data-act="bd:thread"]');
      await sleep(120);
      check("desk: the id goes to Threads", (await page.getAttribute("#app", "data-view")) === "threads" &&
        ops(intents, "thread-open").slice(-1)[0].ask === "1a1a1a1a");

      // A project narrows the Board.
      await boardOpen(page, intents, Object.assign({}, FULL, { v: 3 }));
      await page.click('#bd-w-c1a2b3 .meta [data-act="bd:pfilter"][data-p="shop"]');
      await sleep(120);
      const narrowed = await page.evaluate(() => ({
        chip: (document.querySelector(".bdchips .bdonly") || {}).textContent,
        cards: [...document.querySelectorAll(".bdarea .wi")].map((w) => w.id.slice(5)),
        folds: [...document.querySelectorAll(".bdfoldh")].map((f) => f.getAttribute("data-p")),
        waits: [...document.querySelectorAll(".bdwaits .wait .wrow")].map((r) => r.textContent.slice(0, 22)),
        repos: [...document.querySelectorAll(".prt tr.rg th")].map((t) => t.textContent) }));
      check("desk: a project tag narrows the Board to it, a chip says so",
        /只看 shop/.test(narrowed.chip) && narrowed.cards.join() === "c1a2b3,f0a1b2,9a8b7c,0f1e2d", narrowed);
      check("desk: its folds, its waits, its PRs alone",
        narrowed.folds.join() === "shop" && narrowed.waits.length === 1 && /lead-shop/.test(narrowed.waits[0]) &&
        narrowed.repos.join("|") === "acme/shop 2|acme/billing 1", narrowed);
      await page.click(".bdchips .bdonly");
      await sleep(100);
      check("desk: its ✕ clears it", !(await page.$(".bdonly")) && (await page.$$(".bdfoldh")).length === 4);
      await page.click('.bdg-svg .nd[data-p="infra"]');
      await sleep(100);
      check("desk: a project in the map narrows it too, marked there",
        /只看 infra/.test(await page.textContent(".bdchips")) && (await page.$('.bdg-svg .nd.sel[data-p="infra"]')) !== null);
      await page.click('.bdg-svg .nd[data-p="infra"]');
      await sleep(100);
      check("desk: tapped again, clears", !(await page.$(".bdonly")));

      // A card waiting on Jing answered from itself.
      await page.click('#bd-w-b1c2d3 [data-act="bd:reply-open"]');
      await sleep(60);
      await page.fill("#bd-w-b1c2d3 .bdreply textarea", "今晚跑，我來");
      await page.click('#bd-w-b1c2d3 [data-act="bd:reply-send"]');
      await sleep(80);
      const said = ops(intents, "lead-say").slice(-1)[0] || {};
      check("desk: a card waiting on Jing answers through the Inbox's way, its lead's project",
        said.project === P("infra") && said.text === "今晚跑，我來" && !(await page.$("#bd-w-b1c2d3 .bdreply")), said);

      // 交給 lead: the lead's room, the message written, not sent.
      const sends = ops(intents, "agent-prompt").length + ops(intents, "lead-say").length;
      await page.click('#bd-w-c6d7e8 [data-act="bd:hand"]');
      await sleep(120);
      check("desk: 交給 lead opens that project's lead's room", (ops(intents, "room-watch").slice(-1)[0] || {}).project === P("infra"));
      await page.evaluate((h) => GM.renderHerd(h), herdOf(AGENTS));
      await sleep(250);
      const box = await page.$eval('.rcol[data-name="lead-infra"] textarea.agent-text', (t) => t.value);
      check("desk: its box holds the message, what is left of it said",
        box.startsWith("這件沒人接：整理 nginx 的 rate limit 設定（工作項目 c6d7e8）") && box.includes("三個方案選一個"), box);
      check("desk: and nothing is sent", ops(intents, "agent-prompt").length + ops(intents, "lead-say").length === sends);
      await page.click('[data-act="bd:back"]');
      await sleep(150);
      // A project with no lead: said, none started.
      const noLead = Object.assign({}, FULL, { v: 9, meta: Object.assign({}, FULL.meta, { agents: AGENTS.filter((a) => a.name !== "lead-infra") }) });
      await page.evaluate((p) => GM.boardPatch(p), noLead);
      await sleep(120);
      const rws = ops(intents, "room-watch").length;
      await page.click('#bd-w-c6d7e8 [data-act="bd:hand"]');
      await sleep(80);
      check("desk: a project with no lead says so, starts none",
        /infra 現在沒有 lead/.test(await page.textContent("#status")) && ops(intents, "room-watch").length === rws &&
        !ops(intents, "agent-new").length);
      check("desk: no page errors", !errors.length, errors);
      await ctx.close();
    }

    // ---- The phone -------------------------------------------------------
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, hash: "#board" });
      await sleep(100);
      await page.evaluate((p) => GM.boardPatch(p), FULL);
      await sleep(150);
      await page.$eval('#bd-w-d4e5f6 .blk [data-name="pushfix"]', (b) => b.scrollIntoView({ block: "center" }));
      await sleep(60);
      const at = await page.evaluate(() => document.querySelector("#app").scrollTop);
      check("phone: (the Board is scrolled down to the card)", at > 100, at);
      await page.click('#bd-w-d4e5f6 .blk [data-name="pushfix"]');
      await sleep(120);
      await page.evaluate((h) => GM.renderHerd(h), herdOf(AGENTS));
      await sleep(300);
      const land = await page.evaluate(() => ({ chat: !!document.querySelector(".room .chat"),
        to: (document.querySelector(".chat .cfoot") || {}).getAttribute && document.querySelector(".chat .cfoot").getAttribute("data-name"),
        back: !!document.querySelector(".room .rback") }));
      check("phone: a name opens the room as its chat, the box written to that agent, the way back shown",
        land.chat && land.to === "pushfix" && land.back, land);
      if (SHOTS) await page.screenshot({ path: path.join(SHOTS, "room-phone.png") });
      await page.click(".room .rback");
      await sleep(200);
      const back = await page.evaluate(() => ({ view: document.querySelector("#app").getAttribute("data-view"),
                                                at: document.querySelector("#app").scrollTop }));
      check("phone: 回 Board, where it was", back.view === "board" && Math.abs(back.at - at) <= 2, { back, at });
      await page.evaluate((p) => GM.boardPatch(Object.assign({}, p, { v: 2 })), FULL);
      await sleep(120);
      await page.evaluate(() => { document.querySelector("#app").scrollTop = 0; });
      await page.click('#bd-w-c1a2b3 .meta [data-p="billing"]');
      await sleep(120);
      const wide = await page.evaluate(() => ({ doc: document.documentElement.scrollWidth, w: window.innerWidth,
                                                bar: (document.querySelector(".bdfilter") || {}).textContent }));
      check("phone: narrowed to a project, said on top, nothing sideways", /只看 billing/.test(wide.bar) && wide.doc <= wide.w, wide);
      const sub = await page.$eval(".bdtile .sub", (e) => e.textContent);
      check("phone: the items tile counts its own asks, not every project's", /billing 的工作項目共 \d+ 則 ask/.test(sub), sub);
      const t = await page.$eval('#bd-w-c6d7e8 [data-act="bd:hand"]', (b) => b.getBoundingClientRect().height).catch(() => null);
      check("phone: 交給 lead is a finger's target", t === null || t >= 30, t);
      if (SHOTS) {
        await page.evaluate(() => { document.querySelector("#app").scrollTop = 0; });
        await sleep(80);
        await page.screenshot({ path: path.join(SHOTS, "board-filter-phone.png") });
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
