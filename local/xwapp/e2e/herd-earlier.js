// `Load earlier' in a room: a column (and the chat) show a button when
// there is more, which asks Emacs to read a bigger limit; once at the most
// the sidecar returns, a note instead.  On a phone and at a desk.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const RD = "claude-deploy-platform";
const PATH = "/Users/x/.claude/projects/-p/abc.jsonl";
const herd = () => Object.assign({}, HERD, { agents: HERD.agents.map((a) => Object.assign({}, a, { meter: false, subs: [] })) });
const conv = (limit, earlier, n) => ({ name: RD, path: PATH, cli: "claude", earlier, limit,
  entries: Array.from({ length: n }, (_, i) => ({ role: "assistant", text: "turn " + i, ts: "t" })) });
// A `load earlier' result: EXTRA turns prepended before the N already shown.
const convEarlier = (limit, earlier, n, extra) => ({ name: RD, path: PATH, cli: "claude", earlier, limit,
  entries: Array.from({ length: n + extra }, (_, i) => ({ role: "assistant",
    text: i < extra ? "earlier " + i : "turn " + (i - extra), ts: "t" })) });

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const lastWatch = () => intents.filter((i) => i.op === "room-watch").pop();
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(120);

      // ---- A column ----
      await page.evaluate((p) => GM.setConversation(p), conv(60, true, 3));
      await sleep(60);
      const col = '.rcol[data-name="' + RD + '"] .rbody';
      check(W("a column with more shows a Load earlier button"),
        !!(await page.$(col + ' .loadmore [data-act="a:load-earlier"]')));
      await tap(col + ' [data-act="a:load-earlier"]');
      await sleep(60);
      const w = lastWatch();
      check(W("clicking asks Emacs for a bigger limit for that column"),
        w && (w.limits || []).some((l) => l.name === RD && l.limit === 120), w);
      await page.evaluate((p) => GM.setConversation(p), conv(120, true, 6));
      await sleep(60);
      const grown = await page.$$eval(col + " .ent", (es) => es.length);
      check(W("the answer shows more turns, the button still there"),
        grown === 6 && !!(await page.$(col + ' [data-act="a:load-earlier"]')), grown);
      // At the most the sidecar returns: a note, no button.
      await page.evaluate((p) => GM.setConversation(p), conv(200, true, 8));
      await sleep(60);
      check(W("at the cap, a note instead of the button"),
        !(await page.$(col + ' [data-act="a:load-earlier"]')) && /most recent 200/.test(await page.$eval(col, (b) => b.textContent)));

      // ---- Load earlier holds the reading position, not a jump ----
      // A tall column, scrolled to the middle: loading earlier prepends turns
      // above, and the view should move down by their height so the message
      // you were reading stays put, instead of keeping scrollTop (a jump).
      await page.evaluate((p) => GM.setConversation(p), conv(60, true, 30));
      await sleep(60);
      await page.$eval(col, (b) => { b.scrollTop = Math.round((b.scrollHeight - b.clientHeight) / 2); });
      const before = await page.$eval(col, (b) => ({ top: b.scrollTop, h: b.scrollHeight }));
      // Click without Playwright scrolling the (off-screen) button into view,
      // which would reset the scroll we are testing.
      await page.$eval(col + ' [data-act="a:load-earlier"]', (el) => el.click());
      await sleep(40);
      await page.evaluate((p) => GM.setConversation(p), convEarlier(120, true, 30, 12));
      await sleep(80);
      const after = await page.$eval(col, (b) => ({ top: b.scrollTop, h: b.scrollHeight }));
      check(W("loading earlier holds the reading position (scroll follows the prepended height)"),
        after.h > before.h + 20 && before.top > 0 && Math.abs((after.top - before.top) - (after.h - before.h)) <= 3,
        { before, after });

      // ---- The chat ----
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(120);
      await page.evaluate((p) => GM.setChat(p), { project: DP, earlier: true, missing: [],
        entries: [{ who: RD, kind: "say", text: "hello", ts: "t" }] });
      await sleep(60);
      check(W("the chat with more shows a Load earlier button"),
        !!(await page.$('.chat .cbody [data-act="a:chat-earlier"]')));
      await tap('.chat .cbody [data-act="a:chat-earlier"]');
      await sleep(60);
      const wc = lastWatch();
      check(W("clicking asks Emacs for a bigger chat limit"), wc && wc.chatLimit === 400, wc);

      const wide = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
      check(W("no sideways scroll"), wide <= 0, wide);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const bad = results.filter((r) => !r).length;
  console.log(bad ? bad + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(bad ? 1 : 0);
})();
