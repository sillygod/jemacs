// `Load earlier' in a room, incremental: the poll shows the recent tail,
// and `load earlier' fetches one older page before what is held (room-earlier
// / chat-earlier, with how many are held as `before'), prepended and kept.
// The reading position holds across a prepend; the start of the conversation
// is a clear end; and what is held is bounded (MAX_HELD), so loading far back
// slides the window rather than growing without limit.  Phone and desk.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const RD = "claude-deploy-platform";
const PATH = "/Users/x/.claude/projects/-p/abc.jsonl";
const herd = () => Object.assign({}, HERD, { agents: HERD.agents.map((a) => Object.assign({}, a, { meter: false, subs: [] })) });
// Entries with unique timestamps, so their signatures differ.
const turns = (prefix, n, start) => Array.from({ length: n }, (_, i) =>
  ({ role: "assistant", text: prefix + " " + (start + i), ts: "t" + String(start + i).padStart(6, "0") }));
const chatTurns = (prefix, n, start) => Array.from({ length: n }, (_, i) =>
  ({ who: RD, kind: "turn", text: prefix + " " + (start + i), ts: "t" + String(start + i).padStart(6, "0") }));

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const lastOf = (op) => intents.filter((i) => i.op === op).pop();
      const col = '.rcol[data-name="' + RD + '"] .rbody';
      const ents = (sel) => page.$$eval(sel + " .ent", (es) => es.length);
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(120);

      // ---- A column: the recent tail, then pages back ----
      await page.evaluate((p) => GM.setConversation(p), { name: RD, path: PATH, cli: "claude", earlier: true, entries: turns("turn", 3, 100) });
      await sleep(60);
      check(W("the recent tail with more shows a Load earlier button"),
        (await ents(col)) === 3 && !!(await page.$(col + ' [data-act="a:load-earlier"]')));
      await tap(col + ' [data-act="a:load-earlier"]');
      await sleep(60);
      const re = lastOf("room-earlier");
      check(W("clicking asks Emacs for the page before what is held"),
        re && re.name === RD && re.before === 3, re);
      // The older page arrives: prepended, more still to come.
      await page.evaluate((p) => GM.earlierConversation(p), { name: RD, before: 3, earlier: true, entries: turns("older", 3, 50) });
      await sleep(60);
      check(W("the older page is prepended, kept with the recent tail"),
        (await ents(col)) === 6 && /older 50/.test(await page.$eval(col, (b) => b.textContent)) &&
        !!(await page.$(col + ' [data-act="a:load-earlier"]')), await ents(col));

      // ---- Reading position holds across a prepend ----
      // A tall column (a fresh recent tail, resetting the held list), scrolled
      // to the middle: a prepended page should move the scroll down by its
      // height so the message you were on stays put.
      await page.evaluate((p) => GM.setConversation(p), { name: RD, path: PATH, cli: "claude", earlier: true, entries: turns("tall", 40, 300) });
      await sleep(60);
      await page.$eval(col, (b) => { b.scrollTop = Math.round((b.scrollHeight - b.clientHeight) / 2); });
      const before = await page.$eval(col, (b) => ({ top: b.scrollTop, h: b.scrollHeight }));
      await page.$eval(col + ' [data-act="a:load-earlier"]', (el) => el.click()); // no auto-scroll
      await sleep(40);
      await page.evaluate((p) => GM.earlierConversation(p), { name: RD, before: 40, earlier: true, entries: turns("deep", 20, 260) });
      await sleep(80);
      const after = await page.$eval(col, (b) => ({ top: b.scrollTop, h: b.scrollHeight }));
      check(W("a prepend holds the reading position (scroll follows the added height)"),
        after.h > before.h + 20 && before.top > 0 && Math.abs((after.top - before.top) - (after.h - before.h)) <= 3,
        { before, after });

      // ---- The start of the conversation is a clear end ----
      await page.evaluate((p) => GM.setConversation(p), { name: RD, path: PATH, cli: "claude", earlier: true, entries: turns("recent", 3, 500) });
      await sleep(40);
      await page.$eval(col + ' [data-act="a:load-earlier"]', (el) => el.click());
      await sleep(40);
      await page.evaluate((p) => GM.earlierConversation(p), { name: RD, before: 3, earlier: false, entries: turns("first", 2, 400) });
      await sleep(60);
      check(W("at the start of the conversation: a clear end, no button"),
        !(await page.$(col + ' [data-act="a:load-earlier"]')) &&
        /Start of the conversation/.test(await page.$eval(col, (b) => b.textContent)));

      // ---- What is held is bounded (MAX_HELD): a huge page slides the window ----
      await page.evaluate((p) => GM.setConversation(p), { name: RD, path: PATH, cli: "claude", earlier: true, entries: turns("fresh", 5, 9000) });
      await sleep(40);
      await page.$eval(col + ' [data-act="a:load-earlier"]', (el) => el.click()).catch(() => {});
      await sleep(40);
      await page.evaluate((p) => GM.earlierConversation(p), { name: RD, before: 5, earlier: true, entries: turns("bulk", 1200, 1000) });
      await sleep(120);
      const held = await ents(col);
      check(W("what is held is bounded, not grown without limit"), held > 0 && held <= 800, held);

      // ---- The chat pages back the same way ----
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(120);
      const cb = ".chat .cbody";
      await page.evaluate((p) => GM.setChat(p), { project: DP, earlier: true, missing: [], absent: [], entries: chatTurns("c", 3, 100) });
      await sleep(60);
      check(W("the chat's recent tail with more shows a Load earlier button"),
        !!(await page.$(cb + ' [data-act="a:chat-earlier"]')));
      await tap(cb + ' [data-act="a:chat-earlier"]');
      await sleep(60);
      const ce = lastOf("chat-earlier");
      check(W("clicking asks Emacs for the chat page before what is held"), ce && ce.before === 3, ce);
      await page.evaluate((p) => GM.earlierChat(p), { before: 3, earlier: false, entries: chatTurns("co", 3, 50) });
      await sleep(60);
      check(W("the chat's older page is prepended, and it reaches its start"),
        (await ents(cb)) === 6 && !(await page.$(cb + ' [data-act="a:chat-earlier"]')) &&
        /Start of the conversation/.test(await page.$eval(cb, (b) => b.textContent)), await ents(cb));

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
