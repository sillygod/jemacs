// Clicking a room sub-agent card opens that sub's own transcript over the
// room -- its conversation and tool calls, read like a column's -- with its
// who and title in the header; Close (the overlay) returns.  On a phone and
// at a desk.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const RD = "claude-deploy-platform";
const QA = "qa-deploy-platform"; // kind agy
const PATH = "/Users/x/.claude/projects/-p/abc.jsonl";

const SUBS = [{ id: "a3db8f1c2e4a6b7c9", who: "general-purpose", title: "Build R4 sync debug project",
                started: new Date(Date.now() - 90e3).toISOString(), tools: 12, tokens: 52000, doing: "" }];
const AGY_SUB = [{ id: "b7c9d2e1f0a3b4c5d", who: "Explore", title: "Map the auth flow",
                   started: new Date(Date.now() - 30e3).toISOString(), tools: 3, tokens: 9000, doing: "" }];
const herd = () => Object.assign({}, HERD, {
  agents: HERD.agents.map((a) => Object.assign({}, a, { meter: false,
    subs: a.name === RD ? SUBS : a.name === QA ? AGY_SUB : [] })),
});
const SUBCONV = { name: RD, sub: "a3db8f1c2e4a6b7c9", earlier: false, entries: [
  { role: "user", kind: "prompt", text: "Review target: src/pay.py", ts: "t" },
  { role: "tools", ts: "t", tools: [{ name: "Read", hint: "src/pay.py" }] },
  { role: "assistant", text: "Found the bug in the retry path.", ts: "t" },
] };
// Tall enough to scroll, for the scroll-keeping check.
const TALLSUB = { name: RD, sub: "a3db8f1c2e4a6b7c9", earlier: false,
  entries: Array.from({ length: 40 }, (_, i) => ({ role: "assistant", text: "line " + i + "\nmore\nmore\nmore", ts: "t" })) };

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(120);
      // The column needs a conversation first -- its parent transcript path
      // is where a sub's path is built from.
      await page.evaluate((p) => GM.setConversation(p),
        { name: RD, path: PATH, cli: "claude", earlier: false, entries: [{ role: "assistant", text: "on it", ts: "t" }] });
      await sleep(80);

      const card = '.rcol[data-name="' + RD + '"] .rsubs .sub[data-sub="a3db8f1c2e4a6b7c9"]';
      check(W("the sub-agent card is in the column"), !!(await page.$(card)));
      // agy (and grok) keep no openable sub transcript -- the card shows but
      // does not open, rather than opening a broken overlay.
      const agyCard = '.rcol[data-name="' + QA + '"] .rsubs .sub';
      check(W("an agy sub-agent card shows but is not clickable"),
        !!(await page.$(agyCard)) && !(await page.$(agyCard + '[data-act="a:sub-open"]')));
      await tap(card);
      await sleep(80);
      const watch = intents.filter((i) => i.op === "sub-watch");
      check(W("clicking it asks Emacs to watch that sub, with the parent path"),
        watch.length === 1 && watch[0].name === RD && watch[0].sub === "a3db8f1c2e4a6b7c9" && watch[0].path === PATH, watch);
      const reading = await page.$eval(".subview .subbody", (b) => b.textContent).catch(() => "");
      check(W("the overlay opens, reading until Emacs answers"), /Reading/.test(reading), reading);

      await page.evaluate((p) => GM.setSubConversation(p), SUBCONV);
      await sleep(80);
      const body = await page.$eval(".subview .subbody", (b) => b.textContent);
      check(W("its conversation and tool calls show"), /Found the bug in the retry path\./.test(body) && /Read/.test(body), body);
      const head = await page.$eval(".subview .subhead", (h) => h.textContent);
      check(W("its who and title are in the header"), /general-purpose/.test(head) && /Build R4 sync debug project/.test(head), head);
      // A stray push for a different sub is ignored.
      await page.evaluate(() => GM.setSubConversation({ name: "claude-deploy-platform", sub: "other", entries: [{ role: "assistant", text: "nope", ts: "t" }] }));
      await sleep(40);
      check(W("a push for another sub is not shown"), !/nope/.test(await page.$eval(".subview .subbody", (b) => b.textContent)));

      // A poll while the reader is scrolled up keeps their place -- it must
      // not snap the overlay back to the top each time Emacs answers.
      await page.evaluate((p) => GM.setSubConversation(p), TALLSUB);
      await sleep(60);
      const mid = await page.$eval(".subview .subbody", (b) => {
        b.scrollTop = Math.floor((b.scrollHeight - b.clientHeight) / 2); return b.scrollTop; });
      await page.evaluate((p) => GM.setSubConversation(p), TALLSUB); // a poll, same content
      await sleep(60);
      const after = await page.$eval(".subview .subbody", (b) => b.scrollTop);
      check(W("a poll while scrolled up keeps the reader's place"), mid > 0 && after === mid, { mid, after });

      await tap('.subview [data-act="a:sub-close"]');
      await sleep(80);
      check(W("Close removes the overlay and tells Emacs to stop"),
        !(await page.$(".subview")) && intents.some((i) => i.op === "sub-unwatch"));

      const wide = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
      check(W("no sideways scroll"), wide <= 0, wide);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }

    // The chat view keeps no per-agent conversation, so a fresh page has an
    // empty convs -- the state the chat view is always in.  The sub card
    // there must still open: Emacs resolves the transcript from the name,
    // not a path the page lacks.  (A new context each time, so nothing from a
    // column flow has seeded convs.)
    for (const where of ["phone", "desk"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => "chat/" + where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(120);
      await page.evaluate((p) => GM.setChat(p), { project: DP, earlier: false, missing: [],
        entries: [{ who: RD, kind: "say", text: "hello", ts: "t" }] });
      await sleep(60);
      await page.selectOption(".chat .chat-to", RD);
      await sleep(80);
      const chatCard = '.chat .cmeter .sub[data-sub="a3db8f1c2e4a6b7c9"]';
      check(W("the sub-agent card shows in the chat meter box"), !!(await page.$(chatCard)));
      await tap(chatCard);
      await sleep(80);
      const cw = intents.filter((i) => i.op === "sub-watch").pop();
      check(W("clicking it asks Emacs to watch that sub by name, no path needed"),
        cw && cw.name === RD && cw.sub === "a3db8f1c2e4a6b7c9", cw);
      const creading = await page.$eval(".subview .subbody", (b) => b.textContent).catch(() => "");
      check(W("the overlay opens, not a dead 'not known yet'"), /Reading/.test(creading), creading);
      await page.evaluate((p) => GM.setSubConversation(p), SUBCONV);
      await sleep(60);
      check(W("its conversation shows once Emacs answers"),
        /Found the bug in the retry path\./.test(await page.$eval(".subview .subbody", (b) => b.textContent)));
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
