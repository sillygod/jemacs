// A turn the Mac's sleep cut short: the agent reads as blocked, and its
// card (herd and room) counts down to the herd continuing it on its own
// ("continuing after sleep in Ns") with a Cancel that tells Emacs to leave
// it blocked.  Once the herd has stopped retrying, the card says so.  A
// dropped connection is carried on the same way, in its own words: the
// card never says the computer slept.  Desk and phone.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";

// A herd where the implementer's turn was cut by the Mac sleeping, with the
// continue ~40s out.
const herd = (over) => {
  const h = JSON.parse(JSON.stringify(HERD));
  const a = h.agents.find((x) => x.name === "claude-deploy-platform");
  a.state = "blocked"; a.reason = "its turn was cut off: the computer slept";
  a.continueAt = Date.now() + 40000; a.continueStopped = false;
  Object.assign(a, over || {});
  return h;
};

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const lastOf = (op) => intents.filter((i) => i.op === op).pop();
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(120);

      // ---- The herd card ----
      const row = '.arow[data-name="claude-deploy-platform"]';
      check(W("the slept agent reads as blocked"), !!(await page.$(row + ".s-blocked")));
      check(W("its card counts down to a continue"),
        /continuing after sleep in \d+s/.test(await page.$eval(row, (r) => r.textContent)));
      check(W("the countdown offers a Cancel"), !!(await page.$(row + ' [data-act="a:agent-sleep-cancel"]')));
      await tap(row + ' [data-act="a:agent-sleep-cancel"]');
      await sleep(80);
      check(W("Cancel tells Emacs to leave that agent blocked"),
        (lastOf("agent-sleep-cancel") || {}).name === "claude-deploy-platform", lastOf("agent-sleep-cancel"));

      // Once the herd has stopped retrying, the card says so -- no countdown,
      // no Cancel.
      await page.evaluate((h) => GM.renderHerd(h), herd({ continueAt: 0, continueStopped: true }));
      await sleep(100);
      check(W("a stopped agent says it stopped retrying"),
        /stopped retrying/.test(await page.$eval(row, (r) => r.textContent)));
      check(W("a stopped agent has no Cancel"), !(await page.$(row + ' [data-act="a:agent-sleep-cancel"]')));

      // A dropped connection: the same countdown and stop note, in its own
      // words -- and the per-second tick keeps them.
      const dropped = { reason: "its turn was cut off: the connection dropped",
                        continueAfter: "after a dropped connection",
                        continueAgain: "the connection dropped again" };
      await page.evaluate((h) => GM.renderHerd(h), herd(dropped));
      await sleep(100);
      const said = async () => page.$eval(row, (r) => r.textContent);
      check(W("a dropped connection counts down in its own words"),
        /continuing after a dropped connection in \d+s/.test(await said()), await said());
      check(W("it does not say the computer slept"), !/sleep|slept/.test(await said()));
      await sleep(1100);
      check(W("the tick keeps the dropped connection's words"),
        /continuing after a dropped connection in \d+s/.test(await said()) && !/sleep/.test(await said()), await said());
      await page.evaluate((h) => GM.renderHerd(h), herd(Object.assign({}, dropped, { continueAt: 0, continueStopped: true })));
      await sleep(100);
      check(W("a stopped dropped connection says so in its words"),
        /stopped retrying — the connection dropped again/.test(await said()), await said());

      // A healthy agent shows none of this.
      check(W("a healthy agent has no continue line"),
        !(await page.$('.arow[data-name="qa-deploy-platform"] [data-act="a:agent-sleep-cancel"]')) &&
        !/continuing after sleep/.test(await page.$eval('.arow[data-name="qa-deploy-platform"]', (r) => r.textContent)));

      // ---- The room card ----
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      await page.evaluate((h) => GM.renderHerd(h), herd());
      await sleep(120);
      const col = '.rcol[data-name="claude-deploy-platform"]';
      check(W("the room card counts down too"),
        /continuing after sleep in \d+s/.test(await page.$eval(col, (r) => r.textContent)));
      check(W("the room card offers Cancel"), !!(await page.$(col + ' [data-act="a:agent-sleep-cancel"]')));
      await tap(col + ' [data-act="a:agent-sleep-cancel"]');
      await sleep(80);
      check(W("the room Cancel tells Emacs for that agent"),
        (lastOf("agent-sleep-cancel") || {}).name === "claude-deploy-platform");

      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
