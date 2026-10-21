// A turn killed by an expired login: the agent reads as blocked with
// "its login expired", and its card (herd and room) offers a one-click
// /login that tells Emacs to run it.  Desk and phone.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
// A herd where the implementer's login expired mid-answer.
const herd = () => {
  const h = JSON.parse(JSON.stringify(HERD));
  const a = h.agents.find((x) => x.name === "claude-deploy-platform");
  a.state = "blocked"; a.reason = "its login expired -- run /login"; a.login = true;
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
      check(W("the login-expired agent shows blocked with a plain reason"),
        !!(await page.$(row + ".s-blocked")) && /login expired/.test(await page.$eval(row, (r) => r.textContent)));
      check(W("its card offers a one-click /login"), !!(await page.$(row + ' [data-act="a:agent-login"]')));
      await tap(row + ' [data-act="a:agent-login"]');
      await sleep(80);
      check(W("clicking /login tells Emacs to run it for that agent"),
        (lastOf("agent-login") || {}).name === "claude-deploy-platform", lastOf("agent-login"));

      // An agent whose login is fine has no /login button.
      check(W("a healthy agent has no /login"),
        !(await page.$('.arow[data-name="qa-deploy-platform"] [data-act="a:agent-login"]')));

      // ---- The /login code-entry screen: open/copy the sign-in URL ----
      const url = "https://claude.ai/oauth/authorize?client=cli&state=abc123";
      const withUrl = () => { const h = herd(); const a = h.agents.find((x) => x.name === "claude-deploy-platform");
        a.reason = "sign in: open the link, then paste the code"; a.login = false; a.url = url; return h; };
      await page.evaluate((h) => GM.renderHerd(h), withUrl());
      await sleep(100);
      check(W("the sign-in URL is offered to open"), !!(await page.$(row + ' [data-act="a:login-open"]')));
      await tap(row + ' [data-act="a:login-open"]');
      await sleep(80);
      check(W("Open sign-in link opens that URL"), (lastOf("open-browser") || {}).url === url, lastOf("open-browser"));
      await tap(row + ' [data-act="a:login-copy"]');
      await sleep(80);
      check(W("Copy link copies that URL"), (lastOf("copy") || {}).text === url, lastOf("copy"));

      // ---- The room card ----
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      await page.evaluate((h) => GM.renderHerd(h), herd());
      await sleep(120);
      const col = '.rcol[data-name="claude-deploy-platform"]';
      check(W("the room card offers /login too"), !!(await page.$(col + ' [data-act="a:agent-login"]')));
      await tap(col + ' [data-act="a:agent-login"]');
      await sleep(80);
      check(W("the room /login tells Emacs for that agent"),
        (lastOf("agent-login") || {}).name === "claude-deploy-platform");

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
