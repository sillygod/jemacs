// One tab per app: a pull request clicked again reuses and refocuses the
// tab already showing pr-view, instead of a new tab each time.  The tabs
// arrange it over a BroadcastChannel they share (same origin): an open
// pr-view tab, asked to show a url, navigates to it and comes forward.
// This drives that directly -- a second same-origin tab broadcasts the
// request, and the open pr-view tab follows it, opening no new tab.  (The
// herd page's side, XW.openApp from a chip click, is applied live; bringing
// a background tab to the front is all a browser allows and may be refused.)
const { start, watch, until, sleep, BOX, PHONE } = require("./appslib");
const PR = "https://bitbucket.org/unno/shop/pull-requests/42";
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const txt = (page) => page.textContent("#app").catch(() => "");
const prUrl = "/open/pr-view#pr=" + encodeURIComponent(PR);

(async () => {
  const s = await start();
  try {
    const root = s.urls.local;
    const ctx = await s.browser.newContext(PHONE);
    await ctx.route(/^https:\/\//, (r) => r.fulfill({ status: 200, contentType: "text/plain", body: "away" }));
    const errors = [];

    // Tab A: pr-view, showing the checkout's changes.
    const a = await ctx.newPage();
    watch(a, errors);
    await a.goto(BOX + "/open/pr-view#local=" + encodeURIComponent(root));
    await until(async () => (await a.$$eval(".tree-file", (bs) => bs.length).catch(() => 0)) > 0, 10000);
    check("pr-view shows the checkout first", /Uncommitted changes/.test(await txt(a)));

    // Tab B: another same-origin page (clickup-view) broadcasts a request to
    // show a PR in pr-view -- as the herd page's XW.openApp would.
    const b = await ctx.newPage();
    watch(b, errors);
    await b.goto(BOX + "/open/clickup-view#task=86abc123");
    await until(async () => /\S/.test(await txt(b)), 10000);
    await b.evaluate((url) => new BroadcastChannel("xwapp-open").postMessage({ nav: "pr-view", url }), prUrl);

    // Tab A follows it to the PR -- reused in place, no new tab.
    check("the open pr-view tab shows the PR it was asked for (reused)",
      await until(async () => /Retry the payment webhook/.test(await txt(a)), 8000), await txt(a));
    check("no new tab was opened (only the two we made)", ctx.pages().length === 2, ctx.pages().length);

    // Asked for what it already shows: it does not reload.
    await until(async () => (await a.evaluate(() => location.hash)).indexOf("pr=") >= 0, 4000);
    const mark = await a.evaluate(() => { window.__kept = (window.__kept || 0) + 1; return window.__kept; });
    await b.evaluate((url) => new BroadcastChannel("xwapp-open").postMessage({ nav: "pr-view", url }), prUrl);
    await sleep(400);
    check("asked for the PR it already shows, it does not reload",
      (await a.evaluate(() => window.__kept || 0)) === mark);

    check("no page errors", errors.length === 0, errors);
    await ctx.close();
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
