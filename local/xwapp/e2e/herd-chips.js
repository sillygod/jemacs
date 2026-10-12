// The herd page's pull request and task chips on a phone: links to
// pr-view's and clickup-view's pages there, through Emacs's address
// that holds no secret, when Emacs says they open; one tab per app,
// used again.  Else the forge's and ClickUp's own pages.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const BB = "https://bitbucket.org/unno/deploy-platform/pull-requests/378";
const JNPR = "https://bitbucket.org/unno/jnotes/pull-requests/12";

const herd = (open, patch) => { const h = JSON.parse(JSON.stringify(HERD)); if (open) h.open = open; if (patch) patch(h); return h; };
const chips = (page) => page.$$eval(".wchip", (cs) => cs.map((c) => ({ tag: c.tagName.toLowerCase(), pr: c.classList.contains("pr"),
  href: c.getAttribute("href"), target: c.getAttribute("target"), rel: c.getAttribute("rel"), id: c.getAttribute("data-id") })));

(async () => {
  const browser = await webkit.launch();
  try {
    const { ctx, page, errors } = await phonePage(browser);
    const show = async (h) => { await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, h); await sleep(80); };

    await show(herd({ pr: true, task: true }));
    let cs = await chips(page);
    const pr = cs.find((c) => c.pr && c.id === "378");
    const task = cs.find((c) => !c.pr && c.id === "86ph1a");
    check("a PR chip opens pr-view's page there, at that PR", pr && pr.tag === "a" && pr.href === "/open/pr-view#pr=" + encodeURIComponent(BB) && pr.target === "pr-view", pr);
    check("a task chip opens clickup-view's page there, at that task", task && task.tag === "a" && task.href === "/open/clickup-view#task=86ph1a" && task.target === "clickup-view", task);
    check("every chip is a link to them", cs.every((c) => c.tag === "a" && /^\/open\/(pr-view#pr=|clickup-view#task=)/.test(c.href)), cs.map((c) => c.href));

    // Tapped: the link opens, in its app's tab; the next PR in the same one.
    const popup = ctx.waitForEvent("page");
    await page.tap('.wchip.pr[data-id="378"]');
    const tab = await popup;
    await tab.waitForLoadState("domcontentloaded").catch(() => {});
    check("tapped, it opens in a tab of its own", tab.url().endsWith("/open/pr-view#pr=" + encodeURIComponent(BB)), tab.url());
    let another = false;
    ctx.once("page", () => { another = true; });
    await page.tap('.wchip.pr[data-id="12"]');
    await sleep(600);
    check("another PR opens in that same tab", !another && tab.url().endsWith("/open/pr-view#pr=" + encodeURIComponent(JNPR)), { another, url: tab.url() });
    const popup2 = ctx.waitForEvent("page");
    await page.tap('.wchip.task[data-id="86ph1a"]');
    const tab2 = await popup2;
    check("a task, in clickup-view's own tab", tab2 !== tab && tab2.url().endsWith("/open/clickup-view#task=86ph1a"), tab2.url());
    check("the herd page stays where it was", /\/ghostherd\/ui\/index\.html/.test(page.url()), page.url());

    // Not opened there: the forge's and ClickUp's pages, as before.
    await show(herd({ pr: false, task: false }));
    cs = await chips(page);
    check("when they do not open there, the forge and ClickUp",
      cs.find((c) => c.id === "378").href === BB && cs.find((c) => c.id === "86ph1a").href === "https://app.clickup.com/t/86ph1a" &&
      cs.every((c) => c.target === "_blank" && c.rel === "noopener noreferrer"), cs);
    await show(herd(null));
    check("nor when Emacs does not say", (await chips(page)).find((c) => c.id === "378").href === BB);
    await show(herd({ pr: true, task: true }, (h) => { h.work[0].pr.url = "https://gitlab.example/unno/x/merge_requests/378"; }));
    check("a PR of no forge pr-view reads: its own page", (await chips(page)).find((c) => c.id === "378").href === "https://gitlab.example/unno/x/merge_requests/378");
    check("no page errors", errors.length === 0, errors);
    await ctx.close();
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
