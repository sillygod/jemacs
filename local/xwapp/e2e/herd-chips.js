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

    // ---- Named in a conversation: another repository's too ----
    // An agent at work on two: a PR of the other is named with it, and
    // opened by it -- there may be no checkout of it here.
    const GH = "https://github.com/wingeatech/backend-interview/pull/";
    const ref = (id, own, state, url) => Object.assign({ id, own, title: "PR " + id, state, draft: false, url, error: null },
      own ? { host: "bitbucket", owner: "unno", repo: "deploy-platform" } : { host: "github", owner: "wingeatech", repo: "backend-interview" });
    const named = (h) => {
      const lead = h.agents.find((a) => a.name === "lead-deploy-platform");
      lead.links.tasks = [];
      lead.links.prs = [ref(2, false, "OPEN", GH + "2"), ref(378, false, "OPEN", GH + "378"),
                        ref(371, true, "OPEN", BB.replace("378", "371")), ref(378, true, "OPEN", BB)];
    };
    const namedChips = () => page.$$eval('.arow[data-name="lead-deploy-platform"] .links .wchip.pr', (cs) => cs.map((c) => ({
      text: c.textContent, href: c.getAttribute("href"), title: c.title, tag: c.tagName.toLowerCase(),
      root: c.getAttribute("data-root"), repo: c.getAttribute("data-repo") })));
    await show(herd({ pr: true, task: true }, named));
    let ns = await namedChips();
    check("another repository's PR is named with it; its own is not",
      JSON.stringify(ns.map((c) => c.text)) === JSON.stringify(["backend-interview#2 Open", "backend-interview#378 Open", "#371 Open"]), ns.map((c) => c.text));
    check("its branch's PR is not said twice; another repository's of the same number is", ns.filter((c) => /378/.test(c.text)).length === 1);
    check("whose it is, in full, on a hover", ns[0].title === "wingeatech/backend-interview#2: PR 2", ns[0].title);
    check("on the phone, it opens in pr-view there by its address",
      ns[0].tag === "a" && ns[0].href === "/open/pr-view#pr=" + encodeURIComponent(GH + "2"), ns[0]);

    // ---- A chip whose state is not trusted says "?" ----
    // One once open whose latest look failed keeps its chip but is shown
    // unsure -- "?" not a stale Open; a merged one stays merged; a number
    // that was never a real PR (no state, an error) is left out.
    const namedUnsure = (h) => {
      const lead = h.agents.find((a) => a.name === "lead-deploy-platform");
      lead.links.tasks = [];
      lead.links.prs = [
        Object.assign(ref(395, true, "OPEN", BB.replace("378", "395")),
          { unsure: true, error: "the forge refused the token" }),
        ref(360, true, "MERGED", BB.replace("378", "360")),
        { id: 999, own: false, host: "github", owner: "wingeatech", repo: "backend-interview",
          title: "", state: "", draft: false, url: "", error: "no such pull request" },
      ];
    };
    await show(herd({ pr: true, task: true }, namedUnsure));
    const us = await namedChips();
    // The unsure one is kept up front (treated as live, like an open one).
    check("an unsure PR says '?', not a stale Open, and keeps its chip",
      us.some((c) => /#395\s*\?/.test(c.text)) && !us.some((c) => /#395\s*Open/.test(c.text)), us.map((c) => c.text));
    check("the unsure chip carries no state colour (no pr-<state> class)",
      await page.$eval('.arow[data-name="lead-deploy-platform"] .links .wchip.pr[data-id="395"]',
        (e) => !e.className.split(/\s+/).some((k) => /^pr-/.test(k))));
    check("a number that was never a real PR is left out", !us.some((c) => /#999/.test(c.text)), us.map((c) => c.text));
    // A merged one is final and sure -- it waits behind +N; opened, it reads merged.
    await page.click('.arow[data-name="lead-deploy-platform"] .links .wmore');
    await sleep(80);
    const expanded = await namedChips();
    check("a merged one, revealed, still reads merged (not unsure)",
      expanded.some((c) => /#360\s*Merged/.test(c.text)), expanded.map((c) => c.text));

    // At the desk: Emacs is told which -- another's by its repository.
    const desk = await phonePage(browser, { host: "127.0.0.1", context: { viewport: { width: 1280, height: 900 }, isMobile: false, hasTouch: false } });
    await desk.page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd({ pr: true, task: true }, named));
    await sleep(80);
    ns = await desk.page.$$eval('.arow[data-name="lead-deploy-platform"] .links .wchip.pr', (cs) => cs.map((c) => ({
      tag: c.tagName.toLowerCase(), root: c.getAttribute("data-root"), repo: c.getAttribute("data-repo") })));
    check("at the desk, another's names its repository, its own its checkout",
      ns[0].tag === "button" && ns[0].repo === "github/wingeatech/backend-interview" && ns[0].root === null &&
      ns[2].repo === null && ns[2].root === "~/src/deploy-platform/", ns);
    await desk.page.click('.arow[data-name="lead-deploy-platform"] .links .wchip.pr[data-id="2"]');
    await desk.page.click('.arow[data-name="lead-deploy-platform"] .links .wchip.pr[data-id="371"]');
    await sleep(150);
    const opened = desk.intents.filter((i) => i.op === "open-pr").map((i) => ({ id: i.id, root: i.root, repo: i.repo }));
    check("and is asked to open it so",
      JSON.stringify(opened) === JSON.stringify([{ id: 2, repo: "github/wingeatech/backend-interview" }, { id: 371, root: "~/src/deploy-platform/" }]), opened);
    check("no page errors at the desk", desk.errors.length === 0, desk.errors);
    await desk.ctx.close();
    check("no page errors", errors.length === 0, errors);
    await ctx.close();
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
