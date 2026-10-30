// The Threads tab's project chips and its order (ask 7c6eb080): All and a
// chip a project, Recent | Cost.  The herd filters and orders the list
// before it pages it (herd_threads), so the page asks for the view and
// shows the herd's list of it -- the loaded page, as that view shows it,
// only until the herd answers; a list of the view before is not shown.
// The viewer's choice is kept in this browser, and the page works with
// storage refused.  Desk and phone; Emacs played here.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const { LIST, serve } = require("./threads-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 900 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
// Each thread's task cost: t4 the costliest, t1 none known.
const TOTAL = { t2: 52000, t3: 9000, t4: 120000, t5: 400 };
const ALL = LIST.threads.map((t) => Object.assign({}, t, { cost: TOTAL[t.id] ? { id: "k" + t.id, mode: "lead", total: TOTAL[t.id] } : null }));
const PROJECTS = [".emacs.d", "deploy-platform", "jnotes"];
// What the herd answers for a view: as server/herd.py `threads' does it.
function herd(view, limit) {
  let list = ALL.filter((t) => !view.project || t.projects.includes(view.project)).sort((a, b) => b.at - a.at);
  if (view.sort === "cost") list = list.sort((a, b) => (!a.cost - !b.cost) || ((b.cost || {}).total || 0) - ((a.cost || {}).total || 0));
  return Object.assign({}, LIST, { threads: list.slice(0, limit), more: list.length > limit, projects: PROJECTS,
    view: { project: view.project || "", sort: view.sort || "recent" } });
}
const ids = (page) => page.$$eval(".thlist .throw", (rs) => rs.map((r) => r.getAttribute("data-id")));
const bar = (page) => page.evaluate(() => ({
  chips: [...document.querySelectorAll(".thbar .chip")].map((c) => c.textContent + (c.classList.contains("on") ? "*" : "")),
  sort: [...document.querySelectorAll(".thbar .msort button")].filter((b) => b.classList.contains("on")).map((b) => b.textContent),
}));
const stored = (page) => page.evaluate(() => [localStorage.getItem("ghostherd.threads.project"), localStorage.getItem("ghostherd.threads.sort")]);

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const stop = serve(page, intents);
      const views = () => intents.filter((i) => i.op === "threads-watch" || i.op === "threads-view");
      await sleep(150);
      await tap('.tab[data-tab="threads"]');
      await sleep(120);
      check(W("watching asks for every project's threads, newest first"),
        JSON.stringify(views().pop()) === JSON.stringify({ op: "threads-watch", project: "", sort: "recent" }), views());
      await page.evaluate((l) => GM.renderThreads(l), herd({ project: "", sort: "recent" }, 40));
      await sleep(150);
      let b = await bar(page);
      check(W("a chip a project, All first and on; Recent on"),
        b.chips.join() === "All*,.emacs.d,deploy-platform,jnotes" && b.sort.join() === "Recent", b);
      check(W("the list newest first"), (await ids(page)).join() === "t1,t2,t3,t4,t5");

      // ---- A project: asked of the herd, the loaded page shown of it till it answers ----
      await tap('.thbar .chip[data-project="jnotes"]');
      await sleep(80);
      check(W("a chip asks the herd for that project's threads"),
        JSON.stringify(views().pop()) === JSON.stringify({ op: "threads-view", project: "jnotes", sort: "recent" }));
      check(W("until it answers, the loaded page's threads of it"), (await ids(page)).join() === "t2,t3");
      check(W("the chip is on"), (await bar(page)).chips.includes("jnotes*"));
      check(W("and kept for this viewer"), JSON.stringify(await stored(page)) === JSON.stringify(["jnotes", "recent"]));
      // A list asked for before the change comes late: not shown.
      await page.evaluate((l) => GM.renderThreads(l), herd({ project: "", sort: "recent" }, 40));
      await sleep(80);
      check(W("a list of the view before, come late, is not shown"), (await ids(page)).join() === "t2,t3");
      await page.evaluate((l) => GM.renderThreads(l), herd({ project: "jnotes", sort: "recent" }, 1));
      await sleep(80);
      check(W("the herd's list of it, paged, with Older threads"),
        (await ids(page)).join() === "t2" && !!(await page.$(".thmore")));

      // ---- By cost: the costliest first, none known last ----
      await tap('.thbar [data-act="th:sort"][data-sort="cost"]');
      await sleep(80);
      check(W("Cost asks for the same project by cost"),
        JSON.stringify(views().pop()) === JSON.stringify({ op: "threads-view", project: "jnotes", sort: "cost" }));
      await tap('.thbar .chip[data-project=""]');
      await sleep(80);
      check(W("All keeps the order"), JSON.stringify(views().pop()) === JSON.stringify({ op: "threads-view", project: "", sort: "cost" }));
      await page.evaluate((l) => GM.renderThreads(l), herd({ project: "", sort: "cost" }, 40));
      await sleep(100);
      check(W("by cost: the costliest first, one with none known last"), (await ids(page)).join() === "t4,t2,t3,t5,t1");
      b = await bar(page);
      check(W("Cost on, All on"), b.sort.join() === "Cost" && b.chips[0] === "All*", b);
      check(W("both kept"), JSON.stringify(await stored(page)) === JSON.stringify(["", "cost"]));

      // ---- Loaded again: the kept view; a project gone from the list still a chip ----
      await page.evaluate(() => localStorage.setItem("ghostherd.threads.project", "taiex"));
      intents.length = 0;
      await page.reload();
      await page.waitForFunction(() => window.GM && window.XW);
      await sleep(150);
      if (!(await page.$(".threadspage"))) await tap('.tab[data-tab="threads"]');
      await sleep(120);
      check(W("loaded again, it watches the view kept"),
        JSON.stringify(views().pop()) === JSON.stringify({ op: "threads-watch", project: "taiex", sort: "cost" }), views());
      await page.evaluate((l) => GM.renderThreads(l), herd({ project: "taiex", sort: "cost" }, 40));
      await sleep(100);
      b = await bar(page);
      check(W("a project none of the threads read names is still a chip, on"),
        b.chips.join() === "All,.emacs.d,deploy-platform,jnotes,taiex*" && b.sort.join() === "Cost", b);
      check(W("and the list says it has none of it"),
        (await ids(page)).length === 0 && /No threads of taiex/.test(await page.textContent(".thlist")));
      stop();
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }

    // ---- Storage refused: every project's, newest first, and the chips still work ----
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: Object.assign({}, DESK) });
      await page.addInitScript(() => {
        const no = () => { throw new Error("storage refused"); };
        Object.defineProperty(window, "localStorage", { get: no });
      });
      intents.length = 0;
      await page.reload();
      await page.waitForFunction(() => window.GM && window.XW);
      await sleep(150);
      if (!(await page.$(".threadspage"))) await page.click('.tab[data-tab="threads"]');
      await sleep(120);
      const w = intents.filter((i) => i.op === "threads-watch").pop() || {};
      check("no storage: it watches every project's, newest first", w.project === "" && w.sort === "recent", w);
      await page.evaluate((l) => GM.renderThreads(l), herd({ project: "", sort: "recent" }, 40));
      await sleep(100);
      await page.click('.thbar .chip[data-project="deploy-platform"]');
      await sleep(80);
      check("no storage: a chip still asks for its project",
        (intents.filter((i) => i.op === "threads-view").pop() || {}).project === "deploy-platform");
      check("no storage: no page errors", errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
