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
// What the herd answers for a view: as server/herd.py `threads' does it
// (a task a thread here, so threads.py by_cost is a plain sort).
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
// One task (kA) across t2, t3 and t5, its cost on t5 -- begun first; t4
// a costlier task of its own; t1, the newest, none known.  As the herd lists them:
// newest first, or by cost (threads.py by_cost) -- the costliest task
// first, a task's thread with the number first and its others after it.
const KA = { id: "kA", mode: "lead", total: 90000 };
const SAME = { id: "kA", mode: "lead", same: "t5", sameTitle: "Fetch the release notes for 4.2" };
const byId = Object.fromEntries(LIST.threads.map((t) => [t.id, t]));
const G = (view, rows) => Object.assign({}, LIST, { threads: rows.map(([id, cost]) => Object.assign({}, byId[id], { cost })),
  more: false, projects: PROJECTS, view });
const G_RECENT = G({ project: "", sort: "recent" },
  [["t1", null], ["t2", SAME], ["t3", SAME], ["t4", { id: "kB", mode: "lead", total: 120000 }], ["t5", KA]]);
const G_COST = G({ project: "", sort: "cost" },
  [["t4", { id: "kB", mode: "lead", total: 120000 }], ["t5", KA], ["t2", SAME], ["t3", SAME], ["t1", null]]);
const G_DP = G({ project: "deploy-platform", sort: "cost" }, [["t4", { id: "kB", mode: "lead", total: 120000 }], ["t5", KA]]);
const G_JN = G({ project: "jnotes", sort: "cost" }, [["t2", KA], ["t3", Object.assign({}, SAME, { same: "t2" })]]);
// Every project's by cost, the first three loaded; and jnotes's, as the
// herd has it: t3 first, its task's cost the most, not loaded.
const KB = { id: "kB", mode: "lead", total: 120000 };
const SLICE = Object.assign(G({ project: "", sort: "cost" }, [["t4", KB], ["t5", KA], ["t2", SAME]]), { more: true });
const G_JN2 = G({ project: "jnotes", sort: "cost" }, [["t3", { id: "kC", mode: "lead", total: 200000 }], ["t2", KA]]);
const costs = (page) => page.$$eval(".thlist .throw", (rs) => rs.map((r) => {
  const c = r.querySelector(".thcost");
  return c ? (c.classList.contains("thsame") ? "s" : "n") : "-";
}).join(""));
const opened = (intents) => intents.filter((i) => i.op === "thread-open").map((i) => i.id);

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
      check(W(where === "desk" ? "the desk keeps its open thread, not in jnotes, till the herd answers"
                                : "the phone opens no thread on a chip"),
        where === "desk" ? opened(intents).join() === "t1" : opened(intents).length === 0, opened(intents));
      check(W("and kept for this viewer"), JSON.stringify(await stored(page)) === JSON.stringify(["jnotes", "recent"]));
      // A list asked for before the change comes late: not shown.
      await page.evaluate((l) => GM.renderThreads(l), herd({ project: "", sort: "recent" }, 40));
      await sleep(80);
      check(W("a list of the view before, come late, is not shown"),
        (await ids(page)).join() === "t2,t3" && opened(intents).length === (where === "desk" ? 1 : 0));
      await page.evaluate((l) => GM.renderThreads(l), herd({ project: "jnotes", sort: "recent" }, 1));
      await sleep(80);
      check(W("the herd's list of it, paged, with Older threads"),
        (await ids(page)).join() === "t2" && !!(await page.$(".thmore")));
      if (where === "desk") {
        check(W("the herd's list without the open thread: the desk opens its first"),
          opened(intents).join() === "t1,t2" &&
          (await page.$eval(".thlist .throw.on", (r) => r.getAttribute("data-id"))) === "t2", opened(intents));
      }

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

      // ---- A task's threads by cost: its number first, and no jump ----
      await tap('.thbar [data-act="th:sort"][data-sort="recent"]');
      await tap('.thbar .chip[data-project=""]');
      await sleep(80);
      await page.evaluate((l) => GM.renderThreads(l), G_RECENT);
      await sleep(120);
      check(W("newest first, the task's cost on the thread it began with"),
        (await ids(page)).join() === "t1,t2,t3,t4,t5" && (await costs(page)) === "-ssnn",
        [await ids(page), await costs(page)]);
      await tap('.thbar [data-act="th:sort"][data-sort="cost"]');
      await sleep(80);
      const preview = (await ids(page)).join();
      check(W("by cost, until the herd answers: the costliest task, then the task with its number first"),
        preview === "t4,t5,t2,t3,t1" && (await costs(page)) === "nnss-", [preview, await costs(page)]);
      await page.evaluate((l) => GM.renderThreads(l), G_COST);
      await sleep(100);
      check(W("the herd's list comes: no row moves"), (await ids(page)).join() === preview, await ids(page));
      intents.length = 0;
      await tap('.thbar .chip[data-project="deploy-platform"]');
      await sleep(120);
      await page.evaluate((l) => GM.renderThreads(l), G_DP);
      await sleep(100);
      // The loaded page has none of jnotes's now: the desk waits for the herd's list.
      await tap('.thbar .chip[data-project="jnotes"]');
      await sleep(120);
      const before = opened(intents).slice();
      await page.evaluate((l) => GM.renderThreads(l), G_JN);
      await sleep(150);
      check(W(where === "desk" ? "a chip opens the first of the herd's list, once it comes"
                                : "the phone opens none"),
        where === "desk" ? before.join() === (before.length ? "t4" : "") && opened(intents).join() === before.concat(["t2"]).join() &&
                           (await page.$eval(".thlist .throw.on", (r) => r.getAttribute("data-id"))) === "t2"
                         : opened(intents).length === 0, [before, opened(intents)]);

      // ---- By cost, the herd's first of a project, not the loaded page's ----
      // Every project's by cost, three loaded: of jnotes's only t2 -- while
      // the herd's first of jnotes is t3, its task the costliest.
      await tap('.thbar .chip[data-project=""]');
      await sleep(80);
      await page.evaluate((l) => GM.renderThreads(l), SLICE);
      await sleep(120);
      if (where === "desk") { await page.click('.thlist .throw[data-id="t4"]'); await sleep(120); }
      intents.length = 0;
      await tap('.thbar .chip[data-project="jnotes"]');
      await sleep(80);
      const slice = (await ids(page)).join(), atPick = opened(intents).slice();
      await page.evaluate((l) => GM.renderThreads(l), G_JN2);
      await sleep(150);
      check(W(where === "desk" ? "by cost, the desk opens the herd's first of a project, not the loaded page's"
                                : "by cost, the phone opens none"),
        slice === "t2" && (await ids(page)).join() === "t3,t2" &&
        (where === "desk" ? atPick.length === 0 && opened(intents).join() === "t3" &&
                            (await page.$eval(".thlist .throw.on", (r) => r.getAttribute("data-id"))) === "t3"
                          : opened(intents).length === 0), [slice, atPick, opened(intents)]);

      // ---- Two chips picked before the herd answers: each of the loaded page ----
      await tap('.thbar .chip[data-project=""]');
      await sleep(80);
      await page.evaluate((l) => GM.renderThreads(l), SLICE);
      await sleep(120);
      await tap('.thbar .chip[data-project="deploy-platform"]');
      const one = (await ids(page)).join();
      await tap('.thbar .chip[data-project="jnotes"]');
      const two = (await ids(page)).join();
      check(W("two chips picked quickly: each the loaded page's threads of it, never none"),
        one === "t4,t5" && two === "t2" && !/No threads of/.test(await page.textContent(".thlist")), [one, two]);
      await page.evaluate((l) => GM.renderThreads(l), G({ project: "deploy-platform", sort: "cost" }, [["t4", KB], ["t5", KA]]));
      await sleep(80);
      check(W("the first chip's list, come late, is not shown"), (await ids(page)).join() === "t2", await ids(page));
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
