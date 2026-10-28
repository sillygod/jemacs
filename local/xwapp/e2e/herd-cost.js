// What a task cost, on the Threads tab: each thread's task in its row --
// its total and the lines its commits changed, or why none -- and under
// the open thread's summary, lead / workers / sub-agents and per 100
// lines; above the list, lead mode against direct mode, a week or a month.
// On a phone and at a desk.  Fixture data, Emacs played here.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const { DETAILS, LIST } = require("./threads-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };

const COSTS = {
  t1: { id: "k1", agent: "lead-.emacs.d", mode: "lead", prompt: "三項補強都做", total: 6512000, own: 287000, workers: 6225000,
        subs: 0, asks: 1, commits: 2, lines: 1472, code: "yes" },
  t2: { id: "k2", agent: "lead-jnotes", mode: "lead", prompt: "", total: 2500000, own: 484000, workers: 2000000,
        subs: 16000, asks: 2, commits: 1, lines: 0, code: "unmerged" },
  t3: { id: "k3", agent: "primary", mode: "lead", prompt: "jnote 專案狀況如何", total: 900000, own: 900000, workers: 0,
        subs: 0, asks: 2, commits: 0, lines: 0, code: "no" },
};
const side = (tasks, median, total, per100, lines, coded, noCode, own) =>
  ({ tasks, median, total, per100, lines, coded, noCode, unmerged: 0, unknown: 0, own });
const COMPARE = [
  { days: 7, since: "2026-10-22", lead: side(80, 259265, 124810072, 1468009, 8502, 9, 51, 16),
    direct: side(44, 261628, 20339800, 22108478, 92, 1, 41, 100) },
  { days: 30, since: "2026-09-29", lead: side(210, 300000, 400000000, 1200000, 33000, 30, 120, 14),
    direct: side(150, 250000, 60000000, null, 0, 0, 150, 100) },
];
const withCost = (t) => Object.assign({}, t, { cost: COSTS[t.id] || null });
const DATA = Object.assign({}, LIST, { threads: LIST.threads.map(withCost), costs: COMPARE });

function serve(page, intents) {
  let seen = 0;
  const timer = setInterval(async () => {
    while (seen < intents.length) {
      const it = intents[seen++];
      if (it.op !== "thread-open" || !DETAILS[it.id]) continue;
      await page.evaluate((p) => GM.renderThread(p), withCost(DETAILS[it.id])).catch(() => {});
    }
  }, 40);
  return () => clearInterval(timer);
}

const table = (page) => page.$$eval(".thcosts .thctable tbody tr", (rs) =>
  Object.fromEntries(rs.map((r) => [r.querySelector("th").textContent, [...r.querySelectorAll("td")].map((d) => d.textContent)])));

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const stop = serve(page, intents);
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      await page.click('.tab[data-tab="threads"]').catch(() => page.evaluate(() => GM.showView && GM.showView("threads")));
      await sleep(150);
      await page.evaluate((d) => GM.renderThreads(d), DATA);
      await sleep(250);

      // ---- Lead mode against direct mode ----
      let t = await table(page);
      check(W("a week's tasks, median, total and per 100 lines, each mode"),
        t["tasks"].join() === "80,44" && t["median per task"].join() === "259k,262k" && t["total"].join() === "124.8M,20.3M" &&
        t["per 100 lines"].join() === "1.5M,22.1M", t);
      check(W("the lines and the tasks they came in, no code, the lead's own share"),
        t["lines changed"].join() === "8502 in 9,92 in 1" && t["no code"].join() === "51,41" && t["lead's own share"].join() === "16%,—", t);
      const head = await page.textContent(".thcosts .thtalkhead");
      check(W("since when, in what unit"), /since 2026-10-22/.test(head) && /weighted tokens/.test(head), head);
      await tap('.thcosts [data-act="th:cost-days"][data-days="30"]');
      await sleep(100);
      t = await table(page);
      check(W("a month at a tap; none per 100 lines with no lines"),
        t["tasks"].join() === "210,150" && t["per 100 lines"].join() === "1.2M,—", t);
      await tap('.thcosts [data-act="th:cost-days"][data-days="7"]');
      await sleep(100);

      // ---- Each thread's task ----
      const chips = await page.$$eval(".thlist .throw", (rs) => rs.map((r) => {
        const c = r.querySelector(".thcost");
        return c ? { text: c.textContent, title: c.title } : null;
      }));
      check(W("a thread's task in its row: its total and the lines it changed"), chips[0] && chips[0].text === "6.5M · 1472 lines", chips[0]);
      check(W("its title: the task, lead / workers / sub-agents, its asks and commits"),
        chips[0] && /Its task: 三項補強都做/.test(chips[0].title) && /the lead 287k · workers 6.2M · sub-agents 0/.test(chips[0].title) &&
        /1 ask · 1472 lines changed in 2 commits/.test(chips[0].title), chips[0] && chips[0].title);
      check(W("made but not merged yet, and no code, say so"),
        chips[1] && chips[1].text === "2.5M · not merged yet" && chips[2] && chips[2].text === "900k · no code", chips.slice(1, 3));
      check(W("a thread whose task is not known: no cost"), chips[3] === null && chips[4] === null, chips.slice(3));

      if (where === "desk") {
        await page.waitForSelector(".thmain .thcostline", { timeout: 3000 }).catch(() => {});
      } else {
        await tap('.throw[data-id="t1"]');
        await page.waitForSelector(".thcostline", { timeout: 3000 }).catch(() => {});
      }
      const line = await page.$eval(".thcostline", (p) => p.textContent).catch(() => "");
      check(W("the open thread: its task's cost, each part, per 100 lines"),
        line === "Its task: 6.5M weighted tokens — lead 287k · workers 6.2M · sub-agents 0 · 1472 lines · 442k per 100 lines", line);
      if (where === "phone") {
        await tap('[data-act="th:back"]');
        await sleep(150);
        const fits = await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth + 1);
        check(W("the comparison fits the phone"), fits);
      }
      check(W("no page error"), errors.length === 0, errors);
      stop();
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
