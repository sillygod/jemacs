// What a task cost, on the Threads tab: each thread's task in its row --
// its total and the lines its commits changed, or why none; a second
// thread of one task says whose it is instead -- and under the open
// thread's summary, lead / workers / sub-agents, per 100 lines and its
// commits; above the list, lead mode against direct mode, a week or a
// month.  A task told to an agent directly is a thread of its own.  On a
// phone and at a desk.  Fixture data, Emacs played here.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const { DETAILS: ASKED, LIST } = require("./threads-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };

const now = Date.now();
const C = (sha, subject, lines, merged, of) => ({ sha, subject, lines, merged, of: of || null, at: now / 1000 });
const COSTS = {
  t1: { id: "k1", agent: "lead-.emacs.d", mode: "lead", prompt: "三項補強都做", total: 6512000, own: 287000, workers: 6225000,
        subs: 0, asks: 1, commits: 2, merged: 1, mergedLines: 1000, lines: 1472, code: "yes", threadCount: 2,
        made: [C("03ed56b76613dcb2", "feat: what one task cost", 1000, true), C("c4f217ac952539ef", "feat: the tab shows it", 472, false),
               C("9c986fb657c77416", "the same, rebased", 472, false, "c4f217ac952539ef")] },
  t2: { id: "k1", mode: "lead", same: "t1", sameTitle: "Three hardenings of the agy guard, approved by the user" },
  t3: { id: "k3", agent: "primary", mode: "lead", prompt: "jnote 專案狀況如何", total: 900000, own: 900000, workers: 0,
        subs: 0, asks: 2, commits: 0, merged: 0, mergedLines: 0, lines: 0, code: "no" },
  "task:k9": { id: "k9", agent: "claude-deploy-platform", mode: "direct", prompt: "Deploy the staging fix", total: 1200000,
        own: 1100000, workers: 0, subs: 100000, asks: 0, commits: 1, merged: 0, mergedLines: 0, lines: 120, code: "yes",
        threadCount: 1, made: [C("dd03b9d5aa11bb22", "fix: the staging config", 120, false)] },
};
// A task told to an agent directly: You → the agent.
const DIRECT = { id: "task:k9", title: "Deploy the staging fix", status: "answered", direct: true,
  lanes: [{ name: "user", kind: "you", project: "", role: "", worktree: false },
          { name: "claude-deploy-platform", kind: "claude", project: "deploy-platform", role: "", worktree: false }],
  msgs: [{ kind: "message", from: "user", to: "claude-deploy-platform", at: now - 30 * 60e3, head: "Deploy the staging fix",
           text: "Deploy the staging fix" },
         { kind: "answer", from: "claude-deploy-platform", to: "user", at: now - 20 * 60e3, head: "Deployed; the PR is open.",
           text: "Deployed; the PR is open." }],
  waiting: [], rootFrom: "user", rootTo: "claude-deploy-platform", projects: ["deploy-platform"], cross: false,
  started: now - 30 * 60e3, at: now - 20 * 60e3 };
const DETAILS = Object.assign({}, ASKED, { "task:k9": DIRECT });
const side = (tasks, median, total, per100, lines, coded, noCode, own, merged) =>
  ({ tasks, median, total, per100, lines, coded, noCode, unknown: 0, own, merged: merged || 0 });
const COMPARE = [
  { days: 7, since: "2026-10-22", lead: side(80, 259265, 124810072, 1468009, 8502, 9, 51, 16, 8000),
    direct: side(44, 261628, 20339800, 22108478, 92, 1, 41, 100, 0) },
  { days: 30, since: "2026-09-29", lead: side(210, 300000, 400000000, 1200000, 33000, 30, 120, 14),
    direct: side(150, 250000, 60000000, null, 0, 0, 150, 100) },
];
const withCost = (t) => Object.assign({}, t, { cost: COSTS[t.id] || null });
const directRow = { id: DIRECT.id, title: DIRECT.title, status: DIRECT.status, participants: ["user", "claude-deploy-platform"],
  projects: DIRECT.projects, cross: false, count: 2, open: 0, started: DIRECT.started, at: DIRECT.at, rootFrom: "user",
  rootTo: "claude-deploy-platform", direct: true };
const DATA = Object.assign({}, LIST, { threads: LIST.threads.map(withCost).concat([withCost(directRow)]), costs: COMPARE });

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
      check(W("how many of the lines are merged yet"), t["of them merged"] && t["of them merged"].join() === "8000,0", t);
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
        /1 ask · 1472 lines changed in 2 commits, 1 merged/.test(chips[0].title), chips[0] && chips[0].title);
      check(W("a second thread of one task says whose it is, not its cost again"),
        chips[1] && /^same task as “Three hardenings of the agy guard, a.*…”$/.test(chips[1].text) && !/\d+(\.\d)?[kM]/.test(chips[1].text),
        chips[1]);
      check(W("no code says so"), chips[2] && chips[2].text === "900k · no code", chips[2]);
      check(W("a thread whose task is not known: no cost"), chips[3] === null && chips[4] === null, chips.slice(3, 5));
      const direct = await page.$eval('.throw[data-id="task:k9"]', (r) => ({
        path: r.querySelector(".thpath").textContent, title: r.querySelector(".thtitle").textContent,
        proj: [...r.querySelectorAll(".thproj")].map((p) => p.textContent).join(), cost: r.querySelector(".thcost").textContent,
      })).catch(() => null);
      check(W("a task told directly is a thread: You → the agent, its words, its project, its cost"),
        direct && direct.path === "You → claude-deploy-platform" && direct.title === "Deploy the staging fix" &&
        direct.proj === "deploy-platform" && direct.cost === "1.2M · 120 lines", direct);

      if (where === "desk") {
        await page.waitForSelector(".thmain .thcostline", { timeout: 3000 }).catch(() => {});
      } else {
        await tap('.throw[data-id="t1"]');
        await page.waitForSelector(".thcostline", { timeout: 3000 }).catch(() => {});
      }
      const line = await page.$eval(".thcostline", (p) => p.textContent).catch(() => "");
      check(W("the open thread: its task's cost, across its threads, each part, merged, per 100 lines"),
        line === "Its task, across 2 threads: 6.5M weighted tokens — lead 287k · workers 6.2M · sub-agents 0 · 1472 lines (1 merged) · 442k per 100 lines", line);
      const made = await page.$$eval(".thcommits li", (ls) => ls.map((l) => l.textContent)).catch(() => []);
      check(W("and its commits: sha, subject, lines, merged or not; a copy counted once"),
        made.length === 3 && made[0] === "03ed56b7feat: what one task cost1000 lines · merged" &&
        made[1] === "c4f217acfeat: the tab shows it472 lines · not merged" &&
        made[2] === "9c986fb6the same, rebased472 lines · not merged · a copy of c4f217ac, not counted again", made);
      if (where === "phone") {
        await tap('[data-act="th:back"]');
        await sleep(150);
      }
      await tap('.throw[data-id="task:k9"]');
      await page.waitForFunction(() => /You told/.test((document.querySelector(".thsummary") || {}).textContent || ""),
        null, { timeout: 3000 }).catch(() => {});
      const opened = await page.evaluate(() => ({
        summary: (document.querySelector(".thsummary") || {}).textContent,
        texts: [...document.querySelectorAll(".thtext")].map((t) => t.textContent),
        line: (document.querySelector(".thcostline") || {}).textContent,
        made: [...document.querySelectorAll(".thcommits li")].map((l) => l.textContent),
      }));
      check(W("a direct thread opened: the user's words, the reply that ended the turn, the cost and the commits"),
        opened.summary === "You told claude-deploy-platform directly, not through a lead" &&
        opened.texts.join("|") === "Deploy the staging fix|Deployed; the PR is open." &&
        opened.line === "Its task: 1.2M weighted tokens — itself 1.1M · workers 0 · sub-agents 100k · 120 lines (0 merged) · 1M per 100 lines" &&
        opened.made.join() === "dd03b9d5fix: the staging config120 lines · not merged", opened);
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
