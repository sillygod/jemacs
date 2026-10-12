// pr-view and clickup-view on your phone, end to end: opened by Emacs's
// address that holds no secret, they show what it names, with the
// forges read by Emacs; what Emacs would do on the desk -- open a link,
// copy one -- they do on the phone; a reload comes back to the same
// place; and they fit the phone.  At the desk, as before.
const { webkit } = require("playwright");
const { start, watch, measure, until, sleep, log, BOX, PHONE } = require("./appslib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const PR = "https://bitbucket.org/unno/shop/pull-requests/42";
const intents = (app, op) => log().filter((w) => w && w.intent && w.app === app && (!op || w.intent.op === op)).map((w) => w.intent);
const did = (key) => log().filter((w) => w && w[key] !== undefined);
const fits = (m, bar) => !m.pageScroll && !m.past.length && !m.small.length && !m.smallFonts.filter((f) => !/act-close|file-viewed|^input$/.test(f)).length && m.top <= bar;
const text = (page, sel) => page.textContent(sel).catch(() => "");
// A tab opened without an opener starts blank, then goes.
const landed = async (page) => { await until(() => page.url() && page.url() !== "about:blank", 5000); return page.url(); };
const hidden = (page, sel) => page.$eval(sel, (e) => e.hidden || getComputedStyle(e).display === "none").catch(() => true);

(async () => {
  const s = await start();
  try {
    const ctx = await s.browser.newContext(PHONE);
    // The forge's and ClickUp's own pages, away from the tailnet: https,
    // which the proxy does not tunnel, answered here.
    await ctx.route(/^https:\/\//, (r) => r.fulfill({ status: 200, contentType: "text/plain", body: "away" }));
    const errors = [];
    const tab = await ctx.newPage();
    watch(tab, errors);

    // ---- pr-view, at a pull request ----
    await tab.goto(BOX + "/open/pr-view#pr=" + encodeURIComponent(PR));
    const shown = await until(async () => /Retry the payment webhook/.test(await text(tab, "#app")), 10000);
    check("the address opens pr-view at that pull request", shown && tab.url() === BOX + "/" + s.secretOf("pr") + "/pr-view/ui/index.html#pr=" + encodeURIComponent(PR),
      tab.url().replace(s.secretOf("pr"), "SECRET"));
    check("which it asked Emacs for, Emacs reading Bitbucket", intents("pr-view", "ready").some((i) => i.pr === PR) &&
      log().includes("pr-view reopened") && did("http").some((w) => /repositories\/unno\/shop\/pullrequests\/42$/.test(w.url)));
    await until(async () => /Payment webhook is dropped/.test(await text(tab, "#app")));
    check("its ClickUp task looked up, by Emacs", /IN PROGRESS/i.test(await text(tab, ".task-chip")));
    check("in the bar: back, copy and open; no Create", !(await hidden(tab, "#btn-back")) && !(await hidden(tab, "#btn-copy")) &&
      !(await hidden(tab, "#btn-browser")) && (await hidden(tab, "#btn-create")));
    let m = await measure(tab);
    check("the pull request fits the phone", fits(m, 56), m);
    check("its bar says which, whole, beside its buttons", await tab.$eval("#context", (c) =>
      /#42/.test(c.textContent) && c.scrollWidth <= c.clientWidth + 1 && c.getBoundingClientRect().width > 60));
    for (const t of ["files", "commits"]) {
      await tab.tap('.tab[data-tab="' + t + '"]');
      await sleep(300);
      m = await measure(tab);
      check("its " + t + " fit too", fits(m, 56), m);
    }

    // What Emacs would do on the desk, done here.
    const open1 = ctx.waitForEvent("page");
    await tab.tap("#btn-browser");
    const bb = await open1;
    check("Open: the pull request on Bitbucket, in a tab of the phone", (await landed(bb)) === PR, bb.url());
    await bb.close();
    await tab.tap('.tab[data-tab="commits"]');
    await sleep(200);
    const open2 = ctx.waitForEvent("page");
    await tab.tap(".commits .linkish");
    const commit = await open2;
    check("a commit, the same", /bitbucket\.org\/unno\/shop\/commits\//.test(await landed(commit)), commit.url());
    await commit.close();
    await tab.tap("#btn-copy");
    await sleep(300);
    check("Copy: on the phone's clipboard, said so", /Copied PR link|Could not copy/.test(await text(tab, "#status")), await text(tab, "#status"));
    check("Emacs opened nothing and copied nothing", !did("browse").length && !did("copy").length &&
      !intents("pr-view").some((i) => /open-browser|copy-url|open-task/.test(i.op)), { browse: did("browse"), copy: did("copy") });

    // ---- clickup-view, from the task chip ----
    await tab.tap('.tab[data-tab="overview"]');
    await sleep(200);
    const open3 = ctx.waitForEvent("page");
    await tab.tap(".task-chip");
    const cu = await open3;
    watch(cu, errors);
    const cuShown = await until(async () => /Payment webhook is dropped/.test(await text(cu, "#app")), 10000);
    check("its task opens in clickup-view's page there", cuShown &&
      cu.url() === BOX + "/" + s.secretOf("task") + "/clickup-view/ui/index.html#task=86abc123", cu.url().replace(s.secretOf("task"), "SECRET"));
    check("which asked Emacs for it", intents("clickup-view", "ready").some((i) => i.task === "86abc123") && log().includes("clickup-view reopened"));
    m = await measure(cu);
    check("the task fits the phone", fits(m, 100), m);
    const open4 = ctx.waitForEvent("page");
    await cu.tap("#btn-browser");
    const cuWeb = await open4;
    check("Open: the task on ClickUp, in a tab of the phone", (await landed(cuWeb)) === "https://app.clickup.com/t/86abc123", cuWeb.url());
    await cuWeb.close();
    const open5 = ctx.waitForEvent("page");
    await cu.tap('.md a[href^="https://bitbucket.org"]');
    const prWeb = await open5;
    check("a link in it, the same", (await landed(prWeb)) === PR, prWeb.url());
    await prWeb.close();
    await cu.tap("#btn-copy");
    await sleep(300);
    check("Copy: on the phone's clipboard, said so", /Copied link|Could not copy/.test(await text(cu, "#status")), await text(cu, "#status"));
    check("and Emacs opened and copied nothing", !did("browse").length && !did("copy").length &&
      !intents("clickup-view").some((i) => /open-browser|copy-url/.test(i.op)));
    check("where you are, space and list, whole on a line of its own", await cu.$eval("#crumbs", (c) => {
      const bar = c.closest(".top").querySelector(".actions").getBoundingClientRect();
      const r = c.getBoundingClientRect();
      return c.scrollWidth <= c.clientWidth + 1 && r.top >= bar.bottom - 1 && /Back-End/.test(c.textContent) && /BACKLOG/.test(c.textContent);
    }));
    // Its address follows the page: a list has no task in it, a task does.
    await cu.tap('#crumbs .crumb:last-child');
    await until(async () => (await cu.$$(".row")).length >= 2, 8000);
    check("on its list, the address names no task", !/#task=/.test(cu.url()), cu.url());
    await cu.tap('.row[data-id="86abc123"]');
    await until(async () => /Payment webhook is dropped/.test(await text(cu, "h1, .task-title, #app")) && /#task=/.test(cu.url()), 8000);
    check("back on the task, it does", cu.url().endsWith("#task=86abc123"), cu.url());
    await cu.reload();
    check("reloaded, the task again", await until(async () => /Payment webhook is dropped/.test(await text(cu, "#app")), 10000) &&
      intents("clickup-view", "ready").filter((i) => i.task === "86abc123").length >= 2);

    // ---- back in pr-view: its list, and a reload ----
    await tab.tap("#btn-back");
    await until(async () => !!(await tab.$(".pr-row")), 8000);
    check("its list keeps the repository in the address", tab.url().endsWith("#repo=" + encodeURIComponent("bitbucket/unno/shop")), tab.url());
    check("no Create on the phone", await hidden(tab, "#btn-create"));
    m = await measure(tab);
    check("the list fits the phone", fits(m, 56), m);
    check("a row's when, comments and reviewers on one line under its title", await tab.$eval('.pr-row[data-id="42"]', (r) => {
      const tops = [".col:not(.activity)", ".activity", ".reviewers"].map((s) => Math.round(r.querySelector(s).getBoundingClientRect().top));
      return tops.every((t) => Math.abs(t - tops[0]) <= 8) && tops[0] > r.querySelector(".summary").getBoundingClientRect().bottom - 1;
    }));
    await tab.reload();
    check("reloaded, the same list", await until(async () => (await tab.$$(".pr-row")).length === 3, 10000) &&
      intents("pr-view", "ready").some((i) => i.repo === "bitbucket/unno/shop"));
    await tab.tap('.pr-row[data-id="42"]');
    await until(async () => !!(await tab.$('.tab[data-tab="overview"]')));
    check("a pull request opened from it is kept in the address", tab.url().endsWith("#pr=" + encodeURIComponent(PR)), tab.url());
    check("no page errors on the phone", errors.length === 0, errors);
    await ctx.close();

    // ---- at the desk, as before ----
    const desk = await webkit.launch();
    try {
      const dpage = await (await desk.newContext()).newPage();
      const derr = [];
      watch(dpage, derr);
      await dpage.goto(s.urls.pr);
      await until(async () => !!(await dpage.$(".pr-row")), 10000);
      check("at the desk: Create, and the bar's words", !(await hidden(dpage, "#btn-create")) && /List/.test(await dpage.textContent("#btn-back")));
      await dpage.click('.pr-row[data-id="42"]');
      await until(async () => !!(await dpage.$('.tab[data-tab="overview"]')));
      await dpage.click("#btn-browser");
      await dpage.click("#btn-copy");
      check("Open and Copy are Emacs's there", await until(() => did("browse").length === 1 && did("copy").length === 1),
        { browse: did("browse"), copy: did("copy") });
      check("no page errors at the desk", derr.length === 0, derr);
    } finally {
      await desk.close();
    }
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
