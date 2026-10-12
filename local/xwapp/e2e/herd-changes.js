// The herd page says what a checkout has not committed: a chip on its
// work line -- on the default branch too, which says nothing of a pull
// request -- that opens pr-view's view of it.  On a phone a link through
// Emacs's address that holds no secret, when Emacs says pr-view opens
// there; at the desk a button, and Emacs opens it.  A clean checkout
// says nothing.
const path = require("path");
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
// At the desk the page is Emacs's own, a file, and speaks to it by its title.
const DESK = "file://" + path.join(process.env.LOCAL || path.resolve(__dirname, "../.."), "ghostherd", "ui", "index.html");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DP = "~/src/deploy-platform/";
const JN = "~/src/jnotes/";
const LOCAL = "/open/pr-view#local=" + encodeURIComponent(DP);

const herd = (patch) => {
  const h = JSON.parse(JSON.stringify(HERD));
  h.open = { pr: true, task: true };
  Object.assign(h.work[0], { branch: "main", pr: null, tasks: [], changes: { files: 3, added: 12, deleted: 4 } });
  h.work[1].changes = { files: 1, added: 0, deleted: 5 };
  if (patch) patch(h);
  return h;
};
// The project's own work line, not an agent's.
const line = (page, project) => page.$eval('.pgroup[data-project="' + project + '"] > .work', (w) => ({
  text: w.textContent, notes: [...w.querySelectorAll(".wnote")].map((n) => n.textContent),
  chips: [...w.querySelectorAll(".wchip")].map((c) => ({ cls: c.className, tag: c.tagName.toLowerCase(), text: c.textContent,
    href: c.getAttribute("href"), target: c.getAttribute("target"), title: c.title })),
})).catch(() => null);

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- On a phone ----
    const intents = [];
    const { ctx, page, errors } = await phonePage(browser, { intents });
    const show = async (h) => { await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, h); await sleep(80); };
    await show(herd());
    let dp = await line(page, DP);
    const chg = dp && dp.chips.find((c) => /chg/.test(c.cls));
    check("the default branch says what it has not committed", chg && chg.text === "3 uncommitted+12−4" &&
      chg.title === "3 files not committed, +12 −4", dp);
    check("and nothing of a pull request", dp && !dp.notes.length && dp.chips.length === 1, dp);
    check("a link to pr-view's view of it, in pr-view's tab", chg && chg.tag === "a" && chg.href === LOCAL && chg.target === "pr-view", chg);
    const jn = await line(page, JN);
    check("beside a pull request; a count of none not said",
      jn && jn.chips.map((c) => c.cls.split(" ")[1]).join(",") === "pr,chg,task" && jn.chips[1].text === "1 uncommitted−5", jn);
    const fit = await page.$$eval(".wchip.chg", (cs) => cs.every((c) => c.getBoundingClientRect().right <= document.documentElement.clientWidth));
    check("it fits the phone", fit);

    const popup = ctx.waitForEvent("page");
    await page.tap('.pgroup[data-project="' + DP + '"] .wchip.chg');
    const tab = await popup;
    await tab.waitForLoadState("domcontentloaded").catch(() => {});
    check("tapped, pr-view opens at it", tab.url().endsWith(LOCAL), tab.url());
    await tab.close();

    // In its room, the same.
    await page.tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
    await sleep(150);
    const room = await page.$eval(".rwork .wchip.chg", (c) => c.getAttribute("href")).catch(() => null);
    check("in its room, the same", room === LOCAL, room);
    await page.evaluate(() => document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" })));
    await sleep(100);

    // Clean: the default branch says nothing at all.
    await show(herd((h) => { h.work[0].changes = false; }));
    check("a clean default branch says nothing", (await line(page, DP)) === null);
    await show(herd((h) => { h.work[1].changes = { files: 0, added: 0, deleted: 0 }; }));
    check("nor a clean branch of a pull request", !(await line(page, JN)).chips.some((c) => /chg/.test(c.cls)));

    // pr-view not said to open there: a button that says why it does nothing.
    await show(herd((h) => { h.open = { pr: false, task: false }; }));
    dp = await line(page, DP);
    check("where pr-view does not open, no link", dp.chips[0].tag === "button" && !dp.chips[0].href, dp.chips);
    await page.tap('.pgroup[data-project="' + DP + '"] .wchip.chg');
    await sleep(100);
    check("tapped, it says so, and asks Emacs for nothing",
      /not known yet/.test(await page.textContent("#status").catch(() => "")) && !intents.some((i) => i.op === "open-changes"));
    check("no page errors on the phone", errors.length === 0, errors);
    await ctx.close();

    // ---- At the desk: Emacs opens it ----
    const dctx = await browser.newContext({ viewport: { width: 1280, height: 900 } });
    const desk = await dctx.newPage();
    const derr = [];
    desk.on("pageerror", (e) => derr.push(String(e)));
    await desk.goto(DESK);
    await desk.waitForFunction(() => window.GM && window.XW);
    await desk.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
    await sleep(80);
    const btn = await desk.$eval('.pgroup[data-project="' + DP + '"] .wchip.chg', (c) => c.tagName.toLowerCase());
    await desk.click('.pgroup[data-project="' + DP + '"] .wchip.chg');
    let asked = null;
    for (let i = 0; i < 40 && !asked; i++) {
      const t = await desk.title();
      const m = /^ghmem:(.*)$/.exec(t);
      const intent = m && JSON.parse(m[1]);
      if (intent && intent.op === "open-changes") asked = intent;
      else if (m) await desk.evaluate(() => { document.title = "Herd"; });
      await sleep(50);
    }
    check("at the desk a button: Emacs is asked to open that checkout's",
      btn === "button" && asked && asked.root === DP, { btn, asked });
    check("no page errors at the desk", derr.length === 0, derr);
    await dctx.close();
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
