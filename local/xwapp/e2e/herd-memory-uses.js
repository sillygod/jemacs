// The Memory tab shows how often each fact reached an agent (ask 17f83e5d):
// asks and recalls in the last 30 days and the last of them, a rule's
// openings apart -- and, while the herd has counted for less than 30 days,
// "since <the day counting began>", never a bare zero claiming a history it
// does not have.  "Unused" shows the facts no ask or recall used in the
// window; "Used" orders by uses.  A sidecar that does not count shows none
// of it.  Desk and phone; GM.* driven as Emacs pushes, no sidecar.
// Screenshots go to shots/memory-uses/ (SHOTS overrides).
const { webkit } = require("playwright");
const fs = require("fs");
const path = require("path");
const { phonePage } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const SHOTS = process.env.SHOTS || path.join(__dirname, "shots", "memory-uses");
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };

const now = Date.now();
const iso = (ms) => new Date(ms).toISOString();
const DAY = 86400e3;
const PROJECTS = { user: { key: "user", label: "User (shared)" },
  projects: [{ key: "-src-dp", label: "deploy-platform", project: "~/src/deploy-platform/" }] };
const uses = (o) => Object.assign({ ask: 0, recall: 0, opening: 0, used: 0, last: null, last_opening: null, total: 0 }, o);
const fact = (name, title, u, extra) => Object.assign({ name, title, description: title.toLowerCase(),
  type: "project", author: "claude-dp", body: title, shared: false, created: iso(now - 60 * DAY),
  updated: iso(now - 50 * DAY), uses: u }, extra);
const FACTS = [
  fact("branch-discipline", "Branch discipline", uses({ ask: 4, recall: 1, opening: 6, used: 5, last: iso(now - 2 * 3600e3), total: 9 }),
       { type: "feedback" }),
  fact("deploy-trigger", "Deploy trigger", uses({ ask: 1, used: 1, last: iso(now - 3 * DAY), total: 1 })),
  fact("old-habit", "Old habit", uses({ last: iso(now - 45 * DAY), total: 2 })),
  fact("never-came-up", "Never came up", uses({})),
  fact("a-rule-only-given", "A rule only given", uses({ opening: 6, last_opening: iso(now - DAY) }), { type: "feedback" }),
  fact("brand-new", "Brand new", uses({}), { created: iso(now - 2 * DAY), updated: iso(now - 2 * DAY) }),
  fact("promoted-one", "Promoted one", uses({}), { promoted: "CLAUDE.md" }),
];
const OLD = iso(now - 90 * DAY);     // counting began long ago
const YOUNG = iso(now - 5 * DAY);    // counting began 5 days ago

const names = (page) => page.$$eval("#mem-list .mhead", (es) => es.map((e) => e.getAttribute("data-name")));
const useOf = (page, n) => page.$eval('.mhead[data-name="' + n + '"] .muse', (e) => e.textContent).catch(() => null);
const overflow = (page) => page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

async function show(page, since, facts) {
  await page.evaluate(() => GM.showView("memory"));
  await page.evaluate((p) => GM.setMemoryProjects(p), PROJECTS);
  await page.evaluate((a) => GM.renderMemory({ key: "-src-dp", facts: a.f, usesSince: a.s, usesDays: a.s ? 30 : null }),
                      { f: facts || FACTS, s: since });
  await sleep(80);
}

(async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const browser = await webkit.launch();
  try {
    for (const [label, ctxOpts] of [["desk", DESK], ["phone", PHONE]]) {
      const { ctx, page, errors } = await phonePage(browser, { context: ctxOpts });
      const W = (n) => label + ": " + n;
      const tap = (sel) => (label === "phone" ? page.tap(sel) : page.click(sel));
      await page.evaluate(() => { try { localStorage.removeItem("ghostherd.memory.sort"); } catch (e) {} });

      // ---- Counting for long: "in 30 days" ----
      await show(page, OLD);
      check(W("the list says what is counted"),
        /Uses: asks and recalls in 30 days/.test(await page.$eval(".musehead", (e) => e.textContent)));
      let u = await useOf(page, "branch-discipline");
      check(W("a used fact: how many, of which kind, and when last"),
        /used 5× in 30 days \(4 asks, 1 recall\) · last 2h ago/.test(u), u);
      check(W("a rule's openings show apart"), /given in full at 6 starts/.test(u), u);
      u = await useOf(page, "old-habit");
      check(W("used before the window: none in it, and when it last was"), /no use in 30 days · last used/.test(u), u);
      u = await useOf(page, "never-came-up");
      check(W("never used: no use in 30 days"), u === "no use in 30 days", u);
      u = await useOf(page, "a-rule-only-given");
      check(W("a rule only ever given at starts is not used"), /^no use in 30 days · given in full at 6 starts$/.test(u), u);
      u = await useOf(page, "brand-new");
      check(W("a fact written after counting began says so"), /^no use since it was written, /.test(u), u);

      // ---- Unused: the facts no ask or recall used ----
      check(W("Unused shows when counts come"), await page.$eval("#mem-unused", (e) => !e.hidden && e.offsetWidth > 0));
      await tap("#mem-unused");
      await sleep(60);
      check(W("Unused shows exactly the unused, promoted ones aside"),
        same((await names(page)).sort(), ["a-rule-only-given", "brand-new", "never-came-up", "old-habit"]), await names(page));
      check(W("Unused is pressed"), await page.$eval("#mem-unused", (e) => e.getAttribute("aria-pressed") === "true"));
      await page.fill("#mem-filter", "habit");
      await sleep(60);
      check(W("the text filter narrows within Unused"), same(await names(page), ["old-habit"]), await names(page));
      await page.fill("#mem-filter", "");
      await sleep(60);
      await page.screenshot({ path: path.join(SHOTS, label + "-unused.png"), fullPage: true });
      await tap("#mem-unused");
      await sleep(60);
      check(W("off again, every fact but the promoted"), (await names(page)).length === 6, await names(page));

      // ---- Used: most used first ----
      await tap('[data-act="mem-sort"][data-sort="used"]');
      await sleep(60);
      const order = await names(page);
      check(W("Used orders by uses, then the last"),
        same(order.slice(0, 2), ["branch-discipline", "deploy-trigger"]), order);
      check(W("the Used sort is kept for this viewer"),
        await page.evaluate(() => localStorage.getItem("ghostherd.memory.sort")) === "used");
      await page.screenshot({ path: path.join(SHOTS, label + "-used.png"), fullPage: true });
      check(W("nothing runs off the page's edge"), (await overflow(page)) <= 0, await overflow(page));
      await tap('[data-act="mem-sort"][data-sort="recent"]');
      await sleep(40);

      // ---- Counting began 5 days ago: honest about it ----
      await show(page, YOUNG);
      check(W("young counting: the list says since when, and that earlier uses are not known"),
        /asks and recalls since .*counting began then, so earlier uses are not known/.test(
          await page.$eval(".musehead", (e) => e.textContent)));
      u = await useOf(page, "never-came-up");
      check(W("young counting: an unused fact is 'no use since', not a zero"),
        /^no use since .*, when counting began$/.test(u), u);
      u = await useOf(page, "branch-discipline");
      check(W("young counting: uses are 'since', not 'in 30 days'"), /used 5× since /.test(u) && !/30 days/.test(u), u);
      await tap("#mem-unused");
      await sleep(40);
      check(W("young counting: Unused says since"),
        /used since/.test(await page.$eval("#mem-unused", (e) => e.title)));
      await tap("#mem-unused");
      await sleep(40);

      // ---- A sidecar that does not count: none of it ----
      await show(page, null, FACTS.map((f) => Object.assign({}, f, { uses: undefined })));
      check(W("no counts: no line, no header"), !(await page.$(".muse")) && !(await page.$(".musehead")));
      check(W("no counts: no Unused button"), await page.$eval("#mem-unused", (e) => e.hidden && e.offsetWidth === 0));
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log("\n" + (results.length - failed) + "/" + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
