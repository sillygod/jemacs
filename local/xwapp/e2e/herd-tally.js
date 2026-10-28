// What the agents' replies cost, in tokens: each agent's today and over
// the last seven days, beside its context bar, and each project's in its
// head, with its lead's share -- one number each, the kinds in its title.
// On a phone and at a desk: on the Agents tab and in a room's columns.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const LEAD = "lead-deploy-platform";
const RD = "claude-deploy-platform";
const QA = "qa-deploy-platform";

const t = (w, input, write, read, output) => ({ w, input, write, read, output });
const span = (today, week) => ({ today, week });
const TALLY = {
  [LEAD]: span(t(1250000, 2000, 300000, 8000000, 40000), t(12400000, 9000, 2000000, 90000000, 300000)),
  [RD]: span(t(3400000, 5000, 900000, 21000000, 120000), t(41000000, 20000, 8000000, 280000000, 900000)),
};
const PROJECT = {
  project: DP,
  today: t(4650000, 7000, 1200000, 29000000, 160000), week: t(53400000, 29000, 10000000, 370000000, 1200000),
  lead: span(t(1250000, 0, 0, 0, 0), t(12400000, 0, 0, 0, 0)),
  workers: span(t(3400000, 0, 0, 0, 0), t(41000000, 0, 0, 0, 0)),
};
const meter = (tokens) => ({ model: "Opus 5.5", effort: "xhigh", tokens, window: 1000000, pending: false, compacted: false });
const herd = (extra) => Object.assign({}, HERD, {
  agents: HERD.agents.map((a) => Object.assign({}, a, {
    meter: a.name === QA ? false : meter(380944), tally: TALLY[a.name] || false, switching: false })),
  projectTally: [PROJECT],
}, extra || {});

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const { ctx, page, errors } = await phonePage(browser, { context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const show = (h) => page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, h);
      const row = (name) => '.arow[data-name="' + name + '"]';
      const read = (scope) => page.$eval(scope, (r) => {
        const m = r.querySelector(".meter .mtally");
        if (!m) return null;
        const meter = r.querySelector(".meter").getBoundingClientRect();
        const box = m.getBoundingClientRect();
        return { text: m.textContent, title: m.title, inside: box.right <= meter.right + 0.5 && box.left >= meter.left - 0.5,
                 words: (r.querySelector(".meter .mwords") || {}).textContent || "" };
      });
      await show(herd());
      await sleep(100);

      const lead = await read(row(LEAD));
      check(W("today's and the week's, one number each, beside the context"),
        lead && lead.text === "today 1.3M · 7d 12.4M" && lead.words === "38% · 381k / 1M", lead);
      check(W("its title: the unit, then the kinds of each"),
        lead && /input-equivalent tokens/.test(lead.title) &&
        lead.title.includes("Today: 1.3M — input 2k · cache write 300k · cache read 8M · output 40k") &&
        lead.title.includes("Last 7 days: 12.4M — input 9k · cache write 2M · cache read 90M · output 300k"), lead && lead.title);
      check(W("it stays within the row"), lead && lead.inside, lead);
      // An agent with no meter, the sidecar having counted nothing of it:
      // neither; one with a count but no meter (agy before its first
      // reply's context): the count alone.
      const none = await read(row(QA));
      check(W("nothing counted: no tally"), none === null, none);
      await show(herd({ agents: herd().agents.map((a) => a.name === QA ? Object.assign({}, a, { tally: TALLY[RD] }) : a) }));
      await sleep(100);
      const bare = await read(row(QA));
      check(W("a tally with no meter shows alone"), bare && bare.text === "today 3.4M · 7d 41M", bare);

      // ---- A project's ----
      await show(herd());
      await sleep(100);
      const head = await page.$eval('.pgroup[data-project="' + DP + '"] .phead', (h) => {
        const p = h.querySelector(".ptally");
        return p && { text: p.textContent, title: p.title };
      });
      check(W("a project's today and week, and its lead's share of the week"),
        head && head.text === "today 4.7M · 7d 53.4M · lead 23%", head);
      check(W("its title: lead and workers apart, each span"),
        head && head.title.includes("Today: 4.7M — lead 1.3M (27%), workers 3.4M") &&
        head.title.includes("Last 7 days: 53.4M — lead 12.4M (23%), workers 41M"), head && head.title);
      const other = await page.$('.pgroup[data-project="~/src/jnotes/"] .phead .ptally');
      check(W("a project the sidecar has counted nothing of: none"), !other);
      if (where === "phone") {
        const fits = await page.$eval('.pgroup[data-project="' + DP + '"] .phead', (h) =>
          h.scrollWidth <= h.clientWidth + 1 && document.documentElement.scrollWidth <= window.innerWidth + 1);
        check(W("the head still fits the phone"), fits);
      }

      // ---- In a room's columns ----
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      const col = await read('.rcol[data-name="' + LEAD + '"] .rtop');
      check(W("a column's head has it too"), col && col.text === "today 1.3M · 7d 12.4M", col);
      check(W("no page error"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
