// The Agents tab's order: the state that needs you first, then the most
// recently updated -- not by name.  Groups too, by their most recent
// agent.  A blocked group stays on top however old; among same-state
// groups (and agents within one), the latest rises.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

const now = Date.now();
// A valid agent is involved enough to clone one the fixture already made.
const mk = (name, project, state, since) =>
  Object.assign(JSON.parse(JSON.stringify(HERD.agents[0])),
    { name, project, state, since, kind: "claude", role: "", meter: false, subs: [], links: false, work: false });

// Names are chosen so alphabetical and recency disagree: the newest group
// (z) and newest agent (z-zzz) sort last by name.  By our rule: m first
// (blocked, however old), then z (newest 5m) before a (1h); and within z,
// z-zzz (5m) before z-aaa (1h).  Alphabetical would give m, a, z and
// z-aaa, z-zzz.
const herd = () => Object.assign(JSON.parse(JSON.stringify(HERD)), {
  agents: [
    mk("z-aaa", "~/z/", "idle", now - 60 * 60e3),
    mk("z-zzz", "~/z/", "idle", now - 5 * 60e3),
    mk("a-only", "~/a/", "idle", now - 60 * 60e3),
    mk("m-blocked", "~/m/", "blocked", now - 2 * 3600e3),
  ],
  asks: [], work: [],
});

(async () => {
  const browser = await webkit.launch();
  try {
    const { ctx, page, errors } = await phonePage(browser, {});
    await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
    await sleep(100);
    const groupOrder = await page.$$eval(".pgroup", (els) => els.map((e) => e.getAttribute("data-project")));
    check("blocked group first, then idle groups newest-first (m, z, a) -- not alphabetical",
      JSON.stringify(groupOrder) === JSON.stringify(["~/m/", "~/z/", "~/a/"]), groupOrder);
    const zRows = await page.$$eval('.pgroup[data-project="~/z/"] .arow', (els) => els.map((e) => e.getAttribute("data-name")));
    check("within a group, the more recently updated leads (not by name)",
      JSON.stringify(zRows) === JSON.stringify(["z-zzz", "z-aaa"]), zRows);
    check("no page errors", errors.length === 0, errors);
    await ctx.close();
  } finally {
    await browser.close();
  }
  const bad = results.filter((r) => !r).length;
  console.log(bad ? bad + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(bad ? 1 : 0);
})();
