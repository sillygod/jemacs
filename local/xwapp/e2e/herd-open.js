// How long the herd page takes to open on a phone, and that it does not
// wait for what it does not show: a real Emacs serving ghostherd's page
// (herd-open.el, with a herd of 928 sessions and an Inbox of 25), through
// the proxy that does what `tailscale serve' does, over a phone's link --
// slowed to its bandwidth and round trip.  Opened cold (nothing cached)
// and again (a reload: the files asked with their ETags); for each, when
// the Inbox was drawn and what came before it.  Then the Sessions tab,
// which asks for the sessions as it shows.
//
// LINKS=wifi,cell,poor picks the links timed (cell alone by default);
// TIMES=1 prints each request.  With LOCAL another checkout's local/, its
// page and Emacs code: the checks fail on what waited for the sessions.
const fs = require("fs");
const path = require("path");
const { start, watch, BOX, PHONE, until, sleep } = require("./appslib");

const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

const LINKS = {
  wifi: { kbps: 20000, rtt: 40 },
  cell: { kbps: 5000, rtt: 120 },
  poor: { kbps: 1500, rtt: 250 },
};
const kb = (b) => (b / 1024).toFixed(1);

// What Emacs noted: each request, intent, sidecar call and `next' answered.
const emacsLog = (dir) => {
  try {
    return fs.readFileSync(path.join(dir, "log.jsonl"), "utf8").trim().split("\n").filter(Boolean).map(JSON.parse);
  } catch (_) { return []; }
};

// The page loaded -- PAGE reloaded, or a new one of CTX -- until its
// Inbox is drawn: when each request ended, and the page's own marks.
async function open(s, ctx, page) {
  const again = !!page;
  page = page || (await ctx.newPage());
  const reqs = [];
  const t0 = Date.now();
  const short = (u) => u.replace(/^https?:\/\/[^/]+\/[0-9a-f]{32}\//, "/").replace(/\?page=[0-9a-f]+/, "");
  const ended = (r) => {
    const at = Date.now() - t0;
    reqs.push(Promise.all([r.sizes().catch(() => null), r.response().catch(() => null)]).then(([sz, rs]) => ({
      url: short(r.url()), method: r.method(), at, status: rs ? rs.status() : 0,
      bytes: sz ? sz.responseBodySize : 0, enc: rs ? rs.headers()["content-encoding"] || "" : "",
    })));
  };
  page.on("requestfinished", ended);
  if (again) await page.reload({ waitUntil: "commit" });
  else await page.goto(BOX + "/" + s.secretOf("herd") + "/ghostherd/ui/index.html", { waitUntil: "commit" });
  // Marks, by the page's own clock, from its first parse on.
  await page.evaluate(() => {
    const m = (window.__marks = {});
    const at = () => Math.round(performance.timeOrigin + performance.now());
    const look = () => {
      if (!m.inbox && document.querySelector(".ibcard")) m.inbox = at();
      if (!m.zero && document.querySelector(".ibempty:not(.ibcoming)")) m.zero = at();
      if (!m.coming && document.querySelector(".ibcoming")) m.coming = at();
      const n = document.getElementById("sessions-n");
      if (!m.count && n && n.textContent) m.count = at();
    };
    look();
    new MutationObserver(look).observe(document, { subtree: true, childList: true, characterData: true });
  });
  const drawn = await until(() => page.evaluate(() => !!(window.__marks && window.__marks.inbox)), 30000);
  await sleep(400);
  page.off("requestfinished", ended);
  const marks = await page.evaluate(() => Object.assign({}, window.__marks, {
    dcl: Math.round(performance.timeOrigin + performance.getEntriesByType("navigation")[0].domContentLoadedEventEnd) }));
  const rel = (t) => (t ? t - t0 : null);
  const done = (await Promise.all(reqs)).sort((a, b) => a.at - b.at);
  const inbox = rel(marks.inbox);
  const before = done.filter((q) => q.at <= inbox);
  return { page, drawn, inbox, dcl: rel(marks.dcl), zero: rel(marks.zero), coming: rel(marks.coming), count: rel(marks.count),
           reqs: done, before, nextBefore: before.filter((q) => /\/next$/.test(q.url)).reduce((a, q) => a + q.bytes, 0) };
}

function show(name, r) {
  console.log(`     ${name}: DOMContentLoaded ${r.dcl} ms · Inbox drawn ${r.inbox} ms · ` +
              `${r.before.length} requests, ${kb(r.before.reduce((a, q) => a + q.bytes, 0))} KB before it ` +
              `(${kb(r.nextBefore)} KB of next)`);
  if (process.env.TIMES)
    r.reqs.forEach((q) => console.log(`       ${String(q.at).padStart(6)} ${q.method.padEnd(4)} ${String(q.status).padEnd(3)} ` +
                                      `${kb(q.bytes).padStart(7)} KB ${q.enc.padEnd(4)} ${q.url}`));
}

(async () => {
  const names = (process.env.LINKS || "cell").split(",");
  for (const name of names) {
    const s = await start({ el: "herd-open.el", link: LINKS[name] });
    try {
      const ctx = await s.browser.newContext(PHONE);
      const errors = [];
      const cold = await open(s, ctx);
      watch(cold.page, errors);
      show(name + " cold", cold);
      await sleep(500);
      const warm = await open(s, ctx, cold.page);
      show(name + " again", warm);
      if (name !== names[0]) continue;

      // ---- Opening on the Inbox waits for nothing it does not show ----
      for (const [when, r] of [["cold", cold], ["again", warm]]) {
        check(`${name} ${when}: the Inbox is drawn`, r.drawn);
        check(`${name} ${when}: no session list comes before the Inbox (next ≤ 64 KB before it)`,
          r.nextBefore <= 64 * 1024, { nextKB: kb(r.nextBefore) });
        check(`${name} ${when}: the Inbox says it is on its way, not "Inbox zero", until it comes`,
          r.coming !== null && r.zero === null, { coming: r.coming, zero: r.zero, inbox: r.inbox });
      }
      const log = emacsLog(s.dir);
      const lists = log.filter((e) => e.what.rpc === "memory_list");
      check(`${name}: Emacs listed the sessions by their count alone for both loads`,
        lists.length === 2 && lists.every((e) => e.what.limit === 1), lists.map((e) => e.what.limit));
      const ops = log.filter((e) => e.what.intent).map((e) => e.what.intent);
      const first = ops.slice(ops.lastIndexOf("xwapp-seen") + 1);
      check(`${name}: the Inbox is the first thing the page asks of Emacs`, first[0] === "inbox-watch", first.slice(0, 4));
      const page = warm.page;
      check(`${name}: the Sessions tab's badge is their count`,
        (await page.$eval("#sessions-n", (n) => n.textContent)) === "928");

      // ---- The Sessions tab asks for them as it shows ----
      await page.tap('.tab[data-tab="sessions"]');
      const coming = await page.$eval("#sess-list", (b) => b.textContent).catch(() => "");
      check(`${name}: Sessions says the list is on its way`, /Listing the sessions/.test(coming), coming.slice(0, 60));
      const listed = await until(() => page.$eval("#sess-list .sum", (b) => b.textContent === "928 of 928 sessions").catch(() => false), 15000);
      check(`${name}: Sessions lists all 928`, listed);
      await page.tap('.tab[data-tab="inbox"]');
      await sleep(300);
      await page.tap('.tab[data-tab="search"]');
      await sleep(300);
      check(`${name}: Search lists the recent ones, from the list it has`,
        (await page.$$eval("#results .srow", (r) => r.length)) === 12);
      await page.tap('.tab[data-tab="sessions"]');
      await sleep(800);
      const all = emacsLog(s.dir).filter((e) => e.what.rpc === "memory_list" && e.what.limit === 5000);
      check(`${name}: asked for once, not again on each tab shown`, all.length === 1, all.length);
      check(`${name}: no page errors`, errors.length === 0, errors);
    } finally {
      await s.stop();
    }
  }
  const failed = results.filter((x) => !x).length;
  console.log(`\n${results.length - failed}/${results.length} passed`);
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
