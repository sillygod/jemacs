// The herd page's Log tab as lines arrive (GM.logAdd) with a week of rows
// drawn -- a deep click, or Show earlier pressed often: each new line is
// put on top in place, not the list rebuilt.  It lands right (on top,
// under its day, the count and the Issues count with it), keeps out what
// the filters and a moment's window keep out, keeps a filter's focus and
// what is being read where it is, and costs about the same as with 300.
// A day that turns, or a line older than the last, redraws it all.  Desk
// and phone, on a clock set to a known afternoon.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const A = "claude-deploy-platform", B = "qa-deploy-platform";
const DAY = 86400e3;

// A week: A's work began six and a half days ago, under 3600 lines; every
// hundredth an issue.  The newest is an hour back, today.
function week(now) {
  const es = [{ id: 1, t: now - 6.5 * DAY, session: A, kind: "state", text: "idle → working  input sent",
                old: "idle", new: "working", reason: "input sent", pinned: false }];
  for (let i = 0; i < 3600; i++) {
    es.push({ id: 10 + i, t: now - 6.4 * DAY + i * 150e3, session: i % 4 ? A : B, kind: "input",
              text: "← line " + i, pinned: false, issue: i % 100 === 0 });
  }
  return es;
}
let nextId = 100000;
const line = (t, extra) => Object.assign({ id: nextId++, t, session: A, kind: "input", text: "← a new line", pinned: false }, extra || {});

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      // 15:00 local: today holds the last hours of the week, and midnight
      // is a setSystemTime away.
      const afternoon = new Date(); afternoon.setHours(15, 0, 0, 0);
      const { ctx, page, errors } = await phonePage(browser, { clock: { time: afternoon },
                                                                context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const add = (e) => page.evaluate((x) => GM.logAdd(x), e).then(() => sleep(30));
      // The page's now -- its clock, not this process's.
      const pnow = () => page.evaluate(() => Date.now());
      const state = () => page.evaluate(() => {
        const box = document.getElementById("log-list");
        const rows = box.querySelectorAll(".lrow");
        const first = rows[0];
        let head = first && first.previousElementSibling;
        while (head && head.tagName !== "H2") head = head.previousElementSibling;
        const count = document.querySelector('[data-act="issues-only"] .count');
        return { n: rows.length, first: first ? +first.getAttribute("data-id") : 0,
                 head: head ? head.textContent : "", heads: Array.from(box.querySelectorAll("h2")).map((h) => h.textContent),
                 sum: (box.querySelector(".sum") || {}).textContent || "", issues: count ? +count.textContent : 0,
                 kept: !!(window.__kept && window.__kept.isConnected) };
      });
      // Mark a drawn row, the last: a rebuild would replace it.
      const keep = () => page.evaluate(() => {
        const rows = document.querySelectorAll("#log-list .lrow");
        window.__kept = rows[rows.length - 1];
      });

      await tap('.tab[data-tab="log"]');
      await sleep(60);
      const now = await page.evaluate(() => Date.now());
      await page.evaluate((es) => GM.renderLog({ entries: es }), week(now));
      await tap('[data-act="range"][data-range="' + 7 * DAY + '"]');
      await sleep(80);
      // The deep click: A's segment, begun at the week's start.
      await page.evaluate(() => document.querySelector('#lanes .seg[data-id="1"]').click());
      await sleep(200);
      let s = await state();
      check(W("a deep click draws the whole week"), s.n === 3601 && s.sum === "3601 entries", s);
      const issues0 = s.issues;

      // ---- A plain line ----
      await keep();
      const x1 = line(await pnow(), { text: "← fresh one" });
      await add(x1);
      s = await state();
      check(W("a new line is on top, under Today, the one Today"),
        s.first === x1.id && s.head === "Today" && s.heads.filter((h) => h === "Today").length === 1, s);
      check(W("the count line counts it, every row still drawn"), s.sum === "3602 entries" && s.n === 3602, s);
      check(W("the list is not rebuilt: a row drawn before is the same node"), s.kept, s);
      check(W("a plain line leaves the Issues count be"), s.issues === issues0, [s.issues, issues0]);

      // ---- An issue ----
      const x2 = line(await pnow(), { session: B, kind: "state", text: "working → blocked  Approve?", old: "working",
                                    new: "blocked", reason: "Approve?", issue: true });
      await add(x2);
      s = await state();
      check(W("an issue is on top and counted on the chip"), s.first === x2.id && s.issues === issues0 + 1 && s.kept, s);
      check(W("its lane shows it at once"), !!(await page.$('#lanes .seg.st-blocked[data-id="' + x2.id + '"]')));

      // ---- A new agent, and the picker ----
      const x3 = line(await pnow(), { session: "agy-newcomer", text: "← hello" });
      await add(x3);
      const opts = await page.$$eval("#log-session option", (os) => os.map((o) => o.textContent));
      check(W("a new agent joins the picker; an agent's count goes up"),
        opts.includes("agy-newcomer (1)") && opts.includes(B + " (901)"), opts);

      // ---- Cost: about the same as with 300 drawn ----
      const ms = await page.evaluate(async () => {
        const ts = [];
        for (let k = 0; k < 7; k++) {
          const t0 = performance.now();
          GM.logAdd({ id: 200000 + k, t: Date.now(), session: "claude-deploy-platform", kind: "input", text: "← timed " + k, pinned: false });
          void document.body.offsetHeight;
          ts.push(performance.now() - t0);
        }
        return ts.sort((a, b) => a - b)[3];
      });
      check(W("with the week drawn a new line costs well under a rebuild (median ms)"), ms < 100, ms);

      // ---- What is being read stays put ----
      const mid = await page.evaluate(() => {
        const r = document.querySelectorAll("#log-list .lrow")[1800];
        r.scrollIntoView({ block: "center" });
        return { id: r.getAttribute("data-id"), top: r.getBoundingClientRect().top };
      });
      await add(line(await pnow()));
      const after = await page.evaluate((id) => document.querySelector('.lrow[data-id="' + id + '"]').getBoundingClientRect().top, mid.id);
      check(W("a row being read halfway down does not move"), Math.abs(after - mid.top) <= 1, [mid.top, after]);
      await page.evaluate(() => { document.getElementById("app").scrollTop = 0; });

      // ---- The text filter: "line", which the week's lines all have ----
      await page.fill("#log-filter", "line");
      await sleep(80);
      const f0 = await state();
      check(W("filtered to \"line\": the week's lines, all still drawn"), f0.n > 3600 && !/showing/.test(f0.sum), f0);
      await keep();
      await add(line(await pnow(), { text: "← not this one" }));
      s = await state();
      check(W("a line the filter keeps out stays out"), s.n === f0.n && s.sum === f0.sum && s.first === f0.first, s);
      const x4 = line(await pnow(), { text: "← a fresh line" });
      await add(x4);
      s = await state();
      check(W("a line it lets through is on top, the list not rebuilt"),
        s.first === x4.id && s.n === f0.n + 1 && s.sum === (f0.n + 1) + " entries" && s.kept, s);
      const focus = await page.evaluate(() => [document.activeElement && document.activeElement.id,
                                               document.getElementById("log-filter").value]);
      check(W("the filter being typed keeps its focus and its words"), focus[0] === "log-filter" && focus[1] === "line", focus);
      await page.fill("#log-filter", "");
      await sleep(60);

      // ---- Issues ----
      await tap('[data-act="issues-only"]');
      await sleep(60);
      const n0 = (await state()).n;
      await add(line(await pnow(), { text: "← plain" }));
      s = await state();
      check(W("Issues on: a plain line stays out"), s.n === n0, [s.n, n0]);
      const x5 = line(await pnow(), { kind: "ask", text: "ask 1a2b3c4d from lead-x failed: gone", issue: true });
      await add(x5);
      s = await state();
      check(W("Issues on: an issue is on top, the chip counts it"), s.first === x5.id && s.n === n0 + 1 && s.issues === issues0 + 2, s);
      await tap('[data-act="issues-only"]');
      await sleep(60);

      // ---- A moment's window ----
      await tap('.lrow[data-id="3570"] [data-act="log-zoom"]');  // hours back
      await sleep(80);
      const z0 = await state();
      const x6 = line(await pnow(), { kind: "ask", text: "ask 2b3c4d5e from lead-x failed: gone", issue: true });
      await add(x6);
      s = await state();
      check(W("zoomed hours back: a new line is not in its half hour, nor its Issues count"),
        s.n === z0.n && s.first === z0.first && s.sum === z0.sum && s.issues === z0.issues &&
        !!(await page.$('[data-act="log-unzoom"]')), [z0, s]);
      await tap('[data-act="log-unzoom"]');
      await sleep(120);
      check(W("out of it, the line is there on top"), (await state()).first === x6.id);
      await page.evaluate(() => { document.getElementById("app").scrollTop = 0; });
      await tap('.lrow[data-id="' + x6.id + '"] [data-act="log-zoom"]');
      await sleep(80);
      const x7 = line(await pnow(), { text: "← in the moment" });
      await add(x7);
      s = await state();
      check(W("zoomed on now: a new line is on top of the half hour"), s.first === x7.id && !!(await page.$('[data-act="log-unzoom"]')), s);
      await tap('[data-act="log-unzoom"]');
      await sleep(120);

      // ---- A line older than the last: drawn where it belongs ----
      const x8 = line(now - 3600e3 - 60e3, { text: "← late arrival" });
      await add(x8);
      const at = await page.evaluate((id) => {
        const rows = Array.from(document.querySelectorAll("#log-list .lrow"));
        const i = rows.findIndex((r) => +r.getAttribute("data-id") === id);
        return { i, before: i > 0 ? +rows[i - 1].getAttribute("data-id") : 0, after: +rows[i + 1].getAttribute("data-id") };
      }, x8.id);
      check(W("a line older than the last is drawn in its place, not on top"), at.i > 0 && at.after === 3609, at);

      // ---- Midnight: the day turns ----
      const t = new Date(await page.evaluate(() => Date.now()));
      t.setDate(t.getDate() + 1); t.setHours(0, 0, 30, 0);
      await page.clock.setSystemTime(t);
      const x9 = line(t.getTime(), { text: "← after midnight" });
      await add(x9);
      s = await state();
      check(W("after midnight: the line under a new Today, the day before now Yesterday"),
        s.first === x9.id && s.head === "Today" && s.heads[0] === "Today" && s.heads[1] === "Yesterday" &&
        s.heads.filter((h) => h === "Today").length === 1, s.heads.slice(0, 3));

      check(W("no sideways scroll"),
        (await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth)) <= 0);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
