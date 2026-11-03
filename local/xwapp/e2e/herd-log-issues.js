// The herd page's Log tab, looking back: a lane's segment always lands on
// its row and flashes it (past the drawn rows, a filter, the range); the
// Issues chip keeps the entries Emacs marks worth a look; and an entry's
// time zooms to its moment, the half hour around it, lanes and list, with
// a chip (or Esc) to leave -- the minute's redraw keeping it.  Desk and phone.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const MIN = 60e3;
const A = "claude-deploy-platform", B = "qa-deploy-platform", C = "grok-jnotes";

// A small afternoon.  One moment is the left-open ask at -165m: its half
// hour, -180m to -150m, holds 2 3 4 9 5 6; 1, 7, 8 and 10 lie outside.
function afternoon(now) {
  const at = (m) => now - m * MIN;
  const st = (id, m, s, old, nu, reason, extra) => Object.assign({ id, t: at(m), session: s, kind: "state",
    text: old + " → " + nu + (reason ? "  " + reason : ""), old, new: nu, reason: reason || "", pinned: false }, extra || {});
  const line = (id, m, s, kind, text, extra) => Object.assign({ id, t: at(m), session: s, kind, text, pinned: false }, extra || {});
  return [
    st(1, 200, A, "idle", "working", "input sent"),
    line(2, 178, A, "input", "← fix the flaky test"),
    st(3, 170, B, "working", "blocked", "Approve?", { why: "blocked", screen: "❯ 1. Yes\n  2. No\n", issue: true }),
    st(4, 168, B, "blocked", "working", "reported working"),
    line(9, 166, A, "report", "reports working"),
    line(5, 165, C, "ask", "ask a1b2c3d4 left open: grok-jnotes has begun no turn on it yet", { issue: true }),
    line(6, 164, C, "screen", "grok-jnotes: a /compact in progress", { why: "compaction", screen: "compacting…\n" }),
    line(7, 60, C, "screen", "grok-jnotes: a limit or quota message", { why: "limit", screen: "out of quota\n", issue: true }),
    st(8, 30, A, "working", "done", "seen"),
    st(10, 5, A, "done", "working", "input sent"),
  ];
}

// Far back: A's working began five hours ago, under 400 newer lines; B's
// began 30 hours ago, before the 24h range.
function deep(now) {
  const es = [
    { id: 5001, t: now - 30 * 60 * MIN, session: B, kind: "state", text: "idle → working  input sent", old: "idle", new: "working", reason: "input sent", pinned: false },
    { id: 5000, t: now - 300 * MIN, session: A, kind: "state", text: "idle → working  input sent", old: "idle", new: "working", reason: "input sent", pinned: false },
  ];
  for (let i = 0; i < 400; i++) {
    es.push({ id: 6000 + i, t: now - 240 * MIN + i * 30e3, session: A, kind: "input", text: "← line " + i, pinned: false });
  }
  return es;
}

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const { ctx, page, errors } = await phonePage(browser, { clock: true, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const ids = () => page.$$eval("#log-list .lrow", (es) => es.map((e) => +e.getAttribute("data-id")));
      const on = (act) => page.$$eval('.log-tools [data-act="' + act + '"]', (es) => es.map((e) => e.classList.contains("on")));
      const rangeOn = () => page.$$eval('.log-tools [data-act="range"].on', (es) => es.map((e) => e.textContent));
      const flashed = (id) => page.$('.lrow[data-id="' + id + '"].flash').then((x) => !!x);
      // The SEGMENT'th segment of SESSION's lane, by where it is drawn --
      // not by its attributes, which this change renamed.
      const tapSeg = async (session, nth) => {
        const box = await page.evaluate(([s, n]) => {
          const lane = Array.from(document.querySelectorAll("#lanes .lane"))
            .find((l) => { const b = l.querySelector(".lane-name"); return b && b.getAttribute("data-session") === s; });
          const seg = lane && lane.querySelectorAll(".seg")[n];
          if (!seg) return null;
          seg.scrollIntoView({ block: "center" });
          const r = seg.getBoundingClientRect();
          return { x: r.left + Math.min(r.width / 2, 6), y: r.top + r.height / 2 };
        }, [session, nth || 0]);
        if (!box) return false;
        if (where === "phone") await page.touchscreen.tap(box.x, box.y);
        else await page.mouse.click(box.x, box.y);
        await sleep(80);
        return true;
      };

      await tap('.tab[data-tab="log"]');
      await sleep(60);

      // ---- 1. A click on a lane lands on its row ----
      let now = await page.evaluate(() => Date.now());
      await page.evaluate((es) => GM.renderLog({ entries: es }), deep(now));
      await sleep(80);
      check(W("the far-back row is not drawn at first"), !(await page.$('.lrow[data-id="5000"]')));
      check(W("a segment past the drawn rows: its row drawn and flashed"),
        (await tapSeg(A)) && (await flashed(5000)), await ids().then((x) => x.length));

      await tap('[data-act="screens-only"]');
      await sleep(60);
      check(W("Screens on: the list holds no ordinary line"), (await ids()).length === 0, (await ids()).length);
      await tapSeg(A);
      check(W("Screens on, an ordinary segment: Screens set aside, its row flashed"),
        (await flashed(5000)) && !(await on("screens-only"))[0], await on("screens-only"));

      await page.fill("#log-filter", "nothing like it");
      await sleep(60);
      await tapSeg(A);
      check(W("a filter it fails: cleared, its row flashed"),
        (await flashed(5000)) && (await page.$eval("#log-filter", (i) => i.value)) === "");

      await tapSeg(B);
      check(W("a segment begun before the range: widened to the least that holds it, its row flashed"),
        (await flashed(5001)) && JSON.stringify(await rangeOn()) === '["7d"]', await rangeOn());

      // ---- 2. Issues ----
      await tap('[data-act="range"][data-range="' + 24 * 3600e3 + '"]');
      await sleep(40);
      now = await page.evaluate(() => Date.now());
      await page.evaluate((es) => GM.renderLog({ entries: es }), afternoon(now));
      await sleep(80);
      check(W("Issues sits beside Screens; Blocked only is gone"),
        !!(await page.$('.log-tools [data-act="issues-only"]')) && !(await page.$('[data-act="blocked-only"]')));
      check(W("the Issues chip counts the range's issues"),
        (await page.$eval('[data-act="issues-only"] .count', (c) => c.textContent).catch(() => "")) === "3");
      await tap('[data-act="issues-only"]');
      await sleep(60);
      check(W("Issues keeps only the entries marked worth a look"),
        JSON.stringify((await ids()).sort((a, b) => a - b)) === "[3,5,7]", await ids());
      await tap('[data-act="screens-only"]');
      await sleep(60);
      check(W("Issues and Screens: the issues with a screen"),
        JSON.stringify((await ids()).sort((a, b) => a - b)) === "[3,7]", await ids());
      await tap('[data-act="screens-only"]');
      await sleep(60);

      // ---- 3. Zoom to a moment ----
      await tap('.lrow[data-id="5"] [data-act="log-zoom"]');
      await sleep(80);
      const chip = await page.$('.log-tools [data-act="log-unzoom"]');
      check(W("an entry's time zooms: a chip names the half hour"),
        !!chip && /\d\d:\d\d–\d\d:\d\d/.test(await chip.textContent()), chip && (await chip.textContent()));
      check(W("zoomed, no range chip is on"), (await rangeOn()).length === 0, await rangeOn());
      check(W("the list is the half hour, all of it: Issues set aside"),
        JSON.stringify((await ids()).sort((a, b) => a - b)) === "[2,3,4,5,6,9]" && !(await on("issues-only"))[0], await ids());
      check(W("its row is marked and flashed"), !!(await page.$('.lrow[data-id="5"].moment')) && (await flashed(5)));
      const lanes = await page.evaluate(() => ({
        moments: document.querySelectorAll("#lanes .lane-bar .moment").length,
        lanes: document.querySelectorAll("#lanes .lane:not(.axis)").length,
        axis: Array.from(document.querySelectorAll("#lanes .axis .lane-bar span")).map((s) => s.textContent),
        blocked: !!document.querySelector('#lanes .seg.st-blocked'),
      }));
      check(W("the lanes are the half hour: the moment down each, no \"now\" at its end, B's block in it"),
        lanes.lanes > 0 && lanes.moments === lanes.lanes && !lanes.axis.includes("now") && lanes.blocked, lanes);
      // From the lanes: A's working began before the half hour -- the
      // moment moves to where it began.
      const before = await chip.textContent();
      await tapSeg(A);
      const moved = await page.$eval('[data-act="log-unzoom"]', (c) => c.textContent).catch(() => "");
      check(W("zoomed, a segment begun before it: the moment moves to it, its row flashed"),
        (await flashed(1)) && moved && moved !== before, [before, moved]);

      // Leave: Issues back as it was, the moment's row found again.
      await tap('[data-act="log-unzoom"]');
      await sleep(80);
      check(W("the chip leaves: the range back, Issues back on"),
        !(await page.$('[data-act="log-unzoom"]')) && JSON.stringify(await rangeOn()) === '["24h"]' &&
        (await on("issues-only"))[0] && JSON.stringify((await ids()).sort((a, b) => a - b)) === "[3,5,7]", await ids());

      if (where === "desk") {
        await tap('.lrow[data-id="7"] [data-act="log-zoom"]');
        await sleep(60);
        await page.keyboard.press("Escape");
        await sleep(60);
        check(W("Esc leaves a zoom"), !(await page.$('[data-act="log-unzoom"]')) && (await on("issues-only"))[0]);
      }
      await tap('[data-act="issues-only"]');  // off
      await sleep(40);

      // Picking a range leaves the zoom for that range.
      await tap('.lrow[data-id="3"] [data-act="log-zoom"]');
      await sleep(60);
      await tap('[data-act="range"][data-range="' + 6 * 3600e3 + '"]');
      await sleep(60);
      check(W("a range picked while zoomed: out of it, that range on"),
        !(await page.$('[data-act="log-unzoom"]')) && JSON.stringify(await rangeOn()) === '["6h"]', await rangeOn());

      // The minute's redraw keeps a moment's window: zoomed on A's start of
      // work five minutes ago, its open segment grows as time goes on, the
      // window's edges stay where they were.
      await tap('.lrow[data-id="10"] [data-act="log-zoom"]');
      await sleep(60);
      const look = () => page.evaluate(() => {
        const seg = document.querySelector('#lanes .seg[data-id="10"]');
        return { w: seg ? parseFloat(seg.style.width) : -1,
                 axis: Array.from(document.querySelectorAll("#lanes .axis .lane-bar span")).map((s) => s.textContent),
                 chip: (document.querySelector('[data-act="log-unzoom"]') || {}).textContent };
      });
      const was = await look();
      await page.clock.fastForward(5 * MIN);
      await sleep(80);
      const is = await look();
      check(W("five redraws on: still zoomed, the same window, the open segment longer"),
        is.chip && is.chip === was.chip && JSON.stringify(is.axis) === JSON.stringify(was.axis) &&
        is.w > was.w + 10, { was, is });
      check(W("a recent moment's window goes past now: no \"now\" at its end"), !was.axis.includes("now"), was.axis);

      if (where === "phone") {
        const sizes = await page.evaluate(() => ({
          chip: document.querySelector('.log-tools [data-act="log-unzoom"]').getBoundingClientRect(),
          time: document.querySelector('.lrow [data-act="log-zoom"]').getBoundingClientRect(),
        }));
        check(W("the zoom chip is on screen, a finger tall"),
          sizes.chip.left >= 0 && sizes.chip.right <= 390 && sizes.chip.height >= 32, sizes.chip);
        check(W("a row's time is a finger's target"), sizes.time.height >= 28, sizes.time);
      }
      await tap('[data-act="log-unzoom"]');
      await sleep(40);

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
