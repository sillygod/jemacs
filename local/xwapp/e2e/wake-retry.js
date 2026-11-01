// A phone that wakes asks again at once (ask 804e93bd).  It sleeps as a
// phone does: what it had under way hangs -- its next held, no answer
// reaching it.  It wakes two ways: told it is shown again
// (visibilitychange) while that next still hangs, as a dead connection
// does; or that next failing first, and told after.  Either way the
// page asks again at once, says nothing of Emacs not answering, and has
// one next asking at most.  Emacs truly gone, it
// still backs off, and says so once a second try has failed.  Taken by
// another device meanwhile, it stops.  The herd page, served by a real
// Emacs (wake.el) through a proxy that plays `tailscale serve' and
// sleeps (appslib.js); the page's eyes, phoneEyes.
//
// Each wake prints its MEASURE line; with LOCAL another checkout's, the
// same of it (its checks may fail: a kit before this one waits 1 s).
const path = require("path");
const fs = require("fs");
const { start, watch, phoneEyes, until, sleep, log, BOX, PHONE } = require("./appslib");
const { measure, pageOf, copyTree, herdFile, shows, settled } = require("./wakelib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const said = (tab) => tab.evaluate(() => window.__said.slice());
const nexts = (tab) => tab.evaluate(() => ({ now: window.__nexts.now, most: window.__nexts.most, at: window.__nexts.at.slice() }));

(async () => {
  const s = await start({ el: "wake.el", desk: true, setup: (run) => { copyTree(run); herdFile(run, "first"); } });
  try {
    const ctx = await s.browser.newContext(PHONE);
    await phoneEyes(ctx);
    const errors = [];
    const tab = await ctx.newPage();
    watch(tab, errors);
    await tab.goto(BOX + "/open/ghostherd#agents");
    check("the herd page opens on the phone", await until(shows(tab, "first"), 10000));
    await tab.evaluate(() => { window.KEPT = "this page's own"; });

    // ---- Asleep a moment: the next it had asks again at once ----
    await settled(tab);
    s.nap();
    await tab.evaluate(() => __vis("hidden"));
    await sleep(1300);
    const n0 = (await nexts(tab)).at.length, t0 = Date.now();
    await tab.evaluate(() => __vis("visible"));
    await sleep(30);
    s.wake("hang");
    check("woken: the next it had, hanging, is asked again at once",
      await until(async () => (await nexts(tab)).at.length > n0, 1000) && Date.now() - t0 < 400, Date.now() - t0);
    await sleep(500);
    check("and nothing said of Emacs not answering", (await said(tab)).length === 0, await said(tab));
    check("still the app's, and the same page", pageOf(s.dir) !== "nil" && (await tab.evaluate(() => window.KEPT)) === "this page's own");

    // ---- Asleep past the gone, two ways of waking ----
    for (const order of ["told first", "failed first"]) {
      await settled(tab);
      await settled(tab);
    s.nap();
      await tab.evaluate(() => __vis("hidden"));
      check(order + ": asleep, the page is let go", await until(() => pageOf(s.dir) === "nil", 10000));
      const marker = "awake " + order;
      herdFile(s.dir, marker);
      const from = s.traffic.length, t1 = Date.now();
      if (order === "told first") {
        // Its next hangs, dead: only being told can wake it.
        await tab.evaluate(() => __vis("visible"));
        await sleep(30);
        s.wake("hang");
      } else {
        // Its next fails first, and the page is told after.
        s.wake("fail");
        await sleep(30);
        await tab.evaluate(() => __vis("visible"));
      }
      const back = await until(shows(tab, marker), 15000);
      const line = measure("herd page, wake (" + order + ")", t1, s.traffic.slice(from));
      check(order + ": it comes back in place at once: " + line, back && Date.now() - t1 < 900 &&
        (await tab.evaluate(() => window.KEPT)) === "this page's own");
      check(order + ": nothing said of Emacs not answering", (await said(tab)).length === 0, await said(tab));
    }
    check("one next asking at most, all along", (await nexts(tab)).most === 1, await nexts(tab));

    // ---- Emacs gone: it backs off, and says so once a retry failed ----
    s.down(true);
    s.cut();
    const tg = Date.now(), fromOut = s.traffic.length;
    const sayAt = await (async () => {
      const end = Date.now() + 5000;
      while (Date.now() < end) {
        if ((await said(tab)).some((t) => /not answering/.test(t))) return Date.now() - tg;
        await sleep(25);
      }
      return null;
    })();
    await sleep(Math.max(0, 7500 - (Date.now() - tg)));
    const tries = s.traffic.slice(fromOut).filter((t) => t.status === 502 && /\/(next|hello)\?/.test(t.path)).map((t) => t.at - tg);
    const gaps = tries.slice(1).map((t, i) => t - tries[i]);
    check("Emacs gone: not answering said once a second try failed too, not at the first",
      sayAt !== null && sayAt >= 800, { sayAt, tries });
    check("and it backs off: 1 s, then 2 s, then 4 s -- no tight loop",
      tries.length >= 3 && tries.length <= 5 && gaps.length >= 2 && gaps[0] >= 800 && gaps[1] >= 1800, { tries, gaps });
    // Woken during it: one try at once, not a burst.
    await tab.evaluate(() => __vis("hidden"));
    await sleep(1100);
    const tw = Date.now(), fromW = s.traffic.length;
    await tab.evaluate(() => __vis("visible"));
    await sleep(700);
    // A try is its next (or hello), and whatever intent waits goes with it.
    const burst = s.traffic.slice(fromW).filter((t) => t.status === 502 && /\/(next|hello)\?/.test(t.path)).map((t) => t.at - tw);
    check("woken while Emacs is gone: one try at once, then the backoff again", burst.length === 1 && burst[0] < 200,
      s.traffic.slice(fromW).map((t) => [t.path.replace(/\?.*/, ""), t.status, t.at - tw]));
    // Back.  The wake kept the backoff (ask c3733d70): its next try is
    // up to 10 s off, Emacs gone for a while.
    s.down(false);
    const tb = Date.now();
    check("Emacs back: the page goes on, the notice gone, by its next try (10 s at most)", await until(async () =>
      !(await tab.$("#xw-notice")) && pageOf(s.dir) !== "nil", 11000), await said(tab));
    check("and not before it: the wake kept the backoff", Date.now() - tb > 3000, Date.now() - tb);
    check("one next asking at most, through it all", (await nexts(tab)).most === 1, await nexts(tab));

    // ---- Not answering said, the network back before the page wakes:
    // woken, it is unsaid at once, not on a try that waits (ask c3733d70) ----
    await settled(tab);
    s.down(true);
    s.cut();
    const tn0 = Date.now(), fromN = s.traffic.length;
    // Said, and a third try failed: the one after waits 4 s.
    await until(async () => (await said(tab)).filter((t) => /not answering/.test(t)).length >= 2 &&
      s.traffic.slice(fromN).filter((t) => t.status === 502 && /\/(next|hello)\?/.test(t.path)).length >= 3, 8000);
    const shownN = !!(await tab.$("#xw-notice"));
    await tab.evaluate(() => __vis("hidden"));
    s.down(false);
    await sleep(1100);
    const tn = Date.now();
    await tab.evaluate(() => __vis("visible"));
    const unsaid = await until(async () => !(await tab.$("#xw-notice")), 3000);
    check("not answering said, the network back, then woken: unsaid at once", shownN && unsaid && Date.now() - tn < 900,
      { shownN, ms: Date.now() - tn, since: tn - tn0 });
    check("and it goes on", await until(async () => pageOf(s.dir) !== "nil", 3000));

    // ---- Taken by another device while it slept: it stops ----
    await settled(tab);
    s.nap();
    await tab.evaluate(() => __vis("hidden"));
    await until(() => pageOf(s.dir) === "nil", 10000);
    const desk = await s.browser.newContext({ viewport: { width: 1280, height: 860 } });
    const dpage = await desk.newPage();
    await dpage.goto(s.urls.herd + "#agents");
    await until(() => pageOf(s.dir) !== "nil", 5000);
    const taken = pageOf(s.dir);
    await tab.evaluate(() => __vis("visible"));
    s.wake("hang");
    check("taken meanwhile: it says so at once, and stays as it is",
      await until(async () => (await said(tab)).some((t) => /open elsewhere/.test(t)), 3000) &&
      (await tab.evaluate(() => window.KEPT)) === "this page's own", await said(tab));
    const quiet = s.traffic.length;
    await tab.evaluate(() => { __vis("hidden"); __vis("visible"); });
    await sleep(1500);
    check("and asks no more, woken again or not; the other device keeps the app",
      s.traffic.length === quiet && pageOf(s.dir) === taken, s.traffic.slice(quiet).map((t) => t.path));
    await desk.close();
    // The browser says each 502 of Emacs gone: those are not the page's.
    const own = errors.filter((e) => !/status of 502/.test(e));
    check("no page errors", own.length === 0, own);
    await ctx.close();
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
