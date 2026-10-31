// A phone that slept, and woke (ask fc2f4594).  Asleep past the gone
// (here 1.5 s, at your desk 10 s), the page is let go, and wakes to a 410
// free.  The herd page comes back where it is: no file loaded again, its
// scroll and what it held kept, and it asks Emacs again for what it
// shows.  A page that does not -- pr-view -- loads again as before, its
// files answered 304 now, not sent again.  A page taken by another device
// meanwhile still stops; and one whose ui/ changed meanwhile loads again,
// to have it.  Through a proxy that plays `tailscale serve', and sleeps:
// what the phone had under way is cut, what it asks waits until it
// wakes.  Real Emacs: wake.el (the herd page) and apps.el (pr-view).
//
// Each wake prints what it cost -- the bytes of the answers' bodies
// from the wake until the page is usable, and how long that took --
// as MEASURE lines.  Run with LOCAL set to another checkout's local/
// (its xwapp, ghostherd/ui and pr-view), the same measures, of it.
const fs = require("fs");
const path = require("path");
const { start, watch, until, sleep, log, BOX, PHONE } = require("./appslib");
const { measure, text, fresh, pageOf, NAME, copyTree, herdFile, shows } = require("./wakelib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
(async () => {
  // ---- The herd page: it comes back where it is ----
  {
    const s = await start({ el: "wake.el", desk: true, setup: (run) => { copyTree(run); herdFile(run, "first"); } });
    const intents = (op) => log().filter((w) => w && w.intent && (!op || w.intent.op === op)).map((w) => w.intent);
    try {
      const ctx = await s.browser.newContext(PHONE);
      const errors = [];
      const tab = await ctx.newPage();
      watch(tab, errors);
      await tab.goto(BOX + "/open/ghostherd#agents");
      check("the herd page opens on the phone", await until(shows(tab, "first"), 10000));
      await tab.evaluate(() => {
        window.KEPT = "this page's own";
        document.getElementById("app").scrollTop = 900;
      });
      const scroll = await tab.evaluate(() => document.getElementById("app").scrollTop);

      // Asleep, past the gone.
      s.sleep();
      check("asleep, the page is let go, and the app closed", await until(() => pageOf(s.dir) === "nil" && log().includes("closed"), 10000),
        pageOf(s.dir));
      const before = intents().length;
      herdFile(s.dir, "awake");
      const from = s.traffic.length, t0 = Date.now();
      s.wake();
      const usable = await until(shows(tab, "awake"), 15000);
      const line = measure("herd page, wake", t0, s.traffic.slice(from));
      check("awake, it shows what Emacs has now: " + line, usable);
      const after = s.traffic.slice(from);
      const kept = await tab.evaluate(() => window.KEPT);
      check("it came back where it is: not loaded again", kept === "this page's own", kept);
      check("no file of it asked for again", !after.some((t) => /\/ui\//.test(t.path)), after.map((t) => t.path));
      const now = await tab.evaluate(() => document.getElementById("app").scrollTop);
      check("its scroll kept", scroll > 100 && Math.abs(now - scroll) <= 2, { scroll, now });
      check("and its view: still the agents, as its address says", /#agents$/.test(tab.url()) &&
        (await tab.evaluate(() => document.getElementById("app").getAttribute("data-view"))) === "agents");
      const again = log().slice(0).filter((w) => w && w.intent).slice(before).map((w) => w.intent.op);
      check("the app was opened again for it, and it asked again for what it shows",
        log().includes("reopened") && again.includes("ready") && again.includes("herd-watch") && again.includes("threads-peek"), again);
      check("a soft hello, the app's again", after.some((t) => /\/hello\?page=[0-9a-f]+&soft=1$/.test(t.path) && t.status === 204) &&
        pageOf(s.dir) !== "nil");
      check("no notice left", !(await tab.$("#xw-notice")));
      check("an intent goes on as before", await (async () => {
        const n = intents("herd-watch").length;
        await tab.evaluate(() => { GM.showView("log"); GM.showView("agents"); });
        return until(() => intents("herd-watch").length > n, 5000);
      })());

      // ---- Taken by another device while it slept: it stops ----
      s.sleep();
      await until(() => pageOf(s.dir) === "nil", 10000);
      const desk = await s.browser.newContext({ viewport: { width: 1280, height: 860 } });
      const dpage = await desk.newPage();
      await dpage.goto(s.urls.herd + "#agents");
      await until(() => pageOf(s.dir) !== "nil", 5000);
      const taken = pageOf(s.dir);
      s.wake();
      check("taken meanwhile, it says so, and stays as it is",
        await until(async () => /open elsewhere/.test(await text(tab, "#xw-notice")), 10000) &&
        (await tab.evaluate(() => window.KEPT)) === "this page's own", await text(tab, "#xw-notice"));
      const quiet = s.traffic.length;
      await sleep(2500);
      check("and asks no more; the other device keeps the app", s.traffic.length === quiet && pageOf(s.dir) === taken,
        s.traffic.slice(quiet).map((t) => t.path));
      await desk.close();

      // ---- A ui/ file changed while it slept: it loads again ----
      await tab.reload();
      await until(shows(tab, "awake"), 10000);
      await tab.evaluate(() => { window.KEPT = "this page's own"; });
      s.sleep();
      await until(() => pageOf(s.dir) === "nil", 10000);
      const appJs = path.join(s.dir, "tree", "ghostherd", "ui", "app.js");
      fs.appendFileSync(appJs, "\n// changed while the phone slept\n");
      fs.utimesSync(appJs, new Date(), new Date(Date.now() + 5000));
      herdFile(s.dir, "changed");
      const from2 = s.traffic.length, t2 = Date.now();
      s.wake();
      const back = await until(shows(tab, "changed"), 15000);
      const line2 = measure("herd page, wake after a ui/ file changed", t2, s.traffic.slice(from2));
      const after2 = s.traffic.slice(from2).filter((t) => /\/ui\//.test(t.path));
      check("a ui/ file changed meanwhile: it loads again, to have it: " + line2,
        back && (await tab.evaluate(() => window.KEPT)) === undefined);
      const changed = (t) => /\/ghostherd\/ui\/app\.js$/.test(t.path);
      check("the file changed sent whole, though the browser had it; each other it had, not modified",
        after2.some((t) => changed(t) && t.asked && t.status === 200) && fresh(after2.filter((t) => !changed(t))),
        after2.map((t) => t.path.replace("/SECRET/", "") + " " + t.status + (t.asked ? " asked" : "")));
      check("herd: no page errors", errors.length === 0, errors);
      await ctx.close();
    } finally {
      await s.stop();
    }
  }

  // ---- pr-view, which does not come back in place: it loads again ----
  {
    const s = await start();
    try {
      const ctx = await s.browser.newContext(PHONE);
      const errors = [];
      const tab = await ctx.newPage();
      watch(tab, errors);
      // No ctx.route here: Playwright turns the browser's cache off for it.
      await tab.goto(BOX + "/open/pr-view#repo=" + encodeURIComponent("bitbucket/unno/shop"));
      check("pr-view opens on the phone", await until(async () => (await tab.$$(".pr-row")).length === 3, 10000));
      await tab.evaluate(() => { window.KEPT = "this page's own"; });
      s.sleep();
      // Let go: held 2 s, then gone 1.5 s after.
      await sleep(4500);
      const from = s.traffic.length, t0 = Date.now();
      s.wake();
      const back = await until(async () => (await tab.evaluate(() => window.KEPT).catch(() => "")) === undefined &&
        (await tab.$$(".pr-row")).length === 3, 15000);
      const line = measure("pr-view, wake", t0, s.traffic.slice(from));
      check("it loads again, and shows its list: " + line, back);
      const files = s.traffic.slice(from).filter((t) => /\/ui\//.test(t.path));
      check("its files and the kit's not sent again: each the browser had, not modified", fresh(files),
        files.map((t) => t.path.replace("/SECRET/", "") + " " + t.status + (t.asked ? " asked" : "")));
      check("pr-view: no page errors", errors.length === 0, errors);
      await ctx.close();
    } finally {
      await s.stop();
    }
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
