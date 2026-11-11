// The herd page over a phone's slow link (ask 804e93bd): its first load,
// a wake, and a wake after a ui/ file changed, each measured -- the
// bytes of the answers' bodies, and the time until the page is usable --
// through a proxy that plays `tailscale serve' and a link of 1.6 Mbit/s
// down and 150 ms there and back (appslib.js OPTS.link), the phone
// sleeping as a phone does.  The page's files and the kit's go gzip'd,
// and the page loads and works so, on the phone and at the desk; what
// changes, its nexts, too once past 8 KB, and plain within.  Real
// Emacs: wake.el.
//
// MEASURE lines; with LOCAL another checkout's, the same of it (its
// checks of gzip fail: a kit and an Emacs before this one send plain).
const fs = require("fs");
const path = require("path");
const { start, watch, phoneEyes, until, sleep, BOX, PHONE } = require("./appslib");
const { measure, pageOf, copyTree, herdFile, shows, settled } = require("./wakelib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const LINK = { kbps: 1600, rtt: 150 };
const name = (t) => t.path.replace("/SECRET/", "").replace(/\?.*/, "");
const files = (traffic) => traffic.filter((t) => /\/ui\//.test(t.path));

(async () => {
  const s = await start({ el: "wake.el", desk: true, link: LINK, setup: (run) => { copyTree(run); herdFile(run, "first"); } });
  try {
    // ---- First load, on the phone ----
    const ctx = await s.browser.newContext(PHONE);
    await phoneEyes(ctx);
    const errors = [];
    const tab = await ctx.newPage();
    watch(tab, errors);
    const t0 = Date.now();
    await tab.goto(BOX + "/open/ghostherd#agents");
    const loaded = await until(shows(tab, "first"), 20000);
    const first = s.traffic.slice();
    measure("herd page, first load, 1.6 Mbit/s 150 ms", t0, first);
    check("the herd page loads on the phone over the slow link", loaded);
    const big = files(first).filter((t) => /(app\.js|style\.css|xwapp\.js|index\.html|base\.css)$/.test(name(t)));
    check("its files and the kit's came gzip'd", big.length >= 5 && big.every((t) => t.status === 200 && t.enc === "gzip"),
      big.map((t) => name(t) + " " + t.status + " " + (t.enc || "plain") + " " + t.bytes));
    const app = first.find((t) => /ghostherd\/ui\/app\.js$/.test(name(t)));
    check("app.js gzip'd and lean, some 105 KB, not 495", app && app.bytes < 120000, app && app.bytes);
    check("and it works: its agents shown, its intents going", await tab.$$eval(".arow", (r) => r.length) > 30 &&
      (await tab.evaluate(() => typeof GM.renderHerd === "function")));
    // What changes, its nexts: gzip'd once past what the proxy's socket
    // takes at once (xwapp--gzip-next-min), plain within it.
    const nexts = first.filter((t) => /\/next\?/.test(t.path) && t.status === 200);
    check("what changes, its nexts: gzip'd past 8 KB, plain within",
      nexts.length > 0 && nexts.every((t) => t.enc === "gzip" || (!t.enc && t.bytes < 8 * 1024)),
      nexts.map((t) => (t.enc || "plain") + " " + t.bytes));
    console.log("     nexts of the first load: " + nexts.map((t) => (t.enc || "plain") + " " + t.bytes).join(", "));

    // ---- A wake, each way: told first, or what slept fails first ----
    for (const order of ["told first", "failed first"]) {
      await settled(tab);
      await settled(tab);
    s.nap();
      await tab.evaluate(() => __vis("hidden"));
      await until(() => pageOf(s.dir) === "nil", 10000);
      herdFile(s.dir, "awake " + order);
      const from = s.traffic.length, t1 = Date.now();
      if (order === "told first") {
        await tab.evaluate(() => __vis("visible"));
        await sleep(30);
        s.wake("hang");
      } else {
        s.wake("fail");
        await sleep(30);
        await tab.evaluate(() => __vis("visible"));
      }
      const woke = await until(shows(tab, "awake " + order), 15000);
      measure("herd page, wake (" + order + "), 1.6 Mbit/s 150 ms", t1, s.traffic.slice(from));
      check("a wake over the slow link (" + order + "): back in place, nothing said of Emacs", woke &&
        !(await tab.evaluate(() => window.__said.some((t) => /not answering/.test(t)))));
    }

    // ---- A wake after a ui/ file changed: it loads again ----
    await settled(tab);
    s.nap();
    await tab.evaluate(() => __vis("hidden"));
    await until(() => pageOf(s.dir) === "nil", 10000);
    const appJs = path.join(s.dir, "tree", "ghostherd", "ui", "app.js");
    fs.appendFileSync(appJs, "\n// changed while the phone slept\n");
    fs.utimesSync(appJs, new Date(), new Date(Date.now() + 5000));
    herdFile(s.dir, "changed");
    const from2 = s.traffic.length, t2 = Date.now();
    await tab.evaluate(() => __vis("visible"));
    await sleep(30);
    s.wake("hang");
    const again = await until(shows(tab, "changed"), 20000);
    const after2 = s.traffic.slice(from2);
    measure("herd page, wake after a ui/ file changed, 1.6 Mbit/s 150 ms", t2, after2);
    const changed = files(after2).find((t) => /ghostherd\/ui\/app\.js$/.test(name(t)));
    check("a ui/ file changed: it loads again, the file sent gzip'd, the rest not modified", again && changed &&
      changed.status === 200 && changed.enc === "gzip" &&
      files(after2).filter((t) => t !== changed && t.asked).every((t) => t.status === 304),
      files(after2).map((t) => name(t) + " " + t.status + " " + (t.enc || "plain")));
    // The browser says the 502 of a wake that failed first: not the page's.
    const own = errors.filter((e) => !/status of 502/.test(e));
    check("phone: no page errors", own.length === 0, own);
    await ctx.close();

    // ---- At the desk: gzip'd too, and it works ----
    const desk = await s.browser.newContext({ viewport: { width: 1280, height: 860 } });
    const dpage = await desk.newPage();
    const derr = [];
    watch(dpage, derr);
    const enc = {};
    dpage.on("response", (r) => { enc[r.url().replace(/^.*\/ui\//, "")] = r.headers()["content-encoding"] || "plain"; });
    await dpage.goto(s.urls.herd + "#agents");
    check("at the desk the page loads gzip'd too, and works", await until(shows(dpage, "changed"), 10000) &&
      enc["app.js"] === "gzip" && enc["xwapp.js"] === "gzip", enc);
    check("desk: no page errors", derr.length === 0, derr);
    await desk.close();
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
