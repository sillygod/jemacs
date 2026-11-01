// A page taken by another device, used here again (ask c3733d70): the
// herd page on the phone, taken by the one at the desk, says so with a
// button, Use it here.  Pressed, it is the app's again -- the desk's
// told it is taken, and offered the same -- and goes on where it is: a
// draft in an agent's prompt, its "more" open, scrolled, nothing loaded
// again; the desk takes it back the same way.  One loaded before its
// files changed loads again instead.  The herd page, served by a real
// Emacs (wake.el) through a proxy that plays `tailscale serve'; the
// desk's browser reaches Emacs straight.
const fs = require("fs");
const path = require("path");
const { start, watch, phoneEyes, until, sleep, BOX, PHONE } = require("./appslib");
const { pageOf, NAME, copyTree, herdFile, shows, text } = require("./wakelib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const ROW = '.arow[data-name="' + NAME + '"]';
const notice = (page) => page.evaluate(() => {
  const n = document.getElementById("xw-notice");
  return n ? { text: n.textContent, act: (n.querySelector(".xw-notice-act") || {}).textContent || null } : null;
});
const takenSaid = (page) => async () => {
  const n = await notice(page);
  return !!n && /open elsewhere/.test(n.text) && n.act === "Use it here";
};
// What the phone's page is, as you left it.
const state = (tab) => tab.evaluate((row) => ({
  kept: window.KEPT,
  draft: (document.querySelector(row + " .agent-text") || {}).value,
  more: (document.querySelector(row + ' [data-act="a:more"]') || {}).textContent,
  top: document.getElementById("app").scrollTop,
}), ROW);

(async () => {
  const s = await start({ el: "wake.el", desk: true, setup: (run) => { copyTree(run); herdFile(run, "first"); } });
  try {
    const ctx = await s.browser.newContext(PHONE);
    await phoneEyes(ctx);
    const errors = [];
    const tab = await ctx.newPage();
    watch(tab, errors);
    await tab.goto(BOX + "/open/ghostherd#agents");
    check("the herd page opens on the phone", await until(shows(tab, "first"), 10000) &&
      (await until(() => /^"[0-9a-f]+"$/.test(pageOf(s.dir)), 5000)));
    const phone = pageOf(s.dir);
    // A draft in an agent's prompt, its "more" open, the page scrolled.
    await tab.tap(ROW + ' [data-act="a:compose"]');
    await tab.fill(ROW + " .agent-text", "a draft, not sent yet");
    await tab.tap(ROW + ' [data-act="a:more"]');
    await tab.evaluate(() => {
      window.KEPT = "this page's own";
      document.getElementById("app").scrollTop = 700;
    });
    const was = await state(tab);
    check("its state: a draft, more open, scrolled", was.draft === "a draft, not sent yet" && was.more === "less" && was.top > 100, was);

    // ---- The desk takes it ----
    const desk = await s.browser.newContext({ viewport: { width: 1280, height: 860 } });
    const dpage = await desk.newPage();
    const derr = [];
    watch(dpage, derr);
    await dpage.goto(s.urls.herd + "#agents");
    await until(shows(dpage, "first"), 10000);
    // page.txt is written every 0.1 s: wait for it to say the desk's.
    await until(() => pageOf(s.dir) !== phone && pageOf(s.dir) !== "nil", 5000);
    const deskPage = pageOf(s.dir);
    check("the desk takes it: the phone says so, with Use it here", deskPage !== phone && deskPage !== "nil" &&
      (await until(takenSaid(tab), 5000)), await notice(tab));

    // ---- Use it here, on the phone ----
    herdFile(s.dir, "phone again");
    const from = s.traffic.length;
    await tab.tap("#xw-notice .xw-notice-act");
    check("pressed: the phone's is the app's again", await until(() => pageOf(s.dir) === phone, 5000), pageOf(s.dir));
    check("and the desk is told it is taken, with Use it here", await until(takenSaid(dpage), 5000), await notice(dpage));
    check("the phone goes on, and shows what Emacs has now", await until(shows(tab, "phone again"), 5000) &&
      !(await notice(tab)), await notice(tab));
    const now = await state(tab);
    check("where it was: its draft, more open, its scroll, the page itself", now.kept === "this page's own" &&
      now.draft === was.draft && now.more === "less" && Math.abs(now.top - was.top) <= 2, { was, now });
    const files = s.traffic.slice(from).filter((t) => /\/ui\//.test(t.path));
    const hello = s.traffic.slice(from).find((t) => /\/hello\?/.test(t.path));
    check("nothing loaded again; a hello that takes it, not a soft one", files.length === 0 && hello &&
      hello.status === 204 && !/soft=1/.test(hello.path), { files: files.map((t) => t.path), hello: hello && hello.path });

    // ---- The desk takes it back, and the phone again: by hand, each ----
    herdFile(s.dir, "desk again");
    await dpage.click("#xw-notice .xw-notice-act");
    check("the desk's Use it here: the desk's again, and the phone told", await until(() => pageOf(s.dir) === deskPage, 5000) &&
      (await until(takenSaid(tab), 5000)) && (await until(shows(dpage, "desk again"), 5000)), await notice(tab));
    const quiet = s.traffic.length;
    await sleep(2500);
    check("and neither takes it from the other on its own", pageOf(s.dir) === deskPage &&
      !s.traffic.slice(quiet).some((t) => /\/hello\?/.test(t.path)), s.traffic.slice(quiet).map((t) => t.path));
    herdFile(s.dir, "phone once more");
    await tab.tap("#xw-notice .xw-notice-act");
    check("the phone's again, still where it was", (await until(() => pageOf(s.dir) === phone, 5000)) &&
      (await until(shows(tab, "phone once more"), 5000)) && (await state(tab)).draft === was.draft &&
      (await state(tab)).kept === "this page's own");
    check("one next asking at most, all along", (await tab.evaluate(() => window.__nexts.most)) === 1,
      await tab.evaluate(() => window.__nexts.most));

    // ---- Taken, and its files changed meanwhile: it loads again ----
    await dpage.click("#xw-notice .xw-notice-act");
    await until(takenSaid(tab), 5000);
    const appJs = path.join(s.dir, "tree", "ghostherd", "ui", "app.js");
    fs.appendFileSync(appJs, "\n// changed while the desk had it\n");
    fs.utimesSync(appJs, new Date(), new Date(Date.now() + 5000));
    herdFile(s.dir, "changed");
    await tab.tap("#xw-notice .xw-notice-act");
    const again = await until(async () => (await tab.evaluate(() => window.KEPT).catch(() => "x")) === undefined &&
      (await shows(tab, "changed")()), 15000);
    check("files changed meanwhile: Use it here loads it again, and it is the app's", again &&
      pageOf(s.dir) !== deskPage && pageOf(s.dir) !== "nil" && (await until(takenSaid(dpage), 5000)), pageOf(s.dir));
    check("phone: no page errors", errors.length === 0, errors);
    check("desk: no page errors", derr.length === 0, derr);
    await desk.close();
    await ctx.close();
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
