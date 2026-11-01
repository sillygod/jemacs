// The kit's half of a page in a web browser (ask c3733d70), against an
// Emacs played here, request by request, so each race is run as it
// goes and not as it may: an intent under way as the page comes back,
// a 410 while a try waits, "not answering" said and the network back,
// a page taken and used here again.  The Emacs (fakeEmacs) answers as
// xwapp.el does -- hello, a soft one only when free; next held; an
// intent of a page not the app's refused 410, with why -- and the test
// holds, fails or lets go what it says.  Playwright's route plays it,
// which costs the browser its cache: nothing here needs one.
//
// With LOCAL another checkout's local/, its kit, the same checks (a kit
// before this one fails those of what it did wrong).
const fs = require("fs");
const path = require("path");
const { webkit } = require("playwright");
const { phoneEyes, until, sleep } = require("./appslib");
const LOCAL = process.env.LOCAL || path.resolve(__dirname, "../..");
const KIT = path.join(LOCAL, "xwapp", "ui");
const ORIGIN = "http://box.test";
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

// A page of the app's: a draft, a page to scroll, and its BACK (#noback:
// none) -- asking Emacs again for what it shows, "ready", as its load did.
const HTML = `<!doctype html><html><head><meta charset="utf-8"><title>T</title>
<link rel="stylesheet" href="xwapp/ui/base.css"></head><body>
<textarea id="draft"></textarea><div style="height:4000px"></div>
<script src="xwapp/ui/xwapp.js"></script>
<script>
window.BACKS = 0;
window.LOST = [];
const back = location.hash === "#noback" ? {} : { back: () => { BACKS++; emit("ready"); } };
const emit = XW.bridge("t:", null, back);
window.say = (op, extra) => emit(op, extra, () => { LOST.push(op); });
emit("ready");
</script></body></html>`;

// Emacs, played.  PAGE: the app's page, or null; WHY: what a page not
// the app's is told when none is; DOWN: every request answered 502, as
// serve does with Emacs gone; HOLD: intents held until release(); LOG:
// each request as it came; TOOK: each intent Emacs took.
function fakeEmacs() {
  const E = { page: null, why: "free", ui: "ui-1", down: false, hold: false, failIntents: false,
              held: [], waiting: null, log: [], took: [] };
  const answer = (route, res) => route.fulfill(res).catch(() => {});
  const refuse = (route, why) => answer(route, { status: 410, contentType: "application/json",
    body: JSON.stringify(why === "free" ? { why, ui: E.ui } : { why }) });
  const whyNot = () => (E.page ? "taken" : E.why);
  // Its held next answered: 410 WHY, or STATUS.
  E.endNext = (why, status) => {
    const w = E.waiting;
    E.waiting = null;
    if (!w) return false;
    if (status) answer(w.route, { status, body: "" });
    else refuse(w.route, why);
    return true;
  };
  E.free = () => { E.page = null; E.why = "free"; E.endNext("free"); };
  E.takenBy = (other) => { E.page = other; E.endNext("taken"); };
  E.calls = (calls) => {
    const w = E.waiting;
    E.waiting = null;
    if (w) answer(w.route, { status: 200, contentType: "application/json", body: JSON.stringify(calls) });
  };
  // The intent held first: taken (204), or refused (410 WHY).
  E.release = (why) => {
    const h = E.held.shift();
    if (!why) {
      E.took.push(h.intent);
      return answer(h.route, { status: 204, body: "" });
    }
    return refuse(h.route, why);
  };
  E.handle = (route) => {
    const req = route.request();
    const u = new URL(req.url());
    const at = u.pathname.replace(/^\/S\//, "");
    if (at === "index.html") return answer(route, { status: 200, contentType: "text/html", body: HTML });
    if (at.startsWith("xwapp/ui/")) {
      const file = path.join(KIT, at.slice("xwapp/ui/".length));
      return answer(route, fs.existsSync(file)
        ? { status: 200, contentType: /\.css$/.test(file) ? "text/css" : "application/javascript", body: fs.readFileSync(file) }
        : { status: 404, body: "" });
    }
    const page = u.searchParams.get("page"), soft = u.searchParams.get("soft") === "1";
    let intent = null;
    if (at === "intent") try { intent = JSON.parse(req.postData()); } catch (_) {}
    E.log.push({ op: at, page, soft, intent, at: Date.now() });
    if (E.down) return answer(route, { status: 502, body: "" });
    if (at === "hello") {
      if (soft && (E.page || E.why !== "free")) return refuse(route, whyNot());
      if (E.page !== page) E.endNext("taken");
      E.page = page;
      return answer(route, { status: 204, headers: { "Xwapp-Ui": E.ui }, body: "" });
    }
    if (page !== E.page) return refuse(route, whyNot());
    if (at === "next") {
      if (E.waiting) E.calls([]);
      E.waiting = { route, page };
      return undefined;
    }
    if (at === "intent") {
      if (E.failIntents) return answer(route, { status: 502, body: "" });
      if (E.hold) {
        E.held.push({ route, page, intent });
        return undefined;
      }
      E.took.push(intent);
      return answer(route, { status: 204, body: "" });
    }
    return answer(route, { status: 404, body: "" });
  };
  return E;
}

async function open(browser, hash) {
  const E = fakeEmacs();
  const ctx = await browser.newContext({ viewport: { width: 390, height: 700 } });
  await phoneEyes(ctx);
  await ctx.route(ORIGIN + "/**", E.handle);
  const tab = await ctx.newPage();
  const errors = [];
  tab.on("pageerror", (e) => errors.push(String(e)));
  await tab.goto(ORIGIN + "/S/index.html" + (hash || ""));
  // Its load done with: the app's, its next held, its first intents taken.
  await until(() => E.waiting && E.took.some((i) => i.op === "ready") && E.took.some((i) => i.op === "xwapp-seen"), 5000);
  const me = E.page;
  return { E, ctx, tab, errors, me };
}
const said = (tab) => tab.evaluate(() => window.__said.slice());
const notice = (tab) => tab.evaluate(() => {
  const n = document.getElementById("xw-notice");
  return n ? { text: n.textContent, act: (n.querySelector(".xw-notice-act") || {}).textContent || null } : null;
});
const ops = (list, op) => list.filter((i) => i && i.op === op);
const sent = (E, op) => E.log.filter((l) => l.intent && l.intent.op === op).length;

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- An intent under way as the page comes back, then taken ----
    {
      const { E, ctx, tab, errors, me } = await open(browser);
      E.hold = true;
      await tab.evaluate(() => say("send", { text: "a message for an agent" }));
      await until(() => E.held.length === 1, 3000);
      // Let go while it is under way: the page comes back where it is.
      E.free();
      await until(() => E.log.some((l) => l.op === "hello" && l.soft) && E.page === me, 3000);
      E.hold = false;
      await sleep(100);
      // Emacs had it before it let go; its answer comes now.
      E.release();
      await sleep(400);
      check("an intent under way as the page came back, answered taken after: sent once, taken once",
        sent(E, "send") === 1 && ops(E.took, "send").length === 1, { sent: sent(E, "send"), took: E.took.map((i) => i.op) });
      check("and what came back asked went: BACK's once, whether you look", (await tab.evaluate(() => BACKS)) === 1 &&
        ops(E.took, "ready").length === 2 && ops(E.took, "xwapp-seen").length >= 2, E.took.map((i) => i.op));
      check("nothing said, nothing lost", (await said(tab)).length === 0 && (await tab.evaluate(() => LOST.length)) === 0 &&
        errors.length === 0, { said: await said(tab), errors });
      await ctx.close();
    }

    // ---- The same, answered 410 free after: it goes again, once ----
    {
      const { E, ctx, tab, errors, me } = await open(browser);
      E.hold = true;
      await tab.evaluate(() => say("send", { text: "a message for an agent" }));
      await until(() => E.held.length === 1, 3000);
      E.free();
      await until(() => E.log.some((l) => l.op === "hello" && l.soft) && E.page === me, 3000);
      E.hold = false;
      await sleep(100);
      // Emacs had let it go when it came: not taken.
      E.release("free");
      await sleep(400);
      const hellos = E.log.filter((l) => l.op === "hello").length;
      check("one answered 410 free after the page came back: no second coming back, nor a soft hello Emacs refuses",
        hellos === 2 && (await tab.evaluate(() => BACKS)) === 1 && E.page === me, { hellos, page: E.page });
      check("it goes again, and is taken once", sent(E, "send") === 2 && ops(E.took, "send").length === 1,
        { sent: sent(E, "send"), took: E.took.map((i) => i.op) });
      check("nothing said, nothing lost", (await said(tab)).length === 0 && (await tab.evaluate(() => LOST.length)) === 0 &&
        errors.length === 0, { said: await said(tab), errors });
      await ctx.close();
    }

    // ---- "Not answering" said, the network back, the page woken ----
    for (const how of ["shown again", "online"]) {
      const { E, ctx, tab } = await open(browser);
      E.down = true;
      E.endNext(null, 502);
      // Three tries failed: the next waits 4 s.
      await until(() => E.log.filter((l) => l.op === "next").length >= 3, 6000);
      await sleep(50);
      const up = await notice(tab);
      if (how === "shown again") await tab.evaluate(() => __vis("hidden"));
      E.down = false;
      if (how === "shown again") await sleep(1100);
      const t0 = Date.now();
      await tab.evaluate((h) => (h === "online" ? window.dispatchEvent(new Event("online")) : __vis("visible")), how);
      const gone = await until(async () => !(await notice(tab)), 2500);
      const ms = Date.now() - t0;
      check("not answering said, the network back, woken (" + how + "): it is unsaid at once",
        up && /not answering/.test(up.text) && gone && ms < 600, { up, ms });
      check("and the page goes on (" + how + "): its next held again, an intent taken", await (async () => {
        await tab.evaluate(() => say("after"));
        return until(() => ops(E.took, "after").length === 1 && E.waiting, 2000);
      })());
      await ctx.close();
    }

    // ---- Woken while Emacs is still gone: the backoff stays ----
    {
      const { E, ctx, tab } = await open(browser);
      E.down = true;
      E.endNext(null, 502);
      // Two tries failed: the next waits 2 s.
      await until(() => E.log.filter((l) => l.op === "next").length >= 2, 4000);
      await sleep(300);
      const n = E.log.filter((l) => l.op === "next").length;
      await tab.evaluate(() => window.dispatchEvent(new Event("online")));
      await until(() => E.log.filter((l) => l.op === "next").length > n, 1000);
      const woke = E.log.filter((l) => l.op === "next").pop().at;
      await until(() => E.log.filter((l) => l.op === "next").length > n + 1, 6000);
      const after = E.log.filter((l) => l.op === "next").pop().at - woke;
      check("woken while Emacs is gone: one try at once, and the one after it waits longer still (4 s), not 1 s",
        after >= 3500, { after });
      E.down = false;
      // Its next try 8 s after the last: Emacs gone a while, it asks no more often.
      check("Emacs back: unsaid by its next try", await until(async () => !(await notice(tab)), 10000));
      await ctx.close();
    }

    // ---- A 410 while a try waits: said at once ----
    for (const what of ["taken", "unknown"]) {
      const { E, ctx, tab } = await open(browser);
      E.failIntents = true;
      await tab.evaluate(() => say("fails"));
      await until(() => E.log.some((l) => l.intent && l.intent.op === "fails"), 2000);
      await sleep(50);
      const t0 = Date.now();
      if (what === "taken") E.takenBy("another");
      else { E.page = null; E.waiting && E.waiting.route.fulfill({ status: 404, body: "" }).catch(() => {}); E.waiting = null; }
      const shown = await until(async () => {
        const n = await notice(tab);
        return n && (what === "taken" ? /open elsewhere/ : /does not know this page/).test(n.text);
      }, 2000);
      const ms = Date.now() - t0;
      const quiet = E.log.length;
      await sleep(1500);
      check("a " + (what === "taken" ? "410 taken" : "404") + " while a try waits: said at once, not after it, and nothing asked after",
        shown && ms < 400 && E.log.length === quiet, { ms, after: E.log.slice(quiet).map((l) => l.op) });
      await ctx.close();
    }

    // ---- Taken: Use it here ----
    {
      const { E, ctx, tab, errors, me } = await open(browser);
      await tab.fill("#draft", "a draft not sent");
      await tab.evaluate(() => { window.KEPT = "this page's own"; window.scrollTo(0, 1500); });
      // An intent under way as it is taken: it is lost, and its answer,
      // when it comes, says nothing of the page as it is.
      E.hold = true;
      await tab.evaluate(() => say("under way"));
      await until(() => E.held.length === 1, 2000);
      E.takenBy("another");
      const taken = await until(async () => {
        const n = await notice(tab);
        return n && /open elsewhere/.test(n.text) && n.act === "Use it here";
      }, 2000);
      check("taken: it says so, with Use it here; what was under way, lost", taken &&
        (await tab.evaluate(() => LOST)).includes("under way"), await notice(tab));
      E.hold = false;
      await tab.click("#xw-notice .xw-notice-act");
      const back = await until(() => E.page === me, 2000);
      const hello = E.log.filter((l) => l.op === "hello").pop();
      check("Use it here: a hello that takes it, from where it is", back && hello && !hello.soft && hello.page === me, hello);
      await until(() => E.waiting, 2000);
      E.release("taken");
      await sleep(300);
      const st = await tab.evaluate(() => ({ kept: window.KEPT, draft: document.getElementById("draft").value,
        y: window.scrollY, backs: BACKS }));
      check("and it goes on where it is: its draft, its scroll, the page itself, BACK asked once",
        st.kept === "this page's own" && st.draft === "a draft not sent" && Math.abs(st.y - 1500) <= 2 && st.backs === 1, st);
      check("the answer to what was under way, come after, says nothing of the page as it is",
        !(await notice(tab)), await notice(tab));
      await tab.evaluate(() => say("later"));
      check("it is the app's: its next held, an intent taken", await until(() => ops(E.took, "later").length === 1 &&
        E.waiting && E.waiting.page === me, 2000));
      // Taken again, and used here again.
      E.takenBy("another");
      await until(async () => (await notice(tab)) !== null, 2000);
      await tab.click("#xw-notice .xw-notice-act");
      check("taken again, and used here again", await until(() => E.page === me && E.waiting && !E.waiting.done, 2000) &&
        (await tab.evaluate(() => BACKS)) === 2 && !(await notice(tab)));
      check("no page errors", errors.length === 0, errors);
      await ctx.close();
    }

    // ---- Use it here, loaded before its files changed: it loads again ----
    for (const how of ["files changed", "no back"]) {
      const { E, ctx, tab, me } = await open(browser, how === "no back" ? "#noback" : "");
      await tab.evaluate(() => { window.KEPT = "this page's own"; });
      E.takenBy("another");
      await until(async () => (await notice(tab)) !== null, 2000);
      check("taken (" + how + "): Use it here offered", ((await notice(tab)) || {}).act === "Use it here");
      if (how === "files changed") E.ui = "ui-2";
      await tab.click("#xw-notice .xw-notice-act");
      const again = await until(async () => (await tab.evaluate(() => window.KEPT).catch(() => "")) === undefined &&
        E.page && E.page !== me && E.waiting && E.waiting.page === E.page, 5000);
      const last = E.log.filter((l) => l.op === "hello").pop();
      check("Use it here (" + how + "): it loads again, and takes it with a hello of its own, not soft",
        again && !last.soft && last.page === E.page, { page: E.page, last });
      await ctx.close();
    }

    // ---- Closed: no button ----
    {
      const { E, ctx, tab } = await open(browser);
      E.page = null;
      E.why = "closed";
      E.endNext("closed");
      await until(async () => (await notice(tab)) !== null, 2000);
      const n = await notice(tab);
      check("closed: said, with nothing to press", n && /closed this page/.test(n.text) && n.act === null, n);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
