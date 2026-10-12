// pr-view at what a checkout has not committed, on your phone: opened by
// Emacs's address that holds no secret, at the checkout the herd page
// links to; Emacs reads it with git, and the page shows it in a pull
// request's file view -- each file, a new one whole, why one is not
// shown.  Refresh reads it again, a reload comes back to it, Back is to
// its repository's pull requests; a checkout Emacs does not vouch for is
// refused.  It fits the phone.
const fs = require("fs");
const path = require("path");
const { start, watch, measure, until, sleep, log, BOX, PHONE } = require("./appslib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const intents = (op) => log().filter((w) => w && w.intent && w.app === "pr-view" && (!op || w.intent.op === op)).map((w) => w.intent);
const fits = (m, bar) => !m.pageScroll && !m.past.length && !m.small.length && !m.smallFonts.filter((f) => !/file-viewed|^input$/.test(f)).length && m.top <= bar;
const text = (page, sel) => page.textContent(sel).catch(() => "");
const hidden = (page, sel) => page.$eval(sel, (e) => e.hidden || getComputedStyle(e).display === "none").catch(() => true);
const files = (page) => page.$$eval(".tree-file", (bs) => bs.map((b) => b.getAttribute("data-path")).sort());

(async () => {
  const s = await start();
  try {
    const root = s.urls.local;
    const ctx = await s.browser.newContext(PHONE);
    await ctx.route(/^https:\/\//, (r) => r.fulfill({ status: 200, contentType: "text/plain", body: "away" }));
    const errors = [];
    const tab = await ctx.newPage();
    watch(tab, errors);

    await tab.goto(BOX + "/open/pr-view#local=" + encodeURIComponent(root));
    const shown = await until(async () => (await files(tab)).length > 0, 10000);
    check("the address opens pr-view at the checkout's uncommitted changes", shown &&
      /Uncommitted changes/.test(await text(tab, "h1")) && tab.url().endsWith("#local=" + encodeURIComponent(root)),
      tab.url().replace(s.secretOf("pr"), "SECRET"));
    check("which it asked Emacs for", intents("ready").some((i) => i.local === root));
    const chips = await tab.$$eval(".detail-head .chip", (cs) => cs.map((c) => c.textContent));
    check("its branch, how much, its last commit", chips[0] === "⎇ fix/payment-retry" &&
      chips[1] === "5 files +5 −2" && /^[0-9a-f]{7,} Pay an order$/.test(chips[2]), chips);
    check("every file: those changed, and those git does not track yet",
      JSON.stringify(await files(tab)) === JSON.stringify(["README.md", "icon.ico", "logo.png", "notes/retry.md", "src/pay.py"]), await files(tab));
    check("in the bar Refresh and Back -- the checkout is on Bitbucket; no Copy, Open or Create",
      !(await hidden(tab, "#btn-refresh")) && !(await hidden(tab, "#btn-back")) && (await hidden(tab, "#btn-copy")) &&
      (await hidden(tab, "#btn-browser")) && (await hidden(tab, "#btn-create")));

    await tab.tap('.tree-file[data-path="src/pay.py"]');
    await sleep(200);
    const diff = await tab.$$eval(".file-diff .diff-row", (rs) => rs.map((r) => r.className.replace("diff-row ", "") + " " + r.querySelector(".code").textContent));
    check("a file changed, line by line", diff.includes("del def pay(order):") && diff.includes("add def pay(order, retries=3):") &&
      diff.includes("ctx     total = order.total"), diff);
    await tab.tap('.tree-file[data-path="notes/retry.md"]');
    await sleep(200);
    check("a new one, whole", (await text(tab, ".file-diff")).includes("Retry a failed charge three times."));
    await tab.tap('.tree-file[data-path="logo.png"]');
    await sleep(200);
    check("a new one not shown, and why", (await text(tab, ".file-diff")).trim() === "Not shown: binary.", await text(tab, ".file-diff"));
    await tab.tap('.tree-file[data-path="icon.ico"]');
    await sleep(200);
    check("a changed one git calls binary, the same", (await text(tab, ".file-diff")).trim() === "Not shown: binary.", await text(tab, ".file-diff"));
    const m = await measure(tab);
    check("it fits the phone", fits(m, 56), m);
    check("the checkout's long path wraps, whole", await tab.$eval(".local-root", (p) =>
      p.scrollWidth <= p.clientWidth + 1 && p.textContent.endsWith("/checkout/")));

    // Changed since: Refresh reads it again.
    fs.writeFileSync(path.join(root, "CHANGELOG.md"), "Retries.\n");
    await tab.tap("#btn-refresh");
    check("Refresh reads it again", await until(async () => (await files(tab)).includes("CHANGELOG.md"), 8000) &&
      intents("local").some((i) => i.root === root), await files(tab));
    await tab.reload();
    check("reloaded, the same checkout", await until(async () => (await files(tab)).length === 6, 10000) &&
      intents("ready").filter((i) => i.local === root).length >= 2);

    // Back: its repository's pull requests.
    await tab.tap("#btn-back");
    check("Back: its repository's pull requests", await until(async () => !!(await tab.$(".pr-row")), 8000));
    // Shown a checkout's changes by Emacs -- pr-view-changes, in a browser
    // tab -- the tab keeps it in its address, for a reload.
    await tab.evaluate((root) => BB.renderLocal({ root, branch: "x", diff: "" }), root);
    check("a checkout's changes shown keep it in the address", tab.url().endsWith("#local=" + encodeURIComponent(root)), tab.url());

    // A checkout Emacs does not vouch for: refused, and said so.
    const other = await ctx.newPage();
    watch(other, errors);
    await other.goto(BOX + "/open/pr-view#local=" + encodeURIComponent(path.dirname(root.replace(/\/$/, "")) + "/"));
    check("a checkout Emacs does not vouch for is refused, said so",
      await until(async () => /Not a checkout this page may show/.test(await text(other, "#status")), 8000) &&
      !(await other.$(".tree-file")), await text(other, "#status"));
    check("no page errors", errors.length === 0, errors);
    await ctx.close();
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
