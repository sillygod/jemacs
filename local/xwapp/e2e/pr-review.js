// A review of what a checkout has not committed, on your phone: a line
// of pr-view's diff picked takes a comment under it; the comments are
// kept -- a reload, Refresh, another file -- and go together, from the
// bar at the foot, to the one picked of those Emacs says may get them.
// What Emacs is given to send is the comments at their files and lines.
// A send that fails keeps them; a line that moved keeps its comment, at
// the file's head.  It fits the phone; at the desk, the keys.
const fs = require("fs");
const path = require("path");
const { start, watch, measure, sideways, until, sleep, log, BOX, PHONE } = require("./appslib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const intents = (op) => log().filter((w) => w && w.intent && w.app === "pr-view" && (!op || w.intent.op === op)).map((w) => w.intent);
const sent = () => log().filter((w) => w && w.review);
const text = (page, sel) => page.textContent(sel).catch(() => "");
const files = (page) => page.$$eval(".tree-file", (bs) => bs.map((b) => b.getAttribute("data-path")).sort());
const row = (side, line) => '.file-diff .diff-row[data-side="' + side + '"][data-line="' + line + '"]';
// What follows each line: its comments, or the box.
const after = (page) => page.$$eval(".file-diff > *", (els) => {
  const out = [];
  let at = null;
  for (const e of els) {
    if (e.classList.contains("diff-row")) at = e.getAttribute("data-side") + e.getAttribute("data-line");
    else if (e.classList.contains("rv-card")) out.push(at + " card " + e.querySelector(".rv-text").textContent);
    else if (e.classList.contains("rv-box")) out.push(at + " box");
  }
  return out;
});
const cards = (page) => page.$$eval(".rv-card .rv-text", (ts) => ts.map((t) => t.textContent));
const badges = (page) => page.$$eval(".tree-file", (bs) => bs.filter((b) => b.querySelector(".tree-rv"))
  .map((b) => b.getAttribute("data-path") + "=" + b.querySelector(".tree-rv").textContent).sort());
const fits = (m, bar) => !m.pageScroll && !m.past.length && !m.small.length && !m.smallFonts.length && m.top <= bar;
const pick = async (page, file) => { await page.tap('.tree-file[data-path="' + file + '"]'); await sleep(150); };

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
    await until(async () => (await files(tab)).length > 0, 10000);
    check("it says a line takes a comment", /Pick a line to comment on it/.test(await text(tab, ".rv-hint")));
    check("no bar while there is nothing to send", !(await tab.$(".rv-bar")));

    // A line picked: a box under it, the line marked, the box ready to type in.
    await pick(tab, "src/pay.py");
    await tab.tap(row("new", 1));
    await sleep(150);
    check("a line picked opens a box under it", JSON.stringify(await after(tab)) === JSON.stringify(["new1 box"]), await after(tab));
    check("the line is marked, the box says which and takes the keys",
      (await tab.$eval(row("new", 1), (r) => r.classList.contains("rv-at"))) &&
      (await text(tab, ".rv-box .rv-where")) === "Line 1" &&
      (await tab.evaluate(() => document.activeElement && document.activeElement.classList.contains("rv-input"))));
    const m = await measure(tab);
    check("the box fits the phone", fits(m, 56), m);
    await tab.fill(".rv-input", "Name it max_retries.");
    await tab.tap('.rv-box [data-rv-act="save"]');
    await sleep(150);
    check("Add: a comment under its line", JSON.stringify(await after(tab)) === JSON.stringify(["new1 card Name it max_retries."]), await after(tab));
    check("the bar: one comment, to the first Emacs offers, of all it offers",
      /^1 comment/.test(await text(tab, ".rv-toggle")) &&
      (await tab.$eval("#rv-to", (sel) => sel.value)) === "lead-shop" &&
      JSON.stringify(await tab.$$eval("#rv-to option", (os) => os.map((o) => o.textContent))) ===
        JSON.stringify(["lead-shop · claude", "rd · grok", "qa · agy"]));
    check("the file says it has one", JSON.stringify(await badges(tab)) === JSON.stringify(["src/pay.py=1"]), await badges(tab));
    check("the hint gives way to the bar", !(await tab.$(".rv-hint")));
    const m2 = await measure(tab);
    check("the bar fits the phone", fits(m2, 56), m2);

    // Written and left for another line: kept as written.  The same line
    // again with nothing written: the box goes.
    await tab.tap(row("old", 1));
    await sleep(100);
    await tab.fill(".rv-input", "Keep the old name working.");
    await tab.tap(row("new", 2));
    await sleep(150);
    check("a comment left for another line is kept, and the box moves",
      JSON.stringify(await after(tab)) === JSON.stringify(["old1 card Keep the old name working.", "new1 card Name it max_retries.", "new2 box"]),
      await after(tab));
    await tab.tap(row("new", 2));
    await sleep(150);
    check("the same line again, nothing written: the box goes", !(await tab.$(".rv-box")));

    // Another file; then a reload, which keeps them.
    await pick(tab, "README.md");
    await tab.tap(row("new", 3));
    await sleep(100);
    await tab.fill(".rv-input", "One.\nTwo.");
    await tab.tap('.rv-box [data-rv-act="save"]');
    await sleep(150);
    check("a comment in another file", JSON.stringify(await after(tab)) === JSON.stringify(["new3 card One.\nTwo."]), await after(tab));
    await tab.reload();
    await until(async () => (await files(tab)).length > 0, 10000);
    await sleep(200);
    check("reloaded, they are still there",
      /^3 comments/.test(await text(tab, ".rv-toggle")) &&
      JSON.stringify(await badges(tab)) === JSON.stringify(["README.md=1", "src/pay.py=2"]), await badges(tab));

    // Opened, the bar lists them in the diff's order; one goes to its line.
    await tab.tap(".rv-toggle");
    await sleep(100);
    const listed = await tab.$$eval(".rv-item .rv-go", (bs) => bs.map((b) => b.querySelector(".rv-item-at").textContent));
    check("the list: file by file, line by line, as the diff has them",
      JSON.stringify(listed) === JSON.stringify(["README.md:3", "src/pay.py:1 before", "src/pay.py:1"]), listed);
    const m3 = await measure(tab);
    check("the opened bar fits the phone", fits(m3, 56), m3);
    await tab.screenshot({ path: path.join(__dirname, "shots", "pr-review-phone-bar.png") });
    await tab.fill(".rv-note", "Run the tests after.");
    const go = tab.locator(".rv-item .rv-go").nth(2);
    const gi = await go.getAttribute("data-rv-go");
    await go.tap();
    await sleep(200);
    check("one picked from the list: its file, it in view",
      (await tab.$eval(".tree-file.selected", (b) => b.getAttribute("data-path"))) === "src/pay.py" &&
      (await tab.$eval('.rv-card[data-rv="' + gi + '"]', (c) => { const r = c.getBoundingClientRect(); return r.top >= 0 && r.bottom <= innerHeight; })));

    // Sent to rd: what Emacs is given, the page told, the bar gone.
    await tab.selectOption("#rv-to", "rd");
    await tab.tap(".rv-send");
    check("Send: to the one picked, once", await until(async () => sent().length === 1, 6000) && sent()[0].review === "rd", sent());
    const want = "Comments on what is not committed in " + root + " (branch fix/payment-retry), each on a line of its diff against HEAD:\n" +
      "\n1. README.md:3\n   > A failed charge is tried again.\n   One.\n   Two.\n" +
      "\n2. src/pay.py, line 1 before, taken out:\n   > def pay(order):\n   Keep the old name working.\n" +
      "\n3. src/pay.py:1\n   > def pay(order, retries=3):\n   Name it max_retries.\n" +
      "\nRun the tests after.\n";
    check("what rd is sent: each comment at its file and line, with the line, then the note", sent()[0].text === want, sent()[0].text);
    check("the page is told, and the comments go",
      await until(async () => (await text(tab, "#status")) === "Sent to rd · grok", 4000) &&
      (await tab.$eval("#status", (e) => e.classList.contains("ok"))) && !(await tab.$(".rv-bar")) && !(await tab.$(".rv-card")),
      await text(tab, "#status"));
    await tab.reload();
    await until(async () => (await files(tab)).length > 0, 10000);
    await sleep(200);
    check("sent, a reload does not bring them back", !(await tab.$(".rv-card")) && !(await tab.$(".rv-bar")) && !!(await tab.$(".rv-hint")));

    // To one gone since: said, and the comment stays to send again.
    await pick(tab, "src/pay.py");
    await tab.tap(row("new", 3));
    await sleep(100);
    await tab.fill(".rv-input", "Pass the retries on.");
    await tab.tap('.rv-box [data-rv-act="save"]');
    await sleep(150);
    check("the one picked last is picked again", (await tab.$eval("#rv-to", (sel) => sel.value)) === "rd");
    await tab.selectOption("#rv-to", "qa");
    await tab.tap(".rv-send");
    check("a send that fails says why", await until(async () => /qa is not in the herd any more/.test(await text(tab, "#status")), 4000) &&
      (await tab.$eval("#status", (e) => e.classList.contains("error"))), await text(tab, "#status"));
    check("and keeps the comment, to send again",
      JSON.stringify(await cards(tab)) === JSON.stringify(["Pass the retries on."]) &&
      (await tab.$eval(".rv-send", (b) => !b.disabled && b.textContent === "Send")) && sent().length === 1);
    // An answer about another checkout's review -- one this page showed
    // before -- leaves this one's be.
    await tab.evaluate(() => BB.reviewSent({ root: "/elsewhere/", said: "Sent to x" }));
    await sleep(100);
    check("sent another checkout's: this one's comments stay",
      JSON.stringify(await cards(tab)) === JSON.stringify(["Pass the retries on."]) && (await text(tab, "#status")) !== "Sent to x");

    // Changed, saved; a line that moved keeps its comment at the file's head.
    await tab.tap('.rv-card [data-rv-act="edit"]');
    await sleep(100);
    check("Edit: the box, with the comment in it", (await tab.$eval(".rv-input", (t) => t.value)) === "Pass the retries on." &&
      (await text(tab, '.rv-box [data-rv-act="save"]')) === "Save");
    await tab.fill(".rv-input", "Pass retries to charge().");
    await tab.tap('.rv-box [data-rv-act="save"]');
    await sleep(150);
    check("Save: the comment changed", JSON.stringify(await cards(tab)) === JSON.stringify(["Pass retries to charge()."]));
    fs.writeFileSync(path.join(root, "src/pay.py"), "# Paying.\ndef pay(order, retries=3):\n    total = order.total\n    return charge(total, retries=retries)\n");
    await tab.tap("#btn-refresh");
    await until(async () => (await text(tab, ".file-diff")).includes("# Paying."), 8000);
    await sleep(150);
    check("Refresh, the line moved: the comment at the file's head, with its line",
      /Not at their lines any more/.test(await text(tab, ".rv-lost")) &&
      (await text(tab, ".rv-lost .rv-where")) === "Line 3" &&
      (await text(tab, ".rv-lost .rv-code")) === "    return charge(total, retries=retries)" &&
      JSON.stringify(await cards(tab)) === JSON.stringify(["Pass retries to charge()."]), await text(tab, ".rv-lost"));
    await tab.tap(".rv-toggle");
    await sleep(100);
    await tab.tap('.rv-item [data-rv-act="remove"]');
    await sleep(150);
    check("Remove: gone, and the bar with it", !(await tab.$(".rv-card")) && !(await tab.$(".rv-bar")));

    // A long line scrolled sideways: its box stays in view.
    fs.writeFileSync(path.join(root, "src/long.py"), "x = " + JSON.stringify("y".repeat(300)) + "\n");
    await tab.tap("#btn-refresh");
    await until(async () => (await files(tab)).includes("src/long.py"), 8000);
    await pick(tab, "src/long.py");
    await tab.$eval(".file-diff", (d) => { d.scrollLeft = 600; });
    // Tapped where it shows -- its text, past its cell -- not brought back
    // into view first.
    const at = await tab.$eval(row("new", 1) + " .code", (c) => { const r = c.getBoundingClientRect();
      return { x: innerWidth / 2, y: r.top + r.height / 2 }; });
    await tab.touchscreen.tap(at.x, at.y);
    await sleep(150);
    const box = await tab.$eval(".rv-box", (b) => { const r = b.getBoundingClientRect();
      return { left: r.left, right: r.right, w: innerWidth, scrolled: document.querySelector(".file-diff").scrollLeft }; })
      .catch(() => null);
    check("a long line's box stays in view, scrolled sideways", box && box.scrolled > 0 && box.left >= 0 && box.right <= box.w + 1, box);
    const side = await sideways(tab);
    check("nothing else scrolls sideways", !side.sideways.length && !side.past.length, side);
    await tab.screenshot({ path: path.join(__dirname, "shots", "pr-review-phone.png") });
    check("no page errors on the phone", errors.length === 0, errors);
    await ctx.close();

    // At the desk: click a line, write, ⌘↩ adds; Escape puts away an empty box.
    const desk = await s.browser.newContext({ viewport: { width: 1280, height: 800 } });
    await desk.route(/^https:\/\//, (r) => r.fulfill({ status: 200, contentType: "text/plain", body: "away" }));
    const dtab = await desk.newPage();
    const derrors = [];
    watch(dtab, derrors);
    await dtab.goto(BOX + "/open/pr-view#local=" + encodeURIComponent(root));
    await until(async () => (await files(dtab)).length > 0, 10000);
    await dtab.click('.tree-file[data-path="README.md"]');
    await dtab.click(row("new", 3));
    await dtab.keyboard.type("Say how often.");
    await dtab.keyboard.press("Meta+Enter");
    await sleep(150);
    check("at the desk, ⌘↩ adds it", JSON.stringify(await after(dtab)) === JSON.stringify(["new3 card Say how often."]), await after(dtab));
    await dtab.click(row("new", 2));
    await dtab.screenshot({ path: path.join(__dirname, "shots", "pr-review-desk.png") });
    await sleep(100);
    await dtab.keyboard.press("Escape");
    await sleep(100);
    // Text dragged over to copy is not a line picked.
    const span = await dtab.$eval(row("new", 3) + " .code", (c) => { const r = c.getBoundingClientRect(); return { y: r.top + r.height / 2, x: r.left }; });
    await dtab.mouse.move(span.x + 4, span.y);
    await dtab.mouse.down();
    await dtab.mouse.move(span.x + 120, span.y, { steps: 5 });
    await dtab.mouse.up();
    await sleep(100);
    check("text selected in a line opens no box", !(await dtab.$(".rv-box")) &&
      (await dtab.evaluate(() => String(getSelection()).length > 3)), await dtab.evaluate(() => String(getSelection())));
    check("Escape puts away a box with nothing written, and stays on the page",
      !(await dtab.$(".rv-box")) && !!(await dtab.$(".rv-card")) && intents("back-list").length === 0);
    await dtab.click('.rv-card [data-rv-act="edit"]');
    await dtab.keyboard.type(" Twice?");
    await dtab.keyboard.press("Escape");
    await sleep(100);
    check("but not one changed", !!(await dtab.$(".rv-box")) && (await dtab.$eval(".rv-input", (t) => t.value)) === "Say how often. Twice?");
    await dtab.click(".rv-send");
    check("Send goes with the change in it, to the first offered",
      await until(async () => sent().length === 2, 6000) && sent()[1].review === "lead-shop" &&
      /\n1\. README\.md:3\n   > A failed charge is tried again\.\n   Say how often\. Twice\?\n$/.test(sent()[1].text) &&
      (await until(async () => (await text(dtab, "#status")) === "Sent to lead-shop", 4000)), sent()[1]);
    check("the page asked Emacs to send three times, once a Send",
      intents("local-review").length === 3 && intents("local-review").every((i) => i.root === root));
    check("no page errors at the desk", derrors.length === 0, derrors);
    await desk.close();
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
