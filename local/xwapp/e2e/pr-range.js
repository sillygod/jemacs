// A review comment can cover a range of lines: select two or more lines of
// the diff, click, and the box covers them; what Emacs is given cites the
// range file:start-end and quotes the lines.
const { start, watch, until, sleep, log, BOX, PHONE } = require("./appslib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const sent = () => log().filter((w) => w && w.review);
const text = (page, sel) => page.textContent(sel).catch(() => "");
const files = (page) => page.$$eval(".tree-file", (bs) => bs.map((b) => b.getAttribute("data-path")).sort());
const pick = async (page, file) => { await page.tap('.tree-file[data-path="' + file + '"]'); await sleep(150); };
// Select the new-side lines FROM..TO, then click to comment on them.
const selectRange = (page, from, to) => page.evaluate(([from, to]) => {
  const want = [];
  for (let n = from; n <= to; n++) want.push(String(n));
  const rows = [...document.querySelectorAll('.file-diff .diff-row[data-side="new"][data-line]')]
    .filter((r) => want.includes(r.getAttribute("data-line")));
  const sel = window.getSelection();
  sel.removeAllRanges();
  const rg = document.createRange();
  const a = rows[0].querySelector(".code"), b = rows[rows.length - 1].querySelector(".code");
  rg.setStart(a, 0);
  rg.setEnd(b, b.childNodes.length);
  sel.addRange(rg);
  rows[rows.length - 1].dispatchEvent(new MouseEvent("click", { bubbles: true }));
}, [from, to]);

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
    await pick(tab, "src/pay.py");

    await selectRange(tab, 1, 2);
    await sleep(150);
    check("a selection over two lines opens a range box", (await text(tab, ".rv-box .rv-where")) === "Lines 1–2",
      await text(tab, ".rv-box .rv-where"));
    check("every line in the range is marked", (await tab.$$eval('.file-diff .diff-row[data-side="new"].rv-at',
      (rs) => rs.map((r) => r.getAttribute("data-line")))).join(",") === "1,2");

    await tab.fill(".rv-input", "Fold these.");
    await tab.tap('.rv-box [data-rv-act="save"]');
    await sleep(150);
    check("a card over the range", JSON.stringify(await tab.$$eval(".rv-card .rv-text", (ts) => ts.map((t) => t.textContent))) ===
      JSON.stringify(["Fold these."]));
    check("the bar names the range", (await tab.$(".rv-toggle")) && /^1 comment/.test(await text(tab, ".rv-toggle")));
    await tab.tap(".rv-toggle");
    await sleep(100);
    check("the list names the range file:start-end",
      (await tab.$$eval(".rv-item .rv-item-at", (xs) => xs.map((x) => x.textContent)))[0] === "src/pay.py:1-2",
      await tab.$$eval(".rv-item .rv-item-at", (xs) => xs.map((x) => x.textContent)));

    await tab.selectOption("#rv-to", "rd");
    await tab.tap(".rv-send");
    check("sent once, to the one picked", await until(async () => sent().length === 1, 6000) && sent()[0].review === "rd", sent());
    const want = "\n1. src/pay.py:1-2\n   > def pay(order, retries=3):\n   >     total = order.total\n   Fold these.\n";
    check("the range is cited start-end and the lines quoted", sent().length === 1 && sent()[0].text.includes(want), sent()[0] && sent()[0].text);
    check("no page errors", errors.length === 0, errors);
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
