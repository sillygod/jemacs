// Comments on a PR's diff go to the agent on its head branch, not the
// forge: open a PR, pick a line (or a range) of the Files changed diff,
// comment, and send it to the one pr-view offers.  Nothing is posted.
const { start, watch, until, sleep, log, BOX, PHONE } = require("./appslib");
const PR = "https://bitbucket.org/unno/shop/pull-requests/42";
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const reviews = () => log().filter((w) => w && w.review);
const posts = () => log().filter((w) => w && w.http && w.http !== "GET");
const text = (page, sel) => page.textContent(sel).catch(() => "");
const row = (side, line) => '.file-diff .diff-row[data-side="' + side + '"][data-line="' + line + '"]';
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
    const ctx = await s.browser.newContext(PHONE);
    await ctx.route(/^https:\/\//, (r) => r.fulfill({ status: 200, contentType: "text/plain", body: "away" }));
    const errors = [];
    const tab = await ctx.newPage();
    watch(tab, errors);
    await tab.goto(BOX + "/open/pr-view#pr=" + encodeURIComponent(PR));
    await until(async () => /Retry the payment webhook/.test(await tab.textContent("#app")), 10000);
    await tab.click('.tab[data-tab="files"]');
    await until(async () => !!(await tab.$(".file-diff")), 8000);
    await sleep(200);
    check("the PR's diff takes comments (an agent is on its head branch)",
      !!(await tab.$(".file-diff.rv-on")) && !!(await tab.$(row("new", 11))), await text(tab, ".file-diff"));

    // A single line: comment and send to the agent on the branch.
    await tab.tap(row("new", 11));
    await sleep(150);
    check("a line of the PR diff opens a box", /^Line 11$/.test(await text(tab, ".rv-box .rv-where")), await text(tab, ".rv-box .rv-where"));
    await tab.fill(".rv-input", "Cap the retries.");
    await tab.tap('.rv-box [data-rv-act="save"]');
    await sleep(150);
    await tab.selectOption("#rv-to", "pr-rd");
    await tab.tap(".rv-send");
    check("sent once, to the agent on the branch, as a PR review",
      await until(async () => reviews().length === 1, 6000) && reviews()[0].review === "pr-rd" && reviews()[0].pr === true,
      reviews());
    const r0 = reviews()[0] && reviews()[0].text;
    check("the text names the pull request and its branch, at the file and line",
      r0 && r0.startsWith("Comments on the pull request (branch feature/") &&
      r0.includes("\n1. shop/payments/webhook.py:11\n") && r0.includes("Cap the retries."), r0);
    check("nothing was posted to the forge", posts().length === 0, posts());
    check("the page was told it went",
      await until(async () => (await text(tab, "#status")) === "Sent to pr-rd", 4000));

    // A range of lines, in the PR view too.
    await sleep(200);
    await selectRange(tab, 11, 13);
    await sleep(150);
    check("a selection over PR-diff lines opens a range box", (await text(tab, ".rv-box .rv-where")) === "Lines 11–13",
      await text(tab, ".rv-box .rv-where"));
    await tab.fill(".rv-input", "Fold the loop.");
    await tab.tap('.rv-box [data-rv-act="save"]');
    await sleep(150);
    await tab.selectOption("#rv-to", "pr-rd");
    await tab.tap(".rv-send");
    check("the range is cited start-end in the PR review",
      await until(async () => reviews().length === 2, 6000) &&
      reviews()[1].text.includes("\n1. shop/payments/webhook.py:11-13\n") && reviews()[1].pr === true,
      reviews()[1] && reviews()[1].text);
    check("still nothing posted to the forge", posts().length === 0, posts());
    check("no page errors", errors.length === 0, errors);
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
