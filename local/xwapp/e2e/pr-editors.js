// Which agent changed which file, marked on the file tree -- checkout and
// PR -- and a *cold open*: the edits cache is empty when the diff opens, so
// no marks show at first; a beat later Emacs warms it and pushes the marks
// (pr-view-refresh-editors -> setEditors), with no manual refresh and no
// reload of the diff.  The pill shows the editor's name (first + a count
// when several), the full names in its tooltip; a file no agent edited
// gets none.  It fits the phone.
const { start, watch, until, sleep, BOX, PHONE } = require("./appslib");
const PR = "https://bitbucket.org/unno/shop/pull-requests/42";
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const text = (page, sel) => page.textContent(sel).catch(() => "");
const files = (page) => page.$$eval(".tree-file", (bs) => bs.map((b) => b.getAttribute("data-path")).sort());
const marked = (page) => page.$$eval(".tree-file .tree-editor", (es) => es.length);
const editor = (page, path) => page.$eval('.tree-file[data-path="' + path + '"]', (b) => {
  const e = b.querySelector(".tree-editor");
  return e ? { text: e.textContent, title: e.getAttribute("title") } : null;
}).catch(() => null);

(async () => {
  const s = await start();
  try {
    const root = s.urls.local;
    const ctx = await s.browser.newContext(PHONE);
    await ctx.route(/^https:\/\//, (r) => r.fulfill({ status: 200, contentType: "text/plain", body: "away" }));
    const errors = [];
    const tab = await ctx.newPage();
    watch(tab, errors);

    // ---- The checkout's diff, opened cold ------------------------------
    await tab.goto(BOX + "/open/pr-view#local=" + encodeURIComponent(root));
    await until(async () => (await files(tab)).length > 0, 10000);
    // Cold at first: the tree is there, but no marks yet.
    check("a cold open shows the diff with no marks yet", (await marked(tab)) === 0, await marked(tab));
    // The cache warms and the marks arrive on their own -- no refresh click.
    check("the marks appear without a manual refresh",
      await until(async () => (await marked(tab)) > 0, 6000), await marked(tab));

    const pay = await editor(tab, "src/pay.py");
    check("a file one agent edited is marked with its name", pay && pay.text === "rd" &&
      pay.title === "Edited by rd · grok", pay);
    const readme = await editor(tab, "README.md");
    check("a file two agents edited shows the first and a count, both named in its tooltip",
      readme && readme.text === "rd +1" && readme.title === "Edited by rd · grok, lead-shop · claude", readme);
    check("a file no agent is said to have edited gets no mark",
      (await editor(tab, "notes/retry.md")) === null && (await editor(tab, "icon.ico")) === null,
      { notes: await editor(tab, "notes/retry.md"), icon: await editor(tab, "icon.ico") });
    check("the file tree still fits the phone", await tab.$eval(".tree-body", (b) =>
      b.scrollWidth <= b.clientWidth + 1));

    // ---- The PR's diff (cache warm by now) -----------------------------
    const prtab = await ctx.newPage();
    watch(prtab, errors);
    await prtab.goto(BOX + "/open/pr-view#pr=" + encodeURIComponent(PR));
    await until(async () => /Retry the payment webhook/.test(await text(prtab, "#app")), 10000);
    await prtab.click('.tab[data-tab="files"]');
    await until(async () => (await files(prtab)).length > 0, 8000);
    await until(async () => !!(await editor(prtab, "shop/payments/webhook.py")), 6000);
    const hook = await editor(prtab, "shop/payments/webhook.py");
    check("in the PR, the file the head-branch agent edited is marked",
      hook && hook.text === "pr-rd" && hook.title === "Edited by pr-rd · claude", hook);
    check("a PR file no agent edited gets no mark",
      (await editor(prtab, "shop/settings.py")) === null, await editor(prtab, "shop/settings.py"));

    check("no page errors", errors.length === 0, errors);
    await ctx.close();
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
