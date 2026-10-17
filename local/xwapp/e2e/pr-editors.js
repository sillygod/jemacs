// Which agent changed which file, marked on the file tree -- in the
// checkout's diff and the PR's.  Emacs says, per file, the agents of this
// checkout (or of the PR's head branch) that edited it; the tree shows a
// muted pill: the editor's name, the first and a count when more than one,
// the full names in its tooltip.  A file no agent is said to have edited
// gets no mark.  It fits the phone.
const { start, watch, until, sleep, BOX, PHONE } = require("./appslib");
const PR = "https://bitbucket.org/unno/shop/pull-requests/42";
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const text = (page, sel) => page.textContent(sel).catch(() => "");
const files = (page) => page.$$eval(".tree-file", (bs) => bs.map((b) => b.getAttribute("data-path")).sort());
// The editor pill on a file row: its text and tooltip, or null when none.
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

    // ---- The checkout's diff -------------------------------------------
    await tab.goto(BOX + "/open/pr-view#local=" + encodeURIComponent(root));
    await until(async () => (await files(tab)).length > 0, 10000);

    const pay = await editor(tab, "src/pay.py");
    check("a file one agent edited is marked with its name", pay && pay.text === "rd" &&
      pay.title === "Edited by rd · grok", pay);
    const readme = await editor(tab, "README.md");
    check("a file two agents edited shows the first and a count, both named in its tooltip",
      readme && readme.text === "rd +1" && readme.title === "Edited by rd · grok, lead-shop · claude", readme);
    check("a file no agent is said to have edited gets no mark",
      (await editor(tab, "notes/retry.md")) === null && (await editor(tab, "icon.ico")) === null,
      { notes: await editor(tab, "notes/retry.md"), icon: await editor(tab, "icon.ico") });
    // The marks do not push the row off the phone.
    check("the file tree still fits the phone", await tab.$eval(".tree-body", (b) =>
      b.scrollWidth <= b.clientWidth + 1));

    // ---- The PR's diff -------------------------------------------------
    const prtab = await ctx.newPage();
    watch(prtab, errors);
    await prtab.goto(BOX + "/open/pr-view#pr=" + encodeURIComponent(PR));
    await until(async () => /Retry the payment webhook/.test(await text(prtab, "#app")), 10000);
    await prtab.click('.tab[data-tab="files"]');
    await until(async () => (await files(prtab)).length > 0, 8000);
    await sleep(150);
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
