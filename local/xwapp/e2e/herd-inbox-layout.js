// The two layout fixes the Inbox had (ask 806d7f96), as the Board keeps
// them now its items are there:
//  desk -- the list is stable; opening an item must not move it;
//  phone -- a line opens in place to the full detail, one at a time.
// Markdown in a body; titles plain.  The question needs the user (要你處理);
// the report is of no work item (最近回報).
const { webkit } = require("playwright");
const { phonePage, sleep, EMPTY_BOARD, boardUnfold } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1400, height: 900 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 780 }, deviceScaleFactor: 2, isMobile: true, hasTouch: true };
const now = Date.now();
const mk = (o) => Object.assign({ cli: "claude", project: "~/p/", at: now - 60000, read: false, override: null,
  whyLabel: "", body: "", preview: "", code: "", note: "" }, o);
const ITEMS = [
  mk({ id: "q:1", group: "need", kind: "question", tag: "Question", tone: "accent", agent: "claude-gz",
    title: "A long question title that should wrap to two lines on a narrow phone rather than be cut to one",
    preview: "shall I open a PR?", whyLabel: "It ended its turn with a question", body: "Done.\nShall I open a PR?", replyVia: "agent" }),
  mk({ id: "r:1", group: "fyi", kind: "report", tag: "Report", tone: "green", agent: "lead-x",
    title: "Ask **806d7f96** is complete", preview: "cleaned **scratch** and `git`",
    whyLabel: "It reported", body: "Ask **806d7f96** is done.\n\n- updated `inbox-tab`\n- cleaned scratch" }),
];
const push = (page) => page.evaluate(([it, b]) => { GM.renderInbox({ items: it, count: 1 }); GM.boardPatch(b); }, [ITEMS, EMPTY_BOARD]);

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- DESK: stable across opening ----
    {
      const { ctx, page, errors } = await phonePage(browser, { intents: [], context: DESK });
      await sleep(150);
      await push(page);
      await sleep(120);
      await boardUnfold(page);
      const geo = () => page.evaluate(() => {
        const l = document.querySelector(".bd-need .bdneeds").getBoundingClientRect();
        const r = document.querySelector('.bdnrow[data-id="q:1"]').getBoundingClientRect();
        return { left: Math.round(l.left), width: Math.round(l.width), rowTop: Math.round(r.top) };
      });
      const z = await geo();                                    // nothing open
      check("desk: the list is wide from the first draw", z.width >= 800, z);
      await page.click('.bdnrow[data-id="r:1"] .bdnr');
      await sleep(120);
      const p = await geo();                                    // an item open
      check("desk: opening an item does not move the list", z.left === p.left && z.width === p.width && z.rowTop === p.rowTop, { z, p });
      // Markdown: the body renders it (bold, inline code, list); the title
      // on its line shows plain text with the markup stripped.
      const bodyHtml = await page.$eval(".bdnrow.open .ibbody", (e) => e.innerHTML);
      check("desk: the body renders Markdown (bold, code, list)",
        /<strong>806d7f96<\/strong>/.test(bodyHtml) && /<code>inbox-tab<\/code>/.test(bodyHtml) && /<li>/.test(bodyHtml), bodyHtml);
      const title = await page.$eval('.bdnrow[data-id="r:1"] .ibtitle', (e) => e.innerHTML);
      check("desk: the title is plain (markup stripped), said once", title.includes("Ask 806d7f96 is complete") && !title.includes("*") &&
        !/<strong>/.test(title) && (await page.$$eval(".bdnrow.open .ibtitle", (es) => es.length)) === 1, title);
      check("desk: no page errors", errors.length === 0, errors);
      await ctx.close();
    }
    // ---- PHONE: lines open in place to the full detail ----
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: PHONE });
      await sleep(150);
      await push(page);
      await sleep(120);
      await boardUnfold(page);
      const sent = (op) => intents.filter((i) => i.op === op);
      const has = (s) => page.$(s).then((e) => !!e);
      check("phone: lines shut show no body", !(await has(".bdnrow .bdnbody")));
      check("phone: lines shut show no answer or reply", !(await has(".bdnrow .ibbtn, .bdnrow .iboptbtn, .bdnrow .ibreplybox")));
      check("phone: the title is one line, cut", await page.$eval('.bdnrow[data-id="q:1"] .ibtitle', (e) =>
        getComputedStyle(e).whiteSpace === "nowrap" && getComputedStyle(e).textOverflow === "ellipsis" && e.getBoundingClientRect().height < 24));
      await page.tap('.bdnrow[data-id="q:1"] .bdnr');
      await sleep(120);
      check("phone: tapping a line opens it to the full detail",
        (await has(".bdnrow.open .bdnbody")) && (await has(".bdnrow.open .ibreplybox")));
      check("phone: the open line shows the full body text",
        (await page.$eval(".bdnrow.open .ibbody", (e) => e.textContent)).includes("Shall I open a PR"));
      check("phone: the open line's title wraps, whole", (await page.$eval(".bdnrow.open .ibtitle", (e) => getComputedStyle(e).whiteSpace)) === "normal");
      check("phone: opening a line marks it read", sent("inbox-set").some((i) => i.id === "q:1" && i.read === true));
      await page.fill(".bdnrow.open .ibreplybox", "yes");
      await sleep(60);
      check("phone: typing in it does not close it", await has(".bdnrow.open .bdnbody"));
      await page.tap('.bdnrow[data-id="r:1"] .bdnr');
      await sleep(120);
      check("phone: one open at a time", (await page.$$eval(".bdnrow.open", (es) => es.length)) === 1);
      check("phone: the newly tapped one is the open one",
        (await page.$eval(".bdnrow.open .ibtitle", (e) => e.textContent)).includes("Ask 806d7f96 is complete"));
      // Markdown: the open one's body renders it; its title is plain.
      check("phone: the open body renders Markdown",
        /<strong>806d7f96<\/strong>/.test(await page.$eval(".bdnrow.open .ibbody", (e) => e.innerHTML)));
      check("phone: the title is plain (markup stripped)",
        !(await page.$eval(".bdnrow.open .ibtitle", (e) => e.innerHTML)).includes("*"));
      const h = await page.$eval(".bdnrow.open .ibbtn", (e) => e.getBoundingClientRect().height);
      check("phone: action buttons keep a 44px touch target", h >= 44, h);
      await page.tap(".bdnrow.open .bdnr");
      await sleep(120);
      check("phone: tapping the open line's head closes it", (await page.$$eval(".bdnrow.open", (es) => es.length)) === 0);
      check("phone: no page errors", errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
