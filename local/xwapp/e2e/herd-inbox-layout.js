// The two Inbox layout fixes (ask 806d7f96):
//  desk -- the pane is full-width and stable; picking an item must not move it;
//  phone -- a card expands in place to the full desk detail, one at a time.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
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

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- DESK: full-width and stable across picking ----
    {
      const { ctx, page, errors } = await phonePage(browser, { intents: [], context: DESK });
      await sleep(150);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 1 }), ITEMS);
      await sleep(120);
      const geo = () => page.evaluate(() => {
        const ip = document.querySelector(".inboxpage").getBoundingClientRect();
        const lc = document.querySelector(".iblistcol").getBoundingClientRect();
        return { ipLeft: Math.round(ip.left), ipWidth: Math.round(ip.width), listLeft: Math.round(lc.left) };
      });
      const z = await geo();                                    // nothing picked (Inbox zero)
      check("desk: the pane is full width from the first render", z.ipWidth >= 1390 && z.ipLeft === 0, z);
      await page.click('.ibrow[data-id="r:1"]');
      await sleep(120);
      const p = await geo();                                    // an item picked
      check("desk: picking an item does not move the layout",
        z.ipLeft === p.ipLeft && z.ipWidth === p.ipWidth && z.listLeft === p.listLeft, { z, p });
      // Markdown: the body renders it (bold, inline code, list); the title and
      // the list preview show plain text with the markup stripped.
      const bodyHtml = await page.$eval(".ibdetail .ibbody", (e) => e.innerHTML);
      check("desk: the body renders Markdown (bold, code, list)",
        /<strong>806d7f96<\/strong>/.test(bodyHtml) && /<code>inbox-tab<\/code>/.test(bodyHtml) && /<li>/.test(bodyHtml), bodyHtml);
      const h1 = await page.$eval(".ibdetail .ibh1", (e) => e.innerHTML);
      check("desk: the detail title is plain (markup stripped)",
        h1.includes("Ask 806d7f96 is complete") && !h1.includes("*") && !/<strong>/.test(h1), h1);
      const rowTitle = await page.$eval('.ibrow[data-id="r:1"] .ibtitle', (e) => e.innerHTML);
      check("desk: the list title is plain", !rowTitle.includes("*") && !/<strong>/.test(rowTitle), rowTitle);
      check("desk: no page errors", errors.length === 0, errors);
      await ctx.close();
    }
    // ---- PHONE: cards expand in place to the full detail ----
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: PHONE });
      await sleep(150);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 1 }), ITEMS);
      await sleep(120);
      const sent = (op) => intents.filter((i) => i.op === op);
      const has = (s) => page.$(s).then((e) => !!e);
      check("phone: collapsed cards show no body", !(await has(".ibcard .ibcardbody")));
      check("phone: collapsed cards show no actions", !(await has(".ibcard .ibbtn, .ibcard .iboptbtn")));
      check("phone: the title is clamped to two lines",
        (await page.$eval(".ibcard .ibtitle", (e) => getComputedStyle(e).webkitLineClamp)) === "2");
      await page.tap('.ibcard[data-id="q:1"]');
      await sleep(120);
      check("phone: tapping a card expands it to the full detail",
        (await has(".ibcard.open .ibcardbody")) && (await has(".ibcard.open .ibreplybox")));
      check("phone: the open card shows the full body text",
        (await page.$eval(".ibcard.open .ibbody", (e) => e.textContent)).includes("Shall I open a PR"));
      check("phone: opening a card marks it read", sent("inbox-set").some((i) => i.id === "q:1" && i.read === true));
      await page.fill(".ibcard.open .ibreplybox", "yes");
      await sleep(60);
      check("phone: typing in a card does not collapse it", await has(".ibcard.open .ibcardbody"));
      await page.tap('.ibcard[data-id="r:1"]');
      await sleep(120);
      check("phone: one card open at a time", (await page.$$eval(".ibcard.open", (es) => es.length)) === 1);
      check("phone: the newly tapped card is the open one",
        (await page.$eval(".ibcard.open .ibtitle", (e) => e.textContent)).includes("Ask 806d7f96 is complete"));
      // Markdown: the open card's body renders it; its title is plain.
      check("phone: the open card body renders Markdown",
        /<strong>806d7f96<\/strong>/.test(await page.$eval(".ibcard.open .ibbody", (e) => e.innerHTML)));
      check("phone: the card title is plain (markup stripped)",
        !(await page.$eval(".ibcard.open .ibtitle", (e) => e.innerHTML)).includes("*"));
      const h = await page.$eval(".ibcard.open .ibbtn", (e) => e.getBoundingClientRect().height);
      check("phone: action buttons keep a 44px touch target", h >= 44, h);
      await page.tap(".ibcard.open .ibcardhead");
      await sleep(120);
      check("phone: tapping the open card's head collapses it", (await page.$$eval(".ibcard.open", (es) => es.length)) === 0);
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
