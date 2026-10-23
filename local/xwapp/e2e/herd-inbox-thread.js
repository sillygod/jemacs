// The Inbox detail shows the thread inline and the whole report (ask 021396a7):
//  - picking an item asks Emacs for its detail (once, not every tick);
//  - the full report renders (Markdown), not just its first line;
//  - the thread is drawn below with the Threads tab's own renderer -- the desk
//    swimlane, the phone "A → B" list -- and its bubbles expand in place
//    without leaving the Inbox; a quiet "Open in Threads" link, no old button.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1400, height: 900 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 780 }, deviceScaleFactor: 2, isMobile: true, hasTouch: true };
const now = Date.now();
const RID = "report:lead-x:" + now;
const mk = (o) => Object.assign({ cli: "claude", project: "~/p/", at: now - 60000, read: false, override: null,
  whyLabel: "", body: "", preview: "", code: "", note: "" }, o);
const ITEMS = [
  mk({ id: RID, group: "fyi", kind: "report", tag: "Report", tone: "green", agent: "lead-x",
    title: "Ask 021396a7 is complete", preview: "inline thread, full report",
    whyLabel: "It reported", body: "Ask 021396a7 is done." }),      // one-line head (what the tick sends)
];
const FULL = "## Summary\n\nAsk **021396a7** is complete.\n\n- inline thread\n- full report shown here";
const THREAD = {
  id: "t1", title: "do the thing", status: "answered",
  participants: ["user", "lead-x", "rd"], projects: ["p"], cross: false,
  count: 3, open: 0, started: now - 60000, at: now - 10000, rootFrom: "lead-x", rootTo: "rd",
  lanes: [
    { name: "user", kind: "you", project: "", role: "", worktree: false },
    { name: "lead-x", kind: "claude", project: "p", role: "lead", worktree: false },
    { name: "rd", kind: "claude", project: "p", role: "", worktree: false },
  ],
  msgs: [
    { kind: "message", from: "user", to: "lead-x", at: now - 60000, head: "do the thing", text: "do the thing", logged: true },
    { kind: "ask", from: "lead-x", to: "rd", at: now - 40000, head: "please handle X", text: "please handle X in full over several lines", round: 1, of: 1 },
    { kind: "answer", from: "rd", to: "lead-x", at: now - 20000, head: "done X", text: "done X, with the full details here", auto: false },
  ],
  waiting: [],
};

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- DESK: full report + inline swimlane ----
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: DESK });
      await sleep(150);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 0 }), ITEMS);
      await sleep(80);
      const has = (s) => page.$(s).then((e) => !!e);
      const sent = (op) => intents.filter((i) => i.op === op);

      await page.click('.ibrow[data-id="' + RID + '"]');
      await sleep(80);
      const d1 = sent("inbox-detail");
      check("desk: picking a report asks Emacs for its detail", d1.length === 1 && d1[0].id === RID && d1[0].kind === "report" && d1[0].agent === "lead-x", d1);

      // A steady tick (renderInbox again) must NOT trigger another fetch.
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 0 }), ITEMS);
      await sleep(60);
      check("desk: the steady tick does not re-fetch the detail", sent("inbox-detail").length === 1, sent("inbox-detail"));

      // Emacs pushes the full report and the thread (two tagged pushes).
      await page.evaluate((p) => { GM.inboxDetailData({ for: p.id, report: p.full }); GM.inboxDetailData({ for: p.id, thread: p.thread }); },
        { id: RID, full: FULL, thread: THREAD });
      await sleep(80);

      const body = await page.$eval(".ibdetail .ibbody", (e) => e.innerHTML);
      check("desk: the body shows the FULL report as Markdown",
        /<strong>021396a7<\/strong>/.test(body) && /<li>/.test(body) && /full report shown here/.test(body), body);

      check("desk: the inline thread is drawn below as the swimlane",
        (await has(".ibdetail .ibthwrap .thdiag")) && (await page.$$eval(".ibdetail .ibthwrap .thmsg", (es) => es.length)) === 3);
      check("desk: a quiet 'Open in Threads' link, and no old 'Open thread' button",
        (await has(".ibthwrap .ibthlink")) && !(await has(".ibthread")));

      // A bubble expands in place, showing the whole message, without leaving the Inbox.
      const collapsed = await page.$eval('.ibthwrap .thcard[data-i="1"]', (e) => e.getAttribute("aria-expanded"));
      await page.click('.ibthwrap .thcard[data-i="1"]');
      await sleep(60);
      const expanded = await page.$eval('.ibthwrap .thcard[data-i="1"]', (e) => e.getAttribute("aria-expanded"));
      const txt = await page.$eval('.ibthwrap .thcard[data-i="1"] .thtext', (e) => e.textContent);
      check("desk: a thread bubble expands in place to the whole message",
        collapsed === "false" && expanded === "true" && txt.includes("please handle X in full over several lines"), { collapsed, expanded, txt });
      check("desk: expanding a bubble stays in the Inbox (no navigation to Threads)",
        (await has(".ibdetail")) && !(await has(".threadspage")));
      check("desk: no page errors", errors.length === 0, errors);
      await ctx.close();
    }
    // ---- PHONE: full report + inline conversation list ----
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: PHONE });
      await sleep(150);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 0 }), ITEMS);
      await sleep(80);
      const has = (s) => page.$(s).then((e) => !!e);

      await page.tap('.ibcard[data-id="' + RID + '"]');
      await sleep(80);
      await page.evaluate((p) => { GM.inboxDetailData({ for: p.id, report: p.full }); GM.inboxDetailData({ for: p.id, thread: p.thread }); },
        { id: RID, full: FULL, thread: THREAD });
      await sleep(80);

      check("phone: the open card shows the FULL report as Markdown",
        /<strong>021396a7<\/strong>/.test(await page.$eval(".ibcard.open .ibbody", (e) => e.innerHTML)));
      check("phone: the inline thread is the compact A → B list",
        (await has(".ibcard.open .ibthwrap .thconv")) && (await page.$$eval(".ibcard.open .ibthwrap .thpmsg", (es) => es.length)) === 3);
      check("phone: no desk swimlane at 390px", !(await has(".ibcard.open .thdiag")));

      await page.tap('.ibcard.open .ibthwrap .thpmsg[data-i="2"]');
      await sleep(60);
      check("phone: a conversation bubble expands in place",
        (await page.$eval('.ibcard.open .thpmsg[data-i="2"]', (e) => e.getAttribute("aria-expanded"))) === "true" &&
        (await page.$eval('.ibcard.open .thpmsg[data-i="2"] .thtext', (e) => e.textContent)).includes("done X, with the full details here"));
      check("phone: tapping a bubble does not collapse the card", await has(".ibcard.open .ibcardbody"));
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
