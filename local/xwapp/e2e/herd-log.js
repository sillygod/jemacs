// The herd page's screen captures: the per-agent "Capture" button saves a
// screen to the log with an optional note, and the Log tab filters to
// screens, shows one in full, pins/un-pins and deletes an entry, and draws
// a week's worth a window at a time rather than all at once.  Phone and desk.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const now = Date.now();
// A small log: a plain line, a blocked capture, an auto limit capture, a
// pinned manual capture.
const ENTRIES = [
  { id: 1, t: now - 5000, session: "claude-deploy-platform", kind: "state", text: "idle → working  input sent", old: "idle", new: "working", reason: "input sent", pinned: false },
  { id: 2, t: now - 4000, session: "qa-deploy-platform", kind: "state", text: "working → blocked  Approve?", old: "working", new: "blocked", reason: "Approve?", pinned: false, why: "blocked", screen: "❯ 1. Yes\n  2. No\n> " },
  { id: 3, t: now - 3000, session: "grok-jnotes", kind: "screen", text: "grok-jnotes: a limit or quota message", pinned: false, why: "limit", screen: "you are out of quota\n> " },
  { id: 4, t: now - 2000, session: "grok-jnotes", kind: "screen", text: "grok-jnotes: screen captured — stuck on a menu", pinned: true, why: "manual", note: "stuck on a menu", screen: "pick one:\n❯ keep\n  drop\n" },
];

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const rows = () => page.$$eval("#log-list .lrow", (es) => es.length);
      const lastOf = (op) => intents.filter((i) => i.op === op).pop();

      // ---- The Capture button on an agent card ----
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(100);
      const row = '.arow[data-name="grok-jnotes"]';
      check(W("every card has a Capture button"),
        (await page.$$eval('.arow [data-act="a:capture"]', (es) => es.length)) === HERD.agents.length);
      await tap(row + ' [data-act="a:capture"]');
      await sleep(40);
      await page.fill(row + " .capture-text", "look at this");
      await tap(row + ' [data-act="a:capture-save"]');
      await sleep(80);
      const cap = lastOf("agent-capture");
      check(W("Capture saves with the agent and the note"),
        cap && cap.name === "grok-jnotes" && cap.note === "look at this", cap);

      // ---- The Log tab ----
      await tap('.tab[data-tab="log"]');
      await sleep(60);
      await page.evaluate((es) => GM.renderLog({ entries: es }), ENTRIES);
      await sleep(60);
      check(W("all four entries show"), (await rows()) === 4, await rows());

      // Filter to screens: the plain state line drops out.
      await tap('[data-act="screens-only"]');
      await sleep(40);
      check(W("the Screens filter keeps only entries with a screen"), (await rows()) === 3, await rows());

      // Show one in full: its <pre> appears.
      await tap('.lrow[data-id="3"] [data-act="screen"]');
      await sleep(40);
      check(W("Screen opens the capture in full"),
        /out of quota/.test(await page.$eval("#log-list", (b) => b.textContent)) &&
        !!(await page.$("#log-list pre.screen")));

      // The manual capture shows its tag, pin mark and note.
      const four = await page.$eval('.lrow[data-id="4"]', (r) => r.textContent);
      check(W("a pinned manual capture shows its mark, tag and note"),
        /📌/.test(four) && /manual/.test(four) && /stuck on a menu/.test(four), four);

      // Pin the auto limit capture.
      await tap('.lrow[data-id="3"] [data-act="log-pin"]');
      await sleep(40);
      const pinI = lastOf("log-pin");
      check(W("Pin asks Emacs to pin it"), pinI && pinI.id === 3 && pinI.pinned === true, pinI);
      await page.evaluate(() => GM.logPinned({ id: 3, pinned: true }));
      await sleep(40);
      check(W("once pinned it carries the mark and an Unpin"),
        !!(await page.$('.lrow[data-id="3"].pinned')) &&
        !!(await page.$('.lrow[data-id="3"] [data-act="log-unpin"]')));

      // Delete the blocked capture.
      await tap('.lrow[data-id="2"] [data-act="log-delete"]');
      await sleep(40);
      const delI = lastOf("log-delete");
      check(W("Delete asks Emacs to remove it"), delI && delI.id === 2, delI);
      await page.evaluate(() => GM.logDeleted({ id: 2 }));
      await sleep(40);
      check(W("once deleted the row is gone"),
        !(await page.$('.lrow[data-id="2"]')) && (await rows()) === 2, await rows());

      // ---- Windowing: a week of entries, drawn a window at a time ----
      await tap('[data-act="screens-only"]');  // off
      await sleep(30);
      const many = Array.from({ length: 400 }, (_, i) =>
        ({ id: 1000 + i, t: now - i * 1000, session: "claude-deploy-platform",
           kind: "life", text: "entry " + i, pinned: false }));
      await page.evaluate((es) => GM.renderLog({ entries: es }), many);
      await sleep(80);
      const capped = await rows();
      check(W("a week of entries is not all drawn at once"), capped > 0 && capped <= 300, capped);
      check(W("a Show earlier button offers the rest"), !!(await page.$('[data-act="log-more"]')));
      await tap('[data-act="log-more"]');
      await sleep(60);
      check(W("Show earlier draws more"), (await rows()) > capped, await rows());

      check(W("no sideways scroll"),
        (await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth)) <= 0);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
