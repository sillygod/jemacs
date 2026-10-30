// A long prompt reaches its agent whole, and one that does not reach
// Emacs is not lost with the send: its words go back in the box.
//
// At the desk the page is Emacs's own, a file, and speaks to it by its
// title, of which WebKit hands Emacs the first 1000 UTF-16 units: the
// prompt goes in parts, read here as xwapp reads them.  On a phone it is
// POSTed.  Too long for either (past 2M characters), it is not sent and
// stays in the box.  The prompts are made up.
const path = require("path");
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const { readTitles } = require("./titlelib");
const DESK = "file://" + path.join(process.env.LOCAL || path.resolve(__dirname, "../.."), "ghostherd", "ui", "index.html");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DP = "~/src/deploy-platform/";
const NAME = "claude-deploy-platform";
const BOX = '.rcol[data-name="' + NAME + '"] .rfoot textarea';
const SEND = '.rcol[data-name="' + NAME + '"] .rfoot [data-act="a:send"]';
const DESKCTX = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };

// Some 20,000 characters: CJK, runs of spaces, an emoji, quotes, a backslash, lines.
function synthetic(n) {
  const u = "長い中文の文章    \"q\" \\ \u{1F600}\n    　";
  let s = "";
  while (s.length < n) s += u;
  return s.slice(0, n).replace(/[\ud800-\udbff]$/, "x").trim();
}
const TEXT = synthetic(20000);

async function openRoom(page, act) {
  await act('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
  await sleep(180);
  await act('.room [data-act="a:room-view"][data-view="columns"]');
  await sleep(250);
}

// The box's text set as typing would, without typing 20,000 characters.
const fillBox = (page, text) => page.$eval(BOX, (ta, t) => {
  ta.value = t;
  ta.dispatchEvent(new Event("input", { bubbles: true }));
}, text);

// Emacs at the desk: the title as WebKit hands it over -- its first 1000
// units -- read and answered, parts strung together (titlelib.js).
// REFUSE(title) makes a title one Emacs cannot read, which it says to
// the page; OPTS as readTitles takes them.
async function emacs(page, until, refuse, ms, opts) {
  return (await readTitles(page, "ghmem:", until, Object.assign({ refuse, ms, idle: "Herd" }, opts))).got;
}

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- At the desk: by the title ----
    {
      const ctx = await browser.newContext(DESKCTX);
      const page = await ctx.newPage();
      const errors = [];
      page.on("pageerror", (e) => errors.push(String(e)));
      await page.goto(DESK);
      await page.waitForFunction(() => window.GM && window.XW);
      await emacs(page, (g) => g.some((i) => i.op === "xwapp-hello"), null, 3000);
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(120);
      await openRoom(page, (s) => page.click(s));

      await fillBox(page, TEXT);
      const reading = emacs(page, (g) => g.some((i) => i.op === "agent-prompt"));
      await page.click(SEND);
      const got = await reading;
      const ap = got.find((i) => i.op === "agent-prompt");
      check("desk: a 20,000-character prompt reaches its agent whole, through titles cut at 1000",
        !!ap && ap.name === NAME && ap.text === TEXT, ap ? { name: ap.name, len: ap.text.length, want: TEXT.length } : got);
      check("desk: and the box is empty for the next", (await page.$eval(BOX, (t) => t.value)) === "");

      // Emacs could not read it: the words are back in the box.
      await fillBox(page, "a made-up prompt that will not get through");
      const refused = emacs(page, (g) => g.some((i) => i.unread), (t) => /agent-prompt/.test(t), 5000);
      await page.click(SEND);
      await refused;
      await sleep(100);
      const back = await page.$eval(BOX, (t) => t.value);
      const status = await page.$eval("#status", (s) => (s.hidden ? "" : s.textContent));
      check("desk: a prompt Emacs did not get is back in the box, and the page says so",
        back === "a made-up prompt that will not get through" && /Not sent/.test(status), { back, status });

      // What was written meanwhile stays, after it.  Emacs reads the
      // title only once the box has more in it.
      await fillBox(page, "the first");
      await page.click(SEND);
      await page.waitForFunction((sel) => document.querySelector(sel).value === "", BOX);
      await fillBox(page, "written since");
      await emacs(page, (g) => g.some((i) => i.unread), (t) => /agent-prompt/.test(t), 5000);
      await sleep(100);
      const both = await page.$eval(BOX, (t) => t.value);
      check("desk: given back before what was written since, not over it", both === "the first\n\nwritten since", both);

      // Emacs busy 3 s mid-way through the prompt's parts -- a GC, a sync
      // process: the page waits for it, as Emacs waits for a part.
      // (The last case's Not sent stays up until something else is said.)
      await page.evaluate(() => { document.getElementById("status").hidden = true; });
      await fillBox(page, TEXT);
      let stalled = 0;
      const slow = emacs(page, (g) => g.some((i) => i.op === "agent-prompt"), null, 30000,
        { stall: (it) => (it.xwpart !== undefined && it.i === 5 && !stalled ? (stalled = 3000) : 0) });
      await page.click(SEND);
      const sg = await slow;
      await sleep(100);
      const sp = sg.find((i) => i.op === "agent-prompt");
      const st = await page.$eval("#status", (s) => (s.hidden ? "" : s.textContent));
      check("desk: Emacs busy 3 s mid-way through a long prompt: it arrives whole, nothing said Not sent",
        stalled && sp && sp.text === TEXT && !/Not sent/.test(st) && (await page.$eval(BOX, (t) => t.value)) === "",
        { stalled, len: sp && sp.text.length, st });

      // The answer that lands late: Emacs reads prompt A, then is busy past
      // the page's 10 s before its answer lands.  The page has given A up
      // -- back in the box, Not sent -- and sent B over it.  A's answer
      // must not clear B; and as Emacs had A, A comes out of the box again
      // and Not sent goes: sent again, it would go twice.
      const A = "a made-up prompt Emacs reads late";
      const B = "a made-up prompt sent after it";
      await fillBox(page, A);
      const raced = emacs(page, (g) => g.some((i) => i.op === "agent-prompt" && i.text === B), null, 30000,
        { stall: (it) => (it.op === "agent-prompt" && it.text === A ? 10600 : 0) });
      await page.click(SEND);
      await page.waitForFunction((sel) => document.querySelector(sel).value === "", BOX);
      await fillBox(page, B);
      await page.click(SEND);
      // Given up after 10 s: A back in the box, before nothing.
      await page.waitForFunction((sel) => document.querySelector(sel).value !== "", BOX, { timeout: 12000 }).catch(() => {});
      const given = await page.$eval(BOX, (t) => t.value);
      const said = await page.$eval("#status", (s) => (s.hidden ? "" : s.textContent));
      const rg = await raced;
      await sleep(150);
      const after = await page.$eval(BOX, (t) => t.value);
      const st2 = await page.$eval("#status", (s) => (s.hidden ? "" : s.textContent));
      const prompts = rg.filter((i) => i.op === "agent-prompt").map((i) => i.text);
      check("desk: unread 10 s with another behind it, A was given back, Not sent", given === A && /Not sent/.test(said), { given, said });
      check("desk: A's late answer clears nothing of B's: Emacs gets A and B, once each",
        JSON.stringify(prompts) === JSON.stringify([A, B]), prompts);
      check("desk: Emacs had A after all: out of the box again, and Not sent gone", after === "" && !/Not sent/.test(st2), { after, st2 });
      check("desk: no page errors", errors.length === 0, errors);
      await ctx.close();
    }

    // ---- On a phone: POSTed ----
    {
      const intents = [];
      let refuse = false;
      const { ctx, page, errors } = await phonePage(browser, {
        intents, refuse: (it) => (refuse && it.op === "agent-prompt" ? 413 : 0),
      });
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(140);
      await openRoom(page, (s) => page.tap(s));

      await fillBox(page, TEXT);
      await page.tap(SEND);
      for (let i = 0; i < 50 && !intents.some((x) => x.op === "agent-prompt"); i++) await sleep(50);
      const ap = intents.find((x) => x.op === "agent-prompt");
      check("phone: a 20,000-character prompt reaches its agent whole", !!ap && ap.name === NAME && ap.text === TEXT,
        ap ? ap.text.length : intents.map((x) => x.op));

      // Emacs answers it cannot take it (too big): back in the box, and
      // the intents after it are not held up.
      refuse = true;
      await fillBox(page, "a made-up prompt Emacs will refuse");
      await page.tap(SEND);
      await page.waitForFunction((sel) => document.querySelector(sel).value !== "", BOX, { timeout: 5000 }).catch(() => {});
      const back = await page.$eval(BOX, (t) => t.value);
      const status = await page.$eval("#status", (s) => (s.hidden ? "" : s.textContent));
      check("phone: a prompt Emacs refuses is back in the box, and the page says so",
        back === "a made-up prompt Emacs will refuse" && /Not sent/.test(status), { back, status });
      refuse = false;
      const before = intents.length;
      await fillBox(page, "and the next one goes");
      await page.tap(SEND);
      for (let i = 0; i < 40 && intents.length === before; i++) await sleep(50);
      check("phone: the next is not held up behind it", intents.slice(before).some((x) => x.op === "agent-prompt" && x.text === "and the next one goes"),
        intents.slice(before));

      // Past what the page sends: not sent, kept, said.
      const huge = "字".repeat(2 * 1024 * 1024 + 10);
      const n0 = intents.length;
      await fillBox(page, huge);
      await page.tap(SEND);
      await sleep(300);
      const kept = await page.$eval(BOX, (t) => t.value.length);
      const said = await page.$eval("#status", (s) => (s.hidden ? "" : s.textContent));
      check("phone: a prompt past 2M characters is not sent, stays in the box, and the page says why",
        intents.length === n0 && kept === huge.length && /Too long to send/.test(said), { sent: intents.length - n0, kept, said });
      check("phone: no page errors", errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const passed = results.filter(Boolean).length;
  console.log(`\n${passed}/${results.length} passed`);
  process.exit(passed === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
