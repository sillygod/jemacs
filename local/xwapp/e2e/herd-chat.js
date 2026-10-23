// The room's Chat view: the timeline and composer share one centred column
// on a wide window (and the Columns view and phone do not regress); and "jump
// to what you asked" -- a sticky line with your latest message, click to jump
// and flash it, ↑/↓ through the earlier ones, paging back with the B(2)
// cursor when one is older than what is loaded.  Desk and phone.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const WIDE = { viewport: { width: 1912, height: 900 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const LEAD = "lead-deploy-platform";
const you = (ts, text) => ({ who: "user", to: LEAD, kind: "turn", ts, text });
const lead = (ts, text) => ({ who: LEAD, kind: "turn", ts, text });
// The lead carries a full model/context meter, so the room's meter shows (and
// we can check it lines up with the composer).
const HERD_M = Object.assign({}, HERD, { agents: HERD.agents.map((a) =>
  a.name === LEAD ? Object.assign({}, a, { meter: { model: "Opus 5.5", effort: "xhigh", tokens: 239000, window: 1000000 } }) : a) });

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? WIDE : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const txt = (sel) => page.$eval(sel, (e) => e.textContent).catch(() => "");
      const rect = (sel) => page.$eval(sel, (el) => { const r = el.getBoundingClientRect(); return { w: Math.round(r.width), left: Math.round(r.left) }; }).catch(() => null);
      const setChat = (es, earlier) => page.evaluate((a) => GM.setChat({ project: "~/src/deploy-platform/", earlier: a.earlier, missing: [], absent: [], entries: a.es }), { es, earlier: !!earlier });
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD_M);
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(150);

      // ---- Layout: one shared centred column ----
      await setChat([you("t1", "好，三件都做"), lead("t2", "on it")], false);
      await sleep(100);
      const cmsg = await rect(".chat .cmsg"), cfoot = await rect(".chat .cfoot"), cbody = await rect(".chat .cbody");
      if (where === "desk") {
        check(W("timeline and composer share one centred column"),
          cmsg.w === cfoot.w && cmsg.w < cbody.w && cmsg.left === cfoot.left && cmsg.left > 100, { cmsg, cfoot, cbody });
        check(W("the column is readable, wider than before"), cmsg.w > 920 && cmsg.w <= 1200, cmsg.w);
      } else {
        check(W("on the phone the chat fills the width"), cmsg.left < 20, cmsg);
      }

      // ---- The meter rides in the composer's column, not at the page edge ----
      const cmeter = await rect(".chat .cmeter");
      const meter = await rect(".chat .cmeter .meter");          // the meter's content
      const chatTo = await rect(".chat .cfoot .chat-to");        // the composer's first control
      check(W("the meter sits in the same column as the composer"),
        cmeter && Math.abs(cmeter.left - cfoot.left) <= 1 && Math.abs(cmeter.w - cfoot.w) <= 1, { cmeter, cfoot });
      check(W("the meter's left edge lines up with the composer's"),
        meter && chatTo && Math.abs(meter.left - chatTo.left) <= 1, { meter, chatTo });

      // ---- The recipient <select> is themed, not the native control ----
      const sel = ".chat .cfoot .chat-to";
      const look = await page.$eval(sel, (e) => {
        const s = getComputedStyle(e);
        return { appearance: s.appearance || s.webkitAppearance, bg: s.backgroundColor, img: s.backgroundImage };
      });
      check(W("the recipient select is not in the default look"),
        look.appearance === "none" && look.bg === "rgb(14, 17, 22)" && /url\(|svg/.test(look.img), look);

      // ---- Jump through your questions (chat) ----
      await setChat([you("t1", "first question"), lead("t2", "ok"), you("t3", "second question"),
                     lead("t4", "sure"), you("t5", "latest question")], false);
      await sleep(100);
      check(W("the jump line shows your latest message"), /latest question/.test(await txt(".chat .cjump .jtext")));
      check(W("the position reads 3/3"), /3\s*\/\s*3/.test(await txt(".chat .cjump .jpos")));
      await tap('.chat .cjump [data-act="a:jump-prev"]');
      await sleep(60);
      check(W("↑ steps to the earlier question"), /second question/.test(await txt(".chat .cjump .jtext")));
      check(W("the earlier message is flashed"), !!(await page.$(".chat .cbody .cmsg.flash")));
      await tap('.chat .cjump [data-act="a:jump-prev"]');
      await sleep(60);
      check(W("↑ again reaches the first question"), /first question/.test(await txt(".chat .cjump .jtext")));
      check(W("at the oldest loaded with no more, ↑ is disabled"),
        await page.$eval('.chat .cjump [data-act="a:jump-prev"]', (b) => b.disabled));
      await tap('.chat .cjump [data-act="a:jump-next"]');
      await sleep(60);
      check(W("↓ steps back toward the latest"), /second question/.test(await txt(".chat .cjump .jtext")));
      await page.$eval('.chat .cjump [data-act="a:jump-go"]', (el) => el.click());
      await sleep(50);
      check(W("clicking the line jumps and flashes"), !!(await page.$(".chat .cbody .cmsg.flash")));

      // ---- Paging back to a question older than what is loaded ----
      await setChat([you("t10", "loaded question")], true);  // one You, more in the transcript
      await sleep(80);
      await tap('.chat .cjump [data-act="a:jump-prev"]');
      await sleep(80);
      const asked = intents.filter((i) => i.op === "chat-earlier").pop();
      check(W("↑ past the loaded pages back with the cursor"), asked && asked.before === 1, asked);
      await page.evaluate((es) => GM.earlierChat({ before: 1, earlier: true, entries: es }), [you("t5", "older question"), lead("t6", "x")]);
      await sleep(100);
      check(W("after paging, the line shows the older question"), /older question/.test(await txt(".chat .cjump .jtext")));
      // ↑ again pages, but the window's end is reached (empty page, more still
      // in the transcript): say so rather than failing silently.
      await tap('.chat .cjump [data-act="a:jump-prev"]');
      await sleep(80);
      await page.evaluate(() => GM.earlierChat({ before: 3, earlier: true, entries: [] }));
      await sleep(100);
      check(W("at the window's end, a note says older is in the transcript"),
        /transcript/i.test(await txt(".chat .cjump")));

      // ---- The same, in a column ----
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      await page.evaluate((name) => GM.setConversation({ name, path: "/x.jsonl", cli: "claude", earlier: false,
        entries: [{ role: "user", ts: "c1", text: "col question one" }, { role: "assistant", ts: "c2", text: "ok" },
                  { role: "user", ts: "c3", text: "col question two" }] }), LEAD);
      await sleep(120);
      const colSel = '.rcol[data-name="' + LEAD + '"]';
      check(W("the column's jump line shows the latest You there"), /col question two/.test(await txt(colSel + " .rjump .jtext")));
      await tap(colSel + ' .rjump [data-act="a:jump-prev"]');
      await sleep(60);
      check(W("↑ in the column steps to the earlier question"), /col question one/.test(await txt(colSel + " .rjump .jtext")));
      check(W("the column message is flashed"), !!(await page.$(colSel + " .rbody .ent.you.flash")));

      const wide = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
      check(W("no sideways scroll"), wide <= 0, wide);
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
