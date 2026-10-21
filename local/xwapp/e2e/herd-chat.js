// The room's Chat view: the timeline and the composer share one centred
// column on a wide window (they line up, the right half is not left empty),
// and the Columns view and the phone do not regress.  Desk and phone.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const WIDE = { viewport: { width: 1912, height: 900 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const entry = (who, kind, ts, text, extra) => Object.assign({ who, kind, ts, text }, extra || {});

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? WIDE : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const rect = (sel) => page.$eval(sel, (el) => { const r = el.getBoundingClientRect(); return { w: Math.round(r.width), left: Math.round(r.left) }; }).catch(() => null);
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(150);
      await page.evaluate(() => GM.setChat({ project: "~/src/deploy-platform/", earlier: false, missing: [], absent: [],
        entries: [{ who: "user", to: "lead-deploy-platform", kind: "turn", ts: "t1", text: "好，三件都做" },
                  { who: "lead-deploy-platform", kind: "turn", ts: "t2", text: "on it" }] }));
      await sleep(120);

      const cmsg = await rect(".chat .cmsg");
      const cfoot = await rect(".chat .cfoot");
      const cbody = await rect(".chat .cbody");
      if (where === "desk") {
        // One shared column: the timeline and the composer are the same
        // width, left-aligned the same, and narrower than the full body.
        check(W("the timeline and composer share one width"), cmsg.w === cfoot.w && cmsg.w < cbody.w, { cmsg, cfoot, cbody });
        check(W("that column is centred (aligned left edges)"), cmsg.left === cfoot.left && cmsg.left > 100, { cmsg, cfoot });
        check(W("the column is a readable width, wider than before"), cmsg.w > 920 && cmsg.w <= 1200, cmsg.w);
      } else {
        // The phone is unchanged: the column fills the width, no centring gap.
        check(W("on the phone the chat fills the width"), cmsg.left < 20, cmsg);
      }
      const wide = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
      check(W("no sideways scroll"), wide <= 0, wide);

      // The Columns view still renders (CSS is scoped to the chat).
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      check(W("the Columns view still renders"), !!(await page.$(".room .rcol")) || !!(await page.$(".room .rcols")));
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
