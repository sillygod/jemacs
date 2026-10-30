// /login's last step (ask 25b5f652): signed in, its CLI waits for Return
// before it gives the input box back.  The agent reads as blocked with
// "signed in: press Return to continue", and its card (Agents tab and
// room, never folded on the phone) offers a Press Return -- and no
// /login -- that tells Emacs to press it; the Inbox has it under Needs you
// (and Stuck) with the same.  Desk and phone.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const NAME = "claude-deploy-platform";
// A herd where the implementer has just signed in again.
const herd = () => {
  const h = JSON.parse(JSON.stringify(HERD));
  const a = h.agents.find((x) => x.name === NAME);
  a.state = "blocked"; a.reason = "signed in: press Return to continue";
  a.login = false; a.url = ""; a.loginContinue = true;
  return h;
};
const now = Date.now();
const ITEMS = [{
  id: "login-continue:" + NAME, kind: "login-continue", group: "need", tag: "Login", tone: "warn", agent: NAME,
  cli: "claude", project: DP, title: "Signed in — press Return to continue", at: now - 20000,
  whyLabel: "Its sign-in waits on one key", preview: "Its login went through; Return gives it its input box back",
  state: "blocked", read: false, override: null,
  body: "Its /login went through, and the CLI waits for Return before it gives the input box back.",
  code: "", note: "" }];

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const sent = () => intents.filter((i) => i.op === "agent-login-continue");
      const shown = (sel) => page.$eval(sel, (el) => { const s = getComputedStyle(el); return s.display !== "none" && s.visibility !== "hidden"; }).catch(() => false);

      // ---- The Agents tab ----
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(120);
      const row = '.arow[data-name="' + NAME + '"]';
      check(W("the signed-in agent shows blocked, saying Return finishes it"),
        !!(await page.$(row + ".s-blocked")) && /signed in: press Return to continue/.test(await page.$eval(row, (r) => r.textContent)));
      check(W("its card offers Press Return"), !!(await page.$(row + ' [data-act="a:agent-login-continue"]')));
      check(W("and no /login, which would take that Return"), !(await page.$(row + ' [data-act="a:agent-login"]')));
      await tap(row + ' [data-act="a:agent-login-continue"]');
      await sleep(80);
      check(W("Press Return tells Emacs, for that agent"),
        sent().length === 1 && sent()[0].name === NAME, sent());
      check(W("no other agent has it"),
        (await page.$$('.arow:not([data-name="' + NAME + '"]) [data-act="a:agent-login-continue"]')).length === 0);

      // ---- The room ----
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      await page.evaluate((h) => GM.renderHerd(h), herd());
      await sleep(120);
      const col = '.rcol[data-name="' + NAME + '"]';
      check(W("the room card offers Press Return too"), !!(await page.$(col + ' [data-act="a:agent-login-continue"]')));
      check(W("shown with the card collapsed: it needs the user"),
        !(await page.$(col + ".expanded")) && (await shown(col + ' [data-act="a:agent-login-continue"]')));
      await tap(col + ' [data-act="a:agent-login-continue"]');
      await sleep(80);
      check(W("the room's Press Return tells Emacs, for that agent"),
        sent().length === 2 && sent()[1].name === NAME, sent());

      // ---- The Inbox ----
      await tap('.tab[data-tab="inbox"]');
      await sleep(80);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 1 }), ITEMS);
      await sleep(120);
      const card = (where === "phone" ? ".ibcard" : ".ibrow") + '[data-id="login-continue:' + NAME + '"]';
      check(W("the Inbox lists it under Needs you"), !!(await page.$(card)));
      await tap('[data-act="ib:filter"][data-f="stuck"]');
      await sleep(80);
      check(W("and under Stuck"), !!(await page.$(card)));
      await tap(card);
      await sleep(120);
      check(W("it says why"), /waits on one key/.test(await page.textContent(".app")));
      const btn = '[data-act="ib:login-continue"][data-id="login-continue:' + NAME + '"]';
      check(W("it offers Press Return"), !!(await page.$(btn)) && /Press Return/.test(await page.textContent(btn)));
      await tap(btn);
      await sleep(80);
      check(W("Press Return in the Inbox tells Emacs, for that agent"),
        sent().length === 3 && sent()[2].name === NAME, sent());

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
