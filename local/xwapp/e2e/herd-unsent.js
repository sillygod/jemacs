// A message the herd pasted that its CLI never took (ask 0d5895f1): Return
// pressed again and it is still in the input box.  Its card says "message
// not submitted" with a Press Return that tells Emacs to press it, and the
// Inbox has it under Needs you (and Stuck) with the same.  Desk and phone.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const NAME = "claude-deploy-platform";
const HEAD = "[ghostherd message from lead-deploy-platform → claude-deploy-platform]";
const herd = () => {
  const h = JSON.parse(JSON.stringify(HERD));
  const a = h.agents.find((x) => x.name === NAME);
  a.state = "idle"; a.unsent = HEAD; a.unsentAsk = false;
  return h;
};
const now = Date.now();
const ITEMS = [{
  id: "unsent:" + NAME, kind: "unsent", group: "need", tag: "Not sent", tone: "warn", agent: NAME,
  cli: "claude", project: "~/src/deploy-platform/", title: "Message not submitted", at: now - 30000,
  whyLabel: "It is still in its input box", preview: HEAD, state: "idle", read: false, override: null,
  body: "The herd pasted a message into its input box and pressed Return again, but its CLI never took it: it is still there, unsent.\n\n> " + HEAD,
  code: "", note: "" }];

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const lastOf = (op) => intents.filter((i) => i.op === op).pop();

      // ---- The herd card ----
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(120);
      const row = '.arow[data-name="' + NAME + '"]';
      check(W("its card says the message was not submitted"),
        /message not submitted/.test(await page.$eval(row, (r) => r.textContent)));
      check(W("its card offers Press Return"), !!(await page.$(row + ' [data-act="a:agent-submit"]')));
      await tap(row + ' [data-act="a:agent-submit"]');
      await sleep(80);
      check(W("Press Return tells Emacs, for that agent"),
        (lastOf("agent-submit") || {}).name === NAME, lastOf("agent-submit"));
      const others = await page.$$('.arow:not([data-name="' + NAME + '"]) [data-act="a:agent-submit"]');
      check(W("an agent with nothing unsent has no Press Return"), others.length === 0);

      // ---- The Inbox ----
      await tap('.tab[data-tab="inbox"]');
      await sleep(80);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 1 }), ITEMS);
      await sleep(120);
      const card = (where === "phone" ? ".ibcard" : ".ibrow") + '[data-id="unsent:' + NAME + '"]';
      check(W("the Inbox lists it under Needs you"), !!(await page.$(card)));
      await tap(card);
      await sleep(120);
      check(W("it says why"), /still in its input box/.test(await page.textContent(".app")));
      check(W("it offers Press Return"), !!(await page.$('[data-act="ib:submit"][data-id="unsent:' + NAME + '"]')));
      await tap('[data-act="ib:submit"][data-id="unsent:' + NAME + '"]');
      await sleep(80);
      check(W("Press Return in the Inbox tells Emacs, for that agent"),
        intents.filter((i) => i.op === "agent-submit").length === 2 && lastOf("agent-submit").name === NAME,
        intents.filter((i) => i.op === "agent-submit"));

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
