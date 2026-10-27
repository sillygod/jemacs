// An agent whose directory is gone (ask 0c54c5b6): its worktree removed
// outside the herd.  Its card says so and where its branch stands, with a
// Stop that asks first -- and says the commits a branch still has; the
// Inbox has it under Needs you, worded by its branch, with a Stop that
// tells Emacs.  The Inbox items are Emacs's own: `ghostherd-inbox--agent-items'
// in a batch Emacs, not a copy here.  Desk and phone.
const path = require("path");
const { execFileSync } = require("child_process");
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const LOCAL = process.env.LOCAL || path.resolve(__dirname, "../..");
const SAFE = "claude-deploy-platform";   // its branch merged
const RISK = "grok-jnotes";              // its branch 3 commits ahead
const gone = (branch, state, ahead) => ({ path: "~/.ghostherd/worktrees/p/" + branch + "/", worktree: true,
  branch, base: "master", state, ahead, since: Date.now() - 60000 });

// The items as Emacs builds them from each agent's `:gone'.
function emacsItems() {
  const el = `
(progn
  (require 'ghostherd-memory-page)
  (let ((g (lambda (b s n) (list :path (format "~/.ghostherd/worktrees/p/%s/" b) :worktree t
                                 :branch b :base "master" :state s :ahead n :since 1000.0))))
    (princ (json-encode
            (vconcat
             (seq-filter (lambda (it) (equal (plist-get it :kind) "gone"))
                         (append
                          (ghostherd-inbox--agent-items
                           (list :name "${SAFE}" :kind "claude" :state "idle" :role ""
                                 :project "~/src/deploy-platform/" :gone (funcall g "rd-tabs" "merged" 0)))
                          (ghostherd-inbox--agent-items
                           (list :name "${RISK}" :kind "grok" :state "idle" :role ""
                                 :project "~/src/jnotes/" :gone (funcall g "rd-reading" "ahead" 3))))))))))`;
  const out = execFileSync(process.env.EMACS || "emacs",
    ["-Q", "--batch", "-L", path.join(LOCAL, "ghostherd"), "-L", path.join(LOCAL, "xwapp"), "--eval", el],
    { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] });
  return JSON.parse(out).map((it) => Object.assign({ read: false, override: null }, it));
}

const herd = () => {
  const h = JSON.parse(JSON.stringify(HERD));
  h.agents.find((x) => x.name === SAFE).gone = gone("rd-tabs", "merged", 0);
  Object.assign(h.agents.find((x) => x.name === RISK), { gone: gone("rd-reading", "ahead", 3) });
  return h;
};

(async () => {
  const ITEMS = emacsItems();
  check("Emacs builds one Gone item per agent", ITEMS.length === 2 && ITEMS.every((it) => it.kind === "gone" && it.group === "need"), ITEMS);
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const stops = () => intents.filter((i) => i.op === "agent-stop-gone");

      // ---- The herd cards ----
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(120);
      const row = (n) => '.arow[data-name="' + n + '"]';
      check(W("the card of a merged one says its worktree is gone, safe to stop"),
        /worktree gone — branch rd-tabs merged into master, safe to stop/.test(await page.$eval(row(SAFE), (r) => r.textContent)));
      check(W("the card of one with commits says how many"),
        /worktree gone — branch rd-reading has 3 commits not on master/.test(await page.$eval(row(RISK), (r) => r.textContent)));
      check(W("only a gone one's card has a Stop"),
        (await page.$$('.arow [data-act="a:stop-gone"]')).length === 2);
      // Stop asks first; Cancel sends nothing.
      await tap(row(RISK) + ' [data-act="a:stop-gone"]');
      await sleep(80);
      const detail = await page.$eval(".ask-wrap .ask-detail", (e) => e.textContent).catch(() => "");
      check(W("Stop asks, naming the commits its branch still has"),
        /3 commits not on master: it is kept/.test(detail), detail);
      await page.click('.ask-wrap .ask-buttons button:has-text("Cancel")');
      await sleep(60);
      check(W("Cancel sends nothing"), stops().length === 0 && !(await page.$(".ask-wrap")));
      await tap(row(SAFE) + ' [data-act="a:stop-gone"]');
      await sleep(80);
      await page.click('.ask-wrap .ask-buttons button:has-text("Stop")');
      await sleep(80);
      check(W("Stop in the dialog tells Emacs, for that agent"),
        stops().length === 1 && stops()[0].name === SAFE, stops());

      // ---- The Inbox ----
      await tap('.tab[data-tab="inbox"]');
      await sleep(80);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: it.length }), ITEMS);
      await sleep(120);
      const card = (n) => (where === "phone" ? ".ibcard" : ".ibrow") + '[data-id="gone:' + n + '"]';
      check(W("the Inbox lists both under Needs you"), !!(await page.$(card(SAFE))) && !!(await page.$(card(RISK))));
      check(W("the badge counts them"), (await page.$eval("#inbox-n", (e) => e.textContent)) === "2");
      await tap(card(SAFE));
      await sleep(120);
      const text = await page.textContent(".app");
      check(W("a merged one: safe to stop, and why"),
        /Worktree gone — safe to stop/.test(text) && /removed outside the herd/.test(text) &&
        /nothing is lost by stopping it/.test(text) && /A respawn is refused/.test(text));
      const safeBtn = '[data-act="ib:stop-gone"][data-id="gone:' + SAFE + '"]';
      check(W("it offers Stop it"), (await page.$eval(safeBtn, (b) => b.textContent)) === "Stop it");
      await tap(safeBtn);
      await sleep(100);
      check(W("Stop it tells Emacs, for that agent"),
        stops().length === 2 && stops()[1].name === SAFE, stops());
      check(W("and the item leaves"), !(await page.$(card(SAFE))));
      // The one whose branch has commits.
      if (!(await page.$('[data-act="ib:stop-gone"][data-id="gone:' + RISK + '"]'))) await tap(card(RISK));
      await sleep(120);
      const risk = await page.textContent(".app");
      check(W("one with commits: how many, and that work may be lost"),
        /Worktree gone — 3 commits not merged/.test(risk) && /3 commits that are not on/.test(risk) && /may be lost/.test(risk));
      const riskBtn = '[data-act="ib:stop-gone"][data-id="gone:' + RISK + '"]';
      check(W("its Stop says anyway, and is not the way on"),
        (await page.$eval(riskBtn, (b) => b.textContent)) === "Stop it anyway" &&
        !(await page.$eval(riskBtn, (b) => b.classList.contains("ok"))));
      check(W("it can wait: Later"), !!(await page.$('[data-act="ib:done"][data-id="gone:' + RISK + '"]')));
      await tap(riskBtn);
      await sleep(100);
      check(W("Stop it anyway tells Emacs, for that agent"),
        stops().length === 3 && stops()[2].name === RISK, stops());
      // Stopped, Emacs sends it no more: the badge goes.
      await page.evaluate(() => GM.renderInbox({ items: [], count: 0 }));
      await sleep(80);
      check(W("once stopped, nothing is left"), !(await page.$('[data-id^="gone:"]')));

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
