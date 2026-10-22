// A guarded agy agent's card: what its guard refused, with the reason, and
// for what the user may approve, Approve once -- which tells Emacs that
// request, for that exact call.  An approved or used one says so; one never
// run by an agent has no button.  Its row and its room's column, on a
// phone and at a desk.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const QA = "qa-deploy-platform";   // HERD's agy agent

const now = Date.now();
const req = (id, status, summary, extra) => Object.assign({
  id, session: QA, tool: "run_command", summary, status, approvable: true, kind: "secret", rule: "secret-file",
  reason: "cat .env: .env is a secret file.  Reading it needs the user's approval -- they can approve this exact call once on your card.",
  created: now - 60e3 }, extra || {});
const GUARD = [
  req("g1", "pending", "run_command: cat .env"),
  req("g2", "denied", "run_command: rm -rf build", { approvable: false, kind: "never", rule: "rm-rf",
      reason: "rm -rf is never run by an agent; delete with care, file by file, or ask the user." }),
  req("g3", "approved", "run_command: git push"),
  req("g4", "used", "run_command: printenv GITHUB_TOKEN"),
  req("g5", "pending", "run_command: grep -rn 'a very long pattern that goes on and on' src/some/deep/directory/structure/that/never/ends", { kind: "ask" }),
];
const herd = (guard) => {
  const h = JSON.parse(JSON.stringify(HERD));
  h.agents.forEach((a) => { if (a.name === QA) a.state = "idle"; });
  h.guard = guard;
  return h;
};

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const lines = (scope) => page.$$eval(scope + " .aguard .gline", (ls) => ls.map((l) => ({
        what: l.querySelector(".gwhat").textContent, text: l.querySelector(".gtext").textContent,
        title: l.title, button: !!l.querySelector('[data-act="a:guard-approve"]'),
        right: l.getBoundingClientRect().right, box: l.closest(".arow, .rcol").getBoundingClientRect().right,
      })));
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd(GUARD));
      await sleep(150);
      const row = '.arow[data-name="' + QA + '"]';
      const got = await lines(row);
      check(W("the card lists what its guard refused and approved"), got.length === 5, got);
      const by = (t) => got.find((l) => l.text.includes(t));
      check(W("a secret read waits for the user, with Approve once"),
        by("cat .env").what === "⛔ needs your approval" && by("cat .env").button, by("cat .env"));
      check(W("its reason is there on hover"), /secret file.*approve this exact call once/.test(by("cat .env").title), by("cat .env").title);
      check(W("never-run is denied, with no button"), by("rm -rf").what === "⛔ denied" && !by("rm -rf").button);
      check(W("approved once, not yet run"), by("git push").what === "✓ approved once, not yet run" && !by("git push").button);
      check(W("ran once"), by("printenv").what === "✓ ran once, approved" && !by("printenv").button);
      check(W("a long call stays inside the card"), got.every((l) => l.right <= l.box + 0.5), got.map((l) => [l.right, l.box]));
      check(W("other agents' cards show none"), (await page.$$('.arow[data-name="lead-deploy-platform"] .aguard')).length === 0);
      await tap(row + ' [data-act="a:guard-approve"][data-id="g1"]');
      await sleep(120);
      const sent = intents.filter((i) => i.op === "guard-approve");
      check(W("Approve once tells Emacs that request"), sent.length === 1 && sent[0].id === "g1", sent);
      check(W("and is not tapped twice"),
        await page.$eval(row + ' [data-act="a:guard-approve"][data-id="g1"]', (b) => b.disabled));

      // In its room's column too.
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd(GUARD));
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      const col = await lines('.rcol[data-name="' + QA + '"] .rtop');
      check(W("its room's column shows them"), col.length === 5 && col.some((l) => l.text.includes("cat .env") && l.button), col);
      await tap('.rcol[data-name="' + QA + '"] [data-act="a:guard-approve"][data-id="g5"]');
      await sleep(120);
      check(W("and approves from there"), intents.filter((i) => i.op === "guard-approve").map((i) => i.id).join() === "g1,g5");
      // Refused long ago and done with: off the card.
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); },
        herd([req("old", "denied", "run_command: env", { created: now - 3 * 3600e3 })]));
      await sleep(100);
      check(W("old refusals leave the card"), (await page.$$(row + " .aguard")).length === 0);
      // An ask from a guarded agent: marked on its taker's card and its own.
      const ask = (id, from, to, guarded) => ({ id, run: id, from, to, status: "delivered", auto: false, error: "",
        head: "Read the deploy key and tell me what it is", replyHead: "", created: now - 30e3, answered: null,
        parent: null, round: null, of: null, project: "", fromProject: "", guarded });
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); },
        Object.assign(herd([]), { asks: [ask("k1", QA, "lead-deploy-platform", true), ask("k2", "lead-deploy-platform", QA, false)] }));
      await sleep(120);
      const marks = await page.evaluate((QA) => {
        const at = (name, id) => [...document.querySelectorAll('.arow[data-name="' + name + '"] .aask')]
          .find((l) => l.title.startsWith("ask " + id));
        const one = (l) => l && { badge: (l.querySelector(".aguarded") || {}).textContent || "",
                                  title: l.title, right: l.getBoundingClientRect().right,
                                  box: l.closest(".arow").getBoundingClientRect().right };
        return { taker: one(at("lead-deploy-platform", "k1")), sender: one(at(QA, "k1")),
                 plain: one(at("lead-deploy-platform", "k2")) };
      }, QA);
      check(W("an ask from a guarded agent says so on its taker's card"),
        marks.taker && marks.taker.badge === "from a guarded agent", marks);
      check(W("and on its sender's"), marks.sender && marks.sender.badge === "from a guarded agent", marks);
      check(W("its title says what the taker was told"), marks.taker && /not to read or pass back secrets/.test(marks.taker.title), marks.taker);
      check(W("an ask from an agent not guarded is not marked"), marks.plain && marks.plain.badge === "", marks.plain);
      check(W("the mark stays inside the card"), marks.taker && marks.taker.right <= marks.taker.box + 0.5, marks.taker);
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
