// The herd's checks (ask bd7efaa8): a fact made into a rule run before each
// shell call.  A check proposal on the Memory tab and in the Inbox shows
// the fact, the hint and what it denies and lets through, and is approved
// as "Turn the check on"; the Memory tab's Checks list each store's checks
// on or off with how many calls each denied and the last of them, and
// turns one off, on again, or removes it (asking first).  A fact with a
// check wears a badge.  Desk and phone; drives GM.* as Emacs would push and
// reads the intents back -- no sidecar.  Screenshots go to shots/checks/
// (SHOTS overrides).
const { webkit } = require("playwright");
const fs = require("fs");
const path = require("path");
const { phonePage } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const SHOTS = process.env.SHOTS || path.join(__dirname, "shots", "checks");
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };

const now = Date.now();
const PROJECTS = { user: { key: "user", label: "User (shared)" },
  projects: [{ key: "-src-dp", label: "deploy-platform", project: "~/src/deploy-platform/" }] };
const FACTS = [
  { name: "grep-cli-binaries-cheaply", title: "Grep cli binaries cheaply", description: "strings | grep -F",
    type: "feedback", author: "claude-dp", body: "Never grep a CLI binary with a leading .{0,N}.", shared: false },
  { name: "never-probe-the-users-live-emacs", title: "Never probe the users live emacs", description: "scratch first",
    type: "feedback", author: "claude-dp", body: "No long evals on the live Emacs.", shared: false },
  { name: "retry-scheduler", title: "Retry scheduler", description: "worker.py", type: "project",
    author: "claude-dp", body: "In worker.py.", shared: false },
];
const LONG = "grep -a -o '.{0,60}PreToolUse' ~/.local/bin/claude && " + "x".repeat(400);
const RULE = {
  fact: "grep-cli-binaries-cheaply",
  hint: "Run `timeout 30 strings FILE | grep -F -i -m 20 'literal'` instead.",
  check: { command: { all: ["^[efg]?grep\\b", "\\s['\"]?\\.\\{0,\\d+"] },
           line: { all: ["/bin/(?:claude|grok|agy)"], none: ["\\bstrings\\b"] } },
  catches: ["grep -a -o '.{0,60}PreToolUse' ~/.local/bin/claude", "grep -aoE '.{0,80}x' \"$(which grok)\""],
  passes: ["timeout 30 strings ~/.local/bin/claude | grep -F x", "grep -o '.{0,60}error' sidecar.log", "grep -ao 'x.{0,9}' ~/.local/bin/claude"],
};
const PROPOSALS = [
  { id: "p-lint", kind: "lint", status: "pending", project: "-src-dp", file: "lint", author: "claude-dp", diff: "{}",
    facts: [{ name: "grep-cli-binaries-cheaply", title: "Grep cli binaries cheaply", gone: false, promoted: false }],
    rule: RULE },
  { id: "p-md", kind: "promote", status: "pending", project: "-src-dp", file: "CLAUDE.md", author: "lead-dp",
    diff: "--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@ -1 +1,2 @@\n ctx\n+Never commit to main.",
    facts: [{ name: "retry-scheduler", title: "Retry scheduler", gone: false, promoted: false }], rule: null },
];
const CHECKS = [
  Object.assign({}, RULE, { title: "Grep cli binaries cheaply", gone: false, status: "on", author: "claude-dp",
    count: 3, last_ms: now - 2 * 3600e3, last_session: "claude-dp", last_command: LONG }),
  { fact: "never-probe-the-users-live-emacs", title: "Never probe the users live emacs", gone: false, status: "off",
    hint: "Do it in a scratch Emacs.", check: { command: { all: ["^emacsclient\\b", "profiler-report"] } },
    catches: ["emacsclient -e '(profiler-report)'"], passes: ["emacsclient -s s -e '(profiler-report)'"],
    count: 0, last_ms: null, last_session: "", last_command: "" },
  { fact: "gone-fact", title: "Gone fact", gone: true, status: "on", hint: "h", check: { line: { all: ["x"] } },
    catches: ["x"], passes: ["y"], count: 1, last_ms: now - 60e3, last_session: "agy-dp", last_command: "x" },
];

const sent = (intents, op) => intents.filter((i) => i.op === op);
const overflow = (page) => page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);

(async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const browser = await webkit.launch();
  try {
    for (const [label, ctxOpts] of [["desk", DESK], ["phone", PHONE]]) {
      const { ctx, page, intents, errors } = await phonePage(browser, { context: ctxOpts });
      const W = (n) => label + ": " + n;
      const tap = (sel) => (label === "phone" ? page.tap(sel) : page.click(sel));

      await page.evaluate(() => GM.showView("memory"));
      await page.evaluate((p) => GM.setMemoryProjects(p), PROJECTS);
      await page.evaluate((f) => GM.renderMemory({ key: "-src-dp", facts: f }), FACTS);
      await page.evaluate((p) => GM.renderProposals({ key: "-src-dp", proposals: p }), PROPOSALS);
      await page.evaluate((c) => GM.renderChecks({ key: "-src-dp", rules: c }), CHECKS);
      await sleep(120);

      // ---- A check proposal: the fact, the hint, what it denies and lets through ----
      const prop = '.mprop.ckprop';
      check(W("a check proposal shows, beside a promotion"),
        (await page.$$(prop)).length === 1 && (await page.$$("#mem-proposals .mprop")).length === 2);
      check(W("it names its fact"),
        /Grep cli binaries cheaply/.test(await page.$eval(prop + " .mpfact", (e) => e.textContent)));
      check(W("it shows the hint"), /strings FILE \| grep -F/.test(await page.$eval(prop + " .ckhint", (e) => e.textContent)));
      check(W("it lists what it denies"), (await page.$$(prop + " .ckno")).length === 2);
      check(W("and what it lets through"), (await page.$$(prop + " .ckok")).length === 3);
      check(W("its patterns are behind a fold"),
        await page.$eval(prop + " details.ckmore", (e) => !e.open && /one command/.test(e.textContent)));
      check(W("a promotion still shows its diff"), !!(await page.$('#mem-proposals .mprop:not(.ckprop) .mpdiff')));
      check(W("approving reads as turning the check on"),
        (await page.$eval(prop + ' [data-act="prop-approve"]', (e) => e.textContent)) === "Turn the check on");
      await tap(prop + ' [data-act="prop-approve"]');
      await sleep(60);
      const appr = sent(intents, "memory-proposal-approve").pop();
      check(W("Turn the check on approves the proposal by its id"), appr && appr.id === "p-lint", appr);

      // ---- The store's checks: on/off, counts, the last denied call ----
      const rows = await page.$$eval(".mchecks .mcheckrow", (es) => es.map((e) => e.textContent));
      check(W("the Checks list every check"), rows.length === 3, rows.length);
      check(W("a check says how many calls it denied, when and whose"),
        /denied 3 calls · last 2h ago by claude-dp/.test(rows[0]), rows[0]);
      check(W("one that denied nothing says so"), /denied nothing yet/.test(rows[1]), rows[1]);
      check(W("the last denied call shows, cut to a line"),
        await page.$eval(".mcheckrow .cklast", (e) => e.scrollWidth > e.clientWidth && /PreToolUse/.test(e.textContent)));
      check(W("on and off are marked"),
        JSON.stringify(await page.$$eval(".mchecks .ckstate", (es) => es.map((e) => e.textContent))) === '["on","off","on"]');
      check(W("a check whose fact is gone shows it struck"),
        await page.$eval('.mcheckrow:nth-child(4) .mpfact', (e) => e.classList.contains("gone")).catch(() => false)
          || (await page.$$(".mchecks .mpfact.gone")).length === 1);

      // ---- The facts wear their checks ----
      check(W("a fact with a check on wears the badge"),
        await page.$eval('.mhead[data-name="grep-cli-binaries-cheaply"] .mcheck', (e) => e.textContent === "check"));
      check(W("a fact whose check is off says so"),
        await page.$eval('.mhead[data-name="never-probe-the-users-live-emacs"] .mcheck', (e) => e.classList.contains("off")));
      check(W("a fact with no check wears none"), !(await page.$('.mhead[data-name="retry-scheduler"] .mcheck')));

      // ---- Open one: its hint and examples ----
      await tap('.mcheckrow .mptop[data-fact="grep-cli-binaries-cheaply"]');
      await sleep(60);
      check(W("opening a check shows its hint and examples"),
        await page.$eval(".mcheckrow", (e) => !!e.querySelector(".ckhint") && e.querySelectorAll(".ckno").length === 2));
      await page.screenshot({ path: path.join(SHOTS, label + "-memory.png"), fullPage: true });
      check(W("nothing runs off the page's edge"), (await overflow(page)) <= 0, await overflow(page));

      // ---- Turn off, turn on ----
      await tap('[data-act="check-off"][data-fact="grep-cli-binaries-cheaply"]');
      await sleep(40);
      let s = sent(intents, "lint-set").pop();
      check(W("Turn off sends the store, the fact and off"),
        s && s.key === "-src-dp" && s.fact === "grep-cli-binaries-cheaply" && s.status === "off", s);
      await tap('[data-act="check-on"][data-fact="never-probe-the-users-live-emacs"]');
      await sleep(40);
      s = sent(intents, "lint-set").pop();
      check(W("Turn on sends on"), s && s.fact === "never-probe-the-users-live-emacs" && s.status === "on", s);

      // ---- Remove asks first ----
      await tap('[data-act="check-rm-ask"][data-fact="gone-fact"]');
      await sleep(40);
      check(W("Remove asks first"), /Remove this check\?/.test(await page.$eval(".mchecks", (e) => e.textContent)));
      check(W("nothing is removed yet"), sent(intents, "lint-remove").length === 0);
      await tap('[data-act="check-rm-cancel"][data-fact="gone-fact"]');
      await sleep(40);
      check(W("Keep backs out"), !/Remove this check\?/.test(await page.$eval(".mchecks", (e) => e.textContent)));
      await tap('[data-act="check-rm-ask"][data-fact="gone-fact"]');
      await sleep(40);
      await tap('[data-act="check-rm"][data-fact="gone-fact"]');
      await sleep(40);
      const rm = sent(intents, "lint-remove").pop();
      check(W("Remove sends the store and the fact"), rm && rm.key === "-src-dp" && rm.fact === "gone-fact", rm);

      // ---- Emacs pushes the result: the check is gone, no checks hide the section ----
      await page.evaluate((c) => GM.renderChecks({ key: "-src-dp", rules: c }), CHECKS.slice(0, 2));
      await sleep(40);
      check(W("a removed check leaves the list"), (await page.$$(".mchecks .mcheckrow")).length === 2);
      await page.evaluate(() => GM.renderChecks({ key: "-src-dp", rules: [] }));
      await sleep(40);
      check(W("no checks, no section"), (await page.$eval("#mem-checks", (e) => e.innerHTML)) === "");

      // ---- The Inbox: a check proposal to approve ----
      await page.evaluate(() => document.querySelector('.tab[data-tab="inbox"]').click());
      await page.evaluate((r) => GM.renderInbox({ count: 1, items: [{
        id: "proposal:p-lint", group: "need", kind: "proposal", tag: "Proposal", tone: "purple",
        agent: "claude-dp", cli: "claude", project: "-src-dp", at: Date.now() - 60e3,
        title: "Make “Grep cli binaries cheaply” a check", whyLabel: "A check before every shell call",
        body: "Every shell call an agent of this project makes is checked against it.",
        preview: r.hint, facts: ["Grep cli binaries cheaply"], lint: true, hint: r.hint,
        catches: r.catches, passes: r.passes, proposalId: "p-lint", read: false, override: null }] }), RULE);
      await sleep(120);
      if (label === "desk") await page.click('.ibrow[data-id="proposal:p-lint"]');
      else if (await page.$('.ibcard[data-id="proposal:p-lint"]')) await page.tap('.ibcard[data-id="proposal:p-lint"]');
      await sleep(120);
      check(W("the Inbox item shows the hint and the examples"),
        await page.evaluate(() => !!document.querySelector(".ibprop .ckhint") && document.querySelectorAll(".ibprop .ckno").length === 2
          && document.querySelectorAll(".ibprop .ckok").length === 3));
      check(W("no diff for a check"), !(await page.$(".ibprop .ibdiff")));
      await page.screenshot({ path: path.join(SHOTS, label + "-inbox.png"), fullPage: true });
      const before = sent(intents, "memory-proposal-approve").length;
      await tap('.ibprop [data-act="ib:prop-approve"]');
      await sleep(80);
      const ia = sent(intents, "memory-proposal-approve");
      check(W("the Inbox's Turn the check on approves it by its id"),
        ia.length === before + 1 && ia[ia.length - 1].id === "p-lint", ia[ia.length - 1]);
      check(W("nothing runs off the Inbox's edge"), (await overflow(page)) <= 0, await overflow(page));
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log("\n" + (results.length - failed) + "/" + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
