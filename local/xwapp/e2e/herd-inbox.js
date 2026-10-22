// The Inbox tab: one list of what needs the user, then updates.  The page
// lands here (home); each kind handles in place and leaves when done; the
// tab badge counts what needs the user.  Desk and phone.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const now = Date.now();
const mk = (o) => Object.assign({ cli: "claude", project: "~/src/deploy-platform/", at: now - 120000,
  whyLabel: "", body: "", preview: "", code: "", note: "", read: false, override: null }, o);
const ITEMS = [
  mk({ id: "blocked:qa", group: "need", kind: "blocked", tag: "Permission", tone: "warn", agent: "qa-deploy-platform",
    cli: "agy", at: now - 120000, title: "Wants to run a command", preview: "npm test -- --runInBand",
    whyLabel: "It is waiting on a menu", body: "Run this command in ~/src/deploy-platform?", code: "npm test -- --runInBand",
    options: [{ n: 1, label: "Yes, run command" }, { n: 2, label: "This conversation" }, { n: 3, label: "Always npm" }, { n: 4, label: "No, cancel" }] }),
  mk({ id: "guard:g1", group: "need", kind: "guard", tag: "Guard", tone: "red", agent: "agy-tai-commit", cli: "agy",
    project: "~/src/taiex/", at: now - 240000, title: "Needs your approval", preview: "cat .env",
    whyLabel: "The guard denied this call", body: "It reads a secret file.", code: "cat .env", note: "Rule: secret-file", guardId: "g1" }),
  mk({ id: "question:lead:5", group: "need", kind: "question", tag: "Question", tone: "accent", agent: "lead-deploy-platform",
    at: now - 1080000, title: "Open a ClickUp card for #405?", preview: "PR needs a ClickUp card. Open one?",
    whyLabel: "It ended its turn with a question", body: "#405 deployed.\nOpen a ClickUp card for it?", replyVia: "lead" }),
  mk({ id: "proposal:p1", group: "need", kind: "proposal", tag: "Proposal", tone: "purple", agent: "lead-deploy-platform",
    at: now - 3600000, title: "Promote 2 facts into CLAUDE.md", preview: "Branch discipline · Uv lock",
    whyLabel: "Memory promotion", body: "Two stable facts become project instructions.",
    facts: ["Branch discipline", "Uv lock"], diff: "--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@ -1 +1,2 @@\n ctx\n+## Branches and PRs", proposalId: "p1" }),
  mk({ id: "login:jn", group: "need", kind: "login", tag: "Login", tone: "warn", agent: "claude-jnotes", project: "~/src/jnotes/",
    at: now - 10800000, title: "Login expired", preview: "Its turn stopped until someone runs /login", whyLabel: "Its CLI stopped", body: "Token expired." }),
  mk({ id: "report:lead:9", group: "fyi", kind: "report", tag: "Report", tone: "green", agent: "lead-.emacs.d", project: "~/.emacs.d/",
    at: now - 360000, title: "Guard hardenings handed out", preview: "scoped token and marked asks", whyLabel: "It reported", body: "On the three hardenings." }),
  mk({ id: "handoff:h1", group: "fyi", kind: "handoff", tag: "Handoff", tone: "purple", agent: "lead-jnotes", project: "~/src/jnotes/",
    at: now - 2400000, title: "Answered: claude -p closed asks", preview: ".emacs.d fixed it", whyLabel: "Cross-project handoff, answered", body: "Fixed in 72f7736." }),
];

async function load(browser, where) {
  const intents = [];
  const ctx0 = where === "desk" ? DESK : {};
  const { ctx, page, errors } = await phonePage(browser, { intents, context: ctx0 });
  return { ctx, page, errors, intents };
}
const sent = (intents, op) => intents.filter((i) => i.op === op);

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const { ctx, page, errors, intents } = await load(browser, where);
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));

      // ---- Home: the page lands on Inbox, and asks Emacs to watch it ----
      await sleep(150);
      check(W("the page opens on the Inbox (home)"),
        !!(await page.$(".inboxpage")) && (await page.$eval('.tab[data-tab="inbox"]', (b) => b.classList.contains("on"))));
      check(W("it asks Emacs to watch the Inbox"), sent(intents, "inbox-watch").length >= 1);

      // ---- The list: Needs-you + Updates, badge ----
      await page.evaluate((it) => GM.renderInbox({ items: it, count: it.filter((x) => x.group === "need").length }), ITEMS);
      await sleep(120);
      check(W("the tab badge counts what needs you"), (await page.$eval("#inbox-n", (e) => e.textContent)) === "5");
      if (where === "desk") {
        check(W("Needs-you lists the 5 need items"), (await page.$$eval(".iblistcol .ibrow", (es) => es.length)) >= 5);
      } else {
        check(W("phone shows cards with inline actions"),
          !!(await page.$(".ibcard.need .iboptbtn")) && !!(await page.$(".ibcard.need .ibbtn.ok")));
      }

      // ---- Filters: Approvals shows only guard + proposal ----
      await tap('[data-act="ib:filter"][data-f="approve"]');
      await sleep(80);
      const approveKinds = await page.$$eval(".ibtag", (es) => es.map((e) => e.textContent));
      check(W("the Approvals filter shows only Guard and Proposal"),
        approveKinds.every((t) => t === "Guard" || t === "Proposal") && approveKinds.includes("Guard") && approveKinds.includes("Proposal"));
      await tap('[data-act="ib:filter"][data-f="all"]');
      await sleep(80);

      // ---- Each kind's action routes to the right intent ----
      if (where === "desk") await page.click('.ibrow[data-id="blocked:qa"]');
      await sleep(60);
      // blocked: answer option 1
      await tap('[data-act="ib:answer"][data-id="blocked:qa"][data-n="1"]');
      await sleep(80);
      const ans = sent(intents, "agent-answer").pop();
      check(W("answering a menu sends agent-answer {name,n}"), ans && ans.name === "qa-deploy-platform" && ans.n === 1, ans);
      check(W("the answered item leaves the list"), !(await page.$('[data-id="blocked:qa"]')));

      // guard approve
      if (where === "desk") await page.click('.ibrow[data-id="guard:g1"]');
      await sleep(60);
      await tap('[data-act="ib:guard-approve"][data-id="guard:g1"]');
      await sleep(80);
      check(W("Approve once sends guard-approve {id}"), (sent(intents, "guard-approve").pop() || {}).id === "g1");

      // proposal approve
      if (where === "desk") await page.click('.ibrow[data-id="proposal:p1"]');
      await sleep(60);
      await tap('[data-act="ib:prop-approve"][data-id="proposal:p1"]');
      await sleep(80);
      check(W("Approve proposal sends memory-proposal-approve {id}"), (sent(intents, "memory-proposal-approve").pop() || {}).id === "p1");

      // question reply
      if (where === "desk") await page.click('.ibrow[data-id="question:lead:5"]');
      await sleep(60);
      await page.fill('.ibreplybox[data-id="question:lead:5"]', "好，開一張");
      await tap('[data-act="ib:reply"][data-id="question:lead:5"]');
      await sleep(80);
      const say = sent(intents, "lead-say").pop();
      check(W("replying to a lead question sends lead-say {project,text}"),
        say && say.project === "~/src/deploy-platform/" && say.text === "好，開一張", say);

      // ---- Mark read / done and Undo ----
      if (where === "desk") await page.click('.ibrow[data-id="report:lead:9"]');
      await sleep(60);
      await tap('[data-act="ib:done"][data-id="report:lead:9"]');
      await sleep(80);
      check(W("marking read sends inbox-set {done:true}"), (sent(intents, "inbox-set").filter((i) => i.id === "report:lead:9" && i.done === true).length) === 1);
      check(W("the done item leaves at once, with an Undo"), !(await page.$('[data-id="report:lead:9"].ibrow, .ibcard [data-id]')) || !!(await page.$('[data-act="ib:undo"]')));
      check(W("Undo is offered"), !!(await page.$('[data-act="ib:undo"]')));
      await tap('[data-act="ib:undo"]');
      await sleep(80);
      check(W("Undo sends inbox-set {done:false}"), sent(intents, "inbox-set").some((i) => i.id === "report:lead:9" && i.done === false));

      // ---- Auto-resolve: an item whose cause is gone leaves on the next push ----
      await page.evaluate((it) => GM.renderInbox({ items: it.filter((x) => x.id !== "guard:g1"), count: 4 }), ITEMS);
      await sleep(80);
      check(W("an item gone from the push leaves the list (auto-resolve)"), !(await page.$('[data-id="guard:g1"]')));
      check(W("the badge drops to 4"), (await page.$eval("#inbox-n", (e) => e.textContent)) === "4");

      // ---- A worker's turn end, by how it began, and a lead started for a handoff ----
      const WORKER = [
        mk({ id: "report:claude-godzilla:7", group: "fyi", kind: "report", tag: "Report", tone: "green",
          agent: "claude-godzilla", project: "~/src/godzilla/", at: now - 60000,
          title: "Shipped the import", preview: "nothing left open", whyLabel: "It reported",
          body: "Shipped the import; nothing left open." }),
        mk({ id: "question:claude-godzilla:8", group: "need", kind: "question", tag: "Question", tone: "accent",
          agent: "claude-godzilla", project: "~/src/godzilla/", at: now - 30000,
          title: "Open a PR?", preview: "Shall I open a PR?", whyLabel: "It ended its turn with a question",
          body: "Done.\nShall I open a PR?", replyVia: "agent" }),
        mk({ id: "lead-started:lead-jnotes", group: "fyi", kind: "lead-started", tag: "Lead", tone: "muted",
          agent: "lead-jnotes", project: "~/src/jnotes/", at: now - 90000,
          title: "Lead started for ~/src/jnotes/", preview: "Started to take a handoff from lead-deploy-platform",
          whyLabel: "The herd started a lead for a handoff",
          body: "The herd started lead-jnotes to take a cross-project handoff from lead-deploy-platform." }),
      ];
      await page.evaluate((it) => GM.renderInbox({ items: it, count: it.filter((x) => x.group === "need").length }), WORKER);
      await sleep(100);
      // a worker's user-started turn end shows (an ask-started one is simply not pushed)
      check(W("a worker's user-started turn end shows as a Report"), !!(await page.$('[data-id="report:claude-godzilla:7"]')));
      // a lead the herd started for a handoff shows as an Update, kept until read
      check(W("a lead started for a handoff shows as an Update"), !!(await page.$('[data-id="lead-started:lead-jnotes"]')));
      // replying to a worker's question goes to the agent itself, not a lead
      if (where === "desk") await page.click('.ibrow[data-id="question:claude-godzilla:8"]');
      await sleep(60);
      await page.fill('.ibreplybox[data-id="question:claude-godzilla:8"]', "yes, open it");
      await tap('[data-act="ib:reply"][data-id="question:claude-godzilla:8"]');
      await sleep(80);
      const aprompt = sent(intents, "agent-prompt").pop();
      check(W("replying to a worker's question sends agent-prompt {name,text}"),
        aprompt && aprompt.name === "claude-godzilla" && aprompt.text === "yes, open it", aprompt);
      check(W("no leak to lead-say for a worker reply"), sent(intents, "lead-say").every((i) => i.project !== "~/src/godzilla/"));
      // marking the lead-started Update read removes it, with inbox-set
      if (where === "desk") await page.click('.ibrow[data-id="lead-started:lead-jnotes"]');
      await sleep(60);
      await tap('[data-act="ib:done"][data-id="lead-started:lead-jnotes"]');
      await sleep(80);
      check(W("marking the lead-started Update read sends inbox-set {done:true}"),
        sent(intents, "inbox-set").some((i) => i.id === "lead-started:lead-jnotes" && i.done === true));

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
