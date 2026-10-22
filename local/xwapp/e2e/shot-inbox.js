// Screenshots of the Inbox tab from fixture data (desk + phone), for review.
const { webkit } = require("playwright");
const fs = require("fs"); const path = require("path");
const { phonePage, sleep } = require("./herdlib");
const SHOTS = path.join(__dirname, "shots");
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };
const now = Date.now();
const mk = (o) => Object.assign({ cli: "claude", project: "~/src/deploy-platform/", at: now - 120000,
  whyLabel: "", body: "", preview: "", code: "", note: "", read: false, override: null }, o);
const ITEMS = [
  mk({ id: "blocked:qa", group: "need", kind: "blocked", tag: "Permission", tone: "warn", agent: "qa-deploy-platform", cli: "agy",
    at: now - 120000, title: "Wants to run a command", preview: "npm test -- --runInBand", whyLabel: "It is waiting on a menu",
    body: "Run this command in ~/src/deploy-platform?", code: "npm test -- --runInBand",
    options: [{ n: 1, label: "Yes, run command" }, { n: 2, label: "Yes, and always allow in this conversation" }, { n: 3, label: "Always allow commands that start with “npm”" }, { n: 4, label: "No, cancel" }] }),
  mk({ id: "guard:g1", group: "need", kind: "guard", tag: "Guard", tone: "red", agent: "agy-tai-commit", cli: "agy",
    project: "~/src/taiex/", at: now - 240000, title: "Needs your approval", preview: "cat .env",
    whyLabel: "The guard denied this call", body: "It reads a secret file inside the workspace. The agent carries on with other work and says what it needs.",
    code: "cat .env", note: "Rule: secret-file (.env). Nothing else is approved by this.", guardId: "g1", read: true }),
  mk({ id: "question:lead:5", group: "need", kind: "question", tag: "Question", tone: "accent", agent: "lead-deploy-platform",
    at: now - 1080000, title: "Open a ClickUp card for #405?", preview: "PR 上的 ticket 檢查提醒缺 ClickUp 卡。要開一張嗎？",
    whyLabel: "It ended its turn with a question", body: "#405 已部署，兩個排查 agent 都換上新的 prompt。\nPR 上的 ticket 檢查提醒缺 ClickUp 卡。要開一張嗎？", replyVia: "lead" }),
  mk({ id: "proposal:p1", group: "need", kind: "proposal", tag: "Proposal", tone: "purple", agent: "lead-deploy-platform",
    at: now - 3600000, title: "Promote 4 facts into CLAUDE.md", preview: "Branches and PRs · Backend dependencies · …", whyLabel: "Memory promotion",
    body: "Four stable rules from the shared memory become part of the project instructions, versioned with the code.",
    facts: ["branch-discipline", "uv-lock-must-follow-pyproject", "pod-env-grep-leaks-secrets", "prefer-resume-over-drain"],
    diff: "--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@ -22,6 +22,43 @@\n context line\n+## Branches and PRs\n+\n+Never commit or push to `main`.\n ## UI Verification", proposalId: "p1", read: true }),
  mk({ id: "login:jn", group: "need", kind: "login", tag: "Login", tone: "warn", agent: "claude-jnotes", project: "~/src/jnotes/",
    at: now - 10800000, title: "Login expired", preview: "Its turn stopped until someone runs /login", whyLabel: "Its CLI stopped", body: "The session token expired mid-turn.", read: true }),
  mk({ id: "report:lead:9", group: "fyi", kind: "report", tag: "Report", tone: "green", agent: "lead-.emacs.d", project: "~/.emacs.d/",
    at: now - 360000, title: "Guard hardenings handed out", preview: "scoped token, env allowlist and marked asks", whyLabel: "It reported", body: "claude-.emacs.d-2 is on the three hardenings." }),
  mk({ id: "handoff:h1", group: "fyi", kind: "handoff", tag: "Handoff", tone: "purple", agent: "lead-jnotes", project: "~/src/jnotes/",
    at: now - 2400000, title: "Answered: claude -p children closed asks", preview: ".emacs.d fixed it (72f7736)", whyLabel: "Cross-project handoff, answered", body: "Fixed by tying each agent to its terminal." }),
];
async function shot(browser, name, ctxOpts, sel) {
  const { ctx, page } = await phonePage(browser, { context: ctxOpts });
  await sleep(150);
  await page.evaluate((it) => GM.renderInbox({ items: it, count: it.filter((x) => x.group === "need").length }), ITEMS);
  await sleep(150);
  if (sel) { await page.click(sel).catch(() => {}); await sleep(120); }
  await page.screenshot({ path: path.join(SHOTS, name) });
  await ctx.close();
}
(async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const browser = await webkit.launch();
  try {
    await shot(browser, "inbox-desk.png", DESK, '.ibrow[data-id="guard:g1"]');
    await shot(browser, "inbox-desk-proposal.png", DESK, '.ibrow[data-id="proposal:p1"]');
    await shot(browser, "inbox-phone.png", PHONE, null);
  } finally { await browser.close(); }
  console.log("shots: " + path.join(SHOTS, "inbox-{desk,desk-proposal,phone}.png"));
})().catch((e) => { console.error(e); process.exit(1); });
