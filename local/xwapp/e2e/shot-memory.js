// Screenshots of the Memory tab from fixture data (desk + phone), for review.
const { webkit } = require("playwright");
const fs = require("fs"); const path = require("path");
const { phonePage } = require("./herdlib");
const SHOTS = path.join(__dirname, "shots");
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };
const now = Date.now(); const iso = (ms) => new Date(ms).toISOString();
const PROJECTS = { user: { key: "user", label: "User (shared)" },
  projects: [{ key: "-src-dp", label: "deploy-platform", project: "~/src/deploy-platform/" },
             { key: "-src-jn", label: "jnotes", project: "~/src/jnotes/" }] };
const FACTS = [
  { name: "prefers-reasoned-design", title: "Prefers reasoned design", description: "a researched proposal, not a menu",
    type: "user", author: "user", updated: iso(now - 3600e3),
    body: "**Why:** Jing wants the reasoning, not a multiple-choice menu.\n\n- show the design and the trade-offs\n- ask \"how would you design X\" to see the thinking", shared: true },
  { name: "never-commit-to-main", title: "Never commit to main", description: "always cut a branch and open a PR",
    type: "feedback", author: "claude-deploy-platform", updated: iso(now - 7200e3),
    body: "Never commit straight to `main`.\n\n- cut a branch named for the work\n- open a PR and let qa review", shared: false },
  { name: "retry-scheduler", title: "Retry scheduler", description: "lives in worker.py, under schedule_retry",
    type: "project", author: "claude-deploy-platform", updated: iso(now - 600e3),
    body: "The retry scheduler lives in `worker.py`, under `schedule_retry`.\n\nIt backs off exponentially, capped at 5 tries.", shared: false },
  { name: "bitbucket-pr-auth", title: "Bitbucket PR auth", description: "the token lives in pr-view",
    type: "reference", author: "grok-deploy-platform", updated: iso(now - 86400e3),
    body: "Bitbucket PR calls authenticate with the app password pr-view holds.", shared: false },
];
const PROPOSALS = [
  { id: "p1", project: "-src-dp", file: "CLAUDE.md", author: "claude-deploy-platform", status: "pending",
    facts: [{ name: "never-commit-to-main", title: "Never commit to main", gone: false, promoted: false }],
    diff: "--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@ -8,6 +8,8 @@ Working conventions\n ## Working conventions\n \n-- Commit directly to main for small fixes.\n+- Never commit straight to `main`: cut a branch and open a PR.\n+- Let qa review before merge.\n" }];
async function shot(browser, name, ctxOpts, opener) {
  const { ctx, page } = await phonePage(browser, { context: ctxOpts });
  await page.evaluate(() => GM.showView("memory"));
  await page.evaluate((p) => GM.setMemoryProjects(p), PROJECTS);
  await page.evaluate((f) => GM.renderMemory({ key: "-src-dp", facts: f }), FACTS);
  await sleep(120);
  if (opener) { await opener(page); await sleep(120); }
  await page.screenshot({ path: path.join(SHOTS, name) });
  await ctx.close();
}
(async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const browser = await webkit.launch();
  try {
    await shot(browser, "memory-desk.png", DESK);
    await shot(browser, "memory-desk-open.png", DESK, async (page) => {
      await page.click('.mhead[data-name="retry-scheduler"]');
    });
    await shot(browser, "memory-phone.png", PHONE);
    await shot(browser, "memory-phone-open.png", PHONE, async (page) => {
      await page.click('.mhead[data-name="prefers-reasoned-design"]');
    });
    // The Proposals section (a pending promotion), desk and phone.
    const props = async (page) => page.evaluate((p) => GM.renderProposals({ key: "-src-dp", proposals: p }), PROPOSALS);
    await shot(browser, "memory-proposals-desk.png", DESK, props);
    await shot(browser, "memory-proposals-phone.png", PHONE, props);
  } finally { await browser.close(); }
  console.log("shots: " + path.join(SHOTS, "memory-{desk,desk-open,phone,phone-open,proposals-desk,proposals-phone}.png"));
})().catch((e) => { console.error(e); process.exit(1); });
