// Screenshots of the Agents tab's Primary spot from fixture data (desk +
// phone): the pinned card when primary runs, and the bar when it does not.
const { webkit } = require("playwright");
const fs = require("fs"); const path = require("path");
const { phonePage, HERD, sleep } = require("./herdlib");
const SHOTS = path.join(__dirname, "shots");
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };
const PD = "~/.ghostherd/primary/";

const herd = (running) => {
  const h = JSON.parse(JSON.stringify(HERD));
  h.primary = { directory: PD, name: "primary", kind: "claude", state: running ? "working" : "" };
  if (running) {
    h.agents.push(Object.assign({}, h.agents[0], {
      name: "primary", kind: "claude", role: "primary", project: PD, state: "working",
      notes: "primary: the herd-wide lead -- routes work to projects, tracks it, reports",
      reason: "Handing the login fix to deploy-platform's lead",
      meter: { model: "Opus 5.5", effort: "xhigh", tokens: 32000, window: 200000, pending: false },
      said: "jnotes（~/Downloads/mycrafts/jnotes/）目前沒有在做任何事。", saidAt: Date.now() - 90e3,
      since: Date.now() - 90e3, links: false, work: false }));
  }
  return h;
};

async function shot(browser, name, ctxOpts, running) {
  const { ctx, page } = await phonePage(browser, { context: ctxOpts });
  await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd(running));
  await sleep(150);
  await page.screenshot({ path: path.join(SHOTS, name) });
  await ctx.close();
}

(async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const browser = await webkit.launch();
  try {
    await shot(browser, "primary-running-desk.png", DESK, true);
    await shot(browser, "primary-running-phone.png", PHONE, true);
    await shot(browser, "primary-idle-desk.png", DESK, false);
    await shot(browser, "primary-idle-phone.png", PHONE, false);
  } finally { await browser.close(); }
  console.log("shots: " + path.join(SHOTS, "primary-{running,idle}-{desk,phone}.png"));
})().catch((e) => { console.error(e); process.exit(1); });
