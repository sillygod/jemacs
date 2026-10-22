// Screenshots of the Threads tab from fixture data (desk + phone), for review.
const { webkit } = require("playwright");
const fs = require("fs"); const path = require("path");
const { phonePage, sleep } = require("./herdlib");
const { LIST, serve } = require("./threads-fixture");
const SHOTS = path.join(__dirname, "shots");
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };

async function shot(browser, name, ctxOpts, steps) {
  const intents = [];
  const { ctx, page } = await phonePage(browser, { intents, context: ctxOpts });
  const stop = serve(page, intents);
  await sleep(150);
  await page.evaluate(() => document.querySelector('.tab[data-tab="threads"]').click());
  await page.evaluate((l) => GM.renderThreads(l), LIST);
  await sleep(250);
  for (const s of steps || []) { await page.click(s); await sleep(250); }
  await page.screenshot({ path: path.join(SHOTS, name) });
  stop();
  await ctx.close();
}
(async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const browser = await webkit.launch();
  try {
    await shot(browser, "threads-desk.png", DESK, ['.throw[data-id="t2"]', '.thcard[data-i="0"]']);
    await shot(browser, "threads-desk-waiting.png", DESK, ['.throw[data-id="t3"]']);
    await shot(browser, "threads-phone.png", PHONE, []);
    await shot(browser, "threads-phone-thread.png", PHONE, ['.throw[data-id="t2"]']);
  } finally { await browser.close(); }
  console.log("shots: " + path.join(SHOTS, "threads-{desk,desk-waiting,phone,phone-thread}.png"));
})().catch((e) => { console.error(e); process.exit(1); });
