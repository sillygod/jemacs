// What you put in a form on the herd page survives the page being drawn
// again -- every 5s, and whenever Emacs has news -- whether or not the
// form still has the keys: in Emacs's WebKit a click elsewhere, or a
// popup menu, takes them, and the next drawing used to put back an
// empty form, its first CLI chosen.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const draw = () => page.evaluate((h) => GM.renderHerd(h), HERD);
      const away = () => page.evaluate(() => document.activeElement && document.activeElement.blur());
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(100);
      const group = '.pgroup[data-project="~/src/deploy-platform/"]';

      // ---- A new agent ----
      await tap(group + ' [data-act="a:new-agent"]');
      await page.selectOption(group + " .new-kind", "grok");
      await page.fill(group + " .new-name", "rd");
      await away();
      await draw();
      await sleep(50);
      const form = async () => page.$eval(group + " .newform", (f) => ({
        kind: f.querySelector(".new-kind").value, name: f.querySelector(".new-name").value })).catch(() => null);
      check(W("the CLI picked and the name typed survive a drawing"), JSON.stringify(await form()) === JSON.stringify({ kind: "grok", name: "rd" }), await form());
      // Picked again after the drawing: what it says now.
      await page.selectOption(group + " .new-kind", "agy");
      await draw();
      check(W("and the CLI picked after it"), (await form()).kind === "agy", await form());
      await tap(group + ' [data-act="a:new-go"]');
      await sleep(150);
      const made = intents.filter((i) => i.op === "agent-new");
      check(W("Start: that CLI, that name"), made.length === 1 && made[0].kind === "agy" && made[0].name === "rd" &&
        made[0].project === "~/src/deploy-platform/", made);
      await tap(group + ' [data-act="a:new-agent"]');
      check(W("opened again, a fresh form"), JSON.stringify(await form()) === JSON.stringify({ kind: "claude", name: "" }), await form());
      await page.selectOption(group + " .new-kind", "grok");
      await tap(group + ' [data-act="a:new-cancel"]');
      await tap(group + ' [data-act="a:new-agent"]');
      check(W("Cancel forgets it too"), JSON.stringify(await form()) === JSON.stringify({ kind: "claude", name: "" }), await form());
      await tap(group + ' [data-act="a:new-cancel"]');

      // ---- A note ----
      const row = '.arow[data-name="claude-deploy-platform"]';
      await tap(row + ' [data-act="a:more"]');
      await tap(row + ' [data-act="a:notes"]');
      await page.fill(row + " .notes-text", "implementer, auth only");
      await away();
      await draw();
      await sleep(50);
      const note = await page.$eval(row + " .notes-text", (i) => i.value).catch(() => null);
      check(W("a note being written survives a drawing"), note === "implementer, auth only", note);
      await tap(row + ' [data-act="a:notes-save"]');
      await sleep(150);
      const saved = intents.filter((i) => i.op === "agent-notes");
      check(W("Save: what was written"), saved.length === 1 && saved[0].text === "implementer, auth only", saved);
      await tap(row + ' [data-act="a:notes"]');
      const again = await page.$eval(row + " .notes-text", (i) => i.value).catch(() => null);
      check(W("opened again, the agent's notes as they are"), again === "implementer", again);
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
