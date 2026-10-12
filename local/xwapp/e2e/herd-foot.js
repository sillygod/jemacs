// What claude shows only on its screen, on the herd page: the work it
// has running in the background, and the prompt it suggests you send
// next -- the box's placeholder, put in by Use (or Tab at the desk), in
// an agent's row, its room column and the room's chat.  Gone from the
// screen, gone from the page; what you typed stays.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const RD = "claude-deploy-platform";
const LEAD = "lead-deploy-platform";
const SAID = "Run the retry tests again";

const herd = (patch) => {
  const h = JSON.parse(JSON.stringify(HERD));
  h.agents.forEach((a) => Object.assign(a, { background: [], suggestion: "" }));
  Object.assign(h.agents.find((a) => a.name === RD), { state: "idle", background: ["1 shell"], suggestion: SAID });
  if (patch) patch(h);
  return h;
};
const box = (page, sel) => page.$eval(sel, (b) => ({
  hint: b.querySelector("textarea.agent-text").placeholder, value: b.querySelector("textarea.agent-text").value,
  use: !!b.querySelector(".use") && !b.querySelector(".use").hidden })).catch(() => null);

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const draw = (h) => page.evaluate((h) => GM.renderHerd(h), h);
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(100);
      const row = '.arow[data-name="' + RD + '"]';

      // ---- An agent's row ----
      const bg = await page.$eval(row + " .bgw", (b) => b.textContent).catch(() => null);
      check(W("work in the background, on its row"), bg === "1 shell running", bg);
      check(W("none where there is none"), !(await page.$('.arow[data-name="' + LEAD + '"] .bgw')));
      await tap(row + ' [data-act="a:compose"]');
      await sleep(80);
      let b = await box(page, row);
      const hint = "↳ " + SAID + (where === "desk" ? "  (Tab takes it)" : "");
      check(W("what it suggests is its box's placeholder, and Use is there"), b && b.hint === hint && b.use && b.value === "", b);
      if (where === "phone") {
        await tap(row + ' [data-act="a:use-suggestion"]');
      } else {
        await page.focus(row + " textarea.agent-text");
        await page.keyboard.press("Tab");
      }
      await sleep(50);
      b = await box(page, row);
      check(W(where === "phone" ? "Use puts it in the box" : "Tab in the empty box puts it in"), b.value === SAID, b);
      if (where === "desk") {
        await page.keyboard.type(", slower");
        await page.keyboard.press("Tab");
        b = await box(page, row);
        check(W("Tab in a box with words in it is Tab"), b.value === SAID + ", slower", b);
      }
      await tap(row + ' [data-act="a:send"]');
      await sleep(150);
      const sent = intents.filter((i) => i.op === "agent-prompt");
      check(W("sent as a prompt you wrote"), sent.length === 1 && sent[0].name === RD && sent[0].text.startsWith(SAID), sent);
      await tap('.arow[data-name="' + LEAD + '"] [data-act="a:compose"]');
      await sleep(80);
      b = await box(page, '.arow[data-name="' + LEAD + '"]');
      check(W("an agent that suggests nothing: as before"), b && /^Prompt for lead-deploy-platform/.test(b.hint) && !b.use, b);

      // ---- Its room column ----
      await tap('.pgroup[data-project="~/src/deploy-platform/"] [data-act="a:room"]');
      await sleep(150);
      if (await page.$(".room .chat")) {
        await tap('.room [data-act="a:room-view"][data-view="columns"]');
        await sleep(150);
      }
      const col = '.rcol[data-name="' + RD + '"]';
      check(W("in its column too"), (await page.$eval(col + " .bgw", (x) => x.textContent).catch(() => null)) === "1 shell running");
      b = await box(page, col);
      check(W("its column's box, the same"), b && b.hint === hint && b.use, b);
      await page.fill(col + " textarea.agent-text", "my own words");
      // The screen says no more: the column, drawn again in place, follows.
      await draw(herd((h) => Object.assign(h.agents.find((a) => a.name === RD), { background: [], suggestion: "" })));
      await sleep(100);
      b = await box(page, col);
      check(W("gone from the screen, gone from the box -- what was typed stays"),
        b && /^Prompt for claude-deploy-platform/.test(b.hint) && !b.use && b.value === "my own words", b);
      check(W("and the work in the background"), !(await page.$(col + " .bgw")));

      // ---- The room's chat ----
      await draw(herd());
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(150);
      await page.selectOption(".chat-to", RD);
      await sleep(50);
      b = await box(page, ".cfoot");
      check(W("in the chat, writing to it"), b && b.hint === hint && b.use, b);
      await page.selectOption(".chat-to", LEAD);
      await sleep(50);
      b = await box(page, ".cfoot");
      check(W("writing to another, its own hint"), b && /^Message lead-deploy-platform/.test(b.hint) && !b.use, b);
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
