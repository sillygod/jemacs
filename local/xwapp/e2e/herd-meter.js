// What each agent runs on and how full its context is -- a chip and a
// bar -- and the chip's form that switches a claude's model for its
// session alone, on a phone and at a desk: on the Agents tab, in a
// room's columns, and in its chat for the agent the box writes to.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const LEAD = "lead-deploy-platform";
const RD = "claude-deploy-platform";
const QA = "qa-deploy-platform";

const meter = (model, effort, tokens, window, extra) =>
  Object.assign({ model, effort, tokens, window, pending: false, compacted: false }, extra || {});
const PICKERS = { claude: { models: ["Default", "Opus", "Fable", "Sonnet", "Haiku"],
                            efforts: ["low", "medium", "high", "xhigh", "max"] } };
const herd = (meters, extra) => Object.assign({}, HERD, {
  pickers: PICKERS,
  agents: HERD.agents.map((a) => Object.assign({}, a, { meter: meters[a.name] || false, switching: false }, (extra || {})[a.name] || {})),
});
const METERS = {
  [LEAD]: meter("Opus 5.5", "xhigh", 380944, 1000000),
  [RD]: meter("Opus 5.5", "xhigh", 833282, 1000000),
  // An agy: no meter, whatever it is sent.
};

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const show = (h) => page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, h);
      const row = (name) => '.arow[data-name="' + name + '"]';
      const read = (scope) => page.$eval(scope, (r) => {
        const m = r.querySelector(".meter");
        if (!m) return null;
        const chip = m.querySelector(".mchip");
        const fill = m.querySelector(".mbar span");
        return { chip: chip.textContent, button: chip.tagName === "BUTTON", pending: chip.classList.contains("pending"),
                 words: (m.querySelector(".mwords") || {}).textContent || "", level: fill ? fill.className : "",
                 width: fill ? fill.style.width : "", form: !!r.querySelector(".modelform") };
      });
      await show(herd(METERS));
      await sleep(100);

      // ---- Seen ----
      const lead = await read(row(LEAD));
      check(W("its model and effort, and its context of its window"),
        lead.chip === "Opus 5.5xhigh" && lead.words === "38% · 381k / 1M" && lead.width === "38%" && lead.level === "lvl-ok", lead);
      const rd = await read(row(RD));
      check(W("coloured as the usage bars are: past 70%"), rd.level === "lvl-warm" && rd.words === "83% · 833k / 1M", rd);
      check(W("no meter for an agent without one"), (await read(row(QA))) === null);
      await show(herd(Object.assign({}, METERS, {
        [LEAD]: meter("Opus 4.6", "high", 230000, 200000),
        [RD]: meter("Sonnet 5.5", "", 833282, 1000000, { pending: true }),
        [QA]: meter("Opus 5.5", "xhigh", null, 1000000, { compacted: true }),
      })));
      await sleep(100);
      const full = await read(row(LEAD));
      check(W("past its window: red, the bar full, the number as it is"),
        full.level === "lvl-hot" && full.width === "100%" && full.words === "115% · 230k / 200k", full);
      const pending = await read(row(RD));
      check(W("a switch since its last reply: the model its next one is on, marked"),
        pending.pending && pending.chip === "Sonnet 5.5" && pending.words === "83% · 833k / 1M", pending);
      const compacted = await read(row(QA));
      check(W("compacted since: no size, and no button on a CLI the herd cannot switch"),
        compacted.words === "compacted" && compacted.level === "" && !compacted.button, compacted);

      // ---- Switched ----
      await show(herd(METERS));
      await sleep(100);
      await tap(row(LEAD) + " .mchip");
      await sleep(100);
      const picked = await page.$$eval(row(LEAD) + " .modelform .mopt.on", (b) => b.map((x) => x.textContent));
      check(W("the form opens on what it runs on now"), JSON.stringify(picked) === '["Opus","xhigh"]', picked);
      const note = await page.$eval(row(LEAD) + " .mnote", (n) => n.textContent);
      check(W("and says it is for this session only"), /This session only/.test(note), note);
      await tap(row(LEAD) + ' .mopt[data-model="Sonnet"]');
      await tap(row(LEAD) + ' .mopt[data-effort="high"]');
      // Drawn again, as the page is every few seconds: the picks stay.
      await show(herd(METERS));
      await sleep(100);
      const kept = await page.$$eval(row(LEAD) + " .modelform .mopt.on", (b) => b.map((x) => x.textContent));
      check(W("what was picked outlives a redraw"), JSON.stringify(kept) === '["Sonnet","high"]', kept);
      await tap(row(LEAD) + ' [data-act="a:model-go"]');
      await sleep(100);
      let sent = intents.filter((i) => i.op === "agent-model");
      check(W("Switch tells Emacs: that agent, that model, that effort"),
        sent.length === 1 && sent[0].name === LEAD && sent[0].model === "Sonnet" && sent[0].effort === "high", sent);
      check(W("and the form closes"), !(await read(row(LEAD))).form);
      // An effort picked again is unpicked: the picker's own.
      await tap(row(LEAD) + " .mchip");
      await tap(row(LEAD) + ' .mopt[data-effort="xhigh"]');
      await tap(row(LEAD) + ' [data-act="a:model-go"]');
      await sleep(100);
      sent = intents.filter((i) => i.op === "agent-model");
      check(W("an effort unpicked is left as the picker has it"), sent.length === 2 && sent[1].model === "Opus" && sent[1].effort === "", sent[1]);
      // Cancel tells nothing.
      await tap(row(LEAD) + " .mchip");
      await tap(row(LEAD) + ' [data-act="a:model-cancel"]');
      await sleep(100);
      check(W("Cancel closes it and tells Emacs nothing"),
        !(await read(row(LEAD))).form && intents.filter((i) => i.op === "agent-model").length === 2);
      // At work: the form says why it cannot yet.
      await tap(row(RD) + " .mchip");
      await sleep(100);
      const busy = await page.$eval(row(RD), (r) => ({ go: r.querySelector('[data-act="a:model-go"]').disabled,
                                                       note: r.querySelector(".mnote").textContent }));
      check(W("an agent at work: Switch held, and why"), busy.go && /It is working/.test(busy.note), busy);
      await tap(row(RD) + ' [data-act="a:model-cancel"]');
      // While it switches: no second switch.
      await show(herd(METERS, { [LEAD]: { switching: true } }));
      await sleep(100);
      const switching = await read(row(LEAD));
      check(W("while it switches, it says so and is not a button"), !switching.button && /switching…/.test(switching.chip), switching);
      if (where === "phone") {
        const h = await page.$eval(row(RD) + " .mchip", (b) => b.getBoundingClientRect().height);
        check(W("the chip big enough for a finger"), h >= 32, h);
      }

      // ---- In a room ----
      await show(herd(METERS));
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      const col = '.rcol[data-name="' + LEAD + '"] .rtop';
      const head = await read(col);
      check(W("a column's head has its meter"), head && head.words === "38% · 381k / 1M", head);
      await tap(col + " .mchip");
      await tap(col + ' .mopt[data-model="Haiku"]');
      await tap(col + ' [data-act="a:model-go"]');
      await sleep(100);
      sent = intents.filter((i) => i.op === "agent-model");
      check(W("switched from its column"), sent.length === 3 && sent[2].name === LEAD && sent[2].model === "Haiku", sent[2]);

      // ---- In its chat ----
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(150);
      const chat = await read(".chat .cmeter");
      check(W("the chat shows the meter of whom its box writes to"), chat && chat.words === "38% · 381k / 1M", chat);
      await page.selectOption(".chat .chat-to", RD);
      await sleep(100);
      const other = await read(".chat .cmeter");
      check(W("and of the next one, once the box writes to it"), other && other.words === "83% · 833k / 1M", other);
      await page.selectOption(".chat .chat-to", QA);
      await sleep(100);
      check(W("none for one without"), (await page.$eval(".chat .cmeter", (m) => getComputedStyle(m).display)) === "none");
      await page.selectOption(".chat .chat-to", LEAD);
      await sleep(100);
      await tap(".chat .cmeter .mchip");
      await tap('.chat .cmeter .mopt[data-model="Fable"]');
      await tap('.chat .cmeter .mopt[data-effort="max"]');
      await tap('.chat .cmeter [data-act="a:model-go"]');
      await sleep(100);
      sent = intents.filter((i) => i.op === "agent-model");
      check(W("switched from the chat, for whom the box writes to"),
        sent.length === 4 && sent[3].name === LEAD && sent[3].model === "Fable" && sent[3].effort === "max", sent[3]);

      const wide = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
      check(W("no sideways scroll"), wide <= 0, wide);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const bad = results.filter((r) => !r).length;
  console.log(bad ? bad + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(bad ? 1 : 0);
})();
