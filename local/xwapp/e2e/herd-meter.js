// What each agent runs on and how full its context is -- a chip and a
// bar -- and the chip's form that switches a model: a claude's for its
// session alone, grok's and agy's as their default (they have a picker
// but no meter, so a bare "Model" button with no model to show).  On a
// phone and at a desk: on the Agents tab, in a room's columns, and in
// its chat for the agent the box writes to.
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
const PICKERS = {
  claude: { models: ["Default", "Opus", "Fable", "Sonnet", "Haiku"],
            efforts: ["low", "medium", "high", "xhigh", "max"], scope: "session" },
  grok: { models: ["grok-4.7", "grok-4.7-build-fast", "grok-4.6", "grok-4.5"],
          efforts: ["low", "medium", "high", "xhigh"], scope: "default" },
  agy: { models: ["Gemini 4 Argon", "Gemini 3.8 Flash", "Claude Opus 5.5", "Claude Sonnet 5.5", "GPT-OSS 120B"],
         efforts: ["low", "medium", "high"], scope: "default" },
};
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
      // agy reports no meter, but the herd knows its picker, so a bare
      // Model button stands in -- a switch, with no context to show.
      const noMeter = await read(row(QA));
      check(W("no meter for an agent without one: a bare switch, no context"),
        noMeter && noMeter.button && noMeter.chip === "Model" && noMeter.words === "" && noMeter.width === "", noMeter);
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
      check(W("compacted since: no size, and -- agy being switchable -- a button all the same"),
        compacted.words === "compacted" && compacted.level === "" && compacted.button, compacted);
      // A kind whose picker the herd does not know: a meter, but no button.
      await show(Object.assign(herd(Object.assign({}, METERS, {
        [QA]: meter("whatever", "", null, 1000000, { compacted: true }) })),
        { pickers: { claude: PICKERS.claude } }));
      await sleep(100);
      check(W("no button on a CLI whose picker the herd does not know"), !(await read(row(QA))).button);

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
      // Waiting on the sub-agents it handed work to: its box is free.
      await show(herd(METERS, { [RD]: { state: "waiting", reason: "waiting on 4 sub-agents" } }));
      await sleep(100);
      await tap(row(RD) + " .mchip");
      await sleep(100);
      const waiting = await page.$eval(row(RD), (r) => {
        const st = r.querySelector(".amain .st");
        return { go: r.querySelector('[data-act="a:model-go"]').disabled, badge: st.textContent,
                 colour: getComputedStyle(st).backgroundColor, edge: getComputedStyle(r).borderLeftColor };
      });
      check(W("waiting: a badge and colour of its own, and Switch open to it"),
        !waiting.go && waiting.badge === "waiting" && waiting.colour === "rgb(57, 197, 207)" && waiting.edge === waiting.colour, waiting);
      const tally = await page.$$eval('.pgroup[data-project="' + DP + '"] .phead .st', (l) => l.map((s) => s.textContent));
      check(W("counted after those at work, before the idle"), JSON.stringify(tally) === '["1 blocked","1 waiting","1 idle"]', tally);
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
      // Its ask of qa answered, and it waiting on its sub-agents since.
      const asked = { id: "k1", run: "k1", from: RD, to: QA, status: "answered", auto: false,
                      created: Date.now() - 120e3, answered: Date.now() - 60e3, replyHead: "all green" };
      await page.evaluate((h) => GM.renderHerd(h), Object.assign(herd(METERS, { [RD]: { state: "waiting" } }), { asks: [asked] }));
      await sleep(150);
      const top = await page.$eval('.rcol[data-name="' + RD + '"]', (c) => getComputedStyle(c).borderTopColor);
      check(W("a waiting agent's column says so in its colour"), top === "rgb(57, 197, 207)", top);
      const run = await page.$eval(".room .rrun", (r) => r.textContent);
      check(W("and its run is not done: it is at work"), run.includes("answered; " + RD + " at work"), run);
      await page.evaluate((h) => GM.renderHerd(h), herd(METERS));
      await sleep(100);

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
      const chatBare = await read(".chat .cmeter");
      check(W("a bare Model button for one with a picker but no meter"),
        chatBare && chatBare.button && chatBare.chip === "Model", chatBare);
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

      // ---- grok and agy: switchable with no meter of their own ----
      await show(herd(METERS, { [QA]: { state: "idle" } }));   // QA is agy; METERS gives it none
      await sleep(100);
      const agy = await read(row(QA));
      const agyBare = await page.$eval(row(QA) + " .mchip", (b) => b.classList.contains("mbare"));
      check(W("agy: no meter, still a bare Model button to switch by"),
        agy && agy.button && agy.chip === "Model" && agyBare, { agy, agyBare });
      await tap(row(QA) + " .mchip");
      await sleep(100);
      const agyNote = await page.$eval(row(QA) + " .mnote", (n) => n.textContent);
      check(W("its form says the switch is the CLI's default, not the session"),
        /becomes the CLI's default/.test(agyNote), agyNote);
      await tap(row(QA) + ' .mopt[data-model="Claude Opus 5.5"]');
      await tap(row(QA) + ' .mopt[data-effort="high"]');
      await tap(row(QA) + ' [data-act="a:model-go"]');
      await sleep(100);
      const agySent = intents.filter((i) => i.op === "agent-model" && i.name === QA);
      check(W("agy switched: its name, model and effort"),
        agySent.length === 1 && agySent[0].model === "Claude Opus 5.5" && agySent[0].effort === "high", agySent);
      // grok, in another project, offers the switch the same way.
      const grokRow = '.arow[data-name="grok-jnotes"]';
      const grok = await read(grokRow);
      check(W("grok too: a bare Model button, no meter"), grok && grok.button && grok.chip === "Model", grok);
      await tap(grokRow + " .mchip");
      await sleep(100);
      const grokModels = await page.$$eval(grokRow + ' .modelform .mopt[data-model]', (b) => b.map((x) => x.textContent));
      check(W("grok's own models are offered"),
        grokModels.includes("grok-4.6") && grokModels.includes("grok-4.7"), grokModels);
      await tap(grokRow + ' [data-act="a:model-cancel"]');
      // Once its model is read from its screen (a meter with no window),
      // the chip shows it and stays a switch -- no context bar.
      await show(herd(Object.assign({}, METERS, { "grok-jnotes": meter("Grok 4.6", "high", null, null) })));
      await sleep(100);
      const grokLit = await read(grokRow);
      check(W("a read model shows in the chip, still a switch, no bar"),
        grokLit && grokLit.button && grokLit.chip === "Grok 4.6high" && grokLit.words === "" && grokLit.width === "", grokLit);
      // grok carries a context window (from signals.json) -> a percent bar.
      await show(herd(Object.assign({}, METERS, { "grok-jnotes": meter("Grok 4.6", "high", 163889, 256000) })));
      await sleep(100);
      const grokCtx = await read(grokRow);
      check(W("grok with a window shows a bar and percent"),
        grokCtx && grokCtx.words === "64% · 164k / 256k" && grokCtx.width === "64%" && grokCtx.level === "lvl-ok", grokCtx);
      // agy has no window total -> the number alone, no bar.
      await show(herd(Object.assign({}, METERS, { [QA]: meter("Gemini 3.8 Flash", "medium", 189225, null) })));
      await sleep(100);
      const agyCtx = await read(row(QA));
      check(W("agy without a window shows the number, no bar"),
        agyCtx && agyCtx.words === "189k" && agyCtx.width === "" && agyCtx.chip === "Gemini 3.8 Flashmedium", agyCtx);

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
