// Primary, the herd-wide lead, on the page: the Agents tab's header opens
// its room -- a chat, there before primary runs, whose first message
// starts it -- and once it runs, the bar says what it is doing and the
// chat writes to it.  On a phone and at a desk.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PD = "~/.ghostherd/primary/";
const PRIMARY = { directory: PD, name: "primary", kind: "claude", state: "" };

const herd = (running) => {
  const h = JSON.parse(JSON.stringify(HERD));
  h.primary = Object.assign({}, PRIMARY, running ? { state: "working" } : {});
  if (running) {
    h.agents.push(Object.assign({}, h.agents[0], {
      name: "primary", kind: "claude", role: "primary", project: PD, state: "working",
      notes: "primary: the herd-wide lead -- routes work to projects, tracks it, reports",
      reason: "Handing the login fix to deploy-platform's lead", since: Date.now() - 30e3, links: false }));
  }
  return h;
};

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const txt = (sel) => page.$eval(sel, (e) => e.textContent).catch(() => null);
      const show = (h) => page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, h);
      const sent = (op) => intents.filter((i) => i.op === op);

      // ---- Not running: the header says so, the room starts it ----
      await show(herd(false));
      await sleep(120);
      const bar = await page.$eval(".primarybar .primary-open", (b) => {
        const r = b.getBoundingClientRect();
        return { text: b.textContent, right: r.right, width: document.documentElement.clientWidth,
                 above: !!document.querySelector("#primarybar + #herd"), h: r.height };
      }).catch(() => null);
      check(W("the Agents tab's header has primary, before the projects"),
        bar && bar.above && /^Primary/.test(bar.text) && /not running · writing to it starts it/.test(bar.text), bar);
      check(W("it fits the screen"), bar && bar.right <= bar.width, bar);
      if (where === "phone") check(W("big enough for a finger"), bar && bar.h >= 44, bar);
      await tap('[data-act="a:primary-room"]');
      await sleep(200);
      check(W("its room is primary's, named so"), (await txt(".room .rhead .pname")) === "Primary");
      check(W("a chat, not columns: no view toggle"),
        !!(await page.$(".room .chat")) && !(await page.$('.room [data-act="a:room-view"]')));
      // The room must ASK Emacs for the chat, not the device-default view: on
      // the desktop that default is "columns", and the live bug was Emacs then
      // reading columns and the chat staying "Reading the conversations…".
      const rw = sent("room-watch").pop();
      check(W("the room asks Emacs for the chat whatever the device default"),
        rw && rw.project === PD && rw.view === "chat", rw);
      check(W("it says primary is not running, and how to start it"),
        /Primary is not running\.  Write to it below/.test(await txt(".chat .cbody")));
      const to = await page.$eval(".chat .chat-to", (s) => [...s.options].map((o) => [o.value, o.textContent]));
      check(W("the box writes to primary, which starts when you write"),
        JSON.stringify(to) === JSON.stringify([["@primary", "primary (starts when you write)"]]), to);
      check(W("its placeholder says so"), (await page.$eval(".chat textarea", (t) => t.placeholder)).startsWith("Message primary"));
      await page.fill(".chat textarea", "what is everyone doing?");
      await tap('.chat [data-act="a:send"]');
      await sleep(120);
      check(W("sending tells Emacs: primary-say, the words"),
        JSON.stringify(sent("primary-say")) === JSON.stringify([{ op: "primary-say", text: "what is everyone doing?" }]),
        sent("primary-say"));
      check(W("and no lead is started for its room"), sent("lead-say").length === 0);

      // ---- Running: the header shows what it does; the chat writes to it ----
      await page.evaluate((h) => GM.renderHerd(h), herd(true));
      await sleep(120);
      const to2 = await page.$eval(".chat .chat-to", (s) => [...s.options].map((o) => o.value));
      check(W("up, the box writes to primary itself"), JSON.stringify(to2) === JSON.stringify(["primary"]), to2);
      // The live bug: on the desktop the primary room's roomView is "columns"
      // (it has no toggle), so setChat took the conversation but never painted
      // it -- the render keyed on roomView, not that the room is a chat.  The
      // chat must fill with primary's conversation, not sit on "Reading…".
      await page.evaluate((pd) => GM.setChat({ project: pd, earlier: false, missing: [], absent: [], entries: [
        { who: "user", to: "primary", kind: "turn", ts: "p1", text: "what is everyone doing?" },
        { who: "primary", kind: "turn", ts: "p2", text: "jnotes is idle; deploy-platform is mid-deploy." }] }), PD);
      await sleep(80);
      const cbody = await txt(".chat .cbody");
      check(W("the chat paints primary's conversation, not stuck on Reading…"),
        /what is everyone doing\?/.test(cbody) && /jnotes is idle/.test(cbody) &&
        !/Reading the conversations/.test(cbody), cbody.slice(0, 80));
      await page.fill(".chat textarea", "and the login?");
      await tap('.chat [data-act="a:send"]');
      await sleep(120);
      const prompts = sent("agent-prompt");
      check(W("to the agent, as any prompt"), prompts.length === 1 && prompts[0].name === "primary" && prompts[0].text === "and the login?", prompts);
      await page.evaluate(() => GM.showView("agents"));
      await sleep(120);
      const bar2 = await txt(".primarybar .primary-open");
      check(W("the header shows its state and what it is doing"),
        /working/.test(bar2) && /Handing the login fix/.test(bar2), bar2);
      const group = '.pgroup[data-project="' + PD + '"]';
      check(W("its own group offers no + New agent"),
        !!(await page.$(group)) && !(await page.$(group + ' [data-act="a:new-agent"]')));
      check(W("a project's group still does"), !!(await page.$('.pgroup[data-project="~/src/deploy-platform/"] [data-act="a:new-agent"]')));
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
