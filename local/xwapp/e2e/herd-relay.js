// The herd page's relay, on a phone: an agent that took over another's
// name shows its place in the line; one whose CLI is used up offers its
// relay first; Relay asks which CLI, the herd's choice the one with the
// keys, and tells Emacs.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

const herd = () => {
  const h = JSON.parse(JSON.stringify(HERD));
  h.relayKinds = [{ kind: "claude", usedUp: "its claude usage (5h) is used up, until 15:00" },
                  { kind: "grok", usedUp: "" }, { kind: "agy", usedUp: "" }];
  h.agents.forEach((a) => Object.assign(a, { generation: 1, line: [], relayTo: a.kind === "grok" ? "claude" : "grok", usedUp: "" }));
  const rd = h.agents.find((a) => a.name === "claude-deploy-platform");
  Object.assign(rd, { state: "idle", usedUp: "its claude usage (5h) is used up, until 15:00", relayTo: "grok" });
  const heir = h.agents.find((a) => a.name === "grok-jnotes");
  Object.assign(heir, { generation: 2, line: ["claude"] });
  return h;
};
const row = (page, name) => page.$('.arow[data-name="' + name + '"]');

(async () => {
  const browser = await webkit.launch();
  try {
    const intents = [];
    const { ctx, page, errors } = await phonePage(browser, { intents });
    await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
    await sleep(100);

    const gen = await page.$eval('.arow[data-name="grok-jnotes"] .genb', (b) => ({ text: b.textContent, title: b.title })).catch(() => null);
    check("an heir shows its place in the line, and the CLIs of its name", gen && gen.text === "II" &&
      gen.title === "grok-jnotes II, the CLIs of its name: claude → grok", gen);
    check("the first of a name shows none", !(await page.$('.arow[data-name="lead-deploy-platform"] .genb')));

    const first = await page.$eval('.arow[data-name="claude-deploy-platform"] .aacts > button', (b) => ({ text: b.textContent, cls: b.className, title: b.title }));
    check("used up, its relay comes first", first.text === "Relay to grok" && /primary/.test(first.cls) && /used up/.test(first.title), first);
    check("not used up, none up front", !(await page.$('.arow[data-name="lead-deploy-platform"] .aacts > [data-act="a:relay"]')));

    await page.tap('.arow[data-name="claude-deploy-platform"] .aacts > [data-act="a:relay"]');
    await sleep(100);
    const dlg = await page.$eval(".ask", (d) => ({
      msg: d.querySelector(".ask-msg").textContent,
      detail: d.querySelector(".ask-detail").textContent,
      buttons: [...d.querySelectorAll("button")].map((b) => [b.textContent, b.className]),
      focused: document.activeElement && document.activeElement.textContent,
      right: d.getBoundingClientRect().right, width: document.documentElement.clientWidth,
    }));
    check("it asks which CLI", dlg.msg === "Relay claude-deploy-platform to…" &&
      /^Its claude usage \(5h\) is used up, until 15:00\. A new agent takes its name/.test(dlg.detail), dlg);
    check("each CLI, the used-up one said, its own CLI again; the herd's choice last, with the keys",
      JSON.stringify(dlg.buttons) === JSON.stringify([["Cancel", "ghost"], ["claude again · used up", "ghost"], ["agy", "ghost"], ["grok", "primary"]]) &&
      dlg.focused === "grok", dlg);
    check("the question fits the phone", dlg.right <= dlg.width, dlg);
    await page.tap(".ask button.primary");
    await sleep(200);
    const sent = intents.filter((i) => i.op === "agent-relay");
    check("Emacs is told: that agent, that CLI", sent.length === 1 && sent[0].name === "claude-deploy-platform" && sent[0].kind === "grok", intents);

    // Any agent: under more.
    await page.tap('.arow[data-name="lead-deploy-platform"] [data-act="a:more"]');
    await sleep(100);
    await page.tap('.arow[data-name="lead-deploy-platform"] .more [data-act="a:relay"]');
    await sleep(100);
    const lead = await page.$$eval(".ask button", (bs) => bs.map((b) => b.textContent));
    check("any agent's relay is under more", JSON.stringify(lead) === JSON.stringify(["Cancel", "claude again · used up", "agy", "grok"]), lead);
    await page.tap(".ask button.ghost");
    await sleep(100);
    check("Cancel tells Emacs nothing", intents.filter((i) => i.op === "agent-relay").length === 1 && !(await page.$(".ask")));
    check("no page errors", errors.length === 0, errors);
    await ctx.close();
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
