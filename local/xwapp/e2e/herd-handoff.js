// A handoff on the cards: deploy-platform's lead hands work to jnotes'
// lead.  The sender's card says which project it waits on, the taker's
// which project waits on it -- on the Agents tab and in a room's columns
// and its runs, on a phone and at a desk; while the herd starts a lead
// for it, that it is starting; then the answer, or why it failed.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const JN = "~/src/jnotes/";
const UN = "~/src/unnoiac/";
const LEAD = "lead-deploy-platform";
const JLEAD = "lead-jnotes";

const now = Date.now();
const herd = (asks) => {
  const h = JSON.parse(JSON.stringify(HERD));
  h.agents.push(Object.assign({}, h.agents.find((a) => a.name === "grok-jnotes"),
    { name: JLEAD, kind: "claude", role: "lead", notes: "lead: plans the work, hands it out", state: "working" }));
  h.asks = asks;
  return h;
};
const handoff = (extra) => Object.assign({
  id: "h1", run: "h1", from: LEAD, to: JLEAD, status: "delivered", auto: false, error: "",
  head: "The notes API renamed `token' to `access_token'; deploy-platform's sync calls it.",
  replyHead: "", created: now - 90e3, answered: null, parent: null, round: null, of: null,
  project: JN, fromProject: DP }, extra || {});

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const { ctx, page, errors } = await phonePage(browser, { context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const show = (asks) => page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd(asks));
      const line = (scope) => page.$eval(scope + " .aask.ask-handoff", (l) => ({
        what: l.querySelector(".awhat").textContent,
        text: (l.querySelector(".atext") || {}).textContent || "",
        title: l.title, cls: l.className,
        // Its label whole, and inside the card.
        whole: l.querySelector(".awhat").scrollWidth <= l.querySelector(".awhat").clientWidth,
        right: l.querySelector(".awhat").getBoundingClientRect().right, width: l.getBoundingClientRect().right,
      })).catch(() => null);
      const row = (name) => '.arow[data-name="' + name + '"]';
      const room = async (project) => {
        await page.evaluate(() => GM.showView("agents"));
        await sleep(50);
        await tap('.pgroup[data-project="' + project + '"] [data-act="a:room"]');
        await sleep(150);
        await tap('.room [data-act="a:room-view"][data-view="columns"]');
        await sleep(150);
      };
      const col = (name) => '.rcol[data-name="' + name + '"] .rtop';
      const runs = () => page.$$eval(".room .rrun", (rs) => rs.map((r) => r.textContent));

      // ---- Handed off, its lead at work on it ----
      await show([handoff()]);
      await sleep(100);
      const sent = await line(row(LEAD));
      check(W("the sender's card: waiting on the project, a handoff"),
        sent && sent.what === "⇢ waiting on jnotes (handoff)" && sent.text.startsWith("The notes API renamed"), sent);
      check(W("its title says who, from where, to whom and where"),
        sent && sent.title === "handoff h1 from " + LEAD + " in " + DP + " to " + JLEAD + " in " + JN + ": " + handoff().head, sent);
      const took = await line(row(JLEAD));
      check(W("the taker's card: which project waits on it"),
        took && took.what === "⇠ deploy-platform is waiting (handoff)", took);
      check(W("both labels are whole, inside their cards"),
        sent && took && sent.whole && took.whole && sent.right <= sent.width && took.right <= took.width, [sent, took]);
      // A project whose name runs long: the label gives way, the card holds.
      await page.evaluate((h) => GM.renderHerd(h), herd([handoff({ fromProject: "~/src/deploy-platform-and-its-very-long-sibling/" })]));
      await sleep(100);
      const long = await line(row(JLEAD));
      check(W("a long project's label stays inside the card"), long && long.right <= long.width + 0.5, long);
      await show([handoff()]);
      await sleep(100);
      check(W("an ordinary ask is no handoff"), !(await page.$(row("claude-deploy-platform") + " .aask.ask-handoff")));

      await room(DP);
      const rs = await line(col(LEAD));
      check(W("in its room's column, the sender's the same"), rs && rs.what === "⇢ waiting on jnotes (handoff)", rs);
      const run = await runs();
      check(W("its room's run names the taker and its project"),
        run.length === 1 && run[0].includes(LEAD + " → " + JLEAD + " (jnotes)") && run[0].includes("waiting on " + JLEAD + " (jnotes)"), run);
      await room(JN);
      const rt = await line(col(JLEAD));
      check(W("in the taker's room, its column says which project waits"),
        rt && rt.what === "⇠ deploy-platform is waiting (handoff)", rt);
      const jrun = await runs();
      check(W("and the run shows there too"), jrun.length === 1 && jrun[0].includes(LEAD + " → " + JLEAD + " (jnotes)"), jrun);

      // ---- To a project with no lead: the herd starts one ----
      await show([handoff({ id: "h2", run: "h2", to: "", status: "queued", project: UN })]);
      await sleep(100);
      const starting = await line(row(LEAD));
      check(W("waiting while the herd starts that project's lead"),
        starting && starting.what === "⇢ waiting on unnoiac (handoff) · starting its lead" &&
        starting.title.includes("to the lead being started in " + UN), starting);
      check(W("no card takes it yet"), !(await page.$(row(JLEAD) + " .aask")));
      await room(DP);
      const srun = await runs();
      check(W("its run: unnoiac's lead, starting"), srun.length === 1 && srun[0].includes(LEAD + " → unnoiac's lead, starting") &&
        srun[0].includes("waiting on unnoiac's lead, starting"), srun);

      // ---- Queued behind the lead's own work ----
      await show([handoff({ status: "queued" })]);
      await sleep(100);
      const q = [await line(row(LEAD)), await line(row(JLEAD))];
      check(W("queued: both say so"), q[0] && q[0].what === "⇢ waiting on jnotes (handoff) · queued" &&
        q[1] && q[1].what === "⇠ deploy-platform is waiting (handoff) · queued", q);

      // ---- Answered, or failed ----
      await show([handoff({ status: "answered", answered: now - 10e3, replyHead: "Done on fix/token, PR 12." })]);
      await sleep(100);
      const ans = await line(row(LEAD));
      check(W("the answer comes back to the sender"),
        ans && ans.what === "⇠ jnotes answered (handoff)" && ans.text === "Done on fix/token, PR 12." && /ask-answered/.test(ans.cls), ans);
      check(W("the taker's card lets it go"), !(await page.$(row(JLEAD) + " .aask")));
      const why = "unnoiac/ is not a git checkout, and has no lead";
      await show([handoff({ id: "h3", run: "h3", to: "", status: "failed", project: UN, error: why, answered: now - 5e3 })]);
      await sleep(100);
      const failed = await line(row(LEAD));
      check(W("a refused handoff says why"),
        failed && failed.what === "⇢ handoff to unnoiac failed: " + why && /ask-failed/.test(failed.cls), failed);
      await room(DP);
      const frun = await runs();
      check(W("and its run says it stopped"), frun.length === 1 && frun[0].includes("stopped: handoff to unnoiac's lead, starting failed"), frun);

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
