// An agent's sub-agents, on a phone and at a desk: the ones running,
// under its column and above the chat's box as its screen lists them --
// their time running on between looks -- counted on its row; the lines
// that started them marked with how each is doing; and each report where
// it came back, its start in sight and the rest a press away.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const LEAD = "lead-deploy-platform";
const RD = "claude-deploy-platform";
const QA = "qa-deploy-platform";

const ago = (s) => new Date(Date.now() - s * 1000).toISOString();
const subsNow = () => [
  { id: "a3db8", who: "general-purpose", title: "Build R4 sync debug project", started: ago(18 * 60 + 35), tools: 61,
    tokens: 195800, doing: "Bash · Testing Bug-1-only backfill in the sync project, with the long name it was given" },
  { id: "a6c3a", who: "Explore", title: "Find where the retry scheduler is configured", started: ago(42), tools: 1,
    tokens: null, doing: "" },
];
const herd = (subs, extra) => Object.assign({}, HERD, {
  agents: HERD.agents.map((a) => Object.assign({}, a, { meter: false, subs: [] },
    a.name === RD ? { subs, background: ["1 shell", "2 agents"] } : {},
    a.name === QA ? { background: ["2 agents"] } : {}, (extra || {})[a.name] || {})),
});

const REPORT = "## stats: built and verified\n\n`python3 build.py stats` succeeds in about 6 seconds.\n\n" +
  Array.from({ length: 14 }, (_, n) => "- check " + (n + 1) + " passed").join("\n") + "\n\nNothing was committed.";
const ts = ago(120);
const TOOLS = { role: "tools", ts, tools: [
  { name: "Agent", hint: "Build R4 sync debug project", sub: "a3db8" },
  { name: "Agent", hint: "Build R3 stats debug project", sub: "a6568", state: "completed" },
  { name: "Agent", hint: "Build R2 authorization debug project", sub: "a6c3x", state: "killed" },
  { name: "Agent", hint: "Build R5 timezone debug project", sub: "aa3d4" },
  { name: "Bash", hint: "Locate README" },
] };
const CONV = { name: RD, entries: [
  { role: "user", kind: "prompt", text: "Build the five rounds", ts },
  TOOLS,
  { role: "sub", kind: "report", sub: "a6568", who: "general-purpose", title: "Build R3 stats debug project", text: REPORT,
    status: "completed", tokens: 195958, uses: 61, ms: 931357, ts },
  { role: "sub", kind: "report", sub: "a6c3x", who: "general-purpose", title: "Build R2 authorization debug project",
    text: "Stopped before the build.", status: "killed", tokens: 1200, uses: 1, ms: 4000, ts },
  { role: "sub", kind: "report", sub: "b1", who: "", title: "", text: "Halfway: the schema is done.", status: null,
    tokens: null, uses: null, ms: null, ts },
  { role: "assistant", text: "Round 3 is done; round 4 is still building.", ts },
] };

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const { ctx, page, errors } = await phonePage(browser, { context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const show = (h) => page.evaluate((h) => { GM.renderHerd(h); }, h);
      await page.evaluate(() => {
        window.copied = [];
        const exec = document.execCommand.bind(document);
        document.execCommand = (cmd, ...rest) => {
          if (cmd !== "copy") return exec(cmd, ...rest);
          const ta = [...document.querySelectorAll("body > textarea[readonly]")].pop();
          window.copied.push(ta ? ta.value : null);
          return true;
        };
      });
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd(subsNow()));
      await sleep(100);

      // ---- Counted on its row ----
      const bg = (name) => page.$eval('.arow[data-name="' + name + '"] .amain', (r) => {
        const b = r.querySelector(".bgw");
        return b ? { text: b.textContent, title: b.title } : null;
      });
      const rd = await bg(RD);
      check(W("its row counts them, as their transcripts have it, not its footer"),
        rd && rd.text === "1 shell · 2 sub-agents running" && /Build R4 sync debug project, Find where/.test(rd.title), rd);
      const qa = await bg(QA);
      check(W("one whose transcript says none: its footer, as it was"), qa && qa.text === "2 agents running", qa);

      // ---- Under its column ----
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      if (await page.$(".room .chat")) {
        await tap('.room [data-act="a:room-view"][data-view="columns"]');
        await sleep(150);
      }
      await page.evaluate((c) => GM.setConversation(c), CONV);
      await sleep(100);
      const colOf = (name) => '.rcol[data-name="' + name + '"]';
      const COL = colOf(RD);
      const strip = await page.$$eval(COL + " .rsubs .sub", (ss) => ss.map((s) => ({
        who: s.querySelector(".swho").textContent, title: s.querySelector(".stitle").textContent,
        stat: s.querySelector(".sstat").textContent, doing: (s.querySelector(".sdoing") || {}).textContent || "" })));
      check(W("a line each, oldest first: what it is, how long, its tool calls and context"),
        strip.length === 2 && strip[0].who === "general-purpose" && strip[0].title === "Build R4 sync debug project" &&
        /^18m 3\ds · 61 tools · 196k$/.test(strip[0].stat) && /^4\ds · 1 tool$/.test(strip[1].stat), strip);
      check(W("and its latest step under it; none said, none shown"),
        /^Bash · Testing Bug-1/.test(strip[0].doing) && strip[1].doing === "", strip.map((s) => s.doing));
      const t0 = await page.$eval(COL + " .rsubs .since", (s) => s.textContent);
      await sleep(2100);
      const t1 = await page.$eval(COL + " .rsubs .since", (s) => s.textContent);
      check(W("its time runs on between looks"), t0 !== t1 && /^18m 3\ds$/.test(t1), [t0, t1]);
      const none = await page.$eval(colOf(LEAD) + " .rsubs", (s) => getComputedStyle(s).display);
      check(W("none under an agent with none"), none === "none", none);
      const out = await page.$eval(COL, (col) => {
        const edge = col.querySelector(".rsubs").getBoundingClientRect().right;
        // Past its edge, or its words spilling out of it uncut.
        return [...col.querySelectorAll(".rsubs .sline > *, .rsubs .sdoing")]
          .filter((x) => x.getBoundingClientRect().right > edge + 0.5 ||
            (x.scrollWidth > x.clientWidth + 1 && getComputedStyle(x).overflowX !== "hidden")).map((x) => x.className);
      });
      check(W("each line within its column: the long cut short"), out.length === 0, out);

      // ---- The lines that started them ----
      const summary = () => page.$eval(COL + " .rbody .ent.tools .linkish", (b) => b.textContent);
      check(W("the tools' line says how many it started are running"),
        (await summary()) === "▸ Agent ×4 · Bash · 1 running", await summary());
      await tap(COL + " .rbody .ent.tools .linkish");
      await sleep(100);
      const marks = await page.$$eval(COL + " .rbody .trow", (rs) => rs.map((r) => {
        const m = r.querySelector(".tstate");
        return m ? m.className.replace("tstate", "").trim() + ":" + m.textContent : "";
      }));
      check(W("opened: running, finished, how one ended, and nothing where it is not known"),
        JSON.stringify(marks) === JSON.stringify(["run:●", ":✓", "bad:killed", "", ""]), marks);
      // It finished: the herd says so before its conversation does.
      await show(herd([subsNow()[1]]));
      await sleep(100);
      check(W("one gone from the herd's list is no longer marked running"),
        (await page.$$eval(COL + " .rbody .trow .tstate.run", (m) => m.length)) === 0 &&
        (await summary()) === "▾ Agent ×4 · Bash", await summary());
      await tap(COL + " .rbody .ent.tools .linkish");

      // ---- What came back ----
      const card = (n) => page.$eval(COL + " .rbody .ent.sub:nth-of-type(" + n + ")", (e) => ({
        head: e.querySelector(".ehead .who").textContent, meta: e.querySelector(".smeta").textContent,
        folded: e.classList.contains("folded"), bad: e.classList.contains("bad"),
        h2: !!e.querySelector(".ebody h2"), more: (e.querySelector(".cmore") || {}).textContent || "",
        height: (e.querySelector(".ebody") || { getBoundingClientRect: () => ({ height: 0 }) }).getBoundingClientRect().height }));
      const subs = await page.$$eval(COL + " .rbody .ent.sub", (es) => es.map((e) => ({
        head: e.querySelector(".ehead .who").textContent, meta: e.querySelector(".smeta").textContent,
        folded: e.classList.contains("folded"), bad: e.classList.contains("bad"),
        h2: !!e.querySelector(".ebody h2"), more: (e.querySelector(".cmore") || {}).textContent || "",
        height: e.querySelector(".ebody").getBoundingClientRect().height })));
      check(W("a report: whose, what it was asked, how it ended and what it took"),
        subs[0].head === "↩ general-purpose · Build R3 stats debug project" &&
        subs[0].meta === "finished · 15m 31s · 61 tools · 196k tokens" && subs[0].h2, subs[0]);
      check(W("a long one folded to its start, a press away"), subs[0].folded && subs[0].more === "more" && subs[0].height < 170, subs[0]);
      check(W("one that did not finish says how it ended, marked"),
        subs[1].bad && subs[1].meta === "killed · 4s · 1 tool · 1k tokens" && !subs[1].folded && !subs[1].more, subs[1]);
      check(W("a message with no ending yet, from one not named"),
        subs[2].head === "↩ sub-agent" && subs[2].meta === "sent a message" && !subs[2].bad, subs[2]);
      await tap(COL + " .rbody .ent.sub .cmore");
      await sleep(100);
      const opened = await page.$eval(COL + " .rbody .ent.sub", (e) => ({
        folded: e.classList.contains("folded"), more: e.querySelector(".cmore").textContent,
        last: e.querySelector(".ebody").textContent.includes("Nothing was committed.") }));
      check(W("more: the whole of it"), !opened.folded && opened.more === "less" && opened.last, opened);
      // A redraw -- the conversation moved on -- keeps it open.
      await page.evaluate((c) => GM.setConversation(c), Object.assign({}, CONV, {
        entries: CONV.entries.concat([{ role: "assistant", text: "Still waiting on round 4.", ts }]) }));
      await sleep(100);
      check(W("and it stays open as the conversation moves on"),
        !(await page.$eval(COL + " .rbody .ent.sub", (e) => e.classList.contains("folded"))));
      await tap(COL + " .rbody .ent.sub .cmore");
      await sleep(100);
      check(W("less: folded again"), await page.$eval(COL + " .rbody .ent.sub", (e) => e.classList.contains("folded")));
      await tap(COL + " .rbody .ent.sub > .ehead .ecopy");
      await sleep(100);
      const copied = await page.evaluate(() => window.copied.splice(0));
      check(W("Copy copies the report as written"), copied.length === 1 && copied[0] === REPORT, copied);

      // ---- The strip grows: the column stays at its end ----
      await show(herd([]));
      await sleep(100);
      await page.$eval(COL + " .rbody", (b) => { b.scrollTop = b.scrollHeight; });
      await show(herd(subsNow()));
      await sleep(100);
      const end = await page.$eval(COL + " .rbody", (b) => b.scrollHeight - b.scrollTop - b.clientHeight);
      check(W("a strip that appears leaves its conversation at its end"), end < 2, end);

      // ---- Above the chat's box ----
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(150);
      await page.selectOption(".chat .chat-to", RD);
      await sleep(100);
      const chat = await page.$$eval(".chat .cmeter .sub .stitle", (ts) => ts.map((t) => t.textContent));
      check(W("the chat shows the ones of whom its box writes to"),
        JSON.stringify(chat) === JSON.stringify(subsNow().map((u) => u.title)), chat);
      await page.selectOption(".chat .chat-to", LEAD);
      await sleep(100);
      check(W("none for one with none"), (await page.$eval(".chat .cmeter", (m) => getComputedStyle(m).display)) === "none");

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
