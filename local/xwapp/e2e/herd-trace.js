// How an answer's work went (ask 4093758e): one line of the herd's counts
// beside an answer -- in Threads (the counts behind it when the card is
// opened), in the room's columns (an ask's outcome, an answer message) and in
// the chat.  A rough one is marked; an answer without a line shows none.
// Desk and phone; GM.* driven as Emacs pushes, no sidecar.  Screenshots go
// to shots/trace/ (SHOTS overrides).
const { webkit } = require("playwright");
const fs = require("fs");
const path = require("path");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const SHOTS = process.env.SHOTS || path.join(__dirname, "shots", "trace");
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };
const DP = "~/src/deploy-platform/";
const LEAD = "lead-deploy-platform";
const now = Date.now();
const min = 60e3;
const NUDGE = "rough going: if it found a non-obvious gotcha, `herd remember` it";
const SMOOTH = "How it went: 4 min · 12 tool calls, none failed · asked others 1× (qa-dp)";
const ROUGH = "How it went: 38 min · 108 tool calls, 5 failed (5 shell exits) · 1 check denial · " + NUDGE;
const DETAIL = { seconds: 2280, seen: true, why: "", cli: "claude", tools: { Bash: 70, Read: 25, Edit: 13 },
  failed: { shell: 5 }, failures_known: true, subs: 0, subs_read: 0, sub_tools: 0, sub_failed: {},
  guard: 0, lint: 1, asks: {}, relays: 0 };
const L = (name, kind, project, role) => ({ name, kind, project: project || "", role: role || "", worktree: false });
const M = (kind, from, to, ago, head, extra) => Object.assign({ kind, from, to, at: now - ago * min, head, text: head, ask: "", round: null, of: null, auto: false, logged: false }, extra || {});
const THREAD = { id: "t9", title: "Fix the build", status: "answered",
  lanes: [L(LEAD, "claude", "deploy-platform", "lead"), L("claude-dp", "claude", "deploy-platform"), L("qa-dp", "agy", "deploy-platform")],
  msgs: [M("ask", LEAD, "claude-dp", 50, "Fix the build", { ask: "k1" }),
         M("ask", "claude-dp", "qa-dp", 30, "Check it", { ask: "k2" }),
         M("answer", "qa-dp", "claude-dp", 26, "Looks fine.", { ask: "k2", trace: SMOOTH, rough: false, traceDetail: Object.assign({}, DETAIL, { tools: { run_command: 12 }, failed: {}, failures_known: false, cli: "agy", lint: 0 }) }),
         M("answer", "claude-dp", LEAD, 12, "Fixed in 72f7736.", { ask: "k1", trace: ROUGH, rough: true, traceDetail: DETAIL }),
         M("ask", LEAD, "qa-dp", 10, "And the docs?", { ask: "k3" }),
         M("answer", "qa-dp", LEAD, 9, "Done.", { ask: "k3" })],
  waiting: [], rootFrom: LEAD, rootTo: "claude-dp", projects: ["deploy-platform"], cross: false, started: now - 50 * min, at: now - 9 * min };
const LIST = { threads: [{ id: "t9", title: THREAD.title, status: "answered", participants: THREAD.lanes.map((l) => l.name),
  projects: ["deploy-platform"], cross: false, count: THREAD.msgs.length, open: 0, started: THREAD.started, at: THREAD.at,
  rootFrom: LEAD, rootTo: "claude-dp" }], talking: [], live: 0, more: false };

const overflow = (page) => page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);

(async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : PHONE });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));

      // ---- Threads ----
      await page.evaluate(() => document.querySelector('.tab[data-tab="threads"]').click());
      await sleep(120);
      await page.evaluate((l) => GM.renderThreads(l), LIST);
      await sleep(120);
      if (where === "phone") {
        await tap('.throw[data-id="t9"]');
        await sleep(100);
      }
      await page.evaluate((t) => GM.renderThread(t), THREAD);
      await sleep(150);
      const card = where === "desk" ? ".thcard" : ".thpmsg";
      const traces = await page.$$eval(card + " .thtrace", (es) => es.map((e) => ({ t: e.textContent, rough: e.classList.contains("rough") })));
      check(W("each answer with a line shows it on its card"),
        traces.length === 2 && traces[0].t === SMOOTH && traces[1].t === ROUGH, traces);
      check(W("a rough one is marked, a smooth one is not"), traces[1].rough && !traces[0].rough, traces);
      check(W("an answer without a line shows none"),
        await page.$$eval(card, (cs) => !cs[5].querySelector(".thtrace")));
      check(W("the counts are not shown until the card is opened"), !(await page.$(card + " .thtracedetail")));
      await tap(card + '[data-i="3"]');
      await sleep(120);
      const detail = await page.$eval(card + '[data-i="3"] .thtracedetail', (e) => e.textContent).catch(() => "");
      check(W("opened, the counts behind the line"),
        detail === "tools: Bash 70, Read 25, Edit 13 · failed: shell exit 5 · check denials: 1", detail);
      await tap(card + '[data-i="2"]');
      await sleep(120);
      const agyDetail = await page.$eval(card + '[data-i="2"] .thtracedetail', (e) => e.textContent).catch(() => "");
      check(W("for a CLI whose failures are not readable, it says so"),
        agyDetail === "tools: run_command 12 · failures not readable for agy", agyDetail);
      await page.screenshot({ path: path.join(SHOTS, where + "-threads.png"), fullPage: true });
      check(W("Threads: nothing off the page's edge"), (await overflow(page)) <= 0, await overflow(page));

      // ---- The room: an ask's outcome and an answer message, in a column; then the chat ----
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      await page.evaluate((a) => GM.setConversation({ name: a.lead, path: "/x.jsonl", cli: "claude", earlier: false, entries: [
        { role: "herd", kind: "ask", to: "claude-dp", ask: "k1", text: "Fix the build", ts: "c1",
          answer: "Fixed in 72f7736.", trace: a.rough },
        { role: "user", who: "qa-dp", kind: "answer", ask: "k3", text: "Done.", trace: a.smooth, ts: "c2" },
        { role: "user", who: "qa-dp", kind: "answer", ask: "k4", text: "No line here.", ts: "c3" }] }),
        { lead: LEAD, rough: ROUGH, smooth: SMOOTH });
      await sleep(150);
      const col = '.rcol[data-name="' + LEAD + '"]';
      const roomTraces = await page.$$eval(col + " .trace", (es) => es.map((e) => ({ t: e.textContent, rough: e.classList.contains("rough") })));
      check(W("room: the ask's answer and the answer message carry their lines"),
        roomTraces.length === 2 && roomTraces[0].t === ROUGH && roomTraces[0].rough && roomTraces[1].t === SMOOTH && !roomTraces[1].rough, roomTraces);
      check(W("room: the line sits under the answer, not in it"),
        await page.$eval(col + " .eanswer", (e) => e.querySelector(".trace") === e.lastElementChild));
      await page.screenshot({ path: path.join(SHOTS, where + "-room.png"), fullPage: true });

      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(150);
      await page.evaluate((a) => GM.setChat({ project: a.dp, earlier: false, missing: [], absent: [], entries: [
        { who: "qa-dp", to: a.lead, kind: "answer", ask: "k3", text: "Done.", trace: a.smooth, ts: "d1" },
        { who: a.lead, to: "claude-dp", kind: "ask", ask: "k1", text: "Fix the build", answer: "Fixed in 72f7736.", trace: a.rough, ts: "d2" }] }),
        { dp: DP, lead: LEAD, rough: ROUGH, smooth: SMOOTH });
      await sleep(150);
      const chat = await page.$$eval(".chat .trace", (es) => es.map((e) => e.textContent));
      check(W("chat: an answer and an ask's outcome carry their lines"), chat.length === 2 && chat.includes(SMOOTH) && chat.includes(ROUGH), chat);
      check(W("no sideways scroll"), (await overflow(page)) <= 0, await overflow(page));
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log("\n" + (results.length - failed) + "/" + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
