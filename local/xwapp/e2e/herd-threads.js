// The Threads tab: who is talking to whom across the herd.  Every open ask
// across the top; the threads; the open one as a sequence diagram -- a lane
// per participant, an arrow per message, pointing the way it went; a click
// for the whole message; the Inbox's way in.  The phone: the list, then the
// thread as a conversation.  Fixture data, Emacs played by threads-fixture.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const { LIST, DETAILS, serve } = require("./threads-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const now = Date.now();
const opens = (intents) => intents.filter((i) => i.op === "thread-open").map((i) => i.id || i.ask || ("agent:" + i.agent));
const ITEMS = [
  { id: "handoff:h1", group: "fyi", kind: "handoff", tag: "Handoff", tone: "purple", agent: "lead-jnotes", cli: "", project: "~/src/jnotes/",
    title: "Answered: claude -p children closed asks", at: now - 40 * 60e3, whyLabel: "Cross-project handoff, answered", body: "Fixed.", preview: "Fixed.", state: "", read: false, override: null },
  { id: "question:lead-jnotes:3", group: "need", kind: "question", tag: "Question", tone: "accent", agent: "lead-jnotes", cli: "claude", project: "~/src/jnotes/",
    title: "Which branch?", at: now - 5 * 60e3, whyLabel: "It ended its turn with a question", body: "Which branch?", preview: "Which branch?", state: "idle", read: false, override: null, replyVia: "lead" },
];

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- The desk ------------------------------------------------------
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: DESK });
      const stop = serve(page, intents);
      await sleep(150);
      check("desk: the page asks for the Threads badge as it starts", intents.some((i) => i.op === "threads-peek"));
      await page.evaluate(() => GM.threadsBadge({ live: 3 }));
      check("desk: the tab says how many threads are live", (await page.textContent('.tab[data-tab="threads"]')).includes("3 live"));
      await page.click('.tab[data-tab="threads"]');
      await sleep(150);
      check("desk: opening the tab watches the threads", intents.some((i) => i.op === "threads-watch"));
      await page.evaluate((l) => GM.renderThreads(l), LIST);
      await sleep(200);
      check("desk: with none open, the newest thread opens", opens(intents)[0] === "t1", opens(intents));

      const strip = await page.$$eval(".thtalk .thchip", (cs) => cs.map((c) => c.textContent));
      check("desk: talking now shows every open ask and handoff", strip.length === 4, strip);
      check("desk: a handoff's arrow is the dashed one, a working ask says how long",
        strip[0].startsWith("primary⇢lead-jnotes") && /working 58m/.test(strip[1]) && /queued/.test(strip[3]), strip);

      const rows = await page.$$eval(".thlist .throw", (rs) => rs.map((r) => ({
        st: r.querySelector(".thst").textContent, path: r.querySelector(".thpath").textContent,
        projs: [...r.querySelectorAll(".thproj")].map((p) => p.textContent), cross: !!r.querySelector(".thcross"),
        count: r.querySelector(".thcount").textContent })));
      check("desk: one row per thread, its status first", rows.map((r) => r.st).join() === "Live,Answered,Waiting,Answered,Failed", rows.map((r) => r.st));
      check("desk: its participants as a path, You first when the user started it", rows[0].path === "You → lead-.emacs.d → claude-.emacs.d-2", rows[0].path);
      check("desk: its projects, cross-project, how many messages",
        rows[1].projs.join() === "jnotes,.emacs.d" && rows[1].cross && rows[1].count === "6 messages" && !rows[0].cross, rows[1]);

      await page.click('.throw[data-id="t2"]');
      await sleep(200);
      check("desk: a row opens its thread", opens(intents).slice(-1)[0] === "t2");
      const lanes = await page.$$eval(".thlanehead", (ls) => ls.map((l) => l.textContent));
      check("desk: a lane per participant, with its project and role",
        lanes.join("|") === "lead-jnotesjnotes · lead|lead-.emacs.d.emacs.d · lead|claude-.emacs.d-2.emacs.d · worktree|claude-.emacs.d.emacs.d", lanes);
      // Each arrow points at the lane its message went to.
      const geo = await page.evaluate(() => {
        const centers = [...document.querySelectorAll(".thlanehead")].map((h) => { const r = h.getBoundingClientRect(); return r.left + r.width / 2; });
        return [...document.querySelectorAll(".thmsg")].map((m) => {
          const head = m.querySelector(".thhead").getBoundingClientRect();
          const line = m.querySelector(".thline");
          return { kind: [...m.classList].find((c) => c.startsWith("k-")), dir: m.getAttribute("data-dir"),
                   tip: m.getAttribute("data-dir") === "right" ? head.right : head.left, centers,
                   style: getComputedStyle(line).borderTopStyle };
        });
      });
      const D = DETAILS.t2;
      const idx = (n) => D.lanes.findIndex((l) => l.name === n);
      const pointed = geo.map((g, i) => Math.abs(g.tip - g.centers[idx(D.msgs[i].to)]) <= 8);
      check("desk: a row per message, each arrow's head at the lane it went to", geo.length === 6 && pointed.every(Boolean), { pointed, geo: geo.map((g) => [g.kind, g.dir, Math.round(g.tip)]) });
      check("desk: asks and the handoff point on, answers back",
        geo.map((g) => g.dir).join() === "right,right,left,right,left,left", geo.map((g) => g.dir));
      check("desk: the handoff to another project is dashed, an ask solid",
        geo[0].kind === "k-handoff" && geo[0].style === "dashed" && geo[1].style === "solid", geo.slice(0, 2));
      const card = await page.$eval('.thcard[data-i="0"]', (c) => c.textContent);
      check("desk: a message shows its first lines, who to whom and when", /lead-jnotes → lead-\.emacs\.d/.test(card) && !/What we need/.test(card), card);
      await page.click('.thcard[data-i="0"]');
      await sleep(100);
      const full = await page.$eval('.thcard[data-i="0"]', (c) => ({ text: c.textContent, open: c.getAttribute("aria-expanded") }));
      check("desk: a click shows the whole message", full.open === "true" && /What we need from \.emacs\.d/.test(full.text), full);
      await page.click('.thcard[data-i="1"]');
      await sleep(100);
      check("desk: the round of an ask made while answering", /round 1\/5/.test(await page.textContent('.thcard[data-i="1"]')));
      await page.click('.thcard[data-i="0"]');
      await sleep(100);
      check("desk: and a click again, its first lines", (await page.getAttribute('.thcard[data-i="0"]', "aria-expanded")) === "false");

      await page.click('.thchip:has-text("claude-jnotes")');
      await sleep(200);
      check("desk: a chip in talking now opens its thread", opens(intents).slice(-1)[0] === "a31" &&
        (await page.textContent(".thh1")).startsWith("Sum up jnotes"));
      const waitText = await page.textContent(".thwait");
      check("desk: where it waits now", waitText === "lead-jnotes waits on claude-jnotes · primary waits on lead-jnotes", waitText);
      const first = await page.textContent('.thcard[data-i="0"]');
      check("desk: the user's prompt that set it going, as the log has it", /You → primary/.test(first) && /as the herd log has it/.test(first), first);
      await page.click('.throw[data-id="t1"]');
      await sleep(200);
      check("desk: a thread being worked on says who is working", (await page.textContent(".thwait")) === "claude-.emacs.d-2 is working · 6m");
      // The log had only the herd's note before the user's words: Emacs
      // opened the thread with them as its task's transcript has them.
      const behind = Object.assign({}, DETAILS.t1, { msgs: [Object.assign({}, DETAILS.t1.msgs[0], { logged: false, transcript: true }),
                                                            ...DETAILS.t1.msgs.slice(1)] });
      await page.evaluate((d) => GM.renderThread(d), behind);
      await sleep(100);
      const read = await page.textContent('.thcard[data-i="0"]');
      check("desk: the user's words behind a note, as its task's transcript has them",
        /You → lead-\.emacs\.d/.test(read) && /as its task's transcript has it/.test(read) && !/herd log/.test(read), read);
      await page.evaluate((d) => GM.renderThread(d), DETAILS.t1);
      await sleep(100);

      // The Inbox's way in: the thread is drawn inline below the item, with a
      // quiet "Open in Threads" link in its header (shown once Emacs pushes the
      // thread for the picked item -- no always-on button any more).
      const STUB = {
        id: "stub", title: "t", status: "answered", participants: ["user", "lead-jnotes"], projects: ["jnotes"],
        cross: false, count: 1, open: 0, started: now - 60e3, at: now - 10e3, rootFrom: "lead-jnotes", rootTo: "claude-jnotes",
        lanes: [{ name: "user", kind: "you", project: "", role: "", worktree: false },
                { name: "lead-jnotes", kind: "claude", project: "jnotes", role: "lead", worktree: false },
                { name: "claude-jnotes", kind: "claude", project: "jnotes", role: "", worktree: false }],
        msgs: [{ kind: "ask", from: "lead-jnotes", to: "claude-jnotes", at: now - 40e3, head: "h", text: "full" }], waiting: [],
      };
      await page.click('.tab[data-tab="inbox"]');
      check("desk: leaving the tab stops watching it", intents.some((i) => i.op === "threads-unwatch"));
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 1 }), ITEMS);
      await sleep(100);
      await page.click('.ibrow[data-id="handoff:h1"]');
      await sleep(80);
      check("desk: picking an Inbox item asks for its detail", intents.some((i) => i.op === "inbox-detail" && i.id === "handoff:h1" && i.ask === "h1"));
      await page.evaluate((th) => GM.inboxDetailData({ for: "handoff:h1", thread: th }), STUB);
      await sleep(60);
      await page.click('.ibthwrap [data-act="th:from-inbox"]');
      await sleep(250);
      check("desk: Open in Threads on an Inbox item opens the thread of its ask",
        opens(intents).slice(-1)[0] === "h1" && (await page.textContent(".thh1")) === "claude -p children closed asks" &&
        (await page.getAttribute('.tab[data-tab="threads"]', "class")).includes("on"));
      await page.click('.tab[data-tab="inbox"]');
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 1 }), ITEMS);
      await page.click('.ibrow[data-id="question:lead-jnotes:3"]');
      await sleep(80);
      await page.evaluate((th) => GM.inboxDetailData({ for: "question:lead-jnotes:3", thread: th }), STUB);
      await sleep(60);
      await page.click('.ibthwrap [data-act="th:from-inbox"]');
      await sleep(250);
      check("desk: and of an agent's item, that agent's latest thread", opens(intents).slice(-1)[0] === "agent:lead-jnotes");
      await page.evaluate(() => document.querySelector('.tab[data-tab="threads"]').click());
      await page.evaluate(() => GM.renderThread({ error: "ghostherd-memory JSON-RPC -32602: no thread with rd in it" }));
      await sleep(80);
      check("desk: a thread that cannot be read says why", (await page.textContent(".thmain")).trim() === "no thread with rd in it");
      check("desk: no page errors", errors.length === 0, errors);
      stop();
      await ctx.close();
    }
    // ---- The phone -----------------------------------------------------
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents });
      const stop = serve(page, intents);
      await sleep(150);
      await page.tap('.tab[data-tab="threads"]');
      await page.evaluate((l) => GM.renderThreads(l), LIST);
      await sleep(200);
      check("phone: the list first -- no thread opened for it", opens(intents).length === 0 && !(await page.$(".thconv")) && (await page.$$(".throw")).length === 5);
      const wide = await page.evaluate(() => document.scrollingElement.scrollWidth <= window.innerWidth + 1);
      check("phone: nothing wider than the screen", wide);
      const tall = await page.$eval(".throw", (r) => r.getBoundingClientRect().height);
      check("phone: rows big enough for a finger", tall >= 44, tall);
      await page.tap('.throw[data-id="t2"]');
      await sleep(250);
      const conv = await page.$$eval(".thpmsg", (ms) => ms.map((m) => m.querySelector(".thphead").textContent));
      check("phone: the thread as a conversation, each message A → B",
        conv.length === 6 && conv[0].startsWith("lead-jnotes⇢lead-.emacs.d") && conv[2].startsWith("claude-.emacs.d-2→lead-.emacs.d"), conv);
      check("phone: no lanes on the phone", !(await page.$(".thdiag")));
      await page.tap('.thpmsg[data-i="0"]');
      await sleep(100);
      check("phone: a tap shows the whole message", /What we need from \.emacs\.d/.test(await page.textContent('.thpmsg[data-i="0"]')));
      await page.tap('[data-act="th:back"]');
      await sleep(150);
      check("phone: back to the list", intents.some((i) => i.op === "thread-close") && (await page.$$(".throw")).length === 5);
      await page.tap('.thchip:has-text("claude-.emacs.d-2")');
      await sleep(250);
      check("phone: a chip opens its thread too", opens(intents).slice(-1)[0] === "a11" && (await page.$$(".thpmsg")).length === 2);
      check("phone: where it waits", (await page.textContent(".thwait")) === "claude-.emacs.d-2 is working · 6m");
      check("phone: no page errors", errors.length === 0, errors);
      stop();
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
