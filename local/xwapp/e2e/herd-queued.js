// What an agent's CLI holds queued -- typed while it worked, read off its
// screen by Emacs -- on the herd page, at the desk and on a phone: its
// card counts them; its room (each column, and the chat the phone opens)
// ends with each, faint and marked queued, and a Send now that tells
// Emacs which one it showed.  Delivered, the conversation's own entry
// takes its place -- never both, never neither: whether the transcript
// says it before the screen lets it go or after.  Taken back by hand, it
// leaves after a moment.
const { webkit } = require("playwright");
const fs = require("fs");
const path = require("path");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const LEAD = "lead-deploy-platform";
const RD = "claude-deploy-platform";
const QA = "qa-deploy-platform";
const Q1 = "另外你這個 retry 的測試：also run the slow suite, and keep this long enough that it wraps in a phone's column";
const Q2 = "and the docs";
const Q3 = "never mind that";
// Where the screenshots go: SHOTS, else nowhere.
const SHOTS = process.env.SHOTS || "";

const herd = (queued) => Object.assign({}, HERD, {
  agents: HERD.agents.map((a) => Object.assign({}, a, { meter: false, subs: [], queued: [], queuedNow: false },
    a.name === RD ? Object.assign({ state: "working" }, queued.rd || {}) : {},
    a.name === QA ? queued.qa || {} : {})),
});
const iso = (msAgo) => new Date(Date.now() - msAgo).toISOString();
const CONV = (extra) => ({ name: RD, path: "/x/rd.jsonl", cli: "claude", earlier: false, entries: [
  { role: "user", who: "", kind: "prompt", text: "Run the slow tests", ts: iso(120e3) },
  { role: "assistant", text: "Running them now; this takes a few minutes.", ts: iso(110e3) },
].concat(extra || []) });
const CHAT = (extra) => ({ project: DP, earlier: false, missing: [], absent: [], entries: [
  { who: "user", to: RD, kind: "prompt", text: "Run the slow tests", ts: iso(120e3), agent: RD },
  { who: RD, kind: "turn", text: "Running them now; this takes a few minutes.", ts: iso(110e3), agent: RD },
].concat(extra || []) });

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, clock: true, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const show = (q) => page.evaluate((h) => GM.renderHerd(h), herd(q));
      const shot = async (name) => { if (SHOTS) await page.screenshot({ path: path.join(SHOTS, where + "-" + name + ".png") }); };
      const lastOf = (op) => intents.filter((i) => i.op === op).pop();
      const BOTH = { rd: { queued: [Q1, Q2], queuedNow: true }, qa: { queued: ["agy sendnow two: 6 plus 6"], queuedNow: false } };

      // ---- Its card ----
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd(BOTH));
      await sleep(120);
      const mark = (name) => page.$eval('.arow[data-name="' + name + '"] .amain', (r) => {
        const m = r.querySelector(".qdw");
        return m ? { text: m.textContent, title: m.title } : null;
      });
      const m = await mark(RD);
      check(W("its card counts what its CLI holds queued"), m && m.text === "2 queued" && m.title.includes(Q2), m);
      check(W("one each"), ((await mark(QA)) || {}).text === "1 queued");
      check(W("none on an agent with nothing queued"), (await mark(LEAD)) === null);
      if (where === "desk") await shot("card");

      // ---- The room: the chat (the phone opens it), then the columns ----
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      if (!(await page.$(".room .chat"))) {
        await tap('.room [data-act="a:room-view"][data-view="chat"]');
        await sleep(150);
      }
      await page.evaluate((c) => GM.setChat(c), CHAT());
      await sleep(150);
      const chatQueue = () => page.$$eval(".chat .cbody > .queued", (qs) => qs.map((q) => ({
        name: q.getAttribute("data-name"),
        items: [...q.querySelectorAll(".ent.qd")].map((e) => ({ who: e.querySelector(".who").textContent, text: e.querySelector(".ebody").textContent })),
        note: q.querySelector(".qnote span").textContent, now: !!q.querySelector('[data-act="a:send-now"]') })));
      let cq = await chatQueue();
      check(W("chat: it ends with each queued message, whom it is to"),
        cq.length === 2 && cq[0].name === RD && cq[0].items.length === 2 && cq[0].items[0].who === "You → " + RD + " · queued" &&
        cq[0].items[0].text === Q1 && cq[0].items[1].text === Q2 && cq[1].name === QA && cq[1].items[0].text === "agy sendnow two: 6 plus 6", cq);
      check(W("chat: when each will be read, by its CLI"),
        cq[0].note === "it reads it after its current step" && cq[1].note === "it reads it after this turn", cq.map((q) => q.note));
      check(W("chat: Send now where its CLI offers the key, not where it does not"), cq[0].now && !cq[1].now, cq);
      const isLast = await page.$eval(".chat .cbody", (b) => b.lastElementChild.classList.contains("queued"));
      check(W("chat: after everything said"), isLast);
      const atEnd = await page.$eval(".chat .cbody", (b) => b.scrollHeight - b.scrollTop - b.clientHeight < 2);
      check(W("chat: read at its end, it stays there"), atEnd);
      await shot("chat");
      await tap('.chat .queued[data-name="' + RD + '"] [data-act="a:send-now"]');
      await sleep(80);
      check(W("chat: Send now tells Emacs which agent, and the message it showed"),
        (lastOf("agent-send-now") || {}).name === RD && lastOf("agent-send-now").text === Q1, lastOf("agent-send-now"));
      // Delivered: the chat's own entry, and the queued one gone.
      await page.evaluate((c) => GM.setChat(c), CHAT([{ who: "user", to: RD, kind: "prompt", text: Q1, ts: iso(0), agent: RD }]));
      await sleep(120);
      cq = await chatQueue();
      const inChat = await page.$$eval(".chat .cbody .ent", (es) => es.map((e) => (e.querySelector(".ebody") || {}).textContent || ""));
      check(W("chat: delivered, said once -- the chat's entry, not the queued"),
        inChat.filter((t) => t === Q1).length === 1 && cq[0].items.map((i) => i.text).join("|") === Q2, { inChat, cq });
      const sideways = () => page.evaluate((sel) => ({
        page: document.documentElement.scrollWidth <= window.innerWidth,
        out: [...document.querySelectorAll(sel + " .queued .ent, " + sel + " .queued .qnote")]
          .filter((x) => x.getBoundingClientRect().right > x.closest(sel).getBoundingClientRect().right + 0.5).length }), where === "phone" ? ".chat .cbody" : ".chat .cbody");
      let sw = await sideways();
      check(W("chat: nothing queued runs past its edge, nor the page sideways"), sw.page && sw.out === 0, sw);

      await tap('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(150);
      await show(BOTH);
      await page.evaluate((c) => GM.setConversation(c), CONV());
      await sleep(150);
      const COL = '.rcol[data-name="' + RD + '"]';
      const colQueue = () => page.$eval(COL + " .rbody", (b) => {
        const q = b.querySelector(".queued");
        return {
          last: !!q && b.lastElementChild === q,
          items: q ? [...q.querySelectorAll(".ent.qd")].map((e) => ({
            who: e.querySelector(".who").textContent, text: e.querySelector(".ebody").textContent,
            leaving: e.classList.contains("leaving"), faint: Number(getComputedStyle(e).opacity) < 1 })) : [],
          now: !!(q && q.querySelector('[data-act="a:send-now"]')),
          said: [...b.querySelectorAll(".ent.you:not(.qd) .ebody")].map((e) => e.textContent),
          atEnd: b.scrollHeight - b.scrollTop - b.clientHeight < 2,
        };
      });
      let q = await colQueue();
      check(W("column: it ends with each, faint, marked queued"),
        q.last && q.items.length === 2 && q.items.every((i) => i.faint && i.who === "You · queued") &&
        q.items[0].text === Q1 && q.items[1].text === Q2, q);
      check(W("column: at its end"), q.atEnd, q);
      check(W("column: Send now"), q.now);
      const head = await page.$eval(COL + " .rtop .qdw", (m) => ({ text: m.textContent, cut: m.scrollWidth > m.clientWidth + 1,
        w: m.getBoundingClientRect().width })).catch(() => null);
      check(W("column: its head counts them, not cut away"), head && head.text === "2 queued" && !head.cut && head.w > 20, head);
      const out = await page.$eval(COL, (col) => {
        const edge = col.getBoundingClientRect().right;
        return [...col.querySelectorAll(".queued .ent, .queued .qnote, .queued button")]
          .filter((x) => x.getBoundingClientRect().right > edge + 0.5).map((x) => x.className);
      });
      check(W("column: within its column"), out.length === 0 && (await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)), out);
      const qaCol = await page.$eval('.rcol[data-name="' + QA + '"] .rbody', (b) => ({
        items: b.querySelectorAll(".queued .ent.qd").length, now: !!b.querySelector('.queued [data-act="a:send-now"]') }))
        .catch(() => null);
      check(W("column: another agent's, in its own column, no Send now where none is offered"),
        qaCol && qaCol.items === 1 && !qaCol.now, qaCol);
      await page.$eval(COL + " .queued", (e) => e.scrollIntoView({ block: "end" }));
      await shot("columns");
      await tap(COL + ' [data-act="a:send-now"]');
      await sleep(80);
      check(W("column: Send now tells Emacs"), intents.filter((i) => i.op === "agent-send-now").length === 2 &&
        lastOf("agent-send-now").name === RD && lastOf("agent-send-now").text === Q1, lastOf("agent-send-now"));

      // ---- Delivered: the transcript says it before the screen lets go ----
      await page.evaluate((c) => GM.setConversation(c), CONV([{ role: "user", who: "", kind: "prompt", text: Q1, ts: iso(0) }]));
      await sleep(100);
      q = await colQueue();
      check(W("transcript first: said once, as the conversation's"),
        q.said.filter((t) => t === Q1).length === 1 && q.items.map((i) => i.text).join("|") === Q2, q);
      // ---- ... and the screen lets go before the transcript says it ----
      await show({ rd: { queued: [], queuedNow: false }, qa: BOTH.qa });
      await sleep(100);
      q = await colQueue();
      check(W("screen first: still there, leaving -- not gone in between"),
        q.items.length === 1 && q.items[0].text === Q2 && q.items[0].leaving && !q.now, q);
      check(W("screen first: the card no longer counts it"), (await page.$eval(COL + " .rtop", (t) => !!t.querySelector(".qdw"))) === false);
      await page.evaluate((c) => GM.setConversation(c), CONV([{ role: "user", who: "", kind: "prompt", text: Q1, ts: iso(0) },
                                                              { role: "user", who: "", kind: "prompt", text: Q2, ts: iso(0) }]));
      await sleep(100);
      q = await colQueue();
      check(W("then the transcript: said once, as the conversation's"),
        q.said.filter((t) => t === Q2).length === 1 && q.items.length === 0, q);

      // ---- Cut, then whole: one message, not two ----
      await show({ rd: { queued: ["typed at agy's terminal: what is 2+2, and keep this one lo..."], queuedNow: true } });
      await sleep(80);
      await show({ rd: { queued: ["typed at agy's terminal: what is 2+2, and keep this one long enough to wrap", "page: 6+6?"], queuedNow: true } });
      await sleep(80);
      q = await colQueue();
      check(W("drawn cut, then whole: still one, not leaving"),
        q.items.length === 2 && q.items[0].text === "typed at agy's terminal: what is 2+2, and keep this one long enough to wrap" &&
        !q.items[0].leaving && q.items[1].text === "page: 6+6?", q);
      await show({ rd: { queued: [], queuedNow: false } });
      await ctx.clock.fastForward(11000);
      await sleep(150);

      // ---- Taken back by hand: leaves after a moment ----
      await show({ rd: { queued: [Q3], queuedNow: true } });
      await sleep(80);
      await show({ rd: { queued: [], queuedNow: false } });
      await sleep(80);
      q = await colQueue();
      check(W("taken back: leaving a moment"), q.items.length === 1 && q.items[0].leaving, q);
      await ctx.clock.fastForward(11000);
      await sleep(150);
      q = await colQueue();
      check(W("then gone"), q.items.length === 0 && !q.said.includes(Q3), q);

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
