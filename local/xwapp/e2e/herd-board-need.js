// The Board's top: the stretch it is of, and 要你處理.  One button pair
// on top -- 24 小時 | 7 天 -- for the whole page, the tiles saying which
// stretch their figures are of; then one list of what needs Jing: the
// Inbox's Needs-you, the leads' 需要 Jing lines, the items nobody has --
// each once, what stops an agent first, then what else needs Jing, then
// what nobody has, the newest first in each -- each handled right there,
// by the Inbox's own cards and actions: the same intents the Inbox sends,
// and the Inbox has it no more.  Drawn from the Inbox's push before the
// board has come; the Inbox watched while the Board is shown.  Desk and
// phone; Emacs played by the test (board-fixture.js, and the Inbox's
// items here).  Synthetic data.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const { FULL, CARDS, META, now } = require("./board-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const min = 60000;
const ops = (intents, op) => intents.filter((i) => i.op === op);
// The same intents, their keys in any order.
const canon = (v) => JSON.stringify(v, (k, x) => (x && typeof x === "object" && !Array.isArray(x)
  ? Object.keys(x).sort().reduce((o, key) => { o[key] = x[key]; return o; }, {}) : x));
const same = (a, b) => canon(a) === canon(b);

// The Inbox's items, as Emacs sends them: six that need Jing (two the
// board's too: the question and the lead's line need:11), a report (an
// update), and a sign-in the user moved to Updates.
const it = (id, kind, tag, tone, agent, ago, extra) => Object.assign({ id, kind, group: "need", tag, tone, agent, cli: "claude",
  project: "~/src/infra/", title: id, preview: "", at: now - ago * min, read: false, override: null, whyLabel: "", body: "", state: "" }, extra || {});
const ITEMS = [
  it("blocked:pushfix", "blocked", "Permission", "warn", "pushfix", 6, { project: "~/src/mobile/", title: "Permission: run xcodebuild test",
    options: [{ n: 1, label: "Yes" }, { n: 2, label: "No, and tell it why" }] }),
  it("guard:g1", "guard", "Guard", "red", "certrot", 2, { guardId: "g1", title: "Guard: kubectl delete secret tls-staging", code: "kubectl delete secret tls-staging" }),
  it("question:lead-infra:1", "question", "Question", "accent", "lead-infra", 30, { title: "rotate.sh 要現在跑，還是等今晚的維護時段？", replyVia: "lead" }),
  it("proposal:5", "proposal", "Proposal", "purple", "lead-infra", 50, { proposalId: 5, title: "Promote 1 fact into CLAUDE.md", diff: "+ a line", facts: ["rotate order"] }),
  it("need:11", "need", "需要 Jing", "red", "lead-infra", 35, { cli: "", title: "在今晚的維護時段，照順序逐個 namespace 跑 rotate.sh", event: 11, item: "b1c2d3" }),
  it("ask-failed:abcd1234", "ask-failed", "Ask failed", "red", "lead-shop", 10, { project: "~/src/shop/", title: "The ask to cartfix failed" }),
  it("report:copybot:1", "report", "Report", "muted", "copybot", 40, { group: "update", project: "~/src/shop/", title: "Copy fixed" }),
  it("login:helmsync", "login", "Login", "warn", "helmsync", 3, { override: "update", project: "~/src/ops-helm/", title: "helmsync needs /login" }),
];
// One more item nobody has, with no line of Jing's: its last ask failed.
const ORPHAN = Object.assign({}, CARDS.find((c) => c.id === "9a8b7c"), { id: "orph01", status: "orphan", title: "對帳報表沒人接",
  projects: ["billing"], agents: ["lead-billing"], last: now - 300 * min, labels: [], tickets: [],
  unanswered: { status: "failed", from: "lead-billing", to: "ratefix", head: "對帳報表重跑一次" } });
const BOARD = Object.assign({}, FULL, { cards: FULL.cards.concat([ORPHAN]) });
const ORDER = ["guard:g1", "blocked:pushfix", "question:lead-infra:1", "proposal:5", "ask-failed:abcd1234", "need:11", "need:12", "orphan:orph01"];

const listed = (page) => page.$$eval(".bdneeds .ibcard", (cs) => cs.map((c) => c.getAttribute("data-id")));
const open = (page) => page.$$eval(".bdneeds .ibcard.open", (cs) => cs.map((c) => c.getAttribute("data-id")));

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const W = (n) => where + ": " + n;
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      await sleep(100);

      // ---- What Emacs watches ------------------------------------------------
      await tap('.tab[data-tab="agents"]');
      await sleep(80);
      const at0 = intents.length;
      await tap('.tab[data-tab="board"]');
      await sleep(100);
      const asked = intents.slice(at0).filter((i) => /watch/.test(i.op));
      check(W("the Board opened asks for the board and the Inbox's push in one intent: no round trip between them"),
        same(asked, [{ op: "herd-unwatch" }, { op: "board-watch", days: 1, inbox: true }]), asked);
      check(W("the stretch on top before anything has come, 24 小時 picked"),
        await page.evaluate(() => { const b = document.querySelector('.bd-top [data-act="bd:days"][data-days="1"]');
          return !!b && b.getAttribute("aria-pressed") === "true" && !!document.querySelector('.bd-top [data-days="7"]'); }));

      // ---- Drawn from the Inbox's push, before the board ------------------
      await page.evaluate((items) => GM.renderInbox({ items, count: 6 }), ITEMS);
      await sleep(100);
      const early = await listed(page);
      check(W("the Inbox's push alone: what needs Jing drawn, the board still being read"),
        early.join() === ORDER.filter((id) => id !== "need:12" && id !== "orphan:orph01").join() &&
        (await page.textContent(".bd-main")).includes("讀取"), early);

      // ---- The board come: all three, each once, in order --------------------
      await page.evaluate((p) => GM.boardPatch(p), BOARD);
      await sleep(150);
      const all = await listed(page);
      check(W("three sources, each once, in order: what stops an agent, what else needs Jing, what nobody has"),
        all.join() === ORDER.join(), all);
      check(W("a lead's line the Inbox has is its item, not a second one"), all.filter((id) => id === "need:11").length === 1);
      check(W("an item nobody has whose line is listed rides on that line, said so"),
        await page.$eval('.bdneeds [data-id="need:12"]', (c) => /沒人接/.test(c.textContent)) && !all.includes("orphan:c6d7e8"));
      check(W("updates and what was moved to Updates are not here"), !all.includes("report:copybot:1") && !all.includes("login:helmsync"));
      const head = await page.textContent(".bd-need .section");
      check(W("its head counts them by kind"), /要你處理/.test(head) && /8 件/.test(head) && /會卡住 agent 4/.test(head) &&
        /需要 Jing 3/.test(head) && /沒人接 1/.test(head), head);
      const tile = await page.$eval('.bdtile[data-to="need"]', (t) => ({ big: t.querySelector(".big").textContent, tl: t.querySelector(".tl1").textContent }));
      check(W("the tile once 等 Jing is 要你處理, its count the list's, a way to it"), tile.big === "8" && /要你處理/.test(tile.tl) &&
        !!(await page.$("#bd-need")) && !(await page.$("#bd-jing")), tile);
      const stack = await page.evaluate(() => ["bd-top", "bd-need", "bd-strip"].map((c) => Math.round(document.querySelector("." + c).getBoundingClientRect().top)));
      check(W("on top: the stretch, then 要你處理, then the tiles"), stack[0] < stack[1] && stack[1] < stack[2], stack);

      // ---- The tiles say which stretch -----------------------------------------
      const tlr = await page.$$eval(".bdtile .tlr", (ts) => ts.map((t) => t.textContent));
      check(W("each tile says its stretch: the items and PRs 24 小時內, what is open and needs you 現在"),
        tlr.join("|") === "24 小時內|現在|現在|24 小時內", tlr);

      // ---- A card opens in place; its buttons do not close it -------------------
      await tap('.bdneeds [data-act="ib:pick"][data-id="question:lead-infra:1"]');
      await sleep(80);
      check(W("a tap opens a card in place"), (await open(page)).join() === "question:lead-infra:1");
      await page.click('.bdneeds .ibcard.open .ibreplybox');
      await page.click('.bdneeds .ibcard.open .ibwhy');
      await sleep(60);
      check(W("taps in it -- its box, its words -- leave it open"), (await open(page)).join() === "question:lead-infra:1");
      await tap('.bdneeds [data-act="ib:pick"][data-id="blocked:pushfix"]');
      await sleep(60);
      check(W("another tapped: that one open, one at a time"), (await open(page)).join() === "blocked:pushfix");
      await tap('.bdneeds .ibcard.open .ibcardhead');
      await sleep(60);
      check(W("its head tapped: closed"), (await open(page)).length === 0);

      // ---- Handled here, as in the Inbox ---------------------------------------
      const did = async (id, act, sel) => {
        if (!(await open(page)).includes(id)) await tap('.bdneeds [data-act="ib:pick"][data-id="' + id + '"]');
        await sleep(60);
        const n = intents.length;
        await tap('.bdneeds .ibcard.open ' + sel);
        await sleep(100);
        return intents.slice(n).filter((i) => i.op !== "inbox-set" || act === "inbox-set");
      };
      let sent = await did("blocked:pushfix", "agent-answer", '[data-act="ib:answer"][data-n="1"]');
      check(W("a menu answered: its option sent to the agent"), same(sent, [{ op: "agent-answer", name: "pushfix", n: 1 }]), sent);
      sent = await did("guard:g1", "guard-approve", '[data-act="ib:guard-approve"]');
      check(W("a guarded call approved once"), same(sent, [{ op: "guard-approve", id: "g1" }]), sent);
      await tap('.bdneeds [data-act="ib:pick"][data-id="question:lead-infra:1"]');
      await sleep(60);
      await page.fill('.bdneeds .ibcard.open .ibreplybox', "等今晚的維護時段");
      sent = await did("question:lead-infra:1", "lead-say", '[data-act="ib:reply"]');
      check(W("a question answered, through its lead"), same(sent, [{ op: "lead-say", project: "~/src/infra/", text: "等今晚的維護時段" }]), sent);
      sent = await did("proposal:5", "memory-proposal-reject", '[data-act="ib:prop-reject"]');
      check(W("a proposal rejected"), same(sent, [{ op: "memory-proposal-reject", id: 5, reason: "" }]), sent);
      sent = await did("ask-failed:abcd1234", "inbox-set", '[data-act="ib:done"]');
      check(W("a failed ask marked read"), same(sent, [{ op: "inbox-set", id: "ask-failed:abcd1234", done: true }]), sent);
      sent = await did("need:11", "board-need-done", '[data-act="ib:need-done"]');
      check(W("a lead's line handled"), same(sent, [{ op: "board-need-done", id: 11 }]), sent);
      sent = await did("need:12", "board-need-done", '[data-act="ib:need-done"]');
      check(W("a lead's line only the board had, handled the same"), same(sent, [{ op: "board-need-done", id: 12 }]), sent);
      const left = await listed(page);
      check(W("each handled is gone from the list at once"), left.join() === "orphan:orph01", left);
      check(W("and says what was done"), /已處理/.test(await page.textContent(".bd-need")));

      // ---- An item nobody has -------------------------------------------------
      await tap('.bdneeds [data-act="bd:npick"][data-id="orphan:orph01"]');
      await sleep(80);
      const orph = await page.$eval('.bdneeds .ibcard.open', (c) => c.textContent);
      check(W("an item nobody has opens to why, with 交給 lead and the way to its card"), /失敗/.test(orph) && /對帳報表重跑一次/.test(orph) &&
        !!(await page.$('.bdneeds .ibcard.open [data-act="bd:hand"]')) && !!(await page.$('.bdneeds .ibcard.open [data-act="bd:goto"][data-id="orph01"]')), orph);
      await tap('.bdneeds .ibcard.open [data-act="bd:goto"]');
      await sleep(700);
      const seen = await page.evaluate(() => { const c = document.getElementById("bd-w-orph01"); const r = c && c.getBoundingClientRect();
        return !!c && !c.classList.contains("folded") && r.top >= 0 && r.top < window.innerHeight; });
      check(W("to its card: opened and in view"), seen);

      // ---- Emacs's next push has them no more; nor has the Inbox ----------------
      const handled = ["blocked:pushfix", "guard:g1", "question:lead-infra:1", "proposal:5", "ask-failed:abcd1234", "need:11"];
      // A push sent before Emacs had the actions: the handled stay out a while.
      await page.evaluate((items) => GM.renderInbox({ items, count: 6 }), ITEMS);
      await sleep(80);
      check(W("a push from before the actions does not bring them back"), (await listed(page)).join() === "orphan:orph01", await listed(page));
      await page.evaluate((items) => GM.renderInbox({ items, count: 0 }), ITEMS.filter((x) => !handled.includes(x.id)));
      // The board looked at again: the lines closed, the item whose line it
      // was at rest, the other one's ask asked again.
      await page.evaluate((p) => GM.boardPatch(Object.assign({}, p, { v: 2, jing: p.jing.filter((j) => j.id !== "need:11" && j.id !== "need:12"),
        cards: p.cards.filter((c) => c.id !== "orph01").map((c) => (c.id === "c6d7e8" ? Object.assign({}, c, { status: "quiet", needs: [] })
          : c.id === "b1c2d3" ? Object.assign({}, c, { needs: [] }) : c)) })), BOARD);
      await sleep(100);
      const none = await page.evaluate(() => { const p = document.querySelector(".bd-need .bdneednone");
        return { text: p && p.textContent, h: p ? Math.round(p.getBoundingClientRect().height) : 0, head: !!document.querySelector(".bd-need .section"),
                 cards: document.querySelectorAll(".bdneeds .ibcard").length }; });
      check(W("nothing left: one quiet line, not a section"), none.text === "現在沒有要你處理的" && none.h > 0 && none.h < 30 && !none.head && !none.cards, none);
      await sleep(6200);
      check(W("what was done is said a while, then the line alone"), !(await page.$(".bd-need .ibtoast")));
      const leaving = intents.length;
      await tap('.tab[data-tab="inbox"]');
      await sleep(120);
      const inInbox = await page.evaluate(() => [...document.querySelectorAll(".inboxpage [data-id]")].map((e) => e.getAttribute("data-id")));
      check(W("the Inbox has none of them either"), handled.every((id) => !inInbox.includes(id)), inInbox);
      check(W("leaving the Board for the Inbox keeps its watch"), !ops(intents.slice(leaving), "inbox-unwatch").length &&
        ops(intents.slice(leaving), "board-unwatch").length === 1, intents.slice(leaving).map((i) => i.op));
      await tap('.tab[data-tab="agents"]');
      await sleep(80);
      check(W("leaving for Agents lets the Inbox's watch go"), ops(intents.slice(leaving), "inbox-unwatch").length === 1);

      // ---- The stretch: 7 days, for the whole page -------------------------------
      await tap('.tab[data-tab="board"]');
      await sleep(100);
      await page.evaluate((p) => GM.boardPatch(Object.assign({}, p, { v: 10, full: true })), BOARD);
      await sleep(100);
      const before = await page.$$eval(".bdtile .big", (bs) => bs.map((b) => b.textContent));
      const n = intents.length;
      await tap('.bd-top [data-act="bd:days"][data-days="7"]');
      await sleep(80);
      check(W("7 天 asks Emacs for 7 days, pressed at once"), same(intents.slice(n).filter((i) => i.op === "board-days"), [{ op: "board-days", days: 7 }]) &&
        (await page.getAttribute('.bd-top [data-days="7"]', "aria-pressed")) === "true", intents.slice(n));
      check(W("until the 7 days' board comes, the tiles say theirs is changing"),
        (await page.$$eval(".bdtile .tlr", (ts) => ts.map((t) => t.textContent))).filter((t) => /換成 7 天/.test(t)).length === 2);
      // The 7 days' board: the window's items and PRs more, its asks more.
      const more = CARDS.slice(0, 3).map((c, i) => Object.assign({}, c, { id: "w7" + i, status: "done", last: now - (30 + i) * 60 * min,
        prs: [Object.assign({}, c.prs[0] || { host: "bitbucket", owner: "acme", repo: "shop", id: 900 + i, state: "MERGED", title: "x", own: true }, { id: 900 + i })] }));
      await page.evaluate((p) => GM.boardPatch(p), Object.assign({}, BOARD, { v: 11, full: true, cards: BOARD.cards.concat(more),
        meta: Object.assign({}, META, { days: 7, counts: Object.assign({}, META.counts, { asks: 61 }) }) }));
      await sleep(120);
      const after = await page.$$eval(".bdtile .big", (bs) => bs.map((b) => b.textContent));
      const tl7 = await page.$$eval(".bdtile .tlr", (ts) => ts.map((t) => t.textContent));
      const sub = await page.$eval(".bdtile .sub", (s) => s.textContent);
      check(W("its board come: every figure of the window is of 7 days, and says so"),
        +after[0] === +before[0] + 3 && +after[3] === +before[3] + 3 && /7 天內 61 則 ask/.test(sub) &&
        tl7.join("|") === "7 天內|現在|現在|7 天內", { before, after, tl7, sub });
      check(W("the window's cross-project asks say 7 天 too"), /7 天內的跨專案 ask/.test(await page.textContent('[data-act="bd:graph"][data-g="all"]')));
      check(W("no button at the foot any more"), !(await page.$(".bd-foot")) && (await page.$$('[data-act="bd:days"]')).length === 2);
      // Kept for this browser: the page loaded again (on the Board) asks for 7 days.
      const m = intents.length;
      await page.reload();
      await page.waitForFunction(() => window.GM && window.XW);
      await sleep(150);
      check(W("kept: loaded again, the Board asks for 7 days, 7 天 pressed"), ops(intents.slice(m), "board-watch").map((i) => i.days).join() === "7" &&
        (await page.getAttribute('.bd-top [data-days="7"]', "aria-pressed")) === "true");
      if (where === "phone") {
        const h = await page.$$eval(".bd-top .bdday", (bs) => bs.map((b) => Math.round(b.getBoundingClientRect().height)));
        check(W("the stretch's buttons a finger's size"), h.every((x) => x >= 40), h);
        await page.evaluate((items) => GM.renderInbox({ items, count: 6 }), ITEMS);
        await page.evaluate((p) => GM.boardPatch(Object.assign({}, p, { v: 20, full: true })), BOARD);
        await sleep(120);
        await tap('.bdneeds [data-act="ib:pick"][data-id="proposal:5"]');
        await sleep(100);
        const wide = await page.evaluate(() => ({ doc: document.documentElement.scrollWidth, w: window.innerWidth,
          over: [...document.querySelectorAll(".bd-need *")].filter((e) => e.getBoundingClientRect().right > window.innerWidth + 1).length }));
        check(W("要你處理 fits the screen, a card open, nothing sideways"), wide.doc <= wide.w && wide.over === 0, wide);
        const btn = await page.$$eval(".bdneeds .ibcard.open .ibbtn", (bs) => bs.map((b) => Math.round(b.getBoundingClientRect().height)));
        check(W("its buttons a finger's size"), btn.length > 0 && btn.every((x) => x >= 40), btn);
      }
      check(W("no page errors"), !errors.length, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
