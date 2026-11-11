// The Board folded, on a big day (board-big-fixture.js: 60 需要 Jing lines
// of one lead over 20 items, 7 of another, a question that stops an agent;
// 63 items).  要你處理 by agent: a group a line -- its counts, how new, 到對話,
// 整組標為已處理 -- the one with what stops an agent first and open, the rest
// folded; in a group its latest report's lines, then the earlier ones by
// item, folded; an entry one line, its words once, opened in place.
// 整組標為已處理 asks for a second tap, handles the group's lines alone, in one
// intent, and says how many.  Cards a line or two until opened, kept so
// for the device; sections folded by their heads, kept; chips on top to
// each.  到對話: the agent's room, at the entry that said it, flashed -- held
// there or read for (room-at); found nowhere, the room's end, said so; 回
// Board where the Board was.  Desk (columns) and phone (chat); Emacs
// played by the test.  Synthetic data.
const { webkit } = require("playwright");
const { phonePage, sleep, HERD, agent } = require("./herdlib");
const { BIG, INBOX, NEEDS, AGENTS, LEAD, LEAD2, AGY, P, now } = require("./board-big-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1360, height: 980 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const ops = (intents, op) => intents.filter((i) => i.op === op);
const herdOf = (agents) => Object.assign({}, HERD, { work: [], asks: [],
  agents: agents.map((a) => agent(a.name, { project: a.project, role: a.role, state: a.role ? "idle" : "working", kind: "claude",
                                            reason: a.role ? "" : "at work", meter: false, subs: [] })) });
// A stop of lead-beta's own: its group then has what stops it, and lines.
const BLOCKED = { id: "blocked:" + LEAD2, kind: "blocked", group: "need", tag: "Permission", tone: "warn", agent: LEAD2, cli: "claude",
  project: P("beta"), title: "Permission: run helm upgrade", preview: "Bash: helm upgrade", at: now - 3 * 60e3, read: false, override: null,
  whyLabel: "", body: "helm upgrade --install", state: "", options: [{ n: 1, label: "Yes" }, { n: 2, label: "No" }] };
const iso = (ms) => new Date(ms).toISOString();
const latest = (agent) => Math.max(...NEEDS.filter((n) => n.agent === agent).map((n) => n.at));

// A lead's conversation, its last 30 entries of 1000: one of them (995)
// the report that wrote its latest lines.
const REPORT_AT = latest(LEAD);
const REPORT = NEEDS.filter((n) => n.agent === LEAD && n.at === REPORT_AT);
const entry = (i) => i === 995
  ? { role: "assistant", kind: "", text: "這輪做完了。\n\n" + REPORT.map((n) => "- **需要 Jing**：" + n.text).join("\n"), ts: iso(REPORT_AT) }
  : { role: i % 2 ? "assistant" : "user", kind: i % 2 ? "" : "prompt", text: (i % 2 ? "回覆 " : "問題 ") + i + " " + "細節如下，".repeat(20),
      ts: iso(REPORT_AT - (996 - i) * 60e3) };
const range = (a, b) => Array.from({ length: b - a }, (_, k) => entry(a + k));
const TAIL = range(970, 1000);
const hit = (i, e, extra) => Object.assign({ i, role: e.role, kind: e.kind || "", who: "", to: "", ask: "", ts: e.ts, head: e.text.slice(0, 24),
  snippet: e.text.replace(/\s+/g, " ").slice(0, 120) }, extra || {});
const chatItems = (name, es) => es.map((e) => e.role === "user"
  ? { who: "user", to: name, kind: "prompt", text: e.text, ts: e.ts, agent: name }
  : { who: name, kind: "turn", text: e.text, ts: e.ts, agent: name });

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const W = (n) => where + ": " + n;
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {}, hash: "#board" });
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const inbox = INBOX.concat([BLOCKED]);
      const push = async (v) => {
        await page.evaluate((items) => GM.renderInbox({ items, count: items.length, needsOnly: true }), inbox);
        await page.evaluate((p) => GM.boardPatch(p), Object.assign({}, BIG, { v: v || 1, full: true }));
        await sleep(150);
      };
      await page.evaluate(() => { try { localStorage.clear(); } catch (e) { /* none */ } });
      await page.reload();
      await page.waitForFunction(() => window.GM && window.XW);
      await sleep(120);
      await push();

      // ---- 要你處理, by agent ----------------------------------------------
      const groups = await page.$$eval(".bdneeds .bdng", (gs) => gs.map((g) => ({ a: g.getAttribute("data-agent"), open: g.classList.contains("open"),
        head: g.querySelector(".bdngt").textContent, talk: !!g.querySelector('.bdngh [data-act="bd:talk"]'),
        all: (g.querySelector('.bdngh [data-act="bd:nall"]') || {}).textContent || "" })));
      check(W("要你處理 is a group an agent: what stops one first, then the newest"),
        groups.map((g) => g.a).join() === [LEAD2, AGY, LEAD].join() || groups.map((g) => g.a).join() === [AGY, LEAD2, LEAD].join(), groups.map((g) => g.a));
      const beta = groups.find((g) => g.a === LEAD2), alpha = groups.find((g) => g.a === LEAD), gamma = groups.find((g) => g.a === AGY);
      check(W("a group with what stops an agent opens by itself; the rest are folded"), beta.open && gamma.open && !alpha.open, groups);
      check(W("a group's head: its counts by kind, how new, 到對話, 整組標為已處理 where it has lines"),
        /需要 Jing 60/.test(alpha.head) && /19m/.test(alpha.head) && /會卡住 agent 1/.test(beta.head) && /需要 Jing 7/.test(beta.head) &&
        alpha.talk && gamma.talk && alpha.all === "整組標為已處理" && !gamma.all, groups);
      const top = await page.evaluate(() => ({ total: document.querySelector("#app").scrollHeight,
        need: Math.round(document.querySelector("#bd-need").getBoundingClientRect().height) }));
      check(W("68 waits fold into a screenful"), top.need < (where === "phone" ? 700 : 500), top);

      // Opened: its latest report's lines, then the earlier ones folded.
      await tap('.bdngt[data-agent="' + LEAD + '"]');
      await sleep(100);
      const rows = await page.$$eval('.bdng[data-agent="' + LEAD + '"] .bdngb > .bdnrow', (rs) => rs.map((r) => r.getAttribute("data-id")));
      const want = REPORT.map((n) => "need:" + n.id);
      check(W("opened: its latest report's lines first, those alone"), rows.join() === want.join(), { rows, want });
      const old = await page.$eval('.bdnold[data-agent="' + LEAD + '"]', (b) => ({ t: b.textContent, x: b.getAttribute("aria-expanded") }));
      check(W("then the earlier ones, folded, said so"), old.t.includes("較早的 " + (60 - want.length) + " 件（之後的報告沒再提）") && old.x === "false" &&
        !(await page.$('.bdng[data-agent="' + LEAD + '"] .bdnolds')), old);
      await tap('.bdnold[data-agent="' + LEAD + '"]');
      await sleep(100);
      const subs = await page.$$eval('.bdng[data-agent="' + LEAD + '"] .bdnolds .bdnitem', (hs) => hs.map((h) => h.textContent));
      const oldRows = await page.$$eval('.bdng[data-agent="' + LEAD + '"] .bdnolds .bdnrow', (rs) => rs.length);
      check(W("the earlier lines by work item, each its item and count"), oldRows === 60 - want.length && subs.length >= 15 &&
        subs.every((t) => /^a1[0-9a-f]{4} alpha 的工作 a1[0-9a-f]{4} \d+ 件$/.test(t.replace(/\s+/g, " ").trim())), subs.slice(0, 3));

      // A line each: its words once, no preview.
      const line = await page.$eval('.bdng[data-agent="' + LEAD + '"] .bdnrow', (r) => {
        const t = r.querySelector(".ibtitle").textContent;
        const b = r.querySelector(".bdnr").getBoundingClientRect();
        return { h: Math.round(b.height), titleH: Math.round(r.querySelector(".ibtitle").getBoundingClientRect().height),
          times: r.textContent.split(t).length - 1, prev: !!r.querySelector(".ibcardprev"),
          btns: [...r.querySelectorAll(".bdnl > button")].map((x) => x.getAttribute("data-act")),
          tag: r.querySelector(".ibtag").textContent, age: r.querySelector(".ibage").textContent };
      });
      check(W("an entry is one line: tag, title, age, 標為已處理, 到對話 -- its words once, no preview"),
        line.titleH <= 22 && line.times === 1 && !line.prev && line.tag === "需要 Jing" && /m$/.test(line.age) &&
        line.btns.join() === "ib:pick,ib:need-done,bd:talk", line);
      await tap('.bdng[data-agent="' + LEAD + '"] .bdnrow .bdnr');
      await sleep(100);
      const opened = await page.$eval(".bdnrow.open", (r) => ({ id: r.getAttribute("data-id"), txt: r.textContent,
        title: r.querySelector(".ibtitle").textContent, body: !!r.querySelector(".bdnbody"),
        item: (r.querySelector('.bdnbody [data-act="ib:need-board"]') || {}).textContent || "" }));
      check(W("tapped: its detail under it, its words still once"), opened.id === want[0] && opened.body &&
        opened.txt.split(opened.title).length - 1 === 1 && /到工作項目 a1/.test(opened.item), opened);
      await tap(".bdnrow.open .bdnr");
      await sleep(80);
      check(W("tapped again: folded"), !(await page.$(".bdnrow.open")));

      // ---- 整組標為已處理 -----------------------------------------------------
      const n0 = intents.length;
      await tap('.bdng[data-agent="' + LEAD2 + '"] [data-act="bd:nall"]');
      await sleep(80);
      const armed = await page.$eval('.bdng[data-agent="' + LEAD2 + '"] [data-act="bd:nall"]', (b) => ({ t: b.textContent, arm: b.classList.contains("arm") }));
      check(W("整組標為已處理 asks again first, sending nothing"), armed.arm && armed.t === "確定？標 7 件" &&
        !ops(intents.slice(n0), "board-need-done").length, armed);
      await sleep(5300);
      check(W("left alone, it lets go"), (await page.textContent('.bdng[data-agent="' + LEAD2 + '"] [data-act="bd:nall"]')) === "整組標為已處理");
      await tap('.bdng[data-agent="' + LEAD2 + '"] [data-act="bd:nall"]');
      await sleep(80);
      await tap('.bdng[data-agent="' + LEAD2 + '"] [data-act="bd:nall"]');
      await sleep(120);
      const sent = ops(intents.slice(n0), "board-need-done");
      const ids = NEEDS.filter((n) => n.agent === LEAD2).map((n) => n.id).sort((a, b) => a - b);
      check(W("tapped again: one intent, the group's lines, each its event"), sent.length === 1 &&
        JSON.stringify((sent[0].ids || []).slice().sort((a, b) => a - b)) === JSON.stringify(ids) && sent[0].id === undefined, sent);
      const after = await page.evaluate((a) => ({ rows: [...document.querySelectorAll('.bdng[data-agent="' + a + '"] .bdnrow')].map((r) => r.getAttribute("data-id")),
        toast: (document.querySelector(".bd-need .ibtoast") || {}).textContent || "",
        head: document.querySelector(".bd-need .section").textContent }), LEAD2);
      check(W("its lines gone at once, what stops the agent left, how many said"),
        after.rows.join() === "blocked:" + LEAD2 && /已把 lead-beta 的 7 件「需要 Jing」標為已處理/.test(after.toast) && /62 件/.test(after.head), after);
      await push(2);
      check(W("a push from before Emacs closed them does not bring them back"),
        (await page.$$eval('.bdng[data-agent="' + LEAD2 + '"] .bdnrow', (rs) => rs.length)) === 1);

      // ---- The cards: a line or two, opened kept --------------------------
      const cards = await page.$$eval(".bdarea .wi", (ws) => ws.map((w) => ({ id: w.id.slice(5), folded: w.classList.contains("folded"),
        h: Math.round(w.getBoundingClientRect().height), urgent: w.classList.contains("urgent"),
        pill: (w.querySelector(".pill") || {}).textContent, tags: [...w.querySelectorAll(".tag")].map((t) => t.textContent) })));
      check(W("every card starts folded, a line or two"), cards.length > 20 && cards.every((c) => c.folded && c.h <= 64),
        cards.filter((c) => !c.folded || c.h > 64).slice(0, 3));
      // One waited on long: its lead's wait, not 沒人接 -- nor is the one
      // whose last ask went nowhere besides, its lines open.
      const long = BIG.cards.find((c) => c.late);
      const jc = cards.find((c) => c.id === "a10000"), lc = cards.find((c) => c.id === long.id);
      check(W("folded, one waiting on Jing still shows it, how long once long: marked, and how many -- none nobody's"),
        jc.urgent && /^等 Jing \d+$/.test(jc.pill) && lc.urgent && /^等你 \d+ 小時 · \d+$/.test(lc.pill) &&
        !cards.some((c) => c.pill === "沒人接"), { jc, lc });
      const card0 = await page.$eval("#bd-w-a10000 .wfold", (b) => b.textContent);
      check(W("its line or two: title, status, projects, asks, PRs"), /alpha 的工作 a10000/.test(card0) && /alpha、infra/.test(card0) &&
        /\d+ ask/.test(card0) && /2 PR/.test(card0), card0);
      await tap("#bd-w-a10000 .wfold");
      await sleep(100);
      const full = await page.$eval("#bd-w-a10000", (w) => ({ folded: w.classList.contains("folded"), prs: w.querySelectorAll(".bdpr").length,
        needs: w.querySelectorAll(".bdneed").length, talks: w.querySelectorAll('.bdneed [data-act="bd:talk"]').length }));
      check(W("tapped: the whole card, a 到對話 by each of its 需要 Jing"), !full.folded && full.prs === 2 && full.needs >= 2 && full.talks === full.needs, full);
      check(W("kept for this device"), (await page.evaluate(() => localStorage.getItem("ghostherd.board.open"))) === '["a10000"]');

      // ---- Sections and their chips ----------------------------------------
      const chips = await page.$$eval(".bd-top .bdjchip", (cs) => cs.map((c) => c.textContent.replace(/\s+/g, " ").trim()));
      check(W("beside the stretch, a chip a section, its count"),
        chips.join("|") === "要你處理 62|現在 3|工作項目 63|PR 62|單號 23", chips);
      await tap('.bdsech[data-sec="now"]');
      await sleep(100);
      check(W("a section's head folds it"), await page.$eval('.bdsecb[data-sec="now"]', (b) => b.hidden) &&
        (await page.getAttribute('.bdsech[data-sec="now"]', "aria-expanded")) === "false");
      await tap('.bdsech[data-sec="tickets"]');
      await sleep(80);
      // Loaded again: what was opened and closed, as it was.
      await page.reload();
      await page.waitForFunction(() => window.GM && window.XW);
      await sleep(120);
      await push(3);
      const kept = await page.evaluate(() => ({ now: document.querySelector('.bdsecb[data-sec="now"]').hidden,
        tickets: document.querySelector('.bdsecb[data-sec="tickets"]').hidden, items: document.querySelector('.bdsecb[data-sec="items"]').hidden,
        card: !document.querySelector("#bd-w-a10000").classList.contains("folded"),
        others: document.querySelectorAll(".bdarea .wi:not(.folded)").length }));
      check(W("loaded again: the sections closed and the card opened as they were"), kept.now && kept.tickets && !kept.items && kept.card && kept.others === 1, kept);
      await tap('.bdjchip[data-to="tickets"]');
      await sleep(700);
      const jumped = await page.evaluate(() => ({ open: !document.querySelector('.bdsecb[data-sec="tickets"]').hidden,
        top: Math.round(document.querySelector("#bd-tickets").getBoundingClientRect().top) }));
      check(W("a closed section's chip opens it and goes to it"), jumped.open && jumped.top >= 0 && jumped.top < 200, jumped);
      await tap('.bdjchip[data-to="prs"]');
      await sleep(700);
      const prsTop = await page.evaluate(() => Math.round(document.querySelector("#bd-prs").getBoundingClientRect().top));
      check(W("a chip goes to its section"), prsTop >= 0 && prsTop < 200, prsTop);
      await tap("#bd-w-a10000 .wh");
      await sleep(80);
      check(W("a card folded again is kept so"), (await page.evaluate(() => localStorage.getItem("ghostherd.board.open"))) === "[]");
      if (where === "phone") {
        const wide = await page.evaluate(() => ({ doc: document.documentElement.scrollWidth, w: window.innerWidth,
          over: [...document.querySelectorAll(".bd-top *, .bd-need *")].filter((e) => e.getBoundingClientRect().right > window.innerWidth + 1).length }));
        check(W("the chips wrap, nothing sideways"), wide.doc <= wide.w && wide.over === 0, wide);
        const hs = await page.$$eval(".bd-top .bdjchip, .bdng .bdnb, .bdngt", (bs) => bs.map((b) => Math.round(b.getBoundingClientRect().height)));
        check(W("its buttons a finger's size"), hs.every((x) => x >= 36), hs);
      }

      // ---- 到對話 ---------------------------------------------------------
      const chat = where === "phone";
      const body = chat ? ".chat .cbody" : '.rcol[data-name="' + LEAD + '"] .rbody';
      const live = async (name, es) => {
        await page.evaluate((h) => GM.renderHerd(h), herdOf(AGENTS));
        await sleep(150);
        if (chat) await page.evaluate((p) => GM.setChat(p), { project: P(name === LEAD ? "alpha" : "beta"), earlier: true, missing: [], absent: [], entries: chatItems(name, es) });
        else await page.evaluate((p) => GM.setConversation(p), { name, path: "/Users/x/.claude/projects/-p/" + name + ".jsonl", cli: "claude", earlier: true, entries: es });
        await sleep(150);
      };
      const lastFind = () => ops(intents, "room-find").slice(-1)[0];
      const found = (f, hits) => page.evaluate((p) => GM.foundConversation(p), { name: f.name, q: f.q, seq: f.seq, total: hits.length, capped: false, entries: 1000, hits });
      const inView = (sel) => page.evaluate(([s, b]) => {
        const el = document.querySelector(s), box = document.querySelector(b);
        if (!el || !box) return false;
        const r = el.getBoundingClientRect(), br = box.getBoundingClientRect();
        return r.bottom > br.top + 4 && r.top < br.bottom - 4 && r.top < window.innerHeight;
      }, [sel, body]);

      // (1) A line of the latest report: held in the room's tail.
      await page.evaluate(() => { document.querySelector("#app").scrollTop = 0; });
      await tap('.bdngt[data-agent="' + LEAD + '"]');   // folded, as loaded again it was not opened
      await sleep(100);
      if (!(await page.$('.bdng[data-agent="' + LEAD + '"].open'))) { await tap('.bdngt[data-agent="' + LEAD + '"]'); await sleep(100); }
      await page.$eval('.bdng[data-agent="' + LEAD + '"] .bdnrow [data-act="bd:talk"]', (b) => b.scrollIntoView({ block: "center" }));
      await sleep(80);
      const boardAt = await page.evaluate(() => document.querySelector("#app").scrollTop);
      const f0 = intents.length;
      await tap('.bdng[data-agent="' + LEAD + '"] .bdnrow [data-act="bd:talk"]');
      await sleep(120);
      const rw = ops(intents.slice(f0), "room-watch").slice(-1)[0] || {};
      check(W("到對話 opens the agent's room"), rw.project === P("alpha") && (await page.getAttribute("#app", "data-view")) === "agents", rw);
      await live(LEAD, TAIL);
      await sleep(100);
      const f1 = lastFind();
      check(W("and looks for what the line says in its whole conversation, by its first words"),
        !!f1 && f1.name === LEAD && f1.q && f1.q.split(" ").every((w) => REPORT[0].text.includes(w)) && f1.q.length >= 8, f1);
      // Its hits: Jing quoting it later; the report; an older mention.
      const quote = { role: "user", kind: "prompt", text: "你說的「" + REPORT[0].text + "」我再想想", ts: iso(REPORT_AT + 3600e3) };
      await found(f1, [hit(400, entry(401)), hit(995, entry(995)), hit(998, quote)]);
      await sleep(300);
      const sel = chat ? body + ' .cmsg[data-i="25"]' : body + ' .ent[data-i="25"]';
      const flashed = await page.$eval(sel, (e) => e.classList.contains("flash")).catch(() => false);
      check(W("at the report that wrote it -- not Jing's quote of it -- flashed, in view"), flashed && (await inView(sel)), { flashed });
      check(W("the room says the way back"), !!(await page.$(".room .rback")));
      await tap(".room .rback");
      await sleep(250);
      const back = await page.evaluate(() => ({ view: document.querySelector("#app").getAttribute("data-view"), at: document.querySelector("#app").scrollTop }));
      check(W("回 Board: the Board where it was"), back.view === "board" && Math.abs(back.at - boardAt) <= 2, { back, boardAt });

      // (2) An older line, from a card's 需要 Jing: not in the tail -- the page holding it is read.
      await page.evaluate((p) => GM.boardPatch(p), Object.assign({}, BIG, { v: 4, full: true }));
      await sleep(100);
      const oldN = NEEDS.find((n) => n.agent === LEAD && n.at < REPORT_AT && n.item === "a10003") || NEEDS.find((n) => n.agent === LEAD && n.at < REPORT_AT);
      await page.evaluate((id) => { const w = document.querySelector("#bd-w-" + id + " .wfold"); if (w) w.click(); }, oldN.item);
      await sleep(100);
      const cardTalk = '#bd-w-' + oldN.item + ' .bdneed [data-act="bd:talk"][data-q^="' + oldN.text.slice(0, 6) + '"]';
      await page.$eval(cardTalk, (b) => b.scrollIntoView({ block: "center" }));
      await tap(cardTalk);
      await sleep(120);
      await live(LEAD, TAIL);
      await sleep(100);
      const f2 = lastFind();
      const oldEntry = { role: "assistant", kind: "", text: "- 需要 Jing：" + oldN.text, ts: iso(oldN.at) };
      const a0 = intents.length;
      await found(f2, [hit(120, oldEntry)]);
      await sleep(150);
      const at = ops(intents.slice(a0), "room-at")[0];
      check(W("one the room does not hold: the page of its conversation holding it is read"), !!at && at.name === LEAD && at.start === 100, at);
      const page2 = Array.from({ length: 60 }, (_, k) => (100 + k === 120 ? oldEntry : entry(100 + k)));
      await page.evaluate((p) => GM.atConversation(p), { name: LEAD, seq: at.seq, start: 100, total: 1000, earlier: true, later: true, entries: page2 });
      await sleep(300);
      const awaySel = body + ' .away .ent[data-i="20"]';
      check(W("and shown, at it, flashed; Back to latest there"), (await page.$eval(awaySel, (e) => e.classList.contains("flash")).catch(() => false)) &&
        (await inView(awaySel)) && !!(await page.$(body + ' [data-act="a:away-back"]')));
      await tap(".room .rback");
      await sleep(250);

      // (3) Said nowhere it can be found: the room's end, and says so.
      await page.evaluate((p) => GM.boardPatch(p), Object.assign({}, BIG, { v: 5, full: true }));
      await sleep(100);
      const t0 = intents.length;
      await tap('.bdng[data-agent="' + LEAD + '"] .bdngh [data-act="bd:talk"]');
      await sleep(120);
      await live(LEAD, TAIL);
      await sleep(100);
      const g1 = ops(intents.slice(t0), "room-find");
      await found(g1[0], []);
      await sleep(150);
      const g2 = ops(intents.slice(t0), "room-find");
      check(W("not found by its first words: by its longest, once more"), g2.length === 2 && g2[1].q !== g2[0].q, g2.map((f) => f.q));
      if (g2[1]) await found(g2[1], []);
      await sleep(200);
      const end = await page.evaluate((b) => { const e = document.querySelector(b); return e ? e.scrollHeight - e.scrollTop - e.clientHeight : -1; }, body);
      check(W("found nowhere: the room at its end, said so, its find closed"),
        /找不到那一則：停在對話最後面/.test(await page.textContent("#status")) && end >= 0 && end < 40 && !(await page.$(body.split(" ")[0] + " .jump.find")), { end });
      await tap(".room .rback");
      await sleep(200);

      // An open ask's 到對話: its taker's room, the hit of its ask picked.
      const waitTalk = '.bdwaits .wait [data-act="bd:talk"]';
      if (await page.$('.bdsecb[data-sec="now"][hidden]')) { await tap('.bdsech[data-sec="now"]'); await sleep(80); }
      const wq = await page.$eval(waitTalk, (b) => ({ name: b.getAttribute("data-name"), ask: b.getAttribute("data-ask"), want: b.getAttribute("data-want") }));
      check(W("an open ask's 到對話 is to its taker, as it came in"), wq.name === "rd-alpha" && wq.ask === "op000001" && wq.want === "ask-in", wq);
      // A reload drops the intent on its way: WebKit says so as an error.
      const errs = errors.filter((e) => !/access control checks|network connection was lost/.test(e));
      check(W("no page errors"), !errs.length, errs);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
