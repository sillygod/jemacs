// Find in a conversation: the room's jump strip turns into a find bar --
// words, a count that opens the list of hits, ↑/↓, ✕.  The words go to
// Emacs (room-find) once typed -- an IME's composition first ends -- the
// hits come back (GM.foundConversation), a hit held is scrolled to and
// flashed, its words marked; one that is not loads the page holding it
// (room-at, GM.atConversation) in place of the live tail, which goes on
// being merged unseen until "Back to latest".  Esc closes the list, then
// the bar.  Columns and the chat, on the desk and on a phone.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const RD = "claude-deploy-platform";
const LEAD = "lead-deploy-platform";
const PATH = "/Users/x/.claude/projects/-p/abc.jsonl";
const herd = () => Object.assign({}, HERD, { agents: HERD.agents.map((a) => Object.assign({}, a, { meter: false, subs: [] })) });
const ts = (i) => new Date(Date.UTC(2026, 10, 3, 1, 0, 0) + i * 60e3).toISOString();

// The whole conversation is 1000 entries; the room holds the last 30.
// Three say 搜尋: 120 (long gone from the tail), 990 and 998 (held).
const SAYS = {
  120: { role: "user", kind: "prompt", text: "請幫我搜尋舊的對話，越早的越好" },
  990: { role: "assistant", text: "好的：我們在房間裡加上搜尋功能，" + "細節如下。".repeat(3) },
  998: { role: "user", kind: "prompt", text: "搜尋 works on the phone too?" },
};
const entry = (i) => Object.assign({ role: i % 2 ? "assistant" : "user", kind: i % 2 ? "" : "prompt",
  text: (i % 2 ? "answer " : "question ") + i + " " + "lorem ipsum dolor sit amet ".repeat(4), ts: ts(i) }, SAYS[i] || {}, { ts: ts(i) });
const range = (a, b) => Array.from({ length: b - a }, (_, k) => entry(a + k));
const TAIL = range(970, 1000);
const hit = (i) => { const e = entry(i); return { i, role: e.role, kind: e.kind || "", who: "", ts: e.ts, head: e.text.slice(0, 24),
  snippet: e.text.replace(/\s+/g, " ").slice(0, 160) }; };
const HITS = [hit(120), hit(990), hit(998)];
// The chat's items for the same agent: who said what, as herd_chat makes them.
const chatItems = (name) => TAIL.map((e) => e.role === "user"
  ? { who: "user", to: name, kind: "prompt", text: e.text, ts: e.ts, agent: name }
  : { who: name, kind: "turn", text: e.text, ts: e.ts, agent: name });

(async () => {
  const browser = await webkit.launch();
  try {
    for (const [where, roomView] of [["desk", "columns"], ["desk", "chat"], ["phone", "chat"], ["phone", "columns"]]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + "/" + roomView + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const lastOf = (op) => intents.filter((i) => i.op === op).pop();
      const count = (op) => intents.filter((i) => i.op === op).length;
      const chat = roomView === "chat";
      const name = chat ? LEAD : RD;
      const strip = chat ? ".chat .cjump" : '.rcol[data-name="' + RD + '"] .rjump';
      const body = chat ? ".chat .cbody" : '.rcol[data-name="' + RD + '"] .rbody';
      const held = chat ? ".cmsg" : ".ent";
      const txt = (sel) => page.$eval(sel, (e) => e.textContent).catch(() => "");
      const has = (sel) => page.$(sel).then((e) => !!e);
      const visibleIn = (sel) => page.$eval(sel, (el, b) => {
        const r = el.getBoundingClientRect(), br = document.querySelector(b).getBoundingClientRect();
        return r.bottom > br.top + 4 && r.top < br.bottom - 4;
      }, body).catch(() => false);
      const live = (entries) => chat
        ? page.evaluate((es) => GM.setChat({ project: "~/src/deploy-platform/", earlier: true, missing: [], absent: [], entries: es }), entries)
        : page.evaluate((es) => GM.setConversation({ name: "claude-deploy-platform", path: "/Users/x/.claude/projects/-p/abc.jsonl", cli: "claude", earlier: true, entries: es }), entries);
      const liveTail = (extra) => live(chat ? chatItems(name).concat(extra || []) : TAIL.concat(extra || []));
      const found = (seq, hits, extra) => page.evaluate((p) => GM.foundConversation(p),
        Object.assign({ name, q: "搜尋", seq, total: hits.length, capped: false, entries: 1000, hits }, extra || {}));
      const at = (seq, start, n) => page.evaluate((p) => GM.atConversation(p),
        { name, seq, start, total: 1000, earlier: start > 0, later: start + n < 1000, entries: range(start, start + n) });

      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      await tap('.room [data-act="a:room-view"][data-view="' + roomView + '"]');
      await sleep(150);
      await liveTail();
      await sleep(80);

      // ---- The strip has a find button; it opens the bar, the box focused ----
      check(W("the strip has a find button beside what you asked"), await has(strip + " .jline") && await has(strip + " .jfind"));
      await tap(strip + " .jfind");
      await sleep(80);
      check(W("the find bar replaces the strip, its box focused"),
        await has(strip + " .jump.find .fq") && !(await has(strip + " .jline")) &&
        await page.evaluate(() => !!(document.activeElement && document.activeElement.classList.contains("fq"))));
      check(W("its box says whose conversation it looks in"),
        (await page.$eval(strip + " .fq", (i) => i.placeholder)).includes(name));

      // ---- Words typed through an IME go once the composition ends ----
      const n0 = count("room-find");
      await page.$eval(strip + " .fq", (i) => {
        i.dispatchEvent(new CompositionEvent("compositionstart", { bubbles: true }));
        i.value = "sou";
        i.dispatchEvent(new InputEvent("input", { bubbles: true, isComposing: true, data: "sou" }));
      });
      await sleep(450);
      check(W("nothing is sent while the IME composes"), count("room-find") === n0);
      await page.$eval(strip + " .fq", (i) => {
        i.value = "搜尋";
        i.dispatchEvent(new CompositionEvent("compositionend", { bubbles: true, data: "搜尋" }));
      });
      await sleep(80);
      check(W("the count waits on the answer"), (await txt(strip + " .fcount")) === "…");
      await sleep(400);
      const rf = lastOf("room-find");
      check(W("the composed words go to Emacs, for the agent"),
        count("room-find") === n0 + 1 && rf.q === "搜尋" && rf.name === name && rf.project === DP && rf.seq > 0, rf);

      // ---- The answer: the count, and the list as the quick look ----
      await found(rf.seq, HITS);
      await sleep(80);
      check(W("the count says how many entries say it"), (await txt(strip + " .fcount")) === "3");
      check(W("the list opens while the box has the keyboard"), await has(strip + " .flist:not([hidden]) .fhit"));
      const rows = await page.$$eval(strip + " .fhit", (rs) => rs.map((r) => ({ k: r.getAttribute("data-k"), t: r.textContent,
        mark: (r.querySelector(".fsnip mark") || {}).textContent || "", who: (r.querySelector(".fwho") || {}).textContent || "" })));
      check(W("the list is newest first, the words marked, who said each"),
        rows.length === 3 && rows[0].k === "2" && rows[0].t.includes("phone too") && rows.every((r) => r.mark === "搜尋") &&
        rows[0].who === "You" && rows[1].who === name, rows);
      // A stale answer (an older seq) is not taken.
      await found(rf.seq - 1, [hit(990)], { total: 1 });
      await sleep(40);
      check(W("an answer to older words is dropped"), (await txt(strip + " .fcount")) === "3");

      // ---- A hit the body holds: scrolled to, flashed, its words marked ----
      await page.$eval(body, (b) => { b.scrollTop = b.scrollHeight; });
      await tap(strip + ' .fhit[data-k="1"]');
      await sleep(120);
      const j990 = chat ? 20 : 20;  // its place in the 30 held
      check(W("going to a held hit closes the list and counts it"),
        !(await has(strip + " .flist:not([hidden])")) && (await txt(strip + " .fcount")) === "2/3");
      check(W("the held hit is flashed, its words marked, in sight"),
        await has(body + " " + held + '[data-i="' + j990 + '"].flash') &&
        (await txt(body + " " + held + '[data-i="' + j990 + '"] mark.fm')) === "搜尋" &&
        await visibleIn(body + " " + held + '[data-i="' + j990 + '"] mark.fm'), await txt(body + " mark.fm"));
      const n1 = count("room-at");
      check(W("no page is read for it"), n1 === 0);

      // A poll redrawing the live body keeps the hit's words marked.
      await liveTail();
      await sleep(60);

      // ---- ↑ to a hit long gone from the tail: the page holding it ----
      await tap(strip + ' [data-act="a:find-prev"]');
      await sleep(80);
      const ra = lastOf("room-at");
      check(W("an earlier hit not held asks for the page holding it"),
        ra && ra.name === name && ra.start === 100 && ra.limit === 60 && ra.seq > 0, ra);
      check(W("the live body stays until the page comes"), !(await has(body + " .away")));
      await at(ra.seq, 100, 60);
      await sleep(150);
      check(W("the page is shown in place of the tail, where it is said"),
        await has(body + " .away") && (await txt(body + " .awayhead")).includes("101–160 of 1,000"));
      check(W("the hit there is flashed, marked, in sight"),
        await has(body + ' .away .ent[data-i="20"].flash') && (await txt(body + ' .away .ent[data-i="20"] mark.fm')) === "搜尋" &&
        await visibleIn(body + ' .away .ent[data-i="20"] mark.fm'));
      check(W("the count follows it, and Back to latest is offered"),
        (await txt(strip + " .fcount")) === "1/3" && await has(body + ' [data-act="a:away-back"]'));

      // ---- While away, the live tail does not move the page ----
      const before = await page.$eval(body, (b) => ({ top: b.scrollTop, first: b.querySelector('.ent[data-i="0"]').textContent }));
      await liveTail(chat ? [{ who: name, kind: "turn", text: "fresh news 1", ts: ts(1000), agent: name }]
        : [{ role: "assistant", text: "fresh news 1", ts: ts(1000) }]);
      await sleep(120);
      const after = await page.$eval(body, (b) => ({ top: b.scrollTop, first: b.querySelector('.ent[data-i="0"]').textContent,
        news: b.textContent.includes("fresh news") }));
      check(W("a live update while away does not yank the view"),
        after.top === before.top && after.first === before.first && !after.news, { before, after });

      // ---- Paging around the hit ----
      const hitTop = () => page.$eval(body, (b) => b.querySelector('.ent[data-i="' + (b.querySelectorAll(".away .ent").length > 60 ? 80 : 20) + '"]').getBoundingClientRect().top);
      const t0 = await hitTop();
      await page.$eval(body + ' [data-act="a:away-earlier"]', (b) => b.click());   // no scroll to it
      await sleep(60);
      const re = lastOf("room-at");
      check(W("Load earlier asks for the page before it"), re.start === 40 && re.seq > ra.seq, re);
      await at(re.seq, 40, 60);
      await sleep(120);
      const t1 = await hitTop();
      check(W("an earlier page is prepended, the reading place held"),
        (await page.$$eval(body + " .away .ent", (es) => es.length)) === 120 && (await txt(body + " .awayhead")).includes("41–160") &&
        Math.abs(t1 - t0) <= 3, { t0, t1 });
      await page.$eval(body, (b) => { b.scrollTop = b.scrollHeight; });
      await tap(body + ' [data-act="a:away-later"]');
      await sleep(60);
      const rl = lastOf("room-at");
      check(W("Load later asks for the page after it"), rl.start === 160, rl);
      await at(rl.seq, 160, 60);
      await sleep(120);
      check(W("a later page is appended"), (await page.$$eval(body + " .away .ent", (es) => es.length)) === 180 &&
        (await txt(body + " .awayhead")).includes("41–220"));

      // ---- Back to latest: the live tail, with what came meanwhile ----
      await tap(body + ' [data-act="a:away-back"]');
      await sleep(150);
      const back = await page.$eval(body, (b) => ({ away: !!b.querySelector(".away"), news: b.textContent.includes("fresh news 1"),
        foot: b.scrollHeight - b.scrollTop - b.clientHeight }));
      check(W("Back to latest shows the live tail, what came meanwhile, at its foot"),
        !back.away && back.news && back.foot < 40, back);
      check(W("the find bar stays open"), await has(strip + " .jump.find"));

      // ---- Esc: the list, then the bar; the You strip is back ----
      await tap(strip + " .fcount");
      await sleep(60);
      check(W("the count opens the list again"), await has(strip + " .flist:not([hidden])"));
      await page.focus(strip + " .fq");
      await page.keyboard.press("Escape");
      await sleep(60);
      check(W("Esc closes the list first"), !(await has(strip + " .flist:not([hidden])")) && await has(strip + " .jump.find"));
      await page.keyboard.press("Escape");
      await sleep(80);
      check(W("Esc again closes the bar: the You strip is back"),
        !(await has(strip + " .jump.find")) && await has(strip + " .jline") && !(await has(body + " mark.fm")));
      check(W("and the room stays open"), await has(".room"));

      // ---- Reopened (by / at the desk): its words, found again ----
      const n2 = count("room-find");
      if (where === "desk") {
        await page.click(body + " " + held + '[data-i="3"]');
        await page.evaluate(() => document.activeElement && document.activeElement.blur && document.activeElement.blur());
        await page.keyboard.press("/");
      } else await tap(strip + " .jfind");
      await sleep(120);
      const rf2 = lastOf("room-find");
      check(W((where === "desk" ? "/ opens" : "the button opens") + " it again on its words, sent again"),
        await has(strip + " .jump.find") && (await page.$eval(strip + " .fq", (i) => i.value)) === "搜尋" &&
        count("room-find") === n2 + 1 && rf2.q === "搜尋", rf2);
      await found(rf2.seq, HITS);
      await sleep(60);

      // ---- ✕ while away is back to latest too ----
      await tap(strip + ' [data-act="a:find-prev"]');   // the newest
      await sleep(60);
      await tap(strip + ' [data-act="a:find-prev"]');
      await sleep(60);
      await tap(strip + ' [data-act="a:find-prev"]');   // 120: not held
      await sleep(60);
      const ra2 = lastOf("room-at");
      await at(ra2.seq, 100, 60);
      await sleep(100);
      check(W("stepping back reaches the old hit's page"), await has(body + " .away") && (await txt(strip + " .fcount")) === "1/3");
      check(W("↑ stops at the earliest"), await page.$eval(strip + ' [data-act="a:find-prev"]', (b) => b.disabled));
      await tap(strip + ' [data-act="a:find-close"]');
      await sleep(120);
      check(W("✕ closes the bar and goes back to the live tail"),
        !(await has(body + " .away")) && !(await has(strip + " .jump.find")) && await has(strip + " .jline"));

      // ---- Nothing found; and every session searched instead ----
      await tap(strip + " .jfind");
      await sleep(60);
      await page.fill(strip + " .fq", "nowhere said");
      await sleep(450);
      const rf3 = lastOf("room-find");
      check(W("plain typing is sent too"), rf3.q === "nowhere said", rf3);
      await page.evaluate((p) => GM.foundConversation(p), { name, q: "nowhere said", seq: rf3.seq, total: 0, capped: false, entries: 1000, hits: [] });
      await sleep(60);
      check(W("no hit: a zero, and the list says so"),
        (await txt(strip + " .fcount")) === "0" && (await txt(strip + " .flist .fsum")).includes("Not said in " + name));
      if (where === "phone") {
        const fit = await page.$eval(strip + " .jump.find", (f) => {
          const r = f.getBoundingClientRect(), i = f.querySelector(".fq").getBoundingClientRect();
          const buttons = [...f.querySelectorAll("button")].map((b) => b.getBoundingClientRect());
          return { overflow: f.scrollWidth - f.clientWidth, input: Math.round(i.width), inside: buttons.every((b) => b.right <= r.right + 1 && b.width >= 28),
            font: getComputedStyle(f.querySelector(".fq")).fontSize };
        });
        check(W("at phone width the bar fits: a wide box, finger-sized buttons, no zoom on focus"),
          fit.overflow <= 0 && fit.input >= 150 && fit.inside && fit.font === "16px", fit);
      }
      await tap(strip + ' [data-act="a:find-all"]');
      await sleep(150);
      const se = lastOf("search");
      check(W("Search all sessions opens the Search tab with the words"),
        (await page.$eval("#q", (i) => i.value).catch(() => "")) === "nowhere said" && se && se.q === "nowhere said" &&
        !!lastOf("room-close") && !(await has(".room")), se);

      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
