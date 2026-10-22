// The Threads and Inbox tabs on a short desk: each column scrolls by itself
// under the header and the Talking-now strip, which stay put.  A thread
// picked far down the list shows from its top; Emacs's pushes every few
// seconds keep where each column was scrolled.  The phone keeps one flow.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const { LIST, DETAILS, serve } = require("./threads-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const SHORT = { viewport: { width: 1280, height: 560 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const now = Date.now();

// Forty threads, the last one far down; each opens as t2 does, with its own id.
const many = Object.assign({}, LIST, { threads: Array.from({ length: 40 }, (_, i) =>
  Object.assign({}, LIST.threads[i % 5], { id: "m" + i, title: "Thread " + i + " " + LIST.threads[i % 5].title, at: now - i * 60e3 })) });
const tall = (id) => Object.assign({}, DETAILS.t2, { id, title: "Thread " + id,
  msgs: DETAILS.t2.msgs.concat(DETAILS.t2.msgs, DETAILS.t2.msgs) });

(async () => {
  const browser = await webkit.launch();
  try {
    const intents = [];
    const { ctx, page, errors } = await phonePage(browser, { intents, context: SHORT });
    const stop = () => {};
    // Answer opens of m* with a tall thread of that id.
    const answer = setInterval(async () => {
      for (const it of intents.splice(0)) {
        if (it.op === "thread-open" && it.id && it.id.startsWith("m")) await page.evaluate((d) => GM.renderThread(d), tall(it.id)).catch(() => {});
      }
    }, 40);
    await sleep(150);
    await page.click('.tab[data-tab="threads"]');
    await page.evaluate((l) => GM.renderThreads(l), many);
    await sleep(250);
    const geo = () => page.evaluate(() => {
      const r = (s) => { const el = document.querySelector(s); return el ? el.getBoundingClientRect() : null; };
      const sc = (s) => { const el = document.querySelector(s); return el ? el.scrollTop : null; };
      return { app: document.querySelector("#app").scrollTop, doc: document.scrollingElement.scrollTop,
               tabs: r(".tabs").top, talk: r(".thtalk") && r(".thtalk").top, list: sc(".thlist"), main: sc(".thmain"),
               h1: r(".thh1") && r(".thh1").top, mainTop: r(".thmain") && r(".thmain").top, vh: innerHeight,
               listScrolls: (() => { const l = document.querySelector(".thlist"); return l && l.scrollHeight > l.clientHeight; })() };
    });
    let g = await geo();
    check("desk: the list scrolls by itself, not the page", g.listScrolls && g.app === 0 && g.doc === 0, g);
    // Scroll the thread down, then the list to its bottom, and pick the last thread.
    await page.evaluate(() => { document.querySelector(".thmain").scrollTop = 400; document.querySelector(".thlist").scrollTop = 99999; });
    await sleep(80);
    g = await geo();
    check("desk: scrolling the list moves nothing else", g.app === 0 && g.tabs >= 0 && g.talk > 0 && g.main > 0, g);
    await page.click('.throw[data-id="m39"]');
    await sleep(300);
    g = await geo();
    check("desk: a thread picked far down shows from its top", g.main === 0 && g.h1 >= g.mainTop && g.h1 < g.vh, g);
    check("desk: the header and Talking now stay put", g.app === 0 && g.tabs >= 0 && g.talk > 0, g);
    check("desk: and the list stays where it was scrolled", g.list > 300, g);
    // Emacs pushes again: nothing jumps.
    await page.evaluate(() => { document.querySelector(".thmain").scrollTop = 150; });
    const before = await geo();
    await page.evaluate((l) => GM.renderThreads(Object.assign({}, l, { live: 2 })), many);
    await page.evaluate((d) => GM.renderThread(d), tall("m39"));
    await sleep(150);
    g = await geo();
    check("desk: a push keeps both columns where they were", g.list === before.list && g.main === before.main, { before, g });
    // Emacs pushes another thread in its place -- the one asked for by an
    // agent, whose latest changed: it too shows from its top.
    await page.evaluate(() => { document.querySelector(".thmain").scrollTop = 150; });
    await page.evaluate((d) => GM.renderThread(d), tall("m38"));
    await sleep(150);
    g = await geo();
    check("desk: another thread pushed in its place shows from its top", g.main === 0 && g.list === before.list, g);

    // The Inbox, the same way.
    const items = Array.from({ length: 30 }, (_, i) => ({ id: "report:" + i, group: i < 15 ? "need" : "fyi", kind: i < 15 ? "question" : "report",
      tag: "Report", tone: "green", agent: "lead-x", cli: "claude", project: "~/src/x/", title: "Item " + i, at: now - i * 60e3,
      whyLabel: "It reported", body: Array.from({ length: 60 }, (_, j) => "line " + j).join("\n"), preview: "p", state: "", read: false, override: null, replyVia: "lead" }));
    await page.click('.tab[data-tab="inbox"]');
    await page.evaluate((it) => GM.renderInbox({ items: it, count: 15 }), items);
    await sleep(150);
    const ig = () => page.evaluate(() => {
      const sc = (s) => document.querySelector(s).scrollTop;
      const l = document.querySelector(".iblistcol"), d = document.querySelector(".ibdetail");
      return { app: document.querySelector("#app").scrollTop, list: sc(".iblistcol"), detail: sc(".ibdetail"),
               listScrolls: l.scrollHeight > l.clientHeight, h1: (document.querySelector(".ibh1") || { getBoundingClientRect: () => ({ top: -1 }) }).getBoundingClientRect().top,
               dTop: d.getBoundingClientRect().top, vh: innerHeight };
    });
    let h = await ig();
    check("desk: the Inbox's list scrolls by itself too", h.listScrolls && h.app === 0, h);
    await page.click('.ibrow[data-id="report:1"]');
    await page.evaluate(() => { document.querySelector(".ibdetail").scrollTop = 500; document.querySelector(".iblistcol").scrollTop = 99999; });
    await sleep(80);
    await page.click('.ibrow[data-id="report:29"]');
    await sleep(150);
    h = await ig();
    check("desk: an Inbox item picked far down shows from its top", h.detail === 0 && h.h1 >= h.dTop && h.h1 < h.vh && h.app === 0 && h.list > 200, h);
    const ib = await ig();
    await page.evaluate((it) => GM.renderInbox({ items: it, count: 15 }), items.map((x) => Object.assign({}, x, { preview: "p2" })));
    await sleep(100);
    h = await ig();
    check("desk: an Inbox push keeps its columns where they were", h.list === ib.list && h.detail === ib.detail, { ib, h });
    check("desk: no page errors", errors.length === 0, errors);
    clearInterval(answer);
    stop();
    await ctx.close();

    // The phone: one flow, the page scrolls; a thread opens at its top.
    {
      const intents2 = [];
      const { ctx: c2, page: p2, errors: e2 } = await phonePage(browser, { intents: intents2 });
      const stop2 = serve(p2, intents2);
      const answer2 = setInterval(async () => {
        for (const it of intents2.slice(-1)) {
          if (it.op === "thread-open" && it.id && it.id.startsWith("m") && !it.answered) {
            it.answered = true;
            await p2.evaluate((d) => GM.renderThread(d), tall(it.id)).catch(() => {});
          }
        }
      }, 40);
      await sleep(150);
      await p2.tap('.tab[data-tab="threads"]');
      await p2.evaluate((l) => GM.renderThreads(l), many);
      await sleep(200);
      const flow = await p2.evaluate(() => { const a = document.querySelector("#app"); return { scrolls: a.scrollHeight > a.clientHeight }; });
      check("phone: one column, the page scrolls", flow.scrolls, flow);
      await p2.evaluate(() => { document.querySelector("#app").scrollTop = 99999; });
      await sleep(80);
      await p2.tap('.throw[data-id="m39"]');
      await sleep(250);
      const top = await p2.evaluate(() => document.querySelector("#app").scrollTop);
      check("phone: a thread opens at its top", top === 0, top);
      const app2 = () => p2.evaluate(() => { const a = document.querySelector("#app"); return { top: a.scrollTop, tall: a.scrollHeight > a.clientHeight + 400 }; });
      await p2.evaluate(() => { document.querySelector("#app").scrollTop = 600; });
      await p2.evaluate((d) => GM.renderThread(d), tall("m38"));
      await sleep(150);
      const a2 = await app2();
      check("phone: another thread pushed in its place starts at its top", a2.tall && a2.top === 0, a2);
      check("phone: no page errors", e2.length === 0, e2);
      clearInterval(answer2);
      stop2();
      await c2.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
