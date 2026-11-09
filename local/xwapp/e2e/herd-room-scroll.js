// Reading up a room's conversation while it is still coming: the entry
// you are looking at stays where it is.  On another device a conversation
// comes in parts (ghostherd-memory-page.el "In parts"): its newest first,
// the older ones after, prepended ("head"); a change of its end alone; a
// whole again when a part was missed (room-resend).  WebKit has no scroll
// anchoring: what is put above the screen pushes what you read down
// unless the page holds it.  Scrolled up -- reading -- each of those keeps
// the entry at the top of the screen at the same place; at the end the
// conversation stays at its end.  A finger on it (or its fling after)
// holds what is above until it lets go.  Columns and chat, phone (390 px)
// and desk; the test plays Emacs's part.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const RD = "claude-deploy-platform";
const PATH = "/Users/x/.claude/projects/-p/abc.jsonl";
const herd = () => Object.assign({}, HERD, { agents: HERD.agents.map((a) => Object.assign({}, a, { meter: false, subs: [] })) });

// Entries N..M, each a few lines tall and its own words.
const words = (n) => "entry " + n + " — " + Array.from({ length: 2 + (n % 4) }, (_, k) => "line " + k + " of what was said in turn " + n + ".").join("\n\n");
const conv = (from, to) => Array.from({ length: to - from + 1 }, (_, i) =>
  ({ role: "assistant", text: words(from + i), ts: "2026-11-09T10:" + String(Math.floor((from + i) / 60)).padStart(2, "0") + ":" + String((from + i) % 60).padStart(2, "0") + "Z" }));
const chat = (from, to) => conv(from, to).map((e) => ({ who: RD, agent: RD, kind: "turn", text: e.text, ts: e.ts }));

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      for (const how of ["columns", "chat"]) {
        const intents = [];
        const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
        const W = (n) => where + " " + how + ": " + n;
        const body = how === "chat" ? ".chat .cbody" : '.rcol[data-name="' + RD + '"] .rbody';
        const set = how === "chat" ? "setChat" : "setConversation";
        const make = how === "chat" ? chat : conv;
        const send = (p) => page.evaluate(([fn, p]) => GM[fn](p), [set, Object.assign(how === "chat" ? { project: DP, missing: [], absent: [] }
          : { name: RD, path: PATH, cli: "claude" }, p)]);
        const ofEntry = (n) => page.evaluate(([b, n]) => {
          const box = document.querySelector(b), top = box.getBoundingClientRect().top;
          const e = [...box.querySelectorAll(":scope > [data-i]")].find((x) => x.textContent.includes("entry " + n + " —"));
          return e ? Math.round(e.getBoundingClientRect().top - top) : null;
        }, [body, n]);
        // The entry at the top of the screen: its number and where it is.
        const looking = () => page.evaluate((b) => {
          const box = document.querySelector(b), top = box.getBoundingClientRect().top;
          const e = [...box.querySelectorAll(":scope > [data-i]")].find((x) => x.getBoundingClientRect().bottom > top + 8);
          const m = e && /entry (\d+) —/.exec(e.textContent);
          return m ? { n: +m[1], y: Math.round(e.getBoundingClientRect().top - top) } : null;
        }, body);
        const scrollTo = (n, y) => page.evaluate(([b, n, y]) => {
          const box = document.querySelector(b), top = box.getBoundingClientRect().top;
          const e = [...box.querySelectorAll(":scope > [data-i]")].find((x) => x.textContent.includes("entry " + n + " —"));
          box.scrollTop += e.getBoundingClientRect().top - top - y;
        }, [body, n, y]);
        const atEnd = () => page.$eval(body, (b) => b.scrollHeight - b.scrollTop - b.clientHeight < 4);
        // Read up far enough from its end not to count as at it (40 px).
        const up = (tell) => page.$eval(body, (b, tell) => { const d = b.scrollHeight - b.scrollTop - b.clientHeight; return tell ? d : d > 120; }, tell);
        const held = async (was, n) => { const y = await ofEntry(was.n); return { was, now: y, ok: y !== null && Math.abs(y - was.y) <= 2, n }; };

        await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
        await sleep(80);
        await page.click('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
        await sleep(120);
        await page.click('.room [data-act="a:room-view"][data-view="' + how + '"]');
        await sleep(120);

        // ---- The older parts come while you read up ------------------------
        await send({ v: 1, earlier: true, rest: 3, entries: make(88, 99) });
        await sleep(60);
        check(W("its newest first, at its end"), await atEnd());
        // The first older part comes while you are at the end: out of sight.
        await send({ v: 2, base: 1, earlier: true, rest: 2, head: make(76, 87) });
        await sleep(60);
        check(W("at its end, an older part leaves it at its end"), await atEnd());
        await scrollTo(90, 30);
        await sleep(40);
        check(W("read up from its end (the scene)"), await up(), await up(true));
        let was = await looking();
        await send({ v: 3, base: 2, earlier: true, rest: 1, head: make(64, 75) });
        await sleep(80);
        let r = await held(was);
        check(W("reading up, an older part comes: the entry you look at stays put"), r.ok, r);
        // Its end changes while you read up there.
        was = await looking();
        await send({ v: 4, base: 3, drop: 0, entries: make(100, 101) });
        await sleep(80);
        r = await held(was);
        check(W("reading up, its end grows: the entry you look at stays put"), r.ok, r);
        await send({ v: 5, base: 4, earlier: true, rest: 0, head: make(52, 63) });
        await sleep(80);
        r = await held(was);
        check(W("and the last older part: still put"), r.ok, r);

        // ---- Its end changes while an older part waits unseen ---------------
        // Back to the end; a whole anew (a reopen), an older part at the
        // end (out of sight); read up, then its end grows: what came above
        // and below together must not move you.
        await send({ v: 10, earlier: true, rest: 2, entries: make(88, 99) });
        await sleep(60);
        await page.$eval(body, (b) => { b.scrollTop = b.scrollHeight; });
        await sleep(40);
        await send({ v: 11, base: 10, earlier: true, rest: 1, head: make(76, 87) });
        await sleep(60);
        await scrollTo(89, 20);
        await sleep(40);
        check(W("read up again (the scene)"), await up(), await up(true));
        was = await looking();
        await send({ v: 12, base: 11, drop: 0, entries: make(100, 103) });
        await sleep(80);
        r = await held(was);
        check(W("an older part unseen, then its end grows: the entry you look at stays put"), r.ok, r);
        await send({ v: 13, base: 12, earlier: false, rest: 0, head: make(66, 75) });
        await sleep(80);
        r = await held(was);
        check(W("then the rest: still put"), r.ok, r);

        // ---- Across a resend ---------------------------------------------------
        // All of it here, read in its middle; a part missed, it comes whole
        // again -- its newest first, the rest after -- and you are where you were.
        await send({ v: 20, earlier: false, rest: 0, entries: make(40, 99) });
        await sleep(80);
        await scrollTo(70, 25);
        await sleep(40);
        was = await looking();
        await send({ v: 30, earlier: true, rest: 2, entries: make(88, 99) });
        await sleep(80);
        r = await held(was);
        check(W("a whole again, its newest first: the entry you read is still there, put"), r.ok, r);
        await send({ v: 31, base: 30, earlier: true, rest: 1, head: make(66, 87) });
        await sleep(80);
        r = await held(was);
        check(W("its first older part: put"), r.ok, r);
        await send({ v: 32, base: 31, earlier: false, rest: 0, head: make(40, 65) });
        await sleep(80);
        r = await held(was);
        check(W("and once the rest has come: put"), r.ok, r);
        check(W("nothing lost: every entry once"), (await page.$$eval(body + " > [data-i]", (es) => es.length)) === 60);

        // ---- At its end, it stays at its end --------------------------------------
        await page.$eval(body, (b) => { b.scrollTop = b.scrollHeight; });
        await sleep(40);
        await send({ v: 40, earlier: true, rest: 1, entries: make(88, 99) });
        await sleep(60);
        check(W("at its end: a whole again, at its end"), await atEnd());
        await send({ v: 41, base: 40, earlier: false, rest: 0, head: make(40, 87) });
        await sleep(60);
        await send({ v: 42, base: 41, drop: 0, entries: make(100, 101) });
        await sleep(60);
        check(W("at its end: its rest, a change -- still at its end"), await atEnd());

        // ---- A finger on it holds what is above -------------------------------------
        await send({ v: 50, earlier: true, rest: 2, entries: make(88, 99) });
        await sleep(60);
        await page.$eval(body, (b) => { b.scrollTop = b.scrollHeight; });
        await send({ v: 51, base: 50, earlier: true, rest: 1, head: make(76, 87) });
        await sleep(60);
        await scrollTo(90, 10);
        await sleep(40);
        check(W("read up once more (the scene)"), await up(), await up(true));
        was = await looking();
        // A touch starts on it (iOS then moves it itself: a scrollTop set by
        // the page meanwhile is lost -- what comes above must wait).
        await page.$eval(body, (b) => b.dispatchEvent(new Event("touchstart", { bubbles: true })));
        const n0 = await page.$$eval(body + " > [data-i]", (es) => es.length);
        await send({ v: 52, base: 51, earlier: false, rest: 0, head: make(60, 75) });
        await sleep(80);
        const n1 = await page.$$eval(body + " > [data-i]", (es) => es.length);
        check(W("a finger on it: what came above waits, nothing redrawn under it"), n1 === n0, { n0, n1 });
        await page.$eval(body, (b) => b.dispatchEvent(new Event("touchend", { bubbles: true })));
        await sleep(400);
        const n2 = await page.$$eval(body + " > [data-i]", (es) => es.length);
        r = await held(was);
        check(W("let go: it comes, the entry you looked at put"), n2 === 40 && r.ok, Object.assign(r, { n2 }));

        // ---- As iOS does it: the page's scrollTop lost under a finger ------------
        // What Jing saw on the phone: opened, its newest first, the older parts
        // coming while you are at its end (out of sight); you drag up to read,
        // and one more comes under your finger.  iOS moves the scroller itself
        // then and drops a scrollTop the page sets -- played here by the body
        // dropping the page's writes while a finger is on it.  Redrawn then,
        // every older part shows at once above where you were: the oldest of
        // the conversation, not what you were reading.
        await send({ v: 60, earlier: true, rest: 3, entries: make(88, 99) });
        await sleep(60);
        await page.$eval(body, (b) => { b.scrollTop = b.scrollHeight; });
        await send({ v: 61, base: 60, earlier: true, rest: 2, head: make(76, 87) });
        await send({ v: 62, base: 61, earlier: true, rest: 1, head: make(64, 75) });
        await sleep(60);
        await page.$eval(body, (b) => b.dispatchEvent(new Event("touchstart", { bubbles: true })));
        await scrollTo(90, 10);  // the finger's own scroll
        await page.$eval(body, (b) => {
          const d = Object.getOwnPropertyDescriptor(Element.prototype, "scrollTop");
          window.__finger = true;
          Object.defineProperty(b, "scrollTop", { configurable: true, get() { return d.get.call(this); },
                                                  set(v) { if (!window.__finger) d.set.call(this, v); } });
        });
        await sleep(40);
        was = await looking();
        await send({ v: 63, base: 62, earlier: false, rest: 0, head: make(52, 63) });
        await sleep(80);
        const under = await looking();
        check(W("iOS: a part under your finger does not throw you to the oldest"), under && under.n === was.n && Math.abs(under.y - was.y) <= 2,
          { was, now: under });
        await page.$eval(body, (b) => { window.__finger = false; delete b.scrollTop; b.dispatchEvent(new Event("touchend", { bubbles: true })); });
        await sleep(400);
        r = await held(was);
        check(W("iOS: let go, all of it there, the entry you read put"), r.ok && (await page.$$eval(body + " > [data-i]", (es) => es.length)) === 48, r);

        const wide = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
        check(W("no sideways scroll"), wide <= 0, wide);
        check(W("no page errors"), errors.length === 0, errors);
        await ctx.close();
      }
    }
  } finally {
    await browser.close();
  }
  const bad = results.filter((r) => !r).length;
  console.log(bad ? bad + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(bad ? 1 : 0);
})();
