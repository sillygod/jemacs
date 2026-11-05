// How long a room takes to open on a phone: a real Emacs serving the herd
// page (herd-room.el, a room of four agents with long conversations, the
// sidecar played from their transcripts), through the proxy that does
// what `tailscale serve' does, over a phone's link slowed to its bandwidth
// and round trip.  From the tap on the room to the column in view drawn,
// and to the rest of the room come -- in the Chat view, the phone's own,
// and in Columns.  Then what the rest must not do: move a column you are
// reading (at its end it stays at its end, scrolled up it stays put).
// That an agent's next turn comes alone, not its whole conversation
// again; that Load earlier and find still work; and that a slow room
// open lands in the timings and the Log, as a slow page open does.
//
// LINKS=slow,wifi,desk picks the links (all by default): slow is a
// phone relayed through a DERP server, 15 KB/s and 300 ms round trip, as
// iphone171 was on 2026-11-05; desk is this machine's browser on Emacs's
// loopback port, which takes each conversation whole at once and then
// what changes.  TIMES=1 prints each `next'.  With LOCAL
// another checkout's local/ -- master's -- its page and Emacs: the
// numbers before, and the checks that fail on what it does.
const fs = require("fs");
const path = require("path");
const { start, watch, BOX, PHONE, until, sleep, log } = require("./appslib");

const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

const LINKS = {
  wifi: { kbps: 20000, rtt: 40 },
  slow: { kbps: 120, rtt: 300 },
};
const PROJECT = "/Users/someone/src/fox/";
const COLUMN = 60;
const kb = (b) => (b / 1024).toFixed(1);
const s1 = (ms) => (ms / 1000).toFixed(2) + " s";

// The chat's items, as herd-room.el makes them: how many it shows.
function chatTotal(dir) {
  let n = 0;
  for (const f of fs.readdirSync(path.join(dir, "transcripts"))) {
    const lines = fs.readFileSync(path.join(dir, "transcripts", f), "utf8").trim().split("\n").slice(-150).map(JSON.parse);
    n += lines.filter((e) => e.role === "assistant" || (e.role === "user" && e.kind === "prompt") || (e.role === "herd" && e.kind === "ask")).length;
  }
  return Math.min(200, n);
}

// The `next's answered from AT on -- the one held since before among
// them: when each was asked and done, its bytes.
const nexts = (s, at) => s.traffic.filter((t) => /\/next\?/.test(t.path) && t.end && t.end > at && t.bytes > 2)
  .map((t) => ({ at: t.at - at, end: t.end - at, bytes: t.bytes, enc: t.enc }));

async function tapRoom(page) {
  await page.tap('.pgroup[data-project="' + PROJECT + '"] [data-act="a:room"]');
}

async function back(page) {
  await page.tap("#btn-back");
  await until(() => page.evaluate(() => !!document.querySelector(".pgroup")), 8000);
}

// Each column's entries now, by name, and how far the in-view one is from
// its end.
const columns = (page) => page.evaluate(() => {
  const out = {};
  document.querySelectorAll(".rcol").forEach((c) => {
    const b = c.querySelector(".rbody");
    out[c.getAttribute("data-name")] = { n: b.querySelectorAll(".ent").length,
      fromEnd: Math.round(b.scrollHeight - b.scrollTop - b.clientHeight), loading: !!b.querySelector(".loadmore button[disabled]") };
  });
  return out;
});

// At the desk: the room comes whole, each column at once, then what
// changes alone -- no part waits on another.
async function desk() {
  console.log("\n-- desk: this machine's browser, Emacs's loopback port");
  const s = await start({ el: "herd-room.el", desk: true });
  const W = (n) => "desk: " + n;
  try {
    const ctx = await s.browser.newContext({ viewport: { width: 1600, height: 900 }, deviceScaleFactor: 2 });
    const page = await ctx.newPage();
    const errors = [];
    watch(page, errors);
    await page.goto(s.urls.herd);
    await page.waitForFunction(() => window.GM && window.XW);
    await page.click('.tab[data-tab="agents"]');
    await until(() => page.evaluate((p) => !!document.querySelector('.pgroup[data-project="' + p + '"]'), PROJECT), 30000);
    await sleep(1000);
    const t0 = Date.now();
    await page.click('.pgroup[data-project="' + PROJECT + '"] [data-act="a:room"]');
    await until(() => page.evaluate(() => !!document.querySelector(".rcol .rbody .ent")), 30000);
    const first = await columns(page);
    const drawn = Date.now() - t0;
    await until(async () => Object.values(await columns(page)).every((c) => c.n >= COLUMN), 30000);
    const all = await columns(page);
    console.log(`   columns: drawn ${s1(drawn)}, ${Object.values(first).map((c) => c.n).join("/")} entries at once`);
    const watched = (log(s.dir).filter((w) => w.intent === "room-watch").pop() || {});
    check(W("the room asks for no split, and each column comes whole"), watched.split === false &&
          Object.values(first).some((c) => c.n === COLUMN) && Object.values(all).every((c) => c.n === COLUMN),
          { split: watched.split, first });
    const name = Object.keys(all)[0];
    const flushes = () => log(s.dir).filter((w) => w.flush).length;
    const before = flushes();
    fs.appendFileSync(path.join(s.dir, "transcripts", name + ".jsonl"),
                      JSON.stringify({ role: "assistant", text: "A desk turn, said just now.", ts: new Date().toISOString() }) + "\n");
    const came = await until(() => page.evaluate((n) => /A desk turn/.test(document.querySelector('.rcol[data-name="' + n + '"] .rbody').textContent), name), 15000);
    const sent = log(s.dir).filter((w) => w.flush).slice(before).flatMap((w) => w.flush).filter((c) => c.fn === "setConversation");
    check(W("a new turn comes alone"), came && sent.length >= 1 && sent.every((c) => c.bytes < 2048), { came, sent });
    check(W("no page errors"), errors.length === 0, errors.slice(0, 4));
    await ctx.close();
  } finally {
    await s.stop();
  }
}

(async () => {
  const names = (process.env.LINKS || "slow,wifi,desk").split(",");
  const summary = [];
  for (const name of names) {
    if (name === "desk") { await desk(); continue; }
    const link = LINKS[name];
    console.log(`\n-- ${name}: ${link.kbps / 8} KB/s, ${link.rtt} ms round trip`);
    const s = await start({ el: "herd-room.el", link });
    const W = (n) => name + ": " + n;
    try {
      const ctx = await s.browser.newContext(PHONE);
      const page = await ctx.newPage();
      const errors = [];
      watch(page, errors);
      await page.goto(BOX + "/" + s.secretOf("herd") + "/ghostherd/ui/index.html");
      await page.waitForFunction(() => window.GM && window.XW);
      await page.tap('.tab[data-tab="agents"]');
      await until(() => page.evaluate((p) => !!document.querySelector('.pgroup[data-project="' + p + '"]'), PROJECT), 30000);
      // The page's own load over: what follows is the room's.
      await sleep(link.rtt * 4 + 1500);

      // ---- The Chat view: the phone's own -------------------------------
      const total = chatTotal(s.dir);
      let t0 = Date.now();
      await tapRoom(page);
      await until(() => page.evaluate(() => !!document.querySelector(".chat .cbody .cmsg")), 60000);
      const chatFirst = Date.now() - t0;
      const chatShown = await page.$$eval(".chat .cbody .cmsg", (m) => m.length);
      await until(() => page.evaluate((n) => document.querySelectorAll(".chat .cbody .cmsg").length >= n, total), 90000);
      const chatAll = Date.now() - t0;
      const chatAtEnd = await page.$eval(".chat .cbody", (b) => b.scrollHeight - b.scrollTop - b.clientHeight < 2);
      const chatNexts = nexts(s, t0);
      console.log(`   chat: drawn ${s1(chatFirst)} (${chatShown} of ${total} items) · all ${s1(chatAll)} · ` +
                  `${chatNexts.length} nexts, ${kb(chatNexts.reduce((a, q) => a + q.bytes, 0))} KB`);
      check(W("the chat: its newest drawn, then all of it, still at its end"), chatShown > 0 && chatAtEnd);
      await back(page);

      // ---- Columns ---------------------------------------------------------
      // Turned to Columns once (the page keeps it), then the room opened
      // anew: the tap timed.
      await tapRoom(page);
      await until(() => page.evaluate(() => !!document.querySelector('.room [data-act="a:room-view"][data-view="columns"]')), 30000);
      await page.tap('.room [data-act="a:room-view"][data-view="columns"]');
      await until(async () => Object.values(await columns(page)).every((c) => c.n >= COLUMN), 120000);
      await back(page);
      await sleep(link.rtt * 4 + 1000);

      t0 = Date.now();
      await tapRoom(page);
      await until(() => page.evaluate(() => !!document.querySelector(".rcol .rbody .ent")), 60000);
      const colFirst = Date.now() - t0;
      const inView = await page.evaluate(() => {
        const box = document.querySelector(".rcols").getBoundingClientRect();
        const c = [...document.querySelectorAll(".rcol")].find((x) => { const r = x.getBoundingClientRect(); return r.left >= box.left - 1 && r.left < box.right / 2; });
        return c && c.getAttribute("data-name");
      });
      const first = await columns(page);
      // Which column it drew first -- the one you see?
      const drawnFirst = Object.keys(first).filter((n) => first[n].n > 0);
      // The second column, off to the side, scrolled up a little once it
      // has come: what it shows must stay put as the rest comes.
      const other = Object.keys(first).find((n) => n !== inView);
      await until(() => page.evaluate((n) => document.querySelector('.rcol[data-name="' + n + '"] .rbody .ent') !== null, other), 60000);
      const anchor = await page.evaluate((n) => {
        const b = document.querySelector('.rcol[data-name="' + n + '"] .rbody');
        b.scrollTop = Math.max(0, b.scrollHeight - b.clientHeight - 300);
        const top = b.getBoundingClientRect().top;
        const e = [...b.querySelectorAll(".ent")].find((x) => x.getBoundingClientRect().bottom > top + 20);
        return e && { text: e.textContent.slice(0, 120), y: Math.round(e.getBoundingClientRect().top - top) };
      }, other);
      // Sampled as the rest comes: the column in view, at its end, stays there.
      let worst = 0, samples = 0;
      const sampling = (async () => {
        while (true) {
          const c = await columns(page).catch(() => null);
          if (!c) break;
          samples++;
          worst = Math.max(worst, c[inView] ? c[inView].fromEnd : 0);
          if (Object.values(c).every((x) => x.n >= COLUMN)) break;
          await sleep(60);
        }
      })();
      await until(async () => Object.values(await columns(page)).every((c) => c.n >= COLUMN), 120000);
      const colAll = Date.now() - t0;
      await sampling;
      const after = await columns(page);
      const held = anchor && await page.evaluate(([n, a]) => {
        const b = document.querySelector('.rcol[data-name="' + n + '"] .rbody');
        const top = b.getBoundingClientRect().top;
        const e = [...b.querySelectorAll(".ent")].find((x) => x.textContent.slice(0, 120) === a.text);
        return e ? Math.round(e.getBoundingClientRect().top - top) : null;
      }, [other, anchor]);
      const colNexts = nexts(s, t0);
      const firstNext = colNexts.find((q) => q.end >= colFirst - 50) || colNexts[colNexts.length - 1];
      console.log(`   columns: ${inView} drawn ${s1(colFirst)} (${first[inView] ? first[inView].n : 0} entries) · all four ` +
                  `${s1(colAll)} · ${colNexts.length} nexts, ${kb(colNexts.reduce((a, q) => a + q.bytes, 0))} KB; ` +
                  `the one that drew it ${kb(firstNext ? firstNext.bytes : 0)} KB`);
      if (process.env.TIMES) colNexts.forEach((q) => console.log(`      next ${String(q.at).padStart(6)} → ${String(q.end).padStart(6)} ms ${kb(q.bytes).padStart(6)} KB ${q.enc}`));
      summary.push({ link: name, chatFirst, chatAll, colFirst, colAll });
      check(W("the column in view is the first drawn"), drawnFirst.length >= 1 && drawnFirst.includes(inView), drawnFirst);
      if (name === "slow") check(W("the column in view drawn within 2.5 s of the tap"), colFirst <= 2500, colFirst);
      check(W("the rest comes: every column its newest 60"), Object.values(after).every((c) => c.n === COLUMN), after);
      check(W("the column in view stayed at its end as the rest came"), samples > 0 && worst < 4, { worst, samples });
      check(W("a column scrolled up stayed put as the rest came"), anchor && held !== null && Math.abs(held - anchor.y) <= 3,
            { anchor: anchor && anchor.y, held });

      // ---- What changes, alone ----------------------------------------------
      const col = '.rcol[data-name="' + inView + '"] .rbody';
      const tAdd = Date.now();
      const said = (n) => JSON.stringify({ role: "assistant", text: "A fresh turn " + n + ", said just now.",
                                            ts: new Date(Date.now() + n).toISOString() }) + "\n";
      fs.appendFileSync(path.join(s.dir, "transcripts", inView + ".jsonl"), said(1) + said(2));
      const came = await until(() => page.evaluate((c) => /A fresh turn 2/.test(document.querySelector(c).textContent), col), 15000);
      await sleep(link.rtt * 2 + 300);
      const changeNexts = nexts(s, tAdd).filter((q) => q.bytes > 0);
      const biggest = changeNexts.reduce((m, q) => Math.max(m, q.bytes), 0);
      console.log(`   a change: shown ${s1(Date.now() - tAdd)} · its nexts ${changeNexts.map((q) => kb(q.bytes) + " KB").join(", ")}`);
      check(W("an agent's new turns come alone, not its conversation again"), came && biggest < 3 * 1024, { came, biggest });
      const atEnd = await page.$eval(col, (b) => b.scrollHeight - b.scrollTop - b.clientHeight < 4);
      check(W("at its end, it follows the new turns"), atEnd);

      // ---- Load earlier -----------------------------------------------------
      const had = await page.$$eval(col + " .ent", (e) => e.length);
      await page.tap(col + ' [data-act="a:load-earlier"]');
      const more = await until(() => page.evaluate(([c, n]) => document.querySelectorAll(c + " .ent").length > n, [col, had]), 30000);
      const now = await page.$$eval(col + " .ent", (e) => e.length);
      check(W("Load earlier brings the page before"), more && now >= had + 50, { had, now });

      // ---- Find, in the whole conversation ----------------------------------
      await page.tap('.rcol[data-name="' + inView + '"] .jfind');
      await page.fill('.rcol[data-name="' + inView + '"] .fq', "zeppelin");
      const counted = await until(() => page.evaluate((n) => {
        const c = document.querySelector('.rcol[data-name="' + n + '"] .fcount');
        return c && c.textContent === "1";
      }, inView), 15000);
      await page.press('.rcol[data-name="' + inView + '"] .fq', "Enter");
      const found = await until(() => page.evaluate((c) => /zeppelin answer/.test((document.querySelector(c + " .flash") || {}).textContent || ""), col), 15000);
      check(W("find counts its words in the whole conversation, and goes to them"), counted && found, { counted, found });
      await page.tap('.rcol[data-name="' + inView + '"] [data-act="a:find-close"]').catch(() => {});

      // ---- Its timing ---------------------------------------------------------
      await sleep(1500);
      let rooms = [];
      await until(() => {
        try {
          rooms = fs.readFileSync(path.join(s.dir, "timings.jsonl"), "utf8").trim().split("\n").map(JSON.parse)
            .filter((r) => r.kind === "room");
        } catch (_) { rooms = []; }
        return rooms.length >= 3;
      }, 10000);
      console.log(`   room timings: ${rooms.map((r) => r.d.toFixed(2) + " s" + (r.slow ? " slow" : "")).join(", ") || "none"}`);
      check(W("each room opened is timed, from the tap to its conversation drawn"), rooms.length >= 3, rooms.length);

      // ---- A slow one, in the Log -------------------------------------------
      // The room opened while Emacs is busy 2 s -- its watch held at the
      // proxy until Emacs is made so: slow, and said where it waited.
      if (name === "slow") {
        await back(page);
        await sleep(1500);
        s.holdAt((r) => r.method === "POST" && /\/intent$/.test(r.path),
                 () => s.askEmacs("POST", "/e2e-stall?ms=2000").then(() => sleep(50)));
        await tapRoom(page);
        await until(() => page.evaluate(() => !!document.querySelector(".rcol .rbody .ent")), 60000);
        let slowOne = null;
        await until(() => {
          try {
            slowOne = fs.readFileSync(path.join(s.dir, "timings.jsonl"), "utf8").trim().split("\n").map(JSON.parse)
              .filter((r) => r.kind === "room" && r.slow).pop() || null;
          } catch (_) { slowOne = null; }
          return slowOne;
        }, 15000);
        console.log(`   a room opened with Emacs busy: ${slowOne ? slowOne.d.toFixed(2) + " s, " + JSON.stringify(slowOne.sum) : "no record"}`);
        await page.tap('.tab[data-tab="log"]');
        const logged = await until(() => page.evaluate(() => /slow room \d+\.\d s: Emacs busy/.test(document.body.textContent)), 15000);
        check(W("a slow room open lands in the Log, said where it waited"), slowOne && logged, { record: !!slowOne, logged });

        // Left before it is drawn: given up, no record -- not a minute's
        // "slow room" once the moment's cap is up.
        const count = () => { try { return fs.readFileSync(path.join(s.dir, "timings.jsonl"), "utf8").trim().split("\n")
          .map(JSON.parse).filter((r) => r.kind === "room").length; } catch (_) { return 0; } };
        await page.tap('.tab[data-tab="agents"]');
        await until(() => page.evaluate((p) => !!document.querySelector('.pgroup[data-project="' + p + '"]'), PROJECT), 15000);
        await sleep(1500);
        const before = count();
        await tapRoom(page);
        await page.tap("#btn-back");
        await sleep(3000);
        await tapRoom(page);
        await until(() => page.evaluate(() => !!document.querySelector(".rcol .rbody .ent, .chat .cbody .cmsg")), 30000);
        await sleep(1500);
        check(W("a room left before it is drawn is not timed; the next one is"), count() === before + 1, { before, after: count() });
      }
      check(W("no page errors"), errors.length === 0, errors.slice(0, 4));
      await ctx.close();
    } finally {
      await s.stop();
    }
  }
  console.log("\n   " + summary.map((m) => `${m.link}: chat ${s1(m.chatFirst)} (all ${s1(m.chatAll)}), ` +
                                       `column ${s1(m.colFirst)} (all ${s1(m.colAll)})`).join("\n   "));
  const failed = results.filter((r) => !r).length;
  console.log(failed ? `\n${failed} of ${results.length} failed` : `\nall ${results.length} passed`);
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
