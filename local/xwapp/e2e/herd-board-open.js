// How the Board opens on a phone, and what needs you on it, through a real
// Emacs (herd-room.el: a room of four, the sidecar played -- here with a
// lead's 需要 Jing line on the item, E2E_NEEDS, and herd_board as slow as
// E2E_BOARD_MS says, a big herd's) and the proxy that does what `tailscale
// serve' does, over a phone's link slowed to its bandwidth and round trip.
// From the tap on Board to 要你處理 drawn -- from the Inbox's push, which
// the Board now watches too -- and to the rest of the board.  Then: the
// line once, not twice (the Inbox's item and the board's line are one);
// asleep past Emacs's gone and woken, the Board asks for both again by
// itself; Handled on the Board closes the line, and the Inbox has it no
// more.  Synthetic data.
//
// BOARD_MS (1500 by default) is how long herd_board takes; LINKS=slow,wifi
// picks the links; ROUNDS (3) how many opens each, the median said.  With
// LOCAL another checkout's local/ -- master's -- the numbers before (there
// 要你處理 is the 等 Jing section under the board) and the checks that fail.
const { start, watch, phoneEyes, BOX, PHONE, until, sleep, log } = require("./appslib");

const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const LINKS = { wifi: { kbps: 20000, rtt: 40 }, slow: { kbps: 120, rtt: 300 } };
const PROJECT = "/Users/someone/src/fox/";
const BOARD_MS = process.env.BOARD_MS || "1500";
const ROUNDS = +(process.env.ROUNDS || 3);
const s1 = (ms) => (ms == null ? "—" : (ms / 1000).toFixed(2) + " s");
const median = (xs) => { const v = xs.filter((x) => x != null).sort((a, b) => a - b); return v.length ? v[Math.floor(v.length / 2)] : null; };
// What the page shows: of the line, 要你處理's card of it, or -- before --
// the 等 Jing section's line under the board; of the board, its tiles.
const SEEN = () => [!!document.querySelector('.bdneeds .bdng[data-agent="lead-fox"]') ||
  !!document.querySelector('.inboxpage [data-id="need:7"]') ||
  [...document.querySelectorAll(".bd-jing .ji")].some((l) => /amber or teal/.test(l.textContent)),
  !!document.querySelector(".bd-strip .bdtile")];
const BOARD = () => !!document.querySelector(".bd-strip .bdtile");

(async () => {
  const summary = [];
  for (const name of (process.env.LINKS || "slow,wifi").split(",")) {
    const link = LINKS[name];
    const W = (n) => name + ": " + n;
    console.log(`\n-- ${name}: ${link.kbps / 8} KB/s, ${link.rtt} ms round trip; herd_board ${BOARD_MS} ms`);
    const s = await start({ el: "herd-room.el", link, env: { E2E_NEEDS: "1", E2E_BOARD_MS: BOARD_MS, E2E_GONE: "1.5" } });
    try {
      const ctx = await s.browser.newContext(PHONE);
      await phoneEyes(ctx);
      const url = BOX + "/" + s.secretOf("herd") + "/ghostherd/ui/index.html";
      const errors = [];
      // Until both are drawn: when each was, from T0.
      const timed = async (page, t0, needOnly) => {
        let tNeed = null, tBoard = null;
        while (Date.now() - t0 < 30000 && (tNeed == null || (tBoard == null && !needOnly))) {
          const [n, b] = await page.evaluate(SEEN).catch(() => [false, false]);
          if (n && tNeed == null) tNeed = Date.now() - t0;
          if (b && tBoard == null) tBoard = Date.now() - t0;
          await sleep(20);
        }
        return [tNeed, tBoard];
      };

      // ---- Each round a page of its own: nothing of the Inbox's or the
      // board's in it yet.  Opened from Agents (the page loaded there), and
      // loaded on #board (the phone's tab reopened on it).
      const runs = { tab: [[], []], load: [[], []], home: [[], []] };
      for (let i = 0; i < ROUNDS; i++) {
        let p = await ctx.newPage();
        watch(p, errors);
        await p.goto(url + "#agents");
        await p.waitForFunction(() => window.GM && window.XW);
        await until(() => p.evaluate((pr) => !!document.querySelector('.pgroup[data-project="' + pr + '"]'), PROJECT), 30000);
        await sleep(link.rtt * 4 + 1500);
        let t0 = Date.now();
        const at1 = log(s.dir).length;
        await p.tap('.tab[data-tab="board"]');
        const [n1, b1] = await timed(p, t0);
        if (i === 0) {
          // What went to the page meanwhile, by call: the bytes the link carried.
          const by = {};
          log(s.dir).slice(at1).filter((w) => w.flush).forEach((w) => w.flush.forEach((c) => (by[c.fn] = (by[c.fn] || 0) + c.bytes)));
          console.log("   sent from the tap: " + Object.keys(by).map((k) => k + " " + (by[k] / 1024).toFixed(1) + " KB").join(", "));
        }
        runs.tab[0].push(n1); runs.tab[1].push(b1);
        await p.close();
        p = await ctx.newPage();
        watch(p, errors);
        t0 = Date.now();
        await p.goto(url + "#board");
        const [n2, b2] = await timed(p, t0);
        runs.load[0].push(n2); runs.load[1].push(b2);
        await p.close();
        // The page's home, loaded with no place asked for: the Board now --
        // once the Inbox (02e94b8), its line there.
        p = await ctx.newPage();
        watch(p, errors);
        t0 = Date.now();
        await p.goto(url);
        const [n3] = await timed(p, t0, true);
        runs.home[0].push(n3);
        console.log(`   round ${i + 1}: tab tap -> 要你處理 ${s1(n1)}, the board ${s1(b1)};  load on #board -> 要你處理 ${s1(n2)}, the board ${s1(b2)};` +
                    `  load home -> what needs you ${s1(n3)}`);
        await sleep(link.rtt * 4 + 1500);
      }
      for (const k of ["tab", "load"])
        summary.push(`${name} ${k === "tab" ? "tab tap from Agents" : "load on #board   "}: 要你處理 ${s1(median(runs[k][0]))}, the board ${s1(median(runs[k][1]))} (median of ${ROUNDS})`);
      summary.push(`${name} load home         : what needs you ${s1(median(runs.home[0]))} (median of ${ROUNDS})`);
      check(W("要你處理 is drawn before the board has come, from a tap and on a load"),
        median(runs.tab[0]) < median(runs.tab[1]) && median(runs.load[0]) < median(runs.load[1]), runs);

      // ---- The one red count: the tab open, 要你處理's own; closed, as Emacs
      // counts the same -- a page loaded on Agents, its first look.
      let q = await ctx.newPage();
      watch(q, errors);
      await q.goto(url + "#board");
      await until(() => q.evaluate(() => !!document.querySelector(".bd-need .bdng, .bd-need .bdneednone") &&
        !!(document.querySelector("#board-n") || {}).textContent), 20000);
      const openN = await q.textContent("#board-n");
      await q.close();
      q = await ctx.newPage();
      watch(q, errors);
      const peek0 = log(s.dir).length;
      await q.goto(url + "#agents");
      await until(() => q.evaluate(() => !!(document.querySelector("#board-n") || {}).textContent), 20000);
      const closedN = await q.textContent("#board-n");
      check(W("the Board's count with its tab closed is Emacs's look, and the same as with it open"),
        openN === "1" && closedN === openN && log(s.dir).slice(peek0).some((w) => w.rpc === "herd_board_counts"), { openN, closedN });
      await q.close();
      await sleep(link.rtt * 4 + 1500);

      const page = await ctx.newPage();
      watch(page, errors);
      await page.goto(url + "#agents");
      await page.waitForFunction(() => window.GM && window.XW);
      await until(() => page.evaluate((pr) => !!document.querySelector('.pgroup[data-project="' + pr + '"]'), PROJECT), 30000);
      await sleep(link.rtt * 4 + 1500);

      // ---- On the Board: the Inbox's push, the line once --------------------
      const at = log(s.dir).length;
      await page.tap('.tab[data-tab="board"]');
      const fns = () => log(s.dir).slice(at).filter((w) => w.flush).flatMap((w) => w.flush.map((c) => c.fn));
      await until(() => fns().includes("renderInbox") && fns().includes("boardPatch"), 15000);
      const asked = log(s.dir).slice(at).filter((w) => w.intent).map((w) => w.intent);
      check(W("opened, the Board asks once (board-watch, the Inbox with it), and Emacs sends the Inbox's push, then the board"),
        asked.includes("board-watch") && !asked.includes("inbox-watch") &&
        fns().indexOf("renderInbox") >= 0 && fns().indexOf("renderInbox") < fns().indexOf("boardPatch"), { asked, sent: [...new Set(fns())] });
      // Its group, folded (nothing in it stops an agent), opened.
      if (!(await page.$('.bdng.open[data-agent="lead-fox"]'))) await page.tap('.bdngt[data-agent="lead-fox"]');
      await sleep(100);
      const ids = await page.$$eval(".bdneeds .bdnrow", (cs) => cs.map((c) => c.getAttribute("data-id")));
      check(W("the lead's line once: the Inbox's item and the board's line are one"), ids.join() === "need:7", ids);


      // ---- Asleep past the gone, and woken: both asked for again ------------
      const nap = log(s.dir).length;
      s.sleep();
      await page.evaluate(() => __vis("hidden"));
      await until(() => log(s.dir).slice(nap).some((w) => w.watches === "gone"), 15000);
      const gone = log(s.dir).slice(nap).find((w) => w.watches === "gone");
      check(W("asleep past the gone: Emacs stops the Inbox's and the board's looks"), gone && !gone.inbox && !gone.board, gone);
      s.wake();
      await page.evaluate(() => __vis("visible"));
      await until(() => ["inbox-watch", "board-watch"].every((op) => log(s.dir).slice(nap).some((w) => w.intent === op)), 20000);
      const woke = log(s.dir).slice(nap).filter((w) => w.intent).map((w) => w.intent);
      check(W("woken on the Board: it asks for the Inbox's push and the board again, by itself"),
        woke.includes("inbox-watch") && woke.includes("board-watch"), woke);
      const still = await until(() => page.evaluate(() => !!document.querySelector('.bdneeds .bdnrow[data-id="need:7"]')), 15000);
      check(W("and what needs you is still on top"), still);

      // ---- Handled on the Board: gone there and from the Inbox ---------------
      await page.tap('.bdneeds [data-act="ib:need-done"][data-id="need:7"]');
      const closed = await until(() => log(s.dir).some((w) => w.rpc === "herd_board_need_close"), 15000);
      await sleep(link.rtt * 4 + 3000);
      const left = await page.$$eval(".bdneeds .bdnrow", (cs) => cs.length);
      const quiet = await page.evaluate(() => { const p = document.querySelector(".bd-need .bdneednone"); return p ? Math.round(p.getBoundingClientRect().height) : 0; });
      check(W("Handled on the Board: Emacs closes the line, and 要你處理 is a quiet line"), closed && left === 0 && quiet > 0 && quiet < 40,
        { closed, left, quiet });
      // Loaded again, the Board has it no more: Emacs's own look.
      await page.reload();
      await until(() => page.evaluate(() => !!document.querySelector(".bd-need .bdneednone, .bd-need .bdng")), 30000);
      await sleep(link.rtt * 4 + 2000);
      const still7 = await page.evaluate(() => !!document.querySelector('.bdng[data-agent="lead-fox"]') ||
        [...document.querySelectorAll("[data-id]")].some((e) => e.getAttribute("data-id") === "need:7"));
      check(W("and loaded again, Emacs lists it no more"), !still7);
      check(W("no page errors"), errors.filter((e) => !/Failed to load resource|network connection was lost|could not be found|status of 50[02]|access control checks/.test(e)).length === 0,
        errors.slice(0, 4));
      await ctx.close();
    } finally {
      await s.stop();
    }
  }
  console.log("\n" + summary.join("\n"));
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
