// Copy on each turn of a room, yours and the agents': what the turn
// says as it was written -- the markdown, not the page's drawing of it
// -- in the columns and in the chat, on a phone and at a desk.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const RD = "claude-deploy-platform";
const LEAD = "lead-deploy-platform";
const ts = Date.now() / 1000 - 600;

const YOU = "Fix the retry tooltip\n\nkeep the 3s default";
const REPLY = "## Done\n\n- `retry.js`: **fixed**\n- tests: 12 pass\n\n```js\nretry({ wait: 3000 });\n```";
const ASK = "Can you review the retry change?";
const ANSWER = "Looks good, ship it";
const CONV = {
  name: RD,
  entries: [
    { role: "user", text: YOU, ts, notes: ["you are the rd"] },
    { role: "tools", tools: [{ name: "Bash", hint: "npm test" }], ts },
    { role: "assistant", text: REPLY, ts },
    { role: "herd", kind: "ask", to: LEAD, ask: "3", text: ASK, answer: ANSWER, ts },
    { role: "user", text: "", ts },
  ],
};
const LONG = Array.from({ length: 80 }, (_, k) => "- line " + k + " of a long report").join("\n");
const LONG_REPLY = "## Report\n\n" + LONG;
const CHAT = {
  project: DP,
  entries: [
    { agent: RD, who: "user", to: RD, kind: "prompt", text: YOU, ts },
    { agent: RD, who: RD, kind: "turn", text: LONG, ts },
    { agent: RD, who: RD, to: LEAD, kind: "ask", ask: "3", text: ASK, answer: ANSWER, ts },
  ],
};

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const { ctx, page, errors } = await phonePage(browser, { context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      // This device's clipboard, as the page reaches it over plain http:
      // the selected textarea XW.copy puts up for the click, read as it
      // is copied.
      await page.evaluate(() => {
        window.copied = [];
        const exec = document.execCommand.bind(document);
        document.execCommand = (cmd, ...rest) => {
          if (cmd !== "copy") return exec(cmd, ...rest);
          const ta = [...document.querySelectorAll("body > textarea[readonly]")].pop();
          window.copied.push(ta ? ta.value : null);
          return true;
        };
      });
      const copied = () => page.evaluate(() => window.copied.splice(0));
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      if (await page.$(".room .chat")) {
        await tap('.room [data-act="a:room-view"][data-view="columns"]');
        await sleep(150);
      }

      // ---- An agent's column ----
      await page.evaluate((c) => GM.setConversation(c), CONV);
      await sleep(100);
      const col = '.rcol[data-name="' + RD + '"] .rbody';
      const ents = col + " > .ent";
      const heads = await page.$$eval(ents, (es) => es.map((e) => !!e.querySelector(":scope > .ehead .ecopy")));
      check(W("a Copy on your turn, its reply and its ask; none on its tools or on a turn with no words"),
        JSON.stringify(heads) === JSON.stringify([true, false, true, true, false]), heads);
      await tap(ents + ":nth-child(1) .ecopy");
      await tap(ents + ":nth-child(3) .ecopy");
      await tap(ents + ":nth-child(4) > .ehead .ecopy");
      await tap(ents + ":nth-child(4) .eanswer .ecopy");
      const got = await copied();
      check(W("your words, without the herd's note before them"), got[0] === YOU, got[0]);
      check(W("its reply as it wrote it, markdown and all"), got[1] === REPLY, got[1]);
      check(W("its ask, and the answer to it, each on its own"), got[2] === ASK && got[3] === ANSWER, got.slice(2));
      const flash = await page.$eval("#status", (s) => (s.hidden ? "" : s.textContent));
      check(W("and says so"), flash === "Copied", flash);

      // Where the button sits: after the time, in the head, inside the column.
      const box = await page.$eval(ents + ":nth-child(3) > .ehead", (h) => {
        const r = (el) => el.getBoundingClientRect();
        const c = h.closest(".rcol");
        return { when: r(h.querySelector(".when")).right, copy: r(h.querySelector(".ecopy")), col: r(c).right, head: r(h).right };
      });
      check(W("at the head's end, just after the time, inside the column"),
        box.copy.left >= box.when - 1 && box.copy.left - box.when < 16 &&
        Math.abs(box.copy.right - box.head) <= 1 && box.copy.right <= box.col + 1, box);
      const bare = await page.$eval(ents + ":nth-child(4) .eanswer .ehead", (h) =>
        ({ copy: h.querySelector(".ecopy").getBoundingClientRect().right, head: h.getBoundingClientRect().right }));
      check(W("at the end of a head with no time too"), Math.abs(bare.copy - bare.head) <= 1, bare);
      if (where === "phone") check(W("big enough for a finger"), box.copy.height >= 32, box.copy.height);

      // A conversation that moves on is drawn again, and the buttons copy
      // the turns now under them.
      await page.evaluate((c) => GM.setConversation(c), Object.assign({}, CONV, {
        entries: [{ role: "assistant", text: "Newer reply", ts }].concat(CONV.entries) }));
      await sleep(100);
      await tap(ents + ":nth-child(1) .ecopy");
      await tap(ents + ":nth-child(4) .ecopy");
      const moved = await copied();
      check(W("after a redraw, each Copy is still its own turn's"), moved[0] === "Newer reply" && moved[1] === REPLY, moved);

      // A long reply read to its end: its head, Copy and all, still in
      // sight at the top of the column.  Found where it shows, by point --
      // a tap on the button itself would scroll to it first.
      const seen = (body, item) => page.$eval(body, (b, item) => {
        const btn = b.querySelector(item + " > .ehead .ecopy");
        const r = btn.getBoundingClientRect(), v = b.getBoundingClientRect();
        const x = r.left + r.width / 2, y = r.top + r.height / 2;
        // Solid, and the colour behind it: the text it covers does not show.
        const clear = "rgba(0, 0, 0, 0)";
        let under = b;
        while (under && getComputedStyle(under).backgroundColor === clear) under = under.parentElement;
        const bg = getComputedStyle(btn.closest(".ehead")).backgroundColor;
        return { scrolled: b.scrollTop > 0, inView: r.top >= v.top - 1 && r.bottom <= v.bottom + 1,
                 onTop: document.elementFromPoint(x, y) === btn, solid: bg !== clear && under && bg === getComputedStyle(under).backgroundColor, x, y };
      }, item);
      const tapAt = (p) => (where === "phone" ? page.touchscreen.tap(p.x, p.y) : page.mouse.click(p.x, p.y));
      await page.evaluate((c) => GM.setConversation(c), { name: RD, entries: [{ role: "user", text: YOU, ts }, { role: "assistant", text: LONG_REPLY, ts }] });
      await sleep(100);
      await page.$eval(col, (b) => { b.scrollTop = b.scrollHeight; });
      const end = await seen(col, ".ent.me");
      check(W("a long reply read to its end: its Copy still in sight, on top"), end.scrolled && end.inView && end.onTop && end.solid, end);
      await tapAt(end);
      const fromEnd = await copied();
      check(W("and it copies the whole reply from there"), fromEnd[0] === LONG_REPLY, (fromEnd[0] || "").length);

      // ---- The room's chat ----
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(150);
      await page.evaluate((c) => GM.setChat(c), CHAT);
      await sleep(100);
      const items = ".chat .cbody > .ent";
      const folded = await page.$eval(items + ":nth-child(2)", (e) => e.classList.contains("folded"));
      await tap(items + ":nth-child(1) .ecopy");
      await tap(items + ":nth-child(2) .ecopy");
      await tap(items + ":nth-child(3) > .ehead .ecopy");
      await tap(items + ":nth-child(3) .eanswer .ecopy");
      const chat = await copied();
      check(W("in the chat: a long turn, folded, copies whole"), folded && chat[1] === LONG, { folded, len: (chat[1] || "").length });
      check(W("in the chat: yours, and an ask and its answer"), chat[0] === YOU && chat[2] === ASK && chat[3] === ANSWER, chat);
      // Opened, a long turn read past its middle: the same.
      await tap(items + ":nth-child(2) .cmore");
      await sleep(100);
      await page.$eval(".chat .cbody", (b) => {
        const r = b.querySelector(".ent:nth-child(2)").getBoundingClientRect();
        b.scrollTop += r.bottom - b.getBoundingClientRect().top - b.clientHeight / 2;
      });
      const mid = await seen(".chat .cbody", ".ent:nth-child(2)");
      check(W("in the chat: a long turn opened and read on, its Copy in sight, on top"), mid.scrolled && mid.inView && mid.onTop && mid.solid, mid);
      await tapAt(mid);
      const fromMid = await copied();
      check(W("and it copies the whole turn"), fromMid[0] === LONG, (fromMid[0] || "").length);

      const wide = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
      check(W("no sideways scroll"), wide <= 0, wide);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const bad = results.filter((r) => !r).length;
  console.log(bad ? bad + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(bad ? 1 : 0);
})();
