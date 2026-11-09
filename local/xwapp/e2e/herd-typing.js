// Typing on a phone: a room's box focused, the bars fold away.  With the
// keyboard up a phone shows a third of the page; the tabs, the project's
// line, the jump strip, the meter and who the chat goes to used to take
// all but two lines of it.  Focused, they fold away -- who it goes to a
// tag at the box's left, "lead ▾", the select over it -- and come back
// when the box is left, or the keyboard put away.  What the conversation
// shows stays where it was on the screen, at its end or read up, going
// in and coming out.  Image, 💡, the tag, a scroll do not end it.  A
// desktop never types.  The keyboard is played: the browser's viewport
// as iOS has it: the visual viewport as short as an iPhone's Safari leaves
// with its keyboard up, the layout viewport as it was.  (Resizing
// Playwright's viewport with a box focused has its WebKit here abort --
// its NSTextInputContext short of a selector -- with or without typing.)
// Chat and columns; the test plays Emacs's part.  Synthetic data.
const { webkit } = require("playwright");
const path = require("path");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const SHOTS = process.env.SHOTS || "";
const shot = (page, name) => SHOTS ? page.screenshot({ path: path.join(SHOTS, name + ".png") }) : null;
const DP = "~/src/deploy-platform/";
const LEAD = "lead-deploy-platform", RD = "claude-deploy-platform";
const PATH = "/Users/x/.claude/projects/-p/abc.jsonl";
// iPhone Safari: the page under its bars, then what its keyboard leaves
// (the Zhuyin keyboard and its candidates, as Jing's screenshot has it).
const OPEN = { width: 390, height: 664 }, KEYS = 333;
// The visual viewport, played: its height the test's (__keys), a resize
// said as the keyboard moves.
const keyboard = () => {
  const vv = new EventTarget();
  let h = null;
  Object.defineProperties(vv, { height: { get: () => (h == null ? innerHeight : h) }, width: { get: () => innerWidth },
                                offsetTop: { get: () => 0 }, offsetLeft: { get: () => 0 }, scale: { get: () => 1 } });
  Object.defineProperty(window, "visualViewport", { get: () => vv });
  window.__keys = (to) => { h = to; vv.dispatchEvent(new Event("resize")); };
};
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const herd = () => Object.assign({}, HERD, { agents: HERD.agents.map((a) => a.name === LEAD
  ? Object.assign({}, a, { meter: { model: "Opus 5.5", effort: "xhigh", compacted: true }, suggestion: "what else could be smoother" })
  : Object.assign({}, a, { subs: [] })) });

const words = (n) => "entry " + n + " — " + Array.from({ length: 2 + (n % 3) }, (_, k) => "line " + k + " of what was said in turn " + n + ".").join("\n\n");
const ts = (n) => "2026-11-09T10:" + String(Math.floor(n / 60)).padStart(2, "0") + ":" + String(n % 60).padStart(2, "0") + "Z";
const chat = (from, to) => Array.from({ length: to - from + 1 }, (_, i) => {
  const n = from + i;
  return n % 5 === 0 ? { who: "user", agent: LEAD, to: LEAD, kind: "turn", text: "entry " + n + " — you asked the lead about turn " + n, ts: ts(n) }
    : { who: LEAD, agent: LEAD, kind: "turn", text: words(n), ts: ts(n) };
});
const conv = (from, to) => Array.from({ length: to - from + 1 }, (_, i) => {
  const n = from + i;
  return n % 5 === 0 ? { role: "user", text: "entry " + n + " — you asked about turn " + n, ts: ts(n) }
    : { role: "assistant", text: words(n), ts: ts(n) };
});

(async () => {
  const browser = await webkit.launch();
  try {
    for (const how of ["chat", "columns"]) {
      const W = (n) => "phone " + how + ": " + n;
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: { viewport: OPEN }, init: keyboard });
      const name = how === "chat" ? LEAD : RD;
      const body = how === "chat" ? ".chat .cbody" : '.rcol[data-name="' + name + '"] .rbody';
      const box = how === "chat" ? ".chat .cfoot textarea" : '.rcol[data-name="' + name + '"] .rfoot textarea';
      const foot = how === "chat" ? ".chat .cfoot" : '.rcol[data-name="' + name + '"] .rfoot';
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(80);
      await page.click('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(120);
      await page.click('.room [data-act="a:room-view"][data-view="' + how + '"]');
      await sleep(120);
      if (how === "chat") await page.evaluate((p) => GM.setChat(p), { project: DP, missing: [], absent: [], v: 1, entries: chat(1, 60) });
      else await page.evaluate((p) => GM.setConversation(p), { name: RD, path: PATH, cli: "claude", v: 1, entries: conv(1, 60) });
      await sleep(150);
      if (how === "columns") {
        // Its own column on the screen, as a swipe would.
        await page.$eval('.rcol[data-name="' + name + '"]', (c) => c.scrollIntoView({ inline: "start" }));
        await sleep(100);
      }

      const typing = () => page.evaluate(() => document.body.classList.contains("typing"));
      const tall = () => page.$eval(body, (b) => Math.round(b.clientHeight));
      const atEnd = () => page.$eval(body, (b) => b.scrollHeight - b.scrollTop - b.clientHeight < 4);
      const shown = (sel) => page.evaluate((sel) => { const e = document.querySelector(sel); return !!e && e.getClientRects().length > 0; }, sel);
      const bars = how === "chat" ? ["header.top", ".room .rhead", ".chat .cjump", ".chat .cmeter"]
        : ["header.top", ".room .rhead", '.rcol[data-name="' + name + '"] .rtop', '.rcol[data-name="' + name + '"] .rjump'];
      const barsShown = async () => { const o = {}; for (const b of bars) o[b] = await shown(b); return o; };
      // Where entry N shows on the screen.
      const yOf = (n) => page.evaluate(([b, n]) => {
        const e = [...document.querySelector(b).querySelectorAll(":scope > [data-i]")].find((x) => x.textContent.includes("entry " + n + " —"));
        return e ? Math.round(e.getBoundingClientRect().top) : null;
      }, [body, n]);
      const scrollTo = (n, y) => page.evaluate(([b, n, y]) => {
        const box = document.querySelector(b), top = box.getBoundingClientRect().top;
        const e = [...box.querySelectorAll(":scope > [data-i]")].find((x) => x.textContent.includes("entry " + n + " —"));
        box.scrollTop += e.getBoundingClientRect().top - top - y;
      }, [body, n, y]);
      const keys = (up) => page.evaluate((h) => __keys(h), up ? KEYS : null);
      // A finger's press, as iOS gives it the page: mouse events after the
      // touch (Playwright's tap on a box has its WebKit here abort, its
      // NSTextInputContext short of a selector).
      const press = (sel) => page.click(sel);
      const leave = () => page.evaluate(() => document.activeElement && document.activeElement.blur());
      // Past what typing holds in place after it comes or goes (TYPE_KEEP,
      // ended at once by a finger, not by a test's scroll).
      const settle = () => sleep(1000);
      // Entry N's bottom on the screen, below the conversation's top.
      const seen = (n) => page.evaluate(([b, n]) => {
        const box = document.querySelector(b);
        const e = [...box.querySelectorAll(":scope > [data-i]")].find((x) => x.textContent.includes("entry " + n + " —"));
        return !!e && e.getBoundingClientRect().bottom > box.getBoundingClientRect().top + 4;
      }, [body, n]);

      // ---- Before: the keyboard up, the bars where they were ----------------
      check(W("the bars shown (the scene)"), Object.values(await barsShown()).every(Boolean), await barsShown());
      const open = await tall();
      await keys(true);
      await sleep(120);
      const before = await tall();
      check(W("the keyboard up without the box focused: no typing (the old layout)"), !(await typing()));
      await shot(page, how + "-1-before");
      await keys(false);
      await sleep(120);

      // ---- Typing, at its end -----------------------------------------------
      check(W("at its end (the scene)"), await atEnd());
      await press(box);
      await sleep(60);
      check(W("the box focused: typing"), await typing());
      await keys(true);
      await sleep(150);
      const after = await tall();
      console.log("     " + how + ": the conversation " + open + " px tall without the keyboard; with it " + before + " px before, " + after + " px typing");
      check(W("the bars folded away"), Object.values(await barsShown()).every((v) => !v), await barsShown());
      check(W("the conversation gets 150 px or more of the keyboard's screen back"), after - before >= 150, { before, after });
      check(W("at its end it stays at its end"), await atEnd());
      await shot(page, how + "-2-typing");
      // The tag: whose, at the box's left, on its line; the box wide enough.
      const row = await page.evaluate(([foot, chat]) => {
        const f = document.querySelector(foot), r = (e) => e && e.getBoundingClientRect();
        const t = f.querySelector(".ttag"), ta = r(f.querySelector("textarea")), sel = chat && r(f.querySelector("select.chat-to"));
        const tag = t ? r(t) : { left: 0, right: 0, width: 0, bottom: 0, top: 0 };
        const send = r(f.querySelector('[data-act="a:send"]'));
        return { tag: t ? t.textContent : "", tagL: Math.round(tag.left), tagR: Math.round(tag.right), tagW: Math.round(tag.width),
                 tagB: Math.round(tag.bottom), taL: Math.round(ta.left), taW: Math.round(ta.width), taB: Math.round(ta.bottom),
                 sendR: Math.round(send.right), over: sel ? Math.abs(sel.left - tag.left) + Math.abs(sel.top - tag.top) + Math.abs(sel.width - tag.width) : 0,
                 rowW: Math.round(r(f).width) };
      }, [foot, how === "chat"]);
      console.log("     " + how + ": typing, the box " + row.taW + " px wide of its row's " + row.rowW + (how === "chat" ? ", the tag " + row.tagW : ""));
      if (how === "chat") {
        check(W("the tag says whose, at the box's left, on its line"),
          row.tag === "lead" && row.tagR <= row.taL && Math.abs(row.tagB - row.taB) <= 2 && row.tagL < 20, row);
        check(W("the select lies over the tag: a tap on it opens the list"), row.over <= 2, row);
      } else check(W("a column's box: no tag, its placeholder says whose"), !row.tagW &&
        (await page.$eval(box, (t) => t.placeholder)).includes(name), row);
      check(W("the box is wide enough: 170 px or more, empty"), row.taW >= 170 && row.sendR <= 390, row);

      // ---- Not ended by what passes through -----------------------------------
      // A scroll of the conversation, a finger's.
      await page.evaluate((b) => {
        const box = document.querySelector(b);
        box.dispatchEvent(new Event("touchstart", { bubbles: true }));
        box.scrollTop -= 200;
        box.dispatchEvent(new Event("touchend", { bubbles: true }));
      }, body);
      await sleep(400);
      check(W("a scroll of the conversation: still typing, the box focused"), await typing() &&
        await page.evaluate((s) => document.activeElement === document.querySelector(s), box));
      // 💡: what it suggests in the box, the box keeping the keyboard.
      if (how === "chat") {
        await press(foot + " .use");
        await sleep(400);
        check(W("💡: its words in the box, still typing, the box focused"), await typing() &&
          await page.evaluate((s) => document.activeElement === document.querySelector(s) && /smoother/.test(document.querySelector(s).value), box));
        // Words in the box: 💡 would put its words in their place; its room is the box's.
        const writing = await page.evaluate(([f, b]) => ({ use: !!document.querySelector(f + " .use").getClientRects().length,
          w: Math.round(document.querySelector(b).getBoundingClientRect().width), row: Math.round(document.querySelector(f).getBoundingClientRect().width) }), [foot, box]);
        console.log("     " + how + ": typing with words in it, the box " + writing.w + " px wide");
        check(W("words in the box: 💡 gives way, the box 200 px or more"), !writing.use && writing.w >= 200, writing);
        await page.fill(box, "");
        check(W("the box emptied: 💡 back"), await page.$eval(foot + " .use", (u) => u.getClientRects().length > 0));
      }
      // Image: its picker up, then put away.
      const chooser = page.waitForEvent("filechooser", { timeout: 3000 }).catch(() => null);
      await press(foot + ' [data-act="a:image"]');
      const picked = await chooser;
      await page.evaluate(() => document.activeElement && document.activeElement.blur());  // the picker takes the focus
      await sleep(500);
      check(W("Image: its picker up, the box's focus gone with it: still typing"), !!picked && await typing(), { picked: !!picked });
      await page.evaluate(() => document.getElementById("image-pick").dispatchEvent(new Event("cancel")));
      await sleep(400);
      check(W("the picker put away, nothing focused: typing ends"), !(await typing()));
      await press(box);
      await sleep(200);
      check(W("a tap on the box: typing again"), await typing());

      if (how === "chat") {
        // The tag: another to write to, then on with the box.
        await page.selectOption(".chat .cfoot select.chat-to", RD);
        await sleep(400);
        const now = await page.evaluate(() => ({ tag: document.querySelector(".chat .cfoot .ttag").textContent,
          to: document.querySelector(".chat .cfoot").getAttribute("data-name"),
          focus: document.activeElement === document.querySelector(".chat .cfoot textarea") }));
        check(W("picked from the tag: it says the new one, the box focused, still typing"),
          now.tag === RD && now.to === RD && now.focus && await typing(), now);
        await page.selectOption(".chat .cfoot select.chat-to", LEAD);
        await sleep(200);
      }

      // ---- Out of it, at its end ---------------------------------------------
      await page.$eval(body, (b) => { b.scrollTop = b.scrollHeight; });
      await sleep(100);
      await leave();
      await sleep(250);
      check(W("the box left: typing ends, the bars back"), !(await typing()) && Object.values(await barsShown()).every(Boolean), await barsShown());
      check(W("left at its end: at its end"), await atEnd());
      await keys(false);
      await sleep(150);
      check(W("the keyboard down: the conversation as tall as before, at its end"), (await tall()) === open && await atEnd(), { open, now: await tall() });

      // ---- Read up: what is read stays where it was on the screen --------------
      await settle();
      await scrollTo(30, 60);
      await sleep(100);
      const y0 = await yOf(30);
      await press(box);
      await sleep(80);
      const y1 = await yOf(30);
      await keys(true);
      await sleep(200);
      const y2 = await yOf(30);
      check(W("read up, going in: the entry read stays where it was on the screen"), Math.abs(y1 - y0) <= 2 && Math.abs(y2 - y0) <= 2, { y0, y1, y2 });
      check(W("and what the bars covered shows above it"), await seen(29));
      await shot(page, how + "-3-reading");
      // Coming out: an entry read low on the screen, where the bars don't come.
      await settle();
      await scrollTo(40, 150);
      await sleep(100);
      const z0 = await yOf(40);
      await leave();
      await sleep(250);
      const z1 = await yOf(40);
      await keys(false);
      await sleep(200);
      const z2 = await yOf(40);
      check(W("read up, coming out: the entry read stays where it was on the screen"),
        Math.abs(z1 - z0) <= 2 && Math.abs(z2 - z0) <= 2 && !(await typing()), { z0, z1, z2 });

      // ---- The keyboard put away, the box kept (Android's back) ----------------
      await press(box);
      await sleep(80);
      await keys(true);
      await sleep(200);
      check(W("typing (the scene)"), await typing());
      await keys(false);
      await sleep(500);
      check(W("the keyboard put away, the box still focused: typing ends"), !(await typing()) &&
        await page.evaluate((s) => document.activeElement === document.querySelector(s), box));
      await press(box);
      await sleep(80);
      check(W("a tap on the box again: typing"), await typing());

      // ---- The room drawn anew under the box ----------------------------------
      await page.evaluate((h) => { h.agents = h.agents.concat([Object.assign({}, h.agents[1], { name: "claude-deploy-platform-2" })]); GM.renderHerd(h); }, herd());
      await sleep(800);
      check(W("the room drawn anew, the box gone with its focus: typing ends"), !(await typing()) && Object.values(await barsShown()).some(Boolean));

      check(W("no page errors"), errors.length === 0, errors.slice(0, 3));
      await ctx.close();
    }

    // ---- A desktop never types ---------------------------------------------------
    for (const how of ["chat", "columns"]) {
      const W = (n) => "desk " + how + ": " + n;
      const { ctx, page, errors } = await phonePage(browser, { context: DESK });
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd());
      await sleep(80);
      await page.click('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(120);
      await page.click('.room [data-act="a:room-view"][data-view="' + how + '"]');
      await sleep(120);
      const box = how === "chat" ? ".chat .cfoot textarea" : '.rcol[data-name="' + RD + '"] .rfoot textarea';
      const look = () => page.evaluate((how) => {
        const r = (s) => { const e = document.querySelector(s); const b = e && e.getBoundingClientRect(); return b ? [b.left, b.top, b.width, b.height].map(Math.round).join(",") : "none"; };
        return { top: r("header.top"), head: r(".room .rhead"), sel: how === "chat" ? r(".cfoot select.chat-to") : "-", box: r(how === "chat" ? ".cfoot textarea" : ".rfoot textarea"),
                 tag: [...document.querySelectorAll(".ttag")].some((t) => t.getClientRects().length > 0) };
      }, how);
      const was = await look();
      await page.click(box);
      await sleep(200);
      const now = await look();
      check(W("the box focused: no typing, nothing moved, no tag"), !(await page.evaluate(() => document.body.classList.contains("typing"))) &&
        JSON.stringify(was) === JSON.stringify(now) && !now.tag, { was, now });
      check(W("no page errors"), errors.length === 0, errors.slice(0, 3));
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
