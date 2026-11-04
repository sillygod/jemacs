// A room's columns side by side and narrow (3, 4, 5 at 1890x920): each
// head is four short lines, the same height in every column, so the
// conversations start level -- state, name, kind, time and Esc / Screen
// as icons; model, context, tokens; its asks, else what it does, else its
// notes; its chips one line, the rest behind +N, which opens them.  What
// needs the user is never folded.  The box to write in is one line until
// it has the keyboard or words, and sends as before.  A table scrolls
// sideways in its own box, no cell squeezed to a letter a line -- in the
// columns, the chat and on a phone.  Two wide columns, and the phone's
// head and footer, are as they were.
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const { P, AGENTS, CONVS, herd, openRoom } = require("./room-fixture");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1890, height: 920 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };
const col = (name) => '.rcol[data-name="' + name + '"]';
const LEAD = "lead-ops";

// Each column's head, strip, conversation: heights and where it starts.
const columns = (page) => page.$$eval(".rcol", (cs) => cs.map((c) => {
  const r = (s) => c.querySelector(s).getBoundingClientRect();
  return { name: c.getAttribute("data-name"), width: Math.round(r(".rtop").width), top: Math.round(r(".rtop").height),
    bodyTop: Math.round(r(".rbody").top), body: Math.round(r(".rbody").height), foot: Math.round(r(".rfoot").height) };
}));
// Every table cell in SEL: its width against its font's em, and the
// table's box against its message.
const cells = (page, sel) => page.$$eval(sel + " .msg-body table", (ts) => ts.map((t) => {
  const em = parseFloat(getComputedStyle(t.querySelector("td")).fontSize);
  const ws = [...t.querySelectorAll("th, td")].map((c) => c.getBoundingClientRect().width / em);
  return { least: Math.min(...ws), box: Math.round(t.getBoundingClientRect().width), msg: Math.round(t.parentElement.getBoundingClientRect().width),
    scrolls: t.scrollWidth > t.clientWidth + 1, scrollW: t.scrollWidth, clientW: t.clientWidth };
}));
const noSideways = (page, sel) => page.evaluate((s) => ({
  page: document.documentElement.scrollWidth <= window.innerWidth && document.body.scrollWidth <= window.innerWidth,
  boxes: [...document.querySelectorAll(s)].every((b) => b.scrollWidth <= b.clientWidth + 1),
}), sel);
const shown = (page, sel) => page.$eval(sel, (e) => { const s = getComputedStyle(e); return s.display !== "none" && s.visibility !== "hidden" && e.getClientRects().length > 0; }).catch(() => false);

// A table wider than any column, the chat's too: eighteen columns, CJK first.
const WIDE = "| 環境 | " + Array.from({ length: 17 }, (_, i) => "欄位" + (i + 1)).join(" | ") + " |\n|" + "---|".repeat(18) + "\n" +
  "| production 狀態 | " + Array.from({ length: 17 }, (_, i) => "值 " + (i + 1) + " 的說明文字").join(" | ") + " |";

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- 3, 4 and 5 columns: heads level and short; tables whole ----
    for (const n of [3, 4, 5]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: DESK });
      const W = (s) => n + " columns: " + s;
      await openRoom(page, n, "columns", (s) => page.click(s), sleep);
      const cs = await columns(page);
      const tops = cs.map((c) => c.top), starts = cs.map((c) => c.bodyTop);
      check(W("every head is the same height"), new Set(tops).size === 1, cs);
      check(W("a head is at most 110 px"), Math.max(...tops) <= 110, tops);
      check(W("the conversations start level"), new Set(starts).size === 1, starts);
      check(W("the box to write in is one line, its footer short"), cs.every((c) => c.foot <= 48), cs.map((c) => c.foot));
      const t = await cells(page, col(LEAD));
      check(W("the table's cells are each at least 3em wide"), t.length && t.every((x) => x.least >= 3), t);
      check(W("the table is no wider than its message"), t.every((x) => x.box <= x.msg), t);
      const ns = await noSideways(page, ".rbody, .rcols");
      check(W("neither the page, a column nor the row of columns scrolls sideways"), ns.page && ns.boxes, ns);
      // Line 1: Esc and Screen are icons on it, at its end.
      const line1 = await page.$eval(col(LEAD) + " .rtop", (top) => {
        const a = top.querySelector(".amain").getBoundingClientRect();
        return [...top.querySelectorAll(".aacts .sec")].map((b) => { const r = b.getBoundingClientRect();
          return { act: b.getAttribute("data-act"), inLine: r.top >= a.top - 1 && r.bottom <= a.bottom + 1, right: Math.round(a.right + 54 - r.right),
            w: Math.round(r.width), text: getComputedStyle(b).fontSize }; });
      });
      check(W("Esc and Screen are icons on the head's first line"), line1.length === 2 && line1.every((b) => b.inLine && b.w <= 26 && b.text === "0px"), line1);
      check(W("the head's first line is one line"), (await page.$eval(col(LEAD) + " .amain", (e) => e.getBoundingClientRect().height)) <= 23);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }

    // ---- 4 columns, at work: the lines, +N, Esc / Screen, ▾, the box ----
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: DESK });
      const W = (s) => "4 columns: " + s;
      await openRoom(page, 4, "columns", (s) => page.click(s), sleep);
      const lead = col(LEAD);
      const txt = (s) => page.$eval(s, (e) => e.textContent.trim()).catch(() => "");
      const title = (s) => page.$eval(s, (e) => e.title).catch(() => "");

      // Line 3: notes for an idle agent, in full in its title with its state.
      check(W("an idle agent's third line is its notes"), (await txt(lead + " .rsay")) === "lead: plans the work, hands it out");
      const t3 = await title(lead + " .rsay");
      check(W("its title has its notes and what the herd read"), t3.includes("Notes: lead: plans") && t3.includes("idle: seen"), t3);
      check(W("notes are not also on line 1"), !(await shown(page, lead + " .anotes")));
      check(W("who started an agent is in its third line's title"),
        (await title(col("investigator") + " .rsay")).includes("Started by lead-ops"));

      // Line 4: the chips, one line, +N for the rest.
      const chips = () => page.$eval(lead + " .wline", (l) => {
        const vis = [...l.querySelectorAll(".wchip")].filter((c) => c.getClientRects().length);
        const fit = l.querySelector(".wfit");
        return { h: Math.round(l.getBoundingClientRect().height), shown: vis.length, all: l.querySelectorAll(".wchip").length,
          fit: fit && !fit.hidden ? fit.textContent : "", inside: vis.every((c) => c.getBoundingClientRect().right <= l.getBoundingClientRect().right + 0.5) };
      }).catch(() => ({}));
      const click = (s) => page.click(s, { timeout: 2000 }).catch(() => {});
      const c0 = await chips();
      // The lead's 24 named tasks: the wide head shows 4 and +20.
      check(W("the chips are one line, none cut at its edge"), c0.h <= 24 && c0.inside && c0.shown >= 1, c0);
      check(W("+N counts every chip not shown"), c0.fit === "+" + (24 - c0.shown), c0);
      await click(lead + " .wfit");
      await sleep(150);
      const c1 = await chips();
      check(W("+N opens them all, the line wrapping"), c1.shown === 24 && c1.h > 24 && c1.fit === "less", c1);
      const top1 = await page.$eval(lead + " .rtop", (e) => e.getBoundingClientRect().height);
      check(W("opened, the head grows to hold them"), top1 > 110, top1);
      // A herd tick redraws the head: still open.
      await page.evaluate((h) => GM.renderHerd(h), herd(4));
      await sleep(120);
      check(W("they stay open through a redraw"), (await chips()).shown === 24);
      await click(lead + " .wfit");
      await sleep(150);
      const c2 = await chips();
      check(W("less folds them to one line again"), c2.h <= 24 && c2.fit === c0.fit, c2);
      // The branch's chips and the named ones share the line.
      const ip = await page.$eval(col("ipguard") + " .wline", (l) => [...l.querySelectorAll(".wbranch, .wchip")]
        .filter((c) => c.getClientRects().length).map((c) => c.textContent.trim())).catch(() => [""]);
      check(W("a branch, its PR and what was named share the one line"), ip[0].startsWith("⎇") && ip.some((s) => /#8/.test(s)), ip);

      // Esc sends Escape; Screen shows the screen, its icon then the conversation's.
      await page.click(lead + ' .aacts [data-act="a:agent-interrupt"]');
      await sleep(80);
      const esc = intents.filter((i) => i.op === "agent-interrupt").pop();
      check(W("Esc sends Escape to its agent"), esc && esc.name === LEAD, esc);
      await page.click(lead + ' .aacts [data-act="a:room-screen"]');
      await sleep(120);
      const watch = intents.filter((i) => i.op === "room-watch").pop();
      check(W("Screen asks for its screen"), watch && (watch.screens || []).includes(LEAD), watch);
      check(W("Screen's icon turns to the conversation's"), await page.$(lead + ' .aacts .scr-on[data-act="a:room-screen"]').then(Boolean));
      check(W("still one line, still level"), new Set((await columns(page)).map((c) => c.top)).size === 1);
      await page.click(lead + ' .aacts [data-act="a:room-screen"]');
      await sleep(120);
      check(W("pressed again, back to the conversation"), !(await page.$(lead + " .aacts .scr-on")));

      // ▾ opens the whole head; a press on line 3 does too.
      await click(lead + " .cx");
      await sleep(120);
      check(W("▾ opens the whole head: notes, every part"), await page.$(lead + ".expanded").then(Boolean) &&
        await shown(page, lead + " .anotes") && !(await shown(page, lead + " .rsay")) && await shown(page, lead + ' .aacts [data-act="a:agent-interrupt"]'));
      check(W("the whole head has the wide one's +20"), (await txt(lead + " .links .wmore")) === "+20");
      await click(lead + " .cx");
      await sleep(120);
      await click(col("investigator") + " .rsay");
      await sleep(120);
      check(W("a press on the third line opens the head too"), await page.$(col("investigator") + ".expanded").then(Boolean));
      await click(col("investigator") + " .cx");
      await sleep(120);

      // Asks: the first in line 3, +N for the rest; the head as high as the others.
      const asks = [
        { id: "a1", from: LEAD, to: "investigator", status: "delivered", head: "Find where the first odd request came from", created: Date.now() - 600e3 },
        { id: "a2", from: LEAD, to: "ipguard", status: "delivered", head: "Allowlist the ingress", created: Date.now() - 500e3 },
        { id: "a3", from: LEAD, to: "claude", status: "queued", head: "Rotate the tokens", created: Date.now() - 100e3 }];
      await page.evaluate((h) => GM.renderHerd(h), Object.assign(herd(4), { asks }));
      await sleep(150);
      const say = await txt(lead + " .rsay");
      check(W("a lead's open asks: the first on line 3, +2 for the rest"), /asked investigator/.test(say) && /\+2$/.test(say), say);
      check(W("the asker's title lists them all"), ["a1", "a2", "a3"].length === 3 &&
        /Find where/.test(await title(lead + " .rsay")) && /Rotate/.test(await title(lead + " .rsay")));
      check(W("a taker's line 3 is the ask it is on"), /ask from lead-ops/.test(await txt(col("investigator") + " .rsay")));
      check(W("with asks, every head is still the same height"), new Set((await columns(page)).map((c) => c.top)).size === 1,
        await columns(page));

      // A blocked agent's menu needs the user: it is never folded.
      const blocked = herd(4);
      Object.assign(blocked.agents.find((a) => a.name === "claude"), { state: "blocked", reason: "Command",
        prompt: { question: "Run this command?\nnpm test", options: [{ n: 1, label: "Yes" }, { n: 2, label: "No" }] } });
      await page.evaluate((h) => GM.renderHerd(h), blocked);
      await sleep(150);
      check(W("a blocked agent's menu and choices stay in its head"), await shown(page, col("claude") + " .areason.aprompt") &&
        await shown(page, col("claude") + ' .choices [data-act="a:agent-answer"]'));
      await page.evaluate((h) => GM.renderHerd(h), herd(4));
      await sleep(120);

      // The box: one line; the keyboard grows it; words keep it grown; it sends.
      const ta = lead + " .rfoot textarea";
      const h = () => page.$eval(ta, (e) => Math.round(e.getBoundingClientRect().height));
      const h0 = await h();
      check(W("the box is one line when idle"), h0 <= 34, h0);
      await page.focus(ta);
      await sleep(60);
      const h1 = await h();
      check(W("it grows when it has the keyboard"), h1 > h0 + 10, { h0, h1 });
      await page.keyboard.type("check the allowlist");
      await page.$eval(lead + " .rbody", (b) => b.focus && b.click && null);
      await page.evaluate(() => document.activeElement.blur());
      await sleep(60);
      check(W("with words in it, it stays grown"), (await h()) === h1);
      await page.evaluate((h) => GM.renderHerd(h), herd(4));
      await sleep(120);
      check(W("the words are kept through a redraw"), (await page.$eval(ta, (e) => e.value)) === "check the allowlist");
      await page.click(lead + ' .rfoot [data-act="a:send"]');
      await sleep(120);
      const sent = intents.filter((i) => i.op === "agent-prompt").pop();
      check(W("Send sends it to its agent"), sent && sent.name === LEAD && sent.text === "check the allowlist", sent);
      check(W("sent, the box is one line again"), (await h()) === h0);
      await page.focus(ta);
      await page.keyboard.type("and the ingress");
      await page.keyboard.press("Meta+Enter");
      await sleep(120);
      const sent2 = intents.filter((i) => i.op === "agent-prompt").pop();
      check(W("⌘↩ sends too"), sent2 && sent2.text === "and the ingress", sent2);
      await page.keyboard.type("x");
      await page.keyboard.press("Control+Enter");
      await sleep(120);
      check(W("C-↩ sends too"), intents.filter((i) => i.op === "agent-prompt").pop().text === "x");
      const img = await page.$eval(lead + ' .rfoot [data-act="a:image"]', (b) => ({ w: Math.round(b.getBoundingClientRect().width), f: getComputedStyle(b).fontSize, t: b.textContent.trim() }));
      check(W("Image is an icon, its word kept for the reader"), img.w <= 34 && img.f === "0px" && img.t === "Image", img);

      // A table wider than any column scrolls in its own box.
      await page.evaluate((p) => GM.setConversation(p), { name: LEAD, path: "/x/lead-ops.jsonl", cli: "claude", earlier: false,
        entries: CONVS[LEAD].concat([{ role: "assistant", text: WIDE, ts: new Date().toISOString() }]) });
      await sleep(150);
      const wide = (await cells(page, col(LEAD))).pop();
      check(W("a wide table scrolls sideways in its own box"), wide.scrolls && wide.box <= wide.msg && wide.least >= 3, wide);
      const ns = await noSideways(page, ".rbody, .rcols");
      check(W("and not the column or the page"), ns.page && ns.boxes, ns);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }

    // ---- Two wide columns: the whole head, as before ----
    {
      const { ctx, page, errors } = await phonePage(browser, { context: DESK });
      await openRoom(page, 2, "columns", (s) => page.click(s), sleep);
      const lead = col(LEAD);
      check("2 columns: the whole head -- notes on line 1, Esc and Screen as words, no ▾",
        await shown(page, lead + " .anotes") && !(await shown(page, lead + " .rsay")) && !(await shown(page, lead + " .cx")) &&
        (await page.$eval(lead + ' .aacts [data-act="a:agent-interrupt"]', (b) => b.textContent)) === "Esc" &&
        (await page.$eval(lead + ' .aacts [data-act="a:agent-interrupt"]', (b) => getComputedStyle(b).position)) === "static");
      check("2 columns: the wide head's +20", (await page.$eval(lead + " .links .wmore", (b) => b.textContent)) === "+20");
      check("2 columns: the box is two lines, Image a word", (await page.$eval(lead + " .rfoot textarea", (e) => e.getBoundingClientRect().height)) > 45 &&
        (await page.$eval(lead + ' .rfoot [data-act="a:image"]', (b) => getComputedStyle(b).fontSize)) !== "0px");
      // Narrowed, the window makes them narrow columns: the short head; widened, the whole one again.
      await page.setViewportSize({ width: 1100, height: 920 });
      await sleep(250);
      const narrowed = await columns(page);
      check("2 columns, the window narrowed to 1100: the short head, level", narrowed.every((c) => c.top <= 110) &&
        new Set(narrowed.map((c) => c.top)).size === 1 && await shown(page, lead + " .rsay"), narrowed);
      await page.setViewportSize({ width: 1890, height: 920 });
      await sleep(250);
      check("widened again: the whole head", await shown(page, lead + " .anotes") && !(await shown(page, lead + " .rsay")));
      check("2 columns: no page errors", errors.length === 0, errors);
      await ctx.close();
    }

    // ---- The chat, at the desk: its tables too ----
    {
      const { ctx, page, errors } = await phonePage(browser, { context: DESK });
      await openRoom(page, 4, "chat", (s) => page.click(s), sleep);
      await page.evaluate((es) => GM.setChat({ project: "~/src/shop-api/", earlier: false, missing: [], absent: [], entries: es }),
        [{ who: "user", to: LEAD, kind: "prompt", text: "表格？", ts: new Date(Date.now() - 60e3).toISOString(), agent: LEAD },
         { who: LEAD, kind: "turn", text: CONVS[LEAD][3].text + "\n\n" + WIDE, ts: new Date().toISOString(), agent: LEAD }]);
      await sleep(200);
      const t = await cells(page, ".chat .cbody");
      check("chat: every table cell is at least 3em wide", t.length === 2 && t.every((x) => x.least >= 3), t);
      check("chat: the wide table scrolls in its own box", t[1] && t[1].scrolls && t[1].box <= t[1].msg, t[1]);
      const ns = await noSideways(page, ".chat .cbody");
      check("chat: neither the page nor the timeline scrolls sideways", ns.page && ns.boxes, ns);
      check("chat: no page errors", errors.length === 0, errors);
      await ctx.close();
    }

    // ---- The phone: its own head and footer; the table fix ----
    for (const view of ["columns", "chat"]) {
      const { ctx, page, errors } = await phonePage(browser, { context: PHONE });
      await openRoom(page, 4, view, (s) => page.tap(s), sleep);
      const V = (s) => "phone " + view + ": " + s;
      if (view === "columns") {
        const lead = col(LEAD);
        check(V("its head keeps the phone's ▾ and folds as before"), await shown(page, lead + " .cx") &&
          !(await shown(page, lead + " .anotes")) && !(await shown(page, lead + ' .aacts [data-act="a:agent-interrupt"]')));
        check(V("none of the narrow desk column's lines"), !(await shown(page, lead + " .rsay")) && !(await shown(page, lead + " .wfit")) &&
          !(await shown(page, lead + " .wline .wchip")));
        const ta = await page.$eval(lead + " .rfoot textarea", (e) => ({ h: Math.round(e.getBoundingClientRect().height), min: getComputedStyle(e).minHeight, resize: getComputedStyle(e).resize }));
        check(V("its box is the phone's"), ta.min === "38px" && ta.resize === "none", ta);
        await page.tap(lead + " .cx");
        await sleep(150);
        check(V("▾ shows the chips, wrapping, with the wide +20"), await shown(page, lead + " .links .wmore") &&
          (await page.$eval(lead + " .links .wmore", (b) => b.textContent)) === "+20" && !(await shown(page, lead + " .wfit")));
        const t = await cells(page, lead);
        check(V("the table's cells are each at least 3em wide"), t.length && t.every((x) => x.least >= 3), t);
        const ns = await noSideways(page, ".rbody");
        check(V("neither the page nor a column scrolls sideways"), ns.page && ns.boxes, ns);
      } else {
        await page.evaluate((es) => GM.setChat({ project: "~/src/shop-api/", earlier: false, missing: [], absent: [], entries: es }),
          [{ who: LEAD, kind: "turn", text: CONVS[LEAD][3].text + "\n\n" + WIDE, ts: new Date().toISOString(), agent: LEAD }]);
        await sleep(200);
        const t = await cells(page, ".chat .cbody");
        check(V("every table cell is at least 3em wide"), t.length === 2 && t.every((x) => x.least >= 3), t);
        check(V("the wide table scrolls in its own box"), t[1] && t[1].scrolls && t[1].box <= t[1].msg, t[1]);
        const ns = await noSideways(page, ".chat .cbody");
        check(V("neither the page nor the timeline scrolls sideways"), ns.page && ns.boxes, ns);
      }
      check(V("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
