// The top bar at every width, phone to wide desk: nothing in it runs
// over anything else, no tab or button label wraps, the open tab and the
// tabs' counts are on screen or a swipe away, and every action -- Back in
// a room, + New agent, Phone, Import, Refresh -- is in the bar or behind
// ⋯.  The room's head below it fits too.  A screenshot of each, on the
// Agents tab and in a room, in shots/topbar/.
const fs = require("fs");
const path = require("path");
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const OUT = process.env.SHOTS || path.join(__dirname, "shots", "topbar");
fs.mkdirSync(OUT, { recursive: true });
const DP = "~/src/deploy-platform/";
const WIDTHS = (process.env.WIDTHS || "1400,1100,900,870,760,640,390").split(",").map(Number);
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const sources = (n) => Array.from({ length: n }, (_, i) => ({ source_path: "/s/" + i, project: DP, mtime: i }));

// What the bar looks like, read in the page.
function look() {
  const shown = (e) => !!e && e.getClientRects().length > 0;
  const box = (e) => { const r = e.getBoundingClientRect(); return { l: r.left, r: r.right, t: r.top, b: r.bottom }; };
  const name = (e) => e.id || e.getAttribute("data-tab") || (e.hasAttribute("data-xw-phone") ? "phone" : e.className);
  // Lines a label takes: its pieces' tops, apart by more than half a line.
  const lines = (e) => {
    const range = document.createRange();
    range.selectNodeContents(e);
    const tops = [...range.getClientRects()].filter((q) => q.width > 0).map((q) => q.top).sort((a, b) => a - b);
    return tops.reduce((n, t, i) => n + (i && t - tops[i - 1] > 8 ? 1 : 0), tops.length ? 1 : 0);
  };
  const meet = (a, b) => a.l < b.r - 0.5 && b.l < a.r - 0.5 && a.t < b.b - 0.5 && b.t < a.b - 0.5;
  const W = document.documentElement.clientWidth;
  const top = document.querySelector(".top");
  const tabs = document.querySelector(".tabs");
  const tb = box(tabs);
  // The bar's pieces: the mark, what the tabs show, each button shown.
  const parts = [];
  const mark = document.querySelector(".mark");
  if (shown(mark)) parts.push(["mark", box(mark)]);
  parts.push(["tabs", tb]);
  const buttons = [...document.querySelectorAll(".actions button")].filter(shown).filter((b) => !b.closest(".more.open"));
  buttons.forEach((b) => parts.push([name(b), box(b)]));
  const overlaps = [];
  parts.forEach(([n, a], i) => parts.slice(i + 1).forEach(([m, b]) => { if (meet(a, b)) overlaps.push(n + "/" + m); }));
  const off = parts.filter(([, a]) => a.l < -0.5 || a.r > W + 0.5).map(([n]) => n);
  const tabList = [...tabs.querySelectorAll(".tab")];
  const on = tabs.querySelector(".tab.on");
  // Each tab, scrolled to if it must be: wholly in what the tabs show.
  const keep = tabs.scrollLeft;
  const unreachable = tabList.filter((t) => {
    let r = t.getBoundingClientRect();
    if (r.left < tb.l) tabs.scrollLeft -= tb.l - r.left;
    else if (r.right > tb.r) tabs.scrollLeft += r.right - tb.r;
    r = t.getBoundingClientRect();
    return r.left < tb.l - 1 || r.right > tb.r + 1 || r.right > W + 0.5;
  }).map(name);
  tabs.scrollLeft = keep;
  const onBox = on && box(on);
  return {
    W,
    height: Math.round(top.getBoundingClientRect().height),
    pageSideways: document.documentElement.scrollWidth > W,
    barSideways: top.scrollWidth > top.clientWidth,
    overlaps, off,
    wrapped: tabList.concat(buttons).filter((e) => lines(e) > 1).map(name),
    scrolls: tabs.scrollWidth > tabs.clientWidth,
    onTab: on && on.getAttribute("data-tab"),
    onInView: !!onBox && onBox.l >= tb.l - 1 && onBox.r <= tb.r + 1,
    unreachable,
    counts: ["inbox-n", "threads-n", "sessions-n"].map((id) => {
      const c = document.getElementById(id);
      return shown(c) && c.textContent.trim() && c.getClientRects().length === 1 ? "" : id;
    }).filter(Boolean),
    bar: buttons.map(name),
    mark: shown(mark),
  };
}

// The room's head: nothing past its edges, its parts apart, its buttons
// one line each.
function lookHead() {
  const head = document.querySelector(".room .rhead");
  if (!head) return null;
  const hb = head.getBoundingClientRect();
  const out = [];
  const walk = (e) => {
    for (const c of e.children) {
      if (!c.getClientRects().length) continue;
      const r = c.getBoundingClientRect();
      if (r.left < hb.left - 0.5 || r.right > hb.right + 0.5) out.push((c.className || c.tagName) + " " + Math.round(r.left) + "-" + Math.round(r.right));
      if (getComputedStyle(c).overflowX === "visible") walk(c);
    }
  };
  walk(head);
  const kids = [...head.children].filter((c) => c.getClientRects().length && c.getBoundingClientRect().width > 0);
  const overlaps = [];
  kids.forEach((a, i) => kids.slice(i + 1).forEach((b) => {
    const p = a.getBoundingClientRect(), q = b.getBoundingClientRect();
    if (p.left < q.right - 0.5 && q.left < p.right - 0.5 && p.top < q.bottom - 0.5 && q.top < p.bottom - 0.5)
      overlaps.push(a.className + "/" + b.className);
  }));
  const tall = [...head.querySelectorAll(".rview button")].filter((b) => b.getBoundingClientRect().height > 40).map((b) => b.textContent);
  return { past: out, overlaps, tall, right: Math.round(hb.right), W: document.documentElement.clientWidth };
}

async function barChecks(page, W, state) {
  const b = await page.evaluate(look);
  check(W(state + ": the bar is one row, nothing sideways"), b.height <= 52 && !b.pageSideways && !b.barSideways, b);
  check(W(state + ": nothing in the bar runs over anything else, or off the screen"), !b.overlaps.length && !b.off.length, b);
  check(W(state + ": no tab or button label wraps"), !b.wrapped.length, b);
  check(W(state + ": the open tab is in view"), b.onInView, b);
  check(W(state + ": every tab, and its count, on screen or a swipe away"), !b.unreachable.length && !b.counts.length, b);
  return b;
}

// Each action in the bar, or behind ⋯: on screen, and the thing a click
// there lands on.
async function reachable(page, sel) {
  const hit = (sel) => page.evaluate((sel) => {
    const e = document.querySelector(sel);
    if (!e || !e.getClientRects().length) return false;
    const r = e.getBoundingClientRect();
    if (r.left < 0 || r.right > innerWidth || r.top < 0 || r.bottom > innerHeight) return false;
    return e.contains(document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2));
  }, sel);
  if (await hit(sel)) return "bar";
  if (!(await hit("#btn-more"))) return "";
  await page.click("#btn-more");
  await sleep(50);
  const there = await hit(sel);
  await page.keyboard.press("Escape");
  await sleep(50);
  return there ? "more" : "";
}

(async () => {
  const browser = await webkit.launch();
  try {
    const runs = WIDTHS.map((w) => ["desk", w]).concat(WIDTHS.includes(390) ? [["phone", 390]] : []);
    for (const [where, w] of runs) {
      const intents = [];
      const desk = where === "desk";
      const { ctx, page, errors } = await phonePage(browser, desk
        ? { intents, host: "127.0.0.1", context: { viewport: { width: w, height: 800 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false } }
        : { intents });
      const W = (n) => where + " " + w + ": " + n;
      const tap = (sel) => (desk ? page.click(sel) : page.tap(sel));
      await page.evaluate(([h, s]) => {
        // At the desk Emacs says the page can be had on a phone.
        if (!XW.elsewhere) XW.kit({ xwapp: "phone" });
        GM.setContext({ sources: s });
        GM.renderInbox({ items: [], count: 3 });
        GM.threadsBadge({ live: 1 });
        GM.showView("agents");
        GM.renderHerd(h);
      }, [HERD, sources(802)]);
      await sleep(120);

      // ---- The Agents tab ----
      const a = await barChecks(page, W, "Agents");
      console.log("     " + W("bar: " + (a.mark ? "HERD, " : "") + "tabs" + (a.scrolls ? " (scroll)" : "") + ", " + a.bar.join(" ")));
      await page.screenshot({ path: path.join(OUT, where + "-" + w + "-agents.png") });

      // ---- A room: Back ----
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(200);
      const r = await barChecks(page, W, "room");
      console.log("     " + W("bar: " + (r.mark ? "HERD, " : "") + "tabs" + (r.scrolls ? " (scroll)" : "") + ", " + r.bar.join(" ")));
      const head = await page.evaluate(lookHead);
      check(W("the room's head: nothing past its edges, its parts apart, its buttons one line"),
        head && !head.past.length && !head.overlaps.length && !head.tall.length, head);
      const where2 = {};
      for (const [n, sel] of [["Back", "#btn-back"], ["New agent", "#btn-new"], ["Import", "#btn-import"], ["Refresh", "#btn-refresh"]]
        .concat(desk ? [["Phone", "[data-xw-phone]"]] : [])) where2[n] = await reachable(page, sel);
      check(W("every action in the bar or behind ⋯"), Object.values(where2).every(Boolean), where2);
      console.log("     " + W(JSON.stringify(where2)));
      check(W("still in the room after ⋯ and Esc"), !!(await page.$(".room")));
      await page.evaluate(() => document.activeElement && document.activeElement.blur());
      if (desk) await page.mouse.move(w / 2, 400);
      await page.screenshot({ path: path.join(OUT, where + "-" + w + "-room.png") });

      // ---- Counts as large as they come: still no overlap, no wrap ----
      await page.evaluate((s) => {
        GM.setContext({ sources: s });
        GM.renderInbox({ items: [], count: 128 });
        GM.threadsBadge({ live: 12 });
        GM.showView("log");
      }, sources(12345));
      await sleep(120);
      await barChecks(page, W, "Log, big counts");

      // ---- ⋯ itself, where it is: what it holds works, and it shuts ----
      if (await page.evaluate(() => { const m = document.getElementById("btn-more"); return !!m && getComputedStyle(m).display !== "none"; })) {
        await tap("#btn-more");
        await sleep(60);
        const open = await page.evaluate(() => ({ open: document.getElementById("more").classList.contains("open"),
          expanded: document.getElementById("btn-more").getAttribute("aria-expanded") }));
        check(W("⋯ opens its menu"), open.open && open.expanded === "true", open);
        if (w === 870 || where === "phone") await page.screenshot({ path: path.join(OUT, where + "-" + w + "-more.png") });
        await tap("#btn-import");
        await sleep(60);
        check(W("Import from it imports, and the menu shuts"),
          intents.some((i) => i.op === "import") && !(await page.$(".more.open")), intents.map((i) => i.op));
        await tap("#btn-more");
        await sleep(40);
        await tap(".app");
        await sleep(40);
        check(W("a tap elsewhere shuts it"), !(await page.$(".more.open")));
      } else {
        check(W("wide: Phone, Import and Refresh in the bar, no ⋯"), a.bar.includes("btn-import") && a.bar.includes("btn-refresh"), a.bar);
      }

      // ---- Tabs that scroll: a wheel turns them ----
      if (desk && (await page.evaluate(() => { const t = document.querySelector(".tabs"); return t.scrollWidth > t.clientWidth; }))) {
        const before = await page.evaluate(() => { const t = document.querySelector(".tabs"); t.scrollLeft = 0; return t.scrollLeft; });
        const tb = await page.$eval(".tabs", (t) => { const r = t.getBoundingClientRect(); return { x: r.left + 20, y: r.top + r.height / 2 }; });
        await page.mouse.move(tb.x, tb.y);
        await page.mouse.wheel(0, 120);
        await sleep(150);
        const after = await page.$eval(".tabs", (t) => t.scrollLeft);
        check(W("a mouse wheel over the tabs scrolls them sideways"), after > before, { before, after });
      }
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  console.log("shots: " + OUT);
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
