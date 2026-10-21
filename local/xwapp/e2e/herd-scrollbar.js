// Scrollbars wear the theme's colours, not the browser's light default.
// base.css sets color-scheme and themed, token-driven scrollbar colours, so
// every scroll area -- here an overflowing transcript column -- gets a thin
// dark thumb in the dark theme, and would follow a light theme's tokens.
// Checks the computed styling in dark and in a simulated light theme, and
// writes before/after/light screenshots of a Columns transcript that scrolls.
const { webkit } = require("playwright");
const fs = require("fs");
const path = require("path");
const { execFileSync } = require("child_process");
const { HERD, sleep } = require("./herdlib");

const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

const LOCAL = process.env.LOCAL || path.resolve(__dirname, "../..");
const UIDIR = path.join(LOCAL, "ghostherd", "ui");
const PRDIR = path.join(LOCAL, "pr-view", "ui");
const XWAPP = path.join(LOCAL, "xwapp");
const SECRET = "0123456789abcdef0123456789abcdef";
const SHOTS = path.join(__dirname, "shots");
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const RD = "claude-deploy-platform";
const COL = '.rcol[data-name="' + RD + '"] .rbody';

// An xwapp page on a desk, served from this checkout as Emacs would -- the
// herd by default, or pr-view ({ app: "pr-view", wait: false }, which only
// needs the stylesheets, not its Emacs data).  base.css may be overridden
// (the pre-fix file for the "before" shot).
const DIRS = { xwapp: path.join(XWAPP, "ui"), ghostherd: UIDIR, "pr-view": PRDIR };
async function deskPage(browser, baseCss, opts = {}) {
  const app = opts.app || "ghostherd";
  const BOX = "http://box.tail0.ts.net";
  const ctx = await browser.newContext(DESK);
  await ctx.route("**/*", async (route) => {
    const url = new URL(route.request().url());
    if (url.origin !== BOX) return route.fulfill({ status: 200, contentType: "text/plain", body: "away" });
    const parts = url.pathname.split("/").filter(Boolean);
    if (parts[0] !== SECRET) return route.fulfill({ status: 404, body: "Not Found" });
    const rest = parts.slice(1);
    if (rest[0] === "hello") return route.fulfill({ status: 204 });
    if (rest[0] === "intent") return route.fulfill({ status: 204 });
    if (rest[0] === "next") { await sleep(1500); return route.fulfill({ status: 200, contentType: "application/json", body: "[]" }).catch(() => {}); }
    const rel = rest.slice(2).join("/");
    if (baseCss && rest[0] === "xwapp" && rel === "base.css")
      return route.fulfill({ status: 200, contentType: "text/css; charset=utf-8", body: baseCss });
    const base = DIRS[rest[0]] || null;
    const file = base && path.join(base, ...rest.slice(2));
    if (!file || !fs.existsSync(file)) return route.fulfill({ status: 404, body: "Not Found" });
    const type = { html: "text/html", js: "text/javascript", css: "text/css" }[file.split(".").pop()] || "application/octet-stream";
    return route.fulfill({ status: 200, contentType: type + "; charset=utf-8", body: fs.readFileSync(file) });
  });
  const page = await ctx.newPage();
  await page.goto(BOX + "/" + SECRET + "/" + app + "/ui/index.html");
  if (opts.wait === false) await page.waitForLoadState("load");
  else await page.waitForFunction(() => window.GM && window.XW);
  return { ctx, page };
}

// A long, wrapping transcript so the column overflows and shows its scrollbar.
const longTurns = (n) => Array.from({ length: n }, (_, i) => ({
  role: i % 2 ? "user" : "assistant",
  text: "Turn " + (i + 1) + ": " + Array(6).fill("a sufficiently long line of transcript text that wraps").join(" "),
  ts: "t" + String(i).padStart(6, "0"),
}));

async function openColumn(page) {
  await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
  await sleep(120);
  await page.click('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
  await sleep(160);
  await page.click('.room [data-act="a:room-view"][data-view="columns"]');
  await sleep(140);
  await page.evaluate((p) => GM.setConversation(p), { name: RD, path: "/x/abc.jsonl", cli: "claude", earlier: false, entries: longTurns(40) });
  await sleep(120);
}

const RGB = (hex) => { const n = parseInt(hex.slice(1), 16); return `rgb(${(n >> 16) & 255}, ${(n >> 8) & 255}, ${n & 255})`; };

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- Computed styling: dark theme (the real tokens) ----
    {
      const { ctx, page } = await deskPage(browser, null);
      await openColumn(page);

      const overflows = await page.$eval(COL, (b) => b.scrollHeight > b.clientHeight + 10);
      check("the transcript column overflows (there is a scrollbar to style)", overflows);

      const cs = await page.evaluate(() => getComputedStyle(document.documentElement).colorScheme);
      check("the page declares color-scheme: dark", /dark/.test(cs), { cs });

      const sw = await page.$eval(COL, (el) => getComputedStyle(el).scrollbarWidth);
      check("scroll areas are thin (scrollbar-width)", sw === "thin", { sw });

      // WebKit (Safari, the xwidget) ignores scrollbar-color and themes via
      // the pseudo-element -- which it does compute, so we read it there.
      const line = RGB((await page.evaluate(() => getComputedStyle(document.documentElement).getPropertyValue("--line").trim())));
      const thumb = await page.$eval(COL, (el) => getComputedStyle(el, "::-webkit-scrollbar-thumb").backgroundColor);
      check("the thumb is the theme's --line token", thumb === line, { thumb, line });
      const barW = await page.$eval(COL, (el) => getComputedStyle(el, "::-webkit-scrollbar").width);
      check("the scrollbar is thin and present (12px gutter)", barW === "12px", { barW });
      const ch = (thumb.match(/\d+/g) || []).map(Number);
      check("the thumb is dark, not the browser's default light grey",
        ch.length >= 3 && ch[0] < 128 && ch[1] < 128 && ch[2] < 128, { thumb });

      await ctx.close();
    }

    // ---- The colours follow the tokens: a simulated light theme ----
    {
      const { ctx, page } = await deskPage(browser, null);
      await openColumn(page);
      // Flip to a light theme the way a future theme block would: redefine
      // the tokens and color-scheme on :root.  The scrollbar must follow.
      const lightLine = "#d0d7de";
      await page.evaluate((hex) => {
        const r = document.documentElement;
        r.style.colorScheme = "light";
        r.style.setProperty("--line", hex);
        r.style.setProperty("--bg", "#ffffff");
        r.style.setProperty("--muted", "#57606a");
      }, lightLine);
      await sleep(60);
      const cs = await page.evaluate(() => getComputedStyle(document.documentElement).colorScheme);
      check("light theme: color-scheme becomes light", /light/.test(cs) && !/dark/.test(cs), { cs });
      const thumb = await page.$eval(COL, (el) => getComputedStyle(el, "::-webkit-scrollbar-thumb").backgroundColor);
      check("light theme: the thumb follows the token to its light value", thumb === RGB(lightLine), { thumb, want: RGB(lightLine) });
      await ctx.close();
    }

    // ---- pr-view matches: the fix lives in the shared base.css ----
    {
      const { ctx, page } = await deskPage(browser, null, { app: "pr-view", wait: false });
      const cs = await page.evaluate(() => getComputedStyle(document.documentElement).colorScheme);
      check("pr-view declares color-scheme: dark (the same base.css)", /dark/.test(cs), { cs });
      const line = RGB((await page.evaluate(() => getComputedStyle(document.documentElement).getPropertyValue("--line").trim())));
      const thumb = await page.evaluate(() => getComputedStyle(document.documentElement, "::-webkit-scrollbar-thumb").backgroundColor);
      check("pr-view's thumb is the same --line token (the two pages match)", thumb === line, { thumb, line });
      await ctx.close();
    }

    // ---- Screenshots: before (default), after (dark), after (light) ----
    // Playwright's WebKit draws overlay scrollbars that never paint in a static
    // capture (the real page keeps them -- unobtrusive).  To make the bar
    // visible for the shot only, force it to a classic, space-reserving bar
    // (-webkit-appearance: none on an overflow:scroll element).  The colours
    // stay the page's real tokens, so what you see is the real theming.
    // Resetting the standard scrollbar-* back to auto makes WebKit fall back
    // to the ::-webkit-scrollbar pseudo (which base.css colours from tokens);
    // -webkit-appearance: none on an overflow:scroll element then paints it
    // classic.  Colours are untouched, so the shot shows the real theming.
    const PAINT = `* { scrollbar-width: auto !important; scrollbar-color: auto !important; }
      .rbody, .cbody, .subbody, .rsubs, .app, textarea { overflow-y: scroll !important; }
      ::-webkit-scrollbar { -webkit-appearance: none !important; width: 12px; height: 12px; }`;
    // The pre-fix look: the browser's own default scrollbar -- a light track
    // and grey thumb, as the user reported -- which WebKit otherwise hides as
    // an overlay.  (The real bug is this bar glaring on the dark page.)
    const DEFAULT_LIGHT = `::-webkit-scrollbar-track { background: #fbfbfb !important; }
      ::-webkit-scrollbar-thumb { background: #c7c7c7 !important; border: 3px solid #fbfbfb !important; border-radius: 8px; }`;
    fs.mkdirSync(SHOTS, { recursive: true });
    const orig = execFileSync("git", ["-C", XWAPP, "show", "HEAD:./ui/base.css"], { encoding: "utf8" });
    const shoot = async (name, baseCss, extra, light) => {
      const { ctx, page } = await deskPage(browser, baseCss);
      await openColumn(page);
      if (light) await page.evaluate(() => {
        const r = document.documentElement;
        r.style.colorScheme = "light";
        r.style.setProperty("--bg", "#ffffff");
        r.style.setProperty("--panel", "#f6f8fa");
        r.style.setProperty("--text", "#1f2328");
        r.style.setProperty("--muted", "#57606a");
        r.style.setProperty("--line", "#d0d7de");
      });
      await page.addStyleTag({ content: PAINT + (extra || "") });
      await sleep(90);
      const gutter = await page.$eval(COL, (el) => el.offsetWidth - el.clientWidth);
      check(name + ": the scrollbar is painted (reserves a gutter) for the shot", gutter >= 10, { gutter });
      await page.screenshot({ path: path.join(SHOTS, "scrollbar-" + name + ".png") });
      await ctx.close();
    };
    await shoot("before-dark", orig, DEFAULT_LIGHT, false);
    await shoot("after-dark", null, "", false);
    await shoot("after-light", null, "", true);
    console.log("shots: " + path.join(SHOTS, "scrollbar-{before-dark,after-dark,after-light}.png"));
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
