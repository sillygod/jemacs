// The Memory tab's order (ask 340c2363).  The server lists a store's facts
// by file name, shared ones first, so the dates beside them jumped about
// and the list read as random.  The page sorts its view: newest change
// first by default (updated, else created; a fact with neither last, by
// name), or A–Z by the name it shows -- a toggle beside Promoted, kept per
// viewer in localStorage.  Filter and Promoted work on the sorted list; a
// page that may not store the choice still sorts.  Desk widths and phone.
// Screenshots go to shots/memory-sort/ (SHOTS overrides).
const { webkit } = require("playwright");
const fs = require("fs");
const path = require("path");
const { phonePage } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const SHOTS = process.env.SHOTS || path.join(__dirname, "shots", "memory-sort");
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };

const now = Date.now();
const iso = (ms) => new Date(ms).toISOString();
const PROJECTS = { user: { key: "user", label: "User (shared)" },
  projects: [{ key: "-src-dp", label: "deploy-platform", project: "~/src/deploy-platform/" }] };
const fact = (name, title, extra) => Object.assign({ name, title, description: title.toLowerCase(),
  type: "project", author: "claude-deploy-platform", body: title, shared: false }, extra);
// In the server's order: shared first, then by file name.  Neither the
// recent order nor A–Z is this one.
const FACTS = [
  fact("prefers-reasoned-design", "Prefers reasoned design", { type: "user", shared: true, author: "",
    updated: iso(now - 70 * 86400e3) }),
  fact("agent-token-hardening", "Agent token and env hardening", { updated: iso(now - 86400e3) }),
  fact("cli-trust-stores", "Cli trust stores", { type: "reference", created: iso(now - 3 * 3600e3) }),
  fact("old-promoted", "Old promoted", { updated: iso(now - 9 * 86400e3), promoted: "CLAUDE.md" }),
  fact("undated-alpha", "Alpha undated", {}),
  fact("undated-beta", "Beta undated", {}),
  fact("worktree-trust", "Worktree trust", { updated: iso(now - 300e3), created: iso(now - 9e8) }),
  fact("young-promoted", "Young promoted", { updated: iso(now - 60e3), promoted: "AGENTS.md" }),
];
const RECENT = ["worktree-trust", "cli-trust-stores", "agent-token-hardening", "prefers-reasoned-design",
                "undated-alpha", "undated-beta"];
const AZ = ["agent-token-hardening", "undated-alpha", "undated-beta", "cli-trust-stores",
            "prefers-reasoned-design", "worktree-trust"];

const order = (page) => page.$$eval("#mem-list .mhead", (es) => es.map((e) => e.getAttribute("data-name")));
const pressed = (page) => page.$$eval('[data-act="mem-sort"]', (es) =>
  es.filter((e) => e.getAttribute("aria-pressed") === "true" && e.classList.contains("on"))
    .map((e) => e.getAttribute("data-sort")));
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

async function show(page, facts) {
  await page.evaluate(() => GM.showView("memory"));
  await page.evaluate((p) => GM.setMemoryProjects(p), PROJECTS);
  await page.evaluate((f) => GM.renderMemory({ key: "-src-dp", facts: f }), facts || FACTS);
  await sleep(80);
}

// The toolbar on one line or wrapped as the phone has it, never overlapping
// itself, a label never broken, nothing past the page's edge.
async function toolbar(page) {
  return page.evaluate(() => {
    const vw = document.documentElement.clientWidth;
    const parts = [...document.querySelectorAll(".mem-tools select, .mem-tools input, .mem-tools button")];
    const r = parts.map((e) => ({ id: e.id || e.getAttribute("data-sort") || e.getAttribute("data-act") || e.tagName,
      b: e.getBoundingClientRect() }));
    const overlaps = [];
    for (let i = 0; i < r.length; i++)
      for (let j = i + 1; j < r.length; j++) {
        const a = r[i].b, b = r[j].b;
        // The two halves of the order control share a border: 1px is theirs.
        const w = Math.min(a.right, b.right) - Math.max(a.left, b.left);
        const h = Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top);
        if (w > 1.5 && h > 1.5) overlaps.push(r[i].id + "/" + r[j].id);
      }
    const wrapped = [...document.querySelectorAll(".mem-tools button")].filter((b) => {
      const range = document.createRange(); range.selectNodeContents(b);
      const tops = [...range.getClientRects()].map((x) => Math.round(x.top));
      return tops.length && Math.max(...tops) - Math.min(...tops) > 4;
    }).map((b) => b.textContent);
    const off = r.filter((x) => x.b.left < -0.5 || x.b.right > vw + 0.5).map((x) => x.id);
    const sideways = document.documentElement.scrollWidth > vw + 1;
    // Rows: which controls share the filter's line, and Promoted's.
    const box = (sel) => document.querySelector(sel).getBoundingClientRect();
    const mid = (r) => (r.top + r.bottom) / 2;
    const row = (a, b) => Math.abs(mid(box(a)) - mid(box(b))) < 4;
    const rows = { orderByFilter: row("#mem-filter", '[data-sort="recent"]'),
                   promotedBySuggest: row("#mem-promoted", '[data-act="mem-suggest"]'),
                   filterWidth: Math.round(box("#mem-filter").width) };
    return { overlaps, wrapped, off, sideways, rows };
  });
}

(async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const browser = await webkit.launch();
  try {
    for (const [label, ctxOpts] of [["desk", DESK], ["phone", PHONE]]) {
      const host = label === "desk" ? "127.0.0.1" : undefined;
      const { ctx, page, errors, intents } = await phonePage(browser, { context: ctxOpts, host });
      const W = (n) => label + ": " + n;
      await show(page);

      // ---- newest first, by default ----
      check(W("a new viewer starts on Recent"), same(await pressed(page), ["recent"]), await pressed(page));
      check(W("newest change first; created when not updated; undated last, by name"),
        same(await order(page), RECENT), await order(page));
      const shared = await page.$eval('.mhead[data-name="prefers-reasoned-design"]', (e) => !!e.querySelector(".mshared"));
      check(W("a shared fact sorts in with the rest, its badge on"),
        shared && (await order(page))[0] !== "prefers-reasoned-design");
      // The age, without the "by AUTHOR · " before it.
      const ages = await page.$$eval("#mem-list .mmeta", (es) => es.map((e) => {
        const t = e.textContent.trim();
        return t.startsWith("by ") ? (t.includes(" · ") ? t.split(" · ").pop() : "") : t;
      }));
      check(W("the age column reads newest to oldest, created standing in for updated"),
        /m ago$/.test(ages[0]) && /h ago$/.test(ages[1]) && /d ago$/.test(ages[2]) &&
        /[A-Z][a-z]{2} \d/.test(ages[3]) && ages[4] === "" && ages[5] === "", ages);
      if (label === "desk") await page.screenshot({ path: path.join(SHOTS, "desk-recent.png") });

      // ---- A–Z, and back ----
      await page.click('[data-act="mem-sort"][data-sort="name"]');
      await sleep(60);
      check(W("A–Z orders by the name each fact shows"), same(await order(page), AZ), await order(page));
      check(W("A–Z is the one pressed"), same(await pressed(page), ["name"]), await pressed(page));
      const kept = await page.evaluate(() => localStorage.getItem("ghostherd.memory.sort"));
      check(W("the choice is kept in this browser"), kept === "name", kept);
      check(W("sorting asks nothing of Emacs"),
        !intents.some((i) => /sort/.test(JSON.stringify(i))), intents.map((i) => i.op));
      if (label === "desk") await page.screenshot({ path: path.join(SHOTS, "desk-az.png") });

      // ---- the filter and Promoted keep working, in the chosen order ----
      await page.fill("#mem-filter", "undated");
      await sleep(60);
      check(W("the filter narrows the sorted list"),
        same(await order(page), ["undated-alpha", "undated-beta"]), await order(page));
      await page.fill("#mem-filter", "trust");
      await sleep(60);
      check(W("a filter in A–Z keeps A–Z"),
        same(await order(page), ["cli-trust-stores", "worktree-trust"]), await order(page));
      await page.click('[data-act="mem-sort"][data-sort="recent"]');
      await sleep(60);
      check(W("the same filter in Recent is newest first"),
        same(await order(page), ["worktree-trust", "cli-trust-stores"]), await order(page));
      await page.fill("#mem-filter", "");
      await sleep(60);
      await page.click("#mem-promoted");
      await sleep(60);
      check(W("Promoted shows the promoted facts, newest first"),
        same(await order(page), ["young-promoted", "old-promoted"]), await order(page));
      await page.click('[data-act="mem-sort"][data-sort="name"]');
      await sleep(60);
      check(W("Promoted in A–Z"), same(await order(page), ["old-promoted", "young-promoted"]), await order(page));
      await page.click("#mem-promoted");
      await sleep(60);

      // ---- remembered: the page loaded again comes back to the choice ----
      await page.reload();
      await page.waitForFunction(() => window.GM && window.XW);
      await show(page);
      check(W("loaded again, A–Z is still chosen"), same(await pressed(page), ["name"]), await pressed(page));
      check(W("and the list is still A–Z"), same(await order(page), AZ), await order(page));
      await page.click('[data-act="mem-sort"][data-sort="recent"]');
      await sleep(40);
      await page.reload();
      await page.waitForFunction(() => window.GM && window.XW);
      await show(page);
      check(W("Recent chosen again is remembered too"),
        same(await pressed(page), ["recent"]) && same(await order(page), RECENT), await order(page));

      // ---- a fact learned now lands on top ----
      await page.evaluate((f) => GM.renderMemory({ key: "-src-dp", facts: f.concat([
        Object.assign({}, f[2], { name: "just-learned", title: "Just learned", updated: new Date().toISOString() })]) }), FACTS);
      await sleep(60);
      check(W("a fact pushed live goes to the top"), (await order(page))[0] === "just-learned", await order(page));
      await show(page);

      // ---- the toolbar fits ----
      const widths = label === "desk" ? [1400, 1100, 900, 780, 760, 641] : [390, 360, 320];
      for (const w of widths) {
        await page.setViewportSize({ width: w, height: ctxOpts.viewport.height });
        await sleep(80);
        const t = await toolbar(page);
        if (w === 760) await page.screenshot({ path: path.join(SHOTS, "desk-760.png") });
        check(W("toolbar at " + w + ": nothing overlaps, no label breaks, nothing off the page"),
          !t.overlaps.length && !t.wrapped.length && !t.off.length && !t.sideways, t);
        // The order sits by the filter, Promoted by Suggest, and the filter
        // keeps room to type in.
        check(W("toolbar at " + w + ": the order by the filter, Promoted by Suggest, the filter wide enough"),
          t.rows.orderByFilter && t.rows.promotedBySuggest && t.rows.filterWidth >= Math.min(200, w * 0.3), t.rows);
      }
      await page.setViewportSize(ctxOpts.viewport);
      await sleep(60);
      if (label === "phone") await page.screenshot({ path: path.join(SHOTS, "phone-recent.png") });

      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }

    // ---- storage refused: still sorts, starts on Recent, no error ----
    // A page of the same context, opened after storage is taken away -- as a
    // private window or a refusing origin has it.
    const { ctx, page, errors } = await phonePage(browser, { context: DESK, host: "127.0.0.1" });
    const url = page.url();
    await page.close();
    await ctx.addInitScript(() => {
      const no = () => { throw new DOMException("refused", "SecurityError"); };
      Object.defineProperty(window, "localStorage", { get: no, configurable: true });
    });
    const p2 = await ctx.newPage();
    const errs2 = [];
    p2.on("pageerror", (e) => errs2.push(String(e)));
    await p2.goto(url);
    await p2.waitForFunction(() => window.GM && window.XW);
    const refused = await p2.evaluate(() => { try { localStorage; return false; } catch (e) { return true; } });
    await show(p2);
    check("no storage: the page cannot store here", refused);
    check("no storage: Recent, newest first", same(await pressed(p2), ["recent"]) && same(await order(p2), RECENT),
      await order(p2));
    await p2.click('[data-act="mem-sort"][data-sort="name"]');
    await sleep(60);
    check("no storage: A–Z still works for this page", same(await order(p2), AZ), await order(p2));
    check("no storage: no page errors", errs2.length === 0 && errors.length === 0, errs2.concat(errors));
    await ctx.close();
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log("\n" + (results.length - failed) + "/" + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
