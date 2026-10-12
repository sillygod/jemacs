// pr-view and clickup-view in a narrow window -- Emacs split side by
// side, or a phone: what scrolls sideways that should not, and what
// sticks out of it.  A pre, a diff, a wide table may scroll; nothing else.
const path = require("path");
const fs = require("fs");
const { start, watch, sideways, until, sleep, BOX, PHONE } = require("./appslib");
const OUT = process.env.SHOTS || path.join(__dirname, "shots", "narrow");
fs.mkdirSync(OUT, { recursive: true });
const PR = "https://bitbucket.org/unno/shop/pull-requests/42";
const WIDTHS = (process.env.WIDTHS || "320,348,409,560,760").split(",").map(Number);

(async () => {
  const s = await start();
  let bad = 0;
  try {
    for (const w of WIDTHS) {
      for (const kind of ["desk", "phone"]) {
        if (kind === "phone" && w !== 390 && w !== 348) continue;
        const ctx = await s.browser.newContext(kind === "phone" ? Object.assign({}, PHONE, { viewport: { width: w, height: 800 } }) : { viewport: { width: w, height: 800 }, deviceScaleFactor: 2 });
        await ctx.route(/^https:\/\//, (r) => r.fulfill({ status: 200, contentType: "text/plain", body: "away" }));
        const tab = await ctx.newPage();
        const errors = [];
        watch(tab, errors);
        await tab.goto(BOX + "/open/pr-view#pr=" + encodeURIComponent(PR));
        await until(async () => /Retry the payment webhook/.test(await tab.textContent("#app")), 10000);
        await until(async () => /IN PROGRESS/i.test(await tab.textContent("#app")), 5000);
        await sleep(500);
        const look = async (view) => {
          const r = await sideways(tab);
          const ok = !r.sideways.length && !r.past.length;
          if (!ok) bad++;
          console.log((ok ? "ok   " : "BAD  ") + kind + " " + w + " " + view + (ok ? "" : "  " + JSON.stringify(r)));
          await tab.screenshot({ path: path.join(OUT, kind + "-" + w + "-" + view + ".png") });
        };
        await look("overview");
        await tab.$eval("#comments-list", (e) => e.scrollIntoView());
        await sleep(100);
        await look("comments");
        for (const t of ["files", "commits"]) {
          await tab.click('.tab[data-tab="' + t + '"]');
          await sleep(300);
          await look(t);
        }
        // A table, its cells a row's height each: their lines meet.
        const ragged = await tab.$$eval(".commits tr", (rows) => rows.filter((r) => getComputedStyle(r).display === "table-row")
          .map((r) => new Set([...r.children].map((c) => Math.round(c.getBoundingClientRect().bottom))).size).filter((n) => n > 1).length);
        if (ragged) { bad++; console.log("BAD  " + kind + " " + w + " commit rows ragged: " + ragged); }
        await tab.click("#btn-back");
        await until(async () => !!(await tab.$(".pr-row")), 8000);
        await sleep(300);
        await look("list");
        const cu = await ctx.newPage();
        watch(cu, errors);
        await cu.goto(BOX + "/open/clickup-view#task=86abc123");
        await until(async () => /Payment webhook is dropped/.test(await cu.textContent("body")), 10000);
        await sleep(500);
        const r = await sideways(cu);
        const ok = !r.sideways.length && !r.past.length;
        if (!ok) bad++;
        console.log((ok ? "ok   " : "BAD  ") + kind + " " + w + " task" + (ok ? "" : "  " + JSON.stringify(r)));
        await cu.screenshot({ path: path.join(OUT, kind + "-" + w + "-task.png") });
        if (errors.length) { bad++; console.log("ERRORS " + JSON.stringify(errors)); }
        await ctx.close();
      }
    }
  } finally {
    await s.stop();
  }
  console.log(bad ? bad + " bad" : "all fit");
  process.exit(bad ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
