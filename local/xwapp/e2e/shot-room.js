// Screenshots of a project's room, for review: 3, 4 and 5 columns at
// 1890x920, the table at 4, and the phone's room.  With LOCAL pointing at
// another checkout's local/ (master's, say) it shoots that one, so the two
// can be set side by side:
//   node shot-room.js [OUTDIR]          this checkout -> shots/room-*
//   LOCAL=/tmp/m/local node shot-room.js /tmp/before
// It also writes OUTDIR/room-geometry.json: each column's head, jump strip,
// conversation and footer heights, and the phone's head and footer boxes.
const { webkit } = require("playwright");
const fs = require("fs");
const path = require("path");
const { phonePage, sleep } = require("./herdlib");
const { openRoom } = require("./room-fixture");
const OUT = path.resolve(process.argv[2] || path.join(__dirname, "shots"));
const DESK = { viewport: { width: 1890, height: 920 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };

const heights = (page) => page.$$eval(".rcol", (cols) => cols.map((c) => {
  const h = (s) => { const e = c.querySelector(s); return e ? Math.round(e.getBoundingClientRect().height) : 0; };
  return { name: c.getAttribute("data-name"), width: Math.round(c.getBoundingClientRect().width),
    top: h(".rtop"), jump: h(".rjump"), body: h(".rbody"), foot: h(".rfoot"),
    bodyTop: Math.round(c.querySelector(".rbody").getBoundingClientRect().top) };
}));

(async () => {
  fs.mkdirSync(OUT, { recursive: true });
  const browser = await webkit.launch();
  const geo = {};
  try {
    for (const n of [3, 4, 5]) {
      const { ctx, page } = await phonePage(browser, { context: DESK });
      await openRoom(page, n, "columns", (s) => page.click(s), sleep);
      geo["desk" + n] = await heights(page);
      await page.screenshot({ path: path.join(OUT, "room-" + n + ".png") });
      if (n === 4) {
        // The lead's table in sight.
        await page.$eval('.rcol[data-name="lead-ops"] .rbody', (b) => {
          const t = b.querySelector(".msg-body table");
          if (t) b.scrollTop += t.getBoundingClientRect().top - b.getBoundingClientRect().top - 60;
        });
        await sleep(100);
        await page.screenshot({ path: path.join(OUT, "room-4-table.png") });
      }
      await ctx.close();
    }
    for (const view of ["columns", "chat"]) {
      const { ctx, page } = await phonePage(browser, { context: PHONE });
      await openRoom(page, 4, view, (s) => page.tap(s), sleep);
      if (view === "columns") {
        geo.phone = await page.$eval(".rcol", (c) => {
          const box = (e) => { const r = e.getBoundingClientRect(); return [e.className, Math.round(r.left), Math.round(r.top), Math.round(r.width), Math.round(r.height)]; };
          return { top: [...c.querySelectorAll(".rtop *")].filter((e) => e.getClientRects().length).map(box),
                   foot: [...c.querySelectorAll(".rfoot *")].filter((e) => e.getClientRects().length).map(box),
                   body: box(c.querySelector(".rbody")) };
        });
      }
      await page.screenshot({ path: path.join(OUT, "room-phone-" + view + ".png") });
      await ctx.close();
    }
  } finally { await browser.close(); }
  fs.writeFileSync(path.join(OUT, "room-geometry.json"), JSON.stringify(geo, null, 1));
  console.log("shots: " + OUT + "/room-{3,4,5,4-table,phone-columns,phone-chat}.png");
  for (const k of ["desk3", "desk4", "desk5"]) console.log(k, JSON.stringify(geo[k]));
})().catch((e) => { console.error(e); process.exit(1); });
