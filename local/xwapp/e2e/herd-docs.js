// HTML reports an agent's reply names, and the Docs tab (ask ccf857d3):
// under the message wherever it is read -- a room's column and its chat,
// an ask's answer, the Inbox's report and its thread -- an Open link with
// the report's title, which opens the copy in a tab of its own by its
// id under the page's secret (the page asks Emacs nothing for it); the
// Docs tab's documents likewise.  Only a copy's name is ever made a link:
// a record whose id or extension is not one, or a doc whose name is not
// one, is not drawn.  In the xwidget (file:) there is no link to a file
// -- a document would run as one -- but a button that asks Emacs to show
// it.  Desk and phone.  The sandbox the document gets is
// herd-docs-sandbox.js's, against a real Emacs.
//
// The Docs tab's Reports (ask ab8e8838): the reports agents' replies
// named, as the sidecar keeps their copies -- each by its title (or
// file), who wrote it and in which project, when, its size -- opening as
// the Open link does; one whose copy is gone listed, marked, with nothing
// to open; older ones a page at a time, an answer to a list since
// replaced dropped.  One list with the documents (ask 825339b7), newest
// first, each row two lines -- its title and when; its file, its size,
// and, all shown, where it is -- under a filter (a title or a file name)
// and a chip per source with its count: all, each docs directory, the
// reports once there are.
const fs = require("fs");
const path = require("path");
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const SHOTS = path.join(__dirname, "shots");
fs.mkdirSync(SHOTS, { recursive: true });
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1400, height: 900 }, deviceScaleFactor: 1, isMobile: false, hasTouch: false };
const SECRET = "0123456789abcdef0123456789abcdef";
const DP = "~/src/deploy-platform/";
const RD = "claude-deploy-platform";
const ID = "a1b2c3d4e5f60718293a4b5c6d7e8f90";
const IMG = { id: "b1b2c3d4e5f60718293a4b5c6d7e8f90", ext: "png", name: "after.png", w: 640, h: 400, thumb: true };
const REP = { id: ID, ext: "html", name: "viewer-report.html", title: "The viewer, reviewed: what changed & why" };
// None of these is a report's copy: never a link.
const BAD = [{ id: ID.toUpperCase(), ext: "html", name: "up" }, { id: "../../etc/passwd", ext: "html", name: "x" },
             { id: ID, ext: "htm", name: "htm" }, { id: ID + "0", ext: "html", name: "long" },
             { id: ID, ext: "HTML", name: "caps" }, null, "x"];
const DOCS = [
  { name: "sidecar-memory.html", title: "Sidecar 記憶體瘦身", where: "ghostherd", bytes: 34700, mtime: Date.now() / 1000 - 3600 },
  { name: "phone-access.html", title: "Herd 頁面門禁", where: "ghostherd", bytes: 23100, mtime: Date.now() / 1000 - 86400 },
  { name: "notes.html", title: "Notes from the desk", where: "~/notes/", bytes: 2048, mtime: Date.now() / 1000 - 5 * 86400 },
  { name: "my notes.html", title: "a name that is not one", where: "~/notes/", bytes: 10, mtime: 0 },
  { name: "../x.html", title: "climbs", where: "~/notes/", bytes: 10, mtime: 0 },
  { name: "x.HTML", title: "caps", where: "~/notes/", bytes: 10, mtime: 0 },
];
// The Docs tab's Reports, as Emacs gives them from the sidecar: the
// newest page, and the page older than its last.
const R1 = "c1c2c3d4e5f60718293a4b5c6d7e8f90", R2 = "d1d2c3d4e5f60718293a4b5c6d7e8f90";
const R3 = "e1e2c3d4e5f60718293a4b5c6d7e8f90", R4 = "f1f2c3d4e5f60718293a4b5c6d7e8f90";
const R5 = "a1a2c3d4e5f60718293a4b5c6d7e8f90", R6 = "b1b2c3d4e5f60718293a4b5c6d7e8f90";
const SEC = Date.now() / 1000;
const REPORTS1 = [
  // Rewritten and named again: its older copies under it.
  { id: R1, title: "Viewer review: what changed & why", name: "viewer-review.html", agent: RD, project: "deploy-platform",
    at: SEC - 120, bytes: 48200, gone: false,
    older: [{ id: R5, title: "Viewer review: draft 2", name: "viewer-review.html", agent: RD, project: "deploy-platform",
              at: SEC - 1800, bytes: 40100, gone: false },
            { id: R6, title: "Viewer review: draft 1", name: "viewer-review.html", agent: RD, project: "deploy-platform",
              at: SEC - 7200, bytes: 30100, gone: true },
            { id: "../x", title: "climbs", name: "x", at: SEC, bytes: 1, gone: false }] },
  { id: R2, title: "", name: "perf.html", agent: "", project: "", at: SEC - 7200, bytes: 900, gone: false },
  { id: R3, title: "A report since taken away", name: "gone.html", agent: "qa-deploy-platform", project: "deploy-platform",
    at: SEC - 3 * 86400, bytes: 1000, gone: true },
  // Not a copy's id: never drawn.
  { id: "../../etc/passwd", title: "climbs", name: "x", agent: "x", project: "", at: SEC, bytes: 1, gone: false },
  { id: R1.toUpperCase(), title: "caps", name: "x", agent: "x", project: "", at: SEC, bytes: 1, gone: false },
  { id: R2, title: "no time", name: "x", agent: "x", project: "", at: "yesterday", bytes: 1, gone: false },
];
const REPORTS2 = [{ id: R4, title: "An older report", name: "old.html", agent: "lead-deploy-platform", project: "deploy-platform",
  at: SEC - 10 * 86400, bytes: 3000, gone: false }];
const SETS = { reports: { [ID + ".html"]: "<!doctype html><title>The viewer</title><h1 id=h>report</h1>",
                          [R1 + ".html"]: "<!doctype html><title>Viewer review</title><h1 id=h>listed report</h1>" },
               docs: { "sidecar-memory.html": "<!doctype html><title>Sidecar</title><h1 id=h>doc</h1>" } };
const ts = new Date(Date.now() - 60e3).toISOString();
const SAID = "Done.  The write-up is in /private/tmp/x/viewer-report.html";
const CONV = { name: RD, entries: [
  { role: "user", kind: "prompt", text: "Write it up", ts },
  { role: "assistant", text: SAID, ts, images: [IMG, REP].concat(BAD) },
  { role: "herd", kind: "ask", to: "qa-deploy-platform", ask: "1a2b3c4d", text: "Review it", ts,
    answer: "Reviewed: /tmp/review.html", answerImages: [REP] },
] };
const now = Date.now();
const RID = "report:lead-deploy-platform:" + now;
const ITEMS = [{ id: RID, group: "fyi", kind: "report", tag: "Report", tone: "green", agent: "lead-deploy-platform", cli: "claude",
  project: DP, at: now - 60000, read: false, override: null, whyLabel: "It reported", title: "Shipped the viewer",
  body: "Shipped the viewer.", preview: "Shipped", code: "", note: "" }];
const THREAD = { id: "t1", title: "the viewer", status: "answered", participants: ["lead-deploy-platform", RD], projects: ["deploy-platform"],
  cross: false, count: 2, open: 0, started: now - 60000, at: now - 10000, rootFrom: "lead-deploy-platform", rootTo: RD,
  lanes: [{ name: "lead-deploy-platform", kind: "claude", project: "deploy-platform", role: "lead", worktree: false },
          { name: RD, kind: "claude", project: "deploy-platform", role: "", worktree: false }],
  msgs: [{ kind: "ask", from: "lead-deploy-platform", to: RD, at: now - 40000, head: "Write it up", text: "Write it up" },
         { kind: "answer", from: RD, to: "lead-deploy-platform", at: now - 20000, head: "Written", text: "Written: /tmp/r.html", auto: false, images: [REP] }],
  waiting: [] };

const links = (page, sel) => page.$$eval(sel, (as) => as.map((a) => ({
  tag: a.tagName, href: a.getAttribute("href"), target: a.getAttribute("target"), rel: a.getAttribute("rel"),
  text: a.textContent, set: a.getAttribute("data-set"), name: a.getAttribute("data-name"), h: a.getBoundingClientRect().height })));
// The Docs tab's rows as shown: what each is, its two lines, and whether
// each is one line, its time at the right, all of it in the page.
const rowsOf = (page) => page.$$eval("#docs-list > .doc", (ds) => ds.map((d) => {
  const l1 = d.querySelector(".dl1"), l2 = d.querySelector(".dmeta"), t = d.querySelector(".dtitle"), w = d.querySelector(".dwhen");
  const oneLine = (e) => { const rs = [...e.children].map((c) => c.getBoundingClientRect()); return rs.every((r) => r.top < rs[0].bottom && r.bottom > rs[0].top); };
  const open = [...l1.querySelectorAll(":scope > a, :scope > button")].map((a) => ({ tag: a.tagName, href: a.getAttribute("href"),
    target: a.getAttribute("target"), rel: a.getAttribute("rel"), text: a.textContent }));
  return { key: d.getAttribute("data-report") || (open[0] ? (open[0].href || "").split("/").pop() : ""), report: d.classList.contains("report"),
    gone: d.classList.contains("gone"), title: t && t.textContent, when: w && w.textContent, open,
    meta: [...l2.children].map((c) => c.textContent), twoLines: oneLine(l1) && oneLine(l2),
    whenRight: !!w && Math.abs(w.getBoundingClientRect().right - l1.getBoundingClientRect().right) < 1.5,
    inside: d.getBoundingClientRect().right <= innerWidth + 1 };
}));
// Its source chips, as each says itself; * the one shown.
const chipsOf = (page) => page.$$eval(".dchips .chip", (cs) => cs.map((c) => c.textContent.replace(/\s+/g, " ").trim() +
  (c.classList.contains("on") ? "*" : "")));
const isLink = (l) => l.tag === "A" && l.href === "/" + SECRET + "/.reports/" + ID + ".html" && l.target === "_blank" &&
  /noopener/.test(l.rel) && /noreferrer/.test(l.rel);

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [], fileLog = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, fileLog, files: {}, sets: SETS, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(100);
      await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      if (await page.$(".room .chat")) {
        await tap('.room [data-act="a:room-view"][data-view="columns"]');
        await sleep(150);
      }

      // ---- A room's column: the reply's report, and its ask's answer's ----
      await page.evaluate((c) => GM.setConversation(c), CONV);
      await sleep(300);
      const col = '.rcol[data-name="' + RD + '"] .rbody';
      const reps = await links(page, col + " .ereport");
      check(W("an Open link under the reply and under the answer, and none for what is not a copy"), reps.length === 2, reps);
      check(W("each the copy, by its id under the page's secret, in a tab of its own with no opener"), reps.every(isLink), reps);
      check(W("named by its title and its file"), /^Open/.test(reps[0].text) && reps[0].text.includes(REP.title) &&
        reps[0].text.includes(REP.name), reps[0].text);
      check(W("the reply's image is still a thumbnail beside it"), (await page.$$(col + " .ent.me .eimg")).length === 1);
      check(W("it says what a report can show"), /only what the page holds/.test(await page.getAttribute(col + " .ereport", "title")));
      if (where === "phone") check(W("a finger's height"), reps.every((l) => l.h >= 32), reps.map((l) => l.h));
      await page.screenshot({ path: path.join(SHOTS, "docs-report-room-" + where + ".png") });

      // Pressed: the copy in a tab of its own; Emacs is asked nothing.
      const asked = intents.length;
      const popped = ctx.waitForEvent("page");
      await tap(col + " .ent.me .ereport");
      const tab = await popped;
      await tab.waitForLoadState("load").catch(() => {});
      check(W("pressed, the report in a tab of its own"),
        tab.url() === "http://box.tail0.ts.net/" + SECRET + "/.reports/" + ID + ".html" && (await tab.textContent("#h")) === "report", tab.url());
      check(W("and the page asked Emacs nothing for it"), !intents.slice(asked).some((i) => /doc/.test(i.op)), intents.slice(asked));
      await tab.close();

      // ---- The chat ----
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(150);
      await page.evaluate((a) => GM.setChat({ project: a.dp, earlier: false, missing: [], absent: [], entries: [
        { who: "user", to: a.rd, kind: "prompt", ts: a.ts, text: "Write it up" },
        { who: a.rd, kind: "turn", ts: a.ts, text: "Done: /tmp/r.html", images: [a.rep] },
        { who: "lead-deploy-platform", to: "qa", kind: "ask", ask: "1a2b3c4d", ts: a.ts, text: "check", answer: "ok /tmp/q.html", answerImages: [a.rep] },
      ] }), { dp: DP, rd: RD, ts, rep: REP });
      await sleep(300);
      const chat = await links(page, ".chat .cbody .ereport");
      check(W("the chat: under the turn and under the answer"), chat.length === 2 && chat.every(isLink), chat);

      // ---- The Inbox: the report's, and its thread's answer's ----
      await page.click('.tab[data-tab="inbox"]');
      await sleep(150);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 0 }), ITEMS);
      await sleep(100);
      await tap('[data-id="' + RID + '"]');
      await sleep(100);
      await page.evaluate((p) => {
        GM.inboxDetailData({ for: p.id, report: "Shipped the viewer.  /tmp/r.html", images: [p.rep] });
        GM.inboxDetailData({ for: p.id, thread: p.thread });
      }, { id: RID, rep: REP, thread: THREAD });
      await sleep(300);
      check(W("the Inbox: the report's under its words"), (await links(page, ".ibwhy .ereport")).every(isLink) &&
        (await page.$$(".ibwhy .ereport")).length === 1);
      const th = await links(page, ".ibthwrap .ereport");
      check(W("and its thread's answer's"), th.length === 1 && th.every(isLink), th);
      check(W("not inside a button: a thread's card holds none"),
        await page.$$eval(".ibthwrap .ereport", (bs) => bs.every((b) => !b.parentElement.closest("button"))));
      await page.screenshot({ path: path.join(SHOTS, "docs-report-inbox-" + where + ".png"), fullPage: true });

      // ---- The Docs tab: one list, newest first ----
      const before = intents.length;
      await page.click('.tab[data-tab="docs"]');
      await sleep(150);
      check(W("the Docs tab asks Emacs for its list"), intents.slice(before).some((i) => i.op === "docs"));
      const asking = await page.textContent(".page.docs .sum");
      check(W("and says so meanwhile, and that the herd is asked for the reports"),
        /Asking Emacs/.test(asking) && /Asking the herd/.test(asking), asking);
      await page.evaluate((d) => GM.renderDocs({ docs: d }), DOCS);
      await sleep(150);
      const docs = await links(page, "#docs-list .doc:not(.report) .dtitle");
      check(W("a link a document, by its title; none for a name that is not one"),
        docs.length === 3 && docs.map((d) => d.text).join("|") === "Sidecar 記憶體瘦身|Herd 頁面門禁|Notes from the desk", docs.map((d) => d.text));
      check(W("each under the page's secret, a tab of its own with no opener"), docs.every((d) =>
        d.tag === "A" && /^\/0123456789abcdef0123456789abcdef\/\.docs\/[a-z-]+\.html$/.test(d.href) && d.target === "_blank" && /noopener/.test(d.rel)), docs);
      let rows = await rowsOf(page);
      check(W("each row two lines: its title with when, on the right; its file, its size and where it is"),
        rows.length === 3 && rows[0].title === "Sidecar 記憶體瘦身" && rows[0].when === "1h ago" &&
        rows[0].meta.join("|") === "sidecar-memory.html|34 KB|ghostherd" && rows[2].meta.join("|") === "notes.html|2 KB|~/notes" &&
        rows.every((r) => r.twoLines && r.whenRight && r.inside), rows);
      check(W("no Reports chip before there is a report; a chip for each directory, with its count"),
        (await chipsOf(page)).join() === "All 3*,ghostherd 2,~/notes 1", await chipsOf(page));
      check(W("no prose under the list: what a document can show is its link's title"),
        !(await page.$(".docsays")) && /no script runs/.test(await page.getAttribute("#docs-list .dtitle", "title")));
      if (where === "phone") {
        const fit = await page.evaluate(() => ({ wide: document.documentElement.scrollWidth > innerWidth + 1,
          small: [...document.querySelectorAll(".page.docs .dtitle, .page.docs .chip")].filter((a) => a.getBoundingClientRect().height < 32).length }));
        check(W("the Docs tab fits the phone, a finger's height a document and a chip"), !fit.wide && !fit.small, fit);
      }
      check(W("the tab is kept in the address"), /#docs$/.test(page.url()), page.url());
      const p2 = ctx.waitForEvent("page");
      await tap('.page.docs a[href$="sidecar-memory.html"]');
      const doc = await p2;
      await doc.waitForLoadState("load").catch(() => {});
      check(W("pressed, the document in a tab of its own"), (await doc.textContent("#h")) === "doc");
      await doc.close();

      // ---- The reports, in the same list ----
      await page.evaluate((r) => GM.renderReports({ reports: r, more: true, after: false }), REPORTS1);
      await sleep(150);
      rows = await rowsOf(page);
      check(W("the reports among the documents, all newest first; none whose id is not a copy's, or with no time"),
        rows.map((r) => r.key).join() === [R1, "sidecar-memory.html", R2, "phone-access.html", R3, "notes.html"].join(), rows.map((r) => r.key));
      check(W("a Reports chip once there are, its count -- more to ask for, a +"),
        (await chipsOf(page)).join() === "All 6+*,ghostherd 2,~/notes 1,Reports 3+", await chipsOf(page));
      const r1 = rows[0] || { open: [] };
      check(W("a report opens as its Open link does: its copy by id under the secret, a tab of its own, no opener"),
        r1.open.length === 1 && r1.open[0].tag === "A" && r1.open[0].href === "/" + SECRET + "/.reports/" + R1 + ".html" &&
        r1.open[0].target === "_blank" && /noopener/.test(r1.open[0].rel) && /noreferrer/.test(r1.open[0].rel), r1.open);
      check(W("marked a report: by its title and when; who wrote it and where, its file and size"),
        r1.report && r1.title === "Viewer review: what changed & why" && r1.when === "2m ago" &&
        r1.meta.slice(0, 3).join("|") === "from " + RD + " · deploy-platform|viewer-review.html|47 KB" && r1.twoLines && r1.whenRight, r1);
      check(W("one with no title by its file; its writer unknown, said so"),
        rows[2] && rows[2].open[0] && rows[2].open[0].text === "perf.html" && rows[2].meta[0] === "writer unknown", rows[2]);
      check(W("one whose copy is gone: listed, marked, nothing to open"),
        rows[4] && rows[4].gone && rows[4].open.length === 0 && rows[4].meta.includes("its copy is gone") &&
        rows[4].title === "A report since taken away" && rows[4].when === "3d ago" && rows[4].twoLines, rows[4]);
      if (where === "desk") {
        const one = await page.evaluate(() => {
          const ls = [...document.querySelectorAll("#docs-list > .doc")].map((d) => Math.round(d.getBoundingClientRect().left));
          return { lefts: [...new Set(ls)], page: document.querySelector(".page.docs").getBoundingClientRect().width,
            wide: document.documentElement.scrollWidth > innerWidth + 1 };
        });
        check(W("on the desk: one column, as wide as the other tabs' pages"), one.lefts.length === 1 && one.page <= 1000 && !one.wide, one);
      }
      await page.screenshot({ path: path.join(SHOTS, "docs-tab-" + where + ".png"), fullPage: true });

      // ---- A source: a directory's documents, or the reports ----
      await tap('.dchips [data-act="docs:src"][data-src="d:ghostherd"]');
      await sleep(100);
      rows = await rowsOf(page);
      check(W("a directory's chip: its documents alone, where they are not said again"),
        rows.map((r) => r.key).join() === "sidecar-memory.html,phone-access.html" && rows.every((r) => r.meta.length === 2) &&
        (await chipsOf(page)).join() === "All 6+,ghostherd 2*,~/notes 1,Reports 3+" && !(await page.$('[data-act="reports:more"]')), rows);
      await tap('.dchips [data-act="docs:src"][data-src="reports"]');
      await sleep(100);
      const reportRows = () => page.$$eval("#docs-list > .doc", (ds) => ds.map((d) => d.getAttribute("data-report")));
      check(W("the Reports chip: the reports alone"), (await reportRows()).join() === [R1, R2, R3].join(), await reportRows());
      // A report rewritten and named again: one row, its older copies folded.
      const folded = () => page.evaluate((R1) => {
        const d = document.querySelector('#docs-list .doc[data-report="' + R1 + '"]');
        const b = d && d.querySelector('[data-act="reports:older"]');
        return { button: b && b.textContent, expanded: b && b.getAttribute("aria-expanded"), h: b && b.getBoundingClientRect().height,
          older: [...d.querySelectorAll(".dolders .doc.older")].map((o) => ({ id: o.getAttribute("data-report"),
            gone: o.classList.contains("gone"), link: (o.querySelector(".dl1 > a") || {}).href || "", text: o.textContent,
            more: !!o.querySelector('[data-act="reports:older"]') })),
          rows: document.querySelectorAll("#docs-list > .doc").length };
      }, R1);
      let f = await folded();
      check(W("a rewritten report: one row, \"2 older\" under it, folded"),
        f.button === "2 older" && f.expanded === "false" && f.older.length === 0 && f.rows === 3, f);
      await tap('#docs-list [data-act="reports:older"][data-report="' + R1 + '"]');
      await sleep(100);
      f = await folded();
      check(W("tapped: its older copies, newest first, each opening its own copy; one gone, nothing to open"),
        f.expanded === "true" && f.older.map((o) => o.id).join() === [R5, R6].join() &&
        /\/.reports\/a1a2c3d4e5f60718293a4b5c6d7e8f90\.html$/.test(f.older[0].link) && /draft 2/.test(f.older[0].text) &&
        f.older[1].gone && f.older[1].link === "" && !f.older[0].more && f.rows === 3, f);
      if (where === "phone") check(W("its button a finger's height"), f.h >= 32, f.h);
      await page.screenshot({ path: path.join(SHOTS, "docs-reports-older-" + where + ".png"), fullPage: true });
      await page.evaluate((r) => GM.renderReports({ reports: r, more: true, after: false }), REPORTS1);
      await sleep(100);
      check(W("it stays open as the list is drawn again"), (await folded()).older.length === 2);
      await tap('#docs-list [data-act="reports:older"][data-report="' + R1 + '"]');
      await sleep(100);
      f = await folded();
      check(W("tapped again: folded"), f.expanded === "false" && f.older.length === 0, f);
      if (where === "phone") {
        const fit = await page.evaluate(() => ({ wide: document.documentElement.scrollWidth > innerWidth + 1,
          small: [...document.querySelectorAll("#docs-list a.dtitle, #docs-list [data-act]")]
            .filter((a) => a.getBoundingClientRect().height < 32).length }));
        check(W("on the phone: the reports fit, a finger's height each"), !fit.wide && !fit.small, fit);
      }
      await page.screenshot({ path: path.join(SHOTS, "docs-reports-" + where + ".png"), fullPage: true });
      // Older ones: asked from the last shown.
      const was = intents.length;
      await tap('#docs-list [data-act="reports:more"]');
      await sleep(150);
      const more = intents.slice(was).filter((i) => i.op === "reports-more");
      check(W("older reports are asked from the last shown"), more.length === 1 && more[0].id === R3 && more[0].at === REPORTS1[2].at, more);
      check(W("and the page says it is asking"), /Asking/.test(await page.textContent('#docs-list [data-act="reports:more"]')) &&
        await page.$eval('#docs-list [data-act="reports:more"]', (b) => b.disabled));
      // An answer to a list since replaced: dropped.
      await page.evaluate((r) => GM.renderReports({ reports: r, more: false, after: "0".repeat(32) }), REPORTS2);
      await sleep(100);
      check(W("an answer for another list is dropped"), (await reportRows()).length === 3);
      await page.evaluate((a) => GM.renderReports({ reports: a.r, more: false, after: a.after }), { r: REPORTS2, after: R3 });
      await sleep(150);
      const ids = await reportRows();
      check(W("the older page added after the last; nothing more to ask"), ids.join() === [R1, R2, R3, R4].join() &&
        !(await page.$('#docs-list [data-act="reports:more"]')) && (await chipsOf(page)).includes("Reports 4*"), ids);
      // Pressed: the copy, by its id; Emacs is asked nothing for it.
      const asked2 = intents.length;
      const p3 = ctx.waitForEvent("page");
      await tap('#docs-list a[href$="/.reports/' + R1 + '.html"]');
      const rep = await p3;
      await rep.waitForLoadState("load").catch(() => {});
      check(W("pressed, the report in a tab of its own"),
        rep.url() === "http://box.tail0.ts.net/" + SECRET + "/.reports/" + R1 + ".html" && (await rep.textContent("#h")) === "listed report", rep.url());
      check(W("and Emacs was asked nothing for it"), !intents.slice(asked2).some((i) => /doc|report/.test(i.op)), intents.slice(asked2));
      await rep.close();

      // ---- The filter: a title or a file name ----
      await tap('.dchips [data-act="docs:src"][data-src=""]');
      await sleep(100);
      await page.click("#docs-filter");
      await page.keyboard.type("viewer");
      await sleep(150);
      const typed = await page.evaluate(() => ({ focus: document.activeElement && document.activeElement.id,
        value: document.getElementById("docs-filter").value }));
      rows = await rowsOf(page);
      check(W("typed: what its title holds, the rest away; the filter kept, the caret in it"),
        rows.map((r) => r.key).join() === R1 && typed.focus === "docs-filter" && typed.value === "viewer", { rows: rows.map((r) => r.key), typed });
      check(W("each chip counts what matches"), (await chipsOf(page)).join() === "All 1*,ghostherd 0,~/notes 0,Reports 1", await chipsOf(page));
      await page.fill("#docs-filter", "PHONE-ACCESS");
      await sleep(100);
      rows = await rowsOf(page);
      check(W("or its file's name, whatever the case"), rows.map((r) => r.key).join() === "phone-access.html", rows.map((r) => r.key));
      await page.fill("#docs-filter", "nothing like it");
      await sleep(100);
      check(W("nothing matches: said so"), /Nothing here matches “nothing like it”/.test(await page.textContent("#docs-list")) &&
        (await rowsOf(page)).length === 0);
      await page.fill("#docs-filter", "e");
      await tap('.dchips [data-act="docs:src"][data-src="d:~/notes/"]');
      await page.evaluate((d) => GM.renderDocs({ docs: d }), DOCS);
      await sleep(150);
      check(W("the filter and the chip kept as the list comes again"),
        (await page.inputValue("#docs-filter")) === "e" && (await rowsOf(page)).map((r) => r.key).join() === "notes.html" &&
        (await chipsOf(page)).includes("~/notes 1*"), await chipsOf(page));
      await page.fill("#docs-filter", "");
      await tap('.dchips [data-act="docs:src"][data-src=""]');
      await sleep(100);

      // The herd away: said so, the list as it was.
      await page.evaluate(() => GM.renderReports({ error: "herd_reports: no answer in 10s", after: false }));
      await sleep(100);
      check(W("the herd away: said so, the list kept"), /could not be asked for the reports: herd_reports: no answer/.test(
        await page.textContent("#docs-list .derror")) && (await reportRows()).filter(Boolean).length === 4);
      if (where === "desk") {
        const was3 = intents.length;
        await page.click("#btn-refresh");
        await sleep(150);
        check(W("Refresh lists the Docs tab again"), intents.slice(was3).some((i) => i.op === "docs"), intents.slice(was3));
      }
      check(W("only copies' and documents' names were asked for"), fileLog.every((f) =>
        f === IMG.id + ".t.jpg" || f === "reports/" + ID + ".html" || f === "reports/" + R1 + ".html" ||
        f === "docs/sidecar-memory.html"), fileLog);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }

    // ---- Coming back to #docs ----
    {
      const intents = [];
      const { ctx, page } = await phonePage(browser, { intents, hash: "#docs", context: DESK });
      await sleep(300);
      check("a page loaded at #docs opens the Docs tab and asks for its list",
        (await page.$(".tab.on[data-tab=docs]")) && intents.some((i) => i.op === "docs"));
      await ctx.close();
    }

    // ---- The xwidget: no link to a file; Emacs shows it ----
    {
      const ctx = await browser.newContext(DESK);
      const page = await ctx.newPage();
      const errors = [];
      page.on("pageerror", (e) => errors.push(String(e)));
      const UI = path.resolve(__dirname, "../../ghostherd/ui/index.html");
      await page.goto("file://" + UI);
      await page.waitForFunction(() => window.GM && window.XW);
      // Emacs reads each intent off the title and puts it back.
      const said = [];
      const drain = async () => {
        for (let i = 0; i < 40; i++) {
          const t = await page.title();
          if (t.startsWith("ghmem:")) {
            said.push(JSON.parse(t.slice(6)));
            await page.evaluate(() => { document.title = "Herd"; });
          }
          await sleep(60);
        }
      };
      await drain();
      check("xwidget: a document set has no file: URL", await page.evaluate((id) =>
        XW.file(id + ".html", "reports") === "" && XW.file("x.html", "docs") === "", ID));
      await page.click('.tab[data-tab="inbox"]');
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 0 }), ITEMS);
      await sleep(100);
      await page.click('[data-id="' + RID + '"]');
      await page.evaluate((p) => GM.inboxDetailData({ for: p.id, report: "Shipped.  /tmp/r.html", images: [p.rep] }), { id: RID, rep: REP });
      await sleep(150);
      const b = await links(page, ".ibwhy .ereport");
      check("xwidget: the report's Open is a button, not a link", b.length === 1 && b[0].tag === "BUTTON" && !b[0].href &&
        b[0].set === "reports" && b[0].name === ID + ".html", b);
      await page.click(".ibwhy .ereport");
      await drain();
      check("xwidget: pressed, it asks Emacs to show it", said.some((i) => i.op === "doc-open" && i.set === "reports" && i.name === ID + ".html"),
        said.map((i) => i.op));
      await page.click('.tab[data-tab="docs"]');
      await page.evaluate((d) => GM.renderDocs({ docs: d }), DOCS);
      await sleep(100);
      const d = await links(page, "#docs-list .doc:not(.report) .dtitle");
      check("xwidget: the Docs are buttons too", d.length === 3 && d.every((x) => x.tag === "BUTTON" && x.set === "docs"), d);
      await page.click('.page.docs [data-name="phone-access.html"]');
      await drain();
      check("xwidget: pressed, Emacs is asked to show it", said.some((i) => i.op === "doc-open" && i.set === "docs" && i.name === "phone-access.html"));
      await page.evaluate((r) => GM.renderReports({ reports: r, more: false, after: false }), REPORTS1);
      await sleep(100);
      const rb = await links(page, "#docs-list .doc.report .dtitle");
      check("xwidget: the Reports are buttons too, and the gone one none", rb.length === 3 &&
        rb.slice(0, 2).every((x) => x.tag === "BUTTON" && x.set === "reports") && rb[2].tag === "SPAN" &&
        rb[0].name === R1 + ".html", rb);
      await page.click('#docs-list [data-name="' + R1 + '.html"]');
      await drain();
      check("xwidget: a report pressed, Emacs is asked to show it as its Open does",
        said.some((i) => i.op === "doc-open" && i.set === "reports" && i.name === R1 + ".html"));
      check("xwidget: nowhere a link to a document", (await page.$$('a[href*=".reports/"], a[href*=".docs/"]')).length === 0);
      check("xwidget: no page errors", errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
