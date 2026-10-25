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
  { name: "my notes.html", title: "a name that is not one", where: "~/notes/", bytes: 10, mtime: 0 },
  { name: "../x.html", title: "climbs", where: "~/notes/", bytes: 10, mtime: 0 },
  { name: "x.HTML", title: "caps", where: "~/notes/", bytes: 10, mtime: 0 },
];
const SETS = { reports: { [ID + ".html"]: "<!doctype html><title>The viewer</title><h1 id=h>report</h1>" },
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

      // ---- The Docs tab ----
      const before = intents.length;
      await page.click('.tab[data-tab="docs"]');
      await sleep(150);
      check(W("the Docs tab asks Emacs for its list"), intents.slice(before).some((i) => i.op === "docs"));
      check(W("and says so meanwhile"), /Asking Emacs/.test(await page.textContent(".page.docs .sum")));
      await page.evaluate((d) => GM.renderDocs({ docs: d }), DOCS);
      await sleep(150);
      const docs = await links(page, ".page.docs .dtitle");
      check(W("a link a document, by its title; none for a name that is not one"),
        docs.length === 2 && docs.map((d) => d.text).join("|") === "Sidecar 記憶體瘦身|Herd 頁面門禁", docs.map((d) => d.text));
      check(W("each under the page's secret, a tab of its own with no opener"), docs.every((d) =>
        d.tag === "A" && /^\/0123456789abcdef0123456789abcdef\/\.docs\/[a-z-]+\.html$/.test(d.href) && d.target === "_blank" && /noopener/.test(d.rel)), docs);
      const meta = await page.textContent(".page.docs .doc .dmeta");
      check(W("with its file, where it is, its size and age"), /sidecar-memory\.html/.test(meta) && /ghostherd/.test(meta) &&
        /34 KB/.test(meta) && /1h ago/.test(meta), meta);
      check(W("and what a document can show"), /no script runs/.test(await page.textContent(".page.docs .docsays")));
      if (where === "phone") {
        const fit = await page.evaluate(() => ({ wide: document.documentElement.scrollWidth > innerWidth + 1,
          small: [...document.querySelectorAll(".page.docs .dtitle")].filter((a) => a.getBoundingClientRect().height < 32).length }));
        check(W("the Docs tab fits the phone, a finger's height a document"), !fit.wide && !fit.small, fit);
      }
      check(W("the tab is kept in the address"), /#docs$/.test(page.url()), page.url());
      await page.screenshot({ path: path.join(SHOTS, "docs-tab-" + where + ".png") });
      const p2 = ctx.waitForEvent("page");
      await tap('.page.docs a[href$="sidecar-memory.html"]');
      const doc = await p2;
      await doc.waitForLoadState("load").catch(() => {});
      check(W("pressed, the document in a tab of its own"), (await doc.textContent("#h")) === "doc");
      await doc.close();
      check(W("only copies' and documents' names were asked for"), fileLog.every((f) =>
        f === IMG.id + ".t.jpg" || f === "reports/" + ID + ".html" || f === "docs/sidecar-memory.html"), fileLog);
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
      const d = await links(page, ".page.docs .dtitle");
      check("xwidget: the Docs are buttons too", d.length === 2 && d.every((x) => x.tag === "BUTTON" && x.set === "docs"), d);
      await page.click('.page.docs [data-name="phone-access.html"]');
      await drain();
      check("xwidget: pressed, Emacs is asked to show it", said.some((i) => i.op === "doc-open" && i.set === "docs" && i.name === "phone-access.html"));
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
