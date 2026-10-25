// Images an agent's reply names (ask 02aba3db): a thumbnail under the
// message wherever it is read -- a room's column and its chat, an ask's
// answer, the Inbox's report and its thread -- and pressed, the image
// whole over the page; Esc puts it away.  Only a copy's name is ever
// asked for: an image whose id or extension is not one is not drawn,
// and an agent's own <img> or ![](…) stays text.  Under the page's CSP,
// in a browser (by name under the secret) and as the xwidget has it
// (file:, its directory said on the hello).  Desk and phone.
const fs = require("fs");
const os = require("os");
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
const FIX = path.join(__dirname, "fixtures");
const FILES = { [ID + ".png"]: path.join(FIX, "reply-shot.png"), [ID + ".t.jpg"]: path.join(FIX, "reply-shot.t.jpg") };
const IMG = { id: ID, ext: "png", name: "after-light-5-tldr-viewer.png", w: 640, h: 400, thumb: true };
// None of these is a copy's name: never drawn, never asked for.
const BAD = [{ id: "../../etc/passwd", ext: "png", name: "x" }, { id: ID.toUpperCase(), ext: "png", name: "up" },
             { id: ID, ext: "svg", name: "svg" }, { id: ID + "0", ext: "png", name: "long" }, null, "x"];
const ts = new Date(Date.now() - 60e3).toISOString();
const SAID = "Done.  The screenshots are in /private/tmp/x/shots/after-light-5-tldr-viewer.png\n\n" +
  "<img src=\"https://evil.example/a.png\"> ![b](https://evil.example/b.png)";
const CONV = { name: RD, entries: [
  { role: "user", kind: "prompt", text: "Take a screenshot", ts },
  { role: "assistant", text: SAID, ts, images: [IMG].concat(BAD) },
  { role: "herd", kind: "ask", to: "qa-deploy-platform", ask: "1a2b3c4d", text: "Check it in dark mode", ts,
    answer: "Dark looks right: /tmp/dark.png", answerImages: [IMG] },
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
  msgs: [{ kind: "ask", from: "lead-deploy-platform", to: RD, at: now - 40000, head: "Build it", text: "Build it" },
         { kind: "answer", from: RD, to: "lead-deploy-platform", at: now - 20000, head: "Built", text: "Built: /tmp/v.png", auto: false, images: [IMG] }],
  waiting: [] };

async function loaded(page, sel) {
  return page.$$eval(sel, (imgs) => imgs.map((i) => ({ src: i.getAttribute("src"), ok: i.complete && i.naturalWidth > 0 })));
}

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const fileLog = [], intents = [], csp = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, files: FILES, fileLog, context: where === "desk" ? DESK : {} });
      page.on("console", (m) => { if (/Content Security Policy|Refused to load/i.test(m.text())) csp.push(m.text()); });
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

      // ---- A room's column: its reply's image, and its ask's answer's ----
      await page.evaluate((c) => GM.setConversation(c), CONV);
      await sleep(400);
      const col = '.rcol[data-name="' + RD + '"] .rbody';
      const thumbs = await loaded(page, col + " .eimg img");
      check(W("a thumbnail under the reply and under the answer, and none for what is not a copy"),
        thumbs.length === 2, thumbs);
      check(W("each the copy's thumbnail, by its name under the page's secret, loaded"),
        thumbs.every((t) => t.src === "/" + SECRET + "/.files/" + ID + ".t.jpg" && t.ok), thumbs);
      check(W("only the thumbnail was asked for"), fileLog.length > 0 && fileLog.every((f) => f === ID + ".t.jpg"), fileLog);
      const body = await page.$eval(col + " .ent.me .msg-body", (e) => e.innerHTML);
      check(W("the agent's own <img> and ![](…) stay text, not images"), !/<img/i.test(body), body);
      await page.screenshot({ path: path.join(SHOTS, "images-room-" + where + ".png") });
      check(W("the thumbnail is named for the image"),
        (await page.getAttribute(col + " .ent.me .eimg", "title")).startsWith("after-light-5-tldr-viewer.png · 640×400"));

      // ---- Pressed: the image whole ----
      await tap(col + " .ent.me .eimg");
      await sleep(300);
      const full = await loaded(page, ".imgview .imgscroll img");
      check(W("pressed, the image whole over the page"),
        full.length === 1 && full[0].src === "/" + SECRET + "/.files/" + ID + ".png" && full[0].ok, full);
      check(W("named, with Open for a tab of its own"),
        (await page.textContent(".imgview .imgname")) === IMG.name &&
        (await page.getAttribute(".imgview .imgopen", "href")) === "/" + SECRET + "/.files/" + ID + ".png");
      const fitted = await page.$eval(".imgview .imgscroll img", (i) => {
        const r = i.getBoundingClientRect(); return r.width <= innerWidth && r.height <= innerHeight; });
      check(W("fitted to the page"), fitted);
      await page.screenshot({ path: path.join(SHOTS, "images-whole-" + where + ".png") });
      await tap(".imgview .imgscroll img");
      await sleep(80);
      check(W("pressed again, at its own size"), await page.$eval(".imgview", (v) => v.classList.contains("actual")));
      await page.keyboard.press("Escape");
      await sleep(80);
      check(W("Esc puts it away"), !(await page.$(".imgview")));
      if (where === "desk") {
        // From the keyboard: the thumbnail, Enter, Esc -- and back on it.
        await page.focus(col + " .ent.me .eimg");
        await page.keyboard.press("Enter");
        await sleep(150);
        const opened = !!(await page.$(".imgview"));
        await page.keyboard.press("Escape");
        await sleep(80);
        check(W("from the keyboard, Enter shows it and Esc gives the focus back to its thumbnail"),
          opened && await page.evaluate(() => !!(document.activeElement && document.activeElement.classList.contains("eimg"))));
      }
      await tap(col + " .eanswer .eimg");
      await sleep(150);
      await tap(".imgview .imgclose");
      await sleep(80);
      check(W("✕ puts it away too"), !(await page.$(".imgview")));

      // ---- The chat: a turn's image and an answer's ----
      await tap('.room [data-act="a:room-view"][data-view="chat"]');
      await sleep(150);
      await page.evaluate((a) => GM.setChat({ project: a.dp, earlier: false, missing: [], absent: [], entries: [
        { who: "user", to: a.rd, kind: "prompt", ts: a.ts, text: "Take a screenshot" },
        { who: a.rd, kind: "turn", ts: a.ts, text: "Done: /tmp/x.png", images: [a.img] },
        { who: "lead-deploy-platform", to: "qa", kind: "ask", ask: "1a2b3c4d", ts: a.ts, text: "check", answer: "ok /tmp/d.png", answerImages: [a.img] },
      ] }), { dp: DP, rd: RD, ts, img: IMG });
      await sleep(300);
      const chat = await loaded(page, ".chat .cbody .eimg img");
      check(W("the chat: under the turn and under the answer"), chat.length === 2 && chat.every((t) => t.ok), chat);

      // ---- The Inbox: the report's images, and its thread's answer's ----
      await page.click('.tab[data-tab="inbox"]');
      await sleep(150);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 0 }), ITEMS);
      await sleep(100);
      await tap('[data-id="' + RID + '"]');
      await sleep(100);
      await page.evaluate((p) => {
        GM.inboxDetailData({ for: p.id, report: "Shipped the viewer.  /tmp/after.png", images: [p.img] });
        GM.inboxDetailData({ for: p.id, thread: p.thread });
      }, { id: RID, img: IMG, thread: THREAD });
      await sleep(300);
      const report = await loaded(page, ".ibwhy .eimg img");
      check(W("the Inbox: the report's image under its words"), report.length === 1 && report[0].ok, report);
      await page.screenshot({ path: path.join(SHOTS, "images-inbox-" + where + ".png"), fullPage: true });
      const thread = await loaded(page, ".ibthwrap .eimg img");
      check(W("and its thread's answer's, " + (where === "desk" ? "under the card" : "under the message")),
        thread.length === 1 && thread[0].ok, thread);
      check(W("not inside a button: a thread's card holds none"),
        await page.$$eval(".ibthwrap .eimg", (bs) => bs.every((b) => !b.parentElement.closest("button"))));
      await tap(".ibwhy .eimg");
      await sleep(200);
      check(W("pressed in the Inbox, the image whole"), (await loaded(page, ".imgview img")).every((t) => t.ok));
      // Over the page, its keys are not the page's: j does not move the Inbox.
      const sel = await page.evaluate(() => document.querySelector(".ibrow.on, .ibcard.open") ? 1 : 0);
      await page.keyboard.press("j");
      await page.keyboard.press("Escape");
      await sleep(80);
      check(W("the page's keys wait while it is shown"),
        !(await page.$(".imgview")) && (await page.evaluate(() => document.querySelector(".ibrow.on, .ibcard.open") ? 1 : 0)) === sel);

      check(W("nothing the page asked for broke its CSP"), csp.length === 0, csp);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }

    // ---- The xwidget: a page of file:, its images files ----
    {
      const dir = fs.mkdtempSync(path.join(os.tmpdir(), "herd-images-"));
      fs.copyFileSync(FILES[ID + ".png"], path.join(dir, ID + ".png"));
      fs.copyFileSync(FILES[ID + ".t.jpg"], path.join(dir, ID + ".t.jpg"));
      const ctx = await browser.newContext(DESK);
      const page = await ctx.newPage();
      const errors = [], csp = [];
      page.on("pageerror", (e) => errors.push(String(e)));
      page.on("console", (m) => { if (/Content Security Policy|Refused to load/i.test(m.text())) csp.push(m.text()); });
      const UI = path.resolve(__dirname, "../../ghostherd/ui/index.html");
      await page.goto("file://" + UI);
      await page.waitForFunction(() => window.GM && window.XW);
      check("xwidget: the page knows it is not in a browser", await page.evaluate(() => XW.browser === false));
      await page.click('.tab[data-tab="inbox"]');
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 0 }), ITEMS);
      await sleep(100);
      await page.click('[data-id="' + RID + '"]');
      await page.evaluate((p) => GM.inboxDetailData({ for: p.id, report: "Shipped.  /tmp/after.png", images: [p.img] }), { id: RID, img: IMG });
      await sleep(150);
      check("xwidget: before Emacs says where they are, a name and no image",
        (await page.$$(".ibwhy .eimg img")).length === 0 && (await page.textContent(".ibwhy .eimg .eimgname")) === IMG.name);
      // Emacs's answer to the hello, as `xwapp--greet' sends it.
      await page.evaluate((base) => XW.kit({ xwapp: "files", base }), "file://" + dir + "/");
      await sleep(400);
      const xw = await loaded(page, ".ibwhy .eimg img");
      check("xwidget: told, the image a file there, loaded under the page's CSP",
        xw.length === 1 && xw[0].src === "file://" + dir + "/" + ID + ".t.jpg" && xw[0].ok, xw);
      await page.click(".ibwhy .eimg");
      await sleep(300);
      const whole = await loaded(page, ".imgview img");
      check("xwidget: pressed, the image whole, and no Open (no browser to open it in)",
        whole.length === 1 && whole[0].ok && !(await page.$(".imgview .imgopen")), whole);
      // A base not of file: is not taken.
      await page.evaluate(() => XW.kit({ xwapp: "files", base: "https://evil.example/" }));
      check("xwidget: a base that is not a file: is ignored", await page.evaluate((id) => XW.file(id + ".png").startsWith("file://"), ID));
      check("xwidget: nothing broke its CSP", csp.length === 0, csp);
      check("xwidget: no page errors", errors.length === 0, errors);
      await ctx.close();
      fs.rmSync(dir, { recursive: true, force: true });
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
