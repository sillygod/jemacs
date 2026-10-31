// ⌘V in Emacs's xwidget, for every xwapp page (ask fc2f4594).  WebKit
// under Emacs never makes ⌘V a paste: the kit asks Emacs for its
// clipboard, and puts the text in at the caret as typing would -- the
// page's own input events, its undo.  Here the pages are what the
// xwidget loads, files, and Emacs is played as xwapp.el plays it: it
// reads their titles (titlelib.js) and answers the kit's ask with the
// clipboard.  pr-view and clickup-view, which could not paste before;
// the herd page's image, from a clipboard with no text, as before; and
// in a browser ⌘V is the browser's own, the kit asking nothing.  The
// clipboard's text is made up.
const path = require("path");
const { webkit } = require("playwright");
const { readTitles } = require("./titlelib");
const { phonePage, HERD, sleep } = require("./herdlib");
const LOCAL = process.env.LOCAL || path.resolve(__dirname, "../..");
const file = (pkg) => "file://" + path.join(LOCAL, pkg, "ui", "index.html");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const CLIP = "pasted «text» 你好 😀\nline two";

// Emacs at the desk, as xwapp.el is: it reads the page's titles and
// answers the kit's ask for the clipboard with what `clip' holds then.
function deskEmacs(page, prefix, idle) {
  const got = [];
  const self = { got, clip: CLIP, asked: 0 };
  let stop = false, seen = 0;
  const reader = readTitles(page, prefix, () => stop, { ms: 300000, got, idle });
  const answering = (async () => {
    while (!stop) {
      while (seen < got.length) {
        const it = got[seen++];
        if (it.op !== "xwapp-paste") continue;
        self.asked++;
        await page.evaluate((t) => XW.kit({ xwapp: "paste", text: t }), self.clip).catch(() => {});
      }
      await sleep(10);
    }
  })();
  self.stop = async () => { stop = true; await reader; await answering; };
  return self;
}

// What the page heard of a field: its input events, and the kit's xw-paste.
const listen = (page) => page.evaluate(() => {
  window.HEARD = [];
  document.addEventListener("input", (e) => HEARD.push("input:" + (e.inputType || "")), true);
  document.addEventListener("xw-paste", (e) => HEARD.push("xw-paste:" + JSON.stringify(e.detail.text)), true);
});
const heard = (page) => page.evaluate(() => HEARD.splice(0));
const value = (page, sel) => page.$eval(sel, (e) => e.value);
const until = async (pred, ms = 5000) => {
  const end = Date.now() + ms;
  while (Date.now() < end) { if (await pred()) return true; await sleep(20); }
  return false;
};

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- pr-view: a pull request's description and title ----
    {
      const ctx = await browser.newContext(DESK);
      const page = await ctx.newPage();
      const errors = [];
      page.on("pageerror", (e) => errors.push(String(e)));
      await page.goto(file("pr-view"));
      await page.waitForFunction(() => window.BB && window.XW);
      const emacs = deskEmacs(page, "prview:", "PR View");
      await page.evaluate(() => BB.renderCreateForm({ branches: ["main", "fix/retry"], source: "fix/retry", destination: "main" }));
      await listen(page);
      const DESC = 'textarea[name="description"]';
      await page.click(DESC);
      await page.keyboard.type("Before: ");
      await heard(page);
      await page.keyboard.press("Meta+v");
      const pasted = await until(async () => (await value(page, DESC)) === "Before: " + CLIP);
      check("pr-view: ⌘V in its description puts Emacs's clipboard in at the caret",
        pasted && emacs.asked === 1, { value: await value(page, DESC), asked: emacs.asked });
      const h = await heard(page);
      check("as typing would: the field hears xw-paste, then its own input event",
        h[0] === "xw-paste:" + JSON.stringify(CLIP) && h.includes("input:insertText"), h);
      // On the page's undo stack, as typing is: undone, and done again.
      // (This WebKit groups an undo as it likes -- typed text with it.)
      await page.evaluate(() => document.execCommand("undo"));
      const undone = await value(page, DESC);
      await page.evaluate(() => document.execCommand("redo"));
      check("and its undo takes the paste back, its redo brings it again",
        !undone.includes("pasted") && (await value(page, DESC)) === "Before: " + CLIP, { undone, redone: await value(page, DESC) });
      // In the title, over what is selected.
      const TITLE = 'input[name="title"]';
      await page.click(TITLE);
      await page.$eval(TITLE, (i) => { i.value = "Fix the retry"; i.setSelectionRange(4, 7); });
      emacs.clip = "the payment";
      await page.keyboard.press("Meta+v");
      check("pr-view: in its title, over the selection", await until(async () => (await value(page, TITLE)) === "Fix the payment retry"),
        await value(page, TITLE));
      // Nothing editable focused: the kit asks nothing.
      await page.click("#context");
      await page.keyboard.press("Meta+v");
      await sleep(300);
      check("not in a field: Emacs is not asked", emacs.asked === 2, emacs.asked);
      check("pr-view: no page errors", errors.length === 0, errors);
      await emacs.stop();
      await ctx.close();
    }

    // ---- clickup-view: a task's comment box, its draft kept ----
    {
      const ctx = await browser.newContext(DESK);
      const page = await ctx.newPage();
      const errors = [];
      page.on("pageerror", (e) => errors.push(String(e)));
      await page.goto(file("clickup-view"));
      await page.waitForFunction(() => window.CU && window.XW);
      const emacs = deskEmacs(page, "clickup:", "ClickUp");
      const TASK = { id: "86abc123", name: "A made-up task", description: "", status: { status: "open", name: "open", color: "#87909e" },
        assignees: [], tags: [], created: String(Date.now() - 3600e3), updated: String(Date.now()), list: { id: "L1", name: "Backlog" } };
      await page.evaluate((t) => CU.renderTask({ task: t }), TASK);
      const BOX = 'textarea[data-draft="c:86abc123"]';
      await page.waitForSelector(BOX);
      await listen(page);
      await page.click(BOX);
      await page.keyboard.press("Meta+v");
      check("clickup-view: ⌘V in its comment box pastes", await until(async () => (await value(page, BOX)) === CLIP),
        await value(page, BOX));
      // Its draft is what the page keeps from its own input events: drawn
      // again, the box still has it.
      await page.evaluate((t) => CU.renderTask({ task: t }), TASK);
      check("and the page took it as typed: drawn again, its draft has the paste", (await value(page, BOX)) === CLIP,
        await value(page, BOX));
      check("clickup-view: no page errors", errors.length === 0, errors);
      await emacs.stop();
      await ctx.close();
    }

    // ---- the herd page: text, and the image of a clipboard with none ----
    {
      const ctx = await browser.newContext(DESK);
      const page = await ctx.newPage();
      const errors = [];
      page.on("pageerror", (e) => errors.push(String(e)));
      await page.goto(file("ghostherd"));
      await page.waitForFunction(() => window.GM && window.XW);
      const emacs = deskEmacs(page, "ghmem:", "Herd");
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(150);
      await page.click('.pgroup[data-project="~/src/deploy-platform/"] [data-act="a:room"]');
      await sleep(200);
      await page.click('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(250);
      const NAME = "claude-deploy-platform";
      const BOX = '.rcol[data-name="' + NAME + '"] .rfoot textarea';
      await page.click(BOX);
      await page.keyboard.press("Meta+v");
      check("herd: ⌘V in a prompt box pastes the text, through the kit",
        await until(async () => (await value(page, BOX)) === CLIP), await value(page, BOX));
      check("and asks for no image", !emacs.got.some((i) => i.op === "agent-image"));
      emacs.clip = "";
      await page.keyboard.press("Meta+v");
      check("a clipboard with no text: the image is asked for, for that box's agent",
        await until(() => emacs.got.some((i) => i.op === "agent-image" && i.name === NAME)), emacs.got.map((i) => i.op));
      check("and nothing goes in the box", (await value(page, BOX)) === CLIP);
      check("its own paste intent is gone", !emacs.got.some((i) => i.op === "paste"));
      check("herd: no page errors", errors.length === 0, errors);
      await emacs.stop();
      await ctx.close();
    }

    // ---- in a browser, ⌘V is the browser's own ----
    {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, host: "127.0.0.1", context: DESK });
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(150);
      await page.click('.pgroup[data-project="~/src/deploy-platform/"] [data-act="a:room"]');
      await sleep(200);
      await page.click('.room [data-act="a:room-view"][data-view="columns"]');
      await sleep(250);
      await page.click('.rcol[data-name="claude-deploy-platform"] .rfoot textarea');
      await page.keyboard.press("Meta+v");
      await sleep(400);
      check("in a browser the kit asks Emacs nothing on ⌘V", !intents.some((i) => /paste/.test(i.op)), intents.map((i) => i.op));
      check("browser: no page errors", errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
