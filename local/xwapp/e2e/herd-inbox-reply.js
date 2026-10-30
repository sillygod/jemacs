// The Inbox's replies (ask 7c6eb080): a report is answered as a question
// is -- Reply opens the same box, compact until pressed -- and either box
// takes images as a room's does: the Image button, a paste.  Sent, the
// images go after the text, a file name a line, to the agent, its lead
// (by its project) or primary, as the item says; a report sent to is
// read.  Desk (as Emacs's WebKit has it: the image from Emacs's
// clipboard) and phone (a photo picked there, sent in a request).
const path = require("path");
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const SHOT = path.join(__dirname, "fixtures", "reply-shot.png");
const now = Date.now();
const mk = (o) => Object.assign({ cli: "claude", project: "~/src/deploy-platform/", at: now - 120000, group: "fyi",
  kind: "report", tag: "Report", tone: "green", whyLabel: "It reported", body: "", preview: "", code: "", note: "",
  read: false, override: null }, o);
const ITEMS = [
  mk({ id: "question:claude-godzilla:8", group: "need", kind: "question", tag: "Question", tone: "accent",
    agent: "claude-godzilla", project: "~/src/godzilla/", title: "Open a PR?", preview: "Shall I open a PR?",
    whyLabel: "It ended its turn with a question", body: "Done.\nShall I open a PR?", replyVia: "agent" }),
  mk({ id: "report:lead-.emacs.d:9", agent: "lead-.emacs.d", project: "~/.emacs.d/", title: "Guard hardenings handed out",
    preview: "scoped token and marked asks", body: "On the three hardenings.", replyVia: "lead" }),
  mk({ id: "report:primary:4", agent: "primary", project: "~/", title: "Morning sweep done",
    preview: "nothing stuck", body: "Nothing stuck overnight.", replyVia: "primary" }),
];
const sent = (intents, op) => intents.filter((i) => i.op === op);

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, where === "desk"
        ? { intents, host: "127.0.0.1", context: DESK } : { intents });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const open = async (id) => {
        if (where === "desk") return page.click('.ibrow[data-id="' + id + '"]');
        if (await page.$('button.ibcard[data-id="' + id + '"]')) await page.tap('button.ibcard[data-id="' + id + '"]');
      };
      const push = () => page.evaluate((it) => GM.renderInbox({ items: it, count: 1 }), ITEMS);
      await sleep(150);
      await push();
      await sleep(100);

      // ---- A report: compact, Reply opens the box a question has ----
      const R = "report:lead-.emacs.d:9";
      await open(R);
      await sleep(80);
      check(W("a report shows Reply and Mark read, and no box"),
        !!(await page.$('[data-act="ib:reply-open"][data-id="' + R + '"]')) &&
        !!(await page.$('[data-act="ib:done"][data-id="' + R + '"]')) && !(await page.$(".ibreplybox")));
      await tap('[data-act="ib:reply-open"][data-id="' + R + '"]');
      await sleep(80);
      check(W("Reply opens a box, with Send, Image and Cancel"),
        !!(await page.$('.ibreply[data-name="lead-.emacs.d"] .ibreplybox[data-id="' + R + '"]')) &&
        !!(await page.$('.ibreply [data-act="ib:reply"]')) && !!(await page.$('.ibreply [data-act="a:image"]')) &&
        !!(await page.$('.ibreply [data-act="ib:reply-close"]')));
      check(W("the box has the cursor"), await page.evaluate(() => document.activeElement && document.activeElement.classList.contains("ibreplybox")));
      await tap('[data-act="ib:reply-close"]');
      await sleep(80);
      check(W("Cancel makes it compact again"), !(await page.$(".ibreplybox")) && !!(await page.$('[data-act="ib:reply-open"]')));
      await tap('[data-act="ib:reply-open"][data-id="' + R + '"]');
      await sleep(80);

      // ---- The Image button, as a room's ----
      if (where === "desk") {
        await page.click('.ibreply [data-act="a:image"]');
        await sleep(60);
        check(W("Image asks Emacs for the clipboard's image, for the item's agent"),
          (sent(intents, "agent-image").pop() || {}).name === "lead-.emacs.d", sent(intents, "agent-image"));
      } else {
        const [chooser] = await Promise.all([page.waitForEvent("filechooser"), page.tap('.ibreply [data-act="a:image"]')]);
        await chooser.setFiles(SHOT);
        await sleep(400);
        const up = sent(intents, "agent-image-upload").pop() || {};
        check(W("Image picks a photo and sends it for the item's agent"),
          up.name === "lead-.emacs.d" && typeof up.data === "string" && up.data.length > 100, { name: up.name });
      }
      // What Emacs answers: the file saved, attached to the agent's next prompt.
      await page.evaluate(() => GM.attachImage({ name: "lead-.emacs.d", path: "/tmp/herd-e2e/clip-1.png", thumb: null }));
      await sleep(60);
      check(W("the image shows above the box"),
        (await page.$$eval('.ibreply[data-name="lead-.emacs.d"] .attach .chip', (es) => es.map((e) => e.textContent))).join("|").includes("clip-1.png"));
      await page.fill('.ibreplybox[data-id="' + R + '"]', "Good -- merge the first two");
      // The steady tick redraws the Inbox: the box stays open, its text and image kept.
      await push();
      await sleep(80);
      check(W("a redraw keeps the box open, its text and its image"),
        (await page.$eval('.ibreplybox[data-id="' + R + '"]', (t) => t.value)) === "Good -- merge the first two" &&
        !!(await page.$('.ibreply[data-name="lead-.emacs.d"] .attach .chip')));

      // ---- A push while the box is written in: it keeps the cursor ----
      const BOX = '.ibreplybox[data-id="' + R + '"]';
      const inBox = () => page.evaluate((r) => {
        const a = document.activeElement;
        return a && a.classList.contains("ibreplybox") && a.getAttribute("data-id") === r
          ? { start: a.selectionStart, end: a.selectionEnd, dir: a.selectionDirection } : null;
      }, R);
      await page.$eval(BOX, (t) => { t.focus(); t.setSelectionRange(5, 10, "backward"); });
      await push();
      await sleep(80);
      check(W("a push while typing keeps the box focused, with its selection"),
        JSON.stringify(await inBox()) === JSON.stringify({ start: 5, end: 10, dir: "backward" }), await inBox());
      await page.$eval(BOX, (t) => t.setSelectionRange(t.value.length, t.value.length));
      await push();
      await sleep(80);
      const marks = sent(intents, "inbox-set").length;
      await page.keyboard.press("e");
      await sleep(60);
      check(W("`e' typed after a push goes into the box, and marks nothing read"),
        (await page.$eval(BOX, (t) => t.value)) === "Good -- merge the first twoe" &&
        sent(intents, "inbox-set").length === marks && !!(await page.$(BOX)));
      await page.keyboard.press("Backspace");
      // An IME composition open (CJK): a push waits for it to end.
      const NEW = "report:claude-jnotes:3";
      const more = ITEMS.concat([mk({ id: NEW, agent: "claude-jnotes", title: "Notes synced", preview: "both ways", replyVia: "agent" })]);
      const row = '.ibrow[data-id="' + NEW + '"], .ibcard[data-id="' + NEW + '"]';
      await page.$eval(BOX, (t) => {
        t.kept = true;
        t.dispatchEvent(new CompositionEvent("compositionstart", { bubbles: true, data: "" }));
        t.dispatchEvent(new InputEvent("input", { bubbles: true, isComposing: true, data: "ㄏ" }));
      });
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 1 }), more);
      await sleep(80);
      check(W("a push during a composition is not drawn: the box is the same one"),
        (await page.$eval(BOX, (t) => t.kept === true)) && !(await page.$(row)));
      await page.$eval(BOX, (t) => t.dispatchEvent(new CompositionEvent("compositionend", { bubbles: true, data: "好" })));
      await sleep(80);
      check(W("the push held is drawn once the composition ends, the box still focused"),
        !!(await page.$(row)) && (await page.$eval(BOX, (t) => t.kept !== true)) && !!(await inBox()));
      await push();
      await sleep(80);
      await tap('[data-act="ib:reply"][data-id="' + R + '"]');
      await sleep(80);
      const say = sent(intents, "lead-say").pop() || {};
      check(W("a lead's report is answered through its project, the image after the text"),
        say.project === "~/.emacs.d/" && say.text === "Good -- merge the first two\n\n/tmp/herd-e2e/clip-1.png", say);
      check(W("no other route is taken for it"), !sent(intents, "agent-prompt").length && !sent(intents, "primary-say").length);
      check(W("the report is read once sent to"),
        sent(intents, "inbox-set").some((i) => i.id === R && i.done === true));
      check(W("it leaves the list"), !(await page.$('.ibrow[data-id="' + R + '"], .ibcard[data-id="' + R + '"]')));

      // ---- A question's box takes a pasted image too ----
      const Q = "question:claude-godzilla:8";
      await push();
      await sleep(80);
      await open(Q);
      await sleep(80);
      check(W("the question's box has the Image button"), !!(await page.$('.ibreply[data-name="claude-godzilla"] [data-act="a:image"]')));
      if (where === "desk") {
        // A paste with no text in it is the clipboard's image.
        await page.$eval('.ibreplybox[data-id="' + Q + '"]', (ta) => {
          ta.focus();
          const ev = new Event("paste", { bubbles: true, cancelable: true });
          Object.defineProperty(ev, "clipboardData", { value: { types: ["Files"] } });
          ta.dispatchEvent(ev);
        });
        await sleep(60);
        check(W("pasting an image asks for it, for the question's agent"),
          (sent(intents, "agent-image").pop() || {}).name === "claude-godzilla");
        // Emacs's own Cmd-V: it answers with the clipboard, no text in it.
        await page.evaluate(() => GM.paste({ text: "" }));
        await sleep(60);
        check(W("Emacs's paste with no text asks for the image too"),
          sent(intents, "agent-image").filter((i) => i.name === "claude-godzilla").length === 2);
      }
      await page.evaluate(() => GM.attachImage({ name: "claude-godzilla", path: "/tmp/herd-e2e/q.png", thumb: null }));
      await sleep(60);
      check(W("a removed image does not go"), await (async () => {
        await page.evaluate(() => GM.attachImage({ name: "claude-godzilla", path: "/tmp/herd-e2e/drop.png", thumb: null }));
        await sleep(40);
        await tap('.ibreply[data-name="claude-godzilla"] .chip:nth-child(2) [data-act="a:unattach"]');
        await sleep(40);
        return (await page.$$('.ibreply[data-name="claude-godzilla"] .chip')).length === 1;
      })());
      // Only an image, no words: it goes.
      await tap('[data-act="ib:reply"][data-id="' + Q + '"]');
      await sleep(80);
      const ap = sent(intents, "agent-prompt").pop() || {};
      check(W("an image alone answers a worker's question, to the agent"),
        ap.name === "claude-godzilla" && ap.text === "/tmp/herd-e2e/q.png", ap);
      check(W("a question answered is not marked done by the page"), !sent(intents, "inbox-set").some((i) => i.id === Q && i.done));

      // ---- Primary's report goes to primary; nothing written, nothing sent ----
      const P = "report:primary:4";
      await push();
      await sleep(80);
      await open(P);
      await sleep(80);
      await tap('[data-act="ib:reply-open"][data-id="' + P + '"]');
      await sleep(60);
      // Images put in the box: another item picked keeps them, Cancel drops them.
      const pchips = () => page.$$('.ibreply[data-name="primary"] .attach .chip');
      await page.evaluate(() => GM.attachImage({ name: "primary", path: "/tmp/herd-e2e/p.png", thumb: null }));
      await sleep(60);
      await open(R);
      await sleep(60);
      await open(P);
      await sleep(60);
      await tap('[data-act="ib:reply-open"][data-id="' + P + '"]');
      await sleep(60);
      check(W("another item picked keeps the box's images"), (await pchips()).length === 1);
      await tap('[data-act="ib:reply-close"][data-id="' + P + '"]');
      await sleep(60);
      await tap('[data-act="ib:reply-open"][data-id="' + P + '"]');
      await sleep(60);
      check(W("Cancel drops the box's images"), (await pchips()).length === 0);
      const before = intents.length;
      await tap('[data-act="ib:reply"][data-id="' + P + '"]');
      await sleep(60);
      check(W("an empty reply sends nothing"), intents.slice(before).every((i) => !/-say$|agent-prompt/.test(i.op)));
      await page.fill('.ibreplybox[data-id="' + P + '"]', "thanks");
      await page.press('.ibreplybox[data-id="' + P + '"]', "Meta+Enter");
      await sleep(80);
      check(W("primary's report is answered to primary (⌘Return)"), (sent(intents, "primary-say").pop() || {}).text === "thanks");

      // ---- Sent or cancelled with a composition open: drawn at once, the old text gone ----
      // Clicked as a click that leaves the box its focus (Safari's), so
      // no focusout ends the composition first.
      const composing = async (text) => {
        await push();
        await sleep(80);
        await open(R);
        await sleep(60);
        await tap('[data-act="ib:reply-open"][data-id="' + R + '"]');
        await sleep(60);
        await page.fill(BOX, text);
        const box = await page.$(BOX);
        await box.evaluate((t) => {
          t.focus();
          t.dispatchEvent(new CompositionEvent("compositionstart", { bubbles: true, data: "" }));
          t.dispatchEvent(new InputEvent("input", { bubbles: true, isComposing: true, data: "ㄏ" }));
        });
        return box;
      };
      const ended = (box) => box.evaluate((t) => {
        t.dispatchEvent(new CompositionEvent("compositionend", { bubbles: true, data: "好" }));
        t.dispatchEvent(new FocusEvent("focusout", { bubbles: true }));
      });
      const left = () => page.$$eval(".ibreplybox", (ts) => ts.map((t) => t.value).filter(Boolean));
      let box = await composing("sent mid-word");
      await page.$eval('[data-act="ib:reply"][data-id="' + R + '"]', (b) => b.click());
      await sleep(80);
      check(W("Send during a composition: sent, and the Inbox drawn at once"),
        (sent(intents, "lead-say").pop() || {}).text === "sent mid-word" &&
        !(await page.$('.ibrow[data-id="' + R + '"], .ibcard[data-id="' + R + '"]')) && !(await page.$(BOX)));
      await ended(box);
      await sleep(80);
      await push();
      await sleep(80);
      check(W("its composition ending after puts no old text back, in any box"),
        (await left()).length === 0 && !!(await page.$(".ibreplybox")), await left());
      box = await composing("cancelled mid-word");
      await page.$eval('[data-act="ib:reply-close"][data-id="' + R + '"]', (b) => b.click());
      await sleep(80);
      check(W("Cancel during a composition: the box closed at once"),
        !(await page.$(BOX)) && !!(await page.$('[data-act="ib:reply-open"][data-id="' + R + '"]')));
      await ended(box);
      await sleep(80);
      await push();
      await sleep(80);
      await tap('[data-act="ib:reply-open"][data-id="' + R + '"]');
      await sleep(60);
      check(W("and opened again, empty"), (await page.$eval(BOX, (t) => t.value)) === "" && (await left()).length === 0, await left());

      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
