// A project fact made every project's in one approval (ask d197da21).  The
// proposal shows on the Memory tab and in the Inbox: the fact, its text, its
// check -- which moves with it, on or off as it is -- and its counts; it is
// approved as "Move to every project" by its id, or rejected.  Desk and
// phone; drives GM.* as Emacs would push and reads the intents back -- no
// sidecar.  Screenshots go to shots/share/ (SHOTS overrides).
const { webkit } = require("playwright");
const fs = require("fs");
const path = require("path");
const { phonePage } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const SHOTS = process.env.SHOTS || path.join(__dirname, "shots", "share");
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };

const PROJECTS = { user: { key: "user", label: "User (shared)" },
  projects: [{ key: "-src-dp", label: "deploy-platform", project: "~/src/deploy-platform/" }] };
const FACTS = [
  { name: "grep-cli-binaries-cheaply", title: "Grep cli binaries cheaply", description: "strings | grep -F",
    type: "feedback", author: "claude-dp", body: "Never grep a CLI binary with a leading .{0,N}.", shared: false },
];
const TEXT = "Never grep a CLI binary with a leading `.{0,N}`: it backtracks at every byte.\n\n" +
  "**How to apply:** `timeout 30 strings FILE | grep -F -m 20 'literal'`, and " + "a long line ".repeat(30) +
  "<img src=x onerror=alert(1)>";
const PROPOSALS = [
  { id: "p-share", kind: "share", status: "pending", project: "-src-dp", file: "user", author: "lead-dp", diff: TEXT,
    facts: [{ name: "grep-cli-binaries-cheaply", title: "Grep cli binaries cheaply", gone: false, promoted: false,
              description: "strings | grep -F", check: "off" }], rule: null },
  { id: "p-plain", kind: "share", status: "pending", project: "-src-dp", file: "user", author: "lead-dp",
    diff: "Use kctx for any GKE cluster.",
    facts: [{ name: "kctx", title: "Kctx", gone: false, promoted: false, description: "kctx", check: null }], rule: null },
];

const sent = (intents, op) => intents.filter((i) => i.op === op);
const overflow = (page) => page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);

(async () => {
  fs.mkdirSync(SHOTS, { recursive: true });
  const browser = await webkit.launch();
  try {
    for (const [label, ctxOpts] of [["desk", DESK], ["phone", PHONE]]) {
      const { ctx, page, intents, errors } = await phonePage(browser, { context: ctxOpts });
      const W = (n) => label + ": " + n;
      const tap = (sel) => (label === "phone" ? page.tap(sel) : page.click(sel));

      await page.evaluate(() => GM.showView("memory"));
      await page.evaluate((p) => GM.setMemoryProjects(p), PROJECTS);
      await page.evaluate((f) => GM.renderMemory({ key: "-src-dp", facts: f }), FACTS);
      await page.evaluate((p) => GM.renderProposals({ key: "-src-dp", proposals: p }), PROPOSALS);
      await sleep(120);

      // ---- On the Memory tab: the fact, where it goes, its text, its check ----
      const prop = ".mprop.shprop";
      check(W("both show as moves to every project"), (await page.$$(prop)).length === 2);
      check(W("it names its fact"),
        /Grep cli binaries cheaply/.test(await page.$eval(prop + " .mpfact", (e) => e.textContent)));
      check(W("it says where the fact goes"),
        /every project \(the user store\)/.test(await page.$eval(prop + " .mpinto", (e) => e.textContent)));
      check(W("it shows the fact's text, rendered"),
        await page.$eval(prop + " .shtext", (e) => !!e.querySelector("strong") && /backtracks at every byte/.test(e.textContent)));
      check(W("and nothing in it runs"), !(await page.$(prop + " .shtext img[onerror]")));
      check(W("a long text scrolls in its box"),
        await page.$eval(prop + " .shtext", (e) => e.scrollHeight >= e.clientHeight));
      check(W("its check moves too, off as it is"),
        /Its check moves too, and stays off\. Its counts go with it\./.test(await page.$eval(prop + " .shcheck", (e) => e.textContent)));
      check(W("one with no check says only its counts go"),
        (await page.$$eval(prop + " .shcheck", (es) => es.map((e) => e.textContent)))[1] === "Its counts go with it.");
      check(W("no diff for a move"), !(await page.$(prop + " .mpdiff")));
      check(W("approving reads as moving it"),
        (await page.$eval(prop + ' [data-act="prop-approve"]', (e) => e.textContent)) === "Move to every project");
      await page.screenshot({ path: path.join(SHOTS, label + "-memory.png"), fullPage: true });
      check(W("nothing runs off the page's edge"), (await overflow(page)) <= 0, await overflow(page));
      await tap(prop + ' [data-act="prop-approve"]');
      await sleep(60);
      const appr = sent(intents, "memory-proposal-approve").pop();
      check(W("Move to every project approves the proposal by its id"), appr && appr.id === "p-share", appr);

      // ---- Reject asks why, as for any proposal ----
      await tap('[data-act="prop-reject"][data-id="p-plain"]');
      await sleep(60);
      check(W("Reject asks why"), !!(await page.$("#mem-prej")));
      await page.fill("#mem-prej", "keep it this project's");
      await tap('[data-act="prop-reject-send"][data-id="p-plain"]');
      await sleep(60);
      const rej = sent(intents, "memory-proposal-reject").pop();
      check(W("Reject sends its id and why"), rej && rej.id === "p-plain" && rej.reason === "keep it this project's", rej);

      // ---- The Inbox: one item, the text and the check, Move to every project ----
      await page.evaluate(() => document.querySelector('.tab[data-tab="inbox"]').click());
      await page.evaluate((t) => GM.renderInbox({ count: 1, items: [{
        id: "proposal:p-share", group: "need", kind: "proposal", tag: "Proposal", tone: "purple",
        agent: "lead-dp", cli: "claude", project: "-src-dp", at: Date.now() - 60e3,
        title: "Make “Grep cli binaries cheaply” every project's", whyLabel: "A fact for every project",
        body: "It moves to the user store: every project's agents get it, and its counts go with it.  Its check moves too, and stays off.",
        preview: "strings | grep -F", facts: ["Grep cli binaries cheaply"], share: true, text: t, check: "off",
        proposalId: "p-share", read: false, override: null }] }), TEXT);
      await sleep(120);
      if (label === "desk") await page.click('.ibrow[data-id="proposal:p-share"]');
      else if (await page.$('.ibcard[data-id="proposal:p-share"]')) await page.tap('.ibcard[data-id="proposal:p-share"]');
      await sleep(120);
      check(W("the Inbox item shows the fact's text"),
        await page.evaluate(() => /backtracks at every byte/.test((document.querySelector(".ibprop .shtext") || {}).textContent || "")));
      check(W("and its check, said once"),
        await page.evaluate(() => (document.body.innerText.match(/stays off/g) || []).length === 1));
      check(W("the text is the page's, not the card's"),
        await page.evaluate(() => getComputedStyle(document.querySelector(".ibprop .shtext p")).color
          === getComputedStyle(document.body).color));
      check(W("no diff, no check examples"), !(await page.$(".ibprop .ibdiff")) && !(await page.$(".ibprop .ckex")));
      await page.screenshot({ path: path.join(SHOTS, label + "-inbox.png"), fullPage: true });
      const before = sent(intents, "memory-proposal-approve").length;
      const btn = '.ibprop [data-act="ib:prop-approve"]';
      check(W("its approve says what it does"), (await page.$eval(btn, (e) => e.textContent)) === "Move to every project");
      await tap(btn);
      await sleep(80);
      const ia = sent(intents, "memory-proposal-approve");
      check(W("the Inbox's Move approves it by its id"),
        ia.length === before + 1 && ia[ia.length - 1].id === "p-share", ia[ia.length - 1]);
      check(W("nothing runs off the Inbox's edge"), (await overflow(page)) <= 0, await overflow(page));
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const failed = results.filter((r) => !r).length;
  console.log("\n" + (results.length - failed) + "/" + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
