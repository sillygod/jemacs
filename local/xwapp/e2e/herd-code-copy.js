// A Copy on each code block of what the page renders as markdown -- a
// room's turns, the Inbox's detail -- for the command an agent asks you
// to run: the block's text exactly, no language tag, no newline added.
// On a phone a tab on the block's top-right corner, always there, above
// the code, so a long line scrolled sideways never runs under it; at a
// desk inside that corner, shown on hover or focus.  Inline code has none.
// The phone copies through the browser (XW.copy); the desk, as Emacs's
// own WebKit has it (host 127.0.0.1), asks Emacs to.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep, boardPick } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const DP = "~/src/deploy-platform/";
const RD = "claude-deploy-platform";
const ts = Date.now() / 1000 - 600;

const CMD = "! bash /private/tmp/claude-501/x/scratchpad/check.sh --site 75ue-2 \"a & b\" <in.txt";
const LONG = "CLOUDSDK_CORE_ACCOUNT=devops@production-a.iam.gserviceaccount.com KUBECONFIG=/private/tmp/kc " +
  "kubectl --context gke_production-a_asia-east2-a_production-cluster-1 -n argocd logs deploy/argocd-server --timestamps";
const MULTI = "cd ~/src/deploy-platform\n  npm test -- --runInBand\n\necho done";
const REPLY = "Please run this yourself, `inline` stays as it is:\n\n```bash\n" + CMD + "\n```\n\nThen this one:\n\n```\n" +
  LONG + "\n```\n\nAnd:\n\n```sh\n" + MULTI + "\n```\n";
const CONV = { name: RD, entries: [{ role: "user", text: "check the site", ts }, { role: "assistant", text: REPLY, ts }] };
const ITEM = { id: "question:" + RD + ":3", cli: "claude", project: DP, at: Date.now() - 120000, group: "need",
  kind: "question", tag: "Question", tone: "accent", whyLabel: "It ended its turn with a question", agent: RD,
  title: "Run the check?", preview: "Please run this yourself", body: REPLY, code: "", note: "", read: false,
  override: null, replyVia: "agent" };

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, where === "desk"
        ? { intents, host: "127.0.0.1", context: DESK } : { intents });
      const W = (n) => where + ": " + n;
      await page.evaluate(() => {
        window.copied = [];
        const exec = document.execCommand.bind(document);
        document.execCommand = (cmd, ...rest) => {
          if (cmd !== "copy") return exec(cmd, ...rest);
          const ta = [...document.querySelectorAll("body > textarea[readonly]")].pop();
          window.copied.push(ta ? ta.value : null);
          return true;
        };
      });
      // What was copied: this device's clipboard on the phone; on the desk,
      // what the page asked Emacs to put on its own.
      const copied = async () => where === "phone"
        ? page.evaluate(() => window.copied.splice(0))
        : intents.splice(0).filter((i) => i.op === "copy").map((i) => i.text);
      const at = (p) => (where === "phone" ? page.touchscreen.tap(p.x, p.y) : page.mouse.click(p.x, p.y));

      // Where each block's button is, and what is at its middle.
      const blocks = (body) => page.$$eval(body + " .codewrap", (ws) => ws.map((w) => {
        const b = w.querySelector(":scope > .codecopy"), pre = w.querySelector(":scope > pre");
        const r = b.getBoundingClientRect(), p = pre.getBoundingClientRect();
        const x = r.left + r.width / 2, y = r.top + r.height / 2;
        const firstLine = p.top + parseFloat(getComputedStyle(pre).paddingTop) + parseFloat(getComputedStyle(pre).borderTopWidth);
        return { x, y, w: r.width, h: r.height, top: r.top, bottom: r.bottom, left: r.left, right: r.right,
                 preTop: p.top, preRight: p.right, firstLine, opacity: getComputedStyle(b).opacity,
                 onTop: document.elementFromPoint(x, y) === b, scrolls: pre.scrollWidth > pre.clientWidth,
                 vw: window.innerWidth, vh: window.innerHeight };
      }));

      async function suite(label, body) {
        const L = (n) => W(label + ": " + n);
        const counts = await page.$eval(body, (b) => ({
          pre: b.querySelectorAll("pre").length, copy: b.querySelectorAll(".codecopy").length,
          wrapped: [...b.querySelectorAll("pre")].every((p) => p.parentElement.classList.contains("codewrap") &&
                                                          p.previousElementSibling && p.previousElementSibling.matches("button.codecopy")),
          inline: [...b.querySelectorAll(":not(pre) > code")].filter((c) => c.closest(".codewrap")).length,
          text: b.textContent.includes("Copy the code") || /\bCopy\b/.test(b.textContent) }));
        check(L("a Copy on each of its three code blocks, none on its inline code"),
          counts.pre === 3 && counts.copy === 3 && counts.wrapped && counts.inline === 0, counts);
        check(L("the button adds no text to the message"), !counts.text, counts);

        let bs = await blocks(body);
        if (where === "phone") {
          check(L("shown without a hover"), bs.every((b) => b.opacity === "1" && b.w > 0 && b.h > 0), bs.map((b) => b.opacity));
          check(L("at the block's top-right, above its code: the first line is never under it"),
            bs.every((b) => Math.abs(b.right - b.preRight) <= 1 && b.bottom <= b.preTop + 1 && b.bottom <= b.firstLine), bs);
          check(L("small, but big enough for a finger"), bs.every((b) => b.h >= 28 && b.w <= 80), bs.map((b) => [b.w, b.h]));
        } else {
          check(L("not shown until the block is hovered"), bs.every((b) => b.opacity === "0"), bs.map((b) => b.opacity));
          await page.hover(body + " .codewrap:nth-of-type(1) pre");
          await sleep(250);
          bs = await blocks(body);
          check(L("hovered, its block's Copy shows, inside its top-right corner"),
            bs[0].opacity === "1" && bs[0].right <= bs[0].preRight && bs[0].right >= bs[0].preRight - 12 &&
            bs[0].top >= bs[0].preTop && bs[0].top - bs[0].preTop < 12, bs[0]);
        }

        // Each block, its text exactly: no language tag, no newline added,
        // the characters HTML escapes as they were.
        const want = [CMD, LONG, MULTI];
        for (let k = 0; k < 3; k++) {
          await page.$eval(body, (b, k) => b.querySelectorAll(".codewrap")[k].scrollIntoView({ block: "center" }), k);
          if (where === "desk") await page.hover(body + " .codewrap >> nth=" + k);
          await sleep(200);
          const b = (await blocks(body))[k];
          await at(b);
          await sleep(100);
        }
        const got = await copied();
        check(L("each copies its block's text exactly"), JSON.stringify(got) === JSON.stringify(want), got);
        const flash = await page.$eval("#status", (s) => (s.hidden ? "" : s.textContent));
        if (where === "phone") check(L("and says so"), flash === "Copied", flash);

        // The long one scrolled to its end: its button where it was, on top, and it copies the whole line.
        await page.$eval(body, (b) => b.querySelectorAll(".codewrap")[1].scrollIntoView({ block: "center" }));
        const before = (await blocks(body))[1];
        const left = await page.$eval(body, (b) => { const p = b.querySelectorAll(".codewrap pre")[1]; p.scrollLeft = p.scrollWidth; return p.scrollLeft; });
        if (where === "desk") await page.hover(body + " .codewrap >> nth=1");
        await sleep(250);
        const after = (await blocks(body))[1];
        check(L("a block scrolled sideways: its Copy stays at its corner, in sight and on top"),
          before.scrolls && left > 0 && Math.abs(after.x - before.x) < 1 && Math.abs(after.y - before.y) < 1 &&
          after.onTop && after.opacity === "1" && after.left >= 0 && after.right <= after.vw && after.top >= 0 && after.bottom <= after.vh,
          { before, after, left });
        if (where === "phone") check(L("and the line scrolled under nothing"), after.bottom <= after.firstLine, after);
        await at(after);
        await sleep(100);
        const scrolled = await copied();
        check(L("and copies the whole long line"), scrolled.length === 1 && scrolled[0] === LONG, scrolled);
      }

      // ---- An item's detail on the Board (once the Inbox's) ----
      await sleep(150);
      await page.evaluate((it) => { GM.showView("board"); GM.renderInbox({ items: [it], count: 1, needsOnly: true }); }, ITEM);
      await sleep(100);
      await boardPick(page, ITEM.id, where === "desk" ? null : (sel) => page.tap(sel));
      await sleep(150);
      intents.splice(0);
      await suite("the Board", ".ibbody.msg-body");
      const others = intents.filter((i) => i.op !== "copy" && i.op !== "inbox-detail" && i.op !== "inbox-set");
      check(W("a Copy on the Board only copies"), others.length === 0, others);

      // The keyboard: the desk's button shows while it has the focus.
      if (where === "desk") {
        await page.mouse.move(2, 2);
        await sleep(200);
        const o0 = await page.$eval(".ibbody .codecopy", (b) => { b.blur(); return getComputedStyle(b).opacity; });
        await page.focus(".ibbody .codecopy");
        await sleep(250);
        const o1 = await page.$eval(".ibbody .codecopy", (b) => document.activeElement === b && getComputedStyle(b).opacity);
        check(W("with the focus, it shows; without, not"), o0 === "0" && o1 === "1", { o0, o1 });
        await page.keyboard.press("Enter");
        await sleep(100);
        const keyed = await copied();
        check(W("and Enter on it copies"), keyed.length === 1 && keyed[0] === CMD, keyed);
      }

      // ---- A room's column ----
      await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, HERD);
      await sleep(100);
      await (where === "phone" ? page.tap.bind(page) : page.click.bind(page))('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
      await sleep(150);
      if (await page.$(".room .chat")) {
        await (where === "phone" ? page.tap.bind(page) : page.click.bind(page))('.room [data-act="a:room-view"][data-view="columns"]');
        await sleep(150);
      }
      await page.evaluate((c) => GM.setConversation(c), CONV);
      await sleep(150);
      await suite("a room", '.rcol[data-name="' + RD + '"] .rbody .ent.me .ebody');

      // The turn's own Copy still copies the turn as written, code and all.
      intents.splice(0);
      await (where === "phone" ? page.tap.bind(page) : page.click.bind(page))('.rcol[data-name="' + RD + '"] .rbody .ent.me > .ehead .ecopy');
      await sleep(100);
      const turn = await copied();
      check(W("the turn's own Copy still copies all of it, as written"), turn.length === 1 && turn[0] === REPLY, turn);

      const wide = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
      check(W("no sideways scroll"), wide <= 0, wide);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const bad = results.filter((r) => !r).length;
  console.log(bad ? bad + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(bad ? 1 : 0);
})();
