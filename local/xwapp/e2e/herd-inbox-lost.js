// A reply from the Board -- to an item the Inbox once had -- that does not
// reach Emacs goes back in its item's own
// box -- the image lines with it -- and the item back in the list: a
// report it marked read is unmarked.  Written since is kept after it.
// One that reached Emacs after all comes out of the box again.  At the
// desk (by the title: Emacs cannot read it, or answers late) and on a
// phone (Emacs refuses the POST).  The replies are made up.
const path = require("path");
const { webkit } = require("playwright");
const { phonePage, sleep, boardPick } = require("./herdlib");
const { readTitles } = require("./titlelib");
const DESK = "file://" + path.join(process.env.LOCAL || path.resolve(__dirname, "../.."), "ghostherd", "ui", "index.html");
const DESKCTX = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const now = Date.now();
const mk = (o) => Object.assign({ cli: "claude", project: "~/src/deploy-platform/", at: now - 120000, group: "fyi",
  kind: "report", tag: "Report", tone: "green", whyLabel: "It reported", body: "", preview: "", code: "", note: "",
  read: false, override: null }, o);
const R = "report:lead-.emacs.d:9";
const P = "report:primary:4";
const S = "report:claude-jnotes:3";
const Q = "question:claude-godzilla:8";
const ITEMS = [
  mk({ id: Q, group: "need", kind: "question", tag: "Question", tone: "accent", agent: "claude-godzilla",
    project: "~/src/godzilla/", title: "Open a PR?", preview: "Shall I open a PR?", body: "Done.\nShall I open a PR?", replyVia: "agent" }),
  mk({ id: R, agent: "lead-.emacs.d", project: "~/.emacs.d/", title: "Guard hardenings handed out",
    preview: "scoped token and marked asks", body: "On the three hardenings.", replyVia: "lead" }),
  mk({ id: P, agent: "primary", project: "~/", title: "Morning sweep done", preview: "nothing stuck",
    body: "Nothing stuck overnight.", replyVia: "primary" }),
  mk({ id: S, agent: "claude-jnotes", project: "~/src/jnotes/", title: "Notes synced", preview: "both ways", replyVia: "agent" }),
];
const without = (id) => ITEMS.filter((x) => x.id !== id);
const marks = (got, id) => got.filter((i) => i.op === "inbox-set" && i.id === id && "done" in i).map((i) => i.done);

async function shown(page) {
  return page.evaluate(() => {
    const ta = document.querySelector(".ibreplybox");
    const st = document.getElementById("status");
    return {
      rows: Array.from(document.querySelectorAll(".bd-need .bdnrow")).map((e) => e.getAttribute("data-id")),
      sel: (document.querySelector(".bd-need .bdnrow.open") || { getAttribute: () => "" }).getAttribute("data-id"),
      box: ta ? { id: ta.getAttribute("data-id"), text: ta.value } : null,
      status: st.hidden ? "" : st.textContent,
    };
  });
}

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- At the desk: by the title ----
    {
      const ctx = await browser.newContext(DESKCTX);
      const page = await ctx.newPage();
      const errors = [];
      page.on("pageerror", (e) => errors.push(String(e)));
      await page.goto(DESK);
      await page.waitForFunction(() => window.GM && window.XW);
      const got = [];
      let refuse = null, deaf = null, stall = null, running = true;
      // Emacs reading all along, as the test sets it: REFUSE, DEAF, STALL.
      const reader = readTitles(page, "ghmem:", () => !running, {
        ms: 120000, got, idle: "Herd", refuse: (t) => !!(refuse && refuse(t)), deaf: () => !!(deaf && deaf()),
        stall: (it) => (stall ? stall(it) : 0),
      });
      // PRED true, or MS gone by; PRED may be async.
      const until = async (pred, ms) => {
        for (let t = Date.now(); Date.now() - t < (ms || 5000) && !(await pred()); ) await sleep(30);
      };
      await until(() => got.some((i) => i.op === "xwapp-hello"));
      await page.click('.tab[data-tab="board"]');
      const push = (items) => page.evaluate((it) => GM.renderInbox({ items: it, count: 1, needsOnly: true }), items || ITEMS);
      await push();
      await sleep(100);

      // A report's reply, image and all, that Emacs cannot read.
      await boardPick(page, R);
      await page.click('[data-act="ib:reply-open"][data-id="' + R + '"]');
      await page.evaluate(() => GM.attachImage({ name: "lead-.emacs.d", path: "/tmp/herd-e2e/clip-1.png", thumb: null }));
      await page.fill('.ibreplybox[data-id="' + R + '"]', "A made-up reply that will not get through");
      const WANT = "A made-up reply that will not get through\n\n/tmp/herd-e2e/clip-1.png";
      refuse = (t) => /"op":"lead-say"/.test(t);
      await page.click('[data-act="ib:reply"][data-id="' + R + '"]');
      await until(() => marks(got, R).length >= 2);
      refuse = null;
      await sleep(100);
      let s = await shown(page);
      check("desk: a lost reply is back in its report's own box, the image line with it, the report picked",
        s.sel === R && s.box && s.box.id === R && s.box.text === WANT, s);
      check("desk: the report marked read for it is unmarked, after, and is back in the list",
        JSON.stringify(marks(got, R)) === "[true,false]" && s.rows.includes(R), { marks: marks(got, R), rows: s.rows });
      check("desk: the status line says so", /Not sent/.test(s.status) && /report is back on the Board/.test(s.status), s.status);
      // Emacs's push from before it had the unmark does not drop it.
      await push(without(R));
      await sleep(80);
      s = await shown(page);
      check("desk: a push sent before Emacs had the unmark keeps it, and its box", s.rows.includes(R) && s.box && s.box.text === WANT, s);
      // Sent again, it goes whole, and the report is read again.
      await push();
      await sleep(60);
      await page.click('[data-act="ib:reply"][data-id="' + R + '"]');
      await until(() => marks(got, R).length >= 3);
      await sleep(80);
      const says = got.filter((i) => i.op === "lead-say").map((i) => i.text);
      check("desk: sent again from its box, it goes through the lead's project, text and image line",
        JSON.stringify(says) === JSON.stringify([WANT]) && JSON.stringify(marks(got, R)) === "[true,false,true]", { says, marks: marks(got, R) });

      // A question's answer lost: on the Board the question is hidden while
      // its answer is on its way -- Emacs lists it until its agent takes the
      // answer -- so nothing is written in its box meanwhile; lost, it is
      // back, the answer in its box.
      await push();
      await sleep(60);
      await boardPick(page, Q);
      await page.fill('.ibreplybox[data-id="' + Q + '"]', "first answer");
      let typed = false;
      deaf = () => !typed;
      await page.click('[data-act="ib:reply"][data-id="' + Q + '"]');
      await sleep(100);
      await push();            // Emacs still lists it: its agent has not taken the answer
      await sleep(60);
      s = await shown(page);
      check("desk: a question answered is not listed while its answer is on its way", !s.rows.includes(Q), s.rows);
      refuse = (t) => /"op":"agent-prompt"/.test(t);
      const unread0 = got.filter((i) => i.unread).length;
      typed = true;
      await until(() => got.filter((i) => i.unread).length > unread0);
      deaf = refuse = null;
      await sleep(120);
      s = await shown(page);
      check("desk: a lost answer is back in its question's box, the question listed and picked",
        s.box && s.box.id === Q && s.box.text === "first answer" && s.sel === Q && s.rows.includes(Q) && /answer is back/.test(s.status), s);
      check("desk: a question is not marked or unmarked", marks(got, Q).length === 0, marks(got, Q));
      await page.click('[data-act="ib:done"][data-id="' + Q + '"]');
      await sleep(60);

      // Lost while another reply is being written: it waits in its own
      // box, the one being written in is left alone.
      await push(without(Q));
      await sleep(60);
      await boardPick(page, S);
      await page.click('[data-act="ib:reply-open"][data-id="' + S + '"]');
      await page.fill('.ibreplybox[data-id="' + S + '"]', "a made-up reply lost later");
      typed = false;
      deaf = () => !typed;
      await page.click('[data-act="ib:reply"][data-id="' + S + '"]');
      await sleep(100);
      await boardPick(page, P);
      await page.click('[data-act="ib:reply-open"][data-id="' + P + '"]');
      await page.fill('.ibreplybox[data-id="' + P + '"]', "half a reply to another");
      refuse = (t) => /"op":"agent-prompt"/.test(t);
      typed = true;
      await until(() => marks(got, S).length >= 2);
      deaf = refuse = null;
      await sleep(120);
      s = await shown(page);
      check("desk: lost while another reply is written, that one is left as it is",
        s.sel === P && s.box && s.box.id === P && s.box.text === "half a reply to another" && s.rows.includes(S), s);
      await page.click('[data-act="ib:reply-close"][data-id="' + P + '"]');
      await boardPick(page, S);
      await sleep(60);
      s = await shown(page);
      check("desk: and the lost one is in its own box when its report is picked", s.box && s.box.id === S && s.box.text === "a made-up reply lost later", s);
      await page.click('[data-act="ib:reply-close"][data-id="' + S + '"]');
      await page.evaluate(() => { document.getElementById("status").hidden = true; });

      // Emacs busy past 10 s after reading a reply, the report's mark
      // waiting behind it: the page gives the reply up -- back in the box,
      // the report unmarked -- and when Emacs's answer lands saying it had
      // it, takes that back: box emptied, report read and gone again.
      await push();
      await sleep(60);
      await boardPick(page, P);
      await page.click('[data-act="ib:reply-open"][data-id="' + P + '"]');
      await page.fill('.ibreplybox[data-id="' + P + '"]', "a made-up reply Emacs reads late");
      stall = (it) => (it.op === "primary-say" ? 10600 : 0);
      await page.click('[data-act="ib:reply"][data-id="' + P + '"]');
      await until(() => page.evaluate(() => {
        const st = document.getElementById("status");
        return !st.hidden && /Not sent/.test(st.textContent);
      }), 11000);
      const mid = await shown(page);
      await until(() => marks(got, P).length >= 3, 6000);
      stall = null;
      await sleep(150);
      s = await shown(page);
      const prims = got.filter((i) => i.op === "primary-say").map((i) => i.text);
      check("desk: unread 10 s with the report's mark behind it, the reply was given back",
        mid.box && mid.box.id === P && mid.box.text === "a made-up reply Emacs reads late" && mid.rows.includes(P) && /Not sent/.test(mid.status), mid);
      check("desk: Emacs had it after all: out of the box, the report read and gone again, Not sent gone, sent once",
        JSON.stringify(prims) === JSON.stringify(["a made-up reply Emacs reads late"]) &&
        JSON.stringify(marks(got, P)) === "[true,false,true]" && !s.rows.includes(P) && !/Not sent/.test(s.status) &&
        !(s.box && s.box.id === P), { prims, marks: marks(got, P), s });
      check("desk: no page errors", errors.length === 0, errors);
      running = false;
      await reader;
      await ctx.close();
    }

    // ---- On a phone: POSTed, refused ----
    {
      const intents = [];
      let refuse = false;
      const { ctx, page, errors } = await phonePage(browser, {
        intents, refuse: (it) => (refuse && it.op === "lead-say" ? 413 : 0),
      });
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 1, needsOnly: true }), ITEMS);
      await page.tap('.tab[data-tab="board"]').catch(() => page.evaluate(() => document.querySelector('.tab[data-tab="board"]').click()));
      await sleep(150);
      await page.evaluate((it) => GM.renderInbox({ items: it, count: 1, needsOnly: true }), ITEMS);
      await sleep(100);
      await boardPick(page, R, (sel) => page.tap(sel));
      await sleep(60);
      await page.tap('[data-act="ib:reply-open"][data-id="' + R + '"]');
      await page.fill('.ibreplybox[data-id="' + R + '"]', "a made-up reply Emacs will refuse");
      refuse = true;
      await page.tap('[data-act="ib:reply"][data-id="' + R + '"]');
      for (let i = 0; i < 60 && marks(intents, R).length < 2; i++) await sleep(50);
      await sleep(100);
      const s = await shown(page);
      check("phone: a refused reply is back in its report's box, the report back in the list, unmarked after",
        s.box && s.box.id === R && s.box.text === "a made-up reply Emacs will refuse" && s.rows.includes(R) &&
        JSON.stringify(marks(intents, R)) === "[true,false]" && /Not sent/.test(s.status), { s, marks: marks(intents, R) });
      check("phone: no page errors", errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const passed = results.filter(Boolean).length;
  console.log(`\n${passed}/${results.length} passed`);
  process.exit(passed === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
