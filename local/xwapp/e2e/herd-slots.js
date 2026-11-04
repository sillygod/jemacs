// The Agents tab's handoff caps, on a phone and at a desk, served by a real
// Emacs (herd-slots.el): ghostherd's own page code takes what the page
// sends, a cap only when sane -- a whole number in its range, the cap at
// work not above the cap up -- and saves it with customize-save-variable.
// Beside each cap what is in use now; then the handoffs waiting, from whom
// to where, since when.  An out-of-range value is refused by Emacs: the page
// says why and shows the cap as it is, and nothing is saved.  SHOTS=DIR
// keeps screenshots of each (slots-desk.png, slots-phone.png, and
// slots-*-refused.png with a refusal under the caps).
const fs = require("fs");
const os = require("os");
const path = require("path");
const { start, BOX, PHONE, until, sleep } = require("./appslib");

const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 860 }, deviceScaleFactor: 2 };
const SHOTS = process.env.SHOTS || fs.mkdtempSync(path.join(os.tmpdir(), "herd-slots-"));

(async () => {
  const s = await start({ el: "herd-slots.el" });
  const log = () => { try { return fs.readFileSync(path.join(s.dir, "log.jsonl"), "utf8").trim().split("\n").filter(Boolean).map((l) => JSON.parse(l).what); } catch (_) { return []; } };
  const custom = () => { try { return fs.readFileSync(path.join(s.dir, "custom.el"), "utf8"); } catch (_) { return ""; } };
  const caps = () => { try { return fs.readFileSync(path.join(s.dir, "caps.txt"), "utf8").trim(); } catch (_) { return ""; } };
  try {
    for (const where of ["desk", "phone"]) {
      const W = (n) => where + ": " + n;
      const ctx = await s.browser.newContext(where === "desk" ? DESK : PHONE);
      const page = await ctx.newPage();
      const errors = [];
      page.on("pageerror", (e) => errors.push(String(e)));
      await page.goto(BOX + "/" + s.secretOf("herd") + "/ghostherd/ui/index.html#agents");
      await page.waitForFunction(() => window.GM && window.XW);
      await page.evaluate(() => GM.showView && GM.showView("agents"));
      const drawn = await until(() => page.$(".slots .slcap"), 10000);
      check(W("the panel is drawn on the Agents tab"), drawn);
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const read = () => page.evaluate(() => {
        const cap = (f) => {
          const c = document.querySelector('.slcap[data-field="' + f + '"]');
          if (!c) return null;
          const i = c.querySelector(".slinput");
          const btns = c.querySelectorAll(".slbtn");
          return { used: c.querySelector(".slused").textContent, value: i.value, full: c.classList.contains("full"),
                   minus: !btns[0].disabled, plus: !btns[1].disabled,
                   h: Math.round(btns[0].getBoundingClientRect().height), w: Math.round(btns[0].getBoundingClientRect().width) };
        };
        const err = document.querySelector(".slots .slerr");
        return { title: (document.querySelector(".sltitle") || {}).textContent, waiting: (document.querySelector(".slqn") || {}).textContent,
                 working: cap("working"), alive: cap("alive"), err: err ? err.textContent : "",
                 queue: [...document.querySelectorAll(".slq")].map((li) => li.textContent),
                 over: document.documentElement.scrollWidth - window.innerWidth,
                 slotsRight: Math.round(document.querySelector(".slots").getBoundingClientRect().right) };
      });
      let r = await read();
      check(W("what is in use beside each cap: at work 1 of 3, up 3 of 8"),
        r.working && r.working.used === "1" && r.working.value === "3" && r.alive.used === "3" && r.alive.value === "8", r);
      check(W("the handoffs waiting: from → project, since when, in line"),
        r.waiting === "2 waiting" && r.queue.length === 2 &&
        /^lead-argocd → parker.*waiting 4m · next$/.test(r.queue[0]) &&
        /^lead-argocd → genesis2 handoffs.*waiting 4m · 1 ahead$/.test(r.queue[1]), r.queue);
      if (where === "phone") {
        check(W("no sideways scroll; the panel inside the screen"), r.over <= 0 && r.slotsRight <= 390, r);
        check(W("- and + are thumb-sized"), r.working.h >= 34 && r.working.w >= 40, r.working);
      }
      const shot = path.join(SHOTS, "slots-" + where + ".png");
      await (await page.$(".slots")).screenshot({ path: shot });
      await page.screenshot({ path: path.join(SHOTS, "slots-" + where + "-page.png") });

      // ---- + on the cap at work: Emacs takes it, saves it, the page shows it --
      const want = 4;
      await tap('.slcap[data-field="working"] .slbtn[data-d="1"]');
      await until(async () => (await read()).working.value === String(want) && !(await page.$(".slcap.pending")), 5000);
      r = await read();
      check(W("+ sets the cap at work, and Emacs's word is what the page shows"), r.working.value === String(want), r.working);
      check(W("saved by customize-save-variable"), custom().includes("'(ghostherd-handoff-lead-max " + want + ")"), custom());
      check(W("the intent carried the number"), log().some((e) => e.intent === "handoff-caps" && e.field === "working" && e.value === String(want)));

      // ---- An out-of-range value typed: Emacs refuses it, says why ----------
      const type = async (field, text) => {
        const sel = '.slcap[data-field="' + field + '"] .slinput';
        await page.click(sel, { clickCount: 3 });
        await page.fill(sel, text);
        await page.press(sel, "Enter");
      };
      const before = custom();
      await type("alive", "99");
      await until(async () => (await read()).err, 5000);
      r = await read();
      check(W("99 up: refused by Emacs, saying why"),
        r.err === "Leads up for handoffs: a whole number from 1 to 30, not \"99\"", r.err);
      check(W("the field shows the cap as it is"), r.alive.value === "8", r.alive);
      await (await page.$(".slots")).screenshot({ path: path.join(SHOTS, "slots-" + where + "-refused.png") });
      check(W("nothing saved"), custom() === before && caps().endsWith(" 8)"), caps());
      check(W("Emacs had it as typed, and refused it"),
        log().some((e) => e.intent === "handoff-caps" && e.field === "alive" && e.value === "\"99\"") &&
        log().some((e) => e.js === "slotsRefused"));
      await type("working", "3.5");
      await until(async () => /not "3.5"/.test((await read()).err), 5000);
      r = await read();
      check(W("3.5 at work: refused"), /a whole number from 1 to 10, not "3.5"/.test(r.err) && r.working.value === String(want), r);
      await type("alive", "2");
      await until(async () => /not fewer/.test((await read()).err), 5000);
      r = await read();
      check(W("up below at work: refused"), r.err === "Leads up for handoffs: not fewer than may be at work (" + want + ")" && r.alive.value === "8", r);
      check(W("still nothing more saved"), custom() === before);

      // ---- In range, typed: taken; the error goes ----------------------------
      await type("alive", "12");
      await until(async () => (await read()).alive.value === "12" && !(await page.$(".slcap.pending")), 5000);
      r = await read();
      check(W("12 up, typed: taken and saved, the error gone"),
        r.alive.value === "12" && !r.err && custom().includes("'(ghostherd-handoff-lead-alive-max 12)"), [r, custom()]);
      // At a bound, its button is off.
      await type("working", "1");
      await until(async () => (await read()).working.value === "1", 5000);
      r = await read();
      check(W("at its least, - is off; + is on"), !r.working.minus && r.working.plus, r.working);
      // Back as it was, for the next round.
      await type("working", "3");
      await type("alive", "8");
      await until(async () => { const x = await read(); return x.working.value === "3" && x.alive.value === "8"; }, 5000);
      check(W("no errors on the page"), errors.length === 0, errors);
      await ctx.close();
    }
    console.log("screenshots: " + path.join(SHOTS, "slots-desk.png") + " " + path.join(SHOTS, "slots-phone.png"));
  } finally {
    await s.stop();
  }
  const ok = results.filter(Boolean).length;
  console.log(ok + "/" + results.length + " passed");
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
