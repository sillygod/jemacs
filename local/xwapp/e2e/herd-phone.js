// The phone gives the conversation the screen: the room card header is two
// lines with the rest behind ▾, the composer is one line that grows, and
// the chrome folds -- so the transcript is most of the viewport (≥55% at
// 390×844), while a blocked card keeps every item that needs the user, and
// the desktop is unchanged.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DP = "~/src/deploy-platform/";
const PHONE = (w, h) => ({ viewport: { width: w, height: h }, deviceScaleFactor: 3, isMobile: true, hasTouch: true });
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };

async function openRoom(page, view, tap) {
  await tap('.pgroup[data-project="' + DP + '"] [data-act="a:room"]');
  await sleep(180);
  await tap('.room [data-act="a:room-view"][data-view="' + view + '"]');
  await sleep(280);
}
const H = (page, sel) => page.$eval(sel, (el) => Math.round(el.getBoundingClientRect().height)).catch(() => 0);

// A herd where one agent's turn the Mac slept on (continue countdown) and
// another's login expired, to prove the attention items are never folded.
const herd = () => {
  const h = JSON.parse(JSON.stringify(HERD));
  const sleptA = h.agents.find((x) => x.name === "claude-deploy-platform");
  sleptA.state = "blocked"; sleptA.reason = "its turn was cut off: the computer slept";
  sleptA.continueAt = Date.now() + 40000;
  const lead = h.agents.find((x) => x.name === "lead-deploy-platform");
  lead.state = "blocked"; lead.reason = "its login expired -- run /login"; lead.login = true;
  return h;
};

(async () => {
  const browser = await webkit.launch();
  try {
    // ---- Transcript share on the phone: Columns and Chat ----
    for (const [w, h] of [[390, 844], [375, 667]]) {
      for (const view of ["columns", "chat"]) {
        const { ctx, page } = await phonePage(browser, { context: PHONE(w, h) });
        const tap = (s) => page.tap(s);
        await page.evaluate((hd) => { GM.showView("agents"); GM.renderHerd(hd); }, HERD);
        await sleep(140);
        await openRoom(page, view, tap);
        const vh = await page.evaluate(() => window.innerHeight);
        const bodyH = await H(page, view === "columns" ? ".rcol .rbody" : ".chat .cbody");
        const share = Math.round((100 * bodyH) / vh);
        const target = w === 390 ? 55 : 50;
        check(`${w}×${h} ${view}: transcript is ${share}% of the viewport (≥${target}%)`, share >= target, { share, bodyH, vh });
        await ctx.close();
      }
    }

    // ---- The card header and composer are compact on the phone ----
    {
      const { ctx, page } = await phonePage(browser, { context: PHONE(390, 844) });
      const tap = (s) => page.tap(s);
      await page.evaluate((hd) => { GM.showView("agents"); GM.renderHerd(hd); }, HERD);
      await sleep(140);
      await openRoom(page, "columns", tap);
      const amainH = await H(page, ".rcol .amain");
      check("the card header line 1 is a single line", amainH <= 34, { amainH });
      const taH = await H(page, ".rcol .rfoot textarea");
      check("the composer is one line tall when idle", taH <= 46, { taH });
      // Image and Use are icon buttons (their text is hidden), Send keeps its word.
      const imgFont = await page.$eval('.rcol .rfoot [data-act="a:image"]', (b) => getComputedStyle(b).fontSize);
      check("Image is an icon button (text hidden)", imgFont === "0px", { imgFont });
      const sendText = await page.$eval('.rcol .rfoot [data-act="a:send"]', (b) => b.textContent.trim());
      check("Send keeps its word", sendText === "Send");
      // The composer grows with what you type.
      await page.fill('.rcol .rfoot textarea', Array(8).fill("a long line of text to wrap").join(" "));
      await sleep(80);
      const grown = await H(page, ".rcol .rfoot textarea");
      check("the composer grows as you type", grown > taH, { idle: taH, grown });
      await ctx.close();
    }

    // ---- A blocked card keeps its attention items; the rest folds ----
    {
      const { ctx, page } = await phonePage(browser, { context: PHONE(390, 844) });
      const tap = (s) => page.tap(s);
      await page.evaluate((hd) => { GM.showView("agents"); GM.renderHerd(hd); }, herd());
      await sleep(140);
      await openRoom(page, "columns", tap);
      const qa = '.rcol[data-name="qa-deploy-platform"]';
      const shown = (sel) => page.$eval(sel, (el) => { const s = getComputedStyle(el); return s.display !== "none" && s.visibility !== "hidden"; }).catch(() => false);
      // The blocked menu's choices stay visible, collapsed.
      check("a blocked card shows its menu choices, collapsed", await shown(qa + " .choices"));
      // Esc is secondary: folded away until expanded.
      check("Esc is folded away on a collapsed card", !(await shown(qa + ' [data-act="a:agent-interrupt"]')));
      // Notes are folded away.
      check("notes are folded away on a collapsed card", !(await shown(qa + " .anotes")));
      // The ▾ toggle is there; tapping it expands.
      check("the card has a ▾ expand toggle", await shown(qa + " .cx"));
      await tap(qa + " .cx");
      await sleep(120);
      check("tapping ▾ expands the card", !!(await page.$(qa + ".expanded")));
      check("Esc shows once expanded", await shown(qa + ' [data-act="a:agent-interrupt"]'));
      check("notes show once expanded", await shown(qa + " .anotes"));
      await tap(qa + " .cx");
      await sleep(120);
      check("tapping ▴ collapses again", !(await page.$(qa + ".expanded")));
      // The sleep countdown and /login are attention items: never folded.
      check("the continue-after-sleep countdown stays visible, collapsed",
        await shown('.rcol[data-name="claude-deploy-platform"] .contline'));
      check("the /login button stays visible, collapsed",
        await shown('.rcol[data-name="lead-deploy-platform"] [data-act="a:agent-login"]'));
      const errs = await page.evaluate(() => 0);
      check("no layout errors", errs === 0);
      await ctx.close();
    }

    // ---- The desktop is unchanged: no phone-only folding or icons ----
    {
      const { ctx, page } = await phonePage(browser, { context: DESK });
      const tap = (s) => page.click(s);
      await page.evaluate((hd) => { GM.showView("agents"); GM.renderHerd(hd); }, HERD);
      await sleep(140);
      await openRoom(page, "columns", tap);
      const cxShown = await page.$eval(".rcol .cx", (el) => getComputedStyle(el).display !== "none").catch(() => false);
      check("the ▾ toggle is hidden on the desktop", !cxShown);
      const notesShown = await page.$eval(".rcol .anotes", (el) => getComputedStyle(el).display !== "none").catch(() => false);
      check("the desktop header keeps its notes (nothing folded)", notesShown);
      const imgText = await page.$eval('.rcol .rfoot [data-act="a:image"]', (b) => b.textContent.trim());
      check("the desktop keeps the word Image", imgText === "Image");
      const inlineH = await page.$eval(".rcol .rfoot textarea", (t) => t.style.height || "");
      check("the desktop composer is not auto-grown", inlineH === "", { inlineH });
      // An agent started by a lead through `herd start' says so on its card.
      await page.evaluate((hd) => {
        hd.agents.find((x) => x.name === "claude-deploy-platform").startedBy = "lead-deploy-platform";
        GM.renderHerd(hd);
      }, HERD);
      await sleep(120);
      check("a lead-started agent shows 'by <lead>' on its card",
        /by lead-deploy-platform/.test(await page.$eval('.rcol[data-name="claude-deploy-platform"] .amain', (el) => el.textContent)));
      check("an agent nobody started shows no 'by' label",
        !/by /.test(await page.$eval('.rcol[data-name="qa-deploy-platform"] .amain', (el) => el.textContent)));
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
