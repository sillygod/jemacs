// clickup-view: a task's assignees and dates changed from the page, on
// a phone and at the desk, with ClickUp answered by the e2e Emacs (which
// keeps the task as ClickUp would and logs every PUT).
const path = require("path");
const fs = require("fs");
const { start, watch, measure, sideways, until, sleep, log, BOX, PHONE } = require("./appslib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const OUT = path.join(__dirname, "shots", "cu-edit");
fs.mkdirSync(OUT, { recursive: true });
const puts = () => log().filter((w) => w && w.put).map((w) => w.body);
const intents = (op) => log().filter((w) => w && w.intent && w.app === "clickup-view" && w.intent.op === op).map((w) => w.intent);
const text = (page, sel) => page.textContent(sel).catch(() => "");
const ymd = (d) => d.getFullYear() + "-" + String(d.getMonth() + 1).padStart(2, "0") + "-" + String(d.getDate()).padStart(2, "0");
const DUE = Number(require("./fixtures/cu-task.json").due_date);

(async () => {
  for (const kind of ["phone", "desk"]) {
    const s = await start();
    try {
      const ctx = await s.browser.newContext(kind === "phone" ? PHONE : { viewport: { width: 1280, height: 860 } });
      await ctx.route(/^https:\/\//, (r) => r.fulfill({ status: 200, contentType: "text/plain", body: "away" }));
      const page = await ctx.newPage();
      const errors = [];
      watch(page, errors);
      const tap = (sel) => (kind === "phone" ? page.tap(sel) : page.click(sel));
      const K = (n) => kind + ": " + n;
      await page.goto(BOX + "/open/clickup-view#task=86abc123");
      await until(async () => /Payment webhook is dropped/.test(await text(page, "#app")), 10000);
      await sleep(300);
      check(K("assignees and both dates are controls"), await page.$('.edit-btn[data-pop="assignees"]') && await page.$('.edit-btn[data-pop="start"]') && await page.$('.edit-btn[data-pop="due"]'));
      const oneLine = async () => page.$$eval(".edit-btn", (bs) => bs.map((b) => [b.getAttribute("data-pop"), Math.round(b.getBoundingClientRect().height)]));
      const tall = kind === "phone" ? 40 : 32;
      check(K("each control one line: name and chevron, Start and its date"), (await oneLine()).every((h) => h[1] <= tall), await oneLine());
      check(K("the due date as it was"), /Due Oct \d+/.test(await text(page, '.edit-btn[data-pop="due"]')) && /Start\s+—/.test(await text(page, '.edit-btn[data-pop="start"]')),
        [await text(page, '.edit-btn[data-pop="due"]'), await text(page, '.edit-btn[data-pop="start"]')]);

      // ---- Assignees ----
      await tap('.edit-btn[data-pop="assignees"]');
      await until(async () => !!(await page.$(".people-menu .person-item")), 5000);
      check(K("who can be given it, asked of Emacs once"), intents("members").length === 1 && intents("members")[0].list_id === "L1", intents("members"));
      const order = await page.$$eval(".people-menu .person-item .mi-name", (xs) => xs.map((x) => x.textContent));
      check(K("who has it first, then everyone by name"), /^Ada Lovelace/.test(order[0]) && order[1] === "Bob Chen" && order.length === 10, order);
      check(K("a box to find someone, the list being long"), !!(await page.$("#member-filter")));
      if (kind === "desk") check(K("the box has the keys"), await page.evaluate(() => document.activeElement && document.activeElement.id === "member-filter"));
      else check(K("no keyboard thrown up on the phone"), await page.evaluate(() => !document.activeElement || document.activeElement.id !== "member-filter"));
      await page.fill("#member-filter", "jing");
      await sleep(100);
      const found = await page.$$eval(".people-menu .person-item .mi-name", (xs) => xs.map((x) => x.textContent));
      check(K("it finds"), found.length === 1 && found[0] === "Jing Ye", found);
      await tap('.person-item[data-uid="109"]');
      await sleep(50);
      check(K("while it goes: that one busy"), await page.$eval('.person-item[data-uid="109"]', (b) => b.disabled).catch(() => false));
      await until(async () => /Jing Ye/.test(await text(page, '.edit-btn[data-pop="assignees"]')), 4000);
      check(K("assigned: ClickUp told add, the page shows them"), JSON.stringify(puts().slice(-1)[0]) === JSON.stringify({ assignees: { add: [109], rem: [] } }) &&
        /Ada Lovelace.*Jing Ye|Jing Ye.*Ada Lovelace/.test(await text(page, '.edit-btn[data-pop="assignees"]')), puts());
      check(K("the menu stays, ticked, the search kept"), !!(await page.$('.person-item.on[data-uid="109"] .check')) && (await page.inputValue("#member-filter")) === "jing");
      check(K("Emacs says so"), /Assigned to Ada Lovelace, Jing Ye/.test(await text(page, "#status")), await text(page, "#status"));
      await page.fill("#member-filter", "");
      await sleep(100);
      await tap('.person-item[data-uid="7"]');
      await until(async () => !/Ada/.test(await text(page, '.edit-btn[data-pop="assignees"]')), 4000);
      check(K("unassigned: rem, gone from the page"), JSON.stringify(puts().slice(-1)[0]) === JSON.stringify({ assignees: { add: [], rem: [7] } }), puts().slice(-1));
      const after = await page.$$eval(".people-menu .person-item .mi-name", (xs) => xs.map((x) => x.textContent));
      check(K("now who has it on top, then you, then the rest"), after[0] === "Jing Ye" && /^Ada Lovelace/.test(after[1]) && after[2] === "Bob Chen", after);
      let m = await measure(page);
      check(K("one line still, two people in it"), (await oneLine()).every((h) => h[1] <= tall), await oneLine());
      check(K("the people fit"), !m.pageScroll && !m.past.length && (kind === "desk" || (!m.small.length && !m.smallFonts.length)), m);
      await page.screenshot({ path: path.join(OUT, kind + "-people.png") });
      // Away: closed.
      await tap(".task-head h1");
      await sleep(100);
      check(K("a tap elsewhere closes it"), !(await page.$(".people-menu")));
      if (kind === "phone") {
        await tap('.edit-btn[data-pop="assignees"]');
        await until(async () => !!(await page.$("#member-filter")), 3000);
        check(K("opened again, its people known: still no keyboard"), await page.evaluate(() => !document.activeElement || document.activeElement.id !== "member-filter"));
        await tap(".task-head h1");
        await sleep(100);
      }

      // ---- Dates ----
      await tap('.edit-btn[data-pop="due"]');
      await until(async () => !!(await page.$("#date-input")), 3000);
      check(K("the due date's editor, on its day"), (await page.inputValue("#date-input")) === ymd(new Date(DUE)), await page.inputValue("#date-input"));
      check(K("Today, Tomorrow, In a week: one row"), await page.$$eval('[data-act="pick-date"]', (bs) => new Set(bs.map((b) => Math.round(b.getBoundingClientRect().top))).size === 1));
      m = await measure(page);
      const w = await sideways(page);
      check(K("it fits"), !m.pageScroll && !w.sideways.length && !w.past.length && (kind === "desk" || (!m.small.length && !m.smallFonts.length)), [m, w]);
      await page.screenshot({ path: path.join(OUT, kind + "-due.png") });
      await tap('[data-act="pick-date"]:nth-child(2)'); // Tomorrow
      const now = new Date();
      const was = new Date(DUE);
      const tomorrow = new Date(now.getFullYear(), now.getMonth(), now.getDate() + 1, was.getHours(), was.getMinutes()).getTime();
      await until(async () => puts().length >= 3, 3000);
      check(K("Tomorrow: the day, the time it had"), JSON.stringify(puts().slice(-1)[0]) === JSON.stringify({ due_date: tomorrow, due_date_time: false }), [puts().slice(-1)[0], tomorrow]);
      await until(async () => !/…/.test(await text(page, '.edit-btn[data-pop="due"]')), 3000);
      const dueNow = await text(page, '.edit-btn[data-pop="due"]');
      check(K("shown"), dueNow.indexOf(new Date(tomorrow).toLocaleDateString("en-US", { month: "short", day: "numeric" })) !== -1 && !/overdue/.test(dueNow), dueNow);

      await tap('.edit-btn[data-pop="start"]');
      await until(async () => !!(await page.$("#date-input")), 3000);
      check(K("no start yet: empty, nothing to clear"), (await page.inputValue("#date-input")) === "" && !(await page.$('[data-act="clear-date"]')));
      await page.fill("#date-input", "2026-10-20");
      await tap('[data-act="save-date"]');
      await until(async () => puts().length >= 4, 3000);
      check(K("a start picked: 4 in the morning there, as ClickUp keeps a day"), JSON.stringify(puts().slice(-1)[0]) === JSON.stringify({ start_date: new Date(2026, 9, 20, 4, 0).getTime(), start_date_time: false }), puts().slice(-1));
      await until(async () => /Start Oct 20/.test(await text(page, '.edit-btn[data-pop="start"]')), 3000);
      check(K("shown too"), /Start Oct 20/.test(await text(page, '.edit-btn[data-pop="start"]')));

      await tap('.edit-btn[data-pop="start"]');
      await until(async () => !!(await page.$('[data-act="clear-date"]')), 3000);
      await tap('[data-act="clear-date"]');
      await until(async () => puts().length >= 5, 3000);
      check(K("Clear: null"), JSON.stringify(puts().slice(-1)[0]) === JSON.stringify({ start_date: null, start_date_time: false }), puts().slice(-1));
      await until(async () => /Start\s+—/.test(await text(page, '.edit-btn[data-pop="start"]')), 3000);
      check(K("cleared on the page"), /Start\s+—/.test(await text(page, '.edit-btn[data-pop="start"]')));

      // The same day again: nothing sent.
      const before = puts().length;
      await tap('.edit-btn[data-pop="due"]');
      await until(async () => !!(await page.$("#date-input")), 3000);
      await tap('[data-act="save-date"]');
      await sleep(500);
      check(K("the same day saved: nothing goes"), puts().length === before && !(await page.$("#date-input")));

      if (kind === "desk") {
        await page.keyboard.press("d");
        await until(async () => !!(await page.$("#date-input")), 2000);
        check(K("d: the due date"), await page.evaluate(() => document.activeElement && document.activeElement.id === "date-input"));
        await page.keyboard.press("Escape");
        await sleep(100);
        check(K("Escape closes it, back on its button"), !(await page.$("#date-input")) && await page.evaluate(() => document.activeElement && document.activeElement.getAttribute("data-pop") === "due"));
        await page.keyboard.press("a");
        await until(async () => !!(await page.$("#member-filter")), 2000);
        await page.keyboard.type("bob");
        await page.keyboard.press("Enter");
        await until(async () => /Bob Chen/.test(await text(page, '.edit-btn[data-pop="assignees"]')), 3000);
        await page.keyboard.press("Escape"); // from the box: closed
        await sleep(100);
        await page.keyboard.press("a");
        await until(async () => page.evaluate(() => document.activeElement && document.activeElement.id === "member-filter"), 2000);
        check(K("opened again, the search empty"), (await page.inputValue("#member-filter")) === "");
        await page.keyboard.press("ArrowDown");
        await page.keyboard.press("ArrowDown");
        const on = await page.evaluate(() => document.activeElement && document.activeElement.getAttribute("data-uid"));
        await page.keyboard.press("Escape");
        await sleep(100);
        check(K("arrows walk the people; Escape from one closes, back on the button"), on === "109" && !(await page.$(".people-menu")) &&
          await page.evaluate(() => document.activeElement && document.activeElement.getAttribute("data-pop") === "assignees"), on);
        check(K("the people asked for once, opened twice"), intents("members").length === 1, intents("members"));
        check(K("a, a name, Enter: assigned"), JSON.stringify(puts().slice(-1)[0]) === JSON.stringify({ assignees: { add: [101], rem: [] } }), puts().slice(-1));
      }
      check(K("no errors in the page"), !errors.length, errors);
      await ctx.close();
    } finally {
      await s.stop();
    }
  }
  const bad = results.filter((r) => !r).length;
  console.log(bad ? bad + " failed of " + results.length : "all " + results.length + " passed");
  process.exit(bad ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
