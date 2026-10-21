// The Memory tab: the shared-memory facts of a store, listed, filtered,
// opened to read the body as Markdown, edited and deleted (through the
// store's own code -- the intents that carry those go to Emacs), a fact
// added from outside appearing live, and a path-trick name refused.  Desk
// and phone.  Drives GM.* directly, as Emacs would push, and reads the
// intents the page emits back -- no sidecar.
const { webkit } = require("playwright");
const { phonePage } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };

const now = Date.now();
const iso = (ms) => new Date(ms).toISOString();
const PROJECTS = {
  user: { key: "user", label: "User (shared)" },
  projects: [
    { key: "-src-dp", label: "deploy-platform", project: "~/src/deploy-platform/" },
    { key: "-src-jn", label: "jnotes", project: "~/src/jnotes/" },
  ],
};
const facts = () => [
  { name: "prefers-reasoned-design", title: "Prefers reasoned design",
    description: "a researched proposal, not a menu", type: "user", author: "user",
    created: iso(now - 9e8), updated: iso(now - 3600e3),
    body: "**Why:** Jing wants the reasoning.\n\n- show the design\n- not a multiple choice", shared: true },
  { name: "never-commit-to-main", title: "Never commit to main",
    description: "always use a branch", type: "feedback", author: "claude-deploy-platform",
    created: iso(now - 8e8), updated: iso(now - 7200e3),
    body: "Never commit straight to `main`.\n\n- cut a branch\n- open a PR", shared: false },
  { name: "retry-scheduler", title: "Retry scheduler",
    description: "lives in worker.py", type: "project", author: "claude-deploy-platform",
    created: iso(now - 6e8), updated: iso(now - 600e3),
    body: "The retry scheduler lives in `worker.py`, under `schedule_retry`.", shared: false },
];

async function openMemory(page, key) {
  await page.evaluate((k) => { GM.showView("memory"); }, key);
  await page.evaluate((p) => GM.setMemoryProjects(p), PROJECTS);
  await page.evaluate((f) => GM.renderMemory({ key: "-src-dp", facts: f }), facts());
  await sleep(80);
}
const rows = (page) => page.$$eval(".mfact", (es) => es.length);

(async () => {
  const browser = await webkit.launch();
  try {
    for (const [label, ctxOpts] of [["desk", DESK], ["phone", PHONE]]) {
      const { ctx, page, intents } = await phonePage(browser, { context: ctxOpts });
      const W = (n) => label + ": " + n;
      await openMemory(page);

      // ---- list ----
      check(W("the three facts list"), (await rows(page)) === 3, await rows(page));
      const firstName = await page.$eval(".mfact .mname", (e) => e.textContent.trim());
      check(W("a fact shows its title"), firstName === "Prefers reasoned design", firstName);
      check(W("the shared fact is marked shared"),
        await page.$eval('.mfact', (e) => !!e.querySelector(".mshared")));
      check(W("a type badge shows"), await page.$eval(".mtype", (e) => e.textContent.trim().length > 0));
      check(W("the author shows"),
        /claude-deploy-platform/.test(await page.$eval(".mfact:nth-child(2) .mmeta", (e) => e.textContent)));

      // ---- filter ----
      await page.fill("#mem-filter", "retry");
      await sleep(60);
      check(W("the filter narrows to one"), (await rows(page)) === 1, await rows(page));
      check(W("the match is the retry fact"),
        /Retry scheduler/.test(await page.$eval(".mname", (e) => e.textContent)));
      await page.fill("#mem-filter", "");
      await sleep(60);

      // ---- open: body as Markdown ----
      await page.click('.mhead[data-name="retry-scheduler"]');
      await sleep(60);
      check(W("opening a fact shows its body"), await page.$(".mfact.open .mbody") !== null);
      check(W("the body renders Markdown (code)"),
        await page.$eval(".mfact.open .mbody", (e) => !!e.querySelector("code")));

      // ---- edit ----
      await page.click('[data-act="mem-edit"][data-name="retry-scheduler"]');
      await sleep(60);
      await page.fill("#mem-edit-desc", "now in scheduler.py");
      await page.selectOption("#mem-edit-type", "reference");
      await page.fill("#mem-edit-body", "Moved to `scheduler.py`.");
      await page.click('[data-act="mem-save"][data-name="retry-scheduler"]');
      await sleep(60);
      const edit = intents.filter((i) => i.op === "memory-edit").pop();
      check(W("saving emits memory-edit with the fields"),
        edit && edit.key === "-src-dp" && edit.name === "retry-scheduler" &&
        edit.description === "now in scheduler.py" && edit.type === "reference" &&
        edit.body === "Moved to `scheduler.py`.", edit);

      // ---- a shared fact's edit routes to the user store ----
      await page.click('.mhead[data-name="prefers-reasoned-design"]');
      await sleep(40);
      await page.click('[data-act="mem-edit"][data-name="prefers-reasoned-design"]');
      await sleep(40);
      await page.click('[data-act="mem-save"][data-name="prefers-reasoned-design"]');
      await sleep(40);
      const sedit = intents.filter((i) => i.op === "memory-edit").pop();
      check(W("a shared fact's edit goes to the user store"),
        sedit && sedit.name === "prefers-reasoned-design" && sedit.key === "user", sedit);

      // ---- delete (with confirm) ----
      await page.click('.mhead[data-name="never-commit-to-main"]');
      await sleep(40);
      check(W("delete asks to confirm first"),
        await page.$('[data-act="mem-delask"][data-name="never-commit-to-main"]') !== null);
      await page.click('[data-act="mem-delask"][data-name="never-commit-to-main"]');
      await sleep(40);
      check(W("a confirm prompt appears"), await page.$(".mconfirm") !== null);
      await page.click('[data-act="mem-delete"][data-name="never-commit-to-main"]');
      await sleep(40);
      const del = intents.filter((i) => i.op === "memory-delete").pop();
      check(W("confirming emits memory-delete"),
        del && del.name === "never-commit-to-main" && del.key === "-src-dp", del);

      // ---- a fact added from outside appears live ----
      await page.evaluate((f) => GM.renderMemory({ key: "-src-dp",
        facts: f.concat([{ name: "fresh-fact", title: "Fresh fact", description: "added outside",
          type: "project", author: "grok-x", updated: new Date().toISOString(),
          body: "new", shared: false }]) }), facts());
      await sleep(60);
      check(W("a fact added elsewhere appears without a reload"),
        (await rows(page)) === 4 &&
        /Fresh fact/.test(await page.$eval("#mem-list", (e) => e.textContent)), await rows(page));

      // ---- the picker switches store ----
      await page.selectOption("#mem-project", "-src-jn");
      await sleep(40);
      const watch = intents.filter((i) => i.op === "memory-watch").pop();
      check(W("the picker re-watches the chosen store"), watch && watch.key === "-src-jn", watch);

      // ---- a path-trick name is refused (the server's ValueError, surfaced) ----
      await page.evaluate(() => GM.renderMemory({ key: "-src-dp", facts: [
        { name: "../../evil", title: "../../evil", description: "crafted", type: "project",
          author: "x", updated: new Date().toISOString(), body: "b", shared: false }] }));
      await sleep(40);
      await page.click('.mhead[data-name="../../evil"]');
      await sleep(40);
      await page.click('[data-act="mem-delask"][data-name="../../evil"]');
      await sleep(40);
      await page.click('[data-act="mem-delete"][data-name="../../evil"]');
      await sleep(40);
      const bad = intents.filter((i) => i.op === "memory-delete").pop();
      // Emacs forwards it to the sidecar, which refuses the name and reports it.
      await page.evaluate(() => GM.showError("bad project key"));
      await sleep(40);
      check(W("a path-trick delete is sent verbatim and the refusal shows"),
        bad && bad.name === "../../evil" &&
        /bad project key/.test(await page.$eval("#status", (e) => e.textContent).catch(() => "")),
        bad);

      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
