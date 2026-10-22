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

      // ---- the default open sends the current project as a PATH under
      // `project' (so Emacs slugs it), never as `key' -- the live
      // "bad project key" bug (5c17375e): a path has a "/", which valid_key
      // refuses, so a project must not ride in `key'.
      await page.evaluate(() => GM.setContext({ sources: [], current_project: "/Users/jing/.emacs.d" }));
      await page.evaluate(() => GM.showView("memory"));
      await sleep(50);
      const w0 = intents.filter((i) => i.op === "memory-watch").pop();
      check(W("the default open sends the project as a path, not a key"),
        w0 && w0.project === "/Users/jing/.emacs.d" && w0.key === undefined, w0);

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

      // ---- Promote: the Proposals section, the Promoted filter, Suggest ----
      await page.evaluate((f) => GM.renderMemory({ key: "-src-dp", facts: f }), facts());
      await sleep(40);
      // A pending proposal appears with its facts, the target file, the diff,
      // and Approve / Reject.
      await page.evaluate(() => GM.renderProposals({ key: "-src-dp", proposals: [
        { id: "p1", project: "-src-dp", file: "CLAUDE.md", author: "claude-deploy-platform",
          status: "pending",
          facts: [{ name: "never-commit-to-main", title: "Never commit to main", gone: false, promoted: false }],
          diff: "--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@ -1,2 +1,3 @@\n a context line\n-an old rule\n+a new rule\n+- Never commit to main\n" }] }));
      await sleep(60);
      check(W("a pending proposal shows its fact, file and diff"),
        (await page.$$eval(".mprop", (es) => es.length)) === 1 &&
        /Never commit to main/.test(await page.$eval(".mpfacts", (e) => e.textContent)) &&
        /CLAUDE\.md/.test(await page.$eval(".mpinto", (e) => e.textContent)) &&
        /\+a new rule/.test(await page.$eval(".mpdiff", (e) => e.textContent)));
      // The diff is coloured by each line's FIRST character.
      const dlines = await page.$$eval(".mpdiff span", (es) => es.map((e) => ({ cls: e.className, text: e.textContent })));
      const classOf = (needle) => (dlines.find((d) => d.text.includes(needle)) || {}).cls;
      check(W("the diff colours by the first char: +/- added/removed, headers/hunks muted"),
        classOf("--- a/CLAUDE.md") === "dhead" && classOf("+++ b/CLAUDE.md") === "dhead" &&
        classOf("@@ -1,2") === "dhunk" && classOf("-an old rule") === "ddel" &&
        classOf("+a new rule") === "dadd" &&
        // an added bullet: first char + -> green, though its second char is -
        classOf("+- Never commit to main") === "dadd", dlines);
      check(W("a context line is plain (no colour span)"),
        !dlines.some((d) => d.text.includes("a context line")));
      // The colours are the theme tokens: --plus green, --minus red.
      const addC = await page.$eval(".mpdiff .dadd", (e) => getComputedStyle(e).color);
      const delC = await page.$eval(".mpdiff .ddel", (e) => getComputedStyle(e).color);
      check(W("added is the green token, removed the red (var(--plus)/var(--minus))"),
        addC === "rgb(63, 185, 80)" && delC === "rgb(248, 81, 73)", { addC, delC });
      // Approve → emit memory-proposal-approve.
      await page.click('[data-act="prop-approve"][data-id="p1"]');
      await sleep(40);
      const appr = intents.filter((i) => i.op === "memory-proposal-approve").pop();
      check(W("Approve emits the proposal id"), appr && appr.id === "p1", appr);
      // Reject → a reason box → send emits memory-proposal-reject with the reason.
      await page.click('[data-act="prop-reject"][data-id="p1"]');
      await sleep(40);
      check(W("Reject opens a reason box"), (await page.$("#mem-prej")) !== null);
      await page.fill("#mem-prej", "too specific");
      await page.click('[data-act="prop-reject-send"][data-id="p1"]');
      await sleep(40);
      const rej = intents.filter((i) => i.op === "memory-proposal-reject").pop();
      check(W("Reject emits the id and reason"),
        rej && rej.id === "p1" && rej.reason === "too specific", rej);
      // An empty proposals push clears the section.
      await page.evaluate(() => GM.renderProposals({ key: "-src-dp", proposals: [] }));
      await sleep(40);
      check(W("no pending proposals, no section"), (await page.$(".mprop")) === null);

      // The real case (the live bug): more than one proposal, and a proposal
      // with SEVERAL facts -- all of them show, each with its coloured diff.
      // Live, the facts arrived collapsed into one object (the elisp encode)
      // and the whole section rendered empty; the ERT round-trip test guards
      // that encode, this guards that the page paints the real shape.
      await page.evaluate(() => GM.renderProposals({ key: "-src-dp", proposals: [
        { id: "pA", project: "-src-dp", file: ".skillshare/extras/rules/frontend.md",
          author: "lead-deploy-platform", status: "pending",
          facts: [{ name: "tooltip-provider-on-mounted-page", title: "Tooltip provider on mounted page", gone: false, promoted: false }],
          diff: "--- a/frontend.md\n+++ b/frontend.md\n@@ -1 +1,2 @@\n ctx\n+- Every Tooltip needs a TooltipProvider\n" },
        { id: "pB", project: "-src-dp", file: "CLAUDE.md", author: "lead-deploy-platform", status: "pending",
          facts: [{ name: "branch-discipline", title: "Branch discipline", gone: false, promoted: false },
                  { name: "uv-lock-must-follow-pyproject", title: "Uv lock must follow pyproject", gone: false, promoted: false },
                  { name: "pod-env-grep-leaks-secrets", title: "Pod env grep leaks secrets", gone: false, promoted: false },
                  { name: "prefer-resume-over-drain", title: "Prefer resume over drain", gone: false, promoted: false }],
          diff: "--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@ -1 +1,2 @@\n ctx\n+## Branches and PRs\n" }] }));
      await sleep(60);
      check(W("both proposals show, the four-fact one with all four facts"),
        (await page.$$eval(".mprop", (es) => es.length)) === 2 &&
        (await page.$$eval(".mprop", (es) => {
          const multi = es.find((e) => /CLAUDE\.md/.test(e.querySelector(".mpinto").textContent));
          return multi ? multi.querySelectorAll(".mpfact").length : 0;
        })) === 4);
      check(W("each proposal paints its coloured diff"),
        await page.$$eval(".mprop", (es) => es.every((e) => !!e.querySelector(".mpdiff .dadd"))));
      await page.evaluate(() => GM.renderProposals({ key: "-src-dp", proposals: [] }));
      await sleep(40);

      // The Promoted filter: a promoted fact is hidden by default, shown when
      // toggled, and "Put back" emits memory-unpromote.
      await page.evaluate((f) => GM.renderMemory({ key: "-src-dp", facts: f.concat([
        { name: "branch-rule", title: "Branch rule", description: "now in CLAUDE.md", type: "feedback",
          author: "claude-deploy-platform", updated: new Date().toISOString(),
          body: "Use a branch.", shared: false, promoted: "CLAUDE.md 2026-10-22" }]) }), facts());
      await sleep(60);
      check(W("a promoted fact is hidden from the everyday list"),
        !/Branch rule/.test(await page.$eval("#mem-list", (e) => e.textContent)));
      await page.click("#mem-promoted");
      await sleep(60);
      check(W("the Promoted filter shows only promoted facts"),
        /Branch rule/.test(await page.$eval("#mem-list", (e) => e.textContent)) &&
        (await rows(page)) === 1, await rows(page));
      check(W("a promoted fact shows where it went"),
        /CLAUDE\.md/.test(await page.$eval(".mprom", (e) => e.textContent)));
      await page.click('.mhead[data-name="branch-rule"]');
      await sleep(40);
      await page.click('[data-act="mem-unpromote"][data-name="branch-rule"]');
      await sleep(40);
      const unp = intents.filter((i) => i.op === "memory-unpromote").pop();
      check(W("Put back emits memory-unpromote"), unp && unp.name === "branch-rule", unp);
      await page.click("#mem-promoted"); // back to the everyday list
      await sleep(40);

      // Suggest promotions → emit memory-suggest for the watched store.
      await page.click('[data-act="mem-suggest"]');
      await sleep(40);
      const sug = intents.filter((i) => i.op === "memory-suggest").pop();
      check(W("Suggest promotions emits for the store"), sug && (sug.key === "-src-dp" || sug.project), sug);

      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
