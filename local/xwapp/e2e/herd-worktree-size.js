// A worktree agent's size chip says where the space is: its biggest
// parts, a nested dependency like server/.venv by its path, then the rest.
// What the herd made as a copy-on-write clone is said to have been made
// so -- du counts it in full and cannot see what it still shares, so it is
// never said to be shared -- and a dependency the herd linked is said to
// take no space.  Unmeasured parts: the plain title, as before.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const MB = 1024 * 1024;
const WT = "~/.ghostherd/worktrees/jnotes/rd-tabs/";

const herd = (sizeParts, extra) => {
  const h = JSON.parse(JSON.stringify(HERD));
  const a = h.agents.find((x) => x.name === "claude-deploy-platform");
  a.work = Object.assign({ root: WT, branch: "rd-tabs", base: "main", pending: false, error: null, pr: null, tasks: [],
    ahead: 2, size: 800 * MB, sizeParts, deps: false }, extra || {});
  return h;
};
const chip = (page) => page.$eval('.arow[data-name="claude-deploy-platform"] .wchip.size',
  (c) => ({ text: c.textContent, title: c.getAttribute("title") })).catch(() => null);

(async () => {
  const browser = await webkit.launch();
  try {
    const { page, errors } = await phonePage(browser);
    const show = async (h) => { await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, h); await sleep(80); };

    // rd-tabs as it was: node_modules cloned, then reinstalled by the agent;
    // server/.venv the agent's own; a model its tests downloaded.
    await show(herd({ parts: [{ path: "node_modules", bytes: 480 * MB, how: "clone" },
                              { path: "server/.venv", bytes: 220 * MB, how: "uv" },
                              { path: "server/data", bytes: 91 * MB, how: "" }],
                      rest: 9 * MB, linked: [] }));
    let c = await chip(page);
    const lines = c ? c.title.split("\n") : [];
    check("the chip still says the size", c && c.text === "800 MB", c);
    check("its title names the biggest parts, a nested venv by its path, then the rest",
      lines[0] === "worktree on disk: node_modules 480 MB · server/.venv 220 MB · server/data 91 MB · rest 9.0 MB", lines);
    check("what the herd made as a clone is said to have been made so",
      lines[1] === "node_modules was cloned from the main checkout's; server/.venv was made by uv, cloned from its cache. " +
        "du counts a copy-on-write clone in full, though what is unchanged since may still share that disk.", lines[1]);
    check("never claimed shared", c && !/\bshared\b|\bis shared\b|\bfree\b/.test(c.title), c && c.title);
    check("nothing linked, nothing said of links", lines.length === 2, lines);

    // A venv the herd linked: no space here, and said so; nothing made.
    await show(herd({ parts: [{ path: "node_modules", bytes: 480 * MB, how: "" }], rest: 4 * MB, linked: ["server/.venv"] }));
    c = await chip(page);
    check("a linked dependency is said to take no space",
      c && c.title === "worktree on disk: node_modules 480 MB · rest 4.0 MB\nserver/.venv is a link to the main checkout's: no space here.", c && c.title);

    // Nothing left over: no rest.
    await show(herd({ parts: [{ path: "node_modules", bytes: 800 * MB, how: "" }], rest: 0, linked: [] }));
    c = await chip(page);
    check("no rest when there is none", c && c.title === "worktree on disk: node_modules 800 MB", c && c.title);

    // A path is text, never markup.
    await show(herd({ parts: [{ path: 'odd "dir" <b>', bytes: 500 * MB, how: "" }], rest: 300 * MB, linked: [] }));
    c = await chip(page);
    check("a path is shown as text", c && c.title === 'worktree on disk: odd "dir" <b> 500 MB · rest 300 MB', c && c.title);

    // Unmeasured parts (an older Emacs, or before the first du): as before.
    await show(herd(false));
    c = await chip(page);
    check("unmeasured: the plain title", c && c.text === "800 MB" && c.title === "worktree on disk", c);
    await show(herd(false, { size: false }));
    check("no size: no chip", (await chip(page)) === null);

    check("no page errors", errors.length === 0, errors);
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
