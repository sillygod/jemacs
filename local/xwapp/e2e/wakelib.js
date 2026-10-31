// What the suites of a phone that sleeps and wakes share (wake.js,
// wake-retry.js, link.js): the herd page wake.el serves, from a copy of
// this checkout's ui trees, and how a wake is told and measured.
const fs = require("fs");
const path = require("path");
const { HERD } = require("./herdlib");
const LOCAL = process.env.LOCAL || path.resolve(__dirname, "../..");

// What a stretch of TRAFFIC cost, since T0, said as a MEASURE line.
const measure = (what, t0, traffic) => {
  const files = traffic.filter((t) => /\/ui\//.test(t.path));
  const line = what + ": " + traffic.reduce((n, t) => n + t.bytes, 0) + " bytes in " + traffic.length + " answers (" +
    files.filter((t) => t.status === 200).length + " files sent, " + files.filter((t) => t.status === 304).length +
    " not modified" + (files.some((t) => t.enc === "gzip") ? ", " + files.filter((t) => t.enc === "gzip").length + " gzip'd" : "") +
    "), usable in " + (Date.now() - t0) + " ms";
  console.log("MEASURE " + line);
  return line;
};
const text = (page, sel) => page.textContent(sel).catch(() => "");
// A reload's files, as the browser asked for them again: each it had
// (asked with its ETag) not modified -- and the page itself, which this
// WebKit asks for whole on a reload, never with its ETag (measured: not
// on location.reload(), nor on Playwright's).
const fresh = (files) => files.filter((t) => t.asked).length >= 4 &&
  files.every((t) => (t.asked ? t.status === 304 : /\/index\.html$/.test(t.path)));
const pageOf = (dir) => { try { return fs.readFileSync(path.join(dir, "page.txt"), "utf8"); } catch (_) { return ""; } };
const NAME = "claude-deploy-platform";

// The ui trees the herd page is served from, this run's own copy.
function copyTree(run) {
  for (const pkg of ["ghostherd", "xwapp"]) {
    fs.cpSync(path.join(LOCAL, pkg, "ui"), path.join(run, "tree", pkg, "ui"), { recursive: true, dereference: true });
  }
}
// The herd Emacs sends, with MARKER in an agent's line; enough agents
// for the page to scroll on a phone.
function herdFile(run, marker) {
  const h = JSON.parse(JSON.stringify(HERD));
  for (let i = 0; i < 30; i++) h.agents.push(Object.assign({}, h.agents[3], { name: "grok-more-" + i }));
  h.agents.find((a) => a.name === NAME).reason = "marker " + marker;
  fs.writeFileSync(path.join(run, "herd.json"), JSON.stringify(h));
}
const shows = (page, marker) => async () => (await text(page, '.arow[data-name="' + NAME + '"]')).includes("marker " + marker);

// Before the page sleeps: its next held, asked a while ago and still
// asking, so the sleep takes that one with it, as a phone's does.
// Needs phoneEyes.
async function settled(tab, ms = 400) {
  const end = Date.now() + 10000;
  while (Date.now() < end) {
    const n = await tab.evaluate(() => ({ now: window.__nexts.now, last: window.__nexts.at[window.__nexts.at.length - 1] || 0 }));
    if (n.now === 1 && Date.now() - n.last >= ms) return true;
    await new Promise((r) => setTimeout(r, 50));
  }
  return false;
}

module.exports = { LOCAL, measure, text, fresh, pageOf, NAME, copyTree, herdFile, shows, settled };
