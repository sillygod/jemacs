// ghostherd's herd page on a phone, served as `tailscale serve' brings
// it: files from this checkout, hello, next and intents played by the
// test -- and a herd to show, as Emacs would send it.
const fs = require("fs");
const path = require("path");

const LOCAL = process.env.LOCAL || path.resolve(__dirname, "../..");
const UIDIR = path.join(LOCAL, "ghostherd", "ui");
const XWAPP = path.join(LOCAL, "xwapp");
const SECRET = "0123456789abcdef0123456789abcdef";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// An iPhone 14/15: 390 x 844 points, three pixels each.
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };

async function phonePage(browser, opts) {
  opts = opts || {};
  const scheme = opts.scheme || "http";
  const intents = opts.intents || [];
  // Served as Emacs's own WebKit has it, with host 127.0.0.1: the desk.
  const BOX = scheme + "://" + (opts.host || "box.tail0.ts.net");
  const ctx = await browser.newContext(Object.assign({}, PHONE, opts.context || {}));
  await ctx.route("**/*", async (route) => {
    const req = route.request();
    const url = new URL(req.url());
    if (url.origin !== BOX) return route.fulfill({ status: 200, contentType: "text/plain", body: "away" });
    const parts = url.pathname.split("/").filter(Boolean);
    if (parts[0] !== SECRET) return route.fulfill({ status: 404, body: "Not Found" });
    const rest = parts.slice(1);
    if (rest[0] === "hello") return route.fulfill({ status: 204 });
    if (rest[0] === "intent") { intents.push(JSON.parse(req.postData())); return route.fulfill({ status: 204 }); }
    if (rest[0] === "next") { await sleep(1500); return route.fulfill({ status: 200, contentType: "application/json", body: "[]" }).catch(() => {}); }
    // The files Emacs serves the page by name (xwapp-serve-files): from
    // OPTS.files, a name each, as xwapp checks it; each asked for logged.
    if (rest[0] === ".files") {
      if (opts.fileLog) opts.fileLog.push(rest.slice(1).join("/"));
      const name = rest.length === 2 ? decodeURIComponent(rest[1]) : "";
      const f = opts.files && opts.files[name];
      if (!f) return route.fulfill({ status: 404, body: "Not Found" });
      const ftype = { png: "image/png", jpg: "image/jpeg" }[name.split(".").pop()] || "application/octet-stream";
      return route.fulfill({ status: 200, contentType: ftype, headers: { "Cache-Control": "private, max-age=604800, immutable" }, body: fs.readFileSync(f) });
    }
    // Another set (xwapp-serve-files :as): documents, from OPTS.sets[SET],
    // as xwapp serves a :sandbox set -- see herd-docs-sandbox.js for the real one.
    if (/^\.[a-z][a-z0-9-]*$/.test(rest[0] || "") && rest[0] !== ".files") {
      const set = rest[0].slice(1);
      const name = rest.length === 2 ? decodeURIComponent(rest[1]) : "";
      if (opts.fileLog) opts.fileLog.push(set + "/" + name);
      const body = opts.sets && opts.sets[set] && opts.sets[set][name];
      if (body === undefined) return route.fulfill({ status: 404, body: "Not Found" });
      return route.fulfill({ status: 200, contentType: "text/html; charset=utf-8",
        headers: { "Content-Security-Policy": "sandbox; default-src 'none'; style-src 'unsafe-inline'; img-src data:" }, body });
    }
    const base = rest[0] === "xwapp" ? path.join(XWAPP, "ui") : rest[0] === "ghostherd" ? UIDIR : null;
    const file = base && path.join(base, ...rest.slice(2));
    if (!file || !fs.existsSync(file)) return route.fulfill({ status: 404, body: "Not Found" });
    const type = { html: "text/html", js: "text/javascript", css: "text/css" }[file.split(".").pop()] || "application/octet-stream";
    return route.fulfill({ status: 200, contentType: type + "; charset=utf-8", body: fs.readFileSync(file) });
  });
  const page = await ctx.newPage();
  const errors = [];
  page.on("pageerror", (e) => errors.push(String(e)));
  await page.goto(BOX + "/" + SECRET + "/ghostherd/ui/index.html" + (opts.hash || ""));
  await page.waitForFunction(() => window.GM && window.XW);
  return { ctx, page, errors, intents };
}

// ---- A herd: two projects at work, four agents, the usage ----

const now = Date.now();
const DP = "~/src/deploy-platform/";
const JN = "~/src/jnotes/";
const BB = "https://bitbucket.org/unno/deploy-platform/pull-requests/";
const agent = (name, extra) => Object.assign({
  name, kind: "claude", state: "idle", project: DP, role: "", reason: "", notes: "", backend: "tmux", detached: false, manual: false, login: false, url: "",
  continueAt: 0, continueStopped: false, startedBy: "",
  since: now - 2 * 3600e3, progress: null, said: null, saidAt: null, conversation: null, work: false, links: false }, extra || {});
const task = (id, name, status, color) => ({ id, name, status, color: color || "#4194f6", url: "https://app.clickup.com/t/" + id,
  error: null, loading: false, done: false });
const prRef = (id, state, title) => ({ id, title, state, draft: false, url: BB + id, error: null });
const WORK = [
  { project: DP, root: DP, branch: "fix/86ph1a-retry-tooltip", pending: false, error: null,
    pr: { id: 378, title: "Retry tooltip", state: "MERGED", draft: false, url: BB + "378" },
    tasks: [task("86ph1a", "[Phase 1a] 前端：重試次數與逾時的提示", "wait for testing"),
            task("86ds01", "deploy platform 部署失敗時的重試", "in progress")] },
  { project: JN, root: JN, branch: "feat/notes-sync", pending: false, error: null,
    pr: { id: 12, title: "Notes sync", state: "OPEN", draft: false, url: "https://bitbucket.org/unno/jnotes/pull-requests/12" },
    tasks: [task("86jn01", "Sync notes across devices, the phone's and the desk's, without losing a line of either", "in progress")] },
];
const AGENTS = [
  agent("lead-deploy-platform", { role: "lead", notes: "lead: plans the work, hands it out", reason: "done, but you are watching",
    links: { root: DP, prs: [prRef(371, "OPEN", "Release"), prRef(360, "MERGED", "Old")],
             tasks: [task("86p0a", "[Phase 0] 補上 release 流程", "Open", "#87909e"),
                     task("86p0b", "[Phase 0] 開發環境不送事件", "Open", "#87909e"),
                     task("86p1a", "[Phase 1a] 後端：重試排程", "testing", "#f9a03f"),
                     task("86p1b", "[Phase 1b] 合併重複的失敗通知", "in progress")] } }),
  agent("claude-deploy-platform", { state: "working", notes: "implementer", since: now - 300e3,
    reason: "Running the test suite for the retry scheduler" }),
  agent("qa-deploy-platform", { kind: "agy", state: "blocked", notes: "reviewer", since: now - 60e3,
    reason: "Command — Requesting permission for:",
    prompt: { question: "Requesting permission for:\nnpm test -- --runInBand\nin ~/src/deploy-platform\nRun this command?",
              options: [{ n: 1, label: "Yes, run command" },
                        { n: 2, label: "Yes, and always allow in this conversation for commands that start with" },
                        { n: 3, label: "Yes, and always allow for commands that start with 'npm' (Persist to" },
                        { n: 4, label: "No, cancel" }] } }),
  agent("grok-jnotes", { kind: "grok", project: JN, since: now - 86400e3 }),
];
const USAGE = [
  { kind: "claude", at: now - 60e3, windows: [{ label: "5 hours", used: 42, resets: now + 2 * 3600e3 }, { label: "week", used: 71, resets: now + 3 * 86400e3 }] },
  { kind: "grok", at: now - 60e3, windows: [{ label: "day", used: 12, resets: now + 8 * 3600e3 }] },
];
const HERD = { agents: AGENTS, asks: [], hooks: { status: {}, list: [] }, usage: USAGE, kinds: ["claude", "agy", "grok"], work: WORK };

module.exports = { phonePage, PHONE, HERD, sleep };
