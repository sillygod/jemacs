// pr-view and clickup-view on a phone, served by a real Emacs
// (apps.el) with the forges answered from fixtures, through an HTTP
// proxy that does what `tailscale serve' does: the name kept, your
// login put in.  The browser follows Emacs's redirects itself.  Each
// run writes to a directory of its own, removed after; KEEP=1 keeps it.
const { webkit } = require("playwright");
const fs = require("fs");
const os = require("os");
const net = require("net");
const http = require("http");
const path = require("path");
const { spawn } = require("child_process");

const LOCAL = process.env.LOCAL || path.resolve(__dirname, "../..");
const EMACS = process.env.EMACS || "emacs";
let E = null; // this run's directory
const BOX = "http://box.tail0.ts.net";
const PHONE = { viewport: { width: 390, height: 844 }, deviceScaleFactor: 3, isMobile: true, hasTouch: true };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const freePort = () => new Promise((r) => { const s = net.createServer().listen(0, "127.0.0.1", () => { const p = s.address().port; s.close(() => r(p)); }); });
async function until(pred, ms = 8000) {
  const end = Date.now() + ms;
  while (Date.now() < end) { try { if (await pred()) return true; } catch (_) {} await sleep(100); }
  return false;
}
const log = () => { try { return fs.readFileSync(path.join(E, "log.jsonl"), "utf8").trim().split("\n").filter(Boolean).map(JSON.parse).map((e) => e.what); } catch (_) { return []; } };

// OPTS.el: the Emacs to start, apps.el by default; OPTS.desk: this
// machine's browser reaches Emacs's loopback port straight, as it does.
async function start(opts) {
  opts = opts || {};
  const run = E = fs.mkdtempSync(path.join(os.tmpdir(), "xwapp-e2e-"));
  if (opts.setup) opts.setup(run);
  const port = await freePort();
  const emacs = spawn(EMACS, ["-Q", "--batch", "-l", path.join(__dirname, opts.el || "apps.el")],
    { env: Object.assign({}, process.env, { E2E_DIR: E, E2E_PORT: String(port), E2E_LOCAL: LOCAL }), stdio: ["ignore", "ignore", "pipe"] });
  let err = "";
  emacs.stderr.on("data", (d) => { err += d; });
  if (!(await until(() => fs.existsSync(path.join(E, "urls.json")), 15000))) {
    emacs.kill();
    throw new Error("Emacs did not start: " + err.slice(-800));
  }
  await sleep(200);
  const urls = JSON.parse(fs.readFileSync(path.join(E, "urls.json"), "utf8"));
  // Away from the tailnet: the forge, ClickUp, any other site.  What
  // stays is forwarded to the socket, as `tailscale serve unix:' does --
  // not the loopback port, which now answers this machine's browser alone.
  // Faithful to serve: the Host is rewritten to "localhost" (a unix
  // target has no network host), while the browser's Origin is left as
  // the tailnet host -- so the page's own calls carry it.
  const sock = path.join(E, "remote.sock");
  const away = [];
  const proxy = http.createServer((req, res) => {
    const u = new URL(req.url);
    if (u.host !== "box.tail0.ts.net") { away.push(req.url); res.writeHead(200, { "content-type": "text/plain" }); return res.end("away"); }
    const headers = Object.assign({}, req.headers, { host: "localhost", "tailscale-user-login": "me@example.com" });
    delete headers["proxy-connection"];
    const up = http.request({ socketPath: sock, path: u.pathname + u.search, method: req.method, headers }, (r) => {
      res.writeHead(r.statusCode, r.headers);
      r.pipe(res);
    });
    up.on("error", () => { res.writeHead(502); res.end(); });
    req.pipe(up);
  });
  await new Promise((r) => proxy.listen(0, "127.0.0.1", r));
  // No browser -- not installed, say -- and nothing is left running.
  const browser = await webkit.launch({ proxy: Object.assign({ server: "http://127.0.0.1:" + proxy.address().port },
                                                           opts.desk ? { bypass: "127.0.0.1" } : {}) }).catch((e) => {
    proxy.close();
    emacs.kill();
    throw e;
  });
  const stop = async () => {
    await browser.close().catch(() => {});
    proxy.close();
    emacs.kill();
    await sleep(200);
    if (process.env.KEEP) console.log("kept " + run);
    else fs.rmSync(run, { recursive: true, force: true });
  };
  return { browser, urls, away, stop, dir: run, port, secretOf: (app) => urls[app].split("/")[3] };
}

function watch(page, errors) {
  page.on("pageerror", (e) => errors.push(String(e)));
  page.on("console", (m) => { if (m.type() === "error" && !/status of 410/.test(m.text())) errors.push(m.text()); });
}

// What does not fit a phone: the page scrolling sideways, things past
// the right edge that do not sit in something meant to scroll, and
// controls under a finger's 32 points.
async function measure(page) {
  return page.evaluate((NAMES) => {
    const process = { env: { NAMES } };
    const W = document.documentElement.clientWidth;
    const visible = (e) => { const r = e.getBoundingClientRect(); const s = getComputedStyle(e);
      return r.width > 0 && r.height > 0 && s.visibility !== "hidden" && s.display !== "none"; };
    const scrolls = (e) => { for (let p = e.parentElement; p && p !== document.body; p = p.parentElement) {
      const s = getComputedStyle(p); if (/(auto|scroll)/.test(s.overflowX) && p.scrollWidth > p.clientWidth + 1) return true; } return false; };
    const name = (e) => e.tagName.toLowerCase() + (e.id ? "#" + e.id : "") + (e.classList.length ? "." + [...e.classList].join(".") : "");
    const ctl = [...document.querySelectorAll("button, a[href], select, input:not([type=file]):not([type=checkbox]), textarea, [role=button], summary")].filter(visible);
    return {
      width: W,
      pageScroll: document.documentElement.scrollWidth > W + 1 || document.body.scrollWidth > W + 1,
      past: [...document.querySelectorAll("body *")].filter((e) => visible(e) && e.getBoundingClientRect().right > W + 1 && !scrolls(e)).map(name).slice(0, 10),
      small: ctl.filter((e) => e.getBoundingClientRect().height < 32 && !e.closest(".md, .desc, .comment-body, .cbody"))
        .map((e) => (process.env.NAMES ? name(e) + " " : "") + (e.textContent || e.getAttribute("title") || e.tagName).trim().replace(/\s+/g, " ").slice(0, 16) + ":" + Math.round(e.getBoundingClientRect().height)),
      smallFonts: [...document.querySelectorAll("input:not([type=checkbox]):not([type=radio]), textarea, select")].filter(visible).filter((e) => parseFloat(getComputedStyle(e).fontSize) < 16).map(name),
      top: (() => { const t = document.querySelector("header.top, .top"); return t ? Math.round(t.getBoundingClientRect().height) : null; })(),
      fonts: [...document.querySelectorAll("input, textarea, select")].filter(visible).map((e) => parseFloat(getComputedStyle(e).fontSize)).filter((f) => f < 16),
    };
  }, process.env.NAMES || "");
}

// What scrolls sideways that should not -- a pre, a diff, a wide table
// may -- and what sticks out past the window's right edge.
async function sideways(page) {
  return page.evaluate(() => {
    const name = (e) => e.tagName.toLowerCase() + (e.id ? "#" + e.id : "") + (e.classList.length ? "." + [...e.classList].join(".") : "");
    const meant = (e) => e.matches("pre, .file-diff, .md-table, .tbl, .tree-body, .tree-body *") || e.closest("pre, .file-diff, .md-table");
    const out = [];
    const all = [document.documentElement, ...document.querySelectorAll("body *")];
    for (const e of all) {
      const s = getComputedStyle(e);
      const scroller = e === document.documentElement || /(auto|scroll)/.test(s.overflowX);
      if (!scroller || meant(e) || e.getClientRects().length === 0) continue;
      if (e.scrollWidth <= e.clientWidth + 1) continue;
      const r = e.getBoundingClientRect();
      const right = r.left + e.clientWidth - e.scrollLeft;
      const culprits = [...e.querySelectorAll("*")].filter((k) => { const kr = k.getBoundingClientRect(); return kr.width && kr.right > right + 1 && !(k.parentElement && k.parentElement.getBoundingClientRect().right > right + 1 && k.parentElement !== e); })
        .slice(0, 6).map((k) => name(k) + "→" + Math.round(k.getBoundingClientRect().right - right));
      out.push(name(e) + " " + e.clientWidth + "<" + e.scrollWidth + " " + culprits.join(" "));
    }
    // Clipped: things past the window's right edge.
    const W = document.documentElement.clientWidth;
    const past = [...document.querySelectorAll("body *")].filter((k) => { const kr = k.getBoundingClientRect(); const s = getComputedStyle(k);
      return kr.width && kr.height && s.visibility !== "hidden" && kr.right > W + 1 && !k.closest("pre, .file-diff, .md-table") && !(k.parentElement && k.parentElement.getBoundingClientRect().right > W + 1); })
      .slice(0, 6).map((k) => name(k) + "→" + Math.round(k.getBoundingClientRect().right - W));
    return { sideways: out, past };
  });
}

module.exports = { start, watch, measure, sideways, until, sleep, log, BOX, PHONE };
