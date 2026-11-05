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
// machine's browser reaches Emacs's loopback port straight, as it does;
// OPTS.link, {kbps, rtt}: the phone's link, slow -- each way half of RTT
// ms, and the answers' bytes through KBPS kilobits a second, one after
// another; setLink({kbps, rtt}) changes it from then on, as a path that
// turns direct.  OPTS.peer: the device's tailnet address, said to Emacs
// as `tailscale serve' says it (X-Forwarded-For) -- only when given, so
// no Emacs is led to ping a real device.  OPTS.env: more of Emacs's
// environment.  OPTS.spawn(ENV, EL): start that Emacs another way -- in a
// terminal, say -- returning what has a kill().
async function start(opts) {
  opts = opts || {};
  const run = E = fs.mkdtempSync(path.join(os.tmpdir(), "xwapp-e2e-"));
  if (opts.setup) opts.setup(run);
  const port = await freePort();
  const env = Object.assign({}, process.env, opts.env || {}, { E2E_DIR: E, E2E_PORT: String(port), E2E_LOCAL: LOCAL });
  const el = path.join(__dirname, opts.el || "apps.el");
  const emacs = opts.spawn ? opts.spawn(env, el)
    : spawn(EMACS, ["-Q", "--batch", "-l", el], { env, stdio: ["ignore", "ignore", "pipe"] });
  let err = "";
  if (emacs.stderr) emacs.stderr.on("data", (d) => { err += d; });
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
  // The phone asleep (sleep()): what it had under way is cut off, and
  // nothing it asks reaches Emacs until it wakes (wake()), when what it
  // asked meanwhile goes on.  Or, as a phone does, asleep with what it
  // had under way hanging (nap()): no answer reaches it, and it learns
  // only once it wakes that those connections are dead (wake(), which
  // says how).  Each answer that came through is noted in TRAFFIC: when,
  // what, its code, the bytes of its body, how they were encoded, and the
  // ETag the browser asked with, if it had the file -- and when Emacs's
  // answer had all come to the proxy (end).  down(true): Emacs
  // is not there -- every request answered 502, as serve does then.
  // goneAt(PRED): Emacs goes the moment a request PRED takes ({method,
  // path}) reaches it, before Emacs has it -- that one and all under way
  // cut, as Emacs killed then would cut them; it says when.
  // holdAt(PRED, FN): the next request PRED takes waits at the proxy until
  // FN's promise is kept -- Emacs made busy meanwhile, say (askEmacs) --
  // then goes on.  askEmacs(METHOD, PATH): a request straight to Emacs's
  // socket, past the proxy; its code.
  let asleep = false, gone = false, going = null, holding = null;
  const held = [], open = new Set(), dozing = new Set(), dead = new Set(), traffic = [];
  const link = opts.link;
  let linkFree = 0;
  const half = () => (link ? link.rtt / 2 : 0);
  // Down the link: each chunk once the link is free for it, and the end
  // once the last is written -- Node may say the end before then.
  const drip = (r, res) => {
    let writing = 0, ended = false;
    const finish = () => { if (ended && !writing && !res.destroyed) res.end(); };
    r.on("data", (c) => {
      if (res.destroyed) return;
      r.pause();
      writing++;
      const now = Date.now();
      linkFree = Math.max(now, linkFree) + (c.length * 8) / link.kbps;
      setTimeout(() => {
        if (!res.destroyed) res.write(c);
        writing--;
        r.resume();
        finish();
      }, linkFree - now);
    });
    const done = () => { ended = true; finish(); };
    // One with no body may have ended already, while it waited.
    if (r.readableEnded) done();
    else r.on("end", done);
  };
  const forward = (req, res, u) => {
    const headers = Object.assign({}, req.headers, { host: "localhost", "tailscale-user-login": "me@example.com" },
                                  opts.peer ? { "x-forwarded-for": opts.peer } : {});
    delete headers["proxy-connection"];
    const seen = { at: Date.now(), method: req.method, path: u.pathname.replace(/^\/[0-9a-f]{32}\//, "/SECRET/") + u.search,
      status: 0, bytes: 0, enc: "", asked: req.headers["if-none-match"] || "" };
    traffic.push(seen);
    open.add(res);
    res.on("close", () => open.delete(res));
    res.on("error", () => {});
    const up = http.request({ socketPath: sock, path: u.pathname + u.search, method: req.method, headers }, (r) => {
      seen.status = r.statusCode;
      seen.enc = r.headers["content-encoding"] || "";
      r.on("data", (c) => { seen.bytes += c.length; });
      r.on("end", () => { seen.end = Date.now(); });
      // Asleep: the answer never reaches the phone.
      if (res.destroyed || dozing.has(res) || dead.has(res)) return r.resume();
      const answer = () => {
        if (res.destroyed) return r.resume();
        res.writeHead(r.statusCode, r.headers);
        if (link) { drip(r, res); r.resume(); }
        else r.pipe(res);
      };
      // Held until the link has it: the counter above would let it flow.
      if (link) { r.pause(); setTimeout(answer, half()); }
      else answer();
    });
    up.on("error", () => { if (!res.destroyed && !res.headersSent) { res.writeHead(502); res.end(); } });
    req.pipe(up);
  };
  const proxy = http.createServer((req, res) => {
    const u = new URL(req.url);
    if (u.host !== "box.tail0.ts.net") { away.push(req.url); res.writeHead(200, { "content-type": "text/plain" }); return res.end("away"); }
    const go = () => {
      if (going && going.pred({ method: req.method, path: u.pathname })) {
        const g = going;
        going = null;
        gone = true;
        traffic.push({ at: Date.now(), method: req.method, path: u.pathname.replace(/^\/[0-9a-f]{32}\//, "/SECRET/") + u.search,
          status: 0, bytes: 0, enc: "", asked: "" });
        open.forEach((r) => r.destroy());
        res.destroy();
        return g.went(Date.now());
      }
      if (gone) {
        traffic.push({ at: Date.now(), method: req.method, path: u.pathname.replace(/^\/[0-9a-f]{32}\//, "/SECRET/") + u.search,
          status: 502, bytes: 0, enc: "", asked: "" });
        res.writeHead(502, { "content-type": "text/plain" });
        return res.end("Bad Gateway");
      }
      const on = () => {
        if (asleep) held.push(() => forward(req, res, u));
        else forward(req, res, u);
      };
      if (holding && holding.pred({ method: req.method, path: u.pathname, query: u.search })) {
        const h = holding;
        holding = null;
        return Promise.resolve(h.fn()).then(on, on);
      }
      on();
    };
    if (link) setTimeout(go, half());
    else go();
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
  return { browser, urls, away, stop, dir: run, port, secretOf: (app) => urls[app].split("/")[3], traffic,
    setLink: (l) => { if (link) Object.assign(link, l); },
    sleep: () => { asleep = true; open.forEach((res) => res.destroy()); },
    nap: () => { asleep = true; open.forEach((res) => dozing.add(res)); },
    // What slept with the phone, as it wakes: "cut", its connections
    // closed -- WebKit asks a GET again by itself then, unseen by the
    // page; "fail", each answered 502, which the page sees fail; "hang",
    // left as a dead connection is, no answer ever coming.
    wake: (stale) => {
      asleep = false;
      dozing.forEach((res) => {
        if (res.destroyed) return;
        if (stale === "hang") return dead.add(res);
        if (stale === "fail" && !res.headersSent) {
          res.writeHead(502, { "content-type": "text/plain" });
          return res.end("Bad Gateway");
        }
        res.destroy();
      });
      dozing.clear();
      held.splice(0).forEach((go) => go());
    },
    down: (on) => { gone = on; },
    goneAt: (pred) => new Promise((went) => { going = { pred, went }; }),
    holdAt: (pred, fn) => { holding = { pred, fn }; },
    askEmacs: (method, p) => new Promise((done) => {
      const q = http.request({ socketPath: sock, path: p, method,
                               headers: { host: "localhost", "tailscale-user-login": "me@example.com" } },
                             (r) => { r.resume(); r.on("end", () => done(r.statusCode)); });
      q.on("error", () => done(0));
      q.end();
    }),
    cut: () => open.forEach((res) => res.destroy()) };
}

// What a test needs to see of a phone's page, before it loads: its
// visibility, which the test sets (__vis("hidden"), __vis("visible"),
// as a phone locked and unlocked); every notice the kit showed, in
// __said; and its nexts, in __nexts -- how many ask at once now, the
// most ever, and when each was asked.
async function phoneEyes(ctx) {
  await ctx.addInitScript(() => {
    let vis = "visible";
    Object.defineProperty(document, "visibilityState", { get: () => vis, configurable: true });
    Object.defineProperty(document, "hidden", { get: () => vis === "hidden", configurable: true });
    window.__vis = (v) => { vis = v; document.dispatchEvent(new Event("visibilitychange")); };
    window.__said = [];
    new MutationObserver(() => {
      const n = document.getElementById("xw-notice");
      if (n && n.textContent && __said[__said.length - 1] !== n.textContent) __said.push(n.textContent);
    }).observe(document, { subtree: true, childList: true, characterData: true });
    const real = window.fetch.bind(window);
    window.__nexts = { now: 0, most: 0, at: [] };
    window.fetch = (url, opts) => {
      if (!/\/next\?/.test(String(url))) return real(url, opts);
      const n = window.__nexts;
      n.now++;
      n.most = Math.max(n.most, n.now);
      n.at.push(Date.now());
      let done = false;
      const end = () => { if (!done) { done = true; n.now--; } };
      if (opts && opts.signal) opts.signal.addEventListener("abort", end);
      return real(url, opts).then((r) => { r.clone().text().then(end, end); return r; }, (e) => { end(); throw e; });
    };
  });
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

module.exports = { start, watch, phoneEyes, measure, sideways, until, sleep, log, BOX, PHONE };
