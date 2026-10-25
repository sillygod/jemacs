// A document the herd page serves can never act as the page (ask
// ccf857d3).  A real Emacs (docs.el) serves the real page and, beside
// it, the Docs and an agent's HTML report -- sandboxed by xwapp's own
// headers -- to a phone through a proxy that does what `tailscale serve'
// does, and to this machine's browser on loopback.  A document that
// carries a <script> trying fetch, XHR, a beacon, a form, an image, a
// stylesheet, a frame and a navigation at the page's own endpoints
// reaches nothing: no script of it runs, nothing it asks for takes the
// page or sends an intent, and the page goes on as the app's.  Only for
// the tailnet login it is served to; and the Docs tab fits a phone.
const fs = require("fs");
const path = require("path");
const http = require("http");
const { webkit } = require("playwright");
const { start, watch, measure, until, sleep, log, BOX, PHONE } = require("./appslib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 860 } };
const RID = "c0ffee00c0ffee00c0ffee00c0ffee00";
const PNG1 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==";

// What each run's Emacs serves: a doc as ghostherd's are made, one out
// to act, one that would leave by itself, and a report a reply named.
const GOOD = '<!DOCTYPE html><html><head><meta charset="utf-8"><title>A good doc</title>' +
  '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans&display=swap">' +
  "<style>body { font-family: 'IBM Plex Sans', sans-serif; color: #123; } .ok { color: green; }</style></head>" +
  '<body><h1 id="h" class="ok">Read me</h1><img id="pic" alt="dot" src="' + PNG1 + '">' +
  '<p><a id="sib" href="evil.html">its sibling</a></p></body></html>';
const EVIL = '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Evil doc</title>' +
  '<link rel="stylesheet" href="../ghostherd/ui/style.css">' +
  "<style>body { background-image: url(../next?page=evil-css); }</style></head><body>" +
  '<h1 id="h">untouched</h1>' +
  '<img id="img" src="../next?page=evil-img" alt="" width="40" height="40">' +
  '<form id="f" method="post" action="../hello?page=evil-form" style="margin: 20px 0">' +
  '<button id="b" type="submit" style="display: block; width: 200px; height: 60px">Go</button></form>' +
  '<iframe id="fr" src="../ghostherd/ui/index.html?evil-frame" width="200" height="80"></iframe>' +
  '<p style="margin-top: 40px"><a id="nav" href="../intent?page=evil-link">a link at the page\'s intents</a></p>' +
  "<script>" +
  'document.getElementById("h").textContent = "ran";' +
  'fetch("../hello?page=evil-fetch", { method: "POST", body: "{}" }).catch(function () {});' +
  'fetch("../intent?page=evil-fetch", { method: "POST", body: JSON.stringify({ op: "agent-kill" }) }).catch(function () {});' +
  'try { var x = new XMLHttpRequest(); x.open("POST", "../hello?page=evil-xhr"); x.send("{}"); } catch (e) {}' +
  'try { navigator.sendBeacon("../hello?page=evil-beacon", "{}"); } catch (e) {}' +
  'try { window.opener && window.opener.GM && window.opener.GM.flash("evil"); } catch (e) {}' +
  'try { new BroadcastChannel("xwapp-open").postMessage({ nav: "ghostherd", url: "/evil" }); } catch (e) {}' +
  'setTimeout(function () { location.href = "../hello?page=evil-nav"; }, 300);' +
  '</script><script src="../ghostherd/ui/app.js?evil-src"></script></body></html>';
const REFRESH = '<!DOCTYPE html><html><head><title>Leaves by itself</title>' +
  '<meta http-equiv="refresh" content="0;url=../hello?page=evil-refresh"></head><body><p id="h">here</p></body></html>';
const REPORT = '<!doctype html><html><head><title>A report</title></head><body><h1 id="h">untouched</h1>' +
  '<script>document.getElementById("h").textContent = "ran"; fetch("../../hello?page=evil-report", { method: "POST" });</script></body></html>';

function setup(dir) {
  fs.mkdirSync(path.join(dir, "docs"));
  fs.mkdirSync(path.join(dir, "replies"));
  fs.writeFileSync(path.join(dir, "docs", "good.html"), GOOD);
  fs.writeFileSync(path.join(dir, "docs", "evil.html"), EVIL);
  fs.writeFileSync(path.join(dir, "docs", "refresh.html"), REFRESH);
  fs.writeFileSync(path.join(dir, "replies", RID + ".html"), REPORT);
}

const reqs = () => log().filter((w) => w && w.req);
// What a doc asked for at the page's endpoints: each is named evil-...
const evil = () => reqs().filter((r) => /evil-/.test(r.target));
const pageNow = (s) => { try { return JSON.parse(fs.readFileSync(path.join(s.dir, "page.txt"), "utf8")); } catch (_) { return null; } };
// The page id of the last hello, and how many there were.
const hellos = () => reqs().filter((r) => r.req === "POST" && /\/hello\?/.test(r.target));
const idOf = (target) => (/[?&]page=([^&]+)/.exec(target) || [])[1];

// Straight to the proxy's socket, as `tailscale serve' would for LOGIN.
function viaServe(s, target, login) {
  return new Promise((resolve) => {
    const headers = { host: "localhost" };
    if (login) headers["tailscale-user-login"] = login;
    const r = http.request({ socketPath: path.join(s.dir, "remote.sock"), path: target, method: "GET", headers }, (res) => {
      let body = "";
      res.on("data", (d) => { body += d; });
      res.on("end", () => resolve({ code: res.statusCode, csp: res.headers["content-security-policy"] || "", body }));
    });
    r.on("error", (e) => resolve({ code: 0, err: String(e) }));
    r.end();
  });
}

(async () => {
  const s = await start({ el: "docs.el", desk: true, setup });
  const secret = s.urls.secret;
  const ROOT = "/" + secret + "/";
  let policy = "";
  try {
    for (const where of ["phone", "desk"]) {
      const W = (n) => where + ": " + n;
      const base = where === "phone" ? BOX : "http://127.0.0.1:" + s.port;
      const ctx = await s.browser.newContext(where === "phone" ? PHONE : DESK);
      // Google's fonts are away from here: answered empty, as allowed.
      await ctx.route(/^https:\/\//, (r) => r.fulfill({ status: 200, contentType: "text/css", body: "" }));
      const errors = [];
      const heads = {};
      ctx.on("response", (r) => { r.allHeaders().then((h) => { heads[r.url()] = h; }, () => {}); });
      const herd = await ctx.newPage();
      watch(herd, errors);
      const before = hellos().length;
      await herd.goto(base + ROOT + "ghostherd/ui/index.html#docs");
      const listed = await until(async () => (await herd.$$(".page.docs .doc")).length === 3, 10000);
      check(W("the Docs tab, asked of Emacs, lists its documents"), listed, await herd.textContent("#app").catch(() => ""));
      const mine = hellos().slice(before);
      const P = mine.length ? idOf(mine[mine.length - 1].target) : null;
      // Emacs notes whose the page is every 200 ms.
      check(W("the page said hello and is the app's"), P && await until(() => pageNow(s) === P, 2000), { P, now: pageNow(s) });
      const link = await herd.$eval('.page.docs a[href$="/.docs/evil.html"]', (a) =>
        ({ href: a.getAttribute("href"), target: a.target, rel: a.rel })).catch(() => null);
      check(W("each a link under the page's secret, a tab of its own, no opener"),
        link && link.href === ROOT + ".docs/evil.html" && link.target === "_blank" && /noopener/.test(link.rel), link);
      if (where === "phone") {
        const m = await measure(herd);
        check(W("the Docs tab fits the phone"), !m.pageScroll && !m.past.length && !m.small.length, m);
        await herd.screenshot({ path: path.join(__dirname, "shots", "docs-tab-phone.png") });
      }

      // ---- A good doc: shown, sandboxed, its own styles and image ----
      const tap = (p, sel) => (where === "phone" ? p.tap(sel) : p.click(sel));
      let popped = ctx.waitForEvent("page");
      await tap(herd, '.page.docs a[href$="/.docs/good.html"]');
      const good = await popped;
      await good.waitForLoadState("load").catch(() => {});
      await sleep(300);
      const gh = heads[base + ROOT + ".docs/good.html"] || {};
      policy = gh["content-security-policy"] || policy;
      check(W("the doc opens in its tab, under its own policy: sandbox, nothing allowed"),
        good.url() === base + ROOT + ".docs/good.html" && /^sandbox;/.test(gh["content-security-policy"] || "") &&
        !/allow-/.test(gh["content-security-policy"] || ""), { url: good.url(), csp: gh["content-security-policy"] });
      const shown = await good.evaluate(() => {
        const h = document.getElementById("h"), pic = document.getElementById("pic");
        return { text: h.textContent, color: getComputedStyle(h).color, pic: pic.complete && pic.naturalWidth > 0,
                 origin: self.origin, opener: window.opener === null };
      }).catch((e) => ({ err: String(e) }));
      check(W("it reads as made: its inline style and its data: image"),
        shown.text === "Read me" && shown.color === "rgb(0, 128, 0)" && shown.pic, shown);
      check(W("its origin is none, and it has no opener"), shown.origin === "null" && shown.opener, shown);
      if (where === "phone") await good.screenshot({ path: path.join(__dirname, "shots", "docs-good-phone.png") });
      await good.close();

      // ---- A doc out to act ----
      const evilBefore = evil().length;
      popped = ctx.waitForEvent("page");
      await tap(herd, '.page.docs a[href$="/.docs/evil.html"]');
      const bad = await popped;
      const csp = [];
      bad.on("console", (m) => csp.push(m.text()));
      await bad.waitForLoadState("load").catch(() => {});
      await sleep(1200);
      check(W("no script of it ran: its text untouched, and it stayed where it was"),
        (await bad.textContent("#h").catch(() => "")) === "untouched" && bad.url() === base + ROOT + ".docs/evil.html",
        { h: await bad.textContent("#h").catch(() => "?"), url: bad.url() });
      const loaded = await bad.$eval("#img", (i) => i.naturalWidth).catch(() => -1);
      check(W("its image at the page's endpoint did not load"), loaded === 0, loaded);
      await tap(bad, "#b").catch((e) => console.log("  (form: " + String(e).split("\n")[0] + ")"));
      await sleep(600);
      check(W("its form posts nothing"), bad.url() === base + ROOT + ".docs/evil.html" &&
        !reqs().some((r) => /evil-form/.test(r.target)), { url: bad.url() });
      const reached = evil().slice(evilBefore);
      check(W("nothing it asked for reached Emacs: no fetch, XHR, beacon, image, style, frame or script"),
        reached.length === 0, reached);
      check(W("the page is still the app's, and no hello since"),
        pageNow(s) === P && hellos().length === before + mine.length, { now: pageNow(s), P });
      // A link of its own, pressed: a navigation of its own tab, which
      // Emacs answers with nothing.
      await tap(bad, "#nav");
      await sleep(600);
      const nav = evil().slice(evilBefore);
      check(W("a link it holds, pressed, gets nothing from Emacs"),
        nav.length <= 1 && nav.every((r) => r.req === "GET" && r.code >= 400) && pageNow(s) === P, nav);
      await bad.close();

      // ---- A doc that would leave by itself ----
      const refresh = await ctx.newPage();
      const rBefore = evil().length;
      await refresh.goto(base + ROOT + ".docs/refresh.html");
      await sleep(1200);
      const left = evil().slice(rBefore);
      check(W("one that would leave by itself gets nothing either" + (left.length ? " (WebKit followed its refresh: a GET, answered " +
        left.map((r) => r.code).join(",") + ")" : " (WebKit did not follow its refresh)")),
        left.every((r) => r.req === "GET" && r.code >= 400) && pageNow(s) === P, { left, url: refresh.url() });
      await refresh.close();

      // ---- The report a reply named ----
      const report = await ctx.newPage();
      const rr = await report.goto(base + ROOT + ".reports/" + RID + ".html");
      await sleep(500);
      check(W("a report a reply named: sandboxed too, no script ran"),
        rr && rr.status() === 200 && /^sandbox;/.test((await rr.allHeaders())["content-security-policy"] || "") &&
        (await report.textContent("#h")) === "untouched" && !reqs().some((r) => /evil-report/.test(r.target)));
      const guessed = await report.goto(base + ROOT + ".reports/" + "f".repeat(32) + ".html");
      check(W("a guessed report id: nothing"), guessed && guessed.status() === 404);
      for (const p of [".docs/../docs/good.html", ".docs/GOOD.html", ".docs/good.HTML", ".docs/state.eld", ".docs/..%2Fstate.eld",
                       ".files/" + RID + ".html", ".reports/good.html"]) {
        const r = await report.goto(base + ROOT + p).catch(() => null);
        check(W("not served: " + p), r && r.status() === 404, r && r.status());
      }
      const noSecret = await report.goto(base + "/.docs/good.html");
      check(W("not without the page's secret"), noSecret && noSecret.status() === 404);
      await report.close();

      // ---- The page goes on ----
      const asked = log().filter((w) => w && w.intent && w.intent.op === "docs").length;
      await tap(herd, '.tab[data-tab="log"]');
      await tap(herd, '.tab[data-tab="docs"]');
      const again = await until(() => log().filter((w) => w && w.intent && w.intent.op === "docs").length > asked, 5000);
      check(W("the page still talks to Emacs, as the app's"), again && pageNow(s) === P);
      // The doc's own try at one (agent-kill) never came: the page's alone did.
      check(W("no intent but the page's own"), !log().some((w) => w && w.intent &&
        (w.intent.op === "agent-kill" || /evil/.test(JSON.stringify(w.intent)))),
        [...new Set(log().filter((w) => w && w.intent).map((w) => w.intent.op))]);
      check(W("no page errors"), errors.filter((e) => !/Failed to load resource/.test(e)).length === 0, errors);
      await ctx.close();
    }

    // ---- Only for the tailnet login it is served to ----
    const target = ROOT + ".docs/good.html";
    const ok = await viaServe(s, target, "me@example.com");
    check("through serve, for your login: the doc, sandboxed", ok.code === 200 && ok.csp === policy && /^sandbox;/.test(policy), ok.code);
    for (const login of ["someone@else.com", null]) {
      for (const t of [target, ROOT + ".reports/" + RID + ".html", ROOT + "ghostherd/ui/index.html"]) {
        const r = await viaServe(s, t, login);
        check("through serve for " + (login || "no login") + ": nothing (" + t.replace(secret, "SECRET") + ")", r.code === 404, r.code);
      }
    }
  } finally {
    await s.stop();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})();
