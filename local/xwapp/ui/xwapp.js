/* xwapp page kit: what every xwapp page needs on the browser side.
 *
 * Loaded as a classic script before the page's own app.js:
 *
 *   <script src="../../xwapp/ui/xwapp.js"></script>
 *
 * The path holds from a package's ui/ both in the repo (local/<pkg>/ui)
 * and in straight's build (build/<pkg>/ui): xwapp sits beside the
 * package in either, and the browser resolves ".." on the URL, not on
 * the symlink's target.
 *
 *   XW.esc(s)            text for HTML, quotes included
 *   XW.bridge(prefix)    an emit(op, extra) that queues intents for Emacs
 *   XW.sanitizer(opts)   a sanitize(html) keeping a whitelist only
 *   XW.browser           true in a web browser, false in Emacs's xwidget
 *   XW.elsewhere         true on another device, through the proxy
 *   XW.copy(text)        a promise: whether TEXT is on this device's clipboard
 *   XW.open(url, name)   URL in a tab of this device's browser, from a tap
 *   XW.kit(call)         Emacs's calls to the kit itself, in the xwidget
 *
 * An element with data-xw-phone is the page's button for "open it on
 * your phone": shown once Emacs says the page can be reached there,
 * and pressed, the page's address as a QR code.
 */
(function () {
  "use strict";

  // Where this script, and what it loads only when asked, came from.
  const KIT = document.currentScript.src.replace(/[^/]*$/, "");

  function esc(s) {
    return String(s == null ? "" : s).replace(/[&<>"']/g, (c) => ({
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#39;",
    })[c]);
  }

  // ---- Intents ---------------------------------------------------------
  //
  // Emacs polls document.title and resets it once read.  A title set
  // before then would overwrite the unread one, so intents queue and go
  // out one per reset.  The counter keeps two identical intents (a second
  // Refresh) apart -- xwapp ignores a title equal to the last one it read
  // -- and seeding it with the clock keeps a reloaded page's first intent
  // apart from the last page's.

  function titleBridge(prefix) {
    const outbox = [];
    let n = Date.now();
    let sentAt = 0;
    function pump() {
      if (!outbox.length) return;
      // Unread: wait, unless Emacs has evidently stopped reading it.
      if (document.title.startsWith(prefix) && Date.now() - sentAt < 2000) return;
      document.title = prefix + JSON.stringify(outbox.shift());
      sentAt = Date.now();
    }
    setInterval(pump, 50);
    return function emit(op, extra) {
      outbox.push(Object.assign({ op: op, n: ++n }, extra || {}));
      pump();
    };
  }

  // ---- In a web browser -------------------------------------------------
  //
  // Served by Emacs (xwapp-browse), the page has no title Emacs reads and
  // runs no script Emacs sends, so it asks.  hello makes it the app's
  // page; one request for Emacs's calls is always waiting; intents go as
  // POSTs, one at a time, so they arrive in order.  Every URL starts with
  // the app's secret, the first segment of the page's own.
  //
  // A 410 means this page is not the app's, and says why:
  //   taken   another page is: this one stops; reloading takes it back
  //   free    none is, and Emacs can open the app again -- it restarted,
  //           or let this page go while the browser had it frozen.  The
  //           page loads again, and comes back with a soft hello, which
  //           takes nothing another page has meanwhile
  //   closed  none is, and Emacs cannot open the app again by itself
  // When Emacs does not answer at all -- restarting -- the page asks
  // again, less often each time, until it does.

  // Served by Emacs: here, or to another device through a proxy (https).
  const BROWSER = location.protocol !== "file:";
  // On another device already, through the proxy.
  const ELSEWHERE = BROWSER && !/^(127\.0\.0\.1|localhost)$/.test(location.hostname);
  const ROOT = "/" + location.pathname.split("/")[1] + "/";
  const SOFT = "xwapp-soft"; // sessionStorage: this load is the page coming back

  const SAY = {
    taken: "This page is open elsewhere now. Reload to use it here.",
    closed: "Emacs closed this page. Reload to use it here.",
    unknown: "Emacs does not know this page any more. Open it again from Emacs.",
    waiting: "Emacs is not answering. Trying again\u2026 (or open the page again from Emacs)",
  };

  function notice(msg) {
    let el = document.getElementById("xw-notice");
    if (!msg) {
      if (el) el.remove();
      return;
    }
    if (!el) {
      el = document.createElement("div");
      el.id = "xw-notice";
      el.className = "xw-notice";
      el.setAttribute("role", "alert");
      document.body.appendChild(el);
    }
    el.textContent = msg;
  }

  function comingBack() {
    try {
      const soft = sessionStorage.getItem(SOFT) === "1";
      sessionStorage.removeItem(SOFT);
      return soft;
    } catch (e) {
      return false;
    }
  }

  let served = null;
  function servedBridge() {
    if (served) return served;
    const page = Array.from(crypto.getRandomValues(new Uint8Array(8)),
      (b) => b.toString(16).padStart(2, "0")).join("");
    const soft = comingBack();
    const outbox = [];
    let hello = false, sending = false, polling = false, over = false;
    let retry = null, delay = 0;

    function ask(path, body) {
      const opts = body === undefined ? {} : {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body),
      };
      const also = path === "hello" && soft ? "&soft=1" : "";
      return fetch(ROOT + path + "?page=" + page + also, opts).then((r) => {
        if (r.ok) return r;
        return r.json().catch(() => ({})).then((said) => {
          const e = new Error("HTTP " + r.status);
          e.status = r.status;
          e.why = said && said.why;
          throw e;
        });
      });
    }
    function stop(msg) {
      over = true;
      closeQr();
      notice(msg);
    }
    // A request failed: do what Emacs said, or ask again later.
    function trouble(e) {
      if (over || retry) return;
      if (e.status === 410 && e.why === "free") {
        over = true;
        try {
          sessionStorage.setItem(SOFT, "1");
        } catch (_) {
          // Without it the page comes back as a fresh load, taking the app.
        }
        location.reload();
      } else if (e.status === 410) {
        stop(e.why === "closed" ? SAY.closed : SAY.taken);
      } else if (e.status === 404) {
        stop(SAY.unknown);
      } else {
        notice(SAY.waiting);
        delay = Math.min(10000, delay * 2 || 1000);
        retry = setTimeout(() => {
          retry = null;
          connect();
        }, delay);
      }
    }
    function answered() {
      if (!delay) return;
      delay = 0;
      notice(null);
    }
    function connect() {
      if (hello) {
        next();
        pump();
        return;
      }
      ask("hello", {}).then(() => {
        hello = true;
        answered();
        next();
        pump();
      }, trouble);
    }
    function pump() {
      if (!hello || sending || over || retry || !outbox.length) return;
      sending = true;
      ask("intent", outbox[0]).then(() => {
        outbox.shift();
        sending = false;
        answered();
        pump();
      }, (e) => {
        sending = false;
        trouble(e);
      });
    }
    // One call failing must not keep the next from running: in the
    // xwidget each call is a script of its own.
    function run(call) {
      if (call.xwapp === "reload") {
        over = true;
        location.reload();
        return;
      }
      if (call.xwapp) {
        kit(call);
        return;
      }
      const ns = window[call.ns];
      if (!ns || typeof ns[call.fn] !== "function") return;
      try {
        ns[call.fn](call.arg);
      } catch (e) {
        console.error(e);
      }
    }
    // A chain of promises, no timer: a background tab throttles timers.
    function next() {
      if (over || polling || retry) return;
      polling = true;
      ask("next").then((r) => r.json()).then((calls) => {
        polling = false;
        answered();
        calls.forEach(run);
        next();
      }, (e) => {
        polling = false;
        trouble(e);
      });
    }

    // Whether you could be looking, for Emacs to ask with xwapp-seen-p.
    let seen = null;
    function look() {
      const now = document.visibilityState === "visible" && document.hasFocus();
      if (now === seen) return;
      seen = now;
      outbox.push({ op: "xwapp-seen", seen: now });
      pump();
    }
    document.addEventListener("visibilitychange", look);
    window.addEventListener("focus", look);
    window.addEventListener("blur", look);
    look();

    connect();

    served = function emit(op, extra) {
      outbox.push(Object.assign({ op: op }, extra || {}));
      pump();
    };
    return served;
  }

  // The page's emit, for the kit's own intents.
  let kitEmit = null;

  // ---- This device's clipboard ------------------------------------------
  //
  // On another device, Emacs's clipboard is the wrong one.  Called from a
  // click: without a secure context -- a page over plain http on the
  // tailnet -- there is no navigator.clipboard, but a selected textarea
  // and execCommand still copy, as long as it is done within the click.

  function copy(text) {
    text = String(text == null ? "" : text);
    if (window.isSecureContext && navigator.clipboard) {
      return navigator.clipboard.writeText(text).then(() => true, () => false);
    }
    const ta = document.createElement("textarea");
    ta.value = text;
    // Read-only, so a phone shows no keyboard; 16px, so it does not zoom.
    ta.setAttribute("readonly", "");
    ta.style.position = "fixed";
    ta.style.top = "0";
    ta.style.left = "0";
    ta.style.opacity = "0";
    ta.style.fontSize = "16px";
    document.body.appendChild(ta);
    ta.select();
    ta.setSelectionRange(0, text.length);
    let ok = false;
    try {
      ok = document.execCommand("copy");
    } catch (e) {
      ok = false;
    }
    ta.remove();
    return Promise.resolve(ok);
  }

  // ---- This device's browser --------------------------------------------
  //
  // On another device, a link Emacs would open in its browser opens here
  // instead, in a tab of its own -- from the tap itself, for iOS opens
  // none a script asks for later.  NAME, for a page of Emacs's own, is
  // the tab to use again: the one it is in already, say.

  function open(url, name) {
    url = String(url == null ? "" : url);
    if (!/^(https?:|mailto:|\/[^/])/i.test(url)) return false;
    window.open(url, name || "_blank", name ? "" : "noopener");
    return true;
  }

  // ---- One tab per app, across same-origin tabs -------------------------
  //
  // Emacs opens pr-view and clickup-view in a tab of their own.  Clicking a
  // PR or task again should reuse and refocus that tab, not pile up a new
  // one -- but Emacs cannot reach across the browser's tabs to do it, so the
  // tabs arrange it among themselves over a BroadcastChannel they share
  // (same origin).  An app's page beats that it is open and listens for a
  // request to show a url; another page, before opening one, asks an open
  // tab to show it and come forward instead.  Bringing a background tab to
  // the front is all a browser allows from a script, and it may be refused
  // (iOS does not reuse a named tab either) -- the tab is at least sent to
  // what was asked, and a first open, with no tab yet, still happens from
  // the tap.
  const NAV_CHANNEL = "xwapp-open";
  const NAV_TTL = 6000;   // a tab counts as open this long after its last beat
  const NAV_BEAT = 2000;
  const navSeen = {};     // app name -> when a tab of it last beat
  let navCh = null;

  function navChannel() {
    try { return "BroadcastChannel" in window ? new BroadcastChannel(NAV_CHANNEL) : null; }
    catch (e) { return null; }
  }

  function navSetup(app) {
    navCh = navChannel();
    if (!navCh) return;
    navCh.onmessage = (e) => {
      const m = e && e.data;
      if (!m) return;
      if (m.alive) navSeen[m.alive] = Date.now();
      else if (m.gone) delete navSeen[m.gone];
      else if (app && m.nav === app && typeof m.url === "string") {
        try { window.focus(); } catch (_) {}
        const want = m.url.indexOf("#") >= 0 ? m.url.slice(m.url.indexOf("#")) : "";
        if (!want || want !== location.hash) location.href = m.url;
      }
    };
    if (app) {
      const beat = () => { try { navCh.postMessage({ alive: app }); } catch (_) {} };
      beat();
      setInterval(beat, NAV_BEAT);
      window.addEventListener("pagehide", () => { try { navCh.postMessage({ gone: app }); } catch (_) {} });
    }
  }

  // Open APP's page at URL.  When a tab of APP is already open (it has beat
  // lately), ask it to show URL and come forward, and open nothing here.
  // Otherwise open one from this click, so iOS opens it.  Call from a click.
  function openApp(url, app) {
    url = String(url == null ? "" : url);
    if (!/^(https?:|\/[^/])/i.test(url)) return false;
    const live = app && navSeen[app] && Date.now() - navSeen[app] < NAV_TTL;
    if (navCh && app) { try { navCh.postMessage({ nav: app, url }); } catch (_) {} }
    if (!live) {
      const w = window.open(url, app || "_blank", app ? "" : "noopener");
      if (w) { try { w.focus(); } catch (_) {} }
    }
    return true;
  }

  function bridge(prefix, app) {
    if (BROWSER) { navSetup(app); return (kitEmit = servedBridge()); }
    kitEmit = titleBridge(prefix);
    // In a browser the hello is the request that makes the page the
    // app's; in the xwidget it is the page's first intent.
    kitEmit("xwapp-hello");
    return kitEmit;
  }

  // ---- Your phone -------------------------------------------------------
  //
  // A page Emacs also serves to your other devices (xwapp-remote-hosts)
  // can hand itself to your phone: its address there as a QR code, which
  // the phone's camera opens.  The phone's page then takes the app, as
  // any tab does, and this one lets go.  Emacs says whether the page can
  // be reached so in answer to its hello, and gives the address only
  // when asked: it holds the page's secret.

  // Emacs's calls to the kit itself: in a browser through next, in the
  // xwidget as XW.kit(call).
  function kit(call) {
    if (call.xwapp === "phone") {
      document.documentElement.classList.toggle("xw-phone", !ELSEWHERE);
    } else if (call.xwapp === "qr") {
      showQr(call);
    }
  }

  document.addEventListener("click", (e) => {
    const button = e.target.closest && e.target.closest("[data-xw-phone]");
    if (!button || !kitEmit) return;
    e.preventDefault();
    kitEmit("xwapp-qr");
  });

  // Loaded the first time it is needed: most pages never are.
  let qrLib = null;
  function loadQr() {
    if (!qrLib) {
      qrLib = new Promise((resolve, reject) => {
        const s = document.createElement("script");
        s.src = KIT + "vendor/qrcode.js";
        s.addEventListener("load", () => resolve(window.qrcode));
        s.addEventListener("error", () => {
          qrLib = null;
          reject(new Error("its library did not load"));
        });
        document.head.appendChild(s);
      });
    }
    return qrLib;
  }

  // The code as one path, dark on white, in the quiet zone a scanner needs.
  function qrSvg(lib, text) {
    const q = lib(0, "M");
    q.addData(text, "Byte");
    q.make();
    const n = q.getModuleCount();
    const quiet = 4;
    const size = n + 2 * quiet;
    let d = "";
    for (let r = 0; r < n; r++) {
      for (let c = 0; c < n; c++) {
        if (q.isDark(r, c)) d += "M" + (c + quiet) + " " + (r + quiet) + "h1v1h-1z";
      }
    }
    const NS = "http://www.w3.org/2000/svg";
    const svg = document.createElementNS(NS, "svg");
    svg.setAttribute("viewBox", "0 0 " + size + " " + size);
    svg.setAttribute("shape-rendering", "crispEdges");
    svg.setAttribute("class", "xw-qr-code");
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", "This page's address, as a QR code");
    const bg = document.createElementNS(NS, "rect");
    bg.setAttribute("width", size);
    bg.setAttribute("height", size);
    bg.setAttribute("fill", "#fff");
    const fg = document.createElementNS(NS, "path");
    fg.setAttribute("d", d);
    fg.setAttribute("fill", "#000");
    svg.append(bg, fg);
    return svg;
  }

  function escQr(e) {
    if (e.key !== "Escape") return;
    e.preventDefault();
    e.stopPropagation();
    closeQr();
  }

  function closeQr() {
    const box = document.getElementById("xw-qr");
    if (box) box.remove();
    window.removeEventListener("keydown", escQr, true);
  }

  function showQr(call) {
    closeQr();
    const box = document.createElement("div");
    box.id = "xw-qr";
    box.className = "xw-qr";
    box.setAttribute("role", "dialog");
    box.setAttribute("aria-modal", "true");
    box.setAttribute("aria-label", "Open on your phone");
    const card = document.createElement("div");
    card.className = "xw-qr-card";
    const line = (cls, text) => {
      const p = document.createElement("p");
      p.className = cls;
      p.textContent = text;
      return p;
    };
    const close = document.createElement("button");
    close.type = "button";
    close.className = "xw-qr-close";
    close.textContent = "Close";
    close.addEventListener("click", closeQr);
    if (!call.url) {
      card.append(line("xw-qr-say", call.error || "Emacs gave no address."), close);
    } else {
      const say = line("xw-qr-say", "Scan it with your phone's camera.");
      card.append(say, line("xw-qr-host", call.host || ""),
        line("xw-qr-note", "Opened there, the page is your phone's, and this one lets go of it."), close);
      loadQr().then((lib) => {
        if (box.isConnected) card.prepend(qrSvg(lib, call.url));
      }, (e) => {
        say.textContent = "Cannot draw the code: " + e.message + ".";
      });
    }
    box.append(card);
    // A click beside the card closes it, as Esc does.
    box.addEventListener("click", (e) => {
      if (e.target === box) closeQr();
    });
    window.addEventListener("keydown", escQr, true);
    document.body.appendChild(box);
    close.focus();
  }

  // ---- Sanitizer -------------------------------------------------------
  //
  // Pages render text from elsewhere -- ClickUp descriptions and comments,
  // agent transcripts -- and can act on Emacs's behalf: change a status,
  // post, prompt an agent.  So nothing from that text may run.  Parse it
  // into an inert document (no script runs, no image loads), keep a
  // whitelist of tags and attributes, unwrap the rest.  Each page's CSP
  // is the second line: no inline script or handler there either.

  const BASE = {
    A: ["href", "title", "class"],
    P: [], BR: [], HR: [], DIV: [], SPAN: ["class"],
    STRONG: [], B: [], EM: [], I: [], U: [], S: [], DEL: [], INS: [],
    MARK: [], SUB: [], SUP: [], SMALL: [], KBD: [],
    CODE: ["class"], PRE: [], BLOCKQUOTE: [],
    H1: [], H2: [], H3: [], H4: [], H5: [], H6: [],
    UL: ["class"], OL: ["start", "class"], LI: ["class"],
    TABLE: [], THEAD: [], TBODY: [], TR: [],
    TH: ["align", "colspan", "rowspan"], TD: ["align", "colspan", "rowspan"],
    INPUT: ["type", "checked"],
    DETAILS: [], SUMMARY: [],
  };
  const DROP = [
    "SCRIPT", "STYLE", "IFRAME", "FRAME", "FRAMESET", "OBJECT", "EMBED", "APPLET",
    "TEMPLATE", "NOSCRIPT", "SVG", "MATH", "FORM", "TEXTAREA", "SELECT", "OPTION",
    "BUTTON", "LINK", "META", "BASE", "TITLE", "HEAD", "AUDIO", "VIDEO", "SOURCE",
    "TRACK", "CANVAS", "PORTAL",
  ];
  const CHECKS = {
    href: (v) => /^(https?:|mailto:)/i.test(v),
    src: (v) => /^(https?:|data:image\/(png|jpe?g|gif|webp);)/i.test(v),
    align: (v) => /^(left|right|center)$/.test(v),
    colspan: (v) => /^\d{1,3}$/.test(v),
    rowspan: (v) => /^\d{1,3}$/.test(v),
    start: (v) => /^\d{1,6}$/.test(v),
    type: (v) => v === "checkbox",
  };

  /* A sanitize(html) for one page.  OPTS:
   *   images   keep <img> (http(s) or data: images); otherwise dropped,
   *            since a remote image is fetched just by being shown
   *   classes  RegExp a class token must match to stay; default keeps
   *            only language-* (code blocks)
   *   attrs    {TAG: [attribute, ...]} kept beyond the base list
   *   checks   {attribute: value => bool} for those attributes; one
   *            without a check keeps any value (it is only text)  */
  function sanitizer(opts) {
    opts = opts || {};
    const keep = {};
    Object.keys(BASE).forEach((t) => (keep[t] = BASE[t].slice()));
    const drop = new Set(DROP);
    if (opts.images) keep.IMG = ["src", "alt", "title", "class"];
    else ["IMG", "PICTURE"].forEach((t) => drop.add(t));
    Object.keys(opts.attrs || {}).forEach((t) => {
      keep[t] = (keep[t] || []).concat(opts.attrs[t].filter((a) => !(keep[t] || []).includes(a)));
    });
    const checks = Object.assign({}, CHECKS, opts.checks || {});
    const classes = opts.classes || /^language-[\w+#-]+$/;

    function cleanClass(v) {
      return v.split(/\s+/).filter((c) => classes.test(c)).join(" ");
    }

    function cleanNode(node, out) {
      if (node.nodeType === 3) {
        out.appendChild(document.createTextNode(node.data));
        return;
      }
      if (node.nodeType !== 1) return;
      const tag = node.tagName.toUpperCase();
      if (drop.has(tag)) return;
      const allowed = keep[tag];
      // Unwrap what is not kept, and a link whose target is not: an
      // anchor left without its href would still look clickable.
      if (
        !allowed ||
        (tag === "INPUT" && node.getAttribute("type") !== "checkbox") ||
        (tag === "A" && !checks.href((node.getAttribute("href") || "").trim()))
      ) {
        node.childNodes.forEach((k) => cleanNode(k, out));
        return;
      }
      const el = document.createElement(tag.toLowerCase());
      allowed.forEach((name) => {
        if (!node.hasAttribute(name)) return;
        let v = node.getAttribute(name).trim();
        if (name === "checked") {
          el.setAttribute(name, "");
          return;
        }
        if (name === "class") v = cleanClass(v);
        if (v && (checks[name] || (() => true))(v)) el.setAttribute(name, v);
      });
      if (tag === "INPUT") el.setAttribute("disabled", "");
      if (tag === "IMG") el.setAttribute("loading", "lazy");
      node.childNodes.forEach((k) => cleanNode(k, el));
      out.appendChild(el);
    }

    return function sanitize(html) {
      const doc = new DOMParser().parseFromString(
        "<!DOCTYPE html><body>" + String(html || "") + "</body>",
        "text/html"
      );
      const box = document.createElement("div");
      doc.body.childNodes.forEach((k) => cleanNode(k, box));
      return box.innerHTML;
    };
  }

  window.XW = Object.freeze({
    esc: esc,
    bridge: bridge,
    sanitizer: sanitizer,
    browser: BROWSER,
    elsewhere: ELSEWHERE,
    copy: copy,
    open: open,
    openApp: openApp,
    kit: kit,
  });
})();
