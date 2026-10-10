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
 */
(function () {
  "use strict";

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
  // the app's secret, the first segment of the page's own.  A 410 means
  // this page is no longer the app's: another took over, or Emacs let it
  // go when it stopped asking.

  const BROWSER = location.protocol === "http:";
  const ROOT = "/" + location.pathname.split("/")[1] + "/";

  function notice(msg) {
    let el = document.getElementById("xw-notice");
    if (!el) {
      el = document.createElement("div");
      el.id = "xw-notice";
      el.className = "xw-notice";
      el.setAttribute("role", "alert");
      document.body.appendChild(el);
    }
    el.textContent = msg;
  }

  let served = null;
  function servedBridge() {
    if (served) return served;
    const page = Array.from(crypto.getRandomValues(new Uint8Array(8)),
      (b) => b.toString(16).padStart(2, "0")).join("");
    const outbox = [];
    let hello = false, sending = false, over = false;

    function ask(path, body) {
      const opts = body === undefined ? {} : {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body),
      };
      return fetch(ROOT + path + "?page=" + page, opts).then((r) => {
        if (!r.ok) throw new Error(r.status === 410 ? "gone" : "HTTP " + r.status);
        return r;
      });
    }
    function stop(e) {
      if (over) return;
      over = true;
      notice(e && e.message === "gone"
        ? "This page is open elsewhere now, or Emacs let it go. Reload to use it here."
        : "Emacs is not answering. Open the page again from Emacs.");
    }
    function pump() {
      if (!hello || sending || over || !outbox.length) return;
      sending = true;
      ask("intent", outbox[0]).then(() => {
        outbox.shift();
        sending = false;
        pump();
      }, stop);
    }
    // One call failing must not keep the next from running: in the
    // xwidget each call is a script of its own.
    function run(call) {
      if (call.xwapp === "reload") {
        over = true;
        location.reload();
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
      if (over) return;
      ask("next").then((r) => r.json()).then((calls) => {
        calls.forEach(run);
        next();
      }, stop);
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

    ask("hello", {}).then(() => {
      hello = true;
      next();
      pump();
    }, stop);

    served = function emit(op, extra) {
      outbox.push(Object.assign({ op: op }, extra || {}));
      pump();
    };
    return served;
  }

  function bridge(prefix) {
    return BROWSER ? servedBridge() : titleBridge(prefix);
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
  });
})();
