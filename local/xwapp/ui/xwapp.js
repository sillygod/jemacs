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
 *   XW.bridge(prefix[, app[, opts]])
 *                        an emit(op, extra, lost) that queues intents for
 *                        Emacs: false for one too long to send, and LOST
 *                        called if one sent does not reach Emacs; what
 *                        LOST returns, if a function, is called should
 *                        Emacs turn out to have had it after all.  APP
 *                        names the page among the browser's tabs (openApp);
 *                        OPTS.back, a function, makes a page in a browser
 *                        come back where it is when Emacs let it go and
 *                        can have it again, rather than load again: it is
 *                        called then, to ask Emacs again for what the page
 *                        shows (see "In a web browser")
 *   XW.sanitizer(opts)   a sanitize(html) keeping a whitelist only
 *   XW.browser           true in a web browser, false in Emacs's xwidget
 *   XW.elsewhere         true on another device, through the proxy
 *   XW.copy(text)        a promise: whether TEXT is on this device's clipboard
 *   XW.open(url, name)   URL in a tab of this device's browser, from a tap
 *   XW.kit(call)         Emacs's calls to the kit itself, in the xwidget
 *   XW.file(name)        the URL of a file Emacs serves the page by name
 *                        (xwapp-serve-files); "" in the xwidget until Emacs
 *                        has said where, when "xw-files" fires on document
 *   XW.file(name, set)   one of another set Emacs serves; "" in the xwidget,
 *                        where Emacs shows it (xwapp-show-file)
 *
 * An element with data-xw-phone is the page's button for "open it on
 * your phone": shown once Emacs says the page can be reached there,
 * and pressed, the page's address as a QR code.
 *
 * In Emacs's xwidget, ⌘V in a field of the page's pastes the text on
 * Emacs's clipboard (see "⌘V in Emacs" below).  The field hears it first:
 * "xw-paste", cancelable, its detail {text}, "" when the clipboard holds
 * no text -- an image, say, which the page may then ask Emacs for.
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
  // Emacs polls document.title and answers each title it reads.  A title
  // set before then would overwrite the unread one, so intents queue and
  // go out one per answer.  The counter keeps two identical intents (a
  // second Refresh) apart -- xwapp ignores a title equal to the last one
  // it read -- and seeding it with the clock keeps a reloaded page's
  // first intent apart from the last page's.
  //
  // Emacs's answer names the title it read by its intent's number N and
  // its part I (a title of its own is part 0), not by its words, which
  // in WebKit's copy may not be the page's:
  //
  //   XW.kit({xwapp: "read", n: N, i: I, whole: true, idle: IDLE})
  //
  // The kit puts IDLE back as the title only while the title is still
  // that one: Emacs may have read it late, from WebKit's copy, after the
  // page gave it up and set the next, which a plain reset would wipe out
  // unread.  WHOLE: Emacs has the whole intent now, so one the page gave
  // up reached it after all, and its loss is taken back.  "unread" in
  // place of "read": Emacs could make no intent of it, and it is lost.
  //
  // The title Emacs reads is WebKit's copy of it, which is not all of it
  // (measured 2026-10-30, macOS 15's WKWebView under Emacs 31; the page's
  // own document.title keeps all):
  //   - it holds the first 1000 UTF-16 units: a CJK character is one, an
  //     emoji two, and an emoji cut in half leaves Emacs no title at all;
  //   - a run of spaces in it is one space.
  // So a space after another goes as \u0020, which JSON reads back as a
  // space, and an intent whose title would pass TITLE_MAX goes in parts,
  // a title each,
  //
  //   PREFIX{"xwpart":N,"i":K,"n":COUNT,"s":"..."}
  //
  // whose s, strung together in order of K, is the intent's JSON, and N
  // the intent's own n.  Emacs acts on it once it has them all.
  //
  // An intent Emacs could not read, it says so of (XW.kit "unread"); one
  // it left unread for WAIT while another intent waits to go is given up
  // on, and the other sent over it.  Either way it is lost, and its
  // emit's LOST is called -- and taken back if Emacs says after all that
  // it has it whole.

  const TITLE_MAX = 800;               // UTF-16 units: WebKit keeps 1000
  const INTENT_MAX = 2 * 1024 * 1024;  // UTF-16 units of an intent's JSON
  // How long a title may stay unread before it is given up for another
  // intent: as long as Emacs waits for a long intent's next part
  // (xwapp--part-wait in xwapp.el, 10 s).  Emacs busy for a while -- a
  // GC, a sync process -- reads it after; one wedged for good does not
  // hold up the page forever.
  const WAIT = 10000;

  // A lost intent: its LOST is called, and what is left of it not sent.
  function giveUp(rec) {
    if (rec.gone) return;
    rec.gone = true;
    if (!rec.lost) return;
    try {
      rec.undo = rec.lost();
    } catch (e) {
      console.error(e);
    }
  }

  // A lost intent that reached Emacs after all: what its LOST did undone.
  function takeBack(rec) {
    if (!rec.gone || rec.found) return;
    rec.found = true;
    if (typeof rec.undo !== "function") return;
    try {
      rec.undo();
    } catch (e) {
      console.error(e);
    }
  }

  // JSON with no run of spaces for WebKit to fold.  JSON.stringify puts
  // a space nowhere but in a string, where \u0020 is one too.
  function keepSpaces(json) {
    return json.replace(/ {2,}/g, (run) => " " + "\\u0020".repeat(run.length - 1));
  }

  // JSON cut into pieces whose own JSON is at most ROOM units long, none
  // between the halves of an emoji.
  function pieces(json, room) {
    const out = [];
    for (let at = 0; at < json.length;) {
      let end = Math.min(json.length, at + room);
      let over;
      // Escaped again, a " or \ is two: shed what that adds.
      while ((over = JSON.stringify(json.slice(at, end)).length - room) > 0) end -= Math.ceil(over / 2);
      const c = json.charCodeAt(end - 1);
      if (end < json.length && c >= 0xd800 && c < 0xdc00) end--;
      out.push(json.slice(at, end));
      at = end;
    }
    return out;
  }

  // The titles that carry intent N, of JSON, to Emacs: one, or its parts.
  // One that has an n of its page's own (an answer's option) goes as a
  // part even when short, for Emacs to name it by N, which no other has.
  function titles(prefix, n, json, named) {
    json = keepSpaces(json);
    if (named && prefix.length + json.length <= TITLE_MAX) return [prefix + json];
    const room = TITLE_MAX - prefix.length -
      (JSON.stringify({ xwpart: n, i: INTENT_MAX, n: INTENT_MAX, s: "" }).length - 2);
    const all = pieces(json, room);
    return all.map((s, i) => prefix + JSON.stringify({ xwpart: n, i: i, n: all.length, s: s }));
  }

  // The page's title bridges, for Emacs's answers: one, but in a test.
  const titleBridges = [];

  function titleBridge(prefix) {
    // {title, rec, n, i}: rec is the intent's, shared by its parts, and
    // n and i what Emacs names the title by.
    const outbox = [];
    let n = Date.now();
    let sent = null;     // the one last set, until Emacs reads it
    let sentAt = 0;
    // Intents given up unread, by n, for Emacs to say it had after all.
    const given = new Map();
    // Whether there is a title to send, what is left of lost intents dropped.
    function more() {
      while (outbox.length && outbox[0].rec.gone) outbox.shift();
      return outbox.length > 0;
    }
    // Whether another intent than REC's waits to go: its own next part
    // is not another.
    function another(rec) {
      return outbox.some((o) => o.rec !== rec && !o.rec.gone);
    }
    function pump() {
      // Unread, unless the title is no longer an intent: Emacs from
      // before the answers clears it itself.
      if (sent && document.title.startsWith(prefix)) {
        // Wait for WAIT, and then for as long as no other intent waits
        // to go, which goes over it.
        if (Date.now() - sentAt < WAIT || !another(sent.rec)) return;
        const old = sent;
        sent = null;
        given.set(old.n, old.rec);
        if (given.size > 32) given.delete(given.keys().next().value);
        // Its LOST may emit, which pumps: that has sent the next.
        giveUp(old.rec);
        if (sent) return;
      }
      sent = null;
      if (!more()) return;
      sent = outbox.shift();
      document.title = sent.title;
      sentAt = Date.now();
    }
    titleBridges.push({
      // Emacs's answer for a title it read: whether it was this bridge's.
      answer(call) {
        if (sent && sent.n === call.n && sent.i === call.i) {
          const rec = sent.rec;
          sent = null;
          if (typeof call.idle === "string") document.title = call.idle;
          // Its LOST may emit, which pumps, before this does.
          if (call.xwapp === "unread") giveUp(rec);
          pump();
          return true;
        }
        const rec = call.xwapp === "read" && call.whole ? given.get(call.n) : null;
        if (!rec) return false;
        given.delete(call.n);
        takeBack(rec);
        return true;
      },
      // Whether the title is this bridge's, unread.
      showing() {
        return !!sent && document.title === sent.title;
      },
    });
    setInterval(pump, 50);
    // The next part as soon as the title is cleared, not a tick later.
    new MutationObserver(pump).observe(document.head || document.documentElement,
      { subtree: true, childList: true, characterData: true });
    return function emit(op, extra, lost) {
      const id = ++n;
      const intent = Object.assign({ op: op, n: id }, extra || {});
      const json = JSON.stringify(intent);
      if (json.length > INTENT_MAX) return false;
      const rec = { lost: lost, gone: false };
      titles(prefix, id, json, intent.n === id).forEach((title, i) =>
        outbox.push({ title: title, rec: rec, n: id, i: i }));
      pump();
      return true;
    };
  }

  // Emacs's answer for a title it read (see Intents above), to the
  // bridge whose it was.  One no bridge set is cleared, as Emacs did
  // before; one a bridge set, but Emacs could not name, is left until
  // read or given up.  False with no bridge: Emacs clears the title.
  function answer(call) {
    if (!titleBridges.length) return false;
    if (titleBridges.some((b) => b.answer(call))) return true;
    if (typeof call.n !== "number" && typeof call.idle === "string" &&
        !titleBridges.some((b) => b.showing())) {
      document.title = call.idle;
    }
    return true;
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
  //           or let this page go while the browser had it frozen (a
  //           phone asleep).  The page loads again, and comes back with a
  //           soft hello, which takes nothing another page has meanwhile.
  //           A page with a BACK comes back where it is instead -- its
  //           scroll, what it has open, nothing loaded again -- and BACK
  //           asks Emacs again for what it shows, as its load did; only
  //           while it is made of the files Emacs serves now (Emacs names
  //           them, "ui", with the hello and with the 410): one loaded
  //           before they changed loads again
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
  function servedBridge(opts) {
    if (served) return served;
    const page = Array.from(crypto.getRandomValues(new Uint8Array(8)),
      (b) => b.toString(16).padStart(2, "0")).join("");
    const back = opts && typeof opts.back === "function" ? opts.back : null;
    let soft = comingBack();
    const outbox = [];
    let hello = false, greeting = false, sending = false, polling = false, over = false;
    let retry = null, delay = 0;
    // What the page is made of, as Emacs said in answer to its first
    // hello; and whether the hello under way is the page coming back.
    let ui = null, returning = false;

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
          e.ui = said && said.ui;
          throw e;
        });
      });
    }
    function stop(msg) {
      over = true;
      closeQr();
      notice(msg);
      outbox.splice(0).forEach(giveUp);
    }
    // A request failed: do what Emacs said, or ask again later.
    function trouble(e) {
      if (over || retry) return;
      if (e.status === 410 && e.why === "free" && back && ui && e.ui === ui) {
        comeBack();
      } else if (e.status === 410 && e.why === "free") {
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
      if (greeting) return;
      greeting = true;
      ask("hello", {}).then((r) => {
        greeting = false;
        hello = true;
        if (!ui) ui = r.headers.get("Xwapp-Ui");
        answered();
        if (returning) welcomeBack();
        next();
        pump();
      }, (e) => {
        greeting = false;
        trouble(e);
      });
    }
    // Emacs let the page go, and can have it again: a soft hello from
    // where it is, taking nothing another page has meanwhile.  What was
    // under way when it was let go -- its next, an intent not taken --
    // goes again once it is the app's.
    function comeBack() {
      if (returning) return;
      returning = true;
      hello = false;
      soft = true;
      connect();
    }
    // The app's page again: Emacs, which closed what it was doing for it,
    // is told again what it asked on its load -- whether you look, and
    // BACK's -- before the intents that waited.
    function welcomeBack() {
      returning = false;
      const waited = outbox.splice(0);
      seen = null;
      look();
      try {
        back();
      } catch (e) {
        console.error(e);
      }
      outbox.push(...waited);
    }
    // An intent Emacs answers it cannot take -- unreadable (400), too big
    // (413) -- is lost, not asked again: asked again, it would be refused
    // again, and hold up every intent after it.
    function pump() {
      if (!hello || sending || over || retry || !outbox.length) return;
      sending = true;
      ask("intent", outbox[0].intent).then(() => {
        outbox.shift();
        sending = false;
        answered();
        pump();
      }, (e) => {
        sending = false;
        if (e.status === 400 || e.status === 413) {
          giveUp(outbox.shift());
          answered();
          pump();
          return;
        }
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
      outbox.push({ intent: { op: "xwapp-seen", seen: now } });
      pump();
    }
    document.addEventListener("visibilitychange", look);
    window.addEventListener("focus", look);
    window.addEventListener("blur", look);
    look();

    connect();

    served = function emit(op, extra, lost) {
      const intent = Object.assign({ op: op }, extra || {});
      if (JSON.stringify(intent).length > INTENT_MAX) return false;
      outbox.push({ intent: intent, lost: lost });
      pump();
      return true;
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

  function bridge(prefix, app, opts) {
    if (BROWSER) { navSetup(app); return (kitEmit = servedBridge(opts)); }
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

  // ---- Files Emacs serves the page by name ------------------------------
  //
  // In a browser under the page's secret, as the page itself is; in the
  // xwidget, a page of file:, from their directory, which Emacs says in
  // answer to the hello.  Another set's are reached in a browser alone:
  // a document served to be read, not run, would run as a file:.

  let filesBase = BROWSER ? ROOT + ".files/" : "";
  function file(name, set) {
    if (set !== undefined && set !== "files") {
      return BROWSER && /^[a-z][a-z0-9-]*$/.test(String(set))
        ? ROOT + "." + set + "/" + encodeURIComponent(String(name)) : "";
    }
    return filesBase ? filesBase + encodeURIComponent(String(name)) : "";
  }

  // ---- ⌘V in Emacs -----------------------------------------------------
  //
  // In WebKit under Emacs, ⌘V never becomes a paste: the paste is the
  // menu's to send, and Emacs's menu does not send it to the web view, so
  // the page sees the keydown and nothing after -- not for text either.
  // So on that key in a field of the page's the kit asks Emacs for its
  // clipboard, and Emacs answers XW.kit({xwapp: "paste", text}).  The
  // field focused then hears it first -- "xw-paste", cancelable, its
  // detail {text} -- and then the text goes in at the caret as typing
  // would: through execCommand, so the page's own input events fire and
  // the undo stack keeps it (checked in WKWebView).  "" is a clipboard
  // with no text: an image, say, which the page may ask Emacs for.
  // Only on your own key, and only in Emacs: in a browser ⌘V is a paste
  // of its own, from that device's clipboard.

  const TEXT_INPUT = /^(text|search|url|tel|email|password)$/i;
  function typable(el) {
    if (!el || el.disabled || el.readOnly) return false;
    if (el.isContentEditable || el.tagName === "TEXTAREA") return true;
    return el.tagName === "INPUT" && TEXT_INPUT.test(el.type || "text");
  }

  // Pastes asked for and not answered yet: an answer for none is dropped.
  let pasting = 0;
  if (!BROWSER) {
    document.addEventListener("keydown", (ev) => {
      if (!ev.isTrusted || !ev.metaKey || ev.ctrlKey || ev.altKey) return;
      if (String(ev.key).toLowerCase() !== "v" || !kitEmit || !typable(document.activeElement)) return;
      ev.preventDefault();
      pasting++;
      kitEmit("xwapp-paste", null, () => {
        pasting = Math.max(0, pasting - 1);
        return () => pasting++;
      });
    }, true);
  }

  function paste(call) {
    if (BROWSER || pasting <= 0) return;
    pasting--;
    const el = document.activeElement;
    if (!typable(el)) return;
    const text = typeof call.text === "string" ? call.text : "";
    const heard = el.dispatchEvent(new CustomEvent("xw-paste",
      { bubbles: true, cancelable: true, detail: { text: text } }));
    if (!heard || !text) return;
    if (document.execCommand("insertText", false, text)) return;
    // A WebKit without it: the same text, and an input event.
    if (typeof el.setRangeText === "function") {
      el.setRangeText(text, el.selectionStart, el.selectionEnd, "end");
      el.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertFromPaste", data: text }));
    }
  }

  // Emacs's calls to the kit itself: in a browser through next, in the
  // xwidget as XW.kit(call).
  function kit(call) {
    if (call.xwapp === "read" || call.xwapp === "unread") {
      return answer(call);
    } else if (call.xwapp === "paste") {
      paste(call);
    } else if (call.xwapp === "phone") {
      document.documentElement.classList.toggle("xw-phone", !ELSEWHERE);
    } else if (call.xwapp === "qr") {
      showQr(call);
    } else if (call.xwapp === "files" && !BROWSER && /^file:\/\/\//.test(String(call.base || ""))) {
      filesBase = String(call.base).replace(/\/?$/, "/");
      document.dispatchEvent(new CustomEvent("xw-files"));
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
    file: file,
  });
})();
