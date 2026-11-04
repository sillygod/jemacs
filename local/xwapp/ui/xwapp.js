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
 *                        shows (see "In a web browser"); OPTS.ready true:
 *                        the app calls XW.ready once its first view is
 *                        drawn, which ends its load's timing
 *   XW.mark(name)        a moment of the page's load or wake, for its
 *                        timings (see "How long it took"): a-z0-9-
 *   XW.ready(name)       that, and the page is ready: its load is over
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
  //   taken   another page is: this one stops, and says so, with a
  //           button to use it here again -- a hello that takes it, as
  //           a load's does; the other page is told it is taken.  A page
  //           with a BACK goes on where it is, as when it comes back
  //           (below); one without, or loaded before its files changed,
  //           loads again
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
  // again, less often each time, until it does; "not answering" is said
  // once a second try has failed too, not on the first, and unsaid once
  // anything is answered.  A next is held until Emacs has calls, so a
  // try made while that is said goes with whether you look, which Emacs
  // answers at once.
  //
  // What the page asked before it was the app's again -- an intent under
  // way when it was let go, or taken -- is of the page as it was: its
  // answer is no word on the page as it is.  An intent so answered as
  // not taken goes again; one taken is done with.
  //
  // A phone that wakes: what the page had under way when it slept --
  // its next, held -- died with the connection, and fails, or hangs,
  // once it wakes.  So when the page comes back -- shown again after a
  // while hidden, from the back-forward cache, the network back -- a try
  // that waits runs at once, and a next asked before it slept is asked
  // again at once, the old one given up: one is ever asking.  A wake is
  // no outage: the failure of what slept is said nothing of, and a try
  // that waits for a later one is not held back by the page's backoff --
  // which is kept: Emacs gone indeed, the tries after that one wait as
  // long as they would have.

  // Served by Emacs: here, or to another device through a proxy (https).
  const BROWSER = location.protocol !== "file:";
  // On another device already, through the proxy.
  const ELSEWHERE = BROWSER && !/^(127\.0\.0\.1|localhost)$/.test(location.hostname);
  const ROOT = "/" + location.pathname.split("/")[1] + "/";
  // S with no secret in it: the page's own, nor any 32 hex digits, which
  // a secret is.  For what the page tells of itself (its timings).
  function unsecret(s) {
    s = String(s).replace(/[0-9a-f]{32,}/gi, "…");
    const secret = ROOT.slice(1, -1).toLowerCase();
    if (secret) s = s.split(secret).join("…");
    return s.slice(0, 120);
  }
  const SOFT = "xwapp-soft"; // sessionStorage: this load is the page coming back
  // Hidden this long, the page may have slept: what it had asking, asks again.
  const WAKE_AWAY = 1000;

  const SAY = {
    taken: "This page is open elsewhere now.",
    useHere: "Use it here",
    closed: "Emacs closed this page. Reload to use it here.",
    unknown: "Emacs does not know this page any more. Open it again from Emacs.",
    waiting: "Emacs is not answering. Trying again\u2026 (or open the page again from Emacs)",
  };

  // MSG said across the top of the page, or nothing; ACT, {label, run},
  // a button after it.
  function notice(msg, act) {
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
    if (act) {
      const b = document.createElement("button");
      b.type = "button";
      b.className = "xw-notice-act";
      b.textContent = act.label;
      b.addEventListener("click", act.run, { once: true });
      el.append(" ", b);
    }
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

  // ---- How long it took -------------------------------------------------
  //
  // A phone's page is "sometimes" slow, and a slow moment is over before
  // anyone looks.  So the page times its moments and sends the timings
  // to Emacs, which joins them with its own and keeps them -- the slow
  // ones above all (xwapp.el, "How long the page took").  A moment is:
  //
  //   open, reload  from the tap -- the navigation's start -- until the
  //                 page is ready: when the app says so (XW.ready, its
  //                 bridge made with {ready: true}), else once Emacs's
  //                 first calls have run
  //   wake          from the page shown again after a while hidden (or
  //                 back from the back-forward cache, or online again)
  //                 to Emacs's answer to a next asked after it -- asked
  //                 with wake=1, which Emacs answers at once
  //   intent        an intent slow to be answered, from when the page
  //                 queued it -- its turn waited for, the mark "sent" --
  //                 the page shown throughout
  //
  // ended early when the page is hidden ("hidden"), or after a minute
  // ("cap").  Its marks: boot (the kit run), dcl, hello, intent and next
  // (the first answered), and the app's (XW.mark); and each request it
  // made, from Resource Timing: connecting, the wait for the first byte,
  // the rest, the bytes -- the page's files all, the kit's calls only the
  // first of each, the one that ended it and a slow one.  The kit's
  // requests are numbered (n=), for Emacs to know each by.  Sent only to
  // an Emacs whose hello said it takes them, and from when they are slow
  // (Xwapp-Timing, in ms): an Emacs from before says nothing, and is sent
  // nothing.  Paths after the secret alone: no secret, nothing shown.
  // Nothing runs while the page is idle: a moment is begun and ended by
  // the events that make it, and read once it is over.

  // The served bridge's marks, once there is one.
  let moments = null;
  const MARK = /^[a-z][a-z0-9-]{0,23}$/;
  function mark(name) {
    if (moments && MARK.test(String(name))) moments.mark(String(name));
  }
  function ready(name) {
    if (moments && MARK.test(String(name))) moments.ready(String(name));
  }

  let served = null;
  function servedBridge(opts) {
    if (served) return served;
    const page = Array.from(crypto.getRandomValues(new Uint8Array(8)),
      (b) => b.toString(16).padStart(2, "0")).join("");
    const back = opts && typeof opts.back === "function" ? opts.back : null;
    let soft = comingBack();
    // Its moments (see "How long it took").
    const perf = window.performance;
    const timeable = !!(perf && perf.now && perf.getEntriesByType);
    const byApp = !!(opts && opts.ready);   // the app says when it is ready
    const SLOW_REQ = 300;   // ms: a call of the kit's this slow is listed
    const CAP = 60000;      // ms: a moment not over by then ends
    const REQS = 40;        // requests a record lists at most
    let slowMs = null;      // Emacs's word, with its hello: it takes them
    let moment = null;      // {k, t0, base, marks, first, last, why, away}
    let numbered = 0;       // the kit's requests so far
    let wakeNext = false;   // the next asked after a wake: wake=1
    let hiddenLast = -1;    // when the page was last hidden
    // A timing's fault must never stop the page asking: each is done so.
    function timing(fn) {
      if (!timeable) return;
      try {
        fn();
      } catch (e) {
        console.error(e);
      }
    }
    const outbox = [];
    let hello = false, greeting = false, polling = false, over = false;
    // The intent going now, of OUTBOX: it leaves it once Emacs has it.
    let sending = null;
    // WAITING: "not answering" is said.
    let retry = null, delay = 0, failed = 0, waiting = false;
    // The next asking now, as its AbortController, and since when; and
    // since when the page has been hidden, if it is.
    let poll = null, polledAt = 0, hiddenAt = 0;
    // What the page is made of, as Emacs said in answer to its first
    // hello; and whether the hello under way is the page coming back.
    let ui = null, returning = false;
    // One more each time the page sets out to be the app's again: what
    // it asked before is of the page as it was.
    let era = 0;

    // TAG, if given, is told the request's number, as tag.n.
    function ask(path, body, signal, tag) {
      const opts = body === undefined ? {} : {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body),
      };
      if (signal) opts.signal = signal;
      const n = ++numbered;
      if (tag) tag.n = n;
      const also = (path === "hello" && soft ? "&soft=1" : "") + (path === "next" && wakeNext ? "&wake=1" : "");
      return fetch(ROOT + path + "?page=" + page + "&n=" + n + also, opts).then((r) => {
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
    // The next asking now given up: what it brings, if it comes, is of
    // the page as it was (next).
    function letPollGo() {
      if (!poll) return;
      const old = poll;
      poll = null;
      polling = false;
      old.abort();
    }
    function stop(msg, act) {
      over = true;
      waiting = false;
      if (retry) {
        clearTimeout(retry);
        retry = null;
      }
      letPollGo();
      closeQr();
      notice(msg, act);
      outbox.splice(0).forEach(giveUp);
    }
    // A request failed: do what Emacs said, or ask again later.  What
    // Emacs said goes before a try that waits: it is the page's word now.
    function trouble(e) {
      if (over) return;
      if (e.status === 410 || e.status === 404) {
        if (retry) {
          clearTimeout(retry);
          retry = null;
        }
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
        } else if (e.status === 410 && e.why === "closed") {
          stop(SAY.closed);
        } else if (e.status === 410) {
          stop(SAY.taken, { label: SAY.useHere, run: useHere });
        } else {
          stop(SAY.unknown);
        }
        return;
      }
      if (retry) return;
      failed++;
      if (failed > 1) {
        waiting = true;
        notice(SAY.waiting);
      }
      delay = Math.min(10000, delay * 2 || 1000);
      retry = setTimeout(() => {
        retry = null;
        connect();
      }, delay);
    }
    function answered() {
      failed = 0;
      delay = 0;
      if (!waiting) return;
      waiting = false;
      notice(null);
    }
    // The page is back -- shown again, restored, online.  STALE: the next
    // asking now was asked before it slept, and is asked again.  A try
    // that waits goes now; the backoff stays as it is, for the tries
    // after it, should this one fail too.
    // WHY it woke, and AWAY, how long it was hidden, for its moment.
    function wake(stale, why, away) {
      if (over) return;
      if (stale) {
        letPollGo();
        wakeNext = true;
        // What the moment asks is all the timings it needs.
        timing(() => {
          perf.clearResourceTimings();
          begin("wake", perf.now(), { why: why, away: away });
        });
      }
      if (retry) {
        clearTimeout(retry);
        retry = null;
      }
      connect();
    }
    document.addEventListener("visibilitychange", () => {
      if (document.visibilityState === "hidden") {
        hiddenAt = Date.now();
        timing(() => {
          hiddenLast = perf.now();
          end("hidden");
        });
        return;
      }
      const away = hiddenAt;
      hiddenAt = 0;
      wake(!!away && Date.now() - away >= WAKE_AWAY && polledAt <= away, "visible", away ? Date.now() - away : 0);
    });
    window.addEventListener("pageshow", (e) => { if (e.persisted) wake(true, "pageshow"); });
    window.addEventListener("online", () => wake(true, "online"));
    // A long while idle, the browser's timings fill up: emptied then --
    // or, in a moment, given room until it is over.
    if (timeable && perf.addEventListener) {
      perf.addEventListener("resourcetimingbufferfull", () => {
        try {
          if (moment) perf.setResourceTimingBufferSize(1000);
          else perf.clearResourceTimings();
        } catch (_) {}
      });
    }

    function begin(k, t0, extra) {
      if (moment) clearTimeout(moment.cap);
      moment = Object.assign({ k: k, t0: t0, base: Date.now() - perf.now(), marks: {}, first: {}, last: 0 },
                             extra || {});
      moment.cap = setTimeout(() => timing(() => end("cap")), Math.max(0, CAP - (perf.now() - t0)));
    }
    function markNow(name) {
      if (moment && !(name in moment.marks)) moment.marks[name] = Math.max(0, Math.round(perf.now() - moment.t0));
    }
    // The moment over, HOW; LAST, the number of the request that ended it.
    function end(how, last) {
      const m = moment;
      if (!m) return;
      moment = null;
      clearTimeout(m.cap);
      if (last) m.last = last;
      if (slowMs !== null) sendTimings(m, perf.now(), how);
    }
    function loading() {
      return moment && (moment.k === "open" || moment.k === "reload");
    }
    // The app's word: it is ready -- the end of its load, when it said it
    // would say so.
    function readyNow(name) {
      if (!moment) return;
      markNow(name);
      if (loading() && byApp) end("ready");
    }
    // An intent answered: queued at FROM, sent at SENT, numbered N.  One
    // slow from its queueing on is a moment of its own -- the time it
    // waited its turn (behind another, slow or stuck) its mark "sent".
    function intentAnswered(op, from, sent, n) {
      if (!timeable) return;
      if (moment) {
        if (!moment.first.intent) moment.first.intent = n;
        markNow("intent");
        return;
      }
      const now = perf.now();
      if (slowMs !== null && op !== "xwapp-timing" && now - from >= slowMs && hiddenLast < from) {
        sendTimings({ k: "intent", t0: from, base: Date.now() - now, first: { intent: n }, last: n,
                      marks: sent - from >= 1 ? { sent: Math.round(sent - from) } : {} }, now, "answered");
      }
    }
    // A next answered, TAG its own, with COUNT calls, which have run.
    function nextAnswered(tag, count) {
      if (!moment) return;
      if (count && !moment.first.next) {
        moment.first.next = tag.n;
        moment.marks.next = Math.max(0, Math.round(tag.at - moment.t0));
      }
      if (moment.k === "wake" && tag.wake) end("next", tag.n);
      else if (loading() && !byApp && count) end("next", tag.n);
    }
    // A request of the page's, from Resource Timing, as sent: its path
    // after the secret -- of a file served by name, its set alone -- and
    // its times, ms from the moment's start.  Null when not the page's.
    function timedReq(e, m) {
      let u;
      try {
        u = new URL(e.name, location.href);
      } catch (_) {
        return null;
      }
      if (u.origin !== location.origin || u.pathname.indexOf(ROOT) !== 0) return null;
      let p = u.pathname.slice(ROOT.length);
      const kit = /^(hello|next|intent)$/.test(p);
      if (/^\.[a-z][a-z0-9-]*\//.test(p)) p = p.split("/")[0] + "/…";
      const ms = (x) => Math.max(0, Math.round(x));
      const sent = e.requestStart > 0 ? e.requestStart : e.startTime;
      const first = e.responseStart > 0 ? e.responseStart : e.responseEnd;
      const r = { p: unsecret(p), s: ms(e.startTime - m.t0), q: ms(sent - e.startTime), w: ms(first - sent),
                  dl: ms(e.responseEnd - first) };
      if (kit && Number(u.searchParams.get("n")) > 0) r.n = Number(u.searchParams.get("n"));
      if (e.domainLookupEnd > e.domainLookupStart) r.dns = ms(e.domainLookupEnd - e.domainLookupStart);
      if (e.connectEnd > e.connectStart) r.tcp = ms(e.connectEnd - e.connectStart);
      if (e.secureConnectionStart > 0 && e.connectEnd > e.secureConnectionStart) r.tls = ms(e.connectEnd - e.secureConnectionStart);
      if (e.transferSize > 0) r.b = e.transferSize;
      else if (e.encodedBodySize > 0) r.b = e.encodedBodySize;
      if (e.encodedBodySize > 0 && e.decodedBodySize > e.encodedBodySize) r.z = 1;
      if (!kit && e.transferSize === 0 && e.decodedBodySize > 0) r.c = 1;
      return r;
    }
    function listed(r, e, m) {
      if (r.p === "hello") return true;
      if (r.p === "next") return r.n === m.first.next || r.n === m.last || r.dl >= SLOW_REQ;
      if (r.p === "intent") return r.n === m.first.intent || r.n === m.last || e.duration >= SLOW_REQ;
      return loadingKind(m.k) || e.duration >= SLOW_REQ;
    }
    function loadingKind(k) {
      return k === "open" || k === "reload";
    }
    // Moment M, over at UPTO, HOW, sent to Emacs as the kit's intent.
    function sendTimings(m, upTo, how) {
      const reqs = [];
      let omit = 0;
      const navs = loadingKind(m.k) ? perf.getEntriesByType("navigation") : [];
      if (navs[0] && navs[0].domContentLoadedEventEnd > 0 && !("dcl" in m.marks)) {
        m.marks.dcl = Math.round(navs[0].domContentLoadedEventEnd - m.t0);
      }
      navs.concat(perf.getEntriesByType("resource")).forEach((e) => {
        if (e.startTime < m.t0 - 1 || e.startTime > upTo) return;
        const r = timedReq(e, m);
        if (!r) return;
        if (!listed(r, e, m) || reqs.length >= REQS) omit++;
        else reqs.push(r);
      });
      try { perf.clearResourceTimings(); } catch (_) {}
      const rec = { op: "xwapp-timing", k: m.k, at: Math.round(m.base + m.t0), d: Math.max(0, Math.round(upTo - m.t0)),
                    end: how, m: m.marks, r: reqs };
      if (omit) rec.omit = omit;
      if (m.why) rec.why = m.why;
      if (m.away) rec.away = Math.round(m.away);
      outbox.push({ intent: rec });
      pump();
    }
    moments = { mark: (name) => timing(() => markNow(name)), ready: (name) => timing(() => readyNow(name)) };
    function connect() {
      if (hello) {
        if (waiting) {
          seen = null;
          look();
        }
        next();
        pump();
        return;
      }
      if (greeting) return;
      greeting = true;
      ask("hello", {}).then((r) => {
        greeting = false;
        const made = r.headers.get("Xwapp-Ui");
        const slow = r.headers.get("Xwapp-Timing");
        slowMs = slow && /^\d{1,7}$/.test(slow) ? Number(slow) : null;
        timing(() => markNow("hello"));
        // Back, but loaded before the files Emacs serves now: loads again,
        // as the app's page, which it is.
        if (returning && !(ui && made === ui)) {
          over = true;
          location.reload();
          return;
        }
        hello = true;
        if (!ui) ui = made;
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
      era++;
      hello = false;
      soft = true;
      letPollGo();
      connect();
    }
    // Taken by another page, and you would have it here: a hello that
    // takes it, and the page goes on where it is, as one that comes back.
    // One that cannot -- no BACK -- loads again, which takes it as well.
    function useHere() {
      if (!over) return;
      if (!back) {
        location.reload();
        return;
      }
      over = false;
      notice(null);
      returning = true;
      era++;
      hello = false;
      soft = false;
      connect();
    }
    // The app's page again: Emacs, which closed what it was doing for it,
    // is told again what it asked on its load -- whether you look, and
    // BACK's -- before the intents that waited, less any word of whether
    // you looked, which look() has just said as it is now.
    function welcomeBack() {
      returning = false;
      const waited = outbox.splice(0).filter((rec) => rec.intent.op !== "xwapp-seen");
      seen = null;
      look();
      try {
        back();
      } catch (e) {
        console.error(e);
      }
      outbox.push(...waited);
    }
    // REC, which Emacs has, out of the outbox, wherever it is now.
    function sent(rec) {
      const i = outbox.indexOf(rec);
      if (i >= 0) outbox.splice(i, 1);
    }
    // An intent Emacs answers it cannot take -- unreadable (400), too big
    // (413) -- is lost, not asked again: asked again, it would be refused
    // again, and hold up every intent after it.  One asked of the page as
    // it was, and not taken, goes again, now.
    function pump() {
      if (!hello || sending || over || retry || !outbox.length) return;
      const rec = (sending = outbox[0]), mine = era, tag = {};
      const t = timeable ? perf.now() : 0;
      ask("intent", rec.intent, undefined, tag).then(() => {
        sending = null;
        sent(rec);
        answered();
        timing(() => intentAnswered(rec.intent.op, rec.at || t, t, tag.n));
        pump();
      }, (e) => {
        sending = null;
        if (e.status === 400 || e.status === 413) {
          sent(rec);
          giveUp(rec);
          answered();
          pump();
          return;
        }
        if (mine !== era) {
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
    // One that was given up for another (wake, the page the app's again)
    // is done with: its calls, if they came after all, run -- Emacs sent
    // them before it had the other -- and nothing else.
    function next() {
      if (over || polling || retry) return;
      polling = true;
      const mine = (poll = new AbortController());
      polledAt = Date.now();
      mine.wake = wakeNext;
      ask("next", undefined, mine.signal, mine).then((r) => r.json()).then((calls) => {
        if (mine !== poll) {
          calls.forEach(run);
          return;
        }
        poll = null;
        polling = false;
        if (mine.wake) wakeNext = false;
        if (timeable) mine.at = perf.now();
        answered();
        calls.forEach(run);
        timing(() => nextAnswered(mine, calls.length));
        next();
      }, (e) => {
        if (mine !== poll) return;
        poll = null;
        polling = false;
        trouble(e);
      });
    }

    // Whether you could be looking, for Emacs to ask with xwapp-seen-p.
    // Said anew, it is all Emacs needs of it: an older word not yet sent
    // is dropped.
    let seen = null;
    function look() {
      const now = document.visibilityState === "visible" && document.hasFocus();
      if (now === seen) return;
      seen = now;
      for (let i = outbox.length - 1; i >= 0; i--) {
        if (outbox[i] !== sending && outbox[i].intent.op === "xwapp-seen") outbox.splice(i, 1);
      }
      outbox.push({ intent: { op: "xwapp-seen", seen: now } });
      pump();
    }
    document.addEventListener("visibilitychange", look);
    window.addEventListener("focus", look);
    window.addEventListener("blur", look);

    // Its load is a moment, from the navigation's start: a reload, when
    // the browser reloaded it or it came back after Emacs let it go.
    if (document.visibilityState === "visible") {
      timing(() => {
        const nav = perf.getEntriesByType("navigation")[0];
        begin(soft || (nav && nav.type === "reload") ? "reload" : "open", 0, soft ? { why: "back" } : null);
        markNow("boot");
      });
    }

    look();
    connect();

    served = function emit(op, extra, lost) {
      const intent = Object.assign({ op: op }, extra || {});
      if (JSON.stringify(intent).length > INTENT_MAX) return false;
      // When it was queued: a slow one is timed from then.
      outbox.push({ intent: intent, lost: lost, at: timeable ? perf.now() : 0 });
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
    mark: mark,
    ready: ready,
  });
})();
