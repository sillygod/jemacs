(() => {
  "use strict";

  const PREFIX = "ghmem:";
  const $ = (id) => document.getElementById(id);
  const app = $("app");
  const statusEl = $("status");
  const btnBack = $("btn-back");
  const tabsEl = document.querySelector(".tabs");

  // ---- Intents ---------------------------------------------------------
  //
  // Queued and counted by xwapp's page kit (../../xwapp/ui/xwapp.js).  A
  // page in a browser that Emacs let go -- a phone asleep -- comes back
  // where it is (`comeBack'), rather than loading again.

  // Ready -- its load over, for the kit's timings -- once the view it
  // opened on is drawn (READY_BY).
  const emit = XW.bridge(PREFIX, null, { back: comeBack, ready: true });

  // ---- A phone ---------------------------------------------------------
  //
  // style.css lays the page out for a narrow screen and a finger; here,
  // what it says and starts with there.  A touch screen has no keys to
  // hint at; a narrow one starts a room as its chat, one column at a
  // time being the less of the two.

  const TOUCH = window.matchMedia("(pointer: coarse)").matches;
  const NARROW = window.matchMedia("(max-width: 640px)").matches;
  const SLASH = TOUCH ? "" : "  ( / )";
  const SENDS = TOUCH ? "" : "  (⌘↩ / C-↩ sends)";

  function chatHint(to) {
    const a = agentByName(to);
    if (a && a.suggestion) return suggestionHint(a);
    if (to === "@primary") return "Message primary" + (TOUCH ? "" : "  (⌘↩ sends)");
    return "Message " + (to === "@lead" ? "the lead" : to) + "  (@name for another" + (TOUCH ? ")" : " · ⌘↩ sends)");
  }

  // What claude shows only on its screen, as Emacs read it there: work it
  // has running in the background, and the prompt it suggests you send
  // next, faint in its input box.  Here that is the box's placeholder,
  // and Use -- or Tab in an empty box, as in the CLI -- puts it in.
  function agentByName(name) {
    return ((herdNow.data && herdNow.data.agents) || []).find((a) => a.name === name) || null;
  }

  function suggestionHint(a) {
    return "↳ " + a.suggestion + (TOUCH ? "" : "  (Tab takes it)");
  }

  function promptHint(name) {
    const a = agentByName(name);
    return a && a.suggestion ? suggestionHint(a) : "Prompt for " + name + SENDS;
  }

  function useButton(name) {
    const a = agentByName(name);
    return '<button type="button" class="ghost small use" data-act="a:use-suggestion"' + (a && a.suggestion ? "" : " hidden") +
      ' title="Put what it suggests in the box">Use</button>';
  }

  // The composer is one line that grows with what you type, up to a cap
  // (CSS max-height) past which it scrolls -- but only on the phone, where
  // every line costs the conversation.  A desktop keeps its fixed box and
  // its drag-to-resize, so this leaves it alone.
  const phone = () => window.matchMedia("(max-width: 640px)").matches;
  function autoGrow(ta) {
    if (!ta || !phone()) return;
    // rows=2 in the markup (the desktop's box) would keep an empty textarea
    // two lines tall; drop to one row and grow with the text.  Empty, it
    // rests at the CSS one-line min-height -- a long placeholder must not
    // puff `scrollHeight' up to the two lines it would wrap onto.
    ta.rows = 1;
    if (!ta.value) { ta.style.height = ""; return; }
    ta.style.height = "auto";
    const max = parseInt(getComputedStyle(ta).maxHeight, 10);
    ta.style.height = (max ? Math.min(ta.scrollHeight, max) : ta.scrollHeight) + "px";
  }

  // A box drawn once, kept as you type: its placeholder and Use follow
  // what the agent suggests now.
  function suggestIn(box, name, hint) {
    const ta = box.querySelector("textarea.agent-text");
    const use = box.querySelector(".use");
    const a = agentByName(name);
    if (ta && ta.placeholder !== hint) ta.placeholder = hint;
    if (use) use.hidden = !(a && a.suggestion);
    autoGrow(ta);
  }

  function useSuggestion(name, ta) {
    const a = agentByName(name);
    if (!a || !a.suggestion || !ta) return false;
    ta.value = a.suggestion;
    herdNow.compose[name] = ta.value;
    ta.focus();
    ta.setSelectionRange(ta.value.length, ta.value.length);
    return true;
  }

  // Its sub-agents as their transcripts have them, rather than as its
  // footer counts them.
  function backgroundHtml(a) {
    const subs = a.subs || [];
    const bg = (a.background || []).filter((w) => !(subs.length && /^\d+ agents?$/.test(w)));
    if (subs.length) bg.push(subs.length + (subs.length === 1 ? " sub-agent" : " sub-agents"));
    const title = "Running in the background, as its screen says" +
      (subs.length ? "; its sub-agents: " + subs.map((u) => u.title || u.who || "?").join(", ") : "");
    return bg.length ? '<span class="det bgw" title="' + esc(title) + '">' + esc(bg.join(" · ")) + " running</span>" : "";
  }

  // ---- What a CLI holds queued --------------------------------------------
  //
  // A message typed to an agent at work -- at its terminal, or sent from
  // here while it is busy -- waits in its CLI's queue until the agent can
  // read it: claude at its next step, grok and agy once the turn ends.
  // Emacs reads the queue off the agent's screen (a.queued: as drawn, cut
  // to a line by grok and agy).  Its room ends with each, faint, until its
  // conversation says it: that entry is then the message, and this one
  // goes.  Gone from the screen before the transcript has it, it stays a
  // moment (QUEUE_LINGER) rather than blink out and back in.
  const QUEUE_LINGER = 10000;
  const QUEUE_WHEN = { claude: "it reads it after its current step", grok: "it reads it after this turn",
                       agy: "it reads it after this turn" };
  let queueTimer = 0;

  // Each agent's queue as last read, every message with when the page
  // first saw it and when it left the screen.
  function trackQueued(agents) {
    const now = Date.now(), next = {};
    let lingering = false;
    (agents || []).forEach((a) => {
      const left = (herdNow.queued[a.name] || []).slice(), cur = [];
      (a.queued || []).forEach((text) => {
        // The same one, though drawn cut before and whole now (agy cuts
        // the last to fit its alt+m), or the other way.
        let k = left.findIndex((q) => !q.gone && q.text === text);
        if (k < 0) k = left.findIndex((q) => sameQueued(q.text, text));
        if (k >= 0) {
          const q = left.splice(k, 1)[0];
          cur.push(Object.assign(q, { text, gone: 0 }));
        } else cur.push({ text, seen: now, gone: 0 });
      });
      const gone = left.filter((q) => (q.gone = q.gone || now) > now - QUEUE_LINGER);
      if (gone.length) lingering = true;
      if (gone.length || cur.length) next[a.name] = gone.concat(cur);
    });
    herdNow.queued = next;
    clearTimeout(queueTimer);
    if (lingering) queueTimer = setTimeout(() => { if (herdNow.data) GM.renderHerdAgain(); }, QUEUE_LINGER / 2);
  }

  // A message's words, to tell two reads of it alike: no spaces, and a
  // CLI's cut ("…", "...") off its end.
  function qwords(t) { return String(t || "").replace(/\s+/g, "").replace(/(?:…|\.\.\.)$/, ""); }
  const qcut = (t) => /(?:…|\.\.\.)$/.test(String(t || "").trim());

  // Whether A and B are one message read twice: alike, or one cut short
  // of the other.
  function sameQueued(a, b) {
    const A = qwords(a), B = qwords(b);
    return !!A && !!B && (A === B || (qcut(a) && B.startsWith(A)) || (qcut(b) && A.startsWith(B)));
  }

  // Whether ENTRIES, a conversation's, already say Q was said to the agent
  // (SAID picks what was said to it): delivered, its own entry there.
  // Among the newest only, and not long before the queue showed it.  A
  // herd message the conversation says by its body; the queue, whole.
  function queuedSaid(q, entries, said) {
    const a = qwords(q.text), cut = qcut(q.text);
    if (!a) return false;
    return (entries || []).slice(-12).some((e) => {
      if (!said(e) || Date.parse(e.ts) < q.seen - 60000) return false;
      const b = qwords(e.text);
      return !!b && (a === b || (cut && b.startsWith(a)) || (b.length >= 8 && a.includes(b.slice(0, 40))));
    });
  }

  // What of NAME's queue its conversation, ENTRIES, does not say yet.
  function queuedLeft(name, entries, said) {
    return (herdNow.queued[name] || []).filter((q) => !queuedSaid(q, entries, said));
  }

  // The column's own entries said to it, and the chat's said to NAME.
  const saidToCol = (e) => e.role === "user" && e.kind !== "note" && e.kind !== "command";
  const saidToIn = (name) => (e) => e.to === name && e.who !== name && e.kind !== "turn";

  // The end of a room: each message A's CLI holds, faint, marked queued,
  // and the key that sends them now while its CLI offers one.  CHAT says
  // whom each is to.  Send now carries the first's words, for Emacs to
  // press the key only while it is still there.
  function queuedHtml(a, list, chat) {
    if (!list.length) return "";
    const first = list.find((q) => !q.gone);
    return '<div class="queued" data-name="' + esc(a.name) + '">' + list.map((q) =>
      '<div class="ent you qd' + (q.gone ? " leaving" : "") + '">' +
      entHead((chat ? "You → " + a.name : "You") + " · queued", "", null, "", "") + plainBody(q.text) + "</div>").join("") +
      '<div class="qnote"><span>' + esc((first ? "" : "sent to it · ") + (QUEUE_WHEN[a.kind] || "it reads it when it can")) + "</span>" +
      (first && a.queuedNow ? '<button type="button" class="ghost small" data-act="a:send-now" data-name="' + esc(a.name) +
        '" data-text="' + esc(first.text) + '" title="Its CLI\'s own key for it: ' + esc(a.name) +
        ' reads what is queued now, not when it can">Send now</button>' : "") + "</div></div>";
  }

  // A's card: how many messages its CLI holds queued.
  function queuedMark(a) {
    const n = (a.queued || []).length;
    return n ? '<span class="det qdw" title="' + esc("Queued, it reads them when it can:\n" + a.queued.join("\n")) + '">' +
      n + " queued</span>" : "";
  }

  // What decides whether a view's queue is drawn again.
  function queuedKey(names) {
    return names.map((n) => (herdNow.queued[n] || []).map((q) => q.text + (q.gone ? "~" : "")).join("\u0001") +
      ((agentByName(n) || {}).queuedNow ? "!" : "")).join("\u0002");
  }

  // An agent's name opens its terminal in Emacs: at the desk.  On another
  // device that would open on the desk's screen, so it is only its name.
  function agentName(name) {
    return XW.elsewhere ? '<span class="aname">' + esc(name) + "</span>"
      : '<button type="button" class="aname" data-act="a:agent-visit" title="Open in Emacs">' + esc(name) + "</button>";
  }

  // ---- Small helpers ---------------------------------------------------

  const esc = XW.esc;

  // A transcript's ts is ISO text (or ""); a source's mtime is seconds.
  function stamp(v) {
    if (typeof v === "number" && v > 0) return v * 1000;
    if (typeof v === "string" && v) {
      const t = Date.parse(/[zZ]|[+-]\d\d:?\d\d$/.test(v) ? v : v.replace(" ", "T") + "Z");
      return Number.isNaN(t) ? null : t;
    }
    return null;
  }

  function when(v) {
    const t = stamp(v);
    if (!t) return "";
    const s = Math.max(0, (Date.now() - t) / 1000);
    if (s < 60) return "just now";
    if (s < 3600) return Math.floor(s / 60) + "m ago";
    if (s < 86400) return Math.floor(s / 3600) + "h ago";
    if (s < 604800) return Math.floor(s / 86400) + "d ago";
    const d = new Date(t);
    const opts = { month: "short", day: "numeric" };
    if (d.getFullYear() !== new Date().getFullYear()) opts.year = "numeric";
    return d.toLocaleDateString(undefined, opts);
  }

  function fullTime(v) {
    const t = stamp(v);
    return t ? new Date(t).toLocaleString() : "";
  }

  function leaf(project) {
    const p = String(project || "").replace(/\/+$/, "");
    return p ? p.slice(p.lastIndexOf("/") + 1) || p : "—";
  }

  function baseName(path) {
    const p = String(path || "");
    return p.slice(p.lastIndexOf("/") + 1);
  }

  const AGENTS = ["claude", "grok", "agy"];
  const AGENT_NAME = { claude: "Claude", grok: "Grok", agy: "Agy" };

  function agentBadge(agent) {
    const a = AGENTS.includes(agent) ? agent : "other";
    return '<span class="agent a-' + a + '">' + esc(agent || "?") + "</span>";
  }

  let flashTimer = null;
  function setStatus(msg, kind) {
    clearTimeout(flashTimer);
    if (!msg) {
      statusEl.hidden = true;
      return;
    }
    statusEl.hidden = false;
    statusEl.textContent = msg;
    statusEl.className = "status" + (kind ? " " + kind : "");
    if (!kind) flashTimer = setTimeout(() => (statusEl.hidden = true), 3500);
  }

  // Onto the clipboard of the device you are at: Emacs's at the desk.
  function copy(text) {
    if (!XW.elsewhere) return emit("copy", { text: text });
    XW.copy(text).then((ok) => setStatus(ok ? "Copied" : "Could not copy here", ok ? "" : "error"));
  }

  function shortName(name) {
    const s = String(name || "");
    return s.length > 24 ? s.slice(0, 12) + "…" + s.slice(-8) : s;
  }

  // ---- Sanitizer -------------------------------------------------------
  //
  // xwapp's whitelist as it comes: no images, since a transcript holds
  // whatever an agent ever read and a remote <img> would be fetched just
  // by opening it; classes only for code blocks.

  const sanitize = XW.sanitizer();

  // "[tool Bash] npm test" is how the importers note a tool call.
  function toolMarks(src) {
    return src.replace(/^\[tool ([^\]\n`]{1,80})\]/gm, (m, name) => "`⚙ " + name + "`");
  }

  function markdown(src) {
    if (!src || !String(src).trim()) return "";
    if (window.marked && typeof window.marked.parse === "function") {
      try {
        return codeCopies(sanitize(window.marked.parse(toolMarks(String(src)), { gfm: true, breaks: true })));
      } catch (e) {
        /* fall through to plain text */
      }
    }
    return '<div class="plain">' + esc(src) + "</div>";
  }

  // Each code block gets a Copy, for the command an agent asks you to run.
  // Added after the sanitizer, which keeps no button, and in the parsed
  // markup, not the string: a "<pre" inside an attribute's value, spliced
  // around, would let the rest of that value out as markup.  A template's
  // content is inert -- nothing in it loads or runs.  The button is the
  // same for every block, its label drawn by CSS, so it adds no text to
  // the message (search marks, what a reader copies by hand); its click is
  // found by its class (`codeText').
  function codeCopies(html) {
    if (html.indexOf("<pre") < 0) return html;
    const t = document.createElement("template");
    t.innerHTML = html;
    t.content.querySelectorAll("pre").forEach((pre) => {
      if (pre.parentElement && pre.parentElement.closest("pre")) return;
      const wrap = document.createElement("div");
      wrap.className = "codewrap";
      const b = document.createElement("button");
      b.type = "button";
      b.className = "codecopy";
      b.title = "Copy the code";
      b.setAttribute("aria-label", "Copy the code");
      pre.replaceWith(wrap);
      wrap.append(b, pre);
    });
    return t.innerHTML;
  }

  // A code block's text as written: marked ends each block with a newline
  // of its own, which is not the block's; no language tag (it is a class).
  function codeText(pre) {
    return pre ? pre.textContent.replace(/\n$/, "") : "";
  }

  // In the capture phase, so a block in something that opens on a click
  // (an Inbox row) only copies.
  document.addEventListener("click", (ev) => {
    const b = ev.target instanceof Element ? ev.target.closest("button.codecopy") : null;
    if (!b) return;
    ev.preventDefault();
    ev.stopPropagation();
    const text = codeText(b.parentElement && b.parentElement.querySelector(":scope > pre"));
    if (text) copy(text);
    else setStatus("Nothing to copy", "error");
  }, true);

  // Markdown markup removed, for a title or a one-line preview: the plain text
  // of what `markdown' would render, so `**x**' reads as x, not literally.
  function stripMd(src) {
    return String(src || "")
      .replace(/```[\s\S]*?```/g, " ")              // code fences
      .replace(/`([^`]+)`/g, "$1")                  // inline code
      .replace(/\*\*([^*]+)\*\*/g, "$1")            // bold
      .replace(/__([^_]+)__/g, "$1")                // bold
      .replace(/(^|[^\w*])\*([^*\n]+)\*/g, "$1$2")  // italic *x*
      .replace(/\[([^\]]+)\]\([^)]*\)/g, "$1")      // links -> their text
      .replace(/^\s{0,3}#{1,6}\s+/gm, "")           // headings
      .replace(/^\s*[-*+]\s+/gm, "")                // list bullets
      .replace(/\s+/g, " ")
      .trim();
  }
  // esc of the markup-stripped text, for a title / preview.
  function ibPlain(src) { return esc(stripMd(src)); }

  // ---- Images a reply names ----------------------------------------------
  //
  // The sidecar copies each image an agent's reply names, under an id no
  // one can guess, and the message carries them: {id, ext, name, w, h,
  // thumb}.  Emacs serves the copies by name (XW.file) -- to a browser
  // under the page's secret, to the xwidget as files -- and nothing else:
  // the names are checked here too, so a message cannot point the page
  // anywhere.  A thumbnail under the message; pressed, the image whole.

  const IMG_ID = /^[0-9a-f]{32}$/;
  const IMG_EXTS = ["png", "jpg", "gif", "webp"];

  function imgFile(im, thumb) {
    return im.id + (thumb && im.thumb ? ".t.jpg" : "." + im.ext);
  }

  function imagesHtml(list) {
    return thumbsHtml(list) + reportsHtml(list);
  }

  function thumbsHtml(list) {
    const ok = (Array.isArray(list) ? list : []).filter((im) =>
      im && IMG_ID.test(String(im.id)) && IMG_EXTS.includes(im.ext));
    if (!ok.length) return "";
    return '<div class="eimgs">' + ok.map((im) => {
      const name = String(im.name || "image");
      const src = XW.file(imgFile(im, true));
      const w = +im.w || 0, h = +im.h || 0;
      const shape = w > 0 && h > 0 ? ' style="aspect-ratio:' + w + " / " + h + '"' : "";
      return '<button type="button" class="eimg" data-act="img:open" data-img="' + esc(imgFile(im, false)) +
        '" data-name="' + esc(name) + '" title="' + esc(name + (w ? " · " + w + "×" + h : "") + " — see it whole") + '"' + shape + ">" +
        (src ? '<img src="' + esc(src) + '" alt="' + esc(name) + '" loading="lazy" decoding="async">'
          : '<span class="eimgname">' + esc(name) + "</span>") + "</button>";
    }).join("") + "</div>";
  }

  // ---- Documents: HTML reports a reply names, and the Docs tab ----------
  //
  // A report an agent wrote as an HTML page comes among a message's
  // images, {id, ext: "html", name, title}: the sidecar's copy.  It opens
  // in a tab of its own -- a link under the page's secret (XW.file) -- or,
  // in the xwidget, in a view of Emacs's (doc-open).  Emacs serves it as a
  // document to read, never to run: no script of it runs and it reaches
  // nothing of the page's, so only what it holds itself shows.

  const DOC_SAYS = "Opens in a tab of its own, to read: no script runs in it, and only what the page holds " +
    "itself shows -- a stylesheet or picture beside it on disk does not.";
  const DOC_NAME = { docs: /^[A-Za-z0-9][A-Za-z0-9._-]*\.html$/, reports: /^[0-9a-f]{32}\.html$/ };

  // A document's name as the page opens it: SET's NAME, LABEL its text.
  function docLink(set, name, label, cls, title) {
    if (!DOC_NAME[set] || !DOC_NAME[set].test(String(name))) return "";
    const tip = esc((title ? title + " -- " : "") + DOC_SAYS);
    if (XW.browser) {
      const href = XW.file(name, set);
      return '<a class="' + cls + '" data-doc href="' + esc(href) + '" target="_blank" rel="noopener noreferrer" title="' +
        tip + '">' + label + "</a>";
    }
    return '<button type="button" class="' + cls + '" data-act="doc:open" data-set="' + set + '" data-name="' +
      esc(name) + '" title="' + tip + '">' + label + "</button>";
  }

  function reportsHtml(list) {
    const ok = (Array.isArray(list) ? list : []).filter((r) => r && IMG_ID.test(String(r.id)) && r.ext === "html");
    if (!ok.length) return "";
    return '<div class="ereports">' + ok.map((r) => {
      const name = String(r.name || "report.html");
      const title = String(r.title || "").trim();
      return docLink("reports", r.id + ".html",
        '<span class="eropen">Open</span><span class="ertitle">' + esc(title || name) + "</span>" +
        (title ? '<span class="ername">' + esc(name) + "</span>" : ""), "ereport", name);
    }).join("") + "</div>";
  }

  // The Docs tab: one list, newest first -- the documents Emacs lists
  // (renderDocs) and the reports agents' replies named (renderReports):
  // the sidecar's copies, by id, newest first, a page at a time.  Above
  // it, a filter (title or file name) and a chip per source: all, each
  // docs directory, the reports.
  let docs = null; // [{name, title, where, bytes, mtime}] once Emacs has said
  // {list: [{id, title, name, agent, project, at, bytes, gone, older: [...]}], more, error, asking}
  let reports = null;
  // The reports whose older copies are shown, by project and file name:
  // what one is, whichever copy is its newest.
  const reportsOpen = new Set();
  const reportKey = (rep) => String(rep.project || "") + "\0" + String(rep.name || rep.id);
  // What the list shows, for the page's life: the filter's words, and the
  // source -- "" all, "d:" and a directory as Emacs says it, "reports".
  const docsView = { q: "", src: "" };

  function showDocs() {
    view = "docs";
    src = null;
    chrome();
    renderDocs();
    emit("docs");
  }

  function sizeText(n) {
    n = +n || 0;
    return n < 1024 ? n + " B" : n < 1024 * 1024 ? Math.round(n / 1024) + " KB" : (n / 1024 / 1024).toFixed(1) + " MB";
  }

  // A docs directory as its chip says it: ghostherd's own, or the path.
  const dirLabel = (where) => String(where || "").replace(/(.)\/+$/, "$1");

  // The filter and the chips stay put as the list is drawn again: the
  // filter keeps its words, and the caret, while it is typed in.
  function renderDocs() {
    if (view !== "docs") return;
    if (!$("docs-filter")) {
      app.innerHTML = '<div class="page docs"><div class="dbar">' +
        '<input id="docs-filter" type="search" placeholder="Filter by title or file name' + SLASH + '" autocomplete="off" ' +
        'spellcheck="false" aria-label="Filter the documents by title or file name">' +
        '<div class="chips dchips" role="group" aria-label="Where from"></div></div>' +
        '<div class="sum" id="docs-sum"></div><div class="dlist" id="docs-list"></div></div>';
      $("docs-filter").value = docsView.q;
    }
    renderDocsList();
  }

  function renderDocsList() {
    const box = $("docs-list");
    if (!box) return;
    const r = reports;
    const ds = (docs || []).filter((d) => d && DOC_NAME.docs.test(String(d.name)));
    const rs = r ? r.list : [];
    const q = docsView.q.trim().toLowerCase();
    const hit = (x) => !q || String(x.title || "").toLowerCase().includes(q) || String(x.name || "").toLowerCase().includes(q);
    // ghostherd's own directory first, then the others by name.
    const dirs = [...new Set(ds.map((d) => String(d.where || "")))].sort((a, b) =>
      (b === "ghostherd") - (a === "ghostherd") || a.localeCompare(b));
    const srcs = dirs.map((w) => "d:" + w).concat(rs.length ? ["reports"] : []);
    if (docsView.src && !srcs.includes(docsView.src)) docsView.src = "";
    const all = !docsView.src;
    const items = ds.filter(hit).map((d) => ({ src: "d:" + String(d.where || ""), at: stamp(d.mtime) || 0, d }))
      .concat(rs.filter(hit).map((x) => ({ src: "reports", at: stamp(x.at) || 0, x })))
      .sort((a, b) => b.at - a.at);
    const count = (k) => items.filter((i) => i.src === k).length;
    const chip = (k, label, n) => '<button type="button" class="chip' + (docsView.src === k ? " on" : "") +
      '" data-act="docs:src" data-src="' + esc(k) + '" aria-pressed="' + (docsView.src === k) + '">' + esc(label) +
      ' <span class="dcount">' + esc(n) + "</span></button>";
    const chips = app.querySelector(".dchips");
    if (chips) chips.innerHTML = chip("", "All", items.length + (r && r.more ? "+" : "")) +
      dirs.map((w) => chip("d:" + w, dirLabel(w), count("d:" + w))).join("") +
      (rs.length ? chip("reports", "Reports", count("reports") + (r.more ? "+" : "")) : "");
    const asking = [docs ? "" : "Asking Emacs for the documents…", r ? "" : "Asking the herd for the reports…"].filter(Boolean);
    $("docs-sum").textContent = asking.join(" · ");
    const shown = all ? items : items.filter((i) => i.src === docsView.src);
    const rows = shown.map((i) => (i.d ? docRow(i.d, all) : reportRow(i.x, false))).join("");
    const more = r && r.more && rs.length && (all || docsView.src === "reports")
      ? '<div class="foot"><button type="button" class="ghost small" data-act="reports:more"' + (r.asking ? " disabled" : "") +
        ">" + (r.asking ? "Asking…" : "Older reports") + "</button></div>"
      : "";
    const empty = shown.length ? ""
      : q && (ds.length || rs.length) ? '<p class="empty">Nothing here matches “' + esc(docsView.q.trim()) + "”.</p>"
      : docs && !ds.length && (!r || !rs.length) ? '<p class="empty">No HTML document in the folders listed.</p>'
      : "";
    box.innerHTML = (r && r.error ? '<p class="empty derror">The herd could not be asked for the reports: ' + esc(r.error) + "</p>" : "") +
      empty + rows + more;
  }

  // A row: its title, and when, on one line; its file, its size and --
  // all sources shown -- its directory on the next.
  function docRow(d, all) {
    const title = String(d.title || d.name);
    return '<div class="doc" data-src="d:' + esc(d.where || "") + '"><div class="dl1">' +
      docLink("docs", d.name, esc(title), "dtitle", (title !== d.name ? title + " · " : "") + d.name) +
      '<span class="dwhen" title="' + esc(fullTime(d.mtime)) + '">' + esc(when(d.mtime)) + "</span></div>" +
      '<div class="dmeta"><span class="dname">' + esc(d.name) + "</span>" +
      '<span class="dsize">' + esc(sizeText(d.bytes)) + "</span>" +
      (all ? '<span class="dwhere" title="' + esc(d.where || "") + '">' + esc(dirLabel(d.where)) + "</span>" : "") +
      "</div></div>";
  }

  // A report, as the sidecar keeps its copy: it opens as the Open link
  // under its message does, and says who wrote it, in which project.  One
  // whose copy is gone from the directory is still listed, marked, with
  // nothing to open.  A file an agent rewrote and named again is one row,
  // its newest copy: "2 older" under it shows the others, newest first,
  // and folds them away again.
  function reportRow(rep, older) {
    const name = String(rep.name || rep.id + ".html");
    const title = String(rep.title || "").trim() || name;
    const who = String(rep.agent || "");
    const them = older ? [] : (Array.isArray(rep.older) ? rep.older : []).filter((o) =>
      o && IMG_ID.test(String(o.id)) && typeof o.at === "number");
    const open = them.length > 0 && reportsOpen.has(reportKey(rep));
    const head = rep.gone
      ? '<span class="dtitle dgone" title="' + esc(title + " · " + name) + '">' + esc(title) + "</span>"
      : docLink("reports", rep.id + ".html", esc(title), "dtitle", (title !== name ? title + " · " : "") + name);
    const from = (who ? "from " + who : "writer unknown") + (rep.project ? " · " + rep.project : "");
    return '<div class="doc report' + (rep.gone ? " gone" : "") + (older ? " older" : "") + '" data-report="' +
      esc(rep.id) + '"><div class="dl1">' + head +
      '<span class="dwhen" title="' + esc(fullTime(rep.at)) + '">' + esc(when(rep.at)) + "</span></div>" +
      '<div class="dmeta"><span class="dwho" title="' + esc(from) + '">' + esc(from) + "</span>" +
      '<span class="dname">' + esc(name) + "</span>" +
      '<span class="dsize">' + esc(sizeText(rep.bytes)) + "</span>" +
      (rep.gone ? '<span class="dgonesays" title="Its copy is gone from the herd\'s folder: nothing to open.">its copy is gone</span>' : "") +
      (them.length
        ? '<button type="button" class="linkish dolder" data-act="reports:older" data-report="' + esc(rep.id) +
          '" aria-expanded="' + open + '" title="The copies of this file as it was before it was rewritten">' +
          them.length + " older" + "</button>"
        : "") + "</div>" +
      (open ? '<div class="dolders">' + them.map((o) => reportRow(o, true)).join("") + "</div>" : "") + "</div>";
  }

  // The image whole, over the page: fitted to it, at its own size at a
  // press, scrolled.  Esc, ✕ or a press beside it puts it away.  In a
  // browser, Open gives it a tab of its own -- a phone zooms it there.
  // Its message's other images are a ‹ › away, ← → or a swipe: the
  // thumbnails beside it (its .eimgs), not the reports among them.
  function openImage(el) {
    const shot = (b) => {
      const file = b.getAttribute("data-img") || "";
      const url = IMG_ID.test(file.split(".")[0]) ? XW.file(file) : "";
      return url ? { url: url, name: b.getAttribute("data-name") || "" } : null;
    };
    if (!shot(el)) return;
    const group = el.closest(".eimgs");
    const them = group ? Array.from(group.querySelectorAll(".eimg")) : [el];
    const shots = them.map(shot).filter(Boolean);
    let at = Math.max(0, them.filter(shot).indexOf(el));
    const many = shots.length > 1;
    closeImage();
    const wrap = document.createElement("div");
    wrap.className = "imgview";
    wrap.setAttribute("role", "dialog");
    wrap.tabIndex = -1;
    wrap.innerHTML = '<div class="imgbar">' +
      (many ? '<button type="button" class="ghost imgprev" title="The one before (←)">‹</button>' +
        '<span class="imgat"></span>' +
        '<button type="button" class="ghost imgnext" title="The next (→)">›</button>' : "") +
      '<span class="imgname"></span>' +
      (XW.browser ? '<a class="imgopen" target="_blank" rel="noopener noreferrer">Open</a>' : "") +
      '<button type="button" class="ghost imgclose" title="Close (Esc)">✕</button></div>' +
      '<div class="imgscroll"><img alt=""></div>';
    const img = wrap.querySelector("img");
    const open = wrap.querySelector(".imgopen");
    const show = (i) => {
      if (i < 0 || i >= shots.length) return;
      at = i;
      const s = shots[i];
      wrap.classList.remove("actual");
      wrap.setAttribute("aria-label", s.name || "Image");
      wrap.querySelector(".imgname").textContent = s.name;
      img.src = s.url;
      img.alt = s.name;
      if (open) open.href = s.url;
      if (!many) return;
      wrap.querySelector(".imgat").textContent = (i + 1) + " / " + shots.length;
      wrap.querySelector(".imgprev").disabled = i === 0;
      wrap.querySelector(".imgnext").disabled = i === shots.length - 1;
      // ‹ or › with the focus, shut at an end, drops it to the page -- and
      // with it ← → and Esc, which then is the page's own (back).
      const a = document.activeElement;
      if (wrap.isConnected && (!wrap.contains(a) || a.disabled)) wrap.focus();
    };
    show(at);
    wrap.addEventListener("click", (ev) => {
      if (ev.target === img) wrap.classList.toggle("actual");
      else if (ev.target.closest(".imgprev")) show(at - 1);
      else if (ev.target.closest(".imgnext")) show(at + 1);
      else if (ev.target.closest(".imgclose") || ev.target === wrap || ev.target.classList.contains("imgscroll")) closeImage();
    });
    // Over the page: its keys (j, k, 1-4 ...) are not the page's meanwhile.
    wrap.addEventListener("keydown", (ev) => {
      ev.stopPropagation();
      if (ev.key === "Escape") { closeImage(); ev.preventDefault(); }
      else if (many && (ev.key === "ArrowLeft" || ev.key === "ArrowRight")) {
        show(at + (ev.key === "ArrowRight" ? 1 : -1));
        ev.preventDefault();
      }
    });
    // A swipe across it, one finger: the next, or the one before -- not
    // at its own size, where a swipe scrolls it.
    if (many) {
      const scroll = wrap.querySelector(".imgscroll");
      let from = null;
      scroll.addEventListener("touchstart", (ev) => {
        from = ev.touches.length === 1 && !wrap.classList.contains("actual")
          ? { x: ev.touches[0].clientX, y: ev.touches[0].clientY } : null;
      }, { passive: true });
      scroll.addEventListener("touchend", (ev) => {
        const t = from && ev.changedTouches[0];
        const dx = t ? t.clientX - from.x : 0, dy = t ? t.clientY - from.y : 0;
        from = null;
        if (Math.abs(dx) > 40 && Math.abs(dx) > 2 * Math.abs(dy)) show(at + (dx < 0 ? 1 : -1));
      }, { passive: true });
    }
    imgBack = document.activeElement;
    document.body.appendChild(wrap);
    wrap.focus();
  }

  // In the xwidget the images are files, and where they are comes with
  // Emacs's answer to the page's hello: drawn before it, drawn again.
  document.addEventListener("xw-files", () => {
    if (view === "inbox") renderInbox();
    else if (view === "threads") renderThreads();
    else if (herdNow.data) GM.renderHerdAgain();
  });

  let imgBack = null;
  function closeImage() {
    const v = document.querySelector(".imgview");
    if (!v) return;
    v.remove();
    if (imgBack && imgBack.focus && document.contains(imgBack)) imgBack.focus({ preventScroll: true });
    imgBack = null;
  }

  // ---- Query terms -----------------------------------------------------

  function termRe(q) {
    const terms = String(q || "")
      .split(/\s+/)
      .filter((t) => t.length >= 2 || /[^\x00-\x7f]/.test(t))
      .map((t) => t.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"));
    return terms.length ? new RegExp("(" + terms.join("|") + ")", "gi") : null;
  }

  // TEXT with RE's matches in <mark>, everything escaped.
  function hl(text, re) {
    const s = String(text || "");
    if (!re) return esc(s);
    let out = "";
    let last = 0;
    s.replace(re, (m, _g, at) => {
      out += esc(s.slice(last, at)) + "<mark>" + esc(m) + "</mark>";
      last = at + m.length;
      return m;
    });
    return out + esc(s.slice(last));
  }

  // Up to ~300 characters of TEXT around RE's first match.
  function snippet(text, re) {
    const s = String(text || "").replace(/\s+/g, " ").trim();
    let at = 0;
    if (re) {
      re.lastIndex = 0;
      const m = re.exec(s);
      re.lastIndex = 0;
      if (m) at = m.index;
    }
    const start = Math.max(0, at - 90);
    const end = Math.min(s.length, start + 300);
    return (start > 0 ? "…" : "") + hl(s.slice(start, end), re) + (end < s.length ? "…" : "");
  }

  // Wrap RE's matches in ROOT's text in <mark>, for a rendered message.
  function markTerms(root, re) {
    if (!re) return;
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const nodes = [];
    while (walker.nextNode()) nodes.push(walker.currentNode);
    nodes.forEach((node) => {
      re.lastIndex = 0;
      if (!re.test(node.data)) return;
      re.lastIndex = 0;
      const span = document.createElement("span");
      span.innerHTML = hl(node.data, re);
      node.replaceWith(...span.childNodes);
    });
  }

  // ---- Chunks back into messages ---------------------------------------
  //
  // The importer splits a long turn into ~1800-character chunks, each
  // starting with the last ~120 characters of the one before (so a
  // sentence across a cut is findable from either side), and records no
  // turn number.  A chunk that begins with the previous one's tail, in
  // the same role, is a continuation: join it and drop the repeat.

  function overlap(prev, next) {
    const max = Math.min(200, prev.length, next.length);
    for (let k = max; k >= 12; k--) {
      if (next.startsWith(prev.slice(prev.length - k))) return k;
    }
    return 0;
  }

  function messagesOf(chunks) {
    const out = [];
    chunks.forEach((c) => {
      const text = String(c.text || "");
      const prev = out[out.length - 1];
      const same =
        prev && prev.role === c.role && (!prev.ts || !c.ts || prev.ts === c.ts) &&
        prev.last === c.chunk_index - 1;
      const k = same ? overlap(prev.tail, text) : 0;
      if (k) {
        prev.text += text.slice(k);
        prev.last = c.chunk_index;
        prev.tail = text;
      } else {
        out.push({
          role: c.role || "",
          ts: c.ts || "",
          title: c.title || "",
          first: c.chunk_index,
          last: c.chunk_index,
          text: text,
          tail: text,
        });
      }
    });
    return out;
  }

  // ---- State -----------------------------------------------------------

  // The sessions the index holds -- SOURCES only once Emacs sent them
  // (LOADED), when a view that lists them asked (ASKED, not answered
  // yet); STALE when Emacs has said since that they changed.  Until
  // then, their count alone (TOTAL).  See `needSessions'.
  let ctx = { sources: [], byPath: {}, total: 0, currentProject: "", projects: [],
              loaded: false, asked: false, stale: false };
  let filters = { agent: "", project: "" };
  let view = "boot"; // agents | search | sessions | source | log | docs | ...
  let watching = false; // Emacs is sending the herd every few seconds
  let tab = "search"; // the list view a source returns to
  let query = "";
  let searchSeq = 0; // the search whose answer the page wants
  let searchN = Date.now();
  let limit = 30;
  let hits = null; // { q, items } once answered
  let searching = false;
  let sessionsFilter = "";
  let src = null; // the session on screen: { path, meta, total, offset, chunks, focus, re, marks, at }
  let listScroll = { search: 0, sessions: 0 };
  let lastOpened = null; // key of the card a source was opened from
  // The Memory tab's store, facts and view state (see showMemory).
  let mem = { key: "", projects: [], user: { key: "user", label: "User (shared)" },
              facts: [], filter: "", open: new Set(), editing: "", confirmDel: "",
              proposals: [], promotedOnly: false, rejecting: "", sort: memSortKept(),
              checks: [], checkOpen: "", confirmRm: "",
              unusedOnly: false, usesSince: null, usesDays: 30 };
  let memWatching = false; // Emacs is pushing this store's facts

  // How the Memory tab orders its facts -- "recent", "name" or "used" --
  // kept for this viewer in this browser.  Storage can be missing or
  // refused (a private window, the desk's file page): it then starts on
  // "recent".
  function memSortKept() {
    try {
      const v = localStorage.getItem("ghostherd.memory.sort");
      return v === "name" || v === "used" ? v : "recent";
    } catch (e) {
      return "recent";
    }
  }

  function memSortKeep(sort) {
    try {
      localStorage.setItem("ghostherd.memory.sort", sort);
    } catch (e) {
      // Not kept: this page keeps it until it is loaded again.
    }
  }

  function sourceMeta(path) {
    return ctx.byPath[path] || { source_path: path, agent: "", project: "", kind: "", chunks: 0 };
  }

  // The sessions themselves are some 400 KB for a herd of months, and a
  // reply leaves Emacs over tailscale serve's socket at some 340 KB/s: a
  // phone opening on the Inbox waited over a second for a list it does
  // not show.  So the page says it asks for them as it needs them (its
  // ready, `lazy'), is told their count, and asks for them (a refresh
  // with `sessions') when a view that lists them shows -- once, and again
  // only once Emacs has said they changed (an import, a page come back).
  function listsSessions() {
    return view === "sessions" || view === "search";
  }

  function askSessions() {
    ctx.asked = true;
    emit("refresh", { lazy: true, sessions: true });
  }

  function needSessions() {
    if (ctx.asked || (ctx.loaded && !ctx.stale)) return;
    askSessions();
  }

  // Said in place of the list while it is on its way.
  const SESSIONS_COMING = '<p class="empty">Listing the sessions…</p>';

  // ---- Chrome ----------------------------------------------------------

  function chrome() {
    if (watching && view !== "agents") {
      watching = false;
      emit("herd-unwatch");
    }
    if (memWatching && view !== "memory") {
      memWatching = false;
      emit("memory-unwatch");
    }
    // The Board's 要你處理 is drawn from the Inbox's push too.
    if (inboxWatching && view !== "inbox" && view !== "board") {
      inboxWatching = false;
      emit("inbox-unwatch");
    }
    if (threadsWatching && view !== "threads") {
      threadsWatching = false;
      threads.req = null;
      threads.cur = null;
      emit("threads-unwatch");
    }
    if (boardWatching && view !== "board") {
      boardWatching = false;
      clearInterval(boardTimer);
      const cs = Object.values(board.cards);
      board.peek = { orphan: cs.filter((c) => c.status === "orphan").length,
                     jing: cs.filter((c) => c.status === "jing" && (c.needs || []).length).length };
      emit("board-unwatch");
    }
    boardBadge();
    inboxBadge();
    threadsBadge();
    document.querySelectorAll(".tab").forEach((b) => {
      b.classList.toggle("on", view !== "source" && b.getAttribute("data-tab") === view);
    });
    tabInView();
    btnBack.hidden = !(view === "source" || (view === "agents" && herdNow.room));
    const sessionsN = ctx.loaded ? ctx.sources.length : ctx.total;
    $("sessions-n").textContent = sessionsN ? String(sessionsN) : "";
    app.setAttribute("data-view", view);
    // A room scrolls in its columns, not as a page.
    app.classList.toggle("in-room", view === "agents" && !!herdNow.room);
    remember();
  }

  // Where the page is, kept in its URL: loaded again -- reloaded, or
  // come back after Emacs restarted -- it goes back there.  A page
  // Emacs opens has no hash, and shows what Emacs asks for.
  function remember() {
    let where = view === "source" ? tab : view;
    if (where === "boot") return;
    if (where === "agents" && herdNow.room)
      where = "room/" + herdNow.roomView + "/" + encodeURIComponent(herdNow.room);
    try {
      history.replaceState(null, "", "#" + where);
    } catch (e) {
      // Not kept: loaded again, the page starts where Emacs says.
    }
  }

  function restore() {
    const where = location.hash.slice(1);
    const room = /^room\/(columns|chat)\/(.+)$/.exec(where);
    if (room) {
      let project = null;
      try {
        project = decodeURIComponent(room[2]);
      } catch (e) {
        return showAgents();
      }
      showAgents();
      herdNow.roomView = room[1];
      openRoom(project);
    } else if (where === "agents") showAgents();
    else if (where === "log") showLog();
    else if (where === "memory") showMemory();
    else if (where === "sessions") showSessions();
    else if (where === "search") showSearch();
    else if (where === "inbox") showInbox();
    else if (where === "threads") showThreads();
    else if (where === "board") showBoard();
    else if (where === "docs") showDocs();
    // Home: the page opens on the Inbox when nothing else is asked for.
    else showInbox();
  }

  // Emacs let the page go while the phone slept, and has it again: the
  // page is as it was -- scrolled where it was, what it had open still
  // open -- and asks again for what it shows, as its load did.  Emacs
  // closed what it was sending when it let go -- the sessions too, if it
  // was sending them, and they may have changed meanwhile.  What the page
  // shows is asked for first, the rest (ready) after.
  function comeBack() {
    ctx.asked = false;
    if (ctx.loaded) ctx.stale = true;
    if (listsSessions()) askSessions();
    if (watching) emit("herd-watch");
    if (herdNow.room) watchRoom(true);
    if (herdNow.room && herdNow.subView) {
      const sv = herdNow.subView, conv = herdNow.convs[sv.name];
      emit("sub-watch", { name: sv.name, path: (conv && conv.path) || "", sub: sv.sub });
    }
    if (memWatching) emit("memory-watch", mem.key ? { key: mem.key } : { key: "user" });
    // The Board's, only what needs you (showBoard).
    if (inboxWatching) emit("inbox-watch", view === "board" ? { needs: true } : {});
    if (threadsWatching) emit("threads-watch", { project: threads.project, sort: threads.sort });
    // Emacs sends the board whole again: what it sent while the page was
    // away is not here.
    if (boardWatching) emit("board-watch", { days: board.days });
    if (view === "log") emit("log");
    if (view === "docs") emit("docs");
    emit("ready", { lazy: true });
    emit("threads-peek");
    emit("board-peek", { days: board.days });
  }

  function filtersHtml() {
    const counts = {};
    ctx.sources.forEach((s) => (counts[s.agent] = (counts[s.agent] || 0) + 1));
    const chip = (value, label, n) =>
      '<button type="button" class="chip' + (filters.agent === value ? " on" : "") +
      '" data-act="agent" data-agent="' + esc(value) + '">' + esc(label) +
      (n ? ' <span class="count">' + n + "</span>" : "") + "</button>";
    const chips = chip("", "All") + AGENTS.map((a) => chip(a, a, counts[a] || 0)).join("");
    const opt = (value, label, title) =>
      '<option value="' + esc(value) + '"' + (filters.project === value ? " selected" : "") +
      (title ? ' title="' + esc(title) + '"' : "") + ">" + esc(label) + "</option>";
    let opts = opt("", "All projects");
    const cur = ctx.projects.find((p) => p.project === ctx.currentProject);
    if (cur) opts += opt(cur.project, "This project · " + cur.leaf, cur.project);
    ctx.projects.forEach((p) => {
      if (p !== cur) opts += opt(p.project, p.leaf + " (" + p.n + ")", p.project);
    });
    return (
      '<div class="filters"><div class="chips">' + chips + "</div>" +
      '<select id="project" title="Project">' + opts + "</select></div>"
    );
  }

  // ---- Search ----------------------------------------------------------

  function showSearch() {
    view = tab = "search";
    needSessions();
    chrome();
    app.innerHTML =
      '<div class="page">' +
      '<div class="searchbar"><input id="q" type="search" autocomplete="off" spellcheck="false" ' +
      'placeholder="Search past sessions — 中文 or English' + SLASH + '" value="' + esc(query) + '"></div>' +
      filtersHtml() +
      '<div id="results" class="results"></div></div>';
    renderResults();
    app.scrollTop = listScroll.search;
    refocus();
  }

  function hitCard(h, i, re) {
    const meta = sourceMeta(h.source_path);
    const t = h.ts || meta.mtime;
    const title = h.title || (h.session_id ? h.session_id.slice(0, 8) : baseName(h.source_path));
    return (
      '<div class="card hit" tabindex="0" role="button" data-act="open-hit" data-i="' + i + '" ' +
      'data-key="' + esc(h.source_path + "#" + h.chunk_index) + '">' +
      '<div class="card-head">' + agentBadge(h.agent) +
      '<span class="proj" title="' + esc(h.project) + '">' + esc(leaf(h.project)) + "</span>" +
      '<span class="role r-' + esc(h.role) + '">' + esc(h.role || "") + "</span>" +
      '<span class="title">' + esc(title) + "</span>" +
      '<span class="when" title="' + esc(fullTime(t)) + '">' + esc(when(t)) + "</span></div>" +
      '<div class="snip">' + snippet(h.text, re) + "</div></div>"
    );
  }

  function renderResults() {
    const box = $("results");
    if (!box) return;
    if (!query.trim()) {
      if (!ctx.loaded) {
        box.innerHTML = '<h2 class="section">Recent sessions</h2>' + SESSIONS_COMING;
        return;
      }
      const recent = ctx.sources.filter(matchesFilters).slice(0, 12);
      box.innerHTML =
        '<h2 class="section">Recent sessions</h2>' +
        (recent.length
          ? recent.map(sessionRow).join("")
          : '<p class="empty">Nothing imported yet. Import reads claude, grok and agy transcripts.</p>');
      return;
    }
    if (!hits || hits.q !== query.trim()) {
      box.innerHTML = '<p class="empty">' + (searching ? "Searching…" : "") + "</p>";
      return;
    }
    const re = termRe(hits.q);
    const items = hits.items;
    box.innerHTML = items.length
      ? '<div class="sum">' + items.length + (items.length === 1 ? " hit" : " hits") +
        (searching ? " · searching…" : "") + "</div>" +
        items.map((h, i) => hitCard(h, i, re)).join("") +
        (items.length >= limit && limit < 120
          ? '<div class="foot"><button type="button" class="ghost" data-act="more-hits">More</button></div>'
          : "")
      : '<p class="empty">No hits for “' + esc(hits.q) + "”" +
        (filters.agent || filters.project ? " with these filters" : "") + ".</p>";
  }

  let searchTimer = null;
  function search(now) {
    clearTimeout(searchTimer);
    const run = () => {
      searchSeq = ++searchN;
      searching = !!query.trim();
      emit("search", {
        seq: searchSeq,
        q: query.trim(),
        agent: filters.agent,
        project: filters.project,
        limit: limit,
      });
      renderResults();
    };
    if (now) run();
    else searchTimer = setTimeout(run, 250);
  }

  // ---- Sessions --------------------------------------------------------

  function matchesFilters(s) {
    return (!filters.agent || s.agent === filters.agent) &&
      (!filters.project || s.project === filters.project);
  }

  function sessionRow(s) {
    const id = s.session_id ? s.session_id.slice(0, 8) : "";
    return (
      '<div class="card srow" tabindex="0" role="button" data-act="open-source" ' +
      'data-path="' + esc(s.source_path) + '" data-key="' + esc(s.source_path) + '">' +
      '<div class="card-head">' + agentBadge(s.agent) +
      '<span class="proj" title="' + esc(s.project) + '">' + esc(leaf(s.project)) + "</span>" +
      (s.kind && s.kind !== "transcript" ? '<span class="kind">' + esc(s.kind) + "</span>" : "") +
      '<span class="title">' + esc(id || baseName(s.source_path)) + "</span>" +
      '<span class="n">' + (s.chunks || 0) + " chunks</span>" +
      '<span class="when" title="' + esc(fullTime(s.mtime)) + '">' + esc(when(s.mtime)) + "</span></div>" +
      '<div class="path" title="' + esc(s.source_path) + '">' + esc(s.source_path) + "</div></div>"
    );
  }

  function showSessions() {
    view = tab = "sessions";
    needSessions();
    chrome();
    app.innerHTML =
      '<div class="page">' +
      '<div class="searchbar"><input id="sess-filter" type="search" autocomplete="off" spellcheck="false" ' +
      'placeholder="Filter by project, path or session id' + SLASH + '" value="' + esc(sessionsFilter) + '"></div>' +
      filtersHtml() + '<div id="sess-list" class="results"></div></div>';
    renderSessionList();
    app.scrollTop = listScroll.sessions;
    refocus();
  }

  function renderSessionList() {
    const box = $("sess-list");
    if (!box) return;
    if (!ctx.loaded) {
      box.innerHTML = SESSIONS_COMING;
      return;
    }
    const f = sessionsFilter.trim().toLowerCase();
    const rows = ctx.sources.filter((s) =>
      matchesFilters(s) &&
      (!f || [s.project, s.source_path, s.session_id, s.agent].some((x) => String(x || "").toLowerCase().includes(f))));
    box.innerHTML =
      '<div class="sum">' + rows.length + " of " + ctx.sources.length + " sessions</div>" +
      (rows.length ? rows.map(sessionRow).join("") : '<p class="empty">No session matches.</p>');
  }

  // ---- A session -------------------------------------------------------

  const WINDOW = 160;

  function openSource(path, focus, re) {
    const from = document.activeElement && document.activeElement.closest && document.activeElement.closest(".card");
    lastOpened = from ? from.getAttribute("data-key") : null;
    listScroll[view === "sessions" ? "sessions" : "search"] = app.scrollTop;
    const offset = focus == null ? 0 : Math.max(0, focus - 40);
    src = {
      path: path,
      meta: sourceMeta(path),
      total: 0,
      offset: offset,
      chunks: [],
      focus: focus == null ? null : focus,
      re: re || null,
      marks: [],
      at: -1,
      here: null,
      loading: "open",
    };
    view = "source";
    chrome();
    app.innerHTML = '<p class="muted pad">Opening…</p>';
    emit("open-source", { source_path: path, offset: offset, limit: WINDOW, focus: src.focus, mode: "open" });
  }

  function loadMore(dir) {
    if (!src || src.loading) return;
    if (dir === "earlier") {
      const offset = Math.max(0, src.offset - WINDOW);
      src.loading = "earlier";
      emit("open-source", { source_path: src.path, offset: offset, limit: src.offset - offset, mode: "earlier" });
    } else {
      src.loading = "later";
      emit("open-source", {
        source_path: src.path,
        offset: src.offset + src.chunks.length,
        limit: WINDOW,
        mode: "later",
      });
    }
    renderSourceChrome();
  }

  function sourceTitle() {
    const m = src.meta;
    const first = src.chunks[0] || {};
    const title = first.title || (m.session_id ? m.session_id.slice(0, 8) : baseName(src.path));
    return agentBadge(m.agent || first.agent) +
      '<span class="proj" title="' + esc(m.project || first.project) + '">' + esc(leaf(m.project || first.project)) + "</span>" +
      '<span class="title">' + esc(title) + "</span>";
  }

  function whoOf(role, agent) {
    if (role === "user") return "User";
    if (role === "assistant") return AGENT_NAME[agent] || agent || "Assistant";
    return role ? role.charAt(0).toUpperCase() + role.slice(1) : "Note";
  }

  function messageHtml(m, agent, plain) {
    // agy's clean transcript.jsonl prose renders as markdown like the rest;
    // only the old =conversations/*.db= protobuf salvage -- tag-like noise
    // the sanitizer would eat -- stays plain (PLAIN set by its source).
    const body = plain ? '<div class="plain">' + esc(m.text) + "</div>" : markdown(m.text);
    const range = m.first === m.last ? "#" + m.first : "#" + m.first + "–" + m.last;
    return (
      '<section class="msg r-' + esc(m.role || "note") + '" data-first="' + m.first + '" data-last="' + m.last + '">' +
      '<div class="msg-head"><span class="who">' + esc(whoOf(m.role, agent)) + "</span>" +
      (m.ts ? '<span class="when" title="' + esc(fullTime(m.ts)) + '">' + esc(when(m.ts)) + "</span>" : "") +
      '<span class="range">' + range + "</span>" +
      '<button type="button" class="linkish" data-act="copy-msg" title="Copy this message">Copy</button></div>' +
      '<div class="msg-body">' + body + "</div></section>"
    );
  }

  // Resume / Fork, or why not.  Emacs decided; the page only shows it.
  function resumeHtml() {
    const r = src && src.resume;
    if (!r) return "";
    if (!r.ok) return '<span class="noresume" title="' + esc(r.why || "") + '">⊘ ' + esc(r.why || "Not resumable") + "</span>";
    let html = "";
    if (r.running) {
      html += XW.elsewhere ? '<span class="noresume">On ' + esc(r.running) + " already</span>"
        : '<button type="button" class="ghost small" data-act="visit-agent" title="An agent is on this conversation already">Open ' +
          esc(r.running) + "</button>";
    } else {
      html += '<button type="button" class="primary small" data-act="resume" title="Continue this conversation in a new ' +
        esc(r.agent) + ' agent">Resume</button>';
    }
    if (r.fork) {
      html += '<button type="button" class="ghost small" data-act="fork" title="Branch a new conversation off this one">Fork</button>';
    }
    return '<span class="resume">' + html + "</span>";
  }

  // In-page, as pr-view learned: window.confirm() returns false at once
  // inside xwidget's WebKit, without drawing anything.
  function ask(message, detail, choices) {
    const wrap = document.createElement("div");
    wrap.className = "ask-wrap";
    wrap.innerHTML = '<div class="ask" role="dialog"><p class="ask-msg"></p><p class="ask-detail"></p><div class="ask-buttons"></div></div>';
    wrap.querySelector(".ask-msg").textContent = message;
    wrap.querySelector(".ask-detail").textContent = detail || "";
    const row = wrap.querySelector(".ask-buttons");
    const close = () => wrap.remove();
    choices.forEach(([label, cls, fn]) => {
      const b = document.createElement("button");
      b.type = "button";
      b.className = cls;
      b.textContent = label;
      b.addEventListener("click", () => {
        close();
        if (fn) fn();
      });
      row.appendChild(b);
    });
    wrap.addEventListener("click", (ev) => { if (ev.target === wrap) close(); });
    wrap.addEventListener("keydown", (ev) => { if (ev.key === "Escape") { close(); ev.stopPropagation(); } });
    document.body.appendChild(wrap);
    const last = row.lastElementChild;
    if (last) last.focus();
  }

  function resume(fork) {
    const r = src && src.resume;
    if (!r || !r.ok) return;
    const go = (f) => emit("resume", { source_path: src.path, fork: !!f });
    if (fork || r.recent == null) return go(fork);
    const choices = [["Cancel", "ghost", null]];
    if (r.fork) choices.push(["Fork instead", "ghost", () => go(true)]);
    choices.push(["Resume anyway", "primary", () => go(false)]);
    ask("This conversation was written to " + (r.recent < 60 ? r.recent + "s" : span(r.recent * 1000)) + " ago.",
      "It may still be open in an agent; resuming it there too would give it two writers." +
        (r.fork ? " Fork continues from here in a new conversation instead." : ""),
      choices);
  }

  function renderSourceChrome() {
    const head = $("src-head");
    if (!head || !src) return;
    const end = src.offset + src.chunks.length;
    const n = src.marks.length;
    head.innerHTML =
      '<div class="card-head">' + sourceTitle() +
      '<span class="n">' + (src.total ? src.offset + 1 + "–" + end + " of " + src.total + " chunks" : "") + "</span>" +
      (src.re
        ? '<span class="matches">' + (n ? src.at + 1 + " / " + n : "no matches here") +
          '<button type="button" class="linkish" data-act="match-prev" title="Previous match (N)"' + (n ? "" : " disabled") + ">↑</button>" +
          '<button type="button" class="linkish" data-act="match-next" title="Next match (n)"' + (n ? "" : " disabled") + ">↓</button></span>"
        : "") +
      resumeHtml() +
      '<button type="button" class="linkish" data-act="copy-path" title="' + esc(src.path) + '">Copy path</button></div>';
    const top = $("src-earlier");
    const bottom = $("src-later");
    if (top) {
      top.hidden = src.offset <= 0;
      top.innerHTML = '<button type="button" class="ghost" data-act="earlier"' + (src.loading ? " disabled" : "") + ">" +
        (src.loading === "earlier" ? "Loading…" : "Earlier · " + src.offset + " more") + "</button>";
    }
    if (bottom) {
      bottom.hidden = end >= src.total;
      bottom.innerHTML = '<button type="button" class="ghost" data-act="later"' + (src.loading ? " disabled" : "") + ">" +
        (src.loading === "later" ? "Loading…" : "Later · " + (src.total - end) + " more") + "</button>";
    }
  }

  function renderSourceBody() {
    const agent = src.meta.agent || (src.chunks[0] || {}).agent || "";
    // An agy session imported from its =.db= salvage (no transcript.jsonl)
    // stays plain; one from transcript.jsonl is clean prose, so markdown.
    const plain = agent === "agy" && /\.db$/i.test(src.path || "");
    const msgs = messagesOf(src.chunks);
    $("src-body").innerHTML = msgs.length
      ? msgs.map((m) => messageHtml(m, agent, plain)).join("")
      : '<p class="empty">This session has no chunks.</p>';
    src.messages = msgs;
    if (src.re) {
      document.querySelectorAll("#src-body .msg-body").forEach((el) => markTerms(el, src.re));
    }
    src.marks = Array.from(document.querySelectorAll("#src-body mark"));
    // A re-render (Earlier, Later) rebuilds every message: put the hit's
    // highlight and the current match back where they were.
    const els = Array.from(document.querySelectorAll("#src-body .msg"));
    const focusEl = src.focus == null ? null : els.find((m) => holds(m, src.focus));
    if (focusEl) focusEl.classList.add("focus");
    src.at = -1;
    if (src.here) {
      const home = els.find((m) => m.getAttribute("data-first") === src.here.first);
      const mk = home && home.querySelectorAll("mark")[src.here.k];
      if (mk) setHere(src.marks.indexOf(mk));
    }
  }

  function holds(msgEl, chunk) {
    return +msgEl.getAttribute("data-first") <= chunk && chunk <= +msgEl.getAttribute("data-last");
  }

  // Make match I the current one, remembered by its message and its
  // place there, since indexes shift when Earlier prepends.
  function setHere(i) {
    if (src.at >= 0 && src.marks[src.at]) src.marks[src.at].classList.remove("here");
    src.at = i;
    const mk = src.marks[i];
    if (!mk) return;
    mk.classList.add("here");
    const home = mk.closest(".msg");
    src.here = { first: home.getAttribute("data-first"), k: Array.from(home.querySelectorAll("mark")).indexOf(mk) };
  }

  function showSource() {
    view = "source";
    chrome();
    app.innerHTML =
      '<div class="page source"><div id="src-head" class="src-head"></div>' +
      '<div id="src-earlier" class="foot"></div><div id="src-body"></div>' +
      '<div id="src-later" class="foot"></div></div>';
    renderSourceBody();
    renderSourceChrome();
  }

  function focusMessage() {
    const el = document.querySelector("#src-body .msg.focus");
    if (!el) {
      app.scrollTop = 0;
      return;
    }
    el.scrollIntoView({ block: "start" });
    app.scrollTop = Math.max(0, app.scrollTop - 60);
    // The first match inside the hit, so n / N continue from there.
    const first = src.marks.findIndex((mk) => el.contains(mk));
    if (first >= 0) setHere(first);
  }

  function jumpMatch(step) {
    if (!src || !src.marks.length) return;
    setHere((src.at + step + src.marks.length) % src.marks.length);
    src.marks[src.at].scrollIntoView({ block: "center" });
    renderSourceChrome();
  }

  function back() {
    if (view === "agents" && herdNow.subView) {
      herdNow.subView = null;
      herdNow.subConv = null;
      emit("sub-unwatch", {});
      return renderSubView();
    }
    if (view === "agents" && herdNow.room) return closeRoom();
    if (view !== "source") return;
    src = null;
    if (tab === "sessions") showSessions();
    else showSearch();
    if (lastOpened) {
      const card = Array.from(app.querySelectorAll(".card")).find((c) => c.getAttribute("data-key") === lastOpened);
      if (card) card.focus({ preventScroll: true });
    }
  }

  // ---- The agents --------------------------------------------------------
  //
  // Usage per CLI on top, then the herd grouped by project, with what the
  // overlay can do.  Emacs sends the herd every few seconds while this
  // tab is on screen, and at once when a state changes.

  const RANK = { blocked: 0, working: 1, starting: 2, waiting: 3, done: 4, idle: 5, dead: 6 };
  let herdNow = {
    data: null, screens: {}, open: new Set(), more: new Set(), hooksOpen: new Set(), tallyOpen: new Set(),
    // The handoff caps as the user set them and Emacs has not yet said
    // ({field: {v, at}}), and why Emacs last refused one.
    slots: { pending: {}, err: "" },
    compose: {}, composing: new Set(), notes: null, capture: null, newFor: null, pending: false,
    // What the open forms say, kept here as the prompts are: the page is
    // drawn again every 5s and on news, and a form drawn from nothing
    // would come back empty, its first CLI picked.  Set afresh as each
    // form opens.
    notesText: "", newKind: "", newName: "",
    // The New agent form: under a project's head (newFor its project) or
    // above the herd (newFor "*", the project typed in newProject); the
    // past conversation picked (its transcript's path, "" for New), and
    // fork; whether the kind was picked by hand, not the project's own.
    newProject: "", newConv: "", newFork: false, newKindPicked: false,
    // What the form offers, as Emacs sends it when asked -- never with the
    // herd: the places a new agent can go, and each asked project's past
    // conversations ({root, kind, list, why}).
    places: null, convs: {},
    // The projects asked for and not yet answered, and the one whose room
    // Open room waits to open until Emacs says which project it is in.
    convsAsked: new Set(), newRoomWant: null,
    // The model form: whose it is, and the model and effort picked.
    modelFor: null, modelPick: {},
    // Images attached to each agent's next prompt: [{path, thumb}].
    attach: {},
    // The room: its project, each agent's conversation, the agents shown
    // by their screen instead, and the tool lines opened.
    room: null, convs: {}, roomScreen: new Set(), toolsOpen: new Set(),
    // Room cards the phone has expanded past their two-line header.
    cardOpen: new Set(),
    // The sub-agents' reports opened whole.
    subOpen: new Set(),
    // A sub-agent's own transcript, opened over the room: which, and what
    // Emacs sent of it.
    subView: null, subConv: null,
    // The room as one chat: the view shown, the timeline Emacs sent, who
    // the box writes to in each project, and the long messages opened.
    roomView: NARROW ? "chat" : "columns", chat: null, chatTo: {}, chatOpen: new Set(),
    // What each agent's CLI holds queued, as the page has seen it (trackQueued).
    queued: {},
    // A column (by name) or the chat whose `load earlier' is in flight: when
    // its older page arrives, the view holds the message you were reading
    // instead of jumping, the prepended turns pushing it down by their height.
    earlierPending: new Set(), chatEarlierPending: false,
    // Jump to what you asked: which of your own messages is the one the
    // sticky line points at (by its ts, or null = the latest), per view; a
    // pending flag while a `load earlier' fired to reach an older one; and a
    // transient note ("older than what is loaded") on a view's jump line.
    jump: { chat: null, col: {} }, jumpPend: null, jumpNote: {},
    // The find bar each strip turns into (by key: "col:NAME", or "chat"):
    // its words, the agent searched, the sidecar's answer, the hit gone
    // to, its list open -- and, for a strip whose body shows a page of the
    // whole conversation at a hit instead of the live tail, that page.
    // The column last pointed at, for / and Emacs's key to open in.
    find: {}, away: {}, findSeq: 0, lastCol: null,
    // Checkouts whose every task is shown, not two.
    workOpen: new Set(),
    // Narrow columns (by name) whose chips line is opened past its one line.
    chipsOpen: new Set(),
  };

  const IMAGE_BUTTON = '<button type="button" class="ghost small" data-act="a:image" ' +
    (XW.elsewhere ? 'title="A photo, or one from the camera, in the prompt as a file">'
      : 'title="Put the clipboard\'s image in the prompt, as a file (⌘V does too)">') + "Image</button>";
  // Where a prompt is written to an agent (its data-name), and the box:
  // its card, a room's column or chat foot, or the Inbox's reply to it --
  // each takes images the same way.
  const PROMPT_ROW = ".arow, .rcol, .cfoot, .ibreply";
  const PROMPT_BOX = "textarea.agent-text, textarea.ibreplybox";

  function cssName(n) {
    return String(n).replace(/["\\]/g, "\\$&");
  }

  // A press holds a refresh as an edit does: on WebKit a button takes no
  // focus, so pressing Save takes it out of the box being edited before
  // the click -- and a refresh drawn in between takes the button away.
  let pressing = false;

  function busyEditing() {
    if (pressing) return true;
    const a = document.activeElement;
    return !!(a && a.closest && a.closest(".agent-compose, .newform, .notes-edit"));
  }

  function showAgents() {
    if (herdNow.room) {
      herdNow.room = null;
      emit("room-close");
    }
    view = tab = "agents";
    src = null;
    chrome();
    if (!watching) {
      watching = true;
      emit("herd-watch");
    }
    app.innerHTML = '<div class="page wide"><div id="usage" class="usage"></div><div id="slots"></div><div id="primarybar"></div><div id="herd"></div></div>';
    renderAgents();
  }

  // ---- Primary, the herd-wide lead ----------------------------------------
  //
  // The user's one entry point for what crosses projects: its room is a
  // chat, reached from the Agents tab's header, and writing there starts
  // it -- or resumes its last conversation -- when it is not up.

  function primaryInfo() {
    return (herdNow.data && herdNow.data.primary) || null;
  }

  function isPrimaryRoom() {
    const p = primaryInfo();
    return !!(p && herdNow.room && herdNow.room === p.directory);
  }

  // The room is shown as a chat when its view is "chat" OR it is the primary
  // room (which is always a chat and carries no view toggle, so its roomView
  // stays at the device default -- "columns" on the desktop).  Chat renders
  // must use this, not roomView alone, or a desktop/after-reload primary room
  // takes the chat data but never paints it ("Reading the conversations…").
  function roomIsChat() {
    return isPrimaryRoom() || herdNow.roomView === "chat" || roomEmpty();
  }

  // A project's room with no agent in it -- one opened from New agent, or
  // whose agents all went -- is a chat too: its box writes to the lead,
  // which starts when you write (lead-say), as primary's does.
  function roomEmpty() {
    return !!herdNow.data && !isPrimaryRoom() && !roomAgents().length;
  }

  function primaryBarHtml(p) {
    if (!p) return "";
    const a = agentByName(p.name);
    const st = p.state || "";
    return '<div class="primarybar"><button type="button" class="primary-open" data-act="a:primary-room" ' +
      'title="Primary, the herd-wide lead: tell it anything that crosses projects, or ask what everyone is doing">' +
      '<span class="pbname">Primary</span>' +
      (st ? '<span class="st st-' + esc(st) + '">' + esc(st) + "</span>"
        : '<span class="pbidle">not running · writing to it starts it</span>') +
      '<span class="pbsub">' + esc(a && a.reason ? a.reason : "the herd-wide lead · routes work across projects") + "</span>" +
      "</button></div>";
  }

  // Primary's card, pinned at the top of the Agents tab in place of the bar:
  // once it is running, the same card every other agent has -- every button
  // works -- under a header that opens its room/chat, which is what the bar
  // offered.  Its project group is pulled from the list below (renderAgents),
  // so primary shows exactly once.  When it is not running there is no card,
  // so the pinned spot stays the bar: writing to it there is what starts it.
  function primaryPinnedHtml(d) {
    const p = d && d.primary;
    if (!p) return "";
    const mine = (d.agents || []).filter((a) => a.project === p.directory);
    if (!mine.length) return primaryBarHtml(p);
    const kinds = d.kinds || [];
    const rank = (s) => (s in RANK ? RANK[s] : 9);
    mine.sort((a, b) => rank(a.state) - rank(b.state) || (b.since || 0) - (a.since || 0));
    const counts = {};
    mine.forEach((a) => (counts[a.state] = (counts[a.state] || 0) + 1));
    const summary = Object.keys(counts).sort((a, b) => rank(a) - rank(b))
      .map((s) => '<span class="st st-' + esc(s) + '">' + counts[s] + " " + esc(s) + "</span>").join("");
    return '<section class="pgroup primary-pin" data-project="' + esc(p.directory) + '"><div class="phead">' +
      '<button type="button" class="pname" data-act="a:primary-room" ' +
      'title="Open the Primary room — its conversation as a chat">Primary</button>' +
      '<span class="ppath">' + esc(p.directory) + "</span>" + summary +
      '<button type="button" class="linkish" data-act="a:primary-room" title="Open the Primary room — its conversation as a chat">Open room</button>' +
      "</div>" + mine.map((a) => agentRow(a, kinds, asksFor(a.name, d.asks))).join("") + "</section>";
  }

  function untilText(ms) {
    if (!ms) return "";
    const d = ms - Date.now();
    return d <= 0 ? "reset due" : "resets in " + span(d);
  }

  // A hook's file and line, which open in Emacs -- at the desk.
  function hookFile(h) {
    const where = esc(h.file + ":" + h.line + (h.scope && h.scope !== "global" ? "  (" + h.scope + ")" : ""));
    return XW.elsewhere ? '<span class="hfile muted" title="' + where + '">' + esc(h.name + ":" + h.line) + "</span>"
      : '<button type="button" class="linkish hfile" data-act="a:hook-open" data-i="' + h.i + '" title="' + where + '">' +
        esc(h.name + ":" + h.line) + "</button>";
  }

  // A CLI's hooks, under its usage: whether ghostherd's are in, and on
  // a click every hook it runs, each opening its file at its line.
  function hooksHtml(kind, hooks) {
    if (!hooks || !hooks.status) return "";
    const raw = hooks.status[kind];
    const st = raw && { have: raw.have || [], want: raw.want || [] };
    const mine = (hooks.list || []).filter((h) => (h.readers || []).includes(kind));
    if (!st && !mine.length) return "";
    const full = st && st.want.length > 0 && st.have.length === st.want.length;
    const stat = st
      ? '<span class="hstat ' + (full ? "ok" : "miss") + '" title="' +
        esc(full ? "ghostherd's hooks: " + st.have.join(", ")
                 : "Missing: " + st.want.filter((e) => !st.have.includes(e)).join(", ") + ".  M-x ghostherd-install-hooks") + '">' +
        (full ? "ghostherd ✓" : st.have.length ? "ghostherd " + st.have.length + "/" + st.want.length : "ghostherd not installed") + "</span>"
      : "";
    const open = herdNow.hooksOpen.has(kind);
    const rows = open ? '<div class="hlist">' + (mine.length ? mine.map((h) =>
      '<div class="hrow' + (h.ours ? " ours" : "") + (h.enabled === false ? " off" : "") + '">' +
      '<div class="hhead"><span class="hev">' + esc(h.event || "?") + "</span>" +
      (h.matcher ? '<span class="hmatch">' + esc(h.matcher) + "</span>" : "") +
      (h.ours ? '<span class="hmine">ghostherd</span>' : "") +
      (h.enabled === false ? '<span class="hmatch">disabled</span>' : "") +
      hookFile(h) + "</div>" +
      (h.error ? '<div class="hcmd herr">' + esc(h.error) + "</div>"
               : '<div class="hcmd" title="' + esc(h.command) + '">' + esc(h.command) + "</div>") +
      "</div>").join("") : '<p class="muted">No hooks.</p>') + "</div>" : "";
    return '<div class="uhooks"><button type="button" class="linkish" data-act="a:hooks" data-kind="' + esc(kind) + '">' +
      (open ? "hide hooks" : "hooks · " + mine.length) + "</button>" + stat + "</div>" + rows;
  }

  function usageHtml(list, hooks) {
    if (!list || !list.length) {
      return '<p class="empty">No usage yet: the first reading arrives a few seconds after the page opens.</p>';
    }
    // Whole numbers bare, the rest to one place: a float is not a label.
    const pct = (n) => {
      const v = Math.round((Number(n) || 0) * 10) / 10;
      return Number.isInteger(v) ? String(v) : v.toFixed(1);
    };
    return list.map((k) => {
      const wins = (k.windows || []).map((w) => {
        const lvl = w.used >= 90 ? "hot" : w.used >= 70 ? "warm" : "ok";
        return '<div class="uwin" title="' + esc(w.label + ": " + pct(w.used) + "% used, " + pct(100 - w.used) + "% left" +
          (w.resets ? ", resets " + new Date(w.resets).toLocaleString() : "")) + '">' +
          '<span class="ulabel">' + esc(w.label) + "</span>" +
          '<span class="ubar"><span class="lvl-' + lvl + '" style="width:' + Math.max(0, Math.min(100, w.used)) + '%"></span></span>' +
          '<span class="upct">' + pct(w.used) + "%</span>" +
          '<span class="ureset">' + esc(untilText(w.resets)) + "</span></div>";
      }).join("");
      const err = k.error
        ? '<div class="uerr" title="' + esc(k.error) + '">' + (wins ? "stale · " : "") + esc(k.error) + "</div>"
        : "";
      return '<div class="ucard"><div class="uhead">' + agentBadge(k.kind) +
        '<span class="when">' + (k.fetched ? esc(when(k.fetched / 1000)) : "") + "</span></div>" +
        (wins || (err ? "" : '<p class="muted">No windows reported.</p>')) + err + hooksHtml(k.kind, hooks) + "</div>";
    }).join("");
  }

  // ---- The leads the herd starts for handoffs ------------------------------
  //
  // Its two caps, the user's: how many such leads may be at work at once
  // -- owe a handoff its answer; past it a handoff waits its turn -- and
  // how many may be up at all, idle ones too; past it the herd stops the
  // one idle longest that owes nothing.  Each beside what is in use now,
  // set with - / + or typed (Emacs takes only a whole number in range and
  // says why not); then the handoffs waiting, from whom to where, since
  // when.  The counts are the sidecar's, which keeps the queue.

  function slotCap(s, field) {
    const p = herdNow.slots.pending[field];
    return p ? p.v : field === "working" ? s.workingCap : s.aliveCap;
  }

  function slotsHtml(s) {
    if (!s) return "";
    const ctl = (field, label, used, title) => {
      const r = (field === "working" ? s.workingRange : s.aliveRange) || [1, 99];
      const v = slotCap(s, field);
      const n = Number(v);
      const pend = !!herdNow.slots.pending[field];
      return '<div class="slcap' + (s.known && used >= n ? " full" : "") + (pend ? " pending" : "") +
        '" data-field="' + field + '" title="' + esc(title) + '">' +
        '<span class="sllabel">' + esc(label) + "</span>" +
        '<span class="slused">' + (s.known ? used : "–") + '</span><span class="slof">of</span>' +
        '<span class="slstep">' +
        '<button type="button" class="slbtn" data-act="a:slot-step" data-field="' + field + '" data-d="-1" aria-label="' +
          esc(label + ": one fewer") + '"' + (n <= r[0] ? " disabled" : "") + ">−</button>" +
        '<input class="slinput" type="number" inputmode="numeric" pattern="[0-9]*" step="1" min="' + r[0] + '" max="' + r[1] +
          '" data-field="' + field + '" value="' + esc(String(v)) + '" aria-label="' + esc(label + ", at most") + '">' +
        '<button type="button" class="slbtn" data-act="a:slot-step" data-field="' + field + '" data-d="1" aria-label="' +
          esc(label + ": one more") + '"' + (n >= r[1] ? " disabled" : "") + ">+</button>" +
        "</span></div>";
    };
    const q = s.queue || [];
    const waitWords = (x) => {
      if (x.starting) return "starting its lead" + (x.stop ? " · stopping " + x.stop + " to make room" : "");
      const w = x.since ? Date.now() - x.since : 0;
      return "waiting " + (w < 60000 ? "<1m" : span(w)) + " · " + (x.ahead ? x.ahead + " ahead" : "next");
    };
    const list = q.length
      ? '<ul class="slqueue">' + q.map((x) =>
          '<li class="slq' + (x.starting ? " starting" : "") + '" title="' + esc((x.head || "") +
            (x.since ? "\nwaiting since " + new Date(x.since).toLocaleString() : "")) + '">' +
          '<span class="slwho">' + esc(x.from || "user") + " → " + esc(leaf(x.project)) + "</span>" +
          (x.asks > 1 ? '<span class="slmany">' + x.asks + " handoffs</span>" : "") +
          '<span class="slwhen">' + esc(waitWords(x)) + "</span></li>").join("") + "</ul>"
      : "";
    return '<section class="slots" aria-label="Leads the herd starts for handoffs">' +
      '<div class="slhead"><span class="sltitle">Leads for handoffs</span>' +
      (q.length ? '<span class="slqn">' + q.length + " waiting</span>" : "") + "</div>" +
      '<div class="slcaps">' +
      ctl("working", "At work", s.working, "Leads the herd started for handoffs that owe one its answer.  " +
        "Past the cap a handoff to a project with no lead waits its turn, and starts that lead when one frees.") +
      ctl("alive", "Up", s.alive, "Leads the herd started for handoffs that are up, idle ones too.  " +
        "Past the cap the herd stops the one idle longest that owes nothing, keeping its conversation.") +
      "</div>" +
      (herdNow.slots.err ? '<p class="slerr" role="alert">' + esc(herdNow.slots.err) + "</p>" : "") +
      list + "</section>";
  }

  // Drawn with the herd -- but not over a cap being typed, unless FORCE.
  function renderSlots(force) {
    const box = $("slots");
    if (!box) return;
    const a = document.activeElement;
    if (!force && a && a.classList && a.classList.contains("slinput")) return;
    const s = herdNow.data && herdNow.data.handoffSlots;
    // A cap Emacs now says is what was asked, or asked long enough ago
    // that no word is coming, is no longer waited on.
    Object.keys(herdNow.slots.pending).forEach((f) => {
      const p = herdNow.slots.pending[f];
      if ((s && Number(p.v) === (f === "working" ? s.workingCap : s.aliveCap)) || Date.now() - p.at > 8000) {
        delete herdNow.slots.pending[f];
      }
    });
    box.innerHTML = slotsHtml(s);
  }

  function setSlotCap(field, value) {
    herdNow.slots.pending[field] = { v: value, at: Date.now() };
    herdNow.slots.err = "";
    emit("handoff-caps", { field, value });
    renderSlots(true);
  }

  // The asks an agent is in: open ones either way, and for an asker the
  // answers of the last ten minutes.
  function asksFor(name, asks) {
    const recent = Date.now() - 10 * 60 * 1000;
    return (asks || []).filter((k) => {
      const open = k.status === "queued" || k.status === "delivered";
      if (k.to === name) return open;
      return k.from === name && (open || (k.answered || 0) > recent);
    });
  }

  // Who takes ask K, in words: its taker -- for a handoff, with the
  // project it went to, or that project's lead while the herd starts it,
  // or while it waits for a slot to.
  function takerOf(k) {
    if (!k.project) return k.to;
    return k.to ? k.to + " (" + leaf(k.project) + ")"
      : leaf(k.project) + "'s lead, " + (k.waiting ? "queued for a slot" : "starting");
  }

  // A handoff: work one project's lead -- or an agent with none over it --
  // handed to another project's lead.  Its cards name the projects: the
  // sender waits on a project, and the taker's project is waited on by one.
  function handoffWhat(k, name) {
    const open = k.status === "queued" || k.status === "delivered";
    const there = leaf(k.project);
    if (open && k.to === name) {
      const who = k.fromProject ? leaf(k.fromProject) : k.from === "user" ? "the user" : k.from;
      return "⇠ " + who + " is waiting (handoff)" + (k.status === "queued" ? " · queued" : "");
    }
    if (open) return "⇢ waiting on " + there + " (handoff)" +
      (!k.to ? (k.waiting ? " · queued for a lead slot" : " · starting its lead") : k.status === "queued" ? " · queued" : "");
    if (k.status === "answered") return k.auto ? "⇠ " + there + "'s lead stopped without answering" : "⇠ " + there + " answered (handoff)";
    return "⇢ handoff to " + there + " " + k.status + (k.error ? ": " + k.error : "");
  }

  function askLine(k, name) {
    const open = k.status === "queued" || k.status === "delivered";
    const queued = k.status === "queued" ? " (queued)" : "";
    let what;
    if (k.project) what = handoffWhat(k, name);
    else if (open && k.to === name) what = "⇠ ask from " + k.from + queued;
    else if (open) what = "⇢ asked " + k.to + queued;
    else if (k.status === "answered") what = k.auto ? "⇠ " + k.to + " stopped without answering" : "⇠ " + k.to + " answered";
    else what = "⇢ ask to " + k.to + " " + k.status + (k.error ? ": " + k.error : "");
    if (k.round && (k.parent || k.round > 1)) what += " · round " + k.round + "/" + k.of;
    const text = k.status === "answered" ? k.replyHead : open ? k.head : "";
    const t = open ? k.created : k.answered;
    const title = (k.project
      ? "handoff " + k.id + " from " + k.from + (k.fromProject ? " in " + k.fromProject : "") +
        " to " + (k.to || (k.waiting ? "the lead it waits a slot for" : "the lead being started")) + " in " + k.project + ": " + (k.head || "")
      : "ask " + k.id + ": " + (k.head || "")) +
      (k.guarded ? "\nFrom a guarded agent: its taker is told not to read or pass back secrets for it." : "");
    return '<div class="aask ask-' + esc(k.status) + (k.auto ? " ask-auto" : "") + (k.project ? " ask-handoff" : "") +
      (k.guarded ? " ask-guarded" : "") + '" title="' + esc(title) + '">' +
      '<span class="awhat">' + esc(what) + "</span>" +
      (k.guarded ? '<span class="aguarded">from a guarded agent</span>' : "") +
      (text ? '<span class="atext">' + esc(text) + "</span>" : "") +
      '<span class="when">' + esc(t ? when(t / 1000) : "") + "</span></div>";
  }

  // ---- What an agent's guard refused ---------------------------------------
  //
  // A guarded agy's guard denies what is not its to allow, with a reason;
  // what the user may approve says so here, with Approve -- once, for that
  // exact call: the herd lets it through that once and tells the agent to
  // run it again.  Waiting and approved ones stay; the rest for a while.

  // Kept denied by the user, or expired (its time out, its agent
  // respawned): settled, so off the card -- the log keeps them.
  function guardFor(name) {
    const recent = Date.now() - 30 * 60 * 1000;
    return ((herdNow.data && herdNow.data.guard) || []).filter((r) => r.session === name &&
      r.status !== "declined" && r.status !== "expired" &&
      (r.status === "pending" || r.status === "approved" || (r.created || 0) > recent));
  }

  function guardLine(r) {
    const what = r.status === "pending" ? "⛔ needs your approval"
      : r.status === "approved" ? "✓ approved once, not yet run"
      : r.status === "used" ? "✓ ran once, approved"
      : r.status === "lapsed" ? "approval lapsed"
      : "⛔ denied";
    return '<div class="gline g-' + esc(r.status) + '" title="' + esc((r.reason || "") + (r.rule ? "\n(" + r.rule + ")" : "")) + '">' +
      '<span class="gwhat">' + esc(what) + "</span>" +
      '<span class="gtext">' + esc(r.summary || "") + "</span>" +
      (r.status === "pending" && r.approvable
        ? '<button type="button" class="primary small" data-act="a:guard-approve" data-id="' + esc(r.id) +
          '" title="Let exactly this call run, once">Approve once</button>' +
          '<button type="button" class="ghost small" data-act="a:guard-reject" data-id="' + esc(r.id) +
          '" title="Keep it denied; the agent is told not to try it again">Keep denied</button>' : "") +
      '<span class="when">' + esc(r.created ? when(r.created / 1000) : "") + "</span></div>";
  }

  function guardHtml(name) {
    const rs = guardFor(name);
    return rs.length ? '<div class="aguard">' + rs.map(guardLine).join("") + "</div>" : "";
  }

  // ---- Where a run of asks stands ----------------------------------------
  //
  // A run: the asks of an agent nobody asked, and those made to answer
  // them, down the chain -- the lead's ask of rd, rd's rounds with qa.
  // The room's header says where the latest run of each agent stands:
  // who waits on whom, at which round, and once over, how it ended.

  const isOpen = (k) => k.status === "queued" || k.status === "delivered";

  // The room's runs, the latest of each agent that started one, newest
  // first: { from, asks } with ASKS newest first, as the herd sends them.
  function roomRuns(agents, asks) {
    const here = new Set(agents.map((a) => a.name));
    const runs = new Map();
    (asks || []).forEach((k) => {
      if (!k.run || !(here.has(k.from) || here.has(k.to))) return;
      if (!runs.has(k.run)) runs.set(k.run, []);
      runs.get(k.run).push(k);
    });
    const latest = new Map();
    runs.forEach((ks, id) => {
      const from = (ks.find((k) => k.id === id) || ks[ks.length - 1]).from;
      if (!latest.has(from)) latest.set(from, { from, asks: ks });
    });
    return [...latest.values()];
  }

  // The asks from the run's first down to K.
  function chainTo(k, ks) {
    const chain = [k];
    for (let p; chain[0].parent && (p = ks.find((x) => x.id === chain[0].parent)); ) chain.unshift(p);
    return chain;
  }

  function roundOf(k) {
    return k.round ? "round " + k.round + "/" + k.of : "";
  }

  // Each pair that went more than one round, as "rd → qa 3/4".
  function roundsUsed(ks) {
    const most = new Map();
    ks.forEach((k) => {
      const key = k.from + " → " + k.to;
      if (k.round > 1 && !(most.get(key) && most.get(key).round >= k.round)) most.set(key, k);
    });
    return [...most].map(([key, k]) => key + " " + k.round + "/" + k.of).join(", ");
  }

  function runLine(run, agents) {
    const ks = run.asks;
    const open = ks.filter(isOpen);
    const parts = [];
    let cls = "run-on", path, at;
    if (open.length) {
      // Deepest first, then newest: the ask everything above waits on.
      const deepest = open.map((k) => chainTo(k, ks)).sort((a, b) => b.length - a.length)[0];
      const d = deepest[deepest.length - 1];
      path = [deepest[0].from].concat(deepest.map(takerOf));
      const below = ks.find((k) => k.parent === d.id);  // its latest round with someone
      if (below) {
        parts.push(below.status !== "answered" ? takerOf(below) + " " + below.status
          : below.auto ? takerOf(below) + " stopped without answering"
          : (takerOf(below) + " answered " + roundOf(below)).trim());
      } else if (d.round && (d.parent || d.round > 1)) parts.push(roundOf(d));
      parts.push(d.status === "queued" && d.to ? takerOf(d) + " is busy; the ask waits" : "waiting on " + takerOf(d));
      at = d.created;
    } else {
      const top = ks.find((k) => !k.parent) || ks[0];
      const asker = agents.find((a) => a.name === run.from);
      path = [top.from, takerOf(top)];
      const used = roundsUsed(ks);
      at = top.answered;
      if (top.status === "failed" || top.status === "cancelled") {
        cls = "run-failed";
        parts.push("stopped: " + (top.project ? "handoff" : "ask") + " to " + takerOf(top) + " " + top.status +
          (top.error ? " (" + top.error + ")" : ""));
      } else if (top.auto) {
        cls = "run-auto";
        parts.push(takerOf(top) + " stopped without answering");
      } else if (asker && ["working", "starting", "waiting"].includes(asker.state)) {
        parts.push(takerOf(top) + " answered; " + run.from + " at work");
      } else {
        cls = "run-done";
        // The asker's last word, when it came after the run: its report.
        const said = asker && asker.said && asker.saidAt && asker.saidAt >= (top.answered || 0);
        parts.push("done: " + (said ? run.from + ": " + asker.said : top.replyHead || "answered"));
      }
      if (used) parts.splice(parts.length - 1, 0, used);
    }
    return '<div class="rrun ' + cls + '" title="' + esc("run " + ks[ks.length - 1].run) + '">' +
      '<span class="rpath">' + path.map(esc).join(" → ") + "</span>" +
      '<span class="rparts">' + esc(parts.join(" · ")) + "</span>" +
      '<span class="when">' + esc(at ? when(at / 1000) : "") + "</span></div>";
  }

  function runsHtml(agents, asks) {
    return roomRuns(agents, asks).map((r) => runLine(r, agents)).join("");
  }

  // ---- What a project works on -------------------------------------------
  //
  // Its branch's pull request and the ClickUp tasks the PR cites, which
  // Emacs looks up with pr-view and clickup-view -- they hold the tokens.
  // A default branch without a PR says nothing; two tasks show, the rest
  // behind +N.  A click opens the PR in pr-view, a task in clickup-view
  // -- at the desk.  On another device a chip is a link to the forge's or
  // ClickUp's own page instead, which opens there: in a tab, or in their
  // app.

  const QUIET_BRANCHES = ["main", "master", "develop", "HEAD"];

  function fmtBytes(n) {
    if (typeof n !== "number" || n <= 0) return "";
    if (n < 1024) return n + " B";
    const u = ["KB", "MB", "GB", "TB"];
    let i = -1;
    do {
      n /= 1024;
      i++;
    } while (n >= 1024 && i < u.length - 1);
    return (n >= 10 ? Math.round(n) : n.toFixed(1)) + " " + u[i];
  }

  function workFor(project) {
    const d = herdNow.data;
    return ((d && d.work) || []).find((w) => w.project === project) || null;
  }

  function safeColor(c) {
    return /^#[0-9a-fA-F]{3,8}$/.test(c || "") ? c : "#8b949e";
  }

  function prState(pr) {
    // Unsure: the last look failed or never came back, so the state on hand
    // may be stale -- show "?" (in prChip) rather than assert it.
    if (pr.unsure || !pr.state) return "";
    return pr.draft && pr.state === "OPEN" ? "Draft" : pr.state.charAt(0) + pr.state.slice(1).toLowerCase();
  }

  // On a phone, a chip is a link.  A pull request or task opens in
  // pr-view's or clickup-view's page there, when Emacs says it can
  // (herd.open): through Emacs's address for it that holds no secret,
  // its tokens staying in Emacs, in one tab per app used again.  Else on
  // the forge or ClickUp, which want their own login on the phone.
  function chip(cls, attrs, url, inner, app) {
    const href = !XW.elsewhere ? "" : app ? app.href : /^https?:\/\//i.test(url || "") ? url : "";
    const target = app ? app.tab : "_blank";
    // An app chip (pr-view, clickup-view) carries its tab name so a click
    // reuses the tab already showing that app -- see the a[data-app] handler.
    const marker = app ? ' data-app="' + esc(app.tab) + '"' : "";
    return href
      ? '<a class="' + cls + '" href="' + esc(href) + '" target="' + target + '"' + marker + (app ? "" : ' rel="noopener noreferrer"') + attrs + ">" + inner + "</a>"
      : '<button type="button" class="' + cls + '"' + attrs + ">" + inner + "</button>";
  }

  function opens(what) {
    const o = herdNow.data && herdNow.data.open;
    return !!(o && o[what] === true);
  }

  // A pull request of another repository than its project's -- an agent
  // at work on two -- is named with it, and opened by it: there may be
  // no checkout of it here.
  function prChip(root, pr, done) {
    const state = prState(pr);
    const other = pr.own === false;
    const tip = (other ? pr.owner + "/" + pr.repo + "#" + pr.id + ": " : "") +
      (pr.error || pr.title || (state ? "" : "Being looked up"));
    const where = other ? ' data-repo="' + esc([pr.host, pr.owner, pr.repo].join("/")) + '"' : ' data-root="' + esc(root) + '"';
    return chip("wchip pr" + (state ? " pr-" + esc(state.toLowerCase()) : "") + (done ? " wdone" : ""),
      ' data-act="a:work-pr"' + where + ' data-id="' + esc(String(pr.id)) + '" title="' + esc(tip) + '"', pr.url,
      (other ? '<span class="wrepo">' + esc(pr.repo) + "</span>" : "") +
      "#" + esc(String(pr.id)) + " " + esc(state || (pr.error ? "?" : "…")),
      opens("pr") && /^https:\/\/(github\.com|bitbucket\.org)\//.test(pr.url || "")
        ? { href: "/open/pr-view#pr=" + encodeURIComponent(pr.url), tab: "pr-view" } : null);
  }

  function taskChip(t, done) {
    return chip("wchip task" + (done ? " wdone" : ""),
      ' data-act="a:work-task" data-id="' + esc(t.id) + '" title="' +
      esc(t.error || [t.name, t.status].filter(Boolean).join(" · ") || t.id) + '"',
      t.url || "https://app.clickup.com/t/" + encodeURIComponent(t.id),
      (t.status ? '<span class="tdot" style="background:' + safeColor(t.color) + '"></span>' : "") +
      '<span class="wname">' + esc(t.name || t.id) + "</span>" +
      (t.status ? '<span class="tst">' + esc(t.status) + "</span>" : ""),
      opens("task") ? { href: "/open/clickup-view#task=" + encodeURIComponent(t.id), tab: "clickup-view" } : null);
  }

  function moreButton(key, open, n) {
    return '<button type="button" class="linkish wmore" data-act="a:work-more" data-key="' + esc(key) + '">' +
      (open ? "less" : "+" + n) + "</button>";
  }

  // What the checkout has not committed, which Emacs counts with pr-view:
  // a chip that opens pr-view's view of it.  None when it is clean.  Its
  // tracked files changed, and apart from them those git does not track
  // yet -- often a tool's own, never meant to be committed: counted as
  // uncommitted, everything looked committed and was not.
  function changesChip(w) {
    const c = w.changes;
    if (!c || !c.files) return "";
    const fresh = c.untracked || 0;
    const changed = c.files - fresh;
    const added = c.added - (c.untrackedAdded || 0);
    const files = (n) => n + (n === 1 ? " file" : " files");
    const parts = [];
    const said = [];
    if (changed) {
      parts.push("<span>" + changed + " uncommitted</span>" +
        (added ? '<span class="cadd">+' + added + "</span>" : "") +
        (c.deleted ? '<span class="cdel">−' + c.deleted + "</span>" : ""));
      said.push(files(changed) + " changed, not committed, +" + added + " −" + c.deleted);
    }
    if (fresh) {
      parts.push("<span>" + fresh + " untracked</span>");
      said.push(files(fresh) + " git does not track yet, +" + (c.untrackedAdded || 0));
    }
    return chip("wchip chg", ' data-act="a:work-changes" data-root="' + esc(w.root) + '" title="' + esc(said.join("; ")) + '"', "",
      parts.join('<span class="csep">·</span>'),
      opens("pr") ? { href: "/open/pr-view#local=" + encodeURIComponent(w.root), tab: "pr-view" } : null);
  }

  // Where a worktree's space is, from its one du: the biggest parts, then
  // the rest.  du counts a copy-on-write clone in full and cannot see
  // what it still shares, so a part the herd made as a clone is said to
  // have been made so -- never said to be shared.  A dependency the herd
  // linked takes no space here, and is said so.
  function sizeTitle(w) {
    const sp = w.sizeParts;
    if (!sp || !Array.isArray(sp.parts) || !sp.parts.length) return "worktree on disk";
    const parts = sp.parts.map((p) => p.path + " " + fmtBytes(p.bytes));
    const rest = fmtBytes(sp.rest);
    if (rest) parts.push("rest " + rest);
    const lines = ["worktree on disk: " + parts.join(" · ")];
    const made = (how) => sp.parts.filter((p) => p.how === how).map((p) => p.path);
    const was = (xs) => xs.join(", ") + (xs.length === 1 ? " was " : " were ");
    const said = [];
    if (made("clone").length) said.push(was(made("clone")) + "cloned from the main checkout's");
    if (made("uv").length) said.push(was(made("uv")) + "made by uv, cloned from its cache");
    if (said.length) lines.push(said.join("; ") + ". du counts a copy-on-write clone in full, though what is unchanged since may still share that disk.");
    const linked = Array.isArray(sp.linked) ? sp.linked : [];
    if (linked.length) lines.push(linked.join(", ") + (linked.length === 1 ? " is a link" : " are links") + " to the main checkout's: no space here.");
    return lines.join("\n");
  }

  function workHtml(w) {
    if (!w || !w.branch) return "";
    const pr = w.pr || null;
    const tasks = w.tasks || [];
    const changes = changesChip(w);
    // A worktree agent also shows how far its branch is ahead of its base
    // and the worktree's size on disk.
    const ahead = typeof w.ahead === "number" && w.ahead > 0
      ? '<span class="wchip ahead" title="' + esc(w.ahead + " commit" + (w.ahead === 1 ? "" : "s") + " ahead of " + (w.base || "its base")) + '">↑' + w.ahead + "</span>"
      : "";
    const sized = fmtBytes(w.size);
    const size = sized ? '<span class="wchip size" title="' + esc(sizeTitle(w)) + '">' + esc(sized) + "</span>" : "";
    const deps = Array.isArray(w.deps) && w.deps.length
      ? '<span class="wchip dep-skip" title="' + esc(w.deps.map((d) => d.name + " skipped: " + d.reason).join("; ")) + '">deps: ' + esc(w.deps.map((d) => d.name).join(", ")) + " skipped</span>"
      : "";
    // Worktrees a killed agent left behind, kept because unmerged or
    // unclean; the user removes them with M-x ghostherd-worktree-forget.
    const strays = Array.isArray(w.strays) && w.strays.length
      ? '<span class="wchip stray" title="' + esc(w.strays.map((x) => x.path + " [" + x.branch + "]").join("; ")) + ", left by a killed agent; remove with M-x ghostherd-worktree-forget\">" + w.strays.length + " idle worktree" + (w.strays.length === 1 ? "" : "s") + "</span>"
      : "";
    // The default branch without a PR says nothing of one -- but what it
    // has not committed, it says.
    const quiet = !pr && !tasks.length && !w.error && QUIET_BRANCHES.includes(w.branch);
    if (quiet && !changes && !ahead && !size && !deps && !strays) return "";
    const key = w.root + "|" + w.branch;
    const open = herdNow.workOpen.has(key);
    const prPart = quiet ? ""
      : pr ? prChip(w.root, pr)
      : w.error ? '<span class="wnote" title="' + esc(w.error) + '">PR not looked up</span>'
      : w.pending ? '<span class="wnote">PR…</span>'
      : '<span class="wnote">no PR</span>';
    return '<div class="work"><span class="wbranch" title="' + esc(w.root) + '">⎇ ' + esc(w.branch) + "</span>" + prPart + changes + ahead + size + deps + strays +
      (open ? tasks : tasks.slice(0, 2)).map((t) => taskChip(t)).join("") +
      (tasks.length > 2 ? moreButton(key, open, tasks.length - 2) : "") + "</div>";
  }

  // What an agent's conversation names, beyond its branch: the PRs of its
  // repository and the tasks, most recently named first.  A few of those
  // still open show; merged, declined or closed wait behind +N with the
  // others.  One that could not be looked up is not shown: most often an
  // id in a test or a file, not a real one.  What the branch line already
  // shows is not said twice.
  const LINKS_SHOWN = 4;

  function linksHtml(a) {
    const l = a.links;
    if (!l) return "";
    const w = a.work || workFor(a.project);
    const onBranch = w && w.root === l.root && w.pr ? w.pr.id : null;
    const branchTasks = new Set(((w && w.tasks) || []).map((t) => t.id));
    // Hide only a PR that could not be looked up and has no state at all --
    // most often an id in a test or a file, not a real one.  One once known
    // whose latest look failed keeps its chip, shown as "?" (unsure).
    const prs = (l.prs || []).filter((p) => !(p.own !== false && p.id === onBranch) && (p.state || !p.error)).map((p) => {
      const live = p.unsure || !p.state || p.state === "OPEN";
      return { live, html: prChip(l.root, p, !live) };
    });
    const tasks = (l.tasks || []).filter((t) => !branchTasks.has(t.id) && !t.error).map((t) => {
      const live = !t.done;
      return { live, html: taskChip(t, !live) };
    });
    const items = prs.concat(tasks);
    if (!items.length) return "";
    // Half the room for each, the rest for whichever has more.
    const livePrs = prs.filter((i) => i.live), liveTasks = tasks.filter((i) => i.live);
    const half = LINKS_SHOWN / 2;
    const first = livePrs.slice(0, half).concat(liveTasks.slice(0, half));
    const shown = first.concat(livePrs.slice(half).concat(liveTasks.slice(half)).slice(0, LINKS_SHOWN - first.length))
      .sort((x, y) => items.indexOf(x) - items.indexOf(y));
    const rest = items.filter((i) => !shown.includes(i));
    const key = "links|" + a.name;
    const open = herdNow.workOpen.has(key);
    return '<div class="work links"><span class="wbranch" title="Named in its conversation">↳ named</span>' +
      shown.concat(open ? rest : []).map((i) => i.html).join("") +
      (rest.length ? moreButton(key, open, rest.length) : "") + "</div>";
  }

  // A blocked agent's menu, as Emacs read it off the screen: what it
  // asks, in place of the one line that found it, and a button for each
  // choice with its words.  A menu Emacs could not read has none: a
  // number picked blind could be any of its choices.
  function reasonHtml(a) {
    const p = a.state === "blocked" && a.prompt;
    const text = p && p.question ? p.question : a.reason;
    return text && text !== "—" ? '<div class="areason' + (p && p.question ? " aprompt" : "") + '">' + esc(text) + "</div>" : "";
  }

  // A turn the Mac's sleep or a dropped connection cut short: the herd
  // continues it on its own.  `continueAt' is when (ms); the slim line
  // counts down to it (ticked by the per-second loop below) with a
  // Cancel.  Once the herd has stopped retrying, `continueStopped' shows
  // plainly that it will not try again.  `continueAfter' and
  // `continueAgain' are the cause's words ("after a dropped connection",
  // "the connection dropped again"); sleep's when Emacs sends none.
  function continueHtml(a) {
    const after = a.continueAfter || "after sleep";
    if (a.continueAt > 0) {
      const secs = Math.max(0, Math.round((a.continueAt - Date.now()) / 1000));
      return '<div class="contline"><span class="cont" data-continue-at="' + a.continueAt +
        '" data-continue-after="' + esc(after) + '">' +
        esc(contText(secs, after)) + "</span>" +
        '<button type="button" class="ghost small" data-act="a:agent-sleep-cancel" title="Leave it blocked; do not continue it">Cancel</button></div>';
    }
    if (a.continueStopped) {
      return '<div class="contline"><span class="cont stopped">stopped retrying — ' +
        esc(a.continueAgain || "the computer slept again") + "</span></div>";
    }
    return "";
  }

  // What the card's Esc does, by its CLI.  grok's turn does not stop on
  // Esc: Emacs sends it Ctrl+C while it works, Esc to a menu it shows, and
  // nothing at rest, where Ctrl+C would clear its box (ghostherd-interrupt).
  function escTitle(a) {
    return a && a.kind === "grok"
      ? "Stop its turn: Ctrl+C while it works (grok does not stop on Esc), Esc to a menu it shows; nothing at rest"
      : "Send Escape";
  }

  // A message the herd pasted that its CLI never took: Return was pressed
  // again and it is still in the input box.  The card says so, with the one
  // key that sends it -- pressed only while it is verifiably still there.
  function unsentHtml(a) {
    if (!a.unsent) return "";
    return '<div class="contline"><span class="cont stopped" title="' + esc(a.unsent) + '">' +
      "message not submitted — it sits in its input box</span>" +
      '<button type="button" class="ghost small" data-act="a:agent-submit" title="Press Return for it">Press Return</button></div>';
  }

  // /login's last step: signed in, the CLI waits for Return before it gives
  // the input box back.  The card's one click for it -- Emacs presses it
  // only while that step is verifiably still on the screen.
  const LOGIN_CONTINUE_BUTTON = '<button type="button" class="primary small" data-act="a:agent-login-continue" ' +
    'title="Signed in: press Return to continue">Press Return</button>';
  // Signed in again, the turn the expired login stopped waits to go on:
  // Emacs sends it the herd's continue prompt, as after a sleep.
  const CARRY_ON_BUTTON = '<button type="button" class="primary small" data-act="a:agent-carry-on" ' +
    'title="Signed in again: pick its turn up where the expired login stopped it">Continue</button>';

  // Its directory gone -- a worktree removed outside the herd.  The card
  // says so and where its branch stands, with a Stop; a respawn would be
  // refused.  `gone' is null while the directory is there.
  function goneBranchText(g) {
    if (!g.worktree) return "no branch of its own";
    if (g.state === "merged") return "branch " + g.branch + " merged into " + g.base + ", safe to stop";
    if (g.state === "deleted") return "branch " + g.branch + " gone too, safe to stop";
    if (g.state === "ahead") return "branch " + g.branch + " has " + g.ahead + " commit" + (g.ahead === 1 ? "" : "s") + " not on " + g.base;
    if (g.state === "unknown") return "could not tell whether " + g.branch + " is merged";
    return "reading branch " + g.branch + "…";
  }
  function goneHtml(a) {
    const g = a.gone;
    if (!g) return "";
    return '<div class="contline gone"><span class="cont stopped" title="' + esc(g.path) + ' is gone: a respawn is refused">' +
      (g.worktree ? "worktree" : "directory") + " gone — " + esc(goneBranchText(g)) + "</span>" +
      '<button type="button" class="ghost small danger" data-act="a:stop-gone" title="Stop it: it leaves the herd">Stop</button></div>';
  }

  function contText(secs, after) {
    after = after || "after sleep";
    return secs > 0 ? "continuing " + after + " in " + secs + "s" : "continuing " + after + " now…";
  }

  function choicesHtml(a) {
    if (a.state !== "blocked") return "";
    const options = (a.prompt && a.prompt.options) || [];
    if (!options.length) return "";
    return '<div class="choices">' + options.map((o) =>
      '<button type="button" class="ghost small choice" data-act="a:agent-answer" data-n="' + esc(String(o.n)) + '">' +
      '<span class="cn">' + esc(String(o.n)) + "</span>" + esc(o.label) + "</button>").join("") + "</div>";
  }

  // ---- What an agent runs on, and how full its context is ----------------
  //
  // Read from its transcript's last reply: the model and effort, a button
  // where Emacs can switch them -- for this session only, in the CLI's own
  // picker -- and the context as a bar of its model's window.  A switch
  // since that reply is the model its next one will be on.

  function tokens(n) {
    if (n >= 1e6) return +(n / 1e6).toFixed(n % 1e6 ? 1 : 0) + "M";
    return n >= 1000 ? Math.round(n / 1000) + "k" : String(n);
  }

  function pickerFor(a) {
    const p = herdNow.data && herdNow.data.pickers;
    return (p && p[a.kind]) || null;
  }

  // How a switch is scoped, in words: claude's picker does this session
  // alone; grok's and agy's have no such switch, so it is their default.
  function switchHint(p) {
    return p && p.scope === "default"
      ? "Switch its model; it becomes this CLI's default"
      : "Switch its model, for this session only";
  }

  // ---- What its replies cost, in tokens ----------------------------------
  //
  // Today's and the last seven days', as the sidecar tallies them from the
  // transcripts: one number each, input-equivalent tokens -- each kind
  // weighted as it is priced -- and the kinds in its title.  A touch
  // screen shows no title: a tap shows the same lines under it, and
  // another folds them (`herdNow.tallyOpen', by "a:" agent or "p:"
  // project); at the desk the title shows on hover as well.

  function tallyButton(cls, key, lines, label) {
    const open = herdNow.tallyOpen.has(key);
    return '<button type="button" class="' + cls + (open ? " open" : "") + '" data-act="a:tally" data-tally="' + esc(key) +
      '" aria-expanded="' + open + '" title="' + esc(lines.join("\n")) + '">' + esc(label) + "</button>";
  }

  function tallyMore(key, lines) {
    if (!lines || !herdNow.tallyOpen.has(key)) return "";
    return '<div class="tallymore" data-act="a:tally" data-tally="' + esc(key) + '">' +
      lines.map((l) => "<span>" + esc(l) + "</span>").join("") + "</div>";
  }

  const WEIGHTS = "input-equivalent tokens: input ×1, cache write ×1.25 (1-hour ×2), cache read ×0.1, output ×5";

  function kinds(t) {
    return "input " + tokens(t.input) + " · cache write " + tokens(t.write) + " · cache read " + tokens(t.read) +
      " · output " + tokens(t.output);
  }

  function tallyLines(t) {
    if (!t || !t.today || !t.week) return null;
    return ["What its replies cost, in " + WEIGHTS, "Today: " + tokens(t.today.w) + " — " + kinds(t.today),
      "Last 7 days: " + tokens(t.week.w) + " — " + kinds(t.week)];
  }

  function tallyHtml(t, name) {
    const lines = tallyLines(t);
    return lines ? tallyButton("mtally", "a:" + name, lines, "today " + tokens(t.today.w) + " · 7d " + tokens(t.week.w)) : "";
  }

  // A project's: its agents' together -- a stopped one's too -- and its
  // lead's share of the week, the rest its workers'.
  function share(part, all) {
    return all ? Math.round((part / all) * 100) + "%" : "—";
  }

  function projectTallyLines(p) {
    if (!p || !p.week || !p.week.w) return null;
    const line = (label, span) => label + ": " + tokens(p[span].w) + " — lead " + tokens(p.lead[span].w) + " (" +
      share(p.lead[span].w, p[span].w) + "), workers " + tokens(p.workers[span].w);
    return ["What this project's agents' replies cost, in " + WEIGHTS, line("Today", "today"), line("Last 7 days", "week"),
      kinds(p.week)];
  }

  function projectTallyHtml(p) {
    const lines = projectTallyLines(p);
    return lines ? tallyButton("ptally", "p:" + p.project, lines, "today " + tokens(p.today.w) + " · 7d " +
      tokens(p.week.w) + " · lead " + share(p.lead.week.w, p.week.w)) : "";
  }

  function meterHtml(a) {
    const m = a && a.meter;
    const p = pickerFor(a);
    const spent = tallyHtml(a && a.tally, a && a.name);
    // What the tally's title says, under the meter, once it is tapped.
    const more = tallyMore("a:" + (a && a.name), tallyLines(a && a.tally));
    // No meter (grok, agy do not report one) but a picker the herd knows:
    // still offer the switch, with no model to show until it is read.
    if (!m) {
      if (!p) return spent ? '<div class="meter">' + spent + "</div>" + more : "";
      const btn = a.switching
        ? '<span class="mchip"><span class="meff">switching…</span></span>'
        : '<button type="button" class="mchip mbare" data-act="a:model" title="' +
          esc(switchHint(p)) + '">Model</button>';
      return '<div class="meter">' + btn + spent + "</div>" + more;
    }
    const label = esc(m.model) + (m.effort ? '<span class="meff">' + esc(m.effort) + "</span>" : "");
    const cls = "mchip" + (m.pending ? " pending" : "");
    const said = m.pending ? "Switched; its next reply is the first on it" : "What its last reply ran on";
    const chip = a.switching
      ? '<span class="' + cls + '">' + label + '<span class="meff">switching…</span></span>'
      : p
        ? '<button type="button" class="' + cls + '" data-act="a:model" title="' + esc(said + ". " + switchHint(p)) + '">' + label + "</button>"
        : '<span class="' + cls + '" title="' + esc(said) + '">' + label + "</span>";
    let bar = "", words = "";
    if (m.compacted || m.tokens == null) {
      words = m.compacted ? "compacted" : "";
    } else if (m.window) {
      const pct = Math.round((m.tokens / m.window) * 100);
      // Coloured as the usage bars are, and at the same marks.
      bar = '<span class="ubar mbar"><span class="lvl-' + (pct >= 90 ? "hot" : pct >= 70 ? "warm" : "ok") + '" style="width:' +
        Math.min(100, pct) + '%"></span></span>';
      words = pct + "% · " + tokens(m.tokens) + " / " + tokens(m.window);
    } else {
      words = tokens(m.tokens);
    }
    return '<div class="meter">' + chip + (bar || words
      ? '<span class="mctx" title="Context: what its last reply was given, of the window its model has">' + bar +
        (words ? '<span class="mwords">' + esc(words) + "</span>" : "") + "</span>"
      : "") + spent + "</div>" + more;
  }

  // The model and effort to switch to, picked as chips; the effort left
  // unpicked stays as the picker has it.
  function modelFormHtml(a) {
    const p = a && herdNow.modelFor === a.name && pickerFor(a);
    if (!p) return "";
    const pick = herdNow.modelPick;
    const busy = !["idle", "done", "waiting"].includes(a.state);
    const chips = (list, key) => list.map((v) =>
      '<button type="button" class="mopt' + (pick[key] === v ? " on" : "") + '" data-act="a:model-pick" data-' + key + '="' + esc(v) + '">' +
      esc(v) + "</button>").join("");
    return '<div class="modelform">' +
      '<div class="mrow"><span class="mlab">Model</span><span class="mopts">' + chips(p.models, "model") + "</span></div>" +
      '<div class="mrow"><span class="mlab">Effort</span><span class="mopts">' + chips(p.efforts, "effort") + "</span></div>" +
      '<div class="mrow mgo"><button type="button" class="primary small" data-act="a:model-go"' +
      (busy || !pick.model ? " disabled" : "") + ">Switch</button>" +
      '<button type="button" class="ghost small" data-act="a:model-cancel">Cancel</button>' +
      '<span class="mnote">' + esc(busy ? "It is " + a.state + ": switch it once it is idle."
        : p.scope === "default" ? "No session-only switch here: this becomes the CLI's default."
          : "This session only: new sessions keep your default.") + "</span></div></div>";
  }

  function openModelForm(a) {
    const p = pickerFor(a);
    const m = a.meter || {};
    const family = (m.model || "").split(" ")[0];
    herdNow.modelFor = a.name;
    herdNow.modelPick = {
      model: p && p.models.includes(family) ? family : null,
      effort: p && p.efforts.includes(m.effort) ? m.effort : null,
    };
  }

  // ---- Sub-agents ------------------------------------------------------
  //
  // The sub-agents an agent has running, under it as its screen shows
  // them: what each is, how long it has run, its tool calls and context
  // so far, and its latest step.  Their time runs on here between looks.

  function elapsed(ms) {
    const s = Math.max(0, Math.floor(ms / 1000));
    if (s < 60) return s + "s";
    const m = Math.floor(s / 60);
    return m < 60 ? m + "m " + (s % 60) + "s" : Math.floor(m / 60) + "h " + (m % 60) + "m";
  }

  function runFor(started) {
    const t = stamp(started);
    return t ? '<span class="since" data-since="' + t + '">' + esc(elapsed(Date.now() - t)) + "</span>" : "";
  }

  setInterval(() => {
    app.querySelectorAll("[data-since]").forEach((el) => {
      el.textContent = elapsed(Date.now() - +el.getAttribute("data-since"));
    });
    // The continue countdown ticks here too, off its absolute target, so
    // it stays live between herd pushes without a re-render -- in the
    // cause's words, kept on the element.
    app.querySelectorAll("[data-continue-at]").forEach((el) => {
      el.textContent = contText(Math.max(0, Math.round((+el.getAttribute("data-continue-at") - Date.now()) / 1000)),
                                el.getAttribute("data-continue-after"));
    });
  }, 1000);

  function subsHtml(a) {
    const subs = (a && a.subs) || [];
    if (!subs.length) return "";
    // Only claude keeps each sub-agent's own transcript where the herd can
    // read it; grok has none, and agy's is elsewhere in another format.  So
    // only a claude's cards open -- the others are shown, not clickable.
    const open = a && a.kind === "claude";
    return '<div class="subs">' + subs.map((u) => {
      const stats = [runFor(u.started), esc(u.tools + (u.tools === 1 ? " tool" : " tools")),
        u.tokens != null ? esc(tokens(u.tokens)) : ""].filter(Boolean).join(" · ");
      return (open
        ? '<div class="sub" data-act="a:sub-open" data-sub="' + esc(u.id) + '" data-who="' + esc(u.who || "") +
          '" data-title="' + esc(u.title || "") + '" title="Open its transcript">'
        : '<div class="sub">') + '<div class="sline"><span class="sdot" aria-hidden="true"></span>' +
        '<span class="swho">' + esc(u.who || "sub-agent") + "</span>" +
        '<span class="stitle" title="' + esc(u.title || "") + '">' + esc(u.title || "") + "</span>" +
        '<span class="sstat">' + stats + "</span></div>" +
        (u.doing ? '<div class="sdoing" title="' + esc(u.doing) + '">' + esc(u.doing) + "</div>" : "") + "</div>";
    }).join("") + "</div>";
  }

  // Which of its sub-agents are running, as a key: a column redraws its
  // body when that changes, for the marks on the lines that started them.
  function subsKey(a) {
    return ((a && a.subs) || []).map((u) => u.id).join(" ");
  }

  // Put HTML in BOX, a strip below BODY: BODY stays at its bottom if it
  // was there when the strip grows or shrinks.
  function stripUnder(body, box, html) {
    if (!box || box.innerHTML === html) return;
    const atBottom = body && body.scrollHeight - body.scrollTop - body.clientHeight < 40;
    box.innerHTML = html;
    if (atBottom) body.scrollTop = body.scrollHeight;
  }

  function agentRow(a, kinds, asks) {
    const name = a.name;
    const acts = [];
    const menu = choicesHtml(a);
    // A turn killed by an expired login: offer the one-click /login first.
    if (a.login) {
      acts.push('<button type="button" class="primary small" data-act="a:agent-login" title="Run /login to sign in again">/login</button>');
    }
    // On the /login code-entry screen: open or copy the sign-in URL (then
    // the code goes back through Prompt).
    if (a.url) {
      acts.push('<button type="button" class="primary small" data-act="a:login-open" data-url="' + esc(a.url) + '" title="Open the sign-in link">Open sign-in link</button>');
      acts.push('<button type="button" class="ghost small" data-act="a:login-copy" data-url="' + esc(a.url) + '">Copy link</button>');
    }
    // Signed in, the sign-in's last step waits on Return; then its turn
    // waits to go on.
    if (a.loginContinue) acts.push(LOGIN_CONTINUE_BUTTON);
    if (a.carryOn) acts.push(CARRY_ON_BUTTON);
    // Its CLI's usage used up: its relay first.
    if (a.usedUp && a.relayTo && a.relayTo !== a.kind) {
      acts.push('<button type="button" class="primary small" data-act="a:relay" title="' + esc(a.usedUp) + '">Relay to ' + esc(a.relayTo) + "</button>");
    }
    acts.push('<button type="button" class="ghost small" data-act="a:compose">Prompt</button>');
    acts.push('<button type="button" class="ghost small" data-act="a:agent-interrupt" title="' + esc(escTitle(a)) + '">Esc</button>');
    acts.push('<button type="button" class="ghost small" data-act="a:screen">' + (herdNow.open.has(name) ? "Hide screen" : "Screen") + "</button>");
    acts.push('<button type="button" class="ghost small" data-act="a:capture" title="Save this screen to the log, pinned">Capture</button>');
    acts.push('<button type="button" class="linkish" data-act="a:more" title="More">' + (herdNow.more.has(name) ? "less" : "more") + "</button>");
    const more = herdNow.more.has(name)
      ? '<div class="aacts more">' +
        '<button type="button" class="ghost small" data-act="a:agent-abort" title="Send C-c">C-c</button>' +
        '<button type="button" class="ghost small" data-act="a:notes">Notes</button>' +
        '<button type="button" class="ghost small" data-act="a:respawn">Respawn</button>' +
        '<button type="button" class="ghost small" data-act="a:relay" title="Hand its work to a new agent on another CLI">Relay</button>' +
        '<button type="button" class="ghost small danger" data-act="a:kill">Kill</button></div>'
      : "";
    const compose = herdNow.composing.has(name)
      ? '<div class="agent-compose"><div class="attach">' + attachHtml(name) + '</div>' +
        '<textarea class="agent-text" rows="3" placeholder="' + esc(promptHint(name)) + '">' +
        esc(herdNow.compose[name] || "") + '</textarea>' + IMAGE_BUTTON + useButton(name) +
        '<button type="button" class="primary small" data-act="a:send">Send</button></div>'
      : "";
    const notes = herdNow.notes === name
      ? '<div class="notes-edit"><input class="notes-text" value="' + esc(herdNow.notesText) + '" placeholder="Role / notes">' +
        '<button type="button" class="primary small" data-act="a:notes-save">Save</button>' +
        '<button type="button" class="ghost small" data-act="a:notes-cancel">Cancel</button></div>'
      : "";
    const capture = herdNow.capture === name
      ? '<div class="notes-edit"><input class="capture-text" value="" placeholder="Save this screen — note (optional)">' +
        '<button type="button" class="primary small" data-act="a:capture-save">Save screen</button>' +
        '<button type="button" class="ghost small" data-act="a:capture-cancel">Cancel</button></div>'
      : "";
    const screen = herdNow.open.has(name)
      ? '<pre class="screen">' + esc(herdNow.screens[name] || "Capturing…") + "</pre>"
      : "";
    return (
      '<div class="arow s-' + esc(a.state) + '" data-name="' + esc(name) + '">' +
      '<div class="amain">' + stateBadge(a.state) +
      agentName(name) + genBadge(a) + agentBadge(a.kind) + leadBadge(a) +
      (a.startedBy ? '<span class="det startedby" title="Started by ' + esc(a.startedBy) + ' through herd start">by ' + esc(a.startedBy) + "</span>" : "") +
      (a.detached ? '<span class="det" title="Running, no view attached">▪ detached</span>' : "") +
      (a.manual ? '<span class="det" title="State set by hand">manual</span>' : "") +
      (a.progress != null ? '<span class="det">' + esc(a.progress) + "%</span>" : "") + backgroundHtml(a) + queuedMark(a) +
      (a.notes ? '<span class="anotes" title="' + esc(a.notes) + '">' + esc(a.notes) + "</span>" : "") +
      '<span class="when" title="' + esc(a.since ? new Date(a.since).toLocaleString() : "") + '">' +
      esc(a.since ? when(a.since / 1000) : "") + "</span></div>" +
      meterHtml(a) + modelFormHtml(a) + reasonHtml(a) + continueHtml(a) + unsentHtml(a) + goneHtml(a) +
      (a.said ? '<div class="aask said" title="' + esc((a.conversation ? "conversation " + a.conversation + "\n" : "") + a.said) + '">' +
        '<span class="awhat">said</span><span class="atext">' + esc(a.said) + "</span>" +
        '<span class="when">' + esc(a.saidAt ? when(a.saidAt / 1000) : "") + "</span></div>" : "") +
      (asks && asks.length ? '<div class="aasks">' + asks.map((k) => askLine(k, name)).join("") + "</div>" : "") +
      guardHtml(name) +
      (a.work ? workHtml(a.work) : "") + linksHtml(a) + menu +
      '<div class="aacts">' + acts.join("") + "</div>" + more + compose + notes + capture + screen + "</div>"
    );
  }

  function renderAgents() {
    herdNow.pending = false;
    if (herdNow.room) return renderRoom();
    const u = $("usage");
    const box = $("herd");
    if (!u || !box) return;
    const d = herdNow.data;
    if (!d) {
      box.innerHTML = '<p class="empty">Asking Emacs for the herd…</p>';
      return;
    }
    u.innerHTML = usageHtml(d.usage, d.hooks);
    renderSlots(false);
    const pb = $("primarybar");
    if (pb) pb.innerHTML = primaryPinnedHtml(d);
    const kinds = d.kinds || [];
    // Primary is pinned on top (primaryPinnedHtml), so keep its project out of
    // the list below -- otherwise it shows twice.
    const pd = d.primary && d.primary.directory;
    const by = new Map();
    (d.agents || []).forEach((a) => {
      if (pd && a.project === pd) return;
      const k = a.project || "";
      if (!by.has(k)) by.set(k, []);
      by.get(k).push(a);
    });
    const rank = (s) => (s in RANK ? RANK[s] : 9);
    // The state that needs you first (RANK), then the most recently
    // updated -- not by name, so what just moved rises.  A group's
    // recency is its most recent agent's.
    const groups = [...by.entries()].map(([project, list]) => ({
      project,
      list: list.sort((a, b) => rank(a.state) - rank(b.state) || (b.since || 0) - (a.since || 0)),
      since: Math.max(0, ...list.map((a) => a.since || 0)),
    }));
    groups.sort((a, b) => rank(a.list[0].state) - rank(b.list[0].state) || b.since - a.since);
    const keep = app.scrollTop;
    box.innerHTML = (herdNow.newFor === "*" ? newFormHtml(kinds) : "") + (groups.length
      ? groups.map((g) => {
          const counts = {};
          g.list.forEach((a) => (counts[a.state] = (counts[a.state] || 0) + 1));
          const summary = Object.keys(counts).sort((a, b) => rank(a) - rank(b))
            .map((s) => '<span class="st st-' + esc(s) + '">' + counts[s] + " " + esc(s) + "</span>").join("");
          const form = herdNow.newFor === g.project ? newFormHtml(kinds) : "";
          return '<section class="pgroup" data-project="' + esc(g.project) + '"><div class="phead">' +
            (g.project
              ? '<button type="button" class="pname" data-act="a:room" title="Open the room: ' + esc(g.project) +
                '\nits agents\' conversations side by side">' + esc(leaf(g.project)) + "</button>"
              : '<span class="pname">No project</span>') +
            '<span class="ppath">' + esc(g.project) + "</span>" + summary +
            projectTallyHtml((d.projectTally || []).find((p) => p.project === g.project)) +
            (g.project && !(d.primary && g.project === d.primary.directory)
              ? '<button type="button" class="linkish" data-act="a:new-agent">+ New agent</button>' : "") +
            tallyMore("p:" + g.project, projectTallyLines((d.projectTally || []).find((p) => p.project === g.project))) +
            "</div>" + workHtml(workFor(g.project)) + form + g.list.map((a) => agentRow(a, kinds, asksFor(a.name, d.asks))).join("") + "</section>";
        }).join("")
      : herdNow.newFor === "*" ? ""
      : '<p class="empty">No agents in the herd.  + New agent (above) or SPC a h n starts one, in any project.</p>');
    syncNewForm();
    app.scrollTop = keep;
  }

  // ---- The New agent form -------------------------------------------------
  //
  // One form, under a project's head or above the herd with a project
  // field first: any project the herd, project.el or a past conversation
  // knows, offered as you type, or a directory.  Then its kind, and New or
  // one of the project's past conversations -- its worktrees' too, newest
  // first -- to pick up again, in its own directory; fork where its CLI
  // can.  Open room goes to the project's room instead, where writing
  // starts its lead.  What it offers Emacs sends when it is asked, as the
  // form opens and its project changes; the form is filled in here, not
  // drawn again, so a box being typed in keeps its place.

  function newProjectNow() {
    return herdNow.newFor === "*" ? herdNow.newProject.trim() : herdNow.newFor || "";
  }

  function newConvNow() {
    const c = herdNow.convs[newProjectNow()];
    return (c && c.list.find((x) => x.path === herdNow.newConv)) || null;
  }

  function placeOptions() {
    return (herdNow.places || []).map((p) =>
      '<option value="' + esc(p.project) + '">' + esc(p.note || "") + "</option>").join("");
  }

  function convOptions() {
    const c = herdNow.convs[newProjectNow()];
    return '<option value="">New</option>' + ((c && c.list) || []).map((x) =>
      '<option value="' + esc(x.path) + '">' + esc((x.title || "(untitled) " + x.session) + " · " + x.note) +
      "</option>").join("");
  }

  function newFormHtml(kinds) {
    const global = herdNow.newFor === "*";
    const project = newProjectNow();
    return '<div class="newform' + (global ? " newglobal" : "") + '">' +
      (global ? '<input class="new-project" list="new-places" autocomplete="off" spellcheck="false" ' +
        'placeholder="project, or a directory" title="Any project the herd, project.el or a past conversation knows -- or type a directory" value="' +
        esc(herdNow.newProject) + '"><datalist id="new-places">' + placeOptions() + "</datalist>" : "") +
      '<select class="new-kind" title="Its CLI">' + kinds.map((k) => '<option value="' + esc(k) + '"' +
        (k === herdNow.newKind ? " selected" : "") + ">" + esc(k) + "</option>").join("") + "</select>" +
      '<select class="new-conv" title="Start fresh, or pick up one of the project\'s past conversations">' + convOptions() + "</select>" +
      '<input class="new-name" value="' + esc(herdNow.newName) + '" placeholder="name (default: kind-' + esc(leaf(project) || "project") + ')">' +
      '<label class="new-worktree-l" title="Its own git worktree and branch, off the project\'s HEAD"><input type="checkbox" class="new-worktree"' +
        (herdNow.newWorktree ? " checked" : "") + "> worktree</label>" +
      '<label class="new-fork-l" title="Branch a new conversation off it, rather than go on in it" hidden><input type="checkbox" class="new-fork"> fork</label>' +
      '<button type="button" class="primary small new-go" data-act="a:new-go">Start</button>' +
      '<button type="button" class="ghost small" data-act="a:new-room" title="Its room instead: writing there starts its lead">Open room</button>' +
      '<button type="button" class="ghost small" data-act="a:new-cancel">Cancel</button>' +
      '<div class="new-note"></div></div>';
  }

  // Fill the form in from what is chosen and what Emacs sent: the
  // conversations offered, and for one picked, its kind (fixed: only its
  // own CLI resumes it), fork, and what Start does.
  function syncNewForm() {
    const form = app.querySelector(".newform");
    if (!form) return;
    const project = newProjectNow();
    const c = herdNow.convs[project];
    const conv = newConvNow();
    if (!conv) herdNow.newConv = "";
    const fork = !!(conv && conv.fork && herdNow.newFork);
    const sel = form.querySelector(".new-conv");
    sel.innerHTML = convOptions();
    sel.value = herdNow.newConv;
    sel.disabled = !(c && c.list.length);
    const kind = form.querySelector(".new-kind");
    // A conversation's own CLI; else the one picked by hand, else the
    // project's (its ghostherd-project-kind), else the first.
    const want = conv ? conv.kind : (herdNow.newKindPicked && herdNow.newKind) || (c && c.kind) || herdNow.newKind;
    if (want && [...kind.options].some((o) => o.value === want)) kind.value = want;
    kind.disabled = !!conv;
    const name = form.querySelector(".new-name");
    name.hidden = !!conv;
    name.placeholder = "name (default: kind-" + (project ? leaf(project) : "project") + ")";
    form.querySelector(".new-worktree-l").hidden = !!conv;
    form.querySelector(".new-fork-l").hidden = !(conv && conv.fork);
    form.querySelector(".new-fork").checked = fork;
    form.querySelector(".new-go").textContent =
      !conv ? "Start" : fork ? "Fork" : conv.running ? "Show " + conv.running : "Resume";
    const n = c ? c.list.length : 0;
    form.querySelector(".new-note").textContent =
      !project ? (herdNow.places ? "" : "Reading where it can go…")
      : !c ? "Reading its past conversations…"
      : c.why ? c.why
      : conv && conv.running && !fork ? conv.running + " is on it already: it is shown, not started twice"
      : conv ? "Runs in the conversation's own directory, so no worktree"
      : n ? n + " past conversation" + (n === 1 ? "" : "s") + " to pick up again, or New" : "";
  }

  // Ask Emacs for the project's past conversations, once it is typed --
  // NOW, or after a pause in the typing.
  let newConvTimer = null;
  function askNewConversations(now) {
    clearTimeout(newConvTimer);
    const project = newProjectNow();
    syncNewForm();
    if (!project || herdNow.convs[project]) return;
    const go = () => {
      if (newProjectNow() !== project || herdNow.convs[project] || herdNow.convsAsked.has(project)) return;
      herdNow.convsAsked.add(project);
      emit("new-conversations", { project });
    };
    if (now) go();
    else newConvTimer = setTimeout(go, 300);
  }

  function openNewForm(project) {
    herdNow.newFor = project || "*";
    herdNow.newKind = herdNow.newName = herdNow.newConv = "";
    herdNow.newWorktree = herdNow.newFork = herdNow.newKindPicked = false;
    herdNow.convs = {};
    herdNow.convsAsked = new Set();
    herdNow.newRoomWant = null;
    if (!project) {
      const here = (herdNow.places || []).find((p) => p.here);
      herdNow.newProject = here ? here.project : "";
      emit("new-places");
    }
    renderAgents();
    askNewConversations(true);
    const first = app.querySelector(project ? ".newform select" : ".newform .new-project");
    if (first) first.focus();
  }

  function openNewRoom(root) {
    herdNow.newFor = herdNow.newRoomWant = null;
    setStatus("");
    openRoom(root);
  }

  // The top bar's + New agent: the form above the herd, from any tab.
  function newAgentAnywhere() {
    if (herdNow.room || view !== "agents") showAgents();
    openNewForm(null);
  }

  // ---- A project's room ---------------------------------------------------
  //
  // Its agents side by side, each a column of its conversation: what was
  // said to it -- by you, or by another agent through the herd -- and what
  // it answered, its tool calls folded to a line.  Emacs sends a column
  // when its transcript changes, and the screen of an agent whose
  // transcript it does not know.  Columns keep their order and are patched
  // in place: a column being read or typed into stays as it is.

  function openRoom(project) {
    if (!project) return;
    herdNow.room = project;
    herdNow.roomScreen = new Set();
    herdNow.convs = {};
    herdNow.chat = null;
    herdNow.subView = null;
    herdNow.subConv = null;
    herdNow.find = {};
    herdNow.away = {};
    app.innerHTML = "";
    chrome();
    // From the tap to its conversation drawn: a moment of the page's own,
    // in its timings (XW.begin; roomDrawn ends it).
    herdNow.roomTimed = false;
    if (XW.begin) XW.begin("room");
    // Drawn first, so the watch can say which columns are in view; told
    // here what it watches, so the drawing does not watch it too.
    herdNow.watchedChat = roomIsChat();
    renderRoom();
    watchRoom(true); // a (re)open: Emacs re-sends the conversation
  }

  // The room's first conversation drawn -- in a column in view, NAME, or
  // the chat: its moment over (openRoom).  Once a tap.
  function roomDrawn(name) {
    if (herdNow.roomTimed) return;
    const first = herdNow.roomFirst || [];
    if (name && first.length && !first.includes(name)) return;
    herdNow.roomTimed = true;
    XW.ready("room");
  }

  // Pagination: the poll refreshes the recent tail; `load earlier' fetches
  // one older page before what is held, prepended and kept.  MAX_HELD bounds
  // what a column or the chat keeps, so scrolling far back on a long day does
  // not grow memory without limit: loading past it slides the window back
  // (the newest, off-screen, drop and reload when you return to the foot).
  const MAX_HELD = 800;

  // A conversation entry's signature, to align an overlapping poll tail with
  // what is held and to drop a prepend's overlap (entries carry no stable id).
  function sigOf(e) {
    return (e.ts || "") + "|" + (e.who || "") + "|" + (e.kind || "") + "|" + String(e.text || "").slice(0, 48);
  }
  function lastIndexOfSig(list, e) {
    if (!e) return -1;
    const s = sigOf(e);
    for (let i = list.length - 1; i >= 0; i--) if (sigOf(list[i]) === s) return i;
    return -1;
  }
  function trimHead(list) { return list.length > MAX_HELD ? list.slice(list.length - MAX_HELD) : list; }
  function trimTail(list) { return list.length > MAX_HELD ? list.slice(0, MAX_HELD) : list; }
  function prependPage(page, held) {
    if (!held.length) return page.slice();
    const seen = new Set(held.slice(0, page.length + 8).map(sigOf));
    return page.filter((e) => !seen.has(sigOf(e))).concat(held);
  }
  function scrolledUp(body) {
    return !!body && body.scrollHeight - body.scrollTop - body.clientHeight >= 40;
  }

  // Where the reader is in a conversation, BODY (a column's or the chat's),
  // read up from its end: the entry at the top of the screen -- by its
  // signature, which of those it is from the end (two can share one), and
  // how far from the top it shows.  WebKit has no scroll anchoring
  // (overflow-anchor): what a redraw puts above pushes it down unless the
  // page puts it back -- `placeAnchor', whatever came above or below.
  function anchorOf(body) {
    const list = drawn.get(body);
    if (!list || !body.isConnected) return null;
    const top = body.getBoundingClientRect().top;
    for (const el of body.querySelectorAll(":scope > [data-i]")) {
      const r = el.getBoundingClientRect();
      if (r.bottom <= top + 1) continue;
      const i = +el.getAttribute("data-i"), e = list[i];
      if (!e) return null;
      const sig = sigOf(e);
      let nth = 0;
      for (let k = list.length - 1; k > i; k--) if (sigOf(list[k]) === sig) nth++;
      return { sig, nth, y: r.top - top };
    }
    return null;
  }
  function placeAnchor(body, a, entries) {
    if (!a) return false;
    let i = -1;
    for (let k = entries.length - 1, nth = 0; k >= 0; k--) {
      if (sigOf(entries[k]) !== a.sig) continue;
      if (nth++ === a.nth) { i = k; break; }
    }
    const el = i >= 0 && body.querySelector(':scope > [data-i="' + i + '"]');
    if (!el) return false;
    body.scrollTop += el.getBoundingClientRect().top - body.getBoundingClientRect().top - a.y;
    return true;
  }

  // A hand on a conversation: a finger on the screen, the fling after it,
  // a wheel.  iOS moves the scroller itself meanwhile, and a scrollTop the
  // page sets then is lost -- what a redraw put above would show instead
  // of what was read, the oldest of it after a few parts.  So a redraw
  // read up from the end waits (`handHold') until the hand has let go and
  // the scroll has settled (HAND_SETTLE without a scroll event).
  // A tap is no hand on it: only a finger that scrolled, and its fling
  // after while its scroll events go on, or a wheel.  A finger whose end
  // never came (a gesture the system took) is let go of after HAND_STUCK.
  const HAND_SETTLE = 200, HAND_STUCK = 5000;
  const hand = { down: false, scrolled: false, fling: false, at: 0 };
  const handHeld = new Map();
  let handTimer = null;
  function handMoving() {
    const quiet = Date.now() - hand.at;
    return (hand.down && quiet < HAND_STUCK) || (hand.fling && quiet < HAND_SETTLE);
  }
  function handHold(key, redraw) {
    handHeld.set(key, redraw);
    if (handTimer) return;
    const tick = () => {
      handTimer = null;
      if (handMoving()) { handTimer = setTimeout(tick, HAND_SETTLE); return; }
      const all = [...handHeld.values()];
      handHeld.clear();
      all.forEach((f) => f());
    };
    handTimer = setTimeout(tick, HAND_SETTLE);
  }
  addEventListener("touchstart", () => { hand.down = true; hand.scrolled = false; hand.at = Date.now(); }, { passive: true, capture: true });
  ["touchend", "touchcancel"].forEach((t) => addEventListener(t, (e) => {
    hand.down = !!(e.touches && e.touches.length);
    if (!hand.down) { hand.fling = hand.scrolled; hand.at = Date.now(); }
  }, { passive: true, capture: true }));
  addEventListener("wheel", () => { hand.fling = true; hand.at = Date.now(); }, { passive: true, capture: true });
  // The fling goes on as long as its scroll events do.
  addEventListener("scroll", () => {
    if (hand.down) hand.scrolled = true;
    if (handMoving()) hand.at = Date.now();
  }, { passive: true, capture: true });

  // What Emacs should send for the room: each column's conversation and
  // the screens shown instead, or the chat.  The poll reads only the recent
  // tail (default limit); `load earlier' pages are fetched on their own.
  // REOPEN is true when the page is opening (or reopening after a reload) the
  // room, false for a screen or view toggle of a room already open: on a
  // reopen Emacs re-sends the conversation, since a reloaded page lost it while
  // Emacs kept the room; a screen toggle must not re-read the whole thing.
  //
  // In parts (ghostherd-memory-page.el, "In parts"): each conversation by
  // version, then what changed alone; on another device -- a phone's slow
  // link -- its newest first (SPLIT) and the rest after, the columns in
  // view (FIRST) before any other.  An Emacs from before sends it whole.
  function watchRoom(reopen) {
    const chat = roomIsChat();
    if (reopen || !herdNow.roomFirst) herdNow.roomFirst = chat ? [] : colsInView();
    emit("room-watch", {
      project: herdNow.room, screens: [...herdNow.roomScreen],
      // The primary room is always a chat (it has no columns view), so ask
      // Emacs for the chat whatever roomView's default is on this device --
      // otherwise the desktop default (columns) leaves it "Reading…" forever.
      view: chat ? "chat" : herdNow.roomView,
      reopen: !!reopen,
      parts: true, split: !!XW.elsewhere, first: herdNow.roomFirst,
    });
    herdNow.watchedChat = chat;
  }

  // The room's columns you can see, by name: most of each on the screen.
  // Before they are drawn, the first on a phone -- one to the screen --
  // and every one at the desk.
  function colsInView() {
    const box = app.querySelector(".rcols");
    if (!box) {
      const agents = roomAgents().map((a) => a.name);
      return NARROW ? agents.slice(0, 1) : agents;
    }
    const r = box.getBoundingClientRect();
    return [...box.querySelectorAll(".rcol")].filter((c) => {
      const b = c.getBoundingClientRect();
      return b.width > 0 && Math.min(b.right, r.right) - Math.max(b.left, r.left) >= b.width * 0.4;
    }).map((c) => c.getAttribute("data-name"));
  }

  // A part of a conversation, P, taken into HELD, what the page has of it
  // (a column's conv, or the chat): HELD as it now is, or null when it is
  // as it was.  P.v is its version.  With no P.base it is whole -- its
  // newest P.entries, P.rest older ones still to come; MAKE makes it.
  // Else a change of version P.base: P.drop entries off the end and
  // P.entries after them, or P.head, older, before it all.  One older
  // than what is held was overtaken by a whole: old news.  One that does
  // not follow what is held -- a part missed, on a wake -- asks for the
  // whole again (RESEND), once until it comes.  A change with nothing
  // held waits for its whole, coming.
  function takePart(held, p, make, resend) {
    if (p.base == null) {
      if (held && held.v != null && p.v <= held.v) return null;
      return make();
    }
    if (!held || held.v == null || p.v <= held.v) return null;
    if (p.base !== held.v) {
      if (!held.resending) { held.resending = true; resend(); }
      return null;
    }
    held.v = p.v;
    if (p.head) {
      held.entries = p.head.concat(held.entries || []);
      held.rest = p.rest || 0;
      held.more = !!p.earlier;
      held.grew = true;
    } else {
      const keep = Math.max(0, (held.entries || []).length - (p.drop || 0));
      held.entries = (held.entries || []).slice(0, keep).concat(p.entries || []);
    }
    return held;
  }

  // A whole again -- a part was missed (room-resend) -- comes its newest
  // first: drawn at once, it would take away what you read up there, and
  // the older parts after it would put you back nowhere in particular.
  // Read up, the page keeps what it drew until the rest has come (HELD's
  // `hold'), then draws it all, the entry you read where it was.  At its
  // end, as ever.  Whether this part P waits so.
  function holdWhole(held, p, body) {
    if (p.base == null) held.hold = held.rest > 0 && !!body && body.hasAttribute("data-drawn") && scrolledUp(body);
    else if (held.hold && !(held.rest > 0 && scrolledUp(body))) held.hold = false;
    return !!held.hold;
  }

  // While a conversation's rest is on its way, a part of the room comes
  // every so often; none for PARTS_WAIT, one was lost -- the page slept,
  // say -- and what still waits is asked for whole.  No timer otherwise.
  const PARTS_WAIT = 20000;
  let partsDog = null;
  function partsCame() {
    clearTimeout(partsDog);
    partsDog = null;
    const waiting = () => Object.keys(herdNow.convs).filter((n) => herdNow.convs[n] && herdNow.convs[n].rest > 0);
    const chatWaits = () => !!(herdNow.chat && herdNow.chat.rest > 0);
    if (!herdNow.room || !(waiting().length || chatWaits())) return;
    const room = herdNow.room;
    partsDog = setTimeout(() => {
      partsDog = null;
      if (herdNow.room !== room) return;
      waiting().forEach((name) => {
        const c = herdNow.convs[name];
        if (!c.resending) { c.resending = true; emit("room-resend", { project: room, name }); }
      });
      if (chatWaits() && !herdNow.chat.resending) {
        herdNow.chat.resending = true;
        emit("room-resend", { project: room, chat: true });
      }
    }, PARTS_WAIT);
  }

  // Left before its conversation was drawn: no moment to time.
  function roomLeft() {
    if (herdNow.roomTimed) return;
    herdNow.roomTimed = true;
    if (XW.drop) XW.drop("room");
  }

  function closeRoom() {
    if (!herdNow.room) return;
    roomLeft();
    herdNow.room = null;
    if (herdNow.subView) { herdNow.subView = null; herdNow.subConv = null; emit("sub-unwatch", {}); }
    emit("room-close");
    if (herdNow.fromBoard) { herdNow.fromBoard = false; return bdBack(); }
    showAgents();
  }

  // The room's agents, its lead first.
  function roomAgents() {
    const d = herdNow.data;
    return ((d && d.agents) || [])
      .filter((a) => (a.project || "") === herdNow.room)
      .sort((a, b) => (b.role === "lead") - (a.role === "lead") || a.name.localeCompare(b.name));
  }

  function leadBadge(a) {
    return a.role === "lead" ? '<span class="leadb" title="Plans the work and hands it out">lead</span>' : "";
  }

  // The second of a name, the third: an agent that took over another's
  // name and work (M-x ghostherd-relay).
  function roman(n) {
    if (!(n > 0 && n < 40)) return String(n);
    return "X".repeat(Math.floor(n / 10)) + ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"][n % 10];
  }

  function genBadge(a) {
    if (!(a.generation > 1)) return "";
    const line = (a.line || []).concat([a.kind]).join(" → ");
    return '<span class="genb" title="' + esc(a.name + " " + roman(a.generation) + ", the CLIs of its name: " + line) + '">' +
      esc(roman(a.generation)) + "</span>";
  }

  // Relay NAME's work to an heir on another CLI, the herd's choice last
  // so that it has the keys.
  function askRelay(a) {
    const kinds = (herdNow.data && herdNow.data.relayKinds) || [];
    const choices = [["Cancel", "ghost", null]];
    const order = kinds.filter((k) => k.kind !== a.relayTo).concat(kinds.filter((k) => k.kind === a.relayTo));
    order.forEach((k) => choices.push([
      k.kind + (k.kind === a.kind ? " again" : "") + (k.usedUp ? " · used up" : ""),
      k.kind === a.relayTo ? "primary" : "ghost",
      () => emit("agent-relay", { name: a.name, kind: k.kind })]));
    ask("Relay " + a.name + " to…",
      (a.usedUp ? a.usedUp.charAt(0).toUpperCase() + a.usedUp.slice(1) + ". " : "") +
        "A new agent takes its name, its role and its work: its first message is where the work stands, " +
        "read from " + a.name + "'s conversation, its asks and the checkout. " + a.name + " stops; its conversation stays.",
      choices);
  }

  function roomTop(a, asks) {
    const name = a.name;
    const acts = [];
    const menu = choicesHtml(a);
    // Anything that needs the user stays out of the ▾ fold on the phone:
    // /login, its link, its last Return and Continue are kept; Esc and
    // Screen are `sec' (secondary).
    if (a.login) {
      acts.push('<button type="button" class="primary small" data-act="a:agent-login" title="Run /login to sign in again">/login</button>');
    }
    if (a.url) {
      acts.push('<button type="button" class="primary small" data-act="a:login-open" data-url="' + esc(a.url) + '" title="Open the sign-in link">Open sign-in link</button>');
      acts.push('<button type="button" class="ghost small" data-act="a:login-copy" data-url="' + esc(a.url) + '">Copy link</button>');
    }
    if (a.loginContinue) acts.push(LOGIN_CONTINUE_BUTTON);
    if (a.carryOn) acts.push(CARRY_ON_BUTTON);
    // Only Esc and Screen: a narrow column draws them as icons on its
    // first line, and the row they leave takes no room.
    const onlySec = !acts.length;
    acts.push('<button type="button" class="ghost small sec" data-act="a:agent-interrupt" title="' + esc(escTitle(a)) + '">Esc</button>');
    const conv = herdNow.convs[name];
    if (!(conv && conv.none)) {
      const scr = herdNow.roomScreen.has(name);
      acts.push('<button type="button" class="ghost small sec' + (scr ? " scr-on" : "") + '" data-act="a:room-screen" title="' +
        (scr ? "Back to its conversation" : "Its screen, as the terminal shows it") + '">' + (scr ? "Conversation" : "Screen") + "</button>");
    }
    const expanded = herdNow.cardOpen.has(name);
    const cx = '<button type="button" class="cx" data-act="a:card-expand" title="' +
      (expanded ? "Less" : "More: notes, chips, branch, Esc / Screen") + '">' + (expanded ? "▴" : "▾") + "</button>";
    const work = (a.work ? workHtml(a.work) : "") + linksHtml(a);
    return '<div class="amain">' + stateBadge(a.state, a.reason) +
      '<span class="rdot" style="color:' + sessionColor(name) + '" title="Its colour in every column">●</span>' +
      agentName(name) + agentBadge(a.kind) + leadBadge(a) + backgroundHtml(a) + queuedMark(a) +
      (a.startedBy ? '<span class="det startedby" title="Started by ' + esc(a.startedBy) + ' through herd start">by ' + esc(a.startedBy) + "</span>" : "") +
      (a.notes ? '<span class="anotes" title="' + esc(a.notes) + '">' + esc(a.notes) + "</span>" : "") +
      '<span class="when" title="' + esc(a.since ? new Date(a.since).toLocaleString() : "") + '">' +
      esc(a.since ? when(a.since / 1000) : "") + "</span>" + cx + "</div>" +
      meterHtml(a) + modelFormHtml(a) + sayHtml(a, asks) + reasonHtml(a) + continueHtml(a) + unsentHtml(a) + goneHtml(a) +
      (asks && asks.length ? '<div class="aasks">' + asks.map((k) => askLine(k, name)).join("") + "</div>" : "") +
      guardHtml(name) +
      (work ? '<div class="wline">' + work + '<button type="button" class="linkish wfit" data-act="a:chips-more" hidden></button></div>' : "") +
      menu + '<div class="aacts' + (onlySec ? " onlysec" : "") + '">' + acts.join("") + "</div>";
  }

  // A narrow column's head (style.css, .rcol.narrow) says in one line
  // what the wide one says in several: its open asks -- the first, and
  // how many more -- else what it is doing, else its notes; the rest of
  // them in its title.  A press opens the whole head (the ▾).  A blocked
  // agent's menu is not this line's: it keeps its own block.
  function sayHtml(a, asks) {
    asks = asks || [];
    const menu = a.state === "blocked" && a.prompt && a.prompt.question;
    const reason = a.reason && a.reason !== "—" && !menu ? a.reason : "";
    const doing = reason && a.state !== "idle" ? reason : "";
    const by = a.startedBy ? "by " + a.startedBy : "";
    const text = doing || a.notes || reason || by;
    const lines = [];
    if (a.notes) lines.push("Notes: " + a.notes);
    if (reason) lines.push(a.state + ": " + reason);
    if (by) lines.push("Started by " + a.startedBy + " through herd start");
    asks.forEach((k) => lines.push((k.to === a.name ? "ask from " + k.from : "ask to " + (k.to || leaf(k.project || ""))) +
      " (" + k.status + "): " + ((k.status === "answered" ? k.replyHead : k.head) || "")));
    if (!asks.length && !text) return "";
    const inner = asks.length
      ? askLine(asks[0], a.name) + (asks.length > 1 ? '<span class="rmore">+' + (asks.length - 1) + "</span>" : "")
      : '<span class="rtext">' + esc(text) + "</span>";
    return '<div class="rsay" data-act="a:card-expand" title="' + esc(lines.concat(["Press for the whole head"]).join("\n")) + '">' +
      inner + "</div>";
  }

  // A narrow column's chips: one line, those that do not fit (and those
  // the line held back behind +N) counted on one +N at its end, which
  // opens them all.  Read after the heads are drawn, all columns at once:
  // their chips reset, then measured, then cut -- one layout each way.
  function fitChips(cols) {
    const lines = cols.map((col) => {
      const line = col.querySelector(".rtop .wline");
      if (!line) return null;
      const fit = line.querySelector(".wfit");
      const chips = [...line.querySelectorAll(".wline > .work > *:not(.wmore)")];
      chips.forEach((c) => c.classList.remove("wcut"));
      // Held back by the line's own +N, as the wide head has it.
      const held = [...line.querySelectorAll(".wmore")].reduce((n, b) => n + (+(/^\+(\d+)$/.exec(b.textContent) || [0, 0])[1]), 0);
      const narrow = getComputedStyle(line).getPropertyValue("--narrow").trim() === "1";
      const open = col.classList.contains("chips-open");
      // Shown while measured, at its widest, so its room is kept.
      fit.hidden = !narrow;
      fit.textContent = open ? "less" : "+" + (held + chips.length);
      return { line, fit, chips, held, narrow, open };
    });
    const cuts = lines.map((l) => {
      if (!l || !l.narrow || l.open) return [];
      const shown = l.chips.filter((c) => c.getClientRects().length);
      if (!shown.length) return [];
      const right = l.line.getBoundingClientRect().right;
      const over = shown[shown.length - 1].getBoundingClientRect().right > right;
      // Room for +N too, when there is to be one.
      const limit = over || l.held ? right - l.fit.getBoundingClientRect().width - 6 : right;
      // The first that runs past it, and every chip after it.
      const at = shown.findIndex((c) => c.getBoundingClientRect().right > limit);
      return at < 0 ? [] : shown.slice(at);
    });
    lines.forEach((l, i) => {
      if (!l || !l.narrow || l.open) return;
      cuts[i].forEach((c) => c.classList.add("wcut"));
      const n = l.held + cuts[i].length;
      l.fit.hidden = !n;
      l.fit.textContent = "+" + n;
      l.fit.title = n + " more: " + cuts[i].map((c) => c.title || c.textContent.trim()).filter(Boolean).join(" · ");
    });
  }

  // A column is narrow by its own width, not by how many there are: 640
  // px or less, which at the desk gives it the short head (style.css).
  // Measured, not a container query: a column, or a part of one, that is
  // a size container has WebKit lay it out short now and then as its
  // words are replaced, and the conversation beside it, at its end, has
  // its scroll cut by as much (herd-subs.js: the sub-agents' strip that
  // appears leaves it above its end).  Its width changes with the window
  // and the columns beside it; then its chips are fitted again.  Told
  // before the first paint, so a room never shows the wide head first.
  const NARROW_COL = 640;
  const narrowCols = new WeakMap();
  const colsResize = typeof ResizeObserver === "function"
    ? new ResizeObserver((es) => {
      const cols = es.map((e) => e.target).filter((c) => c.isConnected);
      es.forEach((e) => {
        const narrow = e.contentRect.width <= NARROW_COL;
        narrowCols.set(e.target, narrow);
        e.target.classList.toggle("narrow", narrow);
      });
      fitChips(cols);
    })
    : null;

  function toolsSummary(tools) {
    const counts = new Map();
    tools.forEach((t) => counts.set(t.name, (counts.get(t.name) || 0) + 1));
    return [...counts.entries()].map(([n, k]) => (k > 1 ? n + " ×" + k : n)).join(" · ");
  }

  function entHead(who, cls, ts, color, copyHtml) {
    return '<div class="ehead"><span class="who' + (cls ? " " + cls : "") + '"' +
      (color ? ' style="color:' + color + '"' : "") + ">" + esc(who) + "</span>" +
      (ts ? '<span class="when" title="' + esc(fullTime(ts)) + '">' + esc(when(ts)) + "</span>" : "") + (copyHtml || "") + "</div>";
  }

  // A head's Copy: what it heads, as it was written -- an agent's
  // markdown rather than the page's drawing of it, your words without the
  // herd's notes before them.  Found at the click by place, in the list
  // its body was drawn from; PART names which of the entry's texts.
  function copyButton(i, text, part) {
    if (!text || !String(text).trim()) return "";
    return '<button type="button" class="linkish ecopy" data-act="a:copy-entry" data-i="' + i + '"' +
      (part ? ' data-part="' + part + '"' : "") + ' title="Copy it, as written">Copy</button>';
  }

  // What each conversation body was last drawn from.  The data moves on
  // without a redraw now and then -- a conversation that arrives while
  // the page shows something else -- and a Copy must copy the turn it is
  // on, not the one now at its place.
  const drawn = new WeakMap();

  function plainBody(text) {
    return '<div class="ebody plain">' + esc(text || "") + "</div>";
  }

  const SUB_LONG = 600;

  // Each agent has one colour wherever it speaks, the Log's lane colour:
  // an entry's bar is whoever said it.  You are the blue.  Each carries
  // its place in what its body was drawn from (data-i): where a jump to
  // one of yours, or to a find's hit, finds it.
  function entryHtml(e, i, name, kind, keyns) {
    return entryInner(e, i, name, kind, keyns).replace('<div class="ent', '<div data-i="' + i + '" class="ent');
  }

  function entryInner(e, i, name, kind, keyns) {
    const from = e.who && e.who !== "user" ? e.who : "";
    const color = from ? sessionColor(from) : "";
    const bar = color ? ' style="border-left-color:' + color + '"' : "";
    const own = sessionColor(name);
    if (e.role === "assistant") {
      return '<div class="ent me" style="border-left-color:' + own + '">' + entHead(name, "", e.ts, own, copyButton(i, e.text)) +
        '<div class="ebody msg-body">' + markdown(e.text) + "</div>" + imagesHtml(e.images) + "</div>";
    }
    if (e.role === "tools") {
      const tools = e.tools || [];
      // A key namespace keeps the sub-agent overlay's tool groups from
      // sharing toolsOpen keys with the parent column's own entries.
      const key = (keyns || name) + ":" + i;
      const open = herdNow.toolsOpen.has(key);
      // A sub-agent it started: running while its agent says so, else
      // how it ended -- nothing when that is not known.
      const live = new Set(((agentByName(name) || {}).subs || []).map((u) => u.id));
      const mark = (t) => !t.sub && !t.state ? ""
        : !t.state ? (live.has(t.sub) ? '<span class="tstate run" title="Running">●</span>' : "")
        : t.state === "completed" ? '<span class="tstate" title="Finished">✓</span>'
        : '<span class="tstate bad">' + esc(t.state) + "</span>";
      const running = tools.filter((t) => t.sub && !t.state && live.has(t.sub)).length;
      return '<div class="ent tools"><button type="button" class="linkish" data-act="a:room-tools" data-key="' + esc(key) + '">' +
        (open ? "▾ " : "▸ ") + esc(toolsSummary(tools) + (running ? " · " + running + " running" : "")) + "</button>" +
        (open ? '<div class="tlist">' + tools.map((t) => '<div class="trow"><span class="tname">' + esc(t.name) + "</span>" + mark(t) +
          '<span class="thint">' + esc(t.hint || "") + "</span></div>").join("") + "</div>" : "") + "</div>";
    }
    if (e.role === "sub") {
      // A sub-agent's report, where it came back: its start in sight,
      // the rest a press away.
      const key = name + "|" + (e.sub || "") + "|" + (e.ts || i);
      const open = herdNow.subOpen.has(key);
      const text = e.text || "";
      const long = text.length > SUB_LONG || text.split("\n").length > 8;
      const ended = !e.status ? "sent a message" : e.status === "completed" ? "finished" : e.status;
      const stats = [ended, e.ms != null ? elapsed(e.ms) : "", e.uses != null ? e.uses + (e.uses === 1 ? " tool" : " tools") : "",
        e.tokens != null ? tokens(e.tokens) + " tokens" : ""].filter(Boolean).join(" · ");
      return '<div class="ent sub' + (long && !open ? " folded" : "") + (e.status && e.status !== "completed" ? " bad" : "") +
        '" style="border-left-color:' + own + '">' +
        entHead("↩ " + (e.who || "sub-agent") + (e.title ? " · " + e.title : ""), "", e.ts, own, copyButton(i, text)) +
        '<div class="smeta">' + esc(stats) + "</div>" +
        (text ? '<div class="ebody msg-body">' + markdown(text) + "</div>" : "") + imagesHtml(e.images) +
        (long ? '<button type="button" class="linkish cmore" data-act="a:sub-more" data-key="' + esc(key) + '">' + (open ? "less" : "more") + "</button>" : "") +
        "</div>";
    }
    if (e.role === "herd") {
      const what = e.kind === "ask" ? "⇢ asks " + (e.to || "?") + (e.ask ? " · ask " + e.ask : "") : "⇢ answers ask " + (e.ask || "");
      const tcolor = e.to ? sessionColor(e.to) : "";
      // What came of an ask: its answer, the client's error, or nothing yet.
      const outcome = e.kind !== "ask" ? ""
        : e.failed ? '<div class="eanswer failed">' + entHead("the ask failed", "", null, "", copyButton(i, e.answer, "answer")) + plainBody(e.answer) + "</div>"
        : e.answer ? '<div class="eanswer">' + entHead("⇠ " + (e.to || "") + " answered" + (e.auto ? " — stopped without answering" : ""), "", null, tcolor,
          copyButton(i, e.answer, "answer")) + plainBody(e.answer) + imagesHtml(e.answerImages) + traceHtml(e.trace) + "</div>"
        : e.waiting ? '<div class="eanswer waiting">waiting for ' + esc(e.to || "the answer") + "…</div>"
        : "";
      return '<div class="ent herd" style="border-left-color:' + own + '">' +
        entHead(name + " " + what, "", e.ts, own, copyButton(i, e.text)) + plainBody(e.text) +
        (e.kind === "reply" ? imagesHtml(e.images) : "") + outcome + "</div>";
    }
    // Said to it: by you, or through the herd.
    if (e.kind === "command") return '<div class="ent small">› ' + esc(e.text) + "</div>";
    if (e.kind === "note") return '<div class="ent small">' + esc(e.text) + "</div>";
    if (e.kind === "ask") return '<div class="ent from"' + bar + ">" + entHead("⇠ " + from + " asks", "", e.ts, color, copyButton(i, e.text)) + plainBody(e.text) + "</div>";
    if (e.kind === "answer") {
      const how = e.failed ? " — ask failed" : e.auto ? " — stopped without answering" : "";
      return '<div class="ent from"' + bar + ">" + entHead("⇠ " + from + " answered" + how, "", e.ts, color, copyButton(i, e.text)) + plainBody(e.text) +
        imagesHtml(e.images) + traceHtml(e.trace) + "</div>";
    }
    if (e.kind === "message" && from) return '<div class="ent from"' + bar + ">" + entHead("⇠ " + from, "", e.ts, color, copyButton(i, e.text)) + plainBody(e.text) + "</div>";
    return '<div class="ent you">' + entHead("You", "", e.ts, "", copyButton(i, e.text)) + herdNotes(e) + plainBody(e.text) + "</div>";
  }

  // `Load earlier' when there is more to show and the limit can still grow;
  // once it is at the most the sidecar returns, a note instead -- the rest
  // stays in the transcript.
  // The top of a paged view: a button while older turns remain, a note while
  // a page is loading, and a clear end at the start of the conversation.
  function earlierHtml(conv, act) {
    if (!conv) return "";
    // The loading state keeps the button element (same height), so a prepend's
    // reading-position hold is not thrown off by the top changing size.
    if (conv.loading) return '<div class="loadmore"><button type="button" class="ghost small" disabled>Loading earlier…</button></div>';
    if (conv.more) return '<div class="loadmore"><button type="button" class="ghost small" data-act="' + act + '">Load earlier</button></div>';
    // The clear end, once you have paged back to it; nothing for a short
    // conversation shown whole from the start (no note cluttering its top).
    if (conv.limitReached) return '<p class="rnote">Earlier is in the transcript, past what is loaded here.</p>';
    if (conv.paged) return '<p class="rnote">Start of the conversation.</p>';
    return "";
  }

  function roomBodyHtml(a) {
    const name = a.name;
    const conv = herdNow.convs[name];
    const screen = '<pre class="screen">' + esc(herdNow.screens[name] || "Capturing…") + "</pre>";
    if (conv && conv.none) {
      return '<p class="rnote">No conversation known for ' + esc(name) + " yet: its hooks name it once they are in " +
        "(M-x ghostherd-install-hooks, then respawn it), and one started on a conversation by id is found without them. " +
        "Its screen meanwhile:</p>" + screen;
    }
    if (herdNow.roomScreen.has(name)) return screen;
    // Its queue is its screen's: shown whether its conversation is or not.
    if (!conv) return '<p class="rnote">Reading its conversation…</p>' + queuedHtml(a, queuedLeft(name, [], saidToCol));
    if (conv.error) {
      return '<p class="rnote error">Could not read its conversation: ' + esc(conv.error) + "</p>" +
        queuedHtml(a, queuedLeft(name, [], saidToCol));
    }
    const entries = conv.entries || [];
    const queued = queuedLeft(name, entries, saidToCol);
    return earlierHtml(conv, "a:load-earlier") +
      (entries.length ? entries.map((e, i) => entryHtml(e, i, name, a.kind)).join("")
        : queued.length ? "" : '<p class="rnote">Nothing said yet.</p>') + queuedHtml(a, queued);
  }

  function roomColumn(name) {
    return app.querySelector('.rcol[data-name="' + cssName(name) + '"]');
  }

  // A poll's recent tail, P, merged into column NAME's held entries.  At the
  // foot it is live: the tail is aligned onto what is held (by the newest
  // entry they share) and the new entries appended, the head trimmed to
  // MAX_HELD.  Scrolled up it is frozen: the held view is left alone so it
  // does not jump, and the tail catches up when you return to the foot.
  // Returns whether the held entries changed (whether to redraw).
  function mergeRecentConv(name, p) {
    if (p.none || p.error) {
      herdNow.convs[name] = { entries: [], more: false, none: p.none, error: p.error, cli: p.cli, path: p.path };
      return true;
    }
    const recent = p.entries || [];
    const prev = herdNow.convs[name];
    // Until `load earlier' is used, the recent tail IS the whole view: just
    // take it, as the room always has.  Only once paged does the tail have to
    // be merged into the kept earlier pages.
    if (!prev || !prev.paged) {
      herdNow.convs[name] = { entries: recent.slice(), more: !!p.earlier, loading: false, paged: false, cli: p.cli, path: p.path };
      return true;
    }
    prev.path = p.path || prev.path;
    prev.cli = p.cli || prev.cli;
    if (!recent.length) return false;
    const i = lastIndexOfSig(prev.entries, recent[0]);
    if (i < 0) { // a different conversation (a respawn, or the tail slid far): reset
      herdNow.convs[name] = { entries: recent.slice(), more: !!p.earlier, loading: false, paged: false, cli: p.cli, path: p.path };
      return true;
    }
    // A continuation of what is held: freeze while reading history (the tail
    // catches up when you return to the foot), else align and append.
    if (!herdNow.away[findKey(name)] && scrolledUp(roomColumn(name) && roomColumn(name).querySelector(".rbody"))) return false;
    prev.entries = trimHead(prev.entries.slice(0, i).concat(recent));
    return true;
  }

  // An older page, P, prepended to column NAME's held entries, the newest
  // trimmed to MAX_HELD (they reload at the foot).  An empty page while more
  // remains means the reader's window is exhausted (a transcript past the
  // 8 MB tail it reads): stop, and say the rest is in the transcript.
  function prependConv(name, p) {
    const conv = herdNow.convs[name];
    if (!conv) return;
    conv.loading = false;
    conv.paged = true;
    if (p.error) { conv.more = false; conv.error = p.error; return; }
    const page = p.entries || [];
    if (page.length) {
      const all = prependPage(page, conv.entries || []);
      conv.entries = trimTail(all);
      // Its newest cut away: no longer what Emacs's next change follows.
      if (conv.v != null && all.length > conv.entries.length) conv.v = -1;
      conv.more = !!p.earlier;
      conv.limitReached = false;
    } else {
      conv.more = false;
      conv.limitReached = !!p.earlier;  // older exist but past the readable tail
    }
  }

  // The chat's recent tail merged into what is held -- the merged timeline's
  // version of `mergeRecentConv'; the meta (absent, missing) is always
  // refreshed, the entries only when at the foot.
  function mergeRecentChat(p) {
    const recent = p.entries || [];
    const meta = { project: p.project, absent: p.absent, missing: p.missing, error: p.error };
    const prev = herdNow.chat;
    if (p.error) { herdNow.chat = Object.assign({ entries: [], more: false }, meta); return true; }
    if (!prev || !prev.paged) {
      herdNow.chat = Object.assign({ entries: recent.slice(), more: !!p.earlier, loading: false, paged: false }, meta);
      return true;
    }
    Object.assign(prev, meta);
    if (!recent.length) return false;
    const i = lastIndexOfSig(prev.entries, recent[0]);
    if (i < 0) { herdNow.chat = Object.assign({ entries: recent.slice(), more: !!p.earlier, loading: false, paged: false }, meta); return true; }
    if (!herdNow.away.chat && scrolledUp(app.querySelector(".chat .cbody"))) return false;
    prev.entries = trimHead(prev.entries.slice(0, i).concat(recent));
    return true;
  }

  function prependChat(p) {
    const chat = herdNow.chat;
    if (!chat) return;
    chat.loading = false;
    chat.paged = true;
    if (p.error) { chat.more = false; chat.error = p.error; return; }
    const page = p.entries || [];
    if (page.length) {
      const all = prependPage(page, chat.entries || []);
      chat.entries = trimTail(all);
      if (chat.v != null && all.length > chat.entries.length) chat.v = -1;
      chat.more = !!p.earlier;
      chat.limitReached = false;
    } else {
      chat.more = false;
      chat.limitReached = !!p.earlier;
    }
  }

  // Redraw one column's body, staying at the bottom if it was there.  After
  // a `load earlier', the turns prepended at the top would push what you
  // were reading down; keep it in place by moving the scroll down by the
  // height that was added, so the view holds its message instead of jumping.
  function renderRoomBody(a) {
    const col = roomColumn(a.name);
    const body = col && col.querySelector(".rbody");
    if (!body) return;
    if (herdNow.away[findKey(a.name)]) {
      renderAwayBody(findKey(a.name), body);
      return renderColJump(a.name);
    }
    const wasAway = body.hasAttribute("data-away");
    const first = !body.hasAttribute("data-drawn");
    const prevLen = (drawn.get(body) || []).length;
    const entries = (herdNow.convs[a.name] && herdNow.convs[a.name].entries) || [];
    const grewEarlier = herdNow.earlierPending.has(a.name) && entries.length > prevLen;
    const oldHeight = body.scrollHeight, keep = body.scrollTop;
    const atBottom = oldHeight - keep - body.clientHeight < 40;
    // Read up from its end: with a hand on it, the redraw waits; else the
    // entry at the top of the screen is put back where it was.
    const reading = !first && !atBottom && !wasAway;
    if (reading && handMoving()) return handHold("col:" + a.name, () => renderRoomBody(a));
    const anchor = reading ? anchorOf(body) : null;
    body.removeAttribute("data-away");
    body.innerHTML = roomBodyHtml(a);
    drawn.set(body, entries);
    body.setAttribute("data-drawn", "1");
    if (first || atBottom) body.scrollTop = body.scrollHeight;
    else if (placeAnchor(body, anchor, entries)) herdNow.earlierPending.delete(a.name);
    else if (grewEarlier) {
      body.scrollTop = keep + (body.scrollHeight - oldHeight);
      herdNow.earlierPending.delete(a.name);
    } else body.scrollTop = keep;
    markHit(findKey(a.name));
    renderColJump(a.name);
    const conv = herdNow.convs[a.name];
    if (herdNow.jumpPend === "col:" + a.name && conv && !conv.loading) {
      const list = youItems(entries), k = youAt(list, herdNow.jump.col[a.name]);
      if (k < list.length - 1) {
        herdNow.jumpPend = null;
        herdNow.jump.col[a.name] = list[k + 1].ts; renderColJump(a.name);
        flashMsg(body, '.ent.you[data-i="' + list[k + 1].i + '"]');
      } else if (conv.rest > 0) conv.loading = true;  // the rest, on its way, may hold it
      else {
        herdNow.jumpPend = null;
        noteJump("col:" + a.name, "Older questions are in the transcript, past what is loaded here.", () => renderColJump(a.name));
      }
    }
  }

  // A sub-agent's own transcript, over the room: its conversation and tool
  // calls, read the same way a column's is.  Open from a sub-agent card,
  // Close (or Esc) back to the room.
  function renderSubView() {
    const sv = herdNow.subView;
    let ov = app.querySelector(".subview");
    if (!sv || !herdNow.room) { if (ov) ov.remove(); return; }
    if (!ov) { ov = document.createElement("div"); ov.className = "subview"; app.appendChild(ov); }
    const conv = herdNow.subConv;
    const entries = (conv && conv.entries) || [];
    const body = !conv ? '<p class="rnote">Reading its transcript…</p>'
      : conv.error ? '<p class="rnote error">Could not read it: ' + esc(conv.error) + "</p>"
      : (conv.earlier ? '<p class="rnote">Earlier turns are in the transcript, not shown here.</p>' : "") +
        (entries.length ? entries.map((e, i) => entryHtml(e, i, sv.name, "claude", "sub:" + sv.name)).join("")
          : '<p class="rnote">Nothing said yet.</p>');
    const prev = ov.querySelector(".subbody");
    const atBottom = !prev || prev.scrollHeight - prev.scrollTop - prev.clientHeight < 40;
    const keep = prev ? prev.scrollTop : 0;
    ov.innerHTML = '<div class="subhead"><span class="subwho">' + esc(sv.who || "sub-agent") + "</span>" +
      (sv.title ? '<span class="subtitle" title="' + esc(sv.title) + '">' + esc(sv.title) + "</span>" : "") +
      '<span class="submeta">of ' + esc(sv.name) + "</span>" +
      '<button type="button" class="ghost small" data-act="a:sub-close">Close</button></div>' +
      '<div class="subbody">' + body + "</div>";
    const b = ov.querySelector(".subbody");
    drawn.set(b, entries);
    // Stay where the reader is: pinned to the foot only if they were there,
    // else hold the scroll instead of snapping to the top each poll.
    b.scrollTop = atBottom ? b.scrollHeight : keep;
  }

  // ---- The room as one chat ----------------------------------------------
  //
  // The same conversations at the level of who said what to whom: your
  // prompts, the asks and answers between agents, and how each turn of
  // each agent ended -- built by the sidecar from their transcripts.
  // What an agent did along the way stays in its column.  The box writes
  // to one agent, picked above it or by @name at its start.

  // Who the box can write to: the lead first -- or, with none yet, the
  // lead that writing to it starts -- then the others.
  function chatTargets(agents) {
    if (isPrimaryRoom()) {
      const p = agents.find((a) => a.role === "primary");
      return [p ? [p.name, "primary"] : ["@primary", "primary (starts when you write)"]];
    }
    const lead = agents.find((a) => a.role === "lead");
    return (lead ? [[lead.name, "lead · " + lead.name]] : [["@lead", "lead (starts when you write)"]])
      .concat(agents.filter((a) => a !== lead).map((a) => [a.name, a.name]));
  }

  function chatTo(agents) {
    const want = herdNow.chatTo[herdNow.room];
    const targets = chatTargets(agents);
    return targets.some(([v]) => v === want) ? want : targets[0][0];
  }

  function chatHtml(agents) {
    const to = chatTo(agents);
    return '<div class="chat"><div class="cjump"></div><div class="cbody"></div><div class="cmeter"></div>' +
      '<div class="cfoot" data-name="' + esc(to) + '"><div class="attach">' + attachHtml(to) + "</div>" +
      // Typing on a phone, who it goes to is a tag at the box's left, the
      // select over it unseen (`typing'); else the tag is not shown.
      '<span class="chat-to-w"><span class="ttag" aria-hidden="true">' + esc(chatTag(to, agents)) + "</span>" +
      '<select class="chat-to" title="Who it goes to (or start with @name, @lead)">' +
      chatTargets(agents).map(([v, label]) => '<option value="' + esc(v) + '"' + (v === to ? " selected" : "") + ">" + esc(label) + "</option>").join("") +
      '</select></span><textarea class="agent-text" rows="2" placeholder="' + esc(chatHint(to)) + '">' +
      esc(herdNow.compose[to] || "") + "</textarea>" + IMAGE_BUTTON + useButton(to) +
      '<button type="button" class="primary small" data-act="a:send">Send</button></div></div>';
  }

  // Who the box writes to, in a word, for the tag typing shows: the lead
  // and the primary by their role, any other by its name.
  function chatTag(to, agents) {
    if (to === "@lead" || to === "@primary") return to.slice(1);
    const a = agents.find((x) => x.name === to);
    return a && (a.role === "lead" || a.role === "primary") ? a.role : to;
  }

  // The box writes to TO now: what it held, words and images, goes along.
  function setChatTo(to) {
    const foot = app.querySelector(".chat .cfoot");
    if (!foot) return;
    const was = foot.getAttribute("data-name");
    herdNow.chatTo[herdNow.room] = to;
    if (was === to) return;
    const ta = foot.querySelector("textarea");
    herdNow.compose[to] = ta.value;
    delete herdNow.compose[was];
    herdNow.attach[to] = (herdNow.attach[to] || []).concat(herdNow.attach[was] || []);
    delete herdNow.attach[was];
    foot.setAttribute("data-name", to);
    foot.querySelector(".chat-to").value = to;
    foot.querySelector(".ttag").textContent = chatTag(to, roomAgents());
    const F = herdNow.find.chat;
    if (F && F.open && F.name !== chatFindAgent()) {
      if (herdNow.away.chat) backToLatest("chat");
      F.res = null;
      F.cur = -1;
      runFind("chat");
    }
    suggestIn(foot, to, chatHint(to));
    renderAttach(to);
    renderChatMeter();
  }

  // The chat's box writes to one agent at a time: its meter, above the box.
  function renderChatMeter() {
    const foot = app.querySelector(".chat .cfoot");
    const box = app.querySelector(".chat .cmeter");
    if (!foot || !box) return;
    const a = roomAgents().find((x) => x.name === foot.getAttribute("data-name"));
    box.setAttribute("data-name", a ? a.name : "");
    stripUnder(app.querySelector(".chat .cbody"), box, a ? meterHtml(a) + modelFormHtml(a) + subsHtml(a) : "");
  }

  // What the herd put before your words to a lead: a line each, not
  // your words.
  // How the taker's work went: the herd's counts beside an answer -- never
  // a verdict on it (ask_trace.py).  A rough one suggests remembering what
  // it found.  DETAIL, when given, is the counts behind the line.
  function traceHtml(line, rough, detail) {
    if (!line) return "";
    const isRough = rough || / · rough going: /.test(line);
    return '<div class="trace' + (isRough ? " rough" : "") + '">' + esc(line) + "</div>" +
      (detail ? '<div class="tracedetail">' + esc(traceDetailText(detail)) + "</div>" : "");
  }

  // A failure's kind as the line says it (ask_trace.LABELS).
  const TRACE_FAIL = { shell: "shell exit", edit: "edit not applied", denied: "permission denial",
                       timeout: "timeout", other: "other error" };
  function traceDetailText(d) {
    const by = (o, names) => Object.keys(o || {}).sort((a, b) => o[b] - o[a] || a.localeCompare(b))
      .map((k) => ((names && names[k]) || k) + " " + o[k]).join(", ");
    const parts = [];
    if (d.seen) {
      parts.push("tools: " + (by(d.tools) || "none"));
      if (d.failures_known) {
        parts.push("failed: " + (by(d.failed, TRACE_FAIL) || (d.unread ? "none seen" : "none")));
        if (d.unread) parts.push("not told how they ended: " + d.unread);
      } else parts.push("failures not readable for " + (d.cli || "this CLI"));
      if (d.subs) parts.push("sub-agents: " + d.subs + (d.subs_read ? " (" + d.sub_tools + " tool calls" +
        (by(d.sub_failed) ? "; failed: " + by(d.sub_failed, TRACE_FAIL) : "") + ")" : " (their own work not read)"));
    } else if (d.why) parts.push("its tool calls were not seen: " + d.why);
    if (d.guard) parts.push("guard denials: " + d.guard);
    if (d.lint) parts.push("check denials: " + d.lint);
    if (d.asks && Object.keys(d.asks).length) parts.push("asked: " + by(d.asks));
    if (d.relays) parts.push("relays: " + d.relays);
    return parts.join(" · ");
  }

  function herdNotes(e) {
    return (e.notes || []).map((n) => '<div class="hnote">herd note: ' + esc(n) + "</div>").join("");
  }

  const CHAT_LONG = 900;
  function chatItemHtml(e, i) {
    const you = e.who === "user";
    const color = you ? "" : sessionColor(e.who || "");
    const tag = e.kind === "ask" ? " · ask" + (e.ask ? " " + e.ask : "")
      : e.kind === "reply" ? " · answers " + (e.ask || "")
      : e.kind === "answer" ? (e.failed ? " · the ask failed" : e.auto ? " · no answer; the herd sent what it last showed" : " · answers " + (e.ask || ""))
      : "";
    const head = (you ? "You" : e.who || "?") + (e.to ? " → " + e.to : "") + tag;
    const key = [e.agent, e.ts, e.kind, e.ask, e.who].join("|");
    const long = (e.text || "").length > CHAT_LONG || (e.text || "").split("\n").length > 14;
    const open = herdNow.chatOpen.has(key);
    const body = e.kind === "turn" ? '<div class="ebody msg-body">' + markdown(e.text) + "</div>" : plainBody(e.text);
    // An ask its taker never saw carries what came of it.
    const outcome = e.kind !== "ask" || !(e.failed || e.answer || e.waiting) ? ""
      : e.failed ? '<div class="eanswer failed">' + entHead("the ask failed", "", null, "", copyButton(i, e.answer, "answer")) + plainBody(e.answer) + "</div>"
      : e.answer ? '<div class="eanswer">' + entHead("⇠ " + (e.to || "") + " answered", "", null, sessionColor(e.to || ""), copyButton(i, e.answer, "answer")) +
        plainBody(e.answer) + imagesHtml(e.answerImages) + traceHtml(e.trace) + "</div>"
      : '<div class="eanswer waiting">waiting for ' + esc(e.to || "the answer") + "…</div>";
    return '<div class="ent cmsg' + (you ? " you" : "") + (long && !open ? " folded" : "") + '" data-i="' + i + '"' +
      (color ? ' style="border-left-color:' + color + '"' : "") + ">" +
      entHead(head, you ? "" : "", e.ts, color, copyButton(i, e.text)) + herdNotes(e) + body + imagesHtml(e.images) +
      (e.kind === "answer" ? traceHtml(e.trace) : "") + outcome +
      (long ? '<button type="button" class="linkish cmore" data-act="a:chat-more" data-key="' + esc(key) + '">' + (open ? "less" : "more") + "</button>" : "") +
      "</div>";
  }

  // Redraw the timeline, staying at the bottom if it was there -- and after
  // a `load earlier', holding the message you were reading in place (the
  // prepended turns push it down by their height), not jumping.
  function renderChatBody() {
    const body = app.querySelector(".chat .cbody");
    if (!body) return;
    if (herdNow.away.chat) {
      renderAwayBody("chat", body);
      return renderChatJump();
    }
    const wasAway = body.hasAttribute("data-away");
    const c = herdNow.chat;
    const first = !body.hasAttribute("data-drawn");
    const prevLen = (drawn.get(body) || []).length;
    const entries = (c && c.entries) || [];
    const grewEarlier = herdNow.chatEarlierPending && entries.length > prevLen;
    const oldHeight = body.scrollHeight, keep = body.scrollTop;
    const atBottom = oldHeight - keep - body.clientHeight < 40;
    // As a column's (`renderRoomBody'): read up, held in place, or waiting
    // while a hand moves it.
    const reading = !first && !atBottom && !wasAway;
    if (reading && handMoving()) return handHold("chat", renderChatBody);
    const anchor = reading ? anchorOf(body) : null;
    body.removeAttribute("data-away");
    const out = (c && [...(c.absent || []), ...(c.missing || [])]) || [];
    body.innerHTML = isPrimaryRoom() && !roomAgents().length
      ? '<p class="rnote">Primary is not running.  Write to it below: it starts -- or resumes its last ' +
        "conversation -- and reads your message once it is up.</p>"
      : roomEmpty()
      ? '<p class="rnote">No agents here yet.  Write to its lead below: it starts, and reads your ' +
        "message once it is up.</p>"
      : !c ? '<p class="rnote">Reading the conversations…</p>'
      : c.error ? '<p class="rnote error">Could not read the conversations: ' + esc(c.error) + "</p>"
      : earlierHtml(c, "a:chat-earlier") +
        (entries.length ? entries.map(chatItemHtml).join("") : '<p class="rnote">Nobody has said anything yet.</p>') +
        (out.length ? '<p class="rnote">Not in the chat, no conversation known: ' + esc(out.join(", ")) + "</p>" : "") +
        roomAgents().map((a) => queuedHtml(a, queuedLeft(a.name, entries, saidToIn(a.name)), true)).join("");
    drawn.set(body, entries);
    body.setAttribute("data-drawn", "1");
    if (first || atBottom) body.scrollTop = body.scrollHeight;
    else if (placeAnchor(body, anchor, entries)) herdNow.chatEarlierPending = false;
    else if (grewEarlier) {
      body.scrollTop = keep + (body.scrollHeight - oldHeight);
      herdNow.chatEarlierPending = false;
    } else body.scrollTop = keep;
    markHit("chat");
    renderChatJump();
    // A `load earlier' fired to reach an older "You" has arrived: step to it.
    if (herdNow.jumpPend === "chat" && c && !c.loading) {
      const list = youItems(entries), k = youAt(list, herdNow.jump.chat);
      if (k < list.length - 1) {
        herdNow.jumpPend = null;
        herdNow.jump.chat = list[k + 1].ts;
        renderChatJump();
        flashMsg(body, '.cmsg[data-i="' + list[k + 1].i + '"]');
      } else if (c.rest > 0) c.loading = true;  // the rest, on its way, may hold it
      else {
        herdNow.jumpPend = null;
        noteJump("chat", "Older questions are in the transcript, past what is loaded here.", renderChatJump);
      }
    }
  }

  // ---- Jump to what you asked --------------------------------------------
  //
  // Coming back, the user forgets what they asked.  A sticky line shows their
  // latest message in the view (in the merged chat, to any agent); clicking
  // it jumps there and flashes it, and ↑/↓ step through earlier ones -- paging
  // back with the B(2) cursor when one is older than what is loaded, and
  // saying so once nothing older can be loaded.

  function youItems(entries) {
    const out = [];
    for (let i = (entries || []).length - 1; i >= 0; i--) {
      const e = entries[i];
      // Not a note: said by no one -- "Request interrupted by user", a
      // background command that ended, a compaction.
      if (e && (e.who === "user" || e.role === "user") && e.kind !== "note")
        out.push({ ts: e.ts, i, text: (e.text || "").replace(/\s+/g, " ").trim() });
    }
    return out; // newest first
  }

  function youAt(list, ts) {
    if (!ts) return 0;
    const k = list.findIndex((x) => x.ts === ts);
    return k < 0 ? 0 : k;
  }

  // EXTRA goes at the strip's end: its find button, there with nothing
  // of yours to jump to too.
  function jumpBarHtml(list, curTs, conv, note, extra) {
    extra = extra || "";
    if (note) return '<div class="jump"><span class="jnote">' + esc(note) + "</span>" + extra + "</div>";
    if (!list.length) return extra ? '<div class="jump"><span class="jfill"></span>' + extra + "</div>" : "";
    const k = youAt(list, curTs), cur = list[k];
    const older = k < list.length - 1 || !!(conv && conv.more);
    // Of yours among what is loaded; while the rest of the conversation
    // is on its way, among what has come so far -- "…" says so.
    const part = !!(conv && conv.rest > 0);
    return '<div class="jump">' +
      '<button type="button" class="jline" data-act="a:jump-go" title="Jump to this message">' +
      '<span class="jwho">You:</span> <span class="jtext">' + esc(cur.text || "(no text)") + "</span></button>" +
      (list.length > 1 || part ? '<span class="jpos"' +
        (part ? ' title="Your messages among the turns come so far: the rest of the conversation is on its way"' : "") + ">" +
        (list.length - k) + "/" + list.length + (part ? "…" : "") + "</span>" : "") +
      '<button type="button" class="jstep" data-act="a:jump-prev"' + (older ? "" : " disabled") + ' title="Earlier question">↑</button>' +
      '<button type="button" class="jstep" data-act="a:jump-next"' + (k > 0 ? "" : " disabled") + ' title="Later question">↓</button>' +
      extra + "</div>";
  }

  function renderChatJump() {
    const bar = app.querySelector(".chat .cjump");
    if (!bar) return;
    if (findOpen("chat")) return renderFindBar("chat", bar);
    stripOver(app.querySelector(".chat .cbody"), bar, jumpBarHtml(youItems((herdNow.chat && herdNow.chat.entries) || []),
      herdNow.jump.chat, herdNow.chat, herdNow.jumpNote.chat,
      herdNow.chat && !herdNow.chat.error && chatFindAgent() ? findButton("chat") : ""));
  }

  // The strip over a conversation, BAR, made HTML: one that comes, or
  // grows, takes its height from BODY below it -- which, at its end,
  // stays there, as under a strip below it (stripUnder).
  function stripOver(body, bar, html) {
    const atEnd = body && body.hasAttribute("data-drawn") && body.scrollHeight - body.scrollTop - body.clientHeight < 40;
    bar.innerHTML = html;
    if (atEnd) body.scrollTop = body.scrollHeight;
  }

  function flashMsg(body, sel) {
    const el = body && body.querySelector(sel);
    if (!el) return false;
    el.scrollIntoView({ block: "center" });
    el.classList.remove("flash"); void el.offsetWidth; el.classList.add("flash");
    return true;
  }

  function noteJump(key, msg, renderFn) {
    herdNow.jumpNote[key] = msg;
    renderFn();
    setTimeout(() => { if (herdNow.jumpNote[key] === msg) { herdNow.jumpNote[key] = ""; renderFn(); } }, 2800);
  }

  // The same, per column: its own conversation, body, bar, cursor and the
  // room-earlier cursor for paging back.
  function renderColJump(name) {
    const col = roomColumn(name), bar = col && col.querySelector(".rjump");
    if (!bar) return;
    if (findOpen(findKey(name))) return renderFindBar(findKey(name), bar);
    const conv = herdNow.convs[name];
    stripOver(col.querySelector(".rbody"), bar, jumpBarHtml(youItems((conv && conv.entries) || []),
      herdNow.jump.col[name], conv, herdNow.jumpNote["col:" + name],
      conv && !conv.none && !conv.error ? findButton(findKey(name)) : ""));
  }

  function colJump(name, dir) {
    const conv = herdNow.convs[name], entries = (conv && conv.entries) || [];
    const list = youItems(entries);
    if (!list.length) return;
    const col = roomColumn(name), body = col && col.querySelector(".rbody");
    const k = youAt(list, herdNow.jump.col[name]);
    if (dir === 0) return void flashMsg(body, '.ent.you[data-i="' + list[k].i + '"]');
    if (dir > 0) {               // ↑ earlier
      if (k < list.length - 1) {
        herdNow.jump.col[name] = list[k + 1].ts; renderColJump(name);
        flashMsg(body, '.ent.you[data-i="' + list[k + 1].i + '"]');
      } else if (conv && conv.more && !conv.loading) {
        const a = roomAgents().find((x) => x.name === name);
        if (!a) return;
        herdNow.jumpPend = "col:" + name; conv.loading = true; conv.paged = true; herdNow.earlierPending.add(name);
        // The rest of it, on its way, is the page before.
        if (!(conv.rest > 0)) emit("room-earlier", { project: herdNow.room, name: name, before: entries.length });
        renderRoomBody(a);
      } else noteJump("col:" + name, "Older questions are in the transcript, past what is loaded here.", () => renderColJump(name));
    } else if (k > 0) {          // ↓ later
      herdNow.jump.col[name] = list[k - 1].ts; renderColJump(name);
      flashMsg(body, '.ent.you[data-i="' + list[k - 1].i + '"]');
    }
  }

  function chatJump(dir) {
    const chat = herdNow.chat, entries = (chat && chat.entries) || [];
    const list = youItems(entries);
    if (!list.length) return;
    const body = app.querySelector(".chat .cbody");
    const k = youAt(list, herdNow.jump.chat);
    if (dir === 0) return void flashMsg(body, '.cmsg[data-i="' + list[k].i + '"]');
    if (dir > 0) {               // ↑ earlier
      if (k < list.length - 1) {
        herdNow.jump.chat = list[k + 1].ts; renderChatJump();
        flashMsg(body, '.cmsg[data-i="' + list[k + 1].i + '"]');
      } else if (chat && chat.more && !chat.loading) {
        herdNow.jumpPend = "chat"; chat.loading = true; chat.paged = true; herdNow.chatEarlierPending = true;
        if (!(chat.rest > 0)) emit("chat-earlier", { project: herdNow.room, before: entries.length });
        renderChatBody();
      } else noteJump("chat", "Older questions are in the transcript, past what is loaded here.", renderChatJump);
    } else if (k > 0) {          // ↓ later
      herdNow.jump.chat = list[k - 1].ts; renderChatJump();
      flashMsg(body, '.cmsg[data-i="' + list[k - 1].i + '"]');
    }
  }

  // ---- Find in a conversation --------------------------------------------
  //
  // The strip's magnifier turns it into a find bar: the words, how many
  // entries hold them all ("3/17"), ↑/↓ through them, ✕.  The sidecar looks
  // through the agent's WHOLE conversation (room-find, herd_conversation_find),
  // not just the tail the room holds, in what the room shows as said -- not
  // tool calls.  The count opens a list of the hits, a snippet each with who
  // and when: the quick look.  Going to a hit scrolls to it, flashes it and
  // marks its words, unfolded, when the body holds it; else the page of the
  // conversation holding it is read (room-at) and shown in the body instead
  // of the live tail, until "Back to latest" (or ✕) -- the live tail goes on
  // being merged meanwhile, unseen, so nothing moves the page being read.
  // A column finds in its agent; the chat in the agent its box writes to.

  const FIND_PAGE = 60;     // entries of a page read at a hit
  const FIND_BEFORE = 20;   // of them before it
  const FIND_WAIT = 280;    // ms after the last key before the words go
  const FIND_ICON = '<svg viewBox="0 0 16 16" width="13" height="13" aria-hidden="true"><circle cx="6.8" cy="6.8" r="4.6" ' +
    'fill="none" stroke="currentColor" stroke-width="1.7"/><path d="M10.3 10.3 14.2 14.2" stroke="currentColor" ' +
    'stroke-width="1.8" stroke-linecap="round"/></svg>';

  function findKey(name) { return name ? "col:" + name : "chat"; }

  // Whom the chat's find looks in: the agent its box writes to, else its first.
  function chatFindAgent() {
    const agents = roomAgents();
    const to = agents.length ? chatTo(agents) : "";
    return agents.some((a) => a.name === to) ? to : (agents[0] && agents[0].name) || "";
  }

  function findAgentOf(key) { return key === "chat" ? chatFindAgent() : key.slice(4); }
  function findOpen(key) { const F = herdNow.find[key]; return !!(F && F.open); }

  function findStrip(key) {
    if (key === "chat") return app.querySelector(".chat .cjump");
    const col = roomColumn(key.slice(4));
    return col && col.querySelector(".rjump");
  }

  function findBody(key) {
    if (key === "chat") return app.querySelector(".chat .cbody");
    const col = roomColumn(key.slice(4));
    return col && col.querySelector(".rbody");
  }

  function renderStripOf(key) {
    if (key === "chat") renderChatJump();
    else renderColJump(key.slice(4));
  }

  function renderBodyOf(key) {
    if (key === "chat") return renderChatBody();
    const a = roomAgents().find((x) => x.name === key.slice(4));
    if (a) renderRoomBody(a);
  }

  function findButton(key) {
    return '<button type="button" class="jstep jfind" data-act="a:find-open" data-fkey="' + esc(key) + '" ' +
      'title="Find in this conversation, all of it (/)" aria-label="Find in this conversation">' + FIND_ICON + "</button>";
  }

  // Where / (and Emacs's M-x ghostherd-page-find) opens it: the chat's, or
  // the column last pointed at -- else the first, the lead's.
  function defaultFindKey() {
    if (!herdNow.room) return null;
    if (roomIsChat()) return chatFindAgent() ? "chat" : null;
    const agents = roomAgents();
    const name = agents.some((a) => a.name === herdNow.lastCol) ? herdNow.lastCol : agents[0] && agents[0].name;
    return name ? findKey(name) : null;
  }

  function openFind(key) {
    if (!key) return;
    let F = herdNow.find[key];
    if (!F) F = herdNow.find[key] = { q: "", name: findAgentOf(key), res: null, busy: false, cur: -1, list: false, seq: 0 };
    F.open = true;
    herdNow.findLast = key;
    renderStripOf(key);
    const input = findInput(key);
    if (input) { input.focus(); input.select(); }
    // Opened again on the words it had: found again, the conversation
    // having gone on since.
    if (F.q.trim() && F.res) { F.res = null; F.cur = -1; runFind(key); }
  }

  function findInput(key) {
    const bar = findStrip(key);
    return bar && bar.querySelector(".fq");
  }

  function closeFind(key) {
    const F = herdNow.find[key];
    if (!F || !F.open) return;
    F.open = false;
    F.list = false;
    F.pend = null;
    clearTimeout(F.timer);
    F.timer = null;
    if (herdNow.away[key]) backToLatest(key);
    else {
      renderStripOf(key);
      clearMarks(findBody(key));
    }
  }

  // Esc anywhere in the room: the open list, then the open find bar.
  function findEscape() {
    const keys = Object.keys(herdNow.find).filter(findOpen);
    if (!keys.length) return false;
    const listed = keys.find((k) => herdNow.find[k].list);
    if (listed) { herdNow.find[listed].list = false; renderStripOf(listed); return true; }
    closeFind(keys.includes(herdNow.findLast) ? herdNow.findLast : keys[0]);
    return true;
  }

  // The bar, drawn once and then only its count, buttons and list redrawn:
  // its box keeps the words being typed, an IME's composition too.
  function renderFindBar(key, bar) {
    const F = herdNow.find[key];
    let fb = bar.querySelector(".jump.find");
    if (!fb || fb.getAttribute("data-fkey") !== key) {
      bar.innerHTML = '<div class="jump find" data-fkey="' + esc(key) + '">' +
        '<input class="fq" type="search" enterkeyhint="search" autocomplete="off" autocapitalize="off" autocorrect="off" spellcheck="false">' +
        '<button type="button" class="fcount" data-act="a:find-list" data-fkey="' + esc(key) + '" aria-haspopup="true"></button>' +
        '<button type="button" class="jstep" data-act="a:find-prev" data-fkey="' + esc(key) + '" title="Earlier hit (Enter)">↑</button>' +
        '<button type="button" class="jstep" data-act="a:find-next" data-fkey="' + esc(key) + '" title="Later hit (Shift+Enter)">↓</button>' +
        '<button type="button" class="jstep fclose" data-act="a:find-close" data-fkey="' + esc(key) + '" title="Close (Esc)">✕</button>' +
        '</div><div class="fnote" hidden></div><div class="flist" hidden></div>';
      fb = bar.querySelector(".jump.find");
      const input = fb.querySelector(".fq");
      input.value = F.q;
      wireFindInput(key, input);
    }
    const input = fb.querySelector(".fq");
    const who = F.name || findAgentOf(key);
    input.placeholder = who ? "Find in " + who + "’s conversation" : "Find in this conversation";
    const st = findStatus(F);
    const count = fb.querySelector(".fcount");
    count.textContent = st.text;
    count.title = st.title || "";
    count.className = "fcount" + (st.none ? " none" : "") + (st.busy ? " busy" : "");
    count.disabled = !F.res;
    count.setAttribute("aria-expanded", F.list ? "true" : "false");
    const hits = (F.res && F.res.hits) || [];
    fb.querySelector('[data-act="a:find-prev"]').disabled = !hits.length || (F.cur === 0 && !F.res.capped);
    fb.querySelector('[data-act="a:find-next"]').disabled = !hits.length || F.cur === hits.length - 1;
    const note = bar.querySelector(".fnote");
    note.hidden = !F.note;
    note.textContent = F.note || "";
    const list = bar.querySelector(".flist");
    list.hidden = !(F.list && F.res);
    if (!list.hidden) {
      list.innerHTML = findListHtml(key, F);
      const on = list.querySelector(".fhit.on");
      if (on) on.scrollIntoView({ block: "nearest" });
    } else list.innerHTML = "";
  }

  function findStatus(F) {
    const q = F.q.trim();
    if (!termRe(q)) return { text: "" };
    if (F.timer || F.busy || !F.res || F.res.q !== q) return { text: "…", busy: true };
    if (F.res.error) return { text: "!", title: "Could not find: " + F.res.error, none: true };
    const N = F.res.total, n = F.res.hits.length;
    if (!N) return { text: "0", none: true, title: "Not said in " + F.res.name + "’s conversation" };
    const all = N + (N === 1 ? " entry says it" : " entries say it") + " -- the list";
    if (F.cur < 0) return { text: String(N), title: all };
    return { text: (N - n + F.cur + 1) + "/" + N, title: all };
  }

  function wireFindInput(key, input) {
    const F = () => herdNow.find[key];
    input.addEventListener("compositionstart", () => { if (F()) F().composing = true; });
    input.addEventListener("compositionend", () => {
      if (!F()) return;
      F().composing = false;
      findSoon(key, input.value);
    });
    input.addEventListener("input", (ev) => {
      if (!F() || ev.isComposing || F().composing) return;
      findSoon(key, input.value);
    });
    input.addEventListener("keydown", (ev) => {
      // A key that ends an IME's composition is the IME's, not ours.
      if (ev.isComposing || ev.keyCode === 229 || !F()) return;
      if (ev.key === "Enter") findStep(key, ev.shiftKey ? -1 : 1);
      else if (ev.key === "ArrowUp" || ev.key === "ArrowDown") findStep(key, ev.key === "ArrowUp" ? 1 : -1);
      else if (ev.key === "Escape") findEscape();
      else return;
      ev.preventDefault();
      ev.stopPropagation();
    });
  }

  function findSoon(key, q) {
    const F = herdNow.find[key];
    if (!F) return;
    F.q = q;
    clearTimeout(F.timer);
    F.timer = setTimeout(() => runFind(key), FIND_WAIT);
    renderStripOf(key);
  }

  // Send the words, unless they are what was last answered; THEN, when
  // given, once the answer is in.
  function runFind(key, then) {
    const F = herdNow.find[key];
    if (!F) return;
    clearTimeout(F.timer);
    F.timer = null;
    const q = F.q.trim();
    const name = findAgentOf(key);
    if (!termRe(q) || !name) {
      F.res = null; F.cur = -1; F.list = false; F.busy = false; F.seq = ++herdNow.findSeq;
      return renderStripOf(key);
    }
    if (F.res && F.res.q === q && F.res.name === name && !F.busy) return then ? then() : renderStripOf(key);
    F.name = name;
    F.asked = q;
    F.seq = ++herdNow.findSeq;
    F.busy = true;
    F.then = then || null;
    emit("room-find", { project: herdNow.room, name, q, seq: F.seq });
    renderStripOf(key);
  }

  function noteFind(key, msg) {
    const F = herdNow.find[key];
    if (!F) return;
    F.note = msg;
    renderStripOf(key);
    setTimeout(() => { if (F.note === msg) { F.note = ""; renderStripOf(key); } }, 2800);
  }

  // DIR 1 is an earlier hit (↑, Enter), -1 a later one; from none gone to
  // yet, either is the newest.
  function findStep(key, dir) {
    const F = herdNow.find[key];
    if (!F) return;
    const q = F.q.trim();
    if (F.timer || F.busy || !F.res || F.res.q !== q || F.res.name !== findAgentOf(key)) {
      if (!termRe(q)) return;
      const again = () => findStep(key, dir);
      if (F.busy && !F.timer && F.asked === q) { F.then = again; return; }
      return runFind(key, again);
    }
    const hits = F.res.hits;
    if (F.res.error) return noteFind(key, "Could not find: " + F.res.error);
    if (!hits.length) return noteFind(key, "Not said in " + F.res.name + "’s conversation");
    const k = F.cur < 0 ? hits.length - 1 : F.cur - dir;
    if (k < 0) return noteFind(key, F.res.capped ? "Earlier hits are past the newest " + hits.length + ": add a word" : "No earlier hit");
    if (k >= hits.length) return noteFind(key, "No later hit");
    findGo(key, k);
  }

  // Who said hit H, as its entry's head says it in NAME's column.
  function hitWho(h, name) {
    if (h.role === "assistant") return name;
    if (h.role === "sub") return "↩ " + (h.who || "sub-agent");
    if (h.role === "herd") return h.kind === "reply" ? name + " ⇢ answers " + (h.ask || "")
      : h.part === "answer" ? "⇠ " + (h.to || "") + " answered" : name + " ⇢ " + (h.to || "?");
    if (h.kind === "ask") return "⇠ " + (h.who || "") + " asks";
    if (h.kind === "answer") return "⇠ " + (h.who || "") + " answered";
    if (h.kind === "message" && h.who) return "⇠ " + h.who;
    return "You";
  }

  function hitColor(h, name) {
    if (h.role === "assistant" || (h.role === "herd" && h.part !== "answer") || h.role === "sub") return sessionColor(name);
    if (h.role === "herd") return sessionColor(h.to || "");
    return h.who ? sessionColor(h.who) : "";
  }

  function findListHtml(key, F) {
    const res = F.res;
    const re = termRe(res.q);
    const hits = res.hits, N = res.total, first = N - hits.length;
    const all = '<div class="ffoot"><button type="button" class="linkish" data-act="a:find-all" data-fkey="' + esc(key) + '">' +
      "Search all sessions for “" + esc(res.q) + "” →</button></div>";
    if (res.error) return '<div class="fsum error">Could not find: ' + esc(res.error) + "</div>" + all;
    if (!N) return '<div class="fsum">Not said in ' + esc(res.name) + "’s conversation.</div>" + all;
    const rows = [];
    for (let k = hits.length - 1; k >= 0; k--) {
      const h = hits[k];
      const color = hitColor(h, res.name);
      rows.push('<button type="button" class="fhit' + (k === F.cur ? " on" : "") + '" data-act="a:find-go" data-fkey="' + esc(key) + '" data-k="' + k + '">' +
        '<span class="fhead"><span class="fwho"' + (color ? ' style="color:' + color + '"' : "") + ">" + esc(hitWho(h, res.name)) + "</span>" +
        (h.ts ? '<span class="fwhen" title="' + esc(fullTime(h.ts)) + '">' + esc(when(h.ts)) + "</span>" : "") +
        '<span class="fnum">' + (first + k + 1) + "</span></span>" +
        '<span class="fsnip">' + hl(h.snippet || "", re) + "</span></button>");
    }
    return '<div class="fsum">' + N + (N === 1 ? " entry says it" : " entries say it") + " in " + esc(res.name) + "’s conversation" +
      (res.capped ? " · the newest " + hits.length + " here" : "") + "</div>" + rows.join("") + all;
  }

  // Where hit H is among what KEY's live view holds: its index there, or -1.
  // The page of the tail and the find were read apart, so by what the entry
  // is -- when, whose, its start -- the nearest its place when several.
  function heldIndex(key, h) {
    const F = herdNow.find[key];
    if (key === "chat") {
      const items = (herdNow.chat && herdNow.chat.entries) || [];
      for (let j = items.length - 1; j >= 0; j--) {
        const e = items[j];
        if (e.agent === F.res.name && (e.ts || null) === (h.ts || null) && String(e.text || "").startsWith(h.head || "")) return j;
      }
      return -1;
    }
    const held = (herdNow.convs[key.slice(4)] || {}).entries || [];
    const fromEnd = (F.res.entries || 0) - 1 - h.i;
    let best = -1;
    held.forEach((e, j) => {
      if ((e.ts || null) !== (h.ts || null) || (e.role || "") !== (h.role || "") || (e.kind || "") !== (h.kind || "")) return;
      if (!String(e.text || "").startsWith(h.head || "")) return;
      if (best < 0 || Math.abs(held.length - 1 - j - fromEnd) < Math.abs(held.length - 1 - best - fromEnd)) best = j;
    });
    return best;
  }

  // The selector of hit H in KEY's body as it is drawn now, or null.
  function hitSel(key, h) {
    const A = herdNow.away[key];
    if (A) return h.i >= A.start && h.i < A.start + A.entries.length ? '.ent[data-i="' + (h.i - A.start) + '"]' : null;
    const j = heldIndex(key, h);
    return j < 0 ? null : (key === "chat" ? ".cmsg" : ".ent") + '[data-i="' + j + '"]';
  }

  function findGo(key, k) {
    const F = herdNow.find[key];
    const h = F.res.hits[k];
    if (!h) return;
    F.cur = k;
    F.list = false;
    renderStripOf(key);
    const body = findBody(key);
    const A = herdNow.away[key];
    const sel = (!A || A.name === F.res.name) ? hitSel(key, h) : null;
    if (sel && showHit(body, sel, termRe(F.res.q))) return;
    loadAt(key, F.res.name, Math.max(0, h.i - FIND_BEFORE), "jump", h.i);
  }

  // Read the page of NAME's whole conversation from entry START: HOW is
  // "jump" (to entry FOCUS), "earlier" or "later" (beside the page shown).
  function loadAt(key, name, start, how, focus) {
    const F = herdNow.find[key];
    if (!F) return;
    F.pend = { seq: ++herdNow.findSeq, how, focus, name };
    const A = herdNow.away[key];
    if (A && how !== "jump") { A.loading = how; renderBodyOf(key); }
    emit("room-at", { project: herdNow.room, name, start, limit: FIND_PAGE, seq: F.pend.seq });
  }

  function awayStep(key, how) {
    const A = herdNow.away[key];
    if (!A || A.loading || herdNow.find[key] && herdNow.find[key].pend) return;
    if (how === "earlier" && A.earlier) loadAt(key, A.name, Math.max(0, A.start - FIND_PAGE), how);
    else if (how === "later" && A.later) loadAt(key, A.name, A.start + A.entries.length, how);
  }

  // A page read at a hit, or beside one: shown, or added to what is shown.
  function tookAt(p) {
    const key = Object.keys(herdNow.find).find((k) => herdNow.find[k].pend && herdNow.find[k].pend.seq === p.seq);
    if (!key) return;
    const F = herdNow.find[key], pend = F.pend;
    F.pend = null;
    let A = herdNow.away[key];
    if (A) A.loading = "";
    if (p.error) {
      noteFind(key, "Could not read it: " + p.error);
      if (A) renderBodyOf(key);
      return;
    }
    const page = p.entries || [];
    const start = p.start || 0;
    if (pend.how === "jump" || !A || A.name !== pend.name) {
      if (!page.length) return noteFind(key, "That part of the conversation is gone from its transcript");
      A = herdNow.away[key] = { name: pend.name, entries: page, start, scroll: "focus", focus: pend.focus };
      F.open = true;
    } else if (pend.how === "earlier") {
      // Places from the start do not move as the conversation grows: what
      // overlaps what is shown is known exactly.
      A.entries = page.slice(0, Math.max(0, Math.min(page.length, A.start - start))).concat(A.entries);
      A.start = Math.min(A.start, start);
      if (A.entries.length > MAX_HELD) A.entries = A.entries.slice(0, MAX_HELD);
      A.scroll = "earlier";
    } else {
      const end = A.start + A.entries.length;
      A.entries = A.entries.concat(page.slice(Math.max(0, end - start)));
      if (A.entries.length > MAX_HELD) {
        const drop = A.entries.length - MAX_HELD;
        A.entries = A.entries.slice(drop);
        A.start += drop;
      }
    }
    A.total = p.total || 0;
    A.earlier = A.start > 0;
    A.later = A.start + A.entries.length < A.total;
    renderBodyOf(key);
  }

  function backToLatest(key) {
    if (!herdNow.away[key]) return;
    delete herdNow.away[key];
    const F = herdNow.find[key];
    if (F) F.pend = null;
    const body = findBody(key);
    if (body) body.removeAttribute("data-drawn");  // drawn anew, at its foot
    renderBodyOf(key);
    renderStripOf(key);
  }

  function awayHtml(key, A, kind) {
    const fk = ' data-fkey="' + esc(key) + '"';
    const range = A.entries.length ? (A.start + 1).toLocaleString() + "–" + (A.start + A.entries.length).toLocaleString() +
      " of " + (A.total || 0).toLocaleString() : "";
    const top = A.loading === "earlier" ? '<div class="loadmore"><button type="button" class="ghost small" disabled>Loading earlier…</button></div>'
      : A.earlier ? '<div class="loadmore"><button type="button" class="ghost small" data-act="a:away-earlier"' + fk + ">Load earlier</button></div>"
      : '<p class="rnote">Start of the conversation.</p>';
    const foot = A.loading === "later" ? '<div class="loadmore"><button type="button" class="ghost small" disabled>Loading later…</button></div>'
      : A.later ? '<div class="loadmore"><button type="button" class="ghost small" data-act="a:away-later"' + fk + ">Load later</button></div>"
      : "";
    return '<div class="away"><p class="rnote awayhead">Earlier in ' + esc(A.name) + "’s conversation · entries " + range + "</p>" + top +
      A.entries.map((e, j) => entryHtml(e, j, A.name, kind, "away:" + key)).join("") + foot +
      '<div class="awayback"><button type="button" class="primary small" data-act="a:away-back"' + fk + ">↓ Back to latest</button></div></div>";
  }

  // KEY's body showing its page away from the tail: where the reader was,
  // or at the hit just gone to, or held across a page added above.
  function renderAwayBody(key, body) {
    const A = herdNow.away[key];
    const a = roomAgents().find((x) => x.name === A.name);
    const oldHeight = body.scrollHeight, keep = body.scrollTop;
    body.innerHTML = awayHtml(key, A, (a && a.kind) || "");
    drawn.set(body, A.entries);
    body.setAttribute("data-away", "1");
    const how = A.scroll;
    A.scroll = null;
    const F = herdNow.find[key];
    if (how === "focus" && showHit(body, '.ent[data-i="' + (A.focus - A.start) + '"]', F && F.res ? termRe(F.res.q) : null)) return;
    body.scrollTop = how === "earlier" ? keep + (body.scrollHeight - oldHeight) : keep;
    markHit(key);
  }

  // Unfold EL if it is a long message folded, as its "more" would.
  function unfold(el) {
    if (!el.classList.contains("folded")) return;
    const more = el.querySelector('[data-act="a:chat-more"], [data-act="a:sub-more"]');
    if (more) {
      (more.getAttribute("data-act") === "a:chat-more" ? herdNow.chatOpen : herdNow.subOpen).add(more.getAttribute("data-key"));
      more.textContent = "less";
    }
    el.classList.remove("folded");
  }

  function clearMarks(body) {
    if (!body) return;
    body.querySelectorAll("mark.fm").forEach((m) => {
      const parent = m.parentNode;
      m.replaceWith(document.createTextNode(m.textContent));
      if (parent) parent.normalize();
    });
  }

  function markIn(el, re) {
    if (!re) return;
    el.querySelectorAll(".ebody").forEach((b) => markTerms(b, re));
    el.querySelectorAll("mark:not(.fm)").forEach((m) => m.classList.add("fm"));
  }

  // Hit EL (by SEL in BODY): unfolded, its words marked, the first in sight,
  // flashed.  False when the body does not hold it.
  function showHit(body, sel, re) {
    const el = body && body.querySelector(sel);
    if (!el) return false;
    clearMarks(body);
    unfold(el);
    markIn(el, re);
    (el.querySelector("mark.fm") || el).scrollIntoView({ block: "center" });
    el.classList.remove("flash");
    void el.offsetWidth;
    el.classList.add("flash");
    return true;
  }

  // A body drawn again keeps the hit gone to marked (not moved to).
  function markHit(key) {
    const F = herdNow.find[key];
    if (!F || !F.open || !F.res || F.cur < 0) return;
    const h = F.res.hits[F.cur];
    const body = findBody(key);
    const sel = h && body && hitSel(key, h);
    const el = sel && body.querySelector(sel);
    if (el) markIn(el, termRe(F.res.q));
  }

  // The same words in the Search tab, over every session imported.
  function findAll(key) {
    const F = herdNow.find[key];
    const q = (F && F.q.trim()) || "";
    closeRoomState();
    query = q;
    hits = null;
    showSearch();
    if (q) search(true);
  }

  function onFindAction(act, el) {
    const key = el.getAttribute("data-fkey") || (el.closest("[data-fkey]") || { getAttribute: () => "" }).getAttribute("data-fkey");
    if (!key) return;
    const F = herdNow.find[key];
    if (act === "find-open") return openFind(key);
    if (act === "away-back") return backToLatest(key);
    if (act === "away-earlier") return awayStep(key, "earlier");
    if (act === "away-later") return awayStep(key, "later");
    if (!F) return;
    if (act === "find-close") return closeFind(key);
    if (act === "find-prev") return findStep(key, 1);
    if (act === "find-next") return findStep(key, -1);
    if (act === "find-all") return findAll(key);
    if (act === "find-go") return findGo(key, +el.getAttribute("data-k"));
    if (act === "find-list") {
      F.list = !F.list && !!F.res;
      return renderStripOf(key);
    }
  }

  function renderRoom() {
    const project = herdNow.room;
    const agents = roomAgents();
    const asks = (herdNow.data && herdNow.data.asks) || [];
    const counts = {};
    agents.forEach((a) => (counts[a.state] = (counts[a.state] || 0) + 1));
    const rank = (s) => (s in RANK ? RANK[s] : 9);
    const summary = Object.keys(counts).sort((a, b) => rank(a) - rank(b))
      .map((s) => '<span class="st st-' + esc(s) + '">' + counts[s] + " " + esc(s) + "</span>").join("");
    const primary = isPrimaryRoom();
    const chat = roomIsChat();
    // An empty room's chat turns to columns as its lead comes: Emacs is
    // told, so it reads what the view shows.
    if (chat !== herdNow.watchedChat) watchRoom(false);
    const key = project + "\n" + (primary ? "primary" : chat ? "chat" : "columns") + "\n" + agents.map((a) => a.name).join("\n");
    let room = app.querySelector(".room");
    if (!room || room.getAttribute("data-key") !== key) {
      const typing = document.activeElement && document.activeElement.closest && document.activeElement.closest(".jump.find");
      const typingKey = typing && typing.getAttribute("data-fkey");
      const views = [["columns", "Columns"], ["chat", "Chat"]].map(([v, label]) =>
        '<button type="button" class="ghost small' + (herdNow.roomView === v ? " on" : "") + '" data-act="a:room-view" data-view="' + v + '">' +
        label + "</button>").join("");
      app.innerHTML = '<div class="room' + (primary ? " proom" : "") + '"><div class="rhead">' +
        (herdNow.fromBoard ? '<button type="button" class="ghost small rback" data-act="bd:back" title="回到 Board，原來的位置">← 回 Board</button>' : "") +
        '<span class="pname" title="' + esc(project) + '">' +
        (primary ? "Primary" : esc(leaf(project))) + "</span>" +
        '<span class="ppath">' + (primary ? "the herd-wide lead · routes work across projects" : esc(project)) +
        '</span><span class="rsum"></span><span class="rview">' + (primary ? "" : views) +
        '<button type="button" class="ghost small" data-act="room-memory" title="This project\'s shared memory">Memory</button>' +
        '</span><div class="rwork"></div>' +
        '<div class="rruns"></div></div>' +
        (chat ? chatHtml(agents)
          : '<div class="rcols">' + agents.map((a) =>
              '<section class="rcol" data-name="' + esc(a.name) + '"><div class="rtop"></div><div class="rjump"></div><div class="rbody"></div><div class="rsubs"></div>' +
              '<div class="rfoot"><div class="attach">' + attachHtml(a.name) + '</div>' +
              '<textarea class="agent-text" rows="2" placeholder="' + esc(promptHint(a.name)) + '">' +
              esc(herdNow.compose[a.name] || "") + '</textarea>' + IMAGE_BUTTON + useButton(a.name) +
              '<button type="button" class="primary small" data-act="a:send">Send</button></div></section>').join("") + "</div>") +
        "</div>";
      room = app.querySelector(".room");
      room.setAttribute("data-key", key);
      if (chat) renderChatBody();
      else agents.forEach(renderRoomBody);
      if (colsResize && !chat) {
        colsResize.disconnect();
        room.querySelectorAll(".rcol").forEach((c) => colsResize.observe(c));
      }
      if (typingKey) setTimeout(() => { const i = findInput(typingKey); if (i) i.focus(); }, 0);
    }
    room.querySelector(".rsum").innerHTML = summary;
    const rwork = room.querySelector(".rwork");
    if (rwork) rwork.innerHTML = workHtml(workFor(project));
    room.querySelector(".rruns").innerHTML = runsHtml(agents, asks);
    renderSubView();
    if (chat) {
      const foot = room.querySelector(".chat .cfoot");
      const cbody = room.querySelector(".chat .cbody");
      const qk = queuedKey(agents.map((a) => a.name));
      if (cbody && (cbody.getAttribute("data-queued") || "") !== qk) {
        cbody.setAttribute("data-queued", qk);
        if (cbody.hasAttribute("data-drawn")) renderChatBody();
      }
      if (foot) suggestIn(foot, foot.getAttribute("data-name"), chatHint(foot.getAttribute("data-name")));
      renderChatMeter();
      bdLand(true);
      return;
    }
    agents.forEach((a) => {
      const col = roomColumn(a.name);
      col.className = "rcol s-" + a.state + (herdNow.cardOpen.has(a.name) ? " expanded" : "") +
        (herdNow.chipsOpen.has(a.name) ? " chips-open" : "") + (narrowCols.get(col) ? " narrow" : "");
      col.querySelector(".rtop").innerHTML = roomTop(a, asksFor(a.name, asks));
      const body = col.querySelector(".rbody");
      if ((body.getAttribute("data-subs") || "") !== subsKey(a) ||
          (body.getAttribute("data-queued") || "") !== queuedKey([a.name])) {
        body.setAttribute("data-subs", subsKey(a));
        body.setAttribute("data-queued", queuedKey([a.name]));
        if (body.hasAttribute("data-drawn")) renderRoomBody(a);
      }
      stripUnder(body, col.querySelector(".rsubs"), subsHtml(a));
      suggestIn(col, a.name, promptHint(a.name));
      renderColJump(a.name);
    });
    fitChips(agents.map((a) => roomColumn(a.name)).filter(Boolean));
    bdLand(false);
  }

  function onAgentAction(act, el) {
    if (act === "slot-step") {
      const s = herdNow.data && herdNow.data.handoffSlots;
      const field = el.getAttribute("data-field");
      if (!s || !field) return;
      return setSlotCap(field, Number(slotCap(s, field)) + Number(el.getAttribute("data-d")));
    }
    if (act === "tally") {
      const key = el.getAttribute("data-tally") || "";
      if (herdNow.tallyOpen.has(key)) herdNow.tallyOpen.delete(key);
      else herdNow.tallyOpen.add(key);
      return GM.renderHerdAgain();
    }
    if (/^(find|away)-/.test(act)) return onFindAction(act, el);
    const row = el.closest(PROMPT_ROW + ", .cmeter");
    const name = row && row.getAttribute("data-name");
    const group = el.closest(".pgroup");
    const project = group && group.getAttribute("data-project");
    if (act === "hooks") {
      const kind = el.getAttribute("data-kind");
      if (herdNow.hooksOpen.has(kind)) herdNow.hooksOpen.delete(kind);
      else herdNow.hooksOpen.add(kind);
      return renderAgents();
    }
    if (act === "hook-open") return emit("hook-open", { i: +el.getAttribute("data-i") });
    if (act === "jump-go" || act === "jump-prev" || act === "jump-next") {
      const dir = act === "jump-go" ? 0 : act === "jump-prev" ? 1 : -1;
      const col = el.closest(".rcol");
      return col ? colJump(col.getAttribute("data-name"), dir) : chatJump(dir);
    }
    if (act === "room") return openRoom(project);
    if (act === "guard-approve" || act === "guard-reject") {
      el.disabled = true;
      return emit(act, { id: el.getAttribute("data-id") });
    }
    if (act === "primary-room") {
      const p = primaryInfo();
      return p ? openRoom(p.directory) : undefined;
    }
    // A chip still a button on another device has no address yet: Emacs
    // would open it on the desk's screen.
    if ((act === "work-pr" || act === "work-task" || act === "work-changes") && XW.elsewhere) return setStatus("Its address is not known yet");
    if (act === "work-pr") {
      const repo = el.getAttribute("data-repo");
      return emit("open-pr", Object.assign({ id: +el.getAttribute("data-id") },
        repo ? { repo } : { root: el.getAttribute("data-root") }));
    }
    if (act === "work-task") return emit("open-task", { id: el.getAttribute("data-id") });
    if (act === "work-changes") return emit("open-changes", { root: el.getAttribute("data-root") });
    if (act === "use-suggestion") return useSuggestion(name, row && row.querySelector("textarea.agent-text"));
    // A queued message's Send now: Emacs presses its CLI's key for it,
    // only while that message is still queued on its screen.
    if (act === "send-now") {
      el.disabled = true;
      return emit("agent-send-now", { name: el.getAttribute("data-name") || name, text: el.getAttribute("data-text") || "" });
    }
    if (act === "work-more") {
      const key = el.getAttribute("data-key");
      if (herdNow.workOpen.has(key)) herdNow.workOpen.delete(key);
      else herdNow.workOpen.add(key);
      return herdNow.room ? renderRoom() : renderAgents();
    }
    if (act === "room-view") {
      const v = el.getAttribute("data-view");
      if (v === herdNow.roomView) return;
      herdNow.roomView = v;
      remember();
      herdNow.watchedChat = roomIsChat();
      renderRoom();
      herdNow.roomFirst = null;  // the columns in view now
      return watchRoom();
    }
    if (act === "copy-entry") {
      const list = drawn.get(el.closest(".rbody, .cbody, .subbody"));
      const e = list && list[+el.getAttribute("data-i")];
      const text = e && e[el.getAttribute("data-part") || "text"];
      return text ? copy(text) : setStatus("Nothing to copy", "error");
    }
    if (act === "chat-more") {
      const key = el.getAttribute("data-key");
      if (herdNow.chatOpen.has(key)) herdNow.chatOpen.delete(key);
      else herdNow.chatOpen.add(key);
      const body = app.querySelector(".chat .cbody");
      const keep = body ? body.scrollTop : 0;
      renderChatBody();
      if (body) body.scrollTop = keep;
      return;
    }
    if (act === "room-screen") {
      if (herdNow.roomScreen.has(name)) herdNow.roomScreen.delete(name);
      else herdNow.roomScreen.add(name);
      watchRoom();
      const a = roomAgents().find((x) => x.name === name);
      if (a) renderRoomBody(a);
      return renderRoom();
    }
    // A narrow column's +N: every chip, the line wrapping -- the wide
    // head's +N of each part opened with it -- or, as "less", one line again.
    if (act === "chips-more") {
      const a = agentByName(name);
      const keys = ["links|" + name].concat(a && a.work && a.work.branch ? [a.work.root + "|" + a.work.branch] : []);
      const open = !herdNow.chipsOpen.has(name);
      if (open) herdNow.chipsOpen.add(name);
      else herdNow.chipsOpen.delete(name);
      keys.forEach((k) => (open ? herdNow.workOpen.add(k) : herdNow.workOpen.delete(k)));
      return renderRoom();
    }
    if (act === "card-expand") {
      if (herdNow.cardOpen.has(name)) herdNow.cardOpen.delete(name);
      else herdNow.cardOpen.add(name);
      return renderRoom();
    }
    if (act === "sub-more") {
      const key = el.getAttribute("data-key");
      if (herdNow.subOpen.has(key)) herdNow.subOpen.delete(key);
      else herdNow.subOpen.add(key);
      if (el.closest(".chat .cbody")) return renderChatBody();
      const a = roomAgents().find((x) => x.name === name);
      const body = row && row.querySelector(".rbody");
      const keep = body ? body.scrollTop : 0;
      if (a) renderRoomBody(a);
      if (body) body.scrollTop = keep;
      return;
    }
    if (act === "sub-open") {
      const sub = el.getAttribute("data-sub");
      if (!sub) return setStatus("Its transcript is not known yet");
      // The parent path is a hint only: the chat view keeps no per-agent
      // conversation, so convs[name] is empty there.  Emacs resolves the
      // transcript from the name -- here we just name the sub to watch.
      const conv = herdNow.convs[name];
      herdNow.subView = { name, sub, who: el.getAttribute("data-who"), title: el.getAttribute("data-title") };
      herdNow.subConv = null;
      emit("sub-watch", { name, path: (conv && conv.path) || "", sub });
      return renderSubView();
    }
    if (act === "sub-close") {
      herdNow.subView = null;
      herdNow.subConv = null;
      emit("sub-unwatch", {});
      return renderSubView();
    }
    if (act === "load-earlier") {
      const conv = herdNow.convs[name];
      if (!conv || conv.loading || !conv.more) return;
      conv.loading = true;
      conv.paged = true;  // the poll now merges its tail, not replaces the view
      herdNow.earlierPending.add(name);
      // The rest of it, on its way, is the page before: shown as it comes.
      if (!(conv.rest > 0)) emit("room-earlier", { project: herdNow.room, name: name, before: (conv.entries || []).length });
      const a = roomAgents().find((x) => x.name === name);
      if (a) renderRoomBody(a);  // show "Loading earlier…"
      return;
    }
    if (act === "chat-earlier") {
      const c = herdNow.chat;
      if (!c || c.loading || !c.more) return;
      c.loading = true;
      c.paged = true;
      herdNow.chatEarlierPending = true;
      if (!(c.rest > 0)) emit("chat-earlier", { project: herdNow.room, before: (c.entries || []).length });
      renderChatBody();  // show "Loading earlier…"
      return;
    }
    if (act === "room-tools") {
      const key = el.getAttribute("data-key");
      if (herdNow.toolsOpen.has(key)) herdNow.toolsOpen.delete(key);
      else herdNow.toolsOpen.add(key);
      // In the sub-agent overlay there is no column to re-render, and `row'
      // (closest .rcol/.cfoot/.cmeter) is null -- re-render the overlay,
      // keeping the reader's place.
      const sub = el.closest(".subview");
      if (sub) {
        const sb = sub.querySelector(".subbody");
        const skeep = sb ? sb.scrollTop : 0;
        renderSubView();
        const nb = app.querySelector(".subview .subbody");
        if (nb) nb.scrollTop = skeep;
        return;
      }
      if (el.closest(".chat .cbody")) return renderChatBody();
      const a = roomAgents().find((x) => x.name === name);
      const body = row && row.querySelector(".rbody");
      const keep = body ? body.scrollTop : 0;
      if (a) renderRoomBody(a);
      if (body) body.scrollTop = keep;
      return;
    }
    if (act === "model") {
      const a = ((herdNow.data && herdNow.data.agents) || []).find((x) => x.name === name);
      if (herdNow.modelFor === name) herdNow.modelFor = null;
      else if (a) openModelForm(a);
      return renderAgents();
    }
    if (act === "model-pick") {
      const pick = herdNow.modelPick;
      const model = el.getAttribute("data-model");
      const effort = el.getAttribute("data-effort");
      if (model) pick.model = model;
      // An effort picked again is unpicked: the picker's own, then.
      if (effort) pick.effort = pick.effort === effort ? null : effort;
      return renderAgents();
    }
    if (act === "model-go") {
      const pick = herdNow.modelPick;
      herdNow.modelFor = null;
      if (pick.model) emit("agent-model", { name, model: pick.model, effort: pick.effort || "" });
      return renderAgents();
    }
    if (act === "model-cancel") {
      herdNow.modelFor = null;
      return renderAgents();
    }
    if (act === "agent-answer") return emit("agent-answer", { name, n: +el.getAttribute("data-n") });
    if (act === "agent-visit" || act === "agent-interrupt" || act === "agent-abort" || act === "agent-login" || act === "agent-login-continue" || act === "agent-carry-on" || act === "agent-sleep-cancel" || act === "agent-submit") return emit(act, { name });
    if (act === "login-open") return emit("open-browser", { url: el.getAttribute("data-url") });
    if (act === "login-copy") return emit("copy", { text: el.getAttribute("data-url") });
    if (act === "screen") {
      if (herdNow.open.has(name)) herdNow.open.delete(name);
      else {
        herdNow.open.add(name);
        emit("agent-screen", { name });
      }
      return renderAgents();
    }
    if (act === "more") {
      if (herdNow.more.has(name)) herdNow.more.delete(name);
      else herdNow.more.add(name);
      return renderAgents();
    }
    if (act === "compose") {
      if (herdNow.composing.has(name)) herdNow.composing.delete(name);
      else herdNow.composing.add(name);
      renderAgents();
      const ta = app.querySelector('.arow[data-name="' + cssName(name) + '"] textarea');
      if (ta) ta.focus();
      return;
    }
    if (act === "send") return sendPrompt(name);
    if (act === "image") {
      const ta = row && row.querySelector(PROMPT_BOX);
      if (ta) askImage(ta);
      return;
    }
    if (act === "unattach") {
      (herdNow.attach[name] || []).splice(+el.getAttribute("data-i"), 1);
      return renderAttach(name);
    }
    if (act === "notes") {
      const a = ((herdNow.data && herdNow.data.agents) || []).find((x) => x.name === name);
      herdNow.notes = name;
      herdNow.notesText = (a && a.notes) || "";
      renderAgents();
      const inp = app.querySelector(".notes-text");
      if (inp) inp.focus();
      return;
    }
    if (act === "notes-save") {
      const inp = row.querySelector(".notes-text");
      herdNow.notes = null;
      emit("agent-notes", { name, text: inp ? inp.value : herdNow.notesText });
      return renderAgents();
    }
    if (act === "notes-cancel") {
      herdNow.notes = null;
      return renderAgents();
    }
    if (act === "capture") {
      herdNow.capture = name;
      renderAgents();
      const inp = app.querySelector(".capture-text");
      if (inp) inp.focus();
      return;
    }
    if (act === "capture-save") {
      const inp = row.querySelector(".capture-text");
      herdNow.capture = null;
      emit("agent-capture", { name, note: inp ? inp.value : "" });
      return renderAgents();
    }
    if (act === "capture-cancel") {
      herdNow.capture = null;
      return renderAgents();
    }
    if (act === "respawn") {
      return ask("Respawn " + name + "?", "It is relaunched from its recipe. Continue picks the conversation up again; Fresh starts it cold.",
        [["Cancel", "ghost", null], ["Fresh", "ghost", () => emit("agent-respawn", { name, continue: false })],
         ["Continue", "primary", () => emit("agent-respawn", { name, continue: true })]]);
    }
    if (act === "relay") {
      const a = ((herdNow.data && herdNow.data.agents) || []).find((x) => x.name === name);
      return a ? askRelay(a) : undefined;
    }
    if (act === "kill") {
      return ask("Kill " + name + "?", "The agent stops and leaves the herd. Its transcript stays.",
        [["Cancel", "ghost", null], ["Kill", "primary danger", () => emit("agent-kill", { name })]]);
    }
    if (act === "stop-gone") {
      const a = ((herdNow.data && herdNow.data.agents) || []).find((x) => x.name === name);
      const g = a && a.gone;
      const risk = g && g.worktree && g.state === "ahead"
        ? " Its branch " + g.branch + " has " + g.ahead + " commit" + (g.ahead === 1 ? "" : "s") + " not on " + g.base + ": it is kept, but nothing will be working on it."
        : "";
      return ask("Stop " + name + "?", "Its " + (g && !g.worktree ? "directory" : "worktree") + " is gone. It stops and leaves the herd." + risk,
        [["Cancel", "ghost", null], ["Stop", "primary danger", () => emit("agent-stop-gone", { name })]]);
    }
    if (act === "new-agent") return openNewForm(project);
    if (act === "new-go") {
      const form = el.closest(".newform");
      const where = newProjectNow();
      const conv = newConvNow();
      if (!where) {
        setStatus("Which project?", "error");
        const box = form.querySelector(".new-project");
        return box ? box.focus() : undefined;
      }
      herdNow.newFor = null;
      if (conv) emit("resume", { source_path: conv.path, fork: !!(conv.fork && herdNow.newFork), quiet: true });
      else {
        const wt = form.querySelector(".new-worktree");
        emit("agent-new", {
          project: where,
          kind: form.querySelector(".new-kind").value,
          name: form.querySelector(".new-name").value.trim(),
          worktree: !!(wt && wt.checked),
        });
      }
      return renderAgents();
    }
    if (act === "new-room") {
      const where = newProjectNow();
      const c = herdNow.convs[where];
      if (!where) return setStatus("Which project?", "error");
      if (c && c.why) return setStatus(c.why, "error");
      // The room is the project's, by its name as the herd writes it, which
      // only Emacs can say of what was typed -- a subdirectory, a worktree,
      // no slash: until it has, wait for it rather than open a room of the
      // text, whose lead would be a second one, on part of a project.
      if (!c) {
        herdNow.newRoomWant = where;
        setStatus("Finding its project…");
        return askNewConversations(true);
      }
      return openNewRoom(c.root);
    }
    if (act === "new-cancel") {
      herdNow.newFor = herdNow.newRoomWant = null;
      return renderAgents();
    }
  }

  function sendPrompt(name) {
    const ta = app.querySelector('[data-name="' + cssName(name) + '"] textarea.agent-text');
    let text = (ta ? ta.value : herdNow.compose[name] || "").trim();
    // What the box holds, images and all, for it to have back if the
    // prompt does not reach Emacs.
    const draft = withImages(name, text);
    let to = name;
    // In the chat, @name at the start writes to that agent instead, and
    // @lead to the project's lead, or the one it starts.
    const at = ta && ta.closest(".cfoot") && /^@(\S+)\s+/.exec(text);
    if (at) {
      const agents = roomAgents();
      const lead = agents.find((a) => a.role === "lead");
      const named = at[1] === "lead" ? (lead ? lead.name : "@lead")
        : agents.some((a) => a.name === at[1]) ? at[1] : null;
      if (named) {
        to = named;
        text = text.slice(at[0].length).trim();
      }
    }
    const body = withImages(name, text);
    if (!body) return;
    const lost = () => giveBack(name, draft);
    const sent = to === "@lead" ? emit("lead-say", { project: herdNow.room, text: body }, lost)
      : to === "@primary" ? emit("primary-say", { text: body }, lost)
      : emit("agent-prompt", { name: to, text: body }, lost);
    if (sent === false) return setStatus(TOO_LONG, "error");
    herdNow.compose[name] = "";
    herdNow.composing.delete(name);
    delete herdNow.attach[name];
    if (herdNow.room) {
      if (ta) ta.value = "";
      renderAttach(name);
      return;
    }
    if (ta) ta.blur();
    renderAgents();
  }

  // Past what the page sends Emacs in one intent (xwapp.js's INTENT_MAX).
  const TOO_LONG = "Too long to send: over 2 million characters. It is still in the box.";

  // A prompt that did not reach Emacs after all goes back in NAME's box,
  // before what has been written there since: a lost send loses no words.
  // Returns what takes it out again, should Emacs have had it after all
  // (xwapp's emit calls it): sent again, it would go twice.
  function giveBack(name, draft) {
    const box = () => app.querySelector('[data-name="' + cssName(name) + '"] textarea.agent-text');
    const ta = box();
    const now = ((ta ? ta.value : herdNow.compose[name]) || "").trim();
    const text = now ? draft + "\n\n" + now : draft;
    herdNow.compose[name] = text;
    if (ta) {
      ta.value = text;
      autoGrow(ta);
    } else {
      herdNow.composing.add(name);
      if (view === "agents") renderAgents();
    }
    const said = "Not sent: Emacs did not get it. It is back in " + name + "'s box.";
    setStatus(said, "error");
    return () => {
      const ta = box();
      const rest = unGiven((ta ? ta.value : herdNow.compose[name]) || "", draft);
      if (rest !== null) {
        herdNow.compose[name] = rest;
        if (ta) {
          ta.value = rest;
          autoGrow(ta);
        } else if (!rest) {
          herdNow.composing.delete(name);
          if (view === "agents") renderAgents();
        }
      }
      unsay(said);
    };
  }

  // TEXT with DRAFT, given back at its start, taken out again; null when
  // it is no longer there as given, which is then left as it is.
  function unGiven(text, draft) {
    if (text === draft) return "";
    return text.startsWith(draft + "\n\n") ? text.slice(draft.length + 2) : null;
  }

  // The status line cleared, if it still says SAID.
  function unsay(said) {
    if (!statusEl.hidden && statusEl.textContent === said) setStatus("");
  }

  // TEXT with the images attached for NAME's next prompt after it, a
  // file name a line; "" with neither.
  function withImages(name, text) {
    const files = (herdNow.attach[name] || []).map((a) => a.path);
    return [text, files.join("\n")].filter(Boolean).join("\n\n");
  }

  // An image goes with a prompt as a file: Emacs reads the clipboard,
  // saves it, and hands back its name and a preview, shown above the box
  // until the prompt is sent.  Every CLI can read a file, and the page
  // can neither write one nor carry an image through the title it speaks
  // to Emacs by.
  //
  // On another device Emacs's clipboard is not the one you have: the
  // image is picked there -- a phone's photos, its camera, its files --
  // and the page sends it, in a request.  Made smaller first: a photo is
  // larger than a prompt needs, and than a request Emacs takes.
  function askImage(ta) {
    const name = ta.closest(PROMPT_ROW).getAttribute("data-name");
    if (!XW.elsewhere) return emit("agent-image", { name: name });
    pickFor = name;
    picker.value = "";
    if (typing) typeHold = Date.now();
    picker.click();
  }

  const picker = $("image-pick");
  let pickFor = null;
  picker.addEventListener("cancel", () => typeRelease());
  picker.addEventListener("change", () => {
    typeRelease();
    const file = picker.files && picker.files[0];
    const name = pickFor;
    pickFor = null;
    if (!file || !name) return;
    setStatus("Sending the image…");
    shrinkImage(file).then((data) => emit("agent-image-upload", { name: name, data: data }),
      (e) => setStatus(e.message, "error"));
  });

  const IMAGE_MAX = 2000;
  // FILE as a JPEG at most IMAGE_MAX pixels a side, in base64.  Read as
  // a data: URL, the one kind of image the page's CSP loads.
  function shrinkImage(file) {
    return new Promise((resolve, reject) => {
      const reader = new FileReader();
      reader.addEventListener("error", () => reject(new Error("Could not read that image")));
      reader.addEventListener("load", () => {
        const img = new Image();
        img.addEventListener("error", () => reject(new Error("Not an image this browser can show")));
        img.addEventListener("load", () => {
          const k = Math.min(1, IMAGE_MAX / Math.max(img.naturalWidth, img.naturalHeight));
          const c = document.createElement("canvas");
          c.width = Math.round(img.naturalWidth * k);
          c.height = Math.round(img.naturalHeight * k);
          c.getContext("2d").drawImage(img, 0, 0, c.width, c.height);
          resolve(c.toDataURL("image/jpeg", 0.85).replace(/^data:image\/jpeg;base64,/, ""));
        });
        img.src = reader.result;
      });
      reader.readAsDataURL(file);
    });
  }

  const THUMB = /^data:image\/png;base64,[A-Za-z0-9+/=]+$/;
  function attachHtml(name) {
    return (herdNow.attach[name] || []).map((a, i) =>
      '<span class="chip" title="' + esc(a.path) + '">' +
      (THUMB.test(a.thumb || "") ? '<img src="' + a.thumb + '" alt="">' : '<span class="noimg">▧</span>') +
      '<span class="cname">' + esc(a.path.split("/").pop()) + "</span>" +
      '<button type="button" class="linkish" data-act="a:unattach" data-i="' + i + '" title="Remove">×</button></span>').join("");
  }
  function renderAttach(name) {
    const strip = app.querySelector('[data-name="' + cssName(name) + '"] .attach');
    if (strip) strip.innerHTML = attachHtml(name);
  }
  // In Emacs, Cmd-V never becomes a paste: the page kit asks Emacs for
  // its clipboard, and puts the text in at the cursor (xwapp.js, "⌘V in
  // Emacs").  With no text there, the clipboard's image goes to the
  // prompt box the cursor is in.  In a web browser Cmd-V is a paste, and
  // an image in it is asked for below.
  document.addEventListener("xw-paste", (ev) => {
    if (ev.detail && ev.detail.text) return;
    const t = ev.target;
    ev.preventDefault();
    if (t.matches && t.matches(PROMPT_BOX)) askImage(t);
    else setStatus("To attach an image, click into a prompt box first");
  });

  // Inbox desk keys: j/k move, 1-4 answer a menu, a approve, e done.
  document.addEventListener("keydown", (ev) => {
    if (view !== "inbox" || ev.metaKey || ev.ctrlKey || ev.altKey) return;
    inboxKey(ev);
  });
  // Keep the reply text across the Inbox's periodic re-renders.
  const isReplyBox = (t) => !!(t && t.classList && t.classList.contains("ibreplybox"));
  // A reply given back keeps what is written in its box, for when its
  // item is picked again.
  const replyTyped = (ta) => {
    inbox.reply = ta.value;
    const id = ta.getAttribute("data-id");
    if (inbox.back.has(id)) inbox.back.set(id, ta.value);
  };
  app.addEventListener("input", (ev) => {
    if (!isReplyBox(ev.target)) return;
    replyTyped(ev.target);
    if (ev.isComposing) inboxComposing = true;
  });
  // A composition open holds the Inbox's redraws back (`renderInbox');
  // the one held is drawn once it ends -- after the input event that may
  // follow compositionend -- or once the box loses focus, which ends it.
  const composed = (ev) => {
    if (!inboxComposing || !isReplyBox(ev.target)) return;
    inboxComposing = false;
    replyTyped(ev.target);
    setTimeout(() => { if (inboxHeld && !inboxComposing) renderInbox(); }, 0);
  };
  app.addEventListener("compositionstart", (ev) => { if (isReplyBox(ev.target)) inboxComposing = true; });
  app.addEventListener("compositionend", composed);
  app.addEventListener("focusout", composed);

  // A paste with no text in it holds the clipboard's image, or nothing a
  // textarea would take anyway; Emacs says which.
  app.addEventListener("paste", (ev) => {
    const t = ev.target;
    if (!t.matches || !t.matches(PROMPT_BOX)) return;
    if (Array.from((ev.clipboardData && ev.clipboardData.types) || []).includes("text/plain")) return;
    ev.preventDefault();
    askImage(t);
  });

  app.addEventListener("change", (ev) => {
    // A cap typed: sent as typed -- Emacs takes it only when sane, and
    // says why not.
    if (ev.target.classList && ev.target.classList.contains("slinput")) {
      const field = ev.target.getAttribute("data-field");
      const s = herdNow.data && herdNow.data.handoffSlots;
      if (field && s && String(ev.target.value) !== String(slotCap(s, field))) setSlotCap(field, ev.target.value);
      return;
    }
    if (ev.target.classList && ev.target.classList.contains("chat-to")) {
      setChatTo(ev.target.value);
      // Typing, picked from the tag: on with the box.
      const ta = typing && ev.target.closest(".cfoot").querySelector("textarea");
      if (ta) ta.focus();
    }
    if (ev.target.classList && ev.target.classList.contains("new-kind")) {
      herdNow.newKind = ev.target.value;
      herdNow.newKindPicked = true;
    }
    if (ev.target.classList && ev.target.classList.contains("new-worktree")) herdNow.newWorktree = ev.target.checked;
    if (ev.target.classList && ev.target.classList.contains("new-conv")) {
      herdNow.newConv = ev.target.value;
      herdNow.newFork = false;
      syncNewForm();
    }
    if (ev.target.classList && ev.target.classList.contains("new-fork")) {
      herdNow.newFork = ev.target.checked;
      syncNewForm();
    }
  });

  app.addEventListener("input", (ev) => {
    const c = ev.target.classList;
    if (!c) return;
    if (c.contains("agent-text")) {
      const row = ev.target.closest(".arow, .rcol, .cfoot");
      if (row) herdNow.compose[row.getAttribute("data-name")] = ev.target.value;
      autoGrow(ev.target);
    } else if (c.contains("new-name")) {
      herdNow.newName = ev.target.value;
    } else if (c.contains("new-project")) {
      herdNow.newProject = ev.target.value;
      herdNow.newConv = "";
      askNewConversations(false);
    } else if (c.contains("notes-text")) {
      herdNow.notesText = ev.target.value;
    }
  });
  app.addEventListener("keydown", (ev) => {
    const t = ev.target;
    if (!t.classList) return;
    if (t.classList.contains("slinput") && ev.key === "Enter") {
      ev.preventDefault();
      t.blur();
      return;
    }
    if (t.classList.contains("agent-text") && ev.key === "Tab" && !ev.shiftKey && !t.value) {
      const box = t.closest(".arow, .rcol, .cfoot");
      if (box && useSuggestion(box.getAttribute("data-name"), t)) ev.preventDefault();
    } else if (t.classList.contains("agent-text") && ev.key === "Enter" && (ev.metaKey || ev.ctrlKey)) {
      ev.preventDefault();
      sendPrompt(t.closest(".arow, .rcol, .cfoot").getAttribute("data-name"));
    } else if (t.classList.contains("notes-text") && ev.key === "Enter") {
      ev.preventDefault();
      onAgentAction("notes-save", t);
    } else if (t.classList.contains("capture-text") && ev.key === "Enter") {
      ev.preventDefault();
      onAgentAction("capture-save", t);
    } else if (t.classList.contains("ibreplybox") && ev.key === "Enter" && (ev.metaKey || ev.ctrlKey)) {
      ev.preventDefault();
      onInboxAction("reply", t);
    }
  });
  // An edit in progress holds the 5s refresh back; let it through after
  // -- after the click, when a press ended it.
  function drawHeld() {
    setTimeout(() => { if (herdNow.pending && view === "agents" && !busyEditing()) renderAgents(); }, 0);
  }
  const released = () => {
    if (!pressing) return;
    pressing = false;
    drawHeld();
  };
  app.addEventListener("focusout", drawHeld);
  window.addEventListener("pointerdown", () => { pressing = true; }, true);
  // In a room: the column pointed at is where / opens the find; a press
  // outside an open hit list closes it.
  app.addEventListener("pointerdown", (ev) => {
    if (!herdNow.room) return;
    const col = ev.target.closest && ev.target.closest(".rcol");
    if (col) herdNow.lastCol = col.getAttribute("data-name");
    Object.keys(herdNow.find).forEach((k) => {
      const F = herdNow.find[k];
      if (!F.list) return;
      const strip = findStrip(k);
      if (strip && strip.contains(ev.target)) return;
      F.list = false;
      renderStripOf(k);
    });
  });
  window.addEventListener("pointerup", released, true);
  window.addEventListener("pointercancel", released, true);
  window.addEventListener("blur", released);
  // A release the page never heard of, outside it.
  window.addEventListener("pointermove", (ev) => { if (ev.buttons === 0) released(); }, true);

  // ---- The herd log -----------------------------------------------------
  //
  // One lane per agent: its states as coloured segments across the
  // range, rebuilt from the state transitions (old -> new, with when and
  // why).  Below, the entries, newest first.  The lanes are the point:
  // back from an absence, who sat blocked, since when, is one look.

  const HOUR = 3600e3;
  const RANGES = [[6 * HOUR, "6h"], [24 * HOUR, "24h"], [7 * 24 * HOUR, "7d"], [0, "All"]];
  // Rows drawn at once; the rest wait behind a "show earlier", so a week of
  // entries does not build thousands of nodes in one pass.  `open' holds the
  // ids of the entries whose screen is expanded -- ids, not indices, so a
  // delete or a reload does not shift which screen is open.  `issues' keeps
  // the entries Emacs marks worth a look after the fact (`ghostherd-log-
  // issue-p'): a look back, where the Inbox is what needs you now.
  // `zoom' is a moment zoomed to, or null: {t, id, back}, the entry's time
  // and id, and the filters set aside while in it, given back on leaving.
  // `listed' and `drawn' are the list's rows and those drawn, as last drawn.
  const LOGCAP = 300;
  const ZOOM = 15 * 60e3;
  let herd = { entries: null, range: 24 * HOUR, session: "", issues: false,
               screensOnly: false, filter: "", open: new Set(), shown: LOGCAP, zoom: null,
               listed: 0, drawn: 0 };

  function sessionColor(name) {
    let h = 0;
    for (const c of String(name)) h = (h * 31 + c.charCodeAt(0)) >>> 0;
    return "hsl(" + (h % 360) + ", 55%, 62%)";
  }

  function clock(t) {
    const d = new Date(t);
    return d.toLocaleTimeString(undefined, { hour: "2-digit", minute: "2-digit", second: "2-digit", hour12: false });
  }

  function dayLabel(t) {
    const d = new Date(t);
    const today = new Date();
    const y = new Date(today);
    y.setDate(today.getDate() - 1);
    if (d.toDateString() === today.toDateString()) return "Today";
    if (d.toDateString() === y.toDateString()) return "Yesterday";
    return d.toLocaleDateString(undefined, { weekday: "short", month: "short", day: "numeric" });
  }

  function span(ms) {
    const m = Math.round(ms / 60000);
    if (m < 60) return m + "m";
    const h = Math.floor(m / 60);
    return h < 48 ? h + "h" + (m % 60 ? " " + (m % 60) + "m" : "") : Math.round(h / 24) + "d";
  }

  // What the lanes and the list show: the range back to now, or the half
  // hour around a moment -- fixed, so the minute's redraw moves only what
  // is still going on in it.
  function logWindow() {
    const now = Date.now();
    if (herd.zoom) return { start: herd.zoom.t - ZOOM, end: herd.zoom.t + ZOOM, now };
    const entries = herd.entries || [];
    const first = entries.length ? entries[0].t : now - HOUR;
    return { start: herd.range ? now - herd.range : first, end: now, now };
  }

  // Segments per session from the state transitions; the state a range
  // opens in is whatever the last transition before it left.  Each names
  // the entry it began with by id, as the list's rows do.
  function lanes(start, end, now) {
    const by = new Map();
    (herd.entries || []).forEach((e) => {
      if (e.session === "herd") return; // the seam between Emacsen
      if (!by.has(e.session)) by.set(e.session, []);
      by.get(e.session).push(e);
    });
    const out = [];
    by.forEach((list, name) => {
      if (herd.session && name !== herd.session) return;
      let state = null, since = start, reason = "", from = -1;
      const segs = [];
      const marks = [];
      for (const e of list) {
        // Past a moment's end; not past a live range's: "now" is this
        // device's clock, and a line the Mac timed may be a moment ahead.
        if (herd.zoom && e.t > end) break;
        if (e.kind === "state" && e.new) {
          if (e.t >= start && state) segs.push({ state, a: since, b: e.t, reason, id: from });
          state = e.new;
          since = Math.max(e.t, start);
          reason = e.reason || "";
          from = e.id;
        } else if (e.kind === "life") {
          if (e.t >= start) marks.push({ t: e.t, text: e.text, id: e.id });
          if (/^(killed|released)/.test(e.text)) {
            if (state && e.t >= start) segs.push({ state, a: since, b: e.t, reason, id: from });
            state = null;
          }
        }
      }
      // Still in it: to now, or to the window's end when that is past.
      if (state) segs.push({ state, a: since, b: Math.max(since, Math.min(end, now)), reason, id: from, open: end >= now });
      if (segs.length || marks.length) out.push({ name, segs, marks, last: list[list.length - 1].t });
    });
    return out.sort((a, b) => b.last - a.last);
  }

  function laneHtml(l, start, end) {
    const w = Math.max(1, end - start);
    const pct = (t) => (100 * (t - start)) / w;
    const segs = l.segs
      .map((s) => {
        const tip = s.state + " · " + clock(s.a) + (s.open ? " – now" : " – " + clock(s.b)) +
          " (" + span(s.b - s.a) + ")" + (s.reason ? "\n" + s.reason : "");
        return '<span class="seg st-' + esc(s.state) + '" data-act="seg" data-id="' + s.id + '" style="left:' +
          pct(s.a).toFixed(3) + "%;width:" + Math.max(0, pct(s.b) - pct(s.a)).toFixed(3) + '%" title="' + esc(tip) + '"></span>';
      })
      .join("");
    const marks = l.marks
      .map((m) => '<span class="life" data-act="seg" data-id="' + m.id + '" style="left:' + pct(m.t).toFixed(3) +
        '%" title="' + esc(clock(m.t) + " · " + m.text) + '"></span>')
      .join("");
    // The moment zoomed to, a line down every lane.
    const moment = herd.zoom ? '<span class="moment" style="left:' + pct(herd.zoom.t).toFixed(3) + '%"></span>' : "";
    const blocked = l.segs.filter((s) => s.state === "blocked");
    const note = blocked.length
      ? '<span class="lane-note">' + blocked.length + "× blocked · " + span(blocked.reduce((n, s) => n + s.b - s.a, 0)) + "</span>"
      : "";
    return '<div class="lane"><button type="button" class="lane-name" data-act="log-session" data-session="' +
      esc(l.name) + '" title="Show only ' + esc(l.name) + '"><span class="dot" style="background:' + sessionColor(l.name) +
      '"></span>' + esc(l.name) + "</button>" + '<div class="lane-bar">' + segs + marks + moment + "</div>" + note + "</div>";
  }

  function axisHtml(start, end, now) {
    const ticks = [0, 0.25, 0.5, 0.75, 1].map((f) => {
      const t = start + f * (end - start);
      // "now" only where the window ends now: a moment's may end later.
      const label = f === 1 && end === now ? "now" : end - start > 2 * 24 * HOUR ? dayLabel(t) : clock(t).slice(0, 5);
      return '<span style="left:' + f * 100 + '%">' + esc(label) + "</span>";
    });
    return '<div class="lane axis"><span class="lane-name"></span><div class="lane-bar">' + ticks.join("") + "</div><span class=\"lane-note\"></span></div>";
  }

  // The zoom's chip: the half hour it shows, and the way out of it.
  function zoomLabel(t) {
    const day = dayLabel(t);
    return (day === "Today" ? "" : day + " ") + clock(t - ZOOM).slice(0, 5) + "–" + clock(t + ZOOM).slice(0, 5);
  }

  // In the window: past its start, and before its end only in a moment's --
  // a live range ends at now on this device's clock, which may run a
  // moment behind the Mac's that timed the entry.
  function inLogWindow(t, w) {
    return t >= w.start && (!herd.zoom || t <= w.end);
  }

  function issueCount() {
    const w = logWindow();
    let n = 0;
    for (const e of herd.entries || []) {
      if (e.issue && inLogWindow(e.t, w) && (!herd.session || e.session === herd.session)) n++;
    }
    return n;
  }

  function issuesLabel(n) {
    return "Issues" + (n ? ' <span class="count">' + n + "</span>" : "");
  }

  // The agent picker's options: each agent with how many lines it has.
  function sessionOptions() {
    const counts = {};
    (herd.entries || []).forEach((e) => { if (e.session !== "herd") counts[e.session] = (counts[e.session] || 0) + 1; });
    return ['<option value="">All agents</option>']
      .concat(Object.keys(counts).sort().map((s) =>
        '<option value="' + esc(s) + '"' + (herd.session === s ? " selected" : "") + ">" + esc(s) + " (" + counts[s] + ")</option>"))
      .join("");
  }

  function showLog() {
    view = tab = "log";
    src = null;
    chrome();
    const opts = sessionOptions();
    const z = herd.zoom;
    const issues = herd.entries ? issueCount() : 0;
    app.innerHTML =
      '<div class="page wide">' +
      '<div class="filters log-tools"><div class="chips">' +
      (z ? '<button type="button" class="chip on zoomed" data-act="log-unzoom" title="Leave this half hour (Esc)">' +
           esc(zoomLabel(z.t)) + '<span class="unzoom" aria-hidden="true">✕</span></button>' : "") +
      RANGES.map(([ms, label]) => '<button type="button" class="chip' + (!z && herd.range === ms ? " on" : "") +
        '" data-act="range" data-range="' + ms + '">' + label + "</button>").join("") +
      '<button type="button" class="chip' + (herd.issues ? " on" : "") + '" data-act="issues-only" title="' +
        esc("What went wrong, looking back: blocks, input that did not go in, asks unanswered or failed, " +
            "guard holds, denied checks, quotas and screens no rule knew.  What needs you now is the Inbox.") +
        '">' + issuesLabel(issues) + "</button>" +
      '<button type="button" class="chip' + (herd.screensOnly ? " on" : "") + '" data-act="screens-only">Screens</button>' +
      "</div>" +
      '<input id="log-filter" type="search" autocomplete="off" spellcheck="false" placeholder="Filter' + SLASH + '" value="' + esc(herd.filter) + '">' +
      '<select id="log-session" title="Agent">' + opts + "</select></div>" +
      '<div id="lanes" class="lanes"></div><div id="log-list" class="results"></div></div>';
    if (!herd.entries) {
      $("log-list").innerHTML = '<p class="empty">Reading the log…</p>';
      emit("log");
      return;
    }
    renderLanes();
    renderLogList();
  }

  function renderLog() {
    if (view !== "log") return;
    if (!$("lanes")) return showLog();
    // The toolbar has the agents and the chips: rebuild it all.
    const keep = app.scrollTop;
    showLog();
    app.scrollTop = keep;
  }

  function renderLanes() {
    const box = $("lanes");
    if (!box) return;
    const { start, end, now } = logWindow();
    const ls = lanes(start, end, now);
    box.innerHTML = ls.length
      ? axisHtml(start, end, now) + ls.map((l) => laneHtml(l, start, end)).join("")
      : '<p class="empty">No agent did anything in this ' + (herd.zoom ? "half hour" : "range") + ".</p>";
  }

  // WHY, when given, is its title: what the herd read off its screen.
  function stateBadge(s, why) {
    return '<span class="st st-' + esc(s) + '"' + (why && why !== "—" ? ' title="' + esc(s + ": " + why) + '"' : "") + ">" +
      esc(s) + "</span>";
  }

  // A slow load's bar: what it waited on, each its share, in the order
  // Emacs said them (the longest first).
  function timingBar(e) {
    const ph = Array.isArray(e.phases) ? e.phases : [];
    const total = ph.reduce((n, p) => n + (Number(p[2]) || 0), 0);
    if (!total) return "";
    return '<span class="tbar" aria-hidden="true">' + ph.map((p) =>
      '<span class="tseg ts-' + esc(p[0]) + '" style="width:' + ((100 * (Number(p[2]) || 0)) / total).toFixed(1) +
      '%" title="' + esc(p[1] + " " + p[2] + " s") + '"></span>').join("") + "</span>";
  }

  function entryRow(e, re) {
    let body;
    if (e.kind === "state" && e.new) {
      body = stateBadge(e.old) + '<span class="arrow">→</span>' + stateBadge(e.new) +
        (e.reason ? '<span class="reason">' + hl(e.reason, re) + "</span>" : "");
    } else {
      body = '<span class="k-' + esc(e.kind) + '">' + hl(e.text, re) + "</span>" +
        (e.kind === "timing" ? timingBar(e) : "");
    }
    const open = herd.open.has(e.id);
    const why = e.why ? '<span class="why why-' + esc(e.why) + '" title="why this screen was kept">' + esc(e.why) + "</span>" : "";
    const pin = e.pinned ? '<span class="pinmark" title="Pinned: kept until you remove it">📌</span>' : "";
    const note = e.note ? '<span class="lnote">' + hl(e.note, re) + "</span>" : "";
    // A slow load keeps its phases where a screen line keeps its screen.
    const kept = e.kind === "timing" ? "phases" : "screen";
    const screen = e.screen
      ? '<button type="button" class="linkish" data-act="screen" data-id="' + e.id + '">' +
        (open ? "Hide " + kept : kept[0].toUpperCase() + kept.slice(1)) + "</button>"
      : "";
    // Pin/unpin and delete sit on a captured screen and on any pinned line.
    const ops = (e.screen || e.pinned)
      ? '<button type="button" class="linkish" data-act="' + (e.pinned ? "log-unpin" : "log-pin") +
        '" data-id="' + e.id + '">' + (e.pinned ? "Unpin" : "Pin") + "</button>" +
        '<button type="button" class="linkish danger" data-act="log-delete" data-id="' + e.id + '" title="Delete this entry">Delete</button>'
      : "";
    const moment = herd.zoom && herd.zoom.id === e.id;
    // Its time is the way into its moment: the half hour around it.
    return (
      '<div class="lrow' + (e.pinned ? " pinned" : "") + (moment ? " moment" : "") + '" data-id="' + e.id + '">' +
      '<button type="button" class="lt" data-act="log-zoom" data-id="' + e.id + '" title="' +
        esc(new Date(e.t).toLocaleString() + " · show the half hour around it") + '">' + esc(clock(e.t)) + "</button>" +
      '<button type="button" class="ls" data-act="log-session" data-session="' + esc(e.session) + '">' +
      '<span class="dot" style="background:' + sessionColor(e.session) + '"></span>' + esc(e.session) + "</button>" +
      '<span class="lx">' + pin + why + body + note + "</span>" + screen + ops + "</div>" +
      (e.screen && open ? '<pre class="screen">' + esc(e.screen) + "</pre>" : "")
    );
  }

  function logMatches(e, f) {
    return !f || (e.text + " " + e.session + " " + (e.why || "") + " " +
                  (e.note || "") + " " + (e.screen || "")).toLowerCase().includes(f);
  }

  // Whether the list's filters let E through (F, the text filter, lowercased).
  function logPasses(e, f) {
    return !(herd.screensOnly && !e.screen) && !(herd.session && e.session !== herd.session) &&
      !(herd.issues && !e.issue) && logMatches(e, f);
  }

  // The entries the list shows, newest first: in the window, past the filters.
  function logRows() {
    const w = logWindow();
    const f = herd.filter.trim().toLowerCase();
    const rows = [];
    const entries = herd.entries || [];
    for (let i = entries.length - 1; i >= 0; i--) {
      const e = entries[i];
      if (e.t < w.start) break;
      if (inLogWindow(e.t, w) && logPasses(e, f)) rows.push(e);
    }
    return rows;
  }

  function logSum(listed, drawn) {
    return listed + (listed === 1 ? " entry" : " entries") + (listed > drawn ? ", showing " + drawn : "");
  }

  function renderLogList() {
    const box = $("log-list");
    if (!box) return;
    const re = termRe(herd.filter);
    const rows = logRows();
    const entries = herd.entries || [];
    // Draw the most recent `shown' at once; the rest wait behind a button.
    const shown = rows.slice(0, herd.shown);
    const more = rows.length - shown.length;
    // What it drew, for a new line to be put on top of (logAddInPlace).
    herd.listed = rows.length;
    herd.drawn = shown.length;
    let day = "";
    box.innerHTML = rows.length
      ? '<div class="sum">' + logSum(rows.length, shown.length) + "</div>" +
        shown.map((e) => {
          const d = dayLabel(e.t);
          const head = d !== day ? '<h2 class="section" data-day="' + esc(new Date(e.t).toDateString()) + '">' +
            esc(d) + "</h2>" : "";
          day = d;
          return head + entryRow(e, re);
        }).join("") +
        (more > 0 ? '<button type="button" class="ghost more-log" data-act="log-more">Show earlier (' +
          more + " more)</button>" : "")
      : '<p class="empty">' + (!entries.length ? "Nothing logged yet." :
          herd.zoom ? "Nothing matches in this half hour." : "Nothing matches in this range.") + "</p>";
  }

  function logEntry(id) {
    return (herd.entries || []).find((x) => x.id === id);
  }

  // Draw enough rows that E's is among them, when the list has it at all.
  function drawUpTo(e) {
    const at = logRows().indexOf(e);
    if (at >= herd.shown) herd.shown = (Math.floor(at / LOGCAP) + 1) * LOGCAP;
  }

  function flashRow(id) {
    const row = app.querySelector('.lrow[data-id="' + id + '"]');
    if (!row) return false;
    row.scrollIntoView({ block: "center" });
    row.classList.remove("flash");
    void row.offsetWidth;
    row.classList.add("flash");
    return true;
  }

  // A lane's segment or mark: its entry's row, always -- what hides it set
  // aside: a filter it does not pass, a window it is outside (a range
  // widened to hold it; a moment moved to it), the rows not drawn yet.
  function goToEntry(id) {
    const e = logEntry(id);
    if (!e) return;
    if (!app.querySelector('.lrow[data-id="' + id + '"]')) {
      const { start, end, now } = logWindow();
      if (e.t < start || e.t > end) {
        if (herd.zoom) herd.zoom = Object.assign({}, herd.zoom, { t: e.t, id: e.id });
        else herd.range = RANGES.map((r) => r[0]).find((ms) => ms === 0 || now - ms <= e.t);
      }
      if (herd.session && e.session !== herd.session) herd.session = "";
      if (herd.issues && !e.issue) herd.issues = false;
      if (herd.screensOnly && !e.screen) herd.screensOnly = false;
      if (!logMatches(e, herd.filter.trim().toLowerCase())) herd.filter = "";
      drawUpTo(e);
      renderLog();
    }
    flashRow(id);
  }

  // A moment: the half hour around one entry, lanes and list, all of it --
  // the filters that narrowed the list set aside until you leave, so from
  // an issue you see what led to it and what came after.
  function zoomTo(id) {
    const e = logEntry(id);
    if (!e) return;
    const back = herd.zoom ? herd.zoom.back
      : { issues: herd.issues, screensOnly: herd.screensOnly, filter: herd.filter };
    herd.zoom = { t: e.t, id: e.id, back };
    herd.issues = herd.screensOnly = false;
    herd.filter = "";
    herd.shown = LOGCAP;
    renderLog();
    flashRow(id);
  }

  // Back to the range (RANGE, when one was picked), the filters as they
  // were, and the row the moment was zoomed to.
  function unzoom(range) {
    const z = herd.zoom;
    if (!z) return;
    herd.zoom = null;
    Object.assign(herd, z.back);
    if (range !== undefined) herd.range = range;
    herd.shown = LOGCAP;
    const e = logEntry(z.id);
    if (e) drawUpTo(e);
    renderLog();
    if (!flashRow(z.id)) app.scrollTop = 0;
  }

  // A new line, as it is logged: its row put on top, the counts and the
  // lanes brought up to date -- the list not rebuilt.  With a week of rows
  // drawn (a deep click, Show earlier) a rebuild cost each new line a
  // quarter of a second on a phone, and the herd logs hundreds a day.  The
  // toolbar is touched in place too, so a filter being typed keeps its
  // focus.  False when it cannot be done here and the whole Log is drawn
  // instead: not drawn yet, a line older than the one before it, a list
  // that was empty, a new day (its Today and Yesterday move up).
  function logAddInPlace(e, prev) {
    const box = $("log-list"), sel = $("log-session");
    const chip = app.querySelector('.log-tools [data-act="issues-only"]');
    if (!box || !sel || !chip || !$("lanes")) return false;
    if (prev && e.t < prev.t) return false;
    const w = logWindow();
    const f = herd.filter.trim().toLowerCase();
    const shows = inLogWindow(e.t, w) && logPasses(e, f);
    const head = box.querySelector("h2.section");
    const sum = box.querySelector(".sum");
    if (shows && (!head || !sum || head.getAttribute("data-day") !== new Date(e.t).toDateString())) return false;
    if (e.session !== "herd") {
      const opt = Array.from(sel.options).find((o) => o.value === e.session);
      if (!opt) sel.innerHTML = sessionOptions();
      else opt.textContent = e.session + " (" + herd.entries.filter((x) => x.session === e.session).length + ")";
    }
    // Its own, and those a live range has since left behind.
    const issues = issuesLabel(issueCount());
    if (chip.innerHTML !== issues) chip.innerHTML = issues;
    renderLanes();
    if (!shows) return true;
    head.insertAdjacentHTML("afterend", entryRow(e, termRe(herd.filter)));
    // Nothing falls off the foot: the rows drawn grow by this one.
    herd.listed++;
    herd.drawn++;
    if (herd.drawn > herd.shown) herd.shown = herd.drawn;
    sum.textContent = logSum(herd.listed, herd.drawn);
    return true;
  }

  // "now" moves: redraw the open segments now and then.  A moment's window
  // is fixed, so this only stretches what is still going on in it.
  setInterval(() => { if (view === "log") renderLanes(); }, 60000);

  // ---- Focus -----------------------------------------------------------

  function refocus() {
    if (lastOpened) return;
    const input = $("q") || $("sess-filter");
    if (input && document.activeElement !== input) {
      input.focus();
      input.setSelectionRange(input.value.length, input.value.length);
    }
  }

  function cards() {
    return Array.from(app.querySelectorAll(".card"));
  }

  // ---- Events ----------------------------------------------------------

  app.addEventListener("input", (ev) => {
    if (ev.target.id === "q") {
      query = ev.target.value;
      limit = 30;
      lastOpened = null;
      search(false);
    } else if (ev.target.id === "sess-filter") {
      sessionsFilter = ev.target.value;
      renderSessionList();
    } else if (ev.target.id === "log-filter") {
      herd.filter = ev.target.value;
      renderLogList();
    } else if (ev.target.id === "mem-filter") {
      mem.filter = ev.target.value;
      renderMemoryList();
    } else if (ev.target.id === "docs-filter") {
      docsView.q = ev.target.value;
      renderDocsList();
    }
  });

  app.addEventListener("change", (ev) => {
    if (ev.target.id === "log-session") {
      herd.session = ev.target.value;
      renderLog();
      return;
    }
    if (ev.target.id === "mem-project") {
      mem.key = ev.target.value;
      mem.open = new Set(); mem.editing = ""; mem.confirmDel = "";
      emit("memory-watch", { key: mem.key });
      return;
    }
    if (ev.target.id !== "project") return;
    filters.project = ev.target.value;
    onFilters();
  });

  function onFilters() {
    if (view === "sessions") {
      renderSessionList();
      app.querySelectorAll('[data-act="agent"]').forEach((b) =>
        b.classList.toggle("on", b.getAttribute("data-agent") === filters.agent));
      return;
    }
    app.querySelectorAll('[data-act="agent"]').forEach((b) =>
      b.classList.toggle("on", b.getAttribute("data-agent") === filters.agent));
    limit = 30;
    if (query.trim()) search(true);
    else renderResults();
  }

  // ---- Memory tab ------------------------------------------------------
  // The shared-memory facts of one store (the user store, or a project's),
  // pushed live by Emacs (memory-watch/renderMemory) so a fact added
  // elsewhere appears without a reload.  Open a fact to read its body, edit
  // or delete it -- through the store's own code, sidecar-side.

  function showMemory(opts) {
    view = tab = "memory";
    src = null;
    mem.open = new Set(); mem.editing = ""; mem.confirmDel = "";
    mem.proposals = []; mem.promotedOnly = false; mem.rejecting = "";
    mem.checks = []; mem.checkOpen = ""; mem.confirmRm = "";
    mem.unusedOnly = false;
    const target = (opts && opts.project) || herdNow.room || ctx.currentProject || "";
    chrome();
    app.innerHTML =
      '<div class="page wide memory">' +
      '<div class="filters mem-tools">' +
      '<select id="mem-project" title="Project store"></select>' +
      '<div class="mem-find">' +
      '<input id="mem-filter" type="search" autocomplete="off" spellcheck="false" placeholder="Filter' +
      SLASH + '" value="' + esc(mem.filter) + '">' +
      '<span class="msort" role="group" aria-label="Order">' +
      '<button type="button" class="ghost small" data-act="mem-sort" data-sort="recent" aria-pressed="false" title="Newest change first">Recent</button>' +
      '<button type="button" class="ghost small" data-act="mem-sort" data-sort="name" aria-pressed="false" title="By name">A–Z</button>' +
      '<button type="button" class="ghost small" data-act="mem-sort" data-sort="used" aria-pressed="false" title="Most used first: asks and recalls in the last 30 days">Used</button>' +
      '</span></div>' +
      '<button type="button" class="ghost small" id="mem-unused" data-act="mem-unused-toggle" aria-pressed="false" hidden title="Show the facts no ask or recall has used in 30 days">Unused</button>' +
      '<button type="button" class="ghost small" id="mem-promoted" data-act="mem-promoted-toggle" aria-pressed="false" title="Show facts promoted into an instruction file">Promoted</button>' +
      '<button type="button" class="ghost small" data-act="mem-suggest" title="Ask the project\'s lead to review the memory and propose promotions">Suggest promotions</button>' +
      '</div>' +
      '<div id="mem-proposals"></div>' +
      '<div id="mem-checks"></div>' +
      '<div id="mem-list" class="results"><p class="empty">Reading memory…</p></div></div>';
    renderMemPicker();
    emit("memory-watch", target ? { project: target } : { key: "user" });
    memWatching = true;
  }

  function memProjectOptions() {
    const opts = [mem.user].concat(mem.projects);
    if (mem.key && mem.key !== "user" && !mem.projects.some((p) => p.key === mem.key)) {
      opts.push({ key: mem.key, label: mem.key }); // a store the picker list has not caught up to
    }
    return opts.map((o) => '<option value="' + esc(o.key) + '"' +
      (o.key === mem.key ? " selected" : "") + ">" + esc(o.label) + "</option>").join("");
  }

  function renderMemPicker() {
    const sel = $("mem-project");
    if (sel) sel.innerHTML = memProjectOptions();
  }

  function memFact(name) { return mem.facts.find((f) => f.name === name); }
  function memKeyFor(f) { return f && f.shared ? "user" : mem.key; } // a shared fact lives in the user store

  function renderMemoryList() {
    const list = $("mem-list");
    if (!list) return;
    const btn = $("mem-promoted");
    if (btn) { btn.classList.toggle("on", mem.promotedOnly); btn.setAttribute("aria-pressed", mem.promotedOnly ? "true" : "false"); }
    // Counts come with a sidecar that keeps them; without, no Unused.
    const counted = !!mem.usesSince;
    if (!counted) mem.unusedOnly = false;
    const ub = $("mem-unused");
    if (ub) {
      ub.hidden = !counted;
      ub.classList.toggle("on", mem.unusedOnly);
      ub.setAttribute("aria-pressed", mem.unusedOnly ? "true" : "false");
      ub.title = "Show the facts no ask or recall has used " + memWindow();
    }
    document.querySelectorAll('[data-act="mem-sort"]').forEach((b) => {
      const on = b.getAttribute("data-sort") === mem.sort;
      b.classList.toggle("on", on);
      b.setAttribute("aria-pressed", on ? "true" : "false");
    });
    if (!mem.facts.length) { list.innerHTML = '<p class="empty">No memory here yet.</p>'; return; }
    const needles = mem.filter.toLowerCase().split(/\s+/).filter(Boolean);
    const shown = memSorted(mem.facts).filter((f) => {
      // A promoted fact lives in an instruction file now: it waits behind the
      // "Promoted" toggle, out of the everyday list.
      if (mem.promotedOnly ? !f.promoted : f.promoted) return false;
      if (mem.unusedOnly && !(f.uses && f.uses.used === 0)) return false;
      const hay = (f.name + "\n" + f.description + "\n" + f.body).toLowerCase();
      return needles.every((n) => hay.includes(n));
    });
    // Since when uses are counted: a fact used before has no history here.
    const head = counted
      ? '<p class="musehead">Uses: asks and recalls ' + esc(memWindow()) +
        (memYoung() ? " — counting began then, so earlier uses are not known" : "") +
        "; a rule given in full as an agent starts is shown apart.</p>"
      : "";
    list.innerHTML = head + (shown.length
      ? shown.map(renderMemRow).join("")
      : '<p class="empty">' + (mem.promotedOnly ? "Nothing promoted." :
          mem.unusedOnly && !needles.length ? "Every fact here was used " + esc(memWindow()) + "." : "Nothing matches.") + "</p>");
  }

  // The window a fact's uses are counted in: the last 30 days, or -- while
  // the herd has counted for less -- since it began.
  function memYoung() {
    const since = stamp(mem.usesSince);
    return !!since && Date.now() - since < (mem.usesDays || 30) * 86400e3;
  }

  function memDay(v) {
    const t = stamp(v);
    if (!t) return "";
    const d = new Date(t);
    const opts = { month: "short", day: "numeric" };
    if (d.getFullYear() !== new Date().getFullYear()) opts.year = "numeric";
    return d.toLocaleDateString(undefined, opts);
  }

  function memWindow() {
    return memYoung() ? "since " + memDay(mem.usesSince) : "in " + (mem.usesDays || 30) + " days";
  }

  // A fact's uses, honestly: how many asks and recalls in the window and
  // when the last was; none -- since counting began, or since the fact was
  // written, when that is later -- never a bare zero that would claim a
  // history the herd does not have.  A rule's openings apart.
  function memUses(f) {
    const u = f.uses;
    if (!u || !mem.usesSince) return "";
    const parts = [];
    if (u.used) {
      const kinds = [u.ask ? u.ask + (u.ask === 1 ? " ask" : " asks") : "",
                     u.recall ? u.recall + (u.recall === 1 ? " recall" : " recalls") : ""].filter(Boolean);
      parts.push("used " + u.used + "× " + memWindow() + " (" + kinds.join(", ") + ")" +
                 (u.last ? " · last " + when(u.last) : ""));
    } else if (u.last) {
      parts.push("no use " + memWindow() + " · last used " + memDay(u.last));
    } else {
      // Written inside the window: its zero covers only its own days.
      const from = Math.max(stamp(mem.usesSince) || 0, Date.now() - (mem.usesDays || 30) * 86400e3);
      const born = stamp(f.created) && stamp(f.created) > from ? f.created : null;
      parts.push(born ? "no use since it was written, " + memDay(born)
                      : memYoung() ? "no use since " + memDay(mem.usesSince) + ", when counting began"
                      : "no use " + memWindow());
    }
    if (u.opening) parts.push("given in full at " + u.opening + (u.opening === 1 ? " start" : " starts"));
    return '<div class="muse' + (u.used ? "" : " none") + '">' + esc(parts.join(" · ")) + "</div>";
  }

  // The facts in the order the viewer chose: newest change first (updated,
  // else created), so what the team just learned is on top, a fact with
  // neither date last; or by the name the list shows.  Shared facts sort in
  // with the rest.  Only the view: the server's order is by file name, and
  // MEMORY.md is made from it.
  function memSorted(facts) {
    const title = (f) => f.title || f.name || "";
    const byName = (a, b) =>
      title(a).localeCompare(title(b), undefined, { sensitivity: "base" }) ||
      (a.name || "").localeCompare(b.name || "");
    if (mem.sort === "name") return facts.slice().sort(byName);
    if (mem.sort === "used") {
      const used = (f) => (f.uses && f.uses.used) || 0;
      const last = (f) => (f.uses && stamp(f.uses.last)) || 0;
      return facts.slice().sort((a, b) => used(b) - used(a) || last(b) - last(a) || byName(a, b));
    }
    const at = (f) => stamp(f.updated) || stamp(f.created) || 0;
    return facts.slice().sort((a, b) => at(b) - at(a) || byName(a, b));
  }

  function renderMemRow(f) {
    const open = mem.open.has(f.name);
    const badge = '<span class="mtype t-' + esc(f.type) + '">' + esc(f.type) + "</span>";
    const where = f.shared ? '<span class="mshared" title="Shared by every project">shared</span>' : "";
    // Its last change -- created, when never updated: the date the list sorts
    // by.  The page's usual relative style: "1h ago", a date when older.
    const upd = when(f.updated || f.created);
    const meta = (f.author ? "by " + esc(f.author) : "") + (f.author && upd ? " · " : "") + (upd ? esc(upd) : "");
    const prom = f.promoted
      ? '<span class="mprom" title="In an instruction file now">promoted → ' + esc(f.promoted) + "</span>" : "";
    const ck = !f.shared && (mem.checks || []).find((c) => c.fact === f.name);
    const check = ck
      ? '<span class="mcheck' + (ck.status === "on" ? "" : " off") + '" title="' +
        (ck.status === "on" ? "A check denies shell calls this fact warns of" : "Its check is off") + '">check' +
        (ck.status === "on" ? "" : " off") + "</span>" : "";
    const head =
      '<div class="mhead" data-act="mem-open" data-name="' + esc(f.name) + '" tabindex="0" role="button">' +
      '<div class="mtop"><span class="mname">' + esc(f.title || f.name) + "</span>" + badge + where + prom + check +
      '<span class="mmeta">' + meta + "</span></div>" +
      (f.description ? '<div class="mdesc">' + esc(f.description) + "</div>" : "") + memUses(f) + "</div>";
    if (!open) return '<div class="mfact' + (f.promoted ? " promoted" : "") + '">' + head + "</div>";
    if (mem.editing === f.name) return '<div class="mfact open editing">' + head + memEditForm(f) + "</div>";
    const confirming = mem.confirmDel === f.name;
    // A promoted fact's one action is to put it back; otherwise edit/delete.
    const actions = f.promoted
      ? '<div class="macts"><button type="button" class="ghost small" data-act="mem-unpromote" data-name="' +
        esc(f.name) + '">Put back in memory</button></div>'
      : '<div class="macts">' +
        '<button type="button" class="ghost small" data-act="mem-edit" data-name="' + esc(f.name) + '">Edit</button>' +
        (confirming
          ? '<span class="mconfirm">Delete this fact?</span>' +
            '<button type="button" class="ghost small danger" data-act="mem-delete" data-name="' + esc(f.name) + '">Delete</button>' +
            '<button type="button" class="ghost small" data-act="mem-delcancel" data-name="' + esc(f.name) + '">Keep</button>'
          : '<button type="button" class="ghost small" data-act="mem-delask" data-name="' + esc(f.name) + '">Delete</button>') +
        "</div>";
    // What it said before, one line each, newest first (the sidecar's
    // .history/): when, how, by whom, its first line then.
    const hist = f.history && f.history.length
      ? '<div class="mhist"><div class="mhist-h">Earlier versions</div>' +
        f.history.map((h) => '<div class="mhist-i">' + esc(h) + "</div>").join("") + "</div>"
      : "";
    return '<div class="mfact open' + (f.promoted ? " promoted" : "") + '">' + head + '<div class="mbody">' + markdown(f.body) + "</div>" + hist + actions + "</div>";
  }

  // The pending promotions the user decides on: the facts, the target file,
  // the exact diff, Approve / Reject.  Only pending ones are shown.
  function renderMemProposals() {
    const box = $("mem-proposals");
    if (!box) return;
    const pend = (mem.proposals || []).filter((p) => p.status === "pending");
    if (!pend.length) { box.innerHTML = ""; return; }
    box.innerHTML = '<div class="mprops"><h3 class="mprops-h">Proposals</h3>' +
      pend.map(renderMemProposal).join("") + "</div>";
  }

  // A unified diff, coloured by each line's FIRST character: an added line
  // (+) green, a removed line (-) red -- so "+- Never commit…", an added
  // bullet, is green -- the file headers (---/+++) and hunk headers (@@)
  // muted, context plain.  Theme tokens only; wrapping/scrolling is the
  // block's (.mpdiff), so a long line never widens the page.
  function diffHtml(diff) {
    return (diff || "").split("\n").map((line) => {
      const cls = line.startsWith("+++") || line.startsWith("---") ? "dhead"
        : line.startsWith("@@") ? "dhunk"
        : line[0] === "+" ? "dadd"
        : line[0] === "-" ? "ddel"
        : "";
      return cls ? '<span class="' + cls + '">' + esc(line) + "</span>" : esc(line);
    }).join("\n");
  }

  // What a check does, from its own examples: the calls it denies and
  // those it lets through -- the proposal's promise, and the rule's.
  function checkExamples(r) {
    const list = (xs, cls, mark) => (xs || []).map((c) =>
      '<li class="' + cls + '"><span class="ckmark">' + mark + "</span><code>" + esc(c) + "</code></li>").join("");
    return '<div class="ckex"><div class="ckexh">Denies</div><ul>' + list(r.catches, "ckno", "✕") + "</ul>" +
      '<div class="ckexh">Lets through</div><ul>' + list(r.passes, "ckok", "✓") + "</ul></div>";
  }

  // Its patterns, view by view (bin/herd_lint.py): every `all' must match,
  // no `none' may.
  function checkPatterns(check) {
    const views = { command: "one command", code: "code run by an interpreter", line: "the whole line" };
    return '<div class="ckpats">' + Object.keys(check || {}).map((v) => {
      const spec = check[v] || {};
      const pats = (xs, how) => (xs || []).map((x) =>
        '<div class="ckpat"><span class="ckhow">' + how + '</span><code>' + esc(x) + "</code></div>").join("");
      return '<div class="ckview"><div class="ckvh">' + esc(views[v] || v) + "</div>" +
        pats(spec.all, "matches") + pats(spec.none, "but not") + "</div>";
    }).join("") + "</div>";
  }

  // A proposal's buttons: APPROVE (its own words), Reject -- or, once
  // Reject is pressed, why, and Reject / Keep.
  function propActs(p, approve) {
    return mem.rejecting === p.id
      ? '<div class="mpreject"><input id="mem-prej" type="text" placeholder="Why? (optional)" value="">' +
        '<button type="button" class="ghost small danger" data-act="prop-reject-send" data-id="' + esc(p.id) + '">Reject</button>' +
        '<button type="button" class="ghost small" data-act="prop-reject-cancel" data-id="' + esc(p.id) + '">Keep</button></div>'
      : '<div class="mpacts">' +
        '<button type="button" class="primary small" data-act="prop-approve" data-id="' + esc(p.id) + '">' + esc(approve) + "</button>" +
        '<button type="button" class="ghost small" data-act="prop-reject" data-id="' + esc(p.id) + '">Reject</button></div>';
  }

  function renderMemProposal(p) {
    if (p.kind === "lint" && p.rule) return renderCheckProposal(p);
    if (p.kind === "share") return renderShareProposal(p);
    const facts = (p.facts || []).map((f) =>
      '<span class="mpfact' + (f.gone ? " gone" : "") + '" title="' + esc(f.name) + '">' +
      esc(f.title || f.name) + "</span>").join("");
    const acts = propActs(p, "Approve");
    return '<div class="mprop">' +
      '<div class="mptop"><span class="mpfacts">' + facts + '</span>' +
      '<span class="mpinto">→ ' + esc(p.file) + "</span>" +
      '<span class="mpby">' + (p.author ? "by " + esc(p.author) : "") + "</span></div>" +
      '<pre class="mpdiff">' + diffHtml(p.diff) + "</pre>" + acts + "</div>";
  }

  function renderCheckProposal(p) {
    const f = (p.facts || [])[0] || {};
    const acts = propActs(p, "Turn the check on");
    return '<div class="mprop ckprop">' +
      '<div class="mptop"><span class="mpfacts"><span class="mpfact' + (f.gone ? " gone" : "") + '" title="' +
      esc(f.name || "") + '">' + esc(f.title || f.name || "") + "</span></span>" +
      '<span class="mpinto">→ a check before every shell call</span>' +
      '<span class="mpby">' + (p.author ? "by " + esc(p.author) : "") + "</span></div>" +
      '<div class="ckhint">' + esc(p.rule.hint || "") + "</div>" +
      checkExamples(p.rule) +
      '<details class="ckmore"><summary>Its patterns</summary>' + checkPatterns(p.rule.check) + "</details>" +
      acts + "</div>";
  }

  // What a fact moving to every project takes with it: its check, on or
  // off as it is, and its counts.
  function shareCheck(check) {
    return '<div class="shcheck">' + (check
      ? "Its check moves too, and stays " + esc(check) + ". Its counts go with it."
      : "Its counts go with it.") + "</div>";
  }

  // A project fact to make every project's: its text as proposed, and what
  // goes with it.  Approved, the herd moves it at once -- nobody applies it.
  function renderShareProposal(p) {
    const f = (p.facts || [])[0] || {};
    return '<div class="mprop shprop">' +
      '<div class="mptop"><span class="mpfacts"><span class="mpfact' + (f.gone ? " gone" : "") + '" title="' +
      esc(f.name || "") + '">' + esc(f.title || f.name || "") + "</span></span>" +
      '<span class="mpinto">→ every project (the user store)</span>' +
      '<span class="mpby">' + (p.author ? "by " + esc(p.author) : "") + "</span></div>" +
      '<div class="shtext msg-body">' + markdown(p.diff || "") + "</div>" + shareCheck(f.check) +
      propActs(p, "Move to every project") + "</div>";
  }

  // The store's checks: each one's fact, on or off, how many calls it
  // denied and the last of them -- where a false positive shows -- and
  // Turn off / Turn on / Remove.
  function renderMemChecks() {
    const box = $("mem-checks");
    if (!box) return;
    const cs = mem.checks || [];
    if (!cs.length) { box.innerHTML = ""; return; }
    box.innerHTML = '<div class="mchecks"><h3 class="mprops-h">Checks</h3>' + cs.map(renderMemCheck).join("") + "</div>";
  }

  function renderMemCheck(c) {
    const on = c.status === "on";
    const open = mem.checkOpen === c.fact;
    const n = c.count || 0;
    const denied = n
      ? "denied " + n + (n === 1 ? " call" : " calls") +
        (c.last_ms ? " · last " + esc(when(c.last_ms / 1000)) : "") +
        (c.last_session ? " by " + esc(c.last_session) : "")
      : "denied nothing yet";
    const confirming = mem.confirmRm === c.fact;
    const acts = '<div class="mpacts">' +
      '<button type="button" class="' + (on ? "ghost" : "primary") + ' small" data-act="check-' + (on ? "off" : "on") +
      '" data-fact="' + esc(c.fact) + '">' + (on ? "Turn off" : "Turn on") + "</button>" +
      (confirming
        ? '<span class="mconfirm">Remove this check?</span>' +
          '<button type="button" class="ghost small danger" data-act="check-rm" data-fact="' + esc(c.fact) + '">Remove</button>' +
          '<button type="button" class="ghost small" data-act="check-rm-cancel" data-fact="' + esc(c.fact) + '">Keep</button>'
        : '<button type="button" class="ghost small" data-act="check-rm-ask" data-fact="' + esc(c.fact) + '">Remove</button>') +
      "</div>";
    return '<div class="mprop mcheckrow' + (on ? "" : " off") + '">' +
      '<div class="mptop" data-act="check-open" data-fact="' + esc(c.fact) + '" tabindex="0" role="button">' +
      '<span class="mpfacts"><span class="mpfact' + (c.gone ? " gone" : "") + '" title="' +
      esc(c.gone ? "This fact is gone from the memory; the check stays until you remove it" : c.fact) + '">' +
      esc(c.title || c.fact) + "</span></span>" +
      '<span class="ckstate ' + (on ? "on" : "off") + '">' + (on ? "on" : "off") + "</span>" +
      '<span class="mpby">' + denied + "</span></div>" +
      (n && c.last_command ? '<div class="cklast" title="The last call it denied">' + esc(c.last_command) + "</div>" : "") +
      (open ? '<div class="ckhint">' + esc(c.hint || "") + "</div>" + checkExamples(c) +
              '<details class="ckmore"><summary>Its patterns</summary>' + checkPatterns(c.check) + "</details>" : "") +
      acts + "</div>";
  }

  function memEditForm(f) {
    const types = ["project", "feedback", "reference", "user"];
    return '<div class="medit">' +
      '<input id="mem-edit-desc" type="text" value="' + esc(f.description) + '" placeholder="Description">' +
      '<select id="mem-edit-type" title="Type">' + types.map((t) =>
        '<option value="' + t + '"' + (t === f.type ? " selected" : "") + ">" + t + "</option>").join("") + "</select>" +
      '<textarea id="mem-edit-body" rows="8" spellcheck="false">' + esc(f.body) + "</textarea>" +
      '<div class="macts">' +
      '<button type="button" class="primary small" data-act="mem-save" data-name="' + esc(f.name) + '">Save</button>' +
      '<button type="button" class="ghost small" data-act="mem-editcancel" data-name="' + esc(f.name) + '">Cancel</button>' +
      "</div></div>";
  }

  app.addEventListener("input", (ev) => {
    const ta = ev.target instanceof Element && ev.target.closest(".bdreply textarea");
    if (ta) board.replyText[ta.getAttribute("data-jid")] = ta.value;
  });

  app.addEventListener("click", (ev) => {
    const t = ev.target instanceof Element ? ev.target : null;
    if (!t) return;
    // A link would navigate the xwidget off the page and its bridge, so
    // Emacs opens it.  On another device it opens there, in a tab of its
    // own: the page stays.
    const a = t.closest("a[href]");
    if (a) {
      // A document Emacs serves: its own tab, opened by the link itself.
      if (a.hasAttribute("data-doc")) return;
      const href = a.getAttribute("href") || "";
      if (XW.elsewhere && /^https?:/i.test(href)) {
        a.target = "_blank";
        a.rel = "noopener noreferrer";
        return;
      }
      // Emacs's page for a pull request or task: a plain click reuses the
      // tab already showing that app and brings it forward, instead of a new
      // tab each time; a modified or middle click still opens a new tab.
      if (XW.elsewhere && /^\/open\//.test(href)) {
        const appTab = a.getAttribute("data-app");
        if (appTab && typeof XW.openApp === "function" &&
            ev.button === 0 && !ev.metaKey && !ev.ctrlKey && !ev.shiftKey && !ev.altKey) {
          ev.preventDefault();
          XW.openApp(href, appTab);
        }
        return;
      }
      ev.preventDefault();
      if (/^https?:/i.test(href)) emit("open-browser", { url: href });
      return;
    }
    const el = t.closest("[data-act]");
    if (!el || el.disabled) return;
    const act = el.getAttribute("data-act");
    if (act === "img:open") {
      openImage(el);
    } else if (act === "doc:open") {
      emit("doc-open", { set: el.getAttribute("data-set"), name: el.getAttribute("data-name") });
    } else if (act === "docs:src") {
      docsView.src = el.getAttribute("data-src") || "";
      renderDocsList();
    } else if (act === "reports:older") {
      const rep = reports && reports.list.find((x) => x.id === el.getAttribute("data-report"));
      if (rep) {
        const k = reportKey(rep);
        if (reportsOpen.has(k)) reportsOpen.delete(k);
        else reportsOpen.add(k);
        renderDocs();
      }
    } else if (act === "reports:more") {
      const last = reports && reports.list[reports.list.length - 1];
      if (last && !reports.asking) {
        reports.asking = true;
        renderDocs();
        emit("reports-more", { at: last.at, id: last.id });
      }
    } else if (act === "agent") {
      filters.agent = el.getAttribute("data-agent") || "";
      onFilters();
    } else if (act === "open-hit") {
      const h = hits && hits.items[+el.getAttribute("data-i")];
      if (h) {
        el.focus({ preventScroll: true });
        openSource(h.source_path, h.chunk_index, termRe(hits.q));
      }
    } else if (act === "open-source") {
      el.focus({ preventScroll: true });
      openSource(el.getAttribute("data-path"), null, null);
    } else if (act === "more-hits") {
      limit = Math.min(120, limit + 30);
      search(true);
    } else if (act === "earlier" || act === "later") {
      loadMore(act);
    } else if (act === "match-next") {
      jumpMatch(1);
    } else if (act === "match-prev") {
      jumpMatch(-1);
    } else if (act === "copy-msg") {
      const sec = el.closest(".msg");
      const m = src && src.messages.find((x) => String(x.first) === sec.getAttribute("data-first"));
      if (m) copy(m.text);
    } else if (act === "copy-path") {
      if (src) copy(src.path);
    } else if (act.startsWith("a:")) {
      onAgentAction(act.slice(2), el);
    } else if (act.startsWith("ib:")) {
      onInboxAction(act.slice(3), el);
    } else if (act.startsWith("th:")) {
      onThreadsAction(act.slice(3), el);
    } else if (act.startsWith("bd:")) {
      onBoardAction(act.slice(3), el);
    } else if (act === "resume") {
      resume(false);
    } else if (act === "fork") {
      resume(true);
    } else if (act === "visit-agent") {
      if (src) emit("visit-agent", { source_path: src.path });
    } else if (act === "range") {
      const ms = +el.getAttribute("data-range");
      if (herd.zoom) unzoom(ms);
      else {
        herd.range = ms;
        renderLog();
      }
    } else if (act === "issues-only") {
      herd.issues = !herd.issues;
      herd.shown = LOGCAP;
      renderLog();
    } else if (act === "screens-only") {
      herd.screensOnly = !herd.screensOnly;
      herd.shown = LOGCAP;
      renderLog();
    } else if (act === "log-session") {
      herd.session = herd.session === el.getAttribute("data-session") ? "" : el.getAttribute("data-session");
      herd.shown = LOGCAP;
      renderLog();
    } else if (act === "screen") {
      const id = +el.getAttribute("data-id");
      if (herd.open.has(id)) herd.open.delete(id);
      else herd.open.add(id);
      renderLogList();
    } else if (act === "log-more") {
      herd.shown += LOGCAP;
      renderLogList();
    } else if (act === "log-pin" || act === "log-unpin") {
      emit("log-pin", { id: +el.getAttribute("data-id"), pinned: act === "log-pin" });
    } else if (act === "log-delete") {
      emit("log-delete", { id: +el.getAttribute("data-id") });
    } else if (act === "seg") {
      goToEntry(+el.getAttribute("data-id"));
    } else if (act === "log-zoom") {
      zoomTo(+el.getAttribute("data-id"));
    } else if (act === "log-unzoom") {
      unzoom();
    } else if (act === "room-memory") {
      lastOpened = null;
      showMemory({ project: herdNow.room });
    } else if (act === "mem-open") {
      const n = el.getAttribute("data-name");
      if (mem.open.has(n)) mem.open.delete(n);
      else mem.open.add(n);
      mem.confirmDel = "";
      renderMemoryList();
    } else if (act === "mem-edit") {
      mem.editing = el.getAttribute("data-name");
      mem.confirmDel = "";
      renderMemoryList();
      const d = $("mem-edit-desc");
      if (d) d.focus();
    } else if (act === "mem-editcancel") {
      mem.editing = "";
      renderMemoryList();
    } else if (act === "mem-save") {
      const n = el.getAttribute("data-name");
      const f = memFact(n);
      const desc = $("mem-edit-desc"), type = $("mem-edit-type"), body = $("mem-edit-body");
      emit("memory-edit", { key: memKeyFor(f), name: n,
        description: desc ? desc.value : "", type: type ? type.value : "project",
        body: body ? body.value : "" });
      mem.editing = "";
      renderMemoryList();
    } else if (act === "mem-delask") {
      mem.confirmDel = el.getAttribute("data-name");
      renderMemoryList();
    } else if (act === "mem-delcancel") {
      mem.confirmDel = "";
      renderMemoryList();
    } else if (act === "mem-delete") {
      const n = el.getAttribute("data-name");
      const f = memFact(n);
      emit("memory-delete", { key: memKeyFor(f), name: n });
      mem.confirmDel = ""; mem.open.delete(n);
    } else if (act === "mem-unpromote") {
      const n = el.getAttribute("data-name");
      const f = memFact(n);
      emit("memory-unpromote", { key: memKeyFor(f), name: n });
    } else if (act === "mem-unused-toggle") {
      mem.unusedOnly = !mem.unusedOnly;
      mem.open = new Set(); mem.editing = ""; mem.confirmDel = "";
      renderMemoryList();
    } else if (act === "mem-promoted-toggle") {
      mem.promotedOnly = !mem.promotedOnly;
      mem.open = new Set(); mem.editing = ""; mem.confirmDel = "";
      renderMemoryList();
    } else if (act === "mem-sort") {
      const want = el.getAttribute("data-sort");
      mem.sort = want === "name" || want === "used" ? want : "recent";
      memSortKeep(mem.sort);
      renderMemoryList();
    } else if (act === "mem-suggest") {
      emit("memory-suggest", mem.key && mem.key.indexOf("/") >= 0
        ? { project: mem.key } : { key: mem.key || "user" });
    } else if (act === "check-open") {
      const fct = el.getAttribute("data-fact");
      mem.checkOpen = mem.checkOpen === fct ? "" : fct;
      renderMemChecks();
    } else if (act === "check-off" || act === "check-on") {
      emit("lint-set", { key: mem.key || "user", fact: el.getAttribute("data-fact"),
                         status: act === "check-on" ? "on" : "off" });
    } else if (act === "check-rm-ask") {
      mem.confirmRm = el.getAttribute("data-fact");
      renderMemChecks();
    } else if (act === "check-rm-cancel") {
      mem.confirmRm = "";
      renderMemChecks();
    } else if (act === "check-rm") {
      emit("lint-remove", { key: mem.key || "user", fact: el.getAttribute("data-fact") });
      mem.confirmRm = "";
    } else if (act === "prop-approve") {
      emit("memory-proposal-approve", { id: el.getAttribute("data-id") });
    } else if (act === "prop-reject") {
      mem.rejecting = el.getAttribute("data-id");
      renderMemProposals();
      const r = $("mem-prej"); if (r) r.focus();
    } else if (act === "prop-reject-cancel") {
      mem.rejecting = "";
      renderMemProposals();
    } else if (act === "prop-reject-send") {
      const r = $("mem-prej");
      emit("memory-proposal-reject", { id: el.getAttribute("data-id"), reason: r ? r.value : "" });
      mem.rejecting = "";
    }
  });

  document.querySelectorAll(".tab").forEach((b) =>
    b.addEventListener("click", () => {
      lastOpened = null;
      src = null;
      herdNow.fromBoard = false;
      const which = b.getAttribute("data-tab");
      if (which === "inbox") showInbox();
      else if (which === "board") showBoard();
      else if (which === "threads") showThreads();
      else if (which === "sessions") showSessions();
      else if (which === "log") showLog();
      else if (which === "agents") showAgents();
      else if (which === "memory") showMemory();
      else if (which === "docs") showDocs();
      else showSearch();
    }));
  btnBack.addEventListener("click", back);
  $("btn-refresh").addEventListener("click", () => {
    if (view === "log") emit("log");
    else if (view === "agents") emit("usage-refresh");
    else if (view === "docs") emit("docs");
    else if (view === "board") emit("board-resend");
    else if (listsSessions()) askSessions();
    else emit("refresh", { lazy: true });
  });
  $("btn-new").addEventListener("click", newAgentAnywhere);
  $("btn-import").addEventListener("click", () => {
    $("btn-import").disabled = true;
    emit("import");
  });

  // ---- The top bar, narrow ---------------------------------------------
  // style.css folds Phone, Import and Refresh behind ⋯ when the bar is
  // short of room, and lets the tabs scroll sideways if they still do
  // not fit.  ⋯ opens them below it; anything pressed, or Esc, shuts it.
  const moreEl = $("more");
  const btnMore = $("btn-more");
  function moreOpen(open) {
    moreEl.classList.toggle("open", open);
    btnMore.setAttribute("aria-expanded", String(open));
  }
  btnMore.addEventListener("click", () => moreOpen(!moreEl.classList.contains("open")));
  document.addEventListener("click", (ev) => {
    if (moreEl.classList.contains("open") && !btnMore.contains(ev.target)) moreOpen(false);
  });
  // The open tab, scrolled into the bar's view.
  function tabInView() {
    const on = tabsEl.querySelector(".tab.on");
    if (!on || tabsEl.scrollWidth <= tabsEl.clientWidth) return;
    const t = on.getBoundingClientRect();
    const b = tabsEl.getBoundingClientRect();
    if (t.left < b.left) tabsEl.scrollLeft -= b.left - t.left;
    else if (t.right > b.right) tabsEl.scrollLeft += t.right - b.right;
  }
  window.addEventListener("resize", tabInView);
  // A mouse's wheel turns tabs that scroll, as a trackpad swipes them.
  tabsEl.addEventListener("wheel", (ev) => {
    if (tabsEl.scrollWidth <= tabsEl.clientWidth || Math.abs(ev.deltaX) >= Math.abs(ev.deltaY)) return;
    tabsEl.scrollLeft += ev.deltaY;
    ev.preventDefault();
  }, { passive: false });

  document.addEventListener("keydown", (ev) => {
    const t = ev.target;
    const inInput = t && /^(INPUT|SELECT|TEXTAREA)$/.test(t.tagName);
    if (ev.metaKey || ev.ctrlKey || ev.altKey) return;
    if (ev.key === "Escape" && moreEl.classList.contains("open")) {
      moreOpen(false);
      ev.preventDefault();
      return;
    }
    if (inInput) {
      if (ev.key === "Escape") {
        if (t.value && t.tagName === "INPUT") {
          t.value = "";
          t.dispatchEvent(new Event("input", { bubbles: true }));
        } else t.blur();
        ev.preventDefault();
      } else if (ev.key === "ArrowDown" && t.tagName === "INPUT") {
        const c = cards()[0];
        if (c) {
          c.focus();
          ev.preventDefault();
        }
      } else if (ev.key === "Enter" && t.id === "q") {
        search(true);
      }
      return;
    }
    if (ev.key === "/" && view === "agents" && herdNow.room) {
      const key = defaultFindKey();
      if (key) { openFind(key); ev.preventDefault(); }
    } else if (ev.key === "Escape" && view === "agents" && herdNow.room && !herdNow.subView && findEscape()) {
      ev.preventDefault();
    } else if (ev.key === "/" && view !== "source") {
      const input = $("q") || $("sess-filter") || $("log-filter") || $("docs-filter") || $("mem-filter");
      if (input) {
        input.focus();
        ev.preventDefault();
      }
    } else if (ev.key === "Escape" && (view === "source" || (view === "agents" && herdNow.room))) {
      back();
      ev.preventDefault();
    } else if (ev.key === "Escape" && view === "log" && herd.zoom) {
      unzoom();
      ev.preventDefault();
    } else if ((ev.key === "n" || ev.key === "N") && view === "source") {
      jumpMatch(ev.key === "n" ? 1 : -1);
    } else if (ev.key === "ArrowDown" || ev.key === "ArrowUp") {
      const cs = cards();
      if (!cs.length || view === "source") return;
      const i = cs.indexOf(document.activeElement);
      if (ev.key === "ArrowUp" && i <= 0) {
        const input = $("q") || $("sess-filter");
        if (input) input.focus();
      } else {
        const next = cs[Math.max(0, Math.min(cs.length - 1, i + (ev.key === "ArrowDown" ? 1 : -1)))];
        next.focus();
        next.scrollIntoView({ block: "nearest" });
      }
      ev.preventDefault();
    } else if (ev.key === "Enter" && document.activeElement && document.activeElement.classList.contains("card")) {
      document.activeElement.click();
      ev.preventDefault();
    }
  });

  // ---- Calls from Emacs -------------------------------------------------

  // ---- Inbox -----------------------------------------------------------
  // One list of what needs the user, then updates.  Emacs pushes the items
  // (inbox-watch / renderInbox), derived from the herd's own state; each
  // action routes to the same intent the Agents / Memory tabs use, plus
  // inbox-set for read / done / override.  Desk: a list and an in-place
  // handler.  Phone: cards with their actions inline.

  // REPLYING, the report whose reply box is open: a report stays compact
  // until Reply is pressed.  BACK, by item, a reply that did not reach
  // Emacs, given back to its box until sent or cancelled; HELD, by item,
  // one given back, kept in the list a while ({it, until}).
  // GOT: Emacs has sent the Inbox once; until then it is on its way, not empty.
  // GONE, by item, one handled here until a while after (INBOX_HOLD): the
  // Board's list is drawn from the Board's push too, which has it until
  // its next look.
  let inbox = { got: false, items: [], count: 0, sel: "", filter: "all", toast: "", reply: "", replying: "", undo: null, detail: null, thOpen: {},
                back: new Map(), held: new Map(), gone: new Map() };
  let inboxWatching = false;

  const INBOX_FILTERS = [["all", "All"], ["stuck", "Stuck"], ["approve", "Approvals"], ["questions", "Questions"], ["updates", "Updates"]];

  function inboxNarrow() { return window.innerWidth < 720; }

  // Redraw BOX by DRAW, each pane in SELS kept where it was scrolled --
  // Emacs pushes every few seconds -- but those in TOP, which start at
  // their top (the reading pane, a new item picked).
  function keepScroll(box, sels, draw, top) {
    const at = sels.map((sel) => { const el = box.querySelector(sel); return el ? el.scrollTop : 0; });
    draw();
    sels.forEach((sel, i) => {
      const el = box.querySelector(sel);
      if (el) el.scrollTop = top.includes(sel) ? 0 : at[i];
    });
  }

  // An item's time: epoch-ms as Emacs sends it (ghostherd-inbox--ms) -- not
  // seconds, as `stamp' takes a number -- or a date's words.
  function inboxMs(at) { return typeof at === "number" ? (at > 0 ? at : null) : stamp(at); }
  function inboxAge(at) {
    const t = inboxMs(at);
    if (!t) return "";
    const s = Math.max(0, (Date.now() - t) / 1000);
    if (s < 60) return "now";
    if (s < 3600) return Math.floor(s / 60) + "m";
    if (s < 86400) return Math.floor(s / 3600) + "h";
    return Math.floor(s / 86400) + "d";
  }
  function inboxNeed(it) {
    const o = it.override;
    return o === "need" ? true : o === "update" ? false : it.group === "need";
  }
  function inboxPass(it) {
    const f = inbox.filter;
    if (f === "all") return true;
    if (f === "stuck") return it.kind === "blocked" || it.kind === "login" || it.kind === "login-continue" || it.kind === "carry-on" || it.kind === "ask-failed" || it.kind === "unsent";
    if (f === "approve") return it.kind === "guard" || it.kind === "proposal";
    if (f === "questions") return it.kind === "question";
    if (f === "updates") return !inboxNeed(it);
    return true;
  }
  function inboxCur() { return inbox.items.find((x) => x.id === inbox.sel) || null; }

  function showInbox() {
    if (herdNow.room) closeRoomState();
    if (/^orphan:/.test(inbox.sel)) inbox.sel = "";
    view = tab = "inbox";
    src = null;
    chrome();
    app.innerHTML = '<div class="page wide inboxpage"></div>';
    inboxWatching = true;
    emit("inbox-watch");
    renderInbox();
  }

  // Room state torn down without a view change (showInbox leaves agents).
  function closeRoomState() {
    if (!herdNow.room) return;
    roomLeft();
    herdNow.room = null;
    if (herdNow.subView) { herdNow.subView = null; herdNow.subConv = null; emit("sub-unwatch"); }
    emit("room-close");
  }

  let inboxShown = null; // the item the detail pane shows, to start a new one at its top
  // An IME composition open in a reply box (CJK input): a redraw would
  // end it, so one asked for meanwhile waits for it to end (HELD).
  let inboxComposing = false;
  let inboxHeld = false;

  // A box sent or closed: its composition no longer holds the Inbox, and
  // the redraw that clears the box is drawn now -- held, it would be drawn
  // when the composition ends, after `composed' put the old text back.
  function inboxUnhold() {
    inboxComposing = false;
    inboxHeld = false;
  }

  function inboxReplyFocused() {
    const a = document.activeElement;
    return a && a.classList && a.classList.contains("ibreplybox") ? a : null;
  }

  // The whole pane is drawn anew -- every push, every few seconds while
  // agents work -- so a reply box being written in is found again after,
  // with its cursor, selection and scroll: else the next key falls to
  // `inboxKey' (`e' marks the item read and drops the draft).
  function renderInbox() {
    if (view === "board") return renderBoard();
    if (view !== "inbox") return;
    const box = app.querySelector(".inboxpage");
    if (!box) return;
    const ta = inboxReplyFocused();
    if (ta && inboxComposing) { inboxHeld = true; return; }
    inboxHeld = false;
    const kept = ta && box.contains(ta) && { id: ta.getAttribute("data-id"), start: ta.selectionStart, end: ta.selectionEnd,
                                            dir: ta.selectionDirection, top: ta.scrollTop };
    const narrow = inboxNarrow();
    box.classList.toggle("narrow", narrow);
    const fresh = inbox.sel !== inboxShown;
    inboxShown = inbox.sel;
    keepScroll(box, [".iblistcol", ".ibdetail"], () => {
      box.innerHTML = narrow ? inboxPhoneHtml() : inboxDeskHtml();
    }, fresh ? [".ibdetail"] : []);
    const again = kept && box.querySelector('.ibreplybox[data-id="' + cssName(kept.id) + '"]');
    if (!again) return;
    again.focus({ preventScroll: true });
    again.setSelectionRange(kept.start, kept.end, kept.dir);
    again.scrollTop = kept.top;
  }

  function inboxTag(it) {
    return '<span class="ibtag t-' + esc(it.tone) + '">' + esc(it.tag) + "</span>";
  }
  function inboxCli(it) {
    return it.cli ? '<span class="ibcli c-' + esc(it.cli) + '">' + esc(it.cli) + "</span>" : "";
  }

  function inboxRow(it) {
    const on = it.id === inbox.sel;
    const need = inboxNeed(it);
    return '<button type="button" class="ibrow' + (on ? " on" : "") + '" data-act="ib:pick" data-id="' + esc(it.id) + '"' +
      ' aria-current="' + (on ? "true" : "false") + '">' +
      '<span class="ibr1"><span class="ibdot' + (it.read ? " read" : "") + ' t-' + esc(it.tone) + '"></span>' +
      inboxTag(it) + '<span class="ibtitle">' + ibPlain(it.title) + "</span>" +
      '<span class="ibage">' + esc(inboxAge(it.at)) + "</span></span>" +
      (need
        ? '<span class="ibr2">' + inboxCli(it) + '<span class="ibagent">' + esc(it.agent) + "</span>" +
          (it.project ? '<span class="ibproj">· ' + esc(leaf(it.project)) + "</span>" : "") + "</span>" +
          '<span class="ibprev">' + ibPlain(it.preview) + "</span>"
        : '<span class="ibprev">' + esc(it.agent) + " · " + ibPlain(it.preview) + "</span>") +
      "</button>";
  }

  // Said where the items go until Emacs has sent them: "Inbox zero" before
  // then says what is not so, until it is taken back a second later.
  const INBOX_COMING = '<p class="ibempty ibcoming">Asking Emacs…</p>';

  function inboxList() {
    const live = inbox.items.filter(inboxPass);
    const need = live.filter(inboxNeed);
    const fyi = live.filter((it) => !inboxNeed(it));
    const count = inbox.items.filter(inboxNeed).length;
    const filters = INBOX_FILTERS.map(([f, label]) =>
      '<button type="button" class="ibfilter' + (inbox.filter === f ? " on" : "") + '" data-act="ib:filter" data-f="' + f + '">' + label + "</button>").join("");
    return '<div class="ibfilters">' + filters + "</div>" +
      '<div class="ibsec"><h2 class="ibneed">Needs you</h2><span class="ibsub">' +
      count + " waiting · most urgent first</span></div>" +
      (need.length ? need.map(inboxRow).join("")
        : inbox.got ? '<p class="ibempty">Nothing needs you here. The herd is moving on its own.</p>' : INBOX_COMING) +
      '<div class="ibsec fyi"><h2>Updates</h2><span class="ibsub">reports and handoffs, nothing to decide</span></div>' +
      fyi.map(inboxRow).join("") +
      '<p class="ibhint">j / k move · 1–4 answer · a approve · e done</p>';
  }

  // BODY defaults to the item's own (truncated) body; the detail pane passes
  // the full report when Emacs has fetched it for the selected item.
  function inboxWhy(it, body, imgs) {
    if (body == null) body = it.body;
    return '<div class="ibwhy"><span class="ibwhylab">' + esc(it.whyLabel || "") + "</span>" +
      '<div class="ibbody msg-body">' + markdown(body || "") + "</div>" + imagesHtml(imgs) +
      (it.code ? '<code class="ibcode">' + esc(it.code) + "</code>" : "") +
      (it.note ? '<p class="ibnote">' + esc(it.note) + "</p>" : "") + "</div>";
  }

  // The actions for one item, shared by the desk detail and the phone card.
  function inboxActions(it, phone) {
    const d = esc(it.id);
    if (it.kind === "blocked") {
      return '<div class="ibopts">' + (it.options || []).map((o) =>
        '<button type="button" class="iboptbtn" data-act="ib:answer" data-id="' + d + '" data-n="' + esc(String(o.n)) + '">' +
        '<span class="ibn">' + esc(String(o.n)) + "</span>" + esc(o.label) + "</button>").join("") + "</div>";
    }
    if (it.kind === "guard") {
      return '<div class="ibacts"><button type="button" class="ibbtn ok" data-act="ib:guard-approve" data-id="' + d + '">Approve this call once</button>' +
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Keep it denied</button>' +
        (phone ? "" : '<span class="ibmeta">The agent is told and runs exactly this call again. It lapses in 30 min.</span>') + "</div>";
    }
    if (it.kind === "need") {
      return '<div class="ibacts"><button type="button" class="ibbtn ok" data-act="ib:need-done" data-id="' + d + '">已處理</button>' +
        (it.item ? '<button type="button" class="ibbtn" data-act="ib:need-board" data-id="' + d + '">在 Board 打開</button>' : "") +
        (phone ? "" : '<span class="ibmeta">A lead wrote it as 需要 Jing; handled, it leaves the Board too.</span>') + "</div>";
    }
    if (it.kind === "question") {
      return inboxReplyBox(it, "Write the answer it is waiting for",
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Answer later</button>');
    }
    if (it.kind === "report" && inbox.replying === it.id) {
      return inboxReplyBox(it, "Write your reply",
        '<button type="button" class="ibbtn" data-act="ib:reply-close" data-id="' + d + '">Cancel</button>');
    }
    if (it.kind === "report") {
      return '<div class="ibacts"><button type="button" class="ibbtn" data-act="ib:reply-open" data-id="' + d + '">Reply</button>' +
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Mark read</button></div>';
    }
    if (it.kind === "proposal" && it.lint) {
      return '<div class="ibprop">' +
        '<div class="ibfacts">' + (it.facts || []).map((f) => '<span class="ibfact">' + esc(f) + "</span>").join("") + "</div>" +
        '<div class="ckhint">' + esc(it.hint || "") + "</div>" + checkExamples(it) +
        '<div class="ibacts"><button type="button" class="ibbtn ok" data-act="ib:prop-approve" data-id="' + d + '">Turn the check on</button>' +
        '<button type="button" class="ibbtn" data-act="ib:prop-reject" data-id="' + d + '">Reject</button></div></div>';
    }
    if (it.kind === "proposal" && it.share) {
      return '<div class="ibprop">' +
        '<div class="ibfacts">' + (it.facts || []).map((f) => '<span class="ibfact">' + esc(f) + "</span>").join("") + "</div>" +
        '<div class="shtext msg-body">' + markdown(it.text || "") + "</div>" +
        '<div class="ibacts"><button type="button" class="ibbtn ok" data-act="ib:prop-approve" data-id="' + d + '">Move to every project</button>' +
        '<button type="button" class="ibbtn" data-act="ib:prop-reject" data-id="' + d + '">Reject</button></div></div>';
    }
    if (it.kind === "proposal") {
      return '<div class="ibprop">' +
        '<div class="ibfacts">' + (it.facts || []).map((f) => '<span class="ibfact">' + esc(f) + "</span>").join("") + "</div>" +
        '<pre class="mpdiff ibdiff">' + diffHtml(it.diff || "") + "</pre>" +
        '<div class="ibacts"><button type="button" class="ibbtn ok" data-act="ib:prop-approve" data-id="' + d + '">Approve — lead applies it on a branch</button>' +
        '<button type="button" class="ibbtn" data-act="ib:prop-reject" data-id="' + d + '">Reject</button></div></div>';
    }
    if (it.kind === "login") {
      return '<div class="ibacts"><button type="button" class="ibbtn send" data-act="ib:login" data-id="' + d + '">Run /login</button>' +
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Later</button></div>';
    }
    if (it.kind === "login-continue") {
      return '<div class="ibacts"><button type="button" class="ibbtn send" data-act="ib:login-continue" data-id="' + d + '">Press Return</button>' +
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Later</button></div>';
    }
    if (it.kind === "carry-on") {
      return '<div class="ibacts"><button type="button" class="ibbtn send" data-act="ib:carry-on" data-id="' + d + '">Continue</button>' +
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Later</button></div>';
    }
    if (it.kind === "unsent") {
      return '<div class="ibacts"><button type="button" class="ibbtn send" data-act="ib:submit" data-id="' + d + '">Press Return</button>' +
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Later</button></div>';
    }
    if (it.kind === "relay") {
      return '<div class="ibacts"><button type="button" class="ibbtn send" data-act="ib:relay" data-id="' + d + '">Relay to ' + esc(it.relayTo || "") + "</button>" +
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Later</button></div>';
    }
    // Its directory gone: Stop, as `herd stop' does.  Where work may be lost
    // the button says so, and is not the one that looks like the way on.
    if (it.kind === "gone") {
      return '<div class="ibacts"><button type="button" class="ibbtn' + (it.safe ? " ok" : "") + '" data-act="ib:stop-gone" data-id="' + d + '">' +
        (it.safe ? "Stop it" : "Stop it anyway") + "</button>" +
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Later</button></div>';
    }
    return '<div class="ibacts"><button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Mark read</button></div>';
  }

  // The box a question's answer or a reply to a report is written in, sent
  // as `ib:reply' routes it, and the images attached for its agent's next
  // prompt as a room's box takes them (the Image button, a paste).  LATER,
  // the button beside Send that puts it off.
  function inboxReplyBox(it, placeholder, later) {
    const d = esc(it.id);
    return '<div class="ibreply" data-name="' + esc(it.agent) + '">' +
      '<div class="attach">' + attachHtml(it.agent) + "</div>" +
      '<textarea class="ibreplybox" data-id="' + d + '" rows="3" placeholder="' + placeholder + '">' + esc(inbox.reply) + "</textarea>" +
      '<div class="ibacts"><button type="button" class="ibbtn send" data-act="ib:reply" data-id="' + d + '">Send</button>' +
      IMAGE_BUTTON + later + "</div></div>";
  }

  // The full detail of one item: the desk's right pane, and a phone card once
  // it is opened.  One renderer, so the two never drift.  PHONE picks the
  // compact action layout in `inboxActions'.
  function inboxDetail(it, phone) {
    const d = inbox.detail && inbox.detail.for === it.id ? inbox.detail : null;
    const body = d && d.report ? d.report : it.body;
    return '<div class="ibhead">' + inboxTag(it) + '<h1 class="ibh1">' + ibPlain(it.title) + "</h1></div>" +
      '<div class="ibmeta">' + inboxCli(it) + '<span class="ibagent">' + esc(it.agent) + "</span>" +
      (it.project ? "<span>· " + esc(leaf(it.project)) + "</span>" : "") +
      (it.state ? "<span>· " + esc(it.state) + "</span>" : "") +
      '<span>· ' + esc(inboxAge(it.at)) + "</span></div>" +
      inboxWhy(it, body, d && d.report ? d.images : null) + inboxActions(it, phone) + inboxThreadInline(it, phone, d);
  }

  function inboxDeskHtml() {
    const cur = inboxCur();
    const detail = inbox.toast
      ? '<div class="ibtoast" role="status">' + esc(inbox.toast) +
        (inbox.undo ? ' <button type="button" class=" iblink" data-act="ib:undo">Undo</button>' : "") + "</div>"
      : "";
    const body = cur
      ? inboxDetail(cur, false)
      : '<div class="ibzero"><p class="ibzh">Inbox zero</p><p>Everything that needed you is answered. Updates keep arriving here.</p></div>';
    return '<section class="iblistcol">' + inboxList() + "</section>" +
      '<main class="ibdetail">' + detail + body + "</main>";
  }

  // A phone's card of an item, the Board's too.  Collapsed: the whole card
  // is one tap target that opens it.  Open: a container (it holds buttons,
  // so not a button itself) whose head taps to collapse, and the full desk
  // detail below -- one open at a time.  O, the Board's: PICK, its act
  // (`ib:pick'); HEAD, more in its head; WHO, its line under the head;
  // BODY, in place of the item's detail; AFTER, below it; PHONE, the
  // actions' compact layout (a phone's).
  function inboxCard(it, o) {
    o = o || {};
    const pick = o.pick || "ib:pick";
    const who = o.who != null ? o.who : '<span class="ibcardwho">' + esc(it.agent) + (it.cli ? " · " + esc(it.cli) : "") +
      (it.project ? " · " + esc(leaf(it.project)) : "") + "</span>";
    const tone = "t-" + esc(it.tone) + (o.need || inboxNeed(it) ? " need" : "");
    const head = inboxTag(it) + (o.head || "") + '<span class="ibtitle">' + ibPlain(it.title) + "</span>" +
      '<span class="ibage">' + esc(inboxAge(it.at)) + "</span>";
    return it.id === inbox.sel
      ? '<div class="ibcard open ' + tone + '" data-id="' + esc(it.id) + '">' +
        '<button type="button" class="ibcardhead" data-act="' + pick + '" data-id="' + esc(it.id) + '" aria-expanded="true">' + head + "</button>" + who +
        '<div class="ibcardbody">' + (o.body != null ? o.body : inboxDetail(it, o.phone !== false)) + (o.after || "") + "</div></div>"
      : '<button type="button" class="ibcard ' + tone + '" data-act="' + pick + '" data-id="' + esc(it.id) + '" aria-expanded="false">' +
        '<span class="ibcardhead">' + head + "</span>" + who +
        (it.preview ? '<span class="ibcardprev">' + ibPlain(it.preview) + "</span>" : "") + "</button>";
  }

  function inboxPhoneHtml() {
    const live = inbox.items.filter(inboxPass);
    const need = live.filter(inboxNeed);
    const fyi = live.filter((it) => !inboxNeed(it));
    const filters = INBOX_FILTERS.map(([f, label]) =>
      '<button type="button" class="ibfilter' + (inbox.filter === f ? " on" : "") + '" data-act="ib:filter" data-f="' + f + '">' + label + "</button>").join("");
    const card = (it) => inboxCard(it);
    return (inbox.toast ? '<div class="ibtoast" role="status">' + esc(inbox.toast) +
            (inbox.undo ? ' <button type="button" class="iblink" data-act="ib:undo">Undo</button>' : "") + "</div>" : "") +
      '<div class="ibfilters">' + filters + "</div>" +
      '<div class="ibsec"><h2 class="ibneed">Needs you</h2></div>' +
      (need.length ? need.map(card).join("")
        : inbox.got ? '<p class="ibempty">Inbox zero. The herd is moving on its own.</p>' : INBOX_COMING) +
      (fyi.length ? '<div class="ibsec fyi"><h2>Updates · ' + fyi.length + "</h2></div>" + fyi.map(card).join("") : "");
  }

  // Ask Emacs for the selected item's thread (and, for a report/question,
  // its full last message) -- only on a pick, never on the steady tick.
  function inboxFetchThread(it) {
    if (!it) return;
    const m = /^(?:ask-failed|handoff):(.+)$/.exec(it.id || "");
    if (!m && it.kind !== "report" && it.kind !== "question") return;
    emit("inbox-detail", { id: it.id, kind: it.kind, agent: it.agent, ask: m ? m[1] : "" });
  }

  // Pick an item: clear any thread/report fetched for the last one, mark it
  // read, and fetch this one's.
  function inboxSelect(it) {
    inbox.sel = it ? it.id : "";
    const back = (it && inbox.back.get(it.id)) || "";
    inbox.reply = back;
    inbox.replying = back && it.kind === "report" ? it.id : "";
    inbox.toast = "";
    inbox.detail = null;
    inbox.thOpen = {};
    if (it) { inboxMarkRead(it); inboxFetchThread(it); }
  }

  // Resolve an item: hide it at once (optimistic), toast, move to the next.
  function inboxResolve(it, msg, undoable) {
    inbox.held.delete(it.id);
    inbox.gone.set(it.id, Date.now() + INBOX_HOLD);
    const rest = inbox.items.filter((x) => x.id !== it.id);
    // The Board's list, ranked its own way, closes instead.
    const next = view === "board" ? null : rest.filter(inboxNeed)[0] || rest[0] || null;
    inbox.items = rest;
    inbox.sel = next ? next.id : "";
    inbox.reply = "";
    inbox.replying = "";
    inbox.toast = msg;
    inbox.undo = undoable ? { id: it.id } : null;
    inbox.detail = null;
    inbox.thOpen = {};
    if (next) inboxFetchThread(next);
    if (undoable) {
      const token = it.id;
      setTimeout(() => { if (inbox.undo && inbox.undo.id === token) { inbox.undo = null; inbox.toast = ""; renderInbox(); } }, 6000);
    } else if (view === "board") {
      // Over 要你處理 a while, not for good: an empty list is a quiet line.
      setTimeout(() => { if (inbox.toast === msg && !inbox.undo) { inbox.toast = ""; renderInbox(); } }, 6000);
    }
    renderInbox();
  }

  // A reply that did not reach Emacs goes back in its item's own box,
  // before what has been written there since, its image lines and all,
  // and the item back in the list -- a report unmarked, as it was marked
  // read for the reply.  Shown at once, unless another reply is being
  // written; then it waits in its box for the item to be picked.  Returns
  // what takes it out again, should Emacs have had it after all (xwapp's
  // emit calls it): sent again, it would go twice.
  function inboxGiveBack(it, draft) {
    const unmark = it.kind === "report";
    if (unmark) emit("inbox-set", { id: it.id, done: false });
    const box = () => app.querySelector('.ibreplybox[data-id="' + cssName(it.id) + '"]');
    const ta = box();
    const now = (ta ? ta.value : inbox.back.get(it.id) || "").trim();
    const text = now ? draft + "\n\n" + now : draft;
    inbox.back.set(it.id, text);
    inbox.gone.delete(it.id);
    inbox.held.set(it.id, { it: it, until: Date.now() + INBOX_HOLD });
    if (!inbox.items.some((x) => x.id === it.id)) inbox.items.unshift(it);
    if (inbox.toast === "Reply sent to " + it.agent) inbox.toast = "";
    if (inbox.sel === it.id) {
      inbox.reply = text;
      if (unmark) inbox.replying = it.id;
      if (ta) ta.value = text;
    } else if (!inbox.reply.trim()) {
      inboxSelect(it);
    }
    renderInbox();
    const said = "Not sent: Emacs did not get it. " + (unmark
      ? "The report is back in the Inbox, your reply in its box."
      : "Your answer is back in the question's box in the Inbox.");
    setStatus(said, "error");
    return () => {
      const ta = box();
      const rest = unGiven(ta ? ta.value : inbox.back.get(it.id) || "", draft);
      unsay(said);
      if (rest === null) return;
      if (rest) inbox.back.set(it.id, rest);
      else inbox.back.delete(it.id);
      if (inbox.sel === it.id) {
        inbox.reply = rest;
        if (ta) ta.value = rest;
      }
      if (rest) return;
      // Nothing of it left to send: the item goes, as it went when sent.
      if (unmark) emit("inbox-set", { id: it.id, done: true });
      if (inbox.sel === it.id) return inboxResolve(it, "Reply sent to " + it.agent, false);
      inbox.held.delete(it.id);
      inbox.items = inbox.items.filter((x) => x.id !== it.id);
      renderInbox();
    };
  }

  // How long an item given back stays in the list whatever Emacs pushes:
  // a push it sent before the unmark reached it would drop the item.
  const INBOX_HOLD = 20000;
  function inboxKeepHeld() {
    const t = Date.now();
    inbox.held.forEach((h, id) => {
      if (t > h.until) inbox.held.delete(id);
      else if (!inbox.items.some((x) => x.id === id)) inbox.items.unshift(h.it);
    });
  }

  // A guard request kept denied: the herd closes it and tells the agent,
  // so it leaves the card as well -- not only this list.
  function guardKeepDenied(it) {
    emit("guard-reject", { id: it.guardId });
    inboxResolve(it, "Kept denied — the agent is told not to try it again", false);
  }

  function inboxMarkRead(it) {
    if (it && !it.read) { it.read = true; emit("inbox-set", { id: it.id, read: true }); }
  }

  function onInboxAction(act, el) {
    const id = el.getAttribute("data-id") || inbox.sel;
    // On the Board, a lead's line the Inbox has not sent yet too (`bdNeeds').
    const it = inbox.items.find((x) => x.id === id) || bdNeedExtra.get(id);
    if (act === "filter") { inbox.filter = el.getAttribute("data-f"); renderInbox(); return; }
    if (act === "undo") {
      if (inbox.undo) {
        emit("inbox-set", { id: inbox.undo.id, done: false });
        inbox.gone.delete(inbox.undo.id);
        inbox.undo = null;
        inbox.toast = "";
        renderInbox();
      }
      return;
    }
    if (act === "thread-msg") {
      const i = el.getAttribute("data-i");
      inbox.thOpen[i] = !inbox.thOpen[i];
      renderInbox();
      return;
    }
    if (!it) return;
    if (act === "pick") {
      // On a phone -- and the Board's list -- a card expands in place;
      // tapping the open one closes it.
      if ((inboxNarrow() || view === "board") && id === inbox.sel) { inbox.sel = ""; inbox.detail = null; inbox.thOpen = {}; renderInbox(); return; }
      inboxSelect(it); renderInbox(); return;
    }
    if (act === "answer") { emit("agent-answer", { name: it.agent, n: +el.getAttribute("data-n") }); inboxResolve(it, "Sent " + el.getAttribute("data-n") + " to " + it.agent, false); return; }
    if (act === "guard-approve") { emit("guard-approve", { id: it.guardId }); inboxResolve(it, "Approved once — the agent was told to run it again", false); return; }
    if (act === "prop-approve") { emit("memory-proposal-approve", { id: it.proposalId }); inboxResolve(it, it.lint ? "Check on — turn it off on the Memory tab" : "Approved — handed to the lead to apply on a branch", false); return; }
    if (act === "prop-reject") { emit("memory-proposal-reject", { id: it.proposalId, reason: "" }); inboxResolve(it, it.lint ? "Rejected — no check; the fact stays in memory" : "Rejected — the facts stay in memory", false); return; }
    if (act === "reply-open" || act === "reply-close") {
      inbox.replying = act === "reply-open" ? it.id : "";
      inbox.reply = act === "reply-open" ? inbox.back.get(it.id) || "" : "";
      // Cancel drops the images put in the box too, and a reply given
      // back; another item picked keeps them for when this one is back.
      if (act === "reply-close") { delete herdNow.attach[it.agent]; inbox.back.delete(it.id); inboxUnhold(); }
      renderInbox();
      const ta = act === "reply-open" && app.querySelector('.ibreplybox[data-id="' + cssName(it.id) + '"]');
      if (ta) ta.focus();
      return;
    }
    if (act === "reply") {
      const ta = app.querySelector('.ibreplybox[data-id="' + cssName(it.id) + '"]');
      const text = withImages(it.agent, ((ta && ta.value) || inbox.reply || "").trim());
      if (!text) { setStatus("Write the answer first"); return; }
      // A lead's through its project (`ghostherd-lead-say' finds the lead
      // from it, and starts one if it is gone); primary's through primary.
      // Lost on the way, it is back in this item's box, and the item back
      // in the list.
      const lost = () => inboxGiveBack(it, text);
      const sent = it.replyVia === "primary" ? emit("primary-say", { text: text }, lost)
        : it.replyVia === "agent" ? emit("agent-prompt", { name: it.agent, text: text }, lost)
        : emit("lead-say", { project: it.project, text: text }, lost);
      if (sent === false) { setStatus(TOO_LONG, "error"); return; }
      inboxUnhold();
      delete herdNow.attach[it.agent];
      inbox.back.delete(it.id);
      // A report answered is read, as Mark read has it; a question's goes
      // when its agent takes the answer.
      if (it.kind === "report") emit("inbox-set", { id: it.id, done: true });
      inboxResolve(it, "Reply sent to " + it.agent, false);
      return;
    }
    if (act === "login") { emit("agent-login", { name: it.agent }); inboxResolve(it, "/login sent to " + it.agent, false); return; }
    if (act === "login-continue") { emit("agent-login-continue", { name: it.agent }); inboxResolve(it, "Return pressed for " + it.agent + "'s sign-in", false); return; }
    if (act === "carry-on") { emit("agent-carry-on", { name: it.agent }); inboxResolve(it, "Continuing " + it.agent + "'s turn", false); return; }
    if (act === "relay") { emit("agent-relay", { name: it.agent, kind: it.relayTo }); inboxResolve(it, "Relayed to " + it.relayTo, false); return; }
    if (act === "submit") { emit("agent-submit", { name: it.agent }); inboxResolve(it, "Return pressed for " + it.agent, false); return; }
    if (act === "stop-gone") { emit("agent-stop-gone", { name: it.agent }); inboxResolve(it, "Stopped " + it.agent, false); return; }
    if (act === "done" && it.kind === "guard") { guardKeepDenied(it); return; }
    if (act === "need-done") { emit("board-need-done", { id: it.event }); inboxResolve(it, "已處理", false); return; }
    if (act === "need-board") {
      if (view === "board") { bdGoto(it.item || ""); return; }
      board.goto = it.item || "";
      showBoard();
      if (board.got && board.cards[board.goto]) { const g = board.goto; board.goto = ""; onBoardAction("goto", { getAttribute: (k) => (k === "data-id" ? g : null) }); }
      return;
    }
    if (act === "done") { emit("inbox-set", { id: it.id, done: true }); inboxResolve(it, "Marked read", true); return; }
  }

  // Desk keys when the Inbox has focus and no text field does.
  function inboxKey(e) {
    if (view !== "inbox") return;
    const el = document.activeElement;
    if (el && (el.tagName === "TEXTAREA" || el.tagName === "INPUT")) return;
    const live = inbox.items.filter(inboxPass);
    const i = live.findIndex((x) => x.id === inbox.sel);
    const move = (d) => {
      const n = live[Math.max(0, Math.min(live.length - 1, (i < 0 ? 0 : i) + d))];
      if (n) { inboxSelect(n); renderInbox(); }
    };
    const cur = inboxCur();
    if (e.key === "j" || e.key === "ArrowDown") { e.preventDefault(); move(1); }
    else if (e.key === "k" || e.key === "ArrowUp") { e.preventDefault(); move(-1); }
    else if (cur && cur.kind === "blocked" && /^[1-9]$/.test(e.key)) {
      const o = (cur.options || []).find((x) => String(x.n) === e.key);
      if (o) { e.preventDefault(); emit("agent-answer", { name: cur.agent, n: +e.key }); inboxResolve(cur, "Sent " + e.key + " to " + cur.agent, false); }
    } else if (cur && e.key === "a" && (cur.kind === "guard" || cur.kind === "proposal")) {
      e.preventDefault();
      onInboxAction(cur.kind === "guard" ? "guard-approve" : "prop-approve", { getAttribute: () => cur.id });
    } else if (cur && e.key === "e" && cur.kind === "guard") { e.preventDefault(); guardKeepDenied(cur); }
    else if (cur && e.key === "e") { e.preventDefault(); emit("inbox-set", { id: cur.id, done: true }); inboxResolve(cur, "Marked read", true); }
  }

  function inboxBadge() {
    const b = $("inbox-n");
    if (b) { b.textContent = inbox.count ? String(inbox.count) : ""; b.classList.toggle("hidden", !inbox.count); }
  }

  // ---- Threads ---------------------------------------------------------
  // Who is talking to whom, across the herd, and about what.  Across the
  // top, every open ask and handoff; then the threads -- a chain of work: a
  // root ask, every ask made while answering one in it (handoffs to other
  // projects too) and the answers, with the user's prompt that set it going
  // -- and the open one as a sequence diagram, a lane per participant.
  // Read-only.  Emacs pushes the list while the tab is shown (threads-watch
  // / renderThreads) and one thread at a time (thread-open / renderThread).
  // Phone: the list, then the thread as a conversation, "A → B".

  // PROJECT / SORT, the view the list is of: one project's threads ("" for
  // every one's), newest first or by what they cost -- done by the herd
  // before it cuts the list to a page (herd_threads).  PROJECTS, every
  // project the threads it read name, for the chips.  RAW, the herd's last
  // list as it came, LIST shown from it (thPreview) until the herd's list
  // of the view picked comes.
  let threads = { list: [], raw: [], talking: [], live: 0, more: false, req: null, cur: null, error: "", open: {}, phone: false,
                  costs: [], costDays: 7, costOpen: false, projects: [], repick: false, ...thViewKept() };
  let threadsWatching = false;

  const TH_STATUS = { live: "Live", waiting: "Waiting", answered: "Answered", failed: "Failed" };
  const TH_LABEL = { ask: "ask", handoff: "handoff", answer: "answer", message: "message", failed: "failed", cancelled: "cancelled" };

  // The view the viewer picked, kept for them in this browser.  Storage
  // can be missing or refused (a private window, the desk's file page):
  // every project's threads then, newest first.
  function thViewKept() {
    try {
      const project = localStorage.getItem("ghostherd.threads.project") || "";
      return { project: project, sort: localStorage.getItem("ghostherd.threads.sort") === "cost" ? "cost" : "recent" };
    } catch (e) {
      return { project: "", sort: "recent" };
    }
  }

  function thViewKeep() {
    try {
      localStorage.setItem("ghostherd.threads.project", threads.project);
      localStorage.setItem("ghostherd.threads.sort", threads.sort);
    } catch (e) {
      // Not kept: this page keeps it until it is loaded again.
    }
  }

  // The page of threads loaded, as the view now picked shows it -- until
  // the herd's own list of that view comes, in its order, so the list does
  // not jump when it does: newest first; by cost (server/threads.py
  // by_cost), a task's threads together, the costliest task first and
  // none known last, the thread that shows its cost first in its group
  // and the "same task as ..." ones after it, newest first.
  function thPreview(list) {
    const p = threads.project;
    const out = list.filter((t) => !p || (t.projects || []).includes(p)).sort((a, b) => (b.at || 0) - (a.at || 0));
    if (threads.sort !== "cost") return out;
    // Each task's total, from the thread that shows it -- loaded, if not
    // one of this project's.
    const total = {};
    list.forEach((t) => { if (t.cost && t.cost.id && !t.cost.same) total[t.cost.id] = t.cost.total || 0; });
    const groups = new Map();
    out.forEach((t) => {
      const k = t.cost && t.cost.id in total ? "task:" + t.cost.id : "thread:" + t.id;
      if (!groups.has(k)) groups.set(k, []);
      groups.get(k).push(t);
    });
    // The one that shows it: as the herd picks it (threads.py carrier) when
    // that one is not among these.
    const carrier = (ts) => ts.find((t) => t.cost && !t.cost.same) || ts.find((t) => t.direct) ||
      ts.reduce((a, b) => ((b.started || 0) < (a.started || 0) ? b : a));
    const rows = Array.from(groups.values()).map((ts) => {
      const first = carrier(ts);
      const c = ts[0].cost;
      return { known: !!(c && c.id in total), total: c && c.id in total ? total[c.id] : 0, at: ts[0].at || 0,
               ts: [first].concat(ts.filter((t) => t !== first)) };
    });
    rows.sort((a, b) => (b.known - a.known) || (b.total - a.total) || (b.at - a.at));
    return rows.reduce((acc, g) => acc.concat(g.ts), []);
  }

  // The thread open now, by its id: the one shown, else the one asked for.
  function thOpenId() {
    return threads.cur ? threads.cur.id : (threads.req && threads.req.id) || "";
  }

  function thNarrow() { return window.innerWidth < 720; }
  function thWho(n) { return n === "user" ? "You" : String(n || "?"); }
  function thAge(ms) {
    if (!ms) return "";
    const s = Math.max(0, (Date.now() - ms) / 1000);
    return s < 60 ? "now" : s < 3600 ? Math.floor(s / 60) + "m" : s < 86400 ? Math.floor(s / 3600) + "h" : Math.floor(s / 86400) + "d";
  }
  function thTime(ms) {
    if (!ms) return "";
    const d = new Date(ms);
    const p = (n) => String(n).padStart(2, "0");
    const day = d.toDateString() === new Date().toDateString() ? "" : (d.getMonth() + 1) + "/" + d.getDate() + " ";
    return day + p(d.getHours()) + ":" + p(d.getMinutes());
  }
  function thStatus(st) { return '<span class="thst s-' + esc(st) + '">' + esc(TH_STATUS[st] || st) + "</span>"; }
  function thLabel(m) { return m.kind === "answer" && m.auto ? "stopped · its last words" : TH_LABEL[m.kind] || m.kind; }
  function thRound(m) { return m.round && m.of ? "round " + m.round + "/" + m.of : ""; }
  // An answer's trace inside its card (spans: the card is a button); the
  // counts behind it once the card is opened.
  function thTrace(m, open) {
    if (!m.trace) return "";
    return '<span class="thtrace' + (m.rough ? " rough" : "") + '">' + esc(m.trace) + "</span>" +
      (open && m.traceDetail ? '<span class="thtracedetail">' + esc(traceDetailText(m.traceDetail)) + "</span>" : "");
  }

  // ---- What a task cost ----------------------------------------------------
  // A thread's task: the user's message that set it going and all it set
  // off -- the lead's replies, its asks' takers', their sub-agents' -- in
  // weighted tokens (the tally's unit), and the lines its commits changed,
  // merged or not.  A task two threads are part of (a lead's two asks for
  // it) shows its cost on one; the other says whose it is.  Above the
  // list, lead mode against direct mode, folded to a line.

  const TASK_CODE = { no: "no code", unknown: "code not readable", pending: "code not read yet" };
  function taskMade(c) { return TASK_CODE[c.code] || c.lines + (c.lines === 1 ? " line" : " lines"); }
  function taskMerged(c) { return c.commits ? (c.merged === c.commits ? "all merged" : (c.merged || 0) + " merged") : ""; }
  // A short prompt -- a go-ahead -- with the one it goes on from.
  function taskTitle(c) {
    return "Its task" + (c.prompt ? ": " + c.prompt + (c.context ? " ← " + c.context : "") : "") + "\n" +
      tokens(c.total) + " weighted tokens — " +
      (c.mode === "lead" ? "the lead " : c.agent + " itself ") + tokens(c.own) + " · workers " + tokens(c.workers) +
      " · sub-agents " + tokens(c.subs) + "\n" + c.asks + (c.asks === 1 ? " ask" : " asks") + " · " +
      (c.code === "yes" ? c.lines + " lines changed in " + c.commits + (c.commits === 1 ? " commit" : " commits") +
        ", " + taskMerged(c) : taskMade(c));
  }
  function thCost(c) {
    if (!c) return "";
    if (c.same) {
      const of = c.sameTitle && c.sameTitle.length > 40 ? c.sameTitle.slice(0, 39) + "…" : c.sameTitle || "";
      return '<span class="thcost thsame" title="One task with another thread: its cost is shown there, once">' +
        "same task as “" + esc(of) + "”</span>";
    }
    // In its row, what it cost and the lines it changed, if any: why none
    // -- no code, not read yet -- is the open thread's to say.
    return '<span class="thcost" title="' + esc(taskTitle(c)) + '">' + esc(tokens(c.total)) +
      (c.code === "yes" ? " · " + esc(taskMade(c)) : "") + "</span>";
  }
  function thCostLine(c) {
    if (!c) return "";
    const per = c.code === "yes" && c.lines ? " · " + tokens(Math.round((c.total * 100) / c.lines)) + " per 100 lines" : "";
    const merged = c.code === "yes" ? " (" + taskMerged(c) + ")" : "";
    // Whose tokens they are, the parts that had none left out.
    const parts = [[c.mode === "lead" ? "lead" : "itself", c.own], ["workers", c.workers], ["sub-agents", c.subs]]
      .filter((x) => x[1]).map((x) => x[0] + " " + tokens(x[1]));
    return '<p class="thcostline" title="' + esc(taskTitle(c)) + '">Its task' +
      (c.threadCount > 1 ? ", across " + c.threadCount + " threads" : "") + ": " + esc(tokens(c.total)) + " weighted tokens" +
      (parts.length ? " — " + esc(parts.join(" · ")) : "") + " · " + esc(taskMade(c)) + esc(merged) + esc(per) + "</p>";
  }
  // What a short prompt told an agent directly goes on from (cost.py
  // `contexts'): a quiet line under its title, not part of it.
  function thContext(t) {
    const c = t && t.direct && t.cost;
    return c && !c.same && c.context ? String(c.context) : "";
  }
  // The open thread's commits: each, its lines and whether it is in a main
  // branch yet; a copy (an amend, a rebase, a cherry-pick) counted where
  // its first was made.
  function thCommits(c) {
    const made = (c && c.made) || [];
    if (!made.length) return "";
    return '<ul class="thcommits" aria-label="Commits">' + made.map((m) =>
      '<li class="' + (m.of ? "copy" : "") + '"><code>' + esc(String(m.sha).slice(0, 8)) + "</code>" +
      '<span class="thcsubj">' + esc(m.subject || "") + "</span>" +
      '<span class="thcmeta">' + esc(m.lines + (m.lines === 1 ? " line" : " lines")) + " · " +
      (m.merged ? "merged" : "not merged") +
      (m.of ? " · a copy of " + esc(String(m.of).slice(0, 8)) + ", not counted again" : "") + "</span></li>").join("") + "</ul>";
  }
  // Folded, one line: a week's or a month's median per task that changed
  // code, per 100 lines, the lead's share -- the table a press away, open
  // or folded for the page's life.
  function thCosts() {
    const all = threads.costs || [];
    const c = all.find((x) => x.days === threads.costDays) || all[0];
    if (!c || !c.lead || !c.direct) return "";
    const n = (v) => (v == null ? "—" : tokens(v));
    const row = (label, a, b) => "<tr><th>" + esc(label) + "</th><td>" + esc(a) + "</td><td>" + esc(b) + "</td></tr>";
    const L = c.lead, D = c.direct;
    const open = !!threads.costOpen;
    const days = all.map((x) => '<button type="button" class="thcd' + (x.days === c.days ? " on" : "") +
      '" data-act="th:cost-days" data-days="' + x.days + '" title="The last ' + x.days + ' days">' + x.days + "d</button>").join("");
    const sub = "through a lead, or told to an agent directly · weighted tokens · since " + c.since;
    const fig = (label, a, b) => '<span class="thcfig"><span class="thclab">' + esc(label) + "</span> lead <b>" + esc(a) +
      "</b> · direct <b>" + esc(b) + "</b></span>";
    const bar = '<div class="thcbar"><h2>What a task costs</h2>' +
      '<span class="thcsum" title="' + esc(sub) + '">' + fig("median per coded task", n(L.medianCoded), n(D.medianCoded)) +
      fig("per 100 lines", n(L.per100), n(D.per100)) +
      '<span class="thcfig"><span class="thclab">lead\'s share</span> <b>' + esc(L.own == null ? "—" : L.own + "%") + "</b></span></span>" +
      '<span class="thcctl"><span class="thcds">' + days + "</span>" +
      '<button type="button" class="thctoggle" data-act="th:cost-table" aria-expanded="' + open + '">' +
      (open ? "Table ▾" : "Table ▸") + "</button></span></div>";
    if (!open) return '<section class="thcosts" aria-label="What a task costs">' + bar + "</section>";
    // A task whose code is not known yet, or cannot be: in neither "lines
    // changed" nor "no code" -- said when there is one, as `herd cost' says it.
    const some = (key, label) => (L[key] || D[key] ? row(label, String(L[key] || 0), String(D[key] || 0)) : "");
    return '<section class="thcosts open" aria-label="What a task costs">' + bar +
      '<p class="thsub thcsince">' + esc(sub) + "</p>" +
      '<table class="thctable"><thead><tr><th></th><th>lead mode</th><th>direct</th></tr></thead><tbody>' +
      row("tasks", String(L.tasks), String(D.tasks)) + row("median per task", n(L.median), n(D.median)) +
      // A go-ahead is a task of its own: the tasks that changed code apart.
      row("median per coded task", n(L.medianCoded), n(D.medianCoded)) +
      row("total", n(L.total), n(D.total)) + row("per 100 lines", n(L.per100), n(D.per100)) +
      row("lines changed", L.lines + " in " + L.coded, D.lines + " in " + D.coded) +
      row("of them merged", String(L.merged || 0), String(D.merged || 0)) +
      row("no code", String(L.noCode), String(D.noCode)) +
      some("pending", "not read yet") + some("unknown", "not readable") +
      row("lead's own share", L.own == null ? "—" : L.own + "%", "—") + "</tbody></table></section>";
  }

  function threadsBadge() {
    const b = $("threads-n");
    if (b) { b.textContent = threads.live ? threads.live + " live" : ""; b.classList.toggle("hidden", !threads.live); }
  }

  function showThreads(req) {
    if (herdNow.room) closeRoomState();
    view = tab = "threads";
    src = null;
    chrome();
    app.innerHTML = '<div class="page wide threadspage"></div>';
    if (!threadsWatching) { threadsWatching = true; emit("threads-watch", { project: threads.project, sort: threads.sort }); }
    if (req) openThread(req);
    else renderThreads();
  }

  function openThread(req) {
    threads.req = req;
    threads.cur = null;
    threads.error = "";
    threads.open = {};
    threads.phone = true;
    threads.toTop = true;
    emit("thread-open", req);
    renderThreads();
  }

  function renderThreads() {
    if (view !== "threads") return;
    const box = app.querySelector(".threadspage");
    if (!box) return;
    const narrow = thNarrow();
    box.classList.toggle("narrow", narrow);
    const top = threads.toTop;
    threads.toTop = false;
    keepScroll(box, [".thlist", ".thmain"], () => {
      box.innerHTML = narrow ? threadsPhoneHtml() : threadsDeskHtml();
    }, top ? [".thmain"] : []);
    // The phone's one flow: a thread opened starts at its top.
    if (narrow && top) app.scrollTop = 0;
  }

  // Nobody talking: nothing -- the tab's "N live" says so.
  function thTalk() {
    if (!threads.talking.length) return "";
    const chips = threads.talking.map((e) =>
      '<button type="button" class="thchip" data-act="th:open" data-ask="' + esc(e.ask) + '" title="' + esc(e.kind + " " + e.ask) + '">' +
      '<span class="thname">' + esc(thWho(e.from)) + "</span>" +
      '<span class="tharrow k-' + esc(e.kind) + '">' + (e.kind === "handoff" ? "⇢" : "→") + "</span>" +
      '<span class="thname">' + esc(thWho(e.to)) + "</span>" +
      '<span class="thwhat">' + esc(e.status === "queued" ? "queued" : (e.working ? "working " : "waiting ") + thAge(e.since)) + "</span></button>").join("");
    return '<section class="thtalk" aria-label="Talking now"><div class="thtalkhead"><h2>Talking now</h2>' +
      '<span class="thsub">every open ask and handoff in the herd, across projects</span></div>' +
      '<div class="thchips">' + chips + "</div></section>";
  }

  function thRow(t) {
    const on = !!(threads.cur ? t.id === threads.cur.id : threads.req && threads.req.id === t.id);
    const ctx = thContext(t);
    return '<button type="button" class="throw' + (on ? " on" : "") + '" data-act="th:open" data-id="' + esc(t.id) + '" aria-current="' + (on ? "true" : "false") + '">' +
      '<span class="thr1">' + thStatus(t.status) + '<span class="thtitle">' + esc(t.title || "(no text)") + "</span>" +
      '<span class="thage">' + esc(thAge(t.at)) + "</span></span>" +
      (ctx ? '<span class="thctx" title="' + esc("What it goes on from: " + ctx) + '">← ' + esc(ctx) + "</span>" : "") +
      '<span class="thpath">' + esc((t.participants || []).map(thWho).join(" → ")) + "</span>" +
      '<span class="thr3">' + (t.projects || []).map((p) => '<span class="thproj">' + esc(p) + "</span>").join("") +
      (t.cross ? '<span class="thcross">cross-project</span>' : "") + thCost(t.cost) +
      '<span class="thcount">' + t.count + (t.count === 1 ? " message" : " messages") + "</span></span></button>";
  }

  // Above the list: a chip a project, every one's first -- the one picked
  // too, though none of its threads is among those read -- and the order.
  function thBar() {
    const names = threads.projects.slice();
    if (threads.project && !names.includes(threads.project)) names.push(threads.project);
    const chip = (p, label) => '<button type="button" class="chip' + (threads.project === p ? " on" : "") +
      '" data-act="th:project" data-project="' + esc(p) + '" aria-pressed="' + (threads.project === p) + '">' + esc(label) + "</button>";
    const order = (k, label, title) => '<button type="button" class="ghost small' + (threads.sort === k ? " on" : "") +
      '" data-act="th:sort" data-sort="' + k + '" aria-pressed="' + (threads.sort === k) + '" title="' + title + '">' + label + "</button>";
    return '<div class="thbar">' +
      (names.length ? '<div class="chips thpchips" role="group" aria-label="Project">' + chip("", "All") +
        names.map((p) => chip(p, p)).join("") + "</div>" : "") +
      '<span class="msort" role="group" aria-label="Order">' + order("recent", "Recent", "Newest activity first") +
      order("cost", "Cost", "What its task cost, the most first; those whose cost is not known last") + "</span></div>";
  }

  function thList() {
    return '<section class="thlist" aria-label="Threads">' + thBar() +
      (threads.list.length ? threads.list.map(thRow).join("")
        : threads.project ? '<p class="thquiet thpad">No threads of ' + esc(threads.project) + " among the recent ones.</p>"
        : '<p class="thquiet thpad">No threads yet: one starts when an agent asks another, hands work to another project, or you tell an agent something directly.</p>') +
      (threads.more ? '<button type="button" class="ghost thmore" data-act="th:more">Older threads</button>' : "") + "</section>";
  }

  function thLaneSub(l) {
    if (l.name === "user") return "the user";
    if (l.role === "primary") return "herd-wide";
    return [l.project, l.worktree ? "worktree" : l.role === "lead" ? "lead" : ""].filter(Boolean).join(" · ") || l.kind || "";
  }

  // Where it is waiting now: the deepest open asks first.
  function thWaiting(t) {
    const w = t.waiting || [];
    if (!w.length) return "";
    const deepest = Math.max(...w.map((x) => x.depth || 0));
    return w.map((x) => {
      if ((x.depth || 0) === deepest && x.working) return thWho(x.to) + " is working · " + thAge(x.since);
      if (x.status === "queued") return thWho(x.from) + "'s ask to " + thWho(x.to) + " is queued";
      return thWho(x.from) + " waits on " + thWho(x.to);
    }).join(" · ");
  }

  function thSummary(t) {
    const first = (t.msgs || [])[0];
    const ask = (t.msgs || []).find((m) => m.kind === "ask" || m.kind === "handoff");
    const who = (t.lanes || []).length;
    if (t.direct) return "You told " + thWho(t.rootTo) + " directly, not through a lead";
    return (first && first.kind === "message" ? "You asked " + thWho(first.to) + "; " : "") +
      (ask ? thWho(ask.from) + (ask.kind === "handoff" ? " handed it to " : " asked ") + thWho(ask.to) : "") +
      (who > 2 ? " · " + who + " in it" : "") + (t.cross ? " · across projects" : "");
  }

  // OPENMAP / MSGACT let the Inbox reuse this exact renderer with its own
  // expand state and its own toggle action (so clicks re-render the Inbox,
  // not the Threads tab); the Threads tab keeps the defaults.
  function thDiagram(t, openMap, msgAct) {
    openMap = openMap || threads.open;
    msgAct = msgAct || "th:msg";
    const lanes = t.lanes || [];
    const n = Math.max(1, lanes.length);
    const at = {};
    lanes.forEach((l, i) => (at[l.name] = i));
    const cols = "grid-template-columns:repeat(" + n + ",minmax(0,1fr));";
    const heads = lanes.map((l) =>
      '<div class="thlanehead"><span class="thlanechip k-' + esc(l.kind || "none") + '">' + esc(thWho(l.name)) + "</span>" +
      '<span class="thlanesub">' + esc(thLaneSub(l)) + "</span></div>").join("");
    const rows = (t.msgs || []).map((m, i) => {
      const a = at[m.from], b = at[m.to];
      if (a === undefined || b === undefined) return "";
      const lo = Math.min(a, b), hi = Math.max(a, b);
      const inset = (50 / (hi - lo + 1)).toFixed(3) + "%";
      const gc = "grid-column:" + (lo + 1) + " / " + (hi + 2) + ";";
      const right = b >= a;
      const open = !!openMap[i];
      const round = thRound(m);
      const life = lanes.map((_, j) => '<div class="thlife" style="grid-column:' + (j + 1) + '"></div>').join("");
      return '<div class="thmsg k-' + esc(m.kind) + '" style="' + cols + '" data-i="' + i + '" data-dir="' + (right ? "right" : "left") + '">' + life +
        '<div class="tharrowcell" style="' + gc + '">' +
        '<div class="thline" style="left:' + inset + ";right:" + inset + '"></div>' +
        '<svg class="thhead" style="' + (right ? "right:calc(" + inset + " - 2px)" : "left:calc(" + inset + " - 2px)") + '" width="10" height="10" viewBox="0 0 10 10" aria-hidden="true">' +
        '<path d="' + (right ? "M1 1 L9 5 L1 9" : "M9 1 L1 5 L9 9") + '"></path></svg>' +
        '<span class="thlabel">' + esc(thLabel(m)) + "</span></div>" +
        '<button type="button" class="thcard" style="' + gc + '" data-act="' + msgAct + '" data-i="' + i + '" aria-expanded="' + open + '" title="' + (open ? "Show less" : "Show the whole message") + '">' +
        '<span class="thmeta"><span>' + esc(thWho(m.from)) + " → " + esc(thWho(m.to)) + "</span><span>· " + esc(thTime(m.at)) + "</span>" +
        (round ? "<span>· " + esc(round) + "</span>" : "") + (m.logged ? "<span>· as the herd log has it</span>" : "") +
        (m.transcript ? "<span>· as its task's transcript has it</span>" : "") + "</span>" +
        '<span class="thtext">' + esc(open ? m.text : m.head) + "</span>" + thTrace(m, open) + "</button>" +
        (m.images ? '<div class="thimgs" style="' + gc + '">' + imagesHtml(m.images) + "</div>" : "") + "</div>";
    }).join("");
    const wait = thWaiting(t);
    return '<div class="thdiagwrap"><div class="thdiag">' +
      '<div class="thlanes" style="' + cols + '">' + heads + "</div>" + rows +
      (wait ? '<div class="thwaitrow"><span class="thwait">' + esc(wait) + "</span></div>" : "") + "</div></div>" +
      '<div class="thlegend"><span><i class="lg k-ask"></i>ask</span><span><i class="lg k-answer"></i>answer</span>' +
      '<span><i class="lg k-handoff"></i>handoff to another project</span><span><i class="lg k-message"></i>message</span>' +
      '<span><i class="lg k-failed"></i>failed</span></div>';
  }

  function thHeader(t) {
    const ctx = thContext(t);
    return '<div class="thhead1">' + thStatus(t.status) + '<h1 class="thh1">' + esc(t.title || "(no text)") + "</h1></div>" +
      (ctx ? '<p class="thctx thh1ctx" title="What it goes on from">← ' + esc(ctx) + "</p>" : "") +
      '<p class="thsummary">' + esc(thSummary(t)) + "</p>" + thCostLine(t.cost) + thCommits(t.cost);
  }

  function thMain() {
    const t = threads.cur;
    if (threads.error) return '<p class="thquiet">' + esc(threads.error.replace(/^.*?:\s*/, "")) + "</p>";
    if (!t) return threads.req ? '<p class="thquiet">Reading the thread…</p>'
      : '<p class="thquiet">Pick a thread to see who said what to whom.</p>';
    return thHeader(t) + thDiagram(t);
  }

  function threadsDeskHtml() {
    return thTalk() + thCosts() + '<div class="thbody">' + thList() + '<main class="thmain" aria-label="Thread">' + thMain() + "</main></div>";
  }

  // The phone: the thread as a conversation, each message "A → B".  The
  // Inbox reuses it inline (OPENMAP / MSGACT, as in `thDiagram').
  function thConv(t, openMap, msgAct) {
    openMap = openMap || threads.open;
    msgAct = msgAct || "th:msg";
    const wait = thWaiting(t);
    return (t.msgs || []).map((m, i) => {
      const open = !!openMap[i];
      const round = thRound(m);
      return '<button type="button" class="thpmsg k-' + esc(m.kind) + '" data-act="' + msgAct + '" data-i="' + i + '" aria-expanded="' + open + '">' +
        '<span class="thphead"><span class="thpwho">' + esc(thWho(m.from)) + "</span>" +
        '<span class="tharrow k-' + esc(m.kind) + '">' + (m.kind === "handoff" ? "⇢" : "→") + "</span>" +
        '<span class="thpwho">' + esc(thWho(m.to)) + "</span>" +
        '<span class="thpkind">' + esc(thLabel(m)) + "</span></span>" +
        '<span class="thmeta"><span>' + esc(thTime(m.at)) + "</span>" + (round ? "<span>· " + esc(round) + "</span>" : "") + "</span>" +
        '<span class="thtext">' + esc(open ? m.text : m.head) + "</span>" + thTrace(m, open) + "</button>" + imagesHtml(m.images);
    }).join("") + (wait ? '<p class="thwait">' + esc(wait) + "</p>" : "");
  }

  function threadsPhoneHtml() {
    if (threads.phone && threads.req) {
      const t = threads.cur;
      return '<div class="thpback"><button type="button" class="ghost" data-act="th:back">← Threads</button></div>' +
        (t ? thHeader(t) + '<div class="thconv">' + thConv(t) + "</div>" : thMain());
    }
    return thTalk() + thCosts() + thList();
  }

  // A small text link to the full Threads tab, in the inline thread's header.
  function inboxThreadLink(it) {
    return '<button type="button" class="ibthlink" data-act="th:from-inbox" data-id="' + esc(it.id) + '">Open in Threads →</button>';
  }

  // The thread an item belongs to, drawn inline below its own content: the
  // desk swimlane or (phone) the compact "A → B" list -- the SAME renderers
  // the Threads tab uses, with the Inbox's own expand state.  Only for the
  // selected item, whose thread Emacs fetched (D is `inbox.detail' for it).
  function inboxThreadInline(it, phone, d) {
    if (!d || !d.thread || !(d.thread.msgs || []).length) return "";
    const t = d.thread;
    const body = phone
      ? '<div class="thconv">' + thConv(t, inbox.thOpen, "ib:thread-msg") + "</div>"
      : thDiagram(t, inbox.thOpen, "ib:thread-msg");
    return '<section class="ibthwrap"><div class="ibthhead"><span class="ibthlab">Thread</span>' +
      inboxThreadLink(it) + "</div>" + body + "</section>";
  }

  // A project picked on the desk (REPICK): once the herd's list of it
  // comes, the thread open, when that list has it not, gives way to its
  // first.  Judged by the herd's list, not the loaded page's preview of
  // it: by cost the herd's first may be one the page had not loaded, and
  // a thread opened from the preview would be in its list, and kept.
  // True when it opened one.
  function thRepick() {
    if (!threads.repick) return false;
    threads.repick = false;
    if (thNarrow() || !threads.list.length) return false;
    const id = thOpenId();
    if (id && threads.list.some((t) => t.id === id)) return false;
    openThread({ id: threads.list[0].id });
    return true;
  }

  function onThreadsAction(act, el) {
    if (act === "open") {
      const id = el.getAttribute("data-id"), ask = el.getAttribute("data-ask");
      if (view !== "threads") showThreads(id ? { id } : { ask });
      else openThread(id ? { id } : { ask });
    } else if (act === "more") {
      emit("threads-more");
    } else if (act === "project" || act === "sort") {
      if (act === "project") threads.project = el.getAttribute("data-project") || "";
      else threads.sort = el.getAttribute("data-sort") === "cost" ? "cost" : "recent";
      thViewKeep();
      emit("threads-view", { project: threads.project, sort: threads.sort });
      // From the herd's list, not the one shown: a chip picked again
      // before the herd answers would filter a filtered list.
      threads.list = thPreview(threads.raw);
      // On the desk, a project's list without the thread open opens its
      // first, once the herd's list of it comes (thRepick).
      if (act === "project") threads.repick = true;
      renderThreads();
    } else if (act === "cost-days") {
      threads.costDays = +el.getAttribute("data-days") || 7;
      renderThreads();
    } else if (act === "cost-table") {
      threads.costOpen = !threads.costOpen;
      renderThreads();
    } else if (act === "msg") {
      const i = el.getAttribute("data-i");
      threads.open[i] = !threads.open[i];
      renderThreads();
    } else if (act === "back") {
      threads.phone = false;
      threads.req = null;
      threads.cur = null;
      emit("thread-close");
      renderThreads();
    } else if (act === "from-inbox") {
      const it = inbox.items.find((x) => x.id === el.getAttribute("data-id"));
      if (!it) return;
      const ask = /^(?:ask-failed|handoff):(.+)$/.exec(it.id);
      showThreads(ask ? { ask: ask[1] } : { agent: it.agent });
    }
  }


  // ---- Board -----------------------------------------------------------
  // The herd's work at a glance, item by item: what work there is, what is
  // still open and who waits on whom across projects, the pull requests
  // and how they stand, and each item's history.  A work item is the asks
  // of one piece of work, put together by the herd (server/board.py) and
  // put right by a lead (`herd item').  Emacs sends it only while the tab
  // is shown, in parts: the whole once, then the cards that changed, by
  // version (boardPatch); an item's timeline when its card is opened
  // (boardTimeline).  Text in Traditional Chinese, Jing's own.

  let board = { got: false, v: 0, cards: {}, waits: [], jing: [], meta: { counts: {}, edges: [], days: 1, more: false },
                error: "", filter: "all", graph: "now", fold: new Map(), tlOpen: new Set(), tlAll: new Set(), tl: {},
                days: boardDaysKept(), html: {}, order: "", flash: "", goto: "", openProj: new Set(),
                // The project the Board is narrowed to ("" for all), the badge's
                // counts Emacs looked at while the tab was closed, where the page
                // was when a name took it to a room, the inline replies.
                proj: "", peek: null, back: null, replying: "", replyText: {},
                // The ticket the Board is narrowed to ("" none, "none" the items
                // with none), what ClickUp says of each ticket, the rows opened.
                ticket: "", tickets: [], openTkt: new Set() };
  let boardWatching = false;
  let boardTimer = 0;

  // Each status: its label, its order on the board, and whether its card
  // starts folded: work going on open, work at rest folded.
  const BD_STATUS = { live: ["進行中", 0, false], jing: ["等 Jing", 1, false], orphan: ["沒人接", 2, false],
                      tail: ["收尾", 3, false], quiet: ["安靜", 4, true], done: ["完成", 5, true] };
  const BD_PR = { OPEN: ["open", "opn"], MERGED: ["已 merge", "mrg"], DECLINED: ["關閉", "off"], SUPERSEDED: ["被取代", "off"] };
  const BD_PR_ORDER = ["OPEN", "MERGED", "DEPLOYED", "DECLINED", "SUPERSEDED", ""];
  const BD_KIND = { ask: "ask", handoff: "handoff", answer: "回覆", failed: "失敗", cancelled: "撤回",
                    need: "Jing", deploy: "部署", open: "開 PR", merge: "merge", closed: "完成" };
  const BD_NEED_STATE = { done: "已處理", replaced: "lead 後來改寫", none: "", closed: "項目已關" };

  // How far back the viewer lists quiet and done items, kept for them.
  function boardDaysKept() {
    try { return localStorage.getItem("ghostherd.board.days") === "7" ? 7 : 1; } catch (e) { return 1; }
  }
  function boardDaysKeep(d) {
    try { localStorage.setItem("ghostherd.board.days", String(d)); } catch (e) { /* this page keeps it */ }
  }

  function bdTime(ms) {
    if (!ms) return "";
    const d = new Date(ms), p = (n) => String(n).padStart(2, "0");
    const day = d.toDateString() === new Date().toDateString() ? "" : (d.getMonth() + 1) + "/" + d.getDate() + " ";
    return day + p(d.getHours()) + ":" + p(d.getMinutes());
  }
  function bdAge(ms) {
    if (!ms) return "";
    const s = Math.max(0, (Date.now() - ms) / 1000);
    return s < 60 ? "剛剛" : s < 3600 ? Math.floor(s / 60) + " 分" : s < 86400 ? Math.floor(s / 3600) + " 小時" : Math.floor(s / 86400) + " 天";
  }
  function bdClip(s, n) { s = String(s || ""); return s.length > n ? s.slice(0, n - 1) + "…" : s; }
  function bdCards() {
    return Object.values(board.cards).sort((a, b) =>
      (BD_STATUS[a.status] || [0, 9])[1] - (BD_STATUS[b.status] || [0, 9])[1] || (b.last || 0) - (a.last || 0));
  }
  function bdFolded(c) {
    const mine = board.fold.get(c.id);
    return mine === undefined ? !!(BD_STATUS[c.status] || [])[2] : mine;
  }
  function bdLink(id) {
    return id ? '<button type="button" class="bdilink" data-act="bd:goto" data-id="' + esc(id) + '" title="到這個工作項目">' + esc(id) + "</button>" : "";
  }
  function bdPrKey(p) { return p.host + ":" + p.owner + "/" + p.repo + "#" + p.id; }

  // An agent's name: a way to its room, at its column (or, in a chat, the
  // box written to it).  One that left the herd says so instead.
  function bdAgentInfo(name) { return ((board.meta && board.meta.agents) || []).find((a) => a.name === name) || null; }
  function bdAgent(name, cls) {
    if (!name) return "";
    if (name === "user" || name === "Jing" || /^lead of /.test(name)) return '<span class="bdnm' + (cls ? " " + cls : "") + '">' + esc(name === "user" ? "你" : name) + "</span>";
    const a = bdAgentInfo(name);
    return '<button type="button" class="bdname' + (a ? "" : " gone") + (cls ? " " + cls : "") + '" data-act="bd:agent" data-name="' + esc(name) +
      '" title="' + (a ? "到 " + esc(name) + " 的 room" : esc(name) + " 已不在 herd 裡") + '">' + esc(name) + "</button>";
  }
  function bdProjTag(p) {
    return '<button type="button" class="pj' + (board.proj === p ? " sel" : "") + '" data-act="bd:pfilter" data-p="' + esc(p) + '" title="只看 ' + esc(p) + '">' + esc(p) + "</button>";
  }
  // Narrowed to a project: its cards, the asks to or from it, what waits on Jing in it.
  function bdInProj(c) { return !board.proj || (c.projects || []).includes(board.proj); }
  // Narrowed to a ticket -- or to the items with none: with a project too,
  // both hold (a ticket spans projects; one project's part of it).
  // A card's tickets: as the sidecar names them, each with when last named;
  // from one before it did, its CU- labels, named when the item last moved.
  function bdTks(c) {
    return c.tickets || (c.labels || []).filter((l) => /^CU-[0-9a-z]+$/.test(l)).map((l) => ({ id: l.slice(3), last: c.last }));
  }
  function bdTkIds(c) { return bdTks(c).map((t) => t.id); }
  function bdInTicket(c) {
    if (!board.ticket) return true;
    const ids = bdTkIds(c);
    return board.ticket === "none" ? !ids.length : ids.includes(board.ticket);
  }
  function bdInFilter(c) { return bdInProj(c) && bdInTicket(c); }
  function bdItemIn(id) { const c = board.cards[id]; return !board.ticket || (c && bdInTicket(c)); }
  function bdWaitsShown() {
    return board.waits.filter((w) => (!board.proj || w.fromProj === board.proj || w.toProj === board.proj) && bdItemIn(w.item));
  }
  function bdTkInfo(id) { return (board.tickets || []).find((t) => t.id === id) || null; }
  function bdTkName(t) { return t === "none" ? "沒有單號的" : "CU-" + t; }
  function bdLeadOf(proj) {
    return ((board.meta && board.meta.agents) || []).find((a) => a.role === "lead" && leaf(a.project) === proj) || null;
  }
  // The badge: items nobody has, and those waiting on Jing by a lead's line.
  function bdUrgent(c) { return c.status === "orphan" || (c.status === "jing" && (c.needs || []).length > 0); }
  function bdPrState(p) {
    // A 部署 line naming it says more than the forge's MERGED.
    if (p.deployed && p.state !== "DECLINED") return ["已部署", "dep"];
    if (p.unsure || !p.state) return ["?", "unk"];
    return BD_PR[p.state] || [p.state.toLowerCase(), "unk"];
  }
  function bdCross(w) { return w.kind === "handoff" || (w.fromProj && w.toProj && w.fromProj !== w.toProj); }

  // The tab's red count: items nobody has (沒人接), and those waiting on Jing
  // by a lead's 需要 Jing line -- of those the Board shows, in its window
  // (board.days).  The tab open, from its own cards; closed, from what Emacs
  // looked at (boardBadge: herd_board_counts with the same window and the
  // sidecar's same rules, on news, never the board itself).
  function boardBadge() {
    const b = $("board-n");
    if (!b) return;
    const n = board.got && boardWatching ? Object.values(board.cards).filter(bdUrgent).length
      : board.peek ? (board.peek.jing || 0) + (board.peek.orphan || 0) : 0;
    b.textContent = n ? String(n) : "";
    b.className = "count" + (n ? " badge red" : "");
    b.title = n ? "沒人接或等 Jing 的工作項目" : "";
  }

  function showBoard() {
    if (herdNow.room) closeRoomState();
    view = tab = "board";
    src = null;
    chrome();
    app.innerHTML = '<div class="page boardpage"><div class="bdbody"></div></div>';
    board.html = {};
    board.order = "";
    // The Inbox's push too, first -- only what needs you, all it shows of
    // the Inbox: 要你處理 is drawn from it, before the board -- a heavier
    // look -- has come.  Asked in the same intent (Emacs starts both):
    // intents go one at a time, and a second one would hold the board back
    // a round trip on a phone's link.
    if (!boardWatching) {
      boardWatching = true;
      inboxWatching = true;
      emit("board-watch", { days: board.days, inbox: true });
    } else if (!inboxWatching) { inboxWatching = true; emit("inbox-watch", { needs: true }); }
    // Ages ("5 分") move on with no new data: drawn again now and then.
    clearInterval(boardTimer);
    boardTimer = setInterval(() => { if (view === "board") renderBoard(); }, 30000);
    renderBoard();
  }

  // ---- Its parts --------------------------------------------------------

  function bdStrip(cards) {
    const n = {};
    cards.forEach((c) => (n[c.status] = (n[c.status] || 0) + 1));
    const k = board.meta.counts || {};
    const seg = (cls, v) => '<span class="bdseg ' + cls + '" style="flex:' + v + '"></span>';
    const lg = (cls, label, v) => '<span class="bdlg ' + cls + '"><i></i>' + esc(label) + " " + v + "</span>";
    const order = Object.keys(BD_STATUS).filter((s) => n[s]);
    const days = bdRange(board.meta.days);
    // Which stretch each figure is of: the items, their asks and PRs the
    // window's -- said changing while the board of the one picked is on
    // its way; what is open, and what needs you, now.
    const range = (w) => '<span class="tlr' + (w && bdRanging() ? " ing" : "") + '">' +
      (w ? days + "內" + (bdRanging() ? " · 換成 " + bdRange(board.days) + "…" : "") : "現在") + "</span>";
    const t1 = '<button type="button" class="bdtile" data-act="bd:jump" data-to="items"><div class="tl1">工作項目' + range(true) + "</div>" +
      '<div class="big">' + cards.length + "</div>" +
      '<div class="sbar">' + order.map((s) => seg("st-" + s, n[s])).join("") + "</div>" +
      '<div class="legend">' + order.map((s) => lg("st-" + s, BD_STATUS[s][0], n[s])).join("") + "</div>" +
      '<div class="sub">' + (board.proj
        // Narrowed: the herd's counts are every project's; its own cards' asks instead.
        ? esc(board.proj) + " 的工作項目共 " + cards.reduce((s, c) => s + (c.asks || 0), 0) + " 則 ask"
        : days + "內 " + (k.asks || 0) + " 則 ask：跨專案 " + (k.xproj || 0) + "、FYI " + (k.fyi || 0) +
          "、轉述 " + (k.relays || 0) + "、撤回／失敗 " + (k.failed || 0)) + "</div></button>";
    const w = bdWaitsShown(), cross = w.filter(bdCross), inner = w.filter((x) => !bdCross(x));
    const oldest = w.reduce((a, b) => (!a || (b.since || 0) < (a.since || 0) ? b : a), null);
    const pairs = (list, f) => {
      const c = {};
      list.forEach((x) => { const key = f(x); c[key] = (c[key] || 0) + 1; });
      return Object.keys(c).slice(0, 2).map((key) => key + (c[key] > 1 ? " ×" + c[key] : "")).join("、");
    };
    const t2 = '<button type="button" class="bdtile" data-act="bd:jump" data-to="now"><div class="tl1">開著的等待' + range(false) + "</div>" +
      '<div class="big">' + w.length + "</div>" +
      '<div class="sub">' + (w.length ? "跨專案 " + cross.length + (cross.length ? "：" + esc(pairs(cross, (x) => (x.fromProj || x.from) + " → " + (x.toProj || x.to))) : "") +
        "<br>專案內 " + inner.length + (inner.length ? "：" + esc(pairs(inner, (x) => x.fromProj || x.from)) : "") +
        "<br>最久的開了 " + esc(bdAge(oldest.since)) + "（" + esc(bdTime(oldest.since)) + "）" : "沒有開著的 ask") + "</div></button>";
    const nd = bdNeeds(), nn = bdNeedCounts(nd);
    const t3 = '<button type="button" class="bdtile t-jing" data-act="bd:jump" data-to="need"><div class="tl1">要你處理' + range(false) + "</div>" +
      '<div class="big">' + nd.length + "</div>" +
      '<div class="sub">' + (nd.length ? NEED_RANKS.map((l, i) => (nn[i] ? (i === 2 ? '<b class="warn">' : "") + l + " " + nn[i] + (i === 2 ? "</b>" : "") : ""))
        .filter(Boolean).join("<br>") : "沒有要你處理的") + "</div></button>";
    const prs = bdAllPrs(cards);
    const ps = {};
    prs.forEach((r) => {
      const s = r.pr.deployed && r.pr.state !== "DECLINED" ? "DEPLOYED" : r.pr.unsure ? "" : r.pr.state || "";
      ps[s] = (ps[s] || 0) + 1;
    });
    const pcls = (s) => "s-" + (s === "DEPLOYED" ? "dep" : s ? (BD_PR[s] || ["", "unk"])[1] : "unk");
    const plab = (s) => (s === "DEPLOYED" ? "已部署" : s ? (BD_PR[s] || [s])[0] : "不明");
    const porder = BD_PR_ORDER.filter((s) => ps[s]);
    const t4 = '<button type="button" class="bdtile" data-act="bd:jump" data-to="prs"><div class="tl1">PR' + range(true) + "</div>" +
      '<div class="big">' + prs.length + "</div>" +
      '<div class="sbar">' + porder.map((s) => seg(pcls(s), ps[s])).join("") + "</div>" +
      '<div class="legend">' + porder.map((s) => lg(pcls(s), plab(s), ps[s])).join("") + "</div>" +
      '<div class="sub">狀態取自 Bitbucket／GitHub</div></button>';
    return '<section class="bdstrip">' + t1 + t2 + t3 + t4 + "</section>";
  }

  // ---- The map: projects, and who waits on whom between them ------------

  function bdGraph(mode) {
    const narrow = phone();
    const W = narrow ? 360 : 640, H = narrow ? 320 : 300, RX = narrow ? 44 : 48, RY = 16;
    const names = [];
    const add = (p) => { if (p && !names.includes(p) && names.length < 9) names.push(p); };
    const inner = {};
    const jingBy = {};
    if (mode === "now") {
      board.waits.forEach((w) => { add(w.fromProj); add(w.toProj); if (!bdCross(w) && w.fromProj) inner[w.fromProj] = (inner[w.fromProj] || 0) + 1; });
      board.jing.forEach((j) => {
        const c = j.item && board.cards[j.item];
        const p = j.project || (c && c.projects[0]) || "";
        if (p) { add(p); (jingBy[p] = jingBy[p] || []).push(j); }
      });
      bdCards().filter((c) => c.status !== "quiet" && c.status !== "done").forEach((c) => c.projects.forEach(add));
    } else {
      (board.meta.edges || []).slice().sort((a, b) => b.n - a.n).forEach((e) => { add(e.from); add(e.to); });
    }
    const jing = mode === "now" && Object.keys(jingBy).length > 0;
    if (!names.length) return '<p class="bdempty">' + (mode === "now" ? "沒有開著的等待" : "這段時間沒有跨專案的 ask") + "</p>";
    const pos = {};
    const cx = W / 2, cy = H / 2, rx = W / 2 - RX - 8, ry = H / 2 - RY - 22;
    if (names.length === 1 && !jing) pos[names[0]] = [cx, cy];
    else if (names.length === 2 && !jing) { pos[names[0]] = [cx - rx * 0.7, cy]; pos[names[1]] = [cx + rx * 0.7, cy]; }
    else names.forEach((n, i) => {
      const a = -Math.PI / 2 + (2 * Math.PI * i) / names.length;
      pos[n] = [cx + rx * Math.cos(a), cy + ry * Math.sin(a)];
    });
    if (jing) pos.Jing = [cx, cy];
    const busy = new Set();
    board.waits.forEach((w) => { busy.add(w.fromProj); busy.add(w.toProj); });
    const trim = (px, py, qx, qy) => {
      const dx = qx - px, dy = qy - py, d = Math.hypot(dx, dy) || 1, ux = dx / d, uy = dy / d;
      const r = 1 / Math.sqrt((ux / RX) ** 2 + (uy / RY) ** 2);
      return [px + ux * (r + 3), py + uy * (r + 3)];
    };
    const out = [];
    const edge = (a, b, bend, cls, label, width, dash, mk) => {
      const [x1, y1] = pos[a], [x2, y2] = pos[b];
      const mx = (x1 + x2) / 2, my = (y1 + y2) / 2, dx = x2 - x1, dy = y2 - y1, d = Math.hypot(dx, dy) || 1;
      const qx = mx - (dy / d) * bend, qy = my + (dx / d) * bend;
      const [sx, sy] = trim(x1, y1, qx, qy), [ex, ey] = trim(x2, y2, qx, qy);
      out.push('<path class="' + cls + '" d="M' + sx.toFixed(1) + "," + sy.toFixed(1) + " Q" + qx.toFixed(1) + "," + qy.toFixed(1) +
        " " + ex.toFixed(1) + "," + ey.toFixed(1) + '" stroke-width="' + width + '" marker-end="url(#bdm-' + mk + ')"' +
        (dash ? ' stroke-dasharray="5 4"' : "") + "/>");
      // A label as long as its edge leaves room for: its words cut to fit,
      // else what comes after "·" alone (how long it is open), else none.
      const room = Math.hypot(ex - sx, ey - sy) - 8;
      const fit = (s) => {
        let w = 0, out = "";
        for (const ch of String(s)) {
          w += /[\u2e80-\uffff]/.test(ch) ? 11 : 6.5;
          if (w > room) return out.length > 3 ? out.slice(0, -1) + "…" : "";
          out += ch;
        }
        return out;
      };
      let text = label ? fit(label) : "";
      if (label && (!text || text.endsWith("…")) && label.includes(" · ")) {
        const [what, age] = label.split(" · ");
        const short = fit(what.slice(0, Math.max(0, Math.floor((room - 6.5 * (age.length + 3)) / 11))) + "… · " + age);
        text = short && !short.startsWith("…") ? short : fit(age);
      }
      if (text) {
        const lx = 0.25 * sx + 0.5 * qx + 0.25 * ex, ly = 0.25 * sy + 0.5 * qy + 0.25 * ey - 5;
        out.push('<text class="el" x="' + lx.toFixed(1) + '" y="' + ly.toFixed(1) + '" text-anchor="middle"><title>' + esc(label) + "</title>" + esc(text) + "</text>");
      }
    };
    if (mode === "now") {
      const by = {};
      board.waits.filter(bdCross).forEach((w) => {
        const f = w.fromProj, t = w.toProj;
        if (!pos[f] || !pos[t] || f === t) return;
        const key = f + "\u0000" + t;
        (by[key] = by[key] || []).push(w);
      });
      Object.keys(by).forEach((key) => {
        const [f, t] = key.split("\u0000"), ws = by[key];
        const old = ws.reduce((a, b) => ((b.since || 0) < (a.since || 0) ? b : a));
        const label = (ws.length > 1 ? ws.length + " 則" : bdClip(old.head, 24)) + " · " + bdAge(old.since);
        edge(f, t, by[t + "\u0000" + f] ? 16 : 0, "e-now", label, 2.2, false, "now");
      });
      Object.keys(jingBy).forEach((p) => {
        if (!pos[p]) return;
        const js = jingBy[p];
        edge(p, "Jing", 0, "e-jing", js.length > 1 ? js.length + " 件" : bdClip(js[0].title, 24), 2, false, "jing");
      });
    } else {
      const es = board.meta.edges || [];
      const has = new Set(es.map((e) => e.from + "\u0000" + e.to));
      es.forEach((e) => {
        if (!pos[e.from] || !pos[e.to]) return;
        const back = has.has(e.to + "\u0000" + e.from);
        edge(e.from, e.to, e.relay ? (back ? 40 : 28) : back ? 14 : 0, e.relay ? "e-relay" : "e-all",
          (e.relay ? "轉述 " : "") + e.n, (1.2 + Math.min(e.n, 8) * 0.5).toFixed(1), e.relay, e.relay ? "relay" : "all");
      });
    }
    Object.keys(pos).forEach((n) => {
      const [x, y] = pos[n];
      const k = n === "Jing" ? "jing" : busy.has(n) || mode === "all" ? "act" : "idle";
      const sel = board.proj && n === board.proj ? " sel" : board.proj && n !== "Jing" ? " dim" : "";
      out.push('<g class="nd nd-' + k + sel + '"' + (n === "Jing" ? "" : ' data-act="bd:pfilter" data-p="' + esc(n) + '"') + '><title>' +
        (n === "Jing" ? "Jing" : (board.proj === n ? "不再只看 " : "只看 ") + esc(n)) + '</title><ellipse cx="' + x.toFixed(1) + '" cy="' + y.toFixed(1) + '" rx="' + RX + '" ry="' + RY + '"/>' +
        '<text x="' + x.toFixed(1) + '" y="' + (y + 4).toFixed(1) + '" text-anchor="middle" class="nl">' + esc(bdClip(n, 11)) + "</text></g>");
      if (mode === "now" && inner[n]) {
        out.push('<text class="bdg" x="' + x.toFixed(1) + '" y="' + (y + RY + 13).toFixed(1) + '" text-anchor="middle">內部 ' + inner[n] + "</text>");
      }
    });
    const mk = (id, color) => '<marker id="bdm-' + id + '" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">' +
      '<path d="M0,0 L10,5 L0,10 z" fill="' + color + '"/></marker>';
    return '<svg class="bdg-svg" viewBox="0 0 ' + W + " " + H + '" role="img" aria-label="' + (mode === "now" ? "開著的等待" : "跨專案的 ask") + '">' +
      "<defs>" + mk("now", "#79c0ff") + mk("jing", "#f85149") + mk("all", "#8b949e") + mk("relay", "#d29922") + "</defs>" + out.join("") + "</svg>";
  }

  function bdNow() {
    const n = (board.meta.edges || []).reduce((s, e) => s + e.n, 0);
    const tabs = '<div class="gtabs">' +
      '<button type="button" class="bdchip' + (board.graph === "now" ? " on" : "") + '" data-act="bd:graph" data-g="now">開著的</button>' +
      '<button type="button" class="bdchip' + (board.graph === "all" ? " on" : "") + '" data-act="bd:graph" data-g="all">' +
      esc(bdRange(board.meta.days)) + "內的跨專案 ask（" + n + "）</button></div>";
    const key = '<div class="gkey"><span><i class="k-now"></i>開著的 ask</span><span><i class="k-jing"></i>等 Jing</span>' +
      '<span><i class="k-all"></i>已結束（線越粗則數越多）</span><span><i class="k-relay"></i>轉述</span></div>';
    return '<div class="bdnow"><div class="gcard">' + tabs + '<div class="gscroll">' + bdGraph(board.graph) + "</div>" + key + "</div>" +
      '<div class="bdwaits">' + bdWaits() + "</div></div>";
  }

  function bdWaits() {
    const w = bdWaitsShown();
    const kids = {};
    w.forEach((x) => { if (x.under) (kids[x.under] = kids[x.under] || []).push(x); });
    const how = (x) => (x.working ? "在做" : x.status === "queued" ? "排隊中" : "沒在動");
    const ref = (ask) => '<button type="button" class="ref" data-act="bd:thread" data-ask="' + esc(ask) + '" title="在 Threads 打開">' + esc(ask) + "</button>";
    const chain = (x, depth) => (kids[x.ask] || []).map((k) =>
      '<div class="chain" style="padding-left:' + (4 + depth * 12) + 'px">└ ' + bdAgent(k.from) + " → " + bdAgent(k.to) +
      ' <span class="age">' + esc(bdTime(k.since)) + " · " + how(k) + "</span> " + esc(bdClip(k.head, 60)) + " " + ref(k.ask) + "</div>" +
      chain(k, depth + 1)).join("");
    const row = (x) => '<div class="wait"><div class="wrow">' + bdAgent(x.from, "who") +
      '<span class="arr">' + (x.kind === "handoff" ? "⇢" : "→") + "</span>" + bdAgent(x.to, "who") +
      '<span class="age">' + esc(bdTime(x.since)) + " · " + esc(bdAge(x.since)) + " · " + how(x) + "</span>" + ref(x.ask) + "</div>" +
      '<div class="what">' + esc(x.head) + " " + bdLink(x.item) + "</div>" + chain(x, 0) + "</div>";
    const top = w.filter((x) => !x.under);
    const cross = top.filter(bdCross), inner = top.filter((x) => !bdCross(x));
    let html = "";
    if (cross.length) html += '<div class="wl-h">跨專案</div>' + cross.map(row).join("");
    if (inner.length) html += '<div class="wl-h">專案內</div>' + inner.map(row).join("");
    if (!w.length) html += '<p class="bdempty">沒有開著的 ask</p>';
    return html;
  }

  // ---- The cards ----------------------------------------------------------

  function bdPrChip(p, cardId) {
    const [lab, cls] = bdPrState(p);
    const dest = p.dest ? " → " + p.dest : "";
    const title = (p.title || "") + (p.unsure ? "（上次沒查到，狀態可能已變）" : "") + (p.own ? "" : "（屬於 " + p.of + "）") +
      (p.opened ? "\n" + bdTime(p.opened.at) + " 回報開好（" + p.opened.ask + "）" : "") +
      (p.merged ? "\n" + bdTime(p.merged) + " merge" : "") +
      (p.deployed ? "\n部署：" + p.deployed.text + "（" + bdTime(p.deployed.at) + "）" : "");
    const inner = '<span class="prn">' + esc(p.repo + " #" + p.id) + '</span><span class="prs">' + esc(lab) + "</span>" +
      (dest ? '<span class="at">' + esc(dest) + "</span>" : "");
    const c = "bdpr s-" + cls + (p.own ? "" : " notown");
    return p.url ? '<a class="' + c + '" href="' + esc(p.url) + '" title="' + esc(title) + '">' + inner + "</a>"
      : '<span class="' + c + '" title="' + esc(title) + '">' + inner + "</span>";
  }

  function bdNeedLine(n, withItem) {
    return '<span class="bdneed"><span class="ntext">' + esc(n.text || n.title) + "</span>" +
      '<span class="nmeta">' + bdAgent(n.agent) + " · " + esc(bdTime(n.at)) + (withItem && n.item ? " " : "") + "</span>" +
      (withItem ? bdLink(n.item) : "") +
      '<button type="button" class="bddone" data-act="bd:need-done" data-event="' + esc(String(n.event || n.id)) + '" title="你處理好了：不再列為等你">已處理</button></span>';
  }

  // What a card waits on, and what can be done from it: 交給 lead for an
  // item nobody has, an inline reply for one waiting on Jing.
  function bdHand(c) {
    return '<div class="bdacts"><button type="button" class="bdact" data-act="bd:hand" data-id="' + esc(c.id) + '" title="打開它專案 lead 的 room，訊息先填好，你看過再送">交給 lead</button></div>';
  }

  function bdBlock(c) {
    const b = c.block || {};
    if (c.status === "orphan") {
      const age = bdAge(c.last);
      if (c.needs && c.needs.length)
        return '<div class="blk"><span class="bl">沒人接</span><span>安靜 ' + esc(age) + "，還有「需要 Jing」沒處理：" +
          c.needs.map((n) => bdNeedLine(n, false)).join("") + "</span></div>" + bdHand(c);
      const u = c.unanswered || {};
      const why = u.status === "failed" ? "失敗" : u.status === "cancelled" ? "撤回" : "對方停下沒回答";
      return '<div class="blk"><span class="bl">沒人接</span><span>安靜 ' + esc(age) + "，最後一則 ask " + why + "：" +
        bdAgent(u.from || "") + " → " + bdAgent(u.to || "") + "：" + esc(u.head || "") + "</span></div>" + bdHand(c);
    }
    if (c.needs && c.needs.length) {
      return '<div class="blk"><span class="bl">等 Jing</span><span>' + c.needs.map((n) => bdNeedLine(n, false)).join("") + "</span></div>" + bdReply(c);
    }
    if (c.status === "jing" || (c.jingOf && c.jingOf.length)) {
      const js = board.jing.filter((j) => (c.jingOf || []).includes(j.id));
      if (js.length && c.status === "jing") {
        return '<div class="blk"><span class="bl">等 Jing</span><span>' + js.map((j) =>
          bdAgent(j.agent) + "：" + '<button type="button" class="bdjl" data-act="bd:inbox" data-id="' + esc(j.id) + '">' + esc(j.title) + "</button>").join("、") +
          '<span class="guess">（依 agent 推得）</span></span></div>' + bdReply(c);
      }
    }
    if (b.kind === "open") {
      const how = b.working ? "在做" : b.status === "queued" ? "排隊中" : "沒在動";
      return '<div class="blk"><span class="bl">卡在</span><span>等 ' + bdAgent(b.to, "strong") + "（" + how + " · " + esc(bdAge(b.since)) + "）：" +
        esc(b.head) + "</span></div>";
    }
    const lab = b.kind === "failed" ? "失敗" : b.kind === "cancelled" ? "撤回" : "最後";
    return '<div class="blk"><span class="bl">' + lab + "</span><span>" + bdAgent(b.from || "") + (b.kind === "answered" ? " 回覆" : "") +
      " · " + esc(bdTime(b.since)) + "：" + esc(b.head || "") + "</span></div>";
  }

  // The answer to what a card waits on, written here: the Inbox's own way for
  // that item -- a lead's question through its lead (or primary), another
  // agent's to the agent -- and for a lead's 需要 Jing line, through it.
  function bdReplyTo(c) {
    const ids = (c.needs || []).map((n) => "need:" + n.id).concat(c.jingOf || []);
    return board.jing.find((j) => ids.includes(j.id) && j.replyVia) || null;
  }
  function bdReply(c) {
    const j = bdReplyTo(c);
    if (!j) return "";
    if (board.replying !== c.id)
      return '<div class="bdacts"><button type="button" class="bdact" data-act="bd:reply-open" data-id="' + esc(c.id) + '">回覆 ' + esc(j.agent) + "</button></div>";
    return '<div class="bdreply" data-id="' + esc(c.id) + '"><textarea rows="3" data-jid="' + esc(j.id) + '" placeholder="寫給 ' + esc(j.agent) +
      '（' + esc(j.replyVia === "agent" ? "直接給它" : j.replyVia === "primary" ? "經 primary" : "經它的專案 lead") + '）">' +
      esc(board.replyText[j.id] || "") + "</textarea>" +
      '<div class="bdacts"><button type="button" class="bdact send" data-act="bd:reply-send" data-id="' + esc(c.id) + '">送出</button>' +
      '<button type="button" class="bdact" data-act="bd:reply-close" data-id="' + esc(c.id) + '">取消</button></div></div>';
  }

  // Off the Board to a room, at an agent: where the page was is kept for
  // 回 Board; PREFILL, a message put in that agent's box, never sent.
  function bdToRoom(project, name, prefill) {
    board.back = { scroll: app.scrollTop };
    if (prefill && name) herdNow.compose[name] = prefill;
    if (name) herdNow.chatTo[project] = name;
    showAgents();
    herdNow.fromBoard = true;
    herdNow.roomLand = name || "";
    herdNow.roomLandAt = Date.now();
    openRoom(project);
  }

  // Back from the room to the Board, as it was: scrolled where it was, its
  // folds, filter and project as they were.
  function bdBack() {
    const at = board.back ? board.back.scroll : 0;
    setStatus("");
    showBoard();
    app.scrollTop = at;
    requestAnimationFrame(() => { app.scrollTop = at; });
  }

  // A room opened from the Board lands on its agent, once its columns are
  // drawn: that column scrolled into view and marked a moment; in a chat,
  // the box written to it.  One no longer in the room says so.
  function bdLand(chat) {
    const name = herdNow.roomLand;
    if (!name) return;
    const agents = roomAgents();
    if (!agents.some((a) => a.name === name)) {
      if (herdNow.data && Date.now() - (herdNow.roomLandAt || 0) > 4000) {
        herdNow.roomLand = "";
        setStatus(name + " 不在這個 room 了");
      }
      return;
    }
    herdNow.roomLand = "";
    if (chat) {
      setChatTo(name);
      const ta = app.querySelector(".chat .cfoot textarea");
      if (ta && herdNow.compose[name]) { ta.value = herdNow.compose[name]; ta.focus(); }
      setStatus("寫給 " + name);
      return;
    }
    const col = roomColumn(name);
    if (!col) return;
    col.scrollIntoView({ inline: "center", block: "nearest" });
    col.setAttribute("data-landed", "1");
    setTimeout(() => col.removeAttribute("data-landed"), 2200);
    const ta = col.querySelector("textarea.agent-text");
    if (ta && herdNow.compose[name]) ta.focus();
  }

  // An item's timeline: its key events first -- the ask that began it, each
  // 需要 Jing and 部署 line, each PR said opened and merged (the forge's
  // time), a request that failed, the closing, what is open -- every ask
  // and answer on asking, the FYI and 收到 among them last of all.
  function bdTimeline(c) {
    const t = board.tl[c.id];
    const folded = [];
    if (c.fyi) folded.push(c.fyi + " 則 FYI");
    if (c.acks) folded.push(c.acks + " 則「收到」");
    const open = board.tlOpen.has(c.id);
    const all = board.tlAll.has(c.id);
    const evs = t && t.events ? bdWithMerges(t.events, c) : null;
    const keys = evs ? evs.filter((e) => e.key).length : 0;
    const sum = '<button type="button" class="bdtlb" data-act="bd:tl" data-id="' + esc(c.id) + '" aria-expanded="' + open + '">' +
      (open ? "▾ " : "▸ ") + "時間線 · " + (evs && !all ? keys + " 件關鍵事件" : c.asks + " 則 ask") +
      (folded.length ? '<span class="fold">（折疊：' + folded.join("、") + "）</span>" : "") + "</button>";
    if (!open) return '<div class="bdtl">' + sum + "</div>";
    if (!t || (!t.events && !t.error)) return '<div class="bdtl">' + sum + '<p class="bdempty">讀取中…</p></div>';
    if (t.error) return '<div class="bdtl">' + sum + '<p class="bdempty">讀不到：' + esc(t.error) + "</p></div>";
    const shown = evs.filter((e) => (all ? true : e.key && !e.fold));
    const rest = evs.length - shown.length;
    return '<div class="bdtl">' + sum + "<ol>" + shown.map((e) => {
      const st = e.kind === "need" && e.state && e.state !== "open" ? '<span class="nst">' + esc(BD_NEED_STATE[e.state] || e.state) + "</span>" : "";
      const who = e.kind === "need" ? bdAgent(e.from) + " → Jing" : e.from && e.to ? bdAgent(e.from) + " → " + bdAgent(e.to) : bdAgent(e.from || "");
      return '<li class="bk-' + esc(e.kind) + (e.fold ? " folded" : "") + (e.state && e.state !== "open" ? " past" : "") + '">' +
        '<span class="tt">' + esc(bdTime(e.at)) + "</span>" +
        '<span class="kk">' + esc((e.relay ? "轉述" : BD_KIND[e.kind] || e.kind) + (e.auto ? "*" : "")) + "</span>" +
        '<span class="tx">' + (who ? '<span class="ft">' + who + "</span> " : "") + esc(e.head) + st +
        (e.ask ? ' <button type="button" class="ref" data-act="bd:thread" data-ask="' + esc(e.ask) + '" title="在 Threads 打開">' + esc(e.ask) + "</button>" : "") +
        "</span></li>";
    }).join("") + "</ol>" + (rest || all ? '<button type="button" class="bdmore" data-act="bd:tlall" data-id="' + esc(c.id) + '">' +
      (all ? "只看關鍵事件" : "顯示全部（再 " + rest + " 則 ask／回覆）") + "</button>" : "") + "</div>";
  }

  // The forge's merge of each of the item's own PRs, among its events.
  function bdWithMerges(events, c) {
    const merges = (c.prs || []).filter((p) => p.own && p.merged).map((p) => ({
      kind: "merge", at: p.merged, head: p.repo + " #" + p.id + " merge" + (p.dest ? " 進 " + p.dest : ""), key: true, from: "", to: "" }));
    return events.concat(merges).sort((a, b) => (a.at || 0) - (b.at || 0));
  }

  function bdCard(c) {
    const st = BD_STATUS[c.status] || [c.status, 9, false];
    const fold = bdFolded(c);
    const quiet = c.status === "quiet" || c.status === "done";
    const head = '<button type="button" class="wh" data-act="bd:fold" data-id="' + esc(c.id) + '" aria-expanded="' + !fold + '">' +
      '<span class="key">' + esc(c.id) + '</span><span class="pill">' + esc(st[0]) + "</span>" +
      (c.status === "live" && c.jingOf && c.jingOf.length ? '<span class="tag t-jing">等 Jing</span>' : "") +
      (c.cross ? '<span class="tag">跨專案</span>' : "") +
      '<span class="n">' + c.asks + " 則 ask" + (c.open ? "（開著 " + c.open + "）" : "") + " · 最後 " + esc(bdTime(c.last)) + "</span></button>";
    const title = '<h3 title="' + esc(c.askTitle ? "第一則 ask：" + c.askTitle : c.named ? "lead 取的名字" : "") + '">' + esc(c.title) + "</h3>";
    const projs = c.projects.map(bdProjTag).join("");
    if (fold) {
      return '<article class="wi st-' + esc(c.status) + ' folded" id="bd-w-' + esc(c.id) + '">' + head + title +
        '<div class="meta">' + projs + (c.prs.length ? '<span class="ag">' + c.prs.length + " 個 PR</span>" : "") +
        (quiet ? '<span class="ag">' + esc(bdBlockLine(c)) + "</span>" : "") + "</div></article>";
    }
    const labels = (c.labels || []).map((l) => /^CU-[0-9a-z]+$/.test(l)
      ? '<button type="button" class="tk' + (board.ticket === l.slice(3) ? " sel" : "") + '" data-act="bd:tfilter" data-t="' + esc(l.slice(3)) +
        '" title="只看 ' + esc(l) + '">' + esc(l) + "</button>"
      : '<span class="tk">' + esc(l) + "</span>").join("");
    const meta = '<div class="meta">' + labels + projs + '<span class="ag">' + c.agents.map((n) => bdAgent(n)).join("、") + "</span></div>";
    const prs = '<div class="prl">' + (c.prs.length ? c.prs.map((p) => bdPrChip(p, c.id)).join("") : '<span class="nopr">沒有 PR</span>') + "</div>";
    const links = c.links && c.links.length ? '<div class="bdlinks">提到：' + c.links.map(bdLink).join(" ") + "</div>" : "";
    const dep = c.deploys && c.deploys.length ? c.deploys[c.deploys.length - 1] : null;
    const deploy = dep ? '<div class="bddep"><span class="bl">部署</span><span>' + esc(dep.text) + ' <span class="nmeta">' +
      bdAgent(dep.agent) + " · " + esc(bdTime(dep.at)) + "</span></span></div>" : "";
    return '<article class="wi st-' + esc(c.status) + '" id="bd-w-' + esc(c.id) + '">' + head + title + meta + prs + bdBlock(c) + deploy + links + bdTimeline(c) + "</article>";
  }

  function bdBlockLine(c) {
    const b = c.block || {};
    return b.head ? bdClip(b.head, 60) : "";
  }

  function bdItems(cards) {
    const n = { all: cards.length, busy: 0, quiet: 0, done: 0 };
    cards.forEach((c) => { if (c.status === "quiet") n.quiet++; else if (c.status === "done") n.done++; else n.busy++; });
    const chip = (f, label) => '<button type="button" class="bdchip' + (board.filter === f ? " on" : "") + '" data-act="bd:filter" data-f="' + f + '">' +
      label + " " + n[f] + "</button>";
    const only = bdOnlyChips();
    return '<div class="bdchips">' + only + chip("all", "全部") + chip("busy", "還有事") + chip("quiet", "安靜") + chip("done", "完成") + "</div>";
  }

  function bdOnlyChips() {
    return (board.proj ? '<button type="button" class="bdchip on bdonly" data-act="bd:pfilter" data-p="' + esc(board.proj) +
      '" title="不再只看 ' + esc(board.proj) + '">只看 ' + esc(board.proj) + ' <span aria-hidden="true">✕</span></button>' : "") +
      (board.ticket ? '<button type="button" class="bdchip on bdonly bdonlyt" data-act="bd:tfilter" data-t="' + esc(board.ticket) +
        '" title="不再只看' + esc(bdTkName(board.ticket)) + '">只看 ' + esc(bdTkName(board.ticket)) + ' <span aria-hidden="true">✕</span></button>' : "");
  }

  function bdShown(cards) {
    const f = board.filter;
    return cards.filter((c) => f === "all" || (f === "busy" ? bdAwake(c) : c.status === f));
  }

  // ---- The default view: what needs Jing's eye, the rest folded ----------
  // On top only what is going on or waits on someone: live, waiting on
  // Jing, nobody's, a PR still open.  Of the items at rest, one a lead named
  // or with BD_OWN_ASKS asks or more keeps a line of its own: work of some
  // size, finished or dropped -- only a lead can say which.  The rest -- on
  // a day of the herd's, most items are one ask: an FYI, a relay, a
  // one-off look -- fold by project into a line each, a tap away.

  const BD_OWN_ASKS = 3;
  function bdAwake(c) { return c.status !== "quiet" && c.status !== "done"; }
  function bdOwnLine(c) { return !!c.named || c.asks >= BD_OWN_ASKS; }

  function bdPlan(cards) {
    if (board.filter !== "all") return { top: bdShown(cards), own: [], folds: [] };
    const rest = cards.filter((c) => !bdAwake(c));
    const by = {};
    rest.filter((c) => !bdOwnLine(c)).forEach((c) => {
      // Narrowed to a project, under it: an item of two projects is its too.
      const p = board.proj || (c.projects && c.projects[0]) || "其他";
      (by[p] = by[p] || []).push(c);
    });
    const folds = Object.keys(by).map((p) => ({
      proj: p, cards: by[p], last: Math.max(...by[p].map((c) => c.last || 0)),
      done: by[p].filter((c) => c.status === "done").length,
    })).sort((a, b) => b.last - a.last);
    return { top: cards.filter(bdAwake), own: rest.filter(bdOwnLine), folds };
  }

  function bdFoldHead(f) {
    const open = board.openProj.has(f.proj);
    const age = bdAge(f.last);
    return '<button type="button" class="bdfoldh" data-act="bd:proj" data-p="' + esc(f.proj) + '" aria-expanded="' + open + '">' +
      '<span class="fl"><span class="fp">' + esc(f.proj) + "</span>：零星 " + f.cards.length + " 則" +
      (f.done ? "（完成 " + f.done + "）" : "") + " · 最後 " + esc(age === "剛剛" ? age : age + "前") + "</span>" +
      '<span class="chev" aria-hidden="true">' + (open ? "▾" : "▸") + "</span></button>";
  }

  // ---- The pull requests, by repository --------------------------------

  function bdAllPrs(cards) {
    const by = {};
    cards.forEach((c) => (c.prs || []).forEach((p) => {
      const k = bdPrKey(p);
      const r = by[k] || (by[k] = { pr: p, items: [], last: 0 });
      if (p.own && !r.pr.own) r.pr = p;
      if (!r.items.includes(c.id)) r.items.push(c.id);
      r.last = Math.max(r.last, p.last || 0);
    }));
    return Object.values(by);
  }

  function bdPrs(cards) {
    const rows = bdAllPrs(cards);
    if (!rows.length) return '<p class="bdempty">這段時間的 ask 沒有提到 PR</p>';
    const repos = {};
    rows.forEach((r) => { const k = r.pr.owner + "/" + r.pr.repo; (repos[k] = repos[k] || []).push(r); });
    const order = Object.keys(repos).sort((a, b) => repos[b].length - repos[a].length || a.localeCompare(b));
    return '<div class="tblw"><table class="prt"><thead><tr><th>PR</th><th>進</th><th>內容</th><th>狀態</th><th>工作項目</th><th>最後提到</th></tr></thead>' +
      order.map((k) => '<tbody><tr class="rg"><th colspan="6">' + esc(k) + ' <span class="cnt">' + repos[k].length + "</span></th></tr>" +
        repos[k].sort((a, b) => b.pr.id - a.pr.id).map((r) => {
          const p = r.pr, [lab, cls] = bdPrState(p), own = r.items.find((i) => (board.cards[i] && (board.cards[i].prs || []).some((q) => bdPrKey(q) === bdPrKey(p) && q.own))) || r.items[0];
          const num = p.url ? '<a href="' + esc(p.url) + '">#' + p.id + "</a>" : "#" + p.id;
          const card = board.cards[own];
          return "<tr><td class=\"pnum\">" + num + '</td><td class="pto">' + esc(p.dest || "") + '</td><td class="pt">' + esc(p.title || "（還沒查到）") +
            '</td><td><span class="bdst s-' + cls + '">' + esc(lab) + "</span></td><td>" + bdLink(own) + ' <span class="pit">' + esc(bdClip(card ? card.title : "", 24)) + "</span>" +
            (r.items.length > 1 ? ' <span class="pit">＋' + (r.items.length - 1) + " 項提到</span>" : "") +
            '</td><td class="pat">' + esc(bdTime(r.last)) + "</td></tr>";
        }).join("") + "</tbody>").join("") + "</table></div>";
  }

  // ---- The tickets: one row each, its items a tap away -------------------
  // A ticket is a label on an item; here each the shown items carry is a row
  // -- what ClickUp says of it (the work chips' lookup and cache), its items
  // by status, their PRs, its projects, when an ask last named it -- and
  // opened, its items' cards: the Board grouped by ticket.  A hint where
  // ClickUp and the work disagree, display only: nothing is written to
  // ClickUp.

  function bdTicketRows(cards) {
    const by = {};
    cards.forEach((c) => bdTks(c).forEach((t) => {
      const r = by[t.id] || (by[t.id] = { id: t.id, cards: [], last: 0 });
      if (!r.cards.includes(c)) r.cards.push(c);
      r.last = Math.max(r.last, t.last || 0);
    }));
    return Object.values(by).sort((a, b) => b.last - a.last);
  }

  // Where ClickUp and the work disagree, by the status's type ClickUp gives
  // (open, custom, done, closed), never its name; nothing said while its
  // state is not known for sure.
  function bdMismatch(r, info) {
    if (!info || !info.type || info.unsure || info.error) return "";
    const done = info.type === "done" || info.type === "closed";
    if (done) {
      const busy = r.cards.filter((c) => c.status === "live" || c.status === "jing" || c.status === "orphan");
      if (!busy.length) return "";
      const n = {};
      busy.forEach((c) => (n[c.status] = (n[c.status] || 0) + 1));
      return "ClickUp 已完成，但還有工作項目" + Object.keys(n).map((k) => BD_STATUS[k][0] + " " + n[k]).join("、");
    }
    const all = bdAllPrs(r.cards).map((x) => x.pr), own = all.filter((p) => p.own);
    const prs = own.length ? own : all;
    const landed = (p) => !p.unsure && p.state !== "DECLINED" && (!!p.deployed || p.state === "MERGED");
    if (prs.length && prs.every(landed) && !r.cards.some((c) => c.status === "live"))
      return "ClickUp 還開著，但 PR 都已 merge／部署，也沒有進行中的工作項目";
    return "";
  }

  function bdTkLink(id, info) {
    const app = board.meta && board.meta.openTask === true && XW.elsewhere;
    const href = app ? "/open/clickup-view#task=" + encodeURIComponent(id) : (info && info.url) || "https://app.clickup.com/t/" + encodeURIComponent(id);
    return '<a class="bdtkid" href="' + esc(href) + '"' + (app ? ' data-app="clickup-view"' : "") + ' title="在 ClickUp 打開">CU-' + esc(id) + "</a>";
  }

  function bdTickets(cards) {
    const rows = bdTicketRows(cards);
    const none = cards.filter((c) => !bdTkIds(c).length);
    let mis = 0;
    const html = rows.map((r) => {
      const info = bdTkInfo(r.id);
      const hint = bdMismatch(r, info);
      if (hint) mis++;
      const open = board.openTkt.has(r.id);
      const st = !info || info.loading ? '<span class="bdtkst">…</span>'
        : info.error && !info.status ? '<span class="bdtkst unk" title="' + esc(info.error) + '">?</span>'
        : '<span class="bdtkst' + (info.unsure ? " unk" : "") + '" title="' + esc((info.type || "") + (info.unsure ? "（上次沒查到，狀態可能已變）" : "")) + '">' +
          '<i style="background:' + safeColor(info.color) + '"></i>' + esc(info.status || "?") + (info.unsure ? " ?" : "") + "</span>";
      const n = {};
      r.cards.forEach((c) => (n[c.status] = (n[c.status] || 0) + 1));
      const counts = r.cards.length + " 件：" + Object.keys(BD_STATUS).filter((k) => n[k]).map((k) => BD_STATUS[k][0] + " " + n[k]).join("、");
      const prs = bdAllPrs(r.cards).sort((a, b) => b.pr.own - a.pr.own);
      const projs = [];
      r.cards.forEach((c) => (c.projects || []).forEach((p) => { if (!projs.includes(p)) projs.push(p); }));
      return '<div class="bdtk' + (hint ? " mis" : "") + (open ? " open" : "") + '" data-t="' + esc(r.id) + '">' +
        '<div class="bdtkh" data-act="bd:tkopen" data-t="' + esc(r.id) + '" role="button" aria-expanded="' + open + '">' +
        // What wraps -- id, status, title -- apart from the ▸, which stays
        // on the title's line however long the title.
        '<span class="bdtkt">' + bdTkLink(r.id, info) + st + '<span class="bdtkname">' + esc((info && info.name) || "") + "</span></span>" +
        '<span class="chev" aria-hidden="true">' + (open ? "▾" : "▸") + "</span></div>" +
        '<div class="bdtkm"><span class="bdtkn">' + esc(counts) + "</span>" +
        (prs.length ? '<span class="bdtkprs">' + prs.map((x) => bdPrChip(x.pr)).join("") + "</span>" : "") +
        '<span class="bdtkp">' + projs.map(bdProjTag).join("") + "</span>" +
        '<span class="bdtkat">最後提到 ' + esc(bdTime(r.last)) + "</span>" +
        '<button type="button" class="bdact" data-act="bd:tfilter" data-t="' + esc(r.id) + '">' + (board.ticket === r.id ? "不再只看" : "只看這張") + "</button></div>" +
        (hint ? '<div class="bdtkmis">⚠ ' + esc(hint) + "</div>" : "") +
        (open ? '<div class="cards">' + r.cards.map(bdCard).join("") + "</div>" : "") + "</div>";
    }).join("");
    const noneProjs = [];
    none.forEach((c) => (c.projects || []).forEach((p) => { if (!noneProjs.includes(p)) noneProjs.push(p); }));
    const noneRow = none.length || board.ticket === "none"
      ? '<button type="button" class="bdtk bdtknone' + (board.ticket === "none" ? " sel" : "") + '" data-act="bd:tfilter" data-t="none">' +
        '<span class="bdtkid">沒有單號</span><span class="bdtkn">' + none.length + " 件" +
        (noneProjs.length ? " · " + esc(noneProjs.join("、")) : "") + "</span>" +
        // Drawn as the rows' 只看這張 is (the row itself is the button).
        '<span class="bdact bdtkonly">' + (board.ticket === "none" ? "不再只看" : "只看") + "</span></button>" : "";
    return {
      head: rows.length + " 個單號" + (mis ? " · 對不上 " + mis : "") + " · 狀態取自 ClickUp",
      html: (rows.length ? '<div class="bdtks">' + html + "</div>" : '<p class="bdempty">這些工作項目都沒有單號</p>') + noneRow,
    };
  }

  // ---- 要你處理: what needs Jing's hand, on top ----------------------------
  // One list of three: what the Inbox says needs you (inboxNeed: an agent at
  // a menu, a guard's call, a proposal, a question, a sign-in...), the
  // leads' 需要 Jing lines, and the items nobody has (沒人接) -- each once.
  // A lead's line is the Inbox's need:N item; one of the board's the Inbox
  // has not sent is made as the Inbox makes it (`bdNeedItem'); an item
  // nobody has whose line is listed rides on that line.  What stops an
  // agent until you act first, then what else waits on you, then what
  // nobody has, the newest first in each.  Each is handled here as in the
  // Inbox, by the Inbox's own code (inboxCard, inboxDetail, ib:*), opened in
  // place.  Drawn from the Inbox's push, which comes first and does not
  // wait on the board; the board's part as it comes.  Narrowed, as the
  // rest of the page is, to the project or ticket picked.

  const NEED_STOPS = new Set(["blocked", "guard", "proposal", "question", "login", "login-continue", "carry-on", "unsent", "relay"]);
  const NEED_RANKS = ["會卡住 agent", "需要 Jing", "沒人接"];
  // The lines of the board's the Inbox has not sent, as items, by id: what
  // the Inbox's actions take them from (onInboxAction).
  let bdNeedExtra = new Map();

  function bdNeedItem(j) {
    return { id: j.id, kind: "need", group: "need", tag: "需要 Jing", tone: "red", agent: j.agent || "", cli: "",
             project: j.projectDir || "", title: j.title || "", preview: j.title || "", at: j.at, read: true,
             whyLabel: "A lead's 需要 Jing line",
             body: (j.title || "") + (j.itemTitle ? "\n\nWork item " + j.item + ": " + j.itemTitle : ""),
             event: j.event, item: j.item || "" };
  }

  function bdNeeds() {
    const t = Date.now();
    const gone = (id) => (inbox.gone.get(id) || 0) > t;
    const guess = new Map(board.jing.map((j) => [j.id, j.item]));
    const out = [], seen = new Set();
    const add = (it, rank, at, more) => { seen.add(it.id); out.push(Object.assign({ it, rank, at: inboxMs(at) || 0 }, more || {})); };
    inbox.items.filter(inboxNeed).forEach((it) => {
      if (!gone(it.id)) add(it, NEED_STOPS.has(it.kind) ? 0 : 1, it.at, { item: it.item || guess.get(it.id) || "" });
    });
    bdNeedExtra = new Map();
    board.jing.filter((j) => j.line && !seen.has(j.id) && !gone(j.id)).forEach((j) => {
      const it = bdNeedItem(j);
      bdNeedExtra.set(it.id, it);
      add(it, 1, j.at, { item: it.item });
    });
    Object.values(board.cards).filter((c) => c.status === "orphan").forEach((c) => {
      const ids = (c.needs || []).map((n) => "need:" + n.id);
      const on = out.filter((e) => ids.includes(e.it.id));
      if (on.length) on.forEach((e) => (e.orphan = c));
      // Its line just handled: as handled, until the board has looked again.
      else if (!gone("orphan:" + c.id) && !ids.some(gone)) add({ id: "orphan:" + c.id }, 2, c.last, { card: c, item: c.id });
    });
    return out.filter((e) => {
      const c = e.card || board.cards[e.item];
      if (board.proj && !(c ? bdInProj(c) : leaf(e.it.project || "") === board.proj)) return false;
      return !board.ticket || (c ? bdInTicket(c) : false);
    }).sort((a, b) => a.rank - b.rank || b.at - a.at);
  }
  function bdNeedCounts(all) {
    const n = [0, 0, 0];
    all.forEach((e) => n[e.rank]++);
    return n;
  }

  // One of the list: the Inbox's card -- an item nobody has, drawn as one,
  // its block (why, 交給 lead) and the way to its card when opened.
  function bdNeedCard(e) {
    const narrow = phone();
    const hand = e.orphan ? '<div class="bdneedorph"><span class="tag t-jing">沒人接</span>' +
      "這個工作項目安靜 " + esc(bdAge(e.orphan.last)) + "，沒人接手" + bdHand(e.orphan) + "</div>" : "";
    if (!e.card) return inboxCard(e.it, { phone: narrow, head: e.orphan ? '<span class="tag t-jing">沒人接</span>' : "", after: hand });
    const c = e.card;
    const it = { id: e.it.id, tag: "沒人接", tone: "red", title: c.title, at: c.last, preview: bdBlockLine(c) };
    return inboxCard(it, {
      pick: "bd:npick", need: true,
      who: '<span class="ibcardwho">' + esc(c.id) + " · " + esc((c.projects || []).join("、")) + "</span>",
      body: bdBlock(c) + '<div class="ibacts"><button type="button" class="ibbtn" data-act="bd:goto" data-id="' + esc(c.id) + '">到工作項目 ' +
        esc(c.id) + "</button></div>",
    });
  }

  function bdNeedHtml() {
    const all = bdNeeds();
    const toast = inbox.toast ? '<div class="ibtoast" role="status">' + esc(inbox.toast) +
      (inbox.undo ? ' <button type="button" class="iblink" data-act="ib:undo">Undo</button>' : "") + "</div>" : "";
    // Nothing: a quiet line, not a section.
    if (!all.length) return toast + '<p class="bdneednone">' +
      (inbox.got || board.got ? "現在沒有要你處理的" : "看看有沒有要你處理的…") + "</p>";
    const n = bdNeedCounts(all);
    return '<h2 class="section red">要你處理 <span class="cnt">' + all.length + " 件 · " +
      NEED_RANKS.map((l, i) => (n[i] ? l + " " + n[i] : "")).filter(Boolean).join("、") + "</span></h2>" + toast +
      '<div class="bdneeds">' + all.map(bdNeedCard).join("") + "</div>";
  }

  // Drawn as the Inbox is: a reply box being written in found again after,
  // with its cursor and scroll; nothing drawn while a composition is open.
  function renderBoardNeed(box) {
    const el = box.querySelector(".bd-need");
    if (!el) return;
    const ta = inboxReplyFocused();
    if (ta && inboxComposing && el.contains(ta)) { inboxHeld = true; return; }
    const html = bdNeedHtml();
    if (board.html[".bd-need"] === html) return;
    inboxHeld = false;
    const kept = ta && el.contains(ta) && { id: ta.getAttribute("data-id"), start: ta.selectionStart, end: ta.selectionEnd,
                                            dir: ta.selectionDirection, top: ta.scrollTop };
    el.innerHTML = html;
    board.html[".bd-need"] = html;
    const again = kept && el.querySelector('.ibreplybox[data-id="' + cssName(kept.id) + '"]');
    if (!again) return;
    again.focus({ preventScroll: true });
    again.setSelectionRange(kept.start, kept.end, kept.dir);
    again.scrollTop = kept.top;
  }

  // ---- The stretch: 24 hours or 7 days, for the whole page ----------------

  function bdRange(d) { return d > 1 ? Math.round(d) + " 天" : "24 小時"; }
  // The board come is of another stretch than the one picked: on its way.
  function bdRanging() { return board.got && (board.meta.days > 1 ? 7 : 1) !== board.days; }

  function bdTop() {
    const btn = (d) => '<button type="button" class="bdday' + (board.days === d ? " on" : "") + '" data-act="bd:days" data-days="' + d +
      '" aria-pressed="' + (board.days === d) + '">' + bdRange(d) + "</button>";
    return '<div class="bdtop"><div class="bddays" role="group" aria-label="時間範圍：統計、工作項目、PR、單號">' + btn(1) + btn(7) + "</div>" +
      (board.proj || board.ticket ? '<div class="bdfilter">只看 ' +
        [board.proj ? "<b>" + esc(board.proj) + "</b>" : "", board.ticket ? "<b>" + esc(bdTkName(board.ticket)) + "</b>" : ""].filter(Boolean).join(" 裡 ") +
        " 的" + bdOnlyChips() + "</div>" : "") + "</div>";
  }

  // ---- Drawing ------------------------------------------------------------
  // Each part is drawn as text and put in only when it changed; a card
  // that changed is put in alone, unless the cards' order did.

  function bdSet(box, sel, html) {
    const el = box.querySelector(sel);
    if (el && board.html[sel] !== html) { el.innerHTML = html; board.html[sel] = html; }
  }

  // On top, at once, whatever the board: the stretch and what needs you
  // (from the Inbox's push); the rest once the board has come.
  function renderBoard() {
    if (view !== "board") return;
    const box = app.querySelector(".bdbody");
    if (!box) return;
    if (!box.querySelector(".bd-top")) {
      box.innerHTML = '<div class="bd-top"></div><div class="bd-need" id="bd-need"></div><div class="bd-main"></div>';
      board.html = {};
      board.order = "";
    }
    bdSet(box, ".bd-top", bdTop());
    renderBoardNeed(box);
    const main = box.querySelector(".bd-main");
    if (!board.got) {
      const wait = board.error ? '<p class="bdempty">讀不到 board：' + esc(board.error) + "</p>" : '<p class="bdempty">讀取工作項目…</p>';
      if (main.getAttribute("data-wait") !== wait) { main.innerHTML = wait; main.setAttribute("data-wait", wait); }
      return;
    }
    if (!main.querySelector(".bd-strip")) {
      main.removeAttribute("data-wait");
      main.innerHTML = '<div class="bd-strip"></div>' +
        '<h2 class="section" id="bd-now">現在 <span class="cnt">只列還開著的</span></h2><div class="bd-now"></div>' +
        '<h2 class="section" id="bd-items">工作項目 <span class="cnt">還有事的在前，同一種依最後動靜</span></h2><div class="bd-chips"></div><div class="bdarea"></div>' +
        '<h2 class="section" id="bd-prs">所有 PR <span class="cnt">依 repo 分組 · 狀態取自 Bitbucket</span></h2><div class="bd-prs"></div>' +
        '<h2 class="section" id="bd-tickets">所有單號 <span class="cnt bd-tn"></span></h2><div class="bd-tickets"></div>';
      Object.keys(board.html).forEach((k) => { if (k !== ".bd-top" && k !== ".bd-need") delete board.html[k]; });
      board.order = "";
    }
    const cards = bdCards().filter(bdInFilter);
    bdSet(box, ".bd-strip", bdStrip(cards));
    bdSet(box, ".bd-now", bdNow());
    bdSet(box, ".bd-chips", bdItems(cards));
    const area = box.querySelector(".bdarea");
    const plan = bdPlan(cards);
    const ids = (cs) => cs.map((c) => c.id).join(" ");
    const opened = plan.folds.filter((f) => board.openProj.has(f.proj));
    const shown = plan.top.concat(plan.own, ...opened.map((f) => f.cards));
    const htmls = {};
    shown.forEach((c) => (htmls[c.id] = bdCard(c)));
    // Its shape: which cards where, which folds open.  The same shape, the
    // cards and fold lines that changed are put in alone.
    const shape = board.filter + "|" + ids(plan.top) + "|" + ids(plan.own) + "|" +
      plan.folds.map((f) => f.proj + (board.openProj.has(f.proj) ? "=" + ids(f.cards) : "#" + f.cards.length)).join(";");
    if (shape !== board.order) {
      const card = (c) => htmls[c.id];
      const all = board.filter === "all";
      area.innerHTML = (all && !plan.top.length && (plan.own.length || plan.folds.length)
        ? '<p class="bdempty bdcalm">現在沒有要你看的：沒有進行中、等你、沒人接或 PR 還開著的工作項目</p>' : "") +
        (plan.top.length ? '<div class="cards">' + plan.top.map(card).join("") + "</div>" : "") +
        (plan.own.length ? '<div class="bdsub">安靜，但不只一兩則：lead 取了名，或有 ' + BD_OWN_ASKS + " 則以上的 ask</div>" +
          '<div class="cards">' + plan.own.map(card).join("") + "</div>" : "") +
        (plan.folds.length ? '<div class="bdsub">其餘安靜的，依專案收起</div><div class="bdfolds">' + plan.folds.map((f) =>
          '<div class="bdfold" data-p="' + esc(f.proj) + '">' + bdFoldHead(f) +
          (board.openProj.has(f.proj) ? '<div class="cards">' + f.cards.map(card).join("") + "</div>" : "") + "</div>").join("") + "</div>" : "") ||
        '<p class="bdempty">沒有這一類的工作項目</p>';
      board.order = shape;
      shown.forEach((c) => (board.html["card:" + c.id] = htmls[c.id]));
      plan.folds.forEach((f) => (board.html["fold:" + f.proj] = bdFoldHead(f)));
    } else {
      shown.forEach((c) => {
        if (board.html["card:" + c.id] === htmls[c.id]) return;
        const el = area.querySelector("#bd-w-" + CSS.escape(c.id));
        // Being written in: drawn again once the box lets go.
        if (el && el.contains(document.activeElement) && document.activeElement.tagName === "TEXTAREA") return;
        if (el) el.outerHTML = htmls[c.id];
        board.html["card:" + c.id] = htmls[c.id];
      });
      plan.folds.forEach((f) => {
        const h = bdFoldHead(f);
        if (board.html["fold:" + f.proj] === h) return;
        const el = area.querySelector('.bdfold[data-p="' + CSS.escape(f.proj) + '"] > .bdfoldh');
        if (el) el.outerHTML = h;
        board.html["fold:" + f.proj] = h;
      });
    }
    bdSet(box, ".bd-prs", bdPrs(cards));
    const tk = bdTickets(cards);
    bdSet(box, ".bd-tn", tk.head);
    bdSet(box, ".bd-tickets", tk.html);
  }

  // To a card, opened and marked: whatever folds or filter hid it.
  function bdGoto(id) {
    const c = board.cards[id];
    if (!c) { if (id) setStatus("工作項目 " + id + (board.got ? " 不在這段時間裡" + (board.days > 1 ? "" : "：看看 7 天的") : " 還沒讀到")); return; }
    if (board.filter !== "all" && !bdShown([c]).length) board.filter = "all";
    if (board.filter === "all" && !bdAwake(c) && !bdOwnLine(c)) board.openProj.add((c.projects && c.projects[0]) || "其他");
    board.fold.set(id, false);
    renderBoard();
    const card = $("bd-w-" + id);
    if (card) {
      card.scrollIntoView({ block: "center", behavior: "smooth" });
      card.classList.add("hl");
      setTimeout(() => card.classList.remove("hl"), 1600);
    }
  }

  function onBoardAction(act, el) {
    const id = el.getAttribute("data-id");
    if (act === "jump") {
      const to = $("bd-" + el.getAttribute("data-to"));
      if (to) to.scrollIntoView({ block: "start", behavior: "smooth" });
    } else if (act === "graph") {
      board.graph = el.getAttribute("data-g") === "all" ? "all" : "now";
      renderBoard();
    } else if (act === "filter") {
      board.filter = el.getAttribute("data-f") || "all";
      renderBoard();
    } else if (act === "proj") {
      const p = el.getAttribute("data-p") || "";
      if (board.openProj.has(p)) board.openProj.delete(p); else board.openProj.add(p);
      renderBoard();
    } else if (act === "fold") {
      const c = board.cards[id];
      if (c) board.fold.set(id, !bdFolded(c));
      renderBoard();
    } else if (act === "tl") {
      if (board.tlOpen.has(id)) board.tlOpen.delete(id);
      else {
        board.tlOpen.add(id);
        const c = board.cards[id], t = board.tl[id];
        if (!t || t.error || (c && t.last !== c.last)) { board.tl[id] = { asking: true }; emit("board-timeline", { id }); }
      }
      renderBoard();
    } else if (act === "tlall") {
      if (board.tlAll.has(id)) board.tlAll.delete(id); else board.tlAll.add(id);
      renderBoard();
    } else if (act === "goto") {
      bdGoto(id);
    } else if (act === "thread") {
      showThreads({ ask: el.getAttribute("data-ask") });
    } else if (act === "inbox") {
      // What needs you is handled at the top: opened there, else the Inbox.
      const it = bdNeeds().some((e) => e.it.id === id) && (inbox.items.find((x) => x.id === id) || bdNeedExtra.get(id));
      if (!it) { inbox.sel = id || ""; showInbox(); return; }
      if (inbox.sel !== id) inboxSelect(it);
      renderBoard();
      const to = $("bd-need");
      if (to) to.scrollIntoView({ block: "start", behavior: "smooth" });
    } else if (act === "npick") {
      // An item nobody has, in 要你處理: opened in place, one at a time.
      inbox.sel = inbox.sel === id ? "" : id;
      inbox.detail = null;
      inbox.thOpen = {};
      inbox.reply = "";
      inbox.replying = "";
      renderBoard();
    } else if (act === "agent") {
      const name = el.getAttribute("data-name") || "";
      const a = bdAgentInfo(name);
      if (!a) { setStatus(name + " 已不在 herd 裡（停了），沒有 room 可開"); return; }
      bdToRoom(a.project, name);
    } else if (act === "back") {
      closeRoom();
    } else if (act === "tfilter") {
      const t = el.getAttribute("data-t") || "";
      board.ticket = board.ticket === t ? "" : t;
      renderBoard();
    } else if (act === "tkopen") {
      const t = el.getAttribute("data-t") || "";
      if (board.openTkt.has(t)) board.openTkt.delete(t); else board.openTkt.add(t);
      renderBoard();
    } else if (act === "pfilter") {
      const p = el.getAttribute("data-p") || "";
      board.proj = board.proj === p ? "" : p;
      renderBoard();
    } else if (act === "hand") {
      const c = board.cards[id];
      if (!c) return;
      const proj = (c.projects || [])[0] || "";
      const lead = bdLeadOf(proj);
      if (!lead) { setStatus((proj || "這個項目") + " 現在沒有 lead：要先開一個，Board 不會自己開"); return; }
      const n = (c.needs || [])[0], u = c.unanswered;
      const why = u && (u.status === "failed" ? "失敗了" : u.status === "cancelled" ? "被撤回" : "對方停下沒回答");
      bdToRoom(lead.project, lead.name, "這件沒人接：" + c.title + "（工作項目 " + c.id + "）\n" +
        (n ? "還沒處理的需要 Jing：" + n.text : u ? "最後一則 ask " + why + "：" + (u.head || "") : ""));
    } else if (act === "reply-open" || act === "reply-close") {
      board.replying = act === "reply-open" ? id : "";
      renderBoard();
      const ta = act === "reply-open" && app.querySelector('.bdreply[data-id="' + CSS.escape(id) + '"] textarea');
      if (ta) ta.focus();
    } else if (act === "reply-send") {
      const c = board.cards[id], j = c && bdReplyTo(c);
      const ta = app.querySelector('.bdreply[data-id="' + CSS.escape(id) + '"] textarea');
      const text = ((ta && ta.value) || "").trim();
      if (!j) return;
      if (!text) { setStatus("先寫回覆"); return; }
      const lost = () => { board.replyText[j.id] = text; board.replying = id; renderBoard(); setStatus("沒送到，回覆還在框裡", "error"); };
      const sent = j.replyVia === "primary" ? emit("primary-say", { text: text }, lost)
        : j.replyVia === "agent" ? emit("agent-prompt", { name: j.agent, text: text }, lost)
        : emit("lead-say", { project: j.projectDir, text: text }, lost);
      if (sent === false) { setStatus(TOO_LONG, "error"); return; }
      board.replyText[j.id] = "";
      board.replying = "";
      setStatus("已送給 " + j.agent);
      renderBoard();
    } else if (act === "need-done") {
      const ev = +el.getAttribute("data-event");
      if (ev) {
        emit("board-need-done", { id: ev });
        el.disabled = true;
        el.textContent = "已處理…";
      }
    } else if (act === "days") {
      const d = +el.getAttribute("data-days") > 1 ? 7 : 1;
      if (d === board.days) return;
      board.days = d;
      boardDaysKeep(d);
      emit("board-days", { days: d });
      renderBoard();
    }
  }

  window.GM = {
    flash: (msg) => setStatus(msg),

    // The Board, in parts: the whole (FULL), or the cards that changed and
    // the ids gone since the version the page has -- one it does not have
    // is asked for whole again.  WAITS, JING and META come when they changed.
    boardPatch: (p) => {
      if (!p) return;
      if (!p.full && p.base !== board.v) { emit("board-resend"); return; }
      if (p.full) board.cards = {};
      (p.cards || []).forEach((c) => (board.cards[c.id] = c));
      (p.drop || []).forEach((id) => delete board.cards[id]);
      if ("waits" in p) board.waits = p.waits || [];
      if ("jing" in p) board.jing = p.jing || [];
      if ("tickets" in p) board.tickets = p.tickets || [];
      if (p.meta) board.meta = p.meta;
      board.v = p.v;
      board.got = true;
      board.error = "";
      const go = board.goto;
      // An open timeline whose card moved on is asked for again.
      board.tlOpen.forEach((id) => {
        const c = board.cards[id], t = board.tl[id];
        if (c && t && !t.asking && t.last !== c.last) { t.asking = true; emit("board-timeline", { id }); }
      });
      boardBadge();
      renderBoard();
      // Asked for from the Inbox before the board came: shown now.
      if (go && board.cards[go] && view === "board") {
        board.goto = "";
        onBoardAction("goto", { getAttribute: (k) => (k === "data-id" ? go : null) });
      }
    },

    boardTimeline: (p) => {
      if (!p || !p.id) return;
      const c = board.cards[p.id];
      board.tl[p.id] = p.error ? { error: String(p.error) } : { events: p.events || [], last: c ? c.last : p.last, asking: false };
      renderBoard();
    },

    // The badge's counts, looked at by Emacs while the tab is closed.
    boardBadge: (p) => {
      board.peek = p || null;
      boardBadge();
    },

    boardError: (p) => {
      board.error = String((p && p.error) || "error");
      if (!board.got) renderBoard();
    },

    // The Threads tab's list and what is open now, pushed while it is
    // shown.  The desk opens the newest thread when none is open.
    renderThreads: (p) => {
      // A list of the view before the viewer changed it, asked for before
      // they did: not shown.
      const v = p && p.view;
      const mine = !(v && v.sort && (v.project !== threads.project || v.sort !== threads.sort));
      if (mine) {
        threads.list = threads.raw = (p && p.threads) || [];
        threads.more = !!(p && p.more);
        threads.projects = (p && Array.isArray(p.projects) && p.projects) || [];
      }
      threads.talking = (p && p.talking) || [];
      threads.live = (p && p.live) || 0;
      threads.costs = (p && p.costs) || [];
      threadsBadge();
      if (view !== "threads") return;
      if (!threads.req && !thNarrow() && threads.list.length) openThread({ id: threads.list[0].id });
      else if (!(mine && thRepick())) renderThreads();
    },

    // One thread, whole -- or why it could not be read.
    renderThread: (p) => {
      if (!threads.req || !p) return;
      if (p.error) { threads.error = String(p.error); threads.cur = null; }
      else {
        if (!threads.cur || threads.cur.id !== p.id) { threads.open = {}; threads.toTop = true; }
        threads.cur = p;
        threads.error = "";
      }
      renderThreads();
    },

    // Just the tab's badge, for a page that has not opened it yet.
    threadsBadge: (p) => { threads.live = (p && p.live) || 0; threadsBadge(); },

    // The Inbox items and the Needs-you count, pushed by Emacs every few
    // seconds (and after an action).  The badge updates whatever the view.
    renderInbox: (p) => {
      inbox.got = true;
      const items = (p && p.items) || [];
      // Only what needs you, while the Board is shown: the updates the page
      // had are kept, for the Inbox until its own push comes.
      const ids = new Set(items.map((x) => x.id));
      inbox.items = p && p.needsOnly ? items.concat(inbox.items.filter((x) => !inboxNeed(x) && !ids.has(x.id))) : items;
      inboxKeepHeld();
      inbox.count = (p && p.count) || 0;
      if (inbox.sel && !inbox.items.some((x) => x.id === inbox.sel)) { inbox.sel = ""; inbox.detail = null; inbox.thOpen = {}; }
      inboxBadge();
      if (view === "inbox" || view === "board") renderInbox();
    },

    // The selected item's thread and/or full report, fetched by Emacs on the
    // pick (not every tick).  The two arrive as separate pushes, both tagged
    // with the item id, and are merged; a stale push for another item is kept
    // but shown only when that item is the one selected.
    inboxDetailData: (p) => {
      if (!p || !p.for) return;
      if (!inbox.detail || inbox.detail.for !== p.for) { inbox.detail = { for: p.for }; inbox.thOpen = {}; }
      if (p.thread) inbox.detail.thread = p.thread;
      if (typeof p.report === "string" && p.report) inbox.detail.report = p.report;
      if (Array.isArray(p.images)) inbox.detail.images = p.images;
      if ((view === "inbox" || view === "board") && p.for === inbox.sel) renderInbox();
    },

    // The room's chat: the poll's recent tail of its timeline, merged in.
    setChat: (p) => {
      if (!p || !herdNow.room || p.project !== herdNow.room) return;
      let changed;
      if (p.v == null) changed = mergeRecentChat(p);
      else {
        const meta = { project: p.project, absent: p.absent, missing: p.missing, error: p.error };
        const chat = takePart(herdNow.chat, p, () => Object.assign({
          entries: (p.entries || []).slice(), more: !!p.earlier, loading: false, paged: false, v: p.v, rest: p.rest || 0,
        }, meta), () => emit("room-resend", { project: herdNow.room, chat: true }));
        changed = !!chat;
        if (chat) {
          herdNow.chat = Object.assign(chat, meta);
          const body = app.querySelector(".chat .cbody");
          // A whole again (a part was missed) while you read up: what you
          // read stays drawn until all of it has come (`holdWhole').
          if (holdWhole(chat, p, body)) changed = false;
          // As a column's: read at its end, the rest drawn once it has come.
          if (chat.grew) {
            chat.grew = false;
            herdNow.chatEarlierPending = true;
            if (chat.rest > 0 && !chat.loading && body && body.hasAttribute("data-drawn") && !scrolledUp(body)) changed = false;
            chat.loading = false;
          }
          if (!scrolledUp(body)) chat.entries = trimHead(chat.entries);
        }
        partsCame();
      }
      if (roomIsChat() && changed && !herdNow.away.chat) renderChatBody();
      if (changed && roomIsChat()) roomDrawn(null);
    },

    // An older page of the chat timeline (from `load earlier'): prepended.
    earlierChat: (p) => {
      if (!p || !herdNow.room) return;
      prependChat(p);
      if (roomIsChat()) renderChatBody();
    },

    // An image saved for NAME's next prompt: shown above its box, sent
    // with it.  A box that is shut opens.
    attachImage: (p) => {
      const name = p && p.name;
      if (!name || !p.path) return;
      (herdNow.attach[name] = herdNow.attach[name] || []).push({ path: String(p.path), thumb: p.thumb || null });
      const ta = app.querySelector(PROMPT_BOX.split(", ").map((b) => '[data-name="' + cssName(name) + '"] ' + b).join(", "));
      if (!ta) {
        herdNow.composing.add(name);
        if (view === "agents") renderAgents();
        return;
      }
      renderAttach(name);
      ta.focus();
    },

    // The sidecar's import, read every few seconds; null once it ends.
    importProgress: (p) => {
      $("btn-import").disabled = !!p;
      if (!p) {
        if (statusEl.classList.contains("busy")) setStatus("");
        return;
      }
      if (p.migrating) {
        // Once, after an update: the sidecar brings the index's word
        // weights up to date (sparse_migrate); an import waits for it.
        setStatus("Updating the search index" + (p.total ? ` · ${p.done} of ${p.total} chunks` : ""), "busy");
        return;
      }
      const parts = ["Importing"];
      if (p.agent) parts.push(p.agent);
      parts.push(p.files + (p.files === 1 ? " file" : " files"));
      parts.push(p.imported + " new chunks");
      if (p.current) parts.push(shortName(p.current));
      setStatus(parts.join(" · "), "busy");
    },
    showError: (msg) => {
      if (src) {
        src.loading = null;
        renderSourceChrome();
      }
      searching = false;
      setStatus(String(msg || "Something went wrong"), "error");
    },

    // The sessions, or -- no SOURCES -- their count alone (`needSessions'):
    // CHANGED, they changed since the page was sent them (an import).
    setContext: (p) => {
      p = p || {};
      let counts = null;
      if (Array.isArray(p.sources)) {
        const sources = p.sources;
        sources.sort((a, b) => (b.mtime || 0) - (a.mtime || 0));
        const byPath = {};
        counts = {};
        sources.forEach((s) => {
          byPath[s.source_path] = s;
          if (s.project) counts[s.project] = (counts[s.project] || 0) + 1;
        });
        ctx = {
          sources: sources,
          byPath: byPath,
          total: p.total || sources.length,
          currentProject: p.current_project || "",
          projects: Object.keys(counts)
            .map((k) => ({ project: k, leaf: leaf(k), n: counts[k] }))
            .sort((a, b) => b.n - a.n || a.leaf.localeCompare(b.leaf)),
          loaded: true,
          asked: false,
          stale: false,
        };
      } else {
        ctx.total = p.total || 0;
        ctx.currentProject = p.current_project || "";
        if (p.changed && ctx.loaded) ctx.stale = true;
      }
      if (p.project) filters.project = p.project;
      if (counts && filters.project && !counts[filters.project]) filters.project = "";
      if (p.query) query = p.query;
      if (p.view === "sessions") showSessions();
      else if (p.view === "search") showSearch();
      else if (view === "sessions") showSessions();
      // A page on a view of its own stays there: the sessions come after
      // its load has shown where it was (restore), and after it came back.
      else if (view === "boot" || view === "search") showSearch();
      else chrome();
      if (p.query) search(true);
    },

    setResume: (p) => {
      if (!p || !src || p.source_path !== src.path) return;
      src.resume = p.resume || null;
      renderSourceChrome();
    },

    // The Docs tab's list.
    renderDocs: (p) => {
      docs = Array.isArray(p && p.docs) ? p.docs : [];
      renderDocs();
    },

    // Its reports: the newest, or (AFTER, the id it was asked from) the
    // page older than the last shown -- added only while that is still the
    // last, so an answer to a list since replaced is dropped.
    renderReports: (p) => {
      const got = (Array.isArray(p && p.reports) ? p.reports : []).filter((r) =>
        r && IMG_ID.test(String(r.id)) && typeof r.at === "number");
      if (p && p.after) {
        const last = reports && reports.list[reports.list.length - 1];
        if (!last || last.id !== p.after) return;
        reports.asking = false;
        if (p.error) reports.error = String(p.error);
        else {
          const have = new Set(reports.list.map((r) => r.id));
          reports.list = reports.list.concat(got.filter((r) => !have.has(r.id)));
          reports.more = !!p.more;
          reports.error = "";
        }
      } else if (p && p.error) reports = { list: reports ? reports.list : [], more: false, error: String(p.error) };
      else reports = { list: got, more: !!(p && p.more), error: "" };
      renderDocs();
    },

    showView: (v) => {
      if (v === "agents") showAgents();
      else if (v === "docs") showDocs();
      else if (v === "log") showLog();
      else if (v === "memory") showMemory();
      else if (v === "sessions") showSessions();
      else showSearch();
    },

    setMemoryProjects: (p) => {
      mem.user = (p && p.user) || mem.user;
      mem.projects = (p && p.projects) || [];
      if (view === "memory") renderMemPicker();
    },

    renderMemory: (p) => {
      if (view !== "memory") return;
      mem.key = (p && p.key) || mem.key;
      mem.facts = (p && p.facts) || [];
      mem.usesSince = (p && p.usesSince) || null;
      mem.usesDays = (p && p.usesDays) || 30;
      // The facts loaded: drop a stale memory error (e.g. a resolved
      // "bad project key") so the banner does not linger over a live list.
      if (statusEl.classList.contains("error")) setStatus("");
      renderMemPicker();
      // Not while an editor is open, so a live push does not wipe it.
      if (!mem.editing) renderMemoryList();
    },

    renderChecks: (p) => {
      if (view !== "memory") return;
      mem.checks = (p && p.rules) || [];
      if (mem.confirmRm && !mem.checks.some((c) => c.fact === mem.confirmRm)) mem.confirmRm = "";
      renderMemChecks();
      if (!mem.editing) renderMemoryList();   // the facts' check badges
    },

    renderProposals: (p) => {
      if (view !== "memory") return;
      mem.proposals = (p && p.proposals) || [];
      // A rejection box over a proposal that is gone has nothing to attach to.
      if (mem.rejecting && !mem.proposals.some((x) => x.id === mem.rejecting)) mem.rejecting = "";
      renderMemProposals();
    },

    renderHerd: (p) => {
      herdNow.data = p || null;
      trackQueued(p && p.agents);
      if (view !== "agents") return;
      if (busyEditing()) herdNow.pending = true;
      else {
        // A bug in drawing must say so, not leave "Asking Emacs…" up.
        try {
          renderAgents();
        } catch (e) {
          const box = $("herd");
          if (box) box.innerHTML = '<p class="error">Could not draw the herd: ' + esc(String(e)) + "</p>";
        }
      }
    },

    // The same herd as last sent, which Emacs does not send again: drawn
    // again from what the page has, so what it shows by the clock -- an
    // answer of ten minutes ago, how long until a reset -- moves on.
    renderHerdAgain: () => GM.renderHerd(herdNow.data),

    // Emacs would not take a handoff cap as set: why, under the caps, and
    // the cap shown as it is.
    slotsRefused: (p) => {
      if (p && p.field) delete herdNow.slots.pending[p.field];
      herdNow.slots.err = (p && p.why) || "Refused";
      if (view === "agents" && !herdNow.room) renderSlots(true);
    },

    // Where a new agent can go, for the form's project box; the current
    // project goes in it when nothing is typed yet.
    setNewPlaces: (p) => {
      herdNow.places = (p && p.places) || [];
      const list = $("new-places");
      if (list) list.innerHTML = placeOptions();
      const here = herdNow.places.find((x) => x.here);
      const box = app.querySelector(".newform .new-project");
      if (herdNow.newFor === "*" && !herdNow.newProject && here) {
        herdNow.newProject = here.project;
        if (box) box.value = here.project;
        askNewConversations(true);
      } else syncNewForm();
    },

    // A project's past conversations, as asked; and which project a typed
    // directory is in, and the kind it starts by default.
    setNewConversations: (p) => {
      if (!p || !p.project) return;
      const c = { root: p.root || "", kind: p.kind || "", list: p.conversations || [], why: p.why || "" };
      herdNow.convs[p.project] = c;
      herdNow.convsAsked.delete(p.project);
      if (p.project !== newProjectNow()) return;
      // Open room was pressed before this came: now it can open, or say why not.
      if (herdNow.newRoomWant === p.project && herdNow.newFor) {
        herdNow.newRoomWant = null;
        if (c.why || !c.root) setStatus(c.why || "No project there", "error");
        else return openNewRoom(c.root);
      }
      syncNewForm();
    },

    setScreen: (p) => {
      if (!p) return;
      herdNow.screens[p.name] = p.text || "(no screen yet)";
      const pre = app.querySelector('[data-name="' + cssName(p.name) + '"] pre.screen');
      if (!pre) return;
      // A screen in a room follows its bottom, as a terminal does.
      const body = pre.closest(".rbody");
      const atBottom = body && body.scrollHeight - body.scrollTop - body.clientHeight < 40;
      pre.textContent = herdNow.screens[p.name];
      if (atBottom) body.scrollTop = body.scrollHeight;
    },

    // The poll's recent tail for a column: merged into what is held.
    setConversation: (p) => {
      if (!p || !p.name || !herdNow.room) return;
      let changed;
      if (p.v == null) changed = mergeRecentConv(p.name, p);
      else {
        const conv = takePart(herdNow.convs[p.name], p, () => ({
          entries: (p.entries || []).slice(), more: !!p.earlier, loading: false, paged: false,
          none: p.none, error: p.error, cli: p.cli, path: p.path, v: p.v, rest: p.rest || 0,
        }), () => emit("room-resend", { project: herdNow.room, name: p.name }));
        changed = !!conv;
        if (conv) {
          herdNow.convs[p.name] = conv;
          conv.path = p.path || conv.path;
          conv.cli = p.cli || conv.cli;
          const col = roomColumn(p.name), body = col && col.querySelector(".rbody");
          if (holdWhole(conv, p, body)) changed = false;
          // The rest come: prepended, the reader held where they are --
          // a Load earlier pressed meanwhile is answered by it.  Read at
          // its end, what comes above is out of sight: drawn once, with
          // the last of it, not a whole column again for each part.
          if (conv.grew) {
            conv.grew = false;
            herdNow.earlierPending.add(p.name);
            if (conv.rest > 0 && !conv.loading && body && body.hasAttribute("data-drawn") && !scrolledUp(body)) changed = false;
            conv.loading = false;
          }
          if (!scrolledUp(body)) conv.entries = trimHead(conv.entries);
        }
        partsCame();
      }
      if (view !== "agents") return;
      const a = roomAgents().find((x) => x.name === p.name);
      if (!a) return;
      // A body showing a page of the conversation at a hit stays as it is.
      if (changed && !herdNow.away[findKey(a.name)]) renderRoomBody(a);
      if (changed && !roomIsChat()) roomDrawn(a.name);
      // Whether it has a conversation decides its Screen button -- in a
      // column: one that comes as the room turns to its chat has none.
      const col = roomColumn(a.name);
      if (col) {
        col.querySelector(".rtop").innerHTML = roomTop(a, asksFor(a.name, (herdNow.data && herdNow.data.asks) || []));
        fitChips([col]);
      }
    },

    // An older page for a column (from `load earlier'): prepended to it.
    earlierConversation: (p) => {
      if (!p || !p.name || !herdNow.room) return;
      prependConv(p.name, p);
      if (view !== "agents") return;
      const a = roomAgents().find((x) => x.name === p.name);
      if (a) renderRoomBody(a);
    },

    // Where a find's words are said in an agent's whole conversation.
    foundConversation: (p) => {
      if (!p) return;
      const key = Object.keys(herdNow.find).find((k) => herdNow.find[k].seq === p.seq);
      if (!key) return;
      const F = herdNow.find[key];
      F.busy = false;
      const q = String(p.q || "").trim();
      const hits = Array.isArray(p.hits) ? p.hits : [];
      // The hit gone to stays the one gone to when the same words are
      // found again.
      const was = F.res && F.res.q === q && F.cur >= 0 && F.res.hits[F.cur] ? F.res.hits[F.cur].i : null;
      F.res = { q, name: p.name, hits, total: p.total || hits.length, capped: !!p.capped, entries: p.entries || 0,
                error: p.error ? String(p.error) : "" };
      F.cur = was == null ? -1 : hits.findIndex((h) => h.i === was);
      const then = F.then;
      F.then = null;
      // The quick look, as the words are typed: the list, until a hit is gone to.
      const input = findInput(key);
      if (!then && F.open && F.cur < 0 && input && document.activeElement === input) F.list = true;
      if (then) then();
      else renderStripOf(key);
    },

    // A page of an agent's whole conversation, at a find's hit or beside it.
    atConversation: (p) => { if (p) tookAt(p); },

    // Emacs's M-x ghostherd-page-find: the find bar, as / opens it.
    openFind: () => {
      if (view === "agents" && herdNow.room) openFind(defaultFindKey());
    },

    setSubConversation: (p) => {
      const sv = herdNow.subView;
      if (!p || !sv || p.name !== sv.name || p.sub !== sv.sub) return;
      herdNow.subConv = p;
      renderSubView();
    },

    renderLog: (p) => {
      herd.entries = (p && p.entries) || [];
      if (view === "log") renderLog();
    },

    logAdd: (e) => {
      if (!herd.entries || !e) return;
      const prev = herd.entries[herd.entries.length - 1];
      // In time order, as the list and the lanes read them: a line timed
      // before the last (two Emacsen, a clock set back) goes where it falls.
      let at = herd.entries.length;
      while (at > 0 && herd.entries[at - 1].t > e.t) at--;
      herd.entries.splice(at, 0, e);
      if (view !== "log") return;
      // New entries go on top: keep what is being read where it is.
      const before = app.scrollHeight;
      const top = app.scrollTop;
      if (!logAddInPlace(e, prev)) renderLog();
      if (top > 0) app.scrollTop = top + (app.scrollHeight - before);
    },

    logPinned: (p) => {
      if (!herd.entries || !p) return;
      const e = herd.entries.find((x) => x.id === p.id);
      if (e) e.pinned = !!p.pinned;
      if (view === "log") renderLogList();
    },

    logDeleted: (p) => {
      if (!herd.entries || !p) return;
      herd.entries = herd.entries.filter((x) => x.id !== p.id);
      herd.open.delete(p.id);
      // A state line gone changes its lane too.
      if (view === "log") renderLog();
    },

    renderHits: (p) => {
      if (!p || p.seq !== searchSeq) return; // an older query's answer
      searching = false;
      hits = { q: p.q || "", items: p.hits || [] };
      if (view === "search") renderResults();
    },

    searchFailed: (p) => {
      if (!p || p.seq !== searchSeq) return;
      searching = false;
      setStatus("Search failed: " + (p.error || ""), "error");
      if (view === "search") renderResults();
    },

    renderSource: (p) => {
      if (!p || !src || p.source_path !== src.path || view !== "source") return;
      const chunks = p.chunks || [];
      src.total = p.total || 0;
      if (p.mode === "open") {
        if (src.loading !== "open") return;
        src.offset = p.offset || 0;
        src.chunks = chunks;
        src.resume = p.resume || null;
        src.loading = null;
        showSource();
        focusMessage();
        renderSourceChrome();
        return;
      }
      if (p.mode !== src.loading) return;
      src.loading = null;
      if (p.mode === "earlier") {
        const have = new Set(src.chunks.map((c) => c.chunk_index));
        const fresh = chunks.filter((c) => !have.has(c.chunk_index));
        const fromBottom = app.scrollHeight - app.scrollTop;
        src.chunks = fresh.concat(src.chunks);
        src.offset = p.offset || 0;
        renderSourceBody();
        renderSourceChrome();
        app.scrollTop = app.scrollHeight - fromBottom; // stay on what was read
      } else {
        const have = new Set(src.chunks.map((c) => c.chunk_index));
        src.chunks = src.chunks.concat(chunks.filter((c) => !have.has(c.chunk_index)));
        const top = app.scrollTop;
        renderSourceBody();
        renderSourceChrome();
        app.scrollTop = top;
      }
    },
  };

  // The page is ready -- the end of its load, for the kit's timings
  // (XW.ready) -- once the view it opened on has drawn what Emacs sent
  // for it: the Inbox its items, the herd, the log...  A view not named
  // here is, once Emacs has sent anything.  Each of Emacs's calls says
  // so after it has run; the kit hears only the first that ends a load.
  const READY_BY = { inbox: "renderInbox", agents: "renderHerd", log: "renderLog", memory: "renderMemory",
                     sessions: "setContext", search: "setContext", threads: "renderThreads", docs: "renderDocs",
                     board: "boardPatch" };
  Object.keys(window.GM).forEach((fn) => {
    const f = window.GM[fn];
    if (typeof f !== "function") return;
    window.GM[fn] = function () {
      const out = f.apply(this, arguments);
      const want = READY_BY[view];
      if (want === fn) XW.ready(view);
      else if (!want && view !== "boot") XW.ready("drawn");
      return out;
    };
  });

  // Keep the page exactly as tall as the browser is showing.  The dynamic
  // viewport (the phone's URL bar) is handled by CSS `100dvh'; the keyboard
  // is not -- it covers the bottom without shrinking the layout -- so track
  // the visual viewport into --app-h.  The room/chat is then a flex column
  // of that height: the composer (flex-shrink:0) stays above the keyboard
  // and the transcript (flex:1) shrinks, not the other way round.  On a
  // desktop the visual viewport is the window, so this is a no-op.
  //
  // Where the keyboard comes and goes on a phone (TYPES), a conversation
  // at its end stays at its end; what is read up holds still anyway, its
  // top not moving.
  let appH = 0;
  function fitViewport() {
    const vv = window.visualViewport;
    const h = Math.round(vv ? vv.height : window.innerHeight);
    const ends = TYPES && h !== appH ? roomBodies().filter((b) => !scrolledUp(b)) : [];
    document.documentElement.style.setProperty("--app-h", h + "px");
    appH = h;
    ends.forEach((b) => { b.scrollTop = b.scrollHeight; });
    // When the keyboard shifts the page down, pin it back so the top bar
    // and the composer both sit in the visible band.
    if (vv && vv.offsetTop > 0) window.scrollTo(0, 0);
    if (TYPES) keyboardSeen(h, Math.round(vv ? vv.width : window.innerWidth));
  }

  // ---- Typing on a phone ---------------------------------------------------
  //
  // With its keyboard up a phone shows a third of the page, and a room's
  // bars -- the tabs, the project's, the jump strip, the meter, who the
  // chat goes to -- took all but two lines of it.  So while a room's box
  // has the focus there, the page types (body.typing, style.css): the bars
  // fold away, and who the chat's box goes to is a tag at its left, the
  // select over it unseen.  The box's focus says so, not the visual
  // viewport: the keyboard rises over a few frames, and a layout that
  // followed it would change on the way.  A tap that passes through does
  // not end it -- the select, Image (its picker), 💡 -- focus gone
  // elsewhere does, or the keyboard put away with the box still focused
  // (Android's back); a tap on the box types again.
  //
  // What a conversation shows stays where it was on the screen: the bars
  // folding away show what was above it, coming back they cover it; at its
  // end, it stays at its end.  Then held there (TYPE_KEEP) while the
  // keyboard moves -- iOS scrolls for the box as it does -- unless a finger
  // takes the conversation.
  //
  // A desktop never types -- Emacs's own page neither: a finger, on a
  // screen a phone's size whichever way round.
  const TYPES = TOUCH && Math.min(screen.width, screen.height) <= 600;
  const TYPE_ROW = ".room .cfoot, .room .rfoot";
  const TYPE_GRACE = 150, TYPE_KEEP = 900, TYPE_LOOK = 500, KEYBOARD = 150;
  const typeRow = (el) => !!(el && el.closest && el.closest(TYPE_ROW));
  const typeBox = (el) => !!(el && el.matches && el.matches("textarea.agent-text") && typeRow(el));
  let typing = false, typeOff = null, typeLook = null, typeHold = 0, typeKept = null, typeFrame = 0;
  const kbd = { tall: 0, wide: 0, seen: false, gone: null };

  function roomBodies() {
    return [...app.querySelectorAll(".room .chat .cbody, .room .rcol .rbody")];
  }

  function typeSet(on) {
    if (!TYPES || on === typing) return;
    const was = roomBodies().map((b) => ({ b, end: !scrolledUp(b), top: b.getBoundingClientRect().top }));
    typing = on;
    document.body.classList.toggle("typing", on);
    clearInterval(typeLook);
    // Focus taken away with the box -- a room drawn anew -- tells nothing.
    typeLook = on ? setInterval(typeCheck, TYPE_LOOK) : null;
    kbd.seen = on && keyboardUp();
    clearTimeout(kbd.gone);
    kbd.gone = null;
    const keep = was.map((w) => {
      if (w.end) w.b.scrollTop = w.b.scrollHeight;
      else w.b.scrollTop += w.b.getBoundingClientRect().top - w.top;
      return { b: w.b, end: w.end, a: w.end ? null : anchorOf(w.b) };
    });
    typeKept = { keep, until: Date.now() + TYPE_KEEP };
    cancelAnimationFrame(typeFrame);
    typeFrame = requestAnimationFrame(typeKeeping);
    // A column's head was not there to fit its chips to.
    if (!on) fitChips([...app.querySelectorAll(".room .rcol")]);
  }

  function typeKeep() {
    const k = typeKept;
    if (!k) return false;
    if (Date.now() > k.until || handMoving()) { typeKept = null; return false; }
    k.keep.forEach((w) => {
      if (!w.b.isConnected) return;
      if (w.end) w.b.scrollTop = w.b.scrollHeight;
      else if (w.a) placeAnchor(w.b, w.a, drawn.get(w.b) || []);
    });
    return true;
  }
  function typeKeeping() {
    typeFrame = typeKeep() ? requestAnimationFrame(typeKeeping) : 0;
  }

  // Still typing?  The box, or the select beside it, has the focus -- or
  // Image's picker is up (`typeHold').
  function typeCheck() {
    if (!typing || typeHold) return;
    const a = document.activeElement;
    if (typeBox(a) || (typeRow(a) && a.matches("select"))) return;
    typeSet(false);
  }
  function typeSoon() {
    clearTimeout(typeOff);
    typeOff = setTimeout(typeCheck, TYPE_GRACE);
  }
  function typeRelease() {
    if (!typeHold) return;
    typeHold = 0;
    typeSoon();
  }

  // The keyboard: up while the visual viewport is KEYBOARD short of the
  // tallest it has been at this width.
  function keyboardUp() {
    return !!kbd.tall && appH < kbd.tall - KEYBOARD;
  }
  function keyboardSeen(h, w) {
    if (w !== kbd.wide) { kbd.wide = w; kbd.tall = h; } else kbd.tall = Math.max(kbd.tall, h);
    if (!typing) return;
    if (keyboardUp()) {
      kbd.seen = true;
      clearTimeout(kbd.gone);
      kbd.gone = null;
    } else if (kbd.seen && !kbd.gone && typeBox(document.activeElement)) {
      kbd.gone = setTimeout(() => {
        kbd.gone = null;
        if (typing && !keyboardUp()) typeSet(false);
      }, 250);
    }
  }

  if (TYPES) {
    document.addEventListener("focusin", (ev) => {
      if (!typeBox(ev.target)) return;
      clearTimeout(typeOff);
      typeSet(true);
    });
    document.addEventListener("focusout", () => { if (typing) typeSoon(); });
    // The keyboard put away, the box kept: a tap on it brings both back.
    app.addEventListener("click", (ev) => {
      if (!typing && typeBox(ev.target) && document.activeElement === ev.target) typeSet(true);
    });
    // Image and 💡 leave the box its focus, and so the keyboard.
    document.addEventListener("mousedown", (ev) => {
      if (typing && ev.target.closest && typeRow(ev.target) &&
          ev.target.closest('[data-act="a:image"], [data-act="a:use-suggestion"]')) ev.preventDefault();
    }, true);
    // Back from Image's picker, picked or not: any touch on the page.
    addEventListener("touchstart", () => { if (typeHold && Date.now() - typeHold > 400) typeRelease(); },
      { passive: true, capture: true });
  }
  if (window.visualViewport) {
    visualViewport.addEventListener("resize", fitViewport);
    visualViewport.addEventListener("scroll", fitViewport);
  }
  window.addEventListener("resize", fitViewport);
  window.addEventListener("orientationchange", fitViewport);
  fitViewport();

  // Where it was first, so what it shows -- the Inbox, say -- is the
  // first thing asked of Emacs; then the rest (ready), which no longer
  // brings the sessions with it (`needSessions').
  restore();
  emit("ready", { lazy: true });
  // The Threads tab's badge, before the tab is opened; the Board's -- of
  // what the Board would show, in the window this page chose.
  emit("threads-peek");
  emit("board-peek", { days: board.days });
})();
