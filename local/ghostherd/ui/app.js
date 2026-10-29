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
  // Queued and counted by xwapp's page kit (../../xwapp/ui/xwapp.js).

  const emit = XW.bridge(PREFIX);

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
        return sanitize(window.marked.parse(toolMarks(String(src)), { gfm: true, breaks: true }));
      } catch (e) {
        /* fall through to plain text */
      }
    }
    return '<div class="plain">' + esc(src) + "</div>";
  }

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

  // The Docs tab: the documents Emacs lists (renderDocs), newest first;
  // and the reports agents' replies named (renderReports) -- the sidecar's
  // copies, by id, newest first, a page at a time.
  let docs = null; // [{name, title, where, bytes, mtime}] once Emacs has said
  // {list: [{id, title, name, agent, project, at, bytes, gone, older: [...]}], more, error, asking}
  let reports = null;
  // The reports whose older copies are shown, by project and file name:
  // what one is, whichever copy is its newest.
  const reportsOpen = new Set();
  const reportKey = (rep) => String(rep.project || "") + "\0" + String(rep.name || rep.id);

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

  function renderDocs() {
    if (view !== "docs") return;
    const rows = (docs || []).filter((d) => d && DOC_NAME.docs.test(String(d.name))).map((d) =>
      '<div class="doc">' +
      docLink("docs", d.name, esc(d.title || d.name), "dtitle", d.name) +
      '<div class="dmeta"><span class="dname">' + esc(d.name) + "</span>" +
      '<span class="dwhere" title="' + esc(d.where || "") + '">' + esc(d.where || "") + "</span>" +
      "<span>" + esc(sizeText(d.bytes)) + "</span>" +
      '<span title="' + esc(fullTime(d.mtime)) + '">' + esc(when(d.mtime)) + "</span></div></div>").join("");
    app.innerHTML = '<div class="page docs"><div class="dcols"><section class="dsec" id="docs-documents">' +
      '<h2 class="section">Documents</h2>' +
      '<div class="sum">' + (docs ? docs.length + (docs.length === 1 ? " document" : " documents") : "Asking Emacs…") +
      "</div>" + (docs && !docs.length ? '<p class="empty">No HTML document in the folders listed.</p>' : rows) +
      "</section>" + reportsSection() + "</div>" +
      '<p class="docsays muted">' + esc(DOC_SAYS) + " Documents: ghostherd's docs/ and the folders of " +
      "<code>ghostherd-docs-directories</code>. Reports: the HTML files agents' replies named, as the herd " +
      "copied them then, kept as their images are.</p></div>";
  }

  // The reports, as the sidecar keeps their copies: each opens as the Open
  // link under its message does.  One whose copy is gone from the
  // directory is still listed, marked, with nothing to open.  A file an
  // agent rewrote and named again is one row, its newest copy: "2 older"
  // under it shows the others, newest first, and folds them away again.
  function reportsSection() {
    const r = reports;
    const list = r ? r.list : [];
    const one = (rep, older) => {
      const name = String(rep.name || rep.id + ".html");
      const title = String(rep.title || "").trim() || name;
      const who = String(rep.agent || "");
      const them = older ? [] : (Array.isArray(rep.older) ? rep.older : []).filter((o) =>
        o && IMG_ID.test(String(o.id)) && typeof o.at === "number");
      const open = them.length > 0 && reportsOpen.has(reportKey(rep));
      const head = rep.gone
        ? '<span class="dtitle dgone" title="' + esc(name) + '">' + esc(title) + "</span>"
        : docLink("reports", rep.id + ".html", esc(title), "dtitle", name);
      return '<div class="doc report' + (rep.gone ? " gone" : "") + (older ? " older" : "") + '" data-report="' +
        esc(rep.id) + '">' + head +
        '<div class="dmeta">' +
        (rep.gone ? '<span class="dgonesays">Its copy is gone from the herd\'s folder: nothing to open.</span>' : "") +
        '<span class="dwho">' + (who ? esc(who) : '<span class="muted">writer unknown</span>') + "</span>" +
        (rep.project ? '<span class="dwhere" title="' + esc(rep.project) + '">' + esc(rep.project) + "</span>" : "") +
        '<span title="' + esc(fullTime(rep.at)) + '">' + esc(when(rep.at)) + "</span>" +
        "<span>" + esc(sizeText(rep.bytes)) + "</span>" +
        '<span class="dname">' + esc(name) + "</span>" +
        (them.length
          ? '<button type="button" class="linkish dolder" data-act="reports:older" data-report="' + esc(rep.id) +
            '" aria-expanded="' + open + '" title="The copies of this file as it was before it was rewritten">' +
            them.length + " older" + "</button>"
          : "") + "</div>" +
        (open ? '<div class="dolders">' + them.map((o) => one(o, true)).join("") + "</div>" : "") + "</div>";
    };
    const rows = list.map((rep) => one(rep, false)).join("");
    const sum = !r ? "Asking the herd…"
      : list.length + (list.length === 1 ? " report" : " reports") + (r.more ? ", the newest" : "");
    const more = r && r.more && list.length
      ? '<div class="foot"><button type="button" class="ghost small" data-act="reports:more"' + (r.asking ? " disabled" : "") +
        ">" + (r.asking ? "Asking…" : "Older reports") + "</button></div>"
      : "";
    return '<section class="dsec" id="docs-reports"><h2 class="section">Reports</h2>' +
      '<div class="sum">' + esc(sum) + "</div>" +
      (r && r.error ? '<p class="empty derror">The herd could not be asked: ' + esc(r.error) + "</p>" : "") +
      (r && !list.length && !r.error
        ? '<p class="empty">No HTML report yet: one is listed here once an agent\'s reply names an .html file.</p>'
        : rows) + more + "</section>";
  }

  // The image whole, over the page: fitted to it, at its own size at a
  // press, scrolled.  Esc, ✕ or a press beside it puts it away.  In a
  // browser, Open gives it a tab of its own -- a phone zooms it there.
  function openImage(el) {
    const file = el.getAttribute("data-img") || "";
    const url = IMG_ID.test(file.split(".")[0]) ? XW.file(file) : "";
    if (!url) return;
    closeImage();
    const wrap = document.createElement("div");
    wrap.className = "imgview";
    wrap.setAttribute("role", "dialog");
    wrap.setAttribute("aria-label", el.getAttribute("data-name") || "Image");
    wrap.tabIndex = -1;
    wrap.innerHTML = '<div class="imgbar"><span class="imgname"></span>' +
      (XW.browser ? '<a class="imgopen" target="_blank" rel="noopener noreferrer">Open</a>' : "") +
      '<button type="button" class="ghost imgclose" title="Close (Esc)">✕</button></div>' +
      '<div class="imgscroll"><img alt=""></div>';
    wrap.querySelector(".imgname").textContent = el.getAttribute("data-name") || "";
    const img = wrap.querySelector("img");
    img.src = url;
    img.alt = el.getAttribute("data-name") || "";
    const open = wrap.querySelector(".imgopen");
    if (open) open.href = url;
    wrap.addEventListener("click", (ev) => {
      if (ev.target === img) wrap.classList.toggle("actual");
      else if (ev.target.closest(".imgclose") || ev.target === wrap || ev.target.classList.contains("imgscroll")) closeImage();
    });
    // Over the page: its keys (j, k, 1-4 ...) are not the page's meanwhile.
    wrap.addEventListener("keydown", (ev) => {
      ev.stopPropagation();
      if (ev.key === "Escape") { closeImage(); ev.preventDefault(); }
    });
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

  let ctx = { sources: [], byPath: {}, total: 0, currentProject: "", projects: [] };
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
    if (inboxWatching && view !== "inbox") {
      inboxWatching = false;
      emit("inbox-unwatch");
    }
    if (threadsWatching && view !== "threads") {
      threadsWatching = false;
      threads.req = null;
      threads.cur = null;
      emit("threads-unwatch");
    }
    inboxBadge();
    threadsBadge();
    document.querySelectorAll(".tab").forEach((b) => {
      b.classList.toggle("on", view !== "source" && b.getAttribute("data-tab") === view);
    });
    tabInView();
    btnBack.hidden = !(view === "source" || (view === "agents" && herdNow.room));
    $("sessions-n").textContent = ctx.sources.length ? String(ctx.sources.length) : "";
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
    else if (where === "docs") showDocs();
    // Home: the page opens on the Inbox when nothing else is asked for.
    else showInbox();
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
    // A column (by name) or the chat whose `load earlier' is in flight: when
    // its older page arrives, the view holds the message you were reading
    // instead of jumping, the prepended turns pushing it down by their height.
    earlierPending: new Set(), chatEarlierPending: false,
    // Jump to what you asked: which of your own messages is the one the
    // sticky line points at (by its ts, or null = the latest), per view; a
    // pending flag while a `load earlier' fired to reach an older one; and a
    // transient note ("older than what is loaded") on a view's jump line.
    jump: { chat: null, col: {} }, jumpPend: null, jumpNote: {},
    // Checkouts whose every task is shown, not two.
    workOpen: new Set(),
  };

  const IMAGE_BUTTON = '<button type="button" class="ghost small" data-act="a:image" ' +
    (XW.elsewhere ? 'title="A photo, or one from the camera, in the prompt as a file">'
      : 'title="Put the clipboard\'s image in the prompt, as a file (⌘V does too)">') + "Image</button>";

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
    app.innerHTML = '<div class="page wide"><div id="usage" class="usage"></div><div id="primarybar"></div><div id="herd"></div></div>';
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
  // project it went to, or that project's lead while the herd starts it.
  function takerOf(k) {
    if (!k.project) return k.to;
    return k.to ? k.to + " (" + leaf(k.project) + ")" : leaf(k.project) + "'s lead, starting";
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
      (!k.to ? " · starting its lead" : k.status === "queued" ? " · queued" : "");
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
        " to " + (k.to || "the lead being started") + " in " + k.project + ": " + (k.head || "")
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

  // A message the herd pasted that its CLI never took: Return was pressed
  // again and it is still in the input box.  The card says so, with the one
  // key that sends it -- pressed only while it is verifiably still there.
  function unsentHtml(a) {
    if (!a.unsent) return "";
    return '<div class="contline"><span class="cont stopped" title="' + esc(a.unsent) + '">' +
      "message not submitted — it sits in its input box</span>" +
      '<button type="button" class="ghost small" data-act="a:agent-submit" title="Press Return for it">Press Return</button></div>';
  }

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
    // Its CLI's usage used up: its relay first.
    if (a.usedUp && a.relayTo && a.relayTo !== a.kind) {
      acts.push('<button type="button" class="primary small" data-act="a:relay" title="' + esc(a.usedUp) + '">Relay to ' + esc(a.relayTo) + "</button>");
    }
    acts.push('<button type="button" class="ghost small" data-act="a:compose">Prompt</button>');
    acts.push('<button type="button" class="ghost small" data-act="a:agent-interrupt" title="Send Escape">Esc</button>');
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
      (a.progress != null ? '<span class="det">' + esc(a.progress) + "%</span>" : "") + backgroundHtml(a) +
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
    app.innerHTML = "";
    chrome();
    watchRoom(true); // a (re)open: Emacs re-sends the conversation
    renderRoom();
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

  // What Emacs should send for the room: each column's conversation and
  // the screens shown instead, or the chat.  The poll reads only the recent
  // tail (default limit); `load earlier' pages are fetched on their own.
  // REOPEN is true when the page is opening (or reopening after a reload) the
  // room, false for a screen or view toggle of a room already open: on a
  // reopen Emacs re-sends the conversation, since a reloaded page lost it while
  // Emacs kept the room; a screen toggle must not re-read the whole thing.
  function watchRoom(reopen) {
    emit("room-watch", {
      project: herdNow.room, screens: [...herdNow.roomScreen],
      // The primary room is always a chat (it has no columns view), so ask
      // Emacs for the chat whatever roomView's default is on this device --
      // otherwise the desktop default (columns) leaves it "Reading…" forever.
      view: roomIsChat() ? "chat" : herdNow.roomView,
      reopen: !!reopen,
    });
    herdNow.watchedChat = roomIsChat();
  }

  function closeRoom() {
    if (!herdNow.room) return;
    herdNow.room = null;
    if (herdNow.subView) { herdNow.subView = null; herdNow.subConv = null; emit("sub-unwatch", {}); }
    emit("room-close");
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
    // /login and its link are kept; Esc and Screen are `sec' (secondary).
    if (a.login) {
      acts.push('<button type="button" class="primary small" data-act="a:agent-login" title="Run /login to sign in again">/login</button>');
    }
    if (a.url) {
      acts.push('<button type="button" class="primary small" data-act="a:login-open" data-url="' + esc(a.url) + '" title="Open the sign-in link">Open sign-in link</button>');
      acts.push('<button type="button" class="ghost small" data-act="a:login-copy" data-url="' + esc(a.url) + '">Copy link</button>');
    }
    acts.push('<button type="button" class="ghost small sec" data-act="a:agent-interrupt" title="Send Escape">Esc</button>');
    const conv = herdNow.convs[name];
    if (!(conv && conv.none)) {
      acts.push('<button type="button" class="ghost small sec" data-act="a:room-screen">' +
        (herdNow.roomScreen.has(name) ? "Conversation" : "Screen") + "</button>");
    }
    const expanded = herdNow.cardOpen.has(name);
    const cx = '<button type="button" class="cx" data-act="a:card-expand" title="' +
      (expanded ? "Less" : "More: notes, chips, branch, Esc / Screen") + '">' + (expanded ? "▴" : "▾") + "</button>";
    return '<div class="amain">' + stateBadge(a.state) +
      '<span class="rdot" style="color:' + sessionColor(name) + '" title="Its colour in every column">●</span>' +
      agentName(name) + agentBadge(a.kind) + leadBadge(a) + backgroundHtml(a) +
      (a.startedBy ? '<span class="det startedby" title="Started by ' + esc(a.startedBy) + ' through herd start">by ' + esc(a.startedBy) + "</span>" : "") +
      (a.notes ? '<span class="anotes" title="' + esc(a.notes) + '">' + esc(a.notes) + "</span>" : "") +
      '<span class="when" title="' + esc(a.since ? new Date(a.since).toLocaleString() : "") + '">' +
      esc(a.since ? when(a.since / 1000) : "") + "</span>" + cx + "</div>" +
      meterHtml(a) + modelFormHtml(a) + reasonHtml(a) + continueHtml(a) + unsentHtml(a) + goneHtml(a) +
      (asks && asks.length ? '<div class="aasks">' + asks.map((k) => askLine(k, name)).join("") + "</div>" : "") +
      guardHtml(name) +
      (a.work ? workHtml(a.work) : "") + linksHtml(a) + menu +
      '<div class="aacts">' + acts.join("") + "</div>";
  }

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
  // an entry's bar is whoever said it.  You are the blue.
  function entryHtml(e, i, name, kind, keyns) {
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
    return '<div class="ent you" data-i="' + i + '">' + entHead("You", "", e.ts, "", copyButton(i, e.text)) + herdNotes(e) + plainBody(e.text) + "</div>";
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
    if (!conv) return '<p class="rnote">Reading its conversation…</p>';
    if (conv.error) return '<p class="rnote error">Could not read its conversation: ' + esc(conv.error) + "</p>";
    const entries = conv.entries || [];
    return earlierHtml(conv, "a:load-earlier") +
      (entries.length ? entries.map((e, i) => entryHtml(e, i, name, a.kind)).join("") : '<p class="rnote">Nothing said yet.</p>');
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
    if (scrolledUp(roomColumn(name) && roomColumn(name).querySelector(".rbody"))) return false;
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
      conv.entries = trimTail(prependPage(page, conv.entries || []));
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
    if (scrolledUp(app.querySelector(".chat .cbody"))) return false;
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
      chat.entries = trimTail(prependPage(page, chat.entries || []));
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
    const first = !body.hasAttribute("data-drawn");
    const prevLen = (drawn.get(body) || []).length;
    const entries = (herdNow.convs[a.name] && herdNow.convs[a.name].entries) || [];
    const grewEarlier = herdNow.earlierPending.has(a.name) && entries.length > prevLen;
    const oldHeight = body.scrollHeight, keep = body.scrollTop;
    const atBottom = oldHeight - keep - body.clientHeight < 40;
    body.innerHTML = roomBodyHtml(a);
    drawn.set(body, entries);
    body.setAttribute("data-drawn", "1");
    if (first || atBottom) body.scrollTop = body.scrollHeight;
    else if (grewEarlier) {
      body.scrollTop = keep + (body.scrollHeight - oldHeight);
      herdNow.earlierPending.delete(a.name);
    } else body.scrollTop = keep;
    renderColJump(a.name);
    const conv = herdNow.convs[a.name];
    if (herdNow.jumpPend === "col:" + a.name && conv && !conv.loading) {
      herdNow.jumpPend = null;
      const list = youItems(entries), k = youAt(list, herdNow.jump.col[a.name]);
      if (k < list.length - 1) {
        herdNow.jump.col[a.name] = list[k + 1].ts; renderColJump(a.name);
        flashMsg(body, '.ent.you[data-i="' + list[k + 1].i + '"]');
      } else noteJump("col:" + a.name, "Older questions are in the transcript, past what is loaded here.", () => renderColJump(a.name));
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
      '<select class="chat-to" title="Who it goes to (or start with @name, @lead)">' +
      chatTargets(agents).map(([v, label]) => '<option value="' + esc(v) + '"' + (v === to ? " selected" : "") + ">" + esc(label) + "</option>").join("") +
      '</select><textarea class="agent-text" rows="2" placeholder="' + esc(chatHint(to)) + '">' +
      esc(herdNow.compose[to] || "") + "</textarea>" + IMAGE_BUTTON + useButton(to) +
      '<button type="button" class="primary small" data-act="a:send">Send</button></div></div>';
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
    const c = herdNow.chat;
    const first = !body.hasAttribute("data-drawn");
    const prevLen = (drawn.get(body) || []).length;
    const entries = (c && c.entries) || [];
    const grewEarlier = herdNow.chatEarlierPending && entries.length > prevLen;
    const oldHeight = body.scrollHeight, keep = body.scrollTop;
    const atBottom = oldHeight - keep - body.clientHeight < 40;
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
        (out.length ? '<p class="rnote">Not in the chat, no conversation known: ' + esc(out.join(", ")) + "</p>" : "");
    drawn.set(body, entries);
    body.setAttribute("data-drawn", "1");
    if (first || atBottom) body.scrollTop = body.scrollHeight;
    else if (grewEarlier) {
      body.scrollTop = keep + (body.scrollHeight - oldHeight);
      herdNow.chatEarlierPending = false;
    } else body.scrollTop = keep;
    renderChatJump();
    // A `load earlier' fired to reach an older "You" has arrived: step to it.
    if (herdNow.jumpPend === "chat" && c && !c.loading) {
      herdNow.jumpPend = null;
      const list = youItems(entries), k = youAt(list, herdNow.jump.chat);
      if (k < list.length - 1) {
        herdNow.jump.chat = list[k + 1].ts;
        renderChatJump();
        flashMsg(body, '.cmsg[data-i="' + list[k + 1].i + '"]');
      } else noteJump("chat", "Older questions are in the transcript, past what is loaded here.", renderChatJump);
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
      if (e && (e.who === "user" || e.role === "user"))
        out.push({ ts: e.ts, i, text: (e.text || "").replace(/\s+/g, " ").trim() });
    }
    return out; // newest first
  }

  function youAt(list, ts) {
    if (!ts) return 0;
    const k = list.findIndex((x) => x.ts === ts);
    return k < 0 ? 0 : k;
  }

  function jumpBarHtml(list, curTs, conv, note) {
    if (note) return '<div class="jump"><span class="jnote">' + esc(note) + "</span></div>";
    if (!list.length) return "";
    const k = youAt(list, curTs), cur = list[k];
    const older = k < list.length - 1 || !!(conv && conv.more);
    return '<div class="jump">' +
      '<button type="button" class="jline" data-act="a:jump-go" title="Jump to this message">' +
      '<span class="jwho">You:</span> <span class="jtext">' + esc(cur.text || "(no text)") + "</span></button>" +
      (list.length > 1 ? '<span class="jpos">' + (list.length - k) + "/" + list.length + "</span>" : "") +
      '<button type="button" class="jstep" data-act="a:jump-prev"' + (older ? "" : " disabled") + ' title="Earlier question">↑</button>' +
      '<button type="button" class="jstep" data-act="a:jump-next"' + (k > 0 ? "" : " disabled") + ' title="Later question">↓</button>' +
      "</div>";
  }

  function renderChatJump() {
    const bar = app.querySelector(".chat .cjump");
    if (!bar) return;
    bar.innerHTML = jumpBarHtml(youItems((herdNow.chat && herdNow.chat.entries) || []),
      herdNow.jump.chat, herdNow.chat, herdNow.jumpNote.chat);
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
    const conv = herdNow.convs[name];
    bar.innerHTML = jumpBarHtml(youItems((conv && conv.entries) || []),
      herdNow.jump.col[name], conv, herdNow.jumpNote["col:" + name]);
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
        emit("room-earlier", { project: herdNow.room, name: name, before: entries.length });
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
        emit("chat-earlier", { project: herdNow.room, before: entries.length });
        renderChatBody();
      } else noteJump("chat", "Older questions are in the transcript, past what is loaded here.", renderChatJump);
    } else if (k > 0) {          // ↓ later
      herdNow.jump.chat = list[k - 1].ts; renderChatJump();
      flashMsg(body, '.cmsg[data-i="' + list[k - 1].i + '"]');
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
      const views = [["columns", "Columns"], ["chat", "Chat"]].map(([v, label]) =>
        '<button type="button" class="ghost small' + (herdNow.roomView === v ? " on" : "") + '" data-act="a:room-view" data-view="' + v + '">' +
        label + "</button>").join("");
      app.innerHTML = '<div class="room' + (primary ? " proom" : "") + '"><div class="rhead"><span class="pname" title="' + esc(project) + '">' +
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
    }
    room.querySelector(".rsum").innerHTML = summary;
    const rwork = room.querySelector(".rwork");
    if (rwork) rwork.innerHTML = workHtml(workFor(project));
    room.querySelector(".rruns").innerHTML = runsHtml(agents, asks);
    renderSubView();
    if (chat) {
      const foot = room.querySelector(".chat .cfoot");
      if (foot) suggestIn(foot, foot.getAttribute("data-name"), chatHint(foot.getAttribute("data-name")));
      renderChatMeter();
      return;
    }
    agents.forEach((a) => {
      const col = roomColumn(a.name);
      col.className = "rcol s-" + a.state + (herdNow.cardOpen.has(a.name) ? " expanded" : "");
      col.querySelector(".rtop").innerHTML = roomTop(a, asksFor(a.name, asks));
      const body = col.querySelector(".rbody");
      if ((body.getAttribute("data-subs") || "") !== subsKey(a)) {
        body.setAttribute("data-subs", subsKey(a));
        if (body.hasAttribute("data-drawn")) renderRoomBody(a);
      }
      stripUnder(body, col.querySelector(".rsubs"), subsHtml(a));
      suggestIn(col, a.name, promptHint(a.name));
      renderColJump(a.name);
    });
  }

  function onAgentAction(act, el) {
    if (act === "tally") {
      const key = el.getAttribute("data-tally") || "";
      if (herdNow.tallyOpen.has(key)) herdNow.tallyOpen.delete(key);
      else herdNow.tallyOpen.add(key);
      return GM.renderHerdAgain();
    }
    const row = el.closest(".arow, .rcol, .cfoot, .cmeter");
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
      watchRoom();
      return renderRoom();
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
    if (act === "card-expand") {
      if (herdNow.cardOpen.has(name)) herdNow.cardOpen.delete(name);
      else herdNow.cardOpen.add(name);
      return renderRoom();
    }
    if (act === "sub-more") {
      const key = el.getAttribute("data-key");
      if (herdNow.subOpen.has(key)) herdNow.subOpen.delete(key);
      else herdNow.subOpen.add(key);
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
      emit("room-earlier", { project: herdNow.room, name: name, before: (conv.entries || []).length });
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
      emit("chat-earlier", { project: herdNow.room, before: (c.entries || []).length });
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
    if (act === "agent-visit" || act === "agent-interrupt" || act === "agent-abort" || act === "agent-login" || act === "agent-sleep-cancel" || act === "agent-submit") return emit(act, { name });
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
      const ta = row && row.querySelector("textarea.agent-text");
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
    const files = (herdNow.attach[name] || []).map((a) => a.path);
    if (!text && !files.length) return;
    // The images go after what you wrote, a file name a line.
    const body = [text, files.join("\n")].filter(Boolean).join("\n\n");
    if (to === "@lead") emit("lead-say", { project: herdNow.room, text: body });
    else if (to === "@primary") emit("primary-say", { text: body });
    else emit("agent-prompt", { name: to, text: body });
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
    const name = ta.closest(".arow, .rcol, .cfoot").getAttribute("data-name");
    if (!XW.elsewhere) return emit("agent-image", { name: name });
    pickFor = name;
    picker.value = "";
    picker.click();
  }

  const picker = $("image-pick");
  let pickFor = null;
  picker.addEventListener("change", () => {
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
  // In Emacs, Cmd-V never becomes a paste: the paste is the menu's to
  // send, and Emacs's menu does not send it to the web view, so the page
  // sees the keydown and nothing after -- not for text either.  Emacs
  // reads the clipboard instead, and answers with `paste' below.  In a
  // web browser Cmd-V is a paste, and an image in it is asked for below.
  document.addEventListener("keydown", (ev) => {
    if (XW.browser) return;
    if (!ev.metaKey || ev.ctrlKey || ev.altKey || ev.key.toLowerCase() !== "v") return;
    const el = document.activeElement;
    if (!el || (el.tagName !== "TEXTAREA" && el.tagName !== "INPUT")) return;
    ev.preventDefault();
    emit("paste");
  }, true);

  // Inbox desk keys: j/k move, 1-4 answer a menu, a approve, e done.
  document.addEventListener("keydown", (ev) => {
    if (view !== "inbox" || ev.metaKey || ev.ctrlKey || ev.altKey) return;
    inboxKey(ev);
  });
  // Keep the reply text across the Inbox's periodic re-renders.
  app.addEventListener("input", (ev) => {
    if (ev.target && ev.target.classList && ev.target.classList.contains("ibreplybox")) inbox.reply = ev.target.value;
  });

  // A paste with no text in it holds the clipboard's image, or nothing a
  // textarea would take anyway; Emacs says which.
  app.addEventListener("paste", (ev) => {
    const t = ev.target;
    if (!t.classList || !t.classList.contains("agent-text")) return;
    if (Array.from((ev.clipboardData && ev.clipboardData.types) || []).includes("text/plain")) return;
    ev.preventDefault();
    askImage(t);
  });

  app.addEventListener("change", (ev) => {
    if (ev.target.classList && ev.target.classList.contains("chat-to")) setChatTo(ev.target.value);
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
  // delete or a reload does not shift which screen is open.
  const LOGCAP = 300;
  let herd = { entries: null, range: 24 * HOUR, session: "", blocked: false,
               screensOnly: false, filter: "", open: new Set(), shown: LOGCAP };

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

  function logWindow() {
    const now = Date.now();
    const entries = herd.entries || [];
    const first = entries.length ? entries[0].t : now - HOUR;
    return { start: herd.range ? now - herd.range : first, end: now };
  }

  // Segments per session from the state transitions; the state a range
  // opens in is whatever the last transition before it left.
  function lanes(start, end) {
    const by = new Map();
    (herd.entries || []).forEach((e, i) => {
      if (e.session === "herd") return; // the seam between Emacsen
      if (!by.has(e.session)) by.set(e.session, []);
      by.get(e.session).push(Object.assign({ i: i }, e));
    });
    const out = [];
    by.forEach((list, name) => {
      if (herd.session && name !== herd.session) return;
      let state = null, since = start, reason = "", from = -1;
      const segs = [];
      const marks = [];
      for (const e of list) {
        if (e.t > end) break;
        if (e.kind === "state" && e.new) {
          if (e.t >= start && state) segs.push({ state, a: since, b: e.t, reason, i: from });
          state = e.new;
          since = Math.max(e.t, start);
          reason = e.reason || "";
          from = e.i;
        } else if (e.kind === "life") {
          if (e.t >= start) marks.push({ t: e.t, text: e.text, i: e.i });
          if (/^(killed|released)/.test(e.text)) {
            if (state && e.t >= start) segs.push({ state, a: since, b: e.t, reason, i: from });
            state = null;
          }
        }
      }
      if (state) segs.push({ state, a: since, b: end, reason, i: from, open: true });
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
        return '<span class="seg st-' + esc(s.state) + '" data-act="seg" data-i="' + s.i + '" style="left:' +
          pct(s.a).toFixed(3) + "%;width:" + Math.max(0, pct(s.b) - pct(s.a)).toFixed(3) + '%" title="' + esc(tip) + '"></span>';
      })
      .join("");
    const marks = l.marks
      .map((m) => '<span class="life" data-act="seg" data-i="' + m.i + '" style="left:' + pct(m.t).toFixed(3) +
        '%" title="' + esc(clock(m.t) + " · " + m.text) + '"></span>')
      .join("");
    const blocked = l.segs.filter((s) => s.state === "blocked");
    const note = blocked.length
      ? '<span class="lane-note">' + blocked.length + "× blocked · " + span(blocked.reduce((n, s) => n + s.b - s.a, 0)) + "</span>"
      : "";
    return '<div class="lane"><button type="button" class="lane-name" data-act="log-session" data-session="' +
      esc(l.name) + '" title="Show only ' + esc(l.name) + '"><span class="dot" style="background:' + sessionColor(l.name) +
      '"></span>' + esc(l.name) + "</button>" + '<div class="lane-bar">' + segs + marks + "</div>" + note + "</div>";
  }

  function axisHtml(start, end) {
    const ticks = [0, 0.25, 0.5, 0.75, 1].map((f) => {
      const t = start + f * (end - start);
      const label = f === 1 ? "now" : end - start > 2 * 24 * HOUR ? dayLabel(t) : clock(t).slice(0, 5);
      return '<span style="left:' + f * 100 + '%">' + esc(label) + "</span>";
    });
    return '<div class="lane axis"><span class="lane-name"></span><div class="lane-bar">' + ticks.join("") + "</div><span class=\"lane-note\"></span></div>";
  }

  function showLog() {
    view = tab = "log";
    src = null;
    chrome();
    const counts = {};
    (herd.entries || []).forEach((e) => { if (e.session !== "herd") counts[e.session] = (counts[e.session] || 0) + 1; });
    const opts = ['<option value="">All agents</option>']
      .concat(Object.keys(counts).sort().map((s) =>
        '<option value="' + esc(s) + '"' + (herd.session === s ? " selected" : "") + ">" + esc(s) + " (" + counts[s] + ")</option>"))
      .join("");
    app.innerHTML =
      '<div class="page wide">' +
      '<div class="filters log-tools"><div class="chips">' +
      RANGES.map(([ms, label]) => '<button type="button" class="chip' + (herd.range === ms ? " on" : "") +
        '" data-act="range" data-range="' + ms + '">' + label + "</button>").join("") +
      '<button type="button" class="chip' + (herd.blocked ? " on" : "") + '" data-act="blocked-only">Blocked only</button>' +
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
    const { start, end } = logWindow();
    const ls = lanes(start, end);
    box.innerHTML = ls.length
      ? axisHtml(start, end) + ls.map((l) => laneHtml(l, start, end)).join("")
      : '<p class="empty">No agent did anything in this range.</p>';
  }

  function stateBadge(s) {
    return '<span class="st st-' + esc(s) + '">' + esc(s) + "</span>";
  }

  function entryRow(e, i, re) {
    let body;
    if (e.kind === "state" && e.new) {
      body = stateBadge(e.old) + '<span class="arrow">→</span>' + stateBadge(e.new) +
        (e.reason ? '<span class="reason">' + hl(e.reason, re) + "</span>" : "");
    } else {
      body = '<span class="k-' + esc(e.kind) + '">' + hl(e.text, re) + "</span>";
    }
    const open = herd.open.has(e.id);
    const why = e.why ? '<span class="why why-' + esc(e.why) + '" title="why this screen was kept">' + esc(e.why) + "</span>" : "";
    const pin = e.pinned ? '<span class="pinmark" title="Pinned: kept until you remove it">📌</span>' : "";
    const note = e.note ? '<span class="lnote">' + hl(e.note, re) + "</span>" : "";
    const screen = e.screen
      ? '<button type="button" class="linkish" data-act="screen" data-id="' + e.id + '">' +
        (open ? "Hide screen" : "Screen") + "</button>"
      : "";
    // Pin/unpin and delete sit on a captured screen and on any pinned line.
    const ops = (e.screen || e.pinned)
      ? '<button type="button" class="linkish" data-act="' + (e.pinned ? "log-unpin" : "log-pin") +
        '" data-id="' + e.id + '">' + (e.pinned ? "Unpin" : "Pin") + "</button>" +
        '<button type="button" class="linkish danger" data-act="log-delete" data-id="' + e.id + '" title="Delete this entry">Delete</button>'
      : "";
    return (
      '<div class="lrow' + (e.pinned ? " pinned" : "") + '" data-i="' + i + '" data-id="' + e.id + '">' +
      '<span class="lt" title="' + esc(new Date(e.t).toLocaleString()) + '">' + esc(clock(e.t)) + "</span>" +
      '<button type="button" class="ls" data-act="log-session" data-session="' + esc(e.session) + '">' +
      '<span class="dot" style="background:' + sessionColor(e.session) + '"></span>' + esc(e.session) + "</button>" +
      '<span class="lx">' + pin + why + body + note + "</span>" + screen + ops + "</div>" +
      (e.screen && open ? '<pre class="screen">' + esc(e.screen) + "</pre>" : "")
    );
  }

  function renderLogList() {
    const box = $("log-list");
    if (!box) return;
    const { start } = logWindow();
    const re = termRe(herd.filter);
    const f = herd.filter.trim().toLowerCase();
    const rows = [];
    const entries = herd.entries || [];
    for (let i = entries.length - 1; i >= 0; i--) {
      const e = entries[i];
      if (e.t < start) break;
      if (herd.screensOnly && !e.screen) continue;
      if (herd.session && e.session !== herd.session) continue;
      if (herd.blocked && !(e.kind === "state" && e.new === "blocked")) continue;
      if (f && !(e.text + " " + e.session + " " + (e.why || "") + " " +
                 (e.note || "") + " " + (e.screen || "")).toLowerCase().includes(f)) continue;
      rows.push([e, i]);
    }
    // Draw the most recent `shown' at once; the rest wait behind a button.
    const shown = rows.slice(0, herd.shown);
    const more = rows.length - shown.length;
    let day = "";
    box.innerHTML = rows.length
      ? '<div class="sum">' + rows.length + (rows.length === 1 ? " entry" : " entries") +
          (more > 0 ? ", showing " + shown.length : "") + "</div>" +
        shown.map(([e, i]) => {
          const d = dayLabel(e.t);
          const head = d !== day ? '<h2 class="section">' + esc(d) + "</h2>" : "";
          day = d;
          return head + entryRow(e, i, re);
        }).join("") +
        (more > 0 ? '<button type="button" class="ghost more-log" data-act="log-more">Show earlier (' +
          more + " more)</button>" : "")
      : '<p class="empty">' + (entries.length ? "Nothing matches in this range." : "Nothing logged yet.") + "</p>";
  }

  function goToEntry(i) {
    if (i < 0 || !herd.entries || !herd.entries[i]) return;
    let row = app.querySelector('.lrow[data-i="' + i + '"]');
    if (!row) {
      herd.blocked = false;
      herd.filter = "";
      if (herd.range && herd.entries[i].t < Date.now() - herd.range) herd.range = 0;
      renderLog();
      row = app.querySelector('.lrow[data-i="' + i + '"]');
    }
    if (!row) return;
    row.scrollIntoView({ block: "center" });
    row.classList.remove("flash");
    void row.offsetWidth;
    row.classList.add("flash");
  }

  // "now" moves: redraw the open segments now and then.
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
    } else if (act === "resume") {
      resume(false);
    } else if (act === "fork") {
      resume(true);
    } else if (act === "visit-agent") {
      if (src) emit("visit-agent", { source_path: src.path });
    } else if (act === "range") {
      herd.range = +el.getAttribute("data-range");
      renderLog();
    } else if (act === "blocked-only") {
      herd.blocked = !herd.blocked;
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
      goToEntry(+el.getAttribute("data-i"));
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
      const which = b.getAttribute("data-tab");
      if (which === "inbox") showInbox();
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
    else emit("refresh");
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
    if (ev.key === "/" && view !== "source") {
      const input = $("q") || $("sess-filter") || $("log-filter");
      if (input) {
        input.focus();
        ev.preventDefault();
      }
    } else if (ev.key === "Escape" && (view === "source" || (view === "agents" && herdNow.room))) {
      back();
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

  let inbox = { items: [], count: 0, sel: "", filter: "all", toast: "", reply: "", undo: null, detail: null, thOpen: {} };
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

  function inboxAge(at) {
    const t = stamp(at);
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
    if (f === "stuck") return it.kind === "blocked" || it.kind === "login" || it.kind === "ask-failed" || it.kind === "unsent";
    if (f === "approve") return it.kind === "guard" || it.kind === "proposal";
    if (f === "questions") return it.kind === "question";
    if (f === "updates") return !inboxNeed(it);
    return true;
  }
  function inboxCur() { return inbox.items.find((x) => x.id === inbox.sel) || null; }

  function showInbox() {
    if (herdNow.room) closeRoomState();
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
    herdNow.room = null;
    if (herdNow.subView) { herdNow.subView = null; herdNow.subConv = null; emit("sub-unwatch"); }
    emit("room-close");
  }

  let inboxShown = null; // the item the detail pane shows, to start a new one at its top

  function renderInbox() {
    if (view !== "inbox") return;
    const box = app.querySelector(".inboxpage");
    if (!box) return;
    const narrow = inboxNarrow();
    box.classList.toggle("narrow", narrow);
    const fresh = inbox.sel !== inboxShown;
    inboxShown = inbox.sel;
    keepScroll(box, [".iblistcol", ".ibdetail"], () => {
      box.innerHTML = narrow ? inboxPhoneHtml() : inboxDeskHtml();
    }, fresh ? [".ibdetail"] : []);
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
        : '<p class="ibempty">Nothing needs you here. The herd is moving on its own.</p>') +
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
    if (it.kind === "question") {
      return '<div class="ibreply">' +
        '<textarea class="ibreplybox" data-id="' + d + '" rows="3" placeholder="Write the answer it is waiting for">' + esc(inbox.reply) + "</textarea>" +
        '<div class="ibacts"><button type="button" class="ibbtn send" data-act="ib:reply" data-id="' + d + '">Send</button>' +
        '<button type="button" class="ibbtn" data-act="ib:done" data-id="' + d + '">Answer later</button></div></div>';
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

  function inboxPhoneHtml() {
    const live = inbox.items.filter(inboxPass);
    const need = live.filter(inboxNeed);
    const fyi = live.filter((it) => !inboxNeed(it));
    const filters = INBOX_FILTERS.map(([f, label]) =>
      '<button type="button" class="ibfilter' + (inbox.filter === f ? " on" : "") + '" data-act="ib:filter" data-f="' + f + '">' + label + "</button>").join("");
    const who = (it) => '<span class="ibcardwho">' + esc(it.agent) + (it.cli ? " · " + esc(it.cli) : "") +
      (it.project ? " · " + esc(leaf(it.project)) : "") + "</span>";
    const tone = (it) => "t-" + esc(it.tone) + (inboxNeed(it) ? " need" : "");
    // Collapsed: the whole card is one tap target that opens it.  Open: a
    // container (it holds buttons, so not a button itself) whose head taps to
    // collapse, and the full desk detail below -- one open at a time.
    const card = (it) => it.id === inbox.sel
      ? '<div class="ibcard open ' + tone(it) + '">' +
        '<button type="button" class="ibcardhead" data-act="ib:pick" data-id="' + esc(it.id) + '" aria-expanded="true">' +
        inboxTag(it) + '<span class="ibtitle">' + ibPlain(it.title) + "</span>" +
        '<span class="ibage">' + esc(inboxAge(it.at)) + "</span></button>" + who(it) +
        '<div class="ibcardbody">' + inboxDetail(it, true) + "</div></div>"
      : '<button type="button" class="ibcard ' + tone(it) + '" data-act="ib:pick" data-id="' + esc(it.id) + '" aria-expanded="false">' +
        '<span class="ibcardhead">' + inboxTag(it) + '<span class="ibtitle">' + ibPlain(it.title) + "</span>" +
        '<span class="ibage">' + esc(inboxAge(it.at)) + "</span></span>" + who(it) +
        (it.preview ? '<span class="ibcardprev">' + ibPlain(it.preview) + "</span>" : "") + "</button>";
    return (inbox.toast ? '<div class="ibtoast" role="status">' + esc(inbox.toast) +
            (inbox.undo ? ' <button type="button" class="iblink" data-act="ib:undo">Undo</button>' : "") + "</div>" : "") +
      '<div class="ibfilters">' + filters + "</div>" +
      '<div class="ibsec"><h2 class="ibneed">Needs you</h2></div>' +
      (need.length ? need.map(card).join("") : '<p class="ibempty">Inbox zero. The herd is moving on its own.</p>') +
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
    inbox.reply = "";
    inbox.toast = "";
    inbox.detail = null;
    inbox.thOpen = {};
    if (it) { inboxMarkRead(it); inboxFetchThread(it); }
  }

  // Resolve an item: hide it at once (optimistic), toast, move to the next.
  function inboxResolve(it, msg, undoable) {
    const rest = inbox.items.filter((x) => x.id !== it.id);
    const next = rest.filter(inboxNeed)[0] || rest[0] || null;
    inbox.items = rest;
    inbox.sel = next ? next.id : "";
    inbox.reply = "";
    inbox.toast = msg;
    inbox.undo = undoable ? { id: it.id } : null;
    inbox.detail = null;
    inbox.thOpen = {};
    if (next) inboxFetchThread(next);
    if (undoable) {
      const token = it.id;
      setTimeout(() => { if (inbox.undo && inbox.undo.id === token) { inbox.undo = null; inbox.toast = ""; renderInbox(); } }, 6000);
    }
    renderInbox();
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
    const it = inbox.items.find((x) => x.id === id);
    if (act === "filter") { inbox.filter = el.getAttribute("data-f"); renderInbox(); return; }
    if (act === "undo") {
      if (inbox.undo) { emit("inbox-set", { id: inbox.undo.id, done: false }); inbox.undo = null; inbox.toast = ""; renderInbox(); }
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
      // On a phone a card expands in place; tapping the open one closes it.
      if (inboxNarrow() && id === inbox.sel) { inbox.sel = ""; inbox.detail = null; inbox.thOpen = {}; renderInbox(); return; }
      inboxSelect(it); renderInbox(); return;
    }
    if (act === "answer") { emit("agent-answer", { name: it.agent, n: +el.getAttribute("data-n") }); inboxResolve(it, "Sent " + el.getAttribute("data-n") + " to " + it.agent, false); return; }
    if (act === "guard-approve") { emit("guard-approve", { id: it.guardId }); inboxResolve(it, "Approved once — the agent was told to run it again", false); return; }
    if (act === "prop-approve") { emit("memory-proposal-approve", { id: it.proposalId }); inboxResolve(it, it.lint ? "Check on — turn it off on the Memory tab" : "Approved — handed to the lead to apply on a branch", false); return; }
    if (act === "prop-reject") { emit("memory-proposal-reject", { id: it.proposalId, reason: "" }); inboxResolve(it, it.lint ? "Rejected — no check; the fact stays in memory" : "Rejected — the facts stay in memory", false); return; }
    if (act === "reply") {
      const ta = app.querySelector('.ibreplybox[data-id="' + cssName(it.id) + '"]');
      const text = ((ta && ta.value) || inbox.reply || "").trim();
      if (!text) { setStatus("Write the answer first"); return; }
      if (it.replyVia === "primary") emit("primary-say", { text: text });
      else if (it.replyVia === "agent") emit("agent-prompt", { name: it.agent, text: text });
      else emit("lead-say", { project: it.project, text: text });
      inboxResolve(it, "Reply sent to " + it.agent, false);
      return;
    }
    if (act === "login") { emit("agent-login", { name: it.agent }); inboxResolve(it, "/login sent to " + it.agent, false); return; }
    if (act === "relay") { emit("agent-relay", { name: it.agent, kind: it.relayTo }); inboxResolve(it, "Relayed to " + it.relayTo, false); return; }
    if (act === "submit") { emit("agent-submit", { name: it.agent }); inboxResolve(it, "Return pressed for " + it.agent, false); return; }
    if (act === "stop-gone") { emit("agent-stop-gone", { name: it.agent }); inboxResolve(it, "Stopped " + it.agent, false); return; }
    if (act === "done" && it.kind === "guard") { guardKeepDenied(it); return; }
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

  let threads = { list: [], talking: [], live: 0, more: false, req: null, cur: null, error: "", open: {}, phone: false,
                  costs: [], costDays: 7 };
  let threadsWatching = false;

  const TH_STATUS = { live: "Live", waiting: "Waiting", answered: "Answered", failed: "Failed" };
  const TH_LABEL = { ask: "ask", handoff: "handoff", answer: "answer", message: "message", failed: "failed", cancelled: "cancelled" };

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
  // list, lead mode against direct mode.

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
    return '<span class="thcost" title="' + esc(taskTitle(c)) + '">' + esc(tokens(c.total)) + " · " + esc(taskMade(c)) + "</span>";
  }
  function thCostLine(c) {
    if (!c) return "";
    const per = c.code === "yes" && c.lines ? " · " + tokens(Math.round((c.total * 100) / c.lines)) + " per 100 lines" : "";
    const merged = c.code === "yes" ? " (" + taskMerged(c) + ")" : "";
    return '<p class="thcostline" title="' + esc(taskTitle(c)) + '">Its task' +
      (c.threadCount > 1 ? ", across " + c.threadCount + " threads" : "") + ": " + esc(tokens(c.total)) + " weighted tokens — " +
      (c.mode === "lead" ? "lead " : "itself ") + esc(tokens(c.own)) + " · workers " + esc(tokens(c.workers)) +
      " · sub-agents " + esc(tokens(c.subs)) + " · " + esc(taskMade(c)) + esc(merged) + esc(per) + "</p>";
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
  function thCosts() {
    const all = threads.costs || [];
    const c = all.find((x) => x.days === threads.costDays) || all[0];
    if (!c || !c.lead || !c.direct) return "";
    const n = (v) => (v == null ? "—" : tokens(v));
    const row = (label, a, b) => "<tr><th>" + esc(label) + "</th><td>" + esc(a) + "</td><td>" + esc(b) + "</td></tr>";
    const L = c.lead, D = c.direct;
    const days = all.map((x) => '<button type="button" class="thcd' + (x.days === c.days ? " on" : "") +
      '" data-act="th:cost-days" data-days="' + x.days + '">' + x.days + " days</button>").join("");
    // A task whose code is not known yet, or cannot be: in neither "lines
    // changed" nor "no code" -- said when there is one, as `herd cost' says it.
    const some = (key, label) => (L[key] || D[key] ? row(label, String(L[key] || 0), String(D[key] || 0)) : "");
    return '<section class="thcosts" aria-label="What a task costs"><div class="thtalkhead"><h2>What a task costs</h2>' +
      '<span class="thsub">through a lead, or told to an agent directly · weighted tokens · since ' + esc(c.since) + "</span>" +
      '<span class="thcds">' + days + "</span></div>" +
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
    if (!threadsWatching) { threadsWatching = true; emit("threads-watch"); }
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

  function thTalk() {
    const chips = threads.talking.map((e) =>
      '<button type="button" class="thchip" data-act="th:open" data-ask="' + esc(e.ask) + '" title="' + esc(e.kind + " " + e.ask) + '">' +
      '<span class="thname">' + esc(thWho(e.from)) + "</span>" +
      '<span class="tharrow k-' + esc(e.kind) + '">' + (e.kind === "handoff" ? "⇢" : "→") + "</span>" +
      '<span class="thname">' + esc(thWho(e.to)) + "</span>" +
      '<span class="thwhat">' + esc(e.status === "queued" ? "queued" : (e.working ? "working " : "waiting ") + thAge(e.since)) + "</span></button>").join("");
    return '<section class="thtalk" aria-label="Talking now"><div class="thtalkhead"><h2>Talking now</h2>' +
      '<span class="thsub">every open ask and handoff in the herd, across projects</span></div>' +
      (chips ? '<div class="thchips">' + chips + "</div>" : '<p class="thquiet">Nobody is waiting on anyone right now.</p>') + "</section>";
  }

  function thRow(t) {
    const on = !!(threads.cur ? t.id === threads.cur.id : threads.req && threads.req.id === t.id);
    return '<button type="button" class="throw' + (on ? " on" : "") + '" data-act="th:open" data-id="' + esc(t.id) + '" aria-current="' + (on ? "true" : "false") + '">' +
      '<span class="thr1">' + thStatus(t.status) + '<span class="thtitle">' + esc(t.title || "(no text)") + "</span>" +
      '<span class="thage">' + esc(thAge(t.at)) + "</span></span>" +
      '<span class="thpath">' + esc((t.participants || []).map(thWho).join(" → ")) + "</span>" +
      '<span class="thr3">' + (t.projects || []).map((p) => '<span class="thproj">' + esc(p) + "</span>").join("") +
      (t.cross ? '<span class="thcross">cross-project</span>' : "") + thCost(t.cost) +
      '<span class="thcount">' + t.count + (t.count === 1 ? " message" : " messages") + "</span></span></button>";
  }

  function thList() {
    return '<section class="thlist" aria-label="Threads">' +
      (threads.list.length ? threads.list.map(thRow).join("")
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
        (round ? "<span>· " + esc(round) + "</span>" : "") + (m.logged ? "<span>· as the herd log has it</span>" : "") + "</span>" +
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
    return '<div class="thhead1">' + thStatus(t.status) + '<h1 class="thh1">' + esc(t.title || "(no text)") + "</h1></div>" +
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

  function onThreadsAction(act, el) {
    if (act === "open") {
      const id = el.getAttribute("data-id"), ask = el.getAttribute("data-ask");
      if (view !== "threads") showThreads(id ? { id } : { ask });
      else openThread(id ? { id } : { ask });
    } else if (act === "more") {
      emit("threads-more");
    } else if (act === "cost-days") {
      threads.costDays = +el.getAttribute("data-days") || 7;
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


  window.GM = {
    flash: (msg) => setStatus(msg),

    // The Threads tab's list and what is open now, pushed while it is
    // shown.  The desk opens the newest thread when none is open.
    renderThreads: (p) => {
      threads.list = (p && p.threads) || [];
      threads.talking = (p && p.talking) || [];
      threads.live = (p && p.live) || 0;
      threads.more = !!(p && p.more);
      threads.costs = (p && p.costs) || [];
      threadsBadge();
      if (view !== "threads") return;
      if (!threads.req && !thNarrow() && threads.list.length) openThread({ id: threads.list[0].id });
      else renderThreads();
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
      inbox.items = (p && p.items) || [];
      inbox.count = (p && p.count) || 0;
      if (inbox.sel && !inbox.items.some((x) => x.id === inbox.sel)) { inbox.sel = ""; inbox.detail = null; inbox.thOpen = {}; }
      inboxBadge();
      if (view === "inbox") renderInbox();
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
      if (view === "inbox" && p.for === inbox.sel) renderInbox();
    },

    // The room's chat: the poll's recent tail of its timeline, merged in.
    setChat: (p) => {
      if (!p || !herdNow.room || p.project !== herdNow.room) return;
      const changed = mergeRecentChat(p);
      if (roomIsChat() && changed) renderChatBody();
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
      const ta = app.querySelector('[data-name="' + cssName(name) + '"] textarea.agent-text');
      if (!ta) {
        herdNow.composing.add(name);
        if (view === "agents") renderAgents();
        return;
      }
      renderAttach(name);
      ta.focus();
    },

    // The clipboard, for Cmd-V: text goes in at the cursor; with none,
    // the clipboard's image goes to the prompt box the cursor is in.
    paste: (p) => {
      const el = document.activeElement;
      const text = (p && p.text) || "";
      if (text) {
        if (el && (el.tagName === "TEXTAREA" || el.tagName === "INPUT")) document.execCommand("insertText", false, text);
        return;
      }
      if (el && el.classList && el.classList.contains("agent-text")) askImage(el);
      else setStatus("To attach an image, click into a prompt box first");
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

    setContext: (p) => {
      const sources = (p && p.sources) || [];
      sources.sort((a, b) => (b.mtime || 0) - (a.mtime || 0));
      const byPath = {};
      const counts = {};
      sources.forEach((s) => {
        byPath[s.source_path] = s;
        if (s.project) counts[s.project] = (counts[s.project] || 0) + 1;
      });
      ctx = {
        sources: sources,
        byPath: byPath,
        total: (p && p.total) || sources.length,
        currentProject: (p && p.current_project) || "",
        projects: Object.keys(counts)
          .map((k) => ({ project: k, leaf: leaf(k), n: counts[k] }))
          .sort((a, b) => b.n - a.n || a.leaf.localeCompare(b.leaf)),
      };
      if (p && p.project) filters.project = p.project;
      if (filters.project && !counts[filters.project]) filters.project = "";
      if (p && p.query) query = p.query;
      if (p && p.view === "sessions") showSessions();
      else if (p && p.view === "search") showSearch();
      else if (view === "sessions") showSessions();
      else if (view === "source" || view === "log" || view === "agents") chrome();
      else showSearch();
      if (p && p.query) search(true);
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
      const changed = mergeRecentConv(p.name, p);
      if (view !== "agents") return;
      const a = roomAgents().find((x) => x.name === p.name);
      if (!a) return;
      if (changed) renderRoomBody(a);
      // Whether it has a conversation decides its Screen button -- in a
      // column: one that comes as the room turns to its chat has none.
      const col = roomColumn(a.name);
      if (col) col.querySelector(".rtop").innerHTML = roomTop(a, asksFor(a.name, (herdNow.data && herdNow.data.asks) || []));
    },

    // An older page for a column (from `load earlier'): prepended to it.
    earlierConversation: (p) => {
      if (!p || !p.name || !herdNow.room) return;
      prependConv(p.name, p);
      if (view !== "agents") return;
      const a = roomAgents().find((x) => x.name === p.name);
      if (a) renderRoomBody(a);
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
      herd.entries.push(e);
      if (view !== "log") return;
      // New entries go on top: keep what is being read where it is.
      const before = app.scrollHeight;
      const top = app.scrollTop;
      renderLog();
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
      if (view === "log") renderLogList();
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

  // Keep the page exactly as tall as the browser is showing.  The dynamic
  // viewport (the phone's URL bar) is handled by CSS `100dvh'; the keyboard
  // is not -- it covers the bottom without shrinking the layout -- so track
  // the visual viewport into --app-h.  The room/chat is then a flex column
  // of that height: the composer (flex-shrink:0) stays above the keyboard
  // and the transcript (flex:1) shrinks, not the other way round.  On a
  // desktop the visual viewport is the window, so this is a no-op.
  function fitViewport() {
    const vv = window.visualViewport;
    const h = vv ? vv.height : window.innerHeight;
    document.documentElement.style.setProperty("--app-h", Math.round(h) + "px");
    // When the keyboard shifts the page down, pin it back so the top bar
    // and the composer both sit in the visible band.
    if (vv && vv.offsetTop > 0) window.scrollTo(0, 0);
  }
  if (window.visualViewport) {
    visualViewport.addEventListener("resize", fitViewport);
    visualViewport.addEventListener("scroll", fitViewport);
  }
  window.addEventListener("resize", fitViewport);
  window.addEventListener("orientationchange", fitViewport);
  fitViewport();

  emit("ready");
  restore();
  // The Threads tab's badge, before the tab is opened.
  emit("threads-peek");
})();
