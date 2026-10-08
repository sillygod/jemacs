(() => {
  "use strict";

  const PREFIX = "ghmem:";
  const $ = (id) => document.getElementById(id);
  const app = $("app");
  const statusEl = $("status");
  const btnBack = $("btn-back");

  // ---- Intents ---------------------------------------------------------
  //
  // Queued and counted by xwapp's page kit (../../xwapp/ui/xwapp.js).

  const emit = XW.bridge(PREFIX);

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
  let view = "boot"; // agents | search | sessions | source | log
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

  function sourceMeta(path) {
    return ctx.byPath[path] || { source_path: path, agent: "", project: "", kind: "", chunks: 0 };
  }

  // ---- Chrome ----------------------------------------------------------

  function chrome() {
    if (watching && view !== "agents") {
      watching = false;
      emit("herd-unwatch");
    }
    document.querySelectorAll(".tab").forEach((b) => {
      b.classList.toggle("on", view !== "source" && b.getAttribute("data-tab") === view);
    });
    btnBack.hidden = view !== "source";
    $("sessions-n").textContent = ctx.sources.length ? String(ctx.sources.length) : "";
    app.setAttribute("data-view", view);
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
      'placeholder="Search past sessions — 中文 or English  ( / )" value="' + esc(query) + '"></div>' +
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
      'placeholder="Filter by project, path or session id  ( / )" value="' + esc(sessionsFilter) + '"></div>' +
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

  function messageHtml(m, agent) {
    // agy's text is UTF-8 salvaged from protobuf, full of tag-like noise:
    // as markdown the sanitizer would eat half of it.
    const body = agent === "agy" ? '<div class="plain">' + esc(m.text) + "</div>" : markdown(m.text);
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
      html += '<button type="button" class="ghost small" data-act="visit-agent" title="An agent is on this conversation already">Open ' +
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
    const msgs = messagesOf(src.chunks);
    $("src-body").innerHTML = msgs.length
      ? msgs.map((m) => messageHtml(m, agent)).join("")
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

  const RANK = { blocked: 0, working: 1, starting: 2, done: 3, idle: 4, dead: 5 };
  let herdNow = {
    data: null, screens: {}, open: new Set(), more: new Set(),
    compose: {}, composing: new Set(), notes: null, newFor: null, pending: false,
  };

  function cssName(n) {
    return String(n).replace(/["\\]/g, "\\$&");
  }

  function busyEditing() {
    const a = document.activeElement;
    return !!(a && a.closest && a.closest(".agent-compose, .newform, .notes-edit"));
  }

  function showAgents() {
    view = tab = "agents";
    src = null;
    chrome();
    if (!watching) {
      watching = true;
      emit("herd-watch");
    }
    app.innerHTML = '<div class="page wide"><div id="usage" class="usage"></div><div id="herd"></div></div>';
    renderAgents();
  }

  function untilText(ms) {
    if (!ms) return "";
    const d = ms - Date.now();
    return d <= 0 ? "reset due" : "resets in " + span(d);
  }

  function usageHtml(list) {
    if (!list || !list.length) {
      return '<p class="empty">No usage yet: the first reading arrives a few seconds after the page opens.</p>';
    }
    return list.map((k) => {
      const wins = (k.windows || []).map((w) => {
        const lvl = w.used >= 90 ? "hot" : w.used >= 70 ? "warm" : "ok";
        return '<div class="uwin" title="' + esc(w.label + ": " + w.used + "% used, " + (100 - w.used) + "% left" +
          (w.resets ? ", resets " + new Date(w.resets).toLocaleString() : "")) + '">' +
          '<span class="ulabel">' + esc(w.label) + "</span>" +
          '<span class="ubar"><span class="lvl-' + lvl + '" style="width:' + Math.max(0, Math.min(100, w.used)) + '%"></span></span>' +
          '<span class="upct">' + w.used + "%</span>" +
          '<span class="ureset">' + esc(untilText(w.resets)) + "</span></div>";
      }).join("");
      const err = k.error
        ? '<div class="uerr" title="' + esc(k.error) + '">' + (wins ? "stale · " : "") + esc(k.error) + "</div>"
        : "";
      return '<div class="ucard"><div class="uhead">' + agentBadge(k.kind) +
        '<span class="when">' + (k.fetched ? esc(when(k.fetched / 1000)) : "") + "</span></div>" +
        (wins || (err ? "" : '<p class="muted">No windows reported.</p>')) + err + "</div>";
    }).join("");
  }

  function agentRow(a, kinds) {
    const name = a.name;
    const blocked = a.state === "blocked";
    const acts = [];
    if (blocked) {
      [1, 2, 3].forEach((n) => acts.push('<button type="button" class="ghost small answer" data-act="a:agent-answer" data-n="' + n +
        '" title="Answer ' + n + '">' + n + "</button>"));
    }
    acts.push('<button type="button" class="ghost small" data-act="a:compose">Prompt</button>');
    acts.push('<button type="button" class="ghost small" data-act="a:agent-interrupt" title="Send Escape">Esc</button>');
    acts.push('<button type="button" class="ghost small" data-act="a:screen">' + (herdNow.open.has(name) ? "Hide screen" : "Screen") + "</button>");
    acts.push('<button type="button" class="linkish" data-act="a:more" title="More">' + (herdNow.more.has(name) ? "less" : "more") + "</button>");
    const more = herdNow.more.has(name)
      ? '<div class="aacts more">' +
        '<button type="button" class="ghost small" data-act="a:agent-abort" title="Send C-c">C-c</button>' +
        '<button type="button" class="ghost small" data-act="a:notes">Notes</button>' +
        '<button type="button" class="ghost small" data-act="a:respawn">Respawn</button>' +
        '<button type="button" class="ghost small danger" data-act="a:kill">Kill</button></div>'
      : "";
    const compose = herdNow.composing.has(name)
      ? '<div class="agent-compose"><textarea class="agent-text" rows="3" placeholder="Prompt for ' + esc(name) + '  (⌘↩ / C-↩ sends)">' +
        esc(herdNow.compose[name] || "") + '</textarea><button type="button" class="primary small" data-act="a:send">Send</button></div>'
      : "";
    const notes = herdNow.notes === name
      ? '<div class="notes-edit"><input class="notes-text" value="' + esc(a.notes) + '" placeholder="Role / notes">' +
        '<button type="button" class="primary small" data-act="a:notes-save">Save</button>' +
        '<button type="button" class="ghost small" data-act="a:notes-cancel">Cancel</button></div>'
      : "";
    const screen = herdNow.open.has(name)
      ? '<pre class="screen">' + esc(herdNow.screens[name] || "Capturing…") + "</pre>"
      : "";
    return (
      '<div class="arow s-' + esc(a.state) + '" data-name="' + esc(name) + '">' +
      '<div class="amain">' + stateBadge(a.state) +
      '<button type="button" class="aname" data-act="a:agent-visit" title="Open in Emacs">' + esc(name) + "</button>" +
      agentBadge(a.kind) +
      (a.detached ? '<span class="det" title="Running, no view attached">▪ detached</span>' : "") +
      (a.manual ? '<span class="det" title="State set by hand">manual</span>' : "") +
      (a.progress != null ? '<span class="det">' + esc(a.progress) + "%</span>" : "") +
      (a.notes ? '<span class="anotes" title="' + esc(a.notes) + '">' + esc(a.notes) + "</span>" : "") +
      '<span class="when" title="' + esc(a.since ? new Date(a.since).toLocaleString() : "") + '">' +
      esc(a.since ? when(a.since / 1000) : "") + "</span></div>" +
      (a.reason && a.reason !== "—" ? '<div class="areason">' + esc(a.reason) + "</div>" : "") +
      '<div class="aacts">' + acts.join("") + "</div>" + more + compose + notes + screen + "</div>"
    );
  }

  function renderAgents() {
    herdNow.pending = false;
    const u = $("usage");
    const box = $("herd");
    if (!u || !box) return;
    const d = herdNow.data;
    if (!d) {
      box.innerHTML = '<p class="empty">Asking Emacs for the herd…</p>';
      return;
    }
    u.innerHTML = usageHtml(d.usage);
    const kinds = d.kinds || [];
    const by = new Map();
    (d.agents || []).forEach((a) => {
      const k = a.project || "";
      if (!by.has(k)) by.set(k, []);
      by.get(k).push(a);
    });
    const rank = (s) => (s in RANK ? RANK[s] : 9);
    const groups = [...by.entries()].map(([project, list]) => ({
      project,
      list: list.sort((a, b) => rank(a.state) - rank(b.state) || a.name.localeCompare(b.name)),
    }));
    groups.sort((a, b) => rank(a.list[0].state) - rank(b.list[0].state) || leaf(a.project).localeCompare(leaf(b.project)));
    const keep = app.scrollTop;
    box.innerHTML = groups.length
      ? groups.map((g) => {
          const counts = {};
          g.list.forEach((a) => (counts[a.state] = (counts[a.state] || 0) + 1));
          const summary = Object.keys(counts).sort((a, b) => rank(a) - rank(b))
            .map((s) => '<span class="st st-' + esc(s) + '">' + counts[s] + " " + esc(s) + "</span>").join("");
          const form = herdNow.newFor === g.project
            ? '<div class="newform"><select class="new-kind">' + kinds.map((k) => '<option value="' + esc(k) + '">' + esc(k) + "</option>").join("") +
              '</select><input class="new-name" placeholder="name (default: kind-' + esc(leaf(g.project)) + ')">' +
              '<button type="button" class="primary small" data-act="a:new-go">Start</button>' +
              '<button type="button" class="ghost small" data-act="a:new-cancel">Cancel</button></div>'
            : "";
          return '<section class="pgroup" data-project="' + esc(g.project) + '"><div class="phead">' +
            '<span class="pname" title="' + esc(g.project) + '">' + esc(g.project ? leaf(g.project) : "No project") + "</span>" +
            '<span class="ppath">' + esc(g.project) + "</span>" + summary +
            (g.project ? '<button type="button" class="linkish" data-act="a:new-agent">+ New agent</button>' : "") +
            "</div>" + form + g.list.map((a) => agentRow(a, kinds)).join("") + "</section>";
        }).join("")
      : '<p class="empty">No agents in the herd.  SPC a h n starts one; so does + New agent once there is a project here.</p>';
    app.scrollTop = keep;
  }

  function onAgentAction(act, el) {
    const row = el.closest(".arow");
    const name = row && row.getAttribute("data-name");
    const group = el.closest(".pgroup");
    const project = group && group.getAttribute("data-project");
    if (act === "agent-answer") return emit("agent-answer", { name, n: +el.getAttribute("data-n") });
    if (act === "agent-visit" || act === "agent-interrupt" || act === "agent-abort") return emit(act, { name });
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
    if (act === "notes") {
      herdNow.notes = name;
      renderAgents();
      const inp = app.querySelector(".notes-text");
      if (inp) inp.focus();
      return;
    }
    if (act === "notes-save") {
      const inp = row.querySelector(".notes-text");
      herdNow.notes = null;
      emit("agent-notes", { name, text: inp ? inp.value : "" });
      return renderAgents();
    }
    if (act === "notes-cancel") {
      herdNow.notes = null;
      return renderAgents();
    }
    if (act === "respawn") {
      return ask("Respawn " + name + "?", "It is relaunched from its recipe. Continue picks the conversation up again; Fresh starts it cold.",
        [["Cancel", "ghost", null], ["Fresh", "ghost", () => emit("agent-respawn", { name, continue: false })],
         ["Continue", "primary", () => emit("agent-respawn", { name, continue: true })]]);
    }
    if (act === "kill") {
      return ask("Kill " + name + "?", "The agent stops and leaves the herd. Its transcript stays.",
        [["Cancel", "ghost", null], ["Kill", "primary danger", () => emit("agent-kill", { name })]]);
    }
    if (act === "new-agent") {
      herdNow.newFor = project;
      renderAgents();
      const sel = app.querySelector(".newform select");
      if (sel) sel.focus();
      return;
    }
    if (act === "new-go") {
      const form = el.closest(".newform");
      herdNow.newFor = null;
      emit("agent-new", { project, kind: form.querySelector(".new-kind").value, name: form.querySelector(".new-name").value.trim() });
      return renderAgents();
    }
    if (act === "new-cancel") {
      herdNow.newFor = null;
      return renderAgents();
    }
  }

  function sendPrompt(name) {
    const ta = app.querySelector('.arow[data-name="' + cssName(name) + '"] textarea');
    const text = ta ? ta.value : herdNow.compose[name] || "";
    if (!text.trim()) return;
    emit("agent-prompt", { name, text });
    herdNow.compose[name] = "";
    herdNow.composing.delete(name);
    if (ta) ta.blur();
    renderAgents();
  }

  app.addEventListener("input", (ev) => {
    if (ev.target.classList && ev.target.classList.contains("agent-text")) {
      const row = ev.target.closest(".arow");
      if (row) herdNow.compose[row.getAttribute("data-name")] = ev.target.value;
    }
  });
  app.addEventListener("keydown", (ev) => {
    const t = ev.target;
    if (!t.classList) return;
    if (t.classList.contains("agent-text") && ev.key === "Enter" && (ev.metaKey || ev.ctrlKey)) {
      ev.preventDefault();
      sendPrompt(t.closest(".arow").getAttribute("data-name"));
    } else if (t.classList.contains("notes-text") && ev.key === "Enter") {
      ev.preventDefault();
      onAgentAction("notes-save", t);
    }
  });
  // An edit in progress holds the 5s refresh back; let it through after.
  app.addEventListener("focusout", () => {
    setTimeout(() => { if (herdNow.pending && view === "agents" && !busyEditing()) renderAgents(); }, 0);
  });

  // ---- The herd log -----------------------------------------------------
  //
  // One lane per agent: its states as coloured segments across the
  // range, rebuilt from the state transitions (old -> new, with when and
  // why).  Below, the entries, newest first.  The lanes are the point:
  // back from an absence, who sat blocked, since when, is one look.

  const HOUR = 3600e3;
  const RANGES = [[6 * HOUR, "6h"], [24 * HOUR, "24h"], [7 * 24 * HOUR, "7d"], [0, "All"]];
  let herd = { entries: null, range: 24 * HOUR, session: "", blocked: false, filter: "", open: new Set() };

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
      "</div>" +
      '<input id="log-filter" type="search" autocomplete="off" spellcheck="false" placeholder="Filter  ( / )" value="' + esc(herd.filter) + '">' +
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
    const screen = e.screen
      ? '<button type="button" class="linkish" data-act="screen" data-i="' + i + '">' +
        (herd.open.has(i) ? "Hide screen" : "Screen") + "</button>"
      : "";
    return (
      '<div class="lrow" data-i="' + i + '"><span class="lt" title="' + esc(new Date(e.t).toLocaleString()) + '">' +
      esc(clock(e.t)) + "</span>" +
      '<button type="button" class="ls" data-act="log-session" data-session="' + esc(e.session) + '">' +
      '<span class="dot" style="background:' + sessionColor(e.session) + '"></span>' + esc(e.session) + "</button>" +
      '<span class="lx">' + body + "</span>" + screen + "</div>" +
      (e.screen && herd.open.has(i) ? '<pre class="screen">' + esc(e.screen) + "</pre>" : "")
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
      if (herd.session && e.session !== herd.session) continue;
      if (herd.blocked && !(e.kind === "state" && e.new === "blocked")) continue;
      if (f && !(e.text + " " + e.session).toLowerCase().includes(f)) continue;
      rows.push([e, i]);
    }
    let day = "";
    box.innerHTML = rows.length
      ? '<div class="sum">' + rows.length + (rows.length === 1 ? " entry" : " entries") + "</div>" +
        rows.map(([e, i]) => {
          const d = dayLabel(e.t);
          const head = d !== day ? '<h2 class="section">' + esc(d) + "</h2>" : "";
          day = d;
          return head + entryRow(e, i, re);
        }).join("")
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
    }
  });

  app.addEventListener("change", (ev) => {
    if (ev.target.id === "log-session") {
      herd.session = ev.target.value;
      renderLog();
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

  app.addEventListener("click", (ev) => {
    const t = ev.target instanceof Element ? ev.target : null;
    if (!t) return;
    // A link would navigate the xwidget off the page and its bridge.
    const a = t.closest("a[href]");
    if (a) {
      ev.preventDefault();
      const href = a.getAttribute("href") || "";
      if (/^https?:/i.test(href)) emit("open-browser", { url: href });
      return;
    }
    const el = t.closest("[data-act]");
    if (!el || el.disabled) return;
    const act = el.getAttribute("data-act");
    if (act === "agent") {
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
      if (m) emit("copy", { text: m.text });
    } else if (act === "copy-path") {
      if (src) emit("copy", { text: src.path });
    } else if (act.startsWith("a:")) {
      onAgentAction(act.slice(2), el);
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
      renderLog();
    } else if (act === "log-session") {
      herd.session = herd.session === el.getAttribute("data-session") ? "" : el.getAttribute("data-session");
      renderLog();
    } else if (act === "screen") {
      const i = +el.getAttribute("data-i");
      if (herd.open.has(i)) herd.open.delete(i);
      else herd.open.add(i);
      renderLogList();
    } else if (act === "seg") {
      goToEntry(+el.getAttribute("data-i"));
    }
  });

  document.querySelectorAll(".tab").forEach((b) =>
    b.addEventListener("click", () => {
      lastOpened = null;
      src = null;
      const which = b.getAttribute("data-tab");
      if (which === "sessions") showSessions();
      else if (which === "log") showLog();
      else if (which === "agents") showAgents();
      else showSearch();
    }));
  btnBack.addEventListener("click", back);
  $("btn-refresh").addEventListener("click", () => {
    if (view === "log") emit("log");
    else if (view === "agents") emit("usage-refresh");
    else emit("refresh");
  });
  $("btn-import").addEventListener("click", () => {
    $("btn-import").disabled = true;
    emit("import");
  });

  document.addEventListener("keydown", (ev) => {
    const t = ev.target;
    const inInput = t && /^(INPUT|SELECT|TEXTAREA)$/.test(t.tagName);
    if (ev.metaKey || ev.ctrlKey || ev.altKey) return;
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
    } else if (ev.key === "Escape" && view === "source") {
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

  window.GM = {
    flash: (msg) => setStatus(msg),

    // The sidecar's import, read every few seconds; null once it ends.
    importProgress: (p) => {
      $("btn-import").disabled = !!p;
      if (!p) {
        if (statusEl.classList.contains("busy")) setStatus("");
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

    showView: (v) => {
      if (v === "agents") showAgents();
      else if (v === "log") showLog();
      else if (v === "sessions") showSessions();
      else showSearch();
    },

    renderHerd: (p) => {
      herdNow.data = p || null;
      if (view !== "agents") return;
      if (busyEditing()) herdNow.pending = true;
      else renderAgents();
    },

    setScreen: (p) => {
      if (!p) return;
      herdNow.screens[p.name] = p.text || "(no screen yet)";
      const pre = app.querySelector('.arow[data-name="' + cssName(p.name) + '"] pre.screen');
      if (pre) pre.textContent = herdNow.screens[p.name];
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

  emit("ready");
})();
