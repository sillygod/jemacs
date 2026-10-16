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

  // A box drawn once, kept as you type: its placeholder and Use follow
  // what the agent suggests now.
  function suggestIn(box, name, hint) {
    const ta = box.querySelector("textarea.agent-text");
    const use = box.querySelector(".use");
    const a = agentByName(name);
    if (ta && ta.placeholder !== hint) ta.placeholder = hint;
    if (use) use.hidden = !(a && a.suggestion);
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
    else if (where === "sessions") showSessions();
    else if (where === "search") showSearch();
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

  function messageHtml(m, agent) {
    // agy once imported from a protobuf salvage full of tag-like noise and
    // was shown as plain text; now it imports from transcript.jsonl, clean
    // prose like the rest, so it renders as markdown too.
    const body = markdown(m.text);
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
    data: null, screens: {}, open: new Set(), more: new Set(), hooksOpen: new Set(),
    compose: {}, composing: new Set(), notes: null, newFor: null, pending: false,
    // What the open forms say, kept here as the prompts are: the page is
    // drawn again every 5s and on news, and a form drawn from nothing
    // would come back empty, its first CLI picked.  Set afresh as each
    // form opens.
    notesText: "", newKind: "", newName: "",
    // The model form: whose it is, and the model and effort picked.
    modelFor: null, modelPick: {},
    // Images attached to each agent's next prompt: [{path, thumb}].
    attach: {},
    // The room: its project, each agent's conversation, the agents shown
    // by their screen instead, and the tool lines opened.
    room: null, convs: {}, roomScreen: new Set(), toolsOpen: new Set(),
    // The sub-agents' reports opened whole.
    subOpen: new Set(),
    // A sub-agent's own transcript, opened over the room: which, and what
    // Emacs sent of it.
    subView: null, subConv: null,
    // The room as one chat: the view shown, the timeline Emacs sent, who
    // the box writes to in each project, and the long messages opened.
    roomView: NARROW ? "chat" : "columns", chat: null, chatTo: {}, chatOpen: new Set(),
    // How far back each column (by name) and the chat have been loaded with
    // `load earlier'; kept so a poll does not shrink them.
    roomLimit: {}, chatLimit: 0,
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
    app.innerHTML = '<div class="page wide"><div id="usage" class="usage"></div><div id="herd"></div></div>';
    renderAgents();
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

  function askLine(k, name) {
    const open = k.status === "queued" || k.status === "delivered";
    const queued = k.status === "queued" ? " (queued)" : "";
    let what;
    if (open && k.to === name) what = "⇠ ask from " + k.from + queued;
    else if (open) what = "⇢ asked " + k.to + queued;
    else if (k.status === "answered") what = k.auto ? "⇠ " + k.to + " stopped without answering" : "⇠ " + k.to + " answered";
    else what = "⇢ ask to " + k.to + " " + k.status + (k.error ? ": " + k.error : "");
    if (k.round && (k.parent || k.round > 1)) what += " · round " + k.round + "/" + k.of;
    const text = k.status === "answered" ? k.replyHead : open ? k.head : "";
    const t = open ? k.created : k.answered;
    return '<div class="aask ask-' + esc(k.status) + (k.auto ? " ask-auto" : "") + '" title="' +
      esc("ask " + k.id + ": " + (k.head || "")) + '">' +
      '<span class="awhat">' + esc(what) + "</span>" +
      (text ? '<span class="atext">' + esc(text) + "</span>" : "") +
      '<span class="when">' + esc(t ? when(t / 1000) : "") + "</span></div>";
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
      path = [deepest[0].from].concat(deepest.map((k) => k.to));
      const below = ks.find((k) => k.parent === d.id);  // its latest round with someone
      if (below) {
        parts.push(below.status !== "answered" ? below.to + " " + below.status
          : below.auto ? below.to + " stopped without answering"
          : (below.to + " answered " + roundOf(below)).trim());
      } else if (d.round && (d.parent || d.round > 1)) parts.push(roundOf(d));
      parts.push(d.status === "queued" ? d.to + " is busy; the ask waits" : "waiting on " + d.to);
      at = d.created;
    } else {
      const top = ks.find((k) => !k.parent) || ks[0];
      const asker = agents.find((a) => a.name === run.from);
      path = [top.from, top.to];
      const used = roundsUsed(ks);
      at = top.answered;
      if (top.status === "failed" || top.status === "cancelled") {
        cls = "run-failed";
        parts.push("stopped: ask to " + top.to + " " + top.status + (top.error ? " (" + top.error + ")" : ""));
      } else if (top.auto) {
        cls = "run-auto";
        parts.push(top.to + " stopped without answering");
      } else if (asker && ["working", "starting", "waiting"].includes(asker.state)) {
        parts.push(top.to + " answered; " + run.from + " at work");
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

  function workFor(project) {
    const d = herdNow.data;
    return ((d && d.work) || []).find((w) => w.project === project) || null;
  }

  function safeColor(c) {
    return /^#[0-9a-fA-F]{3,8}$/.test(c || "") ? c : "#8b949e";
  }

  function prState(pr) {
    if (!pr.state) return "";
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
    return href
      ? '<a class="' + cls + '" href="' + esc(href) + '" target="' + target + '"' + (app ? "" : ' rel="noopener noreferrer"') + attrs + ">" + inner + "</a>"
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

  function workHtml(w) {
    if (!w || !w.branch) return "";
    const pr = w.pr || null;
    const tasks = w.tasks || [];
    const changes = changesChip(w);
    // The default branch without a PR says nothing of one -- but what it
    // has not committed, it says.
    const quiet = !pr && !tasks.length && !w.error && QUIET_BRANCHES.includes(w.branch);
    if (quiet && !changes) return "";
    const key = w.root + "|" + w.branch;
    const open = herdNow.workOpen.has(key);
    const prPart = quiet ? ""
      : pr ? prChip(w.root, pr)
      : w.error ? '<span class="wnote" title="' + esc(w.error) + '">PR not looked up</span>'
      : w.pending ? '<span class="wnote">PR…</span>'
      : '<span class="wnote">no PR</span>';
    return '<div class="work"><span class="wbranch" title="' + esc(w.root) + '">⎇ ' + esc(w.branch) + "</span>" + prPart + changes +
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
    const prs = (l.prs || []).filter((p) => !(p.own !== false && p.id === onBranch) && !p.error).map((p) => {
      const live = !p.state || p.state === "OPEN";
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

  function meterHtml(a) {
    const m = a && a.meter;
    const p = pickerFor(a);
    // No meter (grok, agy do not report one) but a picker the herd knows:
    // still offer the switch, with no model to show until it is read.
    if (!m) {
      if (!p) return "";
      const btn = a.switching
        ? '<span class="mchip"><span class="meff">switching…</span></span>'
        : '<button type="button" class="mchip mbare" data-act="a:model" title="' +
          esc(switchHint(p)) + '">Model</button>';
      return '<div class="meter">' + btn + "</div>";
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
      : "") + "</div>";
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
  }, 1000);

  function subsHtml(a) {
    const subs = (a && a.subs) || [];
    if (!subs.length) return "";
    return '<div class="subs">' + subs.map((u) => {
      const stats = [runFor(u.started), esc(u.tools + (u.tools === 1 ? " tool" : " tools")),
        u.tokens != null ? esc(tokens(u.tokens)) : ""].filter(Boolean).join(" · ");
      return '<div class="sub" data-act="a:sub-open" data-sub="' + esc(u.id) + '" data-who="' + esc(u.who || "") +
        '" data-title="' + esc(u.title || "") + '" title="Open its transcript"><div class="sline"><span class="sdot" aria-hidden="true"></span>' +
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
    // Its CLI's usage used up: its relay first.
    if (a.usedUp && a.relayTo && a.relayTo !== a.kind) {
      acts.push('<button type="button" class="primary small" data-act="a:relay" title="' + esc(a.usedUp) + '">Relay to ' + esc(a.relayTo) + "</button>");
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
    const screen = herdNow.open.has(name)
      ? '<pre class="screen">' + esc(herdNow.screens[name] || "Capturing…") + "</pre>"
      : "";
    return (
      '<div class="arow s-' + esc(a.state) + '" data-name="' + esc(name) + '">' +
      '<div class="amain">' + stateBadge(a.state) +
      agentName(name) + genBadge(a) + agentBadge(a.kind) + leadBadge(a) +
      (a.detached ? '<span class="det" title="Running, no view attached">▪ detached</span>' : "") +
      (a.manual ? '<span class="det" title="State set by hand">manual</span>' : "") +
      (a.progress != null ? '<span class="det">' + esc(a.progress) + "%</span>" : "") + backgroundHtml(a) +
      (a.notes ? '<span class="anotes" title="' + esc(a.notes) + '">' + esc(a.notes) + "</span>" : "") +
      '<span class="when" title="' + esc(a.since ? new Date(a.since).toLocaleString() : "") + '">' +
      esc(a.since ? when(a.since / 1000) : "") + "</span></div>" +
      meterHtml(a) + modelFormHtml(a) + reasonHtml(a) +
      (a.said ? '<div class="aask said" title="' + esc((a.conversation ? "conversation " + a.conversation + "\n" : "") + a.said) + '">' +
        '<span class="awhat">said</span><span class="atext">' + esc(a.said) + "</span>" +
        '<span class="when">' + esc(a.saidAt ? when(a.saidAt / 1000) : "") + "</span></div>" : "") +
      (asks && asks.length ? '<div class="aasks">' + asks.map((k) => askLine(k, name)).join("") + "</div>" : "") +
      (a.work ? workHtml(a.work) : "") + linksHtml(a) + menu +
      '<div class="aacts">' + acts.join("") + "</div>" + more + compose + notes + screen + "</div>"
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
    const kinds = d.kinds || [];
    const by = new Map();
    (d.agents || []).forEach((a) => {
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
    box.innerHTML = groups.length
      ? groups.map((g) => {
          const counts = {};
          g.list.forEach((a) => (counts[a.state] = (counts[a.state] || 0) + 1));
          const summary = Object.keys(counts).sort((a, b) => rank(a) - rank(b))
            .map((s) => '<span class="st st-' + esc(s) + '">' + counts[s] + " " + esc(s) + "</span>").join("");
          const form = herdNow.newFor === g.project
            ? '<div class="newform"><select class="new-kind">' + kinds.map((k) => '<option value="' + esc(k) + '"' +
              (k === herdNow.newKind ? " selected" : "") + ">" + esc(k) + "</option>").join("") +
              '</select><input class="new-name" value="' + esc(herdNow.newName) + '" placeholder="name (default: kind-' + esc(leaf(g.project)) + ')">' +
              '<button type="button" class="primary small" data-act="a:new-go">Start</button>' +
              '<button type="button" class="ghost small" data-act="a:new-cancel">Cancel</button></div>'
            : "";
          return '<section class="pgroup" data-project="' + esc(g.project) + '"><div class="phead">' +
            (g.project
              ? '<button type="button" class="pname" data-act="a:room" title="Open the room: ' + esc(g.project) +
                '\nits agents\' conversations side by side">' + esc(leaf(g.project)) + "</button>"
              : '<span class="pname">No project</span>') +
            '<span class="ppath">' + esc(g.project) + "</span>" + summary +
            (g.project ? '<button type="button" class="linkish" data-act="a:new-agent">+ New agent</button>' : "") +
            "</div>" + workHtml(workFor(g.project)) + form + g.list.map((a) => agentRow(a, kinds, asksFor(a.name, d.asks))).join("") + "</section>";
        }).join("")
      : '<p class="empty">No agents in the herd.  SPC a h n starts one; so does + New agent once there is a project here.</p>';
    app.scrollTop = keep;
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
    herdNow.roomLimit = {};
    herdNow.chatLimit = 0;
    app.innerHTML = "";
    chrome();
    watchRoom();
    renderRoom();
  }

  // The default count each view reads (conversation.LIMIT / CHAT_LIMIT) and
  // the most the RPC will return (its clamp): `load earlier' grows from the
  // one toward the other, a page at a time.
  const ROOM_LIMIT = 60, ROOM_MAX = 200;
  const CHAT_LIMIT = 200, CHAT_MAX = 500;

  // What Emacs should send for the room: each column's conversation and
  // the screens shown instead, or the chat; and how far back each has been
  // loaded, so a poll keeps that, not the default.
  function watchRoom() {
    emit("room-watch", {
      project: herdNow.room, screens: [...herdNow.roomScreen], view: herdNow.roomView,
      limits: Object.keys(herdNow.roomLimit).map((name) => ({ name, limit: herdNow.roomLimit[name] })),
      chatLimit: herdNow.chatLimit || 0,
    });
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
    acts.push('<button type="button" class="ghost small" data-act="a:agent-interrupt" title="Send Escape">Esc</button>');
    const conv = herdNow.convs[name];
    if (!(conv && conv.none)) {
      acts.push('<button type="button" class="ghost small" data-act="a:room-screen">' +
        (herdNow.roomScreen.has(name) ? "Conversation" : "Screen") + "</button>");
    }
    return '<div class="amain">' + stateBadge(a.state) +
      '<span class="rdot" style="color:' + sessionColor(name) + '" title="Its colour in every column">●</span>' +
      agentName(name) + agentBadge(a.kind) + leadBadge(a) + backgroundHtml(a) +
      (a.notes ? '<span class="anotes" title="' + esc(a.notes) + '">' + esc(a.notes) + "</span>" : "") +
      '<span class="when" title="' + esc(a.since ? new Date(a.since).toLocaleString() : "") + '">' +
      esc(a.since ? when(a.since / 1000) : "") + "</span></div>" +
      meterHtml(a) + modelFormHtml(a) + reasonHtml(a) +
      (asks && asks.length ? '<div class="aasks">' + asks.map((k) => askLine(k, name)).join("") + "</div>" : "") +
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
  function entryHtml(e, i, name, kind) {
    const from = e.who && e.who !== "user" ? e.who : "";
    const color = from ? sessionColor(from) : "";
    const bar = color ? ' style="border-left-color:' + color + '"' : "";
    const own = sessionColor(name);
    if (e.role === "assistant") {
      return '<div class="ent me" style="border-left-color:' + own + '">' + entHead(name, "", e.ts, own, copyButton(i, e.text)) +
        '<div class="ebody msg-body">' + markdown(e.text) + "</div></div>";
    }
    if (e.role === "tools") {
      const tools = e.tools || [];
      const key = name + ":" + i;
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
        (text ? '<div class="ebody msg-body">' + markdown(text) + "</div>" : "") +
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
          copyButton(i, e.answer, "answer")) + plainBody(e.answer) + "</div>"
        : e.waiting ? '<div class="eanswer waiting">waiting for ' + esc(e.to || "the answer") + "…</div>"
        : "";
      return '<div class="ent herd" style="border-left-color:' + own + '">' +
        entHead(name + " " + what, "", e.ts, own, copyButton(i, e.text)) + plainBody(e.text) + outcome + "</div>";
    }
    // Said to it: by you, or through the herd.
    if (e.kind === "command") return '<div class="ent small">› ' + esc(e.text) + "</div>";
    if (e.kind === "note") return '<div class="ent small">' + esc(e.text) + "</div>";
    if (e.kind === "ask") return '<div class="ent from"' + bar + ">" + entHead("⇠ " + from + " asks", "", e.ts, color, copyButton(i, e.text)) + plainBody(e.text) + "</div>";
    if (e.kind === "answer") {
      const how = e.failed ? " — ask failed" : e.auto ? " — stopped without answering" : "";
      return '<div class="ent from"' + bar + ">" + entHead("⇠ " + from + " answered" + how, "", e.ts, color, copyButton(i, e.text)) + plainBody(e.text) + "</div>";
    }
    if (e.kind === "message" && from) return '<div class="ent from"' + bar + ">" + entHead("⇠ " + from, "", e.ts, color, copyButton(i, e.text)) + plainBody(e.text) + "</div>";
    return '<div class="ent you">' + entHead("You", "", e.ts, "", copyButton(i, e.text)) + herdNotes(e) + plainBody(e.text) + "</div>";
  }

  // `Load earlier' when there is more to show and the limit can still grow;
  // once it is at the most the sidecar returns, a note instead -- the rest
  // stays in the transcript.
  function earlierHtml(conv, canGrow, max, act) {
    if (!conv || !conv.earlier) return "";
    return canGrow
      ? '<div class="loadmore"><button type="button" class="ghost small" data-act="' + act + '">Load earlier</button></div>'
      : '<p class="rnote">The most recent ' + max + " shown; earlier is in the transcript.</p>";
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
    return earlierHtml(conv, (conv.limit || ROOM_LIMIT) < ROOM_MAX, ROOM_MAX, "a:load-earlier") +
      (entries.length ? entries.map((e, i) => entryHtml(e, i, name, a.kind)).join("") : '<p class="rnote">Nothing said yet.</p>');
  }

  function roomColumn(name) {
    return app.querySelector('.rcol[data-name="' + cssName(name) + '"]');
  }

  // Redraw one column's body, staying at the bottom if it was there.
  function renderRoomBody(a) {
    const col = roomColumn(a.name);
    const body = col && col.querySelector(".rbody");
    if (!body) return;
    const first = !body.hasAttribute("data-drawn");
    const atBottom = body.scrollHeight - body.scrollTop - body.clientHeight < 40;
    const keep = body.scrollTop;
    body.innerHTML = roomBodyHtml(a);
    drawn.set(body, (herdNow.convs[a.name] && herdNow.convs[a.name].entries) || []);
    body.setAttribute("data-drawn", "1");
    body.scrollTop = first || atBottom ? body.scrollHeight : keep;
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
        (entries.length ? entries.map((e, i) => entryHtml(e, i, sv.name, "claude")).join("")
          : '<p class="rnote">Nothing said yet.</p>');
    const atBottom = (() => { const b = ov.querySelector(".subbody"); return !b || b.scrollHeight - b.scrollTop - b.clientHeight < 40; })();
    ov.innerHTML = '<div class="subhead"><span class="subwho">' + esc(sv.who || "sub-agent") + "</span>" +
      (sv.title ? '<span class="subtitle" title="' + esc(sv.title) + '">' + esc(sv.title) + "</span>" : "") +
      '<span class="submeta">of ' + esc(sv.name) + "</span>" +
      '<button type="button" class="ghost small" data-act="a:sub-close">Close</button></div>' +
      '<div class="subbody">' + body + "</div>";
    const b = ov.querySelector(".subbody");
    drawn.set(b, entries);
    if (atBottom) b.scrollTop = b.scrollHeight;
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
    return '<div class="chat"><div class="cbody"></div><div class="cmeter"></div>' +
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
        plainBody(e.answer) + "</div>"
      : '<div class="eanswer waiting">waiting for ' + esc(e.to || "the answer") + "…</div>";
    return '<div class="ent cmsg' + (you ? " you" : "") + (long && !open ? " folded" : "") + '"' +
      (color ? ' style="border-left-color:' + color + '"' : "") + ">" +
      entHead(head, you ? "" : "", e.ts, color, copyButton(i, e.text)) + herdNotes(e) + body + outcome +
      (long ? '<button type="button" class="linkish cmore" data-act="a:chat-more" data-key="' + esc(key) + '">' + (open ? "less" : "more") + "</button>" : "") +
      "</div>";
  }

  // Redraw the timeline, staying at the bottom if it was there.
  function renderChatBody() {
    const body = app.querySelector(".chat .cbody");
    if (!body) return;
    const c = herdNow.chat;
    const first = !body.hasAttribute("data-drawn");
    const atBottom = body.scrollHeight - body.scrollTop - body.clientHeight < 40;
    const keep = body.scrollTop;
    const out = (c && [...(c.absent || []), ...(c.missing || [])]) || [];
    body.innerHTML = !c ? '<p class="rnote">Reading the conversations…</p>'
      : c.error ? '<p class="rnote error">Could not read the conversations: ' + esc(c.error) + "</p>"
      : earlierHtml(c, (herdNow.chatLimit || CHAT_LIMIT) < CHAT_MAX, CHAT_MAX, "a:chat-earlier") +
        ((c.entries || []).length ? c.entries.map(chatItemHtml).join("") : '<p class="rnote">Nobody has said anything yet.</p>') +
        (out.length ? '<p class="rnote">Not in the chat, no conversation known: ' + esc(out.join(", ")) + "</p>" : "");
    drawn.set(body, (c && c.entries) || []);
    body.setAttribute("data-drawn", "1");
    body.scrollTop = first || atBottom ? body.scrollHeight : keep;
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
    const chat = herdNow.roomView === "chat";
    const key = project + "\n" + herdNow.roomView + "\n" + agents.map((a) => a.name).join("\n");
    let room = app.querySelector(".room");
    if (!room || room.getAttribute("data-key") !== key) {
      const views = [["columns", "Columns"], ["chat", "Chat"]].map(([v, label]) =>
        '<button type="button" class="ghost small' + (herdNow.roomView === v ? " on" : "") + '" data-act="a:room-view" data-view="' + v + '">' +
        label + "</button>").join("");
      app.innerHTML = '<div class="room"><div class="rhead"><span class="pname" title="' + esc(project) + '">' + esc(leaf(project)) + "</span>" +
        '<span class="ppath">' + esc(project) + '</span><span class="rsum"></span><span class="rview">' + views + '</span><div class="rwork"></div>' +
        '<div class="rruns"></div></div>' +
        (!agents.length ? '<p class="empty">No agents here now.  Back (Esc) to the herd.</p>'
          : chat ? chatHtml(agents)
          : '<div class="rcols">' + agents.map((a) =>
              '<section class="rcol" data-name="' + esc(a.name) + '"><div class="rtop"></div><div class="rbody"></div><div class="rsubs"></div>' +
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
      col.className = "rcol s-" + a.state;
      col.querySelector(".rtop").innerHTML = roomTop(a, asksFor(a.name, asks));
      const body = col.querySelector(".rbody");
      if ((body.getAttribute("data-subs") || "") !== subsKey(a)) {
        body.setAttribute("data-subs", subsKey(a));
        if (body.hasAttribute("data-drawn")) renderRoomBody(a);
      }
      stripUnder(body, col.querySelector(".rsubs"), subsHtml(a));
      suggestIn(col, a.name, promptHint(a.name));
    });
  }

  function onAgentAction(act, el) {
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
    if (act === "room") return openRoom(project);
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
      const conv = herdNow.convs[name];
      const path = conv && conv.path;
      if (!sub || !path) return setStatus("Its transcript is not known yet");
      herdNow.subView = { name, sub, who: el.getAttribute("data-who"), title: el.getAttribute("data-title") };
      herdNow.subConv = null;
      emit("sub-watch", { name, path, sub });
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
      const cur = (conv && conv.limit) || ROOM_LIMIT;
      herdNow.roomLimit[name] = Math.min(ROOM_MAX, cur + ROOM_LIMIT);
      return watchRoom();
    }
    if (act === "chat-earlier") {
      herdNow.chatLimit = Math.min(CHAT_MAX, (herdNow.chatLimit || CHAT_LIMIT) + CHAT_LIMIT);
      return watchRoom();
    }
    if (act === "room-tools") {
      const key = el.getAttribute("data-key");
      if (herdNow.toolsOpen.has(key)) herdNow.toolsOpen.delete(key);
      else herdNow.toolsOpen.add(key);
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
    if (act === "new-agent") {
      herdNow.newFor = project;
      herdNow.newKind = herdNow.newName = "";
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
    if (ev.target.classList && ev.target.classList.contains("new-kind")) herdNow.newKind = ev.target.value;
  });

  app.addEventListener("input", (ev) => {
    const c = ev.target.classList;
    if (!c) return;
    if (c.contains("agent-text")) {
      const row = ev.target.closest(".arow, .rcol, .cfoot");
      if (row) herdNow.compose[row.getAttribute("data-name")] = ev.target.value;
    } else if (c.contains("new-name")) {
      herdNow.newName = ev.target.value;
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
    // A link would navigate the xwidget off the page and its bridge, so
    // Emacs opens it.  On another device it opens there, in a tab of its
    // own: the page stays.
    const a = t.closest("a[href]");
    if (a) {
      const href = a.getAttribute("href") || "";
      if (XW.elsewhere && /^https?:/i.test(href)) {
        a.target = "_blank";
        a.rel = "noopener noreferrer";
        return;
      }
      // Emacs's page for a pull request or task, in that app's tab.
      if (XW.elsewhere && /^\/open\//.test(href)) return;
      ev.preventDefault();
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
      if (m) copy(m.text);
    } else if (act === "copy-path") {
      if (src) copy(src.path);
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

  window.GM = {
    flash: (msg) => setStatus(msg),

    // The room's chat: its timeline, for the project it is of.
    setChat: (p) => {
      if (!p || !herdNow.room || p.project !== herdNow.room) return;
      herdNow.chat = p;
      if (herdNow.roomView === "chat") renderChatBody();
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

    setConversation: (p) => {
      if (!p || !p.name || !herdNow.room) return;
      herdNow.convs[p.name] = p;
      if (view !== "agents") return;
      const a = roomAgents().find((x) => x.name === p.name);
      if (!a) return;
      renderRoomBody(a);
      // Whether it has a conversation decides its Screen button -- in a
      // column: one that comes as the room turns to its chat has none.
      const col = roomColumn(a.name);
      if (col) col.querySelector(".rtop").innerHTML = roomTop(a, asksFor(a.name, (herdNow.data && herdNow.data.asks) || []));
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
  restore();
})();
