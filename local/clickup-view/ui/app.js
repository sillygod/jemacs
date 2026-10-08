(() => {
  "use strict";

  const PREFIX = "clickup:";
  const $ = (id) => document.getElementById(id);
  const app = $("app");
  const statusEl = $("status");
  const progressEl = $("progress");
  const crumbsEl = $("crumbs");
  const btnBack = $("btn-back");
  const btnRefresh = $("btn-refresh");
  const btnCopy = $("btn-copy");
  const btnBrowser = $("btn-browser");

  // ---- Intents ---------------------------------------------------------
  //
  // Emacs polls document.title and resets it once read.  A title set
  // before then would overwrite the unread one, so intents queue and go
  // out one per reset.  The counter keeps two identical intents (a second
  // Refresh) apart; seeding it with the clock keeps a reloaded page's
  // first intent apart from the last page's.

  const outbox = [];
  let seq = Date.now();
  let sentAt = 0;

  function pump() {
    if (!outbox.length) return;
    // Unread: wait, unless Emacs has evidently stopped reading it.
    if (document.title.startsWith(PREFIX) && Date.now() - sentAt < 2000) return;
    document.title = PREFIX + JSON.stringify(outbox.shift());
    sentAt = Date.now();
  }

  function emit(op, extra) {
    outbox.push(Object.assign({ op: op, n: ++seq }, extra || {}));
    pump();
  }

  setInterval(pump, 50);

  // ---- Small helpers ---------------------------------------------------

  function esc(s) {
    return String(s == null ? "" : s).replace(/[&<>"']/g, (c) => ({
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#39;",
    })[c]);
  }

  // Colours from ClickUp go into style attributes: hex only.
  function hex(c, fallback) {
    return /^#[0-9a-f]{3,8}$/i.test(String(c || "")) ? c : fallback || "#8b949e";
  }

  function stamp(ms) {
    const t = Number(ms);
    return ms && Number.isFinite(t) && t > 0 ? t : null;
  }

  function relTime(ms) {
    const t = stamp(ms);
    if (!t) return "";
    const s = Math.max(0, (Date.now() - t) / 1000);
    if (s < 60) return "just now";
    if (s < 3600) return Math.floor(s / 60) + "m ago";
    if (s < 86400) return Math.floor(s / 3600) + "h ago";
    if (s < 604800) return Math.floor(s / 86400) + "d ago";
    return day(ms);
  }

  function day(ms) {
    const t = stamp(ms);
    if (!t) return "";
    const d = new Date(t);
    const opts = { month: "short", day: "numeric" };
    if (d.getFullYear() !== new Date().getFullYear()) opts.year = "numeric";
    return d.toLocaleDateString("en-US", opts);
  }

  function fullTime(ms) {
    const t = stamp(ms);
    return t ? new Date(t).toLocaleString() : "";
  }

  function plural(n, one, many) {
    return n + " " + (n === 1 ? one : many);
  }

  function bytes(n) {
    if (!n) return "";
    if (n < 1024) return n + " B";
    if (n < 1048576) return Math.round(n / 1024) + " KB";
    return (n / 1048576).toFixed(1) + " MB";
  }

  function hue(name) {
    let h = 0;
    for (const c of String(name || "")) h = (h * 31 + c.charCodeAt(0)) >>> 0;
    return h % 360;
  }

  function avatar(p, cls) {
    const name = (p && p.name) || "?";
    const ini =
      (p && p.initials) ||
      name
        .split(/[\s._-]+/)
        .filter(Boolean)
        .slice(0, 2)
        .map((w) => w[0])
        .join("")
        .toUpperCase();
    const bg = p && /^#[0-9a-f]{3,8}$/i.test(p.color || "")
      ? "background:" + p.color
      : "--h:" + hue(name);
    const img = p && /^https?:/i.test(p.avatar || "")
      ? '<img src="' + esc(p.avatar) + '" alt="" loading="lazy">'
      : "";
    return (
      '<span class="avatar ' + (cls || "") + '" style="' + bg + '" title="' + esc(name) + '">' +
      img + "<span>" + esc(ini || "?") + "</span></span>"
    );
  }

  function isDone(st) {
    return !!st && (st.type === "closed" || st.type === "done");
  }

  // Muted in rows.  Not a done-type status such as "wait for testing":
  // the work is waiting, not over.
  function isClosed(st) {
    return !!st && st.type === "closed";
  }

  function dot(st) {
    return '<i class="dot" style="--c:' + hex(st && st.color) + '"></i>';
  }

  // A task link, CU-<id> or bare id → native id; null when it is none.
  // Mirrors clickup-view--ref-re: a custom id (/t/<team>/DEV-12) is not
  // one, and an id has a digit (clickup-view--id-re), so CU-ids is a word.
  const ID = "[0-9a-z]*[0-9][0-9a-z]*";
  const URL_ID = new RegExp("^https?://app\\.clickup\\.com/t/(?:\\d+/)?(" + ID + ")(?:[?#]|$)");
  const LINK_ID = new RegExp("app\\.clickup\\.com/t/(?:\\d+/)?(" + ID + ")(?:[^/0-9a-zA-Z-]|$)");
  const CU_ID = new RegExp("(?:^|[^0-9A-Za-z_])CU-(" + ID + ")(?:[^0-9A-Za-z_]|$)");
  const BARE_ID = new RegExp("^" + ID + "$");

  function taskIdFromUrl(href) {
    const m = URL_ID.exec(String(href || ""));
    return m ? m[1] : null;
  }

  function taskIdFrom(s) {
    s = String(s || "").trim().replace(/^#/, "");
    const link = LINK_ID.exec(s);
    if (link) return link[1];
    const cu = CU_ID.exec(s);
    if (cu) return cu[1];
    return BARE_ID.test(s) ? s : null;
  }

  // ---- Sanitizer -------------------------------------------------------
  //
  // The page can change statuses and post comments, so nothing from
  // ClickUp may run in it.  Markdown goes through marked, which keeps raw
  // HTML; comment HTML is escaped in Elisp but passes here too.  Parse
  // into an inert document (no scripts run, no images load), keep a
  // whitelist of tags and attributes, unwrap the rest.

  const KEEP = {
    A: ["href", "title", "class", "data-task"],
    IMG: ["src", "alt", "title", "class"],
    P: [], BR: [], HR: [], DIV: [], SPAN: ["class"],
    STRONG: [], B: [], EM: [], I: [], U: [], S: [], DEL: [], INS: [],
    MARK: [], SUB: [], SUP: [], SMALL: [], KBD: [],
    CODE: ["class"], PRE: [], BLOCKQUOTE: [],
    H1: [], H2: [], H3: [], H4: [], H5: [], H6: [],
    UL: ["class"], OL: ["start", "class"], LI: ["class", "data-check"],
    TABLE: [], THEAD: [], TBODY: [], TR: [],
    TH: ["align", "colspan", "rowspan"], TD: ["align", "colspan", "rowspan"],
    INPUT: ["type", "checked"],
    DETAILS: [], SUMMARY: [],
  };
  const DROP = new Set([
    "SCRIPT", "STYLE", "IFRAME", "FRAME", "FRAMESET", "OBJECT", "EMBED", "APPLET",
    "TEMPLATE", "NOSCRIPT", "SVG", "MATH", "FORM", "TEXTAREA", "SELECT", "OPTION",
    "BUTTON", "LINK", "META", "BASE", "TITLE", "HEAD", "AUDIO", "VIDEO", "SOURCE",
    "TRACK", "CANVAS", "PORTAL",
  ]);
  const ATTR_OK = {
    href: (v) => /^(https?:|mailto:)/i.test(v),
    src: (v) => /^(https?:|data:image\/(png|jpe?g|gif|webp);)/i.test(v),
    class: () => true,
    "data-task": (v) => /^[0-9a-z]+$/.test(v),
    "data-check": (v) => v === "done" || v === "todo",
    align: (v) => /^(left|right|center)$/.test(v),
    colspan: (v) => /^\d{1,3}$/.test(v),
    rowspan: (v) => /^\d{1,3}$/.test(v),
    start: (v) => /^\d{1,6}$/.test(v),
    type: (v) => v === "checkbox",
  };

  function cleanClass(v) {
    return v
      .split(/\s+/)
      .filter((c) => /^(cu-[a-z-]+|indent-\d|language-[\w+#-]+)$/.test(c))
      .join(" ");
  }

  function cleanNode(node, out) {
    if (node.nodeType === 3) {
      out.appendChild(document.createTextNode(node.data));
      return;
    }
    if (node.nodeType !== 1) return;
    const tag = node.tagName.toUpperCase();
    if (DROP.has(tag)) return;
    const keep = KEEP[tag];
    // Unwrap what is not kept, and a link whose target is not: an anchor
    // left without its href would still look clickable.
    if (
      !keep ||
      (tag === "INPUT" && node.getAttribute("type") !== "checkbox") ||
      (tag === "A" && !ATTR_OK.href((node.getAttribute("href") || "").trim()))
    ) {
      node.childNodes.forEach((k) => cleanNode(k, out));
      return;
    }
    const el = document.createElement(tag.toLowerCase());
    keep.forEach((name) => {
      if (!node.hasAttribute(name)) return;
      let v = node.getAttribute(name).trim();
      if (name === "class") v = cleanClass(v);
      if (name === "checked") v = "";
      else if (!v || !(ATTR_OK[name] || (() => true))(v)) return;
      el.setAttribute(name, v);
    });
    if (tag === "INPUT") el.setAttribute("disabled", "");
    if (tag === "IMG") el.setAttribute("loading", "lazy");
    node.childNodes.forEach((k) => cleanNode(k, el));
    out.appendChild(el);
  }

  function sanitize(html) {
    const doc = new DOMParser().parseFromString(
      "<!DOCTYPE html><body>" + String(html || "") + "</body>",
      "text/html"
    );
    const box = document.createElement("div");
    doc.body.childNodes.forEach((k) => cleanNode(k, box));
    return box.innerHTML;
  }

  // Blank lines a comment starts or ends with.
  function trimBlank(html) {
    return html.replace(/^(?:<p><br><\/p>)+|(?:<p><br><\/p>)+$/g, "");
  }

  function markdown(src) {
    if (!src || !String(src).trim()) return "";
    if (window.marked && typeof window.marked.parse === "function") {
      try {
        return sanitize(window.marked.parse(String(src), { gfm: true, breaks: true }));
      } catch (e) {
        /* fall through to plain text */
      }
    }
    return '<p class="plain">' + esc(src) + "</p>";
  }

  // ---- State -----------------------------------------------------------

  let ctx = { me: null, team: null, spaces: [] };
  let view = { kind: "boot" }; // { kind: home | space | list | task, id }
  const trail = []; // views behind this one, for Back
  let nav = null; // { spec, push } while a view is on its way
  let crumbs = []; // the breadcrumbs on screen: { label, spec }
  let space = null; // the space on screen, as renderSpace sent it
  let list = null; // the list on screen; see newList
  let listOpts = { closed: false, mine: false }; // carried from list to list
  const cache = { space: {}, list: {} }; // what each was when last shown, for Back
  // Pages of a list fetched without asking (ClickUp sends 100 tasks a
  // page, subtasks included), so status counts and the filter see all.
  const AUTO_PAGES = 5;
  let task = null;
  let statuses = null;
  let comments = { items: [], hasMore: false, loading: false, more: false };
  let threads = {}; // comment id → { open, items (null while loading) }
  const drafts = {}; // "c:<task>" / "r:<comment>" → text, kept across renders
  let posting = {}; // draft key → true while its post is out
  let settingStatus = null; // the status name on its way
  let menuOpen = false;
  let infoTimer = null;

  // ---- Status bar ------------------------------------------------------

  function setStatus(msg, kind) {
    clearTimeout(infoTimer);
    if (!msg) {
      statusEl.hidden = true;
      statusEl.textContent = "";
      return;
    }
    statusEl.hidden = false;
    statusEl.className = "status" + (kind ? " " + kind : "");
    statusEl.innerHTML =
      "<span>" + esc(msg) + "</span>" +
      (kind === "error" ? '<button type="button" class="x" title="Dismiss">×</button>' : "");
    if (kind !== "error") infoTimer = setTimeout(() => setStatus(""), 3500);
  }

  function clearInfo() {
    if (!statusEl.classList.contains("error")) setStatus("");
  }

  function progress(on) {
    progressEl.hidden = !on;
  }

  // ---- Navigation ------------------------------------------------------
  //
  // Going forward always loads fresh.  Back shows a space or list as it
  // was left, at the same scroll, since the page still has it; a task
  // loads again, its comments may have moved on.

  function sameView(a, b) {
    return a.kind === b.kind && String(a.id || "") === String(b.id || "");
  }

  // OPTS.push (default true) keeps the current view for Back,
  // OPTS.cached takes the page's copy of a space or list, OPTS.force
  // makes Emacs fetch a space again rather than send its own copy.
  function go(spec, opts) {
    opts = opts || {};
    const push = opts.push !== false;
    if (statusEl.classList.contains("error")) setStatus("");
    if (spec.kind === "home") {
      arrive(spec, push);
      renderHome();
      return;
    }
    const kept = opts.cached && cache[spec.kind] && cache[spec.kind][spec.id];
    if (kept) {
      arrive(spec, push);
      if (spec.kind === "space") showSpace(kept);
      else showList(kept);
      app.scrollTop = spec.scroll || 0;
      return;
    }
    nav = { spec: spec, push: push };
    progress(true);
    if (spec.kind === "task") emit("open-task", { id: spec.id });
    else if (spec.kind === "space") emit("open-space", { id: spec.id, force: !!opts.force });
    else if (spec.kind === "list")
      emit("open-list", { id: spec.id, page: 0, closed: listOpts.closed, mine: listOpts.mine });
  }

  function arrive(spec, push) {
    if (push && view.kind !== "boot" && !sameView(view, spec)) {
      view.scroll = app.scrollTop;
      trail.push(view);
    }
    view = spec;
    nav = null;
    progress(false);
  }

  function back() {
    const from = view;
    nav = null;
    progress(false);
    const prev = trail.pop();
    if (!prev) return;
    go(prev, { push: false, cached: true });
    // Back on a list: mark the row of the task just left.
    if (view === prev && from.kind === "task") {
      const row = app.querySelector('.row[data-id="' + CSS.escape(String(from.id)) + '"]');
      if (row) row.focus({ preventScroll: true });
    }
  }

  function refresh() {
    if (view.kind === "home") emit("refresh-spaces");
    else if (view.kind !== "boot") go(view, { push: false, force: true });
  }

  function viewUrl() {
    if (view.kind === "task" && task) return task.url || "";
    if (view.kind === "list" && list && ctx.team) return "https://app.clickup.com/" + ctx.team + "/v/li/" + list.id;
    return "";
  }

  // Up from the view, each crumb opens that space or list.  A folder has
  // no page of its own: it opens its space, scrolled to the folder.
  function crumbsFor() {
    const sp = (s) => (s && s.id ? { label: s.name || "Space", spec: { kind: "space", id: s.id } } : null);
    const fo = (f, s) =>
      f && f.id && s && s.id ? { label: f.name, spec: { kind: "space", id: s.id, focus: f.id } } : null;
    if (view.kind === "task" && task) {
      const l = task.list;
      return [sp(task.space), fo(task.folder, task.space), l && l.id ? { label: l.name, spec: { kind: "list", id: l.id } } : null];
    }
    if (view.kind === "list" && list) return [sp(list.space), fo(list.folder, list.space), { label: list.name }];
    if (view.kind === "space" && space) return [{ label: space.name || "Space" }];
    return [{ label: view.kind === "home" ? "All spaces" : "ClickUp" }];
  }

  function chrome() {
    app.setAttribute("data-view", view.kind);
    btnBack.hidden = !trail.length;
    const url = viewUrl();
    btnCopy.hidden = !url;
    btnBrowser.hidden = !url;
    crumbs = crumbsFor().filter(Boolean);
    crumbsEl.innerHTML = crumbs
      .map((c, i) =>
        c.spec
          ? '<button type="button" class="crumb" data-act="crumb" data-i="' + i + '">' + esc(c.label) + "</button>"
          : '<span class="crumb here">' + esc(c.label) + "</span>"
      )
      .join('<span class="sep">›</span>');
  }

  // ---- Home ------------------------------------------------------------

  function spaceMark(s) {
    const first = Array.from(String((s && s.name) || "?").trim())[0] || "?";
    return '<span class="sq" style="--c:' + hex(s && s.color) + '">' + esc(first.toUpperCase()) + "</span>";
  }

  function renderHome() {
    chrome();
    app.innerHTML =
      '<div class="page home">' +
      '<section class="block"><h2>Open a task</h2>' +
      '<div class="open-row">' +
      '<input id="open-id" type="text" spellcheck="false" autocomplete="off" ' +
      'placeholder="Task link, CU-id or id">' +
      '<button type="button" class="primary" data-act="open-typed">Open</button>' +
      "</div>" +
      '<p class="muted hint">From a branch, <code>M-x clickup-view</code> opens the task its ' +
      "commits reference.</p></section>" +
      '<section class="block"><h2>Spaces <span class="count" id="space-count"></span></h2>' +
      '<div id="spaces"></div></section>' +
      "</div>";
    renderSpaces();
    const input = $("open-id");
    if (input) input.focus();
  }

  function renderSpaces() {
    const box = $("spaces");
    if (!box) return;
    const spaces = ctx.spaces || [];
    $("space-count").textContent = spaces.length ? String(spaces.length) : "";
    box.innerHTML = spaces.length
      ? '<div class="rows">' +
        spaces
          .map(
            (s) =>
              '<div class="row" role="button" tabindex="0" data-act="open-space" data-id="' + esc(s.id) + '">' +
              spaceMark(s) + '<span class="row-name">' + esc(s.name) + '</span><span class="go">›</span></div>'
          )
          .join("") +
        "</div>"
      : '<p class="muted">' + (ctx.team ? "This workspace has no spaces." : "Not loaded yet: Refresh to try again.") + "</p>";
  }

  // ---- Space -----------------------------------------------------------

  function listRow(l) {
    return (
      '<div class="row' + (l.count ? "" : " quiet") + '" role="button" tabindex="0" data-act="open-list" data-id="' +
      esc(l.id) + '">' + '<span class="li-mark">≡</span><span class="row-name">' + esc(l.name) + "</span>" +
      '<span class="row-count" title="Open tasks">' + esc(l.count) + "</span></div>"
    );
  }

  function listRows(ls) {
    return '<div class="rows">' + ls.map(listRow).join("") + "</div>";
  }

  // Folderless lists first: they are a space's own, usually its main ones.
  function showSpace(p) {
    space = p;
    chrome();
    const lists = p.lists || [];
    let html = lists.length ? section("Lists", String(lists.length), listRows(lists)) : "";
    (p.folders || []).forEach((f) => {
      const ls = f.lists || [];
      html += section(
        f.name,
        String(ls.length),
        ls.length ? listRows(ls) : '<p class="muted">No lists.</p>',
        ' data-folder="' + esc(f.id) + '"'
      );
    });
    app.innerHTML =
      '<div class="page">' +
      '<div class="page-head"><h1>' + spaceMark(p) + "<span>" + esc(p.name || "Space") + "</span></h1></div>" +
      (html || '<p class="muted">No lists in this space.</p>') +
      "</div>";
  }

  // ---- List ------------------------------------------------------------
  //
  // Tasks grouped by status in the list's own order, newest first as
  // ClickUp sends them.  Subtasks fold under their parent (their status
  // often differs from it); one whose parent is not here, under Mine, stands
  // alone, marked ↳.  The filter shows every match flat.

  function newList(p, was) {
    return {
      id: String(p.id),
      name: p.name || "",
      space: p.space || null,
      folder: p.folder || null,
      statuses: p.statuses || [],
      closed: !!p.closed,
      mine: !!p.mine,
      tasks: p.tasks || [],
      page: 0,
      lastPage: false,
      loading: null, // the page on its way
      // What the reader set here outlives a reload of the list.
      open: was ? was.open : new Set(), // parents showing their subtasks
      collapsed: was ? was.collapsed : new Set(), // status groups folded
      filter: was ? was.filter : "",
    };
  }

  function flagButton(key, label, title) {
    const on = listOpts[key];
    return (
      '<button type="button" class="toggle' + (on ? " on" : "") + '" data-act="list-flag" data-flag="' + key +
      '" aria-pressed="' + on + '" title="' + esc(title) + '">' + esc(label) + "</button>"
    );
  }

  function showList(l) {
    list = l;
    listOpts = { closed: l.closed, mine: l.mine };
    chrome();
    const a = document.activeElement;
    const flag = a && a.getAttribute && a.getAttribute("data-flag");
    app.innerHTML =
      '<div class="page">' +
      '<div class="page-head"><h1>' + esc(l.name || "List") + "</h1>" +
      '<div class="toolbar">' +
      '<input id="list-filter" type="text" spellcheck="false" autocomplete="off" ' +
      'placeholder="Filter: name or id  ( / )" value="' + esc(l.filter) + '">' +
      flagButton("mine", "Mine", "Only tasks assigned to you") +
      flagButton("closed", "Closed", "Include closed tasks") +
      "</div></div>" +
      '<p class="muted sum" id="list-sum"></p>' +
      '<div id="list-body"></div></div>';
    renderListBody();
    if (flag) {
      const b = app.querySelector('[data-flag="' + flag + '"]');
      if (b) b.focus();
    }
  }

  function taskRow(t, kids, depth) {
    const id = String(t.id);
    const subs = (kids && kids[id]) || [];
    const open = subs.length > 0 && list.open.has(id);
    const due = stamp(t.due);
    const late = due && due < Date.now() && !isDone(t.status);
    let html =
      '<div class="row' + (isClosed(t.status) ? " closed" : "") + '" role="button" tabindex="0" data-act="open-task" ' +
      'data-id="' + esc(id) + '" style="--d:' + depth + '"' +
      (subs.length ? ' data-subs="' + (open ? "open" : "shut") + '"' : "") + ">" +
      dot(t.status) +
      '<span class="row-main">' +
      (t.parent && !depth ? '<span class="sub-mark" title="Subtask">↳</span>' : "") +
      '<span class="row-name">' + esc(t.name) + "</span>" +
      (subs.length
        ? '<button type="button" class="subs" data-act="subs" data-id="' + esc(id) + '" aria-expanded="' + open +
          '" title="Subtasks (→ / ←)"><i class="chev"></i>' + subs.length + "</button>"
        : "") +
      "</span>" +
      (depth ? '<span class="row-status">' + esc(t.status && t.status.name) + "</span>" : "") +
      (t.priority && t.priority.name
        ? '<span class="row-prio" style="--c:' + hex(t.priority.color) + '" title="' + esc(t.priority.name) +
          ' priority">⚑</span>'
        : "") +
      (due ? '<span class="row-due' + (late ? " late" : "") + '" title="Due ' + esc(fullTime(due)) + '">' + esc(day(due)) + "</span>" : "") +
      '<span class="stack">' + (t.assignees || []).slice(0, 3).map((p) => avatar(p, "xs")).join("") + "</span>" +
      "</div>";
    if (open) subs.forEach((k) => (html += taskRow(k, kids, depth + 1)));
    return html;
  }

  function matcher(raw) {
    const q = raw.trim().toLowerCase();
    if (!q) return null;
    const id = taskIdFrom(raw);
    return (t) => String(t.name || "").toLowerCase().includes(q) || (!!id && String(t.id) === id);
  }

  function renderListBody() {
    const box = $("list-body");
    if (!box || !list) return;
    const l = list;
    const match = matcher(l.filter);
    const shown = match ? l.tasks.filter(match) : l.tasks;
    const here = new Set(l.tasks.map((t) => String(t.id)));
    const kids = {};
    const tops = [];
    shown.forEach((t) => {
      const p = t.parent != null ? String(t.parent) : "";
      if (!match && p && here.has(p)) (kids[p] = kids[p] || []).push(t);
      else tops.push(t);
    });
    const key = (name) => String(name || "").toLowerCase();
    const groups = new Map();
    l.statuses.forEach((s) => groups.set(key(s.name), { st: s, rows: [] }));
    tops.forEach((t) => {
      const k = key(t.status && t.status.name);
      if (!groups.has(k)) groups.set(k, { st: t.status || {}, rows: [] });
      groups.get(k).rows.push(t);
    });
    let html = "";
    groups.forEach((g, k) => {
      if (!g.rows.length) return;
      const shut = !match && l.collapsed.has(k);
      html +=
        '<section class="group">' +
        '<button type="button" class="group-head" data-act="group" data-status="' + esc(k) + '" aria-expanded="' + !shut + '">' +
        '<i class="chev"></i>' + dot(g.st) +
        '<span class="g-name">' + esc(g.st.name) + '</span><span class="count">' + g.rows.length + "</span></button>" +
        (shut ? "" : '<div class="rows">' + g.rows.map((t) => taskRow(t, kids, 0)).join("") + "</div>") +
        "</section>";
    });
    if (!html) {
      html =
        '<p class="muted empty-note">' +
        (match
          ? "No task matches “" + esc(l.filter.trim()) + "”."
          : l.mine
            ? "Nothing here is assigned to you."
            : l.closed
              ? "This list has no tasks."
              : "No open tasks.") +
        "</p>";
    }
    if (l.loading != null) html += '<p class="muted foot">Loading more…</p>';
    else if (!l.lastPage) html += '<button type="button" class="ghost more" data-act="more-tasks">Load more</button>';
    box.innerHTML = html;
    const subs = l.tasks.filter((t) => t.parent).length;
    $("list-sum").textContent = match
      ? shown.length + " of " + l.tasks.length + " match"
      : plural(l.tasks.length - subs, "task", "tasks") +
        (subs ? " · " + plural(subs, "subtask", "subtasks") : "") +
        (l.lastPage ? "" : " so far");
  }

  function loadPage(n) {
    list.loading = n;
    emit("open-list", { id: list.id, page: n, closed: list.closed, mine: list.mine });
  }

  function toggleSubs(id) {
    if (!list || !id) return;
    if (list.open.has(id)) list.open.delete(id);
    else list.open.add(id);
    renderListBody();
    const row = app.querySelector('.row[data-id="' + CSS.escape(id) + '"]');
    if (row) row.focus({ preventScroll: true });
  }

  function toggleGroup(k) {
    if (!list || list.filter.trim()) return;
    if (list.collapsed.has(k)) list.collapsed.delete(k);
    else list.collapsed.add(k);
    renderListBody();
    const head = app.querySelector('.group-head[data-status="' + CSS.escape(k) + '"]');
    if (head) head.focus({ preventScroll: true });
  }

  function toggleFlag(key) {
    if (view.kind !== "list" || !list || !(key in listOpts)) return;
    listOpts = Object.assign({}, listOpts);
    listOpts[key] = !listOpts[key];
    const b = app.querySelector('[data-flag="' + key + '"]');
    if (b) {
      b.classList.toggle("on", listOpts[key]);
      b.setAttribute("aria-pressed", String(listOpts[key]));
    }
    go(view, { push: false });
  }

  function openTyped() {
    const input = $("open-id");
    const raw = input ? input.value : "";
    const id = taskIdFrom(raw);
    if (!id) {
      setStatus("Not a ClickUp task: " + raw.trim(), "error");
      return;
    }
    go({ kind: "task", id: id });
  }

  // ---- Task ------------------------------------------------------------

  function statusControl() {
    const st = (task && task.status) || {};
    const label = settingStatus ? settingStatus + "…" : st.name || "—";
    let menu = "";
    if (menuOpen) {
      const items = statuses
        ? statuses
            .map((s) => {
              const on = s.name === st.name;
              return (
                '<button type="button" class="menu-item' + (on ? " on" : "") +
                '" data-act="set-status" data-status="' + esc(s.name) + '">' +
                dot(s) + '<span class="mi-name">' + esc(s.name) + "</span>" +
                (on ? '<span class="check">✓</span>' : "") + "</button>"
              );
            })
            .join("")
        : '<p class="muted menu-note">Loading statuses…</p>';
      menu = '<div class="menu" role="menu">' + items + "</div>";
    }
    return (
      '<div class="status-ctl">' +
      '<button type="button" class="status-btn' + (isDone(st) ? " done" : "") +
      '" data-act="status-menu" style="--c:' + hex(st.color) + '"' +
      (settingStatus ? " disabled" : "") + ' title="Change status (s)">' +
      '<i class="dot"></i><span>' + esc(label) + '</span><i class="chev down"></i></button>' +
      menu + "</div>"
    );
  }

  function renderStatusSlot() {
    const slot = $("status-slot");
    if (slot) slot.innerHTML = statusControl();
    if (menuOpen) {
      const first = app.querySelector(".menu-item.on") || app.querySelector(".menu-item");
      if (first) first.focus();
    }
  }

  // REFOCUS returns focus to the status button, for keyboard closes; a
  // click elsewhere has already put focus where it belongs.
  function openMenu(on, refocus) {
    if (menuOpen === on) return;
    menuOpen = on;
    renderStatusSlot();
    if (!on && refocus) {
      const btn = app.querySelector(".status-btn");
      if (btn) btn.focus();
    }
  }

  function setStatusTo(name) {
    menuOpen = false;
    if (!task || name === (task.status && task.status.name)) {
      renderStatusSlot();
      return;
    }
    settingStatus = name;
    renderStatusSlot();
    emit("set-status", { task_id: task.id, status: name });
  }

  function people(list) {
    if (!list || !list.length) return '<span class="muted">Unassigned</span>';
    return list
      .map((p) => '<span class="person">' + avatar(p, "sm") + esc(p.name) + "</span>")
      .join("");
  }

  function priorityHtml(p) {
    if (!p || !p.name) return '<span class="muted">—</span>';
    return (
      '<span class="prio" style="--c:' + hex(p.color) + '"><i class="flag">⚑</i>' +
      esc(p.name.charAt(0).toUpperCase() + p.name.slice(1)) + "</span>"
    );
  }

  function datesHtml(t) {
    const parts = [];
    if (stamp(t.start)) parts.push('<span title="' + esc(fullTime(t.start)) + '">Start ' + esc(day(t.start)) + "</span>");
    if (stamp(t.due)) {
      const late = stamp(t.due) < Date.now() && !isDone(t.status);
      parts.push(
        '<span class="' + (late ? "late" : "") + '" title="' + esc(fullTime(t.due)) + '">Due ' +
        esc(day(t.due)) + (late ? " · overdue" : "") + "</span>"
      );
    }
    return parts.length ? parts.join('<span class="sep">→</span>') : '<span class="muted">—</span>';
  }

  // ClickUp often sends a tag's fg equal to its bg; tint from bg alone.
  function tagsHtml(tags) {
    return (tags || [])
      .map((g) => '<span class="tag" style="--c:' + hex(g.bg || g.fg) + '">' + esc(g.name) + "</span>")
      .join("");
  }

  function prop(label, html) {
    return '<div class="k">' + label + '</div><div class="v">' + html + "</div>";
  }

  function section(title, count, body, attrs) {
    return (
      '<section class="block"' + (attrs || "") + "><h2>" + esc(title) +
      (count ? ' <span class="count">' + esc(count) + "</span>" : "") +
      "</h2>" + body + "</section>"
    );
  }

  function subtasksHtml(t) {
    const subs = t.subtasks || [];
    if (!subs.length) return "";
    const ids = new Set(subs.map((s) => s.id));
    const kids = {};
    subs.forEach((s) => {
      const p = ids.has(s.parent) ? s.parent : t.id;
      (kids[p] = kids[p] || []).push(s);
    });
    const rows = [];
    const walk = (pid, depth) =>
      (kids[pid] || []).forEach((s) => {
        rows.push(
          '<div class="row' + (isClosed(s.status) ? " closed" : "") + '" role="button" tabindex="0" ' +
          'data-act="open-task" data-id="' + esc(s.id) + '" style="--d:' + depth + '">' +
          dot(s.status) + '<span class="row-name">' + esc(s.name) + "</span>" +
          '<span class="row-status">' + esc(s.status && s.status.name) + "</span>" +
          (stamp(s.due) ? '<span class="row-due">' + esc(day(s.due)) + "</span>" : "") +
          '<span class="stack">' + (s.assignees || []).slice(0, 3).map((p) => avatar(p, "xs")).join("") + "</span>" +
          "</div>"
        );
        if (depth < 8) walk(s.id, depth + 1);
      });
    walk(t.id, 0);
    const done = subs.filter((s) => isDone(s.status)).length;
    return section("Subtasks", done + "/" + subs.length, '<div class="rows">' + rows.join("") + "</div>");
  }

  function checkItems(items) {
    return (
      "<ul>" +
      (items || [])
        .map((it) =>
          '<li class="' + (it.resolved ? "done" : "") + '"><span class="box">' +
          (it.resolved ? "✓" : "") + "</span><span>" + esc(it.name) + "</span>" +
          (it.children && it.children.length ? checkItems(it.children) : "") + "</li>"
        )
        .join("") +
      "</ul>"
    );
  }

  function countItems(items) {
    let all = 0;
    let done = 0;
    (items || []).forEach((it) => {
      all++;
      if (it.resolved) done++;
      const sub = countItems(it.children);
      all += sub[0];
      done += sub[1];
    });
    return [all, done];
  }

  function checklistsHtml(t) {
    return (t.checklists || [])
      .map((cl) => {
        const n = countItems(cl.items);
        return section(cl.name || "Checklist", n[1] + "/" + n[0], '<div class="checklist">' + checkItems(cl.items) + "</div>");
      })
      .join("");
  }

  function attachmentsHtml(t) {
    const atts = t.attachments || [];
    if (!atts.length) return "";
    return section(
      "Attachments",
      String(atts.length),
      '<div class="atts">' +
        atts
          .map((a) => {
            const thumb = /^https?:/i.test(a.thumb || "")
              ? '<img src="' + esc(a.thumb) + '" alt="" loading="lazy">'
              : '<span class="ext">' + esc((a.ext || "file").toUpperCase()) + "</span>";
            return (
              '<button type="button" class="att" data-act="open-url" data-url="' + esc(a.url) +
              '" title="' + esc(a.title) + '">' + '<span class="thumb">' + thumb + "</span>" +
              '<span class="att-name">' + esc(a.title) + "</span>" +
              '<span class="att-size">' + esc(bytes(a.size)) + "</span></button>"
            );
          })
          .join("") +
        "</div>"
    );
  }

  function renderTask() {
    const t = task;
    chrome();
    const desc = markdown(t.description);
    const ids =
      '<span class="tid">#' + esc(t.id) + "</span>" +
      (t.custom_id ? '<span class="tid">' + esc(t.custom_id) + "</span>" : "") +
      (t.parent
        ? '<button type="button" class="linkish" data-act="open-task" data-id="' + esc(t.parent) +
          '">↑ Parent task</button>'
        : "");
    const tags = tagsHtml(t.tags);
    const created = t.creator
      ? esc(t.creator.name) + ' · <span title="' + esc(fullTime(t.created)) + '">' + esc(relTime(t.created)) + "</span>"
      : esc(relTime(t.created));
    app.innerHTML =
      '<div class="task-grid">' +
      '<section class="task-main">' +
      '<div class="task-head">' +
      '<div class="ids">' + ids + "</div>" +
      "<h1>" + esc(t.name) + "</h1>" +
      '<div class="props">' +
      '<div class="k">Status</div><div class="v" id="status-slot">' + statusControl() + "</div>" +
      prop("Assignees", people(t.assignees)) +
      prop("Priority", priorityHtml(t.priority)) +
      prop("Dates", datesHtml(t)) +
      (tags ? prop("Tags", tags) : "") +
      prop("Created", created) +
      prop("Updated", '<span title="' + esc(fullTime(t.updated)) + '">' + esc(relTime(t.updated)) + "</span>") +
      "</div></div>" +
      section("Description", "", desc ? '<div class="md desc">' + desc + "</div>" : '<p class="muted">No description.</p>') +
      subtasksHtml(t) +
      checklistsHtml(t) +
      attachmentsHtml(t) +
      "</section>" +
      '<aside class="task-side">' +
      '<h2>Comments <span class="count" id="comment-count"></span></h2>' +
      composer("c:" + t.id, "post-comment", "Comment", "Comment (markdown) · ⌘↩ to send", 3) +
      '<div id="comments"></div>' +
      "</aside></div>";
    renderComments();
  }

  function composer(key, act, label, placeholder, rows, extra) {
    const busy = !!posting[key];
    return (
      '<div class="composer">' +
      '<textarea rows="' + rows + '" data-draft="' + esc(key) + '" placeholder="' + esc(placeholder) + '">' +
      esc(drafts[key] || "") + "</textarea>" +
      '<div class="composer-foot"><span class="muted hint">Markdown: **bold**, `code`, lists, tables</span>' +
      '<button type="button" class="primary" data-act="' + act + '" data-post="' + esc(key) +
      '" data-label="' + esc(label) + '"' + (extra || "") + (busy ? " disabled" : "") + ">" +
      (busy ? "Posting…" : esc(label)) + "</button></div></div>"
    );
  }

  function syncPosting() {
    app.querySelectorAll("[data-post]").forEach((b) => {
      const on = !!posting[b.getAttribute("data-post")];
      b.disabled = on;
      b.textContent = on ? "Posting…" : b.getAttribute("data-label");
    });
  }

  // ---- Comments --------------------------------------------------------

  function commentHtml(c, isReply) {
    const id = String(c.id);
    const th = threads[id];
    const n = c.replies || 0;
    let thread = "";
    if (!isReply && th && th.open) {
      thread =
        '<div class="thread">' +
        (th.items === null
          ? '<p class="muted">Loading replies…</p>'
          : th.items.map((r) => commentHtml(r, true)).join("")) +
        composer("r:" + id, "post-reply", "Reply", "Reply (markdown) · ⌘↩ to send", 2, ' data-id="' + esc(id) + '"') +
        "</div>";
    }
    return (
      '<article class="comment' + (isReply ? " reply" : "") + '" data-id="' + esc(id) + '">' +
      avatar(c.author, isReply ? "sm" : "") +
      '<div class="c-body">' +
      '<div class="c-meta"><strong>' + esc((c.author && c.author.name) || "unknown") + "</strong>" +
      '<span title="' + esc(fullTime(c.date)) + '">' + esc(relTime(c.date)) + "</span></div>" +
      '<div class="md c-html">' + trimBlank(sanitize(c.html)) + "</div>" +
      (isReply
        ? ""
        : '<div class="c-actions"><button type="button" class="linkish" data-act="thread" data-id="' + esc(id) + '">' +
          (n ? plural(n, "reply", "replies") : "Reply") + (th && th.open ? " ▴" : "") + "</button></div>") +
      thread + "</div></article>"
    );
  }

  // Re-render the comment list without losing the reply being typed.
  function renderComments() {
    const box = $("comments");
    if (!box) return;
    const a = document.activeElement;
    const key = a && a.getAttribute && a.getAttribute("data-draft");
    const sel = key ? [a.selectionStart, a.selectionEnd] : null;
    let html;
    if (comments.loading && !comments.items.length) {
      html = '<p class="muted">Loading comments…</p>';
    } else if (!comments.items.length) {
      html = '<p class="muted">No comments yet.</p>';
    } else {
      html = comments.items.map((c) => commentHtml(c, false)).join("");
      if (comments.hasMore) {
        html +=
          '<button type="button" class="ghost more" data-act="more-comments"' +
          (comments.more ? " disabled" : "") + ">" +
          (comments.more ? "Loading…" : "Load older comments") + "</button>";
      }
    }
    box.innerHTML = html;
    const count = $("comment-count");
    if (count) count.textContent = comments.items.length ? comments.items.length + (comments.hasMore ? "+" : "") : "";
    if (key) {
      const el = app.querySelector('textarea[data-draft="' + CSS.escape(key) + '"]');
      if (el && el !== document.activeElement) {
        el.focus();
        el.setSelectionRange(sel[0], sel[1]);
      }
    }
  }

  function toggleThread(id) {
    const th = threads[id];
    if (th && th.open) {
      th.open = false;
    } else if (th && th.items) {
      th.open = true;
    } else {
      threads[id] = { open: true, items: null };
      emit("load-replies", { comment_id: id });
    }
    renderComments();
    if (threads[id].open) {
      const el = app.querySelector('textarea[data-draft="r:' + CSS.escape(id) + '"]');
      if (el) el.focus();
    }
  }

  function post(key) {
    if (!task || posting[key]) return;
    const text = String(drafts[key] || "").replace(/\s+$/, "");
    if (!text.trim()) return;
    posting[key] = true;
    syncPosting();
    if (key.startsWith("c:")) {
      emit("post-comment", { task_id: task.id, text: text });
    } else {
      emit("post-reply", { task_id: task.id, comment_id: key.slice(2), text: text });
    }
  }

  function moreComments() {
    const last = comments.items[comments.items.length - 1];
    if (!task || !last || comments.more) return;
    comments.more = true;
    renderComments();
    emit("more-comments", { task_id: task.id, start: last.date, start_id: last.id });
  }

  // ---- Actions ---------------------------------------------------------

  function act(name, el) {
    const id = el.getAttribute("data-id");
    switch (name) {
      case "home":
        go({ kind: "home" });
        break;
      case "crumb": {
        const c = crumbs[Number(el.getAttribute("data-i"))];
        if (c && c.spec) go(c.spec);
        break;
      }
      case "open-typed":
        openTyped();
        break;
      case "open-space":
        if (id) go({ kind: "space", id: id });
        break;
      case "open-list":
        if (id) go({ kind: "list", id: id });
        break;
      case "open-task":
        if (id) go({ kind: "task", id: id });
        break;
      case "group":
        toggleGroup(el.getAttribute("data-status"));
        break;
      case "subs":
        toggleSubs(id);
        break;
      case "list-flag":
        toggleFlag(el.getAttribute("data-flag"));
        break;
      case "more-tasks":
        if (list && list.loading == null && !list.lastPage) {
          loadPage(list.page + 1);
          renderListBody();
        }
        break;
      case "open-url":
        emit("open-browser", { url: el.getAttribute("data-url") || "" });
        break;
      case "status-menu":
        openMenu(!menuOpen);
        break;
      case "set-status":
        setStatusTo(el.getAttribute("data-status"));
        break;
      case "thread":
        toggleThread(id);
        break;
      case "post-comment":
      case "post-reply":
        post(el.getAttribute("data-post"));
        break;
      case "more-comments":
        moreComments();
        break;
    }
  }

  function followLink(a) {
    const href = a.getAttribute("href") || "";
    const id = a.getAttribute("data-task") || taskIdFromUrl(href);
    if (id) go({ kind: "task", id: id });
    else if (/^(https?:|mailto:)/i.test(href)) emit("open-browser", { url: href });
  }

  // Every link is handled here: left alone, one would navigate the
  // xwidget away from the page, and with it the bridge.
  document.addEventListener(
    "click",
    (ev) => {
      const t = ev.target;
      if (!(t instanceof Element)) return;
      if (menuOpen && !t.closest(".status-ctl")) openMenu(false);
      const a = t.closest("a");
      if (a) {
        ev.preventDefault();
        followLink(a);
        return;
      }
      const img = t.closest(".md img");
      if (img) {
        emit("open-browser", { url: img.getAttribute("src") || "" });
        return;
      }
      const btn = t.closest("[data-act]");
      if (btn && !btn.disabled) act(btn.getAttribute("data-act"), btn);
    },
    true
  );
  document.addEventListener("auxclick", (ev) => {
    if (ev.target instanceof Element && ev.target.closest("a")) ev.preventDefault();
  });
  // A file dropped on the page would replace it.
  ["dragover", "drop"].forEach((type) =>
    window.addEventListener(type, (ev) => {
      const types = ev.dataTransfer ? Array.from(ev.dataTransfer.types || []) : [];
      if (types.indexOf("Files") !== -1) ev.preventDefault();
    })
  );
  // Avatars fall back to initials when the picture will not load.
  document.addEventListener(
    "error",
    (ev) => {
      const t = ev.target;
      if (t && t.tagName === "IMG" && t.parentElement && t.parentElement.classList.contains("avatar")) t.remove();
    },
    true
  );

  app.addEventListener("input", (ev) => {
    const t = ev.target;
    const key = t.getAttribute && t.getAttribute("data-draft");
    if (key) drafts[key] = t.value;
    else if (t.id === "list-filter" && list) {
      list.filter = t.value;
      renderListBody();
    }
  });

  statusEl.addEventListener("click", (ev) => {
    if (ev.target.closest(".x")) setStatus("");
  });

  btnBack.addEventListener("click", back);
  btnRefresh.addEventListener("click", refresh);
  btnCopy.addEventListener("click", () => viewUrl() && emit("copy-url", { url: viewUrl() }));
  btnBrowser.addEventListener("click", () => viewUrl() && emit("open-browser", { url: viewUrl() }));

  // Rows and status groups, in screen order, for the arrow keys.
  function stops() {
    return Array.from(app.querySelectorAll(".row, .group-head"));
  }

  document.addEventListener("keydown", (ev) => {
    const t = ev.target;
    const typing = t && /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName);
    if (typing) {
      if (ev.key === "Enter" && (ev.metaKey || ev.ctrlKey) && t.getAttribute("data-draft")) {
        ev.preventDefault();
        post(t.getAttribute("data-draft"));
      } else if (ev.key === "Enter" && t.id === "open-id") {
        ev.preventDefault();
        openTyped();
      } else if (ev.key === "ArrowDown" && (t.id === "open-id" || t.id === "list-filter")) {
        const first = stops()[0];
        if (first) {
          ev.preventDefault();
          first.focus();
        }
      } else if (ev.key === "Escape") {
        if (t.id === "list-filter" && t.value && list) {
          t.value = list.filter = "";
          renderListBody();
        } else {
          t.blur();
        }
      }
      return;
    }
    if (menuOpen) {
      const items = Array.from(app.querySelectorAll(".menu-item"));
      const i = items.indexOf(document.activeElement);
      if (ev.key === "Escape") {
        ev.preventDefault();
        openMenu(false, true);
      } else if (ev.key === "ArrowDown" || ev.key === "ArrowUp") {
        ev.preventDefault();
        const step = ev.key === "ArrowDown" ? 1 : -1;
        const next = items[(i + step + items.length) % items.length];
        if (next) next.focus();
      }
      return;
    }
    if (ev.metaKey || ev.ctrlKey || ev.altKey) return;
    const on = t && t.matches && t.matches(".row, .group-head") ? t : null;
    if (on && (ev.key === "ArrowDown" || ev.key === "ArrowUp")) {
      ev.preventDefault();
      const all = stops();
      const i = all.indexOf(on) + (ev.key === "ArrowDown" ? 1 : -1);
      if (i >= 0 && i < all.length) all[i].focus();
      else if (i < 0) {
        const input = $("list-filter") || $("open-id");
        if (input) input.focus();
      }
    } else if (on && on.hasAttribute("data-subs") && (ev.key === "ArrowRight" || ev.key === "ArrowLeft")) {
      ev.preventDefault();
      if ((on.getAttribute("data-subs") === "open") !== (ev.key === "ArrowRight")) toggleSubs(on.getAttribute("data-id"));
    } else if (ev.key === "Enter" && t && t.getAttribute && t.getAttribute("role") === "button") {
      ev.preventDefault();
      act(t.getAttribute("data-act"), t);
    } else if (ev.key === "Escape") {
      back();
    } else if (ev.key === "r") {
      refresh();
    } else if (ev.key === "s" && view.kind === "task" && task) {
      ev.preventDefault();
      openMenu(true);
    } else if (ev.key === "/" && $("list-filter")) {
      ev.preventDefault();
      $("list-filter").focus();
    }
  });

  // ---- Calls from Emacs ------------------------------------------------
  //
  // Every payload names the space, list, task or comment it is for.  One
  // for anything but what is on screen or on its way (a slow reply for
  // the view just left) is dropped.

  function accept(kind, id) {
    if (nav) {
      if (nav.spec.kind !== kind || String(nav.spec.id) !== String(id)) return false;
      arrive(nav.spec, nav.push);
      return true;
    }
    // A task asked for before the page was ready, or the view on screen.
    if (kind === "task" && view.kind === "boot") {
      arrive({ kind: kind, id: id }, false);
      return true;
    }
    return view.kind === kind && String(view.id) === String(id);
  }

  function onTask(id) {
    return view.kind === "task" && task && String(task.id) === String(id);
  }

  window.CU = {
    flash: (msg) => setStatus(msg),

    showError: (msg) => {
      progress(false);
      posting = {};
      settingStatus = null;
      // A page that failed can be asked for again: Load more.
      if (list && list.loading != null) {
        list.loading = null;
        if (view.kind === "list") renderListBody();
      }
      if (view.kind === "boot") {
        arrive({ kind: "home" }, false);
        renderHome();
      } else {
        syncPosting();
        renderStatusSlot();
        if (comments.more) {
          comments.more = false;
          renderComments();
        }
      }
      setStatus(msg, "error");
    },

    setContext: (p) => {
      ctx = p || ctx;
      if (view.kind === "home") renderSpaces();
      if (view.kind !== "boot") chrome();
    },

    showHome: () => {
      clearInfo();
      arrive({ kind: "home" }, false);
      renderHome();
    },

    renderSpace: (p) => {
      if (!p) return;
      const moved = !sameView(view, { kind: "space", id: p.id });
      if (!accept("space", p.id)) return;
      cache.space[String(p.id)] = p;
      clearInfo();
      showSpace(p);
      // From a folder crumb: show the folder, ready on its first list.
      const folder = view.focus && app.querySelector('[data-folder="' + CSS.escape(String(view.focus)) + '"]');
      if (folder) {
        folder.scrollIntoView({ block: "start" });
        const first = folder.querySelector(".row");
        if (first) first.focus({ preventScroll: true });
      } else if (moved) app.scrollTop = 0;
    },

    // Page 0 opens the list (or reloads it); later pages, fetched by the
    // page itself, add to the list they were asked for, on screen or not.
    renderList: (p) => {
      if (!p) return;
      const page = Number(p.page) || 0;
      const id = String(p.id);
      const closed = !!p.closed;
      const mine = !!p.mine;
      if (page > 0) {
        const l = cache.list[id];
        if (!l || l.loading !== page || l.closed !== closed || l.mine !== mine) return;
        const have = new Set(l.tasks.map((t) => String(t.id)));
        l.tasks = l.tasks.concat((p.tasks || []).filter((t) => !have.has(String(t.id))));
        l.page = page;
        l.lastPage = !!p.last_page;
        l.loading = null;
        const shown = view.kind === "list" && list === l && listOpts.closed === closed && listOpts.mine === mine;
        if (shown && !l.lastPage && page + 1 < AUTO_PAGES) loadPage(page + 1);
        if (shown) renderListBody();
        return;
      }
      if (closed !== listOpts.closed || mine !== listOpts.mine) return; // a toggle since
      const moved = !sameView(view, { kind: "list", id: id });
      if (!accept("list", id)) return;
      const l = newList(p, cache.list[id]);
      l.lastPage = !!p.last_page;
      cache.list[id] = l;
      clearInfo();
      if (!l.lastPage && AUTO_PAGES > 1) l.loading = 1;
      showList(l);
      if (moved) app.scrollTop = 0;
      if (l.loading) emit("open-list", { id: id, page: 1, closed: closed, mine: mine });
    },

    renderTask: (p) => {
      const t = p && p.task;
      if (!t || !accept("task", t.id)) return;
      const same = task && task.id === t.id;
      task = t;
      statuses = p.statuses || (same ? statuses : null);
      settingStatus = null;
      menuOpen = false;
      comments = { items: [], hasMore: false, loading: true, more: false };
      threads = {};
      clearInfo();
      renderTask();
      if (!same) app.scrollTop = 0;
    },

    setStatuses: (p) => {
      if (!task || !p || String(task.list && task.list.id) !== String(p.list_id)) return;
      statuses = p.statuses || [];
      if (menuOpen) renderStatusSlot();
    },

    statusChanged: (p) => {
      if (!p) return;
      // Lists the page keeps show it too, for Back.
      Object.keys(cache.list).forEach((k) =>
        cache.list[k].tasks.forEach((t) => {
          if (String(t.id) === String(p.task_id)) t.status = p.status;
        })
      );
      if (view.kind === "list") renderListBody();
      if (!onTask(p.task_id)) return;
      task.status = p.status;
      settingStatus = null;
      renderStatusSlot();
    },

    setComments: (p) => {
      if (!p || !onTask(p.task_id)) return;
      const items = p.items || [];
      if (p.append) {
        const have = new Set(comments.items.map((c) => String(c.id)));
        comments.items = comments.items.concat(items.filter((c) => !have.has(String(c.id))));
        comments.hasMore = !!p.has_more;
      } else {
        // A reload after posting brings the newest page only: keep the
        // older pages already loaded, so the thread being read stays.
        const fresh = new Set(items.map((c) => String(c.id)));
        const oldest = items.length ? Math.min.apply(null, items.map((c) => Number(c.date) || 0)) : Infinity;
        const keep = comments.items.filter((c) => !fresh.has(String(c.id)) && Number(c.date) < oldest);
        comments.items = items.concat(keep);
        comments.hasMore = keep.length ? comments.hasMore : !!p.has_more;
      }
      comments.loading = false;
      comments.more = false;
      renderComments();
    },

    setReplies: (p) => {
      if (!p) return;
      const id = String(p.comment_id);
      const parent = comments.items.find((c) => String(c.id) === id);
      if (!parent) return;
      const items = (p.items || []).slice().reverse(); // oldest first, as a thread reads
      threads[id] = { open: true, items: items };
      parent.replies = items.length;
      renderComments();
    },

    posted: (p) => {
      if (!p) return;
      const key = p.comment_id ? "r:" + p.comment_id : "c:" + p.task_id;
      drafts[key] = "";
      delete posting[key];
      const el = app.querySelector('textarea[data-draft="' + CSS.escape(key) + '"]');
      if (el) el.value = "";
      syncPosting();
    },
  };

  emit("ready");
})();
