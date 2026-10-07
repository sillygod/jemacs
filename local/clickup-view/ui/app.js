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

  function dot(st) {
    return '<i class="dot" style="--c:' + hex(st && st.color) + '"></i>';
  }

  // A task link, CU-<id> or bare id → native id; null when it is none.
  // Mirrors clickup-view--ref-re: a custom id (/t/<team>/DEV-12) is not one.
  function taskIdFromUrl(href) {
    const m = /^https?:\/\/app\.clickup\.com\/t\/(?:\d+\/)?([0-9a-z]+)(?:[?#]|$)/.exec(
      String(href || "")
    );
    return m ? m[1] : null;
  }

  function taskIdFrom(s) {
    s = String(s || "").trim().replace(/^#/, "");
    const link = /app\.clickup\.com\/t\/(?:\d+\/)?([0-9a-z]+)(?:[^/0-9a-zA-Z-]|$)/.exec(s);
    if (link) return link[1];
    const cu = /(?:^|[^0-9A-Za-z_])CU-([0-9a-z]+)(?:[^0-9A-Za-z_]|$)/.exec(s);
    if (cu) return cu[1];
    return /^[0-9a-z]+$/.test(s) ? s : null;
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
  let view = { kind: "boot" };
  const trail = []; // views behind this one, for Back
  let nav = null; // { spec, push } while a view is on its way
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

  function sameView(a, b) {
    return a.kind === b.kind && String(a.id || "") === String(b.id || "");
  }

  function go(spec, push) {
    if (push === undefined) push = true;
    if (statusEl.classList.contains("error")) setStatus("");
    if (spec.kind === "home") {
      arrive(spec, push);
      renderHome();
      return;
    }
    nav = { spec: spec, push: push };
    progress(true);
    if (spec.kind === "task") emit("open-task", { id: spec.id });
  }

  function arrive(spec, push) {
    if (push && view.kind !== "boot" && !sameView(view, spec)) trail.push(view);
    view = spec;
    nav = null;
    progress(false);
  }

  function back() {
    nav = null;
    progress(false);
    const prev = trail.pop();
    if (prev) go(prev, false);
  }

  function refresh() {
    if (view.kind === "task") go(view, false);
    else emit("refresh-spaces");
  }

  function chrome() {
    btnBack.hidden = !trail.length;
    const url = view.kind === "task" && task && task.url;
    btnCopy.hidden = !url;
    btnBrowser.hidden = !url;
    if (view.kind === "task" && task) {
      const parts = [task.space && task.space.name, task.folder && task.folder.name, task.list && task.list.name]
        .filter(Boolean)
        .map((n) => '<span class="crumb">' + esc(n) + "</span>");
      crumbsEl.innerHTML = parts.join('<span class="sep">›</span>');
    } else {
      crumbsEl.textContent = "ClickUp";
    }
  }

  // ---- Home ------------------------------------------------------------

  function renderHome() {
    task = null;
    chrome();
    app.innerHTML =
      '<section class="home">' +
      "<h2>Open a task</h2>" +
      '<div class="open-row">' +
      '<input id="open-id" type="text" spellcheck="false" autocomplete="off" ' +
      'placeholder="Task link, CU-id or id">' +
      '<button type="button" class="primary" data-act="open-typed">Open</button>' +
      "</div>" +
      '<p class="muted">From a branch, <code>M-x clickup-view</code> opens the task its ' +
      "commits reference.</p>" +
      "</section>";
    const input = $("open-id");
    if (input) input.focus();
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
      '<i class="dot"></i><span>' + esc(label) + '</span><span class="caret">▾</span></button>' +
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

  function section(title, count, body) {
    return (
      '<section class="block"><h2>' + esc(title) +
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
          '<div class="row' + (isDone(s.status) ? " done" : "") + '" role="button" tabindex="0" ' +
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
      case "open-typed":
        openTyped();
        break;
      case "open-task":
        if (id) go({ kind: "task", id: id });
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
    const key = ev.target.getAttribute && ev.target.getAttribute("data-draft");
    if (key) drafts[key] = ev.target.value;
  });

  statusEl.addEventListener("click", (ev) => {
    if (ev.target.closest(".x")) setStatus("");
  });

  btnBack.addEventListener("click", back);
  btnRefresh.addEventListener("click", refresh);
  btnCopy.addEventListener("click", () => task && emit("copy-url", { url: task.url }));
  btnBrowser.addEventListener("click", () => task && emit("open-browser", { url: task.url }));

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
      } else if (ev.key === "Escape") {
        t.blur();
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
    if (ev.key === "Enter" && t && t.getAttribute && t.getAttribute("role") === "button") {
      ev.preventDefault();
      act(t.getAttribute("data-act"), t);
    } else if (ev.key === "Escape") {
      back();
    } else if (ev.key === "r") {
      refresh();
    } else if (ev.key === "s" && view.kind === "task" && task) {
      ev.preventDefault();
      openMenu(true);
    }
  });

  // ---- Calls from Emacs ------------------------------------------------
  //
  // Every payload names the task or comment it is for.  One for anything
  // but what is on screen (a slow reply for the task just left) is dropped.

  function acceptTask(id) {
    if (nav && nav.spec.kind === "task") {
      if (String(nav.spec.id) !== String(id)) return false;
      arrive({ kind: "task", id: id }, nav.push);
      return true;
    }
    if (view.kind === "boot" || (view.kind === "task" && String(view.id) === String(id))) {
      arrive({ kind: "task", id: id }, false);
      return true;
    }
    return false;
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
      if (view.kind === "home") chrome();
    },

    showHome: () => {
      clearInfo();
      arrive({ kind: "home" }, false);
      renderHome();
    },

    renderTask: (p) => {
      const t = p && p.task;
      if (!t || !acceptTask(t.id)) return;
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
      if (!p || !onTask(p.task_id)) return;
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
