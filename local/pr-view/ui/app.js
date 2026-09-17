(() => {
  const PREFIX = "prview:";
  const app = document.getElementById("app");
  const statusEl = document.getElementById("status");
  const contextEl = document.getElementById("context");
  const btnBack = document.getElementById("btn-back");
  const btnRefresh = document.getElementById("btn-refresh");
  const btnCreate = document.getElementById("btn-create");
  const btnBrowser = document.getElementById("btn-browser");
  const btnCopy = document.getElementById("btn-copy");

  let lastList = null;
  let lastDetail = null;
  let selectedStates = ["OPEN", "MERGED"];
  let searchQ = "";
  let page = "list";
  let selectedDiffPath = null;
  let viewedFiles = {};
  let selectedTab = "overview";
  let lastMembers = [];

  function emit(op, extra) {
    const payload = Object.assign({ op: op }, extra || {});
    document.title = PREFIX + JSON.stringify(payload);
  }

  function escapeHtml(s) {
    return String(s == null ? "" : s).replace(/[&<>"']/g, (c) => ({
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#39;",
    })[c]);
  }

  function relTime(iso) {
    if (!iso) return "";
    const t = Date.parse(iso);
    if (Number.isNaN(t)) return iso;
    const s = Math.max(0, (Date.now() - t) / 1000);
    if (s < 60) return "just now";
    if (s < 3600) return Math.floor(s / 60) + "m ago";
    if (s < 86400) return Math.floor(s / 3600) + "h ago";
    if (s < 604800) return Math.floor(s / 86400) + "d ago";
    return new Date(t).toISOString().slice(0, 10);
  }

  function initials(name) {
    const parts = String(name || "?").trim().split(/[\s._-]+/).filter(Boolean);
    if (parts.length >= 2) {
      return (parts[0][0] + parts[1][0]).toUpperCase();
    }
    return String(name || "?").slice(0, 2).toUpperCase();
  }

  function hue(name) {
    let h = 0;
    for (const c of String(name || "")) h = (h * 31 + c.charCodeAt(0)) >>> 0;
    return h % 360;
  }

  function setStatus(msg, kind) {
    if (!msg) {
      statusEl.hidden = true;
      statusEl.textContent = "";
      return;
    }
    statusEl.hidden = false;
    statusEl.textContent = msg;
    statusEl.className = "status" + (kind ? " " + kind : "");
  }

  function inlineMd(s) {
    s = escapeHtml(s);
    s = s.replace(/`([^`]+)`/g, "<code>$1</code>");
    s = s.replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
    s = s.replace(/__([^_]+)__/g, "<strong>$1</strong>");
    s = s.replace(/~~([^~]+)~~/g, "<del>$1</del>");
    s = s.replace(/(^|[^\*])\*([^*\n]+)\*/g, "$1<em>$2</em>");
    s = s.replace(
      /\[([^\]]+)\]\((https?:[^)]+)\)/g,
      '<a href="$2" target="_blank" rel="noopener">$1</a>'
    );
    return s;
  }

  function renderMdTable(rows) {
    const cells = (row) =>
      row
        .replace(/^\||\|$/g, "")
        .split("|")
        .map((c) => c.trim());
    const head = cells(rows[0]);
    let start = 1;
    if (rows[1] && /^\s*\|?[\s:\-|]+$/.test(rows[1])) start = 2;
    let html =
      "<table><thead><tr>" +
      head.map((c) => "<th>" + inlineMd(c) + "</th>").join("") +
      "</tr></thead><tbody>";
    for (let r = start; r < rows.length; r++) {
      html +=
        "<tr>" +
        cells(rows[r]).map((c) => "<td>" + inlineMd(c) + "</td>").join("") +
        "</tr>";
    }
    return html + "</tbody></table>";
  }

  function renderMarkdown(src) {
    if (!src || !String(src).trim()) return "<p class='muted'>No description.</p>";
    const lines = String(src).replace(/\r\n/g, "\n").split("\n");
    const out = [];
    const para = [];
    let i = 0;
    const flush = () => {
      if (!para.length) return;
      out.push("<p>" + inlineMd(para.join(" ")) + "</p>");
      para.length = 0;
    };
    while (i < lines.length) {
      const line = lines[i];
      if (line.startsWith("```")) {
        flush();
        i++;
        const code = [];
        while (i < lines.length && !lines[i].startsWith("```")) {
          code.push(escapeHtml(lines[i]));
          i++;
        }
        i++;
        out.push("<pre><code>" + code.join("\n") + "</code></pre>");
        continue;
      }
      const hm = line.match(/^(#{1,6})\s+(.*)$/);
      if (hm) {
        flush();
        const n = hm[1].length;
        out.push("<h" + n + ">" + inlineMd(hm[2]) + "</h" + n + ">");
        i++;
        continue;
      }
      if (/^(-{3,}|\*{3,}|_{3,})\s*$/.test(line.trim())) {
        flush();
        out.push("<hr>");
        i++;
        continue;
      }
      if (
        line.trim().startsWith("|") &&
        i + 1 < lines.length &&
        /^\s*\|?[\s:\-|]+$/.test(lines[i + 1])
      ) {
        flush();
        const rows = [];
        while (i < lines.length && lines[i].trim().startsWith("|")) {
          rows.push(lines[i]);
          i++;
        }
        out.push(renderMdTable(rows));
        continue;
      }
      if (line.startsWith(">")) {
        flush();
        const q = [];
        while (i < lines.length && lines[i].startsWith(">")) {
          q.push(lines[i].replace(/^>\s?/, ""));
          i++;
        }
        out.push("<blockquote>" + renderMarkdown(q.join("\n")) + "</blockquote>");
        continue;
      }
      const lm = line.match(/^(\s*)([-*+]|\d+\.)\s+(.*)$/);
      if (lm) {
        flush();
        const ordered = /^\d+\./.test(lm[2]);
        const items = [];
        while (i < lines.length) {
          const m = lines[i].match(/^(\s*)([-*+]|\d+\.)\s+(.*)$/);
          if (!m) break;
          items.push(m[3]);
          i++;
        }
        const tag = ordered ? "ol" : "ul";
        out.push(
          "<" +
            tag +
            ">" +
            items
              .map((it) => {
                const task = it.match(/^\[([ xX])\]\s+(.*)$/);
                if (task) {
                  const chk = /x/i.test(task[1]) ? " checked" : "";
                  return (
                    '<li class="task"><input type="checkbox" disabled' +
                    chk +
                    "> " +
                    inlineMd(task[2]) +
                    "</li>"
                  );
                }
                return "<li>" + inlineMd(it) + "</li>";
              })
              .join("") +
            "</" +
            tag +
            ">"
        );
        continue;
      }
      if (!line.trim()) {
        flush();
        i++;
        continue;
      }
      para.push(line);
      i++;
    }
    flush();
    return out.join("\n") || "<p class='muted'>No description.</p>";
  }

  function parseUnifiedDiff(text) {
    const files = [];
    let cur = null;
    let oldLn = 0;
    let newLn = 0;
    const lines = String(text).replace(/\r\n/g, "\n").split("\n");
    for (const line of lines) {
      let m = line.match(/^diff --git a\/(.+) b\/(.+)$/);
      if (m) {
        cur = {
          oldPath: m[1],
          newPath: m[2],
          path: m[2],
          status: "modified",
          hunks: [],
          added: 0,
          deleted: 0,
        };
        files.push(cur);
        continue;
      }
      if (!cur) continue;
      if (line.startsWith("new file")) {
        cur.status = "added";
        continue;
      }
      if (line.startsWith("deleted file")) {
        cur.status = "deleted";
        continue;
      }
      if (line.startsWith("rename ")) {
        cur.status = "renamed";
        continue;
      }
      if (line.startsWith("--- ")) {
        const p = line.slice(4).replace(/^[ab]\//, "");
        if (p === "/dev/null") cur.status = "added";
        else cur.oldPath = p;
        continue;
      }
      if (line.startsWith("+++ ")) {
        const p = line.slice(4).replace(/^[ab]\//, "");
        if (p === "/dev/null") cur.status = "deleted";
        else {
          cur.newPath = p;
          cur.path = p;
        }
        continue;
      }
      m = line.match(/^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@/);
      if (m) {
        oldLn = +m[1];
        newLn = +m[2];
        cur.hunks.push({ header: line, lines: [] });
        continue;
      }
      if (!cur.hunks.length) continue;
      const hunk = cur.hunks[cur.hunks.length - 1];
      if (line.startsWith("+") && !line.startsWith("+++")) {
        hunk.lines.push({ type: "add", text: line.slice(1), old: null, neu: newLn++ });
        cur.added++;
      } else if (line.startsWith("-") && !line.startsWith("---")) {
        hunk.lines.push({ type: "del", text: line.slice(1), old: oldLn++, neu: null });
        cur.deleted++;
      } else if (line.startsWith("\\")) {
        continue;
      } else {
        const text = line.startsWith(" ") ? line.slice(1) : line;
        hunk.lines.push({ type: "ctx", text: text, old: oldLn++, neu: newLn++ });
      }
    }
    return files;
  }

  function buildTree(files) {
    const root = { name: "", dirs: {}, files: [] };
    files.forEach((f, index) => {
      const parts = String(f.path || "unknown").split("/");
      let node = root;
      for (let p = 0; p < parts.length - 1; p++) {
        const name = parts[p];
        node.dirs[name] = node.dirs[name] || { name: name, dirs: {}, files: [] };
        node = node.dirs[name];
      }
      node.files.push({ file: f, index: index, name: parts[parts.length - 1] });
    });
    return root;
  }

  function statusMark(st) {
    if (st === "added") return '<span class="tree-mark add">+</span>';
    if (st === "deleted") return '<span class="tree-mark del">−</span>';
    if (st === "renamed") return '<span class="tree-mark ren">→</span>';
    return '<span class="tree-mark mod"></span>';
  }

  function renderTreeNode(node, filter) {
    const q = (filter || "").toLowerCase();
    const match = (path) => !q || String(path).toLowerCase().indexOf(q) !== -1;
    let html = "";
    const dirNames = Object.keys(node.dirs).sort();
    dirNames.forEach((name) => {
      const child = node.dirs[name];
      const inner = renderTreeNode(child, filter);
      if (q && !inner) return;
      html +=
        '<div class="tree-dir"><div class="tree-dir-name">▾ ' +
        escapeHtml(name) +
        "</div><div class='tree-dir-kids'>" +
        inner +
        "</div></div>";
    });
    node.files
      .slice()
      .sort((a, b) => a.name.localeCompare(b.name))
      .forEach((ent) => {
        if (!match(ent.file.path)) return;
        const sel = ent.file.path === selectedDiffPath ? " selected" : "";
        html +=
          '<button type="button" class="tree-file' +
          sel +
          '" data-path="' +
          escapeHtml(ent.file.path) +
          '">' +
          statusMark(ent.file.status) +
          '<span class="tree-name">' +
          escapeHtml(ent.name) +
          "</span></button>";
      });
    return html;
  }

  function renderFileLines(file) {
    const rows = [];
    (file.hunks || []).forEach((h) => {
      rows.push(
        '<div class="diff-row hunk"><span class="ln"></span><span class="ln"></span><span class="gutter"></span><span class="code">' +
          escapeHtml(h.header) +
          "</span></div>"
      );
      h.lines.forEach((ln) => {
        const sign = ln.type === "add" ? "+" : ln.type === "del" ? "−" : "";
        rows.push(
          '<div class="diff-row ' +
            ln.type +
            '"><span class="ln">' +
            (ln.old != null ? ln.old : "") +
            '</span><span class="ln">' +
            (ln.neu != null ? ln.neu : "") +
            '</span><span class="gutter">' +
            sign +
            '</span><span class="code">' +
            escapeHtml(ln.text) +
            "</span></div>"
        );
      });
    });
    return rows.join("");
  }

  function renderFileDiff(payload) {
    if (payload.diff_loading) {
      return '<div class="diff-split"><div class="file-pane"><p class="muted pane-msg">Loading diff…</p></div></div>';
    }
    if (payload.diff_error) {
      return (
        '<div class="diff-split"><div class="file-pane"><p class="muted pane-msg">Diff unavailable (' +
        escapeHtml(payload.diff_error) +
        ").</p></div></div>"
      );
    }
    const raw = payload.diff || "";
    if (!String(raw).trim()) {
      return '<div class="diff-split"><div class="file-pane"><p class="muted pane-msg">No diff.</p></div></div>';
    }
    const files = parseUnifiedDiff(raw);
    if (!files.length) {
      return '<div class="diff-split"><div class="file-pane"><p class="muted pane-msg">No file changes.</p></div></div>';
    }
    if (!selectedDiffPath || !files.some((f) => f.path === selectedDiffPath)) {
      selectedDiffPath = files[0].path;
    }
    const current = files.find((f) => f.path === selectedDiffPath) || files[0];
    const tree = buildTree(files);
    const stats =
      (current.added ? '<span class="stat-add">+' + current.added + "</span>" : "") +
      (current.deleted ? '<span class="stat-del">−' + current.deleted + "</span>" : "");
    const viewed = !!viewedFiles[current.path];
    return (
      '<div class="diff-split">' +
      '<aside class="file-tree">' +
      '<input class="tree-search" id="tree-search" type="search" placeholder="Filter files…">' +
      '<div class="tree-body" id="tree-body">' +
      renderTreeNode(tree, "") +
      "</div></aside>" +
      '<section class="file-pane">' +
      '<header class="file-head">' +
      '<span class="file-path">' +
      escapeHtml(current.path) +
      "</span>" +
      '<span class="file-stats">' +
      stats +
      "</span>" +
      '<label class="viewed"><input type="checkbox" id="file-viewed"' +
      (viewed ? " checked" : "") +
      "> Viewed</label>" +
      "</header>" +
      '<div class="file-diff">' +
      renderFileLines(current) +
      "</div></section></div>"
    );
  }

  function avatarHtml(name, url, cls) {
    const n = name || "?";
    return (
      '<div class="avatar ' +
      (cls || "") +
      '" style="--h:' +
      hue(n) +
      '" title="' +
      escapeHtml(n) +
      '">' +
      (url
        ? '<img src="' +
          escapeHtml(url) +
          '" alt="" onerror="this.remove()">'
        : "") +
      "<span>" +
      escapeHtml(initials(n)) +
      "</span></div>"
    );
  }

  function isDraft(it) {
    return it.draft === true;
  }

  function badgeFor(it) {
    const st = String(it.state || "").toUpperCase();
    if (st === "MERGED") return "MERGED";
    if (st === "DECLINED" || st === "SUPERSEDED" || st === "CLOSED") return st;
    if (isDraft(it)) return "DRAFT";
    return st || "OPEN";
  }

  function setChrome(kind) {
    page = kind;
    btnBack.hidden = kind === "list";
    btnRefresh.hidden = kind !== "list";
    btnCreate.hidden = kind !== "list";
    btnBrowser.hidden = kind !== "detail";
    btnCopy.hidden = kind !== "detail";
  }

  function matchesSearch(it, q) {
    if (!q) return true;
    const blob = [
      it.title,
      it.author,
      it.source,
      it.destination,
      it.id,
      it.state,
    ]
      .join(" ")
      .toLowerCase();
    return blob.indexOf(q) !== -1;
  }

  function renderList(payload) {
    lastList = payload;
    lastDetail = null;
    setChrome("list");
    const forge = payload.forge || "";
    const owner = payload.owner || "";
    const repo = payload.repo || "";
    contextEl.textContent = (forge ? forge + " · " : "") + owner + "/" + repo;
    if (payload.states && payload.states.length) {
      selectedStates = payload.states.map((s) => String(s).toUpperCase());
    }
    const items = payload.items || [];
    const visible = items.filter((it) => matchesSearch(it, searchQ));
    const chips = [
      ["OPEN", "Open", "open"],
      ["DRAFT", "Draft", "draft"],
      ["MERGED", "Merged", "merged"],
      ["DECLINED", "Declined", "declined"],
    ]
      .map(([id, label, cls]) => {
        const on = selectedStates.indexOf(id) !== -1;
        return (
          '<button type="button" class="filter ' +
          cls +
          (on ? " on" : "") +
          '" data-state="' +
          id +
          '">' +
          label +
          (on ? " ×" : "") +
          "</button>"
        );
      })
      .join("");

    let rows;
    if (!visible.length) {
      rows =
        '<div class="empty">' +
        (items.length ? "No pull requests match this search." : "No pull requests in this filter.") +
        "</div>";
    } else {
      rows =
        '<div class="list-head"><span>Summary</span><span>Created</span><span>Activity</span><span>Reviewers</span></div>' +
        visible
          .map((it) => {
            const id = escapeHtml(it.id);
            const badge = badgeFor(it);
            const reviewers = it.reviewers || [];
            const rev =
              reviewers.length === 0
                ? '<span class="none">No reviewers</span>'
                : reviewers
                    .slice(0, 5)
                    .map((r) => avatarHtml(r.name || r, r.avatar, ""))
                    .join("");
            return (
              '<article class="pr-row" data-id="' +
              id +
              '">' +
              '<div class="summary">' +
              avatarHtml(it.author, it.author_avatar) +
              "<div>" +
              '<div class="title-line">' +
              '<span class="badge ' +
              badge +
              '">' +
              escapeHtml(badge.charAt(0) + badge.slice(1).toLowerCase()) +
              "</span>" +
              '<span class="pr-title">' +
              escapeHtml(it.title) +
              "</span></div>" +
              '<div class="sub">' +
              "<span>" +
              escapeHtml(it.author) +
              " · #" +
              id +
              ", updated " +
              escapeHtml(relTime(it.updated_on)) +
              "</span>" +
              '<span class="branch">' +
              escapeHtml(it.source) +
              "</span>" +
              '<span class="arrow">→</span>' +
              '<span class="branch">' +
              escapeHtml(it.destination) +
              "</span></div></div></div>" +
              '<div class="col">' +
              escapeHtml(relTime(it.created_on || it.updated_on)) +
              "</div>" +
              '<div class="col activity">💬 ' +
              escapeHtml(it.comment_count || 0) +
              "</div>" +
              '<div class="reviewers">' +
              rev +
              (it.url
                ? '<button type="button" class="row-copy" data-url="' +
                  escapeHtml(it.url) +
                  '" title="Copy PR link">Copy</button>'
                : "") +
              "</div></article>"
            );
          })
          .join("");
    }

    app.innerHTML =
      '<div class="toolbar">' +
      '<input class="search" id="pr-search" type="search" placeholder="Search pull requests" value="' +
      escapeHtml(searchQ) +
      '">' +
      '<div class="filters">' +
      chips +
      "</div></div>" +
      rows;

    app.querySelectorAll(".row-copy").forEach((btn) => {
      btn.addEventListener("click", (ev) => {
        ev.stopPropagation();
        emit("copy-url", { url: btn.getAttribute("data-url") || "" });
      });
    });
    app.querySelectorAll(".pr-row").forEach((row) => {
      row.addEventListener("click", () => {
        const id = Number(row.getAttribute("data-id"));
        setStatus("Opening #" + id + "…");
        emit("open-pr", { id: id });
      });
    });
    app.querySelectorAll(".filter").forEach((btn) => {
      btn.addEventListener("click", (ev) => {
        ev.preventDefault();
        const st = btn.getAttribute("data-state");
        const i = selectedStates.indexOf(st);
        if (i >= 0) {
          if (selectedStates.length === 1) return;
          selectedStates = selectedStates.filter((x) => x !== st);
        } else {
          selectedStates = selectedStates.concat([st]);
        }
        setStatus("Loading…");
        emit("refresh-list", { states: selectedStates });
      });
    });
    const search = document.getElementById("pr-search");
    if (search) {
      search.addEventListener("input", () => {
        searchQ = search.value.trim().toLowerCase();
        app.querySelectorAll(".pr-row").forEach((row) => {
          const id = row.getAttribute("data-id");
          const it = items.find((x) => String(x.id) === String(id));
          row.hidden = !it || !matchesSearch(it, searchQ);
        });
      });
    }
    setStatus("");
  }

  function commentCard(c, isReply) {
    const path = c.inline_path
      ? '<div class="comment-file">' + escapeHtml(c.inline_path) + "</div>"
      : "";
    const likes = c.like_count ? " (" + c.like_count + ")" : "";
    return (
      '<article class="comment' +
      (isReply ? " reply" : "") +
      '" data-id="' +
      escapeHtml(c.id) +
      '">' +
      avatarHtml(c.author, c.avatar) +
      '<div class="comment-body">' +
      '<div class="comment-meta"><strong>' +
      escapeHtml(c.author || "unknown") +
      "</strong> <span>" +
      escapeHtml(relTime(c.created_on)) +
      "</span></div>" +
      path +
      '<div class="comment-md">' +
      renderMarkdown(c.content || "") +
      "</div>" +
      '<div class="comment-actions">' +
      '<button type="button" class="linkish" data-act="reply">Reply</button>' +
      '<button type="button" class="linkish" data-act="like">Like' +
      likes +
      "</button>" +
      '<button type="button" class="linkish" data-act="edit">Edit</button>' +
      '<button type="button" class="linkish" data-act="delete">Delete</button>' +
      "</div></div></article>"
    );
  }

  function commentsHtml(comments) {
    const list = comments || [];
    const kids = {};
    const roots = [];
    list.forEach((c) => {
      if (c.parent_id) {
        (kids[c.parent_id] = kids[c.parent_id] || []).push(c);
      } else {
        roots.push(c);
      }
    });
    const newest = (a, b) =>
      (Date.parse(b.created_on || "") || 0) - (Date.parse(a.created_on || "") || 0);
    const oldest = (a, b) =>
      (Date.parse(a.created_on || "") || 0) - (Date.parse(b.created_on || "") || 0);
    roots.sort(newest);
    if (!roots.length && !list.length) {
      return '<p class="muted">No comments yet.</p>';
    }
    function tree(c) {
      const replies = (kids[c.id] || []).slice().sort(oldest);
      return commentCard(c, !!c.parent_id) + replies.map(tree).join("");
    }
    return roots.map(tree).join("");
  }

  function reviewersHtml(pr) {
    const revs = pr.reviewers || [];
    const chips = revs
      .map((r) => {
        const name = r.name || r;
        const uuid = r.uuid || r.nickname || name;
        return (
          '<span class="rev-chip">' +
          avatarHtml(name, r.avatar) +
          "<span>" +
          escapeHtml(name) +
          '</span><button type="button" class="rev-x" data-uuid="' +
          escapeHtml(uuid) +
          '" title="Remove reviewer">×</button></span>'
        );
      })
      .join("");
    const opts = lastMembers
      .map((m) => '<option value="' + escapeHtml(m.nickname || m.name) + '">')
      .join("");
    return (
      '<div class="reviewer-bar" id="reviewer-bar">' +
      '<span class="rev-label">Reviewers</span>' +
      chips +
      '<input id="add-reviewer" list="member-list" placeholder="Add reviewer">' +
      '<datalist id="member-list">' +
      opts +
      "</datalist>" +
      '<button type="button" class="ghost" id="btn-add-reviewer">Add</button></div>'
    );
  }

  function mergeOptions(forge) {
    if (forge === "github") {
      return [
        ["merge", "Merge commit"],
        ["squash", "Squash"],
        ["rebase", "Rebase"],
      ];
    }
    return [
      ["merge_commit", "Merge commit"],
      ["squash", "Squash"],
      ["fast_forward", "Fast-forward"],
    ];
  }

  function renderDetail(payload) {
    lastDetail = payload;
    const pr = (payload && payload.pr) || {};
    const forge = (payload && payload.forge) || (lastList && lastList.forge) || "";
    setChrome("detail");
    btnBrowser.hidden = !pr.url;
    btnCopy.hidden = !pr.url;
    contextEl.textContent = "#" + (pr.id || "") + " · " + (pr.state || "");
    const open = String(pr.state || "").toUpperCase() === "OPEN" && !isDraft(pr);
    const strategies = mergeOptions(forge)
      .map((pair) => '<option value="' + pair[0] + '">' + pair[1] + "</option>")
      .join("");
    const actions = open
      ? '<div class="detail-actions">' +
        '<button type="button" class="warn" id="act-approve">Approve</button>' +
        '<select id="act-strategy">' +
        strategies +
        "</select>" +
        '<label class="check"><input type="checkbox" id="act-close" checked> Close source</label>' +
        '<button type="button" class="success" id="act-merge">Merge</button>' +
        "</div>"
      : "";
    const badge = badgeFor(pr);
    const comments = payload.comments || [];
    const filesN = payload.diff ? parseUnifiedDiff(payload.diff).length : 0;
    const ovOn = selectedTab !== "files" ? " on" : "";
    const fiOn = selectedTab === "files" ? " on" : "";
    app.innerHTML =
      '<div class="detail-wrap"><section class="detail-head">' +
      "<h1>" +
      escapeHtml(pr.title || "") +
      "</h1>" +
      '<div class="chips">' +
      '<span class="badge ' +
      badge +
      '">' +
      escapeHtml(badge.charAt(0) + badge.slice(1).toLowerCase()) +
      "</span>" +
      "<span class='chip'>" +
      escapeHtml(pr.author || "") +
      "</span>" +
      '<span class="chip">' +
      escapeHtml(pr.source || "") +
      " → " +
      escapeHtml(pr.destination || "") +
      "</span>" +
      actions +
      "</div>" +
      reviewersHtml(pr) +
      '<nav class="tabs">' +
      '<button type="button" class="tab' +
      ovOn +
      '" data-tab="overview">Overview</button>' +
      '<button type="button" class="tab' +
      fiOn +
      '" data-tab="files">Files changed' +
      (filesN ? " " + filesN : "") +
      "</button></nav></section>" +
      '<div id="panel-overview" class="panel-overview"' +
      (selectedTab === "files" ? " hidden" : "") +
      ">" +
      '<section class="description">' +
      renderMarkdown(pr.description || "") +
      "</section>" +
      '<section class="comments-wrap">' +
      "<h2>Comments</h2>" +
      '<div id="comments-list">' +
      commentsHtml(comments) +
      "</div>" +
      '<div class="comment-form">' +
      '<textarea id="comment-body" rows="4" placeholder="Leave a comment (markdown)"></textarea>' +
      '<button type="button" class="primary" id="comment-post">Comment</button>' +
      "</div></section></div>" +
      (selectedTab === "files"
        ? '<div id="panel-files" class="panel-files">' +
          renderFileDiff(payload) +
          "</div>"
        : "") +
      "</div>";
    const ap = document.getElementById("act-approve");
    const mg = document.getElementById("act-merge");
    const treeBody = document.getElementById("tree-body");
    const treeSearch = document.getElementById("tree-search");
    const viewedEl = document.getElementById("file-viewed");
    if (treeBody) {
      treeBody.querySelectorAll(".tree-file").forEach((btn) => {
        btn.addEventListener("click", () => {
          selectedDiffPath = btn.getAttribute("data-path");
          renderDetail(lastDetail);
        });
      });
    }
    if (treeSearch && treeBody) {
      treeSearch.addEventListener("input", () => {
        const q = treeSearch.value.trim().toLowerCase();
        treeBody.querySelectorAll(".tree-file").forEach((btn) => {
          const path = (btn.getAttribute("data-path") || "").toLowerCase();
          btn.hidden = q && path.indexOf(q) === -1;
        });
        treeBody.querySelectorAll(".tree-dir").forEach((dir) => {
          const vis = dir.querySelectorAll(".tree-file:not([hidden])").length;
          dir.hidden = q && vis === 0;
        });
      });
    }
    if (viewedEl) {
      viewedEl.addEventListener("change", () => {
        if (!selectedDiffPath) return;
        if (viewedEl.checked) viewedFiles[selectedDiffPath] = true;
        else delete viewedFiles[selectedDiffPath];
      });
    }
    app.querySelectorAll(".tab").forEach((btn) => {
      btn.addEventListener("click", () => {
        selectedTab = btn.getAttribute("data-tab") || "overview";
        renderDetail(lastDetail);
      });
    });
    const postBtn = document.getElementById("comment-post");
    const bodyEl = document.getElementById("comment-body");
    if (postBtn && bodyEl) {
      postBtn.addEventListener("click", () => {
        const text = bodyEl.value.trim();
        if (!text) return;
        setStatus("Posting comment…");
        emit("create-comment", { id: pr.id, content: text });
        bodyEl.value = "";
      });
    }
    const clist = document.getElementById("comments-list");
    if (clist) {
      clist.addEventListener("click", (ev) => {
        const btn = ev.target.closest("[data-act]");
        if (!btn) return;
        const art = btn.closest(".comment");
        const cid = art && art.getAttribute("data-id");
        const act = btn.getAttribute("data-act");
        if (!cid) return;
        if (act === "delete") {
          if (!window.confirm("Delete this comment?")) return;
          setStatus("Deleting comment…");
          emit("delete-comment", { id: pr.id, comment_id: cid });
        } else if (act === "like") {
          setStatus("Liking comment…");
          emit("like-comment", { id: pr.id, comment_id: cid });
        } else if (act === "reply") {
          let box = art.querySelector(".reply-box");
          if (box) {
            box.remove();
            return;
          }
          box = document.createElement("div");
          box.className = "reply-box";
          box.innerHTML =
            '<textarea rows="3" placeholder="Reply (markdown)"></textarea>' +
            '<button type="button" class="primary reply-send">Reply</button>';
          art.querySelector(".comment-body").appendChild(box);
          box.querySelector(".reply-send").addEventListener("click", () => {
            const text = box.querySelector("textarea").value.trim();
            if (!text) return;
            setStatus("Posting reply…");
            emit("create-comment", {
              id: pr.id,
              content: text,
              parent_id: Number(cid) || cid,
            });
          });
        } else if (act === "edit") {
          const md = art.querySelector(".comment-md");
          const existing = (lastDetail.comments || []).find(
            (x) => String(x.id) === String(cid)
          );
          const cur = (existing && existing.content) || "";
          md.innerHTML =
            '<textarea class="edit-area" rows="4"></textarea>' +
            '<button type="button" class="primary edit-save">Save</button>';
          md.querySelector("textarea").value = cur;
          md.querySelector(".edit-save").addEventListener("click", () => {
            const text = md.querySelector("textarea").value.trim();
            if (!text) return;
            setStatus("Updating comment…");
            emit("edit-comment", {
              id: pr.id,
              comment_id: cid,
              content: text,
            });
          });
        }
      });
    }
    const addRev = document.getElementById("btn-add-reviewer");
    const addInp = document.getElementById("add-reviewer");
    if (addRev && addInp) {
      addRev.addEventListener("click", () => {
        const who = addInp.value.trim();
        if (!who) return;
        setStatus("Adding reviewer…");
        emit("add-reviewer", { id: pr.id, nickname: who });
        addInp.value = "";
      });
    }
    app.querySelectorAll(".rev-x").forEach((btn) => {
      btn.addEventListener("click", () => {
        const who = btn.getAttribute("data-uuid");
        if (!who) return;
        setStatus("Removing reviewer…");
        emit("remove-reviewer", { id: pr.id, uuid: who, nickname: who });
      });
    });
    if (ap) {
      ap.addEventListener("click", () => {
        setStatus("Approving #" + pr.id + "…");
        emit("approve-pr", { id: pr.id });
      });
    }
    if (mg) {
      mg.addEventListener("click", () => {
        const strategy = (document.getElementById("act-strategy") || {}).value;
        const close = !!(document.getElementById("act-close") || {}).checked;
        if (
          !window.confirm(
            "Merge pull request #" + pr.id + " into " + (pr.destination || "") + "?"
          )
        ) {
          return;
        }
        setStatus("Merging #" + pr.id + "…");
        emit("merge-pr", { id: pr.id, strategy: strategy, close_source: close });
      });
    }
    setStatus("");
  }

  function optionList(names, selected) {
    const have = {};
    const out = [];
    (names || []).forEach((n) => {
      if (!n || have[n]) return;
      have[n] = true;
      out.push(n);
    });
    if (selected && !have[selected]) out.unshift(selected);
    return out
      .map((n) => {
        const sel = n === selected ? " selected" : "";
        return (
          '<option value="' + escapeHtml(n) + '"' + sel + ">" + escapeHtml(n) + "</option>"
        );
      })
      .join("");
  }

  function renderCreateForm(payload) {
    lastDetail = null;
    setChrome("form");
    btnBack.hidden = false;
    btnRefresh.hidden = true;
    btnCreate.hidden = true;
    btnBrowser.hidden = true;
    btnCopy.hidden = true;
    const branches = payload.branches || [];
    const src = payload.source || "";
    const dst = payload.destination || "main";
    contextEl.textContent = "New pull request";
    app.innerHTML =
      '<form class="form" id="create-form">' +
      "<label>Title<input name='title' required placeholder='Short summary' value='" +
      escapeHtml(src ? src + " -> " + dst : "") +
      "'></label>" +
      "<label>Description<textarea name='description' placeholder='What does this change?'></textarea></label>" +
      '<div class="form-row">' +
      "<label>Source<select name='source'>" +
      optionList(branches, src) +
      "</select></label>" +
      "<label>Destination<select name='destination'>" +
      optionList(branches, dst) +
      "</select></label></div>" +
      '<label class="check"><input type="checkbox" name="close_source" checked> Close source branch after merge</label>' +
      '<div class="form-actions">' +
      '<button type="button" class="ghost" id="create-cancel">Cancel</button>' +
      '<button type="submit" class="primary">Create pull request</button>' +
      "</div></form>";
    document.getElementById("create-cancel").addEventListener("click", () => {
      emit("back-list");
    });
    document.getElementById("create-form").addEventListener("submit", (ev) => {
      ev.preventDefault();
      const fd = new FormData(ev.target);
      const title = String(fd.get("title") || "").trim();
      if (!title) return;
      setStatus("Creating pull request…");
      emit("create-pr", {
        title: title,
        description: String(fd.get("description") || ""),
        source: String(fd.get("source") || ""),
        destination: String(fd.get("destination") || ""),
        close_source: fd.get("close_source") === "on",
      });
    });
    setStatus("");
  }

  window.BB = {
    setStatus: (msg) => setStatus(msg),
    showError: (msg) => setStatus(msg, "error"),
    renderPrList: renderList,
    renderPrDetail: renderDetail,
    renderCreateForm: renderCreateForm,
    setComments: (payload) => {
      const items = (payload && payload.items) || [];
      if (lastDetail) lastDetail.comments = items;
      const el = document.getElementById("comments-list");
      if (el) el.innerHTML = commentsHtml(items);
    },
    setMembers: (payload) => {
      lastMembers = (payload && payload.items) || [];
      const dl = document.getElementById("member-list");
      if (dl) {
        dl.innerHTML = lastMembers
          .map((m) => '<option value="' + escapeHtml(m.nickname || m.name) + '">')
          .join("");
      }
    },
    setReviewers: (payload) => {
      const items = (payload && payload.items) || [];
      if (lastDetail && lastDetail.pr) lastDetail.pr.reviewers = items;
      const bar = document.getElementById("reviewer-bar");
      if (bar && lastDetail) {
        bar.outerHTML = reviewersHtml(lastDetail.pr);
        const addRev = document.getElementById("btn-add-reviewer");
        const addInp = document.getElementById("add-reviewer");
        if (addRev && addInp) {
          addRev.addEventListener("click", () => {
            const who = addInp.value.trim();
            if (!who) return;
            setStatus("Adding reviewer…");
            emit("add-reviewer", {
              id: lastDetail.pr.id,
              nickname: who,
            });
          });
        }
        app.querySelectorAll(".rev-x").forEach((btn) => {
          btn.addEventListener("click", () => {
            const who = btn.getAttribute("data-uuid");
            if (!who) return;
            setStatus("Removing reviewer…");
            emit("remove-reviewer", {
              id: lastDetail.pr.id,
              uuid: who,
              nickname: who,
            });
          });
        });
      }
    },
  };

  btnBack.addEventListener("click", () => emit("back-list"));
  btnRefresh.addEventListener("click", () =>
    emit("refresh-list", { states: selectedStates })
  );
  btnCreate.addEventListener("click", () => {
    setStatus("Loading branches…");
    emit("create-form");
  });
  btnBrowser.addEventListener("click", () => {
    const url = lastDetail && lastDetail.pr && lastDetail.pr.url;
    emit("open-browser", { url: url || "" });
  });
  btnCopy.addEventListener("click", () => {
    const url = lastDetail && lastDetail.pr && lastDetail.pr.url;
    emit("copy-url", { url: url || "" });
  });

  document.addEventListener("keydown", (ev) => {
    if (ev.target && /INPUT|TEXTAREA|SELECT/.test(ev.target.tagName)) return;
    if (ev.key === "Escape" && page !== "list") emit("back-list");
    if (ev.key === "r" && !ev.metaKey && !ev.ctrlKey && page === "list") {
      emit("refresh-list", { states: selectedStates });
    }
  });

  emit("ready");
})();
