/* jwebkit markdown viewer: GFM via marked, ```mermaid via mermaid.run().
   Loaded as a classic script (UMD libs) for older system WebKit. */
(function () {
  'use strict';

  var content = document.getElementById('content');

  function darkMode() {
    return !!(window.matchMedia &&
              window.matchMedia('(prefers-color-scheme: dark)').matches);
  }

  function status(msg, isError) {
    content.innerHTML = '';
    var p = document.createElement('p');
    p.className = isError ? 'jw-error' : 'jw-status';
    p.textContent = msg;
    content.appendChild(p);
  }

  function srcUrl() {
    var q = new URLSearchParams(window.location.search).get('src');
    if (!q) return null;
    // Absolute path on our loopback origin, or a full same-origin URL.
    if (q.charAt(0) === '/') return q;
    try {
      var u = new URL(q, window.location.origin);
      if (u.origin === window.location.origin) return u.pathname + u.search;
    } catch (e) {}
    return null;
  }

  function escapeHtml(s) {
    return String(s)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;');
  }

  function setupMarked() {
    if (!window.marked) throw new Error('marked failed to load');
    // marked 11+: code renderer receives a token {text, lang, ...}.
    var renderer = {
      code: function (token) {
        var lang = (token && token.lang) || '';
        var text = token && token.text != null ? token.text : '';
        var info = String(lang).split(/\s+/)[0];
        if (info === 'mermaid') {
          return '<pre class="mermaid">' + escapeHtml(text) + '</pre>';
        }
        // Fall back to marked's default code rendering.
        var langClass = info ? ' class="language-' + escapeHtml(info) + '"' : '';
        return '<pre><code' + langClass + '>' + escapeHtml(text) + '</code></pre>\n';
      }
    };
    marked.use({ gfm: true, breaks: false, renderer: renderer });
  }

  function setupMermaid() {
    if (!window.mermaid) throw new Error('mermaid failed to load');
    mermaid.initialize({
      startOnLoad: false,
      // Older WebKit: avoid the newer look-and-feel edge cases.
      securityLevel: 'strict',
      theme: darkMode() ? 'dark' : 'default'
    });
  }

  function render(md) {
    setupMarked();
    setupMermaid();
    content.innerHTML = marked.parse(md || '');
    return mermaid.run({
      querySelector: '.mermaid',
      suppressErrors: true
    }).catch(function (err) {
      console.error('mermaid.run', err);
    });
  }

  function load() {
    var src = srcUrl();
    if (!src) {
      status('Missing ?src=/md/TOKEN', true);
      return;
    }
    fetch(src, { cache: 'no-store' })
      .then(function (res) {
        if (!res.ok) throw new Error('HTTP ' + res.status + ' for ' + src);
        return res.text();
      })
      .then(function (md) { return render(md); })
      .catch(function (err) {
        status(String(err && err.message ? err.message : err), true);
      });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', load);
  } else {
    load();
  }
})();
