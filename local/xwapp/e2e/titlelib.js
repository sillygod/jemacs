// Emacs at the desk, reading a page's title as xwapp.el does: WebKit's
// copy of it -- its first 1000 UTF-16 units -- a title equal to the last
// one read ignored, parts strung together, and each title read answered
// by its intent's n and its part's i, which the kit clears only while it
// is that one (see "A title read is answered" in xwapp.el):
//
//   if (!(window.XW && XW.kit && XW.kit(CALL))) document.title = IDLE;
//
// OPTS:
//   ms          how long to read, at most (20 s)
//   got         an array to gather what Emacs got in, as it comes
//   idle        the title Emacs puts back
//   refuse(t)   true for a title to take as one Emacs cannot read
//   deaf()      true while Emacs reads nothing -- busy
//   stall(it)   ms to wait between reading intent IT (a part, or one of
//               its own) and answering it: busy after reading
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// The title's (n, i), as xwapp--title-id finds them: from IT, what it
// was read as, or from the start of JSON when it could not be read.
function titleId(json, it) {
  if (it) {
    const part = it.xwpart !== undefined;
    const n = part ? it.xwpart : it.n;
    const i = part ? it.i : 0;
    return Number.isInteger(n) && Number.isInteger(i) ? [n, i] : null;
  }
  let m = /^\{"xwpart":(\d+),"i":(\d+),/.exec(json);
  if (m) return [+m[1], +m[2]];
  m = /^\{"op":"(?:[^"\\]|\\.)*","n":(\d+)[,}]/.exec(json);
  return m ? [+m[1], 0] : null;
}

// What Emacs got, whole -- or {unread: LENGTH} for a title it could not
// read -- and every title it read, as the page set it.
async function readTitles(page, prefix, until, opts) {
  opts = opts || {};
  const got = opts.got || [], titles = [], parts = {};
  let last = null;
  const end = Date.now() + (opts.ms || 20000);
  while (Date.now() < end && !until(got)) {
    const full = await page.title();
    const t = full.slice(0, 1000);
    if (t.startsWith(prefix) && t !== last && !(opts.deaf && opts.deaf())) {
      last = t;
      titles.push(full);
      let it = null;
      try { it = JSON.parse(t.slice(prefix.length)); } catch (e) { it = null; }
      if (opts.refuse && opts.refuse(t)) it = null;
      const id = titleId(t.slice(prefix.length), it);
      let whole = null;
      if (!it) got.push({ unread: t.length });
      else if (it.xwpart === undefined) whole = it;
      else {
        const p = (parts[it.xwpart] = parts[it.xwpart] || []);
        p[it.i] = it.s;
        if (p.filter((x) => x !== undefined).length === it.n) {
          whole = JSON.parse(p.join(""));
          delete parts[it.xwpart];
        }
      }
      // Acted on as read; the answer is a script that lands after.
      if (whole) got.push(whole);
      const call = Object.assign({ xwapp: it ? "read" : "unread" }, id ? { n: id[0], i: id[1] } : {},
        whole ? { whole: true } : {}, { idle: opts.idle || "Idle" });
      const wait = it && opts.stall ? opts.stall(it) : 0;
      if (wait) await sleep(wait);
      await page.evaluate((c) => { if (!(window.XW && XW.kit && XW.kit(c))) document.title = c.idle; }, call);
      continue;
    }
    await sleep(5);
  }
  return { got, titles };
}

module.exports = { readTitles, titleId };
