// How long the herd page takes to open on a phone, and that it does not
// wait for what it does not show: a real Emacs serving ghostherd's page
// (herd-open.el, with a herd of 928 sessions and an Inbox of 25), through
// the proxy that does what `tailscale serve' does, over a phone's link --
// slowed to its bandwidth and round trip.  Opened cold (nothing cached)
// and again (a reload: the files asked with their ETags); for each, when
// the Inbox was drawn and what came before it.  Then the Sessions tab,
// which asks for the sessions as it shows: how long until they are
// listed, and the bytes they took.  What Emacs tells the page, past
// what its socket takes at once, comes gzip'd.
//
// LINKS=wifi,cell,poor picks the links timed (cell alone by default);
// TIMES=1 prints each request.  With LOCAL another checkout's local/, its
// page and Emacs code: the checks fail on what waited for the sessions.
// BLOCK=1: Emacs runs in a terminal of its own (tmux), and C-t is pressed
// every 20 ms while the phone opens the page cold and lists the
// sessions; how late Emacs read each says how long it was held -- a
// send to the proxy's socket holds it with no key read (xwapp.el, "A big
// answer goes from a thread").
const fs = require("fs");
const path = require("path");
const { spawn, execFileSync } = require("child_process");
const { start, watch, BOX, PHONE, until, sleep } = require("./appslib");

const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

const LINKS = {
  wifi: { kbps: 20000, rtt: 40 },
  cell: { kbps: 5000, rtt: 120 },
  poor: { kbps: 1500, rtt: 250 },
};
const kb = (b) => (b / 1024).toFixed(1);
// What the proxy's socket takes at once (xwapp--send-aside, --gzip-next-min).
const SOCKET = 8 * 1024;

// What Emacs noted: each request, intent, sidecar call and `next' answered.
const emacsLog = (dir) => {
  try {
    return fs.readFileSync(path.join(dir, "log.jsonl"), "utf8").trim().split("\n").filter(Boolean).map(JSON.parse);
  } catch (_) { return []; }
};

// Emacs in a terminal of its own, for BLOCK; and C-t pressed in it.
// Not compiling Emacs's own Lisp meanwhile, as a fresh Emacs in a
// terminal would (native-comp-jit-compilation), each loaded with Emacs
// held a while: batch does not, nor does your Emacs, long since done.
const TMUX = "xwapp-e2e-herd-open-" + process.pid;
let keyboard = null;
const inTerminal = (env, el) => {
  const vars = ["E2E_DIR", "E2E_PORT", "E2E_LOCAL"].map((k) => k + "='" + env[k] + "'").join(" ");
  execFileSync("tmux", ["-L", TMUX, "-f", "/dev/null", "new-session", "-d", "-s", "e2e", "-x", "100", "-y", "30",
    `env ${vars} ${process.env.EMACS || "emacs"} -nw -Q --eval '(setq native-comp-jit-compilation nil)' -l '${el}'`]);
  // One client in control mode types every key: a tmux started for each
  // would take its own few milliseconds.
  keyboard = spawn("tmux", ["-L", TMUX, "-C", "attach", "-t", "e2e"], { stdio: ["pipe", "ignore", "ignore"] });
  return { kill: () => {
    try { keyboard.stdin.end(); } catch (_) {}
    try { execFileSync("tmux", ["-L", TMUX, "kill-server"]); } catch (_) {}
  } };
};
// Press C-t every 20 ms until stop(); then how late each was read: [{at, late}].
function pressing(dir) {
  const sent = [];
  let on = true;
  (async () => {
    while (on) {
      sent.push(Date.now());
      keyboard.stdin.write("send-keys -t e2e C-t\n");
      await sleep(20);
    }
  })();
  return async () => {
    on = false;
    const file = path.join(dir, "keys.log");
    await until(() => fs.existsSync(file) && fs.readFileSync(file, "utf8").trim().split("\n").length >= sent.length, 5000);
    const read = fs.existsSync(file) ? fs.readFileSync(file, "utf8").trim().split("\n").map((l) => Math.round(parseFloat(l) * 1000)) : [];
    fs.rmSync(file, { force: true });
    return sent.map((at, i) => ({ at, late: read[i] ? read[i] - at : null }));
  };
}
const worst = (keys) => keys.reduce((w, k) => (k.late !== null && k.late > w.late ? k : w), { late: -1, at: 0 });
const median = (keys) => { const l = keys.map((k) => k.late).filter((x) => x !== null).sort((a, b) => a - b); return l[Math.floor(l.length / 2)]; };

// The page loaded -- PAGE reloaded, or a new one of CTX -- until its
// Inbox is drawn: when each request ended, and the page's own marks.
async function open(s, ctx, page) {
  const again = !!page;
  page = page || (await ctx.newPage());
  const reqs = [];
  const t0 = Date.now();
  const short = (u) => u.replace(/^https?:\/\/[^/]+\/[0-9a-f]{32}\//, "/").replace(/\?.*$/, "");
  const ended = (r) => {
    const at = Date.now() - t0;
    reqs.push(Promise.all([r.sizes().catch(() => null), r.response().catch(() => null)]).then(([sz, rs]) => ({
      url: short(r.url()), method: r.method(), at, status: rs ? rs.status() : 0,
      bytes: sz ? sz.responseBodySize : 0, enc: rs ? rs.headers()["content-encoding"] || "" : "",
    })));
  };
  page.on("requestfinished", ended);
  if (again) await page.reload({ waitUntil: "commit" });
  else await page.goto(BOX + "/" + s.secretOf("herd") + "/ghostherd/ui/index.html", { waitUntil: "commit" });
  // Marks, by the page's own clock, from its first parse on.
  await page.evaluate(() => {
    const m = (window.__marks = {});
    const at = () => Math.round(performance.timeOrigin + performance.now());
    const look = () => {
      if (!m.inbox && document.querySelector(".ibcard")) m.inbox = at();
      if (!m.zero && document.querySelector(".ibempty:not(.ibcoming)")) m.zero = at();
      if (!m.coming && document.querySelector(".ibcoming")) m.coming = at();
      const n = document.getElementById("sessions-n");
      if (!m.count && n && n.textContent) m.count = at();
    };
    look();
    new MutationObserver(look).observe(document, { subtree: true, childList: true, characterData: true });
  });
  const drawn = await until(() => page.evaluate(() => !!(window.__marks && window.__marks.inbox)), 30000);
  await sleep(400);
  page.off("requestfinished", ended);
  const marks = await page.evaluate(() => Object.assign({}, window.__marks, {
    dcl: Math.round(performance.timeOrigin + performance.getEntriesByType("navigation")[0].domContentLoadedEventEnd) }));
  const rel = (t) => (t ? t - t0 : null);
  const done = (await Promise.all(reqs)).sort((a, b) => a.at - b.at);
  const inbox = rel(marks.inbox);
  const before = done.filter((q) => q.at <= inbox);
  return { page, drawn, inbox, dcl: rel(marks.dcl), zero: rel(marks.zero), coming: rel(marks.coming), count: rel(marks.count),
           reqs: done, before, nextBefore: before.filter((q) => /\/next$/.test(q.url)).reduce((a, q) => a + q.bytes, 0) };
}

function show(name, r) {
  console.log(`     ${name}: DOMContentLoaded ${r.dcl} ms · Inbox drawn ${r.inbox} ms · ` +
              `${r.before.length} requests, ${kb(r.before.reduce((a, q) => a + q.bytes, 0))} KB before it ` +
              `(${kb(r.nextBefore)} KB of next)`);
  if (process.env.TIMES)
    r.reqs.forEach((q) => console.log(`       ${String(q.at).padStart(6)} ${q.method.padEnd(4)} ${String(q.status).padEnd(3)} ` +
                                      `${kb(q.bytes).padStart(7)} KB ${q.enc.padEnd(4)} ${q.url}`));
}

(async () => {
  const names = (process.env.LINKS || "cell").split(",");
  const block = !!process.env.BLOCK;
  for (const name of names) {
    const s = await start(Object.assign({ el: "herd-open.el", link: LINKS[name] }, block ? { spawn: inTerminal } : {}));
    try {
      const ctx = await s.browser.newContext(PHONE);
      const errors = [];
      let idle = null, held = null;
      if (block) {
        await sleep(1000);
        const stop = pressing(s.dir);
        await sleep(1000);
        idle = await stop();
        console.log(`     ${name} idle: key read ${median(idle)} ms late (median), ${worst(idle).late} ms at worst`);
      }
      const stopCold = block ? pressing(s.dir) : null;
      const cold = await open(s, ctx);
      watch(cold.page, errors);
      show(name + " cold", cold);
      if (block) {
        held = await stopCold();
        const w = worst(held);
        console.log(`     ${name} cold: key read ${median(held)} ms late (median), ${w.late} ms at worst ` +
                    `(${w.at - held[0].at} ms into the load); ${held.filter((k) => k.late > 50).length} of ${held.length} past 50 ms`);
      }
      await sleep(500);
      const warm = await open(s, ctx, cold.page);
      show(name + " again", warm);

      // ---- The Sessions tab: its list, asked as it shows ----
      const page = warm.page;
      const loaded = emacsLog(s.dir);
      const badge = await page.$eval("#sessions-n", (n) => n.textContent);
      const stopList = block ? pressing(s.dir) : null;
      const tapped = Date.now();
      await page.tap('.tab[data-tab="sessions"]');
      const coming = await page.$eval("#sess-list", (b) => b.textContent).catch(() => "");
      const listed = await until(() => page.$eval("#sess-list .sum", (b) => b.textContent === "928 of 928 sessions").catch(() => false), 15000);
      const listedIn = Date.now() - tapped;
      const list = s.traffic.filter((t) => /\/next\?/.test(t.path) && t.end >= tapped)
        .reduce((big, t) => (t.bytes > (big ? big.bytes : -1) ? t : big), null);
      console.log(`     ${name} Sessions: listed ${listedIn} ms after the tap · the list ${kb(list ? list.bytes : 0)} KB on the wire` +
                  `${list && list.enc ? " (" + list.enc + ")" : " (plain)"}`);
      if (block) {
        await sleep(300);
        const keys = await stopList();
        const w = worst(keys);
        console.log(`     ${name} Sessions: key read ${median(keys)} ms late (median), ${w.late} ms at worst; ` +
                    `${keys.filter((k) => k.late > 50).length} of ${keys.length} past 50 ms`);
        held = held.concat(keys);
      }
      if (name !== names[0]) continue;

      // ---- Opening on the Inbox waits for nothing it does not show ----
      for (const [when, r] of [["cold", cold], ["again", warm]]) {
        check(`${name} ${when}: the Inbox is drawn`, r.drawn);
        check(`${name} ${when}: no session list comes before the Inbox (next ≤ 64 KB before it)`,
          r.nextBefore <= 64 * 1024, { nextKB: kb(r.nextBefore) });
        check(`${name} ${when}: the Inbox says it is on its way, not "Inbox zero", until it comes`,
          r.coming !== null && r.zero === null, { coming: r.coming, zero: r.zero, inbox: r.inbox });
      }
      const lists = loaded.filter((e) => e.what.rpc === "memory_list");
      check(`${name}: Emacs listed the sessions by their count alone for both loads`,
        lists.length === 2 && lists.every((e) => e.what.limit === 1), lists.map((e) => e.what.limit));
      const ops = loaded.filter((e) => e.what.intent).map((e) => e.what.intent);
      const first = ops.slice(ops.lastIndexOf("xwapp-seen") + 1);
      check(`${name}: the Inbox is the first thing the page asks of Emacs`, first[0] === "inbox-watch", first.slice(0, 4));
      check(`${name}: the Sessions tab's badge is their count`, badge === "928", badge);

      // ---- The Sessions tab asks for them as it shows ----
      check(`${name}: Sessions says the list is on its way`, /Listing the sessions/.test(coming), coming.slice(0, 60));
      check(`${name}: Sessions lists all 928`, listed);
      await page.tap('.tab[data-tab="inbox"]');
      await sleep(300);
      await page.tap('.tab[data-tab="search"]');
      await sleep(300);
      check(`${name}: Search lists the recent ones, from the list it has`,
        (await page.$$eval("#results .srow", (r) => r.length)) === 12);
      await page.tap('.tab[data-tab="sessions"]');
      await sleep(800);
      const all = emacsLog(s.dir).filter((e) => e.what.rpc === "memory_list" && e.what.limit === 5000);
      check(`${name}: asked for once, not again on each tab shown`, all.length === 1, all.length);

      // ---- What Emacs tells the page comes gzip'd past its socket ----
      const nexts = s.traffic.filter((t) => /\/next\?/.test(t.path) && t.status === 200);
      const plainBig = nexts.filter((t) => !t.enc && t.bytes >= SOCKET).map((t) => t.bytes);
      check(`${name}: no answer to next of ${SOCKET / 1024} KB or more goes plain through the proxy`, plainBig.length === 0, plainBig);
      check(`${name}: the session list comes gzip'd, under 100 KB on the wire`,
        list && list.enc === "gzip" && list.bytes < 100 * 1024, list && { enc: list.enc, kb: kb(list.bytes) });
      check(`${name}: no page errors`, errors.length === 0, errors);
      if (block) {
        const w = worst(held);
        check(`${name}: Emacs reads a key within 100 ms all the while the phone loads (worst ${w.late} ms)`,
          w.late >= 0 && w.late < 100, w);
      }
    } finally {
      await s.stop();
    }
  }
  const failed = results.filter((x) => !x).length;
  console.log(`\n${results.length - failed}/${results.length} passed`);
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
