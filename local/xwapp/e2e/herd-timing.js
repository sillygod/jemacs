// The herd page's timings, as a phone's are kept (ask fd9e5442): a real
// Emacs serving ghostherd's page (herd-open.el) through the proxy that
// plays `tailscale serve', over a phone's link.  The page times each of
// its loads and wakes; Emacs joins that with its own times of each
// request and keeps the record -- every one in the run's timings.jsonl,
// the slow ones also as lines of the herd log, of kind timing.
//
//   - a quick open (wifi): a record, not slow, ended by the Inbox drawn;
//     nothing in the Log
//   - a nap and a wake: a wake record, its next asked with wake=1 and
//     answered at once; and a wake while Emacs is busy: slow, Emacs busy
//   - a slow link (poor): a slow open, its time the network's, in the Log
//     with its bar, its phases, and among the Issues
//   - Emacs busy when the hello lands (the proxy holds the hello while
//     Emacs is made to run Lisp 2.5 s): a slow open, its time Emacs busy
//   - the secret: in no record, no timing the page sent, nothing the Log
//     shows -- and the 32 hex digits a secret is, in none of the first two
//
// TIMES=1 prints each record's breakdown.
const fs = require("fs");
const path = require("path");
const { start, watch, phoneEyes, BOX, PHONE, until, sleep } = require("./appslib");
const { settled } = require("./wakelib");

const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

const LINKS = { wifi: { kbps: 20000, rtt: 40 }, poor: { kbps: 1500, rtt: 250 } };
const STALL = 2500;

// What Emacs kept, in the order it kept them.
const records = (dir) => {
  try {
    return fs.readFileSync(path.join(dir, "timings.jsonl"), "utf8").trim().split("\n").filter(Boolean).map(JSON.parse);
  } catch (_) { return []; }
};
// The first N records of KIND, once there are N.
async function kept(s, kind, n = 1) {
  await until(() => records(s.dir).filter((r) => r.kind === kind).length >= n, 20000);
  return records(s.dir).filter((r) => r.kind === kind);
}
// What the moment waited on most.
const most = (r) => r && Object.entries(r.sum).sort((a, b) => b[1] - a[1])[0][0];
const show = (what, r) => {
  if (!r) return;
  console.log(`     ${what}: ${r.kind} ${r.d} s${r.slow ? " (slow)" : ""} · ` +
              Object.entries(r.sum).filter(([, v]) => v > 0).map(([k, v]) => k + " " + v).join(" · "));
  if (process.env.TIMES) r.reqs.forEach((q) => console.log("       " + JSON.stringify(q)));
};

// The page's own timings, as it sent them.
function sentTimings(page, into) {
  page.on("request", (q) => {
    const body = q.postData() || "";
    if (/\/intent\?/.test(q.url()) && /"xwapp-timing"/.test(body)) into.push(body);
  });
}

async function openHerd(s, ctx, sent, errors) {
  const page = await ctx.newPage();
  sentTimings(page, sent);
  watch(page, errors);
  await page.goto(BOX + "/" + s.secretOf("herd") + "/ghostherd/ui/index.html", { waitUntil: "commit" });
  return page;
}

// The Log tab's rows, as drawn.
async function logRows(page) {
  if (!(await page.$('.tab[data-tab="log"].on, .tab[data-tab="log"][aria-selected="true"]'))) await page.tap('.tab[data-tab="log"]');
  await until(() => page.$eval("#log-list", (b) => !!b.querySelector(".sum") ||
    !!(b.querySelector(".empty") && !/Reading/.test(b.textContent))).catch(() => false), 5000);
  return page.$$eval("#log-list .lrow", (rs) => rs.map((r) => ({
    id: r.getAttribute("data-id"), text: (r.querySelector(".lx") || r).textContent,
    timing: !!r.querySelector(".k-timing"), bar: r.querySelectorAll(".tbar .tseg").length,
  })));
}

// Hidden, asleep a while with what it had under way, shown again: a
// phone woken.  The page's word that it is hidden goes before it sleeps.
async function napAndWake(s, page, ms) {
  await settled(page);
  await page.evaluate(() => __vis("hidden"));
  await sleep(300);
  s.nap();
  await sleep(ms);
  await page.evaluate(() => __vis("visible"));
  s.wake("hang");
}

// Made busy: Emacs runs Lisp for STALL ms just as the request PRED takes
// reaches it.
const busyAt = (s, pred) => s.holdAt(pred, () => s.askEmacs("POST", "/e2e-stall?ms=" + STALL).then(() => sleep(150)));

(async () => {
  const sent = [], logText = [], secrets = [], kept_ = [];
  const errors = [];

  // ---- A quick open, a wake, a wake while Emacs is busy ----
  {
    const s = await start({ el: "herd-open.el", link: LINKS.wifi });
    secrets.push(s.secretOf("herd"));
    try {
      const ctx = await s.browser.newContext(PHONE);
      await phoneEyes(ctx);
      const page = await openHerd(s, ctx, sent, errors);
      const [open] = await kept(s, "open");
      show("wifi open", open);
      check("a quick open: one record of it", !!open, records(s.dir).map((r) => r.kind));
      check("not slow: under 1.5 s", open && !open.slow && open.d < 1.5, open && open.d);
      check("ended by the Inbox drawn (XW.ready)", open && open.end === "ready" && "inbox" in open.marks, open && open.marks);
      check("its marks in order: boot, hello, inbox", open && open.marks.boot < open.marks.hello &&
        open.marks.hello <= open.marks.inbox && open.marks.dcl > 0, open && open.marks);
      const reqs = (open && open.reqs) || [];
      check("its page, files and hello listed, each joined with Emacs's own",
        ["ghostherd/ui/index.html", "ghostherd/ui/app.js", "xwapp/ui/xwapp.js", "hello"].every((p) =>
          reqs.some((q) => q.p === p && q.code >= 200 && q.code < 300)) && !reqs.some((q) => q.unmatched),
        reqs.map((q) => q.p + ":" + (q.code || "?")));
      check("its clocks lined up: one machine, the offset nil", open && Math.abs(open.offset) < 0.02, open && open.offset);
      const rows = await logRows(page);
      check("nothing slow, nothing in the Log", !records(s.dir).some((r) => r.slow) && !rows.some((r) => r.timing),
        rows.filter((r) => r.timing));

      // A nap, and the page shown again.
      const from = s.traffic.length;
      await napAndWake(s, page, 2500);
      const [wake] = await kept(s, "wake");
      show("wake", wake);
      const asked = s.traffic.slice(from).filter((t) => /\/next\?.*&wake=1/.test(t.path));
      check("woken, it asks with wake=1, answered at once",
        asked.length >= 1 && asked[0].status === 200 && asked[0].end - asked[0].at < 500,
        asked.map((t) => ({ status: t.status, ms: t.end - t.at })));
      check("a wake record: shown again to that answer, not slow, away 2.5 s",
        wake && wake.end === "next" && wake.why === "visible" && !wake.slow && wake.away >= 2.5 && wake.away < 4, wake);
      check("its next listed, joined", wake && wake.reqs.some((q) => q.p === "next" && q.code === 200), wake && wake.reqs);

      // Again, Emacs busy as the page wakes.
      busyAt(s, (r) => r.method === "GET" && /\/next$/.test(r.path) && /wake=1/.test(r.query));
      await napAndWake(s, page, 2500);
      const wakes = await kept(s, "wake", 2);
      const busy = wakes[1];
      show("busy wake", busy);
      check("woken while Emacs is busy: slow, its time Emacs busy",
        busy && busy.slow && most(busy) === "busy" && busy.sum.busy >= 2.0 && busy.sum.net < 0.5, busy && busy.sum);
      const next = busy && busy.reqs.find((q) => q.p === "next");
      check("its next waited for Emacs, not on the network", next && next.busy >= 2.0, next);
      await sleep(500);
      const rows2 = await logRows(page);
      const line = rows2.find((r) => r.timing);
      check("in the Log, as it happened: a slow wake, Emacs busy, with its bar",
        line && /^slow wake [\d.]+ s: Emacs busy/.test(line.text) && line.bar >= 1, rows2.slice(0, 3));
      logText.push(rows2.map((r) => r.text).join("\n"));
      kept_.push(fs.readFileSync(path.join(s.dir, "timings.jsonl"), "utf8"), fs.readFileSync(path.join(s.dir, "timings.eld"), "utf8"));
    } finally {
      await s.stop();
    }
  }

  // ---- A slow link ----
  {
    const s = await start({ el: "herd-open.el", link: LINKS.poor });
    secrets.push(s.secretOf("herd"));
    try {
      const ctx = await s.browser.newContext(PHONE);
      const page = await openHerd(s, ctx, sent, errors);
      const [open] = await kept(s, "open");
      show("poor open", open);
      check("over a slow link: a slow open", open && open.slow && open.d >= 1.5, open && open.d);
      check("its time the network's, Emacs not busy",
        open && most(open) === "net" && open.sum.busy < 0.3, open && open.sum);
      const app = open && open.reqs.find((q) => q.p === "ghostherd/ui/app.js");
      check("app.js came gzip'd, its download the network's", app && app.z && app.dl > 0.2 && app.net > 0, app);
      const rows = await logRows(page);
      const line = rows.find((r) => r.timing);
      check("in the Log: a slow open, the network first, with its bar",
        line && /^slow open [\d.]+ s: network/.test(line.text) && line.bar >= 1, rows.slice(0, 3));
      const issues = await page.$eval('.log-tools [data-act="issues-only"]', (b) => b.textContent);
      check("among the Issues", /Issues\s*1/.test(issues), issues);
      await page.tap('.lrow[data-id="' + line.id + '"] [data-act="screen"]');
      const phases = await page.$eval("pre.screen", (p) => p.textContent).catch(() => "");
      check("its phases, request by request, a tap away",
        /^open [\d.]+ s, slow/.test(phases) && /app\.js/.test(phases) && /hello#\d/.test(phases), phases.slice(0, 200));
      const wide = await page.evaluate(() => document.documentElement.scrollWidth > document.documentElement.clientWidth + 1);
      check("the phone's page does not scroll sideways for it", !wide);
      logText.push(rows.map((r) => r.text).join("\n") + "\n" + phases);
      kept_.push(fs.readFileSync(path.join(s.dir, "timings.jsonl"), "utf8"), fs.readFileSync(path.join(s.dir, "timings.eld"), "utf8"));
    } finally {
      await s.stop();
    }
  }

  // ---- Emacs busy when the hello lands ----
  {
    const s = await start({ el: "herd-open.el", link: LINKS.wifi });
    secrets.push(s.secretOf("herd"));
    try {
      const ctx = await s.browser.newContext(PHONE);
      busyAt(s, (r) => r.method === "POST" && /\/hello$/.test(r.path));
      await openHerd(s, ctx, sent, errors);
      const [open] = await kept(s, "open");
      show("busy open", open);
      check("Emacs busy as the hello lands: a slow open", open && open.slow, open && open.d);
      check("its time Emacs busy, not the network",
        open && most(open) === "busy" && open.sum.busy >= 2.0 && open.sum.net < 1.0, open && open.sum);
      const hello = open && open.reqs.find((q) => q.p === "hello");
      check("the hello waited for Emacs", hello && hello.busy >= 2.0, hello);
      kept_.push(fs.readFileSync(path.join(s.dir, "timings.jsonl"), "utf8"), fs.readFileSync(path.join(s.dir, "timings.eld"), "utf8"));
    } finally {
      await s.stop();
    }
  }

  // ---- No secret anywhere ----
  const all = sent.join("\n") + "\n" + kept_.join("\n") + "\n" + logText.join("\n");
  check("the secret is in nothing kept, sent or shown", secrets.length === 3 && sent.length >= 4 &&
    !secrets.some((sec) => all.toLowerCase().includes(sec.toLowerCase())), { sent: sent.length });
  check("nor 32 hex digits, which a secret is, in what was sent or kept",
    !/[0-9a-f]{32}/i.test(sent.join("\n") + kept_.join("\n")));
  check("no page errors", errors.length === 0, errors);

  const failed = results.filter((x) => !x).length;
  console.log(`\n${results.length - failed}/${results.length} passed`);
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
