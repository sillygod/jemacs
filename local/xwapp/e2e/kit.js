// The xwapp page kit on its own: XW.sanitizer's options and XW.bridge's
// queue, through a page that loads it the way the apps do.
const path = require("path");
const { webkit } = require("playwright");

const UI = "file://" + path.join(process.env.LOCAL || path.resolve(__dirname, "../.."), "pr-view", "ui", "index.html");
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const results = [];
function check(name, ok, detail) {
  results.push(ok);
  console.log((ok ? "PASS " : "FAIL ") + name + (detail ? "  — " + detail : ""));
}

(async () => {
  const browser = await webkit.launch();
  const page = await browser.newPage();
  await page.goto(UI);
  await sleep(200);

  const r = await page.evaluate(() => {
    const plain = XW.sanitizer();
    const pics = XW.sanitizer({ images: true });
    const cu = XW.sanitizer({
      images: true,
      classes: /^(cu-[a-z-]+|language-[\w+#-]+)$/,
      attrs: { A: ["data-task"] },
      checks: { "data-task": (v) => /^[0-9a-z]+$/.test(v) },
    });
    const hostile = '<p onclick="x()">hi <script>window.p1=1</script><img src="https://evil/a.png" onerror="window.p2=1">' +
      '<a href="javascript:window.p3=1">js</a> <a href="https://ok/">ok</a> <code class="language-js evil">c</code>' +
      '<iframe src="https://evil/"></iframe><span style="color:red" class="cu-x">s</span></p>';
    return {
      plain: plain(hostile),
      pics: pics('<img src="https://ok/a.png" alt="a"><img src="javascript:x"><img src="data:image/png;base64,AA==">'),
      cu: cu('<a href="https://app.clickup.com/t/abc1" data-task="abc1">t</a><a href="https://x" data-task="A B">u</a><span class="cu-mention evil">m</span>'),
      frozen: Object.isFrozen(XW),
    };
  });
  check("default drops script, handlers, iframe, images", !/script|onclick|onerror|iframe|<img/i.test(r.plain), r.plain);
  check("default unwraps a javascript: link, keeps https", !/javascript/.test(r.plain) && /<a href="https:\/\/ok\/">ok<\/a>/.test(r.plain), r.plain);
  check("default keeps only language-* classes, no styles", /<code class="language-js">/.test(r.plain) && !/style=|cu-x|evil/.test(r.plain), r.plain);
  check("images: https and data: kept, javascript: src dropped", (r.pics.match(/<img/g) || []).length === 3 &&
    /src="https:\/\/ok\/a\.png"/.test(r.pics) && /src="data:image\/png/.test(r.pics) && !/javascript/.test(r.pics) &&
    /loading="lazy"/.test(r.pics), r.pics);
  check("extra attrs pass their checks only", /data-task="abc1"/.test(r.cu) && !/data-task="A B"/.test(r.cu) &&
    /<span class="cu-mention">/.test(r.cu), r.cu);
  check("XW is frozen", r.frozen);

  // The bridge: three intents in one tick all reach Emacs, in order, the
  // way xwapp reads them (ignore a title equal to the last, then reset).
  const got = [];
  let last = null;
  const reader = (async () => {
    for (let i = 0; i < 60; i++) {
      const t = await page.title();
      if (t.startsWith("prview:") && t !== last) {
        last = t;
        got.push(JSON.parse(t.slice(7)));
        await page.evaluate(() => { document.title = "PR View"; });
      }
      await sleep(60);
    }
  })();
  await page.evaluate(() => {
    const emit = XW.bridge("prview:");
    emit("one");
    emit("one");
    emit("two", { x: 1 });
  });
  await reader;
  const ops = got.filter((i) => ["one", "two"].includes(i.op));
  check("three quick intents, none lost, in order, each new", ops.map((i) => i.op).join() === "one,one,two" &&
    ops[0].n < ops[1].n && ops[1].n < ops[2].n && ops[2].x === 1, JSON.stringify(ops));

  // A long intent.  Emacs reads the title WebKit keeps, which is its
  // first 1000 UTF-16 units with runs of spaces folded (the page's own
  // document.title folds them too), so the bridge sends one that would
  // be longer in parts.  Read here as xwapp reads them, from the title
  // cut as WebKit cuts it, and strung back together.  The text is made up.
  async function readLikeEmacs(prefix, until, opts) {
    opts = opts || {};
    const got = [], titles = [], parts = {};
    let last = null;
    const end = Date.now() + (opts.ms || 20000);
    while (Date.now() < end && !until(got)) {
      const full = await page.title();
      const t = full.slice(0, 1000);
      if (t.startsWith(prefix) && t !== last && !opts.deaf) {
        last = t;
        titles.push(full);
        let it = null;
        try { it = JSON.parse(t.slice(prefix.length)); } catch (e) { it = null; }
        if (!it) {
          got.push({ unread: t.length });
          await page.evaluate(() => { document.title = "PR View"; XW.kit({ xwapp: "unread" }); });
          continue;
        }
        await page.evaluate(() => { document.title = "PR View"; });
        if (it.xwpart === undefined) got.push(it);
        else {
          const p = (parts[it.xwpart] = parts[it.xwpart] || []);
          p[it.i] = it.s;
          if (p.filter((x) => x !== undefined).length === it.n) got.push(JSON.parse(p.join("")));
        }
      }
      await sleep(5);
    }
    return { got, titles };
  }
  await page.evaluate(() => {
    window.LONG = XW.bridge("kitlong:");
    window.LOST = [];
    const u = "\u9577\u3044\u4e2d\u6587  \u0020 \"q\" \\ \u{1F600}\n    \u3000";
    let s = "";
    while (s.length < 20000) s += u;
    window.WANT = s.slice(0, 20000).replace(/[\ud800-\udbff]$/, "x");
  });
  await sleep(100);
  const longRead = readLikeEmacs("kitlong:", (g) => g.some((i) => i.op === "after"));
  const emitted = await page.evaluate(() => [LONG("before"), LONG("agent-prompt", { name: "a", text: WANT }, () => LOST.push("long")),
    LONG("after", { note: "a  b" })]);
  const lr = await longRead;
  const want = await page.evaluate(() => WANT);
  const longOps = lr.got.filter((i) => ["xwapp-hello", "before", "agent-prompt", "after"].includes(i.op) && i.op !== "xwapp-hello");
  const prompt = lr.got.find((i) => i.op === "agent-prompt");
  check("a 20,000-character prompt goes whole through titles cut at 1000, between the ones around it",
    emitted.join() === "true,true,true" && longOps.map((i) => i.op).join() === "before,agent-prompt,after" &&
    prompt && prompt.text === want && prompt.name === "a", JSON.stringify({ ops: longOps.map((i) => i.op), len: prompt && prompt.text.length }));
  const partTitles = lr.titles.filter((t) => t.indexOf('"xwpart"') > 0);
  check("in parts, each title within 800 UTF-16 units, none with a run of spaces for WebKit to fold",
    partTitles.length > 25 && lr.titles.every((t) => t.length <= 800 && !/ {2}/.test(t)),
    JSON.stringify({ parts: partTitles.length, longest: Math.max(...lr.titles.map((t) => t.length)) }));
  check("its runs of spaces arrive whole", lr.got.find((i) => i.op === "after").note === "a  b" &&
    /\u3000/.test(prompt ? prompt.text : "") && (prompt ? prompt.text : "").indexOf("  \u0020 ") > 0);
  const shortT = lr.titles.find((t) => t.indexOf('"op":"before"') > 0);
  check("a short intent is one title, prefix and JSON, as before", !!shortT &&
    JSON.stringify(JSON.parse(shortT.slice(8))) === shortT.slice(8) && !/xwpart/.test(shortT), shortT);
  check("nothing lost", (await page.evaluate(() => LOST)).length === 0);

  // Past INTENT_MAX it is not sent at all, and emit says so.
  const tooLong = await page.evaluate(() => [LONG("huge", { text: "x".repeat(2 * 1024 * 1024 + 1) }), document.title]);
  await sleep(150);
  check("an intent past 2M characters is refused, and nothing sent", tooLong[0] === false && !/huge/.test(await page.title()), tooLong[0]);

  // A title Emacs cannot read -- one an older page cut -- is said to be,
  // and the intent it was is lost: its LOST is called, the next goes.
  const cutRead = readLikeEmacs("kitlong:", (g) => g.some((i) => i.op === "next-one"));
  await page.evaluate(() => {
    LONG("cut-short", {}, () => LOST.push("cut-short"));
    document.title = "kitlong:" + JSON.stringify({ op: "raw", text: "y".repeat(3000) });
    setTimeout(() => LONG("next-one"), 50);
  });
  const cr = await cutRead;
  check("Emacs could not read it: the page gives it up and goes on",
    cr.got.some((i) => i.unread === 1000) && cr.got.some((i) => i.op === "next-one") &&
    JSON.stringify(await page.evaluate(() => LOST)) === '["cut-short"]', JSON.stringify(cr.got));

  // Left unread 2 s with another to send, it is given up on: sent over.
  await page.evaluate(() => { LOST.length = 0; LONG("unheard", {}, () => LOST.push("unheard")); LONG("heard"); });
  await readLikeEmacs("kitlong:", () => false, { deaf: true, ms: 2300 });
  const overR = await readLikeEmacs("kitlong:", (g) => g.some((i) => i.op === "heard"), { ms: 3000 });
  check("unread for 2 s, it is given up and the next sent over it",
    JSON.stringify(await page.evaluate(() => LOST)) === '["unheard"]' && overR.got.map((i) => i.op).join() === "heard",
    JSON.stringify(overR.got));

  // XW.open: a tab of this device's browser, for a web address or a page
  // of Emacs's own; nothing else.
  const opened = await page.evaluate(() => {
    const seen = [];
    const real = window.open;
    window.open = (url, name, features) => { seen.push([url, name, features]); return null; };
    const said = ["https://bitbucket.org/x/y/pull-requests/1", "mailto:a@b.c", "/open/clickup-view#task=abc1",
                  "javascript:alert(1)", "//evil.example/x", "data:text/html,x", "", null]
      .map((u) => XW.open(u));
    XW.open("/open/pr-view#pr=x", "pr-view");
    window.open = real;
    return { said, seen };
  });
  check("XW.open: web addresses and Emacs's own pages, not javascript:, data: or another site's //",
    JSON.stringify(opened.said) === "[true,true,true,false,false,false,false,false]" && opened.seen.length === 4, JSON.stringify(opened));
  check("XW.open: a web address in a tab of its own, without its opener; Emacs's own page in its app's tab",
    JSON.stringify(opened.seen[0]) === '["https://bitbucket.org/x/y/pull-requests/1","_blank","noopener"]' &&
    JSON.stringify(opened.seen[3]) === '["/open/pr-view#pr=x","pr-view",""]', JSON.stringify(opened.seen));

  await browser.close();
  const passed = results.filter(Boolean).length;
  console.log(`\n${passed}/${results.length} passed`);
  process.exit(passed === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
