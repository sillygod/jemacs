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
