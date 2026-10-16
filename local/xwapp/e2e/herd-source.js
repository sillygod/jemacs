// A session's transcript in the Sessions view renders as markdown -- for
// agy too, now that it imports clean prose from transcript.jsonl rather
// than a protobuf salvage (so no more forced plain text).
const { webkit } = require("playwright");
const { phonePage, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const PATH = "/Users/x/.gemini/antigravity-cli/brain/abc/.system_generated/logs/transcript.jsonl";
const DBPATH = "/Users/x/.gemini/antigravity-cli/conversations/abc.db"; // the old salvage source
const body = async (page) => page.$eval("#src-body .msg .msg-body", (b) => b.innerHTML);

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["phone", "desk"]) {
      const { ctx, page, errors } = await phonePage(browser, { context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      await page.evaluate((path) => {
        GM.setContext({ sources: [{ source_path: path, agent: "agy", project: "~/p/", kind: "agy", chunks: 1, mtime: 2 }],
                        total: 1, current_project: "" });
        GM.showView("sessions");
      }, PATH);
      await sleep(80);
      await tap('.srow[data-path="' + PATH + '"]');
      await sleep(80);
      await page.evaluate((path) => GM.renderSource({ source_path: path, mode: "open", offset: 0, total: 1,
        chunks: [{ role: "assistant", chunk_index: 0, ts: "t", agent: "agy",
                   text: "**Done.** The checks:\n\n- one\n- two" }] }), PATH);
      await sleep(80);
      const html = await body(page);
      check(W("agy from transcript.jsonl renders markdown (bold + list)"),
        /<strong>Done\.<\/strong>/.test(html) && /<li>one<\/li>/.test(html) && !/class="plain"/.test(html), html);

      // The same agy, but imported from the .db salvage (no transcript.jsonl):
      // still plain text, since the salvage is tag-like noise.
      await page.evaluate((path) => {
        GM.setContext({ sources: [{ source_path: path, agent: "agy", project: "~/p/", kind: "agy", chunks: 1, mtime: 3 }], total: 1 });
        GM.showView("sessions");
      }, DBPATH);
      await sleep(80);
      await tap('.srow[data-path="' + DBPATH + '"]');
      await sleep(80);
      await page.evaluate((path) => GM.renderSource({ source_path: path, mode: "open", offset: 0, total: 1,
        chunks: [{ role: "assistant", chunk_index: 0, ts: "t", agent: "agy", text: "**still** salvage noise" }] }), DBPATH);
      await sleep(80);
      const dbHtml = await body(page);
      check(W("agy from the .db salvage stays plain, not markdown"),
        /class="plain"/.test(dbHtml) && /\*\*still\*\*/.test(dbHtml) && !/<strong>/.test(dbHtml), dbHtml);

      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const bad = results.filter((r) => !r).length;
  console.log(bad ? bad + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(bad ? 1 : 0);
})();
