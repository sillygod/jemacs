// jwebkit's own scripts in WebKit (ask fc2f4594): the ERT suite has them
// as strings, here they run.  jwebkit is not an xwapp page, but this is
// where Playwright is.  A hint (`f') onto a field says so, for Emacs to
// send the keys to the page; onto anything else, not.  ⌘V while keys go
// to the page puts the text in the focused field as typing would, the
// field hearing xw-paste first, as the xwapp kit's do.  Esc takes the
// focus off the field.  The scripts are Emacs's: jwebkit.el, in batch.
const path = require("path");
const { execFileSync } = require("child_process");
const { webkit } = require("playwright");
const LOCAL = process.env.LOCAL || path.resolve(__dirname, "../..");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const TEXT = "pasted «text» 你好 😀\nline two";

// The scripts, as jwebkit makes them.
const js = JSON.parse(execFileSync(process.env.EMACS || "emacs", ["-Q", "--batch", "-L", path.join(LOCAL, "jwebkit"), "-l", "jwebkit",
  "--eval", "(princ (json-encode (list :ace (jwebkit--ace-inject-js) :paste (jwebkit--paste-js " + JSON.stringify(TEXT) +
  ") :empty (jwebkit--paste-js \"\") :blur jwebkit--blur-js)))"], { encoding: "utf8" }));

const PAGE = `<!DOCTYPE html><body style="font: 16px sans-serif">
<input id="text" type="text" value="">
<input id="check" type="checkbox">
<button id="button" type="button">Go</button>
<a id="link" href="https://example.com/">a link</a>
<textarea id="area">Before: after</textarea>
<select id="select"><option>a</option><option>b</option></select>
<div id="rich" contenteditable="true" style="min-height: 2em">rich</div>
<textarea id="ro" readonly>read only</textarea>
<iframe id="frame" srcdoc="<textarea id=inner></textarea>" style="height: 60px"></iframe>
</body>`;

(async () => {
  const browser = await webkit.launch();
  try {
    const page = await browser.newPage();
    await page.setContent(PAGE);
    await page.waitForFunction(() => document.getElementById("frame").contentDocument.getElementById("inner"));

    // ---- f: a hint onto a field says so ----
    const act = async (id) => {
      const labels = JSON.parse(await page.evaluate(js.ace));
      const label = await page.$eval("#" + id, (e) => e.getAttribute("data-jw-ace"));
      if (!label || !labels.includes(label)) return { unhinted: true };
      return JSON.parse(await page.evaluate((l) => window.__jwAce.act(l, false), label));
    };
    // A link would leave the page: only its hint's answer is looked at.
    await page.route("https://example.com/**", (r) => r.fulfill({ status: 204 }));
    const want = { text: true, area: true, select: true, rich: true, check: false, button: false };
    const got = {};
    for (const id of Object.keys(want)) got[id] = (await act(id)).edit === true;
    check("a hint onto a text input, a textarea, a select or an editable element says it is a field",
      ["text", "area", "select", "rich"].every((id) => got[id]), got);
    check("onto a checkbox or a button, not", !got.check && !got.button, got);
    const link = await act("link");
    check("a link says its address, and no field", link.href === "https://example.com/" && !link.edit, link);
    await page.goto("about:blank");
    await page.setContent(PAGE);
    await page.waitForFunction(() => document.getElementById("frame").contentDocument.getElementById("inner"));

    // ---- ⌘V while keys go to the page ----
    await page.evaluate(() => {
      window.HEARD = [];
      document.addEventListener("input", (e) => HEARD.push("input:" + e.inputType), true);
      document.addEventListener("xw-paste", (e) => HEARD.push("xw-paste:" + e.detail.text), true);
    });
    await page.$eval("#area", (t) => { t.focus(); t.setSelectionRange(8, 8); });
    const said = await page.evaluate(js.paste);
    const v = await page.$eval("#area", (t) => t.value);
    const heard = await page.evaluate(() => HEARD.splice(0));
    check("the text goes in at the caret", said === "pasted" && v === "Before: " + TEXT + "after", { said, v });
    check("as typing would: xw-paste, then the field's own input event",
      heard[0] === "xw-paste:" + TEXT && heard.includes("input:insertText"), heard);
    await page.evaluate(() => document.execCommand("undo"));
    const undone = await page.$eval("#area", (t) => t.value);
    await page.evaluate(() => document.execCommand("redo"));
    check("on the page's undo stack", undone === "Before: after" && (await page.$eval("#area", (t) => t.value)) === v, undone);
    await page.evaluate(() => HEARD.splice(0));
    const empty = await page.evaluate(js.empty);
    check("no text on the clipboard: nothing goes in, the field hears it with \"\"",
      empty === "empty" && (await page.$eval("#area", (t) => t.value)) === v &&
      (await page.evaluate(() => HEARD.splice(0))).join() === "xw-paste:", empty);
    await page.evaluate(() => {
      const no = (e) => e.preventDefault();
      document.addEventListener("xw-paste", no, { once: true });
    });
    check("a page that takes it itself: nothing goes in", (await page.evaluate(js.paste)) === "taken" &&
      (await page.$eval("#area", (t) => t.value)) === v);
    await page.$eval("#rich", (e) => { e.focus(); getSelection().selectAllChildren(e); getSelection().collapseToEnd(); });
    check("in an editable element too", (await page.evaluate(js.paste)) === "pasted" &&
      (await page.$eval("#rich", (e) => e.innerText)).startsWith("rich" + "pasted «text»"), await page.$eval("#rich", (e) => e.innerText));
    await page.$eval("#ro", (t) => t.focus());
    check("not in a read-only field", (await page.evaluate(js.paste)) === "nofield");
    await page.evaluate(() => document.activeElement.blur());
    check("nor with no field focused", (await page.evaluate(js.paste)) === "nofield");
    await page.evaluate(() => { const f = document.getElementById("frame"); f.focus(); f.contentDocument.getElementById("inner").focus(); });
    check("in a field of a frame of the page's own", (await page.evaluate(js.paste)) === "pasted" &&
      (await page.evaluate(() => document.getElementById("frame").contentDocument.getElementById("inner").value)) === TEXT);

    // ---- Esc: the field lets go ----
    await page.$eval("#text", (t) => t.focus());
    await page.evaluate(js.blur);
    check("Esc takes the focus off the field", await page.evaluate(() => document.activeElement === document.body));
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
