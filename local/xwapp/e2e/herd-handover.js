// The herd page changes hands: what Emacs watched for the page that had
// it stops, and the page that takes it gets only what it opens.  A real
// Emacs (herd-room.el: a room of four, the sidecar played) and two
// browsers: the desk's, on Emacs's loopback port, with a room open --
// and, as Emacs's own page had them live, the Board, the Inbox, Threads
// and a sub-agent's transcript watched too (asked as that page, with its
// own page id) -- then the phone's tab, through the proxy, which takes
// the page over.  Nothing of the desk's -- Board, room, sub-agent,
// Threads -- goes to the phone until the phone opens it; opening each
// there works; the phone asleep past Emacs's gone, and woken, goes on
// with what it had open; and the desk taking the page back gets its room
// again.  Synthetic data.
const fs = require("fs");
const path = require("path");
const { start, watch, phoneEyes, BOX, PHONE, until, sleep, log } = require("./appslib");

const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const PROJECT = "/Users/someone/src/fox/";
const DESK = { viewport: { width: 1400, height: 900 }, deviceScaleFactor: 1 };
const DESKCALLS = ["boardPatch", "setConversation", "setChat", "setSubConversation", "renderThreads"];

(async () => {
  const s = await start({ el: "herd-room.el", desk: true, env: { E2E_GONE: "1.5" } });
  const said = (line) => JSON.stringify({ role: "assistant", text: line, ts: new Date().toISOString() }) + "\n";
  const transcript = (name) => path.join(s.dir, "transcripts", name + ".jsonl");
  // A sub-agent of rd-api's, its own transcript beside rd-api's.
  const subFile = path.join(s.dir, "transcripts", "rd-api", "subagents", "agent-abc123.jsonl");
  fs.mkdirSync(path.dirname(subFile), { recursive: true });
  fs.writeFileSync(subFile, said("The sub-agent read the routes."));
  const notes = () => log(s.dir);
  const flushedSince = (i) => notes().slice(i).filter((w) => w.flush).flatMap((w) => w.flush.map((c) => c.fn));
  try {
    // ---- The desk: a room open, the rest watched ------------------------------
    const dctx = await s.browser.newContext(DESK);
    const desk = await dctx.newPage();
    const derrors = [];
    watch(desk, derrors);
    let deskId = "";
    desk.on("request", (r) => { const m = /[?&]page=([0-9a-f]{16})/.exec(r.url()); if (m) deskId = m[1]; });
    await desk.goto(s.urls.herd);
    await desk.waitForFunction(() => window.GM && window.XW);
    await desk.click('.tab[data-tab="agents"]');
    await until(() => desk.evaluate((p) => !!document.querySelector('.pgroup[data-project="' + p + '"]'), PROJECT), 30000);
    await desk.click('.pgroup[data-project="' + PROJECT + '"] [data-act="a:room"]');
    await until(() => desk.evaluate(() => document.querySelectorAll(".rcol .rbody .ent").length > 0), 30000);
    // As the desk's page, by its id: what Emacs's own page had open live.
    const ask = (op) => desk.evaluate(([id, op]) => fetch("/" + location.pathname.split("/")[1] + "/intent?page=" + id,
      { method: "POST", headers: { "content-type": "text/plain" }, body: JSON.stringify(op) }).then((r) => r.status), [deskId, op]);
    const asked = [await ask({ op: "board-watch", days: 1 }), await ask({ op: "inbox-watch" }),
                   await ask({ op: "threads-watch" }), await ask({ op: "sub-watch", name: "rd-api", sub: "abc123" })];
    await sleep(6000);
    const deskGot = flushedSince(0);
    check("desk: its room, the Board, the sub-agent and the Inbox watched and sent",
      asked.every((c) => c === 204) && ["setConversation", "boardPatch", "setSubConversation", "renderInbox"].every((f) => deskGot.includes(f)),
      { asked, got: [...new Set(deskGot)] });

    // ---- The phone takes the page over --------------------------------------------
    const pctx = await s.browser.newContext(PHONE);
    await phoneEyes(pctx);
    const phone = await pctx.newPage();
    const perrors = [];
    watch(phone, perrors);
    await phone.goto(BOX + "/" + s.secretOf("herd") + "/ghostherd/ui/index.html");
    await phone.waitForFunction(() => window.GM && window.XW);
    await until(() => notes().some((w) => w.watches === "hello" && w.page && w.page !== deskId), 15000);
    const at = notes().findIndex((w) => w.watches === "hello" && w.page && w.page !== deskId);
    const taken = notes()[at];
    check("phone takes over: every watch of the desk's stopped as it says hello",
      taken && !taken.board && !taken.inbox && !taken.threads && !taken.room && !taken.sub && !taken.herd && !taken.peek, taken);
    // What the desk watched goes on changing: the room's words, the
    // sub-agent's, the Board at each look.
    for (let i = 0; i < 3; i++) {
      fs.appendFileSync(transcript("rd-api"), said("A new turn on the desk's column " + i));
      fs.appendFileSync(subFile, said("The sub-agent went on " + i));
      await sleep(2200);
    }
    const leaked = flushedSince(at).filter((f) => DESKCALLS.includes(f));
    check("phone: nothing of the desk's Board, room, sub-agent or Threads comes until it opens them", leaked.length === 0,
      [...new Set(leaked)]);
    check("desk: told it is open elsewhere", await until(() => desk.evaluate(() =>
      /open elsewhere/.test((document.getElementById("xw-notice") || {}).textContent || "")), 8000));

    // ---- Opened on the phone, each works ---------------------------------------------
    await phone.tap('.tab[data-tab="board"]');
    const board = await until(() => phone.evaluate(() => /Build the fox page/.test(document.querySelector(".boardpage") ? document.querySelector(".boardpage").textContent : "")), 15000);
    check("phone: the Board opened shows its items", board);
    const lookOf = () => phone.evaluate(() => { const m = /look (\d+)/.exec(document.querySelector(".boardpage").textContent); return m ? +m[1] : 0; });
    const look1 = await lookOf();
    await phone.tap('.tab[data-tab="agents"]');
    await until(() => phone.evaluate((p) => !!document.querySelector('.pgroup[data-project="' + p + '"]'), PROJECT), 15000);
    await phone.tap('.pgroup[data-project="' + PROJECT + '"] [data-act="a:room"]');
    const roomOk = await until(() => phone.evaluate(() => document.querySelectorAll(".chat .cbody .cmsg, .rcol .rbody .ent").length > 0), 30000);
    check("phone: the room opened shows its conversation", roomOk);
    await sleep(3000);  // the rest of it in
    fs.appendFileSync(transcript("rd-api"), said("Said while the phone reads the room"));
    const live = await until(() => phone.evaluate(() => /Said while the phone reads the room/.test(document.querySelector(".room").textContent)), 15000);
    check("phone: and follows what is said in it", live);

    // ---- Asleep past Emacs's gone, and woken: what it had open goes on ---------------------
    const beforeNap = notes().length;
    s.sleep();
    await phone.evaluate(() => __vis("hidden"));
    await until(() => notes().slice(beforeNap).some((w) => w.watches === "gone"), 15000);
    const gone = notes().slice(beforeNap).find((w) => w.watches === "gone");
    check("asleep past the gone: Emacs stops what it did for the phone", gone && !gone.room && !gone.herd, gone);
    s.wake();
    await phone.evaluate(() => __vis("visible"));
    await until(() => notes().slice(beforeNap).some((w) => w.intent === "room-watch"), 15000);
    fs.appendFileSync(transcript("rd-api"), said("Said after the phone woke"));
    const woke = await until(() => phone.evaluate(() => /Said after the phone woke/.test(document.querySelector(".room").textContent)), 20000);
    check("woken: the same tab watches its room again by itself, and follows it", woke);
    // On the Board, the same.
    await phone.tap('.tab[data-tab="board"]');
    await until(() => phone.evaluate(() => !!document.querySelector(".boardpage .bdarea")), 15000);
    const beforeNap2 = notes().length;
    s.sleep();
    await phone.evaluate(() => __vis("hidden"));
    await until(() => notes().slice(beforeNap2).some((w) => w.watches === "gone"), 15000);
    s.wake();
    await phone.evaluate(() => __vis("visible"));
    await until(() => notes().slice(beforeNap2).some((w) => w.intent === "board-watch"), 15000);
    const look2 = await until(async () => (await lookOf()) > look1 + 1, 20000);
    check("woken on the Board: watched again, its items come on", look2, { look1, now: await lookOf() });

    // ---- The desk takes it back: its room again, the phone's watches stopped -----------------
    const atBack = notes().length;
    await desk.click("#xw-notice .xw-notice-act");
    await until(() => notes().slice(atBack).some((w) => w.watches === "hello" && w.page === deskId), 15000);
    const back = notes().slice(atBack).find((w) => w.watches === "hello" && w.page === deskId);
    check("desk takes it back: the phone's watches stopped as it says hello", back && !back.board && !back.room, back);
    fs.appendFileSync(transcript("rd-api"), said("Said with the desk back"));
    const deskLive = await until(() => desk.evaluate(() => /Said with the desk back/.test(document.querySelector(".room").textContent)), 20000);
    check("desk: its room watched again, and followed", deskLive);

    const quiet = (es) => es.filter((e) => !/Failed to load resource|network connection was lost|could not be found|status of 50[02]|status of 410/.test(e));
    check("no page errors", quiet(derrors).length === 0 && quiet(perrors).length === 0,
      { desk: quiet(derrors).slice(0, 3), phone: quiet(perrors).slice(0, 3) });
    await dctx.close();
    await pctx.close();
  } finally {
    await s.stop();
  }
  const failed = results.filter((r) => !r).length;
  console.log(failed ? failed + " of " + results.length + " failed" : "all " + results.length + " passed");
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
