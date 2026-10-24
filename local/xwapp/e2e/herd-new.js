// + New agent in the top bar: an agent in ANY project, or on one of its
// past conversations, from any tab.  The project box offers the places
// Emacs sends when the form asks (never with the herd); a project's past
// conversations come as it is typed, with its own CLI; one picked is
// resumed (or forked) on its own CLI, one already running is shown.  Open
// room goes to the project's room, which with no agent in it is a chat
// whose box starts the lead.  On a phone and at a desk.
const { webkit } = require("playwright");
const { phonePage, HERD, sleep } = require("./herdlib");
const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };
const DESK = { viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2, isMobile: false, hasTouch: false };
const JN = "~/src/jnotes/";
const FRESH = "~/src/fresh/";
const PLACES = [
  { project: JN, note: "this project · 1 agent", here: true },
  { project: "~/src/deploy-platform/", note: "3 agents · lead", here: false },
  { project: FRESH, note: "last conversation 3d ago", here: false },
];
const CONVS = [
  { path: "/t/a.jsonl", kind: "claude", title: "Fix the sync", session: "aaaa1111", note: "claude · 3d ago", fork: true, running: "" },
  { path: "/t/b", kind: "agy", title: "", session: "bbbb2222", note: "agy · 5d ago · running in agy-jnotes", fork: false, running: "agy-jnotes" },
];

(async () => {
  const browser = await webkit.launch();
  try {
    for (const where of ["desk", "phone"]) {
      const intents = [];
      const { ctx, page, errors } = await phonePage(browser, { intents, context: where === "desk" ? DESK : {} });
      const W = (n) => where + ": " + n;
      const tap = (sel) => (where === "phone" ? page.tap(sel) : page.click(sel));
      const sent = (op) => intents.filter((i) => i.op === op);
      const draw = (h) => page.evaluate((h) => GM.renderHerd(h), h || HERD);
      const away = () => page.evaluate(() => document.activeElement && document.activeElement.blur());
      const form = () => page.$eval(".newform.newglobal", (f) => ({
        project: f.querySelector(".new-project").value,
        kind: f.querySelector(".new-kind").value,
        kindOff: f.querySelector(".new-kind").disabled,
        conv: f.querySelector(".new-conv").value,
        convs: [...f.querySelector(".new-conv").options].map((o) => o.textContent),
        convOff: f.querySelector(".new-conv").disabled,
        nameShown: !f.querySelector(".new-name").hidden,
        wtShown: !f.querySelector(".new-worktree-l").hidden,
        forkShown: !f.querySelector(".new-fork-l").hidden,
        go: f.querySelector(".new-go").textContent,
        note: f.querySelector(".new-note").textContent,
        top: f.parentElement.id === "herd" && f === f.parentElement.firstElementChild,
      })).catch(() => null);

      // From another tab: the button opens the Agents tab, the form on top.
      await page.evaluate((h) => { GM.showView("log"); GM.renderHerd(h); }, HERD);
      await sleep(80);
      const btn = await page.$eval("#btn-new", (b) => {
        const r = b.getBoundingClientRect();
        return { text: b.textContent.trim(), right: r.right, width: document.documentElement.clientWidth, shown: r.width > 0 };
      });
      check(W("+ New agent is in the top bar, on screen"), btn.shown && btn.right <= btn.width && btn.text.startsWith("+ New"), btn);
      await tap("#btn-new");
      await sleep(150);
      let f = await form();
      check(W("it opens the Agents tab with the form above the herd"),
        f && f.top && (await page.$eval(".tab.on", (t) => t.getAttribute("data-tab"))) === "agents", f);
      check(W("the places are asked of Emacs, and nothing else yet"),
        sent("new-places").length === 1 && sent("new-conversations").length === 0, intents);

      // The places come: the current project goes in the box, its
      // conversations are asked.
      await page.evaluate((p) => GM.setNewPlaces({ places: p }), PLACES);
      await sleep(80);
      f = await form();
      check(W("the current project fills the box"), f.project === JN, f);
      const opts = await page.$$eval("#new-places option", (os) => os.map((o) => [o.value, o.textContent]));
      check(W("the box offers every place, with what it has"),
        JSON.stringify(opts) === JSON.stringify(PLACES.map((p) => [p.project, p.note])), opts);
      check(W("its past conversations are asked"),
        JSON.stringify(sent("new-conversations")) === JSON.stringify([{ op: "new-conversations", project: JN }]), sent("new-conversations"));
      check(W("until they come, it says so"), f.note === "Reading its past conversations…" && f.convOff, f);

      await page.evaluate((c) => GM.setNewConversations({ project: "~/src/jnotes/", root: "~/src/jnotes/", kind: "grok", conversations: c }), CONVS);
      await sleep(50);
      f = await form();
      check(W("the project's own CLI is preselected"), f.kind === "grok" && !f.kindOff, f);
      check(W("New first, then the conversations with title, kind and age"),
        JSON.stringify(f.convs) === JSON.stringify(["New", "Fix the sync · claude · 3d ago",
          "(untitled) bbbb2222 · agy · 5d ago · running in agy-jnotes"]) && f.conv === "", f.convs);
      check(W("New: a name, a worktree, Start"), f.nameShown && f.wtShown && !f.forkShown && f.go === "Start", f);

      // A conversation: its own CLI, fixed; no name or worktree; fork.
      await page.selectOption(".newform .new-conv", "/t/a.jsonl");
      await sleep(50);
      f = await form();
      check(W("one picked: its CLI, fixed; no name, no worktree; Resume"),
        f.kind === "claude" && f.kindOff && !f.nameShown && !f.wtShown && f.forkShown && f.go === "Resume", f);
      await page.check(".newform .new-fork");
      await sleep(30);
      check(W("fork ticked: Fork"), (await form()).go === "Fork", await form());
      // The herd drawn again while the form is not focused: what was
      // picked stays.
      await away();
      await draw();
      await sleep(50);
      f = await form();
      check(W("a drawing keeps the project, the conversation and fork"),
        f && f.project === JN && f.conv === "/t/a.jsonl" && f.go === "Fork", f);
      await tap('.newform [data-act="a:new-go"]');
      await sleep(120);
      check(W("Fork: Emacs resumes it by its transcript, forked, without taking focus"),
        JSON.stringify(sent("resume")) === JSON.stringify([{ op: "resume", source_path: "/t/a.jsonl", fork: true, quiet: true }]), sent("resume"));
      check(W("and the form goes"), !(await page.$(".newform")));

      // One already running: shown, not started twice.
      await tap("#btn-new");
      await sleep(80);
      check(W("opened again: the places as they were, its conversations asked afresh"),
        (await form()).project === JN && sent("new-conversations").length === 2, sent("new-conversations"));
      await page.evaluate((c) => GM.setNewConversations({ project: "~/src/jnotes/", root: "~/src/jnotes/", kind: "grok", conversations: c }), CONVS);
      await page.selectOption(".newform .new-conv", "/t/b");
      await sleep(50);
      f = await form();
      check(W("one running: Show it, no fork where its CLI cannot"),
        f.go === "Show agy-jnotes" && !f.forkShown && /agy-jnotes is on it already/.test(f.note), f);

      // A directory typed: its project's conversations, none; Start sends
      // the directory as typed.
      const asked = sent("new-conversations").length;
      await page.fill(".newform .new-project", "");
      await page.type(".newform .new-project", FRESH + "web", { delay: 15 });
      await sleep(450);
      check(W("typing a directory asks for it once, after the pause -- not for each key"),
        JSON.stringify(sent("new-conversations").slice(asked)) === JSON.stringify([{ op: "new-conversations", project: FRESH + "web" }]),
        sent("new-conversations").slice(asked));
      await page.evaluate((p) => GM.setNewConversations({ project: p + "web", root: p, kind: "claude", conversations: [] }), FRESH);
      await sleep(50);
      f = await form();
      check(W("none there: New only, the conversation choice off"), f.convOff && f.conv === "" && f.go === "Start" && f.kind === "claude", f);
      // Drawn while typing: the box keeps what is typed and its focus.
      await page.focus(".newform .new-project");
      await draw();
      await sleep(50);
      check(W("a drawing while typing leaves the box alone"),
        (await form()).project === FRESH + "web" &&
        (await page.evaluate(() => document.activeElement && document.activeElement.classList.contains("new-project"))));
      await tap('.newform [data-act="a:new-go"]');
      await sleep(120);
      check(W("Start: agent-new in the directory typed, the project's CLI"),
        JSON.stringify(sent("agent-new")) === JSON.stringify([{ op: "agent-new", project: FRESH + "web", kind: "claude", name: "", worktree: false }]),
        sent("agent-new"));

      // Nothing typed: Start asks which project, and sends nothing.
      await page.evaluate((h) => { GM.setNewPlaces({ places: [] }); }, null);
      await tap("#btn-new");
      await sleep(80);
      await page.evaluate(() => GM.setNewPlaces({ places: [] }));
      await page.fill(".newform .new-project", "");
      await tap('.newform [data-act="a:new-go"]');
      await sleep(80);
      check(W("no project: nothing sent, the form stays"), sent("agent-new").length === 1 && !!(await page.$(".newform")));

      // Open room before Emacs has said which project the text is in: no
      // room of the text (its lead would be a second, on part of a
      // project) -- it waits, asked once, and opens the project's.
      await page.fill(".newform .new-project", "~/src/fresh");
      await tap('.newform [data-act="a:new-room"]');
      await sleep(400);
      const early = sent("new-conversations").filter((i) => i.project === "~/src/fresh");
      const status = await page.$eval("#status", (e) => (e.hidden ? "" : e.textContent));
      check(W("Open room before the answer: no room yet, asked once, said so"),
        !(await page.$(".room")) && early.length === 1 && /Finding its project/.test(status), { early, status });
      await page.evaluate(() => GM.setNewConversations({ project: "~/src/fresh", root: "~/src/fresh/", kind: "claude", conversations: [] }));
      await sleep(150);
      check(W("and once Emacs says, the project's room opens"),
        (await page.$eval(".room .rhead .ppath", (e) => e.textContent).catch(() => null)) === FRESH);
      await tap("#btn-new");
      await sleep(80);

      // Open room: the project's room, by the name Emacs gave it.
      await page.fill(".newform .new-project", FRESH + "web");
      await sleep(450);
      await page.evaluate((p) => GM.setNewConversations({ project: p + "web", root: p, kind: "claude", conversations: [] }), FRESH);
      await tap('.newform [data-act="a:new-room"]');
      await sleep(200);
      const room = await page.evaluate(() => ({
        name: (document.querySelector(".room .rhead .ppath") || {}).textContent,
        chat: !!document.querySelector(".room .chat"),
        to: [...document.querySelectorAll(".chat .chat-to option")].map((o) => [o.value, o.textContent]),
        note: (document.querySelector(".chat .cbody") || {}).textContent,
      }));
      check(W("Open room: the project's room, not the directory's"), room.name === FRESH, room);
      check(W("with no agent, a chat whose box starts the lead"),
        room.chat && JSON.stringify(room.to) === JSON.stringify([["@lead", "lead (starts when you write)"]]) &&
        /No agents here yet\.  Write to its lead below/.test(room.note), room);
      const rw = sent("room-watch").pop();
      check(W("Emacs is asked for the chat"), rw && rw.project === FRESH && rw.view === "chat", rw);
      await page.fill(".chat textarea", "set this project up");
      await tap('.chat [data-act="a:send"]');
      await sleep(120);
      check(W("writing there: lead-say for the project"),
        JSON.stringify(sent("lead-say")) === JSON.stringify([{ op: "lead-say", project: FRESH, text: "set this project up" }]), sent("lead-say"));
      // The lead comes: on the desk the room goes back to its columns, and
      // Emacs is told; on the phone it stays the chat.
      const before = sent("room-watch").length;
      const h = JSON.parse(JSON.stringify(HERD));
      h.agents.push(Object.assign({}, h.agents[0], { name: "lead-fresh", project: FRESH, role: "lead", state: "starting", links: false }));
      await draw(h);
      await sleep(150);
      const after = await page.evaluate(() => ({ cols: document.querySelectorAll(".room .rcol").length, chat: !!document.querySelector(".room .chat") }));
      if (where === "desk")
        check(W("the lead come, the desk's room is its columns again, and Emacs told"),
          after.cols === 1 && !after.chat && sent("room-watch").length === before + 1 && sent("room-watch").pop().view === "columns",
          { after, watches: sent("room-watch").slice(before) });
      else
        check(W("the lead come, the phone's room stays a chat, now to it"),
          after.chat && sent("room-watch").length === before &&
          (await page.$eval(".chat .chat-to", (s) => s.value)) === "lead-fresh", after);

      // The herd with nobody in it says how to start one.
      await page.evaluate(() => GM.showView("agents"));
      await draw(Object.assign({}, HERD, { agents: [] }));
      await sleep(80);
      const empty = await page.$eval("#herd .empty", (e) => e.textContent).catch(() => null);
      check(W("an empty herd: + New agent or SPC a h n, any project"), /\+ New agent \(above\) or SPC a h n starts one, in any project/.test(empty), empty);
      check(W("no page errors"), errors.length === 0, errors);
      await ctx.close();
    }
  } finally {
    await browser.close();
  }
  const ok = results.filter(Boolean).length;
  console.log(`\n${ok}/${results.length} passed`);
  process.exit(ok === results.length ? 0 : 1);
})().catch((e) => { console.error(e); process.exit(1); });
