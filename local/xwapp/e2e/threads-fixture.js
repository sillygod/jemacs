// Threads as the sidecar sends them (herd_threads / herd_thread, with the
// user's opening prompt Emacs adds), for the Threads tab's tests and
// screenshots: the mockup's five, from fixture data.
const now = Date.now();
const min = 60e3;
const L = (name, kind, project, role, worktree) => ({ name, kind, project: project || "", role: role || "", worktree: !!worktree });
const M = (kind, from, to, ago, head, extra) => Object.assign({ kind, from, to, at: now - ago * min, head, text: head, ask: "", round: null, of: null, auto: false, logged: false }, extra || {});
const LONG = "Three asks to claude-jnotes were closed by a `claude -p` its backend ran.\n" +
  "The backend was started with nohup from a tool shell and left its tree for launchd; the CLIs it runs\n" +
  "inherit GHOSTHERD_SESSION, and the herd took each one's Stop as the agent's own.\n" +
  "What we need from .emacs.d: tie each agent to its terminal, and say what jnotes should change on its side.";

const DETAILS = {
  t1: { id: "t1", title: "Three hardenings of the agy guard, approved by the user", status: "live",
    lanes: [L("user", "you"), L("lead-.emacs.d", "claude", ".emacs.d", "lead"), L("claude-.emacs.d-2", "claude", ".emacs.d", "", true)],
    msgs: [M("message", "user", "lead-.emacs.d", 8, "三項補強都做，agy 也重開", { logged: true }),
           M("ask", "lead-.emacs.d", "claude-.emacs.d-2", 6, "Three hardenings of the agy guard, approved by the user.\nScoped token · env allowlist · mark asks from guarded agents.", { ask: "a11", round: 1, of: 5 })],
    waiting: [{ ask: "a11", from: "lead-.emacs.d", to: "claude-.emacs.d-2", status: "delivered", since: now - 6 * min, working: true, depth: 0 }],
    rootFrom: "lead-.emacs.d", rootTo: "claude-.emacs.d-2", projects: [".emacs.d"], cross: false, started: now - 6 * min, at: now - 6 * min },
  t2: { id: "t2", title: "claude -p children closed asks", status: "answered",
    lanes: [L("lead-jnotes", "claude", "jnotes", "lead"), L("lead-.emacs.d", "claude", ".emacs.d", "lead"),
            L("claude-.emacs.d-2", "claude", ".emacs.d", "", true), L("claude-.emacs.d", "claude", ".emacs.d")],
    msgs: [M("handoff", "lead-jnotes", "lead-.emacs.d", 140, LONG.split("\n")[0], { ask: "h1", text: LONG }),
           M("ask", "lead-.emacs.d", "claude-.emacs.d-2", 135, "Tie each agent to its terminal, not “not under the old pin”.", { ask: "a21", round: 1, of: 5 }),
           M("answer", "claude-.emacs.d-2", "lead-.emacs.d", 89, "9a27404: host pid + headless marker. The evening replayed.", { ask: "a21" }),
           M("ask", "lead-.emacs.d", "claude-.emacs.d", 87, "Review and merge 9a27404, 3 rounds at most.", { ask: "a22", round: 2, of: 5 }),
           M("answer", "claude-.emacs.d", "lead-.emacs.d", 62, "Merged 72f7736. Nothing drops the real agent’s reports.", { ask: "a22" }),
           M("answer", "lead-.emacs.d", "lead-jnotes", 59, "Fixed and live. Still yours: strip GHOSTHERD_* in cli_ai.py.", { ask: "h1" })],
    waiting: [], rootFrom: "lead-jnotes", rootTo: "lead-.emacs.d", projects: ["jnotes", ".emacs.d"], cross: true,
    started: now - 140 * min, at: now - 42 * min },
  t3: { id: "t3", title: "Sum up jnotes: commits, uncommitted work, next steps", status: "waiting",
    lanes: [L("user", "you"), L("primary", "claude", "primary", "primary"), L("lead-jnotes", "claude", "jnotes", "lead"), L("claude-jnotes", "claude", "jnotes")],
    msgs: [M("message", "user", "primary", 68, "jnote 專案狀況如何", { logged: true }),
           M("handoff", "primary", "lead-jnotes", 65, "Sum up jnotes: commits, uncommitted work, next steps.", { ask: "h3" }),
           M("ask", "lead-jnotes", "claude-jnotes", 63, "List today’s commits and what is uncommitted.", { ask: "a31", round: 1, of: 5 })],
    waiting: [{ ask: "a31", from: "lead-jnotes", to: "claude-jnotes", status: "delivered", since: now - 58 * min, working: false, depth: 1 },
              { ask: "h3", from: "primary", to: "lead-jnotes", status: "delivered", since: now - 60 * min, working: false, depth: 0 }],
    rootFrom: "primary", rootTo: "lead-jnotes", projects: ["jnotes"], cross: true, started: now - 65 * min, at: now - 58 * min },
  t4: { id: "t4", title: "Review this project’s memory and propose stable rules", status: "answered",
    lanes: [L("user", "you"), L("lead-deploy-platform", "claude", "deploy-platform", "lead")],
    msgs: [M("ask", "user", "lead-deploy-platform", 190, "Review this project’s memory and propose stable rules.", { ask: "a41" }),
           M("answer", "lead-deploy-platform", "user", 185, "Two proposals: 4 facts → CLAUDE.md, 1 → frontend.md.", { ask: "a41" })],
    waiting: [], rootFrom: "user", rootTo: "lead-deploy-platform", projects: ["deploy-platform"], cross: false, started: now - 190 * min, at: now - 185 * min },
  t5: { id: "t5", title: "Fetch the release notes for 4.2", status: "failed",
    lanes: [L("lead-deploy-platform", "claude", "deploy-platform", "lead"), L("qa-deploy-platform", "agy", "deploy-platform")],
    msgs: [M("ask", "lead-deploy-platform", "qa-deploy-platform", 300, "Fetch the release notes for 4.2", { ask: "a51", round: 1, of: 5 }),
           M("failed", "qa-deploy-platform", "lead-deploy-platform", 295, "qa-deploy-platform died before answering", { ask: "a51" })],
    waiting: [], rootFrom: "lead-deploy-platform", rootTo: "qa-deploy-platform", projects: ["deploy-platform"], cross: false, started: now - 300 * min, at: now - 295 * min },
};
const summary = (d) => ({ id: d.id, title: d.title, status: d.status, participants: d.lanes.map((l) => l.name),
  projects: d.projects, cross: d.cross, count: d.msgs.length, open: d.waiting.length, started: d.started, at: d.at,
  rootFrom: d.rootFrom, rootTo: d.rootTo });
const THREADS = ["t1", "t2", "t3", "t4", "t5"].map((k) => summary(DETAILS[k]));
const TALKING = [
  { ask: "h3", thread: "t3", from: "primary", to: "lead-jnotes", kind: "handoff", status: "delivered", since: now - 60 * min, working: false },
  { ask: "a31", thread: "t3", from: "lead-jnotes", to: "claude-jnotes", kind: "ask", status: "delivered", since: now - 58 * min, working: true },
  { ask: "a11", thread: "t1", from: "lead-.emacs.d", to: "claude-.emacs.d-2", kind: "ask", status: "delivered", since: now - 6 * min, working: true },
  { ask: "a61", thread: "t6", from: "lead-deploy-platform", to: "claude-deploy-platform", kind: "ask", status: "queued", since: now - 1 * min, working: false },
];
const LIST = { threads: THREADS, talking: TALKING, live: 3, more: false };

// The thread a `thread-open' asks for: by id, an ask in it, or an agent's latest.
function find(req) {
  if (req.id) return DETAILS[req.id];
  if (req.ask) return Object.values(DETAILS).find((d) => d.msgs.some((m) => m.ask === req.ask));
  if (req.agent) return Object.values(DETAILS).find((d) => d.lanes.some((l) => l.name === req.agent));
  return null;
}

// Play Emacs: answer each new `thread-open' the page sends.
function serve(page, intents) {
  let seen = 0;
  const timer = setInterval(async () => {
    while (seen < intents.length) {
      const it = intents[seen++];
      if (it.op !== "thread-open") continue;
      const d = find(it) || { error: "ghostherd-memory JSON-RPC -32602: no thread with " + (it.agent || it.ask || it.id) + " in it" };
      await page.evaluate((p) => GM.renderThread(p), d).catch(() => {});
    }
  }, 40);
  return () => clearInterval(timer);
}

module.exports = { DETAILS, THREADS, TALKING, LIST, find, serve };
